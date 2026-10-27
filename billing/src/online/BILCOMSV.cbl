       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILCOMSV.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILCOMSV                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Producer commission service. Reserves commission *
      *               on premium invoice items for the period's        *
      *               producer, earns it as the insured pays, reverses *
      *               earned commission when payments are reversed or  *
      *               items cancelled, and answers producer balance    *
      *               inquiries.                                       *
      *               Ledger model: COMMRSV is the commission expense  *
      *               reserved, COMMPAY the producer liability; the    *
      *               producer PAYABLE_BALANCE is the earned (payable) *
      *               sub-ledger of COMMPAY.                           *
      * REQUESTS    : CMSRSV  reserve commission on ITEM-ID            *
      *               CMSERN  earn commission for PAID-AMOUNT          *
      *               CMSRVS  reverse earned commission / release      *
      *               CMSINQ  producer balance and counts              *
      * CALLED BY   : BILPPER0 (reserve, release), BILPAY family       *
      *               (earn, reverse), BILPRDP screen (inquiry)        *
      * CALLS       : BILSEQGN BILMONEY BILGLPST BILERRLG              *
      * TABLES      : BIL_COMMISSION_ITEM   (C R U)                    *
      *               BIL_COMMISSION_PLAN   (R)                        *
      *               BIL_COMMISSION_RATE   (R)                        *
      *               BIL_PRODUCER          (R U)                      *
      *               BIL_INVOICE_ITEM      (R U)                      *
      *               BIL_CHARGE            (R)                        *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_CONTROL           (R)                        *
      * RULES       : BR-BIL-CMS-001 Only PREMIUM pattern items earn   *
      *                              commission                        *
      *               BR-BIL-CMS-002 Producer is the requested one,    *
      *                              else the period's; no producer =  *
      *                              no commission (warning)           *
      *               BR-BIL-CMS-003 Commission is reserved only for   *
      *                              an ACTV producer on an active     *
      *                              commission plan                   *
      *               BR-BIL-CMS-004 Rate = BIL_COMMISSION_RATE for    *
      *                              plan/product/PREMIUM, else the    *
      *                              plan DEFAULT_RATE_BPS             *
      *               BR-BIL-CMS-005 One live commission item per      *
      *                              invoice item; cancelled items are *
      *                              never reserved                    *
      *               BR-BIL-CMS-006 Reserve posts DR COMMRSV / CR     *
      *                              COMMPAY; a chargeback the reverse *
      *               BR-BIL-CMS-007 A chargeback on return premium    *
      *                              (negative basis) is earned at     *
      *                              once and reduces the payable      *
      *               BR-BIL-CMS-008 Earn basis P: commission earned = *
      *                              rate x amount paid on the item    *
      *               BR-BIL-CMS-009 EARN when earned reaches the      *
      *                              commission or the item is fully   *
      *                              settled; the rounding remainder   *
      *                              is earned on that last payment    *
      *               BR-BIL-CMS-010 Earned commission is added to the *
      *                              producer PAYABLE_BALANCE          *
      *               BR-BIL-CMS-011 A payment reversal un-earns rate  *
      *                              x reversed amount, capped at what *
      *                              was earned; state back to RSRV    *
      *               BR-BIL-CMS-012 Cancelled item: the whole         *
      *                              commission is released (CANC),    *
      *                              earned part taken off the payable *
      *               BR-BIL-CMS-013 Commission already paid out is    *
      *                              still recovered through the       *
      *                              payable (may go negative)         *
      *               BR-BIL-CMS-014 Commission rows are updated only  *
      *                              if unchanged since read           *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILCOMSV'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'BILMONEY'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-PRODUCER-ID           PIC S9(9) COMP VALUE 0.
           05 WS-RATE-BPS              PIC S9(4) COMP VALUE 0.
           05 WS-COMM-AMOUNT           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DELTA                 PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OLD-EARNED            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OLD-STATE             PIC X(4)  VALUE SPACES.
           05 WS-NEW-STATE             PIC X(4)  VALUE SPACES.
           05 WS-LIVE-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-UNEARNED              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-UNPAID-EARNED         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-GL-AMOUNT             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-GL-TXN-TYPE           PIC X(8)  VALUE SPACES.
           05 WS-GL-DR-ACCT            PIC X(8)  VALUE SPACES.
           05 WS-GL-CR-ACCT            PIC X(8)  VALUE SPACES.
           05 WS-MN-TOTAL              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-MN-RATE-BPS           PIC S9(4) COMP VALUE 0.
           05 WS-MN-RESULT             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.

       01  WS-CALLEE.
           05 WS-CALLEE-RC             PIC 9(2).
           05 WS-CALLEE-REASON         PIC X(8).
           05 WS-CALLEE-MESSAGE        PIC X(80).

      * Invoice item with its charge pattern and period attributes
       01  WS-ITEM-CTX.
           05 WS-IT-ACCOUNT-ID         PIC S9(9) COMP.
           05 WS-IT-PP-ID              PIC S9(9) COMP.
           05 WS-IT-AMOUNT             PIC S9(11)V99 COMP-3.
           05 WS-IT-SETTLED            PIC S9(11)V99 COMP-3.
           05 WS-IT-STATUS             PIC X(4).
           05 WS-IT-PATTERN            PIC X(8).
           05 WS-IT-PRODUCT            PIC X(1).
           05 WS-IT-PRODUCER-ID        PIC S9(9) COMP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCMI END-EXEC.
           EXEC SQL INCLUDE BILDCPL END-EXEC.
           EXEC SQL INCLUDE BILDCRT END-EXEC.
           EXEC SQL INCLUDE BILDPRD END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-MNY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY BILCMNY REPLACING LEADING ==CA-== BY ==MNA-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCCMS.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'CMSRSV  '
                 PERFORM 2000-RESERVE
              WHEN 'CMSERN  '
                 PERFORM 2100-EARN
              WHEN 'CMSRVS  '
                 PERFORM 2200-REVERSE
              WHEN 'CMSINQ  '
                 PERFORM 2300-INQUIRE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILCOMSV: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-CM-COMM-AMOUNT
                                          CA-CM-EARNED-AMOUNT
                                          CA-CM-PAYABLE-BALANCE
                                          CA-CM-RESERVED-COUNT
                                          CA-CM-EARNED-COUNT
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO CA-BUS-DATE
           END-IF
           MOVE CA-BUS-DATE            TO WS-BUS-DATE.

      ******************************************************************
      * CMSRSV - reserve commission on one invoice item
       2000-RESERVE.
           PERFORM 3000-REQUIRE-ITEM-ID
           PERFORM 4000-READ-ITEM-CONTEXT
      * BR-BIL-CMS-005 Cancelled items are never reserved
           IF WS-IT-STATUS = 'CANC'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ITMCANC '          TO CA-REASON-CODE
              MOVE 'Cancelled invoice item cannot earn commission'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-CMS-001 Only premium earns commission
           IF WS-IT-PATTERN NOT = 'PREMIUM '
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOCOMMPT'          TO CA-REASON-CODE
              STRING 'Charge pattern ' WS-IT-PATTERN
                     ' does not earn commission'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-CMS-002 Requested producer, else the period's producer
           IF CA-CM-PRODUCER-ID > 0
              MOVE CA-CM-PRODUCER-ID   TO WS-PRODUCER-ID
           ELSE
              MOVE WS-IT-PRODUCER-ID   TO WS-PRODUCER-ID
           END-IF
           IF WS-PRODUCER-ID = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOPRODCR'          TO CA-REASON-CODE
              MOVE 'Direct business - no producer commission'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-PRODUCER
      * BR-BIL-CMS-003 Active producer on an active plan
           IF PRD-PRODUCER-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'PRDINACT'          TO CA-REASON-CODE
              STRING 'Producer status ' PRD-PRODUCER-STATUS
                     ' cannot accrue commission'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4110-READ-COMM-PLAN
           IF CPL-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CPLINACT'          TO CA-REASON-CODE
              STRING 'Commission plan ' CPL-PLAN-CODE
                     ' is not active' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4200-COUNT-LIVE-COMMISSION
           IF WS-LIVE-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'CMIEXIST'          TO CA-REASON-CODE
              MOVE 'Commission already reserved on this item'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4120-READ-RATE
           MOVE WS-IT-AMOUNT           TO WS-MN-TOTAL
           MOVE WS-RATE-BPS            TO WS-MN-RATE-BPS
           PERFORM 8100-APPLY-RATE
           MOVE WS-MN-RESULT           TO WS-COMM-AMOUNT
           MOVE WS-RATE-BPS            TO CA-CM-RATE-BPS
           IF WS-COMM-AMOUNT = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'COMMZERO'          TO CA-REASON-CODE
              MOVE 'Commission rounds to zero - nothing reserved'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 2010-CREATE-COMMISSION-ITEM
           MOVE WS-PRODUCER-ID         TO CA-CM-PRODUCER-ID
           MOVE CMI-COMM-AMOUNT        TO CA-CM-COMM-AMOUNT
           MOVE CMI-EARNED-AMOUNT      TO CA-CM-EARNED-AMOUNT
           PERFORM 4130-READ-PAYABLE
           MOVE PRD-PAYABLE-BALANCE    TO CA-CM-PAYABLE-BALANCE.

      ******************************************************************
       2010-CREATE-COMMISSION-ITEM.
           PERFORM 8000-NEXT-CI-ID
           MOVE SQA-SQ-VALUE           TO CMI-CI-ID
           MOVE WS-PRODUCER-ID         TO CMI-PRODUCER-ID
           MOVE CA-CM-ITEM-ID          TO CMI-ITEM-ID
           MOVE WS-IT-PP-ID            TO CMI-PP-ID
           MOVE WS-IT-AMOUNT           TO CMI-BASIS-AMOUNT
           MOVE WS-RATE-BPS            TO CMI-RATE-BPS
           MOVE WS-COMM-AMOUNT         TO CMI-COMM-AMOUNT
           MOVE 0                      TO CMI-PRODUCER-PAY-ID
      * BR-BIL-CMS-007 Return-premium chargeback is earned at once
           IF WS-COMM-AMOUNT < 0
              MOVE WS-COMM-AMOUNT      TO CMI-EARNED-AMOUNT
              MOVE 'EARN'              TO CMI-CI-STATE
           ELSE
              MOVE 0                   TO CMI-EARNED-AMOUNT
              MOVE 'RSRV'              TO CMI-CI-STATE
           END-IF
           PERFORM 4300-INSERT-COMMISSION
           PERFORM 4400-SET-ITEM-COMMISSION
           IF CMI-EARNED-AMOUNT NOT = 0
              MOVE CMI-EARNED-AMOUNT   TO WS-DELTA
              PERFORM 4500-ADJUST-PAYABLE
           END-IF
      * BR-BIL-CMS-006 DR COMMRSV / CR COMMPAY, reversed for credits
           IF WS-COMM-AMOUNT > 0
              MOVE WS-COMM-AMOUNT      TO WS-GL-AMOUNT
              MOVE 'COMMRSV '          TO WS-GL-DR-ACCT
              MOVE 'COMMPAY '          TO WS-GL-CR-ACCT
           ELSE
              COMPUTE WS-GL-AMOUNT = 0 - WS-COMM-AMOUNT
              MOVE 'COMMPAY '          TO WS-GL-DR-ACCT
              MOVE 'COMMRSV '          TO WS-GL-CR-ACCT
           END-IF
           MOVE 'COMMRSV '             TO WS-GL-TXN-TYPE
           PERFORM 8200-POST-LEDGER.

      ******************************************************************
      * CMSERN - earn commission for a payment applied to the item
       2100-EARN.
           PERFORM 3000-REQUIRE-ITEM-ID
           IF CA-CM-PAID-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAIDAMT '          TO CA-REASON-CODE
              MOVE 'Paid amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4210-READ-LIVE-COMMISSION
           IF CMI-CI-STATE = 'EARN'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'CMIEARND'          TO CA-REASON-CODE
              MOVE 'Commission on this item is already fully earned'
                                       TO CA-MESSAGE
              PERFORM 2190-RETURN-CI-AMOUNTS
              PERFORM 9000-RETURN
           END-IF
           MOVE CMI-PRODUCER-ID        TO WS-PRODUCER-ID
           PERFORM 4100-READ-PRODUCER
           PERFORM 4110-READ-COMM-PLAN
      * BR-BIL-CMS-008 Only earn basis P earns on payment
           IF CPL-EARN-BASIS NOT = 'P'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'EARNBASS'          TO CA-REASON-CODE
              STRING 'Plan ' CPL-PLAN-CODE ' earns on basis '
                     CPL-EARN-BASIS ' not on payment'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 2190-RETURN-CI-AMOUNTS
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4000-READ-ITEM-CONTEXT
           MOVE CA-CM-PAID-AMOUNT      TO WS-MN-TOTAL
           MOVE CMI-RATE-BPS           TO WS-MN-RATE-BPS
           PERFORM 8100-APPLY-RATE
           MOVE WS-MN-RESULT           TO WS-DELTA
           MOVE CMI-EARNED-AMOUNT      TO WS-OLD-EARNED
           MOVE CMI-CI-STATE           TO WS-OLD-STATE
           MOVE 'RSRV'                 TO WS-NEW-STATE
      * BR-BIL-CMS-009 Fully earned: take the remainder, state EARN
           IF WS-OLD-EARNED + WS-DELTA >= CMI-COMM-AMOUNT
              OR WS-IT-SETTLED >= WS-IT-AMOUNT
              COMPUTE WS-DELTA = CMI-COMM-AMOUNT - WS-OLD-EARNED
              MOVE 'EARN'              TO WS-NEW-STATE
           END-IF
           COMPUTE CMI-EARNED-AMOUNT = WS-OLD-EARNED + WS-DELTA
           MOVE WS-NEW-STATE           TO CMI-CI-STATE
           PERFORM 4600-UPDATE-COMMISSION
      * BR-BIL-CMS-010 Earned commission becomes payable
           IF WS-DELTA NOT = 0
              PERFORM 4500-ADJUST-PAYABLE
           END-IF
           PERFORM 2190-RETURN-CI-AMOUNTS
           PERFORM 4130-READ-PAYABLE
           MOVE PRD-PAYABLE-BALANCE    TO CA-CM-PAYABLE-BALANCE.

      ******************************************************************
       2190-RETURN-CI-AMOUNTS.
           MOVE CMI-PRODUCER-ID        TO CA-CM-PRODUCER-ID
           MOVE CMI-RATE-BPS           TO CA-CM-RATE-BPS
           MOVE CMI-COMM-AMOUNT        TO CA-CM-COMM-AMOUNT
           MOVE CMI-EARNED-AMOUNT      TO CA-CM-EARNED-AMOUNT.

      ******************************************************************
      * CMSRVS - un-earn for a reversed payment, or release on cancel
       2200-REVERSE.
           PERFORM 3000-REQUIRE-ITEM-ID
           PERFORM 4210-READ-LIVE-COMMISSION
           PERFORM 4000-READ-ITEM-CONTEXT
           MOVE CMI-PRODUCER-ID        TO WS-PRODUCER-ID
           MOVE CMI-EARNED-AMOUNT      TO WS-OLD-EARNED
           MOVE CMI-CI-STATE           TO WS-OLD-STATE
           IF WS-IT-STATUS = 'CANC'
              PERFORM 2210-RELEASE-COMMISSION
           ELSE
              PERFORM 2220-UNEARN-COMMISSION
           END-IF
           PERFORM 2190-RETURN-CI-AMOUNTS
           PERFORM 4130-READ-PAYABLE
           MOVE PRD-PAYABLE-BALANCE    TO CA-CM-PAYABLE-BALANCE.

      ******************************************************************
      * BR-BIL-CMS-012 Item cancelled: release the whole commission
      * BR-BIL-CMS-013 Paid-out commission is still recovered
       2210-RELEASE-COMMISSION.
           COMPUTE WS-DELTA = 0 - WS-OLD-EARNED
           MOVE 'CANC'                 TO CMI-CI-STATE
           MOVE 0                      TO CMI-EARNED-AMOUNT
           PERFORM 4600-UPDATE-COMMISSION
           MOVE 0                      TO WS-COMM-AMOUNT
           PERFORM 4400-SET-ITEM-COMMISSION
           IF WS-DELTA NOT = 0
              PERFORM 4500-ADJUST-PAYABLE
           END-IF
           IF CMI-COMM-AMOUNT > 0
              MOVE CMI-COMM-AMOUNT     TO WS-GL-AMOUNT
              MOVE 'COMMPAY '          TO WS-GL-DR-ACCT
              MOVE 'COMMRSV '          TO WS-GL-CR-ACCT
           ELSE
              COMPUTE WS-GL-AMOUNT = 0 - CMI-COMM-AMOUNT
              MOVE 'COMMRSV '          TO WS-GL-DR-ACCT
              MOVE 'COMMPAY '          TO WS-GL-CR-ACCT
           END-IF
           MOVE 'COMMRLS '             TO WS-GL-TXN-TYPE
           PERFORM 8200-POST-LEDGER.

      ******************************************************************
      * BR-BIL-CMS-011 Reversed payment un-earns rate x amount
       2220-UNEARN-COMMISSION.
           IF CA-CM-REVERSED-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'REVAMT  '          TO CA-REASON-CODE
              MOVE 'Reversed amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CMI-COMM-AMOUNT < 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'CMICHGBK'          TO CA-REASON-CODE
              MOVE 'Chargeback commission is not payment driven'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-CM-REVERSED-AMOUNT  TO WS-MN-TOTAL
           MOVE CMI-RATE-BPS           TO WS-MN-RATE-BPS
           PERFORM 8100-APPLY-RATE
           MOVE WS-MN-RESULT           TO WS-DELTA
           IF WS-DELTA > WS-OLD-EARNED
              MOVE WS-OLD-EARNED       TO WS-DELTA
           END-IF
           IF WS-DELTA = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOTEARND'          TO CA-REASON-CODE
              MOVE 'No earned commission to reverse'
                                       TO CA-MESSAGE
              PERFORM 2190-RETURN-CI-AMOUNTS
              PERFORM 9000-RETURN
           END-IF
           COMPUTE CMI-EARNED-AMOUNT = WS-OLD-EARNED - WS-DELTA
           MOVE 'RSRV'                 TO CMI-CI-STATE
           PERFORM 4600-UPDATE-COMMISSION
           COMPUTE WS-DELTA = 0 - WS-DELTA
           PERFORM 4500-ADJUST-PAYABLE.

      ******************************************************************
      * CMSINQ - producer balance with reserved and earned totals
       2300-INQUIRE.
           IF CA-CM-PRODUCER-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDREQ  '          TO CA-REASON-CODE
              MOVE 'Producer id is required for commission inquiry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-CM-PRODUCER-ID      TO WS-PRODUCER-ID
           PERFORM 4100-READ-PRODUCER
           PERFORM 4110-READ-COMM-PLAN
           MOVE CPL-DEFAULT-RATE-BPS   TO CA-CM-RATE-BPS
           MOVE PRD-PAYABLE-BALANCE    TO CA-CM-PAYABLE-BALANCE
           PERFORM 4700-SUMMARISE-PRODUCER
           MOVE WS-UNEARNED            TO CA-CM-COMM-AMOUNT
           MOVE WS-UNPAID-EARNED       TO CA-CM-EARNED-AMOUNT.

      ******************************************************************
       3000-REQUIRE-ITEM-ID.
           IF CA-CM-ITEM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ITEMREQ '          TO CA-REASON-CODE
              MOVE 'Invoice item id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-ITEM-CONTEXT.
           MOVE CA-CM-ITEM-ID          TO ITM-ITEM-ID
           EXEC SQL
              SELECT I.ACCOUNT_ID, I.PP_ID, I.ITEM_AMOUNT,
                     I.PAID_AMOUNT + I.WRITEOFF_AMOUNT,
                     I.ITEM_STATUS, C.CHARGE_PATTERN,
                     P.PRODUCT_CODE, P.PRODUCER_ID
                INTO :WS-IT-ACCOUNT-ID, :WS-IT-PP-ID,
                     :WS-IT-AMOUNT, :WS-IT-SETTLED,
                     :WS-IT-STATUS, :WS-IT-PATTERN,
                     :WS-IT-PRODUCT, :WS-IT-PRODUCER-ID
                FROM BILDB.BIL_INVOICE_ITEM I
                JOIN BILDB.BIL_CHARGE C
                  ON C.CHARGE_ID = I.CHARGE_ID
                JOIN BILDB.BIL_POLICY_PERIOD P
                  ON P.PP_ID = I.PP_ID
               WHERE I.ITEM_ID = :ITM-ITEM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ITMNOTFD'       TO CA-REASON-CODE
                 MOVE ITM-ITEM-ID      TO WS-ID-DISP
                 STRING 'Invoice item ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE ITM-ITEM-ID      TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4000-READ-ITEM-CONTEXT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-READ-PRODUCER.
           MOVE WS-PRODUCER-ID         TO PRD-PRODUCER-ID
           EXEC SQL
              SELECT COMM_PLAN_CODE, PAYABLE_BALANCE,
                     PRODUCER_STATUS
                INTO :PRD-COMM-PLAN-CODE, :PRD-PAYABLE-BALANCE,
                     :PRD-PRODUCER-STATUS
                FROM BILDB.BIL_PRODUCER
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PRDNOTFD'       TO CA-REASON-CODE
                 MOVE PRD-PRODUCER-ID  TO WS-ID-DISP
                 STRING 'Producer ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE PRD-PRODUCER-ID  TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4100-READ-PRODUCER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4110-READ-COMM-PLAN.
           MOVE PRD-COMM-PLAN-CODE     TO CPL-PLAN-CODE
           EXEC SQL
              SELECT DEFAULT_RATE_BPS, EARN_BASIS, ACTIVE_FLAG
                INTO :CPL-DEFAULT-RATE-BPS, :CPL-EARN-BASIS,
                     :CPL-ACTIVE-FLAG
                FROM BILDB.BIL_COMMISSION_PLAN
               WHERE PLAN_CODE = :CPL-PLAN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'CPLNOTFD'       TO CA-REASON-CODE
                 STRING 'Commission plan ' CPL-PLAN-CODE
                        ' not found' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE CPL-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '4110-READ-COMM-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-CMS-004 Product-specific rate, else plan default
       4120-READ-RATE.
           MOVE CPL-PLAN-CODE          TO CRT-PLAN-CODE
           MOVE WS-IT-PRODUCT          TO CRT-PRODUCT-CODE
           MOVE 'PREMIUM '             TO CRT-CHARGE-PATTERN
           EXEC SQL
              SELECT RATE_BPS
                INTO :CRT-RATE-BPS
                FROM BILDB.BIL_COMMISSION_RATE
               WHERE PLAN_CODE      = :CRT-PLAN-CODE
                 AND PRODUCT_CODE   = :CRT-PRODUCT-CODE
                 AND CHARGE_PATTERN = :CRT-CHARGE-PATTERN
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE CRT-RATE-BPS     TO WS-RATE-BPS
              WHEN 100
                 MOVE CPL-DEFAULT-RATE-BPS TO WS-RATE-BPS
              WHEN OTHER
                 MOVE CRT-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '4120-READ-RATE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-RATE-BPS < 0 OR WS-RATE-BPS > 5000
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CMSRATE '          TO CA-REASON-CODE
              STRING 'Commission rate out of range on plan '
                     CPL-PLAN-CODE DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4130-READ-PAYABLE.
           MOVE WS-PRODUCER-ID         TO PRD-PRODUCER-ID
           EXEC SQL
              SELECT PAYABLE_BALANCE
                INTO :PRD-PAYABLE-BALANCE
                FROM BILDB.BIL_PRODUCER
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PRD-PRODUCER-ID     TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4130-READ-PAYABLE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-CMS-005 One live (RSRV/EARN) commission item per item
       4200-COUNT-LIVE-COMMISSION.
           MOVE CA-CM-ITEM-ID          TO CMI-ITEM-ID
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-LIVE-COUNT
                FROM BILDB.BIL_COMMISSION_ITEM
               WHERE ITEM_ID   = :CMI-ITEM-ID
                 AND CI_STATE IN ('RSRV', 'EARN')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CMI-ITEM-ID         TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4200-COUNT-LIVE-COMMISSION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4210-READ-LIVE-COMMISSION.
           MOVE CA-CM-ITEM-ID          TO CMI-ITEM-ID
           EXEC SQL
              SELECT CI_ID, PRODUCER_ID, PP_ID, BASIS_AMOUNT,
                     RATE_BPS, COMM_AMOUNT, EARNED_AMOUNT, CI_STATE,
                     PRODUCER_PAY_ID
                INTO :CMI-CI-ID, :CMI-PRODUCER-ID, :CMI-PP-ID,
                     :CMI-BASIS-AMOUNT, :CMI-RATE-BPS,
                     :CMI-COMM-AMOUNT, :CMI-EARNED-AMOUNT,
                     :CMI-CI-STATE, :CMI-PRODUCER-PAY-ID
                FROM BILDB.BIL_COMMISSION_ITEM
               WHERE ITEM_ID   = :CMI-ITEM-ID
                 AND CI_STATE IN ('RSRV', 'EARN')
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'NOCOMMIT'       TO CA-REASON-CODE
                 MOVE 'Item carries no live commission'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE CMI-ITEM-ID      TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4210-READ-LIVE-COMMISSION' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-INSERT-COMMISSION.
           EXEC SQL
              INSERT INTO BILDB.BIL_COMMISSION_ITEM
                     (CI_ID, PRODUCER_ID, ITEM_ID, PP_ID,
                      BASIS_AMOUNT, RATE_BPS, COMM_AMOUNT,
                      EARNED_AMOUNT, CI_STATE, PRODUCER_PAY_ID,
                      UPDATE_TS)
              VALUES (:CMI-CI-ID, :CMI-PRODUCER-ID, :CMI-ITEM-ID,
                      :CMI-PP-ID, :CMI-BASIS-AMOUNT, :CMI-RATE-BPS,
                      :CMI-COMM-AMOUNT, :CMI-EARNED-AMOUNT,
                      :CMI-CI-STATE, :CMI-PRODUCER-PAY-ID,
                      CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CMI-CI-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4300-INSERT-COMMISSION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-SET-ITEM-COMMISSION.
           MOVE CA-CM-ITEM-ID          TO ITM-ITEM-ID
           MOVE WS-COMM-AMOUNT         TO ITM-COMMISSION-AMOUNT
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET COMMISSION_AMOUNT = :ITM-COMMISSION-AMOUNT,
                     UPDATE_TS         = CURRENT TIMESTAMP
               WHERE ITEM_ID = :ITM-ITEM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ITM-ITEM-ID         TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4400-SET-ITEM-COMMISSION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Relative update: concurrent earnings for one producer add up
      * without lost updates (no VERSION_NO on BIL_PRODUCER)
       4500-ADJUST-PAYABLE.
           MOVE WS-PRODUCER-ID         TO PRD-PRODUCER-ID
           EXEC SQL
              UPDATE BILDB.BIL_PRODUCER
                 SET PAYABLE_BALANCE = PAYABLE_BALANCE + :WS-DELTA,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PRD-PRODUCER-ID     TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4500-ADJUST-PAYABLE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-CMS-014 Update only if state and earned are unchanged
       4600-UPDATE-COMMISSION.
           EXEC SQL
              UPDATE BILDB.BIL_COMMISSION_ITEM
                 SET EARNED_AMOUNT = :CMI-EARNED-AMOUNT,
                     CI_STATE      = :CMI-CI-STATE,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE CI_ID         = :CMI-CI-ID
                 AND CI_STATE      = :WS-OLD-STATE
                 AND EARNED_AMOUNT = :WS-OLD-EARNED
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CMI-CI-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4600-UPDATE-COMMISSION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Commission item changed by another task - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4700-SUMMARISE-PRODUCER.
           MOVE WS-PRODUCER-ID         TO CMI-PRODUCER-ID
           EXEC SQL
              SELECT COALESCE(SUM(CASE WHEN CI_STATE = 'RSRV'
                                       THEN 1 ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN CI_STATE = 'EARN'
                                       THEN 1 ELSE 0 END), 0),
                     COALESCE(SUM(COMM_AMOUNT - EARNED_AMOUNT), 0),
                     COALESCE(SUM(CASE WHEN PRODUCER_PAY_ID = 0
                                       THEN EARNED_AMOUNT
                                       ELSE 0 END), 0)
                INTO :CA-CM-RESERVED-COUNT, :CA-CM-EARNED-COUNT,
                     :WS-UNEARNED, :WS-UNPAID-EARNED
                FROM BILDB.BIL_COMMISSION_ITEM
               WHERE PRODUCER_ID = :CMI-PRODUCER-ID
                 AND CI_STATE IN ('RSRV', 'EARN')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CMI-PRODUCER-ID     TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4700-SUMMARISE-PRODUCER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4900-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CTL-CONTROL-KEY     TO WS-ERR-KEY
              MOVE '4900-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       8000-NEXT-CI-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'CMI     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE SQA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE SQA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * MNRATE: WS-MN-RESULT = WS-MN-TOTAL x WS-MN-RATE-BPS / 10000
       8100-APPLY-RATE.
           INITIALIZE WS-MNY-CA
           MOVE WS-MN-TOTAL            TO MNA-MN-TOTAL
           MOVE WS-MN-RATE-BPS         TO MNA-MN-RATE-BPS
           MOVE 'MNRATE  '             TO MNA-REQUEST-ID
           MOVE CA-USER-ID             TO MNA-USER-ID
           MOVE WS-BUS-DATE            TO MNA-BUS-DATE
           MOVE CA-TRACE-ID            TO MNA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO MNA-CALLER-PGM
           CALL WS-MNY-PGM USING WS-MNY-CA
           MOVE MNA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE MNA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE MNA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE
           MOVE MNA-MN-RESULT          TO WS-MN-RESULT.

      ******************************************************************
      * Two-line posting of WS-GL-AMOUNT: debit WS-GL-DR-ACCT, credit
      * WS-GL-CR-ACCT under transaction type WS-GL-TXN-TYPE
       8200-POST-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE CA-TRACE-ID            TO GLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE WS-IT-ACCOUNT-ID       TO GLA-GL-ACCOUNT-ID
           MOVE WS-PRODUCER-ID         TO GLA-GL-PRODUCER-ID
           MOVE WS-GL-TXN-TYPE         TO GLA-GL-TXN-TYPE
           MOVE 'CMI '                 TO GLA-GL-REF-TYPE
           MOVE CMI-CI-ID              TO GLA-GL-REF-ID
           MOVE 0                      TO GLA-GL-REVERSAL-OF
           MOVE 2                      TO GLA-GL-LINE-COUNT
           MOVE WS-GL-DR-ACCT          TO GLA-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLA-GL-DR-CR(1)
           MOVE WS-GL-CR-ACCT          TO GLA-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLA-GL-DR-CR(2)
           MOVE WS-GL-AMOUNT           TO GLA-GL-AMOUNT(1)
                                          GLA-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           MOVE GLA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE GLA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE GLA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Callee RC 08 here is an internal inconsistency; RC >= 12 is
      * passed up unchanged with its reason code
       8900-CHECK-CALLEE.
           IF WS-CALLEE-RC >= 08
              MOVE WS-CALLEE-RC        TO CA-RETURN-CODE
              MOVE WS-CALLEE-REASON    TO CA-REASON-CODE
              MOVE WS-CALLEE-MESSAGE   TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           EVALUATE SQLCODE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 STRING 'Duplicate key ' WS-ERR-KEY
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Commission data locked - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DB2ERR  '       TO CA-REASON-CODE
                 STRING 'BILCOMSV Db2 error SQLCODE ' WS-SQLCODE-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 INITIALIZE ERC-HEADER
                 MOVE 'ERRLOG  '       TO ERC-REQUEST-ID
                 MOVE CA-USER-ID       TO ERC-USER-ID
                 MOVE WS-PROGRAM-ID    TO ERC-CALLER-PGM
                 MOVE WS-PROGRAM-ID    TO ERC-ER-PROGRAM
                 MOVE WS-ERR-PARA      TO ERC-ER-PARAGRAPH
                 MOVE SQLCODE          TO ERC-ER-SQLCODE
                 MOVE CA-MESSAGE       TO ERC-ER-TEXT
                 MOVE WS-ERR-KEY       TO ERC-ER-KEY
                 CALL WS-ERR-PGM USING WS-ERR-CA
           END-EVALUATE
           PERFORM 9000-RETURN.
