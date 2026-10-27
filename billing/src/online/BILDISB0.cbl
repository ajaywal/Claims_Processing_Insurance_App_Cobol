       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDISB0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDISB0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Outgoing money. Creates, approves, voids and     *
      *               inquires disbursements: account overpayment and  *
      *               refund cheques/BACS credits, claim payments      *
      *               raised by ClaimsCore and producer commission     *
      *               payments. Every create reserves the funding      *
      *               (account unapplied cash, claims or commission    *
      *               payable) against DISBPAY in the ledger; a void   *
      *               reverses that posting and restores the funding.  *
      * REQUESTS    : DSBCRT DSBAPR DSBVOID DSBINQ                     *
      * CALLED BY   : BILDSBOV BILCOMPY BILEVTDR, disbursement screens *
      * CALLS       : BILSEQGN BILAUTHZ BILHOLD0 BILGLPST BILAUDIT     *
      *               BILDATES BILERRLG                                *
      * TABLES      : BIL_DISBURSEMENT       (C R U)                   *
      *               BIL_ACCOUNT            (R U)                     *
      *               BIL_PAYMENT_INSTRUMENT (R)                       *
      *               BIL_PAYMENT_RECEIPT    (R U)                     *
      *               BIL_TRANSACTION        (R)                       *
      *               BIL_PRODUCER_PAYMENT   (R U)                     *
      *               BIL_PRODUCER           (U)                       *
      *               BIL_COMMISSION_ITEM    (U)                       *
      *               BIL_CONTROL            (R)                       *
      * RULES       : BR-BIL-DSB-001 Reason must be OVERPAY CLMPAY     *
      *                              COMMPAY or REFUND                 *
      *               BR-BIL-DSB-002 Payee name is mandatory; cheques  *
      *                              also need address and postcode    *
      *               BR-BIL-DSB-003 Reason fixes the source system:   *
      *                              CLMPAY=CLMS COMMPAY=COMM          *
      *                              OVERPAY/REFUND=BILL with account  *
      *               BR-BIL-DSB-004 SOURCE_SYSTEM + SOURCE_REF is     *
      *                              unique among non-void rows        *
      *               BR-BIL-DSB-005 An active DISB hold blocks        *
      *                              BILL-sourced disbursements        *
      *               BR-BIL-DSB-006 Account-funded disbursements      *
      *                              cannot exceed UNAPPLIED_AMT and   *
      *                              reduce it on create               *
      *               BR-BIL-DSB-007 Within DISBURSE authority -> APPR,*
      *                              otherwise PEND routed to approver *
      *               BR-BIL-DSB-008 Ledger DR funding account (UNAPPLD*
      *                              CLMPAY COMMPAY) / CR DISBPAY      *
      *               BR-BIL-DSB-009 BACS for BILL source needs an     *
      *                              active instrument on the account  *
      *               BR-BIL-DSB-010 Due date valid, not before the    *
      *                              business date; defaults to it     *
      *               BR-BIL-DSB-011 Only PEND can be approved; the    *
      *                              requester cannot approve own      *
      *               BR-BIL-DSB-012 Approver needs DISBURSE authority *
      *                              covering the amount               *
      *               BR-BIL-DSB-013 Only PEND or APPR can be voided   *
      *                              and a void reason is required     *
      *               BR-BIL-DSB-014 Void reverses the create posting  *
      *                              and restores account unapplied    *
      *               BR-BIL-DSB-015 Void of a COMMPAY reopens the     *
      *                              producer payment: items back to   *
      *                              EARN, payable balance restored    *
      *               BR-BIL-DSB-017 COMMPAY source ref must be an     *
      *                              existing producer payment; its    *
      *                              producer owns the COMMPAY posting *
      *               BR-BIL-DSB-016 Receipt unapplied is consumed     *
      *                              oldest first and restored newest  *
      *                              first so ACCTBAL stays in step    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDISB0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-AUT-PGM                  PIC X(8)  VALUE 'BILAUTHZ'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-REASON                PIC X(8).
              88 WS-RSN-VALID           VALUES 'OVERPAY ' 'CLMPAY  '
                                               'COMMPAY ' 'REFUND  '.
              88 WS-RSN-ACCT-FUNDED     VALUES 'OVERPAY ' 'REFUND  '.
              88 WS-RSN-CLMPAY          VALUE  'CLMPAY  '.
              88 WS-RSN-COMMPAY         VALUE  'COMMPAY '.
           05 WS-SOURCE                PIC X(4).
              88 WS-SRC-VALID           VALUES 'BILL' 'CLMS' 'COMM'.
              88 WS-SRC-BILL            VALUE  'BILL'.
           05 WS-METHOD                PIC X(4).
              88 WS-MTH-VALID           VALUES 'CHEQ' 'BACS'.
              88 WS-MTH-CHEQUE          VALUE  'CHEQ'.
              88 WS-MTH-BACS            VALUE  'BACS'.
           05 WS-OLD-STATUS            PIC X(4).
           05 WS-NEW-STATUS            PIC X(4).
           05 WS-APPROVER              PIC X(8).
           05 WS-FUND-T-ACCOUNT        PIC X(8).
           05 WS-ORIG-TXN-ID           PIC S9(9) COMP VALUE 0.
           05 WS-PRODUCER-ID           PIC S9(9) COMP VALUE 0.
           05 WS-DUP-COUNT             PIC S9(9) COMP VALUE 0.
           05 WS-REMAINING             PIC S9(11)V99 COMP-3.
           05 WS-TAKE                  PIC S9(11)V99 COMP-3.
           05 WS-RCP-EOF               PIC X     VALUE 'N'.
              88 WS-RCP-END             VALUE 'Y'.
           05 WS-OPEN-CURSOR           PIC X(3)  VALUE SPACES.
              88 WS-OLD-CSR-OPEN        VALUE 'OLD'.
              88 WS-NEW-CSR-OPEN        VALUE 'NEW'.
           05 WS-KEY-NUM               PIC 9(10).
           05 WS-REF-NUM               PIC 9(10).
           05 WS-AMOUNT-DISP           PIC -(10)9.99.
           05 WS-LIMIT-DISP            PIC -(10)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-AUD-ACTION            PIC X(4).
           05 WS-CALLEE-RC             PIC 9(2).
           05 WS-CALLEE-REASON         PIC X(8).
           05 WS-CALLEE-MESSAGE        PIC X(80).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDDSB END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDPIN END-EXEC.
           EXEC SQL INCLUDE BILDTXN END-EXEC.
           EXEC SQL INCLUDE BILDPPY END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE BILDRCP END-EXEC.

      * Receipts holding unapplied cash, oldest first (consume)
           EXEC SQL DECLARE RCP-OLD-CSR CURSOR FOR
              SELECT MONEY_ID, UNAPPLIED_AMOUNT
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE ACCOUNT_ID       = :ACT-ACCOUNT-ID
                 AND UNAPPLIED_AMOUNT > 0
                 AND RECEIPT_STATUS  <> 'RVSD'
               ORDER BY RECEIVED_DATE, MONEY_ID
               FOR FETCH ONLY
           END-EXEC.
      * Receipts with headroom to take cash back, newest first
           EXEC SQL DECLARE RCP-NEW-CSR CURSOR FOR
              SELECT MONEY_ID, RECEIPT_AMOUNT - UNAPPLIED_AMOUNT
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE ACCOUNT_ID       = :ACT-ACCOUNT-ID
                 AND UNAPPLIED_AMOUNT < RECEIPT_AMOUNT
                 AND RECEIPT_STATUS  <> 'RVSD'
               ORDER BY RECEIVED_DATE DESC, MONEY_ID DESC
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.
       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-AUT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ATA-==.
           03 ATA-BODY.
              COPY BILCAUT REPLACING LEADING ==CA-== BY ==ATA-==.
       01  WS-HLD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==HLA-==.
           03 HLA-BODY.
              COPY BILCHLD REPLACING LEADING ==CA-== BY ==HLA-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.
       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCDSB.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'DSBCRT  '
                 PERFORM 2000-CREATE-DISBURSEMENT
              WHEN 'DSBAPR  '
                 PERFORM 2100-APPROVE-DISBURSEMENT
              WHEN 'DSBVOID '
                 PERFORM 2200-VOID-DISBURSEMENT
              WHEN 'DSBINQ  '
                 PERFORM 2300-INQUIRE-DISBURSEMENT
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILDISB0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE CA-DB-DISB-ID          TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-BUSINESS-DATE
           ELSE
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * DSBCRT - validate, reserve funding, decide status by authority,
      * insert, post the ledger and audit.
       2000-CREATE-DISBURSEMENT.
           PERFORM 3000-VALIDATE-CREATE
           PERFORM 3100-CHECK-DUPLICATE-SOURCE
           IF CA-DB-ACCOUNT-ID > 0
              PERFORM 4000-READ-ACCOUNT
           END-IF
           PERFORM 3200-CHECK-FUNDING
           PERFORM 3300-CHECK-PAY-INSTRUMENT
           IF WS-RSN-COMMPAY
              MOVE CA-DB-SOURCE-REF    TO DSB-SOURCE-REF
              PERFORM 4800-READ-PRODUCER-PAYMENT
           END-IF
      * BR-BIL-DSB-005 DISB hold blocks BILL-sourced disbursements
           IF WS-SRC-BILL
              PERFORM 8100-CHECK-DISB-HOLD
           END-IF
           MOVE CA-USER-ID             TO ATA-AT-USER
           PERFORM 8200-CHECK-AUTHORITY
      * BR-BIL-DSB-007 Within authority -> APPR, else PEND + approver
           IF ATA-AT-IS-ALLOWED
              MOVE 'APPR'              TO WS-NEW-STATUS
              MOVE CA-USER-ID          TO WS-APPROVER
           ELSE
              MOVE 'PEND'              TO WS-NEW-STATUS
              MOVE ATA-AT-APPROVER     TO WS-APPROVER
           END-IF
           PERFORM 8000-NEXT-DISB-ID
           PERFORM 4100-INSERT-DISBURSEMENT
           IF WS-RSN-ACCT-FUNDED
              PERFORM 4200-REDUCE-UNAPPLIED
              PERFORM 4250-CONSUME-RECEIPTS
           END-IF
           PERFORM 5000-SET-FUNDING-T-ACCOUNT
           PERFORM 8300-POST-CREATE-LEDGER
           MOVE 'CRT '                 TO WS-AUD-ACTION
           MOVE SPACES                 TO WS-OLD-STATUS
           PERFORM 8400-WRITE-AUDIT
           MOVE DSB-DISB-ID            TO CA-DB-DISB-ID
           MOVE WS-NEW-STATUS          TO CA-DB-STATUS
           MOVE WS-APPROVER            TO CA-DB-APPROVER
           MOVE WS-METHOD              TO CA-DB-PAY-METHOD
           MOVE DSB-DUE-DATE           TO CA-DB-DUE-DATE
           IF WS-NEW-STATUS = 'PEND'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'DSBPEND '          TO CA-REASON-CODE
              STRING 'Disbursement pending approval by '
                     WS-APPROVER DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              MOVE 'Disbursement created and approved'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * DSBAPR - PEND -> APPR by a user with sufficient authority who
      * did not raise the request.
       2100-APPROVE-DISBURSEMENT.
           PERFORM 4300-READ-DISBURSEMENT
      * BR-BIL-DSB-011 Only PEND can be approved, never by requester
           IF DSB-DISB-STATUS NOT = 'PEND'
              SET CA-RC-STATE          TO TRUE
              MOVE 'DSBSTATE'          TO CA-REASON-CODE
              STRING 'Disbursement is ' DSB-DISB-STATUS
                     ' - only PEND can be approved'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF DSB-REQUEST-USER = CA-USER-ID
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'DSBSELF '          TO CA-REASON-CODE
              MOVE 'Requester cannot approve own disbursement'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE DSB-DISB-AMOUNT        TO CA-DB-AMOUNT
           MOVE DSB-SOURCE-SYSTEM      TO WS-SOURCE
           MOVE DSB-ACCOUNT-ID         TO CA-DB-ACCOUNT-ID
      * BR-BIL-DSB-005 Hold is re-checked at approval time
           IF WS-SRC-BILL AND DSB-ACCOUNT-ID > 0
              PERFORM 8100-CHECK-DISB-HOLD
           END-IF
      * BR-BIL-DSB-012 Approver must hold DISBURSE authority
           MOVE CA-USER-ID             TO ATA-AT-USER
           PERFORM 8200-CHECK-AUTHORITY
           IF NOT ATA-AT-IS-ALLOWED
              MOVE ATA-AT-LIMIT        TO WS-LIMIT-DISP
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'AUTHEXC '          TO CA-REASON-CODE
              STRING 'Approval exceeds your DISBURSE limit '
                     WS-LIMIT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'PEND'                 TO WS-OLD-STATUS
           MOVE 'APPR'                 TO WS-NEW-STATUS
           MOVE CA-USER-ID             TO WS-APPROVER
           PERFORM 4400-UPDATE-STATUS
           MOVE 'APPR'                 TO WS-AUD-ACTION
           PERFORM 8400-WRITE-AUDIT
           PERFORM 2310-RETURN-DISBURSEMENT
           MOVE 'Disbursement approved' TO CA-MESSAGE.

      ******************************************************************
      * DSBVOID - cancel a disbursement not yet sent: reverse ledger,
      * give the funding back.
       2200-VOID-DISBURSEMENT.
      * BR-BIL-DSB-013 Void reason mandatory; PEND/APPR only
           IF CA-DB-VOID-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBVRSN '          TO CA-REASON-CODE
              MOVE 'Void reason is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4300-READ-DISBURSEMENT
           IF DSB-DISB-STATUS NOT = 'PEND' AND
              DSB-DISB-STATUS NOT = 'APPR'
              SET CA-RC-STATE          TO TRUE
              MOVE 'DSBSTATE'          TO CA-REASON-CODE
              STRING 'Disbursement is ' DSB-DISB-STATUS
                     ' - only PEND or APPR can be voided'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE DSB-DISB-STATUS        TO WS-OLD-STATUS
           MOVE 'VOID'                 TO WS-NEW-STATUS
           MOVE DSB-APPROVER-USER      TO WS-APPROVER
           MOVE DSB-DISB-REASON        TO WS-REASON
           PERFORM 4400-UPDATE-STATUS
      * BR-BIL-DSB-014 Reverse the create posting, restore unapplied
           PERFORM 4500-FIND-CREATE-TXN
           PERFORM 8310-REVERSE-CREATE-LEDGER
           IF WS-RSN-ACCT-FUNDED AND DSB-ACCOUNT-ID > 0
              MOVE DSB-ACCOUNT-ID      TO CA-DB-ACCOUNT-ID
              PERFORM 4000-READ-ACCOUNT
              PERFORM 4600-RESTORE-UNAPPLIED
              PERFORM 4650-RESTORE-RECEIPTS
           END-IF
      * BR-BIL-DSB-015 Void of COMMPAY reopens the producer payment
           IF WS-RSN-COMMPAY
              PERFORM 4700-REOPEN-PRODUCER-PAYMENT
           END-IF
           MOVE 'VOID'                 TO WS-AUD-ACTION
           PERFORM 8400-WRITE-AUDIT
           PERFORM 2310-RETURN-DISBURSEMENT
           MOVE 'Disbursement voided'  TO CA-MESSAGE.

      ******************************************************************
      * DSBINQ - by DISB-ID, or by SOURCE-SYSTEM + SOURCE-REF.
       2300-INQUIRE-DISBURSEMENT.
           IF CA-DB-DISB-ID = 0
              IF CA-DB-SOURCE-SYSTEM = SPACES OR
                 CA-DB-SOURCE-REF = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DSBKEY  '       TO CA-REASON-CODE
                 MOVE 'Disbursement id or source system/ref required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              PERFORM 4310-READ-BY-SOURCE
           ELSE
              PERFORM 4300-READ-DISBURSEMENT
           END-IF
           PERFORM 2310-RETURN-DISBURSEMENT.

       2310-RETURN-DISBURSEMENT.
           MOVE DSB-DISB-ID            TO CA-DB-DISB-ID
           MOVE DSB-ACCOUNT-ID         TO CA-DB-ACCOUNT-ID
           MOVE DSB-DISB-AMOUNT        TO CA-DB-AMOUNT
           MOVE DSB-DISB-REASON        TO CA-DB-REASON
           MOVE DSB-PAYEE-NAME-TEXT    TO CA-DB-PAYEE-NAME
           IF DSB-PAYEE-ADDR1-IND < 0
              MOVE SPACES              TO CA-DB-PAYEE-ADDR1
           ELSE
              MOVE DSB-PAYEE-ADDR1-TEXT TO CA-DB-PAYEE-ADDR1
           END-IF
           IF DSB-PAYEE-POSTCODE-IND < 0
              MOVE SPACES              TO CA-DB-PAYEE-POSTCODE
           ELSE
              MOVE DSB-PAYEE-POSTCODE  TO CA-DB-PAYEE-POSTCODE
           END-IF
           MOVE DSB-PAY-METHOD         TO CA-DB-PAY-METHOD
           MOVE DSB-INSTR-ID           TO CA-DB-INSTR-ID
           MOVE DSB-DUE-DATE           TO CA-DB-DUE-DATE
           MOVE DSB-DISB-STATUS        TO CA-DB-STATUS
           MOVE DSB-SOURCE-SYSTEM      TO CA-DB-SOURCE-SYSTEM
           MOVE DSB-SOURCE-REF         TO CA-DB-SOURCE-REF
           MOVE DSB-VOID-REASON        TO CA-DB-VOID-REASON
           MOVE DSB-APPROVER-USER      TO CA-DB-APPROVER.

      ******************************************************************
       3000-VALIDATE-CREATE.
           MOVE CA-DB-REASON           TO WS-REASON
           MOVE CA-DB-SOURCE-SYSTEM    TO WS-SOURCE
           MOVE CA-DB-PAY-METHOD       TO WS-METHOD
           IF WS-SOURCE = SPACES
              MOVE 'BILL'              TO WS-SOURCE
           END-IF
           IF WS-METHOD = SPACES
              MOVE 'CHEQ'              TO WS-METHOD
           END-IF
      * BR-BIL-DSB-001 Disbursement reasons
           IF NOT WS-RSN-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBRSN  '          TO CA-REASON-CODE
              STRING 'Invalid disbursement reason ' CA-DB-REASON
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-DB-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBAMT  '          TO CA-REASON-CODE
              MOVE 'Disbursement amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-DSB-002 Payee mandatory; cheque needs a postal address
           IF CA-DB-PAYEE-NAME = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBPAYEE'          TO CA-REASON-CODE
              MOVE 'Payee name is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF NOT WS-MTH-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBMTHD '          TO CA-REASON-CODE
              STRING 'Pay method ' CA-DB-PAY-METHOD
                     ' invalid - use CHEQ or BACS'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-MTH-CHEQUE AND
              (CA-DB-PAYEE-ADDR1 = SPACES OR
               CA-DB-PAYEE-POSTCODE = SPACES)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBADDR '          TO CA-REASON-CODE
              MOVE 'Cheque payee address and postcode are required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-DSB-003 Reason determines the source system
           IF NOT WS-SRC-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBSRC  '          TO CA-REASON-CODE
              STRING 'Invalid source system ' WS-SOURCE
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EVALUATE TRUE
              WHEN WS-RSN-CLMPAY AND WS-SOURCE NOT = 'CLMS'
              WHEN WS-RSN-COMMPAY AND WS-SOURCE NOT = 'COMM'
              WHEN WS-RSN-ACCT-FUNDED AND NOT WS-SRC-BILL
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DSBSRCRS'       TO CA-REASON-CODE
                 STRING 'Reason ' WS-REASON
                        ' not allowed from source ' WS-SOURCE
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           IF WS-RSN-ACCT-FUNDED AND CA-DB-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBACCT '          TO CA-REASON-CODE
              MOVE 'Account is required for overpayment or refund'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF NOT WS-SRC-BILL AND CA-DB-SOURCE-REF = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBSREF '          TO CA-REASON-CODE
              MOVE 'Source reference required for CLMS/COMM payments'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3010-VALIDATE-DUE-DATE.

      * BR-BIL-DSB-010 Due date valid and not in the past
       3010-VALIDATE-DUE-DATE.
           IF CA-DB-DUE-DATE = SPACES
              MOVE WS-BUS-DATE         TO CA-DB-DUE-DATE
           ELSE
              INITIALIZE WS-DTE-CA
              MOVE 'DTVALD  '          TO DTA-REQUEST-ID
              MOVE CA-DB-DUE-DATE      TO DTA-DT-DATE-1
              PERFORM 8800-CALL-DATES
              IF NOT DTA-DT-IS-VALID
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DSBDUE  '       TO CA-REASON-CODE
                 STRING 'Due date ' CA-DB-DUE-DATE ' is not valid'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              IF CA-DB-DUE-DATE < WS-BUS-DATE
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DSBDUEPS'       TO CA-REASON-CODE
                 STRING 'Due date ' CA-DB-DUE-DATE
                        ' is before business date ' WS-BUS-DATE
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-DSB-004 One live disbursement per source system/ref
       3100-CHECK-DUPLICATE-SOURCE.
           IF CA-DB-SOURCE-REF = SPACES
              EXIT PARAGRAPH
           END-IF
           MOVE WS-SOURCE              TO DSB-SOURCE-SYSTEM
           MOVE CA-DB-SOURCE-REF       TO DSB-SOURCE-REF
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-DUP-COUNT
                FROM BILDB.BIL_DISBURSEMENT
               WHERE SOURCE_SYSTEM = :DSB-SOURCE-SYSTEM
                 AND SOURCE_REF    = :DSB-SOURCE-REF
                 AND DISB_STATUS  <> 'VOID'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3100-CHECK-DUPLICATE-SOURCE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DUP-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DSBDUP  '          TO CA-REASON-CODE
              STRING 'Disbursement already exists for '
                     WS-SOURCE ' ref ' CA-DB-SOURCE-REF
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-DSB-006 Account funding limited to unapplied cash
       3200-CHECK-FUNDING.
           IF NOT WS-RSN-ACCT-FUNDED
              EXIT PARAGRAPH
           END-IF
           IF CA-DB-AMOUNT > ACT-UNAPPLIED-AMT
              MOVE ACT-UNAPPLIED-AMT   TO WS-AMOUNT-DISP
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBUNAP '          TO CA-REASON-CODE
              STRING 'Amount exceeds account unapplied of '
                     WS-AMOUNT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-DSB-009 BACS from billing needs an active instrument on
      * the same account; CLMS/COMM payees use the source system's
      * payee mandate so no billing instrument is held for them.
       3300-CHECK-PAY-INSTRUMENT.
           IF NOT WS-MTH-BACS OR NOT WS-SRC-BILL
              EXIT PARAGRAPH
           END-IF
           IF CA-DB-INSTR-ID = 0 AND ACT-AUTOPAY-INSTR-ID > 0
              MOVE ACT-AUTOPAY-INSTR-ID TO CA-DB-INSTR-ID
           END-IF
           MOVE CA-DB-INSTR-ID         TO PIN-INSTR-ID
           MOVE CA-DB-ACCOUNT-ID       TO PIN-ACCOUNT-ID
           EXEC SQL
              SELECT INSTR_TYPE, INSTR_STATUS
                INTO :PIN-INSTR-TYPE, :PIN-INSTR-STATUS
                FROM BILDB.BIL_PAYMENT_INSTRUMENT
               WHERE INSTR_ID   = :PIN-INSTR-ID
                 AND ACCOUNT_ID = :PIN-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF PIN-INSTR-STATUS NOT = 'ACTV'
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'DSBINSTR'    TO CA-REASON-CODE
                    STRING 'Payment instrument is '
                           PIN-INSTR-STATUS ' not ACTV'
                           DELIMITED BY SIZE INTO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DSBINSTR'       TO CA-REASON-CODE
                 MOVE 'BACS refund needs an instrument on the account'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3300-CHECK-PAY-INSTRUMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4000-READ-ACCOUNT.
           MOVE CA-DB-ACCOUNT-ID       TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_STATUS, UNAPPLIED_AMT, AUTOPAY_INSTR_ID,
                     VERSION_NO
                INTO :ACT-ACCOUNT-STATUS, :ACT-UNAPPLIED-AMT,
                     :ACT-AUTOPAY-INSTR-ID, :ACT-VERSION-NO
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 MOVE 'Billing account not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4000-READ-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-DISBURSEMENT.
           MOVE CA-DB-ACCOUNT-ID       TO DSB-ACCOUNT-ID
           MOVE CA-DB-AMOUNT           TO DSB-DISB-AMOUNT
           MOVE WS-REASON              TO DSB-DISB-REASON
           MOVE CA-DB-PAYEE-NAME       TO DSB-PAYEE-NAME-TEXT
           MOVE LENGTH OF DSB-PAYEE-NAME-TEXT TO DSB-PAYEE-NAME-LEN
           MOVE CA-DB-PAYEE-ADDR1      TO DSB-PAYEE-ADDR1-TEXT
           MOVE LENGTH OF DSB-PAYEE-ADDR1-TEXT
                                       TO DSB-PAYEE-ADDR1-LEN
           MOVE CA-DB-PAYEE-POSTCODE   TO DSB-PAYEE-POSTCODE
           MOVE 0                      TO DSB-PAYEE-ADDR1-IND
                                          DSB-PAYEE-POSTCODE-IND
           IF CA-DB-PAYEE-ADDR1 = SPACES
              MOVE -1                  TO DSB-PAYEE-ADDR1-IND
           END-IF
           IF CA-DB-PAYEE-POSTCODE = SPACES
              MOVE -1                  TO DSB-PAYEE-POSTCODE-IND
           END-IF
           MOVE WS-METHOD              TO DSB-PAY-METHOD
           MOVE CA-DB-INSTR-ID         TO DSB-INSTR-ID
           MOVE CA-DB-DUE-DATE         TO DSB-DUE-DATE
           MOVE WS-NEW-STATUS          TO DSB-DISB-STATUS
           MOVE WS-SOURCE              TO DSB-SOURCE-SYSTEM
           MOVE CA-DB-SOURCE-REF       TO DSB-SOURCE-REF
           MOVE CA-USER-ID             TO DSB-REQUEST-USER
           MOVE WS-APPROVER            TO DSB-APPROVER-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_DISBURSEMENT
                   ( DISB_ID, ACCOUNT_ID, DISB_AMOUNT, DISB_REASON,
                     PAYEE_NAME, PAYEE_ADDR1, PAYEE_POSTCODE,
                     PAY_METHOD, INSTR_ID, DUE_DATE, DISB_STATUS,
                     SOURCE_SYSTEM, SOURCE_REF, REQUEST_USER,
                     APPROVER_USER, VOID_REASON, CREATE_TS,
                     UPDATE_TS )
              VALUES
                   ( :DSB-DISB-ID, :DSB-ACCOUNT-ID, :DSB-DISB-AMOUNT,
                     :DSB-DISB-REASON, :DSB-PAYEE-NAME,
                     :DSB-PAYEE-ADDR1 :DSB-PAYEE-ADDR1-IND,
                     :DSB-PAYEE-POSTCODE :DSB-PAYEE-POSTCODE-IND,
                     :DSB-PAY-METHOD, :DSB-INSTR-ID, :DSB-DUE-DATE,
                     :DSB-DISB-STATUS, :DSB-SOURCE-SYSTEM,
                     :DSB-SOURCE-REF, :DSB-REQUEST-USER,
                     :DSB-APPROVER-USER, ' ', CURRENT TIMESTAMP,
                     CURRENT TIMESTAMP )
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DSBDUPID'       TO CA-REASON-CODE
                 MOVE 'Disbursement id already in use - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4100-INSERT-DISBURSEMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DSB-006 Unapplied reduced under optimistic lock; the
      * predicate re-checks the balance so it can never go negative.
       4200-REDUCE-UNAPPLIED.
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET UNAPPLIED_AMT = UNAPPLIED_AMT - :DSB-DISB-AMOUNT,
                     VERSION_NO    = VERSION_NO + 1,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID    = :ACT-ACCOUNT-ID
                 AND VERSION_NO    = :ACT-VERSION-NO
                 AND UNAPPLIED_AMT >= :DSB-DISB-AMOUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'STALEVER'    TO CA-REASON-CODE
                    MOVE 'Account changed by another user - retry'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4200-REDUCE-UNAPPLIED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DSB-016 Take the refund from receipts oldest first
       4250-CONSUME-RECEIPTS.
           MOVE DSB-DISB-AMOUNT        TO WS-REMAINING
           MOVE 'N'                    TO WS-RCP-EOF
           EXEC SQL OPEN RCP-OLD-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4250-CONSUME-RECEIPTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-OLD-CSR-OPEN        TO TRUE
           PERFORM UNTIL WS-RCP-END OR WS-REMAINING = 0
              EXEC SQL
                 FETCH RCP-OLD-CSR
                  INTO :RCP-MONEY-ID, :RCP-UNAPPLIED-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF RCP-UNAPPLIED-AMOUNT < WS-REMAINING
                       MOVE RCP-UNAPPLIED-AMOUNT TO WS-TAKE
                    ELSE
                       MOVE WS-REMAINING TO WS-TAKE
                    END-IF
                    PERFORM 4260-ADJUST-RECEIPT
                    SUBTRACT WS-TAKE   FROM WS-REMAINING
                 WHEN 100
                    SET WS-RCP-END     TO TRUE
                 WHEN OTHER
                    MOVE '4250-CONSUME-RECEIPTS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE RCP-OLD-CSR END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR
           IF WS-REMAINING > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'DSBRCPUN'          TO CA-REASON-CODE
              MOVE 'Receipt unapplied does not cover the account total'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      * WS-TAKE is subtracted from the receipt (negative adds back)
       4260-ADJUST-RECEIPT.
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_RECEIPT
                 SET UNAPPLIED_AMOUNT = UNAPPLIED_AMOUNT - :WS-TAKE,
                     UPDATE_TS        = CURRENT TIMESTAMP
               WHERE MONEY_ID         = :RCP-MONEY-ID
                 AND UNAPPLIED_AMOUNT - :WS-TAKE >= 0
                 AND UNAPPLIED_AMOUNT - :WS-TAKE <= RECEIPT_AMOUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'STALEVER'    TO CA-REASON-CODE
                    MOVE 'Receipt changed by another user - retry'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4260-ADJUST-RECEIPT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-READ-DISBURSEMENT.
           IF CA-DB-DISB-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBKEY  '          TO CA-REASON-CODE
              MOVE 'Disbursement id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-DB-DISB-ID          TO DSB-DISB-ID
           EXEC SQL
              SELECT DISB_ID, ACCOUNT_ID, DISB_AMOUNT, DISB_REASON,
                     PAYEE_NAME, PAYEE_ADDR1, PAYEE_POSTCODE,
                     PAY_METHOD, INSTR_ID, DUE_DATE, DISB_STATUS,
                     SOURCE_SYSTEM, SOURCE_REF, REQUEST_USER,
                     APPROVER_USER, VOID_REASON
                INTO :DSB-DISB-ID, :DSB-ACCOUNT-ID, :DSB-DISB-AMOUNT,
                     :DSB-DISB-REASON, :DSB-PAYEE-NAME,
                     :DSB-PAYEE-ADDR1 :DSB-PAYEE-ADDR1-IND,
                     :DSB-PAYEE-POSTCODE :DSB-PAYEE-POSTCODE-IND,
                     :DSB-PAY-METHOD, :DSB-INSTR-ID, :DSB-DUE-DATE,
                     :DSB-DISB-STATUS, :DSB-SOURCE-SYSTEM,
                     :DSB-SOURCE-REF, :DSB-REQUEST-USER,
                     :DSB-APPROVER-USER, :DSB-VOID-REASON
                FROM BILDB.BIL_DISBURSEMENT
               WHERE DISB_ID = :DSB-DISB-ID
           END-EXEC
           PERFORM 4320-CHECK-READ.

       4310-READ-BY-SOURCE.
           MOVE CA-DB-SOURCE-SYSTEM    TO DSB-SOURCE-SYSTEM
           MOVE CA-DB-SOURCE-REF       TO DSB-SOURCE-REF
           EXEC SQL
              SELECT DISB_ID, ACCOUNT_ID, DISB_AMOUNT, DISB_REASON,
                     PAYEE_NAME, PAYEE_ADDR1, PAYEE_POSTCODE,
                     PAY_METHOD, INSTR_ID, DUE_DATE, DISB_STATUS,
                     SOURCE_SYSTEM, SOURCE_REF, REQUEST_USER,
                     APPROVER_USER, VOID_REASON
                INTO :DSB-DISB-ID, :DSB-ACCOUNT-ID, :DSB-DISB-AMOUNT,
                     :DSB-DISB-REASON, :DSB-PAYEE-NAME,
                     :DSB-PAYEE-ADDR1 :DSB-PAYEE-ADDR1-IND,
                     :DSB-PAYEE-POSTCODE :DSB-PAYEE-POSTCODE-IND,
                     :DSB-PAY-METHOD, :DSB-INSTR-ID, :DSB-DUE-DATE,
                     :DSB-DISB-STATUS, :DSB-SOURCE-SYSTEM,
                     :DSB-SOURCE-REF, :DSB-REQUEST-USER,
                     :DSB-APPROVER-USER, :DSB-VOID-REASON
                FROM BILDB.BIL_DISBURSEMENT
               WHERE SOURCE_SYSTEM = :DSB-SOURCE-SYSTEM
                 AND SOURCE_REF    = :DSB-SOURCE-REF
               ORDER BY CREATE_TS DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           PERFORM 4320-CHECK-READ.

       4320-CHECK-READ.
           EVALUATE SQLCODE
              WHEN 0
                 MOVE DSB-DISB-ID      TO WS-KEY-NUM
                 MOVE WS-KEY-NUM       TO WS-ERR-KEY
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'DSBNOTFD'       TO CA-REASON-CODE
                 MOVE 'Disbursement not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4300-READ-DISBURSEMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Status change guarded by the expected old status so two users
      * acting on the same row cannot both succeed.
       4400-UPDATE-STATUS.
           MOVE WS-NEW-STATUS          TO DSB-DISB-STATUS
           MOVE WS-APPROVER            TO DSB-APPROVER-USER
           MOVE CA-DB-VOID-REASON      TO DSB-VOID-REASON
           IF WS-NEW-STATUS NOT = 'VOID'
              MOVE SPACES              TO DSB-VOID-REASON
           END-IF
           EXEC SQL
              UPDATE BILDB.BIL_DISBURSEMENT
                 SET DISB_STATUS   = :DSB-DISB-STATUS,
                     APPROVER_USER = :DSB-APPROVER-USER,
                     VOID_REASON   = :DSB-VOID-REASON,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE DISB_ID       = :DSB-DISB-ID
                 AND DISB_STATUS   = :WS-OLD-STATUS
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'STALEVER'    TO CA-REASON-CODE
                    MOVE 'Disbursement changed by another user'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4400-UPDATE-STATUS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-FIND-CREATE-TXN.
           MOVE DSB-DISB-ID            TO TXN-REF-ID
           EXEC SQL
              SELECT TXN_ID
                INTO :TXN-TXN-ID
                FROM BILDB.BIL_TRANSACTION
               WHERE REF_TYPE        = 'DISB'
                 AND REF_ID          = :TXN-REF-ID
                 AND TXN_TYPE        = 'DISBCRT '
                 AND REVERSAL_OF_TXN = 0
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE TXN-TXN-ID       TO WS-ORIG-TXN-ID
              WHEN 100
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'DSBNOTXN'       TO CA-REASON-CODE
                 MOVE 'Create posting for disbursement not found'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4500-FIND-CREATE-TXN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4600-RESTORE-UNAPPLIED.
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET UNAPPLIED_AMT = UNAPPLIED_AMT + :DSB-DISB-AMOUNT,
                     VERSION_NO    = VERSION_NO + 1,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID    = :ACT-ACCOUNT-ID
                 AND VERSION_NO    = :ACT-VERSION-NO
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'STALEVER'    TO CA-REASON-CODE
                    MOVE 'Account changed by another user - retry'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN -911
              WHEN -913
                 PERFORM 9910-DEADLOCK
              WHEN OTHER
                 MOVE '4600-RESTORE-UNAPPLIED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DSB-016 Give the cash back to the newest receipts, never
      * beyond a receipt's own amount
       4650-RESTORE-RECEIPTS.
           MOVE DSB-DISB-AMOUNT        TO WS-REMAINING
           MOVE 'N'                    TO WS-RCP-EOF
           EXEC SQL OPEN RCP-NEW-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4650-RESTORE-RECEIPTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-NEW-CSR-OPEN        TO TRUE
           PERFORM UNTIL WS-RCP-END OR WS-REMAINING = 0
              EXEC SQL
                 FETCH RCP-NEW-CSR
                  INTO :RCP-MONEY-ID, :RCP-UNAPPLIED-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF RCP-UNAPPLIED-AMOUNT < WS-REMAINING
                       COMPUTE WS-TAKE = 0 - RCP-UNAPPLIED-AMOUNT
                    ELSE
                       COMPUTE WS-TAKE = 0 - WS-REMAINING
                    END-IF
                    PERFORM 4260-ADJUST-RECEIPT
                    ADD WS-TAKE        TO WS-REMAINING
                 WHEN 100
                    SET WS-RCP-END     TO TRUE
                 WHEN OTHER
                    MOVE '4650-RESTORE-RECEIPTS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE RCP-NEW-CSR END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR.

      ******************************************************************
      * BR-BIL-DSB-015 COMMPAY source ref is the producer payment id;
      * the commission items go back to EARN so the next pay run
      * picks them up again, and the payable balance is restored.
       4700-REOPEN-PRODUCER-PAYMENT.
           IF DSB-SOURCE-REF(1:10) NOT NUMERIC
              EXIT PARAGRAPH
           END-IF
           MOVE DSB-SOURCE-REF(1:10)   TO WS-REF-NUM
           MOVE WS-REF-NUM             TO PPY-PRODUCER-PAY-ID
           EXEC SQL
              SELECT PRODUCER_ID, PAY_AMOUNT
                INTO :PPY-PRODUCER-ID, :PPY-PAY-AMOUNT
                FROM BILDB.BIL_PRODUCER_PAYMENT
               WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
                 AND DISB_ID         = :DSB-DISB-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE '4700-REOPEN-PRODUCER-PAYMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           EXEC SQL
              UPDATE BILDB.BIL_PRODUCER_PAYMENT
                 SET PAY_STATUS      = 'VOID'
               WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4700-REOPEN-PRODUCER-PAYMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              UPDATE BILDB.BIL_COMMISSION_ITEM
                 SET CI_STATE        = 'EARN',
                     PRODUCER_PAY_ID = 0,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4700-REOPEN-PRODUCER-PAYMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              UPDATE BILDB.BIL_PRODUCER
                 SET PAYABLE_BALANCE = PAYABLE_BALANCE
                                     + :PPY-PAY-AMOUNT,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE PRODUCER_ID     = :PPY-PRODUCER-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4700-REOPEN-PRODUCER-PAYMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-DSB-017 COMMPAY is funded from a recorded producer
      * payment; its producer is carried on the ledger posting.
       4800-READ-PRODUCER-PAYMENT.
           IF DSB-SOURCE-REF(1:10) NOT NUMERIC
              OR DSB-SOURCE-REF(11:10) NOT = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBPPYRF'          TO CA-REASON-CODE
              MOVE 'COMMPAY source ref must be a producer payment id'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE DSB-SOURCE-REF(1:10)   TO WS-REF-NUM
           MOVE WS-REF-NUM             TO PPY-PRODUCER-PAY-ID
           EXEC SQL
              SELECT PRODUCER_ID, PAY_AMOUNT, PAY_STATUS
                INTO :PPY-PRODUCER-ID, :PPY-PAY-AMOUNT,
                     :PPY-PAY-STATUS
                FROM BILDB.BIL_PRODUCER_PAYMENT
               WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PPYNOTFD'       TO CA-REASON-CODE
                 MOVE 'Producer payment not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4800-READ-PRODUCER-PAYMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF PPY-PAY-STATUS = 'VOID' OR
              PPY-PAY-AMOUNT NOT = CA-DB-AMOUNT
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSBPPYAM'          TO CA-REASON-CODE
              MOVE 'Amount differs from producer payment, or it is void'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE PPY-PRODUCER-ID        TO WS-PRODUCER-ID.

      ******************************************************************
       4900-READ-BUSINESS-DATE.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-READ-BUSINESS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
      * BR-BIL-DSB-008 Funding T-account by disbursement reason
       5000-SET-FUNDING-T-ACCOUNT.
           EVALUATE TRUE
              WHEN WS-RSN-CLMPAY
                 MOVE 'CLMPAY  '       TO WS-FUND-T-ACCOUNT
              WHEN WS-RSN-COMMPAY
                 MOVE 'COMMPAY '       TO WS-FUND-T-ACCOUNT
              WHEN OTHER
                 MOVE 'UNAPPLD '       TO WS-FUND-T-ACCOUNT
           END-EVALUATE.

      ******************************************************************
       8000-NEXT-DISB-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'DSB     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE SQA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE SQA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE
           MOVE SQA-SQ-VALUE           TO DSB-DISB-ID
           MOVE DSB-DISB-ID            TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY.

      ******************************************************************
       8100-CHECK-DISB-HOLD.
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLA-REQUEST-ID
           MOVE CA-USER-ID             TO HLA-USER-ID
           MOVE WS-BUS-DATE            TO HLA-BUS-DATE
           MOVE CA-TRACE-ID            TO HLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HLA-CALLER-PGM
           MOVE CA-DB-ACCOUNT-ID       TO HLA-HL-ACCOUNT-ID
           MOVE 'DISB'                 TO HLA-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLA-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           MOVE HLA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE HLA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE HLA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE
           IF HLA-HL-IS-ACTIVE
              SET CA-RC-STATE          TO TRUE
              MOVE 'DSBHOLD '          TO CA-REASON-CODE
              MOVE 'Account has an active disbursement hold'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8200-CHECK-AUTHORITY.
           MOVE ATA-AT-USER            TO WS-APPROVER
           INITIALIZE WS-AUT-CA
           MOVE 'AUTCHK  '             TO ATA-REQUEST-ID
           MOVE CA-USER-ID             TO ATA-USER-ID
           MOVE WS-BUS-DATE            TO ATA-BUS-DATE
           MOVE CA-TRACE-ID            TO ATA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ATA-CALLER-PGM
           MOVE WS-APPROVER            TO ATA-AT-USER
           MOVE 'DISBURSE'             TO ATA-AT-LIMIT-TYPE
           MOVE CA-DB-AMOUNT           TO ATA-AT-AMOUNT
           CALL WS-AUT-PGM USING WS-AUT-CA
           MOVE ATA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE ATA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE ATA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8300-POST-CREATE-LEDGER.
           PERFORM 8320-PRIME-LEDGER
           MOVE 'DISBCRT '             TO GLA-GL-TXN-TYPE
           MOVE 0                      TO GLA-GL-REVERSAL-OF
           MOVE WS-FUND-T-ACCOUNT      TO GLA-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLA-GL-DR-CR(1)
           MOVE 'DISBPAY '             TO GLA-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLA-GL-DR-CR(2)
           PERFORM 8330-CALL-LEDGER.

      * BR-BIL-DSB-014 Ledger mirrors the create posting (GLREVS)
       8310-REVERSE-CREATE-LEDGER.
           PERFORM 8320-PRIME-LEDGER
           MOVE 'GLREVS  '             TO GLA-REQUEST-ID
           MOVE 'DISBVOID'             TO GLA-GL-TXN-TYPE
           MOVE WS-ORIG-TXN-ID         TO GLA-GL-REVERSAL-OF
           MOVE 0                      TO GLA-GL-LINE-COUNT
           PERFORM 8330-CALL-LEDGER.

       8320-PRIME-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE CA-TRACE-ID            TO GLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE DSB-ACCOUNT-ID         TO GLA-GL-ACCOUNT-ID
           MOVE WS-PRODUCER-ID         TO GLA-GL-PRODUCER-ID
           MOVE 'DISB'                 TO GLA-GL-REF-TYPE
           MOVE DSB-DISB-ID            TO GLA-GL-REF-ID
           MOVE 2                      TO GLA-GL-LINE-COUNT
           MOVE DSB-DISB-AMOUNT        TO GLA-GL-AMOUNT(1)
                                          GLA-GL-AMOUNT(2).

       8330-CALL-LEDGER.
           CALL WS-GLP-PGM USING WS-GLP-CA
           MOVE GLA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE GLA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE GLA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8400-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE 'DISB    '             TO AUA-AU-ENTITY
           MOVE WS-ERR-KEY             TO AUA-AU-KEY
           MOVE WS-AUD-ACTION          TO AUA-AU-ACTION
           MOVE 'DISB_STATUS'          TO AUA-AU-FIELD
           MOVE WS-OLD-STATUS          TO AUA-AU-BEFORE
           MOVE DSB-DISB-AMOUNT        TO WS-AMOUNT-DISP
           STRING WS-NEW-STATUS ' ' DSB-DISB-REASON ' '
                  WS-AMOUNT-DISP ' ' DSB-VOID-REASON
                  DELIMITED BY SIZE INTO AUA-AU-AFTER
           MOVE WS-PROGRAM-ID          TO AUA-AU-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           MOVE AUA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE AUA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE AUA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8800-CALL-DATES.
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE CA-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           MOVE DTA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE DTA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE DTA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Callee RC >= 12 is passed up unchanged with its reason code.
       8900-CHECK-CALLEE.
           IF WS-CALLEE-RC >= 12
              MOVE WS-CALLEE-RC        TO CA-RETURN-CODE
              MOVE WS-CALLEE-REASON    TO CA-REASON-CODE
              MOVE WS-CALLEE-MESSAGE   TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * A cursor still open here means an error exit from inside a
      * fetch loop; close it so the caller's unit of work is clean.
       9000-RETURN.
           EVALUATE TRUE
              WHEN WS-OLD-CSR-OPEN
                 EXEC SQL CLOSE RCP-OLD-CSR END-EXEC
              WHEN WS-NEW-CSR-OPEN
                 EXEC SQL CLOSE RCP-NEW-CSR END-EXEC
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           MOVE SPACES                 TO WS-OPEN-CURSOR
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'BILDISB0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.

       9910-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Disbursement data locked - retry' TO CA-MESSAGE
           PERFORM 9000-RETURN.
