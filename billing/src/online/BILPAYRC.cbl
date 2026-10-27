       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILPAYRC.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILPAYRC                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Payment receipt service. Records money received  *
      *               from any channel, finds the billing account it   *
      *               belongs to, books it to cash and hands it to the *
      *               distribution engine; money that cannot be placed *
      *               goes to suspense. Also reverses receipts (NSF,   *
      *               wrong account, refund, stopped payment), answers *
      *               receipt enquiries and matches suspense items to  *
      *               an account.                                      *
      * REQUESTS    : PAYRCV  record and distribute a receipt          *
      *               PAYREV  reverse MONEY-ID                         *
      *               PAYINQ  receipt with its distributions           *
      *               PAYMAT  match suspense SUSP-ID to ACCOUNT-NUMBER *
      * CALLED BY   : BILLKBX0 (lockbox) BILACHRT (ACH settle/return)  *
      *               cash-entry and suspense screens                  *
      * CALLS       : BILCODVL BILHOLD0 BILPAYDS BILDLQSV BILAUTHZ     *
      *               BILINSTR BILISCHD BILACCT0 BILSEQGN BILGLPST     *
      *               BILAUDIT BILDATES BILERRLG                       *
      * TABLES      : BIL_PAYMENT_RECEIPT    (C R U)                   *
      *               BIL_SUSPENSE           (C R U)                   *
      *               BIL_ACCOUNT            (R U)                     *
      *               BIL_INVOICE            (R)                       *
      *               BIL_INVOICE_ITEM       (R)                       *
      *               BIL_DISTRIBUTION       (R)                       *
      *               BIL_PAYMENT_INSTRUMENT (R)                       *
      *               BIL_POLICY_PERIOD      (R)                       *
      *               BIL_CHARGE             (C)                       *
      *               BIL_CODE_TABLE         (R)                       *
      *               BIL_TRANSACTION        (R)                       *
      *               BIL_CONTROL            (R)                       *
      * RULES       : BR-BIL-PAY-001 Receipt amount must be above zero *
      *               BR-BIL-PAY-002 Method (PAYMTHD) and source       *
      *                              (RCPTSRC) must be active codes    *
      *               BR-BIL-PAY-003 Received date valid and not after *
      *                              the business date                 *
      *               BR-BIL-PAY-004 Account resolved by id, then by   *
      *                              account number, then by invoice   *
      *               BR-BIL-PAY-005 An invoice on another account is  *
      *                              not targeted; money follows the   *
      *                              account number                    *
      *               BR-BIL-PAY-006 Unresolved money -> suspense      *
      *                              NOACCT; closed account ACCTCLSD   *
      *               BR-BIL-PAY-007 Same account, reference, amount   *
      *                              and date already live = duplicate *
      *               BR-BIL-PAY-008 Instrument must be the account's  *
      *               BR-BIL-PAY-009 Receipt books DR CASH and CR      *
      *                              UNAPPLD (SUSPENSE when unplaced)  *
      *               BR-BIL-PAY-010 Active PYMT hold keeps the money  *
      *                              unapplied on the account          *
      *               BR-BIL-PAY-011 Due items only (mode D), target   *
      *                              invoice first                     *
      *               BR-BIL-PAY-012 Clearing all past due exits open  *
      *                              delinquency processes             *
      *               BR-BIL-PAY-013 Reversal reasons NSF WRONGACC     *
      *                              REFUND STOPPYMT                   *
      *               BR-BIL-PAY-014 Reversal above the user's         *
      *                              REVERSAL limit is refused (RC 20) *
      *               BR-BIL-PAY-015 Reversal undoes distributions,    *
      *                              then reverses the cash entry      *
      *               BR-BIL-PAY-016 NSF fee only while code table     *
      *                              NSFFEE has an active row; billed  *
      *                              as a one-time charge              *
      *               BR-BIL-PAY-017 NSF and stopped payment count as  *
      *                              an instrument failure             *
      *               BR-BIL-PAY-018 Only OPEN suspense matches, and   *
      *                              only to an active account         *
      *               BR-BIL-PAY-019 A reversed receipt is final       *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILPAYRC'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'BILCODVL'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-DST-PGM                  PIC X(8)  VALUE 'BILPAYDS'.
       01  WS-DLQ-PGM                  PIC X(8)  VALUE 'BILDLQSV'.
       01  WS-AUT-PGM                  PIC X(8)  VALUE 'BILAUTHZ'.
       01  WS-PIN-PGM                  PIC X(8)  VALUE 'BILINSTR'.
       01  WS-SCH-PGM                  PIC X(8)  VALUE 'BILISCHD'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-SWITCHES.
           05 WS-RESOLVED-SW           PIC X     VALUE 'N'.
              88 WS-ACCT-RESOLVED                VALUE 'Y'.
              88 WS-ACCT-UNRESOLVED              VALUE 'N'.
           05 WS-DIST-CSR-SW           PIC X     VALUE 'N'.
              88 WS-DIST-CSR-OPEN                VALUE 'Y'.
           05 WS-TXN-CSR-SW            PIC X     VALUE 'N'.
              88 WS-TXN-CSR-OPEN                 VALUE 'Y'.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-END                    VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-MONEY-ID              PIC S9(9) COMP.
           05 WS-ACCOUNT-ID            PIC S9(9) COMP.
           05 WS-ACCT-VERSION          PIC S9(9) COMP.
           05 WS-TARGET-INVOICE-ID     PIC S9(9) COMP.
           05 WS-INV-ACCOUNT-ID        PIC S9(9) COMP.
           05 WS-INSTR-ACCOUNT-ID      PIC S9(9) COMP.
           05 WS-SUSP-REASON           PIC X(8).
           05 WS-SUSP-ID               PIC S9(9) COMP.
           05 WS-DUP-COUNT             PIC S9(9) COMP.
           05 WS-DIST-ROWS             PIC S9(9) COMP.
           05 WS-PASTDUE-BEFORE        PIC S9(11)V99 COMP-3.
           05 WS-PASTDUE-AFTER         PIC S9(11)V99 COMP-3.
           05 WS-UNAPPLIED-DELTA       PIC S9(11)V99 COMP-3.
           05 WS-FEE-AMOUNT            PIC S9(11)V99 COMP-3.
           05 WS-FEE-PP-ID             PIC S9(9) COMP.
           05 WS-OLD-STATUS            PIC X(4).
           05 WS-GL-TXN-TYPE           PIC X(8).
           05 WS-GL-DR-ACCOUNT         PIC X(8).
           05 WS-GL-CR-ACCOUNT         PIC X(8).
           05 WS-GL-AMOUNT             PIC S9(11)V99 COMP-3.
           05 WS-GL-ACCT-ID      PIC S9(9) COMP.
           05 WS-GL-REF-TYPE           PIC X(4).
           05 WS-GL-REF-ID             PIC S9(9) COMP.
           05 WS-FETCH-TXN-ID          PIC S9(9) COMP.
           05 WS-TX-IX                 PIC S9(4) COMP.
           05 WS-TX-COUNT              PIC S9(4) COMP.
           05 WS-TX-ID OCCURS 5 TIMES  PIC S9(9) COMP.
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-NEXT-ID               PIC S9(9) COMP.
           05 WS-AMOUNT-DISP           PIC -(10)9.99.
           05 WS-KEY-DISP              PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-REF-TEXT              PIC X(80).

       01  WS-DIST-ROW.
           05 WS-DR-ITEM-ID            PIC S9(9) COMP.
           05 WS-DR-INVOICE-NO         PIC X(12).
           05 WS-DR-AMOUNT             PIC S9(11)V99 COMP-3.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDRCP END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDINV END-EXEC.
           EXEC SQL INCLUDE BILDSUS END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDCOD END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

           EXEC SQL DECLARE CSR-RCPT-DISTS CURSOR FOR
              SELECT D.ITEM_ID,
                     COALESCE(V.INVOICE_NUMBER, ' '),
                     D.DIST_AMOUNT
                FROM BILDB.BIL_DISTRIBUTION D
                JOIN BILDB.BIL_INVOICE_ITEM T
                  ON T.ITEM_ID = D.ITEM_ID
                LEFT OUTER JOIN BILDB.BIL_INVOICE V
                  ON V.INVOICE_ID = T.INVOICE_ID
               WHERE D.MONEY_ID = :WS-MONEY-ID
               ORDER BY D.DIST_ID
               FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-CASH-TXNS CURSOR FOR
              SELECT T.TXN_ID
                FROM BILDB.BIL_TRANSACTION T
               WHERE T.REF_TYPE = 'RCPT'
                 AND T.REF_ID   = :WS-MONEY-ID
                 AND T.TXN_TYPE IN ('PAYRCV', 'PAYSUSP', 'PAYMATCH')
                 AND T.REVERSAL_OF_TXN = 0
                 AND NOT EXISTS
                     (SELECT 1 FROM BILDB.BIL_TRANSACTION R
                       WHERE R.REVERSAL_OF_TXN = T.TXN_ID)
               ORDER BY T.TXN_ID DESC
               FOR FETCH ONLY
           END-EXEC.

       01  WS-COD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY BILCCOD REPLACING LEADING ==CA-== BY ==CDA-==.

       01  WS-HLD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==HLA-==.
           03 HLA-BODY.
              COPY BILCHLD REPLACING LEADING ==CA-== BY ==HLA-==.

       01  WS-DST-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DSA-==.
           03 DSA-BODY.
              COPY BILCDST REPLACING LEADING ==CA-== BY ==DSA-==.

       01  WS-DLQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DQA-==.
           03 DQA-BODY.
              COPY BILCDLQ REPLACING LEADING ==CA-== BY ==DQA-==.

       01  WS-AUT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ATA-==.
           03 ATA-BODY.
              COPY BILCAUT REPLACING LEADING ==CA-== BY ==ATA-==.

       01  WS-PIN-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PIA-==.
           03 PIA-BODY.
              COPY BILCPIN REPLACING LEADING ==CA-== BY ==PIA-==.

       01  WS-SCH-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SCA-==.
           03 SCA-BODY.
              COPY BILCSCH REPLACING LEADING ==CA-== BY ==SCA-==.

       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

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

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCPAY.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'PAYRCV  '
                 PERFORM 2000-RECEIVE
              WHEN 'PAYREV  '
                 PERFORM 2200-REVERSE
              WHEN 'PAYINQ  '
                 PERFORM 2400-INQUIRE
              WHEN 'PAYMAT  '
                 PERFORM 2600-MATCH-SUSPENSE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILPAYRC: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-PY-DISTRIBUTED
                                          CA-PY-UNAPPLIED
                                          CA-PY-DIST-COUNT
                                          WS-TARGET-INVOICE-ID
                                          WS-ACCOUNT-ID
           MOVE SPACES                 TO WS-GL-REF-TYPE
           MOVE CA-PY-MONEY-ID         TO WS-KEY-DISP
           MOVE WS-KEY-DISP            TO WS-ERR-KEY
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-BUS-DATE
           ELSE
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * PAYRCV - record and distribute a receipt
      ******************************************************************
       2000-RECEIVE.
           PERFORM 3000-VALIDATE-RECEIPT
           PERFORM 3100-RESOLVE-ACCOUNT
           IF WS-ACCT-RESOLVED AND CA-PY-INSTR-ID > 0
              PERFORM 3300-CHECK-INSTRUMENT
           END-IF
           PERFORM 3200-CHECK-DUPLICATE

           MOVE 'RCP     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-ID
           MOVE WS-NEXT-ID             TO WS-MONEY-ID
                                          CA-PY-MONEY-ID
           MOVE WS-MONEY-ID            TO WS-KEY-DISP
           MOVE WS-KEY-DISP            TO WS-ERR-KEY

           IF WS-ACCT-UNRESOLVED
              PERFORM 2100-TO-SUSPENSE
              PERFORM 9000-RETURN
           END-IF

           MOVE WS-ACCOUNT-ID          TO RCP-ACCOUNT-ID
           MOVE 'RCVD'                 TO RCP-RECEIPT-STATUS
           PERFORM 4100-INSERT-RECEIPT
      * BR-BIL-PAY-009 Cash in, held unapplied until distributed
           MOVE 'PAYRCV  '             TO WS-GL-TXN-TYPE
           MOVE 'CASH    '             TO WS-GL-DR-ACCOUNT
           MOVE 'UNAPPLD '             TO WS-GL-CR-ACCOUNT
           MOVE CA-PY-AMOUNT           TO WS-GL-AMOUNT
           MOVE WS-ACCOUNT-ID          TO WS-GL-ACCT-ID
           PERFORM 8300-POST-LEDGER
           MOVE CA-PY-AMOUNT           TO WS-UNAPPLIED-DELTA
           PERFORM 4300-UPDATE-ACCOUNT-UNAPPLIED
           PERFORM 2800-APPLY-TO-ACCOUNT.

      ******************************************************************
      * BR-BIL-PAY-006 Money with no usable account is parked in
      * suspense against account 0 for the cash team to match
       2100-TO-SUSPENSE.
           MOVE 0                      TO RCP-ACCOUNT-ID
           MOVE 'SUSP'                 TO RCP-RECEIPT-STATUS
           PERFORM 4100-INSERT-RECEIPT

           MOVE 'SUS     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-ID
           MOVE WS-NEXT-ID             TO WS-SUSP-ID
           MOVE SPACES                 TO WS-REF-TEXT
           STRING 'ACCT ' CA-PY-ACCOUNT-NUMBER
                  ' INV ' CA-PY-INVOICE-NUMBER
                  ' REF ' CA-PY-REF-NUMBER
                  ' SRC ' CA-PY-SOURCE
                  DELIMITED BY SIZE INTO WS-REF-TEXT
           PERFORM 4400-INSERT-SUSPENSE

      * BR-BIL-PAY-009 Unplaced cash is credited to SUSPENSE
           MOVE 'PAYSUSP '             TO WS-GL-TXN-TYPE
           MOVE 'CASH    '             TO WS-GL-DR-ACCOUNT
           MOVE 'SUSPENSE'             TO WS-GL-CR-ACCOUNT
           MOVE CA-PY-AMOUNT           TO WS-GL-AMOUNT
           MOVE 0                      TO WS-GL-ACCT-ID
           PERFORM 8300-POST-LEDGER

           MOVE WS-SUSP-ID             TO CA-PY-SUSP-ID
           MOVE 0                      TO CA-PY-ACCOUNT-ID
                                          CA-PY-DISTRIBUTED
           MOVE CA-PY-AMOUNT           TO CA-PY-UNAPPLIED
           MOVE 'SUSP'                 TO CA-PY-STATUS
           SET CA-RC-WARNING           TO TRUE
           MOVE WS-SUSP-REASON         TO CA-REASON-CODE
           MOVE WS-SUSP-ID             TO WS-KEY-DISP
           STRING 'Receipt held in suspense item ' WS-KEY-DISP
                  ' reason ' WS-SUSP-REASON
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * PAYREV - reverse a receipt
      ******************************************************************
       2200-REVERSE.
           IF CA-PY-MONEY-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MONEYID '          TO CA-REASON-CODE
              MOVE 'Money id is required for a reversal'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PAY-013 Only the four business reversal reasons
           IF CA-PY-REVERSAL-REASON NOT = 'NSF     '
              AND CA-PY-REVERSAL-REASON NOT = 'WRONGACC'
              AND CA-PY-REVERSAL-REASON NOT = 'REFUND  '
              AND CA-PY-REVERSAL-REASON NOT = 'STOPPYMT'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'REVRSN  '          TO CA-REASON-CODE
              STRING 'Reversal reason ' CA-PY-REVERSAL-REASON
                     ' invalid - NSF WRONGACC REFUND STOPPYMT'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PY-MONEY-ID         TO WS-MONEY-ID
           PERFORM 4000-READ-RECEIPT
      * BR-BIL-PAY-019 Reversal is final
           IF RCP-RECEIPT-STATUS = 'RVSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'RCPRVSD '          TO CA-REASON-CODE
              MOVE 'Receipt has already been reversed'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8500-CHECK-AUTHORITY
           MOVE RCP-RECEIPT-STATUS     TO WS-OLD-STATUS
           MOVE RCP-ACCOUNT-ID         TO WS-ACCOUNT-ID

           IF RCP-RECEIPT-STATUS = 'SUSP'
              PERFORM 2250-REVERSE-SUSPENSE
           ELSE
      * BR-BIL-PAY-015 Items first, then the cash entry
              PERFORM 8250-UNDO-DISTRIBUTIONS
              PERFORM 4060-READ-ACCOUNT-BY-ID
              COMPUTE WS-UNAPPLIED-DELTA = 0 - RCP-RECEIPT-AMOUNT
              PERFORM 4300-UPDATE-ACCOUNT-UNAPPLIED
           END-IF
           PERFORM 2270-REVERSE-CASH-ENTRIES
           PERFORM 4150-MARK-REVERSED

           INITIALIZE WS-AUD-CA
           MOVE 'RECEIPT '             TO AUA-AU-ENTITY
           MOVE 'STAT'                 TO AUA-AU-ACTION
           MOVE 'RECEIPT_STATUS'       TO AUA-AU-FIELD
           MOVE WS-OLD-STATUS          TO AUA-AU-BEFORE
           STRING 'RVSD ' CA-PY-REVERSAL-REASON
                  DELIMITED BY SIZE INTO AUA-AU-AFTER
           PERFORM 8600-WRITE-AUDIT

           IF WS-ACCOUNT-ID > 0
      * BR-BIL-PAY-016 NSF fee is a business option
              IF CA-PY-REVERSAL-REASON = 'NSF     '
                 PERFORM 2300-NSF-FEE
              END-IF
      * BR-BIL-PAY-017 Bounced or stopped money marks the instrument
              IF RCP-INSTR-ID > 0
                 AND (CA-PY-REVERSAL-REASON = 'NSF     '
                   OR CA-PY-REVERSAL-REASON = 'STOPPYMT')
                 PERFORM 8700-INSTRUMENT-FAILURE
              END-IF
           END-IF

           MOVE WS-ACCOUNT-ID          TO CA-PY-ACCOUNT-ID
           MOVE RCP-RECEIPT-AMOUNT     TO CA-PY-AMOUNT
           MOVE 0                      TO CA-PY-DISTRIBUTED
                                          CA-PY-UNAPPLIED
           MOVE 'RVSD'                 TO CA-PY-STATUS
           PERFORM 4600-LOAD-DISTRIBUTIONS.

      ******************************************************************
      * A receipt still in suspense reverses straight out of suspense
       2250-REVERSE-SUSPENSE.
           PERFORM 4800-READ-SUSPENSE-BY-MONEY
           IF SUS-SUSP-STATUS NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'SUSPSTAT'          TO CA-REASON-CODE
              STRING 'Suspense item status ' SUS-SUSP-STATUS
                     ' cannot be reversed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'RVSD'                 TO SUS-SUSP-STATUS
           MOVE 0                      TO SUS-MATCHED-ACCOUNT-ID
           PERFORM 4850-RESOLVE-SUSPENSE
           MOVE SUS-SUSP-ID            TO CA-PY-SUSP-ID.

      ******************************************************************
      * BR-BIL-PAY-015 Every entry that booked this cash (PAYRCV, or
      * PAYSUSP plus PAYMATCH for matched suspense) is reversed through
      * GLREVS so the ledger shows the mirror of each original.
       2270-REVERSE-CASH-ENTRIES.
           PERFORM 4050-LOAD-CASH-TXNS
           IF WS-TX-COUNT = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'NOCASHTX'          TO CA-REASON-CODE
              MOVE 'No cash ledger entry found for this receipt'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM VARYING WS-TX-IX FROM 1 BY 1
                     UNTIL WS-TX-IX > WS-TX-COUNT
              INITIALIZE WS-GLP-CA
              MOVE 'GLREVS  '          TO GLA-REQUEST-ID
              MOVE CA-USER-ID          TO GLA-USER-ID
              MOVE WS-BUS-DATE         TO GLA-BUS-DATE
              MOVE CA-TRACE-ID         TO GLA-TRACE-ID
              MOVE WS-PROGRAM-ID       TO GLA-CALLER-PGM
              MOVE WS-TX-ID(WS-TX-IX)  TO GLA-GL-REVERSAL-OF
              CALL WS-GLP-PGM USING WS-GLP-CA
              IF GLA-RETURN-CODE >= 12
                 MOVE GLA-RETURN-CODE  TO CA-RETURN-CODE
                 MOVE GLA-REASON-CODE  TO CA-REASON-CODE
                 MOVE GLA-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-PERFORM.

      ******************************************************************
      * BR-BIL-PAY-016 Fee amount is the CODE_VALUE of the first active
      * NSFFEE row (e.g. '25.00'). It is raised as a one-time charge on
      * the account's current policy period and scheduled straight away.
       2300-NSF-FEE.
           PERFORM 4700-READ-NSF-FEE
           IF WS-FEE-AMOUNT NOT > 0
              EXIT PARAGRAPH
           END-IF
           PERFORM 4750-FIND-FEE-PERIOD
           IF WS-FEE-PP-ID = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NSFNOPP '          TO CA-REASON-CODE
              MOVE 'Reversed - no policy period to carry the NSF fee'
                                       TO CA-MESSAGE
              EXIT PARAGRAPH
           END-IF
           MOVE 'CHG     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-ID
           MOVE WS-NEXT-ID             TO CHG-CHARGE-ID
           PERFORM 4760-INSERT-FEE-CHARGE

           MOVE 'NSFFEE  '             TO WS-GL-TXN-TYPE
           MOVE 'UNBILLED'             TO WS-GL-DR-ACCOUNT
           MOVE 'FEEREV  '             TO WS-GL-CR-ACCOUNT
           MOVE WS-FEE-AMOUNT          TO WS-GL-AMOUNT
           MOVE WS-ACCOUNT-ID          TO WS-GL-ACCT-ID
           MOVE 'CHRG'                 TO WS-GL-REF-TYPE
           MOVE CHG-CHARGE-ID          TO WS-GL-REF-ID
           PERFORM 8300-POST-LEDGER
           MOVE SPACES                 TO WS-GL-REF-TYPE

           INITIALIZE WS-SCH-CA
           MOVE 'SCHCHG  '             TO SCA-REQUEST-ID
           PERFORM 8900-FILL-SCH-HEADER
           MOVE CHG-CHARGE-ID          TO SCA-SC-CHARGE-ID
           MOVE WS-FEE-PP-ID           TO SCA-SC-PP-ID
           SET SCA-SC-IS-ONE-TIME      TO TRUE
           CALL WS-SCH-PGM USING WS-SCH-CA
           IF SCA-RETURN-CODE >= 12
              MOVE SCA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SCA-REASON-CODE     TO CA-REASON-CODE
              MOVE SCA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           INITIALIZE WS-ACT-CA
           MOVE 'ACCTBAL '             TO ACA-REQUEST-ID
           MOVE CA-USER-ID             TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE CA-TRACE-ID            TO ACA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE WS-ACCOUNT-ID          TO ACA-AC-ACCOUNT-ID
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF ACA-RETURN-CODE >= 12
              MOVE ACA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE ACA-REASON-CODE     TO CA-REASON-CODE
              MOVE ACA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * PAYINQ - receipt with its distributions
      ******************************************************************
       2400-INQUIRE.
           IF CA-PY-MONEY-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MONEYID '          TO CA-REASON-CODE
              MOVE 'Money id is required for an enquiry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PY-MONEY-ID         TO WS-MONEY-ID
           PERFORM 4000-READ-RECEIPT
           PERFORM 2900-RETURN-RECEIPT
           IF RCP-ACCOUNT-ID > 0
              MOVE RCP-ACCOUNT-ID      TO ACT-ACCOUNT-ID
              EXEC SQL
                 SELECT ACCOUNT_NUMBER
                   INTO :ACT-ACCOUNT-NUMBER
                   FROM BILDB.BIL_ACCOUNT
                  WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE ACT-ACCOUNT-NUMBER TO CA-PY-ACCOUNT-NUMBER
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '2400-INQUIRE' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF
           IF RCP-RECEIPT-STATUS = 'SUSP'
              PERFORM 4800-READ-SUSPENSE-BY-MONEY
              MOVE SUS-SUSP-ID         TO CA-PY-SUSP-ID
           END-IF
           PERFORM 4600-LOAD-DISTRIBUTIONS.

      ******************************************************************
      * PAYMAT - move a suspense receipt onto an account and apply it
      ******************************************************************
       2600-MATCH-SUSPENSE.
           IF CA-PY-SUSP-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SUSPID  '          TO CA-REASON-CODE
              MOVE 'Suspense id is required to match'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PY-ACCOUNT-NUMBER = SPACES AND CA-PY-ACCOUNT-ID = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACCTREQD'          TO CA-REASON-CODE
              MOVE 'Account number is required to match suspense'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PY-SUSP-ID          TO SUS-SUSP-ID
           PERFORM 4810-READ-SUSPENSE-BY-ID
      * BR-BIL-PAY-018 Only OPEN suspense can be matched
           IF SUS-SUSP-STATUS NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'SUSPSTAT'          TO CA-REASON-CODE
              STRING 'Suspense item status ' SUS-SUSP-STATUS
                     ' - only OPEN items can be matched'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SUS-MONEY-ID           TO WS-MONEY-ID
                                          CA-PY-MONEY-ID
           PERFORM 4000-READ-RECEIPT
           IF RCP-RECEIPT-STATUS NOT = 'SUSP'
              SET CA-RC-STATE          TO TRUE
              MOVE 'RCPSTAT '          TO CA-REASON-CODE
              STRING 'Receipt status ' RCP-RECEIPT-STATUS
                     ' is not in suspense'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           IF CA-PY-ACCOUNT-ID > 0
              MOVE CA-PY-ACCOUNT-ID    TO WS-ACCOUNT-ID
              PERFORM 4060-READ-ACCOUNT-BY-ID
           ELSE
              PERFORM 4070-READ-ACCOUNT-BY-NUMBER
           END-IF
           IF SQLCODE = 100
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'ACTNOTFD'          TO CA-REASON-CODE
              STRING 'Account ' CA-PY-ACCOUNT-NUMBER ' not found'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF ACT-ACCOUNT-STATUS = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACCTCLSD'          TO CA-REASON-CODE
              MOVE 'Suspense cannot be matched to a closed account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           MOVE 'MTCH'                 TO SUS-SUSP-STATUS
           MOVE WS-ACCOUNT-ID          TO SUS-MATCHED-ACCOUNT-ID
           PERFORM 4850-RESOLVE-SUSPENSE
           PERFORM 4160-ASSIGN-RECEIPT-ACCOUNT

           MOVE 'PAYMATCH'             TO WS-GL-TXN-TYPE
           MOVE 'SUSPENSE'             TO WS-GL-DR-ACCOUNT
           MOVE 'UNAPPLD '             TO WS-GL-CR-ACCOUNT
           MOVE RCP-RECEIPT-AMOUNT     TO WS-GL-AMOUNT
           MOVE WS-ACCOUNT-ID          TO WS-GL-ACCT-ID
           PERFORM 8300-POST-LEDGER
           MOVE RCP-RECEIPT-AMOUNT     TO WS-UNAPPLIED-DELTA
           PERFORM 4300-UPDATE-ACCOUNT-UNAPPLIED

           INITIALIZE WS-AUD-CA
           MOVE 'SUSPENSE'             TO AUA-AU-ENTITY
           MOVE 'STAT'                 TO AUA-AU-ACTION
           MOVE 'MATCHED_ACCOUNT_ID'   TO AUA-AU-FIELD
           MOVE 'OPEN'                 TO AUA-AU-BEFORE
           MOVE ACT-ACCOUNT-NUMBER     TO AUA-AU-AFTER
           PERFORM 8600-WRITE-AUDIT

      * The invoice the payer quoted is still the preferred target
           IF RCP-INVOICE-NUMBER NOT = SPACES
              MOVE RCP-INVOICE-NUMBER  TO INV-INVOICE-NUMBER
              PERFORM 4080-READ-INVOICE
              IF SQLCODE = 0 AND INV-ACCOUNT-ID = WS-ACCOUNT-ID
                 MOVE INV-INVOICE-ID   TO WS-TARGET-INVOICE-ID
              END-IF
           END-IF
           MOVE RCP-RECEIPT-AMOUNT     TO CA-PY-AMOUNT
           PERFORM 2800-APPLY-TO-ACCOUNT.

      ******************************************************************
      * Shared by PAYRCV and PAYMAT once money sits unapplied on
      * WS-ACCOUNT-ID: hold check, distribution, delinquency exit.
       2800-APPLY-TO-ACCOUNT.
           MOVE WS-ACCOUNT-ID          TO CA-PY-ACCOUNT-ID
      * BR-BIL-PAY-010 Payment hold: keep the money unapplied
           PERFORM 8100-CHECK-PAYMENT-HOLD
           IF HLA-HL-IS-ACTIVE
              PERFORM 4000-READ-RECEIPT
              PERFORM 2900-RETURN-RECEIPT
              SET CA-RC-WARNING        TO TRUE
              MOVE 'PYMTHOLD'          TO CA-REASON-CODE
              MOVE 'Payment hold active - receipt held unapplied'
                                       TO CA-MESSAGE
              EXIT PARAGRAPH
           END-IF

           PERFORM 4500-READ-PAST-DUE
           MOVE WS-PASTDUE-AFTER       TO WS-PASTDUE-BEFORE
           PERFORM 8200-DISTRIBUTE
           PERFORM 4500-READ-PAST-DUE
      * BR-BIL-PAY-012 Past due cleared -> close delinquency
           IF WS-PASTDUE-BEFORE > 0 AND WS-PASTDUE-AFTER NOT > 0
              PERFORM 8400-DELINQUENCY-EXIT
           END-IF

           PERFORM 4000-READ-RECEIPT
           PERFORM 2900-RETURN-RECEIPT
           PERFORM 4600-LOAD-DISTRIBUTIONS.

      ******************************************************************
       2900-RETURN-RECEIPT.
           MOVE RCP-MONEY-ID           TO CA-PY-MONEY-ID
           MOVE RCP-ACCOUNT-ID         TO CA-PY-ACCOUNT-ID
           MOVE RCP-RECEIVED-DATE      TO CA-PY-RECEIVED-DATE
           MOVE RCP-RECEIPT-AMOUNT     TO CA-PY-AMOUNT
           MOVE RCP-INSTR-ID           TO CA-PY-INSTR-ID
           MOVE RCP-PAYMENT-METHOD     TO CA-PY-METHOD
           MOVE RCP-REF-NUMBER         TO CA-PY-REF-NUMBER
           MOVE RCP-INVOICE-NUMBER     TO CA-PY-INVOICE-NUMBER
           MOVE RCP-RECEIPT-SOURCE     TO CA-PY-SOURCE
           MOVE RCP-RECEIPT-STATUS     TO CA-PY-STATUS
           MOVE RCP-REVERSAL-REASON    TO CA-PY-REVERSAL-REASON
           MOVE RCP-BATCH-REF          TO CA-PY-BATCH-REF
           MOVE RCP-UNAPPLIED-AMOUNT   TO CA-PY-UNAPPLIED
           IF RCP-RECEIPT-STATUS = 'RVSD'
              MOVE 0                   TO CA-PY-DISTRIBUTED
           ELSE
              COMPUTE CA-PY-DISTRIBUTED = RCP-RECEIPT-AMOUNT
                                        - RCP-UNAPPLIED-AMOUNT
           END-IF.

      ******************************************************************
      * Validation
      ******************************************************************
       3000-VALIDATE-RECEIPT.
      * BR-BIL-PAY-001 Positive amount only
           IF CA-PY-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AMOUNT  '          TO CA-REASON-CODE
              MOVE 'Receipt amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PAY-002 Method and source from the code tables
           MOVE 'PAYMTHD '             TO CDA-CD-TABLE
           MOVE CA-PY-METHOD           TO CDA-CD-CODE
           PERFORM 8800-VALIDATE-CODE
           IF NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADMETH '          TO CA-REASON-CODE
              STRING 'Payment method ' CA-PY-METHOD ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'RCPTSRC '             TO CDA-CD-TABLE
           MOVE CA-PY-SOURCE           TO CDA-CD-CODE
           PERFORM 8800-VALIDATE-CODE
           IF NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADSRC  '          TO CA-REASON-CODE
              STRING 'Receipt source ' CA-PY-SOURCE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PAY-003 Valid date, never ahead of the business day
           IF CA-PY-RECEIVED-DATE = SPACES OR LOW-VALUES
              MOVE WS-BUS-DATE         TO CA-PY-RECEIVED-DATE
           END-IF
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE CA-PY-RECEIVED-DATE    TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADDATE '          TO CA-REASON-CODE
              STRING 'Received date ' CA-PY-RECEIVED-DATE
                     ' is not a valid date'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PY-RECEIVED-DATE > WS-BUS-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FUTDATE '          TO CA-REASON-CODE
              STRING 'Received date ' CA-PY-RECEIVED-DATE
                     ' is after business date ' WS-BUS-DATE
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PAY-004 Account id, then account number, then invoice
       3100-RESOLVE-ACCOUNT.
           SET WS-ACCT-UNRESOLVED      TO TRUE
           MOVE 'NOACCT  '             TO WS-SUSP-REASON
           EVALUATE TRUE
              WHEN CA-PY-ACCOUNT-ID > 0
                 MOVE CA-PY-ACCOUNT-ID TO WS-ACCOUNT-ID
                 PERFORM 4060-READ-ACCOUNT-BY-ID
                 IF SQLCODE = 0
                    SET WS-ACCT-RESOLVED TO TRUE
                 END-IF
              WHEN CA-PY-ACCOUNT-NUMBER NOT = SPACES
                 PERFORM 4070-READ-ACCOUNT-BY-NUMBER
                 IF SQLCODE = 0
                    SET WS-ACCT-RESOLVED TO TRUE
                 END-IF
           END-EVALUATE

           IF CA-PY-INVOICE-NUMBER NOT = SPACES
              MOVE CA-PY-INVOICE-NUMBER TO INV-INVOICE-NUMBER
              PERFORM 4080-READ-INVOICE
              IF SQLCODE = 0
                 EVALUATE TRUE
                    WHEN WS-ACCT-UNRESOLVED
                       MOVE INV-ACCOUNT-ID TO WS-ACCOUNT-ID
                       PERFORM 4060-READ-ACCOUNT-BY-ID
                       IF SQLCODE = 0
                          SET WS-ACCT-RESOLVED TO TRUE
                          MOVE INV-INVOICE-ID
                                       TO WS-TARGET-INVOICE-ID
                       END-IF
                    WHEN INV-ACCOUNT-ID = WS-ACCOUNT-ID
                       MOVE INV-INVOICE-ID TO WS-TARGET-INVOICE-ID
      * BR-BIL-PAY-005 Quoted invoice on another account is ignored
                    WHEN OTHER
                       MOVE 0          TO WS-TARGET-INVOICE-ID
                 END-EVALUATE
              END-IF
           END-IF

      * BR-BIL-PAY-006 A closed account cannot take new money
           IF WS-ACCT-RESOLVED AND ACT-ACCOUNT-STATUS = 'CLSD'
              SET WS-ACCT-UNRESOLVED   TO TRUE
              MOVE 'ACCTCLSD'          TO WS-SUSP-REASON
           END-IF
           IF WS-ACCT-UNRESOLVED
              MOVE 0                   TO WS-ACCOUNT-ID
                                          WS-TARGET-INVOICE-ID
           END-IF.

      ******************************************************************
      * BR-BIL-PAY-007 A live receipt with the same account, reference,
      * amount and received date is a resubmission, not new money
       3200-CHECK-DUPLICATE.
           IF CA-PY-REF-NUMBER = SPACES
              EXIT PARAGRAPH
           END-IF
           MOVE WS-ACCOUNT-ID          TO RCP-ACCOUNT-ID
           MOVE CA-PY-REF-NUMBER       TO RCP-REF-NUMBER
           MOVE CA-PY-AMOUNT           TO RCP-RECEIPT-AMOUNT
           MOVE CA-PY-RECEIVED-DATE    TO RCP-RECEIVED-DATE
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-DUP-COUNT
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE ACCOUNT_ID     = :RCP-ACCOUNT-ID
                 AND REF_NUMBER     = :RCP-REF-NUMBER
                 AND RECEIPT_AMOUNT = :RCP-RECEIPT-AMOUNT
                 AND RECEIVED_DATE  = :RCP-RECEIVED-DATE
                 AND RECEIPT_STATUS <> 'RVSD'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3200-CHECK-DUPLICATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DUP-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DUPPYMT '          TO CA-REASON-CODE
              STRING 'Duplicate receipt ref ' CA-PY-REF-NUMBER
                     ' already recorded'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PAY-008 The paying instrument must be the account's own
       3300-CHECK-INSTRUMENT.
           EXEC SQL
              SELECT ACCOUNT_ID
                INTO :WS-INSTR-ACCOUNT-ID
                FROM BILDB.BIL_PAYMENT_INSTRUMENT
               WHERE INSTR_ID = :CA-PY-INSTR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF WS-INSTR-ACCOUNT-ID NOT = WS-ACCOUNT-ID
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'INSACCT '    TO CA-REASON-CODE
                    MOVE 'Payment instrument belongs to another account'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INSNOTFD'       TO CA-REASON-CODE
                 MOVE 'Payment instrument not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3300-CHECK-INSTRUMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Db2 access
      ******************************************************************
       4000-READ-RECEIPT.
           MOVE WS-MONEY-ID            TO RCP-MONEY-ID
           EXEC SQL
              SELECT ACCOUNT_ID, RECEIVED_DATE, RECEIPT_AMOUNT,
                     UNAPPLIED_AMOUNT, INSTR_ID, PAYMENT_METHOD,
                     REF_NUMBER, INVOICE_NUMBER, RECEIPT_SOURCE,
                     RECEIPT_STATUS, REVERSAL_REASON, BATCH_REF
                INTO :RCP-ACCOUNT-ID, :RCP-RECEIVED-DATE,
                     :RCP-RECEIPT-AMOUNT, :RCP-UNAPPLIED-AMOUNT,
                     :RCP-INSTR-ID, :RCP-PAYMENT-METHOD,
                     :RCP-REF-NUMBER, :RCP-INVOICE-NUMBER,
                     :RCP-RECEIPT-SOURCE, :RCP-RECEIPT-STATUS,
                     :RCP-REVERSAL-REASON, :RCP-BATCH-REF
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE MONEY_ID = :RCP-MONEY-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'RCPNOTFD'       TO CA-REASON-CODE
                 MOVE WS-MONEY-ID      TO WS-KEY-DISP
                 STRING 'Receipt ' WS-KEY-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-RECEIPT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Unreversed entries that booked the receipt's cash
       4050-LOAD-CASH-TXNS.
           MOVE 0                      TO WS-TX-COUNT
           SET WS-FETCH-MORE           TO TRUE
           EXEC SQL OPEN CSR-CASH-TXNS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4050-LOAD-CASH-TXNS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-TXN-CSR-OPEN         TO TRUE
           PERFORM UNTIL WS-FETCH-END OR WS-TX-COUNT = 5
              EXEC SQL
                 FETCH CSR-CASH-TXNS INTO :WS-FETCH-TXN-ID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-TX-COUNT
                    MOVE WS-FETCH-TXN-ID TO WS-TX-ID(WS-TX-COUNT)
                 WHEN 100
                    SET WS-FETCH-END   TO TRUE
                 WHEN OTHER
                    MOVE '4050-LOAD-CASH-TXNS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-CASH-TXNS END-EXEC
           MOVE 'N'                    TO WS-TXN-CSR-SW.

      ******************************************************************
       4060-READ-ACCOUNT-BY-ID.
           MOVE WS-ACCOUNT-ID          TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_ID, ACCOUNT_NUMBER, ACCOUNT_STATUS,
                     VERSION_NO
                INTO :ACT-ACCOUNT-ID, :ACT-ACCOUNT-NUMBER,
                     :ACT-ACCOUNT-STATUS, :ACT-VERSION-NO
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           PERFORM 4090-CHECK-ACCOUNT-READ.

      ******************************************************************
       4070-READ-ACCOUNT-BY-NUMBER.
           MOVE CA-PY-ACCOUNT-NUMBER   TO ACT-ACCOUNT-NUMBER
           EXEC SQL
              SELECT ACCOUNT_ID, ACCOUNT_NUMBER, ACCOUNT_STATUS,
                     VERSION_NO
                INTO :ACT-ACCOUNT-ID, :ACT-ACCOUNT-NUMBER,
                     :ACT-ACCOUNT-STATUS, :ACT-VERSION-NO
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_NUMBER = :ACT-ACCOUNT-NUMBER
           END-EXEC
           PERFORM 4090-CHECK-ACCOUNT-READ.

      ******************************************************************
       4080-READ-INVOICE.
           EXEC SQL
              SELECT INVOICE_ID, ACCOUNT_ID
                INTO :INV-INVOICE-ID, :INV-ACCOUNT-ID
                FROM BILDB.BIL_INVOICE
               WHERE INVOICE_NUMBER = :INV-INVOICE-NUMBER
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4080-READ-INVOICE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Not found is left in SQLCODE for the caller to decide
       4090-CHECK-ACCOUNT-READ.
           EVALUATE SQLCODE
              WHEN 0
                 MOVE ACT-ACCOUNT-ID   TO WS-ACCOUNT-ID
                 MOVE ACT-VERSION-NO   TO WS-ACCT-VERSION
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4090-CHECK-ACCOUNT-READ' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-RECEIPT.
           MOVE WS-MONEY-ID            TO RCP-MONEY-ID
           MOVE CA-PY-RECEIVED-DATE    TO RCP-RECEIVED-DATE
           MOVE CA-PY-AMOUNT           TO RCP-RECEIPT-AMOUNT
                                          RCP-UNAPPLIED-AMOUNT
           MOVE CA-PY-INSTR-ID         TO RCP-INSTR-ID
           MOVE CA-PY-METHOD           TO RCP-PAYMENT-METHOD
           MOVE CA-PY-REF-NUMBER       TO RCP-REF-NUMBER
           MOVE CA-PY-INVOICE-NUMBER   TO RCP-INVOICE-NUMBER
           MOVE CA-PY-SOURCE           TO RCP-RECEIPT-SOURCE
           MOVE SPACES                 TO RCP-REVERSAL-REASON
           MOVE CA-PY-BATCH-REF        TO RCP-BATCH-REF
           MOVE CA-USER-ID             TO RCP-CREATE-USER
           IF RCP-ACCOUNT-ID = 0
              MOVE 0                   TO RCP-INSTR-ID
           END-IF
           EXEC SQL
              INSERT INTO BILDB.BIL_PAYMENT_RECEIPT
                    (MONEY_ID, ACCOUNT_ID, RECEIVED_DATE,
                     RECEIPT_AMOUNT, UNAPPLIED_AMOUNT, INSTR_ID,
                     PAYMENT_METHOD, REF_NUMBER, INVOICE_NUMBER,
                     RECEIPT_SOURCE, RECEIPT_STATUS, REVERSAL_REASON,
                     BATCH_REF, CREATE_USER, CREATE_TS, UPDATE_TS)
              VALUES (:RCP-MONEY-ID, :RCP-ACCOUNT-ID,
                      :RCP-RECEIVED-DATE, :RCP-RECEIPT-AMOUNT,
                      :RCP-UNAPPLIED-AMOUNT, :RCP-INSTR-ID,
                      :RCP-PAYMENT-METHOD, :RCP-REF-NUMBER,
                      :RCP-INVOICE-NUMBER, :RCP-RECEIPT-SOURCE,
                      :RCP-RECEIPT-STATUS, :RCP-REVERSAL-REASON,
                      :RCP-BATCH-REF, :RCP-CREATE-USER,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE 'Money id already used - sequence RCP'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-INSERT-RECEIPT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4150-MARK-REVERSED.
           MOVE CA-PY-REVERSAL-REASON  TO RCP-REVERSAL-REASON
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_RECEIPT
                 SET RECEIPT_STATUS   = 'RVSD',
                     REVERSAL_REASON  = :RCP-REVERSAL-REASON,
                     UNAPPLIED_AMOUNT = 0,
                     UPDATE_TS        = CURRENT TIMESTAMP
               WHERE MONEY_ID         = :RCP-MONEY-ID
                 AND RECEIPT_STATUS  <> 'RVSD'
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Receipt reversed by another task'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4150-MARK-REVERSED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4160-ASSIGN-RECEIPT-ACCOUNT.
           MOVE WS-ACCOUNT-ID          TO RCP-ACCOUNT-ID
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_RECEIPT
                 SET ACCOUNT_ID     = :RCP-ACCOUNT-ID,
                     RECEIPT_STATUS = 'RCVD',
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE MONEY_ID       = :RCP-MONEY-ID
                 AND RECEIPT_STATUS = 'SUSP'
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 MOVE 'RCVD'           TO RCP-RECEIPT-STATUS
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Suspense receipt changed by another task'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4160-ASSIGN-RECEIPT-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Signed WS-UNAPPLIED-DELTA onto the account, optimistic lock
       4300-UPDATE-ACCOUNT-UNAPPLIED.
           MOVE WS-ACCOUNT-ID          TO ACT-ACCOUNT-ID
           MOVE WS-ACCT-VERSION        TO ACT-VERSION-NO
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET UNAPPLIED_AMT = UNAPPLIED_AMT
                                   + :WS-UNAPPLIED-DELTA,
                     UPDATE_TS     = CURRENT TIMESTAMP,
                     VERSION_NO    = VERSION_NO + 1
               WHERE ACCOUNT_ID    = :ACT-ACCOUNT-ID
                 AND VERSION_NO    = :ACT-VERSION-NO
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 ADD 1                 TO WS-ACCT-VERSION
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Account changed by another task - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4300-UPDATE-ACCOUNT-UNAPPLIED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-INSERT-SUSPENSE.
           MOVE WS-SUSP-ID             TO SUS-SUSP-ID
           MOVE WS-MONEY-ID            TO SUS-MONEY-ID
           MOVE CA-PY-AMOUNT           TO SUS-SUSP-AMOUNT
           MOVE WS-REF-TEXT            TO SUS-REF-TEXT-TEXT
           MOVE 80                     TO SUS-REF-TEXT-LEN
           MOVE WS-SUSP-REASON         TO SUS-SUSP-REASON
           MOVE 'OPEN'                 TO SUS-SUSP-STATUS
           MOVE 0                      TO SUS-MATCHED-ACCOUNT-ID
           MOVE WS-BUS-DATE            TO SUS-CREATE-DATE
           MOVE -1                     TO SUS-RESOLVED-DATE-IND
           MOVE SPACES                 TO SUS-RESOLVED-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_SUSPENSE
                    (SUSP_ID, MONEY_ID, SUSP_AMOUNT, REF_TEXT,
                     SUSP_REASON, SUSP_STATUS, MATCHED_ACCOUNT_ID,
                     CREATE_DATE, RESOLVED_DATE, RESOLVED_USER)
              VALUES (:SUS-SUSP-ID, :SUS-MONEY-ID, :SUS-SUSP-AMOUNT,
                      :SUS-REF-TEXT, :SUS-SUSP-REASON,
                      :SUS-SUSP-STATUS, :SUS-MATCHED-ACCOUNT-ID,
                      :SUS-CREATE-DATE,
                      :SUS-RESOLVED-DATE :SUS-RESOLVED-DATE-IND,
                      :SUS-RESOLVED-USER)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE 'Suspense id already used - sequence SUS'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4400-INSERT-SUSPENSE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Past due as BILDLQSV sees it: open DUE items due before today
       4500-READ-PAST-DUE.
           EXEC SQL
              SELECT COALESCE(SUM(ITEM_AMOUNT - PAID_AMOUNT
                                  - WRITEOFF_AMOUNT), 0)
                INTO :WS-PASTDUE-AFTER
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE ACCOUNT_ID  = :WS-ACCOUNT-ID
                 AND ITEM_STATUS = 'DUE '
                 AND DUE_DATE    < :WS-BUS-DATE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-READ-PAST-DUE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Up to 20 distribution rows (reversal mirrors included) for the
      * receipt; RC 04 MOREDIST tells the caller the list is cut short
       4600-LOAD-DISTRIBUTIONS.
           MOVE 0                      TO CA-PY-DIST-COUNT
                                          WS-DIST-ROWS
           SET WS-FETCH-MORE           TO TRUE
           EXEC SQL OPEN CSR-RCPT-DISTS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4600-LOAD-DISTRIBUTIONS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-DIST-CSR-OPEN        TO TRUE
           PERFORM UNTIL WS-FETCH-END
              EXEC SQL
                 FETCH CSR-RCPT-DISTS
                  INTO :WS-DR-ITEM-ID, :WS-DR-INVOICE-NO,
                       :WS-DR-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-DIST-ROWS
                    IF CA-PY-DIST-COUNT < 20
                       ADD 1           TO CA-PY-DIST-COUNT
                       MOVE WS-DR-ITEM-ID
                         TO CA-PY-D-ITEM-ID(CA-PY-DIST-COUNT)
                       MOVE WS-DR-INVOICE-NO
                         TO CA-PY-D-INVOICE-NO(CA-PY-DIST-COUNT)
                       MOVE WS-DR-AMOUNT
                         TO CA-PY-D-AMOUNT(CA-PY-DIST-COUNT)
                    END-IF
                 WHEN 100
                    SET WS-FETCH-END   TO TRUE
                 WHEN OTHER
                    MOVE '4600-LOAD-DISTRIBUTIONS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-RCPT-DISTS END-EXEC
           MOVE 'N'                    TO WS-DIST-CSR-SW
           IF WS-DIST-ROWS > 20 AND CA-RC-OK
              SET CA-RC-WARNING        TO TRUE
              MOVE 'MOREDIST'          TO CA-REASON-CODE
              MOVE WS-DIST-ROWS        TO WS-KEY-DISP
              STRING 'First 20 of ' WS-KEY-DISP ' distributions shown'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       4700-READ-NSF-FEE.
           MOVE 0                      TO WS-FEE-AMOUNT
           MOVE 'NSFFEE  '             TO COD-TABLE-NAME
           EXEC SQL
              SELECT CODE_VALUE
                INTO :COD-CODE-VALUE
                FROM BILDB.BIL_CODE_TABLE
               WHERE TABLE_NAME  = :COD-TABLE-NAME
                 AND ACTIVE_FLAG = 'Y'
               ORDER BY SORT_ORDER
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF FUNCTION TEST-NUMVAL(COD-CODE-VALUE) = 0
                    COMPUTE WS-FEE-AMOUNT ROUNDED =
                            FUNCTION NUMVAL(COD-CODE-VALUE)
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4700-READ-NSF-FEE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Fee rides on the in-force period, else the latest period
       4750-FIND-FEE-PERIOD.
           MOVE 0                      TO WS-FEE-PP-ID
           EXEC SQL
              SELECT PP_ID
                INTO :WS-FEE-PP-ID
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE ACCOUNT_ID = :WS-ACCOUNT-ID
               ORDER BY CASE PP_STATUS WHEN 'INFC' THEN 0 ELSE 1 END,
                        EFF_DATE DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4750-FIND-FEE-PERIOD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4760-INSERT-FEE-CHARGE.
           MOVE WS-FEE-PP-ID           TO CHG-PP-ID
           MOVE WS-ACCOUNT-ID          TO CHG-ACCOUNT-ID
           MOVE 'NSFFEE  '             TO CHG-CHARGE-PATTERN
           MOVE 'ONETIME '             TO CHG-BILLING-INSTR
           MOVE WS-FEE-AMOUNT          TO CHG-CHARGE-AMOUNT
           MOVE WS-BUS-DATE            TO CHG-CHARGE-DATE
                                          CHG-EFF-DATE
           MOVE 'OPEN'                 TO CHG-CHARGE-STATUS
           MOVE WS-MONEY-ID            TO WS-KEY-DISP
           MOVE SPACES                 TO CHG-SOURCE-REF
           STRING 'NSF RCPT ' WS-KEY-DISP
                  DELIMITED BY SIZE INTO CHG-SOURCE-REF
           MOVE CA-USER-ID             TO CHG-CREATE-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_CHARGE
                    (CHARGE_ID, PP_ID, ACCOUNT_ID, CHARGE_PATTERN,
                     BILLING_INSTR, CHARGE_AMOUNT, CHARGE_DATE,
                     EFF_DATE, CHARGE_STATUS, SOURCE_REF,
                     CREATE_USER, CREATE_TS)
              VALUES (:CHG-CHARGE-ID, :CHG-PP-ID, :CHG-ACCOUNT-ID,
                      :CHG-CHARGE-PATTERN, :CHG-BILLING-INSTR,
                      :CHG-CHARGE-AMOUNT, :CHG-CHARGE-DATE,
                      :CHG-EFF-DATE, :CHG-CHARGE-STATUS,
                      :CHG-SOURCE-REF, :CHG-CREATE-USER,
                      CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE 'Charge id already used - sequence CHG'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4760-INSERT-FEE-CHARGE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4800-READ-SUSPENSE-BY-MONEY.
           EXEC SQL
              SELECT SUSP_ID, MONEY_ID, SUSP_AMOUNT, SUSP_STATUS
                INTO :SUS-SUSP-ID, :SUS-MONEY-ID, :SUS-SUSP-AMOUNT,
                     :SUS-SUSP-STATUS
                FROM BILDB.BIL_SUSPENSE
               WHERE MONEY_ID = :WS-MONEY-ID
               ORDER BY SUSP_ID DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           PERFORM 4820-CHECK-SUSPENSE-READ.

      ******************************************************************
       4810-READ-SUSPENSE-BY-ID.
           EXEC SQL
              SELECT SUSP_ID, MONEY_ID, SUSP_AMOUNT, SUSP_STATUS
                INTO :SUS-SUSP-ID, :SUS-MONEY-ID, :SUS-SUSP-AMOUNT,
                     :SUS-SUSP-STATUS
                FROM BILDB.BIL_SUSPENSE
               WHERE SUSP_ID = :SUS-SUSP-ID
           END-EXEC
           PERFORM 4820-CHECK-SUSPENSE-READ.

      ******************************************************************
       4820-CHECK-SUSPENSE-READ.
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'SUSNOTFD'       TO CA-REASON-CODE
                 MOVE 'Suspense item not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4820-CHECK-SUSPENSE-READ' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Close the suspense item as MTCH or RVSD, still guarded on OPEN
       4850-RESOLVE-SUSPENSE.
           MOVE WS-BUS-DATE            TO SUS-RESOLVED-DATE
           MOVE CA-USER-ID             TO SUS-RESOLVED-USER
           EXEC SQL
              UPDATE BILDB.BIL_SUSPENSE
                 SET SUSP_STATUS        = :SUS-SUSP-STATUS,
                     MATCHED_ACCOUNT_ID = :SUS-MATCHED-ACCOUNT-ID,
                     RESOLVED_DATE      = :SUS-RESOLVED-DATE,
                     RESOLVED_USER      = :SUS-RESOLVED-USER
               WHERE SUSP_ID            = :SUS-SUSP-ID
                 AND SUSP_STATUS        = 'OPEN'
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Suspense item resolved by another task'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4850-RESOLVE-SUSPENSE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4900-READ-BUS-DATE.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-READ-BUS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
      * Calls to other services
      ******************************************************************
       8000-NEXT-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE WS-SEQ-NAME            TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 12
              MOVE SQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQA-REASON-CODE     TO CA-REASON-CODE
              MOVE SQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SQA-SQ-VALUE           TO WS-NEXT-ID.

      ******************************************************************
       8100-CHECK-PAYMENT-HOLD.
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLA-REQUEST-ID
           MOVE CA-USER-ID             TO HLA-USER-ID
           MOVE WS-BUS-DATE            TO HLA-BUS-DATE
           MOVE CA-TRACE-ID            TO HLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HLA-CALLER-PGM
           MOVE WS-ACCOUNT-ID          TO HLA-HL-ACCOUNT-ID
           MOVE 'PYMT'                 TO HLA-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLA-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           IF HLA-RETURN-CODE >= 12
              MOVE HLA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HLA-REASON-CODE     TO CA-REASON-CODE
              MOVE HLA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PAY-011 Due items only, quoted invoice first
       8200-DISTRIBUTE.
           INITIALIZE WS-DST-CA
           MOVE 'DSTAPLY '             TO DSA-REQUEST-ID
           MOVE CA-USER-ID             TO DSA-USER-ID
           MOVE WS-BUS-DATE            TO DSA-BUS-DATE
           MOVE CA-TRACE-ID            TO DSA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DSA-CALLER-PGM
           MOVE WS-MONEY-ID            TO DSA-DS-MONEY-ID
           MOVE WS-ACCOUNT-ID          TO DSA-DS-ACCOUNT-ID
           MOVE 0                      TO DSA-DS-AMOUNT
           SET DSA-DS-DUE-ONLY         TO TRUE
           MOVE WS-TARGET-INVOICE-ID   TO DSA-DS-TARGET-INVOICE-ID
           CALL WS-DST-PGM USING WS-DST-CA
           IF DSA-RETURN-CODE >= 12
              MOVE DSA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DSA-REASON-CODE     TO CA-REASON-CODE
              MOVE DSA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8250-UNDO-DISTRIBUTIONS.
           INITIALIZE WS-DST-CA
           MOVE 'DSTUNDO '             TO DSA-REQUEST-ID
           MOVE CA-USER-ID             TO DSA-USER-ID
           MOVE WS-BUS-DATE            TO DSA-BUS-DATE
           MOVE CA-TRACE-ID            TO DSA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DSA-CALLER-PGM
           MOVE WS-MONEY-ID            TO DSA-DS-MONEY-ID
           MOVE WS-ACCOUNT-ID          TO DSA-DS-ACCOUNT-ID
           CALL WS-DST-PGM USING WS-DST-CA
           IF DSA-RETURN-CODE >= 12
              MOVE DSA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DSA-REASON-CODE     TO CA-REASON-CODE
              MOVE DSA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Two-line balanced entry: DR WS-GL-DR-ACCOUNT / CR WS-GL-CR-...
       8300-POST-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE CA-TRACE-ID            TO GLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE WS-GL-TXN-TYPE         TO GLA-GL-TXN-TYPE
           MOVE WS-GL-ACCT-ID    TO GLA-GL-ACCOUNT-ID
           IF WS-GL-REF-TYPE = SPACES
              MOVE 'RCPT'              TO GLA-GL-REF-TYPE
              MOVE WS-MONEY-ID         TO GLA-GL-REF-ID
           ELSE
              MOVE WS-GL-REF-TYPE      TO GLA-GL-REF-TYPE
              MOVE WS-GL-REF-ID        TO GLA-GL-REF-ID
           END-IF
           MOVE 2                      TO GLA-GL-LINE-COUNT
           MOVE WS-GL-DR-ACCOUNT       TO GLA-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLA-GL-DR-CR(1)
           MOVE WS-GL-AMOUNT           TO GLA-GL-AMOUNT(1)
           MOVE WS-GL-CR-ACCOUNT       TO GLA-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLA-GL-DR-CR(2)
           MOVE WS-GL-AMOUNT           TO GLA-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLA-RETURN-CODE >= 12
              MOVE GLA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE GLA-REASON-CODE     TO CA-REASON-CODE
              MOVE GLA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * RC 08 from DLQEXIT means there was no open process to close
       8400-DELINQUENCY-EXIT.
           INITIALIZE WS-DLQ-CA
           MOVE 'DLQEXIT '             TO DQA-REQUEST-ID
           MOVE CA-USER-ID             TO DQA-USER-ID
           MOVE WS-BUS-DATE            TO DQA-BUS-DATE
           MOVE CA-TRACE-ID            TO DQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DQA-CALLER-PGM
           MOVE WS-ACCOUNT-ID          TO DQA-DQ-ACCOUNT-ID
           MOVE 'PAID    '             TO DQA-DQ-EXIT-REASON
           CALL WS-DLQ-PGM USING WS-DLQ-CA
           IF DQA-RETURN-CODE >= 12
              MOVE DQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DQA-REASON-CODE     TO CA-REASON-CODE
              MOVE DQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PAY-014 Reversal authority against the receipt amount
       8500-CHECK-AUTHORITY.
           INITIALIZE WS-AUT-CA
           MOVE 'AUTCHK  '             TO ATA-REQUEST-ID
           MOVE CA-USER-ID             TO ATA-USER-ID
           MOVE WS-BUS-DATE            TO ATA-BUS-DATE
           MOVE CA-TRACE-ID            TO ATA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ATA-CALLER-PGM
           MOVE CA-USER-ID             TO ATA-AT-USER
           MOVE 'REVERSAL'             TO ATA-AT-LIMIT-TYPE
           MOVE RCP-RECEIPT-AMOUNT     TO ATA-AT-AMOUNT
           CALL WS-AUT-PGM USING WS-AUT-CA
           IF ATA-RETURN-CODE >= 12
              MOVE ATA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE ATA-REASON-CODE     TO CA-REASON-CODE
              MOVE ATA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF NOT ATA-AT-IS-ALLOWED
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'AUTHLIM '          TO CA-REASON-CODE
              MOVE ATA-AT-LIMIT        TO WS-AMOUNT-DISP
              STRING 'Reversal over limit ' WS-AMOUNT-DISP
                     ' - refer to ' ATA-AT-APPROVER
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8600-WRITE-AUDIT.
           MOVE WS-PROGRAM-ID          TO AUA-AU-PROGRAM
           MOVE WS-MONEY-ID            TO WS-KEY-DISP
           MOVE WS-KEY-DISP            TO AUA-AU-KEY
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF AUA-RETURN-CODE >= 12
              MOVE AUA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AUA-REASON-CODE     TO CA-REASON-CODE
              MOVE AUA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8700-INSTRUMENT-FAILURE.
           INITIALIZE WS-PIN-CA
           MOVE 'INSFAIL '             TO PIA-REQUEST-ID
           MOVE CA-USER-ID             TO PIA-USER-ID
           MOVE WS-BUS-DATE            TO PIA-BUS-DATE
           MOVE CA-TRACE-ID            TO PIA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO PIA-CALLER-PGM
           MOVE RCP-INSTR-ID           TO PIA-PI-INSTR-ID
           MOVE WS-ACCOUNT-ID          TO PIA-PI-ACCOUNT-ID
           CALL WS-PIN-PGM USING WS-PIN-CA
      * An instrument already withdrawn (RC 24) cannot fail again; the
      * reversal stands and the caller is warned.
           EVALUATE TRUE
              WHEN PIA-RC-STATE
                 SET CA-RC-WARNING     TO TRUE
                 MOVE PIA-REASON-CODE  TO CA-REASON-CODE
                 MOVE 'Reversed - instrument already withdrawn'
                                       TO CA-MESSAGE
              WHEN PIA-RETURN-CODE >= 12
                 MOVE PIA-RETURN-CODE  TO CA-RETURN-CODE
                 MOVE PIA-REASON-CODE  TO CA-REASON-CODE
                 MOVE PIA-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
       8800-VALIDATE-CODE.
           MOVE 0                      TO CDA-RETURN-CODE
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE WS-BUS-DATE            TO CDA-BUS-DATE
           MOVE CA-TRACE-ID            TO CDA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE 'N'                    TO CDA-CD-VALID
           CALL WS-COD-PGM USING WS-COD-CA
           IF CDA-RETURN-CODE >= 90
              MOVE CDA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE CDA-REASON-CODE     TO CA-REASON-CODE
              MOVE CDA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8900-FILL-SCH-HEADER.
           MOVE CA-USER-ID             TO SCA-USER-ID
           MOVE WS-BUS-DATE            TO SCA-BUS-DATE
           MOVE CA-TRACE-ID            TO SCA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SCA-CALLER-PGM.

      ******************************************************************
       9000-RETURN.
           IF WS-DIST-CSR-OPEN
              EXEC SQL CLOSE CSR-RCPT-DISTS END-EXEC
              MOVE 'N'                 TO WS-DIST-CSR-SW
           END-IF
           IF WS-TXN-CSR-OPEN
              EXEC SQL CLOSE CSR-CASH-TXNS END-EXEC
              MOVE 'N'                 TO WS-TXN-CSR-SW
           END-IF
           GOBACK.

      ******************************************************************
       9800-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Receipt rows locked by another task - retry'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       9900-DB2-ERROR.
           IF SQLCODE = -911 OR SQLCODE = -913
              PERFORM 9800-DEADLOCK
           END-IF
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'BILPAYRC Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-ERR-CA
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
