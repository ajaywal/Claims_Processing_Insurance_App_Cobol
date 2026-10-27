       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCHKIS.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMCHKIS                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Claim check issuance. Every PEND check due on or *
      *               before the business date whose payment           *
      *               transactions are all SUBM is numbered (CHKNO),   *
      *               set ISSU with the issue date, sent to billing as *
      *               a CLMPAYMT event and, for cheques, written to    *
      *               the bank positive-pay file. Checks that cannot   *
      *               be issued are held and reported with a reason.   *
      * FILES       : POSPAY  - positive-pay issue file (CLMCHKPP 150) *
      *               CHKPT   - checkpoint / restart (CLMCHKCK, 80)    *
      *               SYSIN   - control card (CLMCHKCC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : after CK-LAST-CHECK-ID; positive-pay details are *
      *               buffered and written only after each commit so   *
      *               the bank never sees an uncommitted check number. *
      * CALLS       : CLMSEQGN SEQNEXT  CLMEVTQ EVPUT                  *
      *               CLMERRLG ERRLOG                                  *
      * TABLES      : CLM_CHECK (R U)  CLM_CLAIM (R)                   *
      *               CLM_TRANSACTION (R)  CLM_CONTACT (R)             *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-CKI-001 A check needs at least one        *
      *                              payment txn and every txn on it   *
      *                              must be SUBM, else it is held     *
      *               BR-CLM-CKI-002 Check amount must equal the sum   *
      *                              of its payment txns and be > 0    *
      *               BR-CLM-CKI-003 Payee name required; a cheque     *
      *                              needs payee address and postcode  *
      *               BR-CLM-CKI-004 Payee with sanction status HIT is *
      *                              never paid (held for compliance)  *
      *               BR-CLM-CKI-005 Check number from CHKNO, issue    *
      *                              date = business date, update      *
      *                              guarded on status PEND            *
      *               BR-CLM-CKI-006 Only CHEQ pay method goes to the  *
      *                              bank positive-pay file            *
      *               BR-CLM-CKI-007 Every issued check raises         *
      *                              CLMPAYMT to billing               *
      *               BR-CLM-CKI-008 Bank sort code and account on the *
      *                              control card must be numeric      *
      * RETURN CODE : 0 ok, 4 checks held, 12 bad control card,        *
      *               16 Db2 or service failure                        *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POSPAY-OUT    ASSIGN TO POSPAY
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-PP.
           SELECT CHKPT-FILE    ASSIGN TO CHKPT
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-CHK.
           SELECT CTL-CARD      ASSIGN TO SYSIN
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-CTL.
           SELECT RPT-OUT       ASSIGN TO RPTOUT
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-RPT.
       DATA DIVISION.
       FILE SECTION.
       FD  POSPAY-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS.
           COPY CLMCHKPP.
       FD  CHKPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMCHKCK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMCHKCC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMCHKIS'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'CLMEVTQ '.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-FS-PP                 PIC X(2).
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-CURSOR-SW             PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN         VALUE 'Y'.
           05 WS-CHECK-SW              PIC X     VALUE 'I'.
              88 WS-CHECK-ISSUABLE      VALUE 'I'.
              88 WS-CHECK-HELD          VALUE 'H'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-COMMIT-FREQ           PIC S9(5) COMP-3 VALUE 200.
           05 WS-MAX-BUFFER            PIC S9(5) COMP-3 VALUE 500.
           05 WS-RESTART-ID            PIC S9(9) COMP   VALUE 0.
           05 WS-LAST-CHECK-ID         PIC S9(9) COMP   VALUE 0.
           05 WS-UNCOMMITTED           PIC S9(5) COMP-3 VALUE 0.
           05 WS-PP-BATCH-NO           PIC 9(4)         VALUE 1.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-SELECTED              PIC S9(9) COMP-3 VALUE 0.
           05 WS-SELECTED-AMT          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-ISSUED                PIC S9(9) COMP-3 VALUE 0.
           05 WS-ISSUED-AMT            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-HELD                  PIC S9(9) COMP-3 VALUE 0.
           05 WS-HELD-AMT              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PP-DETAILS            PIC S9(9) COMP-3 VALUE 0.
           05 WS-PP-AMOUNT             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-EVENTS                PIC S9(9) COMP-3 VALUE 0.

       01  WS-CHECK-WORK.
           05 WS-HOLD-REASON           PIC X(8).
           05 WS-HOLD-TEXT             PIC X(40).
           05 WS-TXN-COUNT             PIC S9(9) COMP.
           05 WS-TXN-SUBM              PIC S9(9) COMP.
           05 WS-TXN-TOTAL             PIC S9(11)V99 COMP-3.
           05 WS-CLAIM-NUMBER          PIC X(12).
           05 WS-POLICY-NUM            PIC S9(9) COMP.
           05 WS-ACTION                PIC X(4).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).
           05 WS-CHECK-ID-10           PIC 9(10).

       01  WS-PP-BUFFER.
           05 WS-PP-BUF-COUNT          PIC S9(5) COMP   VALUE 0.
           05 WS-PP-BUF-IX             PIC S9(5) COMP   VALUE 0.
           05 WS-PP-BUF-REC            PIC X(150) OCCURS 500 TIMES.

           COPY CLMCHKPP REPLACING LEADING ==PP-== BY ==WP-==.

           COPY CLMCHKCK REPLACING LEADING ==CK-== BY ==WK-==.

           COPY CLMCHKCC REPLACING LEADING ==CC-== BY ==WC-==.

           COPY CLMCHKEV.

           COPY CLMCHKRP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCHK END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CHK-CSR CURSOR WITH HOLD FOR
              SELECT K.CHECK_ID, K.CLAIM_ID, K.PAYEE_CONTACT_ID,
                     K.PAYEE_NAME, K.PAYEE_ADDR1, K.PAYEE_POSTCODE,
                     K.CHECK_AMOUNT, K.PAY_METHOD, K.SCHEDULED_DATE,
                     C.CLAIM_NUMBER, C.GENAPP_POL_NUM
                FROM CLMDB.CLM_CHECK K
                     INNER JOIN CLMDB.CLM_CLAIM C
                        ON C.CLAIM_ID = K.CLAIM_ID
               WHERE K.CHECK_STATUS   = 'PEND'
                 AND K.SCHEDULED_DATE <= :WS-BUS-DATE
                 AND K.CHECK_ID       > :WS-RESTART-ID
               ORDER BY K.CHECK_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.
       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-EVT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==EVA-==.
           03 EVA-BODY.
              COPY CLMCEVT REPLACING LEADING ==CA-== BY ==EVA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ISSUE-CHECKS
           PERFORM 2900-FINISH
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMCHKIS RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO CR-M-TEXT WS-ABEND-TEXT
           OPEN INPUT CTL-CARD
           MOVE SPACES                 TO WC-CARD
           IF WS-FS-CTL = '00'
              READ CTL-CARD INTO WC-CARD
                 AT END MOVE SPACES    TO WC-CARD
              END-READ
              CLOSE CTL-CARD
           END-IF
           IF WC-COMMIT-FREQ-X IS NUMERIC AND WC-COMMIT-FREQ > 0
              MOVE WC-COMMIT-FREQ      TO WS-COMMIT-FREQ
           END-IF
           IF WS-COMMIT-FREQ > WS-MAX-BUFFER
              MOVE WS-MAX-BUFFER       TO WS-COMMIT-FREQ
           END-IF
      * BR-CLM-CKI-008 Bank details must be present and numeric
           IF WC-SORT-CODE IS NOT NUMERIC
              OR WC-ACCOUNT-NO IS NOT NUMERIC
              MOVE 12                  TO WS-RETURN-CODE
              MOVE 'Control card bank sort code/account not numeric'
                                       TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           PERFORM 4000-GET-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO CR-H1-BUS-DATE
           PERFORM 1100-RESTART-POSITION
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM
           MOVE SPACES                 TO WP-RECORD
           SET WP-IS-HEADER            TO TRUE
           MOVE WC-SORT-CODE           TO WP-SORT-CODE
           MOVE WC-ACCOUNT-NO          TO WP-ACCOUNT-NO
           MOVE WS-BUS-DATE            TO WP-H-FILE-DATE
           MOVE WS-PP-BATCH-NO         TO WP-H-BATCH-NO
           MOVE 'CLAIMSCORE AJ2026'    TO WP-H-ORIGINATOR
           PERFORM 6300-WRITE-POSPAY.

      ******************************************************************
       1100-RESTART-POSITION.
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
              OPEN OUTPUT POSPAY-OUT
           ELSE
              OPEN INPUT CHKPT-FILE
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE CK-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              IF NOT WK-IN-FLIGHT
                 MOVE 'Restart requested but no in-flight checkpoint'
                                       TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
              MOVE WK-LAST-CHECK-ID    TO WS-RESTART-ID
                                          WS-LAST-CHECK-ID
              MOVE WK-ISSUED           TO WS-ISSUED
              MOVE WK-ISSUED-AMOUNT    TO WS-ISSUED-AMT
              COMPUTE WS-PP-BATCH-NO = WK-PP-BATCH-NO + 1
              OPEN EXTEND CHKPT-FILE
              OPEN EXTEND POSPAY-OUT
              MOVE WK-LAST-CHECK-ID    TO WS-DISP-ID
              STRING 'Restart after check id ' WS-DISP-ID
                     ' - positive-pay batch appended'
                     DELIMITED BY SIZE INTO CR-M-TEXT
              MOVE CR-MESSAGE-LINE     TO RPT-RECORD
              PERFORM 6100-PRINT-LINE
           END-IF
           IF NOT WS-FS-CHK-OK OR WS-FS-PP NOT = '00'
              MOVE 'CHKPT or POSPAY open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       2000-ISSUE-CHECKS.
           EXEC SQL OPEN CHK-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-ISSUE-CHECKS' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4100-FETCH-CHECK
           PERFORM UNTIL WS-EOF
              PERFORM 2100-PROCESS-CHECK
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 SET WK-IN-FLIGHT      TO TRUE
                 PERFORM 4900-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4100-FETCH-CHECK
           END-PERFORM
           EXEC SQL CLOSE CHK-CSR END-EXEC
           MOVE 'N'                    TO WS-CURSOR-SW.

      ******************************************************************
       2100-PROCESS-CHECK.
           ADD 1                       TO WS-SELECTED
           ADD CHK-CHECK-AMOUNT        TO WS-SELECTED-AMT
           SET WS-CHECK-ISSUABLE       TO TRUE
           MOVE SPACES                 TO WS-HOLD-REASON WS-HOLD-TEXT
           PERFORM 3000-VALIDATE-CHECK
           IF WS-CHECK-ISSUABLE
              PERFORM 2200-ISSUE-CHECK
           END-IF
           IF WS-CHECK-ISSUABLE
              ADD 1                    TO WS-ISSUED
              ADD CHK-CHECK-AMOUNT     TO WS-ISSUED-AMT
              MOVE 'ISSU'              TO WS-ACTION
           ELSE
              ADD 1                    TO WS-HELD
              ADD CHK-CHECK-AMOUNT     TO WS-HELD-AMT
              MOVE 'HOLD'              TO WS-ACTION
              MOVE SPACES              TO CHK-CHECK-NUMBER
           END-IF
           MOVE CHK-CHECK-ID           TO WS-LAST-CHECK-ID
           ADD 1                       TO WS-UNCOMMITTED
           PERFORM 6400-PRINT-DETAIL.

      ******************************************************************
      * BR-CLM-CKI-005 Number, issue and guard on PEND
       2200-ISSUE-CHECK.
           MOVE STH-HEADER             TO SQA-HEADER
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE 'CHKNO   '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF NOT SQA-RC-OK AND NOT SQA-RC-WARNING
              STRING 'CHKNO sequence failed ' SQA-REASON-CODE ' '
                     SQA-MESSAGE DELIMITED BY SIZE
                     INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE SQA-SQ-FORMATTED(1:10) TO CHK-CHECK-NUMBER
           EXEC SQL
              UPDATE CLMDB.CLM_CHECK
                 SET CHECK_NUMBER = :CHK-CHECK-NUMBER,
                     CHECK_STATUS = 'ISSU',
                     ISSUE_DATE   = :WS-BUS-DATE,
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CHECK_ID     = :CHK-CHECK-ID
                 AND CHECK_STATUS = 'PEND'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-ISSUE-CHECK'  TO WS-ERR-PARAGRAPH
              MOVE CHK-CHECK-ID        TO WS-DISP-ID
              MOVE WS-DISP-ID          TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET WS-CHECK-HELD        TO TRUE
              MOVE 'NOTPEND '          TO WS-HOLD-REASON
              MOVE 'Status changed since selection'
                                       TO WS-HOLD-TEXT
           ELSE
              PERFORM 8100-RAISE-PAYMENT-EVENT
      * BR-CLM-CKI-006 Positive pay for cheques only
              IF CHK-PAY-METHOD = 'CHEQ'
                 PERFORM 6200-BUFFER-POSPAY
              END-IF
           END-IF.

      ******************************************************************
       2900-FINISH.
           SET WK-ENDED                TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           MOVE SPACES                 TO WP-RECORD
           SET WP-IS-TRAILER           TO TRUE
           MOVE WC-SORT-CODE           TO WP-SORT-CODE
           MOVE WC-ACCOUNT-NO          TO WP-ACCOUNT-NO
           MOVE WS-PP-DETAILS          TO WP-T-DETAIL-COUNT
           MOVE WS-PP-AMOUNT           TO WP-T-AMOUNT-TOTAL
           PERFORM 6300-WRITE-POSPAY
           IF WS-HELD > 0 AND WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF
           PERFORM 6500-PRINT-TOTALS.

      ******************************************************************
       3000-VALIDATE-CHECK.
           PERFORM 4200-SUM-PAYMENT-TXNS
           EVALUATE TRUE
      * BR-CLM-CKI-001 Every payment txn on the check must be SUBM
              WHEN WS-TXN-COUNT = 0
                 MOVE 'NOTXN   '       TO WS-HOLD-REASON
                 MOVE 'No payment transaction on check'
                                       TO WS-HOLD-TEXT
              WHEN WS-TXN-SUBM < WS-TXN-COUNT
                 MOVE 'TXNNSUBM'       TO WS-HOLD-REASON
                 MOVE 'Payment awaiting approval'
                                       TO WS-HOLD-TEXT
      * BR-CLM-CKI-002 Amount reconciles to the payments
              WHEN CHK-CHECK-AMOUNT NOT > 0
                 MOVE 'AMTZERO '       TO WS-HOLD-REASON
                 MOVE 'Check amount not positive'
                                       TO WS-HOLD-TEXT
              WHEN CHK-CHECK-AMOUNT NOT = WS-TXN-TOTAL
                 MOVE 'AMTMISM '       TO WS-HOLD-REASON
                 MOVE 'Amount differs from payment txns'
                                       TO WS-HOLD-TEXT
      * BR-CLM-CKI-003 Payee details complete for the pay method
              WHEN CHK-PAYEE-NAME-TEXT = SPACES
                 MOVE 'PAYEENM '       TO WS-HOLD-REASON
                 MOVE 'Payee name missing' TO WS-HOLD-TEXT
              WHEN CHK-PAY-METHOD = 'CHEQ'
                 AND (CHK-PAYEE-ADDR1-IND < 0
                      OR CHK-PAYEE-POSTCODE-IND < 0
                      OR CHK-PAYEE-POSTCODE = SPACES)
                 MOVE 'PAYEEADR'       TO WS-HOLD-REASON
                 MOVE 'Cheque payee address incomplete'
                                       TO WS-HOLD-TEXT
              WHEN OTHER
                 PERFORM 4300-CHECK-PAYEE-SANCTION
           END-EVALUATE
           IF WS-HOLD-REASON NOT = SPACES
              SET WS-CHECK-HELD        TO TRUE
           END-IF.

      ******************************************************************
       4000-GET-BUSINESS-DATE.
           IF WC-BUS-DATE NOT = SPACES
              MOVE WC-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4000-GET-BUSINESS-DATE' TO WS-ERR-PARAGRAPH
                 MOVE CTL-CONTROL-KEY  TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
       4100-FETCH-CHECK.
           EXEC SQL
              FETCH CHK-CSR
               INTO :CHK-CHECK-ID, :CHK-CLAIM-ID,
                    :CHK-PAYEE-CONTACT-ID, :CHK-PAYEE-NAME,
                    :CHK-PAYEE-ADDR1 :CHK-PAYEE-ADDR1-IND,
                    :CHK-PAYEE-POSTCODE :CHK-PAYEE-POSTCODE-IND,
                    :CHK-CHECK-AMOUNT, :CHK-PAY-METHOD,
                    :CHK-SCHEDULED-DATE,
                    :WS-CLAIM-NUMBER, :WS-POLICY-NUM
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CHK-PAYEE-NAME-LEN < 60
                    MOVE SPACES        TO CHK-PAYEE-NAME-TEXT
                                          (CHK-PAYEE-NAME-LEN + 1:)
                 END-IF
                 IF CHK-PAYEE-ADDR1-IND < 0
                    MOVE SPACES        TO CHK-PAYEE-ADDR1-TEXT
                    MOVE 0             TO CHK-PAYEE-ADDR1-LEN
                 ELSE
                    IF CHK-PAYEE-ADDR1-LEN < 60
                       MOVE SPACES     TO CHK-PAYEE-ADDR1-TEXT
                                          (CHK-PAYEE-ADDR1-LEN + 1:)
                    END-IF
                 END-IF
                 IF CHK-PAYEE-POSTCODE-IND < 0
                    MOVE SPACES        TO CHK-PAYEE-POSTCODE
                 END-IF
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-CHECK' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-SUM-PAYMENT-TXNS.
           EXEC SQL
              SELECT COUNT(*),
                     COALESCE(SUM(CASE WHEN TXN_STATUS = 'SUBM'
                                       THEN 1 ELSE 0 END), 0),
                     COALESCE(SUM(TXN_AMOUNT), 0)
                INTO :WS-TXN-COUNT, :WS-TXN-SUBM, :WS-TXN-TOTAL
                FROM CLMDB.CLM_TRANSACTION
               WHERE CHECK_ID = :CHK-CHECK-ID
                 AND CLAIM_ID = :CHK-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-SUM-PAYMENT-TXNS' TO WS-ERR-PARAGRAPH
              MOVE CHK-CHECK-ID        TO WS-DISP-ID
              MOVE WS-DISP-ID          TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-CKI-004 Confirmed sanction hit blocks payment
       4300-CHECK-PAYEE-SANCTION.
           MOVE CHK-PAYEE-CONTACT-ID   TO CON-CONTACT-ID
           EXEC SQL
              SELECT SANCTION_STATUS
                INTO :CON-SANCTION-STATUS
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :CON-CONTACT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CON-SANCTION-STATUS = 'HIT '
                    MOVE 'SANCTION'    TO WS-HOLD-REASON
                    MOVE 'Payee on sanctions list - compliance'
                                       TO WS-HOLD-TEXT
                 END-IF
              WHEN 100
                 MOVE 'PAYEENF '       TO WS-HOLD-REASON
                 MOVE 'Payee contact not found' TO WS-HOLD-TEXT
              WHEN OTHER
                 MOVE '4300-CHECK-PAYEE-SANCTION' TO WS-ERR-PARAGRAPH
                 MOVE CHK-PAYEE-CONTACT-ID TO WS-DISP-ID
                 MOVE WS-DISP-ID       TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Commit, then release the buffered positive-pay details, then
      * record the checkpoint: the bank file only ever holds committed
      * check numbers.
       4900-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-COMMIT-CHECKPOINT' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM VARYING WS-PP-BUF-IX FROM 1 BY 1
                   UNTIL WS-PP-BUF-IX > WS-PP-BUF-COUNT
              MOVE WS-PP-BUF-REC(WS-PP-BUF-IX) TO WP-RECORD
              PERFORM 6300-WRITE-POSPAY
           END-PERFORM
           MOVE 0                      TO WS-PP-BUF-COUNT
                                          WS-UNCOMMITTED
           MOVE WS-PROGRAM-ID          TO WK-PROGRAM
           MOVE WS-BUS-DATE            TO WK-BUS-DATE
           MOVE WS-LAST-CHECK-ID       TO WK-LAST-CHECK-ID
           MOVE WS-ISSUED              TO WK-ISSUED
           MOVE WS-ISSUED-AMT          TO WK-ISSUED-AMOUNT
           MOVE WS-PP-BATCH-NO         TO WK-PP-BATCH-NO
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE CK-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-CKI-007 CLMPAYMT payload per docs/EVENTS.md
       8100-RAISE-PAYMENT-EVENT.
           MOVE CHK-CHECK-ID           TO PE-CHECK-ID
           MOVE WS-CLAIM-NUMBER        TO PE-CLAIM-NUMBER
           MOVE WS-POLICY-NUM          TO PE-POLICY-NO
           MOVE CHK-CHECK-AMOUNT       TO PE-AMOUNT
           MOVE '+'                    TO PE-AMOUNT-SIGN
           MOVE CHK-PAYEE-NAME-TEXT    TO PE-PAYEE-NAME
           MOVE CHK-PAYEE-ADDR1-TEXT   TO PE-PAYEE-ADDR1
           MOVE CHK-PAYEE-POSTCODE     TO PE-PAYEE-POSTCODE
           MOVE CHK-PAY-METHOD         TO PE-PAY-METHOD
           MOVE CHK-SCHEDULED-DATE     TO PE-SCHEDULED-DATE
           INITIALIZE EVA-BODY
           MOVE STH-HEADER             TO EVA-HEADER
           MOVE 'EVPUT   '             TO EVA-REQUEST-ID
           MOVE 'CLMPAYMT'             TO EVA-EV-TYPE
           MOVE 'O'                    TO EVA-EV-DIRECTION
           MOVE 'CLMS'                 TO EVA-EV-SOURCE
           MOVE 'BILL'                 TO EVA-EV-TARGET
           MOVE CHK-CHECK-ID           TO WS-CHECK-ID-10
           MOVE WS-CHECK-ID-10         TO EVA-EV-KEY
           MOVE PE-PAYLOAD             TO EVA-EV-PAYLOAD
           CALL WS-EVT-PGM USING WS-EVT-CA
           IF NOT EVA-RC-OK
              STRING 'EVPUT failed for check ' WS-CHECK-ID-10 ' '
                     EVA-REASON-CODE ' ' EVA-MESSAGE
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           ADD 1                       TO WS-EVENTS.

      ******************************************************************
       6100-PRINT-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              MOVE RPT-RECORD          TO WS-HOLD-LINE
              ADD 1                    TO WS-PAGE-NO
              MOVE WS-PAGE-NO          TO CR-H1-PAGE
              WRITE RPT-RECORD FROM CR-HEAD-1 AFTER ADVANCING PAGE
              WRITE RPT-RECORD FROM CR-HEAD-2 AFTER ADVANCING 2 LINES
              MOVE SPACES              TO RPT-RECORD
              WRITE RPT-RECORD AFTER ADVANCING 1 LINE
              MOVE 4                   TO WS-LINE-COUNT
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE SPACES                 TO CR-M-TEXT.

      ******************************************************************
       6200-BUFFER-POSPAY.
           MOVE SPACES                 TO WP-RECORD
           SET WP-IS-DETAIL            TO TRUE
           MOVE WC-SORT-CODE           TO WP-SORT-CODE
           MOVE WC-ACCOUNT-NO          TO WP-ACCOUNT-NO
           MOVE CHK-CHECK-NUMBER       TO WP-D-CHECK-NUMBER
           MOVE WS-BUS-DATE            TO WP-D-ISSUE-DATE
           MOVE CHK-CHECK-AMOUNT       TO WP-D-AMOUNT
           SET WP-D-ISSUE              TO TRUE
           MOVE CHK-PAYEE-NAME-TEXT    TO WP-D-PAYEE-NAME
           MOVE WS-CLAIM-NUMBER        TO WP-D-CLAIM-NUMBER
           ADD 1                       TO WS-PP-BUF-COUNT
           MOVE WP-RECORD              TO WS-PP-BUF-REC(WS-PP-BUF-COUNT)
           ADD 1                       TO WS-PP-DETAILS
           ADD CHK-CHECK-AMOUNT        TO WS-PP-AMOUNT.

      ******************************************************************
       6300-WRITE-POSPAY.
           WRITE PP-RECORD FROM WP-RECORD
           IF WS-FS-PP NOT = '00'
              MOVE 'POSPAY write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       6400-PRINT-DETAIL.
           MOVE CHK-CHECK-ID           TO CR-D-CHECK-ID
           MOVE WS-CLAIM-NUMBER        TO CR-D-CLAIM-NUMBER
           MOVE CHK-CHECK-NUMBER       TO CR-D-CHECK-NUMBER
           MOVE CHK-SCHEDULED-DATE     TO CR-D-SCHED-DATE
           MOVE CHK-PAY-METHOD         TO CR-D-METHOD
           MOVE CHK-CHECK-AMOUNT       TO CR-D-AMOUNT
           MOVE CHK-PAYEE-NAME-TEXT    TO CR-D-PAYEE
           MOVE WS-ACTION              TO CR-D-STATUS
           MOVE WS-HOLD-REASON         TO CR-D-REASON
           MOVE WS-HOLD-TEXT           TO CR-D-TEXT
           MOVE CR-DETAIL              TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO CR-M-TEXT
           MOVE CR-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'Checks selected (PEND, due)' TO CR-T-LABEL
           MOVE WS-SELECTED            TO CR-T-COUNT
           MOVE WS-SELECTED-AMT        TO CR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Checks issued (cumulative on restart)'
                                       TO CR-T-LABEL
           MOVE WS-ISSUED              TO CR-T-COUNT
           MOVE WS-ISSUED-AMT          TO CR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Checks held'          TO CR-T-LABEL
           MOVE WS-HELD                TO CR-T-COUNT
           MOVE WS-HELD-AMT            TO CR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Positive-pay details this batch' TO CR-T-LABEL
           MOVE WS-PP-DETAILS          TO CR-T-COUNT
           MOVE WS-PP-AMOUNT           TO CR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'CLMPAYMT events raised' TO CR-T-LABEL
           MOVE WS-EVENTS              TO CR-T-COUNT
           MOVE 0                      TO CR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Return code'          TO CR-T-LABEL
           MOVE WS-RETURN-CODE         TO CR-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE CR-TOTAL-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       9000-TERMINATE.
           CLOSE POSPAY-OUT CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Abend: roll back; buffered positive-pay rows for the rolled-
      * back checks are discarded, never written.
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           MOVE 0                      TO WS-PP-BUF-COUNT
           IF WS-RETURN-CODE < 16 AND WS-RETURN-CODE NOT = 12
              MOVE 16                  TO WS-RETURN-CODE
           END-IF
           STRING '*** RUN ABANDONED: ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO CR-M-TEXT
           MOVE CR-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           DISPLAY 'CLMCHKIS ' WS-ABEND-TEXT
           PERFORM 6500-PRINT-TOTALS
           CLOSE POSPAY-OUT CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           EXEC SQL ROLLBACK END-EXEC
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARAGRAPH       TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'CLMCHKIS Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
