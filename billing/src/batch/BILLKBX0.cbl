       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILLKBX0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILLKBX0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Bank lockbox posting. Proves the lockbox file    *
      *               against its trailer (count, amount, cheque hash) *
      *               and, only when the whole file balances, posts    *
      *               every cheque through BILPAYRC PAYRCV (source     *
      *               LKBX). Receipts that cannot be placed land in    *
      *               suspense; bad details are rejected and reported. *
      * PARM        : 1-4 commit frequency (default 0200)              *
      *               6-15 business date override yyyy-mm-dd           *
      * FILES       : LKBXIN (I) RESTART (I, optional) CHKPT (O)       *
      *               RPTOUT (O)                                       *
      * CALLS       : BILPAYRC BILDATES BILERRLG                       *
      * TABLES      : BIL_PAYMENT_RECEIPT (R) BIL_CONTROL (R)          *
      *               (receipts are written by BILPAYRC)               *
      * RULES       : BR-BIL-LKB-001 One header first, one trailer     *
      *                              last, details in between          *
      *               BR-BIL-LKB-002 Trailer count, amount total and   *
      *                              cheque hash must equal the        *
      *                              details or the whole file is      *
      *                              rejected (RC 12)                  *
      *               BR-BIL-LKB-003 A batch whose receipts are already*
      *                              on file is rejected unless this   *
      *                              is a restart of that batch        *
      *               BR-BIL-LKB-004 Deposit date valid and not after  *
      *                              the business date                 *
      *               BR-BIL-LKB-005 A zero cheque is rejected alone   *
      *               BR-BIL-LKB-006 Each cheque posts as method CHEQ, *
      *                              source LKBX, on the deposit date, *
      *                              referenced by cheque number       *
      *               BR-BIL-LKB-007 Each cheque is a savepoint: a     *
      *                              failed receipt backs out alone    *
      *               BR-BIL-LKB-008 Posted + suspense + rejected must *
      *                              prove back to the trailer total   *
      * RETURN CODE : 0 ok, 4 suspense or held cash, 8 rejects,        *
      *               12 file rejected, 16 abend                       *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOCKBOX-FILE ASSIGN TO LKBXIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-LKB-STATUS.
           SELECT OPTIONAL RESTART-FILE ASSIGN TO RESTART
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RST-STATUS.
           SELECT CHECKPOINT-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CKP-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  LOCKBOX-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  LOCKBOX-REC.
           COPY BILLKBR1.
       FD  RESTART-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RESTART-REC.
           COPY BILLKBK1 REPLACING LEADING ==CKP-== BY ==RST-==.
       FD  CHECKPOINT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHECKPOINT-REC.
           COPY BILLKBK1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILLKBX0'.
       01  WS-PAY-PGM                  PIC X(8)  VALUE 'BILPAYRC'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-FILE-STATUS.
           05 WS-LKB-STATUS            PIC XX.
              88 WS-LKB-OK              VALUE '00'.
              88 WS-LKB-EOF             VALUE '10'.
           05 WS-RST-STATUS            PIC XX.
           05 WS-CKP-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.
           05 WS-HASH-MODULUS          PIC 9(16) VALUE
                                       1000000000000000.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 200.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-BATCH-REF             PIC X(12) VALUE SPACES.
           05 WS-DEPOSIT-DATE          PIC X(10) VALUE SPACES.
           05 WS-RESTART-SEQ           PIC 9(6)  VALUE 0.
           05 WS-LAST-SEQ              PIC 9(6)  VALUE 0.
           05 WS-RESTART-FLAG          PIC X     VALUE 'N'.
              88 WS-RESTARTING          VALUE 'Y'.
           05 WS-FILE-FLAG             PIC X     VALUE 'Y'.
              88 WS-FILE-GOOD           VALUE 'Y'.
              88 WS-FILE-BAD            VALUE 'N'.
           05 WS-HEADER-FLAG           PIC X     VALUE 'N'.
              88 WS-HEADER-SEEN         VALUE 'Y'.
           05 WS-TRAILER-FLAG          PIC X     VALUE 'N'.
              88 WS-TRAILER-SEEN        VALUE 'Y'.
           05 WS-RETURN-CODE           PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-PAGE-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-REC-NO                PIC S9(9) COMP VALUE 0.
           05 WS-BATCH-RECEIPTS        PIC S9(9) COMP VALUE 0.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-REC-NO-DISP           PIC Z(8)9.
           05 WS-COUNT-DISP            PIC Z(8)9.
           05 WS-AMOUNT-DISP           PIC -Z(12)9.99.
           05 WS-HASH-DISP             PIC Z(14)9.

      * Pass 1 control totals proved against the trailer
       01  WS-PROOF.
           05 WS-PF-DETAILS            PIC S9(9) COMP VALUE 0.
           05 WS-PF-AMOUNT             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PF-HASH               PIC S9(18) COMP-3 VALUE 0.
           05 WS-PF-HASH-MOD           PIC S9(18) COMP-3 VALUE 0.
           05 WS-TR-DETAILS            PIC S9(9) COMP VALUE 0.
           05 WS-TR-AMOUNT             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TR-HASH               PIC S9(18) COMP-3 VALUE 0.

       01  WS-COUNTERS.
           05 WS-CNT-READ              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SKIPPED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-APPLIED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-HELD              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SUSPENSE          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-REJECTED          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-READ              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-SKIPPED           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-APPLIED           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-DISTRIBUTED       PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-HELD              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-SUSPENSE          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-REJECTED          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-PROVED            PIC S9(13)V99 COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

       01  WS-PAY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PYA-==.
           03 PYA-BODY.
              COPY BILCPAY REPLACING LEADING ==CA-== BY ==PYA-==.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILLKBP1.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROVE-FILE
           IF WS-FILE-GOOD
              PERFORM 2500-POST-FILE
           ELSE
              MOVE 12                  TO WS-RETURN-CODE
           END-IF
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILLKBX0 OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 1100-READ-PARM
           IF WS-BUS-DATE = SPACES
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           MOVE WS-BUS-DATE            TO LKP-H1-BUS-DATE.

      ******************************************************************
       1100-READ-PARM.
           IF LS-PARM-LEN >= 4 AND LS-PARM-DATA(1:4) IS NUMERIC
              MOVE LS-PARM-DATA(1:4)   TO WS-COMMIT-FREQ
              IF WS-COMMIT-FREQ = 0
                 MOVE 200              TO WS-COMMIT-FREQ
              END-IF
           END-IF
           IF LS-PARM-LEN >= 15 AND LS-PARM-DATA(6:10) NOT = SPACES
              INITIALIZE WS-DTE-CA
              MOVE 'DTVALD  '          TO DTA-REQUEST-ID
              MOVE LS-PARM-DATA(6:10)  TO DTA-DT-DATE-1
              PERFORM 8100-CALL-DATES
              IF NOT DTA-DT-IS-VALID
                 DISPLAY 'BILLKBX0 INVALID PARM DATE '
                         LS-PARM-DATA(6:10)
                 MOVE 16               TO RETURN-CODE
                 GOBACK
              END-IF
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * A restart record for this program, business date and lockbox
      * batch resumes after the last committed detail sequence.
       1200-READ-RESTART.
           OPEN INPUT RESTART-FILE
           IF WS-RST-STATUS = '00'
              READ RESTART-FILE
                 AT END CONTINUE
                 NOT AT END
                    IF RST-PROGRAM = WS-PROGRAM-ID
                       AND RST-RUN-DATE = WS-BUS-DATE
                       AND RST-BATCH-REF = WS-BATCH-REF
                       AND RST-RUNNING
                       AND RST-RESTART-KEY(1:6) IS NUMERIC
                       MOVE RST-RESTART-KEY(1:6) TO WS-RESTART-SEQ
                       SET WS-RESTARTING TO TRUE
                       MOVE SPACES     TO LKP-M-TEXT
                       STRING 'RESTARTING BATCH ' WS-BATCH-REF
                              ' AFTER DETAIL SEQUENCE '
                              RST-RESTART-KEY(1:6)
                              DELIMITED BY SIZE INTO LKP-M-TEXT
                       PERFORM 6200-WRITE-MESSAGE
                    END-IF
              END-READ
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
      * Pass 1 - structure and control totals, nothing is posted
      ******************************************************************
       2000-PROVE-FILE.
           OPEN INPUT LOCKBOX-FILE
           IF NOT WS-LKB-OK
              DISPLAY 'BILLKBX0 OPEN FAILED LKBXIN=' WS-LKB-STATUS
              PERFORM 9800-ABEND
           END-IF
           PERFORM 2100-READ-LOCKBOX
           PERFORM UNTIL WS-LKB-EOF OR WS-FILE-BAD
              PERFORM 3000-PROVE-RECORD
              PERFORM 2100-READ-LOCKBOX
           END-PERFORM
           CLOSE LOCKBOX-FILE

           IF WS-FILE-GOOD
              PERFORM 3200-CHECK-TRAILER
           END-IF
           IF WS-FILE-GOOD
              PERFORM 3300-CHECK-DEPOSIT-DATE
           END-IF
           IF WS-FILE-GOOD
              MOVE WS-BATCH-REF        TO LKP-H1-BATCH-REF
              PERFORM 1200-READ-RESTART
              PERFORM 3400-CHECK-BATCH-LOADED
           END-IF
           IF WS-FILE-BAD
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'LOCKBOX FILE ' WS-BATCH-REF
                     ' REJECTED - NO RECEIPTS POSTED'
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
           END-IF.

      ******************************************************************
       2100-READ-LOCKBOX.
           READ LOCKBOX-FILE
           EVALUATE TRUE
              WHEN WS-LKB-OK
                 ADD 1                 TO WS-REC-NO
              WHEN WS-LKB-EOF
                 CONTINUE
              WHEN OTHER
                 DISPLAY 'BILLKBX0 READ ERROR LKBXIN=' WS-LKB-STATUS
                 PERFORM 9800-ABEND
           END-EVALUATE.

      ******************************************************************
      * Pass 2 - post every detail after the restart point
      ******************************************************************
       2500-POST-FILE.
           MOVE 0                      TO WS-REC-NO
           OPEN INPUT LOCKBOX-FILE
           IF NOT WS-LKB-OK
              DISPLAY 'BILLKBX0 REOPEN FAILED LKBXIN=' WS-LKB-STATUS
              PERFORM 9800-ABEND
           END-IF
           PERFORM 2100-READ-LOCKBOX
           PERFORM UNTIL WS-LKB-EOF
              IF LKB-IS-DETAIL
                 PERFORM 2600-POST-DETAIL
                 MOVE LKB-D-SEQ        TO WS-LAST-SEQ
                 ADD 1                 TO WS-SINCE-COMMIT
                 IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                    PERFORM 7000-COMMIT
                 END-IF
              END-IF
              PERFORM 2100-READ-LOCKBOX
           END-PERFORM
           CLOSE LOCKBOX-FILE.

      ******************************************************************
       2600-POST-DETAIL.
           ADD 1                       TO WS-CNT-READ
           ADD LKB-D-AMOUNT            TO WS-AMT-READ
           IF WS-RESTARTING AND LKB-D-SEQ <= WS-RESTART-SEQ
              ADD 1                    TO WS-CNT-SKIPPED
              ADD LKB-D-AMOUNT         TO WS-AMT-SKIPPED
              EXIT PARAGRAPH
           END-IF
           INITIALIZE LKP-DETAIL
           MOVE LKB-D-SEQ              TO LKP-D-SEQ
           MOVE LKB-D-ACCOUNT-NO       TO LKP-D-ACCOUNT-NO
           MOVE LKB-D-INVOICE-NO       TO LKP-D-INVOICE-NO
           MOVE LKB-D-CHEQUE-NO-X      TO LKP-D-CHEQUE-NO
           MOVE LKB-D-AMOUNT           TO LKP-D-AMOUNT
           MOVE SPACES                 TO WS-ERR-KEY
           STRING WS-BATCH-REF '/' LKB-D-SEQ
                  DELIMITED BY SIZE INTO WS-ERR-KEY

      * BR-BIL-LKB-005 A zero cheque cannot be receipted
           IF LKB-D-AMOUNT = 0
              ADD 1                    TO WS-CNT-REJECTED
              MOVE 'REJECTED - ZERO AMOUNT' TO LKP-D-RESULT
              PERFORM 5000-SET-RC-REJECT
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF

      * BR-BIL-LKB-007 One savepoint per cheque
           PERFORM 4300-SET-SAVEPOINT
           PERFORM 8000-RECEIVE-PAYMENT
           EVALUATE TRUE
              WHEN PYA-RETURN-CODE >= 90
                 DISPLAY 'BILLKBX0 BILPAYRC RC ' PYA-RETURN-CODE
                         ' ' PYA-REASON-CODE ' ' PYA-MESSAGE
                 PERFORM 9800-ABEND
              WHEN PYA-RETURN-CODE >= 12
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 ADD 1                 TO WS-CNT-REJECTED
                 ADD LKB-D-AMOUNT      TO WS-AMT-REJECTED
                 STRING 'REJECTED ' PYA-REASON-CODE ' '
                        PYA-MESSAGE DELIMITED BY SIZE
                        INTO LKP-D-RESULT
                 PERFORM 5000-SET-RC-REJECT
              WHEN PYA-PY-STATUS = 'SUSP'
                 ADD 1                 TO WS-CNT-SUSPENSE
                 ADD LKB-D-AMOUNT      TO WS-AMT-SUSPENSE
                 STRING 'SUSPENSE ' PYA-REASON-CODE
                        DELIMITED BY SIZE INTO LKP-D-RESULT
                 PERFORM 5100-SET-RC-WARNING
              WHEN PYA-REASON-CODE = 'PYMTHOLD'
                 ADD 1                 TO WS-CNT-HELD
                 ADD LKB-D-AMOUNT      TO WS-AMT-HELD
                 MOVE 'HELD UNAPPLIED - PAYMENT HOLD'
                                       TO LKP-D-RESULT
                 PERFORM 5100-SET-RC-WARNING
              WHEN OTHER
                 ADD 1                 TO WS-CNT-APPLIED
                 ADD LKB-D-AMOUNT      TO WS-AMT-APPLIED
                 ADD PYA-PY-DISTRIBUTED TO WS-AMT-DISTRIBUTED
                 IF PYA-PY-UNAPPLIED > 0
                    MOVE 'APPLIED - BALANCE ON ACCOUNT'
                                       TO LKP-D-RESULT
                 ELSE
                    MOVE 'APPLIED'     TO LKP-D-RESULT
                 END-IF
           END-EVALUATE
           IF PYA-RETURN-CODE < 12
              MOVE PYA-PY-MONEY-ID     TO LKP-D-MONEY-ID
              MOVE PYA-PY-STATUS       TO LKP-D-STATUS
              MOVE PYA-PY-DISTRIBUTED  TO LKP-D-DISTRIBUTED
           END-IF
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
      * Validation (pass 1)
      ******************************************************************
      * BR-BIL-LKB-001 Header, details, trailer - in that order
       3000-PROVE-RECORD.
           EVALUATE TRUE
              WHEN WS-REC-NO = 1 AND NOT LKB-IS-HEADER
                 MOVE 'FIRST RECORD IS NOT A LOCKBOX HEADER'
                                       TO LKP-M-TEXT
                 PERFORM 3900-FILE-ERROR
              WHEN WS-TRAILER-SEEN
                 MOVE 'RECORDS FOUND AFTER THE TRAILER'
                                       TO LKP-M-TEXT
                 PERFORM 3900-FILE-ERROR
              WHEN LKB-IS-HEADER
                 IF WS-HEADER-SEEN
                    MOVE 'SECOND HEADER RECORD IN FILE'
                                       TO LKP-M-TEXT
                    PERFORM 3900-FILE-ERROR
                 ELSE
                    SET WS-HEADER-SEEN TO TRUE
                    MOVE LKB-H-BATCH-REF   TO WS-BATCH-REF
                    MOVE LKB-H-DEPOSIT-DATE TO WS-DEPOSIT-DATE
                 END-IF
              WHEN LKB-IS-DETAIL
                 PERFORM 3100-PROVE-DETAIL
              WHEN LKB-IS-TRAILER
                 SET WS-TRAILER-SEEN   TO TRUE
                 IF LKB-T-DETAIL-COUNT IS NUMERIC
                    AND LKB-T-AMOUNT-TOTAL IS NUMERIC
                    AND LKB-T-CHEQUE-HASH IS NUMERIC
                    MOVE LKB-T-DETAIL-COUNT TO WS-TR-DETAILS
                    MOVE LKB-T-AMOUNT-TOTAL TO WS-TR-AMOUNT
                    MOVE LKB-T-CHEQUE-HASH  TO WS-TR-HASH
                 ELSE
                    MOVE 'TRAILER CONTROL FIELDS ARE NOT NUMERIC'
                                       TO LKP-M-TEXT
                    PERFORM 3900-FILE-ERROR
                 END-IF
              WHEN OTHER
                 MOVE WS-REC-NO        TO WS-REC-NO-DISP
                 MOVE SPACES           TO LKP-M-TEXT
                 STRING 'UNKNOWN RECORD TYPE ' LKB-REC-TYPE
                        ' AT RECORD ' WS-REC-NO-DISP
                        DELIMITED BY SIZE INTO LKP-M-TEXT
                 PERFORM 3900-FILE-ERROR
           END-EVALUATE.

      ******************************************************************
      * A detail the trailer cannot prove (non-numeric amount, cheque
      * or sequence) makes the whole file unprovable
       3100-PROVE-DETAIL.
           IF LKB-D-AMOUNT-X IS NOT NUMERIC
              OR LKB-D-CHEQUE-NO-X IS NOT NUMERIC
              OR LKB-D-SEQ IS NOT NUMERIC
              MOVE WS-REC-NO           TO WS-REC-NO-DISP
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'DETAIL AT RECORD ' WS-REC-NO-DISP
                     ' HAS NON-NUMERIC SEQUENCE, AMOUNT OR CHEQUE'
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
           ELSE
              ADD 1                    TO WS-PF-DETAILS
              ADD LKB-D-AMOUNT         TO WS-PF-AMOUNT
              ADD LKB-D-CHEQUE-NO      TO WS-PF-HASH
           END-IF.

      ******************************************************************
      * BR-BIL-LKB-002 Count, amount and cheque hash all prove
       3200-CHECK-TRAILER.
           IF NOT WS-HEADER-SEEN OR NOT WS-TRAILER-SEEN
              MOVE 'FILE IS MISSING ITS HEADER OR TRAILER'
                                       TO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-PF-HASH-MOD =
                   FUNCTION MOD(WS-PF-HASH, WS-HASH-MODULUS)
           IF WS-PF-DETAILS NOT = WS-TR-DETAILS
              MOVE WS-PF-DETAILS       TO WS-COUNT-DISP
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'DETAIL COUNT ' WS-COUNT-DISP
                     ' DOES NOT MATCH TRAILER '
                     LKB-T-DETAIL-COUNT
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF
           IF WS-PF-AMOUNT NOT = WS-TR-AMOUNT
              MOVE WS-PF-AMOUNT        TO WS-AMOUNT-DISP
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'DETAIL AMOUNT TOTAL ' WS-AMOUNT-DISP
                     ' DOES NOT MATCH TRAILER TOTAL'
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF
           IF WS-PF-HASH-MOD NOT = WS-TR-HASH
              MOVE WS-PF-HASH-MOD      TO WS-HASH-DISP
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'CHEQUE HASH ' WS-HASH-DISP
                     ' DOES NOT MATCH TRAILER HASH'
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-LKB-004 Deposit date must be a real, non-future date
       3300-CHECK-DEPOSIT-DATE.
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE WS-DEPOSIT-DATE        TO DTA-DT-DATE-1
           PERFORM 8100-CALL-DATES
           IF NOT DTA-DT-IS-VALID
              OR WS-DEPOSIT-DATE > WS-BUS-DATE
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'DEPOSIT DATE ' WS-DEPOSIT-DATE
                     ' IS INVALID OR AFTER BUSINESS DATE '
                     WS-BUS-DATE
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-LKB-003 A batch already receipted is a re-send, unless
      * this run is restarting that same batch
       3400-CHECK-BATCH-LOADED.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-BATCH-RECEIPTS
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE BATCH_REF      = :WS-BATCH-REF
                 AND RECEIPT_SOURCE = 'LKBX'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3400-CHECK-BATCH-LOADED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-BATCH-RECEIPTS > 0 AND NOT WS-RESTARTING
              MOVE WS-BATCH-RECEIPTS   TO WS-COUNT-DISP
              MOVE SPACES              TO LKP-M-TEXT
              STRING 'BATCH ' WS-BATCH-REF ' ALREADY HAS '
                     WS-COUNT-DISP ' RECEIPTS - DUPLICATE FILE'
                     DELIMITED BY SIZE INTO LKP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF.

      ******************************************************************
       3900-FILE-ERROR.
           SET WS-FILE-BAD             TO TRUE
           PERFORM 6200-WRITE-MESSAGE.

      ******************************************************************
      * Db2 access
      ******************************************************************
       4000-READ-BUSINESS-DATE.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-BUSINESS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
       4300-SET-SAVEPOINT.
           EXEC SQL
              SAVEPOINT LKBXCHQ ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-SET-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-ROLLBACK-SAVEPOINT.
           EXEC SQL
              ROLLBACK TO SAVEPOINT LKBXCHQ
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-ROLLBACK-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Return code escalation
      ******************************************************************
       5000-SET-RC-REJECT.
           IF WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF.

       5100-SET-RC-WARNING.
           IF WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
      * Report
      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM LKP-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO LKP-H1-PAGE
           WRITE REPORT-REC FROM LKP-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM LKP-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM LKP-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE SPACES                 TO LKP-M-TEXT.

       6300-WRITE-TOTAL.
           WRITE REPORT-REC FROM LKP-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

      * BR-BIL-LKB-008 Posted + held + suspense + rejected + skipped
      * must prove back to the trailer total
       6900-WRITE-CONTROL-TOTALS.
           PERFORM 6100-WRITE-HEADINGS
           MOVE 'LOCKBOX CONTROL TOTALS'
                                       TO LKP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'TRAILER DETAILS / AMOUNT'  TO LKP-T-LABEL
           MOVE WS-TR-DETAILS          TO LKP-T-COUNT
           MOVE WS-TR-AMOUNT           TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'DETAILS READ'         TO LKP-T-LABEL
           MOVE WS-CNT-READ            TO LKP-T-COUNT
           MOVE WS-AMT-READ            TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SKIPPED - POSTED BEFORE RESTART' TO LKP-T-LABEL
           MOVE WS-CNT-SKIPPED         TO LKP-T-COUNT
           MOVE WS-AMT-SKIPPED         TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'APPLIED TO ACCOUNTS'  TO LKP-T-LABEL
           MOVE WS-CNT-APPLIED         TO LKP-T-COUNT
           MOVE WS-AMT-APPLIED         TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  OF WHICH DISTRIBUTED TO ITEMS' TO LKP-T-LABEL
           MOVE WS-CNT-APPLIED         TO LKP-T-COUNT
           MOVE WS-AMT-DISTRIBUTED     TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'HELD UNAPPLIED - PAYMENT HOLD' TO LKP-T-LABEL
           MOVE WS-CNT-HELD            TO LKP-T-COUNT
           MOVE WS-AMT-HELD            TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SENT TO SUSPENSE'     TO LKP-T-LABEL
           MOVE WS-CNT-SUSPENSE        TO LKP-T-COUNT
           MOVE WS-AMT-SUSPENSE        TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REJECTED'             TO LKP-T-LABEL
           MOVE WS-CNT-REJECTED        TO LKP-T-COUNT
           MOVE WS-AMT-REJECTED        TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO LKP-T-LABEL
           MOVE WS-CNT-COMMITS         TO LKP-T-COUNT
           MOVE 0                      TO LKP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           IF WS-FILE-GOOD AND WS-CNT-READ > 0
              COMPUTE WS-AMT-PROVED = WS-AMT-SKIPPED + WS-AMT-APPLIED
                                    + WS-AMT-HELD + WS-AMT-SUSPENSE
                                    + WS-AMT-REJECTED
              IF WS-AMT-PROVED = WS-TR-AMOUNT
                 MOVE 'POSTING PROVES TO TRAILER TOTAL' TO LKP-M-TEXT
              ELSE
                 MOVE 'POSTING DOES NOT PROVE TO TRAILER - INVESTIGATE'
                                       TO LKP-M-TEXT
                 PERFORM 5100-SET-RC-WARNING
              END-IF
              PERFORM 6200-WRITE-MESSAGE
           END-IF.

      ******************************************************************
      * Commit and checkpoint
      ******************************************************************
       7000-COMMIT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-CNT-COMMITS
           MOVE 0                      TO WS-SINCE-COMMIT
           INITIALIZE CHECKPOINT-REC
           MOVE WS-PROGRAM-ID          TO CKP-PROGRAM
           MOVE WS-BUS-DATE            TO CKP-RUN-DATE
           MOVE 'P'                    TO CKP-PHASE
           MOVE WS-LAST-SEQ            TO CKP-RESTART-KEY(1:6)
           MOVE WS-BATCH-REF           TO CKP-BATCH-REF
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           SET CKP-RUNNING             TO TRUE
           IF WS-LKB-EOF OR WS-FILE-BAD
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILLKBX0 CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * Calls
      ******************************************************************
      * BR-BIL-LKB-006 Cheque receipt on the deposit date
       8000-RECEIVE-PAYMENT.
           INITIALIZE WS-PAY-CA
           MOVE 'PAYRCV  '             TO PYA-REQUEST-ID
           MOVE WS-BATCH-USER          TO PYA-USER-ID
           MOVE WS-BUS-DATE            TO PYA-BUS-DATE
           MOVE WS-ERR-KEY             TO PYA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO PYA-CALLER-PGM
           MOVE LKB-D-ACCOUNT-NO       TO PYA-PY-ACCOUNT-NUMBER
           MOVE LKB-D-INVOICE-NO       TO PYA-PY-INVOICE-NUMBER
           MOVE LKB-D-AMOUNT           TO PYA-PY-AMOUNT
           MOVE 'CHEQ'                 TO PYA-PY-METHOD
           MOVE 'LKBX'                 TO PYA-PY-SOURCE
           MOVE LKB-D-CHEQUE-NO-X      TO PYA-PY-REF-NUMBER
           MOVE WS-DEPOSIT-DATE        TO PYA-PY-RECEIVED-DATE
           MOVE WS-BATCH-REF           TO PYA-PY-BATCH-REF
           CALL WS-PAY-PGM USING WS-PAY-CA.

      ******************************************************************
       8100-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 90
              DISPLAY 'BILLKBX0 BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       9000-TERMINATE.
           IF WS-FILE-GOOD
              PERFORM 7000-COMMIT
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILLKBX0 COMPLETE BATCH=' WS-BATCH-REF
                   ' READ=' WS-CNT-READ ' APPLIED=' WS-CNT-APPLIED
                   ' SUSP=' WS-CNT-SUSPENSE ' REJ=' WS-CNT-REJECTED
                   ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Unrecoverable: back out the open unit of work, keep the last
      * checkpoint for restart.
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO LKP-M-TEXT
           STRING '*** BILLKBX0 ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK. LAST KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO LKP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILLKBX0 DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA ' KEY ' WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           STRING 'BILLKBX0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9800-ABEND.
