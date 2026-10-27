       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILINVB0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILINVB0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Invoice run for the business date. Every         *
      *               invoice stream with PLAN items whose event date  *
      *               has arrived is billed: one invoice per account   *
      *               and stream, items moved to BILD, prior balance   *
      *               carried, ledger posted, invoice document         *
      *               requested and account balances refreshed.        *
      * INPUT       : SYSIN   control card            (BILINVC1)       *
      *               CHKPT   checkpoint / restart     (BILINVK1)      *
      * OUTPUT      : CHKPT   checkpoint, RPTOUT run report (BILINVH1, *
      *               BILINVR1)                                        *
      * CALLS       : BILSEQGN BILDATES BILHOLD0 BILGLPST BILACCT0     *
      *               BILERRLG                                         *
      * TABLES      : BIL_INVOICE_ITEM      (R U)                      *
      *               BIL_INVOICE_STREAM    (R)                        *
      *               BIL_INVOICE           (C U)                      *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_DOCUMENT_REQUEST  (C)                        *
      *               BIL_CONTROL           (R)                        *
      * RESTART     : last fully committed ACCOUNT_ID in CHKPT; the    *
      *               cursor resumes after it                          *
      * RULES       : BR-BIL-IVB-001 Only ACTV streams of ACTV accounts*
      *                              are invoiced                      *
      *               BR-BIL-IVB-002 An active INVC hold skips the     *
      *                              account for this run              *
      *               BR-BIL-IVB-003 One invoice per account + stream  *
      *                              for PLAN items with event date on *
      *                              or before the business date       *
      *               BR-BIL-IVB-004 Prior balance (open BILD/DUE      *
      *                              items) rides on the account's     *
      *                              first invoice of the run          *
      *               BR-BIL-IVB-005 Due date = earliest item due      *
      *                              date, never before the bill date  *
      *               BR-BIL-IVB-006 An account is all-or-nothing: any *
      *                              failure rolls back all of its     *
      *                              invoices (savepoint)              *
      *               BR-BIL-IVB-007 Commits happen only on account    *
      *                              boundaries, every COMMIT-FREQ     *
      *                              invoices                          *
      *               BR-BIL-IVB-008 A completed business date is not  *
      *                              re-run unless forced              *
      * RETURN CODE : 0 ok, 4 accounts held/skipped, 8 accounts        *
      *               rejected, 16 run abended (restartable)           *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTROL-CARD ASSIGN TO SYSIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CC-STATUS.
           SELECT OPTIONAL CHKPT-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CK-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTROL-CARD
           RECORDING MODE IS F.
           COPY BILINVC1.
       FD  CHKPT-FILE
           RECORDING MODE IS F.
           COPY BILINVK1.
       FD  REPORT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILINVB0'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'BILBATCH'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-FILE-STATUS.
           05 WS-CC-STATUS             PIC XX    VALUE '00'.
           05 WS-CK-STATUS             PIC XX    VALUE '00'.
           05 WS-RP-STATUS             PIC XX    VALUE '00'.

       01  WS-RUN.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-START-TS          PIC X(26) VALUE SPACES.
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-RESTART-ACCT          PIC S9(9) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-RUN-FLAG              PIC X     VALUE 'N'.
              88 WS-RUN-ENDED                    VALUE 'Y'.
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-END-OF-STREAMS               VALUE 'Y'.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.
           05 WS-ABEND-TEXT            PIC X(100) VALUE SPACES.

       01  WS-ACCOUNT-STATE.
           05 WS-CUR-ACCOUNT-ID        PIC S9(9) COMP VALUE 0.
           05 WS-ACCT-FLAG             PIC X     VALUE 'N'.
              88 WS-ACCT-BILLING                 VALUE 'B'.
              88 WS-ACCT-SKIPPED                 VALUE 'S'.
              88 WS-ACCT-REJECTED                VALUE 'R'.
              88 WS-ACCT-NONE                    VALUE 'N'.
           05 WS-FIRST-INVOICE         PIC X     VALUE 'Y'.
              88 WS-IS-FIRST-INVOICE             VALUE 'Y'.
           05 WS-ACCT-INVOICES         PIC S9(9) COMP VALUE 0.
           05 WS-PRIOR-BALANCE         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-REJECT-REASON         PIC X(8)  VALUE SPACES.
           05 WS-REJECT-TEXT           PIC X(80) VALUE SPACES.
           05 WS-A-ITEMS               PIC S9(9) COMP-3 VALUE 0.
           05 WS-A-INVOICE-AMT         PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-A-PRIOR-AMT           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-A-DUE-AMT             PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-STREAM-ROW.
           05 WS-ROW-ACCOUNT-ID        PIC S9(9) COMP.
           05 WS-ROW-STREAM-ID         PIC S9(9) COMP.
           05 WS-ROW-ITEM-COUNT        PIC S9(9) COMP.
           05 WS-ROW-AMOUNT            PIC S9(11)V99 COMP-3.
           05 WS-ROW-MIN-DUE           PIC X(10).

       01  WS-TOTALS.
           05 WS-T-STREAMS-READ        PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ACCTS-BILLED        PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ACCTS-HOLD          PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ACCTS-INACTIVE      PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ACCTS-REJECTED      PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-INVOICES            PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ITEMS               PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-DOCS                PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-COMMITS             PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-INVOICE-AMT         PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-T-PRIOR-AMT           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-T-DUE-AMT             PIC S9(13)V99 COMP-3 VALUE 0.

      * Product totals; the account's share is held in PEND until the
      * account commits so rejected accounts never reach the report
       01  WS-PRODUCT-TABLE.
           05 WS-PROD OCCURS 5 TIMES INDEXED BY PX.
              10 WS-PR-CODE            PIC X.
              10 WS-PR-NAME            PIC X(20).
              10 WS-PR-INVOICES        PIC S9(9) COMP-3.
              10 WS-PR-ITEMS           PIC S9(9) COMP-3.
              10 WS-PR-AMOUNT          PIC S9(13)V99 COMP-3.
              10 WS-PR-PEND-INVOICES   PIC S9(9) COMP-3.
              10 WS-PR-PEND-ITEMS      PIC S9(9) COMP-3.
              10 WS-PR-PEND-AMOUNT     PIC S9(13)V99 COMP-3.

       01  WS-WORK.
           05 WS-DUE-DATE              PIC X(10).
           05 WS-GL-AMOUNT             PIC S9(11)V99 COMP-3.
           05 WS-PROD-CODE             PIC X.
           05 WS-PROD-ITEMS            PIC S9(9) COMP.
           05 WS-PROD-AMOUNT           PIC S9(11)V99 COMP-3.
           05 WS-PAGE                  PIC S9(4) COMP VALUE 0.
           05 WS-LINES                 PIC S9(4) COMP VALUE 99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-CNT-DISP              PIC Z(8)9.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURRENT-TS            PIC X(26).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDINV END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDDOC END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-IVB-001 / 003 Billable streams, account then stream
           EXEC SQL
              DECLARE C-BILLABLE CURSOR WITH HOLD FOR
               SELECT I.ACCOUNT_ID, I.STREAM_ID, COUNT(*),
                      SUM(I.ITEM_AMOUNT), MIN(I.DUE_DATE)
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_INVOICE_STREAM S
                   ON S.STREAM_ID = I.STREAM_ID
                WHERE I.ITEM_STATUS   = 'PLAN'
                  AND I.EVENT_DATE   <= :WS-BUS-DATE
                  AND S.STREAM_STATUS = 'ACTV'
                  AND I.ACCOUNT_ID    > :WS-RESTART-ACCT
                GROUP BY I.ACCOUNT_ID, I.STREAM_ID
                ORDER BY I.ACCOUNT_ID, I.STREAM_ID
                FOR FETCH ONLY
           END-EXEC.

           EXEC SQL
              DECLARE C-INV-PRODUCTS CURSOR FOR
               SELECT P.PRODUCT_CODE, COUNT(*), SUM(I.ITEM_AMOUNT)
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_POLICY_PERIOD P
                   ON P.PP_ID = I.PP_ID
                WHERE I.INVOICE_ID = :INV-INVOICE-ID
                GROUP BY P.PRODUCT_CODE
                FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTC-==.
           03 DTC-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTC-==.
       01  WS-HLD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==HLC-==.
           03 HLC-BODY.
              COPY BILCHLD REPLACING LEADING ==CA-== BY ==HLC-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLC-==.
           03 GLC-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLC-==.
       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACC-==.
           03 ACC-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILINVH1.
           COPY BILINVR1.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF NOT WS-RUN-ENDED
              PERFORM 2000-PROCESS-STREAMS
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN INPUT  CONTROL-CARD
                OUTPUT REPORT-FILE
           IF WS-CC-STATUS NOT = '00' OR WS-RP-STATUS NOT = '00'
              DISPLAY 'BILINVB0 OPEN FAILED SYSIN=' WS-CC-STATUS
                      ' RPTOUT=' WS-RP-STATUS
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           PERFORM 1100-READ-CONTROL-CARD
           PERFORM 1200-RESOLVE-BUS-DATE
           PERFORM 1300-READ-CHECKPOINT
           PERFORM 1400-LOAD-PRODUCT-TABLE
           MOVE WS-PROGRAM-ID          TO RH-PROGRAM-ID
           MOVE 'INVOICE RUN - BILL PLANNED ITEMS' TO RH-TITLE
           MOVE WS-BUS-DATE            TO RH-BUS-DATE
           MOVE WS-RUN-DATE            TO RH-RUN-DATE
           PERFORM 7000-NEW-PAGE
           IF WS-RESTART-ACCT > 0
              MOVE WS-RESTART-ACCT     TO WS-ID-DISP
              STRING 'RESTART AFTER ACCOUNT ID ' WS-ID-DISP
                     DELIMITED BY SIZE INTO RH-TEXT
              PERFORM 7200-WRITE-TEXT
           END-IF.

      ******************************************************************
       1100-READ-CONTROL-CARD.
           MOVE SPACES                 TO CC-RECORD
           READ CONTROL-CARD
              AT END
                 MOVE SPACES           TO CC-RECORD
           END-READ
           IF CC-COMMIT-FREQ NOT = SPACES
              IF CC-COMMIT-FREQ IS NUMERIC AND CC-COMMIT-FREQ-N > 0
                 MOVE CC-COMMIT-FREQ-N TO WS-COMMIT-FREQ
              ELSE
                 STRING 'INVALID COMMIT FREQUENCY ' CC-COMMIT-FREQ
                        DELIMITED BY SIZE INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
           END-IF
           CLOSE CONTROL-CARD.

      ******************************************************************
       1200-RESOLVE-BUS-DATE.
           EXEC SQL
              SELECT BUSINESS_DATE, CHAR(CURRENT DATE, ISO),
                     CURRENT TIMESTAMP
                INTO :CTL-BUSINESS-DATE, :WS-RUN-DATE,
                     :WS-RUN-START-TS
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = 'SYSTEM  '
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE 'SYSTEM'            TO WS-ERR-KEY
              MOVE '1200-RESOLVE-BUS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF CC-BUS-DATE = SPACES
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           ELSE
              MOVE CC-BUS-DATE         TO WS-BUS-DATE
           END-IF
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTC-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTC-DT-DATE-1
           PERFORM 8300-CALL-DATES
           IF NOT DTC-DT-IS-VALID
              STRING 'INVALID BUSINESS DATE ' WS-BUS-DATE
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-BIL-IVB-008 Completed dates are not re-run unless forced
       1300-READ-CHECKPOINT.
           MOVE 0                      TO WS-RESTART-ACCT
           OPEN INPUT CHKPT-FILE
           IF WS-CK-STATUS = '00'
              READ CHKPT-FILE
                 AT END
                    MOVE SPACES        TO CK-RECORD
              END-READ
              CLOSE CHKPT-FILE
           ELSE
              MOVE SPACES              TO CK-RECORD
           END-IF
           IF CK-PROGRAM-ID = WS-PROGRAM-ID
              AND CK-BUS-DATE = WS-BUS-DATE
              AND NOT CC-RESTART-NONE
              EVALUATE TRUE
                 WHEN CK-RUNNING OR CK-FAILED
                    MOVE CK-LAST-KEY-1 TO WS-RESTART-ACCT
                    MOVE CK-COMMITS    TO WS-T-COMMITS
                 WHEN CK-COMPLETE AND NOT CC-RESTART-FORCE
                    DISPLAY 'BILINVB0 BUSINESS DATE ' WS-BUS-DATE
                            ' ALREADY COMPLETE - NOTHING DONE'
                    MOVE 4             TO WS-MAX-RC
                    SET WS-RUN-ENDED   TO TRUE
              END-EVALUATE
           END-IF.

      ******************************************************************
       1400-LOAD-PRODUCT-TABLE.
           INITIALIZE WS-PRODUCT-TABLE
           MOVE 'M'                    TO WS-PR-CODE(1)
           MOVE 'MOTOR'                TO WS-PR-NAME(1)
           MOVE 'H'                    TO WS-PR-CODE(2)
           MOVE 'HOUSE'                TO WS-PR-NAME(2)
           MOVE 'C'                    TO WS-PR-CODE(3)
           MOVE 'COMMERCIAL PROPERTY'  TO WS-PR-NAME(3)
           MOVE 'E'                    TO WS-PR-CODE(4)
           MOVE 'ENDOWMENT'            TO WS-PR-NAME(4)
           MOVE '*'                    TO WS-PR-CODE(5)
           MOVE 'OTHER / UNKNOWN'      TO WS-PR-NAME(5).

      ******************************************************************
       2000-PROCESS-STREAMS.
           EXEC SQL OPEN C-BILLABLE END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-STREAMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           SET WS-ACCT-NONE            TO TRUE
           PERFORM 4000-FETCH-BILLABLE
           PERFORM UNTIL WS-END-OF-STREAMS
              IF WS-ROW-ACCOUNT-ID NOT = WS-CUR-ACCOUNT-ID
                 PERFORM 2200-END-ACCOUNT
                 PERFORM 2100-START-ACCOUNT
              END-IF
              IF WS-ACCT-BILLING
                 PERFORM 2300-BILL-STREAM
              END-IF
              PERFORM 4000-FETCH-BILLABLE
           END-PERFORM
           PERFORM 2200-END-ACCOUNT
           EXEC SQL CLOSE C-BILLABLE END-EXEC
           SET WS-CURSOR-CLOSED        TO TRUE.

      ******************************************************************
       2100-START-ACCOUNT.
           MOVE WS-ROW-ACCOUNT-ID      TO WS-CUR-ACCOUNT-ID
           MOVE 0                      TO WS-ACCT-INVOICES
                                          WS-A-ITEMS
                                          WS-A-INVOICE-AMT
                                          WS-A-PRIOR-AMT
                                          WS-A-DUE-AMT
           MOVE 'Y'                    TO WS-FIRST-INVOICE
           SET WS-ACCT-BILLING         TO TRUE
           PERFORM 2110-CLEAR-PENDING-PRODUCTS
           EXEC SQL
              SAVEPOINT ACCTUNIT ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-START-ACCOUNT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-READ-ACCOUNT
      * BR-BIL-IVB-001 Only active accounts are invoiced
           IF ACT-ACCOUNT-STATUS NOT = 'ACTV'
              SET WS-ACCT-SKIPPED      TO TRUE
              ADD 1                    TO WS-T-ACCTS-INACTIVE
              PERFORM 7300-WRITE-SKIP-LINE
              MOVE 'ACCOUNT NOT ACTIVE' TO R1-REMARK
              PERFORM 7310-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           PERFORM 8100-CHECK-INVOICE-HOLD
      * BR-BIL-IVB-002 An active INVC hold skips the account
           IF HLC-HL-IS-ACTIVE
              SET WS-ACCT-SKIPPED      TO TRUE
              ADD 1                    TO WS-T-ACCTS-HOLD
              PERFORM 7300-WRITE-SKIP-LINE
              MOVE 'INVC HOLD - SKIPPED' TO R1-REMARK
              PERFORM 7310-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           PERFORM 4200-CALC-PRIOR-BALANCE.

      ******************************************************************
       2110-CLEAR-PENDING-PRODUCTS.
           PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > 5
              MOVE 0                   TO WS-PR-PEND-INVOICES(PX)
                                          WS-PR-PEND-ITEMS(PX)
                                          WS-PR-PEND-AMOUNT(PX)
           END-PERFORM.

      ******************************************************************
      * Close out the previous account: refresh balances, merge its
      * product totals and commit on the account boundary
       2200-END-ACCOUNT.
           IF WS-ACCT-BILLING AND WS-ACCT-INVOICES > 0
              PERFORM 8400-REFRESH-BALANCES
           END-IF
           IF WS-ACCT-BILLING AND WS-ACCT-INVOICES > 0
              ADD 1                    TO WS-T-ACCTS-BILLED
              PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > 5
                 ADD WS-PR-PEND-INVOICES(PX) TO WS-PR-INVOICES(PX)
                 ADD WS-PR-PEND-ITEMS(PX)    TO WS-PR-ITEMS(PX)
                 ADD WS-PR-PEND-AMOUNT(PX)   TO WS-PR-AMOUNT(PX)
              END-PERFORM
              ADD WS-ACCT-INVOICES     TO WS-SINCE-COMMIT
                                          WS-T-INVOICES
                                          WS-T-DOCS
              ADD WS-A-ITEMS           TO WS-T-ITEMS
              ADD WS-A-INVOICE-AMT     TO WS-T-INVOICE-AMT
              ADD WS-A-PRIOR-AMT       TO WS-T-PRIOR-AMT
              ADD WS-A-DUE-AMT         TO WS-T-DUE-AMT
           END-IF
           IF NOT WS-ACCT-NONE
              EXEC SQL RELEASE SAVEPOINT ACCTUNIT END-EXEC
           END-IF
      * BR-BIL-IVB-007 Commit on account boundaries only
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 7100-COMMIT-CHECKPOINT
           END-IF
           SET WS-ACCT-NONE            TO TRUE.

      ******************************************************************
      * BR-BIL-IVB-003 One invoice for the account + stream
       2300-BILL-STREAM.
           ADD 1                       TO WS-T-STREAMS-READ
           MOVE 'INV     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           IF WS-ACCT-REJECTED
              EXIT PARAGRAPH
           END-IF
           MOVE SQC-SQ-VALUE           TO INV-INVOICE-ID
           MOVE 'INVNO   '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           IF WS-ACCT-REJECTED
              EXIT PARAGRAPH
           END-IF
           MOVE SQC-SQ-FORMATTED       TO INV-INVOICE-NUMBER
           PERFORM 5000-BUILD-INVOICE
           PERFORM 4300-INSERT-INVOICE
           PERFORM 4310-BILL-STREAM-ITEMS
           IF WS-ACCT-REJECTED
              EXIT PARAGRAPH
           END-IF
           PERFORM 8200-POST-INVOICE-LEDGER
           IF WS-ACCT-REJECTED
              EXIT PARAGRAPH
           END-IF
           PERFORM 4320-INSERT-DOC-REQUEST
           IF WS-ACCT-REJECTED
              EXIT PARAGRAPH
           END-IF
           PERFORM 4400-ADD-PRODUCT-TOTALS
           ADD 1                       TO WS-ACCT-INVOICES
           ADD WS-ROW-ITEM-COUNT       TO WS-A-ITEMS
           ADD INV-INVOICE-AMOUNT      TO WS-A-INVOICE-AMT
           ADD INV-PRIOR-BALANCE       TO WS-A-PRIOR-AMT
           ADD INV-AMOUNT-DUE          TO WS-A-DUE-AMT
           MOVE ACT-ACCOUNT-NUMBER     TO R1-ACCOUNT-NUMBER
           MOVE INV-INVOICE-NUMBER     TO R1-INVOICE-NUMBER
           MOVE INV-STREAM-ID          TO R1-STREAM-ID
           MOVE WS-ROW-ITEM-COUNT      TO R1-ITEMS
           MOVE INV-INVOICE-AMOUNT     TO R1-INVOICE-AMT
           MOVE INV-PRIOR-BALANCE      TO R1-PRIOR-BAL
           MOVE INV-AMOUNT-DUE         TO R1-AMOUNT-DUE
           MOVE INV-DUE-DATE           TO R1-DUE-DATE
           MOVE 'BILLED'               TO R1-REMARK
           PERFORM 7310-WRITE-DETAIL.

      ******************************************************************
      * BR-BIL-IVB-006 Any failure rolls the whole account back
       2900-REJECT-ACCOUNT.
           EXEC SQL ROLLBACK TO SAVEPOINT ACCTUNIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2900-REJECT-ACCOUNT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-ACCT-REJECTED        TO TRUE
           ADD 1                       TO WS-T-ACCTS-REJECTED
           PERFORM 7300-WRITE-SKIP-LINE
           STRING 'REJECTED ' WS-REJECT-REASON
                  DELIMITED BY SIZE INTO R1-REMARK
           PERFORM 7310-WRITE-DETAIL
           MOVE WS-REJECT-TEXT         TO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           IF WS-ACCT-INVOICES > 0
              MOVE WS-ACCT-INVOICES    TO WS-CNT-DISP
              STRING 'INVOICES ROLLED BACK FOR THIS ACCOUNT: '
                     WS-CNT-DISP DELIMITED BY SIZE INTO RH-TEXT
              PERFORM 7200-WRITE-TEXT
           END-IF
           MOVE 0                      TO WS-ACCT-INVOICES
           PERFORM 2110-CLEAR-PENDING-PRODUCTS.

      ******************************************************************
       4000-FETCH-BILLABLE.
           EXEC SQL
              FETCH C-BILLABLE
               INTO :WS-ROW-ACCOUNT-ID, :WS-ROW-STREAM-ID,
                    :WS-ROW-ITEM-COUNT, :WS-ROW-AMOUNT,
                    :WS-ROW-MIN-DUE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-END-OF-STREAMS TO TRUE
              WHEN OTHER
                 MOVE '4000-FETCH-BILLABLE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-READ-ACCOUNT.
           MOVE WS-CUR-ACCOUNT-ID      TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_NUMBER, ACCOUNT_STATUS, DELIVERY_METHOD
                INTO :ACT-ACCOUNT-NUMBER, :ACT-ACCOUNT-STATUS,
                     :ACT-DELIVERY-METHOD
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ACT-ACCOUNT-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4100-READ-ACCOUNT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-IVB-004 Prior balance = open BILD/DUE items of account
       4200-CALC-PRIOR-BALANCE.
           MOVE WS-CUR-ACCOUNT-ID      TO ITM-ACCOUNT-ID
           EXEC SQL
              SELECT COALESCE(SUM(ITEM_AMOUNT - PAID_AMOUNT
                                  - WRITEOFF_AMOUNT), 0)
                INTO :WS-PRIOR-BALANCE
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE ACCOUNT_ID  = :ITM-ACCOUNT-ID
                 AND ITEM_STATUS IN ('BILD', 'DUE ')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-CALC-PRIOR-BALANCE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-INSERT-INVOICE.
           EXEC SQL
              INSERT INTO BILDB.BIL_INVOICE
                     (INVOICE_ID, INVOICE_NUMBER, ACCOUNT_ID,
                      STREAM_ID, BILL_DATE, DUE_DATE, INVOICE_AMOUNT,
                      AMOUNT_DUE, PRIOR_BALANCE, INVOICE_STATUS,
                      ADHOC_FLAG, ITEM_COUNT, DOC_ID, CREATE_TS,
                      UPDATE_TS)
              VALUES (:INV-INVOICE-ID, :INV-INVOICE-NUMBER,
                      :INV-ACCOUNT-ID, :INV-STREAM-ID,
                      :INV-BILL-DATE, :INV-DUE-DATE,
                      :INV-INVOICE-AMOUNT, :INV-AMOUNT-DUE,
                      :INV-PRIOR-BALANCE, :INV-INVOICE-STATUS,
                      :INV-ADHOC-FLAG, :INV-ITEM-COUNT, :INV-DOC-ID,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4300-INSERT-INVOICE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Items must still be PLAN; a short count means a concurrent
      * change (online INVADH or a reschedule) - reject the account
       4310-BILL-STREAM-ITEMS.
           MOVE INV-INVOICE-ID         TO ITM-INVOICE-ID
           MOVE WS-CUR-ACCOUNT-ID      TO ITM-ACCOUNT-ID
           MOVE WS-ROW-STREAM-ID       TO ITM-STREAM-ID
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET INVOICE_ID  = :ITM-INVOICE-ID,
                     ITEM_STATUS = 'BILD',
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID  = :ITM-ACCOUNT-ID
                 AND STREAM_ID   = :ITM-STREAM-ID
                 AND ITEM_STATUS = 'PLAN'
                 AND EVENT_DATE <= :WS-BUS-DATE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4310-BILL-STREAM-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) NOT = WS-ROW-ITEM-COUNT
              MOVE 'ITEMCHNG'          TO WS-REJECT-REASON
              MOVE 'Planned items changed while invoicing'
                                       TO WS-REJECT-TEXT
              PERFORM 2900-REJECT-ACCOUNT
           END-IF.

      ******************************************************************
       4320-INSERT-DOC-REQUEST.
           MOVE 'DOC     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           IF WS-ACCT-REJECTED
              EXIT PARAGRAPH
           END-IF
           MOVE SQC-SQ-VALUE           TO DOC-DOC-ID
           MOVE WS-CUR-ACCOUNT-ID      TO DOC-ACCOUNT-ID
           MOVE 'INVOICE '             TO DOC-DOC-TYPE
           MOVE 'INV '                 TO DOC-REF-TYPE
           MOVE INV-INVOICE-ID         TO DOC-REF-ID
           MOVE ACT-DELIVERY-METHOD    TO DOC-DELIVERY-METHOD
           MOVE 'RQST'                 TO DOC-DOC-STATUS
           MOVE WS-BUS-DATE            TO DOC-REQUEST-DATE
           EXEC SQL
              INSERT INTO BILDB.BIL_DOCUMENT_REQUEST
                     (DOC_ID, ACCOUNT_ID, DOC_TYPE, REF_TYPE, REF_ID,
                      DELIVERY_METHOD, DOC_STATUS, REQUEST_DATE)
              VALUES (:DOC-DOC-ID, :DOC-ACCOUNT-ID, :DOC-DOC-TYPE,
                      :DOC-REF-TYPE, :DOC-REF-ID,
                      :DOC-DELIVERY-METHOD, :DOC-DOC-STATUS,
                      :DOC-REQUEST-DATE)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4320-INSERT-DOC-REQUEST' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE
                 SET DOC_ID    = :DOC-DOC-ID,
                     UPDATE_TS = CURRENT TIMESTAMP
               WHERE INVOICE_ID = :INV-INVOICE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4320-INSERT-DOC-REQUEST' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Product split of the invoice into the account's pending totals
       4400-ADD-PRODUCT-TOTALS.
           EXEC SQL OPEN C-INV-PRODUCTS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-ADD-PRODUCT-TOTALS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL SQLCODE = 100
              EXEC SQL
                 FETCH C-INV-PRODUCTS
                  INTO :WS-PROD-CODE, :WS-PROD-ITEMS,
                       :WS-PROD-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 5100-FIND-PRODUCT
                    ADD 1              TO WS-PR-PEND-INVOICES(PX)
                    ADD WS-PROD-ITEMS  TO WS-PR-PEND-ITEMS(PX)
                    ADD WS-PROD-AMOUNT TO WS-PR-PEND-AMOUNT(PX)
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '4400-ADD-PRODUCT-TOTALS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE C-INV-PRODUCTS END-EXEC.

      ******************************************************************
       5000-BUILD-INVOICE.
           MOVE WS-CUR-ACCOUNT-ID      TO INV-ACCOUNT-ID
           MOVE WS-ROW-STREAM-ID       TO INV-STREAM-ID
           MOVE WS-BUS-DATE            TO INV-BILL-DATE
      * BR-BIL-IVB-005 Earliest item due date, never before bill date
           IF WS-ROW-MIN-DUE < WS-BUS-DATE
              MOVE WS-BUS-DATE         TO INV-DUE-DATE
           ELSE
              MOVE WS-ROW-MIN-DUE      TO INV-DUE-DATE
           END-IF
           MOVE WS-ROW-AMOUNT          TO INV-INVOICE-AMOUNT
           MOVE WS-ROW-ITEM-COUNT      TO INV-ITEM-COUNT
      * BR-BIL-IVB-004 Prior balance only on the first invoice
           IF WS-IS-FIRST-INVOICE
              MOVE WS-PRIOR-BALANCE    TO INV-PRIOR-BALANCE
              MOVE 'N'                 TO WS-FIRST-INVOICE
           ELSE
              MOVE 0                   TO INV-PRIOR-BALANCE
           END-IF
           COMPUTE INV-AMOUNT-DUE = INV-INVOICE-AMOUNT
                                  + INV-PRIOR-BALANCE
           MOVE 'BILD'                 TO INV-INVOICE-STATUS
           MOVE 'N'                    TO INV-ADHOC-FLAG
           MOVE 0                      TO INV-DOC-ID.

      ******************************************************************
       5100-FIND-PRODUCT.
           SET PX                      TO 1
           SEARCH WS-PROD
              AT END
                 SET PX                TO 5
              WHEN WS-PR-CODE(PX) = WS-PROD-CODE
                 CONTINUE
           END-SEARCH.

      ******************************************************************
       7000-NEW-PAGE.
           ADD 1                       TO WS-PAGE
           MOVE WS-PAGE                TO RH-PAGE
           WRITE RPT-RECORD FROM RH-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           WRITE RPT-RECORD FROM R1-COL-HEAD AFTER ADVANCING 2 LINES
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           MOVE 5                      TO WS-LINES.

      ******************************************************************
      * Commit and rewrite the checkpoint with the last whole account
       7100-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7100-COMMIT-CHECKPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-T-COMMITS
           MOVE 0                      TO WS-SINCE-COMMIT
           SET CK-RUNNING              TO TRUE
           PERFORM 7110-WRITE-CHECKPOINT.

      ******************************************************************
       7110-WRITE-CHECKPOINT.
           EXEC SQL
              SET :WS-CURRENT-TS = CURRENT TIMESTAMP
           END-EXEC
           MOVE WS-PROGRAM-ID          TO CK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CK-BUS-DATE
           MOVE WS-CUR-ACCOUNT-ID      TO CK-LAST-KEY-1
           MOVE 0                      TO CK-LAST-KEY-2
           MOVE WS-T-INVOICES          TO CK-UNITS-DONE
           MOVE WS-T-COMMITS           TO CK-COMMITS
           MOVE WS-CURRENT-TS          TO CK-TIMESTAMP
           MOVE WS-RUN-START-TS        TO CK-RUN-START-TS
           OPEN OUTPUT CHKPT-FILE
           WRITE CK-RECORD
           IF WS-CK-STATUS NOT = '00'
              MOVE 'CHECKPOINT WRITE FAILED' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           CLOSE CHKPT-FILE.

      ******************************************************************
       7200-WRITE-TEXT.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           WRITE RPT-RECORD FROM RH-TEXT-LINE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES
           MOVE SPACES                 TO RH-TEXT.

      ******************************************************************
       7300-WRITE-SKIP-LINE.
           MOVE SPACES                 TO R1-DETAIL
           MOVE ACT-ACCOUNT-NUMBER     TO R1-ACCOUNT-NUMBER
           MOVE SPACES                 TO R1-INVOICE-NUMBER
           MOVE WS-ROW-STREAM-ID       TO R1-STREAM-ID
           MOVE 0                      TO R1-ITEMS R1-INVOICE-AMT
                                          R1-PRIOR-BAL R1-AMOUNT-DUE
           MOVE SPACES                 TO R1-DUE-DATE R1-REMARK.

      ******************************************************************
       7310-WRITE-DETAIL.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           WRITE RPT-RECORD FROM R1-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES.

      ******************************************************************
       7400-WRITE-TOTALS.
           PERFORM 7000-NEW-PAGE
           WRITE RPT-RECORD FROM R1-PROD-HEAD AFTER ADVANCING 2 LINES
           PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > 5
              MOVE WS-PR-CODE(PX)      TO R1-PROD-CODE
              MOVE WS-PR-NAME(PX)      TO R1-PROD-NAME
              MOVE WS-PR-INVOICES(PX)  TO R1-PROD-INVOICES
              MOVE WS-PR-ITEMS(PX)     TO R1-PROD-ITEMS
              MOVE WS-PR-AMOUNT(PX)    TO R1-PROD-AMOUNT
              WRITE RPT-RECORD FROM R1-PROD-LINE
                    AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 2 LINES
           MOVE 'BILLABLE STREAMS READ' TO RH-TOT-LABEL
           MOVE WS-T-STREAMS-READ      TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'ACCOUNTS INVOICED'    TO RH-TOT-LABEL
           MOVE WS-T-ACCTS-BILLED      TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'ACCOUNTS SKIPPED - INVC HOLD' TO RH-TOT-LABEL
           MOVE WS-T-ACCTS-HOLD        TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'ACCOUNTS SKIPPED - NOT ACTIVE' TO RH-TOT-LABEL
           MOVE WS-T-ACCTS-INACTIVE    TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'ACCOUNTS REJECTED'    TO RH-TOT-LABEL
           MOVE WS-T-ACCTS-REJECTED    TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'INVOICES CREATED / INVOICE AMOUNT' TO RH-TOT-LABEL
           MOVE WS-T-INVOICES          TO RH-TOT-COUNT
           MOVE WS-T-INVOICE-AMT       TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'INVOICE ITEMS BILLED' TO RH-TOT-LABEL
           MOVE WS-T-ITEMS             TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'PRIOR BALANCE CARRIED' TO RH-TOT-LABEL
           MOVE 0                      TO RH-TOT-COUNT
           MOVE WS-T-PRIOR-AMT         TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'TOTAL AMOUNT DUE ON INVOICES' TO RH-TOT-LABEL
           MOVE WS-T-DUE-AMT           TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'DOCUMENT REQUESTS RAISED' TO RH-TOT-LABEL
           MOVE WS-T-DOCS              TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'COMMITS (INCLUDING PRIOR RUNS TODAY)' TO RH-TOT-LABEL
           MOVE WS-T-COMMITS           TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE.

      ******************************************************************
       7410-WRITE-TOTAL-LINE.
           WRITE RPT-RECORD FROM RH-TOTAL-LINE AFTER ADVANCING 1 LINE.

      ******************************************************************
      * Service call failures: RC >= 90 abends the run, RC 12..24
      * rejects the account, lower codes are accepted
       8000-NEXT-SEQUENCE.
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE 1                      TO SQC-SQ-COUNT
           MOVE WS-BATCH-USER          TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 90
              MOVE SQC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-REASON-CODE     TO WS-REJECT-REASON
              MOVE SQC-MESSAGE         TO WS-REJECT-TEXT
              PERFORM 2900-REJECT-ACCOUNT
           END-IF.

      ******************************************************************
       8100-CHECK-INVOICE-HOLD.
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLC-REQUEST-ID
           MOVE WS-BATCH-USER          TO HLC-USER-ID
           MOVE WS-BUS-DATE            TO HLC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HLC-CALLER-PGM
           MOVE WS-CUR-ACCOUNT-ID      TO HLC-HL-ACCOUNT-ID
           MOVE 'INVC'                 TO HLC-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLC-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           IF HLC-RETURN-CODE >= 90
              MOVE HLC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF HLC-RETURN-CODE >= 12
              MOVE HLC-REASON-CODE     TO WS-REJECT-REASON
              MOVE HLC-MESSAGE         TO WS-REJECT-TEXT
              PERFORM 2900-REJECT-ACCOUNT
           END-IF.

      ******************************************************************
      * DR BILLED / CR UNBILLED; credit invoices post the reverse
       8200-POST-INVOICE-LEDGER.
           IF INV-INVOICE-AMOUNT = 0
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLC-REQUEST-ID
           MOVE WS-BATCH-USER          TO GLC-USER-ID
           MOVE WS-BUS-DATE            TO GLC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO GLC-CALLER-PGM
           MOVE 'INVBILL '             TO GLC-GL-TXN-TYPE
           MOVE INV-ACCOUNT-ID         TO GLC-GL-ACCOUNT-ID
           MOVE 'INV '                 TO GLC-GL-REF-TYPE
           MOVE INV-INVOICE-ID         TO GLC-GL-REF-ID
           MOVE 2                      TO GLC-GL-LINE-COUNT
           IF INV-INVOICE-AMOUNT > 0
              MOVE INV-INVOICE-AMOUNT  TO WS-GL-AMOUNT
              MOVE 'BILLED  '          TO GLC-GL-T-ACCOUNT(1)
              MOVE 'UNBILLED'          TO GLC-GL-T-ACCOUNT(2)
           ELSE
              COMPUTE WS-GL-AMOUNT = 0 - INV-INVOICE-AMOUNT
              MOVE 'UNBILLED'          TO GLC-GL-T-ACCOUNT(1)
              MOVE 'BILLED  '          TO GLC-GL-T-ACCOUNT(2)
           END-IF
           MOVE 'D'                    TO GLC-GL-DR-CR(1)
           MOVE 'C'                    TO GLC-GL-DR-CR(2)
           MOVE WS-GL-AMOUNT           TO GLC-GL-AMOUNT(1)
                                          GLC-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLC-RETURN-CODE >= 90
              MOVE GLC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF GLC-RETURN-CODE >= 12
              MOVE GLC-REASON-CODE     TO WS-REJECT-REASON
              MOVE GLC-MESSAGE         TO WS-REJECT-TEXT
              PERFORM 2900-REJECT-ACCOUNT
           END-IF.

      ******************************************************************
       8300-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTC-USER-ID
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 90
              MOVE DTC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       8400-REFRESH-BALANCES.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTBAL '             TO ACC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ACC-USER-ID
           MOVE WS-BUS-DATE            TO ACC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ACC-CALLER-PGM
           MOVE WS-CUR-ACCOUNT-ID      TO ACC-AC-ACCOUNT-ID
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF ACC-RETURN-CODE >= 90
              MOVE ACC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF ACC-RETURN-CODE >= 12
              MOVE ACC-REASON-CODE     TO WS-REJECT-REASON
              MOVE ACC-MESSAGE         TO WS-REJECT-TEXT
              PERFORM 2900-REJECT-ACCOUNT
           END-IF.

      ******************************************************************
      * Normal end: final commit, completed checkpoint, totals
       9000-RETURN.
           IF NOT WS-RUN-ENDED
              EXEC SQL COMMIT END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '9000-RETURN'    TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              ADD 1                    TO WS-T-COMMITS
              SET CK-COMPLETE          TO TRUE
              PERFORM 7110-WRITE-CHECKPOINT
              PERFORM 7400-WRITE-TOTALS
           END-IF
           IF WS-T-ACCTS-HOLD > 0 OR WS-T-ACCTS-INACTIVE > 0
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF
           IF WS-T-ACCTS-REJECTED > 0
              MOVE 8                   TO WS-MAX-RC
           END-IF
           MOVE WS-MAX-RC              TO WS-CNT-DISP
           STRING 'BILINVB0 ENDED - RETURN CODE ' WS-CNT-DISP
                  DELIMITED BY SIZE INTO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           CLOSE REPORT-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Abend: roll back to the last commit, leave the checkpoint at
      * the last committed account for restart, RC 16
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE C-BILLABLE END-EXEC
              SET WS-CURSOR-CLOSED     TO TRUE
           END-IF
           DISPLAY 'BILINVB0 ABEND: ' WS-ABEND-TEXT
           STRING 'RUN ABENDED - ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           MOVE 'RESTART RESUMES AFTER THE LAST CHECKPOINTED ACCOUNT'
                                       TO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           CLOSE REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           STRING 'Db2 SQLCODE ' WS-SQLCODE-DISP ' IN ' WS-ERR-PARA
                  ' KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO WS-ABEND-TEXT
           IF SQLCODE NOT = -911 AND SQLCODE NOT = -913
              INITIALIZE WS-ERR-CA
              MOVE 'ERRLOG  '          TO ERC-REQUEST-ID
              MOVE WS-BATCH-USER       TO ERC-USER-ID
              MOVE WS-PROGRAM-ID       TO ERC-CALLER-PGM
              MOVE WS-PROGRAM-ID       TO ERC-ER-PROGRAM
              MOVE WS-ERR-PARA         TO ERC-ER-PARAGRAPH
              MOVE SQLCODE             TO ERC-ER-SQLCODE
              MOVE WS-ABEND-TEXT       TO ERC-ER-TEXT
              MOVE WS-ERR-KEY          TO ERC-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9800-ABEND.
