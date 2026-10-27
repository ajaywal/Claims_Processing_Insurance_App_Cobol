       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDSBOV.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDSBOV                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Overpayment sweep. Finds accounts holding        *
      *               unapplied cash that billing no longer needs and  *
      *               raises an OVERPAY refund to the primary payer    *
      *               through BILDISB0 DSBCRT.                         *
      * PARM        : 1-4 commit frequency (default 0200)              *
      *               6-15 business date override yyyy-mm-dd           *
      *               17-23 minimum refund in pence (default 0000500)  *
      * FILES       : RESTART (I, optional) CHKPT (O) RPTOUT (O)       *
      * CALLS       : BILDISB0 BILDATES BILERRLG                       *
      * TABLES      : BIL_ACCOUNT (R) BIL_ACCOUNT_CONTACT (R)          *
      *               BIL_INVOICE_ITEM (R) BIL_POLICY_PERIOD (R)       *
      *               BIL_PAYMENT_RECEIPT (R) BIL_CONTROL (R)          *
      * RULES       : BR-BIL-DOV-001 Candidate has unapplied cash and  *
      *                              no BILD/DUE item still open       *
      *               BR-BIL-DOV-002 No in-force period with future    *
      *                              planned items (cash still needed) *
      *               BR-BIL-DOV-003 Cash must have cleared: no receipt*
      *                              received in the last 10 days      *
      *               BR-BIL-DOV-004 Balances below the minimum refund *
      *                              stay on account                   *
      *               BR-BIL-DOV-005 Payee is the primary payer; a     *
      *                              company payer is paid by name     *
      *               BR-BIL-DOV-006 Autopay accounts are refunded by  *
      *                              BACS to the autopay instrument;   *
      *                              a rejected instrument falls back  *
      *                              to cheque                         *
      *               BR-BIL-DOV-007 Each account is a savepoint: a    *
      *                              failed refund undoes only itself  *
      * RETURN CODE : 0 ok, 4 warnings, 8 rejects, 16 abend            *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       FD  RESTART-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RESTART-REC.
           COPY BILDSBK1 REPLACING LEADING ==CKP-== BY ==RST-==.
       FD  CHECKPOINT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHECKPOINT-REC.
           COPY BILDSBK1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDSBOV'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-DSB-PGM                  PIC X(8)  VALUE 'BILDISB0'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.

       01  WS-FILE-STATUS.
           05 WS-RST-STATUS            PIC XX.
      * Checkpoints are appended per commit: the last one counts
           05 WS-RESTART-SAVE          PIC X(80).
           05 WS-CKP-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-CLEAR-DAYS            PIC S9(5) COMP-3 VALUE 10.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 200.
           05 WS-MIN-REFUND            PIC S9(11)V99 COMP-3 VALUE 5.00.
           05 WS-MIN-PENCE             PIC 9(7).
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-CUTOFF-DATE           PIC X(10) VALUE SPACES.
           05 WS-RESTART-ACCT          PIC S9(9) COMP VALUE 0.
           05 WS-RESTART-NUM           PIC 9(10).
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN         VALUE 'Y'.
           05 WS-RETURN-CODE           PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-PAGE-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-NUM               PIC 9(10).

       01  WS-COUNTERS.
           05 WS-CNT-READ              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-APPROVED          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-PENDING           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-BELOW-MIN         PIC S9(9) COMP VALUE 0.
           05 WS-CNT-HELD              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-REJECTED          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-FALLBACK          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-APPROVED          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMT-PENDING           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMT-LEFT              PIC S9(11)V99 COMP-3 VALUE 0.

       01  WS-PAYEE.
           05 WS-PAYEE-NAME            PIC X(60).
           05 WS-PAYEE-ADDR1           PIC X(60).
           05 WS-PAYEE-POSTCODE        PIC X(8).
           05 WS-PAY-METHOD            PIC X(4).
           05 WS-INSTR-ID              PIC S9(9) COMP.
           05 WS-RESULT                PIC X(18).
           05 WS-OUTCOME               PIC X.
              88 WS-OUT-APPROVED        VALUE 'A'.
              88 WS-OUT-PENDING         VALUE 'P'.
              88 WS-OUT-HELD            VALUE 'H'.
              88 WS-OUT-REJECTED        VALUE 'R'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDACN END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-DOV-001 / 002 / 003 candidate selection
           EXEC SQL DECLARE OVP-CSR CURSOR WITH HOLD FOR
              SELECT A.ACCOUNT_ID, A.ACCOUNT_NUMBER, A.UNAPPLIED_AMT,
                     A.AUTOPAY_FLAG, A.AUTOPAY_INSTR_ID
                FROM BILDB.BIL_ACCOUNT A
               WHERE A.UNAPPLIED_AMT > 0
                 AND A.ACCOUNT_ID    > :WS-RESTART-ACCT
                 AND NOT EXISTS
                     (SELECT 1
                        FROM BILDB.BIL_INVOICE_ITEM I
                       WHERE I.ACCOUNT_ID  = A.ACCOUNT_ID
                         AND I.ITEM_STATUS IN ('BILD', 'DUE '))
                 AND NOT EXISTS
                     (SELECT 1
                        FROM BILDB.BIL_POLICY_PERIOD P,
                             BILDB.BIL_INVOICE_ITEM F
                       WHERE P.ACCOUNT_ID  = A.ACCOUNT_ID
                         AND P.PP_STATUS   = 'INFC'
                         AND F.PP_ID       = P.PP_ID
                         AND F.ITEM_STATUS = 'PLAN'
                         AND F.EVENT_DATE  > :WS-BUS-DATE)
                 AND NOT EXISTS
                     (SELECT 1
                        FROM BILDB.BIL_PAYMENT_RECEIPT R
                       WHERE R.ACCOUNT_ID    = A.ACCOUNT_ID
                         AND R.RECEIVED_DATE > :WS-CUTOFF-DATE)
               ORDER BY A.ACCOUNT_ID
               FOR FETCH ONLY
           END-EXEC.

       01  WS-DSB-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DBA-==.
           03 DBA-BODY.
              COPY BILCDSB REPLACING LEADING ==CA-== BY ==DBA-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILDSBR1.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-ACCOUNTS
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILDSBOV OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 1100-READ-PARM
           IF WS-BUS-DATE = SPACES
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           MOVE WS-BUS-DATE            TO OVR-H1-BUS-DATE
      * BR-BIL-DOV-003 Receipts on or after the cut-off are uncleared
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-1
           COMPUTE DTA-DT-NUMBER = 0 - WS-CLEAR-DAYS
           PERFORM 8100-CALL-DATES
           MOVE DTA-DT-RESULT-DATE     TO WS-CUTOFF-DATE
           PERFORM 1200-READ-RESTART.

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
                 DISPLAY 'BILDSBOV INVALID PARM DATE '
                         LS-PARM-DATA(6:10)
                 MOVE 16               TO RETURN-CODE
                 GOBACK
              END-IF
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           END-IF
           IF LS-PARM-LEN >= 23 AND LS-PARM-DATA(17:7) IS NUMERIC
              MOVE LS-PARM-DATA(17:7)  TO WS-MIN-PENCE
              COMPUTE WS-MIN-REFUND = WS-MIN-PENCE / 100
           END-IF.

      * A restart record for this program and business date resumes
      * after the last committed account.
       1200-READ-RESTART.
           OPEN INPUT RESTART-FILE
           IF WS-RST-STATUS = '00'
              MOVE SPACES              TO WS-RESTART-SAVE
              PERFORM UNTIL WS-RST-STATUS NOT = '00'
                 READ RESTART-FILE
                 IF WS-RST-STATUS = '00'
                    MOVE RESTART-REC   TO WS-RESTART-SAVE
                 END-IF
              END-PERFORM
              MOVE WS-RESTART-SAVE     TO RESTART-REC
              IF RST-PROGRAM = WS-PROGRAM-ID
                 AND RST-RUN-DATE = WS-BUS-DATE
                 AND RST-RUNNING
                 AND RST-RESTART-KEY(1:10) IS NUMERIC
                 MOVE RST-RESTART-KEY(1:10) TO WS-RESTART-NUM
                 MOVE WS-RESTART-NUM TO WS-RESTART-ACCT
                 MOVE SPACES     TO OVR-M-TEXT
                 STRING 'RESTARTING AFTER ACCOUNT ID '
                        RST-RESTART-KEY(1:10)
                        DELIMITED BY SIZE INTO OVR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
              END-IF
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
       2000-PROCESS-ACCOUNTS.
           EXEC SQL OPEN OVP-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-ACCOUNTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4100-FETCH-ACCOUNT
           PERFORM UNTIL WS-EOF
              PERFORM 2100-PROCESS-ACCOUNT
              ADD 1                    TO WS-SINCE-COMMIT
              IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
              END-IF
              PERFORM 4100-FETCH-ACCOUNT
           END-PERFORM
           EXEC SQL CLOSE OVP-CSR END-EXEC
           MOVE 'N'                    TO WS-CURSOR-FLAG.

      ******************************************************************
       2100-PROCESS-ACCOUNT.
           ADD 1                       TO WS-CNT-READ
           MOVE ACT-ACCOUNT-ID         TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           INITIALIZE OVR-DETAIL
           MOVE ACT-ACCOUNT-NUMBER     TO OVR-D-ACCOUNT-NO
           MOVE ACT-UNAPPLIED-AMT      TO OVR-D-UNAPPLIED
      * BR-BIL-DOV-004 Small balances stay on the account
           IF ACT-UNAPPLIED-AMT < WS-MIN-REFUND
              ADD 1                    TO WS-CNT-BELOW-MIN
              ADD ACT-UNAPPLIED-AMT    TO WS-AMT-LEFT
              MOVE 'BELOW MINIMUM'     TO OVR-D-RESULT
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           PERFORM 4200-READ-PRIMARY-PAYER
           IF SQLCODE = 100
              ADD 1                    TO WS-CNT-REJECTED
              MOVE 'NO PRIMARY PAYER'  TO OVR-D-RESULT
              PERFORM 6000-WRITE-DETAIL
              PERFORM 5900-SET-REJECT-RC
              EXIT PARAGRAPH
           END-IF
           PERFORM 5000-BUILD-PAYEE
           PERFORM 4300-SET-SAVEPOINT
           PERFORM 8000-CREATE-REFUND
      * BR-BIL-DOV-006 Instrument rejected -> retry as cheque
           IF DBA-RC-VALIDATION AND DBA-REASON-CODE = 'DSBINSTR'
              AND WS-PAY-METHOD = 'BACS'
              ADD 1                    TO WS-CNT-FALLBACK
              MOVE 'CHEQ'              TO WS-PAY-METHOD
              MOVE 0                   TO WS-INSTR-ID
              PERFORM 8000-CREATE-REFUND
           END-IF
           PERFORM 2200-EVALUATE-OUTCOME
           MOVE WS-PAY-METHOD          TO OVR-D-METHOD
           MOVE WS-PAYEE-NAME          TO OVR-D-PAYEE
           MOVE WS-RESULT              TO OVR-D-RESULT
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
       2200-EVALUATE-OUTCOME.
           EVALUATE TRUE
              WHEN DBA-RC-OK
                 SET WS-OUT-APPROVED   TO TRUE
                 ADD 1                 TO WS-CNT-APPROVED
                 ADD DBA-DB-AMOUNT     TO WS-AMT-APPROVED
                 MOVE 'REFUND APPROVED' TO WS-RESULT
              WHEN DBA-RC-WARNING
                 SET WS-OUT-PENDING    TO TRUE
                 ADD 1                 TO WS-CNT-PENDING
                 ADD DBA-DB-AMOUNT     TO WS-AMT-PENDING
                 STRING 'PEND ' DBA-DB-APPROVER DELIMITED BY SIZE
                        INTO WS-RESULT
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
              WHEN DBA-RETURN-CODE >= 90
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 MOVE SPACES           TO OVR-M-TEXT
                 STRING 'BILDISB0 FAILED ' DBA-REASON-CODE ' '
                        DBA-MESSAGE DELIMITED BY SIZE
                        INTO OVR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
              WHEN DBA-RC-CONFLICT AND DBA-REASON-CODE = 'DSBDUP  '
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 SET WS-OUT-HELD       TO TRUE
                 ADD 1                 TO WS-CNT-HELD
                 MOVE 'ALREADY RAISED'   TO WS-RESULT
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
              WHEN DBA-RC-STATE AND DBA-REASON-CODE = 'DSBHOLD '
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 SET WS-OUT-HELD       TO TRUE
                 ADD 1                 TO WS-CNT-HELD
                 MOVE 'DISB HOLD ACTIVE' TO WS-RESULT
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
              WHEN OTHER
      * BR-BIL-DOV-007 Undo only this account's partial work
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 SET WS-OUT-REJECTED   TO TRUE
                 ADD 1                 TO WS-CNT-REJECTED
                 STRING DBA-REASON-CODE ' RC ' DBA-RETURN-CODE
                        DELIMITED BY SIZE INTO WS-RESULT
                 MOVE SPACES           TO OVR-M-TEXT
                 STRING '  ' ACT-ACCOUNT-NUMBER ' ' DBA-MESSAGE
                        DELIMITED BY SIZE INTO OVR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 5900-SET-REJECT-RC
           END-EVALUATE
           IF WS-OUT-APPROVED OR WS-OUT-PENDING
              MOVE DBA-DB-AMOUNT       TO OVR-D-REFUND
              MOVE DBA-DB-DISB-ID      TO OVR-D-DISB-ID
              MOVE DBA-DB-STATUS       TO OVR-D-STATUS
           END-IF.

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

       4100-FETCH-ACCOUNT.
           EXEC SQL
              FETCH OVP-CSR
               INTO :ACT-ACCOUNT-ID, :ACT-ACCOUNT-NUMBER,
                    :ACT-UNAPPLIED-AMT, :ACT-AUTOPAY-FLAG,
                    :ACT-AUTOPAY-INSTR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * BR-BIL-DOV-005 Lowest-numbered primary contact is the payer
       4200-READ-PRIMARY-PAYER.
           MOVE ACT-ACCOUNT-ID         TO ACN-ACCOUNT-ID
           EXEC SQL
              SELECT CONTACT_TYPE, FIRST_NAME, LAST_NAME,
                     COMPANY_NAME, ADDRESS_LINE1, POSTCODE
                INTO :ACN-CONTACT-TYPE,
                     :ACN-FIRST-NAME   :ACN-FIRST-NAME-IND,
                     :ACN-LAST-NAME    :ACN-LAST-NAME-IND,
                     :ACN-COMPANY-NAME :ACN-COMPANY-NAME-IND,
                     :ACN-ADDRESS-LINE1, :ACN-POSTCODE
                FROM BILDB.BIL_ACCOUNT_CONTACT
               WHERE ACCOUNT_ID   = :ACN-ACCOUNT-ID
                 AND PRIMARY_FLAG = 'Y'
               ORDER BY CONTACT_SEQ
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-READ-PRIMARY-PAYER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      * BR-BIL-DOV-007 Savepoint per account; cursors are retained
       4300-SET-SAVEPOINT.
           EXEC SQL
              SAVEPOINT DSBOV_ACCT ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-SET-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4400-ROLLBACK-SAVEPOINT.
           EXEC SQL
              ROLLBACK TO SAVEPOINT DSBOV_ACCT
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-ROLLBACK-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-DOV-005 / 006 payee and payment route
       5000-BUILD-PAYEE.
           MOVE SPACES                 TO WS-PAYEE-NAME
           IF ACN-CONTACT-TYPE = 'C' AND ACN-COMPANY-NAME-IND >= 0
              MOVE ACN-COMPANY-NAME-TEXT TO WS-PAYEE-NAME
           ELSE
              IF ACN-FIRST-NAME-IND < 0
                 MOVE SPACES           TO ACN-FIRST-NAME-TEXT
              END-IF
              IF ACN-LAST-NAME-IND < 0
                 MOVE SPACES           TO ACN-LAST-NAME-TEXT
              END-IF
              STRING ACN-FIRST-NAME-TEXT DELIMITED BY '  '
                     ' '                 DELIMITED BY SIZE
                     ACN-LAST-NAME-TEXT  DELIMITED BY '  '
                     INTO WS-PAYEE-NAME
           END-IF
           MOVE ACN-ADDRESS-LINE1-TEXT TO WS-PAYEE-ADDR1
           MOVE ACN-POSTCODE           TO WS-PAYEE-POSTCODE
           IF ACT-AUTOPAY-FLAG = 'Y' AND ACT-AUTOPAY-INSTR-ID > 0
              MOVE 'BACS'              TO WS-PAY-METHOD
              MOVE ACT-AUTOPAY-INSTR-ID TO WS-INSTR-ID
           ELSE
              MOVE 'CHEQ'              TO WS-PAY-METHOD
              MOVE 0                   TO WS-INSTR-ID
           END-IF.

       5900-SET-REJECT-RC.
           IF WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM OVR-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO OVR-H1-PAGE
           WRITE REPORT-REC FROM OVR-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM OVR-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM OVR-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6300-WRITE-TOTAL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM OVR-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE 0                      TO OVR-T-AMOUNT.

       6900-WRITE-CONTROL-TOTALS.
           MOVE SPACES                 TO OVR-M-TEXT
           MOVE '*** CONTROL TOTALS ***' TO OVR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'ACCOUNTS SELECTED'    TO OVR-T-LABEL
           MOVE WS-CNT-READ            TO OVR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REFUNDS CREATED - APPROVED' TO OVR-T-LABEL
           MOVE WS-CNT-APPROVED        TO OVR-T-COUNT
           MOVE WS-AMT-APPROVED        TO OVR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REFUNDS CREATED - PENDING APPROVAL' TO OVR-T-LABEL
           MOVE WS-CNT-PENDING         TO OVR-T-COUNT
           MOVE WS-AMT-PENDING         TO OVR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'BACS REJECTED - SENT AS CHEQUE' TO OVR-T-LABEL
           MOVE WS-CNT-FALLBACK        TO OVR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'LEFT ON ACCOUNT - BELOW MINIMUM' TO OVR-T-LABEL
           MOVE WS-CNT-BELOW-MIN       TO OVR-T-COUNT
           MOVE WS-AMT-LEFT            TO OVR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SKIPPED - HOLD OR ALREADY RAISED' TO OVR-T-LABEL
           MOVE WS-CNT-HELD            TO OVR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REJECTED'             TO OVR-T-LABEL
           MOVE WS-CNT-REJECTED        TO OVR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO OVR-T-LABEL
           MOVE WS-CNT-COMMITS         TO OVR-T-COUNT
           PERFORM 6300-WRITE-TOTAL.

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
           MOVE ACT-ACCOUNT-ID         TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO CKP-RESTART-KEY
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           COMPUTE CKP-DONE-COUNT = WS-CNT-APPROVED + WS-CNT-PENDING
           SET CKP-RUNNING             TO TRUE
           IF WS-EOF
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILDSBOV CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       8000-CREATE-REFUND.
           INITIALIZE WS-DSB-CA
           MOVE 'DSBCRT  '             TO DBA-REQUEST-ID
           MOVE WS-BATCH-USER          TO DBA-USER-ID
           MOVE WS-BUS-DATE            TO DBA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DBA-CALLER-PGM
           MOVE ACT-ACCOUNT-ID         TO DBA-DB-ACCOUNT-ID
           MOVE ACT-UNAPPLIED-AMT      TO DBA-DB-AMOUNT
           MOVE 'OVERPAY '             TO DBA-DB-REASON
           MOVE WS-PAYEE-NAME          TO DBA-DB-PAYEE-NAME
           MOVE WS-PAYEE-ADDR1         TO DBA-DB-PAYEE-ADDR1
           MOVE WS-PAYEE-POSTCODE      TO DBA-DB-PAYEE-POSTCODE
           MOVE WS-PAY-METHOD          TO DBA-DB-PAY-METHOD
           MOVE WS-INSTR-ID            TO DBA-DB-INSTR-ID
           MOVE WS-BUS-DATE            TO DBA-DB-DUE-DATE
           MOVE 'BILL'                 TO DBA-DB-SOURCE-SYSTEM
           STRING 'OV' WS-BUS-DATE(1:4) WS-BUS-DATE(6:2)
                  WS-BUS-DATE(9:2) ACT-ACCOUNT-NUMBER
                  DELIMITED BY SIZE INTO DBA-DB-SOURCE-REF
           CALL WS-DSB-PGM USING WS-DSB-CA.

       8100-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              DISPLAY 'BILDSBOV BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       9000-TERMINATE.
           PERFORM 7000-COMMIT
           IF WS-PAGE-COUNT = 0
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILDSBOV COMPLETE READ=' WS-CNT-READ
                   ' APPR=' WS-CNT-APPROVED ' PEND=' WS-CNT-PENDING
                   ' REJ=' WS-CNT-REJECTED ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      * Unrecoverable: back out the open unit of work, keep the last
      * checkpoint for restart.
       9800-ABEND.
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE OVP-CSR END-EXEC
              MOVE 'N'                 TO WS-CURSOR-FLAG
           END-IF
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO OVR-M-TEXT
           STRING '*** BILDSBOV ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK. LAST KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO OVR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILDSBOV DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA ' KEY ' WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           STRING 'Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9800-ABEND.
