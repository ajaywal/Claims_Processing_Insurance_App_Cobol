       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILGAPIN.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILGAPIN                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : GenApp policy intake. GenApp has no event queue, *
      *               so this job reads GENAPP.POLICY rows changed     *
      *               since the high-water mark held in BIL_CONTROL    *
      *               'GENAPPHW' (LAST_EOD_TS), derives the annual     *
      *               premium from the product detail table and raises *
      *               the billing side directly: POLISSUE for a policy *
      *               billing has never seen (account found or created *
      *               from GENAPP.CUSTOMER, then BILPPER0 PPNEW) and   *
      *               POLCHNG when GenApp's premium differs from the   *
      *               billed premium (BILPPER0 PPCHG). A rejected      *
      *               policy is queued as an inbound POL* event so the *
      *               event drainer BILEVTDR retries it.               *
      * PARM        : 1-4 commit frequency (default 0100)              *
      *               6-15 business date override yyyy-mm-dd           *
      * FILES       : CHKPT (O) RPTOUT (O). The high-water mark is the *
      *               restart key: it is committed with the work, so a *
      *               rerun simply resumes after it.                   *
      * CALLS       : BILACCT0 BILPROD0 BILPPER0 BILMONEY BILDATES     *
      *               BILEVTQ BILERRLG                                 *
      * TABLES      : GENAPP.POLICY GENAPP.CUSTOMER GENAPP.MOTOR       *
      *               GENAPP.HOUSE GENAPP.COMMERCIAL (R)               *
      *               BIL_POLICY_PERIOD (R) BIL_CONTROL (C R U)        *
      * RULES       : BR-BIL-GAP-001 Policies changed after the high-  *
      *                              water mark, oldest change first   *
      *               BR-BIL-GAP-002 Product E (life) is not billed    *
      *               BR-BIL-GAP-003 Annual premium: motor = MOTOR     *
      *                              premium; house = 0.30% of the     *
      *                              rebuild value; commercial = sum   *
      *                              of fire, crime, flood and weather *
      *                              premiums                          *
      *               BR-BIL-GAP-004 Default plan MNTHLY10 for motor   *
      *                              and house, QTRLY4 for commercial  *
      *               BR-BIL-GAP-005 New policy: billing account by    *
      *                              GenApp customer, created from     *
      *                              GENAPP.CUSTOMER when missing      *
      *               BR-BIL-GAP-006 Broker id is the producer code;   *
      *                              an agency-bill producer makes the *
      *                              period agency billed (AB)         *
      *               BR-BIL-GAP-007 Known policy is re-priced only    *
      *                              when premium differs and the      *
      *                              period is not cancelled; change   *
      *                              is effective on the GenApp change *
      *                              date, never before the period     *
      *               BR-BIL-GAP-008 Commits fall only on a change     *
      *                              timestamp boundary and move the   *
      *                              high-water mark with them         *
      *               BR-BIL-GAP-009 A rejected policy is queued as an *
      *                              inbound POL* event for retry      *
      * RETURN CODE : 0 ok, 4 warnings, 8 rejects, 16 abend            *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECKPOINT-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CKP-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
       DATA DIVISION.
       FILE SECTION.
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
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILGAPIN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-PRD-PGM                  PIC X(8)  VALUE 'BILPROD0'.
       01  WS-PPR-PGM                  PIC X(8)  VALUE 'BILPPER0'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'BILMONEY'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'BILEVTQ '.

       01  WS-FILE-STATUS.
           05 WS-CKP-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-HW-KEY                PIC X(8)  VALUE 'GENAPPHW'.
           05 WS-LOW-TS                PIC X(26) VALUE
              '0001-01-01-00.00.00.000000'.
           05 WS-HOUSE-RATE-BPS        PIC S9(4) COMP VALUE 30.
           05 WS-PLAN-PERSONAL         PIC X(8)  VALUE 'MNTHLY10'.
           05 WS-PLAN-COMMERCIAL       PIC X(8)  VALUE 'QTRLY4  '.
           05 WS-DEFAULT-DLQ-PLAN      PIC X(8)  VALUE 'STANDARD'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 100.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-HW-TS                 PIC X(26) VALUE SPACES.
           05 WS-LAST-DONE-TS          PIC X(26) VALUE SPACES.
           05 WS-COMMIT-DUE            PIC X     VALUE 'N'.
              88 WS-COMMIT-IS-DUE       VALUE 'Y'.
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

       01  WS-POLICY-WORK.
           05 WS-PREMIUM               PIC S9(11)V99 COMP-3.
           05 WS-PERIL-SUM             PIC S9(11)V99 COMP-3.
           05 WS-PRODUCT               PIC X.
              88 WS-PRD-MOTOR           VALUE 'M'.
              88 WS-PRD-HOUSE           VALUE 'H'.
              88 WS-PRD-COMMERCIAL      VALUE 'C'.
              88 WS-PRD-LIFE            VALUE 'E'.
           05 WS-KNOWN-FLAG            PIC X.
              88 WS-POLICY-KNOWN        VALUE 'Y'.
           05 WS-OUTCOME               PIC X.
              88 WS-OUT-DONE            VALUE 'D'.
              88 WS-OUT-SKIP            VALUE 'S'.
              88 WS-OUT-REJECT          VALUE 'R'.
           05 WS-EFF-DATE              PIC X(10).
           05 WS-EXP-DATE              PIC X(10).
           05 WS-CHANGE-DATE           PIC X(10).
           05 WS-ACCOUNT-ID            PIC S9(9) COMP.
           05 WS-ACCOUNT-NUMBER        PIC X(10).
           05 WS-BROKER-ID             PIC S9(9) COMP.
           05 WS-BROKER-DISP           PIC 9(10).
           05 WS-BILL-METHOD           PIC X(2).
           05 WS-PLAN-CODE             PIC X(8).
           05 WS-RESULT                PIC X(31).
           05 WS-FAIL-REASON           PIC X(8).
           05 WS-FAIL-TEXT             PIC X(80).

       01  WS-COUNTERS.
           05 WS-CNT-READ              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-ISSUED            PIC S9(9) COMP VALUE 0.
           05 WS-CNT-CHANGED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-NO-CHANGE         PIC S9(9) COMP VALUE 0.
           05 WS-CNT-LIFE              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SKIPPED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-ACCT-CREATED      PIC S9(9) COMP VALUE 0.
           05 WS-CNT-REJECTED          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-QUEUED            PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-ISSUED            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMT-CHANGE            PIC S9(11)V99 COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE GENDPOL END-EXEC.
           EXEC SQL INCLUDE GENDCUS END-EXEC.
           EXEC SQL INCLUDE GENDMOT END-EXEC.
           EXEC SQL INCLUDE GENDHOU END-EXEC.
           EXEC SQL INCLUDE GENDCOM END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-GAP-001 Changes after the high-water mark, in order
           EXEC SQL DECLARE GAP-CSR CURSOR WITH HOLD FOR
              SELECT POLICYNUMBER, CUSTOMERNUMBER, ISSUEDATE,
                     EXPIRYDATE, POLICYTYPE, LASTCHANGED, BROKERID
                FROM GENAPP.POLICY
               WHERE LASTCHANGED > :WS-HW-TS
               ORDER BY LASTCHANGED, POLICYNUMBER
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.
       01  WS-PRD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PRA-==.
           03 PRA-BODY.
              COPY BILCPRD REPLACING LEADING ==CA-== BY ==PRA-==.
       01  WS-PPR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PPA-==.
           03 PPA-BODY.
              COPY BILCPPR REPLACING LEADING ==CA-== BY ==PPA-==.
       01  WS-MNY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY BILCMNY REPLACING LEADING ==CA-== BY ==MNA-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-EVT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==EVA-==.
           03 EVA-BODY.
              COPY BILCEVT REPLACING LEADING ==CA-== BY ==EVA-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILEVTP1.
           COPY BILGAPR1.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-CHANGES
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILGAPIN OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           IF LS-PARM-LEN >= 4 AND LS-PARM-DATA(1:4) IS NUMERIC
              MOVE LS-PARM-DATA(1:4)   TO WS-COMMIT-FREQ
              IF WS-COMMIT-FREQ = 0
                 MOVE 100              TO WS-COMMIT-FREQ
              END-IF
           END-IF
           PERFORM 4000-READ-BUSINESS-DATE
           IF LS-PARM-LEN >= 15 AND LS-PARM-DATA(6:10) NOT = SPACES
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           END-IF
           MOVE WS-BUS-DATE            TO GPR-H1-BUS-DATE
           PERFORM 4100-READ-HIGH-WATER
           MOVE WS-HW-TS               TO WS-LAST-DONE-TS
           MOVE SPACES                 TO GPR-M-TEXT
           STRING 'PROCESSING GENAPP CHANGES AFTER ' WS-HW-TS
                  DELIMITED BY SIZE INTO GPR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE.

      ******************************************************************
       2000-PROCESS-CHANGES.
           EXEC SQL OPEN GAP-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-CHANGES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4200-FETCH-POLICY
           PERFORM UNTIL WS-EOF
      * BR-BIL-GAP-008 Commit only between distinct change times so
      * rows sharing a LASTCHANGED value are never split by the mark
              IF WS-COMMIT-IS-DUE
                 AND GPO-LASTCHANGED > WS-LAST-DONE-TS
                 PERFORM 7000-COMMIT
              END-IF
              PERFORM 2100-PROCESS-POLICY
              MOVE GPO-LASTCHANGED     TO WS-LAST-DONE-TS
              ADD 1                    TO WS-SINCE-COMMIT
              IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 SET WS-COMMIT-IS-DUE  TO TRUE
              END-IF
              PERFORM 4200-FETCH-POLICY
           END-PERFORM
           EXEC SQL CLOSE GAP-CSR END-EXEC
           MOVE 'N'                    TO WS-CURSOR-FLAG.

       2100-PROCESS-POLICY.
           ADD 1                       TO WS-CNT-READ
           MOVE GPO-POLICYNUMBER       TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           INITIALIZE GPR-DETAIL WS-POLICY-WORK
           MOVE GPO-POLICYNUMBER       TO GPR-D-POLICY
           MOVE GPO-CUSTOMERNUMBER     TO GPR-D-CUSTOMER
           IF GPO-POLICYTYPE-IND < 0
              MOVE SPACE               TO GPO-POLICYTYPE
           END-IF
           MOVE GPO-POLICYTYPE         TO WS-PRODUCT GPR-D-PRODUCT
      * BR-BIL-GAP-002 Life business is not billed here
           IF WS-PRD-LIFE
              ADD 1                    TO WS-CNT-LIFE
              MOVE 'SKIP'              TO GPR-D-EVENT
              MOVE 'life product not billed' TO GPR-D-RESULT
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           IF NOT WS-PRD-MOTOR AND NOT WS-PRD-HOUSE
              AND NOT WS-PRD-COMMERCIAL
              MOVE 'UNKNOWN PRODUCT TYPE' TO WS-RESULT
              PERFORM 2900-REJECT
              EXIT PARAGRAPH
           END-IF
           PERFORM 5000-DERIVE-PREMIUM
           IF WS-OUT-REJECT
              PERFORM 2900-REJECT
              EXIT PARAGRAPH
           END-IF
           MOVE WS-PREMIUM             TO GPR-D-NEW-PREMIUM
           PERFORM 4300-READ-BILLED-PERIOD
           EXEC SQL
              SAVEPOINT GAPIN_POL ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-PROCESS-POLICY' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-POLICY-KNOWN
              PERFORM 2300-POLICY-CHANGE
           ELSE
              PERFORM 2200-POLICY-ISSUE
           END-IF
           EVALUATE TRUE
              WHEN WS-OUT-REJECT
                 EXEC SQL
                    ROLLBACK TO SAVEPOINT GAPIN_POL
                 END-EXEC
                 IF SQLCODE NOT = 0
                    MOVE '2100-PROCESS-POLICY' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
                 END-IF
                 PERFORM 2900-REJECT
                 PERFORM 8600-QUEUE-FOR-RETRY
              WHEN WS-OUT-SKIP
                 MOVE WS-RESULT        TO GPR-D-RESULT
                 PERFORM 6000-WRITE-DETAIL
              WHEN OTHER
                 MOVE WS-ACCOUNT-NUMBER TO GPR-D-ACCOUNT
                 MOVE PPA-PP-PP-ID     TO GPR-D-PP-ID
                 MOVE WS-RESULT        TO GPR-D-RESULT
                 PERFORM 6000-WRITE-DETAIL
           END-EVALUATE.

      ******************************************************************
      * POLISSUE - first sight of the policy in billing
       2200-POLICY-ISSUE.
           MOVE 'POLISSUE'             TO GPR-D-EVENT
           IF GPO-ISSUEDATE-IND < 0
              SET WS-OUT-REJECT        TO TRUE
              MOVE 'NO ISSUE DATE IN GENAPP' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           MOVE GPO-ISSUEDATE          TO WS-EFF-DATE
           IF GPO-EXPIRYDATE-IND < 0
              INITIALIZE WS-DTE-CA
              MOVE 'DTADDM  '          TO DTA-REQUEST-ID
              MOVE WS-EFF-DATE         TO DTA-DT-DATE-1
              MOVE 12                  TO DTA-DT-NUMBER
              PERFORM 8500-CALL-DATES
              MOVE DTA-DT-RESULT-DATE  TO WS-EXP-DATE
           ELSE
              MOVE GPO-EXPIRYDATE      TO WS-EXP-DATE
           END-IF
      * BR-BIL-GAP-004 Default payment plan by product
           IF WS-PRD-COMMERCIAL
              MOVE WS-PLAN-COMMERCIAL  TO WS-PLAN-CODE
           ELSE
              MOVE WS-PLAN-PERSONAL    TO WS-PLAN-CODE
           END-IF
           PERFORM 8100-FIND-PRODUCER
           PERFORM 8000-FIND-OR-CREATE-ACCOUNT
           IF WS-OUT-REJECT
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-PPR-CA
           MOVE 'PPNEW   '             TO PPA-REQUEST-ID
           PERFORM 8300-PRIME-PERIOD-CALL
           MOVE WS-ACCOUNT-ID          TO PPA-PP-ACCOUNT-ID
           MOVE 1                      TO PPA-PP-TERM-NUMBER
           MOVE WS-PRODUCT             TO PPA-PP-PRODUCT-CODE
           MOVE WS-EFF-DATE            TO PPA-PP-EFF-DATE
           MOVE WS-EXP-DATE            TO PPA-PP-EXP-DATE
           MOVE WS-PLAN-CODE           TO PPA-PP-PLAN-CODE
           MOVE WS-BILL-METHOD         TO PPA-PP-BILLING-METHOD
           MOVE WS-BROKER-ID           TO PPA-PP-PRODUCER-CODE
           MOVE WS-PREMIUM             TO PPA-PP-TOTAL-PREMIUM
           PERFORM 8400-CALL-PERIOD
           IF NOT WS-OUT-REJECT
              ADD 1                    TO WS-CNT-ISSUED
              ADD WS-PREMIUM           TO WS-AMT-ISSUED
              STRING 'ISSUED ' WS-PLAN-CODE ' ' WS-BILL-METHOD
                     DELIMITED BY SIZE INTO WS-RESULT
           END-IF.

      ******************************************************************
      * POLCHNG - GenApp premium no longer matches the billed premium
       2300-POLICY-CHANGE.
           MOVE 'POLCHNG '             TO GPR-D-EVENT
           MOVE PPR-TOTAL-PREMIUM      TO GPR-D-OLD-PREMIUM
           MOVE PPR-ACCOUNT-ID         TO WS-ACCOUNT-ID
           MOVE PPR-PRODUCT-CODE       TO GPR-D-PRODUCT
      * BR-BIL-GAP-007 Re-price only a live period whose premium moved
           IF PPR-PP-STATUS = 'CANC'
              SET WS-OUT-SKIP          TO TRUE
              ADD 1                    TO WS-CNT-SKIPPED
              MOVE 'PERIOD CANCELLED - NOT REPRICED' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           IF WS-PREMIUM = PPR-TOTAL-PREMIUM
              SET WS-OUT-SKIP          TO TRUE
              ADD 1                    TO WS-CNT-NO-CHANGE
              MOVE 'NO BILLING CHANGE' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           MOVE GPO-LASTCHANGED(1:10)  TO WS-CHANGE-DATE
           IF WS-CHANGE-DATE < PPR-EFF-DATE
              MOVE PPR-EFF-DATE        TO WS-CHANGE-DATE
           END-IF
           IF WS-CHANGE-DATE NOT < PPR-EXP-DATE
              SET WS-OUT-SKIP          TO TRUE
              ADD 1                    TO WS-CNT-SKIPPED
              MOVE 'CHANGE AFTER PERIOD EXPIRY' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-PPR-CA
           MOVE 'PPCHG   '             TO PPA-REQUEST-ID
           PERFORM 8300-PRIME-PERIOD-CALL
           MOVE PPR-PP-ID              TO PPA-PP-PP-ID
           MOVE PPR-ACCOUNT-ID         TO PPA-PP-ACCOUNT-ID
           MOVE PPR-TERM-NUMBER        TO PPA-PP-TERM-NUMBER
           MOVE WS-PREMIUM             TO PPA-PP-TOTAL-PREMIUM
           COMPUTE PPA-PP-CHANGE-AMOUNT = WS-PREMIUM
                                        - PPR-TOTAL-PREMIUM
           MOVE WS-CHANGE-DATE         TO PPA-PP-CHANGE-EFF-DATE
           PERFORM 8400-CALL-PERIOD
           IF NOT WS-OUT-REJECT
              ADD 1                    TO WS-CNT-CHANGED
              ADD PPA-PP-CHANGE-AMOUNT TO WS-AMT-CHANGE
              STRING 'CHANGED EFF ' WS-CHANGE-DATE
                     DELIMITED BY SIZE INTO WS-RESULT
           END-IF.

       2900-REJECT.
           ADD 1                       TO WS-CNT-REJECTED
           MOVE WS-RESULT              TO GPR-D-RESULT
           PERFORM 6000-WRITE-DETAIL
           IF WS-FAIL-TEXT NOT = SPACES
              MOVE SPACES              TO GPR-M-TEXT
              STRING '            ' WS-FAIL-REASON ' ' WS-FAIL-TEXT
                     DELIMITED BY SIZE INTO GPR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
           END-IF
           IF WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
       4000-READ-BUSINESS-DATE.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE, PRIOR_BUS_DATE
                INTO :CTL-BUSINESS-DATE, :CTL-PRIOR-BUS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-BUSINESS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      * The first run creates the mark row at the low timestamp so the
      * whole GenApp book is taken on.
       4100-READ-HIGH-WATER.
           MOVE WS-HW-KEY              TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT LAST_EOD_TS
                INTO :CTL-LAST-EOD-TS :CTL-LAST-EOD-TS-IND
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CTL-LAST-EOD-TS-IND < 0
                    MOVE WS-LOW-TS     TO WS-HW-TS
                 ELSE
                    MOVE CTL-LAST-EOD-TS TO WS-HW-TS
                 END-IF
              WHEN 100
                 MOVE WS-LOW-TS        TO WS-HW-TS CTL-LAST-EOD-TS
                 EXEC SQL
                    INSERT INTO BILDB.BIL_CONTROL
                         ( CONTROL_KEY, BUSINESS_DATE, PRIOR_BUS_DATE,
                           BATCH_STATUS, LAST_EOD_TS, CURRENCY_CODE,
                           UPDATE_TS )
                    VALUES
                         ( :CTL-CONTROL-KEY, :CTL-BUSINESS-DATE,
                           :CTL-PRIOR-BUS-DATE, 'O', :CTL-LAST-EOD-TS,
                           'GBP', CURRENT TIMESTAMP )
                 END-EXEC
                 IF SQLCODE NOT = 0
                    MOVE '4100-READ-HIGH-WATER' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
                 END-IF
              WHEN OTHER
                 MOVE '4100-READ-HIGH-WATER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4200-FETCH-POLICY.
           EXEC SQL
              FETCH GAP-CSR
               INTO :GPO-POLICYNUMBER, :GPO-CUSTOMERNUMBER,
                    :GPO-ISSUEDATE  :GPO-ISSUEDATE-IND,
                    :GPO-EXPIRYDATE :GPO-EXPIRYDATE-IND,
                    :GPO-POLICYTYPE :GPO-POLICYTYPE-IND,
                    :GPO-LASTCHANGED,
                    :GPO-BROKERID   :GPO-BROKERID-IND
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF GPO-BROKERID-IND < 0
                    MOVE 0             TO GPO-BROKERID
                 END-IF
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4200-FETCH-POLICY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * Latest term of the policy decides known/unknown
       4300-READ-BILLED-PERIOD.
           MOVE GPO-POLICYNUMBER       TO PPR-GENAPP-POL-NUM
           EXEC SQL
              SELECT PP_ID, ACCOUNT_ID, TERM_NUMBER, PRODUCT_CODE,
                     EFF_DATE, EXP_DATE, TOTAL_PREMIUM, PP_STATUS
                INTO :PPR-PP-ID, :PPR-ACCOUNT-ID, :PPR-TERM-NUMBER,
                     :PPR-PRODUCT-CODE, :PPR-EFF-DATE, :PPR-EXP-DATE,
                     :PPR-TOTAL-PREMIUM, :PPR-PP-STATUS
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE GENAPP_POL_NUM = :PPR-GENAPP-POL-NUM
               ORDER BY TERM_NUMBER DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-POLICY-KNOWN   TO TRUE
              WHEN 100
                 MOVE 'N'              TO WS-KNOWN-FLAG
              WHEN OTHER
                 MOVE '4300-READ-BILLED-PERIOD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-GAP-003 Annual premium from the product detail row
       5000-DERIVE-PREMIUM.
           MOVE 0                      TO WS-PREMIUM
           EVALUATE TRUE
              WHEN WS-PRD-MOTOR
                 PERFORM 5100-MOTOR-PREMIUM
              WHEN WS-PRD-HOUSE
                 PERFORM 5200-HOUSE-PREMIUM
              WHEN WS-PRD-COMMERCIAL
                 PERFORM 5300-COMMERCIAL-PREMIUM
           END-EVALUATE
           IF NOT WS-OUT-REJECT AND WS-PREMIUM NOT > 0
              SET WS-OUT-REJECT        TO TRUE
              MOVE 'NO PREMIUM IN GENAPP' TO WS-RESULT
           END-IF.

       5100-MOTOR-PREMIUM.
           MOVE GPO-POLICYNUMBER       TO GMO-POLICYNUMBER
           EXEC SQL
              SELECT PREMIUM
                INTO :GMO-PREMIUM :GMO-PREMIUM-IND
                FROM GENAPP.MOTOR
               WHERE POLICYNUMBER = :GMO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF GMO-PREMIUM-IND >= 0
                    MOVE GMO-PREMIUM   TO WS-PREMIUM
                 END-IF
              WHEN 100
                 SET WS-OUT-REJECT     TO TRUE
                 MOVE 'NO MOTOR DETAIL IN GENAPP' TO WS-RESULT
              WHEN OTHER
                 MOVE '5100-MOTOR-PREMIUM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * BR-BIL-GAP-003 House: 30 basis points of the property value
       5200-HOUSE-PREMIUM.
           MOVE GPO-POLICYNUMBER       TO GHO-POLICYNUMBER
           EXEC SQL
              SELECT VALUE
                INTO :GHO-VALUE :GHO-VALUE-IND
                FROM GENAPP.HOUSE
               WHERE POLICYNUMBER = :GHO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF GHO-VALUE-IND >= 0 AND GHO-VALUE > 0
                    INITIALIZE WS-MNY-CA
                    MOVE 'MNRATE  '    TO MNA-REQUEST-ID
                    MOVE WS-BATCH-USER TO MNA-USER-ID
                    MOVE WS-BUS-DATE   TO MNA-BUS-DATE
                    MOVE WS-PROGRAM-ID TO MNA-CALLER-PGM
                    MOVE GHO-VALUE     TO MNA-MN-TOTAL
                    MOVE WS-HOUSE-RATE-BPS TO MNA-MN-RATE-BPS
                    CALL WS-MNY-PGM USING WS-MNY-CA
                    IF MNA-RETURN-CODE >= 12
                       DISPLAY 'BILGAPIN BILMONEY RC ' MNA-RETURN-CODE
                               ' ' MNA-MESSAGE
                       PERFORM 9800-ABEND
                    END-IF
                    MOVE MNA-MN-RESULT TO WS-PREMIUM
                 END-IF
              WHEN 100
                 SET WS-OUT-REJECT     TO TRUE
                 MOVE 'NO HOUSE DETAIL IN GENAPP' TO WS-RESULT
              WHEN OTHER
                 MOVE '5200-HOUSE-PREMIUM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * BR-BIL-GAP-003 Commercial: sum of the four peril premiums
       5300-COMMERCIAL-PREMIUM.
           MOVE GPO-POLICYNUMBER       TO GCO-POLICYNUMBER
           EXEC SQL
              SELECT FIREPREMIUM, CRIMEPREMIUM, FLOODPREMIUM,
                     WEATHERPREMIUM, CUSTOMER
                INTO :GCO-FIREPREMIUM    :GCO-FIREPREMIUM-IND,
                     :GCO-CRIMEPREMIUM   :GCO-CRIMEPREMIUM-IND,
                     :GCO-FLOODPREMIUM   :GCO-FLOODPREMIUM-IND,
                     :GCO-WEATHERPREMIUM :GCO-WEATHERPREMIUM-IND,
                     :GCO-CUSTOMER       :GCO-CUSTOMER-IND
                FROM GENAPP.COMMERCIAL
               WHERE POLICYNUMBER = :GCO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE 0                TO WS-PERIL-SUM
                 IF GCO-FIREPREMIUM-IND >= 0
                    ADD GCO-FIREPREMIUM TO WS-PERIL-SUM
                 END-IF
                 IF GCO-CRIMEPREMIUM-IND >= 0
                    ADD GCO-CRIMEPREMIUM TO WS-PERIL-SUM
                 END-IF
                 IF GCO-FLOODPREMIUM-IND >= 0
                    ADD GCO-FLOODPREMIUM TO WS-PERIL-SUM
                 END-IF
                 IF GCO-WEATHERPREMIUM-IND >= 0
                    ADD GCO-WEATHERPREMIUM TO WS-PERIL-SUM
                 END-IF
                 IF GCO-CUSTOMER-IND < 0
                    MOVE SPACES        TO GCO-CUSTOMER
                 END-IF
                 MOVE WS-PERIL-SUM     TO WS-PREMIUM
              WHEN 100
                 SET WS-OUT-REJECT     TO TRUE
                 MOVE 'NO COMMERCIAL DETAIL IN GENAPP' TO WS-RESULT
              WHEN OTHER
                 MOVE '5300-COMMERCIAL-PREMIUM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM GPR-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO GPR-H1-PAGE
           WRITE REPORT-REC FROM GPR-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM GPR-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM GPR-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6300-WRITE-TOTAL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM GPR-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE 0                      TO GPR-T-AMOUNT.

       6900-WRITE-CONTROL-TOTALS.
           MOVE '*** CONTROL TOTALS ***' TO GPR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'GENAPP POLICIES CHANGED' TO GPR-T-LABEL
           MOVE WS-CNT-READ            TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'POLISSUE - NEW PERIODS' TO GPR-T-LABEL
           MOVE WS-CNT-ISSUED          TO GPR-T-COUNT
           MOVE WS-AMT-ISSUED          TO GPR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  BILLING ACCOUNTS CREATED' TO GPR-T-LABEL
           MOVE WS-CNT-ACCT-CREATED    TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'POLCHNG - PREMIUM CHANGES' TO GPR-T-LABEL
           MOVE WS-CNT-CHANGED         TO GPR-T-COUNT
           MOVE WS-AMT-CHANGE          TO GPR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'NO BILLING CHANGE'    TO GPR-T-LABEL
           MOVE WS-CNT-NO-CHANGE       TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'LIFE PRODUCT - NOT BILLED' TO GPR-T-LABEL
           MOVE WS-CNT-LIFE            TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SKIPPED - CANCELLED / EXPIRED' TO GPR-T-LABEL
           MOVE WS-CNT-SKIPPED         TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REJECTED'             TO GPR-T-LABEL
           MOVE WS-CNT-REJECTED        TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  QUEUED FOR RETRY BY BILEVTDR' TO GPR-T-LABEL
           MOVE WS-CNT-QUEUED          TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO GPR-T-LABEL
           MOVE WS-CNT-COMMITS         TO GPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE SPACES                 TO GPR-M-TEXT
           STRING 'HIGH-WATER MARK NOW ' WS-LAST-DONE-TS
                  DELIMITED BY SIZE INTO GPR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE.

      ******************************************************************
      * BR-BIL-GAP-008 The mark moves in the same unit of work as the
      * billing changes it covers
       7000-COMMIT.
           MOVE WS-HW-KEY              TO CTL-CONTROL-KEY
           MOVE WS-LAST-DONE-TS        TO CTL-LAST-EOD-TS
           EXEC SQL
              UPDATE BILDB.BIL_CONTROL
                 SET LAST_EOD_TS = :CTL-LAST-EOD-TS,
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-CNT-COMMITS
           MOVE 0                      TO WS-SINCE-COMMIT
           MOVE 'N'                    TO WS-COMMIT-DUE
           INITIALIZE CHECKPOINT-REC
           MOVE WS-PROGRAM-ID          TO CKP-PROGRAM
           MOVE WS-BUS-DATE            TO CKP-RUN-DATE
           MOVE WS-LAST-DONE-TS        TO CKP-RESTART-KEY
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           COMPUTE CKP-DONE-COUNT = WS-CNT-ISSUED + WS-CNT-CHANGED
           SET CKP-RUNNING             TO TRUE
           IF WS-EOF
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILGAPIN CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-BIL-GAP-005 Billing account per GenApp customer
       8000-FIND-OR-CREATE-ACCOUNT.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTFND '             TO ACA-REQUEST-ID
           PERFORM 8010-PRIME-ACCOUNT-CALL
           MOVE GPO-CUSTOMERNUMBER     TO ACA-AC-GENAPP-CUST-NUM
           CALL WS-ACT-PGM USING WS-ACT-CA
           EVALUATE TRUE
              WHEN ACA-RC-OK OR ACA-RC-WARNING
                 MOVE ACA-AC-ACCOUNT-ID TO WS-ACCOUNT-ID
                 MOVE ACA-AC-ACCOUNT-NUMBER TO WS-ACCOUNT-NUMBER
              WHEN ACA-RC-NOT-FOUND
                 PERFORM 8020-CREATE-ACCOUNT
              WHEN OTHER
                 PERFORM 8900-SERVICE-FAILED
           END-EVALUATE.

       8010-PRIME-ACCOUNT-CALL.
           MOVE WS-BATCH-USER          TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM.

       8020-CREATE-ACCOUNT.
           MOVE GPO-CUSTOMERNUMBER     TO GCU-CUSTOMERNUMBER
           EXEC SQL
              SELECT FIRSTNAME, LASTNAME, HOUSENAME, HOUSENUMBER,
                     POSTCODE, PHONEHOME, PHONEMOBILE, EMAILADDRESS
                INTO :GCU-FIRSTNAME    :GCU-FIRSTNAME-IND,
                     :GCU-LASTNAME     :GCU-LASTNAME-IND,
                     :GCU-HOUSENAME    :GCU-HOUSENAME-IND,
                     :GCU-HOUSENUMBER  :GCU-HOUSENUMBER-IND,
                     :GCU-POSTCODE     :GCU-POSTCODE-IND,
                     :GCU-PHONEHOME    :GCU-PHONEHOME-IND,
                     :GCU-PHONEMOBILE  :GCU-PHONEMOBILE-IND,
                     :GCU-EMAILADDRESS :GCU-EMAILADDRESS-IND
                FROM GENAPP.CUSTOMER
               WHERE CUSTOMERNUMBER = :GCU-CUSTOMERNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-OUT-REJECT     TO TRUE
                 MOVE 'GENAPP CUSTOMER NOT FOUND' TO WS-RESULT
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE '8020-CREATE-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           PERFORM 8030-CLEAR-NULL-CUSTOMER
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTADD '             TO ACA-REQUEST-ID
           PERFORM 8010-PRIME-ACCOUNT-CALL
           MOVE GPO-CUSTOMERNUMBER     TO ACA-AC-GENAPP-CUST-NUM
           MOVE WS-DEFAULT-DLQ-PLAN    TO ACA-AC-DELINQ-PLAN
           MOVE 'MAIL'                 TO ACA-AC-DELIVERY-METHOD
           MOVE 1                      TO ACA-AC-INVOICE-DAY
           MOVE GCU-FIRSTNAME          TO ACA-AC-FIRST-NAME
           MOVE GCU-LASTNAME           TO ACA-AC-LAST-NAME
           IF WS-PRD-COMMERCIAL
              MOVE 'COMM'              TO ACA-AC-ACCOUNT-TYPE
              MOVE 'C'                 TO ACA-AC-CONTACT-TYPE
              IF GCO-CUSTOMER NOT = SPACES
                 MOVE GCO-CUSTOMER     TO ACA-AC-COMPANY-NAME
              ELSE
                 STRING GCU-FIRSTNAME DELIMITED BY '  '
                        ' '           DELIMITED BY SIZE
                        GCU-LASTNAME  DELIMITED BY '  '
                        INTO ACA-AC-COMPANY-NAME
              END-IF
           ELSE
              MOVE 'PERS'              TO ACA-AC-ACCOUNT-TYPE
              MOVE 'P'                 TO ACA-AC-CONTACT-TYPE
           END-IF
           STRING GCU-HOUSENUMBER DELIMITED BY SPACE
                  ' '             DELIMITED BY SIZE
                  GCU-HOUSENAME   DELIMITED BY '  '
                  INTO ACA-AC-ADDR-LINE1
           MOVE GCU-POSTCODE           TO ACA-AC-POSTCODE
           MOVE GCU-EMAILADDRESS       TO ACA-AC-EMAIL
           IF GCU-PHONEHOME NOT = SPACES
              MOVE GCU-PHONEHOME       TO ACA-AC-PHONE
           ELSE
              MOVE GCU-PHONEMOBILE     TO ACA-AC-PHONE
           END-IF
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF ACA-RC-OK OR ACA-RC-WARNING
              ADD 1                    TO WS-CNT-ACCT-CREATED
              MOVE ACA-AC-ACCOUNT-ID   TO WS-ACCOUNT-ID
              MOVE ACA-AC-ACCOUNT-NUMBER TO WS-ACCOUNT-NUMBER
           ELSE
              PERFORM 8900-SERVICE-FAILED
           END-IF.

       8030-CLEAR-NULL-CUSTOMER.
           IF GCU-FIRSTNAME-IND < 0
              MOVE SPACES              TO GCU-FIRSTNAME
           END-IF
           IF GCU-LASTNAME-IND < 0
              MOVE SPACES              TO GCU-LASTNAME
           END-IF
           IF GCU-HOUSENAME-IND < 0
              MOVE SPACES              TO GCU-HOUSENAME
           END-IF
           IF GCU-HOUSENUMBER-IND < 0
              MOVE SPACES              TO GCU-HOUSENUMBER
           END-IF
           IF GCU-POSTCODE-IND < 0
              MOVE SPACES              TO GCU-POSTCODE
           END-IF
           IF GCU-PHONEHOME-IND < 0
              MOVE SPACES              TO GCU-PHONEHOME
           END-IF
           IF GCU-PHONEMOBILE-IND < 0
              MOVE SPACES              TO GCU-PHONEMOBILE
           END-IF
           IF GCU-EMAILADDRESS-IND < 0
              MOVE SPACES              TO GCU-EMAILADDRESS
           END-IF.

      * BR-BIL-GAP-006 Broker id -> producer; agency bill -> AB.
      * An unknown broker bills direct with no producer (warning).
       8100-FIND-PRODUCER.
           MOVE 'DB'                   TO WS-BILL-METHOD
           MOVE GPO-BROKERID           TO WS-BROKER-ID
           IF WS-BROKER-ID = 0
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-PRD-CA
           MOVE 'PRDFND  '             TO PRA-REQUEST-ID
           MOVE WS-BATCH-USER          TO PRA-USER-ID
           MOVE WS-BUS-DATE            TO PRA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO PRA-CALLER-PGM
           MOVE WS-BROKER-ID           TO PRA-PR-PRODUCER-CODE
           CALL WS-PRD-PGM USING WS-PRD-CA
           EVALUATE TRUE
              WHEN PRA-RC-OK OR PRA-RC-WARNING
                 IF PRA-PR-AGENCY-BILL = 'Y'
                    MOVE 'AB'          TO WS-BILL-METHOD
                 END-IF
              WHEN PRA-RC-NOT-FOUND
                 MOVE 0                TO WS-BROKER-ID
                 MOVE GPO-BROKERID     TO WS-BROKER-DISP
                 MOVE SPACES           TO GPR-M-TEXT
                 STRING '            POLICY ' WS-ERR-KEY
                        ' BROKER ' WS-BROKER-DISP
                        ' NOT A BILLING PRODUCER - BILLED DIRECT'
                        DELIMITED BY SIZE INTO GPR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
              WHEN PRA-RETURN-CODE >= 90
                 DISPLAY 'BILGAPIN BILPROD0 RC ' PRA-RETURN-CODE ' '
                         PRA-MESSAGE
                 PERFORM 9800-ABEND
              WHEN OTHER
                 MOVE 0                TO WS-BROKER-ID
           END-EVALUATE.

       8300-PRIME-PERIOD-CALL.
           MOVE WS-BATCH-USER          TO PPA-USER-ID
           MOVE WS-BUS-DATE            TO PPA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO PPA-CALLER-PGM
           MOVE GPO-POLICYNUMBER       TO PPA-PP-GENAPP-POL-NUM
           STRING 'GENAPP ' GPO-LASTCHANGED(1:10)
                  DELIMITED BY SIZE INTO PPA-PP-SOURCE-REF.

       8400-CALL-PERIOD.
           CALL WS-PPR-PGM USING WS-PPR-CA
           IF PPA-RC-OK OR PPA-RC-WARNING
              CONTINUE
           ELSE
              MOVE PPA-REASON-CODE     TO WS-FAIL-REASON
              MOVE PPA-MESSAGE         TO WS-FAIL-TEXT
              IF PPA-RETURN-CODE >= 90
                 DISPLAY 'BILGAPIN BILPPER0 RC ' PPA-RETURN-CODE ' '
                         PPA-MESSAGE
                 PERFORM 9800-ABEND
              END-IF
              SET WS-OUT-REJECT        TO TRUE
              STRING 'BILPPER0 ' PPA-REQUEST-ID ' ' PPA-REASON-CODE
                     DELIMITED BY SIZE INTO WS-RESULT
           END-IF.

       8500-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              DISPLAY 'BILGAPIN BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      * BR-BIL-GAP-009 Rejected policy goes on the inbound queue so
      * BILEVTDR retries it once the cause is fixed
       8600-QUEUE-FOR-RETRY.
           INITIALIZE WS-EVT-CA
           MOVE 'EVPUT   '             TO EVA-REQUEST-ID
           MOVE WS-BATCH-USER          TO EVA-USER-ID
           MOVE WS-BUS-DATE            TO EVA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO EVA-CALLER-PGM
           MOVE 'I'                    TO EVA-EV-DIRECTION
           MOVE 'GENA'                 TO EVA-EV-SOURCE
           MOVE 'BILL'                 TO EVA-EV-TARGET
           MOVE WS-ERR-KEY             TO EVA-EV-KEY
           IF WS-POLICY-KNOWN
              INITIALIZE EVP-POLCHNG
              MOVE 'POLCHNG '          TO EVA-EV-TYPE
              MOVE GPO-POLICYNUMBER    TO EVP-PC-POLICY-NO
              MOVE WS-PREMIUM          TO EVP-PC-PREMIUM
              MOVE WS-CHANGE-DATE      TO EVP-PC-EFF-DATE
              MOVE EVP-POLCHNG         TO EVA-EV-PAYLOAD
           ELSE
              INITIALIZE EVP-POLISSUE
              MOVE 'POLISSUE'          TO EVA-EV-TYPE
              MOVE GPO-POLICYNUMBER    TO EVP-PI-POLICY-NO
              MOVE GPO-CUSTOMERNUMBER  TO EVP-PI-CUSTOMER-NO
              MOVE WS-PRODUCT          TO EVP-PI-PRODUCT
              MOVE WS-EFF-DATE         TO EVP-PI-EFF-DATE
              MOVE WS-EXP-DATE         TO EVP-PI-EXP-DATE
              MOVE WS-PREMIUM          TO EVP-PI-PREMIUM
              MOVE GPO-BROKERID        TO EVP-PI-BROKER-ID
              MOVE WS-PLAN-CODE        TO EVP-PI-PLAN-CODE
              MOVE WS-BILL-METHOD      TO EVP-PI-BILL-METHOD
              MOVE EVP-POLISSUE        TO EVA-EV-PAYLOAD
           END-IF
      * Policies rejected before the dates or premium were known
      * cannot be expressed as an event; they stay on the report.
           IF WS-PREMIUM NOT > 0
              OR (NOT WS-POLICY-KNOWN AND WS-EFF-DATE = SPACES)
              EXIT PARAGRAPH
           END-IF
           CALL WS-EVT-PGM USING WS-EVT-CA
           IF EVA-RC-OK OR EVA-RC-WARNING
              ADD 1                    TO WS-CNT-QUEUED
           ELSE
              IF EVA-RETURN-CODE >= 90
                 DISPLAY 'BILGAPIN BILEVTQ RC ' EVA-RETURN-CODE ' '
                         EVA-MESSAGE
                 PERFORM 9800-ABEND
              END-IF
              MOVE SPACES              TO GPR-M-TEXT
              STRING '            NOT QUEUED: ' EVA-REASON-CODE ' '
                     EVA-MESSAGE DELIMITED BY SIZE INTO GPR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
           END-IF.

      * Account service rejected: RC >= 90 stops the run, any other
      * failure rejects just this policy.
       8900-SERVICE-FAILED.
           IF ACA-RETURN-CODE >= 90
              DISPLAY 'BILGAPIN BILACCT0 RC ' ACA-RETURN-CODE ' '
                      ACA-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           SET WS-OUT-REJECT           TO TRUE
           MOVE ACA-REASON-CODE        TO WS-FAIL-REASON
           MOVE ACA-MESSAGE            TO WS-FAIL-TEXT
           STRING 'BILACCT0 ' ACA-REQUEST-ID ' ' ACA-REASON-CODE
                  DELIMITED BY SIZE INTO WS-RESULT.

      ******************************************************************
       9000-TERMINATE.
           PERFORM 7000-COMMIT
           IF WS-PAGE-COUNT = 0
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILGAPIN COMPLETE READ=' WS-CNT-READ
                   ' ISSUED=' WS-CNT-ISSUED ' CHANGED=' WS-CNT-CHANGED
                   ' REJ=' WS-CNT-REJECTED ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      * The mark only moves on commit, so a rollback leaves it at the
      * last committed change and the rerun picks up from there.
       9800-ABEND.
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE GAP-CSR END-EXEC
              MOVE 'N'                 TO WS-CURSOR-FLAG
           END-IF
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO GPR-M-TEXT
           STRING '*** BILGAPIN ABENDED - WORK SINCE LAST COMMIT '
                  'ROLLED BACK. LAST POLICY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO GPR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILGAPIN DB2 ERROR ' WS-SQLCODE-DISP ' IN '
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
