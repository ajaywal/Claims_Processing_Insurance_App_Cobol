       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILINVDU.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILINVDU                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Due transition for the business date.            *
      *               Phase 1 moves billed items and invoices that     *
      *               have reached their due date to DUE (or PAID when *
      *               already settled), posts the amount falling due   *
      *               and refreshes account balances.                  *
      *               Phase 2 starts delinquency (BILDLQSV DLQSTRT)    *
      *               for policy periods still past due once the       *
      *               delinquency plan's grace days have run out.      *
      * INPUT       : SYSIN   control card            (BILINVC1)       *
      *               CHKPT   checkpoint / restart     (BILINVK1)      *
      * OUTPUT      : CHKPT, RPTOUT run report (BILINVH1, BILINVR2)    *
      * CALLS       : BILDATES BILGLPST BILACCT0 BILDLQSV BILERRLG     *
      * TABLES      : BIL_INVOICE_ITEM      (R U)                      *
      *               BIL_INVOICE           (R U)                      *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_DELINQ_PLAN       (R)                        *
      *               BIL_DELINQ_PROCESS    (R)                        *
      *               BIL_CONTROL           (R)                        *
      * RESTART     : key 1 = last committed ACCOUNT_ID, key 2 =       *
      *               phase (1 due transition, 2 delinquency). Phase 2 *
      *               is idempotent and is always run in full.         *
      * RULES       : BR-BIL-IDU-001 BILD items due on or before the   *
      *                              business date with an open amount *
      *                              become DUE                        *
      *               BR-BIL-IDU-002 Items and invoices already fully  *
      *                              settled at due date close PAID    *
      *               BR-BIL-IDU-003 An invoice becomes DUE when its   *
      *                              due date arrives with an open     *
      *                              amount                            *
      *               BR-BIL-IDU-004 Amount falling due posts DR DUE / *
      *                              CR BILLED                         *
      *               BR-BIL-IDU-005 Account balances are refreshed    *
      *                              after the transition (ACCTBAL)    *
      *               BR-BIL-IDU-006 Delinquency is considered only    *
      *                              for items due before business     *
      *                              date minus the plan GRACE_DAYS    *
      *               BR-BIL-IDU-007 Periods with an OPEN process are  *
      *                              not re-evaluated                  *
      *               BR-BIL-IDU-008 Each account / period is all-or-  *
      *                              nothing (savepoint); commits on   *
      *                              boundaries every COMMIT-FREQ      *
      * RETURN CODE : 0 ok, 4 warnings, 8 rejects, 16 abended          *
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
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILINVDU'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'BILBATCH'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-DLQ-PGM                  PIC X(8)  VALUE 'BILDLQSV'.
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
           05 WS-RESTART-PHASE         PIC S9(4) COMP VALUE 1.
           05 WS-PHASE                 PIC S9(4) COMP VALUE 1.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LAST-KEY              PIC S9(9) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-RUN-FLAG              PIC X     VALUE 'N'.
              88 WS-RUN-ENDED                    VALUE 'Y'.
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-END-OF-CURSOR                VALUE 'Y'.
           05 WS-UNIT-FLAG             PIC X     VALUE 'N'.
              88 WS-UNIT-REJECTED                VALUE 'R'.
              88 WS-UNIT-OK                      VALUE 'N'.
           05 WS-OPEN-CURSOR           PIC X(8)  VALUE SPACES.
           05 WS-ABEND-TEXT            PIC X(100) VALUE SPACES.
           05 WS-REJECT-TEXT           PIC X(80) VALUE SPACES.

       01  WS-ACCOUNT-UNIT.
           05 WS-CUR-ACCOUNT-ID        PIC S9(9) COMP VALUE 0.
           05 WS-A-ITEMS-DUE           PIC S9(9) COMP VALUE 0.
           05 WS-A-ITEMS-PAID          PIC S9(9) COMP VALUE 0.
           05 WS-A-AMOUNT-DUE          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-A-INV-DUE             PIC S9(9) COMP VALUE 0.
           05 WS-A-INV-PAID            PIC S9(9) COMP VALUE 0.
           05 WS-INV-OPEN              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-UPDATED               PIC S9(9) COMP VALUE 0.

      * Active delinquency plans with their grace cut-off date
       01  WS-PLAN-TABLE.
           05 WS-PLAN-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-PLAN OCCURS 50 TIMES INDEXED BY PLX.
              10 WS-PL-CODE            PIC X(8).
              10 WS-PL-GRACE           PIC S9(4) COMP.
              10 WS-PL-CUTOFF          PIC X(10).

       01  WS-CANDIDATE.
           05 WS-CAN-ACCOUNT-ID        PIC S9(9) COMP.
           05 WS-CAN-ACCOUNT-NUMBER    PIC X(10).
           05 WS-CAN-PP-ID             PIC S9(9) COMP.
           05 WS-CAN-PASTDUE           PIC S9(11)V99 COMP-3.
           05 WS-PLAN-CODE             PIC X(8).
           05 WS-GRACE-CUTOFF          PIC X(10).

       01  WS-TOTALS.
           05 WS-T-ACCOUNTS            PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ACCTS-REJECTED      PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ITEMS-DUE           PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ITEMS-PAID          PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-INV-DUE             PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-INV-PAID            PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-AMOUNT-DUE          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-T-CANDIDATES          PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-DLQ-STARTED         PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-DLQ-STARTED-AMT     PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-T-DLQ-BELOW           PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-DLQ-HOLD            PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-DLQ-EXISTS          PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-DLQ-REJECTED        PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-COMMITS             PIC S9(9) COMP-3 VALUE 0.

       01  WS-WORK.
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
           EXEC SQL INCLUDE BILDDPL END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * Accounts with billed items or invoices that have fallen due
           EXEC SQL
              DECLARE C-DUE-ACCOUNTS CURSOR WITH HOLD FOR
               SELECT ACCOUNT_ID
                 FROM BILDB.BIL_INVOICE_ITEM
                WHERE ITEM_STATUS = 'BILD'
                  AND DUE_DATE   <= :WS-BUS-DATE
                  AND ACCOUNT_ID  > :WS-RESTART-ACCT
               UNION
               SELECT ACCOUNT_ID
                 FROM BILDB.BIL_INVOICE
                WHERE INVOICE_STATUS = 'BILD'
                  AND DUE_DATE      <= :WS-BUS-DATE
                  AND ACCOUNT_ID     > :WS-RESTART-ACCT
                ORDER BY 1
           END-EXEC.

      * Billed invoices of one account that have reached due date
           EXEC SQL
              DECLARE C-ACCT-INVOICES CURSOR FOR
               SELECT V.INVOICE_ID,
                      COALESCE((SELECT SUM(I.ITEM_AMOUNT
                                         - I.PAID_AMOUNT
                                         - I.WRITEOFF_AMOUNT)
                                  FROM BILDB.BIL_INVOICE_ITEM I
                                 WHERE I.INVOICE_ID = V.INVOICE_ID),
                               0)
                 FROM BILDB.BIL_INVOICE V
                WHERE V.ACCOUNT_ID     = :WS-CUR-ACCOUNT-ID
                  AND V.INVOICE_STATUS = 'BILD'
                  AND V.DUE_DATE      <= :WS-BUS-DATE
                ORDER BY V.INVOICE_ID
                FOR FETCH ONLY
           END-EXEC.

           EXEC SQL
              DECLARE C-PLANS CURSOR FOR
               SELECT PLAN_CODE, GRACE_DAYS
                 FROM BILDB.BIL_DELINQ_PLAN
                WHERE ACTIVE_FLAG = 'Y'
                ORDER BY PLAN_CODE
                FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-IDU-006 / 007 Periods past due beyond the plan grace
           EXEC SQL
              DECLARE C-GRACE-EXPIRED CURSOR WITH HOLD FOR
               SELECT I.ACCOUNT_ID, A.ACCOUNT_NUMBER, I.PP_ID,
                      SUM(I.ITEM_AMOUNT - I.PAID_AMOUNT
                          - I.WRITEOFF_AMOUNT)
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_ACCOUNT A
                   ON A.ACCOUNT_ID = I.ACCOUNT_ID
                WHERE A.DELINQ_PLAN_CODE = :WS-PLAN-CODE
                  AND A.ACCOUNT_STATUS   = 'ACTV'
                  AND I.ITEM_STATUS      = 'DUE '
                  AND I.DUE_DATE         < :WS-GRACE-CUTOFF
                  AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                      - I.WRITEOFF_AMOUNT > 0
                  AND NOT EXISTS
                      (SELECT 1
                         FROM BILDB.BIL_DELINQ_PROCESS D
                        WHERE D.PP_ID     = I.PP_ID
                          AND D.DP_STATUS = 'OPEN')
                GROUP BY I.ACCOUNT_ID, A.ACCOUNT_NUMBER, I.PP_ID
                ORDER BY I.ACCOUNT_ID, I.PP_ID
                FOR FETCH ONLY
           END-EXEC.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTC-==.
           03 DTC-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTC-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLC-==.
           03 GLC-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLC-==.
       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACC-==.
           03 ACC-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACC-==.
       01  WS-DLQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DQC-==.
           03 DQC-BODY.
              COPY BILCDLQ REPLACING LEADING ==CA-== BY ==DQC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILINVH1.
           COPY BILINVR2.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF NOT WS-RUN-ENDED
              IF WS-RESTART-PHASE = 1
                 PERFORM 2000-DUE-TRANSITION
              END-IF
              PERFORM 3000-DELINQUENCY-PHASE
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN INPUT  CONTROL-CARD
                OUTPUT REPORT-FILE
           IF WS-CC-STATUS NOT = '00' OR WS-RP-STATUS NOT = '00'
              DISPLAY 'BILINVDU OPEN FAILED SYSIN=' WS-CC-STATUS
                      ' RPTOUT=' WS-RP-STATUS
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE WS-PROGRAM-ID          TO RH-PROGRAM-ID
           MOVE 'DUE TRANSITION AND DELINQUENCY START' TO RH-TITLE
           PERFORM 1100-READ-CONTROL-CARD
           PERFORM 1200-RESOLVE-BUS-DATE
           MOVE WS-BUS-DATE            TO RH-BUS-DATE
           MOVE WS-RUN-DATE            TO RH-RUN-DATE
           PERFORM 1300-READ-CHECKPOINT.

      ******************************************************************
       1100-READ-CONTROL-CARD.
           MOVE SPACES                 TO CC-RECORD
           READ CONTROL-CARD
              AT END
                 MOVE SPACES           TO CC-RECORD
           END-READ
           CLOSE CONTROL-CARD
           IF CC-COMMIT-FREQ NOT = SPACES
              IF CC-COMMIT-FREQ IS NUMERIC AND CC-COMMIT-FREQ-N > 0
                 MOVE CC-COMMIT-FREQ-N TO WS-COMMIT-FREQ
              ELSE
                 STRING 'INVALID COMMIT FREQUENCY ' CC-COMMIT-FREQ
                        DELIMITED BY SIZE INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
           END-IF.

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
       1300-READ-CHECKPOINT.
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
                    MOVE CK-LAST-KEY-2 TO WS-RESTART-PHASE
                    MOVE CK-COMMITS    TO WS-T-COMMITS
                    MOVE CK-LAST-KEY-1 TO WS-ID-DISP
                    MOVE CK-LAST-KEY-2 TO WS-CNT-DISP
                    STRING 'RESTART IN PHASE ' WS-CNT-DISP
                           ' AFTER ACCOUNT ID ' WS-ID-DISP
                           DELIMITED BY SIZE INTO RH-TEXT
                    PERFORM 7200-WRITE-TEXT
                 WHEN CK-COMPLETE AND NOT CC-RESTART-FORCE
                    MOVE 'BUSINESS DATE ALREADY COMPLETE - NOTHING DONE'
                                       TO RH-TEXT
                    PERFORM 7200-WRITE-TEXT
                    MOVE 4             TO WS-MAX-RC
                    SET WS-RUN-ENDED   TO TRUE
              END-EVALUATE
           END-IF
           IF WS-RESTART-PHASE NOT = 2
              MOVE 1                   TO WS-RESTART-PHASE
           END-IF.

      ******************************************************************
      * Phase 1 - billed items and invoices that have fallen due
       2000-DUE-TRANSITION.
           MOVE 1                      TO WS-PHASE
           MOVE 99                     TO WS-LINES
           MOVE 'N'                    TO WS-EOF-FLAG
           EXEC SQL OPEN C-DUE-ACCOUNTS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-DUE-TRANSITION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'DUEACCTS'             TO WS-OPEN-CURSOR
           PERFORM 4000-FETCH-DUE-ACCOUNT
           PERFORM UNTIL WS-END-OF-CURSOR
              PERFORM 2100-PROCESS-ACCOUNT
              PERFORM 4000-FETCH-DUE-ACCOUNT
           END-PERFORM
           EXEC SQL CLOSE C-DUE-ACCOUNTS END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR
           MOVE 0                      TO WS-LAST-KEY
           MOVE 2                      TO WS-PHASE
           PERFORM 7100-COMMIT-CHECKPOINT.

      ******************************************************************
      * BR-BIL-IDU-008 Account is all-or-nothing
       2100-PROCESS-ACCOUNT.
           MOVE 0                      TO WS-A-ITEMS-DUE
                                          WS-A-ITEMS-PAID
                                          WS-A-AMOUNT-DUE
                                          WS-A-INV-DUE
                                          WS-A-INV-PAID
           SET WS-UNIT-OK              TO TRUE
           EXEC SQL
              SAVEPOINT DUEUNIT ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-PROCESS-ACCOUNT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-READ-ACCOUNT
           PERFORM 4200-SUM-ITEMS-FALLING-DUE
           PERFORM 4210-MOVE-ITEMS-TO-DUE
           IF WS-UNIT-OK
              PERFORM 4220-CLOSE-SETTLED-ITEMS
              PERFORM 2110-TRANSITION-INVOICES
           END-IF
           IF WS-UNIT-OK AND WS-A-AMOUNT-DUE NOT = 0
              PERFORM 8200-POST-AMOUNT-DUE
           END-IF
           IF WS-UNIT-OK
              PERFORM 8400-REFRESH-BALANCES
           END-IF
           IF WS-UNIT-OK
              ADD 1                    TO WS-T-ACCOUNTS
              ADD WS-A-ITEMS-DUE       TO WS-T-ITEMS-DUE
              ADD WS-A-ITEMS-PAID      TO WS-T-ITEMS-PAID
              ADD WS-A-AMOUNT-DUE      TO WS-T-AMOUNT-DUE
              ADD WS-A-INV-DUE         TO WS-T-INV-DUE
              ADD WS-A-INV-PAID        TO WS-T-INV-PAID
              MOVE 'DUE TRANSITION DONE' TO R2-REMARK
           ELSE
              EXEC SQL ROLLBACK TO SAVEPOINT DUEUNIT END-EXEC
              ADD 1                    TO WS-T-ACCTS-REJECTED
              MOVE WS-REJECT-TEXT      TO R2-REMARK
           END-IF
           EXEC SQL RELEASE SAVEPOINT DUEUNIT END-EXEC
           PERFORM 7300-WRITE-DUE-LINE
           MOVE WS-CUR-ACCOUNT-ID      TO WS-LAST-KEY
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 7100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
      * BR-BIL-IDU-002 / 003 DUE with an open amount, else PAID
       2110-TRANSITION-INVOICES.
           EXEC SQL OPEN C-ACCT-INVOICES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2110-TRANSITION-INVOICES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL SQLCODE = 100
              EXEC SQL
                 FETCH C-ACCT-INVOICES
                  INTO :INV-INVOICE-ID, :WS-INV-OPEN
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF WS-INV-OPEN > 0
                       MOVE 'DUE '     TO INV-INVOICE-STATUS
                       ADD 1           TO WS-A-INV-DUE
                    ELSE
                       MOVE 'PAID'     TO INV-INVOICE-STATUS
                       ADD 1           TO WS-A-INV-PAID
                    END-IF
                    PERFORM 4300-SET-INVOICE-STATUS
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '2110-TRANSITION-INVOICES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE C-ACCT-INVOICES END-EXEC.

      ******************************************************************
      * Phase 2 - start delinquency where the grace period has expired
       3000-DELINQUENCY-PHASE.
           MOVE 2                      TO WS-PHASE
           MOVE 99                     TO WS-LINES
           PERFORM 3100-LOAD-PLANS
           PERFORM VARYING PLX FROM 1 BY 1
                     UNTIL PLX > WS-PLAN-COUNT
              PERFORM 3200-PROCESS-PLAN
           END-PERFORM.

      ******************************************************************
      * BR-BIL-IDU-006 Grace cut-off = business date - GRACE_DAYS
       3100-LOAD-PLANS.
           MOVE 0                      TO WS-PLAN-COUNT
           EXEC SQL OPEN C-PLANS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3100-LOAD-PLANS'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL SQLCODE = 100
              EXEC SQL
                 FETCH C-PLANS
                  INTO :DPL-PLAN-CODE, :DPL-GRACE-DAYS
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF WS-PLAN-COUNT >= 50
                       MOVE 'MORE THAN 50 DELINQUENCY PLANS'
                                       TO WS-ABEND-TEXT
                       PERFORM 9800-ABEND
                    END-IF
                    ADD 1              TO WS-PLAN-COUNT
                    SET PLX            TO WS-PLAN-COUNT
                    MOVE DPL-PLAN-CODE TO WS-PL-CODE(PLX)
                    MOVE DPL-GRACE-DAYS TO WS-PL-GRACE(PLX)
                    INITIALIZE WS-DTE-CA
                    MOVE 'DTADDD  '    TO DTC-REQUEST-ID
                    MOVE WS-BUS-DATE   TO DTC-DT-DATE-1
                    COMPUTE DTC-DT-NUMBER = 0 - DPL-GRACE-DAYS
                    PERFORM 8300-CALL-DATES
                    MOVE DTC-DT-RESULT-DATE TO WS-PL-CUTOFF(PLX)
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '3100-LOAD-PLANS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE C-PLANS END-EXEC.

      ******************************************************************
       3200-PROCESS-PLAN.
           MOVE WS-PL-CODE(PLX)        TO WS-PLAN-CODE
           MOVE WS-PL-CUTOFF(PLX)      TO WS-GRACE-CUTOFF
           MOVE 'N'                    TO WS-EOF-FLAG
           EXEC SQL OPEN C-GRACE-EXPIRED END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3200-PROCESS-PLAN' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'GRACEEXP'             TO WS-OPEN-CURSOR
           PERFORM 4400-FETCH-CANDIDATE
           PERFORM UNTIL WS-END-OF-CURSOR
              PERFORM 3300-START-DELINQUENCY
              PERFORM 4400-FETCH-CANDIDATE
           END-PERFORM
           EXEC SQL CLOSE C-GRACE-EXPIRED END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR.

      ******************************************************************
       3300-START-DELINQUENCY.
           ADD 1                       TO WS-T-CANDIDATES
           EXEC SQL
              SAVEPOINT DLQUNIT ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3300-START-DELINQUENCY' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           INITIALIZE WS-DLQ-CA
           MOVE 'DLQSTRT '             TO DQC-REQUEST-ID
           MOVE WS-BATCH-USER          TO DQC-USER-ID
           MOVE WS-BUS-DATE            TO DQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DQC-CALLER-PGM
           MOVE WS-CAN-ACCOUNT-ID      TO DQC-DQ-ACCOUNT-ID
           MOVE WS-CAN-PP-ID           TO DQC-DQ-PP-ID
           MOVE WS-PLAN-CODE           TO DQC-DQ-PLAN-CODE
           CALL WS-DLQ-PGM USING WS-DLQ-CA
           MOVE SPACES                 TO R2-D-RESULT
           EVALUATE TRUE
              WHEN DQC-RC-OK
                 ADD 1                 TO WS-T-DLQ-STARTED
                 ADD DQC-DQ-PASTDUE-AMOUNT TO WS-T-DLQ-STARTED-AMT
                 MOVE DQC-DQ-PASTDUE-AMOUNT TO WS-CAN-PASTDUE
                 MOVE DQC-DQ-DP-ID     TO WS-ID-DISP
                 STRING 'STARTED - PROCESS ' WS-ID-DISP
                        DELIMITED BY SIZE INTO R2-D-RESULT
              WHEN DQC-RC-WARNING AND DQC-REASON-CODE = 'DLNQHOLD'
                 ADD 1                 TO WS-T-DLQ-HOLD
                 MOVE 'SUPPRESSED - DLNQ HOLD' TO R2-D-RESULT
              WHEN DQC-RC-WARNING
                 ADD 1                 TO WS-T-DLQ-BELOW
                 MOVE 'NOT STARTED - PAST DUE WITHIN THRESHOLD'
                                       TO R2-D-RESULT
              WHEN DQC-RC-CONFLICT AND DQC-REASON-CODE = 'DPEXISTS'
                 ADD 1                 TO WS-T-DLQ-EXISTS
                 MOVE 'PROCESS ALREADY OPEN' TO R2-D-RESULT
              WHEN DQC-RETURN-CODE >= 90
                 STRING 'BILDLQSV ' DQC-REASON-CODE ' '
                        DQC-MESSAGE DELIMITED BY SIZE
                        INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              WHEN OTHER
                 EXEC SQL ROLLBACK TO SAVEPOINT DLQUNIT END-EXEC
                 ADD 1                 TO WS-T-DLQ-REJECTED
                 STRING 'REJECTED ' DQC-REASON-CODE ' '
                        DQC-MESSAGE DELIMITED BY SIZE
                        INTO R2-D-RESULT
           END-EVALUATE
           EXEC SQL RELEASE SAVEPOINT DLQUNIT END-EXEC
           PERFORM 7400-WRITE-DLQ-LINE
           MOVE WS-CAN-ACCOUNT-ID      TO WS-LAST-KEY
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 7100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
       4000-FETCH-DUE-ACCOUNT.
           EXEC SQL
              FETCH C-DUE-ACCOUNTS INTO :WS-CUR-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-END-OF-CURSOR  TO TRUE
              WHEN OTHER
                 MOVE '4000-FETCH-DUE-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-READ-ACCOUNT.
           MOVE WS-CUR-ACCOUNT-ID      TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_NUMBER
                INTO :ACT-ACCOUNT-NUMBER
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
       4200-SUM-ITEMS-FALLING-DUE.
           EXEC SQL
              SELECT COUNT(*),
                     COALESCE(SUM(ITEM_AMOUNT - PAID_AMOUNT
                                  - WRITEOFF_AMOUNT), 0)
                INTO :WS-A-ITEMS-DUE, :WS-A-AMOUNT-DUE
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE ACCOUNT_ID  = :WS-CUR-ACCOUNT-ID
                 AND ITEM_STATUS = 'BILD'
                 AND DUE_DATE   <= :WS-BUS-DATE
                 AND ITEM_AMOUNT - PAID_AMOUNT - WRITEOFF_AMOUNT > 0
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ACT-ACCOUNT-NUMBER  TO WS-ERR-KEY
              MOVE '4200-SUM-ITEMS-FALLING-DUE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-IDU-001 BILD items due with an open amount become DUE
       4210-MOVE-ITEMS-TO-DUE.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET ITEM_STATUS = 'DUE ',
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID  = :WS-CUR-ACCOUNT-ID
                 AND ITEM_STATUS = 'BILD'
                 AND DUE_DATE   <= :WS-BUS-DATE
                 AND ITEM_AMOUNT - PAID_AMOUNT - WRITEOFF_AMOUNT > 0
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE ACT-ACCOUNT-NUMBER  TO WS-ERR-KEY
              MOVE '4210-MOVE-ITEMS-TO-DUE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE SQLERRD(3)             TO WS-UPDATED
           IF SQLCODE = 100
              MOVE 0                   TO WS-UPDATED
           END-IF
           IF WS-UPDATED NOT = WS-A-ITEMS-DUE
              SET WS-UNIT-REJECTED     TO TRUE
              MOVE 'REJECTED ITEMCHNG - items changed during run'
                                       TO WS-REJECT-TEXT
           END-IF.

      ******************************************************************
      * BR-BIL-IDU-002 Settled items reaching due date close PAID
       4220-CLOSE-SETTLED-ITEMS.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET ITEM_STATUS = 'PAID',
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID  = :WS-CUR-ACCOUNT-ID
                 AND ITEM_STATUS = 'BILD'
                 AND DUE_DATE   <= :WS-BUS-DATE
                 AND ITEM_AMOUNT - PAID_AMOUNT - WRITEOFF_AMOUNT <= 0
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE SQLERRD(3)       TO WS-A-ITEMS-PAID
              WHEN 100
                 MOVE 0                TO WS-A-ITEMS-PAID
              WHEN OTHER
                 MOVE ACT-ACCOUNT-NUMBER TO WS-ERR-KEY
                 MOVE '4220-CLOSE-SETTLED-ITEMS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-SET-INVOICE-STATUS.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE
                 SET INVOICE_STATUS = :INV-INVOICE-STATUS,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE INVOICE_ID     = :INV-INVOICE-ID
                 AND INVOICE_STATUS = 'BILD'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE INV-INVOICE-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4300-SET-INVOICE-STATUS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-FETCH-CANDIDATE.
           EXEC SQL
              FETCH C-GRACE-EXPIRED
               INTO :WS-CAN-ACCOUNT-ID, :WS-CAN-ACCOUNT-NUMBER,
                    :WS-CAN-PP-ID, :WS-CAN-PASTDUE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-END-OF-CURSOR  TO TRUE
              WHEN OTHER
                 MOVE WS-PLAN-CODE     TO WS-ERR-KEY
                 MOVE '4400-FETCH-CANDIDATE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       7000-NEW-PAGE.
           ADD 1                       TO WS-PAGE
           MOVE WS-PAGE                TO RH-PAGE
           WRITE RPT-RECORD FROM RH-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           IF WS-PHASE = 1
              WRITE RPT-RECORD FROM R2-DUE-HEAD
                    AFTER ADVANCING 2 LINES
           ELSE
              WRITE RPT-RECORD FROM R2-DLQ-HEAD
                    AFTER ADVANCING 2 LINES
           END-IF
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           MOVE 5                      TO WS-LINES.

      ******************************************************************
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
           MOVE WS-LAST-KEY            TO CK-LAST-KEY-1
           MOVE WS-PHASE               TO CK-LAST-KEY-2
           COMPUTE CK-UNITS-DONE = WS-T-ACCOUNTS + WS-T-CANDIDATES
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
       7300-WRITE-DUE-LINE.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           MOVE ACT-ACCOUNT-NUMBER     TO R2-ACCOUNT-NUMBER
           MOVE WS-A-ITEMS-DUE         TO R2-ITEMS-DUE
           MOVE WS-A-AMOUNT-DUE        TO R2-AMOUNT-DUE
           MOVE WS-A-INV-DUE           TO R2-INV-DUE
           MOVE WS-A-INV-PAID          TO R2-INV-PAID
           MOVE WS-A-ITEMS-PAID        TO R2-ITEMS-PAID
           WRITE RPT-RECORD FROM R2-DUE-LINE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES.

      ******************************************************************
       7400-WRITE-DLQ-LINE.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           MOVE WS-CAN-ACCOUNT-NUMBER  TO R2-D-ACCOUNT-NUMBER
           MOVE WS-PLAN-CODE           TO R2-D-PLAN-CODE
           MOVE WS-CAN-PP-ID           TO R2-D-PP-ID
           MOVE WS-GRACE-CUTOFF        TO R2-D-CUTOFF
           MOVE WS-CAN-PASTDUE         TO R2-D-PASTDUE
           WRITE RPT-RECORD FROM R2-DLQ-LINE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES.

      ******************************************************************
       7500-WRITE-TOTALS.
           MOVE 99                     TO WS-LINES
           PERFORM 7200-WRITE-TEXT
           MOVE 'CONTROL TOTALS'       TO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           MOVE 'ACCOUNTS TRANSITIONED' TO RH-TOT-LABEL
           MOVE WS-T-ACCOUNTS          TO RH-TOT-COUNT
           MOVE WS-T-AMOUNT-DUE        TO RH-TOT-AMOUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'ACCOUNTS REJECTED'    TO RH-TOT-LABEL
           MOVE WS-T-ACCTS-REJECTED    TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'ITEMS MOVED TO DUE'   TO RH-TOT-LABEL
           MOVE WS-T-ITEMS-DUE         TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'ITEMS CLOSED PAID AT DUE DATE' TO RH-TOT-LABEL
           MOVE WS-T-ITEMS-PAID        TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'INVOICES MOVED TO DUE' TO RH-TOT-LABEL
           MOVE WS-T-INV-DUE           TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'INVOICES CLOSED PAID' TO RH-TOT-LABEL
           MOVE WS-T-INV-PAID          TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'DELINQUENCY CANDIDATES' TO RH-TOT-LABEL
           MOVE WS-T-CANDIDATES        TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'DELINQUENCY STARTED / PAST DUE' TO RH-TOT-LABEL
           MOVE WS-T-DLQ-STARTED       TO RH-TOT-COUNT
           MOVE WS-T-DLQ-STARTED-AMT   TO RH-TOT-AMOUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'NOT STARTED - WITHIN THRESHOLD' TO RH-TOT-LABEL
           MOVE WS-T-DLQ-BELOW         TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'SUPPRESSED - DLNQ HOLD' TO RH-TOT-LABEL
           MOVE WS-T-DLQ-HOLD          TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'PROCESS ALREADY OPEN' TO RH-TOT-LABEL
           MOVE WS-T-DLQ-EXISTS        TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'DELINQUENCY REJECTED' TO RH-TOT-LABEL
           MOVE WS-T-DLQ-REJECTED      TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE
           MOVE 'COMMITS (INCLUDING PRIOR RUNS TODAY)' TO RH-TOT-LABEL
           MOVE WS-T-COMMITS           TO RH-TOT-COUNT
           PERFORM 7510-WRITE-TOTAL-LINE.

      ******************************************************************
       7510-WRITE-TOTAL-LINE.
           WRITE RPT-RECORD FROM RH-TOTAL-LINE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES.

      ******************************************************************
      * BR-BIL-IDU-004 Amount falling due: DR DUE / CR BILLED
       8200-POST-AMOUNT-DUE.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLC-REQUEST-ID
           MOVE WS-BATCH-USER          TO GLC-USER-ID
           MOVE WS-BUS-DATE            TO GLC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO GLC-CALLER-PGM
           MOVE 'INVDUE  '             TO GLC-GL-TXN-TYPE
           MOVE WS-CUR-ACCOUNT-ID      TO GLC-GL-ACCOUNT-ID
           MOVE 'ACCT'                 TO GLC-GL-REF-TYPE
           MOVE WS-CUR-ACCOUNT-ID      TO GLC-GL-REF-ID
           MOVE 2                      TO GLC-GL-LINE-COUNT
           MOVE 'DUE     '             TO GLC-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLC-GL-DR-CR(1)
           MOVE 'BILLED  '             TO GLC-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLC-GL-DR-CR(2)
           MOVE WS-A-AMOUNT-DUE        TO GLC-GL-AMOUNT(1)
                                          GLC-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLC-RETURN-CODE >= 90
              MOVE GLC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF GLC-RETURN-CODE >= 12
              SET WS-UNIT-REJECTED     TO TRUE
              STRING 'REJECTED ' GLC-REASON-CODE ' ' GLC-MESSAGE
                     DELIMITED BY SIZE INTO WS-REJECT-TEXT
           END-IF.

      ******************************************************************
       8300-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTC-USER-ID
           MOVE WS-BUS-DATE            TO DTC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 12
              AND DTC-REQUEST-ID NOT = 'DTVALD  '
              STRING 'BILDATES ' DTC-REASON-CODE ' ' DTC-MESSAGE
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-BIL-IDU-005 Refresh the account balances
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
              SET WS-UNIT-REJECTED     TO TRUE
              STRING 'REJECTED ' ACC-REASON-CODE ' ' ACC-MESSAGE
                     DELIMITED BY SIZE INTO WS-REJECT-TEXT
           END-IF.

      ******************************************************************
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
              PERFORM 7500-WRITE-TOTALS
           END-IF
           IF WS-T-DLQ-HOLD > 0 AND WS-MAX-RC < 4
              MOVE 4                   TO WS-MAX-RC
           END-IF
           IF WS-T-ACCTS-REJECTED > 0 OR WS-T-DLQ-REJECTED > 0
              MOVE 8                   TO WS-MAX-RC
           END-IF
           MOVE WS-MAX-RC              TO WS-CNT-DISP
           STRING 'BILINVDU ENDED - RETURN CODE ' WS-CNT-DISP
                  DELIMITED BY SIZE INTO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           CLOSE REPORT-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           EVALUATE WS-OPEN-CURSOR
              WHEN 'DUEACCTS'
                 EXEC SQL CLOSE C-DUE-ACCOUNTS END-EXEC
              WHEN 'GRACEEXP'
                 EXEC SQL CLOSE C-GRACE-EXPIRED END-EXEC
           END-EVALUATE
           DISPLAY 'BILINVDU ABEND: ' WS-ABEND-TEXT
           STRING 'RUN ABENDED - ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO RH-TEXT
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
