       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDLQSV.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDLQSV                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Delinquency process service. Starts a            *
      *               delinquency process for a policy period whose    *
      *               past-due amount exceeds the plan threshold,      *
      *               lays out its dated event steps, exits processes  *
      *               once the past due is cleared and reports a       *
      *               process with its steps.                          *
      * REQUESTS    : DLQSTRT  start process for ACCOUNT-ID / PP-ID    *
      *               DLQEXIT  close open processes of ACCOUNT-ID      *
      *               DLQINQ   process (by DP-ID or account) + steps   *
      * CALLED BY   : BILINVDU, BILDLQEV, BILDISTR (payments),         *
      *               BILDLQP screen                                   *
      * CALLS       : BILSEQGN BILDATES BILHOLD0 BILEVTQ BILAUDIT      *
      *               BILERRLG                                         *
      * TABLES      : BIL_DELINQ_PROCESS    (C R U)                    *
      *               BIL_DELINQ_EVENT      (C R U)                    *
      *               BIL_DELINQ_PLAN       (R)                        *
      *               BIL_DELINQ_STEP       (R)                        *
      *               BIL_ACCOUNT           (R U)                      *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_INVOICE_ITEM      (R)                        *
      *               BIL_CONTROL           (R)                        *
      * RULES       : BR-BIL-DLQ-001 Account and period are required;  *
      *                              the period must belong to the     *
      *                              account                           *
      *               BR-BIL-DLQ-002 Only an ACTV account and a period *
      *                              not yet closed can go delinquent  *
      *               BR-BIL-DLQ-003 Plan defaults to the account's    *
      *                              delinquency plan and must be      *
      *                              active                            *
      *               BR-BIL-DLQ-004 Past due = open amount of DUE     *
      *                              items of the period whose due     *
      *                              date is before the business date  *
      *               BR-BIL-DLQ-005 A process starts only when past   *
      *                              due exceeds PASTDUE_THRESHOLD     *
      *               BR-BIL-DLQ-006 An active DLNQ hold suppresses    *
      *                              delinquency                       *
      *               BR-BIL-DLQ-007 At most one OPEN process per      *
      *                              policy period                     *
      *               BR-BIL-DLQ-008 Step target = start date + days   *
      *                              after start, rolled forward to a  *
      *                              business day                      *
      *               BR-BIL-DLQ-009 A plan without steps cannot start *
      *                              a process                         *
      *               BR-BIL-DLQ-010 Collection status is DLNQ while   *
      *                              any process is open and returns   *
      *                              to NORM when the last one closes  *
      *               BR-BIL-DLQ-011 Every status change is published  *
      *                              to claims as a BILSTAT event      *
      *               BR-BIL-DLQ-012 Exit reason PAID needs past due   *
      *                              at or below threshold; other      *
      *                              reasons close unconditionally     *
      *               BR-BIL-DLQ-013 Closing a process cancels its     *
      *                              pending event steps               *
      *               BR-BIL-DLQ-014 Exit reasons are PAID WRITEOFF    *
      *                              POLCANC MANUAL                    *
      *               BR-BIL-DLQ-015 BILSTAT status is DLNQ on start,  *
      *                              CURR when cleared, PDUE when a    *
      *                              residue below threshold remains   *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDLQSV'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'BILEVTQ '.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-PASTDUE               PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-THRESHOLD             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-STEP-IX               PIC S9(4) COMP VALUE 0.
           05 WS-STEPS-MADE            PIC S9(4) COMP VALUE 0.
           05 WS-PROC-IX               PIC S9(4) COMP VALUE 0.
           05 WS-PROC-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-CLOSED-COUNT          PIC S9(4) COMP VALUE 0.
           05 WS-KEPT-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-OLD-COLL-STATUS       PIC X(4)  VALUE SPACES.
           05 WS-NEW-COLL-STATUS       PIC X(4)  VALUE SPACES.
           05 WS-BILL-STATUS           PIC X(4)  VALUE SPACES.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-AMT-DISP              PIC Z(8)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.
           05 WS-OPEN-CURSOR           PIC X(8)  VALUE SPACES.

      * Open processes of an account, loaded before any are closed
       01  WS-PROC-TABLE.
           05 WS-PROC OCCURS 20 TIMES.
              10 WS-P-DP-ID            PIC S9(9) COMP.
              10 WS-P-PP-ID            PIC S9(9) COMP.
              10 WS-P-PLAN-CODE        PIC X(8).

      * BILSTAT payload (docs/EVENTS.md)
       01  WS-BILSTAT-PAYLOAD.
           05 WS-BS-POLICY-NO          PIC 9(10).
           05 WS-BS-STATUS             PIC X(4).
           05 WS-BS-PASTDUE            PIC 9(11)V99.
           05 WS-BS-PASTDUE-SIGN       PIC X.
           05 WS-BS-AS-OF              PIC X(10).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDDLP END-EXEC.
           EXEC SQL INCLUDE BILDDEV END-EXEC.
           EXEC SQL INCLUDE BILDDST END-EXEC.
           EXEC SQL INCLUDE BILDDPL END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

           EXEC SQL
              DECLARE C-PLAN-STEPS CURSOR FOR
               SELECT STEP_SEQ, EVENT_CODE, DAYS_AFTER_START
                 FROM BILDB.BIL_DELINQ_STEP
                WHERE PLAN_CODE = :DST-PLAN-CODE
                ORDER BY STEP_SEQ
                FOR FETCH ONLY
           END-EXEC.

           EXEC SQL
              DECLARE C-OPEN-PROCS CURSOR FOR
               SELECT DP_ID, PP_ID, PLAN_CODE
                 FROM BILDB.BIL_DELINQ_PROCESS
                WHERE ACCOUNT_ID = :DLP-ACCOUNT-ID
                  AND DP_STATUS  = 'OPEN'
                  AND (PP_ID = :DLP-PP-ID OR :DLP-PP-ID = 0)
                ORDER BY DP_ID
                FOR FETCH ONLY
           END-EXEC.

           EXEC SQL
              DECLARE C-DP-EVENTS CURSOR FOR
               SELECT STEP_SEQ, EVENT_CODE, TARGET_DATE,
                      COMPLETED_DATE, EVENT_STATUS
                 FROM BILDB.BIL_DELINQ_EVENT
                WHERE DP_ID = :DEV-DP-ID
                ORDER BY STEP_SEQ
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
       01  WS-EVT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==EVC-==.
           03 EVC-BODY.
              COPY BILCEVT REPLACING LEADING ==CA-== BY ==EVC-==.
       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUC-==.
           03 AUC-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCDLQ.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'DLQSTRT '
                 PERFORM 2000-START-PROCESS
              WHEN 'DLQEXIT '
                 PERFORM 2100-EXIT-PROCESSES
              WHEN 'DLQINQ  '
                 PERFORM 2200-PROCESS-INQUIRY
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILDLQSV: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           SET WS-CURSOR-CLOSED        TO TRUE
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO CA-BUS-DATE
           END-IF
           MOVE CA-BUS-DATE            TO WS-BUS-DATE.

      ******************************************************************
      * DLQSTRT - open a delinquency process for a policy period
       2000-START-PROCESS.
           PERFORM 3000-VALIDATE-START
           PERFORM 4300-CALC-PASTDUE
           MOVE WS-PASTDUE             TO CA-DQ-PASTDUE-AMOUNT
      * BR-BIL-DLQ-005 Start only above the plan threshold
           IF WS-PASTDUE NOT > WS-THRESHOLD
              SET CA-RC-WARNING        TO TRUE
              MOVE 'BELOWTHR'          TO CA-REASON-CODE
              MOVE WS-PASTDUE          TO WS-AMT-DISP
              STRING 'Past due ' WS-AMT-DISP
                     ' not above plan threshold - no process'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8100-CHECK-DLNQ-HOLD
      * BR-BIL-DLQ-006 An active DLNQ hold suppresses delinquency
           IF HLC-HL-IS-ACTIVE
              SET CA-RC-WARNING        TO TRUE
              MOVE 'DLNQHOLD'          TO CA-REASON-CODE
              MOVE 'Delinquency suppressed by an active DLNQ hold'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4400-COUNT-OPEN-FOR-PERIOD
      * BR-BIL-DLQ-007 One OPEN process per policy period
           IF WS-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DPEXISTS'          TO CA-REASON-CODE
              MOVE CA-DQ-PP-ID         TO WS-ID-DISP
              STRING 'Policy period ' WS-ID-DISP
                     ' already has an open delinquency process'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 2010-CREATE-PROCESS
           PERFORM 2020-CREATE-STEPS
      * BR-BIL-DLQ-009 A plan without steps cannot start a process
           IF WS-STEPS-MADE = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOSTEPS '          TO CA-REASON-CODE
              STRING 'Delinquency plan ' DPL-PLAN-CODE
                     ' has no steps defined'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-DLQ-010 Collection status DLNQ while a process is open
           IF ACT-COLLECTION-STATUS NOT = 'DLNQ'
              MOVE 'DLNQ'              TO WS-NEW-COLL-STATUS
              PERFORM 4600-UPDATE-COLLECTION-STATUS
           END-IF
      * BR-BIL-DLQ-015 DLNQ is published on start
           MOVE 'DLNQ'                 TO WS-BILL-STATUS
           PERFORM 8200-PUBLISH-BILSTAT
           MOVE DLP-DP-ID              TO CA-DQ-DP-ID
           MOVE DLP-START-DATE         TO CA-DQ-START-DATE
           MOVE DLP-DP-STATUS          TO CA-DQ-STATUS
           MOVE DLP-PLAN-CODE          TO CA-DQ-PLAN-CODE
           MOVE SPACES                 TO CA-DQ-EXIT-REASON
           MOVE DLP-DP-ID              TO WS-ID-DISP
           STRING 'Delinquency process ' WS-ID-DISP ' started'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       2010-CREATE-PROCESS.
           MOVE 'DLP     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQC-SQ-VALUE           TO DLP-DP-ID
           MOVE CA-DQ-ACCOUNT-ID       TO DLP-ACCOUNT-ID
           MOVE CA-DQ-PP-ID            TO DLP-PP-ID
           MOVE DPL-PLAN-CODE          TO DLP-PLAN-CODE
           MOVE WS-BUS-DATE            TO DLP-START-DATE
           MOVE WS-PASTDUE             TO DLP-PASTDUE-AMOUNT
           MOVE 0                      TO DLP-CURRENT-STEP
           MOVE 'OPEN'                 TO DLP-DP-STATUS
           MOVE SPACES                 TO DLP-EXIT-REASON
           EXEC SQL
              INSERT INTO BILDB.BIL_DELINQ_PROCESS
                     (DP_ID, ACCOUNT_ID, PP_ID, PLAN_CODE, START_DATE,
                      PASTDUE_AMOUNT, CURRENT_STEP, DP_STATUS,
                      EXIT_REASON, UPDATE_TS)
              VALUES (:DLP-DP-ID, :DLP-ACCOUNT-ID, :DLP-PP-ID,
                      :DLP-PLAN-CODE, :DLP-START-DATE,
                      :DLP-PASTDUE-AMOUNT, :DLP-CURRENT-STEP,
                      :DLP-DP-STATUS, :DLP-EXIT-REASON,
                      CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE DLP-DP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '2010-CREATE-PROCESS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * One BIL_DELINQ_EVENT per plan step, first 10 echoed back
       2020-CREATE-STEPS.
           MOVE 0                      TO WS-STEPS-MADE
           MOVE 0                      TO CA-DQ-STEP-COUNT
           MOVE DPL-PLAN-CODE          TO DST-PLAN-CODE
           EXEC SQL OPEN C-PLAN-STEPS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2020-CREATE-STEPS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'STEPS   '             TO WS-OPEN-CURSOR
           PERFORM UNTIL SQLCODE = 100
              EXEC SQL
                 FETCH C-PLAN-STEPS
                  INTO :DST-STEP-SEQ, :DST-EVENT-CODE,
                       :DST-DAYS-AFTER-START
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 2021-CREATE-ONE-STEP
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '2020-CREATE-STEPS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR.

      ******************************************************************
      * BR-BIL-DLQ-008 Target = start + days, rolled to business day
       2021-CREATE-ONE-STEP.
           INITIALIZE DTC-BODY
           MOVE 'DTADDD  '             TO DTC-REQUEST-ID
           MOVE DLP-START-DATE         TO DTC-DT-DATE-1
           MOVE DST-DAYS-AFTER-START   TO DTC-DT-NUMBER
           PERFORM 8300-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO DEV-TARGET-DATE
           INITIALIZE DTC-BODY
           MOVE 'DTBUSD  '             TO DTC-REQUEST-ID
           MOVE DEV-TARGET-DATE        TO DTC-DT-DATE-1
           PERFORM 8300-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO DEV-TARGET-DATE
           MOVE DLP-DP-ID              TO DEV-DP-ID
           MOVE DST-STEP-SEQ           TO DEV-STEP-SEQ
           MOVE DST-EVENT-CODE         TO DEV-EVENT-CODE
           MOVE 'PEND'                 TO DEV-EVENT-STATUS
           MOVE SPACES                 TO DEV-RESULT-REF
           EXEC SQL
              INSERT INTO BILDB.BIL_DELINQ_EVENT
                     (DP_ID, STEP_SEQ, EVENT_CODE, TARGET_DATE,
                      EVENT_STATUS, RESULT_REF)
              VALUES (:DEV-DP-ID, :DEV-STEP-SEQ, :DEV-EVENT-CODE,
                      :DEV-TARGET-DATE, :DEV-EVENT-STATUS,
                      :DEV-RESULT-REF)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE DLP-DP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '2021-CREATE-ONE-STEP' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-STEPS-MADE
           IF WS-STEPS-MADE NOT > 10
              MOVE WS-STEPS-MADE       TO WS-STEP-IX
                                          CA-DQ-STEP-COUNT
              MOVE DEV-STEP-SEQ        TO CA-DQ-S-SEQ(WS-STEP-IX)
              MOVE DEV-EVENT-CODE      TO CA-DQ-S-EVENT(WS-STEP-IX)
              MOVE DEV-TARGET-DATE     TO CA-DQ-S-TARGET(WS-STEP-IX)
              MOVE SPACES              TO CA-DQ-S-DONE(WS-STEP-IX)
              MOVE DEV-EVENT-STATUS    TO CA-DQ-S-STATUS(WS-STEP-IX)
           END-IF.

      ******************************************************************
      * DLQEXIT - close the account's open processes once cleared
       2100-EXIT-PROCESSES.
           IF CA-DQ-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACCTREQ '          TO CA-REASON-CODE
              MOVE 'Account id is required to exit delinquency'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3100-VALIDATE-EXIT-REASON
           PERFORM 4100-READ-ACCOUNT
           PERFORM 4500-LOAD-OPEN-PROCESSES
           IF WS-PROC-COUNT = 0
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'NODPOPEN'          TO CA-REASON-CODE
              MOVE 'No open delinquency process for the account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-CLOSED-COUNT
                                          WS-KEPT-COUNT
           PERFORM VARYING WS-PROC-IX FROM 1 BY 1
                     UNTIL WS-PROC-IX > WS-PROC-COUNT
              PERFORM 2110-EXIT-ONE-PROCESS
           END-PERFORM
           MOVE CA-DQ-ACCOUNT-ID       TO DLP-ACCOUNT-ID
           MOVE 0                      TO DLP-PP-ID
           PERFORM 4410-COUNT-OPEN-FOR-ACCOUNT
      * BR-BIL-DLQ-010 Back to NORM when the last process closes
           IF WS-COUNT = 0 AND ACT-COLLECTION-STATUS = 'DLNQ'
              MOVE 'NORM'              TO WS-NEW-COLL-STATUS
              PERFORM 4600-UPDATE-COLLECTION-STATUS
           END-IF
           MOVE CA-DQ-EXIT-REASON      TO DLP-EXIT-REASON
           IF WS-CLOSED-COUNT = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'STILLDLQ'          TO CA-REASON-CODE
              MOVE 'Past due still above threshold - nothing closed'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'CLSD'              TO CA-DQ-STATUS
              MOVE WS-CLOSED-COUNT     TO WS-ID-DISP
              STRING 'Delinquency processes closed: ' WS-ID-DISP
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       2110-EXIT-ONE-PROCESS.
           MOVE WS-P-DP-ID(WS-PROC-IX) TO DLP-DP-ID
           MOVE WS-P-PP-ID(WS-PROC-IX) TO DLP-PP-ID
                                          PPR-PP-ID
           MOVE WS-P-PLAN-CODE(WS-PROC-IX) TO DPL-PLAN-CODE
           PERFORM 4210-READ-PLAN
           PERFORM 4300-CALC-PASTDUE
      * BR-BIL-DLQ-012 PAID needs past due at or below threshold
           IF CA-DQ-EXIT-REASON = 'PAID    '
              AND WS-PASTDUE > WS-THRESHOLD
              ADD 1                    TO WS-KEPT-COUNT
              EXIT PARAGRAPH
           END-IF
           PERFORM 4700-CLOSE-PROCESS
           IF SQLERRD(3) = 0
              ADD 1                    TO WS-KEPT-COUNT
              EXIT PARAGRAPH
           END-IF
           PERFORM 4710-CANCEL-PENDING-EVENTS
           ADD 1                       TO WS-CLOSED-COUNT
           MOVE DLP-DP-ID              TO CA-DQ-DP-ID
           MOVE DLP-PP-ID              TO CA-DQ-PP-ID
           MOVE DPL-PLAN-CODE          TO CA-DQ-PLAN-CODE
           MOVE WS-PASTDUE             TO CA-DQ-PASTDUE-AMOUNT
           PERFORM 4200-READ-PERIOD
      * BR-BIL-DLQ-015 CURR when cleared, PDUE for a residue
           IF WS-PASTDUE > 0
              MOVE 'PDUE'              TO WS-BILL-STATUS
           ELSE
              MOVE 'CURR'              TO WS-BILL-STATUS
           END-IF
           PERFORM 8200-PUBLISH-BILSTAT.

      ******************************************************************
      * DLQINQ - by DP-ID, else newest open (or latest) of the account
       2200-PROCESS-INQUIRY.
           IF CA-DQ-DP-ID NOT > 0 AND CA-DQ-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DQKEY   '          TO CA-REASON-CODE
              MOVE 'Process id or account id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-DQ-DP-ID > 0
              MOVE CA-DQ-DP-ID         TO DLP-DP-ID
           ELSE
              PERFORM 4800-FIND-LATEST-PROCESS
           END-IF
           PERFORM 4810-READ-PROCESS
           MOVE DLP-DP-ID              TO CA-DQ-DP-ID
           MOVE DLP-ACCOUNT-ID         TO CA-DQ-ACCOUNT-ID
           MOVE DLP-PP-ID              TO CA-DQ-PP-ID
           MOVE DLP-PLAN-CODE          TO CA-DQ-PLAN-CODE
           MOVE DLP-PASTDUE-AMOUNT     TO CA-DQ-PASTDUE-AMOUNT
           MOVE DLP-START-DATE         TO CA-DQ-START-DATE
           MOVE DLP-DP-STATUS          TO CA-DQ-STATUS
           MOVE DLP-EXIT-REASON        TO CA-DQ-EXIT-REASON
           PERFORM 4820-LIST-PROCESS-EVENTS.

      ******************************************************************
       3000-VALIDATE-START.
      * BR-BIL-DLQ-001 Account and period are required
           IF CA-DQ-ACCOUNT-ID NOT > 0 OR CA-DQ-PP-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DQKEY   '          TO CA-REASON-CODE
              MOVE 'Account id and policy period id are required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-ACCOUNT
      * BR-BIL-DLQ-002 Only active accounts go delinquent
           IF ACT-ACCOUNT-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTNOTAC'          TO CA-REASON-CODE
              STRING 'Account status ' ACT-ACCOUNT-STATUS
                     ' cannot enter delinquency'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-DQ-PP-ID            TO PPR-PP-ID
           PERFORM 4200-READ-PERIOD
           IF PPR-ACCOUNT-ID NOT = CA-DQ-ACCOUNT-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PPACCT  '          TO CA-REASON-CODE
              MOVE 'Policy period does not belong to the account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF PPR-CLOSURE-STATUS = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'PPCLOSED'          TO CA-REASON-CODE
              MOVE 'Policy period is closed for billing'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-DLQ-003 Plan defaults to the account's plan
           IF CA-DQ-PLAN-CODE = SPACES OR LOW-VALUES
              MOVE ACT-DELINQ-PLAN-CODE TO CA-DQ-PLAN-CODE
           END-IF
           MOVE CA-DQ-PLAN-CODE        TO DPL-PLAN-CODE
           PERFORM 4210-READ-PLAN
           IF DPL-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DPLINACT'          TO CA-REASON-CODE
              STRING 'Delinquency plan ' DPL-PLAN-CODE
                     ' is not active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-DLQ-014 Exit reasons PAID WRITEOFF POLCANC MANUAL
       3100-VALIDATE-EXIT-REASON.
           IF CA-DQ-EXIT-REASON = SPACES OR LOW-VALUES
              MOVE 'PAID    '          TO CA-DQ-EXIT-REASON
           END-IF
           EVALUATE CA-DQ-EXIT-REASON
              WHEN 'PAID    '
              WHEN 'WRITEOFF'
              WHEN 'POLCANC '
              WHEN 'MANUAL  '
                 CONTINUE
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'EXITRSN '       TO CA-REASON-CODE
                 STRING 'Exit reason ' CA-DQ-EXIT-REASON
                        ' is not PAID/WRITEOFF/POLCANC/MANUAL'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
       4100-READ-ACCOUNT.
           MOVE CA-DQ-ACCOUNT-ID       TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_STATUS, COLLECTION_STATUS,
                     DELINQ_PLAN_CODE, VERSION_NO
                INTO :ACT-ACCOUNT-STATUS, :ACT-COLLECTION-STATUS,
                     :ACT-DELINQ-PLAN-CODE, :ACT-VERSION-NO
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 MOVE ACT-ACCOUNT-ID   TO WS-ID-DISP
                 STRING 'Billing account ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE ACT-ACCOUNT-ID   TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4100-READ-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-PERIOD.
           EXEC SQL
              SELECT ACCOUNT_ID, GENAPP_POL_NUM, PP_STATUS,
                     CLOSURE_STATUS
                INTO :PPR-ACCOUNT-ID, :PPR-GENAPP-POL-NUM,
                     :PPR-PP-STATUS, :PPR-CLOSURE-STATUS
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE PP_ID = :PPR-PP-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PPNOTFD '       TO CA-REASON-CODE
                 MOVE PPR-PP-ID        TO WS-ID-DISP
                 STRING 'Policy period ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE PPR-PP-ID        TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4200-READ-PERIOD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4210-READ-PLAN.
           EXEC SQL
              SELECT GRACE_DAYS, PASTDUE_THRESHOLD, ACTIVE_FLAG
                INTO :DPL-GRACE-DAYS, :DPL-PASTDUE-THRESHOLD,
                     :DPL-ACTIVE-FLAG
                FROM BILDB.BIL_DELINQ_PLAN
               WHERE PLAN_CODE = :DPL-PLAN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE DPL-PASTDUE-THRESHOLD TO WS-THRESHOLD
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DPLNOTFD'       TO CA-REASON-CODE
                 STRING 'Delinquency plan ' DPL-PLAN-CODE
                        ' not defined'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE DPL-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '4210-READ-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4290-CLOSE-CURSOR.
           IF WS-CURSOR-OPEN
              EVALUATE WS-OPEN-CURSOR
                 WHEN 'STEPS   '
                    EXEC SQL CLOSE C-PLAN-STEPS END-EXEC
                 WHEN 'OPENPROC'
                    EXEC SQL CLOSE C-OPEN-PROCS END-EXEC
                 WHEN 'DPEVENTS'
                    EXEC SQL CLOSE C-DP-EVENTS END-EXEC
              END-EVALUATE
              SET WS-CURSOR-CLOSED     TO TRUE
              MOVE SPACES              TO WS-OPEN-CURSOR
           END-IF.

      ******************************************************************
      * BR-BIL-DLQ-004 Past due = open DUE items due before today
       4300-CALC-PASTDUE.
           EXEC SQL
              SELECT COALESCE(SUM(ITEM_AMOUNT - PAID_AMOUNT
                                  - WRITEOFF_AMOUNT), 0)
                INTO :WS-PASTDUE
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE PP_ID       = :PPR-PP-ID
                 AND ITEM_STATUS = 'DUE '
                 AND DUE_DATE    < :WS-BUS-DATE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PPR-PP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4300-CALC-PASTDUE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-COUNT-OPEN-FOR-PERIOD.
           MOVE CA-DQ-ACCOUNT-ID       TO DLP-ACCOUNT-ID
           MOVE CA-DQ-PP-ID            TO DLP-PP-ID
           PERFORM 4410-COUNT-OPEN-FOR-ACCOUNT.

      ******************************************************************
      * Open processes of DLP-ACCOUNT-ID (one period when DLP-PP-ID set)
       4410-COUNT-OPEN-FOR-ACCOUNT.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_DELINQ_PROCESS
               WHERE ACCOUNT_ID = :DLP-ACCOUNT-ID
                 AND DP_STATUS  = 'OPEN'
                 AND (PP_ID = :DLP-PP-ID OR :DLP-PP-ID = 0)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4410-COUNT-OPEN-FOR-ACCOUNT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4500-LOAD-OPEN-PROCESSES.
           MOVE 0                      TO WS-PROC-COUNT
           MOVE CA-DQ-ACCOUNT-ID       TO DLP-ACCOUNT-ID
           MOVE CA-DQ-PP-ID            TO DLP-PP-ID
           EXEC SQL OPEN C-OPEN-PROCS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-LOAD-OPEN-PROCESSES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'OPENPROC'             TO WS-OPEN-CURSOR
           PERFORM UNTIL SQLCODE = 100 OR WS-PROC-COUNT = 20
              EXEC SQL
                 FETCH C-OPEN-PROCS
                  INTO :DLP-DP-ID, :DLP-PP-ID, :DLP-PLAN-CODE
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-PROC-COUNT
                    MOVE DLP-DP-ID     TO WS-P-DP-ID(WS-PROC-COUNT)
                    MOVE DLP-PP-ID     TO WS-P-PP-ID(WS-PROC-COUNT)
                    MOVE DLP-PLAN-CODE
                                       TO WS-P-PLAN-CODE(WS-PROC-COUNT)
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '4500-LOAD-OPEN-PROCESSES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR.

      ******************************************************************
      * Optimistic lock on the account row; audit the status change
       4600-UPDATE-COLLECTION-STATUS.
           MOVE ACT-COLLECTION-STATUS  TO WS-OLD-COLL-STATUS
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET COLLECTION_STATUS = :WS-NEW-COLL-STATUS,
                     VERSION_NO        = VERSION_NO + 1,
                     UPDATE_TS         = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
                 AND VERSION_NO = :ACT-VERSION-NO
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ACT-ACCOUNT-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4600-UPDATE-COLLECTION-STATUS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Account changed by another task - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           ADD 1                       TO ACT-VERSION-NO
           MOVE WS-NEW-COLL-STATUS     TO ACT-COLLECTION-STATUS
           PERFORM 8400-AUDIT-COLLECTION-STATUS.

      ******************************************************************
       4700-CLOSE-PROCESS.
           MOVE CA-DQ-EXIT-REASON      TO DLP-EXIT-REASON
           MOVE WS-BUS-DATE            TO DLP-EXIT-DATE
           EXEC SQL
              UPDATE BILDB.BIL_DELINQ_PROCESS
                 SET DP_STATUS      = 'CLSD',
                     EXIT_REASON    = :DLP-EXIT-REASON,
                     EXIT_DATE      = :DLP-EXIT-DATE,
                     PASTDUE_AMOUNT = :WS-PASTDUE,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE DP_ID     = :DLP-DP-ID
                 AND DP_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE DLP-DP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4700-CLOSE-PROCESS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-DLQ-013 Closing a process cancels its pending steps
       4710-CANCEL-PENDING-EVENTS.
           MOVE DLP-DP-ID              TO DEV-DP-ID
           EXEC SQL
              UPDATE BILDB.BIL_DELINQ_EVENT
                 SET EVENT_STATUS = 'CANC',
                     RESULT_REF   = :DLP-EXIT-REASON
               WHERE DP_ID        = :DEV-DP-ID
                 AND EVENT_STATUS = 'PEND'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE DLP-DP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4710-CANCEL-PENDING-EVENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4800-FIND-LATEST-PROCESS.
           MOVE CA-DQ-ACCOUNT-ID       TO DLP-ACCOUNT-ID
           MOVE CA-DQ-PP-ID            TO DLP-PP-ID
           EXEC SQL
              SELECT DP_ID
                INTO :DLP-DP-ID
                FROM BILDB.BIL_DELINQ_PROCESS
               WHERE ACCOUNT_ID = :DLP-ACCOUNT-ID
                 AND (PP_ID = :DLP-PP-ID OR :DLP-PP-ID = 0)
               ORDER BY CASE DP_STATUS WHEN 'OPEN' THEN 0 ELSE 1 END,
                        START_DATE DESC, DP_ID DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'DPNOTFD '       TO CA-REASON-CODE
                 MOVE 'No delinquency process for the account'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE DLP-ACCOUNT-ID   TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4800-FIND-LATEST-PROCESS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4810-READ-PROCESS.
           EXEC SQL
              SELECT DP_ID, ACCOUNT_ID, PP_ID, PLAN_CODE, START_DATE,
                     PASTDUE_AMOUNT, CURRENT_STEP, DP_STATUS,
                     EXIT_REASON, EXIT_DATE
                INTO :DLP-DP-ID, :DLP-ACCOUNT-ID, :DLP-PP-ID,
                     :DLP-PLAN-CODE, :DLP-START-DATE,
                     :DLP-PASTDUE-AMOUNT, :DLP-CURRENT-STEP,
                     :DLP-DP-STATUS, :DLP-EXIT-REASON,
                     :DLP-EXIT-DATE :DLP-EXIT-DATE-IND
                FROM BILDB.BIL_DELINQ_PROCESS
               WHERE DP_ID = :DLP-DP-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF DLP-EXIT-DATE-IND < 0
                    MOVE SPACES        TO DLP-EXIT-DATE
                 END-IF
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'DPNOTFD '       TO CA-REASON-CODE
                 MOVE DLP-DP-ID        TO WS-ID-DISP
                 STRING 'Delinquency process ' WS-ID-DISP
                        ' not found' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE DLP-DP-ID        TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4810-READ-PROCESS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4820-LIST-PROCESS-EVENTS.
           MOVE 0                      TO CA-DQ-STEP-COUNT
           MOVE DLP-DP-ID              TO DEV-DP-ID
           EXEC SQL OPEN C-DP-EVENTS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4820-LIST-PROCESS-EVENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'DPEVENTS'             TO WS-OPEN-CURSOR
           PERFORM UNTIL SQLCODE = 100 OR CA-DQ-STEP-COUNT = 10
              EXEC SQL
                 FETCH C-DP-EVENTS
                  INTO :DEV-STEP-SEQ, :DEV-EVENT-CODE,
                       :DEV-TARGET-DATE,
                       :DEV-COMPLETED-DATE :DEV-COMPLETED-DATE-IND,
                       :DEV-EVENT-STATUS
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO CA-DQ-STEP-COUNT
                    MOVE CA-DQ-STEP-COUNT TO WS-STEP-IX
                    MOVE DEV-STEP-SEQ  TO CA-DQ-S-SEQ(WS-STEP-IX)
                    MOVE DEV-EVENT-CODE
                                       TO CA-DQ-S-EVENT(WS-STEP-IX)
                    MOVE DEV-TARGET-DATE
                                       TO CA-DQ-S-TARGET(WS-STEP-IX)
                    IF DEV-COMPLETED-DATE-IND < 0
                       MOVE SPACES     TO CA-DQ-S-DONE(WS-STEP-IX)
                    ELSE
                       MOVE DEV-COMPLETED-DATE
                                       TO CA-DQ-S-DONE(WS-STEP-IX)
                    END-IF
                    MOVE DEV-EVENT-STATUS
                                       TO CA-DQ-S-STATUS(WS-STEP-IX)
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '4820-LIST-PROCESS-EVENTS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR.

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
              MOVE 'SYSTEM'            TO WS-ERR-KEY
              MOVE '4900-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       8000-NEXT-SEQUENCE.
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE 1                      TO SQC-SQ-COUNT
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE CA-TRACE-ID            TO SQC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8100-CHECK-DLNQ-HOLD.
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLC-REQUEST-ID
           MOVE CA-USER-ID             TO HLC-USER-ID
           MOVE WS-BUS-DATE            TO HLC-BUS-DATE
           MOVE CA-TRACE-ID            TO HLC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HLC-CALLER-PGM
           MOVE CA-DQ-ACCOUNT-ID       TO HLC-HL-ACCOUNT-ID
           MOVE 'DLNQ'                 TO HLC-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLC-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           IF HLC-RETURN-CODE >= 12
              MOVE HLC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HLC-REASON-CODE     TO CA-REASON-CODE
              MOVE HLC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-DLQ-011 Billing status to claims via BILSTAT
       8200-PUBLISH-BILSTAT.
           MOVE PPR-GENAPP-POL-NUM     TO WS-BS-POLICY-NO
           MOVE WS-BILL-STATUS         TO WS-BS-STATUS
           IF WS-PASTDUE < 0
              COMPUTE WS-BS-PASTDUE = 0 - WS-PASTDUE
              MOVE '-'                 TO WS-BS-PASTDUE-SIGN
           ELSE
              MOVE WS-PASTDUE          TO WS-BS-PASTDUE
              MOVE '+'                 TO WS-BS-PASTDUE-SIGN
           END-IF
           MOVE WS-BUS-DATE            TO WS-BS-AS-OF
           INITIALIZE WS-EVT-CA
           MOVE 'EVPUT   '             TO EVC-REQUEST-ID
           MOVE CA-USER-ID             TO EVC-USER-ID
           MOVE WS-BUS-DATE            TO EVC-BUS-DATE
           MOVE CA-TRACE-ID            TO EVC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO EVC-CALLER-PGM
           MOVE 'BILSTAT '             TO EVC-EV-TYPE
           MOVE 'O'                    TO EVC-EV-DIRECTION
           MOVE 'BILL'                 TO EVC-EV-SOURCE
           MOVE 'CLMS'                 TO EVC-EV-TARGET
           MOVE WS-BS-POLICY-NO        TO EVC-EV-KEY
           MOVE WS-BILSTAT-PAYLOAD     TO EVC-EV-PAYLOAD
           CALL WS-EVT-PGM USING WS-EVT-CA
           IF EVC-RETURN-CODE >= 12
              MOVE EVC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE EVC-REASON-CODE     TO CA-REASON-CODE
              MOVE EVC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8300-CALL-DATES.
           MOVE CA-USER-ID             TO DTC-USER-ID
           MOVE WS-BUS-DATE            TO DTC-BUS-DATE
           MOVE CA-TRACE-ID            TO DTC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 12
              MOVE DTC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTC-REASON-CODE     TO CA-REASON-CODE
              MOVE DTC-MESSAGE         TO CA-MESSAGE
              PERFORM 4290-CLOSE-CURSOR
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8400-AUDIT-COLLECTION-STATUS.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUC-REQUEST-ID
           MOVE CA-USER-ID             TO AUC-USER-ID
           MOVE WS-BUS-DATE            TO AUC-BUS-DATE
           MOVE CA-TRACE-ID            TO AUC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUC-CALLER-PGM
           MOVE 'ACCOUNT '             TO AUC-AU-ENTITY
           MOVE ACT-ACCOUNT-ID         TO WS-ID-DISP
           MOVE WS-ID-DISP             TO AUC-AU-KEY
           MOVE 'UPD '                 TO AUC-AU-ACTION
           MOVE 'COLLECTION_STATUS'    TO AUC-AU-FIELD
           MOVE WS-OLD-COLL-STATUS     TO AUC-AU-BEFORE
           MOVE WS-NEW-COLL-STATUS     TO AUC-AU-AFTER
           MOVE WS-PROGRAM-ID          TO AUC-AU-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF AUC-RETURN-CODE >= 12
              MOVE AUC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AUC-REASON-CODE     TO CA-REASON-CODE
              MOVE AUC-MESSAGE         TO CA-MESSAGE
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
                 MOVE 'Delinquency data locked - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DB2ERR  '       TO CA-REASON-CODE
                 STRING 'BILDLQSV Db2 error SQLCODE ' WS-SQLCODE-DISP
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
           PERFORM 4290-CLOSE-CURSOR
           PERFORM 9000-RETURN.
