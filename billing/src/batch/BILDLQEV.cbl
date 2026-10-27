       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDLQEV.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDLQEV                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Delinquency event executor. Works the pending    *
      *               BIL_DELINQ_EVENT steps of open processes whose   *
      *               target date has arrived, in process / step       *
      *               order:                                           *
      *                 REMIND   reminder document request             *
      *                 LATEFEE  late-fee charge, scheduled one-time   *
      *                 NOI      notice-of-intent document request     *
      *                 CANCREQ  CANCREQ event to GenApp operations    *
      *                 WRITEOFF small-balance write-off sweep         *
      *               A process whose past due has already been        *
      *               cleared is exited (BILDLQSV DLQEXIT) instead.    *
      * INPUT       : SYSIN   control card            (BILINVC1)       *
      *               CHKPT   checkpoint / restart     (BILINVK1)      *
      * OUTPUT      : CHKPT, RPTOUT run report (BILINVH1, BILDLQR1)    *
      * CALLS       : BILSEQGN BILDATES BILISCHD BILGLPST BILEVTQ      *
      *               BILWOFF0 BILDLQSV BILERRLG                       *
      * TABLES      : BIL_DELINQ_EVENT      (R U)                      *
      *               BIL_DELINQ_PROCESS    (R U)                      *
      *               BIL_DELINQ_STEP       (R)                        *
      *               BIL_DELINQ_PLAN       (R)                        *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_INVOICE_ITEM      (R)                        *
      *               BIL_CHARGE            (C)                        *
      *               BIL_DOCUMENT_REQUEST  (C)                        *
      *               BIL_CONTROL           (R)                        *
      * RESTART     : key 1 = DP_ID, key 2 = STEP_SEQ of the last      *
      *               committed event; completed events are DONE and   *
      *               are never picked up again                       *
      * RULES       : BR-BIL-DEV-001 Only PEND steps of OPEN processes *
      *                              with target date on or before the *
      *                              business date are executed, in    *
      *                              process and step order            *
      *               BR-BIL-DEV-002 A step waits while an earlier     *
      *                              step of its process is pending    *
      *               BR-BIL-DEV-003 When past due is at or below the  *
      *                              plan threshold the process is     *
      *                              exited (PAID) and its steps are   *
      *                              skipped                           *
      *               BR-BIL-DEV-004 Late fee is a one-time charge of  *
      *                              plan LATE_FEE (pattern LATEFEE,   *
      *                              instruction DELINQ); zero fee is  *
      *                              completed without a charge        *
      *               BR-BIL-DEV-005 Late fee posts DR UNBILLED / CR   *
      *                              FEEREV                            *
      *               BR-BIL-DEV-006 CANCREQ asks GenApp operations to *
      *                              cancel for NONPAY as of the       *
      *                              business date with the past due   *
      *               BR-BIL-DEV-007 After the write-off sweep a       *
      *                              cleared process exits WRITEOFF    *
      *               BR-BIL-DEV-008 A failed step is rolled back and  *
      *                              left PEND for the next run; later *
      *                              steps of the process are held     *
      * RETURN CODE : 0 ok, 4 unknown steps skipped, 8 rejects,        *
      *               16 abended                                       *
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
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDLQEV'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'BILBATCH'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-SCH-PGM                  PIC X(8)  VALUE 'BILISCHD'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'BILEVTQ '.
       01  WS-WOF-PGM                  PIC X(8)  VALUE 'BILWOFF0'.
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
           05 WS-RESTART-DP            PIC S9(9) COMP VALUE 0.
           05 WS-RESTART-STEP          PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-RUN-FLAG              PIC X     VALUE 'N'.
              88 WS-RUN-ENDED                    VALUE 'Y'.
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-END-OF-EVENTS                VALUE 'Y'.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.
           05 WS-ABEND-TEXT            PIC X(100) VALUE SPACES.

      * State of the process whose steps are being worked
       01  WS-PROCESS-STATE.
           05 WS-CUR-DP-ID             PIC S9(9) COMP VALUE 0.
           05 WS-DP-FLAG               PIC X     VALUE 'A'.
              88 WS-DP-ACTIVE                    VALUE 'A'.
              88 WS-DP-STOPPED                   VALUE 'S'.
              88 WS-DP-HELD                      VALUE 'H'.
           05 WS-PASTDUE               PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-THRESHOLD             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-EARLIER-PEND          PIC S9(9) COMP VALUE 0.

      * Current event row
       01  WS-EVENT-ROW.
           05 WS-EV-DP-ID              PIC S9(9) COMP.
           05 WS-EV-STEP               PIC S9(4) COMP.
           05 WS-EV-CODE               PIC X(8).
           05 WS-EV-TARGET             PIC X(10).
           05 WS-EV-ACCOUNT-ID         PIC S9(9) COMP.
           05 WS-EV-PP-ID              PIC S9(9) COMP.
           05 WS-EV-PLAN-CODE          PIC X(8).
           05 WS-EV-DOC-TYPE           PIC X(8).

       01  WS-STEP-RESULT.
           05 WS-STEP-FLAG             PIC X     VALUE 'D'.
              88 WS-STEP-DONE                    VALUE 'D'.
              88 WS-STEP-REJECTED                VALUE 'R'.
              88 WS-STEP-SKIPPED                 VALUE 'S'.
              88 WS-STEP-WAITING                 VALUE 'W'.
           05 WS-RESULT-REF            PIC X(20) VALUE SPACES.
           05 WS-RESULT-TEXT           PIC X(62) VALUE SPACES.
           05 WS-STEP-AMOUNT           PIC S9(11)V99 COMP-3 VALUE 0.

      * Counts per event type; entry 6 collects unknown codes
       01  WS-TYPE-TABLE.
           05 WS-TYPE OCCURS 6 TIMES INDEXED BY TX.
              10 WS-TY-CODE            PIC X(8).
              10 WS-TY-DONE            PIC S9(7) COMP-3.
              10 WS-TY-SKIPPED         PIC S9(7) COMP-3.
              10 WS-TY-WAITING         PIC S9(7) COMP-3.
              10 WS-TY-REJECTED        PIC S9(7) COMP-3.
              10 WS-TY-AMOUNT          PIC S9(13)V99 COMP-3.

       01  WS-TOTALS.
           05 WS-T-EVENTS-READ         PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-PROCS-EXITED        PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-COMMITS             PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-UNKNOWN             PIC S9(9) COMP-3 VALUE 0.

      * CANCREQ payload (docs/EVENTS.md)
       01  WS-CANCREQ-PAYLOAD.
           05 WS-CR-POLICY-NO          PIC 9(10).
           05 WS-CR-CANCEL-DATE        PIC X(10).
           05 WS-CR-REASON             PIC X(8)  VALUE 'NONPAY  '.
           05 WS-CR-PASTDUE            PIC 9(11)V99.
           05 WS-CR-PASTDUE-SIGN       PIC X.

       01  WS-WORK.
           05 WS-GL-AMOUNT             PIC S9(11)V99 COMP-3.
           05 WS-EXIT-REASON           PIC X(8).
           05 WS-PAGE                  PIC S9(4) COMP VALUE 0.
           05 WS-LINES                 PIC S9(4) COMP VALUE 99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-STEP-DISP             PIC 9(4).
           05 WS-CNT-DISP              PIC Z(8)9.
           05 WS-AMT-DISP              PIC Z(8)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURRENT-TS            PIC X(26).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDDEV END-EXEC.
           EXEC SQL INCLUDE BILDDLP END-EXEC.
           EXEC SQL INCLUDE BILDDPL END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDDOC END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-DEV-001 Due steps of open processes in step order
           EXEC SQL
              DECLARE C-DUE-EVENTS CURSOR WITH HOLD FOR
               SELECT E.DP_ID, E.STEP_SEQ, E.EVENT_CODE,
                      E.TARGET_DATE, D.ACCOUNT_ID, D.PP_ID,
                      D.PLAN_CODE, COALESCE(S.DOC_TYPE, ' ')
                 FROM BILDB.BIL_DELINQ_EVENT E
                 JOIN BILDB.BIL_DELINQ_PROCESS D
                   ON D.DP_ID = E.DP_ID
                 LEFT JOIN BILDB.BIL_DELINQ_STEP S
                   ON S.PLAN_CODE = D.PLAN_CODE
                  AND S.STEP_SEQ  = E.STEP_SEQ
                WHERE E.EVENT_STATUS = 'PEND'
                  AND E.TARGET_DATE <= :WS-BUS-DATE
                  AND D.DP_STATUS    = 'OPEN'
                  AND (E.DP_ID > :WS-RESTART-DP
                       OR (E.DP_ID = :WS-RESTART-DP
                           AND E.STEP_SEQ > :WS-RESTART-STEP))
                ORDER BY E.DP_ID, E.STEP_SEQ
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
       01  WS-SCH-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SCC-==.
           03 SCC-BODY.
              COPY BILCSCH REPLACING LEADING ==CA-== BY ==SCC-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLC-==.
           03 GLC-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLC-==.
       01  WS-EVT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==EVC-==.
           03 EVC-BODY.
              COPY BILCEVT REPLACING LEADING ==CA-== BY ==EVC-==.
       01  WS-WOF-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==WOC-==.
           03 WOC-BODY.
              COPY BILCWOF REPLACING LEADING ==CA-== BY ==WOC-==.
       01  WS-DLQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DQC-==.
           03 DQC-BODY.
              COPY BILCDLQ REPLACING LEADING ==CA-== BY ==DQC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILINVH1.
           COPY BILDLQR1.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF NOT WS-RUN-ENDED
              PERFORM 2000-PROCESS-EVENTS
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN INPUT  CONTROL-CARD
                OUTPUT REPORT-FILE
           IF WS-CC-STATUS NOT = '00' OR WS-RP-STATUS NOT = '00'
              DISPLAY 'BILDLQEV OPEN FAILED SYSIN=' WS-CC-STATUS
                      ' RPTOUT=' WS-RP-STATUS
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE WS-PROGRAM-ID          TO RH-PROGRAM-ID
           MOVE 'DELINQUENCY EVENT EXECUTION' TO RH-TITLE
           PERFORM 1100-READ-CONTROL-CARD
           PERFORM 1200-RESOLVE-BUS-DATE
           MOVE WS-BUS-DATE            TO RH-BUS-DATE
           MOVE WS-RUN-DATE            TO RH-RUN-DATE
           PERFORM 1300-READ-CHECKPOINT
           PERFORM 1400-LOAD-TYPE-TABLE.

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
           MOVE WS-BATCH-USER          TO DTC-USER-ID
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTC-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
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
                    MOVE CK-LAST-KEY-1 TO WS-RESTART-DP
                    MOVE CK-LAST-KEY-2 TO WS-RESTART-STEP
                    MOVE CK-COMMITS    TO WS-T-COMMITS
                    MOVE CK-LAST-KEY-1 TO WS-ID-DISP
                    MOVE CK-LAST-KEY-2 TO WS-STEP-DISP
                    STRING 'RESTART AFTER PROCESS ' WS-ID-DISP
                           ' STEP ' WS-STEP-DISP
                           DELIMITED BY SIZE INTO RH-TEXT
                    PERFORM 7200-WRITE-TEXT
                 WHEN CK-COMPLETE AND NOT CC-RESTART-FORCE
                    MOVE 'BUSINESS DATE ALREADY COMPLETE - NOTHING DONE'
                                       TO RH-TEXT
                    PERFORM 7200-WRITE-TEXT
                    MOVE 4             TO WS-MAX-RC
                    SET WS-RUN-ENDED   TO TRUE
              END-EVALUATE
           END-IF.

      ******************************************************************
       1400-LOAD-TYPE-TABLE.
           INITIALIZE WS-TYPE-TABLE
           MOVE 'REMIND  '             TO WS-TY-CODE(1)
           MOVE 'LATEFEE '             TO WS-TY-CODE(2)
           MOVE 'NOI     '             TO WS-TY-CODE(3)
           MOVE 'CANCREQ '             TO WS-TY-CODE(4)
           MOVE 'WRITEOFF'             TO WS-TY-CODE(5)
           MOVE 'UNKNOWN '             TO WS-TY-CODE(6).

      ******************************************************************
       2000-PROCESS-EVENTS.
           EXEC SQL OPEN C-DUE-EVENTS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-EVENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4000-FETCH-EVENT
           PERFORM UNTIL WS-END-OF-EVENTS
              ADD 1                    TO WS-T-EVENTS-READ
              IF WS-EV-DP-ID NOT = WS-CUR-DP-ID
                 PERFORM 2100-START-PROCESS
              END-IF
              PERFORM 2200-EXECUTE-STEP
              PERFORM 4000-FETCH-EVENT
           END-PERFORM
           EXEC SQL CLOSE C-DUE-EVENTS END-EXEC
           SET WS-CURSOR-CLOSED        TO TRUE.

      ******************************************************************
      * New process: load its context and exit it if already cleared
       2100-START-PROCESS.
           MOVE WS-EV-DP-ID            TO WS-CUR-DP-ID
           SET WS-DP-ACTIVE            TO TRUE
           PERFORM 4100-READ-CONTEXT
           PERFORM 4200-CALC-PASTDUE
      * BR-BIL-DEV-003 Cleared past due exits the process
           IF WS-PASTDUE NOT > WS-THRESHOLD
              EXEC SQL
                 SAVEPOINT EXITUNIT ON ROLLBACK RETAIN CURSORS
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '2100-START-PROCESS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE 'PAID    '          TO WS-EXIT-REASON
              PERFORM 8600-EXIT-DELINQUENCY
              IF DQC-RETURN-CODE < 12
                 SET WS-DP-STOPPED     TO TRUE
                 ADD 1                 TO WS-T-PROCS-EXITED
              ELSE
                 EXEC SQL ROLLBACK TO SAVEPOINT EXITUNIT END-EXEC
                 SET WS-DP-HELD        TO TRUE
              END-IF
              EXEC SQL RELEASE SAVEPOINT EXITUNIT END-EXEC
           END-IF.

      ******************************************************************
       2200-EXECUTE-STEP.
           MOVE SPACES                 TO WS-RESULT-REF
                                          WS-RESULT-TEXT
           MOVE 0                      TO WS-STEP-AMOUNT
           PERFORM 5000-FIND-TYPE
           EVALUATE TRUE
              WHEN WS-DP-STOPPED
                 SET WS-STEP-SKIPPED   TO TRUE
                 MOVE 'SKIPPED - PAST DUE CLEARED, PROCESS EXITED'
                                       TO WS-RESULT-TEXT
              WHEN WS-DP-HELD
                 SET WS-STEP-WAITING   TO TRUE
                 MOVE 'HELD - EARLIER STEP NOT COMPLETED'
                                       TO WS-RESULT-TEXT
              WHEN OTHER
                 PERFORM 4300-COUNT-EARLIER-PENDING
      * BR-BIL-DEV-002 Wait while an earlier step is still pending
                 IF WS-EARLIER-PEND > 0
                    SET WS-STEP-WAITING TO TRUE
                    SET WS-DP-HELD     TO TRUE
                    MOVE 'WAITING - EARLIER STEP STILL PENDING'
                                       TO WS-RESULT-TEXT
                 ELSE
                    PERFORM 2300-RUN-STEP
                 END-IF
           END-EVALUATE
           EVALUATE TRUE
              WHEN WS-STEP-DONE
                 ADD 1                 TO WS-TY-DONE(TX)
                 ADD WS-STEP-AMOUNT    TO WS-TY-AMOUNT(TX)
              WHEN WS-STEP-SKIPPED
                 ADD 1                 TO WS-TY-SKIPPED(TX)
              WHEN WS-STEP-WAITING
                 ADD 1                 TO WS-TY-WAITING(TX)
              WHEN WS-STEP-REJECTED
                 ADD 1                 TO WS-TY-REJECTED(TX)
           END-EVALUATE
           PERFORM 7300-WRITE-DETAIL
           PERFORM 7100-COUNT-COMMIT.

      ******************************************************************
      * One step = one savepoint unit
       2300-RUN-STEP.
           SET WS-STEP-DONE            TO TRUE
           EXEC SQL
              SAVEPOINT EVTUNIT ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-RUN-STEP'     TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EVALUATE WS-EV-CODE
              WHEN 'REMIND  '
                 IF WS-EV-DOC-TYPE = SPACES
                    MOVE 'DLQREMND'    TO WS-EV-DOC-TYPE
                 END-IF
                 PERFORM 2400-REQUEST-DOCUMENT
              WHEN 'LATEFEE '
                 PERFORM 2500-RAISE-LATE-FEE
              WHEN 'NOI     '
                 IF WS-EV-DOC-TYPE = SPACES
                    MOVE 'DLQNOI  '    TO WS-EV-DOC-TYPE
                 END-IF
                 PERFORM 2400-REQUEST-DOCUMENT
              WHEN 'CANCREQ '
                 PERFORM 2600-REQUEST-CANCELLATION
              WHEN 'WRITEOFF'
                 PERFORM 2700-WRITE-OFF-SWEEP
              WHEN OTHER
                 SET WS-STEP-SKIPPED   TO TRUE
                 ADD 1                 TO WS-T-UNKNOWN
                 STRING 'SKIPPED - UNKNOWN EVENT CODE ' WS-EV-CODE
                        DELIMITED BY SIZE INTO WS-RESULT-TEXT
           END-EVALUATE
           IF WS-STEP-DONE AND NOT WS-DP-STOPPED
              PERFORM 4400-COMPLETE-EVENT
           END-IF
      * BR-BIL-DEV-008 Failed step rolled back, later steps held
           IF WS-STEP-REJECTED
              EXEC SQL ROLLBACK TO SAVEPOINT EVTUNIT END-EXEC
              SET WS-DP-HELD           TO TRUE
           END-IF
           EXEC SQL RELEASE SAVEPOINT EVTUNIT END-EXEC.

      ******************************************************************
      * REMIND / NOI - document request against the process
       2400-REQUEST-DOCUMENT.
           MOVE 'DOC     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           IF WS-STEP-REJECTED
              EXIT PARAGRAPH
           END-IF
           MOVE SQC-SQ-VALUE           TO DOC-DOC-ID
           MOVE WS-EV-ACCOUNT-ID       TO DOC-ACCOUNT-ID
           MOVE WS-EV-DOC-TYPE         TO DOC-DOC-TYPE
           MOVE 'DLP '                 TO DOC-REF-TYPE
           MOVE WS-EV-DP-ID            TO DOC-REF-ID
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
              MOVE WS-EV-DP-ID         TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '2400-REQUEST-DOCUMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE DOC-DOC-ID             TO WS-ID-DISP
           STRING 'DOC ' WS-ID-DISP DELIMITED BY SIZE
                  INTO WS-RESULT-REF
           STRING 'DOCUMENT ' WS-EV-DOC-TYPE ' REQUESTED, ID '
                  WS-ID-DISP DELIMITED BY SIZE INTO WS-RESULT-TEXT.

      ******************************************************************
      * BR-BIL-DEV-004 Late fee as a one-time DELINQ charge
       2500-RAISE-LATE-FEE.
           IF DPL-LATE-FEE NOT > 0
              MOVE 'NOFEE'             TO WS-RESULT-REF
              MOVE 'PLAN HAS NO LATE FEE - STEP COMPLETED'
                                       TO WS-RESULT-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE 'CHG     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           IF WS-STEP-REJECTED
              EXIT PARAGRAPH
           END-IF
           MOVE SQC-SQ-VALUE           TO CHG-CHARGE-ID
           MOVE WS-EV-PP-ID            TO CHG-PP-ID
           MOVE WS-EV-ACCOUNT-ID       TO CHG-ACCOUNT-ID
           MOVE 'LATEFEE '             TO CHG-CHARGE-PATTERN
           MOVE 'DELINQ  '             TO CHG-BILLING-INSTR
           MOVE DPL-LATE-FEE           TO CHG-CHARGE-AMOUNT
           MOVE WS-BUS-DATE            TO CHG-CHARGE-DATE
                                          CHG-EFF-DATE
           MOVE 'OPEN'                 TO CHG-CHARGE-STATUS
           MOVE WS-EV-DP-ID            TO WS-ID-DISP
           MOVE WS-EV-STEP             TO WS-STEP-DISP
           STRING 'DLP' WS-ID-DISP '-' WS-STEP-DISP
                  DELIMITED BY SIZE INTO CHG-SOURCE-REF
           MOVE WS-BATCH-USER          TO CHG-CREATE-USER
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
           IF SQLCODE NOT = 0
              MOVE CHG-SOURCE-REF      TO WS-ERR-KEY
              MOVE '2500-RAISE-LATE-FEE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 8100-SCHEDULE-CHARGE
           IF WS-STEP-REJECTED
              EXIT PARAGRAPH
           END-IF
           PERFORM 8200-POST-LATE-FEE
           IF WS-STEP-REJECTED
              EXIT PARAGRAPH
           END-IF
           MOVE CHG-CHARGE-AMOUNT      TO WS-STEP-AMOUNT
           MOVE CHG-CHARGE-ID          TO WS-ID-DISP
           STRING 'CHG ' WS-ID-DISP DELIMITED BY SIZE
                  INTO WS-RESULT-REF
           MOVE CHG-CHARGE-AMOUNT      TO WS-AMT-DISP
           STRING 'LATE FEE ' WS-AMT-DISP ' CHARGE ' WS-ID-DISP
                  ' DUE ' SCC-SC-FIRST-DUE-DATE
                  DELIMITED BY SIZE INTO WS-RESULT-TEXT.

      ******************************************************************
      * BR-BIL-DEV-006 CANCREQ for NONPAY as of the business date
       2600-REQUEST-CANCELLATION.
           MOVE PPR-GENAPP-POL-NUM     TO WS-CR-POLICY-NO
           MOVE WS-BUS-DATE            TO WS-CR-CANCEL-DATE
           IF WS-PASTDUE < 0
              COMPUTE WS-CR-PASTDUE = 0 - WS-PASTDUE
              MOVE '-'                 TO WS-CR-PASTDUE-SIGN
           ELSE
              MOVE WS-PASTDUE          TO WS-CR-PASTDUE
              MOVE '+'                 TO WS-CR-PASTDUE-SIGN
           END-IF
           INITIALIZE WS-EVT-CA
           MOVE 'EVPUT   '             TO EVC-REQUEST-ID
           MOVE WS-BATCH-USER          TO EVC-USER-ID
           MOVE WS-BUS-DATE            TO EVC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO EVC-CALLER-PGM
           MOVE 'CANCREQ '             TO EVC-EV-TYPE
           MOVE 'O'                    TO EVC-EV-DIRECTION
           MOVE 'BILL'                 TO EVC-EV-SOURCE
           MOVE 'GENA'                 TO EVC-EV-TARGET
           MOVE WS-CR-POLICY-NO        TO EVC-EV-KEY
           MOVE WS-CANCREQ-PAYLOAD     TO EVC-EV-PAYLOAD
           CALL WS-EVT-PGM USING WS-EVT-CA
           IF EVC-RETURN-CODE >= 90
              MOVE EVC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF EVC-RETURN-CODE >= 12
              SET WS-STEP-REJECTED     TO TRUE
              STRING 'REJECTED ' EVC-REASON-CODE ' ' EVC-MESSAGE
                     DELIMITED BY SIZE INTO WS-RESULT-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE WS-PASTDUE             TO WS-STEP-AMOUNT
           MOVE EVC-EV-EVENT-ID        TO WS-ID-DISP
           STRING 'EVT ' WS-ID-DISP DELIMITED BY SIZE
                  INTO WS-RESULT-REF
           STRING 'CANCREQ TO GENA FOR POLICY ' WS-CR-POLICY-NO
                  ' EVENT ' WS-ID-DISP
                  DELIMITED BY SIZE INTO WS-RESULT-TEXT.

      ******************************************************************
      * Small-balance write-off sweep for the account
       2700-WRITE-OFF-SWEEP.
           INITIALIZE WS-WOF-CA
           MOVE 'WOFCRT  '             TO WOC-REQUEST-ID
           MOVE WS-BATCH-USER          TO WOC-USER-ID
           MOVE WS-BUS-DATE            TO WOC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO WOC-CALLER-PGM
           MOVE WS-EV-ACCOUNT-ID       TO WOC-WO-ACCOUNT-ID
           MOVE 0                      TO WOC-WO-ITEM-ID
           MOVE 'DELINQ  '             TO WOC-WO-REASON
           CALL WS-WOF-PGM USING WS-WOF-CA
           IF WOC-RETURN-CODE >= 90
              MOVE WOC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF WOC-RETURN-CODE >= 12
              SET WS-STEP-REJECTED     TO TRUE
              STRING 'REJECTED ' WOC-REASON-CODE ' ' WOC-MESSAGE
                     DELIMITED BY SIZE INTO WS-RESULT-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE WOC-WO-TOTAL-WRITTEN   TO WS-STEP-AMOUNT
           MOVE WOC-WO-ITEMS-WRITTEN   TO WS-STEP-DISP
           STRING 'WOF ' WS-STEP-DISP ' ITEMS'
                  DELIMITED BY SIZE INTO WS-RESULT-REF
           MOVE WOC-WO-TOTAL-WRITTEN   TO WS-AMT-DISP
           STRING 'WRITE-OFF SWEEP ' WS-STEP-DISP ' ITEMS '
                  WS-AMT-DISP DELIMITED BY SIZE INTO WS-RESULT-TEXT
      * BR-BIL-DEV-007 A cleared process exits WRITEOFF
           PERFORM 4200-CALC-PASTDUE
           IF WS-PASTDUE NOT > WS-THRESHOLD
              PERFORM 4400-COMPLETE-EVENT
              MOVE 'WRITEOFF'          TO WS-EXIT-REASON
              PERFORM 8600-EXIT-DELINQUENCY
              IF DQC-RETURN-CODE >= 12
                 SET WS-STEP-REJECTED  TO TRUE
                 STRING 'REJECTED ' DQC-REASON-CODE ' ' DQC-MESSAGE
                        DELIMITED BY SIZE INTO WS-RESULT-TEXT
              ELSE
                 SET WS-DP-STOPPED     TO TRUE
                 ADD 1                 TO WS-T-PROCS-EXITED
                 MOVE 'WRITE-OFF CLEARED PAST DUE - PROCESS EXITED'
                                       TO WS-RESULT-TEXT
              END-IF
           END-IF.

      ******************************************************************
       4000-FETCH-EVENT.
           EXEC SQL
              FETCH C-DUE-EVENTS
               INTO :WS-EV-DP-ID, :WS-EV-STEP, :WS-EV-CODE,
                    :WS-EV-TARGET, :WS-EV-ACCOUNT-ID, :WS-EV-PP-ID,
                    :WS-EV-PLAN-CODE, :WS-EV-DOC-TYPE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-END-OF-EVENTS  TO TRUE
              WHEN OTHER
                 MOVE '4000-FETCH-EVENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Account, policy period and plan of the current process
       4100-READ-CONTEXT.
           MOVE WS-EV-DP-ID            TO WS-ID-DISP
           MOVE WS-ID-DISP             TO WS-ERR-KEY
           MOVE WS-EV-ACCOUNT-ID       TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_NUMBER, DELIVERY_METHOD
                INTO :ACT-ACCOUNT-NUMBER, :ACT-DELIVERY-METHOD
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-READ-CONTEXT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-EV-PP-ID            TO PPR-PP-ID
           EXEC SQL
              SELECT GENAPP_POL_NUM, PP_STATUS
                INTO :PPR-GENAPP-POL-NUM, :PPR-PP-STATUS
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE PP_ID = :PPR-PP-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-READ-CONTEXT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-EV-PLAN-CODE        TO DPL-PLAN-CODE
           EXEC SQL
              SELECT LATE_FEE, PASTDUE_THRESHOLD
                INTO :DPL-LATE-FEE, :DPL-PASTDUE-THRESHOLD
                FROM BILDB.BIL_DELINQ_PLAN
               WHERE PLAN_CODE = :DPL-PLAN-CODE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-READ-CONTEXT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE DPL-PASTDUE-THRESHOLD  TO WS-THRESHOLD.

      ******************************************************************
      * Same measure as BILDLQSV: open DUE items due before today
       4200-CALC-PASTDUE.
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
              MOVE '4200-CALC-PASTDUE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-COUNT-EARLIER-PENDING.
           MOVE WS-EV-DP-ID            TO DEV-DP-ID
           MOVE WS-EV-STEP             TO DEV-STEP-SEQ
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-EARLIER-PEND
                FROM BILDB.BIL_DELINQ_EVENT
               WHERE DP_ID        = :DEV-DP-ID
                 AND STEP_SEQ     < :DEV-STEP-SEQ
                 AND EVENT_STATUS = 'PEND'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-COUNT-EARLIER-PENDING' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Mark the step DONE and advance the process current step
       4400-COMPLETE-EVENT.
           MOVE WS-EV-DP-ID            TO DEV-DP-ID
           MOVE WS-EV-STEP             TO DEV-STEP-SEQ
           MOVE WS-BUS-DATE            TO DEV-COMPLETED-DATE
           MOVE WS-RESULT-REF          TO DEV-RESULT-REF
           EXEC SQL
              UPDATE BILDB.BIL_DELINQ_EVENT
                 SET EVENT_STATUS   = 'DONE',
                     COMPLETED_DATE = :DEV-COMPLETED-DATE,
                     RESULT_REF     = :DEV-RESULT-REF
               WHERE DP_ID        = :DEV-DP-ID
                 AND STEP_SEQ     = :DEV-STEP-SEQ
                 AND EVENT_STATUS = 'PEND'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-COMPLETE-EVENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET WS-STEP-REJECTED     TO TRUE
              MOVE 'REJECTED - STEP NO LONGER PENDING'
                                       TO WS-RESULT-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE WS-EV-DP-ID            TO DLP-DP-ID
           MOVE WS-EV-STEP             TO DLP-CURRENT-STEP
           EXEC SQL
              UPDATE BILDB.BIL_DELINQ_PROCESS
                 SET CURRENT_STEP = :DLP-CURRENT-STEP,
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE DP_ID     = :DLP-DP-ID
                 AND DP_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4400-COMPLETE-EVENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       5000-FIND-TYPE.
           SET TX                      TO 1
           SEARCH WS-TYPE
              AT END
                 SET TX                TO 6
              WHEN WS-TY-CODE(TX) = WS-EV-CODE
                 CONTINUE
           END-SEARCH.

      ******************************************************************
       7000-NEW-PAGE.
           ADD 1                       TO WS-PAGE
           MOVE WS-PAGE                TO RH-PAGE
           WRITE RPT-RECORD FROM RH-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           WRITE RPT-RECORD FROM R3-COL-HEAD AFTER ADVANCING 2 LINES
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           MOVE 5                      TO WS-LINES.

      ******************************************************************
      * Each executed step or exit counts toward the commit frequency
       7100-COUNT-COMMIT.
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
              EXEC SQL COMMIT END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '7100-COUNT-COMMIT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              ADD 1                    TO WS-T-COMMITS
              MOVE 0                   TO WS-SINCE-COMMIT
              SET CK-RUNNING           TO TRUE
              PERFORM 7110-WRITE-CHECKPOINT
           END-IF.

      ******************************************************************
       7110-WRITE-CHECKPOINT.
           EXEC SQL
              SET :WS-CURRENT-TS = CURRENT TIMESTAMP
           END-EXEC
           MOVE WS-PROGRAM-ID          TO CK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CK-BUS-DATE
           MOVE WS-EV-DP-ID            TO CK-LAST-KEY-1
           MOVE WS-EV-STEP             TO CK-LAST-KEY-2
           MOVE WS-T-EVENTS-READ       TO CK-UNITS-DONE
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
       7300-WRITE-DETAIL.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           MOVE ACT-ACCOUNT-NUMBER     TO R3-ACCOUNT-NUMBER
           MOVE WS-EV-DP-ID            TO R3-DP-ID
           MOVE WS-EV-STEP             TO R3-STEP
           MOVE WS-EV-CODE             TO R3-EVENT
           MOVE WS-EV-TARGET           TO R3-TARGET
           MOVE WS-PASTDUE             TO R3-PASTDUE
           MOVE WS-RESULT-TEXT         TO R3-RESULT
           WRITE RPT-RECORD FROM R3-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES.

      ******************************************************************
       7400-WRITE-TOTALS.
           MOVE 99                     TO WS-LINES
           PERFORM 7200-WRITE-TEXT
           WRITE RPT-RECORD FROM R3-TYPE-HEAD AFTER ADVANCING 2 LINES
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 6
              MOVE WS-TY-CODE(TX)      TO R3-T-EVENT
              MOVE WS-TY-DONE(TX)      TO R3-T-DONE
              MOVE WS-TY-SKIPPED(TX)   TO R3-T-SKIPPED
              MOVE WS-TY-WAITING(TX)   TO R3-T-WAITING
              MOVE WS-TY-REJECTED(TX)  TO R3-T-REJECTED
              MOVE WS-TY-AMOUNT(TX)    TO R3-T-AMOUNT
              WRITE RPT-RECORD FROM R3-TYPE-LINE
                    AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 2 LINES
           MOVE 'EVENTS READ'          TO RH-TOT-LABEL
           MOVE WS-T-EVENTS-READ       TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           WRITE RPT-RECORD FROM RH-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'PROCESSES EXITED (PAID / WRITEOFF)' TO RH-TOT-LABEL
           MOVE WS-T-PROCS-EXITED      TO RH-TOT-COUNT
           WRITE RPT-RECORD FROM RH-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'COMMITS (INCLUDING PRIOR RUNS TODAY)' TO RH-TOT-LABEL
           MOVE WS-T-COMMITS           TO RH-TOT-COUNT
           WRITE RPT-RECORD FROM RH-TOTAL-LINE AFTER ADVANCING 1 LINE.

      ******************************************************************
      * Service calls: RC >= 90 abends, RC 12..24 rejects the step
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
              SET WS-STEP-REJECTED     TO TRUE
              STRING 'REJECTED ' SQC-REASON-CODE ' ' SQC-MESSAGE
                     DELIMITED BY SIZE INTO WS-RESULT-TEXT
           END-IF.

      ******************************************************************
       8100-SCHEDULE-CHARGE.
           INITIALIZE WS-SCH-CA
           MOVE 'SCHCHG  '             TO SCC-REQUEST-ID
           MOVE WS-BATCH-USER          TO SCC-USER-ID
           MOVE WS-BUS-DATE            TO SCC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SCC-CALLER-PGM
           MOVE CHG-CHARGE-ID          TO SCC-SC-CHARGE-ID
           MOVE WS-EV-PP-ID            TO SCC-SC-PP-ID
           SET SCC-SC-IS-ONE-TIME      TO TRUE
           CALL WS-SCH-PGM USING WS-SCH-CA
           IF SCC-RETURN-CODE >= 90
              MOVE SCC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF SCC-RETURN-CODE >= 12
              SET WS-STEP-REJECTED     TO TRUE
              STRING 'REJECTED ' SCC-REASON-CODE ' ' SCC-MESSAGE
                     DELIMITED BY SIZE INTO WS-RESULT-TEXT
           END-IF.

      ******************************************************************
      * BR-BIL-DEV-005 Late fee: DR UNBILLED / CR FEEREV
       8200-POST-LATE-FEE.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLC-REQUEST-ID
           MOVE WS-BATCH-USER          TO GLC-USER-ID
           MOVE WS-BUS-DATE            TO GLC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO GLC-CALLER-PGM
           MOVE 'LATEFEE '             TO GLC-GL-TXN-TYPE
           MOVE WS-EV-ACCOUNT-ID       TO GLC-GL-ACCOUNT-ID
           MOVE 'CHG '                 TO GLC-GL-REF-TYPE
           MOVE CHG-CHARGE-ID          TO GLC-GL-REF-ID
           MOVE 2                      TO GLC-GL-LINE-COUNT
           MOVE 'UNBILLED'             TO GLC-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLC-GL-DR-CR(1)
           MOVE 'FEEREV  '             TO GLC-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLC-GL-DR-CR(2)
           MOVE CHG-CHARGE-AMOUNT      TO WS-GL-AMOUNT
           MOVE WS-GL-AMOUNT           TO GLC-GL-AMOUNT(1)
                                          GLC-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLC-RETURN-CODE >= 90
              MOVE GLC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF GLC-RETURN-CODE >= 12
              SET WS-STEP-REJECTED     TO TRUE
              STRING 'REJECTED ' GLC-REASON-CODE ' ' GLC-MESSAGE
                     DELIMITED BY SIZE INTO WS-RESULT-TEXT
           END-IF.

      ******************************************************************
       8600-EXIT-DELINQUENCY.
           INITIALIZE WS-DLQ-CA
           MOVE 'DLQEXIT '             TO DQC-REQUEST-ID
           MOVE WS-BATCH-USER          TO DQC-USER-ID
           MOVE WS-BUS-DATE            TO DQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DQC-CALLER-PGM
           MOVE WS-EV-ACCOUNT-ID       TO DQC-DQ-ACCOUNT-ID
           MOVE WS-EV-PP-ID            TO DQC-DQ-PP-ID
           MOVE WS-EXIT-REASON         TO DQC-DQ-EXIT-REASON
           CALL WS-DLQ-PGM USING WS-DLQ-CA
           IF DQC-RETURN-CODE >= 90
              STRING 'BILDLQSV ' DQC-REASON-CODE ' ' DQC-MESSAGE
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF DQC-RETURN-CODE >= 12
              MOVE SPACES              TO RH-TEXT
              MOVE WS-EV-DP-ID         TO WS-ID-DISP
              STRING 'PROCESS ' WS-ID-DISP ' EXIT FAILED '
                     DQC-REASON-CODE ' ' DQC-MESSAGE
                     DELIMITED BY SIZE INTO RH-TEXT
              PERFORM 7200-WRITE-TEXT
              IF WS-MAX-RC < 8
                 MOVE 8                TO WS-MAX-RC
              END-IF
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
              PERFORM 7400-WRITE-TOTALS
           END-IF
           IF WS-T-UNKNOWN > 0 AND WS-MAX-RC < 4
              MOVE 4                   TO WS-MAX-RC
           END-IF
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 6
              IF WS-TY-REJECTED(TX) > 0
                 MOVE 8                TO WS-MAX-RC
              END-IF
           END-PERFORM
           MOVE WS-MAX-RC              TO WS-CNT-DISP
           STRING 'BILDLQEV ENDED - RETURN CODE ' WS-CNT-DISP
                  DELIMITED BY SIZE INTO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           CLOSE REPORT-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE C-DUE-EVENTS END-EXEC
              SET WS-CURSOR-CLOSED     TO TRUE
           END-IF
           DISPLAY 'BILDLQEV ABEND: ' WS-ABEND-TEXT
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
