       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILEODDR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILEODDR                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : End-of-day driver, run twice by job BILEOD:      *
      *               MODE=BEGIN  first step: BATCH_STATUS must be O   *
      *                           (online day), set B (batch window).  *
      *               MODE=END    last step: read the step-status file *
      *                           written by the job stream, record    *
      *                           each step result in the audit trail, *
      *                           stop at the first step with RC >= 8, *
      *                           otherwise roll BUSINESS_DATE to the  *
      *                           next business day (BILDATES DTBUSD), *
      *                           set PRIOR_BUS_DATE, LAST_EOD_TS and  *
      *                           BATCH_STATUS O.                      *
      * REQUESTS    : n/a (batch); control card MODE=BEGIN | MODE=END  *
      * CALLED BY   : JCL BILEOD steps EODBEGIN and EODEND             *
      * CALLS       : BILDATES (DTBUSD), BILAUDIT (AUDADD), BILERRLG   *
      * TABLES      : BIL_CONTROL (R U)                                *
      * FILES       : CTLCARD  control card      (I)                   *
      *               STEPSTAT step results      (I)                   *
      *               EODRPT   report 132        (O)                   *
      *               CHKPT    checkpoint        (I/O)                 *
      * RULES       : BR-BIL-EOD-001 EOD may only start from status O; *
      *                              B is set by one guarded UPDATE    *
      *               BR-BIL-EOD-002 Restart of BEGIN for the same     *
      *                              business date is accepted (RC 4)  *
      *               BR-BIL-EOD-003 Every scheduled step must report; *
      *                              the latest record for a step wins *
      *               BR-BIL-EOD-004 Stop at the first step with RC 8  *
      *                              or more (99 = abend) or missing;  *
      *                              the date is not rolled and status *
      *                              stays B for operations            *
      *               BR-BIL-EOD-005 Next business date = one business *
      *                              day after the current date, over  *
      *                              weekends and holidays (DTBUSD 1)  *
      *               BR-BIL-EOD-006 Date roll is one UPDATE guarded   *
      *                              by status B and the old date so   *
      *                              a rerun can never roll twice      *
      *               BR-BIL-EOD-007 Status and date changes are       *
      *                              audited                           *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTLCARD-FILE ASSIGN TO CTLCARD
                  FILE STATUS IS WS-CTC-STATUS.
           SELECT STEPSTAT-FILE ASSIGN TO STEPSTAT
                  FILE STATUS IS WS-STS-STATUS.
           SELECT REPORT-FILE  ASSIGN TO EODRPT
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT CHKPT-FILE   ASSIGN TO CHKPT
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CTLCARD-FILE
           RECORDING MODE IS F.
           COPY BILEODC1.
       FD  STEPSTAT-FILE
           RECORDING MODE IS F.
           COPY BILEODS1.
       FD  REPORT-FILE
           RECORDING MODE IS F.
           COPY BILEODP1.
       FD  CHKPT-FILE
           RECORDING MODE IS F.
           COPY BILEODK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILEODDR'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-COMMIT-FREQ              PIC S9(4) COMP VALUE 200.

       01  WS-FILE-STATUS.
           05 WS-CTC-STATUS            PIC XX.
           05 WS-STS-STATUS            PIC XX.
              88 STS-EOF                VALUE '10'.
           05 WS-RPT-STATUS            PIC XX.
           05 WS-CHK-STATUS            PIC XX.

       01  WS-RUN.
           05 WS-RC                    PIC S9(4) COMP VALUE 0.
           05 WS-MODE                  PIC X(5).
           05 WS-BUS-DATE              PIC X(10).
           05 WS-NEXT-DATE             PIC X(10).
           05 WS-RUN-TS                PIC X(21).
           05 WS-STATUS-RECORDS        PIC S9(4) COMP VALUE 0.
           05 WS-UNKNOWN-STEPS         PIC S9(4) COMP VALUE 0.
           05 WS-AUDITS-SINCE-COMMIT   PIC S9(4) COMP VALUE 0.
           05 WS-HIGHEST-RC            PIC 99    VALUE 0.
           05 WS-STOP-STEP             PIC X(8)  VALUE SPACES.
           05 WS-PRIOR-CHK-STATUS      PIC X     VALUE SPACE.
           05 WS-PRIOR-CHK-KEY         PIC X(20) VALUE SPACES.
           05 WS-PRIOR-CHK-DATE        PIC X(10) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-SQLCODE           PIC S9(9) COMP VALUE 0.
           05 WS-LINE-CNT              PIC S9(4) COMP VALUE 0.

      * Nightly schedule, in job-stream order (BILEOD.jcl)
       01  WS-SCHED-VALUES.
           05 FILLER PIC X(8) VALUE 'BILGAPIN'.
           05 FILLER PIC X(8) VALUE 'BILEVTDR'.
           05 FILLER PIC X(8) VALUE 'BILINVB0'.
           05 FILLER PIC X(8) VALUE 'BILINVDU'.
           05 FILLER PIC X(8) VALUE 'BILDLQEV'.
           05 FILLER PIC X(8) VALUE 'BILACHDR'.
           05 FILLER PIC X(8) VALUE 'BILLKBX0'.
           05 FILLER PIC X(8) VALUE 'BILACHRT'.
           05 FILLER PIC X(8) VALUE 'BILDSBOV'.
           05 FILLER PIC X(8) VALUE 'BILCOMPY'.
           05 FILLER PIC X(8) VALUE 'BILAGSTM'.
           05 FILLER PIC X(8) VALUE 'BILDSBSN'.
           05 FILLER PIC X(8) VALUE 'BILDOCGN'.
           05 FILLER PIC X(8) VALUE 'BILAGERP'.
           05 FILLER PIC X(8) VALUE 'BILRECON'.
           05 FILLER PIC X(8) VALUE 'BILGLEXT'.
       01  WS-SCHED REDEFINES WS-SCHED-VALUES.
           05 WS-SCHED-STEP OCCURS 16 TIMES INDEXED BY SCH-IX
                                       PIC X(8).
       01  WS-SCHED-COUNT              PIC S9(4) COMP VALUE 16.
       01  WS-RESULTS.
           05 WS-RES-ENTRY OCCURS 16 TIMES.
              10 WS-RES-RC             PIC XX.
              10 WS-RES-SEEN           PIC X.
       01  WS-STEP-IX                  PIC S9(4) COMP.
       01  WS-STEP-RC-NUM              PIC 99.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       01  RPT-HEAD-1.
           05 FILLER                   PIC X(10) VALUE 'BILEODDR'.
           05 FILLER                   PIC X(20) VALUE SPACES.
           05 FILLER                   PIC X(46) VALUE
              'BILLINGCORE  END-OF-DAY CONTROL'.
           05 FILLER                   PIC X(15) VALUE
              'BUSINESS DATE '.
           05 RH1-BUS-DATE             PIC X(10).
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(6)  VALUE 'MODE '.
           05 RH1-MODE                 PIC X(5).
       01  RPT-HEAD-2.
           05 FILLER                   PIC X(10) VALUE 'RUN'.
           05 RH2-RUN-TS               PIC X(21).
       01  RPT-STEP-HEAD.
           05 FILLER                   PIC X(50) VALUE
              'SEQ  STEP      RC  RESULT'.
       01  RPT-STEP-LINE.
           05 RSL-SEQ                  PIC Z9.
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 RSL-STEP                 PIC X(8).
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RSL-RC                   PIC XX.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RSL-RESULT               PIC X(40).
       01  RPT-MSG-LINE.
           05 FILLER                   PIC X(4)  VALUE '*** '.
           05 RMS-TEXT                 PIC X(100).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE WS-MODE
              WHEN 'BEGIN'
                 PERFORM 2000-BEGIN-EOD
              WHEN 'END  '
                 PERFORM 2200-END-EOD
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE  TO WS-RUN-TS
           MOVE SPACES                 TO RMS-TEXT
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILEODDR EODRPT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3000-READ-CONTROL-CARD
           PERFORM 4000-READ-CONTROL
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE
           PERFORM 6000-READ-CHECKPOINT
           MOVE WS-BUS-DATE            TO RH1-BUS-DATE
           MOVE WS-MODE                TO RH1-MODE
           MOVE WS-RUN-TS              TO RH2-RUN-TS
           WRITE PRT-RECORD FROM RPT-HEAD-1 AFTER ADVANCING PAGE
           WRITE PRT-RECORD FROM RPT-HEAD-2 AFTER ADVANCING 1 LINE
           MOVE SPACES                 TO PRT-RECORD
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE.

      ******************************************************************
      * BR-BIL-EOD-001 Only an online day (O) can enter the window
       2000-BEGIN-EOD.
           EVALUATE CTL-BATCH-STATUS
              WHEN 'O'
                 PERFORM 4100-OPEN-WINDOW
                 MOVE 'STAT'           TO AUA-AU-ACTION
                 MOVE 'BATCH_STATUS'   TO AUA-AU-FIELD
                 MOVE 'O'              TO AUA-AU-BEFORE
                 MOVE 'B'              TO AUA-AU-AFTER
                 PERFORM 8100-AUDIT-CONTROL
                 MOVE 'END-OF-DAY WINDOW OPENED - STATUS O TO B'
                                       TO RMS-TEXT
      * BR-BIL-EOD-002 BEGIN rerun for the same date after a failure
              WHEN 'B'
                 IF WS-PRIOR-CHK-DATE = WS-BUS-DATE
                    AND WS-PRIOR-CHK-STATUS NOT = 'C'
                    MOVE 4             TO WS-RC
                    MOVE 'RESTART - WINDOW ALREADY OPEN FOR THIS DATE'
                                       TO RMS-TEXT
                 ELSE
                    MOVE 12            TO WS-RC
                    MOVE 'BATCH_STATUS B BUT NO EOD IN FLIGHT FOR DATE'
                                       TO RMS-TEXT
                 END-IF
              WHEN OTHER
                 MOVE 12               TO WS-RC
                 STRING 'BATCH_STATUS ' CTL-BATCH-STATUS
                        ' - END OF DAY CANNOT START'
                        DELIMITED BY SIZE INTO RMS-TEXT
           END-EVALUATE
           PERFORM 7100-PRINT-MESSAGE
           IF WS-RC < 12
              MOVE 'BEGIN-DONE'        TO CHK-RESTART-KEY
              SET CHK-RUNNING          TO TRUE
              PERFORM 6100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
       2200-END-EOD.
           IF CTL-BATCH-STATUS NOT = 'B'
              IF CTL-BATCH-STATUS = 'O'
                 AND WS-PRIOR-CHK-STATUS = 'C'
                 AND WS-PRIOR-CHK-KEY(1:6) = 'ROLLED'
                 MOVE 4                TO WS-RC
                 STRING 'END ALREADY COMPLETE - BUSINESS DATE IS NOW '
                        WS-BUS-DATE DELIMITED BY SIZE INTO RMS-TEXT
              ELSE
                 MOVE 12               TO WS-RC
                 STRING 'BATCH_STATUS ' CTL-BATCH-STATUS
                        ' - NO END-OF-DAY WINDOW IS OPEN'
                        DELIMITED BY SIZE INTO RMS-TEXT
              END-IF
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 2300-LOAD-STEP-STATUS
           PERFORM 2400-CHECK-STEPS
           IF WS-STOP-STEP NOT = SPACES
              MOVE 12                  TO WS-RC
              MOVE SPACES              TO RMS-TEXT
              STRING 'END OF DAY HALTED AT ' WS-STOP-STEP
                     ' - DATE NOT ROLLED, STATUS LEFT B'
                     DELIMITED BY SIZE INTO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              MOVE WS-STOP-STEP        TO CHK-RESTART-KEY
              SET CHK-FAILED           TO TRUE
              PERFORM 6100-COMMIT-CHECKPOINT
           ELSE
              PERFORM 2600-ROLL-DATE
           END-IF.

      ******************************************************************
      * BR-BIL-EOD-003 Latest record for a step wins (restart appends)
       2300-LOAD-STEP-STATUS.
           INITIALIZE WS-RESULTS
           OPEN INPUT STEPSTAT-FILE
           IF WS-STS-STATUS NOT = '00'
              MOVE 12                  TO WS-RC
              STRING 'STEPSTAT OPEN FAILED, FILE STATUS '
                     WS-STS-STATUS DELIMITED BY SIZE INTO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           READ STEPSTAT-FILE
           PERFORM UNTIL STS-EOF
              ADD 1                    TO WS-STATUS-RECORDS
              SET SCH-IX               TO 1
              SEARCH WS-SCHED-STEP
                 AT END
                    ADD 1              TO WS-UNKNOWN-STEPS
                    MOVE SPACES        TO RMS-TEXT
                    STRING 'IGNORED STATUS FOR UNSCHEDULED STEP '
                           STS-STEP-NAME DELIMITED BY SIZE
                           INTO RMS-TEXT
                    PERFORM 7100-PRINT-MESSAGE
                 WHEN WS-SCHED-STEP(SCH-IX) = STS-STEP-NAME
                    SET WS-STEP-IX     TO SCH-IX
                    MOVE STS-STEP-RC   TO WS-RES-RC(WS-STEP-IX)
                    MOVE 'Y'           TO WS-RES-SEEN(WS-STEP-IX)
              END-SEARCH
              READ STEPSTAT-FILE
           END-PERFORM
           CLOSE STEPSTAT-FILE.

      ******************************************************************
      * BR-BIL-EOD-004 Stop at the first failed or missing step
       2400-CHECK-STEPS.
           MOVE SPACES                 TO PRT-RECORD
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE
           WRITE PRT-RECORD FROM RPT-STEP-HEAD AFTER ADVANCING 1 LINE
           PERFORM VARYING WS-STEP-IX FROM 1 BY 1
                   UNTIL WS-STEP-IX > WS-SCHED-COUNT
              MOVE WS-STEP-IX          TO RSL-SEQ
              MOVE WS-SCHED-STEP(WS-STEP-IX) TO RSL-STEP
              MOVE WS-RES-RC(WS-STEP-IX) TO RSL-RC
              EVALUATE TRUE
                 WHEN WS-STOP-STEP NOT = SPACES
                    MOVE SPACES        TO RSL-RC
                    MOVE 'NOT REACHED' TO RSL-RESULT
                 WHEN WS-RES-SEEN(WS-STEP-IX) NOT = 'Y'
                    MOVE '--'          TO RSL-RC
                    MOVE 'NO STATUS - STEP DID NOT RUN' TO RSL-RESULT
                    MOVE WS-SCHED-STEP(WS-STEP-IX) TO WS-STOP-STEP
                 WHEN WS-RES-RC(WS-STEP-IX) IS NOT NUMERIC
                    MOVE 'INVALID RC IN STATUS FILE' TO RSL-RESULT
                    MOVE WS-SCHED-STEP(WS-STEP-IX) TO WS-STOP-STEP
                 WHEN OTHER
                    MOVE WS-RES-RC(WS-STEP-IX) TO WS-STEP-RC-NUM
                    PERFORM 5000-CLASSIFY-STEP
                    PERFORM 8200-AUDIT-STEP
              END-EVALUATE
              WRITE PRT-RECORD FROM RPT-STEP-LINE
                    AFTER ADVANCING 1 LINE
           END-PERFORM
           MOVE SPACES                 TO PRT-RECORD
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE.

      ******************************************************************
      * BR-BIL-EOD-005 Next business day through BILDATES
      * BR-BIL-EOD-006 Guarded single UPDATE
       2600-ROLL-DATE.
           MOVE 'DTBUSD  '             TO DTA-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-1
           MOVE 1                      TO DTA-DT-NUMBER
           PERFORM 8000-CALL-BILDATES
           MOVE DTA-DT-RESULT-DATE     TO WS-NEXT-DATE
           IF WS-NEXT-DATE NOT > WS-BUS-DATE
              MOVE 16                  TO WS-RC
              STRING 'NEXT BUSINESS DATE ' WS-NEXT-DATE
                     ' IS NOT AFTER ' WS-BUS-DATE
                     DELIMITED BY SIZE INTO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4200-ROLL-CONTROL
           MOVE 'UPD '                 TO AUA-AU-ACTION
           MOVE 'BUSINESS_DATE'        TO AUA-AU-FIELD
           MOVE WS-BUS-DATE            TO AUA-AU-BEFORE
           MOVE WS-NEXT-DATE           TO AUA-AU-AFTER
           PERFORM 8100-AUDIT-CONTROL
           MOVE 'STAT'                 TO AUA-AU-ACTION
           MOVE 'BATCH_STATUS'         TO AUA-AU-FIELD
           MOVE 'B'                    TO AUA-AU-BEFORE
           MOVE 'O'                    TO AUA-AU-AFTER
           PERFORM 8100-AUDIT-CONTROL
           IF WS-HIGHEST-RC > 0 OR WS-UNKNOWN-STEPS > 0
              MOVE 4                   TO WS-RC
           END-IF
           MOVE SPACES                 TO RMS-TEXT
           STRING 'BUSINESS DATE ROLLED ' WS-BUS-DATE ' TO '
                  WS-NEXT-DATE ' - ONLINE DAY OPEN (STATUS O)'
                  DELIMITED BY SIZE INTO RMS-TEXT
           PERFORM 7100-PRINT-MESSAGE
           MOVE SPACES                 TO CHK-RESTART-KEY
           STRING 'ROLLED ' WS-NEXT-DATE
                  DELIMITED BY SIZE INTO CHK-RESTART-KEY
           SET CHK-COMPLETE            TO TRUE
           PERFORM 6100-COMMIT-CHECKPOINT.

      ******************************************************************
       3000-READ-CONTROL-CARD.
           OPEN INPUT CTLCARD-FILE
           IF WS-CTC-STATUS NOT = '00'
              MOVE 12                  TO WS-RC
              MOVE 'CTLCARD DD MISSING OR UNREADABLE' TO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           READ CTLCARD-FILE
           IF WS-CTC-STATUS NOT = '00'
              OR NOT CTC-KEYWORD-OK
              OR NOT (CTC-MODE-BEGIN OR CTC-MODE-END)
              CLOSE CTLCARD-FILE
              MOVE 12                  TO WS-RC
              MOVE 'CONTROL CARD MUST BE MODE=BEGIN OR MODE=END'
                                       TO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CTC-MODE               TO WS-MODE
           CLOSE CTLCARD-FILE.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT CHAR(BUSINESS_DATE, ISO),
                     CHAR(PRIOR_BUS_DATE, ISO), BATCH_STATUS
                INTO :CTL-BUSINESS-DATE, :CTL-PRIOR-BUS-DATE,
                     :CTL-BATCH-STATUS
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4100-OPEN-WINDOW.
           EXEC SQL
              UPDATE BILDB.BIL_CONTROL
                 SET BATCH_STATUS = 'B',
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CONTROL_KEY  = :CTL-CONTROL-KEY
                 AND BATCH_STATUS = 'O'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-OPEN-WINDOW'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              MOVE 12                  TO WS-RC
              MOVE 'CONTROL ROW CHANGED BY ANOTHER PROCESS - RERUN'
                                       TO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4200-ROLL-CONTROL.
           MOVE WS-NEXT-DATE           TO CTL-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO CTL-PRIOR-BUS-DATE
           EXEC SQL
              UPDATE BILDB.BIL_CONTROL
                 SET PRIOR_BUS_DATE = BUSINESS_DATE,
                     BUSINESS_DATE  = DATE(:CTL-BUSINESS-DATE),
                     LAST_EOD_TS    = CURRENT TIMESTAMP,
                     BATCH_STATUS   = 'O',
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE CONTROL_KEY    = :CTL-CONTROL-KEY
                 AND BATCH_STATUS   = 'B'
                 AND BUSINESS_DATE  = DATE(:CTL-PRIOR-BUS-DATE)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-ROLL-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              EXEC SQL ROLLBACK END-EXEC
              MOVE 12                  TO WS-RC
              MOVE 'CONTROL ROW NO LONGER B FOR THIS DATE - NOT ROLLED'
                                       TO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       5000-CLASSIFY-STEP.
           IF WS-STEP-RC-NUM > WS-HIGHEST-RC
              MOVE WS-STEP-RC-NUM      TO WS-HIGHEST-RC
           END-IF
           EVALUATE TRUE
              WHEN WS-STEP-RC-NUM = 0
                 MOVE 'COMPLETED'      TO RSL-RESULT
              WHEN WS-STEP-RC-NUM < 8
                 MOVE 'COMPLETED WITH WARNINGS' TO RSL-RESULT
              WHEN WS-STEP-RC-NUM = 99
                 MOVE 'ABENDED'        TO RSL-RESULT
                 MOVE WS-SCHED-STEP(WS-STEP-IX) TO WS-STOP-STEP
              WHEN OTHER
                 MOVE 'FAILED'         TO RSL-RESULT
                 MOVE WS-SCHED-STEP(WS-STEP-IX) TO WS-STOP-STEP
           END-EVALUATE.

      ******************************************************************
       6000-READ-CHECKPOINT.
           OPEN INPUT CHKPT-FILE
           IF WS-CHK-STATUS = '00'
              READ CHKPT-FILE
              IF WS-CHK-STATUS = '00'
                 AND CHK-PROGRAM-ID = WS-PROGRAM-ID
                 MOVE CHK-RUN-STATUS   TO WS-PRIOR-CHK-STATUS
                 MOVE CHK-RESTART-KEY  TO WS-PRIOR-CHK-KEY
                 MOVE CHK-BUS-DATE     TO WS-PRIOR-CHK-DATE
              END-IF
              CLOSE CHKPT-FILE
           END-IF.

      ******************************************************************
       6100-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '6100-COMMIT-CHECKPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-AUDITS-SINCE-COMMIT
           MOVE WS-PROGRAM-ID          TO CHK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CHK-BUS-DATE
           MOVE WS-STATUS-RECORDS      TO CHK-ROWS-DONE
           MOVE FUNCTION CURRENT-DATE  TO CHK-TIMESTAMP
           OPEN OUTPUT CHKPT-FILE
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'BILEODDR CHKPT OPEN FAILED ' WS-CHK-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           WRITE CHK-RECORD
           CLOSE CHKPT-FILE.

      ******************************************************************
       7100-PRINT-MESSAGE.
           WRITE PRT-RECORD FROM RPT-MSG-LINE AFTER ADVANCING 1 LINE
           DISPLAY 'BILEODDR ' RMS-TEXT
           MOVE SPACES                 TO RMS-TEXT.

      ******************************************************************
       8000-CALL-BILDATES.
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID
                                          DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              MOVE 16                  TO WS-RC
              STRING 'BILDATES ' DTA-REQUEST-ID ' FAILED '
                     DTA-REASON-CODE ' ' DTA-MESSAGE
                     DELIMITED BY SIZE INTO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-EOD-007 Control row changes are audited
       8100-AUDIT-CONTROL.
           MOVE 'CONTROL '             TO AUA-AU-ENTITY
           MOVE CTL-CONTROL-KEY        TO AUA-AU-KEY
           PERFORM 8300-CALL-BILAUDIT.

      ******************************************************************
       8200-AUDIT-STEP.
           MOVE 'EODSTEP '             TO AUA-AU-ENTITY
           MOVE SPACES                 TO AUA-AU-KEY
           STRING WS-BUS-DATE ' ' WS-SCHED-STEP(WS-STEP-IX)
                  DELIMITED BY SIZE INTO AUA-AU-KEY
           MOVE 'ADD '                 TO AUA-AU-ACTION
           MOVE 'STEP_RC'              TO AUA-AU-FIELD
           MOVE SPACES                 TO AUA-AU-BEFORE
           MOVE SPACES                 TO AUA-AU-AFTER
           STRING WS-RES-RC(WS-STEP-IX) ' ' RSL-RESULT
                  DELIMITED BY SIZE INTO AUA-AU-AFTER
           PERFORM 8300-CALL-BILAUDIT
           ADD 1                       TO WS-AUDITS-SINCE-COMMIT
           IF WS-AUDITS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 6100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
       8300-CALL-BILAUDIT.
           INITIALIZE AUA-HEADER
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO AUA-USER-ID
                                          AUA-CALLER-PGM
                                          AUA-AU-PROGRAM
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF AUA-RETURN-CODE >= 12
              MOVE 16                  TO WS-RC
              STRING 'BILAUDIT FAILED ' AUA-REASON-CODE ' '
                     AUA-MESSAGE DELIMITED BY SIZE INTO RMS-TEXT
              PERFORM 7100-PRINT-MESSAGE
              EXEC SQL ROLLBACK END-EXEC
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           CLOSE REPORT-FILE
           DISPLAY 'BILEODDR ' WS-MODE ' ENDED RC ' WS-RC
           MOVE WS-RC                  TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
                                          WS-ERR-SQLCODE
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO RMS-TEXT
           STRING 'DB2 ERROR SQLCODE ' WS-SQLCODE-DISP ' IN '
                  WS-ERR-PARA DELIMITED BY SIZE INTO RMS-TEXT
           PERFORM 7100-PRINT-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID
                                          ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE WS-ERR-SQLCODE         TO ERC-ER-SQLCODE
           MOVE CTL-CONTROL-KEY        TO ERC-ER-KEY
           MOVE SPACES                 TO ERC-ER-TEXT
           STRING 'BILEODDR Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' mode ' WS-MODE DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           MOVE 16                     TO WS-RC
           PERFORM 9000-RETURN.
