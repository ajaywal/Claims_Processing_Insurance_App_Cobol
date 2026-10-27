       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMEODDR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMEODDR                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : End-of-day driver for CLM_CONTROL. Last step of  *
      *               the nightly stream (CLMEOD): checks the day is   *
      *               ready to close, prints the day's statistics and  *
      *               rolls the claims business date to the next       *
      *               business day.                                    *
      *               PARM: optional yyyy-mm-dd = business date the    *
      *               scheduler expects to close (restart guard).      *
      * CALLS       : CLMDATES (DTVALD, DTADDD, DTBUSD)                *
      *               CLMERRLG (ERRLOG)                                *
      * FILES       : RPTOUT  132-column report                        *
      *               CHKPT   checkpoint record (CLMEODK1)             *
      * TABLES      : CLM_CONTROL      (R U) rows SYSTEM, GLEXTRCT     *
      *               CLM_EVENT_QUEUE  (R)                             *
      *               CLM_CHECK        (R)                             *
      *               CLM_AUDIT_CHAIN  (R)                             *
      *               CLM_TRANSACTION  (R)                             *
      *               CLM_CLAIM        (R)                             *
      *               CLM_ACTIVITY     (R)                             *
      * RULES       : BR-CLM-EOD-001 A date the scheduler did not ask  *
      *                              for is never closed: already      *
      *                              rolled is RC 4 (no-op), behind is *
      *                              RC 12                             *
      *               BR-CLM-EOD-002 EOD starts only from batch status *
      *                              O (open) or H (held); E means an  *
      *                              EOD is already running            *
      *               BR-CLM-EOD-003 The day cannot close until the GL *
      *                              extract for it has completed -    *
      *                              status H and RC 12                *
      *               BR-CLM-EOD-004 Stuck or errored events, unissued *
      *                              due checks and unsealed audit     *
      *                              rows are warnings (RC 4)          *
      *               BR-CLM-EOD-005 Next date = next calendar day     *
      *                              rolled forward to a business day  *
      *               BR-CLM-EOD-006 The roll is one optimistic UPDATE *
      *                              on the expected date in status E  *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE     ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL CHK-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).
       FD  CHK-FILE
           RECORDING MODE IS F.
           COPY CLMEODK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMEODDR'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-RPT-STATUS            PIC XX    VALUE '00'.
           05 WS-CHK-STATUS            PIC XX    VALUE '00'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-NEXT-DATE             PIC X(10) VALUE SPACES.
           05 WS-PARM-DATE             PIC X(10) VALUE SPACES.
           05 WS-GL-DATE               PIC X(10) VALUE SPACES.
           05 WS-RUN-TS                PIC X(26) VALUE SPACES.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.
           05 WS-HOLD-SW               PIC X     VALUE 'N'.
              88 WS-HOLD-DAY            VALUE 'Y'.

       01  WS-COUNTS.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-AMOUNT                PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMOUNT-IND            PIC S9(4) COMP VALUE 0.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY CLMEODR1.

       LINKAGE SECTION.
       01  LK-PARM.
           05 LK-PARM-LEN              PIC S9(4) COMP.
           05 LK-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-START-EOD
           PERFORM 3000-READINESS-CHECKS
           PERFORM 3500-DAY-STATISTICS
           IF WS-HOLD-DAY
              PERFORM 4400-HOLD-DAY
           ELSE
              PERFORM 5000-NEXT-BUSINESS-DATE
              PERFORM 4500-ROLL-DATE
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'CLMEODDR RPTOUT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4000-READ-CONTROL
           MOVE WS-BUS-DATE            TO EOR-H1-BUS-DATE
           MOVE WS-RUN-TS(1:19)        TO EOR-H1-RUN-TS
           OPEN OUTPUT CHK-FILE
           IF WS-CHK-STATUS NOT = '00'
              MOVE 'CHKPT file cannot be opened for output'
                                       TO EOR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           INITIALIZE EOK-CHECKPOINT-REC
           IF LK-PARM-LEN > 0
              MOVE LK-PARM-DATA(1:10)  TO WS-PARM-DATE
              PERFORM 1100-CHECK-PARM-DATE
           END-IF.

      ******************************************************************
      * BR-CLM-EOD-001 Scheduler date guard (makes a rerun safe)
       1100-CHECK-PARM-DATE.
           INITIALIZE DTA-HEADER DTA-BODY
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PARM-DATE           TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK OR NOT DTA-DT-IS-VALID
              STRING 'PARM date ' WS-PARM-DATE ' is not a valid date'
                     DELIMITED BY SIZE INTO EOR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           EVALUATE TRUE
              WHEN WS-BUS-DATE = WS-PARM-DATE
                 CONTINUE
              WHEN WS-BUS-DATE > WS-PARM-DATE
                 STRING 'Business date ' WS-PARM-DATE
                        ' already closed - claims date is now '
                        WS-BUS-DATE ' - nothing to do'
                        DELIMITED BY SIZE INTO EOR-MSG-TEXT
                 PERFORM 6100-WRITE-MSG
                 MOVE 4                TO WS-MAX-RC
                 PERFORM 9000-RETURN
              WHEN OTHER
                 STRING 'Claims business date ' WS-BUS-DATE
                        ' is behind scheduler date ' WS-PARM-DATE
                        ' - a previous EOD did not run'
                        DELIMITED BY SIZE INTO EOR-MSG-TEXT
                 PERFORM 6100-WRITE-MSG
                 MOVE 12               TO WS-MAX-RC
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-EOD-002 Claim the control row for end of day
       2000-START-EOD.
           EXEC SQL
              UPDATE CLMDB.CLM_CONTROL
                 SET BATCH_STATUS = 'E',
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CONTROL_KEY   = 'SYSTEM  '
                 AND BUSINESS_DATE = :WS-BUS-DATE
                 AND BATCH_STATUS IN ('O', 'H')
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 1
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 STRING 'Batch status is ' CTL-BATCH-STATUS
                        ' - another end of day is running or the row'
                        ' changed; not started'
                        DELIMITED BY SIZE INTO EOR-MSG-TEXT
                 PERFORM 6100-WRITE-MSG
                 MOVE 12               TO WS-MAX-RC
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2000-START-EOD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           PERFORM 7000-COMMIT
           MOVE 'R'                    TO EOK-RUN-STATUS
           MOVE WS-BUS-DATE            TO EOK-ROLLED-FROM
           PERFORM 7100-WRITE-CHECKPOINT
           MOVE '*** END OF DAY READINESS CHECKS' TO EOR-MSG-TEXT
           PERFORM 6100-WRITE-MSG.

      ******************************************************************
       3000-READINESS-CHECKS.
      * BR-CLM-EOD-003 GL extract must have run for the business date
           MOVE SPACES                 TO WS-GL-DATE
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :WS-GL-DATE
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = 'GLEXTRCT'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '3000-READINESS-GL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'GL EXTRACT COMPLETED FOR BUSINESS DATE' TO EOR-C-NAME
           MOVE 0                      TO EOR-C-VALUE
           IF WS-GL-DATE NOT = SPACES AND WS-GL-DATE >= WS-BUS-DATE
              MOVE 'OK'                TO EOR-C-RESULT
              MOVE SPACES              TO EOR-C-NOTE
           ELSE
              MOVE 'HOLD'              TO EOR-C-RESULT
              STRING 'Last extract ' WS-GL-DATE ' - rerun CLMGLEXT'
                     DELIMITED BY SIZE INTO EOR-C-NOTE
              SET WS-HOLD-DAY          TO TRUE
           END-IF
           PERFORM 6200-WRITE-CHECK
      * BR-CLM-EOD-004 Warning-level checks
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_STATUS = 'PROC'
           END-EXEC
           MOVE 'INTEGRATION EVENTS LEFT IN PROC' TO EOR-C-NAME
           MOVE 'CLMEVTDR or online EVGET did not finish'
                                       TO EOR-C-NOTE
           PERFORM 3100-WARN-IF-ANY
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_STATUS = 'ERRR'
           END-EXEC
           MOVE 'INTEGRATION EVENTS PARKED IN ERRR' TO EOR-C-NAME
           MOVE 'Retry limit reached - needs operations'
                                       TO EOR-C-NOTE
           PERFORM 3100-WARN-IF-ANY
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_CHECK
               WHERE CHECK_STATUS    = 'PEND'
                 AND SCHEDULED_DATE <= :WS-BUS-DATE
           END-EXEC
           MOVE 'CHECKS DUE BUT NOT ISSUED' TO EOR-C-NAME
           MOVE 'CLMCHKIS left due checks pending'
                                       TO EOR-C-NOTE
           PERFORM 3100-WARN-IF-ANY
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_AUDIT_CHAIN
               WHERE SEALED_FLAG = 'N'
                 AND DATE(EVENT_TS) <= :WS-BUS-DATE
           END-EXEC
           MOVE 'AUDIT CHAIN ROWS NOT SEALED' TO EOR-C-NAME
           MOVE 'CLMSEALR did not seal the day'
                                       TO EOR-C-NOTE
           PERFORM 3100-WARN-IF-ANY.

      ******************************************************************
       3100-WARN-IF-ANY.
           IF SQLCODE NOT = 0
              MOVE '3100-WARN-IF-ANY'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-COUNT               TO EOR-C-VALUE
           IF WS-COUNT = 0
              MOVE 'OK'                TO EOR-C-RESULT
              MOVE SPACES              TO EOR-C-NOTE
           ELSE
              MOVE 'WARNING'           TO EOR-C-RESULT
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF
           PERFORM 6200-WRITE-CHECK.

      ******************************************************************
       3500-DAY-STATISTICS.
           MOVE SPACES                 TO EOR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE '*** BUSINESS DAY STATISTICS' TO EOR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE SPACES                 TO EOR-C-RESULT EOR-C-NOTE
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM
               WHERE OPEN_DATE = :WS-BUS-DATE
           END-EXEC
           MOVE 'CLAIMS OPENED'        TO EOR-C-NAME
           PERFORM 3600-WRITE-STAT
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM
               WHERE CLOSE_DATE = :WS-BUS-DATE
                 AND CLAIM_STATE = 'CLSD'
           END-EXEC
           MOVE 'CLAIMS CLOSED'        TO EOR-C-NAME
           PERFORM 3600-WRITE-STAT
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_STATE NOT IN ('DRFT', 'CLSD')
           END-EXEC
           MOVE 'CLAIMS OPEN AT CLOSE OF DAY' TO EOR-C-NAME
           PERFORM 3600-WRITE-STAT
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_ACTIVITY
               WHERE ACTIVITY_STATUS = 'OPEN'
                 AND DUE_DATE < :WS-BUS-DATE
           END-EXEC
           MOVE 'OPEN ACTIVITIES PAST DUE' TO EOR-C-NAME
           PERFORM 3600-WRITE-STAT
           EXEC SQL
              SELECT COUNT(*) INTO :WS-COUNT
                FROM CLMDB.CLM_TRANSACTION
               WHERE TXN_STATUS = 'PAPR'
           END-EXEC
           MOVE 'TRANSACTIONS AWAITING APPROVAL' TO EOR-C-NAME
           PERFORM 3600-WRITE-STAT
           EXEC SQL
              SELECT COUNT(*), SUM(CHECK_AMOUNT)
                INTO :WS-COUNT, :WS-AMOUNT :WS-AMOUNT-IND
                FROM CLMDB.CLM_CHECK
               WHERE ISSUE_DATE = :WS-BUS-DATE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3500-DAY-STATISTICS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-AMOUNT-IND < 0
              MOVE 0                   TO WS-AMOUNT
           END-IF
           MOVE 'CHECKS ISSUED (COUNT)' TO EOR-C-NAME
           PERFORM 3600-WRITE-STAT
           MOVE 'CHECKS ISSUED (GBP)'  TO EOR-C-NAME
           MOVE WS-AMOUNT              TO EOR-C-VALUE
           PERFORM 6200-WRITE-CHECK.

      ******************************************************************
       3600-WRITE-STAT.
           IF SQLCODE NOT = 0
              MOVE '3600-WRITE-STAT'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-COUNT               TO EOR-C-VALUE
           PERFORM 6200-WRITE-CHECK.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE, PRIOR_BUS_DATE, BATCH_STATUS,
                     CURRENT TIMESTAMP
                INTO :CTL-BUSINESS-DATE, :CTL-PRIOR-BUS-DATE,
                     :CTL-BATCH-STATUS, :WS-RUN-TS
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
       4400-HOLD-DAY.
           EXEC SQL
              UPDATE CLMDB.CLM_CONTROL
                 SET BATCH_STATUS = 'H',
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CONTROL_KEY   = 'SYSTEM  '
                 AND BUSINESS_DATE = :WS-BUS-DATE
                 AND BATCH_STATUS  = 'E'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-HOLD-DAY'     TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 7000-COMMIT
           MOVE 'H'                    TO EOK-RUN-STATUS
           PERFORM 7100-WRITE-CHECKPOINT
           MOVE SPACES                 TO EOR-MSG-TEXT
           STRING 'END OF DAY HELD - business date ' WS-BUS-DATE
                  ' not rolled; fix the HOLD item and rerun CLMEODDR'
                  DELIMITED BY SIZE INTO EOR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 12                     TO WS-MAX-RC.

      ******************************************************************
      * BR-CLM-EOD-006 Single optimistic roll of the business date
       4500-ROLL-DATE.
           EXEC SQL
              UPDATE CLMDB.CLM_CONTROL
                 SET PRIOR_BUS_DATE = BUSINESS_DATE,
                     BUSINESS_DATE  = :WS-NEXT-DATE,
                     BATCH_STATUS   = 'O',
                     LAST_EOD_TS    = CURRENT TIMESTAMP,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE CONTROL_KEY   = 'SYSTEM  '
                 AND BUSINESS_DATE = :WS-BUS-DATE
                 AND BATCH_STATUS  = 'E'
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 1
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 MOVE 'CLM_CONTROL changed during EOD - not rolled'
                                       TO EOR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
              WHEN OTHER
                 MOVE '4500-ROLL-DATE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           PERFORM 7000-COMMIT
           MOVE 'C'                    TO EOK-RUN-STATUS
           MOVE WS-NEXT-DATE           TO EOK-ROLLED-TO
           PERFORM 7100-WRITE-CHECKPOINT
           MOVE SPACES                 TO EOR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           STRING 'END OF DAY COMPLETE - claims business date rolled '
                  'from ' WS-BUS-DATE ' to ' WS-NEXT-DATE
                  DELIMITED BY SIZE INTO EOR-MSG-TEXT
           PERFORM 6100-WRITE-MSG.

      ******************************************************************
      * BR-CLM-EOD-005 Next calendar day rolled to a business day
       5000-NEXT-BUSINESS-DATE.
           INITIALIZE DTA-HEADER DTA-BODY
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE DTA-DT-DATE-1
           MOVE 1                      TO DTA-DT-NUMBER
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              MOVE DTA-MESSAGE         TO EOR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE DTA-DT-RESULT-DATE     TO WS-NEXT-DATE
           INITIALIZE DTA-HEADER DTA-BODY
           MOVE 'DTBUSD  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-NEXT-DATE           TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              MOVE DTA-MESSAGE         TO EOR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE DTA-DT-RESULT-DATE     TO WS-NEXT-DATE
           IF WS-NEXT-DATE NOT > WS-BUS-DATE
              STRING 'Computed next business date ' WS-NEXT-DATE
                     ' is not after ' WS-BUS-DATE
                     DELIMITED BY SIZE INTO EOR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF.

      ******************************************************************
       6000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO EOR-H1-PAGE
           WRITE RPT-RECORD FROM EOR-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM EOR-HEAD-2 AFTER ADVANCING 1
           MOVE 2                      TO WS-LINE-COUNT.

      ******************************************************************
       6050-WRITE-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6000-PRINT-HEADINGS
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6100-WRITE-MSG.
           MOVE EOR-MSG-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE SPACES                 TO EOR-MSG-TEXT.

      ******************************************************************
       6200-WRITE-CHECK.
           MOVE EOR-CHECK-LINE         TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
      * Failure after status E: put the row back to H so the rerun
      * is accepted, then stop.
       6900-ABORT-RUN.
           PERFORM 6100-WRITE-MSG
           EXEC SQL ROLLBACK END-EXEC
           EXEC SQL
              UPDATE CLMDB.CLM_CONTROL
                 SET BATCH_STATUS = 'H',
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CONTROL_KEY   = 'SYSTEM  '
                 AND BATCH_STATUS  = 'E'
           END-EXEC
           EXEC SQL COMMIT END-EXEC
           MOVE 16                     TO WS-MAX-RC
           PERFORM 9000-RETURN.

      ******************************************************************
       7000-COMMIT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       7100-WRITE-CHECKPOINT.
           MOVE WS-PROGRAM-ID          TO EOK-PROGRAM
           MOVE FUNCTION CURRENT-DATE  TO EOK-CHECKPOINT-TS
           CLOSE CHK-FILE
           OPEN OUTPUT CHK-FILE
           WRITE EOK-CHECKPOINT-REC
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'CLMEODDR CHECKPOINT WRITE FAILED ' WS-CHK-STATUS
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF.

      ******************************************************************
       9000-RETURN.
           MOVE SPACES                 TO EOR-C-NOTE EOR-C-RESULT
           MOVE 'RETURN CODE'          TO EOR-C-NAME
           MOVE WS-MAX-RC              TO EOR-C-VALUE
           PERFORM 6200-WRITE-CHECK
           CLOSE RPT-FILE
           CLOSE CHK-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-BUS-DATE            TO ERC-ER-KEY
           STRING 'CLMEODDR Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           EXEC SQL ROLLBACK END-EXEC
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO EOR-MSG-TEXT
           PERFORM 6900-ABORT-RUN.
