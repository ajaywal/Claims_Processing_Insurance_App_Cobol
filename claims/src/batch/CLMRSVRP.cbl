       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMRSVRP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMRSVRP                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Reserve adequacy review. For every open exposure *
      *               compares the open reserve with a benchmark by    *
      *               exposure type and claim severity, checks how     *
      *               long the reserve has gone unchanged and how fast *
      *               it is being paid down, and raises an RSVREV      *
      *               activity for the adjuster on each flagged        *
      *               exposure.                                        *
      *               PARM: optional commit frequency (nnnn).          *
      * CALLS       : CLMACTV0 (ACTNEW)                                *
      *               CLMDATES (DTDIFF)                                *
      *               CLMMONEY (MNPRORA)                               *
      *               CLMERRLG (ERRLOG)                                *
      * FILES       : RPTOUT  132-column report                        *
      *               CHKPT   checkpoint / restart record (CLMRSVK1)   *
      * TABLES      : CLM_EXPOSURE    (R)                              *
      *               CLM_CLAIM       (R)                              *
      *               CLM_TRANSACTION (R)                              *
      *               CLM_ACTIVITY    (R)                              *
      *               CLM_CONTROL     (R)                              *
      * RULES       : BR-CLM-RSV-001 Benchmark by exposure type and    *
      *                              claim severity (1-5, clamped);    *
      *                              '*' row for unlisted types        *
      *               BR-CLM-RSV-002 Under-reserved when the open      *
      *                              reserve is below 80% of the       *
      *                              benchmark still to be paid        *
      *               BR-CLM-RSV-003 Stale when the reserve is open    *
      *                              and unchanged for 90 days or more *
      *               BR-CLM-RSV-004 Velocity: reserve would be used   *
      *                              up within 30 days at the average  *
      *                              payment rate since creation       *
      *               BR-CLM-RSV-005 Exposure open over 30 days with   *
      *                              no reserve and no payment         *
      *               BR-CLM-RSV-006 One open RSVREV activity per      *
      *                              exposure - never duplicated       *
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
           COPY CLMRSVK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMRSVRP'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'CLMMONEY'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-THRESHOLDS.
           05 WS-UNDER-PCT             PIC S9(3) COMP-3 VALUE 80.
           05 WS-STALE-DAYS            PIC S9(5) COMP-3 VALUE 90.
           05 WS-VELOCITY-DAYS         PIC S9(5) COMP-3 VALUE 30.
           05 WS-NO-RESERVE-DAYS       PIC S9(5) COMP-3 VALUE 30.

       01  WS-FILE-STATUS.
           05 WS-RPT-STATUS            PIC XX    VALUE '00'.
           05 WS-CHK-STATUS            PIC XX    VALUE '00'.

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-RESTART-SW            PIC X     VALUE 'N'.
              88 WS-RESTARTING          VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-TS                PIC X(26) VALUE SPACES.
           05 WS-PARM-FREQ             PIC X(5)  JUSTIFIED RIGHT
                                                 VALUE SPACES.
           05 WS-PARM-FREQ-N REDEFINES WS-PARM-FREQ
                                       PIC 9(5).
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-UNCOMMITTED           PIC S9(4) COMP VALUE 0.
           05 WS-RESUME-KEY            PIC S9(9) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.

      * BR-CLM-RSV-001 Benchmark reserve (GBP) by exposure type for
      * claim severity 1..5. Last row '*' applies to unlisted types.
       01  WS-BENCH-VALUES.
           05 FILLER PIC X(43) VALUE
              'VEHDAMAG00025000005000000900000150000025000'.
           05 FILLER PIC X(43) VALUE
              'PROPDAMA00030000008000002000000500000120000'.
           05 FILLER PIC X(43) VALUE
              'BODINJUR00050000015000004000001000000250000'.
           05 FILLER PIC X(43) VALUE
              'MEDPAY  00010000003000000750000150000030000'.
           05 FILLER PIC X(43) VALUE
              'CONTENTS00015000004000001000000250000060000'.
           05 FILLER PIC X(43) VALUE
              'LOSSOFUS00008000002000000400000080000015000'.
           05 FILLER PIC X(43) VALUE
              'GENDAMAG00020000006000001500000400000100000'.
           05 FILLER PIC X(43) VALUE
              '*       00020000006000001500000400000100000'.
       01  WS-BENCH-TABLE REDEFINES WS-BENCH-VALUES.
           05 WS-BENCH OCCURS 8 TIMES INDEXED BY BX.
              10 WS-BN-EXP-TYPE        PIC X(8).
              10 WS-BN-AMOUNT OCCURS 5 TIMES
                                       PIC 9(7).
       01  WS-SEV-IX                   PIC S9(4) COMP VALUE 1.

       01  WS-EXPOSURE-WORK.
           05 WS-LAST-RSV-DATE         PIC X(10) VALUE SPACES.
           05 WS-LAST-RSV-IND          PIC S9(4) COMP VALUE 0.
           05 WS-DAYS-OPEN             PIC S9(7) COMP-3 VALUE 0.
           05 WS-DAYS-UNCHANGED        PIC S9(7) COMP-3 VALUE 0.
           05 WS-BENCHMARK             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-BENCH-REMAINING       PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-UNDER-LIMIT           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-RESERVE-PCT           PIC S9(5) COMP-3 VALUE 0.
           05 WS-VELOCITY              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-FLAGS                 PIC X(20) VALUE SPACES.
           05 WS-FLAG-PTR              PIC S9(4) COMP VALUE 1.
           05 WS-FLAG-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-OPEN-RSVREV           PIC S9(9) COMP VALUE 0.
           05 WS-START-DATE            PIC X(10) VALUE SPACES.
           05 WS-SUBJECT               PIC X(60) VALUE SPACES.

       01  WS-TOTALS.
           05 WS-REVIEWED              PIC S9(7) COMP-3 VALUE 0.
           05 WS-FLAGGED               PIC S9(7) COMP-3 VALUE 0.
           05 WS-UNDER-COUNT           PIC S9(7) COMP-3 VALUE 0.
           05 WS-STALE-COUNT           PIC S9(7) COMP-3 VALUE 0.
           05 WS-VELO-COUNT            PIC S9(7) COMP-3 VALUE 0.
           05 WS-NORSV-COUNT           PIC S9(7) COMP-3 VALUE 0.
           05 WS-ACTS-CREATED          PIC S9(7) COMP-3 VALUE 0.
           05 WS-ACTS-EXISTING         PIC S9(7) COMP-3 VALUE 0.
           05 WS-ACTS-FAILED           PIC S9(7) COMP-3 VALUE 0.
           05 WS-TOTAL-RESERVE         PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-BENCH-REMAIN    PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-UNDER-SHORTFALL       PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(9).
           05 WS-PCT-DISP              PIC ZZ9.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDEXP END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

      * Open exposures on open claims with their last reserve change
           EXEC SQL DECLARE EXPCSR CURSOR WITH HOLD FOR
                SELECT E.EXPOSURE_ID, E.CLAIM_ID, E.EXPOSURE_TYPE,
                       E.ASSIGNED_USER, E.OPEN_RESERVE, E.TOTAL_PAID,
                       E.CREATE_DATE, C.CLAIM_NUMBER, C.SEVERITY,
                       C.ASSIGNED_USER,
                       (SELECT MAX(T.BUSINESS_DATE)
                          FROM CLMDB.CLM_TRANSACTION T
                         WHERE T.EXPOSURE_ID = E.EXPOSURE_ID
                           AND T.TXN_TYPE    = 'RSV '
                           AND T.TXN_STATUS  = 'SUBM')
                  FROM CLMDB.CLM_EXPOSURE E
                  JOIN CLMDB.CLM_CLAIM C
                    ON C.CLAIM_ID = E.CLAIM_ID
                 WHERE E.EXPOSURE_STATE NOT IN ('DRFT', 'CLSD')
                   AND C.CLAIM_STATE    NOT IN ('DRFT', 'CLSD')
                   AND E.EXPOSURE_ID > :WS-RESUME-KEY
                 ORDER BY E.EXPOSURE_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-MNY-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY CLMCMNY REPLACING LEADING ==CA-== BY ==MNA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY CLMRSVR1.

       LINKAGE SECTION.
       01  LK-PARM.
           05 LK-PARM-LEN              PIC S9(4) COMP.
           05 LK-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-REVIEW-EXPOSURES
           PERFORM 7000-COMMIT
           MOVE 'C'                    TO RVK-RUN-STATUS
           PERFORM 7100-WRITE-CHECKPOINT
           PERFORM 6500-PRINT-TOTALS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'CLMRSVRP RPTOUT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           IF LK-PARM-LEN > 0
              MOVE LK-PARM-DATA(1:LK-PARM-LEN) TO WS-PARM-FREQ
              INSPECT WS-PARM-FREQ REPLACING LEADING SPACE BY ZERO
              IF WS-PARM-FREQ-N IS NUMERIC
              AND WS-PARM-FREQ-N > 0 AND WS-PARM-FREQ-N <= 5000
                 MOVE WS-PARM-FREQ-N   TO WS-COMMIT-FREQ
              ELSE
                 MOVE 'Invalid PARM - commit frequency 1-5000'
                                       TO RVR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
              END-IF
           END-IF
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE, CURRENT TIMESTAMP
                INTO :CTL-BUSINESS-DATE, :WS-RUN-TS
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '1000-INITIALISE'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE RVR-H1-BUS-DATE
           MOVE WS-RUN-TS(1:19)        TO RVR-H1-RUN-TS
           PERFORM 1100-READ-RESTART
           MOVE RVR-COL-HEAD           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       1100-READ-RESTART.
           OPEN INPUT CHK-FILE
           IF WS-CHK-STATUS = '00'
              READ CHK-FILE
                 AT END
                    CONTINUE
                 NOT AT END
                    IF RVK-PROGRAM = WS-PROGRAM-ID
                    AND RVK-RUN-ACTIVE
                    AND RVK-BUS-DATE = WS-BUS-DATE
                       SET WS-RESTARTING TO TRUE
                       MOVE RVK-LAST-EXPOSURE-ID TO WS-RESUME-KEY
                    END-IF
              END-READ
              CLOSE CHK-FILE
           END-IF
           OPEN OUTPUT CHK-FILE
           IF WS-CHK-STATUS NOT = '00'
              MOVE 'CHKPT file cannot be opened for output'
                                       TO RVR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           INITIALIZE RVK-CHECKPOINT-REC
           MOVE WS-RESUME-KEY          TO RVK-LAST-EXPOSURE-ID
           IF WS-RESTARTING
              MOVE WS-RESUME-KEY       TO WS-KEY-DISP
              STRING 'RESTART: resuming after exposure id '
                     WS-KEY-DISP DELIMITED BY SIZE INTO RVR-MSG-TEXT
              PERFORM 6100-WRITE-MSG
           END-IF.

      ******************************************************************
       2000-REVIEW-EXPOSURES.
           EXEC SQL OPEN EXPCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-REVIEW-EXPOSURES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-EXPOSURE
           PERFORM UNTIL WS-EOF
              ADD 1                    TO WS-REVIEWED
              ADD EXP-OPEN-RESERVE     TO WS-TOTAL-RESERVE
              PERFORM 2100-ASSESS-EXPOSURE
              IF WS-FLAG-COUNT > 0
                 ADD 1                 TO WS-FLAGGED
                 PERFORM 2200-RAISE-REVIEW
                 PERFORM 6200-WRITE-DETAIL
              END-IF
              MOVE EXP-EXPOSURE-ID     TO RVK-LAST-EXPOSURE-ID
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
                 PERFORM 7100-WRITE-CHECKPOINT
              END-IF
              PERFORM 4100-FETCH-EXPOSURE
           END-PERFORM
           EXEC SQL CLOSE EXPCSR END-EXEC.

      ******************************************************************
       2100-ASSESS-EXPOSURE.
           MOVE SPACES                 TO WS-FLAGS
           MOVE 1                      TO WS-FLAG-PTR
           MOVE 0                      TO WS-FLAG-COUNT WS-VELOCITY
           PERFORM 5000-FIND-BENCHMARK
           MOVE EXP-CREATE-DATE        TO WS-START-DATE
           PERFORM 8100-DAYS-TO-BUS-DATE
           MOVE DTA-DT-RESULT-NUM      TO WS-DAYS-OPEN
           IF WS-LAST-RSV-IND < 0
              MOVE EXP-CREATE-DATE     TO WS-LAST-RSV-DATE
              MOVE WS-DAYS-OPEN        TO WS-DAYS-UNCHANGED
           ELSE
              MOVE WS-LAST-RSV-DATE    TO WS-START-DATE
              PERFORM 8100-DAYS-TO-BUS-DATE
              MOVE DTA-DT-RESULT-NUM   TO WS-DAYS-UNCHANGED
           END-IF
      * BR-CLM-RSV-002 Below 80% of the benchmark still to be paid
           COMPUTE WS-BENCH-REMAINING = WS-BENCHMARK - EXP-TOTAL-PAID
           IF WS-BENCH-REMAINING < 0
              MOVE 0                   TO WS-BENCH-REMAINING
           END-IF
           ADD WS-BENCH-REMAINING      TO WS-TOTAL-BENCH-REMAIN
           MOVE 0                      TO WS-RESERVE-PCT
           IF WS-BENCH-REMAINING > 0
              COMPUTE WS-RESERVE-PCT ROUNDED =
                      EXP-OPEN-RESERVE * 100 / WS-BENCH-REMAINING
              COMPUTE WS-UNDER-LIMIT ROUNDED =
                      WS-BENCH-REMAINING * WS-UNDER-PCT / 100
              IF EXP-OPEN-RESERVE > 0
              AND EXP-OPEN-RESERVE < WS-UNDER-LIMIT
                 STRING 'UNDR ' DELIMITED BY SIZE INTO WS-FLAGS
                        WITH POINTER WS-FLAG-PTR
                 ADD 1                 TO WS-FLAG-COUNT WS-UNDER-COUNT
                 COMPUTE WS-UNDER-SHORTFALL = WS-UNDER-SHORTFALL
                         + WS-BENCH-REMAINING - EXP-OPEN-RESERVE
              END-IF
           END-IF
      * BR-CLM-RSV-003 Open reserve unchanged for 90 days or more
           IF EXP-OPEN-RESERVE > 0
           AND WS-DAYS-UNCHANGED >= WS-STALE-DAYS
              STRING 'STAL ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
              ADD 1                    TO WS-FLAG-COUNT WS-STALE-COUNT
           END-IF
      * BR-CLM-RSV-004 Paid-to-date velocity against the reserve
           IF EXP-TOTAL-PAID > 0 AND WS-DAYS-OPEN >= WS-VELOCITY-DAYS
              PERFORM 8300-PAYMENT-VELOCITY
              IF EXP-OPEN-RESERVE < WS-VELOCITY
                 STRING 'VELO ' DELIMITED BY SIZE INTO WS-FLAGS
                        WITH POINTER WS-FLAG-PTR
                 ADD 1                 TO WS-FLAG-COUNT WS-VELO-COUNT
              END-IF
           END-IF
      * BR-CLM-RSV-005 Open exposure carrying nothing at all
           IF EXP-OPEN-RESERVE = 0 AND EXP-TOTAL-PAID = 0
           AND WS-DAYS-OPEN > WS-NO-RESERVE-DAYS
              STRING 'NORSV ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
              ADD 1                    TO WS-FLAG-COUNT WS-NORSV-COUNT
           END-IF.

      ******************************************************************
      * BR-CLM-RSV-006 One open RSVREV activity per exposure
       2200-RAISE-REVIEW.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-OPEN-RSVREV
                FROM CLMDB.CLM_ACTIVITY
               WHERE EXPOSURE_ID     = :EXP-EXPOSURE-ID
                 AND PATTERN_CODE    = 'RSVREV'
                 AND ACTIVITY_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-RAISE-REVIEW' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-OPEN-RSVREV > 0
              ADD 1                    TO WS-ACTS-EXISTING
              MOVE 'EXISTS'            TO RVR-D-ACTIVITY
           ELSE
              PERFORM 8200-CREATE-ACTIVITY
           END-IF.

      ******************************************************************
       4100-FETCH-EXPOSURE.
           EXEC SQL
              FETCH EXPCSR
               INTO :EXP-EXPOSURE-ID, :EXP-CLAIM-ID,
                    :EXP-EXPOSURE-TYPE, :EXP-ASSIGNED-USER,
                    :EXP-OPEN-RESERVE, :EXP-TOTAL-PAID,
                    :EXP-CREATE-DATE, :CLM-CLAIM-NUMBER,
                    :CLM-SEVERITY, :CLM-ASSIGNED-USER,
                    :WS-LAST-RSV-DATE :WS-LAST-RSV-IND
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-RSV-001 Type row (or '*'), severity clamped to 1..5
       5000-FIND-BENCHMARK.
           MOVE CLM-SEVERITY           TO WS-SEV-IX
           IF WS-SEV-IX < 1
              MOVE 1                   TO WS-SEV-IX
           END-IF
           IF WS-SEV-IX > 5
              MOVE 5                   TO WS-SEV-IX
           END-IF
           SET BX                      TO 1
           SEARCH WS-BENCH
              AT END
                 SET BX                TO 8
              WHEN WS-BN-EXP-TYPE(BX) = EXP-EXPOSURE-TYPE
                 CONTINUE
           END-SEARCH
           MOVE WS-BN-AMOUNT(BX WS-SEV-IX) TO WS-BENCHMARK.

      ******************************************************************
       6000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO RVR-H1-PAGE
           WRITE RPT-RECORD FROM RVR-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM RVR-HEAD-2 AFTER ADVANCING 1
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
           MOVE RVR-MSG-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE SPACES                 TO RVR-MSG-TEXT.

      ******************************************************************
       6200-WRITE-DETAIL.
           MOVE CLM-CLAIM-NUMBER       TO RVR-D-CLAIM-NUMBER
           MOVE EXP-EXPOSURE-ID        TO RVR-D-EXPOSURE-ID
           MOVE EXP-EXPOSURE-TYPE      TO RVR-D-EXP-TYPE
           MOVE WS-SEV-IX              TO RVR-D-SEVERITY
           MOVE EXP-OPEN-RESERVE       TO RVR-D-OPEN-RESERVE
           MOVE EXP-TOTAL-PAID         TO RVR-D-PAID
           MOVE WS-BENCHMARK           TO RVR-D-BENCHMARK
           MOVE WS-RESERVE-PCT         TO RVR-D-PCT
           MOVE WS-LAST-RSV-DATE       TO RVR-D-LAST-CHANGE
           MOVE WS-DAYS-UNCHANGED      TO RVR-D-DAYS
           MOVE WS-VELOCITY            TO RVR-D-VELOCITY
           MOVE WS-FLAGS               TO RVR-D-FLAGS
           MOVE RVR-DET-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RVR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE '*** CONTROL TOTALS'   TO RVR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 'OPEN EXPOSURES REVIEWED THIS RUN' TO RVR-T-LABEL
           MOVE WS-REVIEWED            TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'EXPOSURES FLAGGED'    TO RVR-T-LABEL
           MOVE WS-FLAGGED             TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE WS-UNDER-PCT           TO WS-PCT-DISP
           MOVE SPACES                 TO RVR-T-LABEL
           STRING '  UNDR - BELOW ' WS-PCT-DISP
                  '% OF REMAINING BENCHMARK'
                  DELIMITED BY SIZE INTO RVR-T-LABEL
           MOVE WS-UNDER-COUNT         TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE '  STAL - RESERVE UNCHANGED 90+ DAYS' TO RVR-T-LABEL
           MOVE WS-STALE-COUNT         TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE '  VELO - RESERVE BELOW 30 DAYS OF PAYMENTS'
                                       TO RVR-T-LABEL
           MOVE WS-VELO-COUNT          TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE '  NORSV - NO RESERVE AND NO PAYMENT AFTER 30 DAYS'
                                       TO RVR-T-LABEL
           MOVE WS-NORSV-COUNT         TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'RSVREV ACTIVITIES CREATED' TO RVR-T-LABEL
           MOVE WS-ACTS-CREATED        TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'RSVREV ACTIVITIES ALREADY OPEN' TO RVR-T-LABEL
           MOVE WS-ACTS-EXISTING       TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'RSVREV ACTIVITIES REJECTED BY CLMACTV0'
                                       TO RVR-T-LABEL
           MOVE WS-ACTS-FAILED         TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'TOTAL OPEN RESERVE REVIEWED (GBP)' TO RVR-T-LABEL
           MOVE WS-TOTAL-RESERVE       TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'TOTAL REMAINING BENCHMARK (GBP)' TO RVR-T-LABEL
           MOVE WS-TOTAL-BENCH-REMAIN  TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'SHORTFALL ON UNDER-RESERVED EXPOSURES (GBP)'
                                       TO RVR-T-LABEL
           MOVE WS-UNDER-SHORTFALL     TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL
           MOVE 'RETURN CODE'          TO RVR-T-LABEL
           MOVE WS-MAX-RC              TO RVR-T-VALUE
           PERFORM 6550-WRITE-TOTAL.

      ******************************************************************
       6550-WRITE-TOTAL.
           MOVE RVR-TOT-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       6900-ABORT-RUN.
           PERFORM 6100-WRITE-MSG
           EXEC SQL ROLLBACK END-EXEC
           MOVE 16                     TO WS-MAX-RC
           PERFORM 9000-RETURN.

      ******************************************************************
       7000-COMMIT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-UNCOMMITTED.

      ******************************************************************
       7100-WRITE-CHECKPOINT.
           MOVE WS-PROGRAM-ID          TO RVK-PROGRAM
           IF NOT RVK-RUN-COMPLETE
              MOVE 'R'                 TO RVK-RUN-STATUS
           END-IF
           MOVE WS-BUS-DATE            TO RVK-BUS-DATE
           MOVE WS-REVIEWED            TO RVK-REVIEWED
           MOVE WS-ACTS-CREATED        TO RVK-ACTIVITIES
           MOVE FUNCTION CURRENT-DATE  TO RVK-CHECKPOINT-TS
           CLOSE CHK-FILE
           OPEN OUTPUT CHK-FILE
           WRITE RVK-CHECKPOINT-REC
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'CLMRSVRP CHECKPOINT WRITE FAILED ' WS-CHK-STATUS
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF.

      ******************************************************************
       8100-DAYS-TO-BUS-DATE.
           INITIALIZE DTA-HEADER
           MOVE 'DTDIFF  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE DTA-DT-DATE-2
           MOVE WS-START-DATE          TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              MOVE EXP-EXPOSURE-ID     TO WS-KEY-DISP
              STRING 'CLMDATES DTDIFF failed for exposure '
                     WS-KEY-DISP ' - ' DTA-REASON-CODE
                     DELIMITED BY SIZE INTO RVR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           IF DTA-DT-RESULT-NUM < 0
              MOVE 0                   TO DTA-DT-RESULT-NUM
           END-IF.

      ******************************************************************
       8200-CREATE-ACTIVITY.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTNEW  '             TO AVA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO AVA-USER-ID AVA-CALLER-PGM
           MOVE WS-BUS-DATE            TO AVA-BUS-DATE
           MOVE EXP-CLAIM-ID           TO AVA-AV-CLAIM-ID
           MOVE EXP-EXPOSURE-ID        TO AVA-AV-EXPOSURE-ID
           MOVE 'RSVREV  '             TO AVA-AV-PATTERN-CODE
           IF EXP-ASSIGNED-USER NOT = SPACES
              MOVE EXP-ASSIGNED-USER   TO AVA-AV-ASSIGNED-USER
           ELSE
              MOVE CLM-ASSIGNED-USER   TO AVA-AV-ASSIGNED-USER
           END-IF
           MOVE SPACES                 TO WS-SUBJECT
           STRING 'Reserve review: ' WS-FLAGS
                  DELIMITED BY SIZE INTO WS-SUBJECT
           MOVE WS-SUBJECT             TO AVA-AV-SUBJECT
           CALL WS-ACT-PGM USING WS-ACT-CA
           EVALUATE TRUE
              WHEN AVA-RC-OK
              WHEN AVA-RC-WARNING
                 ADD 1                 TO WS-ACTS-CREATED
                 MOVE 'CREATED'        TO RVR-D-ACTIVITY
              WHEN AVA-RETURN-CODE < 90
                 ADD 1                 TO WS-ACTS-FAILED
                 MOVE AVA-REASON-CODE  TO RVR-D-ACTIVITY
                 IF WS-MAX-RC < 8
                    MOVE 8             TO WS-MAX-RC
                 END-IF
              WHEN OTHER
                 MOVE AVA-MESSAGE      TO RVR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
           END-EVALUATE.

      ******************************************************************
      * Average payments per 30 days = paid * 30 / days open
       8300-PAYMENT-VELOCITY.
           INITIALIZE WS-MNY-CA
           MOVE 'MNPRORA '             TO MNA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO MNA-USER-ID MNA-CALLER-PGM
           MOVE WS-BUS-DATE            TO MNA-BUS-DATE
           MOVE EXP-TOTAL-PAID         TO MNA-MN-TOTAL
           MOVE WS-VELOCITY-DAYS       TO MNA-MN-DAYS-PART
           MOVE WS-DAYS-OPEN           TO MNA-MN-DAYS-WHOLE
           CALL WS-MNY-PGM USING WS-MNY-CA
           IF NOT MNA-RC-OK
              MOVE MNA-MESSAGE         TO RVR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE MNA-MN-RESULT          TO WS-VELOCITY.

      ******************************************************************
       9000-RETURN.
           CLOSE RPT-FILE
           CLOSE CHK-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           MOVE EXP-EXPOSURE-ID        TO WS-KEY-DISP
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-KEY-DISP            TO ERC-ER-KEY
           STRING 'CLMRSVRP Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           EXEC SQL ROLLBACK END-EXEC
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO RVR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 12                     TO WS-MAX-RC
           PERFORM 9000-RETURN.
