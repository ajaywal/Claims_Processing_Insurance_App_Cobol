       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMESCAL.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMESCAL                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Nightly activity escalation sweep.               *
      *               1. OPEN activities past their escalation date    *
      *                  and not yet escalated are flagged, raised to  *
      *                  priority 1 and reassigned to the owner's      *
      *                  supervisor, with a claim history row.         *
      *               2. Overdue (past due date) open activities are   *
      *                  counted per user for the workload report.     *
      *               3. OPEN claims with no activity created or       *
      *                  completed in the aged window get a CLOSEREV   *
      *                  or RSVREV review activity.                    *
      * FILES       : CHKPT   - checkpoint / restart (CLMESCCK, 80)    *
      *               SYSIN   - control card (CLMESCCC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : phase E after EK-LAST-KEY (activity id), phase A *
      *               after EK-LAST-KEY (claim id)                     *
      * CALLS       : CLMACTV0 ACTNEW  CLMHIST0 HISADD                 *
      *               CLMDATES DTADDD  CLMERRLG ERRLOG                 *
      * TABLES      : CLM_ACTIVITY (R U)  CLM_USER (R)  CLM_GROUP (R)  *
      *               CLM_CLAIM (R)  CLM_CONTROL (R)                   *
      * RULES       : BR-CLM-ESC-001 Escalate OPEN activities whose    *
      *                              escalation date has passed and    *
      *                              that are not already escalated    *
      *               BR-CLM-ESC-002 New owner is the user's           *
      *                              supervisor, else the group        *
      *                              supervisor; must be active and    *
      *                              not the owner, else flag in place *
      *               BR-CLM-ESC-003 Escalated activities become       *
      *                              priority 1                        *
      *               BR-CLM-ESC-004 Overdue = OPEN with due date      *
      *                              before the business date; users   *
      *                              at or over the alert level are    *
      *                              flagged to team leaders           *
      *               BR-CLM-ESC-005 Aged claim = OPEN, opened before  *
      *                              the window, no activity created   *
      *                              or completed inside it and no     *
      *                              open review already               *
      *               BR-CLM-ESC-006 Aged claim with nil reserves gets *
      *                              CLOSEREV, otherwise RSVREV        *
      * RETURN CODE : 0 ok, 4 escalations left unrouted, 8 review      *
      *               activity not created, 16 Db2 or service failure  *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHKPT-FILE    ASSIGN TO CHKPT
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-CHK.
           SELECT CTL-CARD      ASSIGN TO SYSIN
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-CTL.
           SELECT RPT-OUT       ASSIGN TO RPTOUT
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-RPT.
       DATA DIVISION.
       FILE SECTION.
       FD  CHKPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMESCCK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMESCCC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMESCAL'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-CUTOFF-DATE           PIC X(10).
           05 WS-AGED-DAYS             PIC S9(5) COMP-3 VALUE 30.
           05 WS-OVERDUE-ALERT         PIC S9(5) COMP-3 VALUE 10.
           05 WS-COMMIT-FREQ           PIC S9(5) COMP-3 VALUE 200.
           05 WS-START-PHASE           PIC X     VALUE 'E'.
           05 WS-RESTART-ACT-ID        PIC S9(9) COMP   VALUE 0.
           05 WS-RESTART-CLAIM-ID      PIC S9(9) COMP   VALUE 0.
           05 WS-LAST-KEY              PIC S9(9) COMP   VALUE 0.
           05 WS-UNCOMMITTED           PIC S9(5) COMP-3 VALUE 0.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-ESC-SELECTED          PIC S9(9) COMP-3 VALUE 0.
           05 WS-ESCALATED             PIC S9(9) COMP-3 VALUE 0.
           05 WS-ESC-UNROUTED          PIC S9(9) COMP-3 VALUE 0.
           05 WS-ESC-RACED             PIC S9(9) COMP-3 VALUE 0.
           05 WS-OVD-USERS             PIC S9(9) COMP-3 VALUE 0.
           05 WS-OVD-ACTIVITIES        PIC S9(9) COMP-3 VALUE 0.
           05 WS-OVD-ALERTS            PIC S9(9) COMP-3 VALUE 0.
           05 WS-AGED-SELECTED         PIC S9(9) COMP-3 VALUE 0.
           05 WS-REVIEWS-RAISED        PIC S9(9) COMP-3 VALUE 0.
           05 WS-REVIEWS-FAILED        PIC S9(9) COMP-3 VALUE 0.

       01  WS-ROW-WORK.
           05 WS-CLAIM-NUMBER          PIC X(12).
           05 WS-NEW-OWNER             PIC X(8).
           05 WS-GRP-SUPERVISOR        PIC X(8).
           05 WS-GRP-SUP-IND           PIC S9(4) COMP.
           05 WS-SUP-ACTIVE            PIC X.
           05 WS-NOTE                  PIC X(61).
           05 WS-OVD-COUNT             PIC S9(9) COMP.
           05 WS-OVD-OLDEST            PIC X(10).
           05 WS-PATTERN               PIC X(8).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).

           COPY CLMESCCK REPLACING LEADING ==EK-== BY ==WK-==.

           COPY CLMESCCC REPLACING LEADING ==EC-== BY ==WC-==.

           COPY CLMESCRP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDACT END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE ESC-CSR CURSOR WITH HOLD FOR
              SELECT A.ACTIVITY_ID, A.CLAIM_ID, A.SUBJECT,
                     A.ASSIGNED_USER, A.DUE_DATE, A.ESCALATION_DATE,
                     C.CLAIM_NUMBER
                FROM CLMDB.CLM_ACTIVITY A
                     INNER JOIN CLMDB.CLM_CLAIM C
                        ON C.CLAIM_ID = A.CLAIM_ID
               WHERE A.ACTIVITY_STATUS = 'OPEN'
                 AND A.ESCALATION_DATE < :WS-BUS-DATE
                 AND A.ESCALATED_FLAG  = 'N'
                 AND A.ACTIVITY_ID     > :WS-RESTART-ACT-ID
               ORDER BY A.ACTIVITY_ID
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE OVD-CSR CURSOR FOR
              SELECT A.ASSIGNED_USER, COUNT(*), MIN(A.DUE_DATE)
                FROM CLMDB.CLM_ACTIVITY A
               WHERE A.ACTIVITY_STATUS = 'OPEN'
                 AND A.DUE_DATE        < :WS-BUS-DATE
               GROUP BY A.ASSIGNED_USER
               ORDER BY 2 DESC, 1
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE AGE-CSR CURSOR WITH HOLD FOR
              SELECT C.CLAIM_ID, C.CLAIM_NUMBER, C.ASSIGNED_USER,
                     C.TOTAL_RESERVES, C.OPEN_DATE
                FROM CLMDB.CLM_CLAIM C
               WHERE C.CLAIM_STATE = 'OPEN'
                 AND C.OPEN_DATE  <= :WS-CUTOFF-DATE
                 AND C.CLAIM_ID    > :WS-RESTART-CLAIM-ID
                 AND NOT EXISTS
                     (SELECT 1
                        FROM CLMDB.CLM_ACTIVITY A
                       WHERE A.CLAIM_ID = C.CLAIM_ID
                         AND (DATE(A.CREATE_TS) > :WS-CUTOFF-DATE
                          OR  DATE(A.COMPLETE_TS) > :WS-CUTOFF-DATE
                          OR (A.ACTIVITY_STATUS = 'OPEN'
                              AND A.PATTERN_CODE IN ('CLOSEREV',
                                                     'RSVREV  '))))
               ORDER BY C.CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.
       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF WS-START-PHASE = 'E'
              PERFORM 2000-ESCALATE-ACTIVITIES
           END-IF
           PERFORM 2300-OVERDUE-BY-USER
           PERFORM 2500-AGED-CLAIM-REVIEW
           PERFORM 2900-FINISH
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMESCAL RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO ER-M-TEXT WS-ABEND-TEXT
           OPEN INPUT CTL-CARD
           MOVE SPACES                 TO WC-CARD
           IF WS-FS-CTL = '00'
              READ CTL-CARD INTO WC-CARD
                 AT END MOVE SPACES    TO WC-CARD
              END-READ
              CLOSE CTL-CARD
           END-IF
           IF WC-COMMIT-FREQ-X IS NUMERIC AND WC-COMMIT-FREQ > 0
              MOVE WC-COMMIT-FREQ      TO WS-COMMIT-FREQ
           END-IF
           IF WC-AGED-DAYS-X IS NUMERIC AND WC-AGED-DAYS > 0
              MOVE WC-AGED-DAYS        TO WS-AGED-DAYS
           END-IF
           IF WC-OVERDUE-ALERT-X IS NUMERIC AND WC-OVERDUE-ALERT > 0
              MOVE WC-OVERDUE-ALERT    TO WS-OVERDUE-ALERT
           END-IF
           PERFORM 4000-GET-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO ER-H1-BUS-DATE
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM
           MOVE STH-HEADER             TO DTA-HEADER
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-1
           COMPUTE DTA-DT-NUMBER = 0 - WS-AGED-DAYS
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              STRING 'CLMDATES DTADDD failed ' DTA-REASON-CODE
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE DTA-DT-RESULT-DATE     TO WS-CUTOFF-DATE
           PERFORM 1100-RESTART-POSITION.

      ******************************************************************
       1100-RESTART-POSITION.
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
           ELSE
              OPEN INPUT CHKPT-FILE
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE EK-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              EVALUATE TRUE
                 WHEN WK-PHASE-ESCALATE
                    MOVE WK-LAST-KEY   TO WS-RESTART-ACT-ID
                 WHEN WK-PHASE-AGED
                    MOVE 'A'           TO WS-START-PHASE
                    MOVE WK-LAST-KEY   TO WS-RESTART-CLAIM-ID
                 WHEN OTHER
                    MOVE 'Restart requested but no in-flight checkpoint'
                                       TO WS-ABEND-TEXT
                    PERFORM 9800-ABEND
              END-EVALUATE
              MOVE WK-ESCALATED        TO WS-ESCALATED
              MOVE WK-REVIEWS-RAISED   TO WS-REVIEWS-RAISED
              OPEN EXTEND CHKPT-FILE
              MOVE WK-LAST-KEY         TO WS-DISP-ID
              STRING 'Restart in phase ' WK-PHASE ' after key '
                     WS-DISP-ID DELIMITED BY SIZE INTO ER-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       2000-ESCALATE-ACTIVITIES.
           MOVE 'ESCALATED ACTIVITIES (OPEN, PAST ESCALATION DATE)'
                                       TO ER-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           EXEC SQL OPEN ESC-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-ESCALATE-ACTIVITIES' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF-SW
           PERFORM 4100-FETCH-ESCALATION
           PERFORM UNTIL WS-EOF
              PERFORM 2100-ESCALATE-ONE
              MOVE ACT-ACTIVITY-ID     TO WS-LAST-KEY
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 SET WK-PHASE-ESCALATE TO TRUE
                 PERFORM 4900-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4100-FETCH-ESCALATION
           END-PERFORM
           EXEC SQL CLOSE ESC-CSR END-EXEC
           SET WK-PHASE-AGED           TO TRUE
           MOVE 0                      TO WS-LAST-KEY
           PERFORM 4900-COMMIT-CHECKPOINT.

      ******************************************************************
      * BR-CLM-ESC-001 Selected by ESC-CSR: OPEN, past escalation date,
      * not yet escalated.
       2100-ESCALATE-ONE.
           ADD 1                       TO WS-ESC-SELECTED
           MOVE SPACES                 TO WS-NOTE
           PERFORM 4200-RESOLVE-SUPERVISOR
           IF WS-NEW-OWNER = ACT-ASSIGNED-USER
              ADD 1                    TO WS-ESC-UNROUTED
              MOVE 'No active supervisor' TO WS-NOTE
           END-IF
           PERFORM 4300-UPDATE-ACTIVITY
           IF SQLERRD(3) = 0
              ADD 1                    TO WS-ESC-RACED
              MOVE 'Closed/escalated meanwhile' TO WS-NOTE
              MOVE ACT-ASSIGNED-USER   TO WS-NEW-OWNER
           ELSE
              ADD 1                    TO WS-ESCALATED
              PERFORM 8200-WRITE-HISTORY
           END-IF
           MOVE ACT-ACTIVITY-ID        TO ER-E-ACTIVITY-ID
           MOVE WS-CLAIM-NUMBER        TO ER-E-CLAIM-NUMBER
           MOVE ACT-SUBJECT-TEXT       TO ER-E-SUBJECT
           MOVE ACT-DUE-DATE           TO ER-E-DUE-DATE
           MOVE ACT-ESCALATION-DATE    TO ER-E-ESC-DATE
           MOVE ACT-ASSIGNED-USER      TO ER-E-FROM-USER
           MOVE WS-NEW-OWNER           TO ER-E-TO-USER
           MOVE WS-NOTE                TO ER-E-NOTE
           MOVE ER-ESC-DETAIL          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
      * BR-CLM-ESC-004 Overdue workload per user (read only)
       2300-OVERDUE-BY-USER.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'OVERDUE OPEN ACTIVITIES BY USER (DUE BEFORE TODAY)'
                                       TO ER-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           EXEC SQL OPEN OVD-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-OVERDUE-BY-USER' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF-SW
           PERFORM 4400-FETCH-OVERDUE
           PERFORM UNTIL WS-EOF
              ADD 1                    TO WS-OVD-USERS
              ADD WS-OVD-COUNT         TO WS-OVD-ACTIVITIES
              PERFORM 4450-READ-USER-NAME
              MOVE USR-USER-ID         TO ER-O-USER
              MOVE USR-USER-NAME-TEXT  TO ER-O-USER-NAME
              MOVE WS-OVD-COUNT        TO ER-O-COUNT
              MOVE WS-OVD-OLDEST       TO ER-O-OLDEST
              MOVE SPACES              TO ER-O-ALERT
              IF WS-OVD-COUNT >= WS-OVERDUE-ALERT
                 ADD 1                 TO WS-OVD-ALERTS
                 MOVE '*** WORKLOAD ALERT' TO ER-O-ALERT
              END-IF
              MOVE ER-OVD-DETAIL       TO RPT-RECORD
              PERFORM 6100-PRINT-LINE
              PERFORM 4400-FETCH-OVERDUE
           END-PERFORM
           EXEC SQL CLOSE OVD-CSR END-EXEC.

      ******************************************************************
       2500-AGED-CLAIM-REVIEW.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           STRING 'AGED CLAIMS - NO ACTIVITY SINCE ' WS-CUTOFF-DATE
                  DELIMITED BY SIZE INTO ER-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           EXEC SQL OPEN AGE-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-AGED-CLAIM-REVIEW' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF-SW
           PERFORM 4500-FETCH-AGED
           PERFORM UNTIL WS-EOF
              PERFORM 2600-RAISE-REVIEW
              MOVE CLM-CLAIM-ID        TO WS-LAST-KEY
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 SET WK-PHASE-AGED     TO TRUE
                 PERFORM 4900-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4500-FETCH-AGED
           END-PERFORM
           EXEC SQL CLOSE AGE-CSR END-EXEC.

      ******************************************************************
      * BR-CLM-ESC-005 Selected by AGE-CSR
      * BR-CLM-ESC-006 Nil reserves -> closure review, else reserve
       2600-RAISE-REVIEW.
           ADD 1                       TO WS-AGED-SELECTED
           IF CLM-TOTAL-RESERVES = 0
              MOVE 'CLOSEREV'          TO WS-PATTERN
           ELSE
              MOVE 'RSVREV  '          TO WS-PATTERN
           END-IF
           INITIALIZE AVA-BODY
           MOVE STH-HEADER             TO AVA-HEADER
           MOVE 'ACTNEW  '             TO AVA-REQUEST-ID
           MOVE CLM-CLAIM-ID           TO AVA-AV-CLAIM-ID
           MOVE WS-PATTERN             TO AVA-AV-PATTERN-CODE
           MOVE CLM-ASSIGNED-USER      TO AVA-AV-ASSIGNED-USER
           CALL WS-ACT-PGM USING WS-ACT-CA
           EVALUATE TRUE
              WHEN AVA-RC-OK
              WHEN AVA-RC-WARNING
                 ADD 1                 TO WS-REVIEWS-RAISED
                 MOVE 'Review activity created' TO WS-NOTE
              WHEN AVA-RETURN-CODE >= 90
                 STRING 'CLMACTV0 ACTNEW failed ' AVA-REASON-CODE
                        ' ' AVA-MESSAGE
                        DELIMITED BY SIZE INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              WHEN OTHER
                 ADD 1                 TO WS-REVIEWS-FAILED
                 MOVE 0                TO AVA-AV-ACTIVITY-ID
                 STRING AVA-REASON-CODE ' ' AVA-MESSAGE
                        DELIMITED BY SIZE INTO WS-NOTE
           END-EVALUATE
           MOVE CLM-CLAIM-NUMBER       TO ER-A-CLAIM-NUMBER
           MOVE CLM-OPEN-DATE          TO ER-A-OPEN-DATE
           MOVE CLM-ASSIGNED-USER      TO ER-A-USER
           MOVE CLM-TOTAL-RESERVES     TO ER-A-RESERVES
           MOVE WS-PATTERN             TO ER-A-PATTERN
           MOVE AVA-AV-ACTIVITY-ID     TO ER-A-ACTIVITY-ID
           MOVE WS-NOTE                TO ER-A-NOTE
           MOVE ER-AGED-DETAIL         TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                 TO WS-NOTE.

      ******************************************************************
       2900-FINISH.
           SET WK-PHASE-END            TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           IF WS-ESC-UNROUTED > 0 AND WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF
           IF WS-REVIEWS-FAILED > 0 AND WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF
           PERFORM 6500-PRINT-TOTALS.

      ******************************************************************
       4000-GET-BUSINESS-DATE.
           IF WC-BUS-DATE NOT = SPACES
              MOVE WC-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4000-GET-BUSINESS-DATE' TO WS-ERR-PARAGRAPH
                 MOVE CTL-CONTROL-KEY  TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
       4100-FETCH-ESCALATION.
           EXEC SQL
              FETCH ESC-CSR
               INTO :ACT-ACTIVITY-ID, :ACT-CLAIM-ID, :ACT-SUBJECT,
                    :ACT-ASSIGNED-USER, :ACT-DUE-DATE,
                    :ACT-ESCALATION-DATE, :WS-CLAIM-NUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF ACT-SUBJECT-LEN < 60
                    MOVE SPACES TO ACT-SUBJECT-TEXT
                                   (ACT-SUBJECT-LEN + 1:)
                 END-IF
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-ESCALATION' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-ESC-002 User supervisor, else group supervisor; the
      * target must be an active user other than the current owner.
       4200-RESOLVE-SUPERVISOR.
           MOVE ACT-ASSIGNED-USER      TO WS-NEW-OWNER
           MOVE ACT-ASSIGNED-USER      TO USR-USER-ID
           EXEC SQL
              SELECT U.SUPERVISOR_ID, G.SUPERVISOR_ID
                INTO :USR-SUPERVISOR-ID,
                     :WS-GRP-SUPERVISOR :WS-GRP-SUP-IND
                FROM CLMDB.CLM_USER U
                     LEFT OUTER JOIN CLMDB.CLM_GROUP G
                        ON G.GROUP_CODE = U.GROUP_CODE
               WHERE U.USER_ID = :USR-USER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF USR-SUPERVISOR-ID = SPACES
                    AND WS-GRP-SUP-IND >= 0
                    MOVE WS-GRP-SUPERVISOR TO USR-SUPERVISOR-ID
                 END-IF
              WHEN 100
                 MOVE SPACES           TO USR-SUPERVISOR-ID
              WHEN OTHER
                 MOVE '4200-RESOLVE-SUPERVISOR' TO WS-ERR-PARAGRAPH
                 MOVE USR-USER-ID      TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF USR-SUPERVISOR-ID NOT = SPACES
              AND USR-SUPERVISOR-ID NOT = ACT-ASSIGNED-USER
              MOVE USR-SUPERVISOR-ID   TO USR-USER-ID
              EXEC SQL
                 SELECT ACTIVE_FLAG
                   INTO :WS-SUP-ACTIVE
                   FROM CLMDB.CLM_USER
                  WHERE USER_ID = :USR-USER-ID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF WS-SUP-ACTIVE = 'Y'
                       MOVE USR-USER-ID TO WS-NEW-OWNER
                    END-IF
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '4200-RESOLVE-SUPERVISOR' TO WS-ERR-PARAGRAPH
                    MOVE USR-USER-ID   TO WS-ERR-KEY
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
      * BR-CLM-ESC-003 Escalated work goes to priority 1. Guarded on
      * OPEN / not escalated in case the handler acted meanwhile.
       4300-UPDATE-ACTIVITY.
           EXEC SQL
              UPDATE CLMDB.CLM_ACTIVITY
                 SET ESCALATED_FLAG = 'Y',
                     ASSIGNED_USER  = :WS-NEW-OWNER,
                     PRIORITY       = '1'
               WHERE ACTIVITY_ID     = :ACT-ACTIVITY-ID
                 AND ACTIVITY_STATUS = 'OPEN'
                 AND ESCALATED_FLAG  = 'N'
           END-EXEC
           IF SQLCODE NOT = 0 AND NOT = 100
              MOVE '4300-UPDATE-ACTIVITY' TO WS-ERR-PARAGRAPH
              MOVE ACT-ACTIVITY-ID     TO WS-DISP-ID
              MOVE WS-DISP-ID          TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-FETCH-OVERDUE.
           EXEC SQL
              FETCH OVD-CSR
               INTO :USR-USER-ID, :WS-OVD-COUNT, :WS-OVD-OLDEST
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4400-FETCH-OVERDUE' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4450-READ-USER-NAME.
           EXEC SQL
              SELECT USER_NAME
                INTO :USR-USER-NAME
                FROM CLMDB.CLM_USER
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF USR-USER-NAME-LEN < 40
                    MOVE SPACES TO USR-USER-NAME-TEXT
                                   (USR-USER-NAME-LEN + 1:)
                 END-IF
              WHEN 100
                 MOVE '(unknown user)' TO USR-USER-NAME-TEXT
              WHEN OTHER
                 MOVE '4450-READ-USER-NAME' TO WS-ERR-PARAGRAPH
                 MOVE USR-USER-ID      TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-FETCH-AGED.
           EXEC SQL
              FETCH AGE-CSR
               INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                    :CLM-ASSIGNED-USER, :CLM-TOTAL-RESERVES,
                    :CLM-OPEN-DATE :CLM-OPEN-DATE-IND
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4500-FETCH-AGED' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4900-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-COMMIT-CHECKPOINT' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-UNCOMMITTED
           MOVE WS-PROGRAM-ID          TO WK-PROGRAM
           MOVE WS-BUS-DATE            TO WK-BUS-DATE
           MOVE WS-LAST-KEY            TO WK-LAST-KEY
           MOVE WS-ESCALATED           TO WK-ESCALATED
           MOVE WS-REVIEWS-RAISED      TO WK-REVIEWS-RAISED
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE EK-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       8200-WRITE-HISTORY.
           INITIALIZE HIA-BODY
           MOVE STH-HEADER             TO HIA-HEADER
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE ACT-CLAIM-ID           TO HIA-HI-CLAIM-ID
           MOVE 'ACTESCAL'             TO HIA-HI-EVENT-TYPE
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           MOVE ACT-ACTIVITY-ID        TO WS-DISP-ID
           STRING 'Activity ' WS-DISP-ID ' escalated from '
                  ACT-ASSIGNED-USER ' to ' WS-NEW-OWNER
                  ' (escalation date ' ACT-ESCALATION-DATE ')'
                  DELIMITED BY SIZE INTO HIA-HI-DESCRIPTION
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF NOT HIA-RC-OK
              STRING 'CLMHIST0 HISADD failed ' HIA-REASON-CODE ' '
                     HIA-MESSAGE DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       6100-PRINT-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              MOVE RPT-RECORD          TO WS-HOLD-LINE
              ADD 1                    TO WS-PAGE-NO
              MOVE WS-PAGE-NO          TO ER-H1-PAGE
              WRITE RPT-RECORD FROM ER-HEAD-1 AFTER ADVANCING PAGE
              MOVE SPACES              TO RPT-RECORD
              WRITE RPT-RECORD AFTER ADVANCING 1 LINE
              MOVE 3                   TO WS-LINE-COUNT
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6200-PRINT-MESSAGE.
           MOVE ER-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                 TO ER-M-TEXT.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO ER-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           MOVE 'Activities selected for escalation' TO ER-T-LABEL
           MOVE WS-ESC-SELECTED        TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Activities escalated (cumulative on restart)'
                                       TO ER-T-LABEL
           MOVE WS-ESCALATED           TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE '  of which left with owner (no supervisor)'
                                       TO ER-T-LABEL
           MOVE WS-ESC-UNROUTED        TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Skipped - changed since selection' TO ER-T-LABEL
           MOVE WS-ESC-RACED           TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Users with overdue activities' TO ER-T-LABEL
           MOVE WS-OVD-USERS           TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Overdue open activities' TO ER-T-LABEL
           MOVE WS-OVD-ACTIVITIES      TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Users at workload alert level' TO ER-T-LABEL
           MOVE WS-OVD-ALERTS          TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Aged claims selected' TO ER-T-LABEL
           MOVE WS-AGED-SELECTED       TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Review activities raised (cumulative)' TO ER-T-LABEL
           MOVE WS-REVIEWS-RAISED      TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Review activities failed' TO ER-T-LABEL
           MOVE WS-REVIEWS-FAILED      TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Return code'          TO ER-T-LABEL
           MOVE WS-RETURN-CODE         TO ER-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE ER-TOTAL-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       9000-TERMINATE.
           CLOSE CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           IF WS-RETURN-CODE < 16
              MOVE 16                  TO WS-RETURN-CODE
           END-IF
           STRING '*** RUN ABANDONED: ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO ER-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           DISPLAY 'CLMESCAL ' WS-ABEND-TEXT
           PERFORM 6500-PRINT-TOTALS
           CLOSE CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           EXEC SQL ROLLBACK END-EXEC
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARAGRAPH       TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'CLMESCAL Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
