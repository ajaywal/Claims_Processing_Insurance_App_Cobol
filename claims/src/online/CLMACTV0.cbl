       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMACTV0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMACTV0                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim activity (diary) engine. Creates single    *
      *               activities from an activity pattern, generates   *
      *               every pattern bound to a workflow trigger event, *
      *               completes and skips activities under assignee /  *
      *               supervisor authority and lists work queues.      *
      * REQUESTS    : ACTNEW ACTGEN ACTCMP ACTSKP ACTLST               *
      * CALLED BY   : claim, exposure, financial, litigation and SIU   *
      *               services; activity screens; escalation batch     *
      * CALLS       : CLMSEQGN CLMDATES CLMHIST0 CLMERRLG              *
      * TABLES      : CLM_ACTIVITY (C R U)  CLM_ACTIVITY_PATTERN (R)   *
      *               CLM_CLAIM (R)  CLM_EXPOSURE (R)  CLM_USER (R)    *
      *               CLM_TRANSACTION (R)  CLM_CONTROL (R)             *
      * RULES       : BR-CLM-ACT-001 Activities cannot be raised on a  *
      *                              closed claim                      *
      *               BR-CLM-ACT-002 Pattern loss type must be the     *
      *                              claim loss type or '*'            *
      *               BR-CLM-ACT-003 Due date = business date plus     *
      *                              TARGET_DAYS business days         *
      *               BR-CLM-ACT-004 Escalation date = due date plus   *
      *                              ESCALATION_DAYS business days     *
      *               BR-CLM-ACT-005 Assignee defaults to the claim    *
      *                              owner, else the requesting user;  *
      *                              assignee must be an active user   *
      *               BR-CLM-ACT-006 Exposure must belong to the claim *
      *                              and must not be closed            *
      *               BR-CLM-ACT-007 Generation skips a pattern that   *
      *                              already has an OPEN activity on   *
      *                              the same claim and exposure       *
      *               BR-CLM-ACT-008 Only the assignee or the          *
      *                              assignee's supervisor may         *
      *                              complete or skip an activity      *
      *               BR-CLM-ACT-009 Approval activities stay open     *
      *                              while their transaction is PAPR   *
      *               BR-CLM-ACT-010 Mandatory activities cannot be    *
      *                              skipped                           *
      *               BR-CLM-ACT-011 Only OPEN activities can be       *
      *                              completed or skipped              *
      *               BR-CLM-ACT-012 User queue lists OPEN activities  *
      *                              by due date then priority; claim  *
      *                              list shows OPEN first             *
      *               BR-CLM-ACT-013 Approval transaction must belong  *
      *                              to the same claim                 *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-WORK-DATE             PIC X(10) VALUE SPACES.
           05 WS-DUE-DATE              PIC X(10) VALUE SPACES.
           05 WS-ESC-DATE              PIC X(10) VALUE SPACES.
           05 WS-DAYS                  PIC S9(4) COMP VALUE 0.
           05 WS-DAY-IX                PIC S9(4) COMP VALUE 0.
           05 WS-OPEN-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-ROW-IX                PIC S9(4) COMP VALUE 0.
           05 WS-ASSIGNEE              PIC X(8)  VALUE SPACES.
           05 WS-LIST-USER             PIC X(8)  VALUE SPACES.
           05 WS-TXN-CLAIM-ID          PIC S9(9) COMP VALUE 0.
           05 WS-TXN-STATUS            PIC X(4)  VALUE SPACES.
           05 WS-EXP-CLAIM-ID          PIC S9(9) COMP VALUE 0.
           05 WS-EXP-STATE             PIC X(4)  VALUE SPACES.
           05 WS-SUBJECT               PIC X(60) VALUE SPACES.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-COUNT-DISP            PIC Z(3)9.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-HIS-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-DATE-REQ              PIC X(8)  VALUE SPACES.
           05 WS-DATE-NUM              PIC S9(5) COMP-3 VALUE 0.

       01  WS-FLAGS.
           05 WS-LIST-MODE             PIC X     VALUE 'U'.
              88 WS-LIST-BY-USER                 VALUE 'U'.
              88 WS-LIST-BY-CLAIM                VALUE 'C'.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-DONE                   VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.
           05 WS-CSR-PAT-SW            PIC X     VALUE 'N'.
              88 WS-CSR-PAT-OPEN                 VALUE 'Y'.
           05 WS-CSR-USR-SW            PIC X     VALUE 'N'.
              88 WS-CSR-USR-OPEN                 VALUE 'Y'.
           05 WS-CSR-CLM-SW            PIC X     VALUE 'N'.
              88 WS-CSR-CLM-OPEN                 VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDACT END-EXEC.
           EXEC SQL INCLUDE CLMDAPT END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CSR-PATTERN CURSOR FOR
              SELECT PATTERN_CODE, SUBJECT, CATEGORY, TRIGGER_EVENT,
                     LOSS_TYPE, TARGET_DAYS, ESCALATION_DAYS,
                     PRIORITY, MANDATORY_FLAG
                FROM CLMDB.CLM_ACTIVITY_PATTERN
               WHERE TRIGGER_EVENT = :CA-AV-EVENT
                 AND (LOSS_TYPE = :CLM-LOSS-TYPE OR LOSS_TYPE = '*')
               ORDER BY PATTERN_CODE
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-USRLST CURSOR FOR
              SELECT A.ACTIVITY_ID, C.CLAIM_NUMBER, A.SUBJECT,
                     A.PRIORITY, A.DUE_DATE, A.ACTIVITY_STATUS,
                     A.ESCALATED_FLAG
                FROM CLMDB.CLM_ACTIVITY A
                     INNER JOIN CLMDB.CLM_CLAIM C
                        ON C.CLAIM_ID = A.CLAIM_ID
               WHERE A.ASSIGNED_USER   = :WS-LIST-USER
                 AND A.ACTIVITY_STATUS = 'OPEN'
               ORDER BY A.DUE_DATE, A.PRIORITY, A.ACTIVITY_ID
               FETCH FIRST 21 ROWS ONLY
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-CLMLST CURSOR FOR
              SELECT A.ACTIVITY_ID, C.CLAIM_NUMBER, A.SUBJECT,
                     A.PRIORITY, A.DUE_DATE, A.ACTIVITY_STATUS,
                     A.ESCALATED_FLAG
                FROM CLMDB.CLM_ACTIVITY A
                     INNER JOIN CLMDB.CLM_CLAIM C
                        ON C.CLAIM_ID = A.CLAIM_ID
               WHERE A.CLAIM_ID = :CA-AV-CLAIM-ID
               ORDER BY CASE A.ACTIVITY_STATUS
                          WHEN 'OPEN' THEN 0 ELSE 1 END,
                        A.DUE_DATE, A.ACTIVITY_ID
               FETCH FIRST 21 ROWS ONLY
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-SVC-RESULT.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SVC-==.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCACT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'ACTNEW  '
                 PERFORM 2100-NEW-ACTIVITY
              WHEN 'ACTGEN  '
                 PERFORM 2200-GENERATE-ACTIVITIES
              WHEN 'ACTCMP  '
                 PERFORM 2300-COMPLETE-ACTIVITY
              WHEN 'ACTSKP  '
                 PERFORM 2400-SKIP-ACTIVITY
              WHEN 'ACTLST  '
                 PERFORM 2500-LIST-ACTIVITIES
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMACTV0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-AV-CREATED-COUNT
           PERFORM 1100-RESOLVE-BUS-DATE.

      ******************************************************************
       1100-RESOLVE-BUS-DATE.
           IF CA-BUS-DATE NOT = SPACES
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '1100-RESOLVE-BUS-DATE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * ACTNEW - one activity from a named pattern
       2100-NEW-ACTIVITY.
           PERFORM 3100-VALIDATE-NEW
           PERFORM 4100-READ-CLAIM
           PERFORM 3200-CHECK-CLAIM-OPEN
           MOVE CA-AV-PATTERN-CODE     TO APT-PATTERN-CODE
           PERFORM 4110-READ-PATTERN
           IF SQLCODE = 100
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'PATNOTFD'          TO CA-REASON-CODE
              STRING 'Activity pattern ' CA-AV-PATTERN-CODE
                     ' is not defined' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-ACT-002 Pattern loss type must match the claim
           IF APT-LOSS-TYPE NOT = '*'
              AND APT-LOSS-TYPE NOT = CLM-LOSS-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PATLOSS '          TO CA-REASON-CODE
              STRING 'Pattern ' CA-AV-PATTERN-CODE
                     ' is for loss type ' APT-LOSS-TYPE
                     ' not ' CLM-LOSS-TYPE DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-AV-EXPOSURE-ID > 0
              PERFORM 3300-CHECK-EXPOSURE
           END-IF
           IF CA-AV-APPROVAL-TXN-ID > 0
              PERFORM 3400-CHECK-APPROVAL-TXN
           END-IF
           PERFORM 5100-RESOLVE-ASSIGNEE
           IF CA-AV-SUBJECT NOT = SPACES
              MOVE CA-AV-SUBJECT       TO WS-SUBJECT
           ELSE
              MOVE APT-SUBJECT-TEXT(1:APT-SUBJECT-LEN)
                                       TO WS-SUBJECT
           END-IF
           PERFORM 2900-CREATE-ACTIVITY
           MOVE 1                      TO CA-AV-CREATED-COUNT
           MOVE ACT-ACTIVITY-ID        TO WS-ID-DISP
           STRING 'Activity ' WS-ID-DISP ' created, due '
                  WS-DUE-DATE DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * ACTGEN - every pattern bound to a trigger event
       2200-GENERATE-ACTIVITIES.
           IF CA-AV-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required to generate activities'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-AV-EVENT = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVENTREQ'          TO CA-REASON-CODE
              MOVE 'Trigger event is required to generate activities'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM
           PERFORM 3200-CHECK-CLAIM-OPEN
           IF CA-AV-EXPOSURE-ID > 0
              PERFORM 3300-CHECK-EXPOSURE
           END-IF
           PERFORM 5100-RESOLVE-ASSIGNEE
           PERFORM 4500-OPEN-PATTERN-CURSOR
           SET WS-FETCH-MORE           TO TRUE
           PERFORM UNTIL WS-FETCH-DONE
              PERFORM 4510-FETCH-PATTERN
              IF WS-FETCH-MORE
                 PERFORM 2210-GENERATE-ONE
              END-IF
           END-PERFORM
           PERFORM 4520-CLOSE-PATTERN-CURSOR
           MOVE CA-AV-CREATED-COUNT    TO WS-COUNT-DISP
           STRING WS-COUNT-DISP ' activities generated for event '
                  CA-AV-EVENT DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-CLM-ACT-007 Skip a pattern that is already open on the
      * same claim / exposure so re-fired events never duplicate work
       2210-GENERATE-ONE.
           PERFORM 4300-COUNT-OPEN-SAME
           IF WS-OPEN-COUNT = 0
              MOVE APT-SUBJECT-TEXT(1:APT-SUBJECT-LEN)
                                       TO WS-SUBJECT
              MOVE 0                   TO CA-AV-APPROVAL-TXN-ID
              PERFORM 2900-CREATE-ACTIVITY
              ADD 1                    TO CA-AV-CREATED-COUNT
              MOVE ACT-ACTIVITY-ID     TO CA-AV-ACTIVITY-ID
           END-IF.

      ******************************************************************
      * ACTCMP - complete an open activity
       2300-COMPLETE-ACTIVITY.
           PERFORM 3500-LOAD-OPEN-ACTIVITY
           PERFORM 3600-CHECK-ACTOR
           PERFORM 3700-CHECK-APPROVAL-CLEARED
           MOVE 'CMPL'                 TO ACT-ACTIVITY-STATUS
           PERFORM 4600-CLOSE-ACTIVITY
           MOVE 'ACTCMP  '             TO WS-HIS-EVENT
           MOVE ACT-ACTIVITY-ID        TO WS-ID-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Activity ' WS-ID-DISP ' completed: '
                  ACT-SUBJECT-TEXT(1:ACT-SUBJECT-LEN)
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           STRING 'Activity ' WS-ID-DISP ' completed'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * ACTSKP - skip an open, non-mandatory activity
       2400-SKIP-ACTIVITY.
           PERFORM 3500-LOAD-OPEN-ACTIVITY
           PERFORM 3600-CHECK-ACTOR
           MOVE ACT-PATTERN-CODE       TO APT-PATTERN-CODE
           PERFORM 4110-READ-PATTERN
      * BR-CLM-ACT-010 Mandatory activities cannot be skipped
           IF SQLCODE = 0 AND APT-MANDATORY-FLAG = 'Y'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTMAND '          TO CA-REASON-CODE
              STRING 'Activity pattern ' ACT-PATTERN-CODE
                     ' is mandatory and cannot be skipped'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3700-CHECK-APPROVAL-CLEARED
           MOVE 'SKIP'                 TO ACT-ACTIVITY-STATUS
           PERFORM 4600-CLOSE-ACTIVITY
           MOVE 'ACTSKP  '             TO WS-HIS-EVENT
           MOVE ACT-ACTIVITY-ID        TO WS-ID-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Activity ' WS-ID-DISP ' skipped: '
                  ACT-SUBJECT-TEXT(1:ACT-SUBJECT-LEN)
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           STRING 'Activity ' WS-ID-DISP ' skipped'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * ACTLST - user work queue or claim activity list
      * BR-CLM-ACT-012 User queue = OPEN by due date / priority;
      *                claim list = all, OPEN first, by due date
       2500-LIST-ACTIVITIES.
           MOVE 0                      TO CA-AV-ROW-COUNT
           INITIALIZE CA-AV-ROW(1)
           PERFORM VARYING WS-ROW-IX FROM 2 BY 1 UNTIL WS-ROW-IX > 20
              MOVE CA-AV-ROW(1)        TO CA-AV-ROW(WS-ROW-IX)
           END-PERFORM
           IF CA-AV-CLAIM-ID > 0
              SET WS-LIST-BY-CLAIM     TO TRUE
              PERFORM 4100-READ-CLAIM
              EXEC SQL OPEN CSR-CLMLST END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '2500-LIST-ACTIVITIES' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              SET WS-CSR-CLM-OPEN      TO TRUE
           ELSE
              SET WS-LIST-BY-USER      TO TRUE
              IF CA-AV-ASSIGNED-USER NOT = SPACES
                 MOVE CA-AV-ASSIGNED-USER TO WS-LIST-USER
              ELSE
                 MOVE CA-USER-ID       TO WS-LIST-USER
              END-IF
              EXEC SQL OPEN CSR-USRLST END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '2500-LIST-ACTIVITIES' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              SET WS-CSR-USR-OPEN      TO TRUE
           END-IF
           SET WS-FETCH-MORE           TO TRUE
           PERFORM UNTIL WS-FETCH-DONE
              PERFORM 4700-FETCH-LIST-ROW
              IF WS-FETCH-MORE
                 IF CA-AV-ROW-COUNT = 20
                    SET CA-RC-WARNING  TO TRUE
                    MOVE 'MOREROWS'    TO CA-REASON-CODE
                    MOVE 'More than 20 activities - refine the list'
                                       TO CA-MESSAGE
                    SET WS-FETCH-DONE  TO TRUE
                 ELSE
                    PERFORM 2510-MOVE-LIST-ROW
                 END-IF
              END-IF
           END-PERFORM
           PERFORM 4720-CLOSE-LIST-CURSOR
           IF CA-AV-ROW-COUNT = 0
              MOVE 'No activities found' TO CA-MESSAGE
           END-IF.

      ******************************************************************
       2510-MOVE-LIST-ROW.
           ADD 1                       TO CA-AV-ROW-COUNT
           MOVE CA-AV-ROW-COUNT        TO WS-ROW-IX
           MOVE ACT-ACTIVITY-ID        TO CA-AV-R-ID(WS-ROW-IX)
           MOVE CLM-CLAIM-NUMBER       TO
                                    CA-AV-R-CLAIM-NUMBER(WS-ROW-IX)
           MOVE SPACES                 TO CA-AV-R-SUBJECT(WS-ROW-IX)
           MOVE ACT-SUBJECT-TEXT(1:ACT-SUBJECT-LEN)
                                       TO CA-AV-R-SUBJECT(WS-ROW-IX)
           MOVE ACT-PRIORITY           TO CA-AV-R-PRIORITY(WS-ROW-IX)
           MOVE ACT-DUE-DATE           TO CA-AV-R-DUE(WS-ROW-IX)
           MOVE ACT-ACTIVITY-STATUS    TO CA-AV-R-STATUS(WS-ROW-IX)
           MOVE ACT-ESCALATED-FLAG     TO CA-AV-R-ESCALATED(WS-ROW-IX).

      ******************************************************************
      * Shared by ACTNEW / ACTGEN: dates, key, insert, history.
      * Expects APT- pattern fields, WS-SUBJECT and WS-ASSIGNEE.
       2900-CREATE-ACTIVITY.
           PERFORM 5200-CALC-DATES
           PERFORM 8000-NEXT-ACTIVITY-ID
           MOVE CA-AV-CLAIM-ID         TO ACT-CLAIM-ID
           MOVE CA-AV-EXPOSURE-ID      TO ACT-EXPOSURE-ID
           MOVE APT-PATTERN-CODE       TO ACT-PATTERN-CODE
           MOVE WS-SUBJECT             TO ACT-SUBJECT-TEXT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-SUBJECT TRAILING))
                                       TO ACT-SUBJECT-LEN
           MOVE APT-PRIORITY           TO ACT-PRIORITY
           MOVE 'OPEN'                 TO ACT-ACTIVITY-STATUS
           MOVE WS-ASSIGNEE            TO ACT-ASSIGNED-USER
           MOVE WS-DUE-DATE            TO ACT-DUE-DATE
           MOVE WS-ESC-DATE            TO ACT-ESCALATION-DATE
           MOVE 'N'                    TO ACT-ESCALATED-FLAG
           MOVE CA-AV-APPROVAL-TXN-ID  TO ACT-APPROVAL-TXN-ID
           PERFORM 4200-INSERT-ACTIVITY
           MOVE ACT-ACTIVITY-ID        TO CA-AV-ACTIVITY-ID
           MOVE WS-DUE-DATE            TO CA-AV-DUE-DATE
           MOVE WS-ASSIGNEE            TO CA-AV-ASSIGNED-USER
           MOVE 'ACTCRT  '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Activity ' APT-PATTERN-CODE ' for '
                  WS-ASSIGNEE ' due ' WS-DUE-DATE ': '
                  WS-SUBJECT DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY.

      ******************************************************************
       3100-VALIDATE-NEW.
           IF CA-AV-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for a new activity'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-AV-PATTERN-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PATREQ  '          TO CA-REASON-CODE
              MOVE 'Activity pattern code is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-001 No new activities on a closed claim
       3200-CHECK-CLAIM-OPEN.
           IF CLM-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLOSD'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER
                     ' is closed - reopen it before adding work'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-006 Exposure must belong to the claim and be open
       3300-CHECK-EXPOSURE.
           EXEC SQL
              SELECT CLAIM_ID, EXPOSURE_STATE
                INTO :WS-EXP-CLAIM-ID, :WS-EXP-STATE
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :CA-AV-EXPOSURE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO WS-EXP-CLAIM-ID
              WHEN OTHER
                 MOVE '3300-CHECK-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-EXP-CLAIM-ID NOT = CA-AV-CLAIM-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPNOTCL'          TO CA-REASON-CODE
              MOVE CA-AV-EXPOSURE-ID   TO WS-ID-DISP
              STRING 'Exposure ' WS-ID-DISP
                     ' does not belong to this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-EXP-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPCLOSD'          TO CA-REASON-CODE
              MOVE CA-AV-EXPOSURE-ID   TO WS-ID-DISP
              STRING 'Exposure ' WS-ID-DISP ' is closed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-013 Approval transaction must be on the same claim
       3400-CHECK-APPROVAL-TXN.
           EXEC SQL
              SELECT CLAIM_ID, TXN_STATUS
                INTO :WS-TXN-CLAIM-ID, :WS-TXN-STATUS
                FROM CLMDB.CLM_TRANSACTION
               WHERE TXN_ID = :CA-AV-APPROVAL-TXN-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO WS-TXN-CLAIM-ID
              WHEN OTHER
                 MOVE '3400-CHECK-APPROVAL-TXN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-TXN-CLAIM-ID NOT = CA-AV-CLAIM-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TXNNOTCL'          TO CA-REASON-CODE
              MOVE CA-AV-APPROVAL-TXN-ID TO WS-ID-DISP
              STRING 'Approval transaction ' WS-ID-DISP
                     ' is not on this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-011 Only OPEN activities can be completed / skipped
       3500-LOAD-OPEN-ACTIVITY.
           IF CA-AV-ACTIVITY-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACTIDREQ'          TO CA-REASON-CODE
              MOVE 'Activity id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4400-READ-ACTIVITY
           IF ACT-ACTIVITY-STATUS NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTNOTOP'          TO CA-REASON-CODE
              MOVE ACT-ACTIVITY-ID     TO WS-ID-DISP
              STRING 'Activity ' WS-ID-DISP ' is '
                     ACT-ACTIVITY-STATUS ', not OPEN'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE ACT-CLAIM-ID           TO CA-AV-CLAIM-ID.

      ******************************************************************
      * BR-CLM-ACT-008 Assignee or the assignee's supervisor only
       3600-CHECK-ACTOR.
           IF CA-USER-ID = ACT-ASSIGNED-USER
              CONTINUE
           ELSE
              MOVE ACT-ASSIGNED-USER   TO USR-USER-ID
              PERFORM 4130-READ-USER
              IF SQLCODE = 100
                 OR USR-SUPERVISOR-ID NOT = CA-USER-ID
                 SET CA-RC-AUTHORITY   TO TRUE
                 MOVE 'NOTOWNER'       TO CA-REASON-CODE
                 STRING 'Only ' ACT-ASSIGNED-USER
                        ' or their supervisor may close this activity'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-009 An approval activity stays open while the
      * financial transaction it represents is pending approval
       3700-CHECK-APPROVAL-CLEARED.
           IF ACT-APPROVAL-TXN-ID > 0
              EXEC SQL
                 SELECT TXN_STATUS
                   INTO :WS-TXN-STATUS
                   FROM CLMDB.CLM_TRANSACTION
                  WHERE TXN_ID = :ACT-APPROVAL-TXN-ID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    CONTINUE
                 WHEN 100
                    MOVE SPACES        TO WS-TXN-STATUS
                 WHEN OTHER
                    MOVE '3700-CHECK-APPROVAL-CLEARED' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
              IF WS-TXN-STATUS = 'PAPR'
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'APRPEND '       TO CA-REASON-CODE
                 MOVE ACT-APPROVAL-TXN-ID TO WS-ID-DISP
                 STRING 'Transaction ' WS-ID-DISP
                        ' still awaits approval in Financials'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
       4100-READ-CLAIM.
           EXEC SQL
              SELECT CLAIM_ID, CLAIM_NUMBER, LOSS_TYPE, CLAIM_STATE,
                     ASSIGNED_USER, ASSIGNED_GROUP
                INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                     :CLM-LOSS-TYPE, :CLM-CLAIM-STATE,
                     :CLM-ASSIGNED-USER, :CLM-ASSIGNED-GROUP
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-AV-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-AV-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Caller tests SQLCODE 100 (meaning differs per request)
       4110-READ-PATTERN.
           EXEC SQL
              SELECT SUBJECT, CATEGORY, TRIGGER_EVENT, LOSS_TYPE,
                     TARGET_DAYS, ESCALATION_DAYS, PRIORITY,
                     MANDATORY_FLAG
                INTO :APT-SUBJECT, :APT-CATEGORY, :APT-TRIGGER-EVENT,
                     :APT-LOSS-TYPE, :APT-TARGET-DAYS,
                     :APT-ESCALATION-DAYS, :APT-PRIORITY,
                     :APT-MANDATORY-FLAG
                FROM CLMDB.CLM_ACTIVITY_PATTERN
               WHERE PATTERN_CODE = :APT-PATTERN-CODE
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4110-READ-PATTERN' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Caller tests SQLCODE 100
       4130-READ-USER.
           EXEC SQL
              SELECT GROUP_CODE, SUPERVISOR_ID, ACTIVE_FLAG
                INTO :USR-GROUP-CODE, :USR-SUPERVISOR-ID,
                     :USR-ACTIVE-FLAG
                FROM CLMDB.CLM_USER
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4130-READ-USER'    TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4200-INSERT-ACTIVITY.
           EXEC SQL
              INSERT INTO CLMDB.CLM_ACTIVITY
                     (ACTIVITY_ID, CLAIM_ID, EXPOSURE_ID, PATTERN_CODE,
                      SUBJECT, PRIORITY, ACTIVITY_STATUS,
                      ASSIGNED_USER, DUE_DATE, ESCALATION_DATE,
                      ESCALATED_FLAG, APPROVAL_TXN_ID, CREATE_TS,
                      COMPLETE_USER)
              VALUES (:ACT-ACTIVITY-ID, :ACT-CLAIM-ID,
                      :ACT-EXPOSURE-ID, :ACT-PATTERN-CODE,
                      :ACT-SUBJECT, :ACT-PRIORITY,
                      :ACT-ACTIVITY-STATUS, :ACT-ASSIGNED-USER,
                      :ACT-DUE-DATE, :ACT-ESCALATION-DATE,
                      :ACT-ESCALATED-FLAG, :ACT-APPROVAL-TXN-ID,
                      CURRENT TIMESTAMP, ' ')
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE ACT-ACTIVITY-ID  TO WS-ID-DISP
                 STRING 'Activity id ' WS-ID-DISP ' already exists'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-INSERT-ACTIVITY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-COUNT-OPEN-SAME.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-OPEN-COUNT
                FROM CLMDB.CLM_ACTIVITY
               WHERE CLAIM_ID        = :CA-AV-CLAIM-ID
                 AND EXPOSURE_ID     = :CA-AV-EXPOSURE-ID
                 AND PATTERN_CODE    = :APT-PATTERN-CODE
                 AND ACTIVITY_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-COUNT-OPEN-SAME' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-READ-ACTIVITY.
           EXEC SQL
              SELECT ACTIVITY_ID, CLAIM_ID, EXPOSURE_ID,
                     PATTERN_CODE, SUBJECT, PRIORITY,
                     ACTIVITY_STATUS, ASSIGNED_USER, DUE_DATE,
                     ESCALATION_DATE, ESCALATED_FLAG, APPROVAL_TXN_ID
                INTO :ACT-ACTIVITY-ID, :ACT-CLAIM-ID,
                     :ACT-EXPOSURE-ID, :ACT-PATTERN-CODE,
                     :ACT-SUBJECT, :ACT-PRIORITY,
                     :ACT-ACTIVITY-STATUS, :ACT-ASSIGNED-USER,
                     :ACT-DUE-DATE, :ACT-ESCALATION-DATE,
                     :ACT-ESCALATED-FLAG, :ACT-APPROVAL-TXN-ID
                FROM CLMDB.CLM_ACTIVITY
               WHERE ACTIVITY_ID = :CA-AV-ACTIVITY-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 MOVE CA-AV-ACTIVITY-ID TO WS-ID-DISP
                 STRING 'Activity ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4400-READ-ACTIVITY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-OPEN-PATTERN-CURSOR.
           EXEC SQL OPEN CSR-PATTERN END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-OPEN-PATTERN-CURSOR' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CSR-PAT-OPEN         TO TRUE.

      ******************************************************************
       4510-FETCH-PATTERN.
           EXEC SQL
              FETCH CSR-PATTERN
               INTO :APT-PATTERN-CODE, :APT-SUBJECT, :APT-CATEGORY,
                    :APT-TRIGGER-EVENT, :APT-LOSS-TYPE,
                    :APT-TARGET-DAYS, :APT-ESCALATION-DAYS,
                    :APT-PRIORITY, :APT-MANDATORY-FLAG
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-FETCH-DONE     TO TRUE
              WHEN OTHER
                 MOVE '4510-FETCH-PATTERN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4520-CLOSE-PATTERN-CURSOR.
           EXEC SQL CLOSE CSR-PATTERN END-EXEC
           MOVE 'N'                    TO WS-CSR-PAT-SW.

      ******************************************************************
      * The status predicate guards against a concurrent close
       4600-CLOSE-ACTIVITY.
           EXEC SQL
              UPDATE CLMDB.CLM_ACTIVITY
                 SET ACTIVITY_STATUS = :ACT-ACTIVITY-STATUS,
                     COMPLETE_TS     = CURRENT TIMESTAMP,
                     COMPLETE_USER   = :CA-USER-ID
               WHERE ACTIVITY_ID     = :ACT-ACTIVITY-ID
                 AND ACTIVITY_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4600-CLOSE-ACTIVITY' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Activity was closed by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4700-FETCH-LIST-ROW.
           IF WS-LIST-BY-CLAIM
              EXEC SQL
                 FETCH CSR-CLMLST
                  INTO :ACT-ACTIVITY-ID, :CLM-CLAIM-NUMBER,
                       :ACT-SUBJECT, :ACT-PRIORITY, :ACT-DUE-DATE,
                       :ACT-ACTIVITY-STATUS, :ACT-ESCALATED-FLAG
              END-EXEC
           ELSE
              EXEC SQL
                 FETCH CSR-USRLST
                  INTO :ACT-ACTIVITY-ID, :CLM-CLAIM-NUMBER,
                       :ACT-SUBJECT, :ACT-PRIORITY, :ACT-DUE-DATE,
                       :ACT-ACTIVITY-STATUS, :ACT-ESCALATED-FLAG
              END-EXEC
           END-IF
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-FETCH-DONE     TO TRUE
              WHEN OTHER
                 MOVE '4700-FETCH-LIST-ROW' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4720-CLOSE-LIST-CURSOR.
           IF WS-CSR-CLM-OPEN
              EXEC SQL CLOSE CSR-CLMLST END-EXEC
              MOVE 'N'                 TO WS-CSR-CLM-SW
           END-IF
           IF WS-CSR-USR-OPEN
              EXEC SQL CLOSE CSR-USRLST END-EXEC
              MOVE 'N'                 TO WS-CSR-USR-SW
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-005 Assignee: explicit, else claim owner, else the
      * requesting user; must be an active user
       5100-RESOLVE-ASSIGNEE.
           EVALUATE TRUE
              WHEN CA-AV-ASSIGNED-USER NOT = SPACES
                 MOVE CA-AV-ASSIGNED-USER TO WS-ASSIGNEE
              WHEN CLM-ASSIGNED-USER NOT = SPACES
                 MOVE CLM-ASSIGNED-USER TO WS-ASSIGNEE
              WHEN OTHER
                 MOVE CA-USER-ID       TO WS-ASSIGNEE
           END-EVALUATE
           MOVE WS-ASSIGNEE            TO USR-USER-ID
           PERFORM 4130-READ-USER
           IF SQLCODE = 100 OR USR-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'USRINACT'          TO CA-REASON-CODE
              STRING 'Assignee ' WS-ASSIGNEE
                     ' is not an active claims user'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-ACT-003 Due = business date + TARGET_DAYS business days
      * BR-CLM-ACT-004 Escalation = due + ESCALATION_DAYS business days
       5200-CALC-DATES.
           MOVE WS-BUS-DATE            TO WS-WORK-DATE
           MOVE APT-TARGET-DAYS        TO WS-DAYS
           PERFORM 5210-ADD-BUSINESS-DAYS
           MOVE WS-WORK-DATE           TO WS-DUE-DATE
           MOVE APT-ESCALATION-DAYS    TO WS-DAYS
           PERFORM 5210-ADD-BUSINESS-DAYS
           MOVE WS-WORK-DATE           TO WS-ESC-DATE.

      ******************************************************************
      * Steps one calendar day at a time and rolls each step forward
      * to a business day, so N steps = N business days.
       5210-ADD-BUSINESS-DAYS.
           MOVE 'DTBUSD  '             TO WS-DATE-REQ
           PERFORM 8200-CALL-DATES
           PERFORM VARYING WS-DAY-IX FROM 1 BY 1
                   UNTIL WS-DAY-IX > WS-DAYS
              MOVE 'DTADDD  '          TO WS-DATE-REQ
              MOVE 1                   TO WS-DATE-NUM
              PERFORM 8200-CALL-DATES
              MOVE 'DTBUSD  '          TO WS-DATE-REQ
              PERFORM 8200-CALL-DATES
           END-PERFORM.

      ******************************************************************
       8000-NEXT-ACTIVITY-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'ACT     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE SQA-SQ-VALUE           TO ACT-ACTIVITY-ID.

      ******************************************************************
       8100-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-AV-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HIA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Date service: WS-DATE-REQ / WS-DATE-NUM set by the caller,
      * WS-WORK-DATE is both input and result
       8200-CALL-DATES.
           INITIALIZE WS-DTE-CA
           MOVE WS-DATE-REQ            TO DTA-REQUEST-ID
           MOVE WS-DATE-NUM            TO DTA-DT-NUMBER
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE WS-WORK-DATE           TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           MOVE DTA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE DTA-DT-RESULT-DATE     TO WS-WORK-DATE.

      ******************************************************************
      * Propagate a callee RC >= 12 unchanged with its reason code
       8900-CHECK-CALLEE.
           IF SVC-RETURN-CODE >= 12
              MOVE SVC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SVC-REASON-CODE     TO CA-REASON-CODE
              MOVE SVC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           IF WS-CSR-PAT-OPEN
              EXEC SQL CLOSE CSR-PATTERN END-EXEC
           END-IF
           IF WS-CSR-USR-OPEN
              EXEC SQL CLOSE CSR-USRLST END-EXEC
           END-IF
           IF WS-CSR-CLM-OPEN
              EXEC SQL CLOSE CSR-CLMLST END-EXEC
           END-IF
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Activity data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMACTV0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                     ' in ' WS-ERR-PARA DELIMITED BY SIZE
                     INTO CA-MESSAGE
              INITIALIZE WS-ERR-CA
              MOVE 'ERRLOG  '          TO ERA-REQUEST-ID
              MOVE CA-USER-ID          TO ERA-USER-ID
              MOVE WS-PROGRAM-ID       TO ERA-CALLER-PGM
              MOVE WS-PROGRAM-ID       TO ERA-ER-PROGRAM
              MOVE WS-ERR-PARA         TO ERA-ER-PARAGRAPH
              MOVE SQLCODE             TO ERA-ER-SQLCODE
              MOVE CA-MESSAGE          TO ERA-ER-TEXT
              MOVE CA-AV-CLAIM-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
