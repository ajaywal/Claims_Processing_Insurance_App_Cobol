       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMNOTE0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMNOTE0                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim notes. Adds topic-coded notes (optionally  *
      *               confidential) and lists the newest notes for a   *
      *               claim, withholding confidential notes from users *
      *               who are neither the claim owner nor the owner's  *
      *               supervisor.                                      *
      * REQUESTS    : NTEADD NTELST                                    *
      * CALLED BY   : claim screens, litigation and SIU services,      *
      *               FNOL intake batch                                *
      * CALLS       : CLMSEQGN CLMCODVL CLMHIST0 CLMAUDCH CLMERRLG     *
      * TABLES      : CLM_NOTE (C R)  CLM_CLAIM (R)  CLM_EXPOSURE (R)  *
      *               CLM_USER (R)  CLM_CONTROL (R)                    *
      * RULES       : BR-CLM-NTE-001 Topic must be an active NOTETOP   *
      *                              code                              *
      *               BR-CLM-NTE-002 Body text is mandatory            *
      *               BR-CLM-NTE-003 Confidential notes are allowed    *
      *                              only for topics LEGAL, SIU and    *
      *                              MEDICAL                           *
      *               BR-CLM-NTE-004 Subject defaults to the topic     *
      *                              description                       *
      *               BR-CLM-NTE-005 Exposure must belong to the claim *
      *               BR-CLM-NTE-006 Confidential notes are visible    *
      *                              only to the claim owner and the   *
      *                              owner's supervisor                *
      *               BR-CLM-NTE-007 Confidential notes are sealed in  *
      *                              the audit chain and noted in      *
      *                              history without their content    *
      *               BR-CLM-NTE-008 List returns the newest 10 notes  *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMNOTE0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-EXP-CLAIM-ID          PIC S9(9) COMP VALUE 0.
           05 WS-HIDDEN-COUNT          PIC S9(9) COMP VALUE 0.
           05 WS-ROW-IX                PIC S9(4) COMP VALUE 0.
           05 WS-CAN-SEE               PIC X     VALUE 'N'.
              88 WS-SEES-CONFIDENTIAL            VALUE 'Y'.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-COUNT-DISP            PIC Z(4)9.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-SUBJECT               PIC X(60) VALUE SPACES.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-DONE                   VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.
           05 WS-CSR-NTE-SW            PIC X     VALUE 'N'.
              88 WS-CSR-NTE-OPEN                 VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDNOT END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CSR-NOTES CURSOR FOR
              SELECT NOTE_ID, CREATE_TS, AUTHOR_USER, SUBJECT
                FROM CLMDB.CLM_NOTE
               WHERE CLAIM_ID = :CA-NT-CLAIM-ID
                 AND (CONFIDENTIAL_FLAG = 'N' OR :WS-CAN-SEE = 'Y')
                 AND (:CA-NT-TOPIC = ' ' OR NOTE_TOPIC = :CA-NT-TOPIC)
                 AND (:CA-NT-EXPOSURE-ID = 0
                      OR EXPOSURE_ID = :CA-NT-EXPOSURE-ID)
               ORDER BY CREATE_TS DESC, NOTE_ID DESC
               FETCH FIRST 10 ROWS ONLY
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

       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.

       01  WS-AUD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ADA-==.
           03 ADA-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==ADA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCNTE.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'NTEADD  '
                 PERFORM 2100-ADD-NOTE
              WHEN 'NTELST  '
                 PERFORM 2200-LIST-NOTES
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMNOTE0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
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
                 MOVE '1000-INITIALISE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * NTEADD
       2100-ADD-NOTE.
           PERFORM 3100-VALIDATE-NOTE
           PERFORM 4100-READ-CLAIM
           IF CA-NT-EXPOSURE-ID > 0
              PERFORM 3200-CHECK-EXPOSURE
           END-IF
           PERFORM 8000-NEXT-NOTE-ID
           MOVE CA-NT-CLAIM-ID         TO NTE-CLAIM-ID
           MOVE CA-NT-EXPOSURE-ID      TO NTE-EXPOSURE-ID
           MOVE CA-NT-TOPIC            TO NTE-NOTE-TOPIC
           MOVE WS-SUBJECT             TO NTE-SUBJECT-TEXT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-SUBJECT TRAILING))
                                       TO NTE-SUBJECT-LEN
           MOVE CA-NT-BODY             TO NTE-BODY-TEXT-TEXT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(CA-NT-BODY TRAILING))
                                       TO NTE-BODY-TEXT-LEN
           MOVE CA-NT-CONFIDENTIAL     TO NTE-CONFIDENTIAL-FLAG
           MOVE CA-USER-ID             TO NTE-AUTHOR-USER
           PERFORM 4200-INSERT-NOTE
           MOVE NTE-NOTE-ID            TO CA-NT-NOTE-ID
      * BR-CLM-NTE-007 Confidential notes: sealed digest + history
      *                entry that never carries the note content
           IF CA-NT-CONFIDENTIAL = 'Y'
              PERFORM 8300-AUDIT-CONFIDENTIAL
              PERFORM 8200-HISTORY-CONFIDENTIAL
           END-IF
           MOVE NTE-NOTE-ID            TO WS-ID-DISP
           STRING 'Note ' WS-ID-DISP ' added to claim '
                  CLM-CLAIM-NUMBER DELIMITED BY SIZE
                  INTO CA-MESSAGE.

      ******************************************************************
      * NTELST
       2200-LIST-NOTES.
           IF CA-NT-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required to list notes'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM
           PERFORM 3300-RESOLVE-VISIBILITY
           MOVE 0                      TO CA-NT-ROW-COUNT
           PERFORM VARYING WS-ROW-IX FROM 1 BY 1 UNTIL WS-ROW-IX > 10
              MOVE 0                   TO CA-NT-R-ID(WS-ROW-IX)
              MOVE SPACES              TO CA-NT-R-TS(WS-ROW-IX)
                                          CA-NT-R-AUTHOR(WS-ROW-IX)
                                          CA-NT-R-SUBJECT(WS-ROW-IX)
           END-PERFORM
           EXEC SQL OPEN CSR-NOTES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-LIST-NOTES'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CSR-NTE-OPEN         TO TRUE
           SET WS-FETCH-MORE           TO TRUE
           PERFORM UNTIL WS-FETCH-DONE
              EXEC SQL
                 FETCH CSR-NOTES
                  INTO :NTE-NOTE-ID, :NTE-CREATE-TS,
                       :NTE-AUTHOR-USER, :NTE-SUBJECT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 2210-MOVE-NOTE-ROW
                 WHEN 100
                    SET WS-FETCH-DONE  TO TRUE
                 WHEN OTHER
                    MOVE '2200-LIST-NOTES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-NOTES END-EXEC
           MOVE 'N'                    TO WS-CSR-NTE-SW
           IF NOT WS-SEES-CONFIDENTIAL
              PERFORM 4300-COUNT-HIDDEN
           END-IF
           MOVE CA-NT-ROW-COUNT        TO WS-COUNT-DISP
           IF WS-HIDDEN-COUNT > 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'CONFHIDE'          TO CA-REASON-CODE
              STRING WS-COUNT-DISP ' notes listed; confidential '
                     'notes withheld' DELIMITED BY SIZE
                     INTO CA-MESSAGE
           ELSE
              STRING WS-COUNT-DISP ' notes listed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-CLM-NTE-008 Newest 10 notes (cursor limits the fetch)
       2210-MOVE-NOTE-ROW.
           ADD 1                       TO CA-NT-ROW-COUNT
           MOVE CA-NT-ROW-COUNT        TO WS-ROW-IX
           MOVE NTE-NOTE-ID            TO CA-NT-R-ID(WS-ROW-IX)
           MOVE NTE-CREATE-TS          TO CA-NT-R-TS(WS-ROW-IX)
           MOVE NTE-AUTHOR-USER        TO CA-NT-R-AUTHOR(WS-ROW-IX)
           MOVE NTE-SUBJECT-TEXT(1:NTE-SUBJECT-LEN)
                                       TO CA-NT-R-SUBJECT(WS-ROW-IX).

      ******************************************************************
       3100-VALIDATE-NOTE.
           IF CA-NT-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for a note' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-NTE-002 Body is mandatory
           IF CA-NT-BODY = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BODYREQ '          TO CA-REASON-CODE
              MOVE 'Note text is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-NT-CONFIDENTIAL = SPACE
              MOVE 'N'                 TO CA-NT-CONFIDENTIAL
           END-IF
           IF CA-NT-CONFIDENTIAL NOT = 'Y' AND NOT = 'N'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CONFFLAG'          TO CA-REASON-CODE
              MOVE 'Confidential flag must be Y or N' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-NTE-001 Topic must be a valid NOTETOP code
           IF CA-NT-TOPIC = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TOPICREQ'          TO CA-REASON-CODE
              MOVE 'Note topic is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8100-VALIDATE-TOPIC
           IF NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADTOPIC'          TO CA-REASON-CODE
              STRING 'Note topic ' CA-NT-TOPIC ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-NTE-003 Confidential only for LEGAL / SIU / MEDICAL
           IF CA-NT-CONFIDENTIAL = 'Y'
              AND CA-NT-TOPIC NOT = 'LEGAL'
              AND CA-NT-TOPIC NOT = 'SIU'
              AND CA-NT-TOPIC NOT = 'MEDICAL'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CONFTOPC'          TO CA-REASON-CODE
              STRING 'Confidential notes need topic LEGAL, SIU or '
                     'MEDICAL' DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-NTE-004 Subject defaults to the topic description
           IF CA-NT-SUBJECT NOT = SPACES
              MOVE CA-NT-SUBJECT       TO WS-SUBJECT
           ELSE
              MOVE CDA-CD-DESC         TO WS-SUBJECT
           END-IF.

      ******************************************************************
      * BR-CLM-NTE-005 Exposure must belong to the claim
       3200-CHECK-EXPOSURE.
           EXEC SQL
              SELECT CLAIM_ID
                INTO :WS-EXP-CLAIM-ID
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :CA-NT-EXPOSURE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO WS-EXP-CLAIM-ID
              WHEN OTHER
                 MOVE '3200-CHECK-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-EXP-CLAIM-ID NOT = CA-NT-CLAIM-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPNOTCL'          TO CA-REASON-CODE
              MOVE CA-NT-EXPOSURE-ID   TO WS-ID-DISP
              STRING 'Exposure ' WS-ID-DISP
                     ' does not belong to this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-NTE-006 Confidential visibility: claim owner or the
      * owner's supervisor
       3300-RESOLVE-VISIBILITY.
           MOVE 'N'                    TO WS-CAN-SEE
           IF CLM-ASSIGNED-USER NOT = SPACES
              IF CA-USER-ID = CLM-ASSIGNED-USER
                 MOVE 'Y'              TO WS-CAN-SEE
              ELSE
                 MOVE CLM-ASSIGNED-USER TO USR-USER-ID
                 EXEC SQL
                    SELECT SUPERVISOR_ID
                      INTO :USR-SUPERVISOR-ID
                      FROM CLMDB.CLM_USER
                     WHERE USER_ID = :USR-USER-ID
                 END-EXEC
                 EVALUATE SQLCODE
                    WHEN 0
                       IF USR-SUPERVISOR-ID = CA-USER-ID
                          MOVE 'Y'     TO WS-CAN-SEE
                       END-IF
                    WHEN 100
                       CONTINUE
                    WHEN OTHER
                       MOVE '3300-RESOLVE-VISIBILITY' TO WS-ERR-PARA
                       PERFORM 9900-DB2-ERROR
                 END-EVALUATE
              END-IF
           END-IF.

      ******************************************************************
       4100-READ-CLAIM.
           EXEC SQL
              SELECT CLAIM_NUMBER, ASSIGNED_USER
                INTO :CLM-CLAIM-NUMBER, :CLM-ASSIGNED-USER
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-NT-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-NT-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-INSERT-NOTE.
           EXEC SQL
              INSERT INTO CLMDB.CLM_NOTE
                     (NOTE_ID, CLAIM_ID, EXPOSURE_ID, NOTE_TOPIC,
                      SUBJECT, BODY_TEXT, CONFIDENTIAL_FLAG,
                      AUTHOR_USER, CREATE_TS)
              VALUES (:NTE-NOTE-ID, :NTE-CLAIM-ID, :NTE-EXPOSURE-ID,
                      :NTE-NOTE-TOPIC, :NTE-SUBJECT, :NTE-BODY-TEXT,
                      :NTE-CONFIDENTIAL-FLAG, :NTE-AUTHOR-USER,
                      CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE NTE-NOTE-ID      TO WS-ID-DISP
                 STRING 'Note id ' WS-ID-DISP ' already exists'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-INSERT-NOTE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-COUNT-HIDDEN.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-HIDDEN-COUNT
                FROM CLMDB.CLM_NOTE
               WHERE CLAIM_ID          = :CA-NT-CLAIM-ID
                 AND CONFIDENTIAL_FLAG = 'Y'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-COUNT-HIDDEN' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       8000-NEXT-NOTE-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'NTE     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE SQA-SQ-VALUE           TO NTE-NOTE-ID.

      ******************************************************************
       8100-VALIDATE-TOPIC.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE WS-BUS-DATE            TO CDA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE 'NOTETOP '             TO CDA-CD-TABLE
           MOVE CA-NT-TOPIC            TO CDA-CD-CODE
           CALL WS-COD-PGM USING WS-COD-CA
           MOVE CDA-HEADER             TO SVC-HEADER
           IF SVC-RETURN-CODE NOT = 08
              PERFORM 8900-CHECK-CALLEE
           END-IF.

      ******************************************************************
       8200-HISTORY-CONFIDENTIAL.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-NT-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE 'NTECONF '             TO HIA-HI-EVENT-TYPE
           MOVE NTE-NOTE-ID            TO WS-ID-DISP
           STRING 'Confidential ' CA-NT-TOPIC ' note ' WS-ID-DISP
                  ' recorded' DELIMITED BY SIZE
                  INTO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HIA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8300-AUDIT-CONFIDENTIAL.
           INITIALIZE WS-AUD-CA
           MOVE 'AUCADD  '             TO ADA-REQUEST-ID
           MOVE CA-USER-ID             TO ADA-USER-ID
           MOVE WS-BUS-DATE            TO ADA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ADA-CALLER-PGM
           MOVE CA-NT-CLAIM-ID         TO ADA-AC-CLAIM-ID
           MOVE 'NTECONF '             TO ADA-AC-EVENT-CODE
           MOVE WS-PROGRAM-ID          TO ADA-AC-PROGRAM
           MOVE NTE-NOTE-ID            TO WS-ID-DISP
           STRING WS-ID-DISP '|' CA-NT-TOPIC '|' CA-USER-ID '|'
                  CA-NT-BODY(1:300) DELIMITED BY SIZE
                  INTO ADA-AC-PAYLOAD
           CALL WS-AUD-PGM USING WS-AUD-CA
           MOVE ADA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

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
           IF WS-CSR-NTE-OPEN
              EXEC SQL CLOSE CSR-NOTES END-EXEC
           END-IF
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Note data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMNOTE0 Db2 error SQLCODE ' WS-SQLCODE-DISP
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
              MOVE CA-NT-CLAIM-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
