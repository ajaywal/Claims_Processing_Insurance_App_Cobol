       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILAUDIT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILAUDIT                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Field-level audit trail. Records who changed     *
      *               which field of which entity, with before and     *
      *               after images, inside the caller's unit of work   *
      *               so the audit row commits or rolls back with the  *
      *               change it describes.                             *
      * REQUESTS    : AUDADD                                           *
      * CALLED BY   : every BIL service that changes a business entity *
      * CALLS       : BILSEQGN, BILERRLG                               *
      * TABLES      : BIL_AUDIT (C)                                    *
      * RULES       : BR-BIL-AUD-001 Entity, key and action are        *
      *                              mandatory                         *
      *               BR-BIL-AUD-002 Action is ADD, UPD, DEL or STAT   *
      *               BR-BIL-AUD-003 UPD needs a field name; STAT      *
      *                              defaults the field to STATUS      *
      *               BR-BIL-AUD-004 An UPD/STAT whose before and      *
      *                              after images are equal is not     *
      *                              recorded (RC 04 AUDNOCHG)         *
      *               BR-BIL-AUD-005 ADD has no before image and DEL   *
      *                              no after image; blank images are  *
      *                              stored as NULL                    *
      *               BR-BIL-AUD-006 Payment-instrument secrets        *
      *                              (TOKEN_REF, SORT_CODE,            *
      *                              EXPIRY_MMYY) are masked to their  *
      *                              last four characters              *
      *               BR-BIL-AUD-007 Program defaults to the caller,   *
      *                              user to BATCH                     *
      *               BR-BIL-AUD-008 Audit keys come from sequence AUD *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.

       01  WS-WORK.
           05 WS-TRAIL                 PIC S9(4) COMP VALUE 0.
           05 WS-VALUE-LEN             PIC S9(4) COMP VALUE 0.
           05 WS-MASK-VALUE            PIC X(100).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-SENSITIVE-FLAG        PIC X     VALUE 'N'.
              88 WS-SENSITIVE                    VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDAUD END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCAUD.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'AUDADD  '
                 PERFORM 2000-ADD-AUDIT
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILAUDIT: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE.

      ******************************************************************
       2000-ADD-AUDIT.
           PERFORM 3000-VALIDATE
           PERFORM 3100-APPLY-DEFAULTS
      * BR-BIL-AUD-004 No-op changes are not audited
           IF (CA-AU-ACTION = 'UPD ' OR CA-AU-ACTION = 'STAT')
           AND CA-AU-BEFORE = CA-AU-AFTER
              SET CA-RC-WARNING        TO TRUE
              MOVE 'AUDNOCHG'          TO CA-REASON-CODE
              STRING 'No change to ' CA-AU-FIELD
                     ' - audit not written'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 5000-MASK-SENSITIVE
           PERFORM 5100-BUILD-IMAGES
           PERFORM 8000-GET-AUDIT-ID
           PERFORM 4000-INSERT-AUDIT.

      ******************************************************************
       3000-VALIDATE.
      * BR-BIL-AUD-001 Mandatory identification
           IF CA-AU-ENTITY = SPACES OR CA-AU-KEY = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUDENTKY'          TO CA-REASON-CODE
              MOVE 'Audit entity name and key are required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-AUD-002 Allowed actions
           EVALUATE CA-AU-ACTION
              WHEN 'ADD '
              WHEN 'DEL '
              WHEN 'STAT'
                 CONTINUE
      * BR-BIL-AUD-003 Field-level update names its field
              WHEN 'UPD '
                 IF CA-AU-FIELD = SPACES
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'AUDFIELD'    TO CA-REASON-CODE
                    MOVE 'Field name is required for an UPD audit'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'AUDACTN '       TO CA-REASON-CODE
                 STRING 'Audit action ' CA-AU-ACTION
                        ' must be ADD, UPD, DEL or STAT'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
       3100-APPLY-DEFAULTS.
           IF CA-AU-ACTION = 'STAT' AND CA-AU-FIELD = SPACES
              MOVE 'STATUS'            TO CA-AU-FIELD
           END-IF
      * BR-BIL-AUD-007 Program and user defaults
           IF CA-AU-PROGRAM = SPACES
              MOVE CA-CALLER-PGM       TO CA-AU-PROGRAM
           END-IF
           IF CA-AU-PROGRAM = SPACES
              MOVE 'UNKNOWN '          TO CA-AU-PROGRAM
           END-IF
           IF CA-USER-ID = SPACES
              MOVE 'BATCH   '          TO CA-USER-ID
           END-IF.

      ******************************************************************
       4000-INSERT-AUDIT.
           MOVE SQC-SQ-VALUE           TO AUD-AUDIT-ID
           MOVE CA-AU-ENTITY           TO AUD-ENTITY-NAME
           MOVE CA-AU-KEY              TO AUD-ENTITY-KEY
           MOVE CA-AU-ACTION           TO AUD-ACTION-CODE
           MOVE CA-AU-FIELD            TO AUD-FIELD-NAME
           MOVE CA-USER-ID             TO AUD-USER-ID
           MOVE CA-AU-PROGRAM          TO AUD-PROGRAM-ID
           EXEC SQL
              INSERT INTO BILDB.BIL_AUDIT
                 (AUDIT_ID, ENTITY_NAME, ENTITY_KEY, ACTION_CODE,
                  FIELD_NAME, BEFORE_VALUE, AFTER_VALUE,
                  USER_ID, PROGRAM_ID, AUDIT_TS)
              VALUES
                 (:AUD-AUDIT-ID, :AUD-ENTITY-NAME,
                  :AUD-ENTITY-KEY, :AUD-ACTION-CODE,
                  :AUD-FIELD-NAME,
                  :AUD-BEFORE-VALUE :AUD-BEFORE-VALUE-IND,
                  :AUD-AFTER-VALUE  :AUD-AFTER-VALUE-IND,
                  :AUD-USER-ID, :AUD-PROGRAM-ID,
                  CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'AUDDUPL '       TO CA-REASON-CODE
                 MOVE 'Audit id already used - check sequence AUD'
                                       TO CA-MESSAGE
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Audit insert timed out - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 MOVE '4000-INSERT-AUDIT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-AUD-006 Mask instrument secrets to the last 4 chars
       5000-MASK-SENSITIVE.
           MOVE 'N'                    TO WS-SENSITIVE-FLAG
           EVALUATE CA-AU-FIELD
              WHEN 'TOKEN_REF'
              WHEN 'SORT_CODE'
              WHEN 'EXPIRY_MMYY'
                 MOVE 'Y'              TO WS-SENSITIVE-FLAG
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           IF WS-SENSITIVE
              MOVE CA-AU-BEFORE        TO WS-MASK-VALUE
              PERFORM 5010-MASK-VALUE
              MOVE WS-MASK-VALUE       TO CA-AU-BEFORE
              MOVE CA-AU-AFTER         TO WS-MASK-VALUE
              PERFORM 5010-MASK-VALUE
              MOVE WS-MASK-VALUE       TO CA-AU-AFTER
           END-IF.

       5010-MASK-VALUE.
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(WS-MASK-VALUE)
                   TALLYING WS-TRAIL FOR LEADING SPACE
           COMPUTE WS-VALUE-LEN = 100 - WS-TRAIL
           IF WS-VALUE-LEN > 4
              MOVE ALL '*'             TO
                   WS-MASK-VALUE(1:WS-VALUE-LEN - 4)
           END-IF.

      ******************************************************************
      * BR-BIL-AUD-005 Before/after images and NULL handling
       5100-BUILD-IMAGES.
           IF CA-AU-ACTION = 'ADD ' OR CA-AU-BEFORE = SPACES
              MOVE -1                  TO AUD-BEFORE-VALUE-IND
              MOVE 0                   TO AUD-BEFORE-VALUE-LEN
           ELSE
              MOVE 0                   TO AUD-BEFORE-VALUE-IND
              MOVE CA-AU-BEFORE        TO AUD-BEFORE-VALUE-TEXT
              MOVE 0                   TO WS-TRAIL
              INSPECT FUNCTION REVERSE(CA-AU-BEFORE)
                      TALLYING WS-TRAIL FOR LEADING SPACE
              COMPUTE AUD-BEFORE-VALUE-LEN = 100 - WS-TRAIL
           END-IF
           IF CA-AU-ACTION = 'DEL ' OR CA-AU-AFTER = SPACES
              MOVE -1                  TO AUD-AFTER-VALUE-IND
              MOVE 0                   TO AUD-AFTER-VALUE-LEN
           ELSE
              MOVE 0                   TO AUD-AFTER-VALUE-IND
              MOVE CA-AU-AFTER         TO AUD-AFTER-VALUE-TEXT
              MOVE 0                   TO WS-TRAIL
              INSPECT FUNCTION REVERSE(CA-AU-AFTER)
                      TALLYING WS-TRAIL FOR LEADING SPACE
              COMPUTE AUD-AFTER-VALUE-LEN = 100 - WS-TRAIL
           END-IF.

      ******************************************************************
      * BR-BIL-AUD-008 Audit key from sequence AUD
       8000-GET-AUDIT-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'AUD     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE > 4
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'BILAUDIT Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE CA-AU-KEY              TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
