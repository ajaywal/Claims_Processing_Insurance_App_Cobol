       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILERRLG IS RECURSIVE.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILERRLG                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Central technical error logger. Writes one row   *
      *               to BIL_ERROR_LOG and a one-line message to       *
      *               SYSOUT (batch) / CEEMSG (CICS) so operations     *
      *               see the failure even when the caller's unit of   *
      *               work is rolled back. Never fails its caller.     *
      * REQUESTS    : ERRLOG                                           *
      * CALLED BY   : every BIL program from its 9900-DB2-ERROR path   *
      * CALLS       : BILSEQGN                                         *
      * TABLES      : BIL_ERROR_LOG (C)                                *
      * RULES       : BR-BIL-ERR-001 Logging never fails the caller:   *
      *                              own errors are DISPLAYed and the  *
      *                              request returns RC 04 at worst    *
      *               BR-BIL-ERR-002 Missing program / paragraph /     *
      *                              user / tran are defaulted, never  *
      *                              rejected                          *
      *               BR-BIL-ERR-003 Severity: -911/-913 are retryable *
      *                              (W), other negative SQLCODEs and  *
      *                              CICS RESPs are errors (E), the    *
      *                              rest are informational (I)        *
      *               BR-BIL-ERR-004 Errors raised by BILSEQGN, or by  *
      *                              a nested ERRLOG, go to SYSOUT     *
      *                              only - the key generator cannot   *
      *                              be re-entered to log itself       *
      *               BR-BIL-ERR-005 Error keys come from sequence ERR *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-NEST-DEPTH               PIC S9(4) COMP VALUE 0.

       LOCAL-STORAGE SECTION.
       01  LS-WORK.
           05 LS-SEVERITY              PIC X     VALUE 'I'.
           05 LS-TRAIL                 PIC S9(4) COMP VALUE 0.
           05 LS-SQLCODE-DISP          PIC -9(9).
           05 LS-RESP-DISP             PIC -9(8).
           05 LS-NOW                   PIC X(21).
           05 LS-SYSOUT-LINE           PIC X(132).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDERR END-EXEC.

       01  LS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCERR.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           ADD 1                       TO WS-NEST-DEPTH
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'ERRLOG  '
                 PERFORM 2000-LOG-ERROR
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILERRLG: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE FUNCTION CURRENT-DATE  TO LS-NOW.

      ******************************************************************
       2000-LOG-ERROR.
           PERFORM 3000-DEFAULT-FIELDS
           PERFORM 5000-CLASSIFY
           PERFORM 2100-WRITE-SYSOUT
      * BR-BIL-ERR-004 No re-entry into the key generator
           IF WS-NEST-DEPTH > 1
           OR CA-CALLER-PGM = WS-SEQ-PGM
           OR CA-ER-PROGRAM = WS-SEQ-PGM
              MOVE 'ERRSYSO '          TO CA-REASON-CODE
              MOVE 'Error written to SYSOUT only'
                                       TO CA-MESSAGE
           ELSE
              PERFORM 8000-GET-ERROR-ID
              IF CA-RC-OK
                 PERFORM 4000-INSERT-ERROR
              END-IF
           END-IF.

      ******************************************************************
       2100-WRITE-SYSOUT.
           MOVE CA-ER-SQLCODE          TO LS-SQLCODE-DISP
           MOVE CA-ER-RESP             TO LS-RESP-DISP
           MOVE SPACES                 TO LS-SYSOUT-LINE
           STRING LS-NOW(1:4) '-' LS-NOW(5:2) '-' LS-NOW(7:2) ' '
                  LS-NOW(9:2) ':' LS-NOW(11:2) ':' LS-NOW(13:2)
                  ' BILERR' LS-SEVERITY ' '
                  CA-ER-PROGRAM ' '
                  CA-ER-PARAGRAPH(1:20) ' SQL=' LS-SQLCODE-DISP
                  ' RESP=' LS-RESP-DISP ' KEY=' CA-ER-KEY
                  ' ' CA-ER-TEXT
                  DELIMITED BY SIZE INTO LS-SYSOUT-LINE
           END-STRING
           DISPLAY LS-SYSOUT-LINE.

      ******************************************************************
      * BR-BIL-ERR-002 Default missing context instead of rejecting
       3000-DEFAULT-FIELDS.
           IF CA-ER-PROGRAM = SPACES
              MOVE CA-CALLER-PGM       TO CA-ER-PROGRAM
           END-IF
           IF CA-ER-PROGRAM = SPACES
              MOVE 'UNKNOWN '          TO CA-ER-PROGRAM
           END-IF
           IF CA-ER-PARAGRAPH = SPACES
              MOVE 'NOT SUPPLIED'      TO CA-ER-PARAGRAPH
           END-IF
           IF CA-ER-TEXT = SPACES
              MOVE 'No error text supplied by caller'
                                       TO CA-ER-TEXT
           END-IF
           IF CA-USER-ID = SPACES
              MOVE 'BATCH   '          TO CA-USER-ID
           END-IF
           IF CA-ER-TRAN-ID = SPACES
              MOVE 'BTCH'              TO CA-ER-TRAN-ID
           END-IF.

      ******************************************************************
       4000-INSERT-ERROR.
           MOVE SQC-SQ-VALUE           TO ERR-ERROR-ID
           MOVE CA-ER-PROGRAM          TO ERR-PROGRAM-ID
           MOVE CA-ER-PARAGRAPH        TO ERR-PARAGRAPH-NAME
           MOVE CA-ER-SQLCODE          TO ERR-SQLCODE-VALUE
           MOVE CA-ER-RESP             TO ERR-CICS-RESP
           MOVE CA-ER-KEY              TO ERR-ENTITY-KEY
           MOVE CA-USER-ID             TO ERR-USER-ID
           MOVE CA-ER-TRAN-ID          TO ERR-TRAN-ID
           MOVE CA-ER-TEXT             TO ERR-ERROR-TEXT-TEXT
           MOVE 0                      TO LS-TRAIL
           INSPECT FUNCTION REVERSE(CA-ER-TEXT)
                   TALLYING LS-TRAIL FOR LEADING SPACE
           COMPUTE ERR-ERROR-TEXT-LEN = 120 - LS-TRAIL
           EXEC SQL
              INSERT INTO BILDB.BIL_ERROR_LOG
                 (ERROR_ID, PROGRAM_ID, PARAGRAPH_NAME,
                  SQLCODE_VALUE, CICS_RESP, ERROR_TEXT,
                  ENTITY_KEY, USER_ID, TRAN_ID, ERROR_TS)
              VALUES
                 (:ERR-ERROR-ID, :ERR-PROGRAM-ID,
                  :ERR-PARAGRAPH-NAME, :ERR-SQLCODE-VALUE,
                  :ERR-CICS-RESP, :ERR-ERROR-TEXT,
                  :ERR-ENTITY-KEY, :ERR-USER-ID, :ERR-TRAN-ID,
                  CURRENT TIMESTAMP)
           END-EXEC
      * BR-BIL-ERR-001 Own failure is reported, never propagated
           IF SQLCODE NOT = 0
              MOVE SQLCODE             TO LS-SQLCODE-DISP
              DISPLAY 'BILERRLG unable to insert BIL_ERROR_LOG '
                      'SQLCODE=' LS-SQLCODE-DISP
                      ' for ' CA-ER-PROGRAM
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ERRLOGNW'          TO CA-REASON-CODE
              STRING 'Error log insert failed SQLCODE '
                     LS-SQLCODE-DISP DELIMITED BY SIZE
                     INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-BIL-ERR-003 Severity classification for operations
       5000-CLASSIFY.
           EVALUATE TRUE
              WHEN CA-ER-SQLCODE = -911 OR CA-ER-SQLCODE = -913
                 MOVE 'W'              TO LS-SEVERITY
              WHEN CA-ER-SQLCODE < 0
                 MOVE 'E'              TO LS-SEVERITY
              WHEN CA-ER-RESP NOT = 0
                 MOVE 'E'              TO LS-SEVERITY
              WHEN OTHER
                 MOVE 'I'              TO LS-SEVERITY
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ERR-005 Error log key from sequence ERR
       8000-GET-ERROR-ID.
           INITIALIZE LS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'ERR     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING LS-SEQ-CA
           IF NOT SQC-RC-OK AND NOT SQC-RC-WARNING
              DISPLAY 'BILERRLG no ERR key: ' SQC-REASON-CODE
                      ' ' SQC-MESSAGE
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ERRNOKEY'          TO CA-REASON-CODE
              MOVE 'Error logged to SYSOUT only - no ERR key'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
       9000-RETURN.
           SUBTRACT 1                  FROM WS-NEST-DEPTH
           GOBACK.
