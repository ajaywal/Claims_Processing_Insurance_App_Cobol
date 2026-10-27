       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMERRLG.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMERRLG                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Central technical error logger. Writes one row   *
      *               to CLM_ERROR_LOG (key from sequence ERR) and a   *
      *               DISPLAY line (SYSOUT in batch, CSSL/job log      *
      *               under CICS) so the error is traceable even when  *
      *               the caller's unit of work is later rolled back.  *
      *               The logger never fails its caller.               *
      * REQUESTS    : ERRLOG (any request id is accepted)              *
      * CALLED BY   : 9900-DB2-ERROR of every CLM program              *
      * CALLS       : CLMSEQGN                                         *
      * TABLES      : CLM_ERROR_LOG (C)                                *
      * RULES       : BR-CLM-ERR-001 The logger always returns to the  *
      *                              caller; RC is 00, or 04 when the  *
      *                              row could not be written          *
      *               BR-CLM-ERR-002 Every error is DISPLAYed before   *
      *                              any Db2 access                    *
      *               BR-CLM-ERR-003 Errors raised by CLMSEQGN are     *
      *                              DISPLAY-only (no recursive CALL)  *
      *               BR-CLM-ERR-004 Missing program, text or user are *
      *                              defaulted, never rejected         *
      *               BR-CLM-ERR-005 A failing insert is DISPLAYed and *
      *                              swallowed                         *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.

       01  WS-WORK.
           05 WS-TEXT-LEN              PIC S9(4) COMP VALUE 0.
           05 WS-TRAIL                 PIC S9(4) COMP VALUE 0.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-RESP-DISP             PIC -9(8).
           05 WS-INS-SQLCODE           PIC -9(9).
           05 WS-NOW                   PIC X(21).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDERR END-EXEC.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCERR.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-DISPLAY-ERROR
      * BR-CLM-ERR-003 The sequence generator logs through us, so an
      * error raised inside CLMSEQGN must not CALL it back (a COBOL
      * run unit may not re-enter an active non-recursive program).
           IF CA-ER-PROGRAM = WS-SEQ-PGM
           OR CA-CALLER-PGM = WS-SEQ-PGM
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ERRDISP '          TO CA-REASON-CODE
              MOVE 'Sequence error logged to job log only'
                                       TO CA-MESSAGE
           ELSE
              PERFORM 2100-WRITE-ERROR-ROW
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
      * BR-CLM-ERR-004 Defaults instead of rejections: a logger that
      * rejected bad input would lose the very error it was given.
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           IF CA-ER-PROGRAM = SPACES
              IF CA-CALLER-PGM = SPACES
                 MOVE 'UNKNOWN '       TO CA-ER-PROGRAM
              ELSE
                 MOVE CA-CALLER-PGM    TO CA-ER-PROGRAM
              END-IF
           END-IF
           IF CA-ER-PARAGRAPH = SPACES
              MOVE 'NOT SUPPLIED'      TO CA-ER-PARAGRAPH
           END-IF
           IF CA-ER-TEXT = SPACES
              MOVE 'No error text supplied by caller'
                                       TO CA-ER-TEXT
           END-IF
           IF CA-USER-ID = SPACES
              MOVE 'SYSTEM  '          TO CA-USER-ID
           END-IF
           MOVE FUNCTION CURRENT-DATE  TO WS-NOW
           MOVE CA-ER-SQLCODE          TO WS-SQLCODE-DISP
           MOVE CA-ER-RESP             TO WS-RESP-DISP.

      ******************************************************************
      * BR-CLM-ERR-002 DISPLAY first: the line survives a rollback of
      * the caller's unit of work and a Db2 outage.
       2000-DISPLAY-ERROR.
           DISPLAY 'CLMERRLG E ' WS-NOW(1:8) '-' WS-NOW(9:6)
                   ' PGM='    CA-ER-PROGRAM
                   ' PARA='   CA-ER-PARAGRAPH
                   ' SQLCODE=' WS-SQLCODE-DISP
                   ' RESP='   WS-RESP-DISP
           DISPLAY 'CLMERRLG E   USER=' CA-USER-ID
                   ' TRAN='   CA-ER-TRAN-ID
                   ' TRACE='  CA-TRACE-ID
                   ' KEY='    CA-ER-KEY
           DISPLAY 'CLMERRLG E   TEXT=' CA-ER-TEXT.

      ******************************************************************
       2100-WRITE-ERROR-ROW.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'ERR     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF NOT SQC-RC-OK AND NOT SQC-RC-WARNING
              DISPLAY 'CLMERRLG W   no ERR key: ' SQC-REASON-CODE
                      ' ' SQC-MESSAGE
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ERRNOKEY'          TO CA-REASON-CODE
              MOVE 'Error logged to job log only - no ERR key'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           MOVE SQC-SQ-VALUE           TO ERR-ERROR-ID
           MOVE CA-ER-PROGRAM          TO ERR-PROGRAM-ID
           MOVE CA-ER-PARAGRAPH        TO ERR-PARAGRAPH-NAME
           MOVE CA-ER-SQLCODE          TO ERR-SQLCODE-VALUE
           MOVE CA-ER-RESP             TO ERR-CICS-RESP
           MOVE CA-ER-KEY              TO ERR-ENTITY-KEY
           MOVE CA-USER-ID             TO ERR-USER-ID
           MOVE CA-ER-TRAN-ID          TO ERR-TRAN-ID
           MOVE CA-ER-TEXT             TO ERR-ERROR-TEXT-TEXT
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-ER-TEXT)
                   TALLYING WS-TRAIL FOR LEADING SPACES
           COMPUTE WS-TEXT-LEN = LENGTH OF CA-ER-TEXT - WS-TRAIL
           MOVE WS-TEXT-LEN            TO ERR-ERROR-TEXT-LEN

           EXEC SQL
              INSERT INTO CLMDB.CLM_ERROR_LOG
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
      * BR-CLM-ERR-005 Insert failures are reported, never raised
           IF SQLCODE NOT = 0
              MOVE SQLCODE             TO WS-INS-SQLCODE
              DISPLAY 'CLMERRLG W   CLM_ERROR_LOG insert failed '
                      'SQLCODE=' WS-INS-SQLCODE
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ERRNOTBL'          TO CA-REASON-CODE
              MOVE 'Error logged to job log only - insert failed'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-CLM-ERR-001 Always return; never abend the caller
       9000-RETURN.
           GOBACK.
