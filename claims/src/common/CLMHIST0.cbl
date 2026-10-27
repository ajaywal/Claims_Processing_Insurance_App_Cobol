       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMHIST0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMHIST0                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim history writer and reader. Every business  *
      *               service records what it did to a claim here      *
      *               (ClaimCenter "History" tab); screens list the    *
      *               newest entries.                                  *
      * REQUESTS    : HISADD  append a history row for CLAIM-ID        *
      *               HISLST  newest 20 rows for CLAIM-ID, optionally  *
      *                       only one EVENT-TYPE                      *
      * CALLED BY   : CLMCLAIM, CLMEXPOS, CLMFINTX, CLMASSGN, CLMLITIG *
      *               CLMSIUSC, CLMRECOV, CLMACTV0, history screen     *
      * CALLS       : CLMSEQGN, CLMERRLG                               *
      * TABLES      : CLM_HISTORY (C R)  CLM_CLAIM (R)                 *
      * RULES       : BR-CLM-HIS-001 Claim id, event type and          *
      *                              description are mandatory         *
      *               BR-CLM-HIS-002 History is only written for an    *
      *                              existing claim                    *
      *               BR-CLM-HIS-003 User defaults to SYSTEM, program  *
      *                              to the calling program            *
      *               BR-CLM-HIS-004 Low-values in descriptions are    *
      *                              replaced by spaces                *
      *               BR-CLM-HIS-005 An identical entry (claim, event, *
      *                              user, text) within 60 seconds is  *
      *                              suppressed with RC 04             *
      *               BR-CLM-HIS-006 History is append-only: no update *
      *                              or delete request exists          *
      *               BR-CLM-HIS-007 Lists are newest first, at most   *
      *                              20 rows; RC 04 HISMORE when older *
      *                              rows exist                        *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.

       01  WS-CONSTANTS.
           05 WS-MAX-ROWS              PIC S9(4) COMP VALUE 20.

       01  WS-WORK.
           05 WS-TRAIL                 PIC S9(4) COMP.
           05 WS-DUP-COUNT             PIC S9(9) COMP.
           05 WS-FETCHED               PIC S9(4) COMP.
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-CLAIM-NUMBER          PIC X(12).
           05 WS-EVENT-FILTER          PIC X(8).
           05 WS-EOF                   PIC X     VALUE 'N'.
              88 WS-END-OF-CURSOR                VALUE 'Y'.
           05 WS-CURSOR-OPEN           PIC X     VALUE 'N'.
              88 WS-CURSOR-IS-OPEN               VALUE 'Y'.
           05 WS-SAVE-SQLCODE          PIC S9(9) COMP.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ID-DISP               PIC 9(9).
           05 WS-ERR-KEY               PIC X(20).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDHIS END-EXEC.

      * BR-CLM-HIS-007 newest first; 21 rows so we know if more exist
           EXEC SQL DECLARE HISCUR CURSOR FOR
              SELECT EVENT_TS, EVENT_TYPE, USER_ID, DESCRIPTION
                FROM CLMDB.CLM_HISTORY
               WHERE CLAIM_ID = :HIS-CLAIM-ID
                 AND (EVENT_TYPE = :WS-EVENT-FILTER
                      OR :WS-EVENT-FILTER = ' ')
               ORDER BY EVENT_TS DESC, HISTORY_ID DESC
               FETCH FIRST 21 ROWS ONLY
               FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCHIS.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
      * BR-CLM-HIS-006 Append-only: only HISADD and HISLST exist
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'HISADD  '
                 PERFORM 2100-ADD-HISTORY
              WHEN 'HISLST  '
                 PERFORM 2200-LIST-HISTORY
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMHIST0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO WS-CURSOR-OPEN
           MOVE CA-HI-CLAIM-ID         TO WS-ID-DISP
           MOVE WS-ID-DISP             TO WS-ERR-KEY.

      ******************************************************************
       2100-ADD-HISTORY.
           PERFORM 3100-VALIDATE-ADD
           PERFORM 4100-CHECK-CLAIM
           PERFORM 4200-CHECK-DUPLICATE
           IF NOT CA-RC-WARNING
              PERFORM 8000-NEXT-HISTORY-ID
              PERFORM 4300-INSERT-HISTORY
           END-IF.

      ******************************************************************
       2200-LIST-HISTORY.
           MOVE 0                      TO CA-HI-ROW-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-MAX-ROWS
              MOVE SPACES              TO CA-HI-R-TS(WS-IX)
                                          CA-HI-R-EVENT(WS-IX)
                                          CA-HI-R-USER(WS-IX)
                                          CA-HI-R-DESC(WS-IX)
           END-PERFORM
           IF CA-HI-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HICLMID '          TO CA-REASON-CODE
              MOVE 'Claim id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-CHECK-CLAIM
           MOVE CA-HI-EVENT-TYPE       TO WS-EVENT-FILTER
           MOVE 'N'                    TO WS-EOF
           MOVE 0                      TO WS-FETCHED
           EXEC SQL OPEN HISCUR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-LIST-HISTORY' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'Y'                    TO WS-CURSOR-OPEN
           PERFORM 4400-FETCH-HISTORY
           PERFORM UNTIL WS-END-OF-CURSOR
              ADD 1                    TO WS-FETCHED
              IF WS-FETCHED <= WS-MAX-ROWS
                 PERFORM 5100-MOVE-ROW
              END-IF
              PERFORM 4400-FETCH-HISTORY
           END-PERFORM
           PERFORM 4500-CLOSE-HISTORY
      * BR-CLM-HIS-007 Tell the caller the list is truncated
           IF WS-FETCHED > WS-MAX-ROWS
              SET CA-RC-WARNING        TO TRUE
              MOVE 'HISMORE '          TO CA-REASON-CODE
              MOVE 'Newest 20 entries shown - older history exists'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
       3100-VALIDATE-ADD.
      * BR-CLM-HIS-001 Claim id, event type and text are mandatory
           IF CA-HI-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HICLMID '          TO CA-REASON-CODE
              MOVE 'Claim id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-HI-EVENT-TYPE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HIEVTYPE'          TO CA-REASON-CODE
              MOVE 'History event type is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-HIS-004 Low-values (unformatted 3270 fields, binary
      * zero padding from batch callers) are blanked before the
      * emptiness check so they never reach Db2 or a report.
           INSPECT CA-HI-DESCRIPTION
                   REPLACING ALL LOW-VALUE BY SPACE
           IF CA-HI-DESCRIPTION = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HIDESC  '          TO CA-REASON-CODE
              MOVE 'History description is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-HIS-003 Who and which program
           IF CA-USER-ID = SPACES
              MOVE 'SYSTEM  '          TO CA-USER-ID
           END-IF
           IF CA-HI-PROGRAM = SPACES
              IF CA-CALLER-PGM = SPACES
                 MOVE 'UNKNOWN '       TO CA-HI-PROGRAM
              ELSE
                 MOVE CA-CALLER-PGM    TO CA-HI-PROGRAM
              END-IF
           END-IF
           MOVE CA-HI-CLAIM-ID         TO HIS-CLAIM-ID
           MOVE CA-HI-EVENT-TYPE       TO HIS-EVENT-TYPE
           MOVE CA-USER-ID             TO HIS-USER-ID
           MOVE CA-HI-PROGRAM          TO HIS-PROGRAM-ID
           MOVE CA-HI-DESCRIPTION      TO HIS-DESCRIPTION-TEXT
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-HI-DESCRIPTION)
                   TALLYING WS-TRAIL FOR LEADING SPACES
           COMPUTE HIS-DESCRIPTION-LEN = LENGTH OF CA-HI-DESCRIPTION
                                       - WS-TRAIL.

      ******************************************************************
      * BR-CLM-HIS-002 History belongs to an existing claim
       4100-CHECK-CLAIM.
           MOVE CA-HI-CLAIM-ID         TO HIS-CLAIM-ID
           EXEC SQL
              SELECT CLAIM_NUMBER
                INTO :WS-CLAIM-NUMBER
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :HIS-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-CHECK-CLAIM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-HIS-005 A double PF-key or a re-driven batch step must
      * not post the same history line twice within a minute.
       4200-CHECK-DUPLICATE.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-DUP-COUNT
                FROM CLMDB.CLM_HISTORY
               WHERE CLAIM_ID    = :HIS-CLAIM-ID
                 AND EVENT_TYPE  = :HIS-EVENT-TYPE
                 AND USER_ID     = :HIS-USER-ID
                 AND DESCRIPTION = :HIS-DESCRIPTION
                 AND EVENT_TS    > CURRENT TIMESTAMP - 60 SECONDS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-CHECK-DUPLICATE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DUP-COUNT > 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'HISDUPL '          TO CA-REASON-CODE
              MOVE 'Identical history entry posted within 60 seconds'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
       4300-INSERT-HISTORY.
           EXEC SQL
              INSERT INTO CLMDB.CLM_HISTORY
                 (HISTORY_ID, CLAIM_ID, EVENT_TYPE, DESCRIPTION,
                  USER_ID, PROGRAM_ID, EVENT_TS)
              VALUES
                 (:HIS-HISTORY-ID, :HIS-CLAIM-ID, :HIS-EVENT-TYPE,
                  :HIS-DESCRIPTION, :HIS-USER-ID, :HIS-PROGRAM-ID,
                  CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'HISDUPID'       TO CA-REASON-CODE
                 MOVE 'History id already used - check HIS sequence'
                                       TO CA-MESSAGE
              WHEN OTHER
                 MOVE '4300-INSERT-HISTORY' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-FETCH-HISTORY.
           EXEC SQL
              FETCH HISCUR
               INTO :HIS-EVENT-TS, :HIS-EVENT-TYPE, :HIS-USER-ID,
                    :HIS-DESCRIPTION
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 'Y'              TO WS-EOF
              WHEN OTHER
                 MOVE '4400-FETCH-HISTORY' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-CLOSE-HISTORY.
           IF WS-CURSOR-IS-OPEN
              MOVE 'N'                 TO WS-CURSOR-OPEN
              EXEC SQL CLOSE HISCUR END-EXEC
           END-IF.

      ******************************************************************
       5100-MOVE-ROW.
           MOVE WS-FETCHED             TO CA-HI-ROW-COUNT
           MOVE HIS-EVENT-TS           TO CA-HI-R-TS(WS-FETCHED)
           MOVE HIS-EVENT-TYPE         TO CA-HI-R-EVENT(WS-FETCHED)
           MOVE HIS-USER-ID            TO CA-HI-R-USER(WS-FETCHED)
           IF HIS-DESCRIPTION-LEN > 0
              MOVE HIS-DESCRIPTION-TEXT(1:HIS-DESCRIPTION-LEN)
                                       TO CA-HI-R-DESC(WS-FETCHED)
           END-IF.

      ******************************************************************
       8000-NEXT-HISTORY-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'HIS     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SQC-SQ-VALUE           TO HIS-HISTORY-ID.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SAVE-SQLCODE
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           PERFORM 4500-CLOSE-HISTORY
           SET CA-RC-DB2-ERROR         TO TRUE
           IF WS-SAVE-SQLCODE = -911 OR WS-SAVE-SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
           END-IF
           STRING 'CLMHIST0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-SAVE-SQLCODE        TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
