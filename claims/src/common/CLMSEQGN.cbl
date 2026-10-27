       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSEQGN.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMSEQGN                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Surrogate key and business number generator for  *
      *               the claims application. Reserves a block of      *
      *               CA-SQ-COUNT values from a named CLM_SEQUENCE row *
      *               and returns the first one, plus a formatted      *
      *               business number (prefix + zero-padded value) for *
      *               numbering sequences such as CLMNO and CHKNO.     *
      * REQUESTS    : SEQNEXT                                          *
      * CALLED BY   : every CLM service that inserts a keyed row,      *
      *               CLMERRLG, CLMEVTQ, CLMHIST0, CLMAUDCH            *
      * CALLS       : CLMERRLG                                         *
      * TABLES      : CLM_SEQUENCE (R U)                               *
      * RULES       : BR-CLM-SEQ-001 Block size 1..1000, default 1     *
      *               BR-CLM-SEQ-002 Unknown sequence is an error,     *
      *                              never auto-created                *
      *               BR-CLM-SEQ-003 Exceeding MAX_VALUE wraps to 1    *
      *                              only when CYCLE_FLAG = 'Y'        *
      *               BR-CLM-SEQ-004 Numbers are allocated by a single *
      *                              UPDATE so concurrent callers can  *
      *                              never receive the same value      *
      *               BR-CLM-SEQ-005 A non-cyclic sequence past 90% of *
      *                              MAX_VALUE still allocates but     *
      *                              returns warning SEQHIGH           *
      *               BR-CLM-SEQ-006 Sequence names are upper-case     *
      *                              alphanumeric                      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-WORK.
           05 WS-BLOCK                 PIC S9(9) COMP VALUE 0.
           05 WS-FIRST-VALUE           PIC S9(9) COMP VALUE 0.
           05 WS-LAST-VALUE            PIC S9(9) COMP VALUE 0.
           05 WS-HIGH-WATER            PIC S9(9) COMP VALUE 0.
           05 WS-DIGITS                PIC 9(10).
           05 WS-PREFIX-LEN            PIC S9(4) COMP VALUE 0.
           05 WS-SQLCODE-DISP          PIC -9(9).

       01  WS-NAME-CHECK               PIC X(8).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDSEQ END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCSEQ.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'SEQNEXT '
                 PERFORM 2000-NEXT-VALUE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMSEQGN: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-SQ-VALUE
           MOVE SPACES                 TO CA-SQ-FORMATTED.

      ******************************************************************
      * BR-CLM-SEQ-001 Block size 1..1000, default 1
       2000-NEXT-VALUE.
           PERFORM 3000-VALIDATE-NAME
           IF CA-SQ-COUNT < 1
              MOVE 1                   TO CA-SQ-COUNT
           END-IF
           IF CA-SQ-COUNT > 1000
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SEQBLOCK'          TO CA-REASON-CODE
              MOVE 'Sequence block larger than 1000 requested'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-SQ-NAME             TO SEQ-SEQ-NAME
           PERFORM 4000-RESERVE-BLOCK
           IF CA-RC-OK OR CA-RC-WARNING
              PERFORM 5000-FORMAT-NUMBER
           END-IF.

      ******************************************************************
      * BR-CLM-SEQ-006 Sequence names are upper-case alphanumeric
      * (the DCLGEN prefix of the owning table or a business number
      * name), left-justified and space-padded.
       3000-VALIDATE-NAME.
           IF CA-SQ-NAME = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SEQNAME '          TO CA-REASON-CODE
              MOVE 'Sequence name is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-SQ-NAME(1:1) = SPACE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SEQNAME '          TO CA-REASON-CODE
              MOVE 'Sequence name must be left-justified'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-SQ-NAME             TO WS-NAME-CHECK
           INSPECT WS-NAME-CHECK CONVERTING
                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
                TO '                                    '
           IF WS-NAME-CHECK NOT = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SEQNAME '          TO CA-REASON-CODE
              STRING 'Sequence name ' CA-SQ-NAME
                     ' contains invalid characters'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-SEQ-004 Single UPDATE allocates the block atomically;
      * the row lock is held until the caller's unit of work ends.
       4000-RESERVE-BLOCK.
           EXEC SQL
              UPDATE CLMDB.CLM_SEQUENCE
                 SET NEXT_VALUE = NEXT_VALUE
                                + (INCREMENT_BY * :CA-SQ-COUNT),
                     UPDATE_TS  = CURRENT TIMESTAMP
               WHERE SEQ_NAME   = :SEQ-SEQ-NAME
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
      * BR-CLM-SEQ-002 Unknown sequence is an error
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'SEQNOTFD'       TO CA-REASON-CODE
                 STRING 'Sequence ' CA-SQ-NAME ' is not defined'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Sequence row locked - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-RESERVE-BLOCK' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE

           EXEC SQL
              SELECT NEXT_VALUE, INCREMENT_BY, MAX_VALUE,
                     CYCLE_FLAG, PREFIX_TEXT
                INTO :SEQ-NEXT-VALUE, :SEQ-INCREMENT-BY,
                     :SEQ-MAX-VALUE, :SEQ-CYCLE-FLAG,
                     :SEQ-PREFIX-TEXT
                FROM CLMDB.CLM_SEQUENCE
               WHERE SEQ_NAME = :SEQ-SEQ-NAME
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-RESERVE-BLOCK' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF

           COMPUTE WS-BLOCK = SEQ-INCREMENT-BY * CA-SQ-COUNT
           COMPUTE WS-FIRST-VALUE = SEQ-NEXT-VALUE - WS-BLOCK
           COMPUTE WS-LAST-VALUE  = SEQ-NEXT-VALUE - SEQ-INCREMENT-BY

      * BR-CLM-SEQ-003 Wrap only when the sequence is cyclic
           IF WS-LAST-VALUE > SEQ-MAX-VALUE
              IF SEQ-CYCLE-FLAG = 'Y'
                 PERFORM 4100-WRAP-SEQUENCE
              ELSE
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'SEQEXHST'       TO CA-REASON-CODE
                 STRING 'Sequence ' CA-SQ-NAME ' exhausted'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           ELSE
              PERFORM 4200-CHECK-HIGH-WATER
           END-IF
           MOVE WS-FIRST-VALUE         TO CA-SQ-VALUE.

      ******************************************************************
       4100-WRAP-SEQUENCE.
           MOVE 1                      TO WS-FIRST-VALUE
           COMPUTE SEQ-NEXT-VALUE = 1 + WS-BLOCK
           EXEC SQL
              UPDATE CLMDB.CLM_SEQUENCE
                 SET NEXT_VALUE = :SEQ-NEXT-VALUE,
                     UPDATE_TS  = CURRENT TIMESTAMP
               WHERE SEQ_NAME   = :SEQ-SEQ-NAME
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-WRAP-SEQUENCE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'SEQWRAP '             TO CA-REASON-CODE
           SET CA-RC-WARNING           TO TRUE
           MOVE 'Sequence wrapped to 1' TO CA-MESSAGE.

      ******************************************************************
      * BR-CLM-SEQ-005 Early warning for non-cyclic sequences: once the
      * last value handed out passes 90% of MAX_VALUE the caller still
      * gets its numbers but RC 04 / SEQHIGH lets batch report it.
       4200-CHECK-HIGH-WATER.
           IF SEQ-CYCLE-FLAG NOT = 'Y'
              COMPUTE WS-HIGH-WATER = (SEQ-MAX-VALUE / 10) * 9
              IF WS-LAST-VALUE > WS-HIGH-WATER
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'SEQHIGH '       TO CA-REASON-CODE
                 STRING 'Sequence ' CA-SQ-NAME
                        ' above 90% of maximum - extend MAX_VALUE'
                        DELIMITED BY SIZE INTO CA-MESSAGE
              END-IF
           END-IF.

      ******************************************************************
      * Business number = prefix (trimmed) + value zero padded so the
      * whole number fills 10 characters, e.g. CLMNO 'CL' -> CL00001234
      * (WS-DIGITS is 10 digits; skip as many as the prefix is long)
       5000-FORMAT-NUMBER.
           MOVE 0                      TO WS-PREFIX-LEN
           INSPECT SEQ-PREFIX-TEXT TALLYING WS-PREFIX-LEN
                   FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE WS-FIRST-VALUE         TO WS-DIGITS
           IF WS-PREFIX-LEN = 0
              MOVE WS-DIGITS           TO CA-SQ-FORMATTED
           ELSE
              STRING SEQ-PREFIX-TEXT(1:WS-PREFIX-LEN)
                     WS-DIGITS(WS-PREFIX-LEN + 1:)
                     DELIMITED BY SIZE INTO CA-SQ-FORMATTED
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'CLMSEQGN Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE CA-SQ-NAME             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
