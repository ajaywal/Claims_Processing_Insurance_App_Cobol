       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMAUTHZ.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMAUTHZ                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Financial authority limits. Resolves the user's  *
      *               authority profile from CLM_USER, reads the limit *
      *               for the limit type and coverage type (falling    *
      *               back to the '*' default row) and says whether    *
      *               AMOUNT is within it. When it is not, walks the   *
      *               supervisor chain (CLM_USER.SUPERVISOR_ID, at     *
      *               most 5 levels) and returns the first supervisor  *
      *               whose own limit covers AMOUNT as the APPROVER.   *
      * REQUESTS    : AUTCHK                                           *
      * CALLED BY   : CLMFINTX (RSVSET PAYCRT TXNAPR), screens         *
      * CALLS       : CLMERRLG                                         *
      * TABLES      : CLM_USER (R) CLM_AUTHORITY_LIMIT (R)             *
      * RULES       : BR-CLM-AUT-001 Limit types are RSVEXP PAYEXP     *
      *                              PAYCLM INCCLM only                *
      *               BR-CLM-AUT-002 Claim-level limit types (PAYCLM,  *
      *                              INCCLM) ignore coverage type and  *
      *                              use the '*' row                   *
      *               BR-CLM-AUT-003 A coverage-specific limit row     *
      *                              overrides the '*' default row     *
      *               BR-CLM-AUT-004 No limit row means zero authority *
      *               BR-CLM-AUT-005 An inactive user has no authority *
      *               BR-CLM-AUT-006 Amount within limit when AMOUNT <=*
      *                              LIMIT (limit is inclusive)        *
      *               BR-CLM-AUT-007 Approver search climbs at most 5  *
      *                              supervisor levels                 *
      *               BR-CLM-AUT-008 Inactive supervisors are skipped  *
      *                              but still count as a level        *
      *               BR-CLM-AUT-009 A supervisor loop (user already   *
      *                              seen in the chain) stops the walk *
      *               BR-CLM-AUT-010 No approver found is a warning    *
      *                              (RC 04 NOAPPRVR), not an error    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMAUTHZ'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-CONSTANTS.
           05 WS-MAX-LEVELS            PIC S9(4) COMP VALUE 5.
           05 WS-DEFAULT-COVERAGE      PIC X(8)  VALUE '*'.

       01  WS-WORK.
           05 WS-LIMIT-TYPE            PIC X(8).
              88 WS-LT-RESERVE-EXP      VALUE 'RSVEXP  '.
              88 WS-LT-PAYMENT-EXP      VALUE 'PAYEXP  '.
              88 WS-LT-PAYMENT-CLM      VALUE 'PAYCLM  '.
              88 WS-LT-INCURRED-CLM     VALUE 'INCCLM  '.
              88 WS-LT-VALID            VALUE 'RSVEXP  ' 'PAYEXP  '
                                              'PAYCLM  ' 'INCCLM  '.
              88 WS-LT-CLAIM-LEVEL      VALUE 'PAYCLM  ' 'INCCLM  '.
           05 WS-COVERAGE              PIC X(8).
           05 WS-LOOKUP-USER           PIC X(8).
           05 WS-USER-FOUND            PIC X     VALUE 'N'.
              88 WS-USER-IS-FOUND       VALUE 'Y'.
           05 WS-USER-LIMIT            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-CANDIDATE             PIC X(8).
           05 WS-LEVEL                 PIC S9(4) COMP VALUE 0.
           05 WS-SEEN-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-IX                    PIC S9(4) COMP VALUE 0.
           05 WS-LOOP-FOUND            PIC X     VALUE 'N'.
              88 WS-IS-LOOP             VALUE 'Y'.
           05 WS-WALK-DONE             PIC X     VALUE 'N'.
              88 WS-WALK-IS-DONE        VALUE 'Y'.
           05 WS-SEEN-USER OCCURS 7 TIMES
                                       PIC X(8).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-LIM-DISP              PIC -(11)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDAUL END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCAUT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'AUTCHK  '
                 PERFORM 2000-AUTHORITY-CHECK
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMAUTHZ: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          CA-AT-APPROVER
           MOVE 'N'                    TO CA-AT-ALLOWED
           MOVE 0                      TO CA-AT-LIMIT
           MOVE SPACES                 TO WS-SEEN-USER(1)
                                          WS-SEEN-USER(2)
                                          WS-SEEN-USER(3)
                                          WS-SEEN-USER(4)
                                          WS-SEEN-USER(5)
                                          WS-SEEN-USER(6)
                                          WS-SEEN-USER(7)
           MOVE 0                      TO WS-SEEN-COUNT.

      ******************************************************************
       2000-AUTHORITY-CHECK.
           PERFORM 3000-VALIDATE-REQUEST
           MOVE CA-AT-USER             TO WS-LOOKUP-USER
           PERFORM 4000-READ-USER
           IF NOT WS-USER-IS-FOUND
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'USRNOTFD'          TO CA-REASON-CODE
              STRING 'User ' CA-AT-USER ' is not defined to claims'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 2100-RECORD-SEEN

      * BR-CLM-AUT-005 An inactive user has no authority
           IF USR-ACTIVE-FLAG NOT = 'Y'
              MOVE 0                   TO WS-USER-LIMIT
           ELSE
              PERFORM 4100-READ-LIMIT
           END-IF
           MOVE WS-USER-LIMIT          TO CA-AT-LIMIT

      * BR-CLM-AUT-006 Limit is inclusive
           IF CA-AT-AMOUNT <= WS-USER-LIMIT
              SET CA-AT-IS-ALLOWED     TO TRUE
           ELSE
              MOVE 'N'                 TO CA-AT-ALLOWED
              PERFORM 5000-FIND-APPROVER
              IF CA-AT-APPROVER = SPACES
                 PERFORM 2200-NO-APPROVER
              END-IF
           END-IF.

      ******************************************************************
       2100-RECORD-SEEN.
           IF WS-SEEN-COUNT < 7
              ADD 1                    TO WS-SEEN-COUNT
              MOVE USR-USER-ID         TO WS-SEEN-USER(WS-SEEN-COUNT)
           END-IF.

      ******************************************************************
      * BR-CLM-AUT-010 No approver in the chain is a warning; the caller
      * decides whether to refuse the transaction or route it manually.
       2200-NO-APPROVER.
           SET CA-RC-WARNING           TO TRUE
           MOVE 'NOAPPRVR'             TO CA-REASON-CODE
           MOVE CA-AT-AMOUNT           TO WS-AMT-DISP
           STRING 'No supervisor within 5 levels can approve '
                  WS-LIMIT-TYPE ' ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       3000-VALIDATE-REQUEST.
           IF CA-AT-USER = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'USERREQD'          TO CA-REASON-CODE
              MOVE 'User id is required for an authority check'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

      * BR-CLM-AUT-001 Only the four defined limit types are checked
           MOVE CA-AT-LIMIT-TYPE       TO WS-LIMIT-TYPE
           IF NOT WS-LT-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LIMTYPE '          TO CA-REASON-CODE
              STRING 'Limit type ' CA-AT-LIMIT-TYPE
                     ' must be RSVEXP PAYEXP PAYCLM or INCCLM'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           IF CA-AT-AMOUNT < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AMTNEG  '          TO CA-REASON-CODE
              MOVE 'Authority amount cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

      * BR-CLM-AUT-002 Claim-level limits are not coverage specific
           IF WS-LT-CLAIM-LEVEL OR CA-AT-COVERAGE-TYPE = SPACES
              MOVE WS-DEFAULT-COVERAGE TO WS-COVERAGE
           ELSE
              MOVE CA-AT-COVERAGE-TYPE TO WS-COVERAGE
           END-IF.

      ******************************************************************
       4000-READ-USER.
           MOVE 'N'                    TO WS-USER-FOUND
           MOVE WS-LOOKUP-USER         TO USR-USER-ID
           EXEC SQL
              SELECT USER_ID, AUTH_PROFILE, SUPERVISOR_ID,
                     ACTIVE_FLAG
                INTO :USR-USER-ID, :USR-AUTH-PROFILE,
                     :USR-SUPERVISOR-ID, :USR-ACTIVE-FLAG
                FROM CLMDB.CLM_USER
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-USER-IS-FOUND  TO TRUE
              WHEN 100
                 CONTINUE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4000-READ-USER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-AUT-003 Coverage-specific row first, then the '*' row
      * BR-CLM-AUT-004 No row at all means zero authority
       4100-READ-LIMIT.
           MOVE 0                      TO WS-USER-LIMIT
           MOVE USR-AUTH-PROFILE       TO AUL-AUTH-PROFILE
           MOVE WS-LIMIT-TYPE          TO AUL-LIMIT-TYPE
           MOVE WS-COVERAGE            TO AUL-COVERAGE-TYPE
           PERFORM 4110-SELECT-LIMIT
           IF SQLCODE = 100
              AND WS-COVERAGE NOT = WS-DEFAULT-COVERAGE
              MOVE WS-DEFAULT-COVERAGE TO AUL-COVERAGE-TYPE
              PERFORM 4110-SELECT-LIMIT
           END-IF
           IF SQLCODE = 0
              MOVE AUL-LIMIT-AMOUNT    TO WS-USER-LIMIT
           END-IF.

      ******************************************************************
       4110-SELECT-LIMIT.
           EXEC SQL
              SELECT LIMIT_AMOUNT
                INTO :AUL-LIMIT-AMOUNT
                FROM CLMDB.CLM_AUTHORITY_LIMIT
               WHERE AUTH_PROFILE  = :AUL-AUTH-PROFILE
                 AND LIMIT_TYPE    = :AUL-LIMIT-TYPE
                 AND COVERAGE_TYPE = :AUL-COVERAGE-TYPE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
              WHEN 100
                 CONTINUE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4110-SELECT-LIMIT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-AUT-007 Climb at most WS-MAX-LEVELS supervisors
       5000-FIND-APPROVER.
           MOVE USR-SUPERVISOR-ID      TO WS-CANDIDATE
           MOVE 0                      TO WS-LEVEL
           MOVE 'N'                    TO WS-WALK-DONE
           PERFORM 5100-EVALUATE-LEVEL
              UNTIL WS-WALK-IS-DONE
                 OR WS-LEVEL >= WS-MAX-LEVELS.

      ******************************************************************
       5100-EVALUATE-LEVEL.
           ADD 1                       TO WS-LEVEL
           IF WS-CANDIDATE = SPACES
              SET WS-WALK-IS-DONE      TO TRUE
           ELSE
              PERFORM 5200-CHECK-LOOP
           END-IF
           IF NOT WS-WALK-IS-DONE
              MOVE WS-CANDIDATE        TO WS-LOOKUP-USER
              PERFORM 4000-READ-USER
              IF NOT WS-USER-IS-FOUND
                 SET WS-WALK-IS-DONE   TO TRUE
              ELSE
                 PERFORM 2100-RECORD-SEEN
                 PERFORM 5300-TRY-CANDIDATE
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-AUT-009 A supervisor already seen means the chain loops
       5200-CHECK-LOOP.
           MOVE 'N'                    TO WS-LOOP-FOUND
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-SEEN-COUNT OR WS-IS-LOOP
              IF WS-SEEN-USER(WS-IX) = WS-CANDIDATE
                 SET WS-IS-LOOP        TO TRUE
              END-IF
           END-PERFORM
           IF WS-IS-LOOP
              SET WS-WALK-IS-DONE      TO TRUE
           END-IF.

      ******************************************************************
      * BR-CLM-AUT-008 Inactive supervisors are skipped, level counted
       5300-TRY-CANDIDATE.
           IF USR-ACTIVE-FLAG = 'Y'
              PERFORM 4100-READ-LIMIT
              IF CA-AT-AMOUNT <= WS-USER-LIMIT
                 MOVE USR-USER-ID      TO CA-AT-APPROVER
                 SET WS-WALK-IS-DONE   TO TRUE
              END-IF
           END-IF
           MOVE USR-SUPERVISOR-ID      TO WS-CANDIDATE.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9800-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Authority tables locked - retry'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'CLMAUTHZ Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-LOOKUP-USER         TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
