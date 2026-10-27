       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILAUTHZ.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILAUTHZ                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Authority limit check. Decides whether a user    *
      *               may perform a monetary or controlled action      *
      *               (write-off, disbursement, reversal, payment plan *
      *               change, hold release) for an amount, from the    *
      *               user's role limit. When the amount is over the   *
      *               limit it walks the supervisor chain to name the  *
      *               first active approver whose role covers it.      *
      * REQUESTS    : AUTCHK                                           *
      * CALLED BY   : write-off, disbursement, payment reversal, plan  *
      *               change and hold services                         *
      * CALLS       : BILERRLG                                         *
      * TABLES      : BIL_USER (R), BIL_AUTHORITY_LIMIT (R)            *
      * RULES       : BR-BIL-AUT-001 The user checked defaults to the  *
      *                              signed-on user                    *
      *               BR-BIL-AUT-002 Limit type is WRITEOFF, DISBURSE, *
      *                              REVERSAL, PLANCHG or HOLDREL      *
      *               BR-BIL-AUT-003 Amount cannot be negative         *
      *               BR-BIL-AUT-004 Unknown or inactive users hold no *
      *                              authority                         *
      *               BR-BIL-AUT-005 A role with no limit row for the  *
      *                              type has no authority, even for a *
      *                              zero amount                       *
      *               BR-BIL-AUT-006 An amount equal to the limit is   *
      *                              within authority                  *
      *               BR-BIL-AUT-007 Over limit: the supervisor chain  *
      *                              is walked at most 5 levels for    *
      *                              the first sufficient approver     *
      *               BR-BIL-AUT-008 Inactive supervisors cannot       *
      *                              approve but the chain continues   *
      *                              to their supervisor               *
      *               BR-BIL-AUT-009 A loop in the supervisor chain    *
      *                              ends the search                   *
      *               BR-BIL-AUT-010 Users never approve their own     *
      *                              requests                          *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILAUTHZ'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

      * BR-BIL-AUT-002 Controlled limit types
       01  WS-LTYPE-DATA.
           05 FILLER                   PIC X(8) VALUE 'WRITEOFF'.
           05 FILLER                   PIC X(8) VALUE 'DISBURSE'.
           05 FILLER                   PIC X(8) VALUE 'REVERSAL'.
           05 FILLER                   PIC X(8) VALUE 'PLANCHG '.
           05 FILLER                   PIC X(8) VALUE 'HOLDREL '.
       01  WS-LTYPE-TABLE REDEFINES WS-LTYPE-DATA.
           05 WS-LTYPE                 PIC X(8) OCCURS 5 TIMES.
       01  WS-LTYPE-COUNT              PIC S9(4) COMP VALUE 5.

       01  WS-CHAIN.
           05 WS-MAX-LEVELS            PIC S9(4) COMP VALUE 5.
           05 WS-LEVEL                 PIC S9(4) COMP VALUE 0.
           05 WS-VISITED-COUNT         PIC S9(4) COMP VALUE 0.
           05 WS-VISITED               PIC X(8) OCCURS 6 TIMES.
           05 WS-NEXT-USER             PIC X(8).
           05 WS-CHAIN-END-FLAG        PIC X     VALUE 'N'.
              88 WS-CHAIN-ENDED                  VALUE 'Y'.
           05 WS-CYCLE-FLAG            PIC X     VALUE 'N'.
              88 WS-CYCLE-FOUND                  VALUE 'Y'.

       01  WS-WORK.
           05 WS-IX                    PIC S9(4) COMP VALUE 0.
           05 WS-TYPE-OK-FLAG          PIC X     VALUE 'N'.
              88 WS-TYPE-OK                      VALUE 'Y'.
           05 WS-USER-FOUND-FLAG       PIC X     VALUE 'N'.
              88 WS-USER-FOUND                   VALUE 'Y'.
           05 WS-LIMIT-FOUND-FLAG      PIC X     VALUE 'N'.
              88 WS-LIMIT-FOUND                  VALUE 'Y'.
           05 WS-LOOKUP-USER           PIC X(8).
           05 WS-LOOKUP-ROLE           PIC X(8).
           05 WS-AMT-DISP              PIC Z(10)9.99.
           05 WS-LIMIT-DISP            PIC Z(10)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDUSR END-EXEC.
           EXEC SQL INCLUDE BILDAUL END-EXEC.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCAUT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'AUTCHK  '
                 PERFORM 2000-CHECK-AUTHORITY
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILAUTHZ: unknown request '
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
           MOVE 0                      TO CA-AT-LIMIT.

      ******************************************************************
       2000-CHECK-AUTHORITY.
           PERFORM 3000-VALIDATE
           MOVE CA-AT-USER             TO WS-LOOKUP-USER
           PERFORM 4000-READ-USER
      * BR-BIL-AUT-004 Unknown / inactive users have no authority
           IF NOT WS-USER-FOUND
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'USRNOTFD'          TO CA-REASON-CODE
              STRING 'User ' CA-AT-USER ' is not a billing user'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF USR-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'USRINACT'          TO CA-REASON-CODE
              STRING 'User ' CA-AT-USER ' is inactive and holds no'
                     ' authority' DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE USR-SUPERVISOR-ID      TO WS-NEXT-USER
           MOVE USR-USER-ROLE          TO WS-LOOKUP-ROLE
           PERFORM 4100-READ-LIMIT
           IF WS-LIMIT-FOUND
              MOVE AUL-LIMIT-AMOUNT    TO CA-AT-LIMIT
           END-IF
      * BR-BIL-AUT-005 BR-BIL-AUT-006 Within the role's own limit
           IF WS-LIMIT-FOUND AND CA-AT-AMOUNT <= AUL-LIMIT-AMOUNT
              MOVE 'Y'                 TO CA-AT-ALLOWED
           ELSE
              PERFORM 5000-FIND-APPROVER
              PERFORM 5100-REPORT-REFERRAL
           END-IF.

      ******************************************************************
       3000-VALIDATE.
      * BR-BIL-AUT-001 Default to the signed-on user
           IF CA-AT-USER = SPACES
              MOVE CA-USER-ID          TO CA-AT-USER
           END-IF
           IF CA-AT-USER = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUTUSER '          TO CA-REASON-CODE
              MOVE 'User to check is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-AUT-002 Limit type
           MOVE 'N'                    TO WS-TYPE-OK-FLAG
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-LTYPE-COUNT OR WS-TYPE-OK
              IF WS-LTYPE(WS-IX) = CA-AT-LIMIT-TYPE
                 MOVE 'Y'              TO WS-TYPE-OK-FLAG
              END-IF
           END-PERFORM
           IF NOT WS-TYPE-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUTLTYPE'          TO CA-REASON-CODE
              STRING 'Limit type ' CA-AT-LIMIT-TYPE
                     ' is not a controlled action'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-AUT-003 Amount
           IF CA-AT-AMOUNT < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUTAMT  '          TO CA-REASON-CODE
              MOVE 'Amount to authorise cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-USER.
           MOVE 'N'                    TO WS-USER-FOUND-FLAG
           MOVE WS-LOOKUP-USER         TO USR-USER-ID
           EXEC SQL
              SELECT USER_ROLE, SUPERVISOR_ID, ACTIVE_FLAG
                INTO :USR-USER-ROLE, :USR-SUPERVISOR-ID,
                     :USR-ACTIVE-FLAG
                FROM BILDB.BIL_USER
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE 'Y'              TO WS-USER-FOUND-FLAG
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4000-READ-USER' TO ERC-ER-PARAGRAPH
                 MOVE WS-LOOKUP-USER   TO ERC-ER-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-READ-LIMIT.
           MOVE 'N'                    TO WS-LIMIT-FOUND-FLAG
           MOVE WS-LOOKUP-ROLE         TO AUL-USER-ROLE
           MOVE CA-AT-LIMIT-TYPE       TO AUL-LIMIT-TYPE
           EXEC SQL
              SELECT LIMIT_AMOUNT
                INTO :AUL-LIMIT-AMOUNT
                FROM BILDB.BIL_AUTHORITY_LIMIT
               WHERE USER_ROLE  = :AUL-USER-ROLE
                 AND LIMIT_TYPE = :AUL-LIMIT-TYPE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE 'Y'              TO WS-LIMIT-FOUND-FLAG
              WHEN 100
                 MOVE 0                TO AUL-LIMIT-AMOUNT
              WHEN OTHER
                 MOVE '4100-READ-LIMIT' TO ERC-ER-PARAGRAPH
                 MOVE WS-LOOKUP-ROLE   TO ERC-ER-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-AUT-007 Walk up to 5 supervisor levels
       5000-FIND-APPROVER.
           MOVE 'N'                    TO WS-CHAIN-END-FLAG
                                          WS-CYCLE-FLAG
           MOVE 1                      TO WS-VISITED-COUNT
           MOVE CA-AT-USER             TO WS-VISITED(1)
           PERFORM VARYING WS-LEVEL FROM 1 BY 1
                   UNTIL WS-LEVEL > WS-MAX-LEVELS
                      OR WS-CHAIN-ENDED
                      OR CA-AT-APPROVER NOT = SPACES
              PERFORM 5010-CHECK-SUPERVISOR
           END-PERFORM.

       5010-CHECK-SUPERVISOR.
           IF WS-NEXT-USER = SPACES
              MOVE 'Y'                 TO WS-CHAIN-END-FLAG
           ELSE
      * BR-BIL-AUT-009 BR-BIL-AUT-010 Stop on a loop (incl. self)
              PERFORM VARYING WS-IX FROM 1 BY 1
                      UNTIL WS-IX > WS-VISITED-COUNT
                 IF WS-VISITED(WS-IX) = WS-NEXT-USER
                    MOVE 'Y'           TO WS-CYCLE-FLAG
                                          WS-CHAIN-END-FLAG
                 END-IF
              END-PERFORM
           END-IF
           IF NOT WS-CHAIN-ENDED
              ADD 1                    TO WS-VISITED-COUNT
              MOVE WS-NEXT-USER        TO WS-VISITED(WS-VISITED-COUNT)
                                          WS-LOOKUP-USER
              PERFORM 4000-READ-USER
              IF NOT WS-USER-FOUND
                 MOVE 'Y'              TO WS-CHAIN-END-FLAG
              ELSE
      * BR-BIL-AUT-008 Inactive supervisor is skipped, chain continues
                 IF USR-ACTIVE-FLAG = 'Y'
                    MOVE USR-USER-ROLE TO WS-LOOKUP-ROLE
                    PERFORM 4100-READ-LIMIT
                    IF WS-LIMIT-FOUND
                    AND CA-AT-AMOUNT <= AUL-LIMIT-AMOUNT
                       MOVE WS-NEXT-USER TO CA-AT-APPROVER
                    END-IF
                 END-IF
                 MOVE USR-SUPERVISOR-ID TO WS-NEXT-USER
              END-IF
           END-IF.

      ******************************************************************
       5100-REPORT-REFERRAL.
           SET CA-RC-AUTHORITY         TO TRUE
           MOVE CA-AT-AMOUNT           TO WS-AMT-DISP
           MOVE CA-AT-LIMIT            TO WS-LIMIT-DISP
           EVALUATE TRUE
              WHEN CA-AT-APPROVER NOT = SPACES
                 MOVE 'AUTAPPRQ'       TO CA-REASON-CODE
                 STRING CA-AT-LIMIT-TYPE ' ' WS-AMT-DISP
                        ' over limit ' WS-LIMIT-DISP
                        ' - approval by ' CA-AT-APPROVER
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN WS-CYCLE-FOUND
                 MOVE 'AUTCYCLE'       TO CA-REASON-CODE
                 STRING 'Supervisor chain of ' CA-AT-USER
                        ' loops - no approver for ' CA-AT-LIMIT-TYPE
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN OTHER
                 MOVE 'AUTNOAPR'       TO CA-REASON-CODE
                 STRING 'No approver within 5 levels for '
                        CA-AT-LIMIT-TYPE ' ' WS-AMT-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
           END-EVALUATE.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
           END-IF
           STRING 'BILAUTHZ Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
