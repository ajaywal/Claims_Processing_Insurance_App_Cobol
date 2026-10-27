       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMASSGN.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMASSGN                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Assignment engine. Chooses the handling group    *
      *               from CLM_ASSIGN_RULE (loss type, segment,        *
      *               severity, region) and the adjuster inside it by  *
      *               round robin or load balancing, assigns           *
      *               exposures and performs manual transfers, while   *
      *               keeping adjuster workload counters current.      *
      * REQUESTS    : ASGCLM ASGEXP ASGXFR                             *
      * CALLED BY   : CLMCLAIM (CLMNEW, CLMROP), CLMEXPOS, CLMLITIG,   *
      *               supervisor transfer screen                       *
      * CALLS       : CLMHIST0 CLMERRLG                                *
      * TABLES      : CLM_ASSIGN_RULE    (R)                           *
      *               CLM_GROUP          (R)                           *
      *               CLM_USER           (R U)                         *
      *               CLM_CODE_TABLE     (R)                           *
      *               CLM_CLAIM          (R U)                         *
      *               CLM_EXPOSURE       (R U)                         *
      *               CLM_ACTIVITY       (U)                           *
      * RULES       : BR-CLM-ASG-001 Rules are evaluated in RULE_SEQ   *
      *                              order; '*' matches any value and  *
      *                              severity must reach MIN_SEVERITY  *
      *               BR-CLM-ASG-002 Region = REGION code-table entry  *
      *                              for the loss postcode area (first *
      *                              2 chars, 1 when the 2nd is a      *
      *                              digit); unknown -> UNKN           *
      *               BR-CLM-ASG-003 Eligible adjuster: active, below  *
      *                              MAX_CLAIMS, LOB_SKILLS holds the  *
      *                              policy type                       *
      *               BR-CLM-ASG-004 RRBN picks the adjuster assigned  *
      *                              longest ago; LOAD picks fewest    *
      *                              open claims (then longest ago)    *
      *               BR-CLM-ASG-005 A matched rule whose group is     *
      *                              inactive or has no eligible       *
      *                              adjuster falls through to the     *
      *                              next rule                         *
      *               BR-CLM-ASG-006 No usable rule -> supervisor of   *
      *                              group FNOL                        *
      *               BR-CLM-ASG-007 OPEN_CLAIMS moves with the claim  *
      *                              owner; LAST_ASSIGN_TS stamped on  *
      *                              every new assignment              *
      *               BR-CLM-ASG-008 Exposures inherit the claim owner *
      *                              unless bodily injury or litigated *
      *                              (routed via the LIAB rules)       *
      *               BR-CLM-ASG-009 Transfer target must be active,   *
      *                              hold the LOB skill and sit in a   *
      *                              group of the same LOB             *
      *               BR-CLM-ASG-010 Transfer over capacity is allowed *
      *                              with warning OVERCAP              *
      *               BR-CLM-ASG-011 On a change of owner, inherited   *
      *                              exposures and the old owner's     *
      *                              open activities move too          *
      *               BR-CLM-ASG-012 Closed claims/exposures cannot be *
      *                              assigned                          *
      *               BR-CLM-ASG-013 Specialist LIT / SIU groups may   *
      *                              receive a claim of any LOB        *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMASSGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(10).
           05 WS-RULE-DISP             PIC 9(4).
           05 WS-FNOL-GROUP            PIC X(8)  VALUE 'FNOL    '.
           05 WS-LIAB-SEGMENT          PIC X(8)  VALUE 'LIAB    '.
           05 WS-POLICY-TYPE           PIC X.
           05 WS-LOSS-TYPE             PIC X(4).
           05 WS-SEGMENT               PIC X(8).
           05 WS-SEVERITY              PIC S9(4) COMP.
           05 WS-REGION                PIC X(4).
           05 WS-AREA                  PIC X(10).
           05 WS-STRATEGY              PIC X(4).
           05 WS-OLD-USER              PIC X(8).
           05 WS-NEW-USER              PIC X(8).
           05 WS-NEW-GROUP             PIC X(8).
           05 WS-OLD-GROUP-TYPE        PIC X(4).
           05 WS-SKILL-COUNT           PIC S9(4) COMP.
           05 WS-RULE-SEQ              PIC S9(4) COMP.
           05 WS-HIST-EVENT            PIC X(8).
           05 WS-HIST-TEXT             PIC X(120).
           05 WS-RULE-EOF-SW           PIC X.
              88 WS-RULE-EOF                     VALUE 'Y'.
           05 WS-USER-EOF-SW           PIC X.
              88 WS-USER-EOF                     VALUE 'Y'.
           05 WS-FOUND-SW              PIC X.
              88 WS-USER-FOUND                   VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDASR END-EXEC.
           EXEC SQL INCLUDE CLMDGRP END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDCOD END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDEXP END-EXEC.

           EXEC SQL DECLARE CSR-RULES CURSOR FOR
              SELECT RULE_SEQ, GROUP_CODE, STRATEGY
                FROM CLMDB.CLM_ASSIGN_RULE
               WHERE ACTIVE_FLAG  = 'Y'
                 AND LOSS_TYPE    IN ('*', :WS-LOSS-TYPE)
                 AND SEGMENT_CODE IN ('*', :WS-SEGMENT)
                 AND REGION_CODE  IN ('*', :WS-REGION)
                 AND MIN_SEVERITY <= :WS-SEVERITY
               ORDER BY RULE_SEQ
               FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-USER-RRBN CURSOR FOR
              SELECT USER_ID, LOB_SKILLS
                FROM CLMDB.CLM_USER
               WHERE GROUP_CODE  = :GRP-GROUP-CODE
                 AND ACTIVE_FLAG = 'Y'
                 AND OPEN_CLAIMS < MAX_CLAIMS
               ORDER BY COALESCE(LAST_ASSIGN_TS,
                        TIMESTAMP('0001-01-01-00.00.00.000000')),
                        USER_ID
               FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-USER-LOAD CURSOR FOR
              SELECT USER_ID, LOB_SKILLS
                FROM CLMDB.CLM_USER
               WHERE GROUP_CODE  = :GRP-GROUP-CODE
                 AND ACTIVE_FLAG = 'Y'
                 AND OPEN_CLAIMS < MAX_CLAIMS
               ORDER BY OPEN_CLAIMS,
                        COALESCE(LAST_ASSIGN_TS,
                        TIMESTAMP('0001-01-01-00.00.00.000000')),
                        USER_ID
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HSC-==.
           03 HSC-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HSC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCASG.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'ASGCLM  '
                 PERFORM 2000-ASSIGN-CLAIM
              WHEN 'ASGEXP  '
                 PERFORM 2100-ASSIGN-EXPOSURE
              WHEN 'ASGXFR  '
                 PERFORM 2200-TRANSFER-CLAIM
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMASSGN: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          CA-AS-ASSIGNED-USER
                                          CA-AS-ASSIGNED-GROUP
                                          WS-NEW-USER
                                          WS-NEW-GROUP
           MOVE 0                      TO CA-AS-RULE-SEQ
                                          WS-RULE-SEQ
           MOVE 'N'                    TO WS-FOUND-SW
           MOVE CA-AS-CLAIM-ID         TO WS-KEY-DISP.

      ******************************************************************
       2000-ASSIGN-CLAIM.
           PERFORM 4000-READ-CLAIM
           MOVE CLM-POLICY-TYPE        TO WS-POLICY-TYPE
           MOVE CLM-ASSIGNED-USER      TO WS-OLD-USER
           IF CA-AS-LOSS-TYPE NOT = SPACES
              MOVE CA-AS-LOSS-TYPE     TO WS-LOSS-TYPE
           ELSE
              MOVE CLM-LOSS-TYPE       TO WS-LOSS-TYPE
           END-IF
           IF CA-AS-SEGMENT NOT = SPACES
              MOVE CA-AS-SEGMENT       TO WS-SEGMENT
           ELSE
              MOVE CLM-SEGMENT-CODE    TO WS-SEGMENT
           END-IF
           IF CA-AS-SEVERITY > 0
              MOVE CA-AS-SEVERITY      TO WS-SEVERITY
           ELSE
              MOVE CLM-SEVERITY        TO WS-SEVERITY
           END-IF
           IF CA-AS-REGION NOT = SPACES
              MOVE CA-AS-REGION        TO WS-REGION
           ELSE
              PERFORM 5000-DERIVE-REGION
           END-IF
           MOVE WS-REGION              TO CA-AS-REGION

           PERFORM 5100-RUN-RULES
           IF NOT WS-USER-FOUND
              PERFORM 5300-FNOL-FALLBACK
           END-IF

           IF WS-NEW-USER = WS-OLD-USER
              AND WS-NEW-GROUP = CLM-ASSIGNED-GROUP
              MOVE 'Claim already with the selected adjuster'
                                       TO CA-MESSAGE
           ELSE
              PERFORM 6000-APPLY-CLAIM-OWNER
              MOVE WS-RULE-SEQ         TO WS-RULE-DISP
              MOVE 'ASSIGN  '          TO WS-HIST-EVENT
              MOVE SPACES              TO WS-HIST-TEXT
              STRING 'Assigned to ' WS-NEW-USER ' group '
                     WS-NEW-GROUP ' by rule ' WS-RULE-DISP
                     ' region ' WS-REGION
                     DELIMITED BY SIZE INTO WS-HIST-TEXT
              PERFORM 8000-WRITE-HISTORY
           END-IF
           MOVE WS-NEW-USER            TO CA-AS-ASSIGNED-USER
           MOVE WS-NEW-GROUP           TO CA-AS-ASSIGNED-GROUP
           MOVE WS-RULE-SEQ            TO CA-AS-RULE-SEQ.

      ******************************************************************
       2100-ASSIGN-EXPOSURE.
           PERFORM 4100-READ-EXPOSURE
           MOVE EXP-CLAIM-ID           TO CA-AS-CLAIM-ID
           PERFORM 4000-READ-CLAIM
           MOVE CLM-POLICY-TYPE        TO WS-POLICY-TYPE
           EVALUATE TRUE
              WHEN CA-AS-TARGET-USER NOT = SPACES
                 MOVE CA-AS-TARGET-USER TO WS-NEW-USER
                 PERFORM 3000-VALIDATE-TARGET-USER
      * BR-CLM-ASG-008 Bodily injury and litigated exposures go to
      * the liability desk through the rule table
              WHEN EXP-COVERAGE-TYPE = 'LIAB_BI '
                OR CLM-LITIGATION-FLAG = 'Y'
                 MOVE CLM-LOSS-TYPE    TO WS-LOSS-TYPE
                 MOVE WS-LIAB-SEGMENT  TO WS-SEGMENT
                 MOVE CLM-SEVERITY     TO WS-SEVERITY
                 IF WS-SEVERITY < 3
                    MOVE 3             TO WS-SEVERITY
                 END-IF
                 PERFORM 5000-DERIVE-REGION
                 PERFORM 5100-RUN-RULES
                 IF NOT WS-USER-FOUND
                    MOVE CLM-ASSIGNED-USER TO WS-NEW-USER
                 END-IF
              WHEN OTHER
                 MOVE CLM-ASSIGNED-USER TO WS-NEW-USER
           END-EVALUATE
           IF WS-NEW-USER = SPACES
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMUNASG'          TO CA-REASON-CODE
              MOVE 'Claim has no owner - assign the claim first'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-NEW-USER NOT = EXP-ASSIGNED-USER
              PERFORM 4500-UPDATE-EXPOSURE-OWNER
              MOVE EXP-EXPOSURE-ID     TO WS-KEY-DISP
              MOVE 'EXPASGN '          TO WS-HIST-EVENT
              MOVE SPACES              TO WS-HIST-TEXT
              STRING 'Exposure ' WS-KEY-DISP ' assigned to '
                     WS-NEW-USER DELIMITED BY SIZE
                     INTO WS-HIST-TEXT
              PERFORM 8000-WRITE-HISTORY
           END-IF
           MOVE WS-NEW-USER            TO CA-AS-ASSIGNED-USER
           MOVE WS-NEW-GROUP           TO CA-AS-ASSIGNED-GROUP
           MOVE WS-RULE-SEQ            TO CA-AS-RULE-SEQ.

      ******************************************************************
       2200-TRANSFER-CLAIM.
           IF CA-AS-TARGET-USER = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TGTUSRRQ'          TO CA-REASON-CODE
              MOVE 'Target user is required for a transfer'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4000-READ-CLAIM
           MOVE CLM-POLICY-TYPE        TO WS-POLICY-TYPE
           MOVE CLM-ASSIGNED-USER      TO WS-OLD-USER
           IF CA-AS-TARGET-USER = WS-OLD-USER
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SAMEUSER'          TO CA-REASON-CODE
              MOVE 'Claim is already assigned to the target user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-AS-TARGET-USER      TO WS-NEW-USER
           PERFORM 3000-VALIDATE-TARGET-USER
           PERFORM 3100-CHECK-GROUP-LOB
           PERFORM 6000-APPLY-CLAIM-OWNER
           MOVE 'TRANSFER'             TO WS-HIST-EVENT
           MOVE SPACES                 TO WS-HIST-TEXT
           STRING 'Transferred from ' WS-OLD-USER ' to '
                  WS-NEW-USER ' group ' WS-NEW-GROUP
                  DELIMITED BY SIZE INTO WS-HIST-TEXT
           PERFORM 8000-WRITE-HISTORY
           MOVE WS-NEW-USER            TO CA-AS-ASSIGNED-USER
           MOVE WS-NEW-GROUP           TO CA-AS-ASSIGNED-GROUP
      * BR-CLM-ASG-010 Supervisor may overload an adjuster knowingly
           IF USR-OPEN-CLAIMS >= USR-MAX-CLAIMS
              SET CA-RC-WARNING        TO TRUE
              MOVE 'OVERCAP '          TO CA-REASON-CODE
              STRING 'Transferred - ' WS-NEW-USER
                     ' is now above MAX_CLAIMS'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-CLM-ASG-009 Target must be an active adjuster with the skill
       3000-VALIDATE-TARGET-USER.
           MOVE WS-NEW-USER            TO USR-USER-ID
           PERFORM 4200-READ-USER
           IF SQLCODE = 100
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'USRNOTFD'          TO CA-REASON-CODE
              STRING 'User ' WS-NEW-USER ' not found'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF USR-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'USRINACT'          TO CA-REASON-CODE
              STRING 'User ' WS-NEW-USER ' is not active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-SKILL-COUNT
           INSPECT USR-LOB-SKILLS TALLYING WS-SKILL-COUNT
                   FOR ALL WS-POLICY-TYPE
           IF WS-SKILL-COUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOSKILL '          TO CA-REASON-CODE
              STRING 'User ' WS-NEW-USER
                     ' lacks the skill for policy type '
                     WS-POLICY-TYPE DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE USR-GROUP-CODE         TO WS-NEW-GROUP.

      ******************************************************************
      * BR-CLM-ASG-009 Same-LOB group: target group type must equal the
      * current group's type (or the loss type when unassigned)
       3100-CHECK-GROUP-LOB.
           IF CLM-ASSIGNED-GROUP NOT = SPACES
              MOVE CLM-ASSIGNED-GROUP  TO GRP-GROUP-CODE
              PERFORM 4300-READ-GROUP
              MOVE GRP-GROUP-TYPE      TO WS-OLD-GROUP-TYPE
           ELSE
              MOVE CLM-LOSS-TYPE       TO WS-OLD-GROUP-TYPE
           END-IF
           MOVE WS-NEW-GROUP           TO GRP-GROUP-CODE
           PERFORM 4300-READ-GROUP
           IF SQLCODE = 100 OR GRP-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GRPINACT'          TO CA-REASON-CODE
              STRING 'Group ' WS-NEW-GROUP ' is not active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-ASG-013 Specialist desks (LIT, SIU) take any LOB
           IF GRP-GROUP-TYPE NOT = WS-OLD-GROUP-TYPE
              AND GRP-GROUP-TYPE NOT = CLM-LOSS-TYPE
              AND GRP-GROUP-TYPE NOT = 'LIT '
              AND GRP-GROUP-TYPE NOT = 'SIU '
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GRPLOBMM'          TO CA-REASON-CODE
              STRING 'Group ' WS-NEW-GROUP ' handles '
                     GRP-GROUP-TYPE ' not ' WS-OLD-GROUP-TYPE
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-CLAIM.
           IF CA-AS-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMIDRQ '          TO CA-REASON-CODE
              MOVE 'Claim id is required for assignment'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-AS-CLAIM-ID         TO CLM-CLAIM-ID
                                          WS-KEY-DISP
           EXEC SQL
              SELECT POLICY_TYPE, LOSS_TYPE, LOSS_POSTCODE,
                     CLAIM_STATE, SEGMENT_CODE, SEVERITY,
                     LITIGATION_FLAG, ASSIGNED_USER, ASSIGNED_GROUP,
                     VERSION_NO
                INTO :CLM-POLICY-TYPE, :CLM-LOSS-TYPE,
                     :CLM-LOSS-POSTCODE :CLM-LOSS-POSTCODE-IND,
                     :CLM-CLAIM-STATE, :CLM-SEGMENT-CODE,
                     :CLM-SEVERITY, :CLM-LITIGATION-FLAG,
                     :CLM-ASSIGNED-USER, :CLM-ASSIGNED-GROUP,
                     :CLM-VERSION-NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CLM-LOSS-POSTCODE-IND < 0
                    MOVE SPACES        TO CLM-LOSS-POSTCODE
                 END-IF
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 STRING 'Claim ' WS-KEY-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-CLAIM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
      * BR-CLM-ASG-012 Closed work is never (re)assigned
           IF CLM-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLSD '          TO CA-REASON-CODE
              MOVE 'Closed claims cannot be assigned'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4100-READ-EXPOSURE.
           IF CA-AS-EXPOSURE-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPIDRQ '          TO CA-REASON-CODE
              MOVE 'Exposure id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-AS-EXPOSURE-ID      TO EXP-EXPOSURE-ID
                                          WS-KEY-DISP
           EXEC SQL
              SELECT CLAIM_ID, COVERAGE_TYPE, EXPOSURE_STATE,
                     ASSIGNED_USER, VERSION_NO
                INTO :EXP-CLAIM-ID, :EXP-COVERAGE-TYPE,
                     :EXP-EXPOSURE-STATE, :EXP-ASSIGNED-USER,
                     :EXP-VERSION-NO
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EXPNOTFD'       TO CA-REASON-CODE
                 STRING 'Exposure ' WS-KEY-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-EXPOSURE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF EXP-EXPOSURE-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPCLSD '          TO CA-REASON-CODE
              MOVE 'Closed exposures cannot be assigned'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4200-READ-USER.
           EXEC SQL
              SELECT GROUP_CODE, LOB_SKILLS, OPEN_CLAIMS,
                     MAX_CLAIMS, ACTIVE_FLAG
                INTO :USR-GROUP-CODE, :USR-LOB-SKILLS,
                     :USR-OPEN-CLAIMS, :USR-MAX-CLAIMS,
                     :USR-ACTIVE-FLAG
                FROM CLMDB.CLM_USER
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-READ-USER'    TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-READ-GROUP.
           EXEC SQL
              SELECT GROUP_TYPE, SUPERVISOR_ID, ACTIVE_FLAG
                INTO :GRP-GROUP-TYPE, :GRP-SUPERVISOR-ID,
                     :GRP-ACTIVE-FLAG
                FROM CLMDB.CLM_GROUP
               WHERE GROUP_CODE = :GRP-GROUP-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE SPACES           TO GRP-GROUP-TYPE
                                          GRP-SUPERVISOR-ID
                 MOVE 'N'              TO GRP-ACTIVE-FLAG
              WHEN OTHER
                 MOVE '4300-READ-GROUP' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-UPDATE-EXPOSURE-OWNER.
           MOVE WS-NEW-USER            TO EXP-ASSIGNED-USER
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET ASSIGNED_USER = :EXP-ASSIGNED-USER,
                     UPDATE_TS     = CURRENT TIMESTAMP,
                     VERSION_NO    = VERSION_NO + 1
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
                 AND VERSION_NO  = :EXP-VERSION-NO
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 0
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Exposure changed by another user - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = 0
                 CONTINUE
              WHEN OTHER
                 MOVE '4500-UPDATE-EXPOSURE-OWNER'
                                       TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-ASG-002 Postcode area -> region via code table REGION
      * (CODE_VALUE = area, PARENT_CODE = region code)
       5000-DERIVE-REGION.
           MOVE 'UNKN'                 TO WS-REGION
           MOVE SPACES                 TO WS-AREA
           IF CLM-LOSS-POSTCODE NOT = SPACES
              MOVE FUNCTION UPPER-CASE(CLM-LOSS-POSTCODE(1:2))
                                       TO WS-AREA(1:2)
              IF WS-AREA(2:1) IS NUMERIC
                 MOVE SPACE            TO WS-AREA(2:1)
              END-IF
              MOVE WS-AREA             TO COD-CODE-VALUE
              EXEC SQL
                 SELECT PARENT_CODE
                   INTO :COD-PARENT-CODE
                   FROM CLMDB.CLM_CODE_TABLE
                  WHERE TABLE_NAME  = 'REGION'
                    AND CODE_VALUE  = :COD-CODE-VALUE
                    AND ACTIVE_FLAG = 'Y'
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF COD-PARENT-CODE NOT = SPACES
                       MOVE COD-PARENT-CODE(1:4) TO WS-REGION
                    END-IF
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '5000-DERIVE-REGION' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
      * BR-CLM-ASG-001 First matching rule in RULE_SEQ order wins
      * BR-CLM-ASG-005 ...unless it cannot staff the claim
       5100-RUN-RULES.
           MOVE 'N'                    TO WS-FOUND-SW
                                          WS-RULE-EOF-SW
           EXEC SQL OPEN CSR-RULES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5100-RUN-RULES'    TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-RULE-EOF OR WS-USER-FOUND
              EXEC SQL
                 FETCH CSR-RULES
                  INTO :ASR-RULE-SEQ, :ASR-GROUP-CODE, :ASR-STRATEGY
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE ASR-GROUP-CODE TO GRP-GROUP-CODE
                    PERFORM 4300-READ-GROUP
                    IF GRP-ACTIVE-FLAG = 'Y'
                       MOVE ASR-STRATEGY TO WS-STRATEGY
                       PERFORM 5200-PICK-USER
                       IF WS-USER-FOUND
                          MOVE ASR-RULE-SEQ   TO WS-RULE-SEQ
                          MOVE ASR-GROUP-CODE TO WS-NEW-GROUP
                       END-IF
                    END-IF
                 WHEN 100
                    SET WS-RULE-EOF    TO TRUE
                 WHEN OTHER
                    EXEC SQL CLOSE CSR-RULES END-EXEC
                    MOVE '5100-RUN-RULES' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-RULES END-EXEC.

      ******************************************************************
      * BR-CLM-ASG-004 RRBN = least recently assigned; LOAD = fewest
      * open claims. Unknown strategies default to round robin.
      * BR-CLM-ASG-003 Capacity in the cursor, LOB skill checked here
       5200-PICK-USER.
           MOVE 'N'                    TO WS-USER-EOF-SW
           IF WS-STRATEGY = 'LOAD'
              EXEC SQL OPEN CSR-USER-LOAD END-EXEC
           ELSE
              EXEC SQL OPEN CSR-USER-RRBN END-EXEC
           END-IF
           IF SQLCODE NOT = 0
              EXEC SQL CLOSE CSR-RULES END-EXEC
              MOVE '5200-PICK-USER'    TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-USER-EOF OR WS-USER-FOUND
              IF WS-STRATEGY = 'LOAD'
                 EXEC SQL
                    FETCH CSR-USER-LOAD
                     INTO :USR-USER-ID, :USR-LOB-SKILLS
                 END-EXEC
              ELSE
                 EXEC SQL
                    FETCH CSR-USER-RRBN
                     INTO :USR-USER-ID, :USR-LOB-SKILLS
                 END-EXEC
              END-IF
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE 0             TO WS-SKILL-COUNT
                    INSPECT USR-LOB-SKILLS TALLYING WS-SKILL-COUNT
                            FOR ALL WS-POLICY-TYPE
                    IF WS-SKILL-COUNT > 0
                       SET WS-USER-FOUND TO TRUE
                       MOVE USR-USER-ID TO WS-NEW-USER
                    END-IF
                 WHEN 100
                    SET WS-USER-EOF    TO TRUE
                 WHEN OTHER
                    PERFORM 5210-CLOSE-USER-CURSOR
                    EXEC SQL CLOSE CSR-RULES END-EXEC
                    MOVE '5200-PICK-USER' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 5210-CLOSE-USER-CURSOR.

      ******************************************************************
       5210-CLOSE-USER-CURSOR.
           IF WS-STRATEGY = 'LOAD'
              EXEC SQL CLOSE CSR-USER-LOAD END-EXEC
           ELSE
              EXEC SQL CLOSE CSR-USER-RRBN END-EXEC
           END-IF.

      ******************************************************************
      * BR-CLM-ASG-006 Nobody eligible: the FNOL supervisor triages
       5300-FNOL-FALLBACK.
           MOVE WS-FNOL-GROUP          TO GRP-GROUP-CODE
           PERFORM 4300-READ-GROUP
           IF GRP-SUPERVISOR-ID = SPACES
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'NOFNOLGP'          TO CA-REASON-CODE
              MOVE 'No rule matched and FNOL group has no supervisor'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE GRP-SUPERVISOR-ID      TO WS-NEW-USER
           MOVE WS-FNOL-GROUP          TO WS-NEW-GROUP
           MOVE 0                      TO WS-RULE-SEQ
           SET WS-USER-FOUND           TO TRUE
           SET CA-RC-WARNING           TO TRUE
           MOVE 'ASGFBACK'             TO CA-REASON-CODE
           STRING 'No assignment rule could staff the claim - sent to'
                  ' FNOL supervisor ' WS-NEW-USER
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-CLM-ASG-007 Workload counters follow the owner
      * BR-CLM-ASG-011 Inherited exposures and open activities follow
       6000-APPLY-CLAIM-OWNER.
           IF WS-OLD-USER NOT = SPACES
              AND WS-OLD-USER NOT = WS-NEW-USER
              MOVE WS-OLD-USER         TO USR-USER-ID
              EXEC SQL
                 UPDATE CLMDB.CLM_USER
                    SET OPEN_CLAIMS = OPEN_CLAIMS - 1
                  WHERE USER_ID     = :USR-USER-ID
                    AND OPEN_CLAIMS > 0
              END-EXEC
              IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                 MOVE '6000-APPLY-CLAIM-OWNER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF
           IF WS-OLD-USER NOT = WS-NEW-USER
              MOVE WS-NEW-USER         TO USR-USER-ID
              EXEC SQL
                 UPDATE CLMDB.CLM_USER
                    SET OPEN_CLAIMS    = OPEN_CLAIMS + 1,
                        LAST_ASSIGN_TS = CURRENT TIMESTAMP
                  WHERE USER_ID = :USR-USER-ID
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '6000-APPLY-CLAIM-OWNER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF

           MOVE WS-NEW-USER            TO CLM-ASSIGNED-USER
           MOVE WS-NEW-GROUP           TO CLM-ASSIGNED-GROUP
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET ASSIGNED_USER  = :CLM-ASSIGNED-USER,
                     ASSIGNED_GROUP = :CLM-ASSIGNED-GROUP,
                     UPDATE_TS      = CURRENT TIMESTAMP,
                     VERSION_NO     = VERSION_NO + 1
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 0
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Claim changed by another user - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = 0
                 CONTINUE
              WHEN OTHER
                 MOVE '6000-APPLY-CLAIM-OWNER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE

           MOVE WS-OLD-USER            TO EXP-ASSIGNED-USER
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET ASSIGNED_USER = :CLM-ASSIGNED-USER,
                     UPDATE_TS     = CURRENT TIMESTAMP,
                     VERSION_NO    = VERSION_NO + 1
               WHERE CLAIM_ID       = :CLM-CLAIM-ID
                 AND EXPOSURE_STATE <> 'CLSD'
                 AND (ASSIGNED_USER = ' '
                      OR ASSIGNED_USER = :EXP-ASSIGNED-USER)
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '6000-APPLY-CLAIM-OWNER' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF

           IF WS-OLD-USER NOT = SPACES
              AND WS-OLD-USER NOT = WS-NEW-USER
              EXEC SQL
                 UPDATE CLMDB.CLM_ACTIVITY
                    SET ASSIGNED_USER = :CLM-ASSIGNED-USER
                  WHERE CLAIM_ID        = :CLM-CLAIM-ID
                    AND ASSIGNED_USER   = :EXP-ASSIGNED-USER
                    AND ACTIVITY_STATUS = 'OPEN'
              END-EXEC
              IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                 MOVE '6000-APPLY-CLAIM-OWNER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF.

      ******************************************************************
       8000-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HSC-REQUEST-ID
           MOVE CA-USER-ID             TO HSC-USER-ID
           MOVE CA-BUS-DATE            TO HSC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HSC-CALLER-PGM
                                          HSC-HI-PROGRAM
           MOVE CLM-CLAIM-ID           TO HSC-HI-CLAIM-ID
           MOVE WS-HIST-EVENT          TO HSC-HI-EVENT-TYPE
           MOVE WS-HIST-TEXT           TO HSC-HI-DESCRIPTION
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF HSC-RETURN-CODE >= 12
              MOVE HSC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HSC-REASON-CODE     TO CA-REASON-CODE
              MOVE HSC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

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
           STRING 'CLMASSGN Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-KEY-DISP            TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
