       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSIUSC.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMSIUSC                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Special Investigation Unit (fraud) scoring.      *
      *               Walks the active CLM_SIU_RULE rows, evaluates    *
      *               each indicator implemented here against the      *
      *               claim, policy snapshot, prior claims, incidents  *
      *               and documents, replaces the claim's score rows,  *
      *               and refers the claim to SIU when the total       *
      *               reaches the referral threshold.                  *
      * REQUESTS    : SIUSCR                                           *
      * CALLED BY   : CLMCLAIM (open), CLMINCDT, SIU rescore batch,    *
      *               SIU screen                                       *
      * CALLS       : CLMDATES CLMACTV0 CLMHIST0 CLMAUDCH CLMERRLG     *
      * TABLES      : CLM_SIU_RULE (R)  CLM_SIU_SCORE (C D)            *
      *               CLM_CLAIM (R U)  CLM_POLICY_SNAPSHOT (R)         *
      *               CLM_INCIDENT (R)  CLM_DOCUMENT (R)               *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-SIU-001 Closed claims are not scored      *
      *               BR-CLM-SIU-002 Only ACTIVE rules score; points   *
      *                              come from CLM_SIU_RULE            *
      *               BR-CLM-SIU-003 Each evaluation replaces all      *
      *                              score rows of the claim           *
      *               BR-CLM-SIU-004 LATERPT reported more than 30     *
      *                              days after the loss               *
      *               BR-CLM-SIU-005 NEWPOL loss within 30 days of     *
      *                              policy inception                  *
      *               BR-CLM-SIU-006 PRIORCL other claims on the same  *
      *                              policy in the 12 months to loss   *
      *               BR-CLM-SIU-007 HIVALTL total loss with incident  *
      *                              damage over 25,000                *
      *               BR-CLM-SIU-008 WKNDTHF theft on a Saturday or    *
      *                              Sunday                            *
      *               BR-CLM-SIU-009 NOPOLREF theft / vandalism with   *
      *                              no police report received         *
      *               BR-CLM-SIU-010 Total >= 50 refers the claim:     *
      *                              SIU_STATUS REFR, SIUREF workflow, *
      *                              history and audit chain; a claim  *
      *                              already with SIU keeps its status *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMSIUSC'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.

       01  WS-THRESHOLDS.
           05 WS-LATE-REPORT-DAYS      PIC S9(4) COMP VALUE 30.
           05 WS-NEW-POLICY-DAYS       PIC S9(4) COMP VALUE 30.
           05 WS-PRIOR-MONTHS          PIC S9(4) COMP VALUE -12.
           05 WS-HIGH-VALUE-TL         PIC S9(11)V99 COMP-3
                                                    VALUE 25000.
           05 WS-REFERRAL-SCORE        PIC S9(4) COMP VALUE 50.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-TOTAL                 PIC S9(4) COMP VALUE 0.
           05 WS-MAX-DAMAGE            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-MAX-DAMAGE-IND        PIC S9(4) COMP VALUE 0.
           05 WS-LOOKBACK-DATE         PIC X(10) VALUE SPACES.
           05 WS-NEW-STATUS            PIC X(4)  VALUE SPACES.
           05 WS-EVIDENCE              PIC X(80) VALUE SPACES.
           05 WS-HIT-SW                PIC X     VALUE 'N'.
              88 WS-RULE-HIT                     VALUE 'Y'.
              88 WS-RULE-MISS                    VALUE 'N'.
           05 WS-CAUSE                 PIC X(10) VALUE SPACES.
              88 WS-THEFT-CAUSE                  VALUE 'THEFT'.
              88 WS-POLICE-CAUSE                 VALUE 'THEFT'
                                                       'VANDALISM'.
           05 WS-DATE-REQ              PIC X(8)  VALUE SPACES.
           05 WS-DATE-1                PIC X(10) VALUE SPACES.
           05 WS-DATE-2                PIC X(10) VALUE SPACES.
           05 WS-DATE-NUM              PIC S9(5) COMP-3 VALUE 0.
           05 WS-DAYS-DISP             PIC ZZZ9.
           05 WS-CNT-DISP              PIC ZZ9.
           05 WS-PTS-DISP              PIC ZZZ9.
           05 WS-OLD-PTS-DISP          PIC ZZZ9.
           05 WS-AMT-DISP              PIC Z(10)9.99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-HIS-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-DONE                   VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.
           05 WS-CSR-RULE-SW           PIC X     VALUE 'N'.
              88 WS-CSR-RULE-OPEN                VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDSRL END-EXEC.
           EXEC SQL INCLUDE CLMDSSC END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDPSN END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CSR-RULES CURSOR FOR
              SELECT RULE_CODE, POINTS
                FROM CLMDB.CLM_SIU_RULE
               WHERE ACTIVE_FLAG = 'Y'
               ORDER BY RULE_CODE
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-SVC-RESULT.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SVC-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.

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
              COPY CLMCSIU.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'SIUSCR  '
                 PERFORM 2000-SCORE-CLAIM
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMSIUSC: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-SI-TOTAL-SCORE
                                          CA-SI-RULES-HIT
           MOVE 'N'                    TO CA-SI-REFERRED
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT > 12
              MOVE SPACES              TO CA-SI-H-RULE(WS-COUNT)
              MOVE 0                   TO CA-SI-H-POINTS(WS-COUNT)
           END-PERFORM
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
      * SIUSCR
       2000-SCORE-CLAIM.
           IF CA-SI-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for SIU scoring'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM
      * BR-CLM-SIU-001 Closed claims are not scored
           IF CLM-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLOSD'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER
                     ' is closed - SIU scoring not run'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CLM-LOSS-CAUSE         TO WS-CAUSE
      * BR-CLM-SIU-003 Replace the previous evaluation
           EXEC SQL
              DELETE FROM CLMDB.CLM_SIU_SCORE
               WHERE CLAIM_ID = :CA-SI-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '2000-SCORE-CLAIM'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-TOTAL
      * BR-CLM-SIU-002 Only active rules, points from the rule table
           EXEC SQL OPEN CSR-RULES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-SCORE-CLAIM'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CSR-RULE-OPEN        TO TRUE
           SET WS-FETCH-MORE           TO TRUE
           PERFORM UNTIL WS-FETCH-DONE
              EXEC SQL
                 FETCH CSR-RULES
                  INTO :SRL-RULE-CODE, :SRL-POINTS
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 2100-EVALUATE-RULE
                 WHEN 100
                    SET WS-FETCH-DONE  TO TRUE
                 WHEN OTHER
                    MOVE '2000-SCORE-CLAIM' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-RULES END-EXEC
           MOVE 'N'                    TO WS-CSR-RULE-SW
           MOVE WS-TOTAL               TO CA-SI-TOTAL-SCORE
           PERFORM 2200-APPLY-SCORE.

      ******************************************************************
      * Dispatch to the COBOL implementation of each indicator; a rule
      * with no implementation here contributes nothing
       2100-EVALUATE-RULE.
           SET WS-RULE-MISS            TO TRUE
           MOVE SPACES                 TO WS-EVIDENCE
           EVALUATE SRL-RULE-CODE
              WHEN 'LATERPT '
                 PERFORM 3100-RULE-LATE-REPORT
              WHEN 'NEWPOL  '
                 PERFORM 3200-RULE-NEW-POLICY
              WHEN 'PRIORCL '
                 PERFORM 3300-RULE-PRIOR-CLAIMS
              WHEN 'HIVALTL '
                 PERFORM 3400-RULE-HIGH-VALUE-TL
              WHEN 'WKNDTHF '
                 PERFORM 3500-RULE-WEEKEND-THEFT
              WHEN 'NOPOLREF'
                 PERFORM 3600-RULE-NO-POLICE-REF
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           IF WS-RULE-HIT AND SRL-POINTS > 0
              PERFORM 4200-INSERT-SCORE
              ADD SRL-POINTS           TO WS-TOTAL
              IF CA-SI-RULES-HIT < 12
                 ADD 1                 TO CA-SI-RULES-HIT
                 MOVE SRL-RULE-CODE    TO
                                     CA-SI-H-RULE(CA-SI-RULES-HIT)
                 MOVE SRL-POINTS       TO
                                     CA-SI-H-POINTS(CA-SI-RULES-HIT)
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-SIU-010 Referral at the threshold; SIU-owned statuses
      * (already referred / investigating) are never downgraded
       2200-APPLY-SCORE.
           MOVE CLM-SIU-STATUS         TO WS-NEW-STATUS
           IF WS-TOTAL >= WS-REFERRAL-SCORE
              AND CLM-SIU-STATUS = 'NONE'
              MOVE 'REFR'              TO WS-NEW-STATUS
           END-IF
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET SIU_SCORE  = :WS-TOTAL,
                     SIU_STATUS = :WS-NEW-STATUS,
                     UPDATE_TS  = CURRENT TIMESTAMP,
                     VERSION_NO = VERSION_NO + 1
               WHERE CLAIM_ID   = :CA-SI-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-APPLY-SCORE'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Claim changed by another user - rescore'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-TOTAL               TO WS-PTS-DISP
           MOVE CA-SI-RULES-HIT        TO WS-CNT-DISP
           MOVE 'SIUSCORE'             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           MOVE CLM-SIU-SCORE          TO WS-OLD-PTS-DISP
           STRING 'SIU score ' WS-PTS-DISP ' from ' WS-CNT-DISP
                  ' indicators (was ' WS-OLD-PTS-DISP ')'
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8200-WRITE-HISTORY
           IF WS-NEW-STATUS NOT = 'NONE'
              MOVE 'Y'                 TO CA-SI-REFERRED
           END-IF
           IF WS-NEW-STATUS = 'REFR' AND CLM-SIU-STATUS = 'NONE'
              PERFORM 8100-GENERATE-SIUREF
              MOVE WS-TOTAL            TO WS-PTS-DISP
              MOVE 'SIUREFR '          TO WS-HIS-EVENT
              MOVE SPACES              TO WS-HIS-DESC
              STRING 'Claim referred to SIU - score ' WS-PTS-DISP
                     ' reached threshold' DELIMITED BY SIZE
                     INTO WS-HIS-DESC
              PERFORM 8200-WRITE-HISTORY
              PERFORM 8300-AUDIT-REFERRAL
              SET CA-RC-WARNING        TO TRUE
              MOVE 'SIUREFR '          TO CA-REASON-CODE
              STRING 'Claim referred to SIU, score ' WS-PTS-DISP
                     DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              MOVE WS-TOTAL            TO WS-PTS-DISP
              STRING 'SIU score ' WS-PTS-DISP ', status '
                     WS-NEW-STATUS DELIMITED BY SIZE
                     INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-CLM-SIU-004 Reported more than 30 days after the loss
       3100-RULE-LATE-REPORT.
           MOVE 'DTDIFF  '             TO WS-DATE-REQ
           MOVE CLM-LOSS-DATE          TO WS-DATE-1
           MOVE CLM-REPORTED-DATE      TO WS-DATE-2
           PERFORM 8000-CALL-DATES
           IF DTA-DT-RESULT-NUM > WS-LATE-REPORT-DAYS
              SET WS-RULE-HIT          TO TRUE
              MOVE DTA-DT-RESULT-NUM   TO WS-DAYS-DISP
              STRING 'Reported ' WS-DAYS-DISP ' days after loss '
                     CLM-LOSS-DATE DELIMITED BY SIZE
                     INTO WS-EVIDENCE
           END-IF.

      ******************************************************************
      * BR-CLM-SIU-005 Loss within 30 days of inception (snapshot);
      * no snapshot means the indicator cannot be assessed
       3200-RULE-NEW-POLICY.
           EXEC SQL
              SELECT EFF_DATE
                INTO :PSN-EFF-DATE
                FROM CLMDB.CLM_POLICY_SNAPSHOT
               WHERE CLAIM_ID = :CA-SI-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE 'DTDIFF  '       TO WS-DATE-REQ
                 MOVE PSN-EFF-DATE     TO WS-DATE-1
                 MOVE CLM-LOSS-DATE    TO WS-DATE-2
                 PERFORM 8000-CALL-DATES
                 IF DTA-DT-RESULT-NUM >= 0
                    AND DTA-DT-RESULT-NUM <= WS-NEW-POLICY-DAYS
                    SET WS-RULE-HIT    TO TRUE
                    MOVE DTA-DT-RESULT-NUM TO WS-DAYS-DISP
                    STRING 'Loss ' WS-DAYS-DISP
                           ' days after inception ' PSN-EFF-DATE
                           DELIMITED BY SIZE INTO WS-EVIDENCE
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '3200-RULE-NEW-POLICY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-SIU-006 Other claims on the policy, 12 months to loss
       3300-RULE-PRIOR-CLAIMS.
           MOVE 'DTADDM  '             TO WS-DATE-REQ
           MOVE CLM-LOSS-DATE          TO WS-DATE-1
           MOVE WS-PRIOR-MONTHS        TO WS-DATE-NUM
           PERFORM 8000-CALL-DATES
           MOVE DTA-DT-RESULT-DATE     TO WS-LOOKBACK-DATE
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM
               WHERE GENAPP_POL_NUM = :CLM-GENAPP-POL-NUM
                 AND CLAIM_ID      <> :CA-SI-CLAIM-ID
                 AND LOSS_DATE BETWEEN :WS-LOOKBACK-DATE
                                   AND :CLM-LOSS-DATE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3300-RULE-PRIOR-CLAIMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET WS-RULE-HIT          TO TRUE
              MOVE WS-COUNT            TO WS-CNT-DISP
              STRING WS-CNT-DISP ' other claim(s) on policy since '
                     WS-LOOKBACK-DATE DELIMITED BY SIZE
                     INTO WS-EVIDENCE
           END-IF.

      ******************************************************************
      * BR-CLM-SIU-007 Total loss with incident damage over 25,000
       3400-RULE-HIGH-VALUE-TL.
           EXEC SQL
              SELECT MAX(ESTIMATED_DAMAGE)
                INTO :WS-MAX-DAMAGE :WS-MAX-DAMAGE-IND
                FROM CLMDB.CLM_INCIDENT
               WHERE CLAIM_ID = :CA-SI-CLAIM-ID
                 AND (TOTAL_LOSS_FLAG = 'Y'
                      OR :CLM-TOTAL-LOSS-FLAG = 'Y')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3400-RULE-HIGH-VALUE-TL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-MAX-DAMAGE-IND = 0
              AND WS-MAX-DAMAGE > WS-HIGH-VALUE-TL
              SET WS-RULE-HIT          TO TRUE
              MOVE WS-MAX-DAMAGE       TO WS-AMT-DISP
              STRING 'Total loss incident damage ' WS-AMT-DISP
                     DELIMITED BY SIZE INTO WS-EVIDENCE
           END-IF.

      ******************************************************************
      * BR-CLM-SIU-008 Theft on a Saturday or Sunday (ISO 6 / 7)
       3500-RULE-WEEKEND-THEFT.
           IF WS-THEFT-CAUSE
              MOVE 'DTDOW   '          TO WS-DATE-REQ
              MOVE CLM-LOSS-DATE       TO WS-DATE-1
              PERFORM 8000-CALL-DATES
              IF DTA-DT-RESULT-NUM >= 6
                 SET WS-RULE-HIT       TO TRUE
                 STRING 'Theft on weekend date ' CLM-LOSS-DATE
                        DELIMITED BY SIZE INTO WS-EVIDENCE
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-SIU-009 Theft / vandalism without a received police
      * report (CLMDOCRQ withdraws this score on receipt)
       3600-RULE-NO-POLICE-REF.
           IF WS-POLICE-CAUSE
              EXEC SQL
                 SELECT COUNT(*)
                   INTO :WS-COUNT
                   FROM CLMDB.CLM_DOCUMENT
                  WHERE CLAIM_ID   = :CA-SI-CLAIM-ID
                    AND DOC_TYPE   = 'POLICERP'
                    AND DOC_STATUS = 'RCVD'
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '3600-RULE-NO-POLICE-REF' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              IF WS-COUNT = 0
                 SET WS-RULE-HIT       TO TRUE
                 STRING CLM-LOSS-CAUSE ' with no police report '
                        'received' DELIMITED BY SIZE
                        INTO WS-EVIDENCE
              END-IF
           END-IF.

      ******************************************************************
       4100-READ-CLAIM.
           EXEC SQL
              SELECT CLAIM_NUMBER, GENAPP_POL_NUM, LOSS_DATE,
                     REPORTED_DATE, LOSS_CAUSE, CLAIM_STATE,
                     TOTAL_LOSS_FLAG, SIU_SCORE, SIU_STATUS,
                     VERSION_NO
                INTO :CLM-CLAIM-NUMBER, :CLM-GENAPP-POL-NUM,
                     :CLM-LOSS-DATE, :CLM-REPORTED-DATE,
                     :CLM-LOSS-CAUSE, :CLM-CLAIM-STATE,
                     :CLM-TOTAL-LOSS-FLAG, :CLM-SIU-SCORE,
                     :CLM-SIU-STATUS, :CLM-VERSION-NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-SI-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-SI-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-INSERT-SCORE.
           MOVE CA-SI-CLAIM-ID         TO SSC-CLAIM-ID
           MOVE SRL-RULE-CODE          TO SSC-RULE-CODE
           MOVE SRL-POINTS             TO SSC-POINTS
           MOVE WS-EVIDENCE            TO SSC-EVIDENCE-TEXT-TEXT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-EVIDENCE TRAILING))
                                       TO SSC-EVIDENCE-TEXT-LEN
           MOVE 0                      TO SSC-EVIDENCE-TEXT-IND
           EXEC SQL
              INSERT INTO CLMDB.CLM_SIU_SCORE
                     (CLAIM_ID, RULE_CODE, POINTS, EVIDENCE_TEXT,
                      EVAL_TS)
              VALUES (:SSC-CLAIM-ID, :SSC-RULE-CODE, :SSC-POINTS,
                      :SSC-EVIDENCE-TEXT :SSC-EVIDENCE-TEXT-IND,
                      CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-INSERT-SCORE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Date service: request, dates and number set by the caller
       8000-CALL-DATES.
           INITIALIZE WS-DTE-CA
           MOVE WS-DATE-REQ            TO DTA-REQUEST-ID
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE WS-DATE-1              TO DTA-DT-DATE-1
           MOVE WS-DATE-2              TO DTA-DT-DATE-2
           MOVE WS-DATE-NUM            TO DTA-DT-NUMBER
           CALL WS-DTE-PGM USING WS-DTE-CA
           MOVE DTA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8100-GENERATE-SIUREF.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTGEN  '             TO AVA-REQUEST-ID
           MOVE CA-USER-ID             TO AVA-USER-ID
           MOVE WS-BUS-DATE            TO AVA-BUS-DATE
           MOVE CA-TRACE-ID            TO AVA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AVA-CALLER-PGM
           MOVE CA-SI-CLAIM-ID         TO AVA-AV-CLAIM-ID
           MOVE 'SIUREF  '             TO AVA-AV-EVENT
           CALL WS-ACT-PGM USING WS-ACT-CA
           MOVE AVA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8200-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-SI-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HIA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Referral and the indicators behind it are sealed in the chain
       8300-AUDIT-REFERRAL.
           INITIALIZE WS-AUD-CA
           MOVE 'AUCADD  '             TO ADA-REQUEST-ID
           MOVE CA-USER-ID             TO ADA-USER-ID
           MOVE WS-BUS-DATE            TO ADA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ADA-CALLER-PGM
           MOVE CA-SI-CLAIM-ID         TO ADA-AC-CLAIM-ID
           MOVE 'SIUREFR '             TO ADA-AC-EVENT-CODE
           MOVE WS-PROGRAM-ID          TO ADA-AC-PROGRAM
           MOVE WS-TOTAL               TO WS-PTS-DISP
           STRING CLM-CLAIM-NUMBER '|' WS-PTS-DISP '|'
                  CA-SI-HIT(1) CA-SI-HIT(2) CA-SI-HIT(3)
                  CA-SI-HIT(4) CA-SI-HIT(5) CA-SI-HIT(6)
                  DELIMITED BY SIZE INTO ADA-AC-PAYLOAD
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
           IF WS-CSR-RULE-OPEN
              EXEC SQL CLOSE CSR-RULES END-EXEC
           END-IF
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'SIU data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMSIUSC Db2 error SQLCODE ' WS-SQLCODE-DISP
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
              MOVE CA-SI-CLAIM-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
