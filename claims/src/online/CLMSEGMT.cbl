       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSEGMT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMSEGMT                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim segmentation and severity. Derives the     *
      *               handling segment and a 1..5 severity from the    *
      *               loss type and cause, the incidents recorded so   *
      *               far (injury, fatality, damage, total loss),      *
      *               litigation and catastrophe tagging, then stores  *
      *               them on the claim and cascades the segment to    *
      *               exposures that follow the claim segment.         *
      * REQUESTS    : SEGCLM                                           *
      * CALLED BY   : CLMCLAIM (CLMNEW, CLMUPD), CLMINCDT, CLMLITIG,   *
      *               CLMTOTLS                                         *
      * CALLS       : CLMHIST0 CLMACTV0 CLMERRLG                       *
      * TABLES      : CLM_CLAIM          (R U)                         *
      *               CLM_INCIDENT       (R)                           *
      *               CLM_LITIGATION     (R)                           *
      *               CLM_EXPOSURE       (U)                           *
      * RULES       : BR-CLM-SEG-001 Base segment by loss type: AUTO   *
      *                              AUTOSTD, PROP PROPSTD, LIAB LIAB, *
      *                              all at severity 2                 *
      *               BR-CLM-SEG-002 AUTO fast track: glass or single  *
      *                              vehicle, damage < 2,500, no       *
      *                              injury, no total loss, no         *
      *                              litigation -> AUTOFT severity 1   *
      *               BR-CLM-SEG-003 Any injury -> AUTOCX (auto) or    *
      *                              LIAB (other), severity >= 3       *
      *               BR-CLM-SEG-004 A fatality -> severity 5          *
      *               BR-CLM-SEG-005 Total loss -> severity >= 4 (auto *
      *                              total loss is AUTOCX)             *
      *               BR-CLM-SEG-006 Property in a catastrophe ->      *
      *                              PROPCAT severity >= 3             *
      *               BR-CLM-SEG-007 Property damage > 50,000 ->       *
      *                              PROPCX severity >= 4              *
      *               BR-CLM-SEG-008 Litigation (flag or open matter)  *
      *                              -> severity 5                     *
      *               BR-CLM-SEG-009 Severity never below the worst    *
      *                              incident severity, never above 5  *
      *               BR-CLM-SEG-010 Closed claims are not re-segmented*
      *               BR-CLM-SEG-011 Exposures with no segment or the  *
      *                              old claim segment follow the claim*
      *               BR-CLM-SEG-012 Severity rising to 4+ on an open  *
      *                              claim raises event SEVESC         *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMSEGMT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.

       01  WS-THRESHOLDS.
           05 WS-FT-DAMAGE-MAX         PIC S9(11)V99 COMP-3 VALUE 2500.
           05 WS-PROPCX-DAMAGE         PIC S9(11)V99 COMP-3
                                                    VALUE 50000.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(10).
           05 WS-SEV-DISP              PIC 9.
           05 WS-OLD-SEV-DISP          PIC 9.
           05 WS-OLD-SEGMENT           PIC X(8).
           05 WS-OLD-SEVERITY          PIC S9(4) COMP.
           05 WS-NEW-SEGMENT           PIC X(8).
           05 WS-NEW-SEVERITY          PIC S9(4) COMP.
           05 WS-REASON                PIC X(60).
           05 WS-REASON-PTR            PIC S9(4) COMP.
           05 WS-CAUSE                 PIC X(10).
              88 WS-GLASS-CAUSE          VALUE 'GLASS' 'WINDSCREEN'.
              88 WS-SINGLE-VEH-CAUSE     VALUE 'SNGLVEH' 'ANIMAL'
                                               'KERB'.
           05 WS-LITIGATED-SW          PIC X     VALUE 'N'.
              88 WS-LITIGATED                    VALUE 'Y'.

       01  WS-INCIDENT-SUMMARY.
           05 WS-INC-COUNT             PIC S9(9) COMP.
           05 WS-INC-DAMAGE            PIC S9(11)V99 COMP-3.
           05 WS-INC-MAX-SEV           PIC S9(4) COMP.
           05 WS-INC-INJURY            PIC S9(9) COMP.
           05 WS-INC-FATAL             PIC S9(9) COMP.
           05 WS-INC-TOTAL-LOSS        PIC S9(9) COMP.
           05 WS-INC-VEHICLES          PIC S9(9) COMP.
           05 WS-OPEN-MATTERS          PIC S9(9) COMP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HSC-==.
           03 HSC-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HSC-==.

       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVC-==.
           03 AVC-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCSEG.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'SEGCLM  '
                 PERFORM 2000-SEGMENT-CLAIM
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMSEGMT: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          CA-SG-SEGMENT
                                          CA-SG-REASON
                                          WS-REASON
           MOVE 0                      TO CA-SG-SEVERITY
           MOVE 1                      TO WS-REASON-PTR
           MOVE 'N'                    TO WS-LITIGATED-SW
           MOVE CA-SG-CLAIM-ID         TO WS-KEY-DISP.

      ******************************************************************
       2000-SEGMENT-CLAIM.
           IF CA-SG-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMIDRQ '          TO CA-REASON-CODE
              MOVE 'Claim id is required for segmentation'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4000-READ-CLAIM
      * BR-CLM-SEG-010 A closed claim keeps the segment it closed with
           IF CLM-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLSD '          TO CA-REASON-CODE
              MOVE 'Closed claims are not re-segmented'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-SUMMARISE-INCIDENTS
           PERFORM 4200-COUNT-OPEN-MATTERS
           MOVE CLM-SEGMENT-CODE       TO WS-OLD-SEGMENT
           MOVE CLM-SEVERITY           TO WS-OLD-SEVERITY
           MOVE CLM-LOSS-CAUSE         TO WS-CAUSE
           IF CLM-LITIGATION-FLAG = 'Y' OR WS-OPEN-MATTERS > 0
              SET WS-LITIGATED         TO TRUE
           END-IF

           PERFORM 5000-BASE-SEGMENT
           EVALUATE CLM-LOSS-TYPE
              WHEN 'AUTO'
                 PERFORM 5100-SEGMENT-AUTO
              WHEN 'PROP'
                 PERFORM 5200-SEGMENT-PROPERTY
              WHEN 'LIAB'
                 PERFORM 5300-SEGMENT-LIABILITY
           END-EVALUATE
           PERFORM 5400-COMMON-SEVERITY

           MOVE WS-NEW-SEGMENT         TO CA-SG-SEGMENT
           MOVE WS-NEW-SEVERITY        TO CA-SG-SEVERITY
           IF WS-REASON = SPACES
              MOVE 'BASE'              TO WS-REASON
           END-IF
           MOVE WS-REASON              TO CA-SG-REASON

           IF WS-NEW-SEGMENT = WS-OLD-SEGMENT
              AND WS-NEW-SEVERITY = WS-OLD-SEVERITY
              MOVE 'Segment and severity unchanged' TO CA-MESSAGE
           ELSE
              PERFORM 4300-UPDATE-CLAIM
              PERFORM 4400-CASCADE-EXPOSURES
              PERFORM 2100-RECORD-CHANGE
           END-IF.

      ******************************************************************
       2100-RECORD-CHANGE.
           MOVE WS-NEW-SEVERITY        TO WS-SEV-DISP
           MOVE WS-OLD-SEVERITY        TO WS-OLD-SEV-DISP
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HSC-REQUEST-ID
           MOVE CA-SG-CLAIM-ID         TO HSC-HI-CLAIM-ID
           MOVE 'SEGMENT '             TO HSC-HI-EVENT-TYPE
           MOVE WS-PROGRAM-ID          TO HSC-HI-PROGRAM
           STRING 'Segment ' WS-OLD-SEGMENT '/' WS-OLD-SEV-DISP
                  ' -> ' WS-NEW-SEGMENT '/' WS-SEV-DISP ': '
                  WS-REASON DELIMITED BY SIZE
                  INTO HSC-HI-DESCRIPTION
           MOVE CA-USER-ID             TO HSC-USER-ID
           MOVE CA-BUS-DATE            TO HSC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HSC-CALLER-PGM
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF HSC-RETURN-CODE >= 12
              MOVE HSC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HSC-REASON-CODE     TO CA-REASON-CODE
              MOVE HSC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           STRING 'Segment set to ' WS-NEW-SEGMENT ' severity '
                  WS-SEV-DISP DELIMITED BY SIZE INTO CA-MESSAGE

      * BR-CLM-SEG-012 Escalation work when severity first reaches 4
           IF CLM-CLAIM-STATE = 'OPEN'
              AND WS-NEW-SEVERITY >= 4
              AND WS-OLD-SEVERITY < 4
              INITIALIZE WS-ACT-CA
              MOVE 'ACTGEN  '          TO AVC-REQUEST-ID
              MOVE CA-USER-ID          TO AVC-USER-ID
              MOVE CA-BUS-DATE         TO AVC-BUS-DATE
              MOVE WS-PROGRAM-ID       TO AVC-CALLER-PGM
              MOVE CA-SG-CLAIM-ID      TO AVC-AV-CLAIM-ID
              MOVE 'SEVESC  '          TO AVC-AV-EVENT
              CALL WS-ACT-PGM USING WS-ACT-CA
              IF AVC-RETURN-CODE >= 90
                 MOVE AVC-RETURN-CODE  TO CA-RETURN-CODE
                 MOVE AVC-REASON-CODE  TO CA-REASON-CODE
                 MOVE AVC-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              IF AVC-RETURN-CODE >= 04
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'SEVESCNA'       TO CA-REASON-CODE
                 MOVE 'Severity escalation activity not created'
                                       TO CA-MESSAGE
              END-IF
           END-IF.

      ******************************************************************
       4000-READ-CLAIM.
           MOVE CA-SG-CLAIM-ID         TO CLM-CLAIM-ID
           EXEC SQL
              SELECT LOSS_TYPE, LOSS_CAUSE, CLAIM_STATE,
                     SEGMENT_CODE, SEVERITY, LITIGATION_FLAG,
                     CAT_CODE, TOTAL_LOSS_FLAG, VERSION_NO
                INTO :CLM-LOSS-TYPE, :CLM-LOSS-CAUSE,
                     :CLM-CLAIM-STATE, :CLM-SEGMENT-CODE,
                     :CLM-SEVERITY, :CLM-LITIGATION-FLAG,
                     :CLM-CAT-CODE, :CLM-TOTAL-LOSS-FLAG,
                     :CLM-VERSION-NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 STRING 'Claim ' WS-KEY-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-CLAIM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * One pass over the claim's incidents; injury is any incident
      * carrying an injury type, a vehicle is any incident with a reg.
       4100-SUMMARISE-INCIDENTS.
           EXEC SQL
              SELECT COUNT(*),
                     COALESCE(SUM(ESTIMATED_DAMAGE), 0),
                     COALESCE(MAX(SEVERITY), 0),
                     COALESCE(SUM(CASE WHEN INJURY_TYPE <> ' '
                                       THEN 1 ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN INJURY_TYPE = 'FATAL'
                                       THEN 1 ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN TOTAL_LOSS_FLAG = 'Y'
                                       THEN 1 ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN VEHICLE_REG <> ' '
                                       THEN 1 ELSE 0 END), 0)
                INTO :WS-INC-COUNT, :WS-INC-DAMAGE, :WS-INC-MAX-SEV,
                     :WS-INC-INJURY, :WS-INC-FATAL,
                     :WS-INC-TOTAL-LOSS, :WS-INC-VEHICLES
                FROM CLMDB.CLM_INCIDENT
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-SUMMARISE-INCIDENTS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF CLM-TOTAL-LOSS-FLAG = 'Y'
              ADD 1                    TO WS-INC-TOTAL-LOSS
           END-IF.

      ******************************************************************
       4200-COUNT-OPEN-MATTERS.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-OPEN-MATTERS
                FROM CLMDB.CLM_LITIGATION
               WHERE CLAIM_ID      = :CLM-CLAIM-ID
                 AND MATTER_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-COUNT-OPEN-MATTERS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-UPDATE-CLAIM.
           MOVE WS-NEW-SEGMENT         TO CLM-SEGMENT-CODE
           MOVE WS-NEW-SEVERITY        TO CLM-SEVERITY
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET SEGMENT_CODE = :CLM-SEGMENT-CODE,
                     SEVERITY     = :CLM-SEVERITY,
                     UPDATE_TS    = CURRENT TIMESTAMP,
                     VERSION_NO   = VERSION_NO + 1
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
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Claim row locked - retry' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4300-UPDATE-CLAIM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-SEG-011 Exposures that never had their own segment, or
      * still carry the claim's previous one, move with the claim.
       4400-CASCADE-EXPOSURES.
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET SEGMENT_CODE = :CLM-SEGMENT-CODE,
                     UPDATE_TS    = CURRENT TIMESTAMP,
                     VERSION_NO   = VERSION_NO + 1
               WHERE CLAIM_ID       = :CLM-CLAIM-ID
                 AND EXPOSURE_STATE <> 'CLSD'
                 AND (SEGMENT_CODE  = ' '
                      OR SEGMENT_CODE = :WS-OLD-SEGMENT)
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4400-CASCADE-EXPOSURES' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-SEG-001 Starting point for every loss type
       5000-BASE-SEGMENT.
           EVALUATE CLM-LOSS-TYPE
              WHEN 'AUTO'
                 MOVE 'AUTOSTD '       TO WS-NEW-SEGMENT
              WHEN 'PROP'
                 MOVE 'PROPSTD '       TO WS-NEW-SEGMENT
              WHEN 'LIAB'
                 MOVE 'LIAB    '       TO WS-NEW-SEGMENT
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'LOSSTYUK'       TO CA-REASON-CODE
                 STRING 'No segmentation rules for loss type '
                        CLM-LOSS-TYPE DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           MOVE 2                      TO WS-NEW-SEVERITY.

      ******************************************************************
       5100-SEGMENT-AUTO.
           EVALUATE TRUE
      * BR-CLM-SEG-003 Injury moves auto into the complex segment
              WHEN WS-INC-INJURY > 0
                 MOVE 'AUTOCX  '       TO WS-NEW-SEGMENT
                 PERFORM 5410-RAISE-TO-3-INJURY
      * BR-CLM-SEG-005 Auto total loss is handled as complex
              WHEN WS-INC-TOTAL-LOSS > 0
                 MOVE 'AUTOCX  '       TO WS-NEW-SEGMENT
      * BR-CLM-SEG-002 Fast track: glass or single vehicle, small,
      * no injury, not a total loss and not litigated
              WHEN (WS-GLASS-CAUSE OR WS-SINGLE-VEH-CAUSE
                    OR WS-INC-VEHICLES = 1)
                   AND WS-INC-DAMAGE < WS-FT-DAMAGE-MAX
                   AND NOT WS-LITIGATED
                 MOVE 'AUTOFT  '       TO WS-NEW-SEGMENT
                 MOVE 1                TO WS-NEW-SEVERITY
                 IF WS-GLASS-CAUSE
                    STRING 'GLASS FAST TRACK;' DELIMITED BY SIZE
                           INTO WS-REASON WITH POINTER WS-REASON-PTR
                 ELSE
                    STRING 'SINGLE VEHICLE FAST TRACK;'
                           DELIMITED BY SIZE
                           INTO WS-REASON WITH POINTER WS-REASON-PTR
                 END-IF
           END-EVALUATE.

      ******************************************************************
       5200-SEGMENT-PROPERTY.
      * BR-CLM-SEG-007 Large property loss
           IF WS-INC-DAMAGE > WS-PROPCX-DAMAGE
              MOVE 'PROPCX  '          TO WS-NEW-SEGMENT
              IF WS-NEW-SEVERITY < 4
                 MOVE 4                TO WS-NEW-SEVERITY
              END-IF
              STRING 'DAMAGE>50K;' DELIMITED BY SIZE
                     INTO WS-REASON WITH POINTER WS-REASON-PTR
           END-IF
      * BR-CLM-SEG-003 Injury on a property loss is a liability matter
           IF WS-INC-INJURY > 0
              MOVE 'LIAB    '          TO WS-NEW-SEGMENT
              PERFORM 5410-RAISE-TO-3-INJURY
           END-IF
      * BR-CLM-SEG-006 Catastrophe handling takes precedence
           IF CLM-CAT-CODE NOT = SPACES
              MOVE 'PROPCAT '          TO WS-NEW-SEGMENT
              IF WS-NEW-SEVERITY < 3
                 MOVE 3                TO WS-NEW-SEVERITY
              END-IF
              STRING 'CAT ' CLM-CAT-CODE ';' DELIMITED BY SIZE
                     INTO WS-REASON WITH POINTER WS-REASON-PTR
           END-IF.

      ******************************************************************
       5300-SEGMENT-LIABILITY.
           IF WS-INC-INJURY > 0
              PERFORM 5410-RAISE-TO-3-INJURY
           END-IF.

      ******************************************************************
       5400-COMMON-SEVERITY.
      * BR-CLM-SEG-005 Any total loss is at least severity 4
           IF WS-INC-TOTAL-LOSS > 0
              IF WS-NEW-SEVERITY < 4
                 MOVE 4                TO WS-NEW-SEVERITY
              END-IF
              STRING 'TOTAL LOSS;' DELIMITED BY SIZE
                     INTO WS-REASON WITH POINTER WS-REASON-PTR
           END-IF
      * BR-CLM-SEG-004 A fatality is always maximum severity
           IF WS-INC-FATAL > 0
              MOVE 5                   TO WS-NEW-SEVERITY
              STRING 'FATALITY;' DELIMITED BY SIZE
                     INTO WS-REASON WITH POINTER WS-REASON-PTR
           END-IF
      * BR-CLM-SEG-008 Litigated claims are maximum severity
           IF WS-LITIGATED
              MOVE 5                   TO WS-NEW-SEVERITY
              STRING 'LITIGATION;' DELIMITED BY SIZE
                     INTO WS-REASON WITH POINTER WS-REASON-PTR
           END-IF
      * BR-CLM-SEG-009 Floor at the worst incident, cap at 5
           IF WS-INC-MAX-SEV > WS-NEW-SEVERITY
              MOVE WS-INC-MAX-SEV      TO WS-NEW-SEVERITY
              STRING 'INCIDENT SEVERITY;' DELIMITED BY SIZE
                     INTO WS-REASON WITH POINTER WS-REASON-PTR
           END-IF
           IF WS-NEW-SEVERITY > 5
              MOVE 5                   TO WS-NEW-SEVERITY
           END-IF
           IF WS-NEW-SEVERITY < 1
              MOVE 1                   TO WS-NEW-SEVERITY
           END-IF.

      ******************************************************************
       5410-RAISE-TO-3-INJURY.
           IF WS-NEW-SEVERITY < 3
              MOVE 3                   TO WS-NEW-SEVERITY
           END-IF
           STRING 'INJURY;' DELIMITED BY SIZE
                  INTO WS-REASON WITH POINTER WS-REASON-PTR.

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
           STRING 'CLMSEGMT Db2 error SQLCODE ' WS-SQLCODE-DISP
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
