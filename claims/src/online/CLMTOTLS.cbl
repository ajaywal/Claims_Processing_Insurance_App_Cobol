       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMTOTLS.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMTOTLS                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Vehicle total-loss evaluation. Compares the      *
      *               estimated damage on an insured-vehicle incident  *
      *               with the risk unit's insured value; when the     *
      *               repair is uneconomic the incident and the claim  *
      *               are flagged total loss, segmentation is re-run   *
      *               and a salvage hint is written to claim history.  *
      *               Shares the incident body (CLMCINC) with CLMINCDT.*
      * REQUESTS    : TLSEVL                                           *
      * CALLED BY   : incident / vehicle screens, CLMINCDT callers     *
      *               after an estimate (reason TLREEVAL)              *
      * CALLS       : CLMERRLG CLMHIST0 CLMAUDCH CLMSEGMT CLMMONEY     *
      * TABLES      : CLM_INCIDENT (R U) CLM_CLAIM (R U)               *
      *               CLM_RISK_UNIT (R)                                *
      * RULES       : BR-CLM-TLS-001 Only vehicle incidents on OPEN    *
      *                              claims are evaluated              *
      *               BR-CLM-TLS-002 Only insured vehicles (a risk unit*
      *                              with an insured value) are        *
      *                              evaluated; third-party vehicles   *
      *                              are valued by their own insurer   *
      *               BR-CLM-TLS-003 A damage estimate is required     *
      *               BR-CLM-TLS-004 Total loss when estimated damage  *
      *                              >= 70% of insured value           *
      *               BR-CLM-TLS-005 Total loss when the vehicle is not*
      *                              driveable and damage >= 60% of    *
      *                              insured value                     *
      *               BR-CLM-TLS-006 Thresholds are basis-point rates  *
      *                              applied through CLMMONEY          *
      *               BR-CLM-TLS-007 Evaluation never removes a total  *
      *                              loss flag; that is a manual review*
      *               BR-CLM-TLS-008 The claim is a total loss when any*
      *                              of its vehicles is                *
      *               BR-CLM-TLS-009 Salvage hint (25% of insured      *
      *                              value) written to claim history   *
      *               BR-CLM-TLS-010 A new total loss re-runs claim    *
      *                              segmentation (CLMSEGMT)           *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMTOTLS'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-SEG-PGM                  PIC X(8)  VALUE 'CLMSEGMT'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'CLMMONEY'.

       01  WS-CONSTANTS.
           05 WS-TL-BPS                PIC S9(4) COMP VALUE 7000.
           05 WS-TL-NOT-DRIVEABLE-BPS  PIC S9(4) COMP VALUE 6000.
           05 WS-SALVAGE-BPS           PIC S9(4) COMP VALUE 2500.

       01  WS-WORK.
           05 WS-THRESHOLD             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-THRESHOLD-ND          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SALVAGE-EST           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DAMAGE-PCT            PIC S9(5)V99 COMP-3 VALUE 0.
           05 WS-RATE-BPS              PIC S9(4) COMP VALUE 0.
           05 WS-OUTCOME               PIC X     VALUE 'N'.
              88 WS-IS-TOTAL-LOSS       VALUE 'Y'.
           05 WS-TRIGGER               PIC X(4)  VALUE SPACES.
           05 WS-OLD-SEGMENT           PIC X(8).
           05 WS-OLD-SEVERITY          PIC S9(4) COMP.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-CLM-DISP              PIC 9(9).
           05 WS-SEV-DISP              PIC 9(4).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-VAL-DISP              PIC -(11)9.99.
           05 WS-PCT-DISP              PIC ZZZZ9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-HIS-EVENT             PIC X(8).
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-AUD-PAYLOAD           PIC X(400).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDINC END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDRSK END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
       01  WS-AUD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==AUA-==.
       01  WS-SEG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SGA-==.
           03 SGA-BODY.
              COPY CLMCSEG REPLACING LEADING ==CA-== BY ==SGA-==.
       01  WS-MNY-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY CLMCMNY REPLACING LEADING ==CA-== BY ==MNA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCINC.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'TLSEVL  '
                 PERFORM 2000-EVALUATE-TOTAL-LOSS
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMTOTLS: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO WS-OUTCOME.

      ******************************************************************
       2000-EVALUATE-TOTAL-LOSS.
           PERFORM 3000-VALIDATE-INCIDENT
           PERFORM 5200-RETURN-INCIDENT
      * BR-CLM-TLS-007 An existing total loss is left alone
           IF INC-TOTAL-LOSS-FLAG = 'Y'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ALREADTL'          TO CA-REASON-CODE
              MOVE 'Already a total loss - reversal is a manual review'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 5000-CALCULATE-THRESHOLDS
           PERFORM 5100-DECIDE

           MOVE CA-IN-INCIDENT-ID      TO WS-ID-DISP
           MOVE INC-ESTIMATED-DAMAGE   TO WS-AMT-DISP
           MOVE RSK-INSURED-VALUE      TO WS-VAL-DISP
           MOVE WS-DAMAGE-PCT          TO WS-PCT-DISP
           IF WS-IS-TOTAL-LOSS
              PERFORM 2100-RECORD-TOTAL-LOSS
           ELSE
              MOVE 'TLSEVL  '          TO WS-HIS-EVENT
              STRING 'Vehicle ' INC-VEHICLE-REG ' repairable: damage '
                     WS-AMT-DISP ' is ' WS-PCT-DISP '% of '
                     WS-VAL-DISP DELIMITED BY SIZE INTO WS-HIS-DESC
              PERFORM 8100-WRITE-HISTORY
              STRING 'Repairable - damage ' WS-PCT-DISP
                     '% of insured value'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       2100-RECORD-TOTAL-LOSS.
           PERFORM 4300-FLAG-INCIDENT
      * BR-CLM-TLS-008 Any total-loss vehicle makes the claim one
           IF CLM-TOTAL-LOSS-FLAG NOT = 'Y'
              PERFORM 4310-FLAG-CLAIM
           END-IF
           MOVE 'Y'                    TO CA-IN-TOTAL-LOSS

           MOVE 'TOTLOSS '             TO WS-HIS-EVENT
           STRING 'Vehicle ' INC-VEHICLE-REG ' total loss (' WS-TRIGGER
                  '): damage ' WS-AMT-DISP ' = ' WS-PCT-DISP
                  '% of ' WS-VAL-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY

      * BR-CLM-TLS-009 Salvage hint for the handler
           MOVE RSK-INSURED-VALUE      TO MNA-MN-TOTAL
           MOVE WS-SALVAGE-BPS         TO WS-RATE-BPS
           PERFORM 8300-APPLY-RATE
           MOVE MNA-MN-RESULT          TO WS-SALVAGE-EST
           MOVE WS-SALVAGE-EST         TO WS-AMT-DISP
           MOVE 'SALVHINT'             TO WS-HIS-EVENT
           STRING 'Refer ' INC-VEHICLE-REG ' to salvage; estimated'
                  ' salvage value ' WS-AMT-DISP
                  ' (25% of insured value)'
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY

           PERFORM 5300-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT
           PERFORM 8500-RESEGMENT
           IF CA-RC-OK
              STRING 'Total loss (' WS-TRIGGER ') - damage '
                     WS-PCT-DISP '% of insured value'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       3000-VALIDATE-INCIDENT.
           IF CA-IN-INCIDENT-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCIDREQ'          TO CA-REASON-CODE
              MOVE 'Incident id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-INCIDENT
      * BR-CLM-TLS-001 Vehicle incidents on open claims only
           IF INC-INCIDENT-TYPE NOT = 'VEH '
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOTVEH  '          TO CA-REASON-CODE
              STRING 'Total loss applies to vehicles, not '
                     INC-INCIDENT-TYPE ' incidents'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE INC-CLAIM-ID           TO CA-IN-CLAIM-ID
           PERFORM 4000-READ-CLAIM
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMSTATE'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ' - evaluation needs OPEN'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-TLS-002 Insured vehicles with a value only
           IF INC-RISK-UNIT-SEQ <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TPVEHCL '          TO CA-REASON-CODE
              MOVE 'Third-party vehicle - its own insurer decides'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4200-READ-RISK-UNIT
           IF RSK-INSURED-VALUE <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOVALUE '          TO CA-REASON-CODE
              MOVE 'Risk unit has no insured value to compare with'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-TLS-003 A damage estimate is required
           IF INC-ESTIMATED-DAMAGE <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NODAMAGE'          TO CA-REASON-CODE
              MOVE 'Record the estimated damage (INCUPD) first'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-CLAIM.
           MOVE CA-IN-CLAIM-ID         TO CLM-CLAIM-ID
           EXEC SQL
              SELECT CLAIM_NUMBER, CLAIM_STATE, SEGMENT_CODE,
                     SEVERITY, TOTAL_LOSS_FLAG
                INTO :CLM-CLAIM-NUMBER, :CLM-CLAIM-STATE,
                     :CLM-SEGMENT-CODE, :CLM-SEVERITY,
                     :CLM-TOTAL-LOSS-FLAG
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE 'Claim of the incident not found'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4000-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-READ-INCIDENT.
           MOVE CA-IN-INCIDENT-ID      TO INC-INCIDENT-ID
           EXEC SQL
              SELECT INCIDENT_ID, CLAIM_ID, INCIDENT_TYPE,
                     RISK_UNIT_SEQ, DESCRIPTION, SEVERITY,
                     VEHICLE_REG, DRIVEABLE_FLAG, ESTIMATED_DAMAGE,
                     TOTAL_LOSS_FLAG, INJURY_TYPE, INJURED_CONTACT_ID
                INTO :INC-INCIDENT-ID, :INC-CLAIM-ID,
                     :INC-INCIDENT-TYPE, :INC-RISK-UNIT-SEQ,
                     :INC-DESCRIPTION, :INC-SEVERITY,
                     :INC-VEHICLE-REG, :INC-DRIVEABLE-FLAG,
                     :INC-ESTIMATED-DAMAGE, :INC-TOTAL-LOSS-FLAG,
                     :INC-INJURY-TYPE, :INC-INJURED-CONTACT-ID
                FROM CLMDB.CLM_INCIDENT
               WHERE INCIDENT_ID = :INC-INCIDENT-ID
                WITH RS USE AND KEEP UPDATE LOCKS
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INCNOTFD'       TO CA-REASON-CODE
                 MOVE CA-IN-INCIDENT-ID TO WS-ID-DISP
                 STRING 'Incident ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4100-READ-INCIDENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-RISK-UNIT.
           MOVE INC-CLAIM-ID           TO RSK-CLAIM-ID
           MOVE INC-RISK-UNIT-SEQ      TO RSK-RISK-UNIT-SEQ
           EXEC SQL
              SELECT VEHICLE_REG, INSURED_VALUE
                INTO :RSK-VEHICLE-REG, :RSK-INSURED-VALUE
                FROM CLMDB.CLM_RISK_UNIT
               WHERE CLAIM_ID      = :RSK-CLAIM-ID
                 AND RISK_UNIT_SEQ = :RSK-RISK-UNIT-SEQ
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'RSKNOTFD'       TO CA-REASON-CODE
                 MOVE 'Incident risk unit is not on the policy snapshot'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-READ-RISK-UNIT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-FLAG-INCIDENT.
           EXEC SQL
              UPDATE CLMDB.CLM_INCIDENT
                 SET TOTAL_LOSS_FLAG = 'Y'
               WHERE INCIDENT_ID     = :INC-INCIDENT-ID
                 AND TOTAL_LOSS_FLAG = 'N'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    PERFORM 4390-ALREADY-FLAGGED
                 END-IF
              WHEN 100
                 PERFORM 4390-ALREADY-FLAGGED
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4300-FLAG-INCIDENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4310-FLAG-CLAIM.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET TOTAL_LOSS_FLAG = 'Y',
                     UPDATE_TS       = CURRENT TIMESTAMP,
                     VERSION_NO      = VERSION_NO + 1
               WHERE CLAIM_ID        = :CLM-CLAIM-ID
                 AND TOTAL_LOSS_FLAG <> 'Y'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
              WHEN 100
                 CONTINUE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4310-FLAG-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4390-ALREADY-FLAGGED.
           SET CA-RC-CONFLICT          TO TRUE
           MOVE 'STALEVER'             TO CA-REASON-CODE
           MOVE 'Incident was flagged by another user - refresh'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       4500-REREAD-CLAIM-SEGMENT.
           EXEC SQL
              SELECT SEGMENT_CODE, SEVERITY
                INTO :CLM-SEGMENT-CODE, :CLM-SEVERITY
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-REREAD-CLAIM-SEGMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4510-UPDATE-CLAIM-SEGMENT.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET SEGMENT_CODE = :SGA-SG-SEGMENT,
                     SEVERITY     = :SGA-SG-SEVERITY,
                     VERSION_NO   = VERSION_NO + 1,
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4510-UPDATE-CLAIM-SEGMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-TLS-006 Thresholds are rates applied through CLMMONEY
       5000-CALCULATE-THRESHOLDS.
           MOVE RSK-INSURED-VALUE      TO MNA-MN-TOTAL
           MOVE WS-TL-BPS              TO WS-RATE-BPS
           PERFORM 8300-APPLY-RATE
           MOVE MNA-MN-RESULT          TO WS-THRESHOLD
           MOVE RSK-INSURED-VALUE      TO MNA-MN-TOTAL
           MOVE WS-TL-NOT-DRIVEABLE-BPS TO WS-RATE-BPS
           PERFORM 8300-APPLY-RATE
           MOVE MNA-MN-RESULT          TO WS-THRESHOLD-ND
           COMPUTE WS-DAMAGE-PCT ROUNDED =
                   INC-ESTIMATED-DAMAGE * 100 / RSK-INSURED-VALUE
              ON SIZE ERROR
                 MOVE 99999.99         TO WS-DAMAGE-PCT
           END-COMPUTE.

      ******************************************************************
       5100-DECIDE.
           EVALUATE TRUE
      * BR-CLM-TLS-004 Damage at or above 70% of value
              WHEN INC-ESTIMATED-DAMAGE >= WS-THRESHOLD
                 SET WS-IS-TOTAL-LOSS  TO TRUE
                 MOVE 'D70 '           TO WS-TRIGGER
      * BR-CLM-TLS-005 Not driveable and at or above 60% of value
              WHEN INC-DRIVEABLE-FLAG = 'N'
               AND INC-ESTIMATED-DAMAGE >= WS-THRESHOLD-ND
                 SET WS-IS-TOTAL-LOSS  TO TRUE
                 MOVE 'ND60'           TO WS-TRIGGER
              WHEN OTHER
                 MOVE 'N'              TO WS-OUTCOME
           END-EVALUATE.

      ******************************************************************
       5200-RETURN-INCIDENT.
           MOVE INC-CLAIM-ID           TO CA-IN-CLAIM-ID
           MOVE INC-INCIDENT-TYPE      TO CA-IN-TYPE
           MOVE INC-RISK-UNIT-SEQ      TO CA-IN-RISK-UNIT-SEQ
           MOVE SPACES                 TO CA-IN-DESCRIPTION
           IF INC-DESCRIPTION-LEN > 0
              MOVE INC-DESCRIPTION-TEXT(1:INC-DESCRIPTION-LEN)
                                       TO CA-IN-DESCRIPTION
           END-IF
           MOVE INC-SEVERITY           TO CA-IN-SEVERITY
           MOVE INC-VEHICLE-REG        TO CA-IN-VEHICLE-REG
           MOVE INC-DRIVEABLE-FLAG     TO CA-IN-DRIVEABLE
           MOVE INC-ESTIMATED-DAMAGE   TO CA-IN-EST-DAMAGE
           MOVE INC-TOTAL-LOSS-FLAG    TO CA-IN-TOTAL-LOSS
           MOVE INC-INJURY-TYPE        TO CA-IN-INJURY-TYPE
           MOVE INC-INJURED-CONTACT-ID TO CA-IN-INJURED-ID.

      ******************************************************************
       5300-BUILD-AUDIT-PAYLOAD.
           MOVE CA-IN-INCIDENT-ID      TO WS-ID-DISP
           MOVE CA-IN-CLAIM-ID         TO WS-CLM-DISP
           MOVE INC-ESTIMATED-DAMAGE   TO WS-AMT-DISP
           MOVE SPACES                 TO WS-AUD-PAYLOAD
           STRING 'INC=' WS-ID-DISP ' CLM=' WS-CLM-DISP
                  ' REG=' INC-VEHICLE-REG
                  ' DRV=' INC-DRIVEABLE-FLAG
                  ' DMG=' WS-AMT-DISP
                  ' VALUE=' WS-VAL-DISP
                  ' PCT=' WS-PCT-DISP
                  ' RULE=' WS-TRIGGER
                  ' TL=Y USER=' CA-USER-ID
                  DELIMITED BY SIZE INTO WS-AUD-PAYLOAD.

      ******************************************************************
       8100-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE CA-BUS-DATE            TO HIA-BUS-DATE
           MOVE CA-TRACE-ID            TO HIA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-IN-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF HIA-RETURN-CODE >= 12
              MOVE HIA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HIA-REASON-CODE     TO CA-REASON-CODE
              MOVE HIA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO WS-HIS-DESC.

      ******************************************************************
       8200-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUCADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE CA-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE CA-IN-CLAIM-ID         TO AUA-AC-CLAIM-ID
           MOVE 'TOTLOSS '             TO AUA-AC-EVENT-CODE
           MOVE WS-AUD-PAYLOAD         TO AUA-AC-PAYLOAD
           MOVE WS-PROGRAM-ID          TO AUA-AC-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF AUA-RETURN-CODE >= 12
              MOVE AUA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AUA-REASON-CODE     TO CA-REASON-CODE
              MOVE AUA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8300-APPLY-RATE.
           MOVE 'MNRATE  '             TO MNA-REQUEST-ID
           MOVE CA-USER-ID             TO MNA-USER-ID
           MOVE CA-BUS-DATE            TO MNA-BUS-DATE
           MOVE CA-TRACE-ID            TO MNA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO MNA-CALLER-PGM
           MOVE WS-RATE-BPS            TO MNA-MN-RATE-BPS
           CALL WS-MNY-PGM USING WS-MNY-CA
           IF MNA-RETURN-CODE >= 8
              MOVE MNA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE MNA-REASON-CODE     TO CA-REASON-CODE
              MOVE MNA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-TLS-010 Re-derive segment and severity after a total
      * loss; persist and record only a real change
       8500-RESEGMENT.
           MOVE CLM-SEGMENT-CODE       TO WS-OLD-SEGMENT
           MOVE CLM-SEVERITY           TO WS-OLD-SEVERITY
           INITIALIZE WS-SEG-CA
           MOVE 'SEGCLM  '             TO SGA-REQUEST-ID
           MOVE CA-USER-ID             TO SGA-USER-ID
           MOVE CA-BUS-DATE            TO SGA-BUS-DATE
           MOVE CA-TRACE-ID            TO SGA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SGA-CALLER-PGM
           MOVE CA-IN-CLAIM-ID         TO SGA-SG-CLAIM-ID
           CALL WS-SEG-PGM USING WS-SEG-CA
           IF SGA-RETURN-CODE >= 12
              MOVE SGA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SGA-REASON-CODE     TO CA-REASON-CODE
              MOVE SGA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF SGA-SG-SEGMENT = SPACES
              OR (SGA-SG-SEGMENT = WS-OLD-SEGMENT
                  AND SGA-SG-SEVERITY = WS-OLD-SEVERITY)
              CONTINUE
           ELSE
              PERFORM 4500-REREAD-CLAIM-SEGMENT
              IF CLM-SEGMENT-CODE NOT = SGA-SG-SEGMENT
                 OR CLM-SEVERITY NOT = SGA-SG-SEVERITY
                 PERFORM 4510-UPDATE-CLAIM-SEGMENT
              END-IF
              MOVE SGA-SG-SEVERITY     TO WS-SEV-DISP
              MOVE 'SEGCHG  '          TO WS-HIS-EVENT
              STRING 'Segment ' WS-OLD-SEGMENT ' -> '
                     SGA-SG-SEGMENT ' severity ' WS-SEV-DISP
                     ' : ' SGA-SG-REASON
                     DELIMITED BY SIZE INTO WS-HIS-DESC
              PERFORM 8100-WRITE-HISTORY
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9800-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Incident or claim row locked - retry'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'CLMTOTLS Db2 error SQLCODE ' WS-SQLCODE-DISP
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
           MOVE CA-IN-INCIDENT-ID      TO WS-ID-DISP
           STRING 'INC ' WS-ID-DISP DELIMITED BY SIZE
                  INTO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
