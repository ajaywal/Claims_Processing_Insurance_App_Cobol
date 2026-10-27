       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMINCDT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMINCDT                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Incident service. Records what was damaged or    *
      *               who was hurt on a claim: vehicle (VEH), insured  *
      *               property (PROP), third-party fixed property      *
      *               (FIXP) and injury (INJ) incidents. Vehicle       *
      *               incidents are matched to the policy risk units by*
      *               registration; unmatched vehicles are third-party.*
      *               A change in estimated damage re-runs claim       *
      *               segmentation. Total-loss evaluation is CLMTOTLS. *
      * REQUESTS    : INCNEW INCINQ INCUPD                             *
      * CALLED BY   : CLMCLAIM (FNOL), claim screens, CLMEXPOS (read)  *
      * CALLS       : CLMSEQGN CLMERRLG CLMHIST0 CLMAUDCH CLMCONTC     *
      *               CLMSEGMT CLMCODVL                                *
      * TABLES      : CLM_INCIDENT (C R U) CLM_CLAIM (R U)             *
      *               CLM_RISK_UNIT (R) CLM_CONTACT (R)                *
      * RULES       : BR-CLM-INC-001 Incidents only on DRFT or OPEN    *
      *                              claims                            *
      *               BR-CLM-INC-002 Incident types VEH PROP INJ FIXP  *
      *               BR-CLM-INC-003 Severity 1..5, default 1          *
      *               BR-CLM-INC-004 Estimated damage cannot be        *
      *                              negative                          *
      *               BR-CLM-INC-005 Vehicle registration must match   *
      *                              the named risk unit; with no risk *
      *                              unit a matching insured vehicle is*
      *                              linked, otherwise third-party     *
      *               BR-CLM-INC-006 One vehicle incident per          *
      *                              registration per claim            *
      *               BR-CLM-INC-007 PROP incidents must name an       *
      *                              insured risk unit; FIXP never does*
      *               BR-CLM-INC-008 Injury incidents need an injured  *
      *                              contact and a valid injury type;  *
      *                              the contact is linked INJURED     *
      *               BR-CLM-INC-009 Incident type and claim cannot    *
      *                              change after creation             *
      *               BR-CLM-INC-010 Total-loss flag is owned by       *
      *                              CLMTOTLS; a damage change on a    *
      *                              total loss asks for re-evaluation *
      *               BR-CLM-INC-011 Damage or injury changes re-run   *
      *                              claim segmentation (CLMSEGMT)     *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMINCDT'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-CON-PGM                  PIC X(8)  VALUE 'CLMCONTC'.
       01  WS-SEG-PGM                  PIC X(8)  VALUE 'CLMSEGMT'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.

       01  WS-WORK.
           05 WS-INC-TYPE              PIC X(4).
              88 WS-TYPE-VEHICLE        VALUE 'VEH '.
              88 WS-TYPE-PROPERTY       VALUE 'PROP'.
              88 WS-TYPE-INJURY         VALUE 'INJ '.
              88 WS-TYPE-FIXED-PROP     VALUE 'FIXP'.
              88 WS-TYPE-VALID          VALUE 'VEH ' 'PROP' 'INJ '
                                              'FIXP'.
           05 WS-CLAIM-STATE           PIC X(4).
              88 WS-CLAIM-OPEN-OR-DRAFT VALUE 'DRFT' 'OPEN'.
           05 WS-OLD-DAMAGE            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OLD-INJURY            PIC X(8).
           05 WS-OLD-REG               PIC X(8).
           05 WS-RESEGMENT             PIC X     VALUE 'N'.
              88 WS-NEED-RESEGMENT      VALUE 'Y'.
           05 WS-TEXT-LEN              PIC S9(4) COMP VALUE 0.
           05 WS-TRAIL                 PIC S9(4) COMP VALUE 0.
           05 WS-DUP-COUNT             PIC S9(9) COMP VALUE 0.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-CLM-DISP              PIC 9(9).
           05 WS-SEQ-DISP              PIC 9(4).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-OLD-AMT-DISP          PIC -(11)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-HIS-EVENT             PIC X(8).
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-AUD-EVENT             PIC X(8).
           05 WS-AUD-PAYLOAD           PIC X(400).
           05 WS-COD-TABLE             PIC X(8).
           05 WS-COD-VALUE             PIC X(10).
           05 WS-COD-REASON            PIC X(8).
           05 WS-OLD-SEGMENT           PIC X(8).
           05 WS-OLD-SEVERITY          PIC S9(4) COMP.

       01  WS-LOWER-CASE               PIC X(26)
                                VALUE 'abcdefghijklmnopqrstuvwxyz'.
       01  WS-UPPER-CASE               PIC X(26)
                                VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDINC END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDRSK END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
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
       01  WS-CON-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==COA-==.
           03 COA-BODY.
              COPY CLMCCON REPLACING LEADING ==CA-== BY ==COA-==.
       01  WS-SEG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SGA-==.
           03 SGA-BODY.
              COPY CLMCSEG REPLACING LEADING ==CA-== BY ==SGA-==.
       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.

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
              WHEN 'INCNEW  '
                 PERFORM 2000-NEW-INCIDENT
              WHEN 'INCINQ  '
                 PERFORM 2100-INQUIRE-INCIDENT
              WHEN 'INCUPD  '
                 PERFORM 2200-UPDATE-INCIDENT
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMINCDT: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO WS-RESEGMENT
           MOVE SPACES                 TO WS-ERR-KEY.

      ******************************************************************
       2000-NEW-INCIDENT.
           PERFORM 3000-VALIDATE-CLAIM
           PERFORM 3100-VALIDATE-COMMON
           EVALUATE TRUE
              WHEN WS-TYPE-VEHICLE
                 PERFORM 3200-VALIDATE-VEHICLE
                 PERFORM 3210-CHECK-DUP-VEHICLE
              WHEN WS-TYPE-PROPERTY
                 PERFORM 3300-VALIDATE-PROPERTY
              WHEN WS-TYPE-FIXED-PROP
                 PERFORM 3300-VALIDATE-PROPERTY
              WHEN WS-TYPE-INJURY
                 PERFORM 3400-VALIDATE-INJURY
           END-EVALUATE
           PERFORM 3500-CLEAR-INAPPLICABLE

           MOVE 'INC'                  TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO CA-IN-INCIDENT-ID
           MOVE 'N'                    TO CA-IN-TOTAL-LOSS
           PERFORM 5000-LOAD-INCIDENT-ROW
           PERFORM 4300-INSERT-INCIDENT

           IF WS-TYPE-INJURY
              PERFORM 8400-LINK-INJURED
           END-IF

           MOVE CA-IN-INCIDENT-ID      TO WS-ID-DISP
           MOVE CA-IN-EST-DAMAGE       TO WS-AMT-DISP
           MOVE 'INCNEW  '             TO WS-HIS-EVENT
           STRING 'Incident ' WS-ID-DISP ' type ' CA-IN-TYPE
                  ' added, est damage ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           MOVE 'INCNEW  '             TO WS-AUD-EVENT
           PERFORM 5100-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT

      * BR-CLM-INC-011 Injury or damage drives segment and severity
           IF WS-TYPE-INJURY OR CA-IN-EST-DAMAGE > 0
              PERFORM 8500-RESEGMENT
           END-IF
           IF CA-RC-OK
              STRING 'Incident ' WS-ID-DISP ' created'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       2100-INQUIRE-INCIDENT.
           PERFORM 4100-READ-INCIDENT
           PERFORM 5200-RETURN-INCIDENT.

      ******************************************************************
       2200-UPDATE-INCIDENT.
           PERFORM 4100-READ-INCIDENT
           MOVE INC-CLAIM-ID           TO CA-IN-CLAIM-ID
      * BR-CLM-INC-009 Type and claim are fixed once created
           IF CA-IN-TYPE NOT = SPACES
              AND CA-IN-TYPE NOT = INC-INCIDENT-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCTYPCH'          TO CA-REASON-CODE
              MOVE 'Incident type cannot be changed - add a new one'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE INC-INCIDENT-TYPE      TO CA-IN-TYPE
           MOVE INC-ESTIMATED-DAMAGE   TO WS-OLD-DAMAGE
           MOVE INC-INJURY-TYPE        TO WS-OLD-INJURY
           MOVE INC-VEHICLE-REG        TO WS-OLD-REG
           PERFORM 3000-VALIDATE-CLAIM
           PERFORM 3100-VALIDATE-COMMON
           EVALUATE TRUE
              WHEN WS-TYPE-VEHICLE
                 PERFORM 3200-VALIDATE-VEHICLE
                 IF CA-IN-VEHICLE-REG NOT = WS-OLD-REG
                    PERFORM 3210-CHECK-DUP-VEHICLE
                 END-IF
              WHEN WS-TYPE-PROPERTY
                 PERFORM 3300-VALIDATE-PROPERTY
              WHEN WS-TYPE-FIXED-PROP
                 PERFORM 3300-VALIDATE-PROPERTY
              WHEN WS-TYPE-INJURY
                 PERFORM 3400-VALIDATE-INJURY
           END-EVALUATE
           PERFORM 3500-CLEAR-INAPPLICABLE

      * BR-CLM-INC-010 Total-loss flag is never changed here
           MOVE INC-TOTAL-LOSS-FLAG    TO CA-IN-TOTAL-LOSS
           PERFORM 5000-LOAD-INCIDENT-ROW
           PERFORM 4400-UPDATE-INCIDENT

           IF CA-IN-EST-DAMAGE NOT = WS-OLD-DAMAGE
              OR CA-IN-INJURY-TYPE NOT = WS-OLD-INJURY
              SET WS-NEED-RESEGMENT    TO TRUE
           END-IF

           MOVE CA-IN-INCIDENT-ID      TO WS-ID-DISP
           MOVE CA-IN-EST-DAMAGE       TO WS-AMT-DISP
           MOVE WS-OLD-DAMAGE          TO WS-OLD-AMT-DISP
           MOVE 'INCUPD  '             TO WS-HIS-EVENT
           STRING 'Incident ' WS-ID-DISP ' updated, damage '
                  WS-OLD-AMT-DISP ' -> ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           MOVE 'INCUPD  '             TO WS-AUD-EVENT
           PERFORM 5100-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT

           IF WS-NEED-RESEGMENT
              PERFORM 8500-RESEGMENT
           END-IF
           IF CA-RC-OK
              IF CA-IN-TOTAL-LOSS = 'Y'
                 AND CA-IN-EST-DAMAGE NOT = WS-OLD-DAMAGE
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'TLREEVAL'       TO CA-REASON-CODE
                 MOVE 'Damage changed on a total loss - re-run TLSEVL'
                                       TO CA-MESSAGE
              ELSE
                 STRING 'Incident ' WS-ID-DISP ' updated'
                        DELIMITED BY SIZE INTO CA-MESSAGE
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-INC-001 Incidents only on draft or open claims
       3000-VALIDATE-CLAIM.
           IF CA-IN-CLAIM-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMIDREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4000-READ-CLAIM
           MOVE CLM-CLAIM-STATE        TO WS-CLAIM-STATE
           IF NOT WS-CLAIM-OPEN-OR-DRAFT
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMSTATE'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is ' CLM-CLAIM-STATE
                     ' - incidents need a DRFT or OPEN claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3100-VALIDATE-COMMON.
      * BR-CLM-INC-002 Incident types
           MOVE CA-IN-TYPE             TO WS-INC-TYPE
           IF NOT WS-TYPE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCTYPE '          TO CA-REASON-CODE
              STRING 'Incident type ' CA-IN-TYPE
                     ' must be VEH PROP INJ or FIXP'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-IN-DESCRIPTION = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCDESC '          TO CA-REASON-CODE
              MOVE 'Incident description is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-INC-003 Severity 1..5, default 1
           IF CA-IN-SEVERITY = 0
              MOVE 1                   TO CA-IN-SEVERITY
           END-IF
           IF CA-IN-SEVERITY < 1 OR CA-IN-SEVERITY > 5
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCSEV  '          TO CA-REASON-CODE
              MOVE 'Incident severity must be between 1 and 5'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-INC-004 Estimated damage cannot be negative
           IF CA-IN-EST-DAMAGE < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCDMG  '          TO CA-REASON-CODE
              MOVE 'Estimated damage cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-INC-005 Registration must match the named risk unit;
      * with no risk unit an insured vehicle with the same reg is
      * linked automatically, otherwise the vehicle is third-party.
       3200-VALIDATE-VEHICLE.
           INSPECT CA-IN-VEHICLE-REG
                   CONVERTING WS-LOWER-CASE TO WS-UPPER-CASE
           IF CA-IN-VEHICLE-REG = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'VEHREG  '          TO CA-REASON-CODE
              MOVE 'Vehicle registration is required for VEH'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-IN-DRIVEABLE = SPACE
              MOVE 'Y'                 TO CA-IN-DRIVEABLE
           END-IF
           IF CA-IN-DRIVEABLE NOT = 'Y' AND CA-IN-DRIVEABLE NOT = 'N'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DRIVEABL'          TO CA-REASON-CODE
              MOVE 'Driveable indicator must be Y or N'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-IN-RISK-UNIT-SEQ > 0
              PERFORM 4200-READ-RISK-UNIT
              IF RSK-VEHICLE-REG NOT = CA-IN-VEHICLE-REG
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'REGMISMT'       TO CA-REASON-CODE
                 MOVE CA-IN-RISK-UNIT-SEQ TO WS-SEQ-DISP
                 STRING 'Reg ' CA-IN-VEHICLE-REG
                        ' does not match risk unit ' WS-SEQ-DISP
                        ' reg ' RSK-VEHICLE-REG
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           ELSE
              PERFORM 4210-FIND-RISK-UNIT-BY-REG
           END-IF.

      ******************************************************************
      * BR-CLM-INC-006 One vehicle incident per registration per claim
       3210-CHECK-DUP-VEHICLE.
           MOVE CA-IN-CLAIM-ID         TO INC-CLAIM-ID
           MOVE CA-IN-VEHICLE-REG      TO INC-VEHICLE-REG
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-DUP-COUNT
                FROM CLMDB.CLM_INCIDENT
               WHERE CLAIM_ID      = :INC-CLAIM-ID
                 AND INCIDENT_TYPE = 'VEH '
                 AND VEHICLE_REG   = :INC-VEHICLE-REG
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3210-CHECK-DUP-VEHICLE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DUP-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'INCDUP  '          TO CA-REASON-CODE
              STRING 'Vehicle ' CA-IN-VEHICLE-REG
                     ' already has an incident on this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-INC-007 PROP is insured property (risk unit required);
      * FIXP is someone else's fixed property (never a risk unit).
       3300-VALIDATE-PROPERTY.
           IF WS-TYPE-PROPERTY
              IF CA-IN-RISK-UNIT-SEQ <= 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RSKREQD '       TO CA-REASON-CODE
                 MOVE 'Insured property incident needs a risk unit'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              PERFORM 4200-READ-RISK-UNIT
           ELSE
              IF CA-IN-RISK-UNIT-SEQ NOT = 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'FIXPRSK '       TO CA-REASON-CODE
                 MOVE 'Third-party property cannot name a risk unit'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-INC-008 Injury needs an injured contact and injury type
       3400-VALIDATE-INJURY.
           IF CA-IN-INJURED-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INJCONT '          TO CA-REASON-CODE
              MOVE 'Injury incident needs the injured contact id'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-IN-INJURED-ID       TO CON-CONTACT-ID
           EXEC SQL
              SELECT CONTACT_ID
                INTO :CON-CONTACT-ID
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :CON-CONTACT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CONNOTFD'       TO CA-REASON-CODE
                 MOVE CA-IN-INJURED-ID TO WS-ID-DISP
                 STRING 'Injured contact ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3400-VALIDATE-INJURY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF CA-IN-INJURY-TYPE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INJTYPE '          TO CA-REASON-CODE
              MOVE 'Injury type is required for INJ'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'INJTYPE '             TO WS-COD-TABLE
           MOVE CA-IN-INJURY-TYPE      TO WS-COD-VALUE
           MOVE 'INJTYPE '             TO WS-COD-REASON
           PERFORM 8300-VALIDATE-CODE.

      ******************************************************************
       3500-CLEAR-INAPPLICABLE.
           IF NOT WS-TYPE-VEHICLE
              MOVE SPACES              TO CA-IN-VEHICLE-REG
              MOVE 'Y'                 TO CA-IN-DRIVEABLE
           END-IF
           IF NOT WS-TYPE-INJURY
              MOVE SPACES              TO CA-IN-INJURY-TYPE
              MOVE 0                   TO CA-IN-INJURED-ID
           END-IF.

      ******************************************************************
       4000-READ-CLAIM.
           MOVE CA-IN-CLAIM-ID         TO CLM-CLAIM-ID
           EXEC SQL
              SELECT CLAIM_NUMBER, CLAIM_STATE, SEGMENT_CODE,
                     SEVERITY, VERSION_NO
                INTO :CLM-CLAIM-NUMBER, :CLM-CLAIM-STATE,
                     :CLM-SEGMENT-CODE, :CLM-SEVERITY,
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
                 MOVE CA-IN-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
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
           IF CA-IN-INCIDENT-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCIDREQ'          TO CA-REASON-CODE
              MOVE 'Incident id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
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
           MOVE CA-IN-CLAIM-ID         TO RSK-CLAIM-ID
           MOVE CA-IN-RISK-UNIT-SEQ    TO RSK-RISK-UNIT-SEQ
           EXEC SQL
              SELECT RISK_UNIT_TYPE, VEHICLE_REG, INSURED_VALUE
                INTO :RSK-RISK-UNIT-TYPE, :RSK-VEHICLE-REG,
                     :RSK-INSURED-VALUE
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
                 MOVE CA-IN-RISK-UNIT-SEQ TO WS-SEQ-DISP
                 STRING 'Risk unit ' WS-SEQ-DISP
                        ' is not on the claim policy snapshot'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-READ-RISK-UNIT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4210-FIND-RISK-UNIT-BY-REG.
           MOVE CA-IN-CLAIM-ID         TO RSK-CLAIM-ID
           MOVE CA-IN-VEHICLE-REG      TO RSK-VEHICLE-REG
           EXEC SQL
              SELECT RISK_UNIT_SEQ
                INTO :RSK-RISK-UNIT-SEQ
                FROM CLMDB.CLM_RISK_UNIT
               WHERE CLAIM_ID    = :RSK-CLAIM-ID
                 AND VEHICLE_REG = :RSK-VEHICLE-REG
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE RSK-RISK-UNIT-SEQ TO CA-IN-RISK-UNIT-SEQ
              WHEN 100
                 MOVE 0                TO CA-IN-RISK-UNIT-SEQ
              WHEN OTHER
                 MOVE '4210-FIND-RISK-UNIT-BY-REG' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-INSERT-INCIDENT.
           EXEC SQL
              INSERT INTO CLMDB.CLM_INCIDENT
                    (INCIDENT_ID, CLAIM_ID, INCIDENT_TYPE,
                     RISK_UNIT_SEQ, DESCRIPTION, SEVERITY,
                     VEHICLE_REG, DRIVEABLE_FLAG, ESTIMATED_DAMAGE,
                     TOTAL_LOSS_FLAG, INJURY_TYPE,
                     INJURED_CONTACT_ID, CREATE_TS)
              VALUES (:INC-INCIDENT-ID, :INC-CLAIM-ID,
                     :INC-INCIDENT-TYPE, :INC-RISK-UNIT-SEQ,
                     :INC-DESCRIPTION, :INC-SEVERITY,
                     :INC-VEHICLE-REG, :INC-DRIVEABLE-FLAG,
                     :INC-ESTIMATED-DAMAGE, :INC-TOTAL-LOSS-FLAG,
                     :INC-INJURY-TYPE, :INC-INJURED-CONTACT-ID,
                     CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'INCDUPKY'       TO CA-REASON-CODE
                 MOVE 'Incident id already exists - sequence error'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4300-INSERT-INCIDENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-UPDATE-INCIDENT.
           EXEC SQL
              UPDATE CLMDB.CLM_INCIDENT
                 SET RISK_UNIT_SEQ      = :INC-RISK-UNIT-SEQ,
                     DESCRIPTION        = :INC-DESCRIPTION,
                     SEVERITY           = :INC-SEVERITY,
                     VEHICLE_REG        = :INC-VEHICLE-REG,
                     DRIVEABLE_FLAG     = :INC-DRIVEABLE-FLAG,
                     ESTIMATED_DAMAGE   = :INC-ESTIMATED-DAMAGE,
                     INJURY_TYPE        = :INC-INJURY-TYPE,
                     INJURED_CONTACT_ID = :INC-INJURED-CONTACT-ID
               WHERE INCIDENT_ID = :INC-INCIDENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INCNOTFD'       TO CA-REASON-CODE
                 MOVE 'Incident was deleted during update'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4400-UPDATE-INCIDENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

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
       5000-LOAD-INCIDENT-ROW.
           MOVE CA-IN-INCIDENT-ID      TO INC-INCIDENT-ID
           MOVE CA-IN-CLAIM-ID         TO INC-CLAIM-ID
           MOVE CA-IN-TYPE             TO INC-INCIDENT-TYPE
           MOVE CA-IN-RISK-UNIT-SEQ    TO INC-RISK-UNIT-SEQ
           MOVE CA-IN-DESCRIPTION      TO INC-DESCRIPTION-TEXT
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-IN-DESCRIPTION)
                   TALLYING WS-TRAIL FOR LEADING SPACES
           COMPUTE INC-DESCRIPTION-LEN =
                   LENGTH OF CA-IN-DESCRIPTION - WS-TRAIL
           MOVE CA-IN-SEVERITY         TO INC-SEVERITY
           MOVE CA-IN-VEHICLE-REG      TO INC-VEHICLE-REG
           MOVE CA-IN-DRIVEABLE        TO INC-DRIVEABLE-FLAG
           MOVE CA-IN-EST-DAMAGE       TO INC-ESTIMATED-DAMAGE
           MOVE CA-IN-TOTAL-LOSS       TO INC-TOTAL-LOSS-FLAG
           MOVE CA-IN-INJURY-TYPE      TO INC-INJURY-TYPE
           MOVE CA-IN-INJURED-ID       TO INC-INJURED-CONTACT-ID.

      ******************************************************************
       5100-BUILD-AUDIT-PAYLOAD.
           MOVE CA-IN-INCIDENT-ID      TO WS-ID-DISP
           MOVE CA-IN-CLAIM-ID         TO WS-CLM-DISP
           MOVE CA-IN-RISK-UNIT-SEQ    TO WS-SEQ-DISP
           MOVE CA-IN-EST-DAMAGE       TO WS-AMT-DISP
           MOVE SPACES                 TO WS-AUD-PAYLOAD
           STRING 'INC=' WS-ID-DISP ' CLM=' WS-CLM-DISP
                  ' TYPE=' CA-IN-TYPE ' RU=' WS-SEQ-DISP
                  ' REG=' CA-IN-VEHICLE-REG
                  ' DRV=' CA-IN-DRIVEABLE
                  ' DMG=' WS-AMT-DISP
                  ' TL=' CA-IN-TOTAL-LOSS
                  ' INJ=' CA-IN-INJURY-TYPE
                  ' USER=' CA-USER-ID
                  DELIMITED BY SIZE INTO WS-AUD-PAYLOAD.

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
       8000-NEXT-SEQUENCE.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE CA-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE WS-SEQ-NAME            TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 8
              MOVE SQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQA-REASON-CODE     TO CA-REASON-CODE
              MOVE SQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

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
           MOVE WS-AUD-EVENT           TO AUA-AC-EVENT-CODE
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
       8300-VALIDATE-CODE.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE CA-BUS-DATE            TO CDA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE WS-COD-TABLE           TO CDA-CD-TABLE
           MOVE WS-COD-VALUE           TO CDA-CD-CODE
           CALL WS-COD-PGM USING WS-COD-CA
           IF CDA-RETURN-CODE >= 12
              MOVE CDA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE CDA-REASON-CODE     TO CA-REASON-CODE
              MOVE CDA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE WS-COD-REASON       TO CA-REASON-CODE
              STRING WS-COD-VALUE ' is not a valid ' WS-COD-TABLE
                     ' code' DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-INC-008 Injured person is linked to the claim as INJURED
       8400-LINK-INJURED.
           INITIALIZE WS-CON-CA
           MOVE 'CONLNK  '             TO COA-REQUEST-ID
           MOVE CA-USER-ID             TO COA-USER-ID
           MOVE CA-BUS-DATE            TO COA-BUS-DATE
           MOVE CA-TRACE-ID            TO COA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO COA-CALLER-PGM
           MOVE CA-IN-INJURED-ID       TO COA-CO-CONTACT-ID
           MOVE CA-IN-CLAIM-ID         TO COA-CO-CLAIM-ID
           MOVE 'INJURED '             TO COA-CO-ROLE
           CALL WS-CON-PGM USING WS-CON-CA
           IF COA-RETURN-CODE >= 12
              AND NOT COA-RC-CONFLICT
              MOVE COA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE COA-REASON-CODE     TO CA-REASON-CODE
              MOVE COA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-INC-011 Re-derive segment and severity; persist and
      * record only a real change (CLMSEGMT may already have saved it)
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
              MOVE SGA-SG-SEVERITY     TO WS-SEQ-DISP
              MOVE 'SEGCHG  '          TO WS-HIS-EVENT
              STRING 'Segment ' WS-OLD-SEGMENT ' -> '
                     SGA-SG-SEGMENT ' severity ' WS-SEQ-DISP
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
           STRING 'CLMINCDT Db2 error SQLCODE ' WS-SQLCODE-DISP
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
