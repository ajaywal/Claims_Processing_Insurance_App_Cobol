       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMRECOV.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMRECOV                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Recoveries. Subrogation against an adverse party *
      *               (open / update / close with outcome), salvage of *
      *               total-loss incidents (assign to a salvage vendor *
      *               / record the sale) and deductible recovery from  *
      *               the insured, billed through BillingCore.         *
      *               Every recovery amount is booked as a recovery    *
      *               reserve or receipt through CLMFINTX.             *
      * REQUESTS    : SUBOPN SUBUPD SUBCLS SALASG SALSLD DEDRCV        *
      * CALLED BY   : recovery screens, litigation close, batch        *
      * CALLS       : CLMFINTX CLMCONTC CLMEVTQ CLMHIST0 CLMAUDCH      *
      *               CLMMONEY CLMDATES CLMERRLG                       *
      * TABLES      : CLM_SUBROGATION (C R U)  CLM_SALVAGE (C R U)     *
      *               CLM_CLAIM (R)  CLM_EXPOSURE (R)  CLM_INCIDENT (R)*
      *               CLM_CONTACT (R)  CLM_CLAIM_CONTACT (R)           *
      *               CLM_POLICY_COVERAGE (R)  CLM_RESERVE_LINE (R)    *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-REC-001 Recoveries only on an OPEN claim  *
      *               BR-CLM-REC-002 No subrogation when the insured   *
      *                              is 100% at fault (FAULT INSD)     *
      *               BR-CLM-REC-003 Liability percent 0.01 - 100.00   *
      *               BR-CLM-REC-004 Adverse party must exist and is   *
      *                              linked to the claim as ADVERSE    *
      *               BR-CLM-REC-005 Estimated recovery = claim total  *
      *                              paid x liability percent          *
      *               BR-CLM-REC-006 Recovery reserve tracks the       *
      *                              estimated recovery (RCVRSV)       *
      *               BR-CLM-REC-007 One subrogation per claim; only   *
      *                              OPEN subrogation can change       *
      *               BR-CLM-REC-008 Close outcome RECOVERD, PARTIAL   *
      *                              or ABANDOND; RECOVERD / PARTIAL   *
      *                              need a receipt, ABANDOND none     *
      *               BR-CLM-REC-009 Closing releases the remaining    *
      *                              recovery reserve (RCVRSV 0)       *
      *               BR-CLM-REC-010 Salvage only for a total-loss     *
      *                              incident on the claim             *
      *               BR-CLM-REC-011 Salvage vendor must be a SALVAGE  *
      *                              vendor not hit by sanctions       *
      *               BR-CLM-REC-012 Sale amount > 0, sale date on or  *
      *                              after assignment and not in the   *
      *                              future; books SALVAGE receipt     *
      *               BR-CLM-REC-013 Insured owes the deductible not   *
      *                              yet applied to paid exposures     *
      *               BR-CLM-REC-014 Deductible owed is reserved once  *
      *                              and billed by DEDBILL event       *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMRECOV'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-FIN-PGM                  PIC X(8)  VALUE 'CLMFINTX'.
       01  WS-CON-PGM                  PIC X(8)  VALUE 'CLMCONTC'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'CLMEVTQ '.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'CLMMONEY'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.

       01  WS-CONSTANTS.
           05 WS-RCV-COST-TYPE         PIC X(4)  VALUE 'CLMC'.
           05 WS-CAT-SUBRO             PIC X(8)  VALUE 'SUBRO'.
           05 WS-CAT-SALVAGE           PIC X(8)  VALUE 'SALVAGE'.
           05 WS-CAT-DEDUCT            PIC X(8)  VALUE 'DEDUCT'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-EXPOSURE-ID           PIC S9(9) COMP VALUE 0.
           05 WS-INCIDENT-ID           PIC S9(9) COMP VALUE 0.
           05 WS-PARTY-ID              PIC S9(9) COMP VALUE 0.
           05 WS-PAYER-ID              PIC S9(9) COMP VALUE 0.
           05 WS-RCV-CATEGORY          PIC X(8)  VALUE SPACES.
           05 WS-RCV-AMOUNT            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-EST-RECOVERY          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DED-OWED              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-RSV-REMAINING         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PRIOR-DED             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMT-DISP              PIC Z(10)9.99.
           05 WS-PCT-DISP              PIC ZZ9.99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-HIS-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-AUD-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-AUD-PAYLOAD           PIC X(400) VALUE SPACES.
           05 WS-FIN-REQ               PIC X(8)  VALUE SPACES.
           05 WS-DATE-REQ              PIC X(8)  VALUE SPACES.
           05 WS-DATE-1                PIC X(10) VALUE SPACES.
           05 WS-DATE-2                PIC X(10) VALUE SPACES.
           05 WS-LINK-ROLE             PIC X(8)  VALUE SPACES.

      * DEDBILL payload per docs/EVENTS.md (CLM -> BIL)
       01  WS-DEDBILL-PAYLOAD.
           05 WS-DB-CLAIM-NUMBER       PIC X(12).
           05 WS-DB-POLICY-NO          PIC 9(10).
           05 WS-DB-AMOUNT             PIC 9(11)V99.
           05 WS-DB-SIGN               PIC X.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDSUB END-EXEC.
           EXEC SQL INCLUDE CLMDSAL END-EXEC.
           EXEC SQL INCLUDE CLMDINC END-EXEC.
           EXEC SQL INCLUDE CLMDEXP END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDPCV END-EXEC.
           EXEC SQL INCLUDE CLMDRSL END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

       01  WS-SVC-RESULT.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SVC-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

       01  WS-FIN-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==FTA-==.
           03 FTA-BODY.
              COPY CLMCFIN REPLACING LEADING ==CA-== BY ==FTA-==.

       01  WS-CON-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==COA-==.
           03 COA-BODY.
              COPY CLMCCON REPLACING LEADING ==CA-== BY ==COA-==.

       01  WS-EVT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==EVA-==.
           03 EVA-BODY.
              COPY CLMCEVT REPLACING LEADING ==CA-== BY ==EVA-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.

       01  WS-AUD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ADA-==.
           03 ADA-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==ADA-==.

       01  WS-MNY-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY CLMCMNY REPLACING LEADING ==CA-== BY ==MNA-==.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCREC.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'SUBOPN  '
                 PERFORM 2100-OPEN-SUBROGATION
              WHEN 'SUBUPD  '
                 PERFORM 2200-UPDATE-SUBROGATION
              WHEN 'SUBCLS  '
                 PERFORM 2300-CLOSE-SUBROGATION
              WHEN 'SALASG  '
                 PERFORM 2400-ASSIGN-SALVAGE
              WHEN 'SALSLD  '
                 PERFORM 2500-SALVAGE-SOLD
              WHEN 'DEDRCV  '
                 PERFORM 2600-DEDUCTIBLE-RECOVERY
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMRECOV: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-RC-TXN-ID
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
           END-IF
           IF CA-RC-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for a recovery request'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM.

      ******************************************************************
      * SUBOPN
       2100-OPEN-SUBROGATION.
           PERFORM 3100-CHECK-CLAIM-OPEN
      * BR-CLM-REC-002 No subrogation against our own insured's fault
           IF CLM-FAULT-RATING = 'INSD'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INSDFLT '          TO CA-REASON-CODE
              MOVE 'Insured is 100% at fault - no subrogation'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4200-READ-SUBROGATION
           IF SQLCODE = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'SUBEXIST'          TO CA-REASON-CODE
              STRING 'Subrogation already exists for claim '
                     CLM-CLAIM-NUMBER ' (' SUB-SUBRO-STATUS ')'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3200-VALIDATE-LIABILITY
           IF CA-RC-ADVERSE-PARTY-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ADVREQ  '          TO CA-REASON-CODE
              MOVE 'Adverse party contact is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-RC-ADVERSE-PARTY-ID TO WS-PARTY-ID
           PERFORM 3300-LINK-ADVERSE-PARTY
           MOVE 0                      TO WS-INCIDENT-ID
           PERFORM 5100-RESOLVE-EXPOSURE
           PERFORM 5200-CALC-EST-RECOVERY
           MOVE CA-RC-CLAIM-ID         TO SUB-CLAIM-ID
           MOVE 'OPEN'                 TO SUB-SUBRO-STATUS
           MOVE CA-RC-ADVERSE-PARTY-ID TO SUB-ADVERSE-PARTY-ID
           PERFORM 5300-SET-CARRIER
           MOVE CA-RC-LIABILITY-PCT    TO SUB-LIABILITY-PCT
           MOVE WS-EST-RECOVERY        TO SUB-EST-RECOVERY
           MOVE WS-BUS-DATE            TO SUB-OPENED-DATE
           EXEC SQL
              INSERT INTO CLMDB.CLM_SUBROGATION
                     (CLAIM_ID, SUBRO_STATUS, ADVERSE_PARTY_ID,
                      ADVERSE_CARRIER, LIABILITY_PCT, EST_RECOVERY,
                      OPENED_DATE, CLOSED_DATE, CLOSE_OUTCOME)
              VALUES (:SUB-CLAIM-ID, :SUB-SUBRO-STATUS,
                      :SUB-ADVERSE-PARTY-ID,
                      :SUB-ADVERSE-CARRIER :SUB-ADVERSE-CARRIER-IND,
                      :SUB-LIABILITY-PCT, :SUB-EST-RECOVERY,
                      :SUB-OPENED-DATE, NULL, ' ')
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'SUBEXIST'       TO CA-REASON-CODE
                 MOVE 'Subrogation was opened by another user'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2100-OPEN-SUBROGATION' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
      * BR-CLM-REC-006 Recovery reserve = estimated recovery
           IF WS-EST-RECOVERY > 0
              MOVE WS-CAT-SUBRO        TO WS-RCV-CATEGORY
              MOVE WS-EST-RECOVERY     TO WS-RCV-AMOUNT
              PERFORM 8100-SET-RECOVERY-RESERVE
           END-IF
           MOVE WS-EST-RECOVERY        TO CA-RC-EST-RECOVERY
           MOVE 'OPEN'                 TO CA-RC-STATUS
           MOVE CA-RC-LIABILITY-PCT    TO WS-PCT-DISP
           MOVE WS-EST-RECOVERY        TO WS-AMT-DISP
           MOVE 'SUBOPEN '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Subrogation opened: liability ' WS-PCT-DISP
                  '% est recovery ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           MOVE 'SUBOPN  '             TO WS-AUD-EVENT
           PERFORM 8400-AUDIT-RECOVERY
           IF WS-EST-RECOVERY = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOPAID  '          TO CA-REASON-CODE
              MOVE 'Subrogation opened - nothing paid yet, estimate 0'
                                       TO CA-MESSAGE
           ELSE
              STRING 'Subrogation opened, est recovery ' WS-AMT-DISP
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * SUBUPD - liability, adverse party / carrier, re-estimate
       2200-UPDATE-SUBROGATION.
           PERFORM 3400-LOAD-OPEN-SUBRO
           IF CA-RC-LIABILITY-PCT NOT = 0
              PERFORM 3200-VALIDATE-LIABILITY
              MOVE CA-RC-LIABILITY-PCT TO SUB-LIABILITY-PCT
           END-IF
           IF CA-RC-ADVERSE-PARTY-ID > 0
              AND CA-RC-ADVERSE-PARTY-ID NOT = SUB-ADVERSE-PARTY-ID
              MOVE CA-RC-ADVERSE-PARTY-ID TO WS-PARTY-ID
              PERFORM 3300-LINK-ADVERSE-PARTY
              MOVE CA-RC-ADVERSE-PARTY-ID TO SUB-ADVERSE-PARTY-ID
           END-IF
           IF CA-RC-ADVERSE-CARRIER NOT = SPACES
              PERFORM 5300-SET-CARRIER
           END-IF
           MOVE SUB-LIABILITY-PCT      TO CA-RC-LIABILITY-PCT
           PERFORM 5200-CALC-EST-RECOVERY
           EXEC SQL
              UPDATE CLMDB.CLM_SUBROGATION
                 SET LIABILITY_PCT    = :SUB-LIABILITY-PCT,
                     ADVERSE_PARTY_ID = :SUB-ADVERSE-PARTY-ID,
                     ADVERSE_CARRIER  =
                        :SUB-ADVERSE-CARRIER :SUB-ADVERSE-CARRIER-IND,
                     EST_RECOVERY     = :WS-EST-RECOVERY
               WHERE CLAIM_ID         = :CA-RC-CLAIM-ID
                 AND SUBRO_STATUS     = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-UPDATE-SUBROGATION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Subrogation was closed by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-REC-006 Keep the recovery reserve on the estimate
           IF WS-EST-RECOVERY NOT = SUB-EST-RECOVERY
              MOVE 0                   TO WS-INCIDENT-ID
              PERFORM 5100-RESOLVE-EXPOSURE
              MOVE WS-CAT-SUBRO        TO WS-RCV-CATEGORY
              MOVE WS-EST-RECOVERY     TO WS-RCV-AMOUNT
              PERFORM 8100-SET-RECOVERY-RESERVE
           END-IF
           MOVE WS-EST-RECOVERY        TO CA-RC-EST-RECOVERY
           MOVE 'OPEN'                 TO CA-RC-STATUS
           MOVE SUB-LIABILITY-PCT      TO WS-PCT-DISP
           MOVE WS-EST-RECOVERY        TO WS-AMT-DISP
           MOVE 'SUBUPD  '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Subrogation updated: liability ' WS-PCT-DISP
                  '% est recovery ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           STRING 'Subrogation updated, est recovery ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * SUBCLS
       2300-CLOSE-SUBROGATION.
           PERFORM 3400-LOAD-OPEN-SUBRO
      * BR-CLM-REC-008 Outcome and receipt rules
           EVALUATE CA-RC-OUTCOME
              WHEN 'RECOVERD'
              WHEN 'PARTIAL '
                 IF CA-RC-AMOUNT NOT > 0
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'RCPTREQ '    TO CA-REASON-CODE
                    STRING 'Outcome ' CA-RC-OUTCOME
                           ' needs the amount recovered'
                           DELIMITED BY SIZE INTO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 'ABANDOND'
                 IF CA-RC-AMOUNT NOT = 0
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'ABNDAMT '    TO CA-REASON-CODE
                    MOVE 'Abandoned subrogation cannot carry a receipt'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'BADOUTCM'       TO CA-REASON-CODE
                 STRING 'Outcome ' CA-RC-OUTCOME ' must be RECOVERD,'
                        ' PARTIAL or ABANDOND'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           MOVE 0                      TO WS-INCIDENT-ID
           PERFORM 5100-RESOLVE-EXPOSURE
           MOVE WS-CAT-SUBRO           TO WS-RCV-CATEGORY
           MOVE SUB-EST-RECOVERY       TO WS-RSV-REMAINING
           IF CA-RC-AMOUNT > 0
              MOVE CA-RC-AMOUNT        TO WS-RCV-AMOUNT
              MOVE SUB-ADVERSE-PARTY-ID TO WS-PAYER-ID
              PERFORM 8200-BOOK-RECOVERY-RECEIPT
              MOVE FTA-FT-RL-RECOVERY-RSV TO WS-RSV-REMAINING
           END-IF
      * BR-CLM-REC-009 Closing releases whatever reserve remains
           IF WS-RSV-REMAINING > 0
              MOVE 0                   TO WS-RCV-AMOUNT
              PERFORM 8100-SET-RECOVERY-RESERVE
           END-IF
           EXEC SQL
              UPDATE CLMDB.CLM_SUBROGATION
                 SET SUBRO_STATUS  = 'CLSD',
                     CLOSED_DATE   = :WS-BUS-DATE,
                     CLOSE_OUTCOME = :CA-RC-OUTCOME
               WHERE CLAIM_ID      = :CA-RC-CLAIM-ID
                 AND SUBRO_STATUS  = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-CLOSE-SUBROGATION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Subrogation was closed by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'CLSD'                 TO CA-RC-STATUS
           MOVE CA-RC-AMOUNT           TO WS-AMT-DISP
           MOVE 'SUBCLOSE'             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Subrogation closed ' CA-RC-OUTCOME
                  ' recovered ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           MOVE 'SUBCLS  '             TO WS-AUD-EVENT
           PERFORM 8400-AUDIT-RECOVERY
           STRING 'Subrogation closed - ' CA-RC-OUTCOME
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * SALASG
       2400-ASSIGN-SALVAGE.
           PERFORM 3100-CHECK-CLAIM-OPEN
           PERFORM 3500-READ-INCIDENT
      * BR-CLM-REC-010 Salvage only for a total-loss incident
           IF INC-TOTAL-LOSS-FLAG NOT = 'Y'
              SET CA-RC-STATE          TO TRUE
              MOVE 'NOTTOTL '          TO CA-REASON-CODE
              MOVE CA-RC-INCIDENT-ID   TO WS-ID-DISP
              STRING 'Incident ' WS-ID-DISP
                     ' is not a total loss - no salvage'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3600-CHECK-SALVAGE-VENDOR
           IF CA-RC-AMOUNT < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADAMT  '          TO CA-REASON-CODE
              MOVE 'Estimated salvage value cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-RC-CLAIM-ID         TO SAL-CLAIM-ID
           MOVE CA-RC-INCIDENT-ID      TO SAL-INCIDENT-ID
           MOVE CA-RC-VENDOR-ID        TO SAL-VENDOR-CONTACT-ID
           MOVE 'ASGN'                 TO SAL-SALVAGE-STATUS
           MOVE WS-BUS-DATE            TO SAL-ASSIGNED-DATE
           MOVE CA-RC-AMOUNT           TO SAL-EST-VALUE
           EXEC SQL
              INSERT INTO CLMDB.CLM_SALVAGE
                     (CLAIM_ID, INCIDENT_ID, VENDOR_CONTACT_ID,
                      SALVAGE_STATUS, ASSIGNED_DATE, EST_VALUE,
                      SALE_DATE, SALE_AMOUNT)
              VALUES (:SAL-CLAIM-ID, :SAL-INCIDENT-ID,
                      :SAL-VENDOR-CONTACT-ID, :SAL-SALVAGE-STATUS,
                      :SAL-ASSIGNED-DATE, :SAL-EST-VALUE, NULL, 0)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'SALEXIST'       TO CA-REASON-CODE
                 MOVE CA-RC-INCIDENT-ID TO WS-ID-DISP
                 STRING 'Salvage already assigned for incident '
                        WS-ID-DISP DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2400-ASSIGN-SALVAGE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE CA-RC-VENDOR-ID        TO WS-PARTY-ID
           MOVE 'VENDOR  '             TO WS-LINK-ROLE
           PERFORM 8500-LINK-CONTACT
           IF CA-RC-AMOUNT > 0
              MOVE CA-RC-INCIDENT-ID   TO WS-INCIDENT-ID
              PERFORM 5100-RESOLVE-EXPOSURE
              MOVE WS-CAT-SALVAGE      TO WS-RCV-CATEGORY
              MOVE CA-RC-AMOUNT        TO WS-RCV-AMOUNT
              PERFORM 8100-SET-RECOVERY-RESERVE
           END-IF
           MOVE 'ASGN'                 TO CA-RC-STATUS
           MOVE CA-RC-VENDOR-ID        TO WS-ID-DISP
           MOVE CA-RC-AMOUNT           TO WS-AMT-DISP
           MOVE 'SALASGN '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Salvage assigned to vendor ' WS-ID-DISP
                  ' est value ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           MOVE 'Salvage assigned'     TO CA-MESSAGE.

      ******************************************************************
      * SALSLD
       2500-SALVAGE-SOLD.
           PERFORM 3100-CHECK-CLAIM-OPEN
           IF CA-RC-INCIDENT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCREQ  '          TO CA-REASON-CODE
              MOVE 'Incident id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT VENDOR_CONTACT_ID, SALVAGE_STATUS,
                     ASSIGNED_DATE, EST_VALUE
                INTO :SAL-VENDOR-CONTACT-ID, :SAL-SALVAGE-STATUS,
                     :SAL-ASSIGNED-DATE, :SAL-EST-VALUE
                FROM CLMDB.CLM_SALVAGE
               WHERE CLAIM_ID    = :CA-RC-CLAIM-ID
                 AND INCIDENT_ID = :CA-RC-INCIDENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'SALNOTFD'       TO CA-REASON-CODE
                 MOVE 'No salvage assignment for this incident'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2500-SALVAGE-SOLD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF SAL-SALVAGE-STATUS NOT = 'ASGN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'SALNOTAS'          TO CA-REASON-CODE
              STRING 'Salvage is ' SAL-SALVAGE-STATUS
                     ', only ASGN salvage can be sold'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3700-VALIDATE-SALE
           MOVE CA-RC-INCIDENT-ID      TO WS-INCIDENT-ID
           PERFORM 5100-RESOLVE-EXPOSURE
           MOVE WS-CAT-SALVAGE         TO WS-RCV-CATEGORY
           MOVE CA-RC-AMOUNT           TO WS-RCV-AMOUNT
           MOVE SAL-VENDOR-CONTACT-ID  TO WS-PAYER-ID
           PERFORM 8200-BOOK-RECOVERY-RECEIPT
           IF FTA-FT-RL-RECOVERY-RSV > 0
              MOVE 0                   TO WS-RCV-AMOUNT
              PERFORM 8100-SET-RECOVERY-RESERVE
           END-IF
           EXEC SQL
              UPDATE CLMDB.CLM_SALVAGE
                 SET SALVAGE_STATUS = 'SOLD',
                     SALE_DATE      = :CA-RC-SALE-DATE,
                     SALE_AMOUNT    = :CA-RC-AMOUNT
               WHERE CLAIM_ID       = :CA-RC-CLAIM-ID
                 AND INCIDENT_ID    = :CA-RC-INCIDENT-ID
                 AND SALVAGE_STATUS = 'ASGN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-SALVAGE-SOLD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Salvage was updated by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'SOLD'                 TO CA-RC-STATUS
           MOVE CA-RC-AMOUNT           TO WS-AMT-DISP
           MOVE 'SALSOLD '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Salvage sold ' CA-RC-SALE-DATE ' for '
                  WS-AMT-DISP DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           MOVE 'SALSLD  '             TO WS-AUD-EVENT
           PERFORM 8400-AUDIT-RECOVERY
           STRING 'Salvage sale recorded for ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * DEDRCV - deductible owed by the insured, billed via BillingCore
       2600-DEDUCTIBLE-RECOVERY.
           PERFORM 3100-CHECK-CLAIM-OPEN
           IF CA-RC-EXPOSURE-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPREQ  '          TO CA-REASON-CODE
              MOVE 'Exposure id is required for deductible recovery'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-INCIDENT-ID
           PERFORM 5100-RESOLVE-EXPOSURE
           PERFORM 5400-CALC-DEDUCTIBLE-OWED
           IF WS-DED-OWED NOT > 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'DEDNONE '          TO CA-REASON-CODE
              MOVE 'Insured owes no deductible on this exposure'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-RC-AMOUNT > 0
              IF CA-RC-AMOUNT > WS-DED-OWED
                 MOVE WS-DED-OWED      TO WS-AMT-DISP
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DEDEXCD '       TO CA-REASON-CODE
                 STRING 'Amount exceeds deductible owed of '
                        WS-AMT-DISP DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE CA-RC-AMOUNT        TO WS-DED-OWED
           END-IF
           MOVE WS-CAT-DEDUCT          TO WS-RCV-CATEGORY
           MOVE WS-DED-OWED            TO WS-RCV-AMOUNT
           PERFORM 8100-SET-RECOVERY-RESERVE
           PERFORM 8600-PUT-DEDBILL-EVENT
           MOVE WS-DED-OWED            TO CA-RC-AMOUNT
                                          CA-RC-EST-RECOVERY
           MOVE WS-DED-OWED            TO WS-AMT-DISP
           MOVE 'DEDBILL '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Deductible ' WS-AMT-DISP
                  ' billed to insured via BillingCore'
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           MOVE 'DEDRCV  '             TO WS-AUD-EVENT
           PERFORM 8400-AUDIT-RECOVERY
           STRING 'Deductible of ' WS-AMT-DISP ' sent to billing'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-CLM-REC-001 Recoveries only on an OPEN claim
       3100-CHECK-CLAIM-OPEN.
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMNOTOP'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ' - recoveries need an OPEN claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-REC-003 Liability percent 0.01 - 100.00
       3200-VALIDATE-LIABILITY.
           IF CA-RC-LIABILITY-PCT NOT > 0
              OR CA-RC-LIABILITY-PCT > 100
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADLIAB '          TO CA-REASON-CODE
              MOVE 'Adverse liability must be 0.01 to 100 percent'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-REC-004 Adverse party must exist; link it as ADVERSE
       3300-LINK-ADVERSE-PARTY.
           EXEC SQL
              SELECT CONTACT_ID
                INTO :CON-CONTACT-ID
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :WS-PARTY-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ADVNOTFD'       TO CA-REASON-CODE
                 MOVE WS-PARTY-ID      TO WS-ID-DISP
                 STRING 'Adverse party contact ' WS-ID-DISP
                        ' not found' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3300-LINK-ADVERSE-PARTY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE 'ADVERSE '             TO WS-LINK-ROLE
           PERFORM 8500-LINK-CONTACT.

      ******************************************************************
      * BR-CLM-REC-007 Only OPEN subrogation can change
       3400-LOAD-OPEN-SUBRO.
           PERFORM 4200-READ-SUBROGATION
           IF SQLCODE = 100
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'SUBNOTFD'          TO CA-REASON-CODE
              STRING 'No subrogation on claim ' CLM-CLAIM-NUMBER
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF SUB-SUBRO-STATUS NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'SUBCLOSD'          TO CA-REASON-CODE
              STRING 'Subrogation is closed (' SUB-CLOSE-OUTCOME ')'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3500-READ-INCIDENT.
           IF CA-RC-INCIDENT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCREQ  '          TO CA-REASON-CODE
              MOVE 'Incident id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT CLAIM_ID, TOTAL_LOSS_FLAG, ESTIMATED_DAMAGE
                INTO :INC-CLAIM-ID, :INC-TOTAL-LOSS-FLAG,
                     :INC-ESTIMATED-DAMAGE
                FROM CLMDB.CLM_INCIDENT
               WHERE INCIDENT_ID = :CA-RC-INCIDENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO INC-CLAIM-ID
              WHEN OTHER
                 MOVE '3500-READ-INCIDENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF INC-CLAIM-ID NOT = CA-RC-CLAIM-ID
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'INCNOTFD'          TO CA-REASON-CODE
              MOVE CA-RC-INCIDENT-ID   TO WS-ID-DISP
              STRING 'Incident ' WS-ID-DISP ' not found on claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-REC-011 Vendor must be SALVAGE type and sanction-clear
       3600-CHECK-SALVAGE-VENDOR.
           IF CA-RC-VENDOR-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'VNDREQ  '          TO CA-REASON-CODE
              MOVE 'Salvage vendor is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT VENDOR_TYPE, SANCTION_STATUS
                INTO :CON-VENDOR-TYPE, :CON-SANCTION-STATUS
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :CA-RC-VENDOR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'VNDNOTFD'       TO CA-REASON-CODE
                 MOVE CA-RC-VENDOR-ID  TO WS-ID-DISP
                 STRING 'Vendor contact ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3600-CHECK-SALVAGE-VENDOR' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF CON-VENDOR-TYPE NOT = 'SALVAGE'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'VNDTYPE '          TO CA-REASON-CODE
              STRING 'Contact is a ' CON-VENDOR-TYPE
                     ' vendor, not SALVAGE' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CON-SANCTION-STATUS = 'HIT '
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SANCHIT '          TO CA-REASON-CODE
              MOVE 'Vendor is on a sanctions list - cannot be used'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-REC-012 Sale amount > 0; assignment <= sale <= today
       3700-VALIDATE-SALE.
           IF CA-RC-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SALEAMT '          TO CA-REASON-CODE
              MOVE 'Salvage sale amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-RC-SALE-DATE = SPACES
              MOVE WS-BUS-DATE         TO CA-RC-SALE-DATE
           END-IF
           MOVE 'DTVALD  '             TO WS-DATE-REQ
           MOVE CA-RC-SALE-DATE        TO WS-DATE-1
           PERFORM 8700-CALL-DATES
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SALEDATE'          TO CA-REASON-CODE
              STRING 'Sale date ' CA-RC-SALE-DATE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DTDIFF  '             TO WS-DATE-REQ
           MOVE SAL-ASSIGNED-DATE      TO WS-DATE-1
           MOVE CA-RC-SALE-DATE        TO WS-DATE-2
           PERFORM 8700-CALL-DATES
           IF DTA-DT-RESULT-NUM < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SALEDATE'          TO CA-REASON-CODE
              STRING 'Sale date is before salvage assignment on '
                     SAL-ASSIGNED-DATE DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-RC-SALE-DATE        TO WS-DATE-1
           MOVE WS-BUS-DATE            TO WS-DATE-2
           PERFORM 8700-CALL-DATES
           IF DTA-DT-RESULT-NUM < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SALEDATE'          TO CA-REASON-CODE
              MOVE 'Sale date cannot be in the future'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4100-READ-CLAIM.
           EXEC SQL
              SELECT CLAIM_NUMBER, GENAPP_POL_NUM, CLAIM_STATE,
                     FAULT_RATING, TOTAL_PAID
                INTO :CLM-CLAIM-NUMBER, :CLM-GENAPP-POL-NUM,
                     :CLM-CLAIM-STATE, :CLM-FAULT-RATING,
                     :CLM-TOTAL-PAID
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-RC-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-RC-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Caller tests SQLCODE 100
       4200-READ-SUBROGATION.
           EXEC SQL
              SELECT SUBRO_STATUS, ADVERSE_PARTY_ID, ADVERSE_CARRIER,
                     LIABILITY_PCT, EST_RECOVERY, OPENED_DATE,
                     CLOSE_OUTCOME
                INTO :SUB-SUBRO-STATUS, :SUB-ADVERSE-PARTY-ID,
                     :SUB-ADVERSE-CARRIER :SUB-ADVERSE-CARRIER-IND,
                     :SUB-LIABILITY-PCT, :SUB-EST-RECOVERY,
                     :SUB-OPENED-DATE, :SUB-CLOSE-OUTCOME
                FROM CLMDB.CLM_SUBROGATION
               WHERE CLAIM_ID = :CA-RC-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-READ-SUBROGATION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Exposure for the recovery line: the one given (must be on the
      * claim) else the exposure on WS-INCIDENT-ID, else the first
      * open exposure of the claim.
       5100-RESOLVE-EXPOSURE.
           IF CA-RC-EXPOSURE-ID > 0
              EXEC SQL
                 SELECT EXPOSURE_ID, CLAIM_ID, COV_SEQ, TOTAL_PAID,
                        DEDUCTIBLE_APPLIED
                   INTO :EXP-EXPOSURE-ID, :EXP-CLAIM-ID,
                        :EXP-COV-SEQ, :EXP-TOTAL-PAID,
                        :EXP-DEDUCTIBLE-APPLIED
                   FROM CLMDB.CLM_EXPOSURE
                  WHERE EXPOSURE_ID = :CA-RC-EXPOSURE-ID
              END-EXEC
              IF SQLCODE = 0 AND EXP-CLAIM-ID NOT = CA-RC-CLAIM-ID
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'EXPNOTCL'       TO CA-REASON-CODE
                 MOVE CA-RC-EXPOSURE-ID TO WS-ID-DISP
                 STRING 'Exposure ' WS-ID-DISP
                        ' does not belong to this claim'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           ELSE
              EXEC SQL
                 SELECT EXPOSURE_ID, CLAIM_ID, COV_SEQ, TOTAL_PAID,
                        DEDUCTIBLE_APPLIED
                   INTO :EXP-EXPOSURE-ID, :EXP-CLAIM-ID,
                        :EXP-COV-SEQ, :EXP-TOTAL-PAID,
                        :EXP-DEDUCTIBLE-APPLIED
                   FROM CLMDB.CLM_EXPOSURE
                  WHERE CLAIM_ID = :CA-RC-CLAIM-ID
                    AND EXPOSURE_STATE <> 'CLSD'
                  ORDER BY CASE WHEN INCIDENT_ID = :WS-INCIDENT-ID
                                 AND :WS-INCIDENT-ID > 0
                                THEN 0 ELSE 1 END,
                           EXPOSURE_SEQ
                  FETCH FIRST 1 ROW ONLY
              END-EXEC
           END-IF
           EVALUATE SQLCODE
              WHEN 0
                 MOVE EXP-EXPOSURE-ID  TO WS-EXPOSURE-ID
                                          CA-RC-EXPOSURE-ID
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EXPNOTFD'       TO CA-REASON-CODE
                 STRING 'No open exposure on this claim to carry '
                        'the recovery' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '5100-RESOLVE-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-REC-005 Est recovery = total paid x liability percent.
      * Percent -> basis points for CLMMONEY; 100% (10000 bps) is
      * outside the S9(4) rate field and is the whole paid amount.
       5200-CALC-EST-RECOVERY.
           EVALUATE TRUE
              WHEN CLM-TOTAL-PAID NOT > 0
                 MOVE 0                TO WS-EST-RECOVERY
              WHEN CA-RC-LIABILITY-PCT = 100
                 MOVE CLM-TOTAL-PAID   TO WS-EST-RECOVERY
              WHEN OTHER
                 INITIALIZE WS-MNY-CA
                 MOVE 'MNRATE  '       TO MNA-REQUEST-ID
                 MOVE CA-USER-ID       TO MNA-USER-ID
                 MOVE WS-BUS-DATE      TO MNA-BUS-DATE
                 MOVE WS-PROGRAM-ID    TO MNA-CALLER-PGM
                 MOVE CLM-TOTAL-PAID   TO MNA-MN-TOTAL
                 COMPUTE MNA-MN-RATE-BPS = CA-RC-LIABILITY-PCT * 100
                 CALL WS-MNY-PGM USING WS-MNY-CA
                 MOVE MNA-HEADER       TO SVC-HEADER
                 PERFORM 8900-CHECK-CALLEE
                 MOVE MNA-MN-RESULT    TO WS-EST-RECOVERY
           END-EVALUATE.

      ******************************************************************
       5300-SET-CARRIER.
           IF CA-RC-ADVERSE-CARRIER = SPACES
              MOVE -1                  TO SUB-ADVERSE-CARRIER-IND
              MOVE 0                   TO SUB-ADVERSE-CARRIER-LEN
           ELSE
              MOVE 0                   TO SUB-ADVERSE-CARRIER-IND
              MOVE CA-RC-ADVERSE-CARRIER TO SUB-ADVERSE-CARRIER-TEXT
              MOVE FUNCTION LENGTH(FUNCTION TRIM(CA-RC-ADVERSE-CARRIER
                   TRAILING))          TO SUB-ADVERSE-CARRIER-LEN
           END-IF.

      ******************************************************************
      * BR-CLM-REC-013 Owed = coverage deductible - deductible already
      * applied, only once the exposure has been paid (paid gross)
      * BR-CLM-REC-014 A deductible already reserved / recovered on
      * the DEDUCT line is never billed twice
       5400-CALC-DEDUCTIBLE-OWED.
           MOVE 0                      TO WS-DED-OWED
           IF EXP-TOTAL-PAID NOT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPNOPAY'          TO CA-REASON-CODE
              MOVE 'Exposure has no payments - deductible not due'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT DEDUCTIBLE_AMOUNT
                INTO :PCV-DEDUCTIBLE-AMOUNT
                FROM CLMDB.CLM_POLICY_COVERAGE
               WHERE CLAIM_ID = :CA-RC-CLAIM-ID
                 AND COV_SEQ  = :EXP-COV-SEQ
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO PCV-DEDUCTIBLE-AMOUNT
              WHEN OTHER
                 MOVE '5400-CALC-DEDUCTIBLE-OWED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE WS-EXPOSURE-ID         TO RSL-EXPOSURE-ID
           MOVE WS-RCV-COST-TYPE       TO RSL-COST-TYPE
           MOVE WS-CAT-DEDUCT          TO RSL-COST-CATEGORY
           EXEC SQL
              SELECT RECOVERY_RESERVE + TOTAL_RECOVERED
                INTO :WS-PRIOR-DED
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE EXPOSURE_ID   = :RSL-EXPOSURE-ID
                 AND COST_TYPE     = :RSL-COST-TYPE
                 AND COST_CATEGORY = :RSL-COST-CATEGORY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO WS-PRIOR-DED
              WHEN OTHER
                 MOVE '5400-CALC-DEDUCTIBLE-OWED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-PRIOR-DED > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DEDBILLD'          TO CA-REASON-CODE
              MOVE WS-PRIOR-DED        TO WS-AMT-DISP
              STRING 'Deductible already billed/recovered: '
                     WS-AMT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-DED-OWED = PCV-DEDUCTIBLE-AMOUNT
                               - EXP-DEDUCTIBLE-APPLIED.

      ******************************************************************
      * Recovery reserve on (exposure, CLMC, category) set to amount
       8100-SET-RECOVERY-RESERVE.
           MOVE 'RCVRSV  '             TO WS-FIN-REQ
           PERFORM 8150-CALL-FINTX.

      ******************************************************************
      * Recovery receipt from WS-PAYER-ID
       8200-BOOK-RECOVERY-RECEIPT.
           MOVE 'RCVCRT  '             TO WS-FIN-REQ
           PERFORM 8150-CALL-FINTX
           MOVE FTA-FT-TXN-ID          TO CA-RC-TXN-ID.

      ******************************************************************
       8150-CALL-FINTX.
           INITIALIZE WS-FIN-CA
           MOVE WS-FIN-REQ             TO FTA-REQUEST-ID
           MOVE CA-USER-ID             TO FTA-USER-ID
           MOVE WS-BUS-DATE            TO FTA-BUS-DATE
           MOVE CA-TRACE-ID            TO FTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO FTA-CALLER-PGM
           MOVE CA-RC-CLAIM-ID         TO FTA-FT-CLAIM-ID
           MOVE WS-EXPOSURE-ID         TO FTA-FT-EXPOSURE-ID
           MOVE WS-RCV-COST-TYPE       TO FTA-FT-COST-TYPE
           MOVE WS-RCV-CATEGORY        TO FTA-FT-COST-CATEGORY
                                          FTA-FT-RECOVERY-CATEGORY
           MOVE WS-RCV-AMOUNT          TO FTA-FT-AMOUNT
           IF WS-FIN-REQ = 'RCVCRT  '
              MOVE WS-PAYER-ID         TO FTA-FT-PAYEE-CONTACT-ID
           END-IF
           CALL WS-FIN-PGM USING WS-FIN-CA
           MOVE FTA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8300-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-RC-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HIA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Recovery money events are sealed in the audit hash chain
       8400-AUDIT-RECOVERY.
           MOVE SPACES                 TO WS-AUD-PAYLOAD
           MOVE CA-RC-AMOUNT           TO WS-AMT-DISP
           STRING CLM-CLAIM-NUMBER '|' CA-REQUEST-ID '|'
                  CA-RC-OUTCOME '|' WS-AMT-DISP '|' WS-HIS-DESC
                  DELIMITED BY SIZE INTO WS-AUD-PAYLOAD
           INITIALIZE WS-AUD-CA
           MOVE 'AUCADD  '             TO ADA-REQUEST-ID
           MOVE CA-USER-ID             TO ADA-USER-ID
           MOVE WS-BUS-DATE            TO ADA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ADA-CALLER-PGM
           MOVE CA-RC-CLAIM-ID         TO ADA-AC-CLAIM-ID
           MOVE WS-AUD-EVENT           TO ADA-AC-EVENT-CODE
           MOVE WS-AUD-PAYLOAD         TO ADA-AC-PAYLOAD
           MOVE WS-PROGRAM-ID          TO ADA-AC-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           MOVE ADA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Link WS-PARTY-ID to the claim with WS-LINK-ROLE unless present
       8500-LINK-CONTACT.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID     = :CA-RC-CLAIM-ID
                 AND CONTACT_ID   = :WS-PARTY-ID
                 AND CONTACT_ROLE = :WS-LINK-ROLE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '8500-LINK-CONTACT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0
              INITIALIZE WS-CON-CA
              MOVE 'CONLNK  '          TO COA-REQUEST-ID
              MOVE CA-USER-ID          TO COA-USER-ID
              MOVE WS-BUS-DATE         TO COA-BUS-DATE
              MOVE WS-PROGRAM-ID       TO COA-CALLER-PGM
              MOVE WS-PARTY-ID         TO COA-CO-CONTACT-ID
              MOVE CA-RC-CLAIM-ID      TO COA-CO-CLAIM-ID
              MOVE WS-LINK-ROLE        TO COA-CO-ROLE
              CALL WS-CON-PGM USING WS-CON-CA
              MOVE COA-HEADER          TO SVC-HEADER
              PERFORM 8900-CHECK-CALLEE
           END-IF.

      ******************************************************************
      * DEDBILL outbound event, layout per docs/EVENTS.md
       8600-PUT-DEDBILL-EVENT.
           MOVE CLM-CLAIM-NUMBER       TO WS-DB-CLAIM-NUMBER
           MOVE CLM-GENAPP-POL-NUM     TO WS-DB-POLICY-NO
           MOVE WS-DED-OWED            TO WS-DB-AMOUNT
           MOVE '+'                    TO WS-DB-SIGN
           INITIALIZE WS-EVT-CA
           MOVE 'EVPUT   '             TO EVA-REQUEST-ID
           MOVE CA-USER-ID             TO EVA-USER-ID
           MOVE WS-BUS-DATE            TO EVA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO EVA-CALLER-PGM
           MOVE 'DEDBILL '             TO EVA-EV-TYPE
           MOVE 'O'                    TO EVA-EV-DIRECTION
           MOVE 'CLMS'                 TO EVA-EV-SOURCE
           MOVE 'BILL'                 TO EVA-EV-TARGET
      * KEY = claim number, identical to payload positions 1-12
           MOVE SPACES                 TO EVA-EV-KEY
           MOVE WS-DB-CLAIM-NUMBER     TO EVA-EV-KEY(1:12)
           MOVE WS-DEDBILL-PAYLOAD     TO EVA-EV-PAYLOAD
           CALL WS-EVT-PGM USING WS-EVT-CA
           MOVE EVA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Date service: WS-DATE-REQ, WS-DATE-1, WS-DATE-2 set by caller
       8700-CALL-DATES.
           INITIALIZE WS-DTE-CA
           MOVE WS-DATE-REQ            TO DTA-REQUEST-ID
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE WS-DATE-1              TO DTA-DT-DATE-1
           MOVE WS-DATE-2              TO DTA-DT-DATE-2
           CALL WS-DTE-PGM USING WS-DTE-CA
           MOVE DTA-HEADER             TO SVC-HEADER
           IF WS-DATE-REQ NOT = 'DTVALD  '
              PERFORM 8900-CHECK-CALLEE
           END-IF.

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
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Recovery data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMRECOV Db2 error SQLCODE ' WS-SQLCODE-DISP
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
              MOVE CLM-CLAIM-NUMBER    TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
