       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMEXPOS.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMEXPOS                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Exposure service. An exposure is one claimant's  *
      *               loss against one policy coverage on a claim and  *
      *               is the level at which reserves and payments are  *
      *               held. Creates exposures with zero reserve lines, *
      *               links the claimant, assigns the handler and      *
      *               raises the opening activities; maintains, closes *
      *               and reopens them.                                *
      * REQUESTS    : EXPNEW EXPINQ EXPUPD EXPCLS EXPROP               *
      * CALLED BY   : CLMCLAIM (FNOL), exposure screens, CLMFINTX (R)  *
      * CALLS       : CLMSEQGN CLMERRLG CLMHIST0 CLMAUDCH CLMACTV0     *
      *               CLMCONTC CLMASSGN CLMCODVL                       *
      * TABLES      : CLM_EXPOSURE (C R U) CLM_RESERVE_LINE (C R)      *
      *               CLM_CLAIM (R) CLM_POLICY_COVERAGE (R)            *
      *               CLM_INCIDENT (R) CLM_CONTACT (R)                 *
      *               CLM_TRANSACTION (R) CLM_CONTROL (R)              *
      * EXPOSURE    : VEHDAMAG BODINJUR PROPDAMA CONTENTS LOSSOFUS     *
      * TYPES         MEDPAY (CHAR(8): long names cut to 8 characters) *
      * RULES       : BR-CLM-EXP-001 Exposures are added, changed and  *
      *                              reopened only on OPEN claims      *
      *               BR-CLM-EXP-002 Exposure type must fit the        *
      *                              coverage type (VEHDAMAG COLL/COMP,*
      *                              BODINJUR LIAB_BI, PROPDAMA        *
      *                              LIAB_PD/DWELLING/BLDG, CONTENTS   *
      *                              CONTENTS, LOSSOFUS COLL/COMP/     *
      *                              BUSINT, MEDPAY MEDPAY)            *
      *               BR-CLM-EXP-003 Incident must be on the same claim*
      *                              and of a type that fits the       *
      *                              exposure type                     *
      *               BR-CLM-EXP-004 First-party coverage must cover   *
      *                              the damaged risk unit of the      *
      *                              incident                          *
      *               BR-CLM-EXP-005 Injury exposures: the claimant is *
      *                              the injured person on the incident*
      *               BR-CLM-EXP-006 One live exposure per claimant,   *
      *                              type and coverage on a claim      *
      *               BR-CLM-EXP-007 Exposure sequence is the next     *
      *                              number within the claim           *
      *               BR-CLM-EXP-008 New exposures open with zero      *
      *                              reserve lines for claim cost      *
      *                              (CLMC) and adjusting expense      *
      *                              (EXAO)                            *
      *               BR-CLM-EXP-009 Validation level NEWL becomes ABLE*
      *                              (ability to pay) when claimant,   *
      *                              coverage and incident are present *
      *               BR-CLM-EXP-010 Assignment failure leaves the     *
      *                              claim owner as handler (warning)  *
      *               BR-CLM-EXP-011 Coverage cannot change once any   *
      *                              financial transaction exists      *
      *               BR-CLM-EXP-012 With payments made the exposure   *
      *                              cannot drop below ABLE            *
      *               BR-CLM-EXP-013 Close needs zero open and pending *
      *                              reserve and no transaction pending*
      *                              approval                          *
      *               BR-CLM-EXP-014 Close reason PAYCMPLT needs at    *
      *                              least one payment                 *
      *               BR-CLM-EXP-015 Updates carry VERSION_NO          *
      *                              (optimistic lock)                 *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMEXPOS'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-CON-PGM                  PIC X(8)  VALUE 'CLMCONTC'.
       01  WS-ASG-PGM                  PIC X(8)  VALUE 'CLMASSGN'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.

       01  WS-CONSTANTS.
           05 WS-DEFAULT-CATEGORY      PIC X(8)  VALUE 'DEFAULT '.
           05 WS-COST-CLAIM            PIC X(4)  VALUE 'CLMC'.
           05 WS-COST-EXPENSE          PIC X(4)  VALUE 'EXAO'.

       01  WS-WORK.
           05 WS-EXP-TYPE              PIC X(8).
              88 WS-EXP-VEH-DAMAGE      VALUE 'VEHDAMAG'.
              88 WS-EXP-BODILY-INJURY   VALUE 'BODINJUR'.
              88 WS-EXP-PROP-DAMAGE     VALUE 'PROPDAMA'.
              88 WS-EXP-CONTENTS        VALUE 'CONTENTS'.
              88 WS-EXP-LOSS-OF-USE     VALUE 'LOSSOFUS'.
              88 WS-EXP-MED-PAY         VALUE 'MEDPAY  '.
              88 WS-EXP-TYPE-VALID      VALUE 'VEHDAMAG' 'BODINJUR'
                                              'PROPDAMA' 'CONTENTS'
                                              'LOSSOFUS' 'MEDPAY  '.
              88 WS-EXP-INJURY-TYPE     VALUE 'BODINJUR' 'MEDPAY  '.
           05 WS-COV-TYPE              PIC X(8).
              88 WS-COV-FIRST-PARTY     VALUE 'COLL    ' 'COMP    '
                                              'DWELLING' 'BLDG    '
                                              'CONTENTS' 'BUSINT  '.
           05 WS-COV-FITS              PIC X     VALUE 'N'.
              88 WS-COV-DOES-FIT        VALUE 'Y'.
           05 WS-INC-FITS              PIC X     VALUE 'N'.
              88 WS-INC-DOES-FIT        VALUE 'Y'.
           05 WS-OLD-CLAIMANT          PIC S9(9) COMP VALUE 0.
           05 WS-OLD-COV-SEQ           PIC S9(4) COMP VALUE 0.
           05 WS-OLD-LEVEL             PIC X(4).
           05 WS-NEW-LEVEL             PIC X(4).
           05 WS-NEXT-SEQ              PIC S9(4) COMP VALUE 0.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-PENDING-COUNT         PIC S9(9) COMP VALUE 0.
           05 WS-FIN-COUNT             PIC S9(9) COMP VALUE 0.
           05 WS-OPEN-RESERVE          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PENDING-RESERVE       PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-RL-COST-TYPE          PIC X(4).
           05 WS-ID-DISP               PIC 9(9).
           05 WS-CLMNT-DISP            PIC 9(9).
           05 WS-PAID-DISP             PIC -(11)9.99.
           05 WS-CLM-DISP              PIC 9(9).
           05 WS-SEQ-DISP              PIC 9(4).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-HIS-EVENT             PIC X(8).
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-AUD-EVENT             PIC X(8).
           05 WS-AUD-PAYLOAD           PIC X(400).
           05 WS-COD-TABLE             PIC X(8).
           05 WS-COD-VALUE             PIC X(10).
           05 WS-COD-REASON            PIC X(8).
           05 WS-WARN-REASON           PIC X(8)  VALUE SPACES.
           05 WS-WARN-MESSAGE          PIC X(80) VALUE SPACES.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDEXP END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDPCV END-EXEC.
           EXEC SQL INCLUDE CLMDINC END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDRSL END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

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
       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.
       01  WS-CON-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==COA-==.
           03 COA-BODY.
              COPY CLMCCON REPLACING LEADING ==CA-== BY ==COA-==.
       01  WS-ASG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ASA-==.
           03 ASA-BODY.
              COPY CLMCASG REPLACING LEADING ==CA-== BY ==ASA-==.
       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCEXP.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'EXPNEW  '
                 PERFORM 2000-NEW-EXPOSURE
              WHEN 'EXPINQ  '
                 PERFORM 2100-INQUIRE-EXPOSURE
              WHEN 'EXPUPD  '
                 PERFORM 2200-UPDATE-EXPOSURE
              WHEN 'EXPCLS  '
                 PERFORM 2300-CLOSE-EXPOSURE
              WHEN 'EXPROP  '
                 PERFORM 2400-REOPEN-EXPOSURE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMEXPOS: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          WS-WARN-REASON
                                          WS-WARN-MESSAGE
           IF CA-BUS-DATE = SPACES
              PERFORM 4900-READ-CONTROL
           END-IF.

      ******************************************************************
       2000-NEW-EXPOSURE.
           IF CA-EX-CLAIM-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMIDREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for a new exposure'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO CA-EX-EXPOSURE-ID
           PERFORM 3000-VALIDATE-CLAIM-OPEN
           PERFORM 3100-VALIDATE-TYPE
           PERFORM 3200-VALIDATE-COVERAGE
           PERFORM 3300-VALIDATE-CLAIMANT
           IF CA-EX-INCIDENT-ID > 0
              PERFORM 3400-VALIDATE-INCIDENT
           END-IF
           PERFORM 3500-CHECK-DUPLICATE
           PERFORM 3600-DERIVE-VALIDATION-LEVEL

           PERFORM 4500-NEXT-EXPOSURE-SEQ
           MOVE 'EXP'                  TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO CA-EX-EXPOSURE-ID
           PERFORM 4600-INSERT-EXPOSURE
           PERFORM 2010-CREATE-RESERVE-LINES

           PERFORM 8400-LINK-CLAIMANT
           PERFORM 8500-ASSIGN-EXPOSURE
           PERFORM 8600-GENERATE-ACTIVITIES

           MOVE CA-EX-EXPOSURE-ID      TO WS-ID-DISP
           MOVE CA-EX-SEQ              TO WS-SEQ-DISP
           MOVE 'EXPNEW  '             TO WS-HIS-EVENT
           STRING 'Exposure ' WS-SEQ-DISP ' (' WS-ID-DISP ') '
                  CA-EX-TYPE ' on ' CA-EX-COVERAGE-TYPE
                  ' opened, level ' CA-EX-VALIDATION-LEVEL
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           MOVE 'EXPNEW  '             TO WS-AUD-EVENT
           PERFORM 4400-READ-EXPOSURE
           PERFORM 5100-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT

           PERFORM 5000-FILL-RESPONSE
           IF WS-WARN-REASON NOT = SPACES
              SET CA-RC-WARNING        TO TRUE
              MOVE WS-WARN-REASON      TO CA-REASON-CODE
              MOVE WS-WARN-MESSAGE     TO CA-MESSAGE
           ELSE
              STRING 'Exposure ' WS-SEQ-DISP ' opened for '
                     CA-EX-ASSIGNED-USER DELIMITED BY SIZE
                     INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-008 Zero reserve lines for claim cost and expense
       2010-CREATE-RESERVE-LINES.
           MOVE WS-COST-CLAIM          TO WS-RL-COST-TYPE
           PERFORM 4610-INSERT-RESERVE-LINE
           MOVE WS-COST-EXPENSE        TO WS-RL-COST-TYPE
           PERFORM 4610-INSERT-RESERVE-LINE.

      ******************************************************************
       2100-INQUIRE-EXPOSURE.
           IF CA-EX-EXPOSURE-ID > 0
              PERFORM 4400-READ-EXPOSURE
           ELSE
              IF CA-EX-CLAIM-ID > 0 AND CA-EX-SEQ > 0
                 PERFORM 4410-READ-EXPOSURE-BY-SEQ
              ELSE
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'EXPKEY  '       TO CA-REASON-CODE
                 MOVE 'Exposure id or claim id + sequence required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           PERFORM 5000-FILL-RESPONSE.

      ******************************************************************
       2200-UPDATE-EXPOSURE.
           PERFORM 3700-LOAD-FOR-CHANGE
           IF EXP-EXPOSURE-STATE NOT = 'OPEN'
              AND EXP-EXPOSURE-STATE NOT = 'DRFT'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPSTATE'          TO CA-REASON-CODE
              STRING 'Exposure is ' EXP-EXPOSURE-STATE
                     ' - reopen it before changing it'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE EXP-CLAIM-ID           TO CA-EX-CLAIM-ID
           MOVE EXP-EXPOSURE-TYPE      TO CA-EX-TYPE
           MOVE EXP-CLAIMANT-ID        TO WS-OLD-CLAIMANT
           MOVE EXP-COV-SEQ            TO WS-OLD-COV-SEQ
           MOVE EXP-VALIDATION-LEVEL   TO WS-OLD-LEVEL
           PERFORM 3000-VALIDATE-CLAIM-OPEN
           PERFORM 3100-VALIDATE-TYPE
           PERFORM 4820-COUNT-FIN-TXNS
      * BR-CLM-EXP-011 Coverage is fixed once money has moved
           IF CA-EX-COV-SEQ NOT = WS-OLD-COV-SEQ
              AND WS-FIN-COUNT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'COVLOCKD'          TO CA-REASON-CODE
              MOVE 'Coverage cannot change after financial activity'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO CA-EX-COVERAGE-TYPE
           PERFORM 3200-VALIDATE-COVERAGE
           PERFORM 3300-VALIDATE-CLAIMANT
           IF CA-EX-INCIDENT-ID > 0
              PERFORM 3400-VALIDATE-INCIDENT
           END-IF
           IF CA-EX-CLAIMANT-ID NOT = WS-OLD-CLAIMANT
              OR CA-EX-COV-SEQ NOT = WS-OLD-COV-SEQ
              PERFORM 3500-CHECK-DUPLICATE
           END-IF
           PERFORM 3600-DERIVE-VALIDATION-LEVEL
      * BR-CLM-EXP-012 Paid exposures stay at ability-to-pay
           IF WS-OLD-LEVEL = 'ABLE'
              AND CA-EX-VALIDATION-LEVEL NOT = 'ABLE'
              AND EXP-TOTAL-PAID > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'LVLDROP '          TO CA-REASON-CODE
              MOVE 'Paid exposure must keep claimant, cover, incident'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           PERFORM 4700-UPDATE-EXPOSURE-DETAILS
           IF CA-EX-CLAIMANT-ID NOT = WS-OLD-CLAIMANT
              PERFORM 8400-LINK-CLAIMANT
           END-IF

           MOVE CA-EX-EXPOSURE-ID      TO WS-ID-DISP
           MOVE 'EXPUPD  '             TO WS-HIS-EVENT
           STRING 'Exposure ' WS-ID-DISP ' updated, level '
                  WS-OLD-LEVEL ' -> ' CA-EX-VALIDATION-LEVEL
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           PERFORM 4400-READ-EXPOSURE
           MOVE 'EXPUPD  '             TO WS-AUD-EVENT
           PERFORM 5100-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT
           PERFORM 5000-FILL-RESPONSE
           STRING 'Exposure ' WS-ID-DISP ' updated'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       2300-CLOSE-EXPOSURE.
           PERFORM 3700-LOAD-FOR-CHANGE
           IF EXP-EXPOSURE-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPSTATE'          TO CA-REASON-CODE
              STRING 'Only an OPEN exposure can be closed (state '
                     EXP-EXPOSURE-STATE ')'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-EX-CLOSE-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLSRSREQ'          TO CA-REASON-CODE
              MOVE 'A close reason is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'EXPCLRSN'             TO WS-COD-TABLE
           MOVE CA-EX-CLOSE-REASON     TO WS-COD-VALUE
           MOVE 'CLSRSN  '             TO WS-COD-REASON
           PERFORM 8300-VALIDATE-CODE
      * BR-CLM-EXP-014 Payments-complete needs a payment
           IF CA-EX-CLOSE-REASON = 'PAYCMPLT'
              AND EXP-TOTAL-PAID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOPAYMNT'          TO CA-REASON-CODE
              MOVE 'PAYCMPLT close reason but nothing has been paid'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3800-CHECK-NO-OPEN-FINANCIALS

           MOVE 'CLSD'                 TO EXP-EXPOSURE-STATE
           MOVE CA-EX-CLOSE-REASON     TO EXP-CLOSE-REASON
           MOVE CA-BUS-DATE            TO EXP-CLOSE-DATE
           MOVE 0                      TO EXP-CLOSE-DATE-IND
           PERFORM 4710-UPDATE-EXPOSURE-STATE

           MOVE CA-EX-EXPOSURE-ID      TO WS-ID-DISP
           MOVE EXP-TOTAL-PAID         TO WS-AMT-DISP
           MOVE 'EXPCLS  '             TO WS-HIS-EVENT
           STRING 'Exposure ' WS-ID-DISP ' closed ' CA-EX-CLOSE-REASON
                  ' total paid ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           PERFORM 4400-READ-EXPOSURE
           MOVE 'EXPCLS  '             TO WS-AUD-EVENT
           PERFORM 5100-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT
           PERFORM 5000-FILL-RESPONSE
           STRING 'Exposure ' WS-ID-DISP ' closed'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       2400-REOPEN-EXPOSURE.
           PERFORM 3700-LOAD-FOR-CHANGE
           IF EXP-EXPOSURE-STATE NOT = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPSTATE'          TO CA-REASON-CODE
              STRING 'Only a CLSD exposure can be reopened (state '
                     EXP-EXPOSURE-STATE ')'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE EXP-CLAIM-ID           TO CA-EX-CLAIM-ID
           PERFORM 3000-VALIDATE-CLAIM-OPEN
           IF CA-EX-CLOSE-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ROPRSREQ'          TO CA-REASON-CODE
              MOVE 'A reopen reason is required (CLOSE-REASON field)'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'EXPRORSN'             TO WS-COD-TABLE
           MOVE CA-EX-CLOSE-REASON     TO WS-COD-VALUE
           MOVE 'ROPRSN  '             TO WS-COD-REASON
           PERFORM 8300-VALIDATE-CODE

           MOVE 'OPEN'                 TO EXP-EXPOSURE-STATE
           MOVE SPACES                 TO EXP-CLOSE-REASON
           MOVE SPACES                 TO EXP-CLOSE-DATE
           MOVE -1                     TO EXP-CLOSE-DATE-IND
           PERFORM 4710-UPDATE-EXPOSURE-STATE

           MOVE CA-EX-EXPOSURE-ID      TO WS-ID-DISP
           MOVE 'EXPROP  '             TO WS-HIS-EVENT
           STRING 'Exposure ' WS-ID-DISP ' reopened, reason '
                  CA-EX-CLOSE-REASON
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           PERFORM 4400-READ-EXPOSURE
           MOVE 'EXPROP  '             TO WS-AUD-EVENT
           PERFORM 5100-BUILD-AUDIT-PAYLOAD
           PERFORM 8200-WRITE-AUDIT
           PERFORM 5000-FILL-RESPONSE
           STRING 'Exposure ' WS-ID-DISP ' reopened'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-CLM-EXP-001 Exposure changes need an OPEN claim
       3000-VALIDATE-CLAIM-OPEN.
           PERFORM 4000-READ-CLAIM
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMSTATE'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ' - exposures need an OPEN claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3100-VALIDATE-TYPE.
           MOVE CA-EX-TYPE             TO WS-EXP-TYPE
           IF NOT WS-EXP-TYPE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPTYPE '          TO CA-REASON-CODE
              STRING 'Exposure type ' CA-EX-TYPE ' is not supported'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-002 Exposure type must fit the coverage type
       3200-VALIDATE-COVERAGE.
           IF CA-EX-COV-SEQ <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'COVSEQRQ'          TO CA-REASON-CODE
              MOVE 'Coverage sequence is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-COVERAGE
           IF CA-EX-COVERAGE-TYPE NOT = SPACES
              AND CA-EX-COVERAGE-TYPE NOT = PCV-COVERAGE-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'COVMISMT'          TO CA-REASON-CODE
              STRING 'Coverage ' CA-EX-COVERAGE-TYPE
                     ' does not match policy coverage '
                     PCV-COVERAGE-TYPE
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE PCV-COVERAGE-TYPE      TO WS-COV-TYPE
                                          CA-EX-COVERAGE-TYPE
           MOVE 'N'                    TO WS-COV-FITS
           EVALUATE TRUE
              WHEN WS-EXP-VEH-DAMAGE
                 IF WS-COV-TYPE = 'COLL' OR 'COMP'
                    SET WS-COV-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-BODILY-INJURY
                 IF WS-COV-TYPE = 'LIAB_BI'
                    SET WS-COV-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-PROP-DAMAGE
                 IF WS-COV-TYPE = 'LIAB_PD' OR 'DWELLING' OR 'BLDG'
                    SET WS-COV-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-CONTENTS
                 IF WS-COV-TYPE = 'CONTENTS'
                    SET WS-COV-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-LOSS-OF-USE
                 IF WS-COV-TYPE = 'COLL' OR 'COMP' OR 'BUSINT'
                    SET WS-COV-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-MED-PAY
                 IF WS-COV-TYPE = 'MEDPAY'
                    SET WS-COV-DOES-FIT TO TRUE
                 END-IF
           END-EVALUATE
           IF NOT WS-COV-DOES-FIT
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'COVNOFIT'          TO CA-REASON-CODE
              STRING 'Coverage ' WS-COV-TYPE
                     ' cannot carry a ' CA-EX-TYPE ' exposure'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3300-VALIDATE-CLAIMANT.
           IF CA-EX-CLAIMANT-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMNTREQ'          TO CA-REASON-CODE
              MOVE 'Claimant contact id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-EX-CLAIMANT-ID      TO CON-CONTACT-ID
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
                 MOVE CA-EX-CLAIMANT-ID TO WS-ID-DISP
                 STRING 'Claimant contact ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3300-VALIDATE-CLAIMANT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-EXP-003 Incident on the same claim, type fits exposure
       3400-VALIDATE-INCIDENT.
           PERFORM 4300-READ-INCIDENT
           IF INC-CLAIM-ID NOT = CA-EX-CLAIM-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCOTHCL'          TO CA-REASON-CODE
              MOVE CA-EX-INCIDENT-ID   TO WS-ID-DISP
              STRING 'Incident ' WS-ID-DISP
                     ' belongs to a different claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'N'                    TO WS-INC-FITS
           EVALUATE TRUE
              WHEN WS-EXP-VEH-DAMAGE
                 IF INC-INCIDENT-TYPE = 'VEH'
                    SET WS-INC-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-INJURY-TYPE
                 IF INC-INCIDENT-TYPE = 'INJ'
                    SET WS-INC-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-PROP-DAMAGE
                 IF INC-INCIDENT-TYPE = 'PROP' OR 'FIXP'
                    SET WS-INC-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-CONTENTS
                 IF INC-INCIDENT-TYPE = 'PROP'
                    SET WS-INC-DOES-FIT TO TRUE
                 END-IF
              WHEN WS-EXP-LOSS-OF-USE
                 IF INC-INCIDENT-TYPE = 'VEH' OR 'PROP'
                    SET WS-INC-DOES-FIT TO TRUE
                 END-IF
           END-EVALUATE
           IF NOT WS-INC-DOES-FIT
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INCNOFIT'          TO CA-REASON-CODE
              STRING 'A ' INC-INCIDENT-TYPE
                     ' incident cannot support a '
                     CA-EX-TYPE ' exposure'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-EXP-004 First-party cover must insure the damaged unit
           IF WS-COV-FIRST-PARTY
              AND NOT WS-EXP-INJURY-TYPE
              AND INC-RISK-UNIT-SEQ NOT = PCV-RISK-UNIT-SEQ
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RSKMISMT'          TO CA-REASON-CODE
              MOVE INC-RISK-UNIT-SEQ   TO WS-SEQ-DISP
              STRING 'Coverage ' WS-COV-TYPE
                     ' does not insure the incident risk unit '
                     WS-SEQ-DISP DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-EXP-005 Injury exposures belong to the injured person
           IF WS-EXP-INJURY-TYPE
              AND INC-INJURED-CONTACT-ID NOT = CA-EX-CLAIMANT-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOTINJRD'          TO CA-REASON-CODE
              MOVE 'Claimant is not the injured person on the incident'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-006 One live exposure per claimant/type/coverage
       3500-CHECK-DUPLICATE.
           MOVE CA-EX-CLAIM-ID         TO EXP-CLAIM-ID
           MOVE CA-EX-CLAIMANT-ID      TO EXP-CLAIMANT-ID
           MOVE CA-EX-TYPE             TO EXP-EXPOSURE-TYPE
           MOVE CA-EX-COV-SEQ          TO EXP-COV-SEQ
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_EXPOSURE
               WHERE CLAIM_ID       = :EXP-CLAIM-ID
                 AND CLAIMANT_ID    = :EXP-CLAIMANT-ID
                 AND EXPOSURE_TYPE  = :EXP-EXPOSURE-TYPE
                 AND COV_SEQ        = :EXP-COV-SEQ
                 AND EXPOSURE_STATE <> 'CLSD'
                 AND EXPOSURE_ID    <> :CA-EX-EXPOSURE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3500-CHECK-DUPLICATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'EXPDUP  '          TO CA-REASON-CODE
              MOVE 'Claimant already has this exposure on the coverage'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-009 NEWL -> ABLE when claimant, coverage, incident
       3600-DERIVE-VALIDATION-LEVEL.
           IF CA-EX-CLAIMANT-ID > 0
              AND CA-EX-COV-SEQ > 0
              AND CA-EX-INCIDENT-ID > 0
              MOVE 'ABLE'              TO CA-EX-VALIDATION-LEVEL
           ELSE
              MOVE 'NEWL'              TO CA-EX-VALIDATION-LEVEL
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-015 Changes must quote the current VERSION_NO
       3700-LOAD-FOR-CHANGE.
           IF CA-EX-EXPOSURE-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPIDREQ'          TO CA-REASON-CODE
              MOVE 'Exposure id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4400-READ-EXPOSURE
           IF CA-EX-VERSION-NO NOT = EXP-VERSION-NO
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Exposure changed by another user - refresh'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-013 Nothing open or pending may remain at close
       3800-CHECK-NO-OPEN-FINANCIALS.
           MOVE CA-EX-EXPOSURE-ID      TO RSL-EXPOSURE-ID
           EXEC SQL
              SELECT COALESCE(SUM(OPEN_RESERVE), 0),
                     COALESCE(SUM(PENDING_APPROVAL), 0)
                INTO :WS-OPEN-RESERVE, :WS-PENDING-RESERVE
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE EXPOSURE_ID = :RSL-EXPOSURE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3800-CHECK-NO-OPEN-FINANCIALS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-OPEN-RESERVE NOT = 0 OR WS-PENDING-RESERVE NOT = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'OPENRSV '          TO CA-REASON-CODE
              MOVE WS-OPEN-RESERVE     TO WS-AMT-DISP
              STRING 'Open reserve ' WS-AMT-DISP
                     ' must be zero (and none pending) to close'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-PENDING-COUNT
                FROM CLMDB.CLM_TRANSACTION
               WHERE EXPOSURE_ID = :RSL-EXPOSURE-ID
                 AND TXN_STATUS  = 'PAPR'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3800-CHECK-NO-OPEN-FINANCIALS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-PENDING-COUNT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'PENDTXN '          TO CA-REASON-CODE
              MOVE WS-PENDING-COUNT    TO WS-ID-DISP
              STRING WS-ID-DISP ' transaction(s) await approval'
                     ' - approve or reject before closing'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-CLAIM.
           MOVE CA-EX-CLAIM-ID         TO CLM-CLAIM-ID
           EXEC SQL
              SELECT CLAIM_NUMBER, CLAIM_STATE, SEGMENT_CODE,
                     ASSIGNED_USER
                INTO :CLM-CLAIM-NUMBER, :CLM-CLAIM-STATE,
                     :CLM-SEGMENT-CODE, :CLM-ASSIGNED-USER
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-EX-CLAIM-ID   TO WS-ID-DISP
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
       4100-READ-COVERAGE.
           MOVE CA-EX-CLAIM-ID         TO PCV-CLAIM-ID
           MOVE CA-EX-COV-SEQ          TO PCV-COV-SEQ
           EXEC SQL
              SELECT COVERAGE_TYPE, RISK_UNIT_SEQ, LIMIT_AMOUNT,
                     DEDUCTIBLE_AMOUNT
                INTO :PCV-COVERAGE-TYPE, :PCV-RISK-UNIT-SEQ,
                     :PCV-LIMIT-AMOUNT, :PCV-DEDUCTIBLE-AMOUNT
                FROM CLMDB.CLM_POLICY_COVERAGE
               WHERE CLAIM_ID = :PCV-CLAIM-ID
                 AND COV_SEQ  = :PCV-COV-SEQ
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'COVNOTFD'       TO CA-REASON-CODE
                 MOVE CA-EX-COV-SEQ    TO WS-SEQ-DISP
                 STRING 'Coverage ' WS-SEQ-DISP
                        ' is not on the claim policy snapshot'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-COVERAGE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-READ-INCIDENT.
           MOVE CA-EX-INCIDENT-ID      TO INC-INCIDENT-ID
           EXEC SQL
              SELECT CLAIM_ID, INCIDENT_TYPE, RISK_UNIT_SEQ,
                     INJURED_CONTACT_ID
                INTO :INC-CLAIM-ID, :INC-INCIDENT-TYPE,
                     :INC-RISK-UNIT-SEQ, :INC-INJURED-CONTACT-ID
                FROM CLMDB.CLM_INCIDENT
               WHERE INCIDENT_ID = :INC-INCIDENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INCNOTFD'       TO CA-REASON-CODE
                 MOVE CA-EX-INCIDENT-ID TO WS-ID-DISP
                 STRING 'Incident ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4300-READ-INCIDENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-READ-EXPOSURE.
           MOVE CA-EX-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           EXEC SQL
              SELECT EXPOSURE_ID, CLAIM_ID, EXPOSURE_SEQ,
                     EXPOSURE_TYPE, COVERAGE_TYPE, COV_SEQ,
                     CLAIMANT_ID, INCIDENT_ID, EXPOSURE_STATE,
                     VALIDATION_LEVEL, SEGMENT_CODE, ASSIGNED_USER,
                     OPEN_RESERVE, TOTAL_PAID, TOTAL_RECOVERED,
                     DEDUCTIBLE_APPLIED, CREATE_DATE, CLOSE_DATE,
                     CLOSE_REASON, VERSION_NO
                INTO :EXP-EXPOSURE-ID, :EXP-CLAIM-ID,
                     :EXP-EXPOSURE-SEQ, :EXP-EXPOSURE-TYPE,
                     :EXP-COVERAGE-TYPE, :EXP-COV-SEQ,
                     :EXP-CLAIMANT-ID, :EXP-INCIDENT-ID,
                     :EXP-EXPOSURE-STATE, :EXP-VALIDATION-LEVEL,
                     :EXP-SEGMENT-CODE, :EXP-ASSIGNED-USER,
                     :EXP-OPEN-RESERVE, :EXP-TOTAL-PAID,
                     :EXP-TOTAL-RECOVERED, :EXP-DEDUCTIBLE-APPLIED,
                     :EXP-CREATE-DATE,
                     :EXP-CLOSE-DATE :EXP-CLOSE-DATE-IND,
                     :EXP-CLOSE-REASON, :EXP-VERSION-NO
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
           END-EXEC
           PERFORM 4405-CHECK-EXPOSURE-READ.

      ******************************************************************
       4405-CHECK-EXPOSURE-READ.
           EVALUATE SQLCODE
              WHEN 0
                 MOVE EXP-EXPOSURE-ID  TO CA-EX-EXPOSURE-ID
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EXPNOTFD'       TO CA-REASON-CODE
                 MOVE 'Exposure not found'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4400-READ-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4410-READ-EXPOSURE-BY-SEQ.
           MOVE CA-EX-CLAIM-ID         TO EXP-CLAIM-ID
           MOVE CA-EX-SEQ              TO EXP-EXPOSURE-SEQ
           EXEC SQL
              SELECT EXPOSURE_ID
                INTO :EXP-EXPOSURE-ID
                FROM CLMDB.CLM_EXPOSURE
               WHERE CLAIM_ID     = :EXP-CLAIM-ID
                 AND EXPOSURE_SEQ = :EXP-EXPOSURE-SEQ
           END-EXEC
           PERFORM 4405-CHECK-EXPOSURE-READ
           PERFORM 4400-READ-EXPOSURE.

      ******************************************************************
      * BR-CLM-EXP-007 Business sequence within the claim (the unique
      * index XEXP01 rejects a concurrent duplicate with -803)
       4500-NEXT-EXPOSURE-SEQ.
           MOVE CA-EX-CLAIM-ID         TO EXP-CLAIM-ID
           EXEC SQL
              SELECT COALESCE(MAX(EXPOSURE_SEQ), 0) + 1
                INTO :WS-NEXT-SEQ
                FROM CLMDB.CLM_EXPOSURE
               WHERE CLAIM_ID = :EXP-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-NEXT-EXPOSURE-SEQ' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-NEXT-SEQ            TO CA-EX-SEQ.

      ******************************************************************
       4600-INSERT-EXPOSURE.
           MOVE CA-EX-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           MOVE CA-EX-CLAIM-ID         TO EXP-CLAIM-ID
           MOVE CA-EX-SEQ              TO EXP-EXPOSURE-SEQ
           MOVE CA-EX-TYPE             TO EXP-EXPOSURE-TYPE
           MOVE CA-EX-COVERAGE-TYPE    TO EXP-COVERAGE-TYPE
           MOVE CA-EX-COV-SEQ          TO EXP-COV-SEQ
           MOVE CA-EX-CLAIMANT-ID      TO EXP-CLAIMANT-ID
           MOVE CA-EX-INCIDENT-ID      TO EXP-INCIDENT-ID
           MOVE 'OPEN'                 TO EXP-EXPOSURE-STATE
           MOVE CA-EX-VALIDATION-LEVEL TO EXP-VALIDATION-LEVEL
           MOVE CLM-SEGMENT-CODE       TO EXP-SEGMENT-CODE
           MOVE CLM-ASSIGNED-USER      TO EXP-ASSIGNED-USER
           MOVE CA-BUS-DATE            TO EXP-CREATE-DATE
           EXEC SQL
              INSERT INTO CLMDB.CLM_EXPOSURE
                    (EXPOSURE_ID, CLAIM_ID, EXPOSURE_SEQ,
                     EXPOSURE_TYPE, COVERAGE_TYPE, COV_SEQ,
                     CLAIMANT_ID, INCIDENT_ID, EXPOSURE_STATE,
                     VALIDATION_LEVEL, SEGMENT_CODE, ASSIGNED_USER,
                     OPEN_RESERVE, TOTAL_PAID, TOTAL_RECOVERED,
                     DEDUCTIBLE_APPLIED, CREATE_DATE, CLOSE_DATE,
                     CLOSE_REASON, UPDATE_TS, VERSION_NO)
              VALUES (:EXP-EXPOSURE-ID, :EXP-CLAIM-ID,
                     :EXP-EXPOSURE-SEQ, :EXP-EXPOSURE-TYPE,
                     :EXP-COVERAGE-TYPE, :EXP-COV-SEQ,
                     :EXP-CLAIMANT-ID, :EXP-INCIDENT-ID,
                     :EXP-EXPOSURE-STATE, :EXP-VALIDATION-LEVEL,
                     :EXP-SEGMENT-CODE, :EXP-ASSIGNED-USER,
                     0, 0, 0, 0, :EXP-CREATE-DATE, NULL,
                     ' ', CURRENT TIMESTAMP, 1)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'EXPSEQDP'       TO CA-REASON-CODE
                 MOVE 'Exposure sequence taken concurrently - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4600-INSERT-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4610-INSERT-RESERVE-LINE.
           MOVE 'RSL'                  TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO RSL-RL-ID
           MOVE CA-EX-CLAIM-ID         TO RSL-CLAIM-ID
           MOVE CA-EX-EXPOSURE-ID      TO RSL-EXPOSURE-ID
           MOVE WS-RL-COST-TYPE        TO RSL-COST-TYPE
           MOVE WS-DEFAULT-CATEGORY    TO RSL-COST-CATEGORY
           EXEC SQL
              INSERT INTO CLMDB.CLM_RESERVE_LINE
                    (RL_ID, CLAIM_ID, EXPOSURE_ID, COST_TYPE,
                     COST_CATEGORY, OPEN_RESERVE, PENDING_APPROVAL,
                     TOTAL_PAID, RECOVERY_RESERVE, TOTAL_RECOVERED,
                     UPDATE_TS)
              VALUES (:RSL-RL-ID, :RSL-CLAIM-ID, :RSL-EXPOSURE-ID,
                     :RSL-COST-TYPE, :RSL-COST-CATEGORY,
                     0, 0, 0, 0, 0, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'RSLDUP  '       TO CA-REASON-CODE
                 STRING 'Reserve line ' WS-RL-COST-TYPE
                        ' already exists for the exposure'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4610-INSERT-RESERVE-LINE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4700-UPDATE-EXPOSURE-DETAILS.
           MOVE CA-EX-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           MOVE CA-EX-COVERAGE-TYPE    TO EXP-COVERAGE-TYPE
           MOVE CA-EX-COV-SEQ          TO EXP-COV-SEQ
           MOVE CA-EX-CLAIMANT-ID      TO EXP-CLAIMANT-ID
           MOVE CA-EX-INCIDENT-ID      TO EXP-INCIDENT-ID
           MOVE CA-EX-VALIDATION-LEVEL TO EXP-VALIDATION-LEVEL
           MOVE CA-EX-VERSION-NO       TO EXP-VERSION-NO
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET COVERAGE_TYPE    = :EXP-COVERAGE-TYPE,
                     COV_SEQ          = :EXP-COV-SEQ,
                     CLAIMANT_ID      = :EXP-CLAIMANT-ID,
                     INCIDENT_ID      = :EXP-INCIDENT-ID,
                     VALIDATION_LEVEL = :EXP-VALIDATION-LEVEL,
                     UPDATE_TS        = CURRENT TIMESTAMP,
                     VERSION_NO       = VERSION_NO + 1
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
                 AND VERSION_NO  = :EXP-VERSION-NO
           END-EXEC
           PERFORM 4790-CHECK-VERSIONED-UPDATE.

      ******************************************************************
       4710-UPDATE-EXPOSURE-STATE.
           MOVE CA-EX-VERSION-NO       TO EXP-VERSION-NO
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET EXPOSURE_STATE = :EXP-EXPOSURE-STATE,
                     CLOSE_REASON   = :EXP-CLOSE-REASON,
                     CLOSE_DATE     = :EXP-CLOSE-DATE
                                      :EXP-CLOSE-DATE-IND,
                     UPDATE_TS      = CURRENT TIMESTAMP,
                     VERSION_NO     = VERSION_NO + 1
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
                 AND VERSION_NO  = :EXP-VERSION-NO
           END-EXEC
           PERFORM 4790-CHECK-VERSIONED-UPDATE.

      ******************************************************************
       4720-UPDATE-ASSIGNED-USER.
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET ASSIGNED_USER = :EXP-ASSIGNED-USER,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE EXPOSURE_ID   = :EXP-EXPOSURE-ID
                 AND ASSIGNED_USER <> :EXP-ASSIGNED-USER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4720-UPDATE-ASSIGNED-USER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4790-CHECK-VERSIONED-UPDATE.
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'STALEVER'    TO CA-REASON-CODE
                    MOVE 'Exposure changed by another user - refresh'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Exposure changed by another user - refresh'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4790-CHECK-VERSIONED-UPDATE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4820-COUNT-FIN-TXNS.
           MOVE CA-EX-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-FIN-COUNT
                FROM CLMDB.CLM_TRANSACTION
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
                 AND TXN_STATUS IN ('SUBM', 'PAPR')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4820-COUNT-FIN-TXNS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4900-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO CA-BUS-DATE.

      ******************************************************************
       5000-FILL-RESPONSE.
           MOVE EXP-EXPOSURE-ID        TO CA-EX-EXPOSURE-ID
           MOVE EXP-CLAIM-ID           TO CA-EX-CLAIM-ID
           MOVE EXP-EXPOSURE-SEQ       TO CA-EX-SEQ
           MOVE EXP-EXPOSURE-TYPE      TO CA-EX-TYPE
           MOVE EXP-COVERAGE-TYPE      TO CA-EX-COVERAGE-TYPE
           MOVE EXP-COV-SEQ            TO CA-EX-COV-SEQ
           MOVE EXP-CLAIMANT-ID        TO CA-EX-CLAIMANT-ID
           MOVE EXP-INCIDENT-ID        TO CA-EX-INCIDENT-ID
           MOVE EXP-EXPOSURE-STATE     TO CA-EX-STATE
           MOVE EXP-VALIDATION-LEVEL   TO CA-EX-VALIDATION-LEVEL
           MOVE EXP-SEGMENT-CODE       TO CA-EX-SEGMENT
           MOVE EXP-ASSIGNED-USER      TO CA-EX-ASSIGNED-USER
           MOVE EXP-OPEN-RESERVE       TO CA-EX-OPEN-RESERVE
           MOVE EXP-TOTAL-PAID         TO CA-EX-TOTAL-PAID
           MOVE EXP-TOTAL-RECOVERED    TO CA-EX-TOTAL-RECOVERED
           MOVE EXP-DEDUCTIBLE-APPLIED TO CA-EX-DEDUCTIBLE-APPLIED
           MOVE EXP-CLOSE-REASON       TO CA-EX-CLOSE-REASON
           MOVE EXP-VERSION-NO         TO CA-EX-VERSION-NO
           MOVE EXP-COV-SEQ            TO CA-EX-COV-SEQ
           PERFORM 4100-READ-COVERAGE
           MOVE PCV-LIMIT-AMOUNT       TO CA-EX-COV-LIMIT
           MOVE PCV-DEDUCTIBLE-AMOUNT  TO CA-EX-COV-DEDUCTIBLE.

      ******************************************************************
       5100-BUILD-AUDIT-PAYLOAD.
           MOVE EXP-EXPOSURE-ID        TO WS-ID-DISP
           MOVE EXP-CLAIM-ID           TO WS-CLM-DISP
           MOVE EXP-OPEN-RESERVE       TO WS-AMT-DISP
           MOVE EXP-CLAIMANT-ID        TO WS-CLMNT-DISP
           MOVE EXP-TOTAL-PAID         TO WS-PAID-DISP
           MOVE SPACES                 TO WS-AUD-PAYLOAD
           STRING 'EXP=' WS-ID-DISP ' CLM=' WS-CLM-DISP
                  ' TYPE=' EXP-EXPOSURE-TYPE
                  ' COV=' EXP-COVERAGE-TYPE
                  ' STATE=' EXP-EXPOSURE-STATE
                  ' LVL=' EXP-VALIDATION-LEVEL
                  ' RSN=' EXP-CLOSE-REASON
                  ' USER=' CA-USER-ID
                  ' RSV=' WS-AMT-DISP
                  ' CLMNT=' WS-CLMNT-DISP
                  ' PAID=' WS-PAID-DISP
                  DELIMITED BY SIZE INTO WS-AUD-PAYLOAD.

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
           MOVE CA-EX-CLAIM-ID         TO HIA-HI-CLAIM-ID
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
           MOVE CA-EX-CLAIM-ID         TO AUA-AC-CLAIM-ID
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
       8400-LINK-CLAIMANT.
           INITIALIZE WS-CON-CA
           MOVE 'CONLNK  '             TO COA-REQUEST-ID
           MOVE CA-USER-ID             TO COA-USER-ID
           MOVE CA-BUS-DATE            TO COA-BUS-DATE
           MOVE CA-TRACE-ID            TO COA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO COA-CALLER-PGM
           MOVE CA-EX-CLAIMANT-ID      TO COA-CO-CONTACT-ID
           MOVE CA-EX-CLAIM-ID         TO COA-CO-CLAIM-ID
           MOVE 'CLAIMANT'             TO COA-CO-ROLE
           MOVE CA-EX-EXPOSURE-ID      TO COA-CO-EXPOSURE-ID
           CALL WS-CON-PGM USING WS-CON-CA
           IF COA-RETURN-CODE >= 12
              AND NOT COA-RC-CONFLICT
              MOVE COA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE COA-REASON-CODE     TO CA-REASON-CODE
              MOVE COA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-EXP-010 Failed assignment keeps the claim owner
       8500-ASSIGN-EXPOSURE.
           INITIALIZE WS-ASG-CA
           MOVE 'ASGEXP  '             TO ASA-REQUEST-ID
           MOVE CA-USER-ID             TO ASA-USER-ID
           MOVE CA-BUS-DATE            TO ASA-BUS-DATE
           MOVE CA-TRACE-ID            TO ASA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ASA-CALLER-PGM
           MOVE CA-EX-CLAIM-ID         TO ASA-AS-CLAIM-ID
           MOVE CA-EX-EXPOSURE-ID      TO ASA-AS-EXPOSURE-ID
           MOVE CLM-SEGMENT-CODE       TO ASA-AS-SEGMENT
           CALL WS-ASG-PGM USING WS-ASG-CA
           IF ASA-RETURN-CODE >= 90
              MOVE ASA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE ASA-REASON-CODE     TO CA-REASON-CODE
              MOVE ASA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF ASA-RETURN-CODE >= 8 OR ASA-AS-ASSIGNED-USER = SPACES
              MOVE 'ASGFALLB'          TO WS-WARN-REASON
              STRING 'Exposure opened; assignment failed ('
                     ASA-REASON-CODE ') - left with '
                     CLM-ASSIGNED-USER
                     DELIMITED BY SIZE INTO WS-WARN-MESSAGE
              MOVE CLM-ASSIGNED-USER   TO CA-EX-ASSIGNED-USER
           ELSE
              MOVE ASA-AS-ASSIGNED-USER TO CA-EX-ASSIGNED-USER
              MOVE CA-EX-EXPOSURE-ID   TO EXP-EXPOSURE-ID
              MOVE ASA-AS-ASSIGNED-USER TO EXP-ASSIGNED-USER
              PERFORM 4720-UPDATE-ASSIGNED-USER
           END-IF.

      ******************************************************************
       8600-GENERATE-ACTIVITIES.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTGEN  '             TO AVA-REQUEST-ID
           MOVE CA-USER-ID             TO AVA-USER-ID
           MOVE CA-BUS-DATE            TO AVA-BUS-DATE
           MOVE CA-TRACE-ID            TO AVA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AVA-CALLER-PGM
           MOVE CA-EX-CLAIM-ID         TO AVA-AV-CLAIM-ID
           MOVE CA-EX-EXPOSURE-ID      TO AVA-AV-EXPOSURE-ID
           MOVE 'EXPOPEN '             TO AVA-AV-EVENT
           MOVE CA-EX-ASSIGNED-USER    TO AVA-AV-ASSIGNED-USER
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF AVA-RETURN-CODE >= 12
              MOVE AVA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AVA-REASON-CODE     TO CA-REASON-CODE
              MOVE AVA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9800-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Exposure or claim row locked - retry'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'CLMEXPOS Db2 error SQLCODE ' WS-SQLCODE-DISP
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
           MOVE CA-EX-EXPOSURE-ID      TO WS-ID-DISP
           STRING 'EXP ' WS-ID-DISP DELIMITED BY SIZE
                  INTO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
