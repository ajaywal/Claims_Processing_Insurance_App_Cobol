       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMFNLP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMFNLP                                          *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * TRANSACTION : CFNL                                             *
      * MAP         : CLMFNM in mapset CLMMSET                         *
      * COMMAREA    : CLMMNU01 navigation + CLMFNL01 FNOL state        *
      * FUNCTION    : First notice of loss in two steps.               *
      *               ENTER  validates the loss details on the screen, *
      *                      verifies the policy (CLMPOLVR POLVRF) and *
      *                      shows insured, period, coverages and      *
      *                      billing status.                           *
      *               PF5    creates the claim (CLMCLAIM CLMNEW) from  *
      *                      the verified entry and shows the claim    *
      *                      number, segment and assigned adjuster.    *
      *               PF4    inquiry on the new claim (XCTL CLMINQP)   *
      *               PF12   cancel / start a new FNOL; PF3 menu.      *
      * CALLED BY   : terminal user (CFNL), XCTL from CLMMNUP          *
      * CALLS       : LINK CLMPOLVR (POLVRF)  LINK CLMCLAIM (CLMNEW)   *
      *               LINK CLMCODVL           LINK CLMDATES (DTVALD,   *
      *               DTDIFF)                 LINK CLMERRLG (ERRLOG)   *
      *               XCTL CLMMNUP CLMINQP                             *
      * TABLES      : none (all data access through the services)      *
      * RULES       : BR-CLM-SFN-001 Policy number numeric 1-999999999 *
      *               BR-CLM-SFN-002 Loss date valid and not future    *
      *               BR-CLM-SFN-003 Loss older than 3 years refused   *
      *                              (outside reporting limitation)    *
      *               BR-CLM-SFN-004 Loss reported after 30 days is    *
      *                              accepted with a late warning      *
      *               BR-CLM-SFN-005 Loss time HH:MM, not in the       *
      *                              future when the loss is today     *
      *               BR-CLM-SFN-006 Loss type, cause, how reported    *
      *                              and relation are code-table values*
      *               BR-CLM-SFN-007 Description at least 10 chars     *
      *               BR-CLM-SFN-008 UK postcode shape when entered    *
      *               BR-CLM-SFN-009 Reporter surname, phone and       *
      *                              relation required                 *
      *               BR-CLM-SFN-010 Phone has 10-15 digits            *
      *               BR-CLM-SFN-011 Policy not in force at loss needs *
      *                              the coverage override and sets    *
      *                              coverage-in-question              *
      *               BR-CLM-SFN-012 Billing DLNQ/CANC sets coverage-  *
      *                              in-question; PDUE warns only      *
      *               BR-CLM-SFN-013 Home/commercial losses need the   *
      *                              loss postcode                     *
      *               BR-CLM-SFN-014 Policy without coverages sets     *
      *                              coverage-in-question              *
      *               BR-CLM-SFN-015 PF5 only for exactly the entry    *
      *                              that was verified                 *
      *               BR-CLM-SFN-016 Reported date is today; how       *
      *                              reported defaults to PHON         *
      *               BR-CLM-SFN-017 Service RC >= 90 backs out the    *
      *                              unit of work                      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMFNLP'.
       01  WS-TRANID                   PIC X(4)  VALUE 'CFNL'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CLMMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'CLMFNM'.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CLMMNUP'.
       01  WS-INQ-PGM                  PIC X(8)  VALUE 'CLMINQP'.
       01  WS-POL-PGM                  PIC X(8)  VALUE 'CLMPOLVR'.
       01  WS-CLM-PGM                  PIC X(8)  VALUE 'CLMCLAIM'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-LIMITS.
           05 WS-MAX-LOSS-AGE-DAYS     PIC S9(7) COMP-3 VALUE 1095.
           05 WS-LATE-REPORT-DAYS      PIC S9(7) COMP-3 VALUE 30.
           05 WS-MIN-DESC-CHARS        PIC S9(4) COMP   VALUE 10.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP VALUE 0.
           05 WS-RESP2                 PIC S9(8) COMP VALUE 0.
           05 WS-RESP-DISP             PIC 9(4).
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-NOW-TIME              PIC X(8).
           05 WS-USERID                PIC X(8).
           05 WS-TASKN                 PIC 9(8).
           05 WS-TRACE-ID              PIC X(16).
           05 WS-SEND-MODE             PIC X.
              88 WS-SEND-ERASE          VALUE 'E'.
              88 WS-SEND-DATAONLY       VALUE 'D'.
           05 WS-ERROR-FLAG            PIC X.
              88 WS-NO-ERROR            VALUE 'N'.
              88 WS-HAS-ERROR           VALUE 'Y'.
           05 WS-LINK-FLAG             PIC X.
              88 WS-LINK-OK             VALUE 'Y'.
              88 WS-LINK-FAILED         VALUE 'N'.
           05 WS-LINK-PGM              PIC X(8).
           05 WS-ERR-FIELD             PIC 9(2).
           05 WS-ERR-TEXT              PIC X(79).
           05 WS-WARN-TEXT             PIC X(79).
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-JX                    PIC S9(4) COMP.
           05 WS-COUNT                 PIC S9(4) COMP.
           05 WS-LEN                   PIC S9(4) COMP.
           05 WS-PTR                   PIC S9(4) COMP.
           05 WS-DIGITS                PIC S9(4) COMP.
           05 WS-CHAR                  PIC X.
           05 WS-LOWER                 PIC X(26)
                 VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 WS-UPPER                 PIC X(26)
                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05 WS-CODE-TABLE            PIC X(8).
           05 WS-CODE-VALUE            PIC X(10).
           05 WS-CODE-FLAG             PIC X.
              88 WS-CODE-VALID          VALUE 'Y'.
           05 WS-DATE-FLAG             PIC X.
              88 WS-DATE-VALID          VALUE 'Y'.
           05 WS-DAYS                  PIC S9(7) COMP-3.
           05 WS-DAYS-DISP             PIC ZZZ9.
           05 WS-HH                    PIC X(2).
           05 WS-MM                    PIC X(2).
           05 WS-DESC-WORK             PIC X(64) OCCURS 3 TIMES.
           05 WS-LOSS-DESC             PIC X(250).
           05 WS-POSTCODE              PIC X(8).
           05 WS-COV-LINE              PIC X(64).
           05 WS-COV-TEXT              PIC X(32).
           05 WS-AMT-EDIT              PIC Z,ZZZ,ZZ9.
           05 WS-SEV-EDIT              PIC ZZ9.
           05 WS-SIU-EDIT              PIC ZZ9.
           05 WS-MORE-EDIT             PIC Z9.

      * Integer parser work area (5100-PARSE-INTEGER)
       01  WS-NUM-PARSE.
           05 WS-NUM-TEXT              PIC X(15).
           05 WS-NUM-VALUE             PIC 9(15).
           05 WS-NUM-FLAG              PIC X.
              88 WS-NUM-VALID           VALUE 'Y'.
           05 WS-NUM-STATE             PIC X.
              88 WS-NUM-LEADING         VALUE 'L'.
              88 WS-NUM-IN-DIGITS       VALUE 'D'.
              88 WS-NUM-TRAILING        VALUE 'T'.

      * Screen image built on every ENTER / PF5 (same layout as state)
       01  WS-SCREEN-STATE.
           03 FILLER.
              COPY CLMFNL01 REPLACING LEADING ==FN-== BY ==WE-==.

       01  WS-COMMAREA.
           03 WS-CA-NAV.
              COPY CLMMNU01.
           03 WS-CA-STATE.
              COPY CLMFNL01.

       01  WS-POL-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==PVA-==.
           03 PVA-BODY.
              COPY CLMCPOL REPLACING LEADING ==CA-== BY ==PVA-==.
       01  WS-CLM-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CLA-==.
           03 CLA-BODY.
              COPY CLMCCLM REPLACING LEADING ==CA-== BY ==CLA-==.
       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

           COPY CLMMSET.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(4000).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE TRUE
              WHEN EIBCALEN = 0
              WHEN CN-OWNER-PGM NOT = WS-PROGRAM-ID
                 PERFORM 2000-NEW-FNOL
              WHEN EIBAID = DFHPF3
                 PERFORM 2800-RETURN-TO-MENU
              WHEN EIBAID = DFHPF12
              WHEN EIBAID = DFHCLEAR
                 PERFORM 2700-CANCEL
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-VERIFY
              WHEN EIBAID = DFHPF5
                 PERFORM 2300-SUBMIT
              WHEN EIBAID = DFHPF4 AND FN-STEP-CREATED
                 PERFORM 2850-INQUIRE-NEW-CLAIM
              WHEN OTHER
                 MOVE LOW-VALUES       TO CLMFNMO
                 MOVE 'Key not active on this screen' TO WS-ERR-TEXT
                 PERFORM 6300-SEND-ERROR
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           INITIALIZE WS-COMMAREA
           EVALUATE TRUE
              WHEN EIBCALEN = LENGTH OF WS-COMMAREA
                 MOVE DFHCOMMAREA(1:EIBCALEN) TO WS-COMMAREA
              WHEN EIBCALEN >= LENGTH OF WS-CA-NAV
                 MOVE DFHCOMMAREA(1:LENGTH OF WS-CA-NAV)
                                       TO WS-CA-NAV
                 MOVE SPACES           TO CN-OWNER-PGM
           END-EVALUATE
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
                     TIME(WS-NOW-TIME) TIMESEP(':')
           END-EXEC
           MOVE EIBTASKN               TO WS-TASKN
           STRING EIBTRNID EIBTRMID WS-TASKN DELIMITED BY SIZE
                  INTO WS-TRACE-ID
           MOVE WS-USERID              TO CN-USER-ID
           MOVE SPACES                 TO WS-ERR-TEXT WS-WARN-TEXT
           SET WS-NO-ERROR             TO TRUE.

      ******************************************************************
      * New conversation: empty entry screen, defaults filled in
      * BR-CLM-SFN-016 How reported defaults to PHON
       2000-NEW-FNOL.
           INITIALIZE WS-CA-STATE
           SET FN-STEP-ENTRY           TO TRUE
           MOVE LOW-VALUES             TO CLMFNMO
           MOVE 'PHON'                 TO FHOWRPO
           MOVE 'N'                    TO FCOVOVO
           IF CN-NAV-MSG NOT = SPACES
              MOVE CN-NAV-MSG          TO FMSGO
              MOVE SPACES              TO CN-NAV-MSG
           ELSE
              MOVE 'Enter the loss details and press ENTER' TO FMSGO
           END-IF
           PERFORM 6400-SET-STEP-TEXT
           MOVE -1                     TO FPOLNOL
           SET WS-SEND-ERASE           TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
      * Step 1: local validation, then policy verification
       2100-VERIFY.
           IF FN-STEP-CREATED
              MOVE LOW-VALUES          TO CLMFNMO
              STRING 'Claim ' FN-C-CLAIM-NUMBER
                     ' already created - PF12 new FNOL, PF4 inquire'
                     DELIMITED BY SIZE INTO WS-ERR-TEXT
              PERFORM 6300-SEND-ERROR
           ELSE
              PERFORM 6000-RECEIVE-MAP
              MOVE LOW-VALUES          TO CLMFNMO
              SET FN-STEP-ENTRY        TO TRUE
              PERFORM 3000-VALIDATE-ENTRY
              IF WS-NO-ERROR
                 PERFORM 8100-LINK-POLVRF
              END-IF
              IF WS-NO-ERROR
                 PERFORM 2150-EVALUATE-POLICY
              END-IF
              PERFORM 6200-ENTRY-TO-SCREEN
              IF WS-NO-ERROR
                 MOVE WE-ENTRY         TO FN-ENTRY
                 SET FN-STEP-VERIFIED  TO TRUE
                 PERFORM 2200-SHOW-POLICY
                 MOVE 'Policy verified - check the details and press PF5
      -               ' to create the claim'
                                       TO FMSGO
                 MOVE -1               TO FPOLNOL
                 PERFORM 6400-SET-STEP-TEXT
                 SET WS-SEND-DATAONLY  TO TRUE
                 PERFORM 6100-SEND-MAP
              ELSE
                 PERFORM 6400-SET-STEP-TEXT
                 PERFORM 6300-SEND-ERROR
              END-IF
           END-IF.

      ******************************************************************
      * Business checks on the verified policy (step 1 continued)
       2150-EVALUATE-POLICY.
           MOVE 'N'                    TO WE-V-COV-IN-QSTN
           MOVE PVA-PV-POLICY-NUMBER      TO WE-V-POLICY-NUM
           MOVE PVA-PV-CUSTOMER-NUM       TO WE-V-CUSTOMER-NUM
           MOVE PVA-PV-POLICY-TYPE        TO WE-V-POLICY-TYPE
           MOVE PVA-PV-STATUS-AT-LOSS     TO WE-V-STATUS-AT-LOSS
           MOVE PVA-PV-BILLING-STATUS     TO WE-V-BILLING-STATUS

      * BR-CLM-SFN-011 Not in force at loss date needs the override
           IF NOT PVA-PV-IS-VERIFIED
              IF WE-E-COV-OVERRIDE = 'Y'
                 MOVE 'Y'              TO WE-V-COV-IN-QSTN
                 STRING 'Policy not in force at loss (status '
                        PVA-PV-STATUS-AT-LOSS
                        ') - claim will be coverage-in-question'
                        DELIMITED BY SIZE INTO WS-WARN-TEXT
              ELSE
                 STRING 'Policy not in force on loss date (status '
                        PVA-PV-STATUS-AT-LOSS
                        ') - enter Y in Cov override to report'
                        DELIMITED BY SIZE INTO WS-ERR-TEXT
                 MOVE 06               TO WS-ERR-FIELD
                 PERFORM 3900-FLAG-FIELD
              END-IF
           END-IF

      * BR-CLM-SFN-012 Premium delinquency puts coverage in question
           EVALUATE PVA-PV-BILLING-STATUS
              WHEN 'DLNQ'
              WHEN 'CANC'
                 MOVE 'Y'              TO WE-V-COV-IN-QSTN
                 IF WS-WARN-TEXT = SPACES
                    STRING 'Billing status ' PVA-PV-BILLING-STATUS
                           ' - claim flagged coverage-in-question'
                           DELIMITED BY SIZE INTO WS-WARN-TEXT
                 END-IF
              WHEN 'PDUE'
                 IF WS-WARN-TEXT = SPACES
                    MOVE 'Premium past due on this policy - check with
      -                  ' billing before any payment'
                                       TO WS-WARN-TEXT
                 END-IF
           END-EVALUATE

      * BR-CLM-SFN-013 Property and commercial losses need a postcode
           IF (PVA-PV-POLICY-TYPE = 'H' OR PVA-PV-POLICY-TYPE = 'C')
              AND WE-E-POSTCODE = SPACES
              MOVE 'Loss postcode is required for home and commercial
      -            ' policies'         TO WS-ERR-TEXT
              MOVE 08                  TO WS-ERR-FIELD
              PERFORM 3900-FLAG-FIELD
           END-IF

      * BR-CLM-SFN-014 No coverage on the policy
           IF PVA-PV-COV-COUNT = 0
              MOVE 'Y'                 TO WE-V-COV-IN-QSTN
              IF WS-WARN-TEXT = SPACES
                 MOVE 'No coverages found on policy - claim will be cov
      -               'erage-in-question'
                                       TO WS-WARN-TEXT
              END-IF
           END-IF

           IF PVA-RC-WARNING AND WS-WARN-TEXT = SPACES
              MOVE PVA-MESSAGE         TO WS-WARN-TEXT
           END-IF.

      ******************************************************************
       2200-SHOW-POLICY.
           MOVE PVA-PV-INSURED-NAME       TO FINSNMO
           MOVE PVA-PV-POLICY-TYPE        TO FPTYPEO
           MOVE PVA-PV-EFF-DATE           TO FPEFFO
           MOVE PVA-PV-EXP-DATE           TO FPEXPO
           MOVE PVA-PV-STATUS-AT-LOSS     TO FPSTATO
           MOVE PVA-PV-BILLING-STATUS     TO FPBILLO
           PERFORM 5300-FORMAT-COVERAGES
           PERFORM 5400-FORMAT-RISK-UNITS
           MOVE SPACES                 TO FCLMNOO FCSTATO FSEGO FSEVO
                                          FADJO FGRPO FFLAGSO
           MOVE WS-WARN-TEXT           TO FWRNO.

      ******************************************************************
      * Step 2: create the claim from the verified entry
       2300-SUBMIT.
           MOVE LOW-VALUES             TO CLMFNMO
           EVALUATE TRUE
              WHEN FN-STEP-CREATED
                 STRING 'Claim ' FN-C-CLAIM-NUMBER
                        ' already created - PF12 for a new FNOL'
                        DELIMITED BY SIZE INTO WS-ERR-TEXT
                 PERFORM 6300-SEND-ERROR
              WHEN NOT FN-STEP-VERIFIED
                 MOVE 'Press ENTER to verify the policy before submitti
      -               'ng'             TO WS-ERR-TEXT
                 PERFORM 6300-SEND-ERROR
              WHEN OTHER
                 PERFORM 6000-RECEIVE-MAP
                 PERFORM 2310-CHECK-UNCHANGED
                 IF WS-NO-ERROR
                    PERFORM 8200-LINK-CLMNEW
                 END-IF
                 IF WS-NO-ERROR
                    PERFORM 2350-SHOW-CLAIM
                 ELSE
                    PERFORM 6400-SET-STEP-TEXT
                    PERFORM 6300-SEND-ERROR
                 END-IF
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-SFN-015 Submit only exactly what was verified
       2310-CHECK-UNCHANGED.
           IF WE-ENTRY NOT = FN-ENTRY
              SET FN-STEP-ENTRY        TO TRUE
              MOVE 'Details changed since verification - press ENTER t
      -            'o verify again'    TO WS-ERR-TEXT
              SET WS-HAS-ERROR         TO TRUE
              MOVE -1                  TO FPOLNOL
              MOVE SPACES              TO FINSNMO FPTYPEO FPEFFO FPEXPO
                                          FPSTATO FPBILLO FCOV1O FCOV2O
                                          FRISKO FWRNO
           END-IF.

      ******************************************************************
       2350-SHOW-CLAIM.
           SET FN-STEP-CREATED         TO TRUE
           MOVE CLA-CL-CLAIM-NUMBER       TO FN-C-CLAIM-NUMBER
           MOVE CLA-CL-CLAIM-ID           TO FN-C-CLAIM-ID
           MOVE CLA-CL-CLAIM-NUMBER       TO FCLMNOO
           MOVE CLA-CL-STATE              TO FCSTATO
           MOVE CLA-CL-SEGMENT            TO FSEGO
           MOVE CLA-CL-SEVERITY           TO WS-SEV-EDIT
           MOVE WS-SEV-EDIT            TO FSEVO
           MOVE CLA-CL-ASSIGNED-USER      TO FADJO
           MOVE CLA-CL-ASSIGNED-GROUP     TO FGRPO
           PERFORM 5500-FORMAT-CLAIM-FLAGS
           EVALUATE TRUE
              WHEN CLA-RC-WARNING
                 MOVE CLA-MESSAGE      TO FWRNO
              WHEN CLA-CL-DUP-SUSPECT-FLAG = 'Y'
                 MOVE 'Possible duplicate of an existing claim on this
      -               'policy - review before working'
                                       TO FWRNO
              WHEN CLA-CL-COVERAGE-IN-QSTN = 'Y'
                 MOVE 'Claim opened with coverage in question - covera
      -               'ge decision required'
                                       TO FWRNO
              WHEN OTHER
                 MOVE SPACES           TO FWRNO
           END-EVALUATE
           IF CLA-CL-ASSIGNED-USER = SPACES
              STRING 'Claim ' CLA-CL-CLAIM-NUMBER
                     ' created - queued to group ' CLA-CL-ASSIGNED-GROUP
                     ' for assignment'
                     DELIMITED BY SIZE INTO FMSGO
           ELSE
              STRING 'Claim ' CLA-CL-CLAIM-NUMBER
                     ' created and assigned to ' CLA-CL-ASSIGNED-USER
                     DELIMITED BY SIZE INTO FMSGO
           END-IF
           PERFORM 6400-SET-STEP-TEXT
           MOVE -1                     TO FPOLNOL
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       2700-CANCEL.
           IF FN-STEP-CREATED
              MOVE 'Ready for the next first notice of loss'
                                       TO CN-NAV-MSG
           ELSE
              MOVE 'FNOL cancelled - nothing was saved' TO CN-NAV-MSG
           END-IF
           PERFORM 2000-NEW-FNOL.

      ******************************************************************
       2800-RETURN-TO-MENU.
           MOVE SPACES                 TO CN-CLAIM-NUMBER
           MOVE 0                      TO CN-EXPOSURE-SEQ
           IF FN-STEP-CREATED
              MOVE FN-C-CLAIM-NUMBER   TO CN-CLAIM-NUMBER
              STRING 'Claim ' FN-C-CLAIM-NUMBER ' created'
                     DELIMITED BY SIZE INTO CN-NAV-MSG
           ELSE
              MOVE SPACES              TO CN-NAV-MSG
           END-IF
           MOVE WS-MENU-PGM            TO WS-LINK-PGM
           PERFORM 2900-XCTL.

       2850-INQUIRE-NEW-CLAIM.
           MOVE FN-C-CLAIM-NUMBER      TO CN-CLAIM-NUMBER
           MOVE 0                      TO CN-EXPOSURE-SEQ
           MOVE SPACES                 TO CN-NAV-MSG
           MOVE WS-INQ-PGM             TO WS-LINK-PGM
           PERFORM 2900-XCTL.

      ******************************************************************
       2900-XCTL.
           MOVE WS-PROGRAM-ID          TO CN-FROM-PGM
           MOVE SPACES                 TO CN-OWNER-PGM
           EXEC CICS XCTL PROGRAM(WS-LINK-PGM)
                     COMMAREA(WS-CA-NAV)
                     LENGTH(LENGTH OF WS-CA-NAV)
                     RESP(WS-RESP)
           END-EXEC
           MOVE WS-RESP                TO WS-RESP-DISP
           MOVE LOW-VALUES             TO CLMFNMO
           STRING 'Transfer to ' WS-LINK-PGM ' failed, CICS RESP '
                  WS-RESP-DISP DELIMITED BY SIZE INTO WS-ERR-TEXT
           PERFORM 6300-SEND-ERROR.

      ******************************************************************
      * Local validation of the whole entry. Every field in error is
      * flagged; the cursor lands on the first (CICS uses the first
      * field with length -1) and the first message is kept.
       3000-VALIDATE-ENTRY.
           PERFORM 3010-CHECK-POLICY-NO
           PERFORM 3020-CHECK-LOSS-DATE
           PERFORM 3030-CHECK-LOSS-TIME
           PERFORM 3040-CHECK-LOSS-CODES
           PERFORM 3060-CHECK-COV-OVERRIDE
           PERFORM 3070-CHECK-DESCRIPTION
           PERFORM 3080-CHECK-POSTCODE
           PERFORM 3090-CHECK-HOW-REPORTED
           PERFORM 3100-CHECK-REPORTER.

      ******************************************************************
      * BR-CLM-SFN-001 Policy number numeric 1-999999999
       3010-CHECK-POLICY-NO.
           MOVE WE-E-POLICY-NO         TO WS-NUM-TEXT
           PERFORM 5100-PARSE-INTEGER
           EVALUATE TRUE
              WHEN WE-E-POLICY-NO = SPACES
                 MOVE 'Policy number is required' TO WS-COV-LINE
              WHEN NOT WS-NUM-VALID
                 MOVE 'Policy number must be numeric' TO WS-COV-LINE
              WHEN WS-NUM-VALUE = 0 OR WS-NUM-VALUE > 999999999
                 MOVE 'Policy number must be 1 to 999999999'
                                       TO WS-COV-LINE
              WHEN OTHER
                 MOVE SPACES           TO WS-COV-LINE
                 MOVE WS-NUM-VALUE     TO WE-V-POLICY-NUM
           END-EVALUATE
           IF WS-COV-LINE NOT = SPACES
              MOVE 01                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-002 Loss date valid and not in the future
      * BR-CLM-SFN-003 Loss older than 3 years is outside limitation
      * BR-CLM-SFN-004 Loss reported after 30 days -> late warning
       3020-CHECK-LOSS-DATE.
           MOVE 0                      TO WE-V-LATE-DAYS
           IF WE-E-LOSS-DATE = SPACES
              MOVE 'Loss date is required (yyyy-mm-dd)' TO WS-COV-LINE
           ELSE
              MOVE SPACES              TO WS-COV-LINE
              MOVE WE-E-LOSS-DATE      TO DTA-DT-DATE-1
              MOVE SPACES              TO DTA-DT-DATE-2
              MOVE 'DTVALD  '          TO DTA-REQUEST-ID
              PERFORM 8400-LINK-DATES
              IF NOT WS-DATE-VALID
                 MOVE 'Loss date is not a valid date (yyyy-mm-dd)'
                                       TO WS-COV-LINE
              END-IF
           END-IF
           IF WS-COV-LINE = SPACES
              IF WE-E-LOSS-DATE > WS-TODAY
                 MOVE 'Loss date cannot be in the future' TO WS-COV-LINE
              ELSE
                 MOVE WE-E-LOSS-DATE   TO DTA-DT-DATE-1
                 MOVE WS-TODAY         TO DTA-DT-DATE-2
                 MOVE 'DTDIFF  '       TO DTA-REQUEST-ID
                 PERFORM 8400-LINK-DATES
                 MOVE DTA-DT-RESULT-NUM TO WS-DAYS
                 EVALUATE TRUE
                    WHEN WS-DAYS > WS-MAX-LOSS-AGE-DAYS
                       MOVE 'Loss is more than 3 years old - refer to t
      -                     'echnical claims'
                                       TO WS-COV-LINE
                    WHEN WS-DAYS > WS-LATE-REPORT-DAYS
                       MOVE WS-DAYS    TO WE-V-LATE-DAYS
                       MOVE WS-DAYS    TO WS-DAYS-DISP
                       IF WS-WARN-TEXT = SPACES
                          STRING 'Late notification: loss reported '
                                 WS-DAYS-DISP ' days after the event'
                                 DELIMITED BY SIZE INTO WS-WARN-TEXT
                       END-IF
                 END-EVALUATE
              END-IF
           END-IF
           IF WS-COV-LINE NOT = SPACES
              MOVE 02                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-005 Loss time HH:MM (HHMM accepted), not future
       3030-CHECK-LOSS-TIME.
           MOVE SPACES                 TO WS-COV-LINE
           IF WE-E-LOSS-TIME NOT = SPACES
              IF WE-E-LOSS-TIME(3:1) = ':'
                 MOVE WE-E-LOSS-TIME(1:2) TO WS-HH
                 MOVE WE-E-LOSS-TIME(4:2) TO WS-MM
              ELSE
                 MOVE WE-E-LOSS-TIME(1:2) TO WS-HH
                 MOVE WE-E-LOSS-TIME(3:2) TO WS-MM
              END-IF
              IF WS-HH IS NOT NUMERIC OR WS-MM IS NOT NUMERIC
                 OR WS-HH > '23' OR WS-MM > '59'
                 MOVE 'Loss time must be HH:MM (00:00-23:59)'
                                       TO WS-COV-LINE
              ELSE
                 STRING WS-HH ':' WS-MM DELIMITED BY SIZE
                        INTO WE-E-LOSS-TIME
                 IF WE-E-LOSS-DATE = WS-TODAY
                    AND WE-E-LOSS-TIME > WS-NOW-TIME(1:5)
                    MOVE 'Loss time cannot be later than now'
                                       TO WS-COV-LINE
                 END-IF
              END-IF
           END-IF
           IF WS-COV-LINE NOT = SPACES
              MOVE 03                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-006 Loss type and cause are code-table values
       3040-CHECK-LOSS-CODES.
           IF WE-E-LOSS-TYPE = SPACES
              MOVE 'Loss type is required' TO WS-COV-LINE
           ELSE
              MOVE 'LOSSTYPE'          TO WS-CODE-TABLE
              MOVE WE-E-LOSS-TYPE      TO WS-CODE-VALUE
              PERFORM 8300-LINK-CODVL
              IF WS-CODE-VALID
                 MOVE SPACES           TO WS-COV-LINE
              ELSE
                 MOVE 'Loss type is not a valid code' TO WS-COV-LINE
              END-IF
           END-IF
           IF WS-COV-LINE NOT = SPACES
              MOVE 04                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF

           IF WE-E-LOSS-CAUSE = SPACES
              MOVE 'Loss cause is required' TO WS-COV-LINE
           ELSE
              MOVE 'LOSSCAUS'          TO WS-CODE-TABLE
              MOVE WE-E-LOSS-CAUSE     TO WS-CODE-VALUE
              PERFORM 8300-LINK-CODVL
              IF WS-CODE-VALID
                 MOVE SPACES           TO WS-COV-LINE
              ELSE
                 MOVE 'Loss cause is not a valid code' TO WS-COV-LINE
              END-IF
           END-IF
           IF WS-COV-LINE NOT = SPACES
              MOVE 05                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
       3060-CHECK-COV-OVERRIDE.
           IF WE-E-COV-OVERRIDE = SPACE
              MOVE 'N'                 TO WE-E-COV-OVERRIDE
           END-IF
           IF WE-E-COV-OVERRIDE NOT = 'Y' AND NOT = 'N'
              MOVE 'Coverage override must be Y or N' TO WS-COV-LINE
              MOVE 06                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-007 Description at least 10 characters; blank lines
      * between text lines are closed up
       3070-CHECK-DESCRIPTION.
           MOVE SPACES                 TO WS-DESC-WORK(1)
                                          WS-DESC-WORK(2)
                                          WS-DESC-WORK(3)
           MOVE 0                      TO WS-JX
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
              IF WE-E-DESC-LINE(WS-IX) NOT = SPACES
                 ADD 1                 TO WS-JX
                 MOVE WE-E-DESC-LINE(WS-IX) TO WS-DESC-WORK(WS-JX)
              END-IF
           END-PERFORM
           MOVE 0                      TO WS-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
              MOVE WS-DESC-WORK(WS-IX) TO WE-E-DESC-LINE(WS-IX)
              INSPECT WS-DESC-WORK(WS-IX) TALLYING WS-COUNT
                      FOR ALL SPACES
           END-PERFORM
           IF (3 * 64) - WS-COUNT < WS-MIN-DESC-CHARS
              MOVE 'Describe the loss (at least 10 characters)'
                                       TO WS-COV-LINE
              MOVE 07                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-008 UK postcode: outward 2-4 chars starting with a
      * letter, one space, inward digit + two letters
       3080-CHECK-POSTCODE.
           IF WE-E-POSTCODE NOT = SPACES
              MOVE WE-E-POSTCODE       TO WS-POSTCODE
              MOVE 0                   TO WS-LEN
              INSPECT WS-POSTCODE TALLYING WS-LEN
                      FOR CHARACTERS BEFORE INITIAL SPACE
              MOVE SPACES              TO WS-COV-LINE
              IF WS-LEN < 2 OR WS-LEN > 4
                 OR WS-POSTCODE(1:1) < 'A' OR WS-POSTCODE(1:1) > 'Z'
                 MOVE 'X'              TO WS-COV-LINE
              ELSE
                 COMPUTE WS-PTR = WS-LEN + 2
                 IF WS-POSTCODE(WS-PTR:1) IS NOT NUMERIC
                    OR WS-POSTCODE(WS-PTR + 1:2) IS NOT ALPHABETIC
                    OR WS-POSTCODE(WS-PTR + 1:1) = SPACE
                    OR WS-POSTCODE(WS-PTR + 2:1) = SPACE
                    MOVE 'X'           TO WS-COV-LINE
                 END-IF
                 IF WS-PTR + 3 <= 8
                    IF WS-POSTCODE(WS-PTR + 3:) NOT = SPACES
                       MOVE 'X'        TO WS-COV-LINE
                    END-IF
                 END-IF
              END-IF
              IF WS-COV-LINE NOT = SPACES
                 MOVE 'Postcode must look like AB1 2CD' TO WS-COV-LINE
                 MOVE 08               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR-FROM-WORK
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-016 How reported defaults to PHON
       3090-CHECK-HOW-REPORTED.
           IF WE-E-HOW-REPORTED = SPACES
              MOVE 'PHON'              TO WE-E-HOW-REPORTED
           END-IF
           MOVE 'HOWRPTD '             TO WS-CODE-TABLE
           MOVE WE-E-HOW-REPORTED      TO WS-CODE-VALUE
           PERFORM 8300-LINK-CODVL
           IF NOT WS-CODE-VALID
              MOVE 'How reported is not a valid code' TO WS-COV-LINE
              MOVE 09                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-009 Reporter surname, phone and relation required
      * BR-CLM-SFN-010 Phone 10-15 digits; digits, spaces, + - ( )
       3100-CHECK-REPORTER.
           IF WE-E-RPT-LAST = SPACES
              MOVE 'Reporter surname is required' TO WS-COV-LINE
              MOVE 10                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF

           MOVE 0                      TO WS-DIGITS
           MOVE SPACES                 TO WS-COV-LINE
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 20
              MOVE WE-E-RPT-PHONE(WS-IX:1) TO WS-CHAR
              EVALUATE TRUE
                 WHEN WS-CHAR IS NUMERIC
                    ADD 1              TO WS-DIGITS
                 WHEN WS-CHAR = SPACE OR '+' OR '-' OR '(' OR ')'
                    CONTINUE
                 WHEN OTHER
                    MOVE 'Phone may hold only digits, spaces and + - ( )
      -                  ''            TO WS-COV-LINE
              END-EVALUATE
           END-PERFORM
           IF WS-COV-LINE = SPACES
              EVALUATE TRUE
                 WHEN WE-E-RPT-PHONE = SPACES
                    MOVE 'Reporter phone is required' TO WS-COV-LINE
                 WHEN WS-DIGITS < 10 OR WS-DIGITS > 15
                    MOVE 'Phone must have 10 to 15 digits'
                                       TO WS-COV-LINE
              END-EVALUATE
           END-IF
           IF WS-COV-LINE NOT = SPACES
              MOVE 11                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF

           IF WE-E-RPT-RELATION = SPACES
              MOVE 'Reporter relation to insured is required'
                                       TO WS-COV-LINE
           ELSE
              MOVE 'RPTRELN '          TO WS-CODE-TABLE
              MOVE WE-E-RPT-RELATION   TO WS-CODE-VALUE
              PERFORM 8300-LINK-CODVL
              IF WS-CODE-VALID
                 MOVE SPACES           TO WS-COV-LINE
              ELSE
                 MOVE 'Relation is not a valid code' TO WS-COV-LINE
              END-IF
           END-IF
           IF WS-COV-LINE NOT = SPACES
              MOVE 12                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR-FROM-WORK
           END-IF.

      ******************************************************************
      * Message for the field is in WS-COV-LINE (used as scratch)
       3890-ERROR-FROM-WORK.
           IF WS-ERR-TEXT = SPACES
              MOVE WS-COV-LINE         TO WS-ERR-TEXT
           END-IF
           PERFORM 3900-FLAG-FIELD.

      * Highlight the field, keep MDT on, cursor candidate
       3900-FLAG-FIELD.
           SET WS-HAS-ERROR            TO TRUE
           EVALUATE WS-ERR-FIELD
              WHEN 01
                 MOVE -1 TO FPOLNOL
                 MOVE DFHUNINT TO FPOLNOA
              WHEN 02
                 MOVE -1 TO FLDATEL
                 MOVE DFHUNIMD TO FLDATEA
              WHEN 03
                 MOVE -1 TO FLTIMEL
                 MOVE DFHUNIMD TO FLTIMEA
              WHEN 04
                 MOVE -1 TO FLTYPEL
                 MOVE DFHUNIMD TO FLTYPEA
              WHEN 05
                 MOVE -1 TO FCAUSEL
                 MOVE DFHUNIMD TO FCAUSEA
              WHEN 06
                 MOVE -1 TO FCOVOVL
                 MOVE DFHUNIMD TO FCOVOVA
              WHEN 07
                 MOVE -1 TO FDESC1L
                 MOVE DFHUNIMD TO FDESC1A
              WHEN 08
                 MOVE -1 TO FPCODEL
                 MOVE DFHUNIMD TO FPCODEA
              WHEN 09
                 MOVE -1 TO FHOWRPL
                 MOVE DFHUNIMD TO FHOWRPA
              WHEN 10
                 MOVE -1 TO FRLNAML
                 MOVE DFHUNIMD TO FRLNAMA
              WHEN 11
                 MOVE -1 TO FRPHONL
                 MOVE DFHUNIMD TO FRPHONA
              WHEN 12
                 MOVE -1 TO FRRELL
                 MOVE DFHUNIMD TO FRRELA
           END-EVALUATE.

      ******************************************************************
      * Parse WS-NUM-TEXT: optional leading/trailing spaces around an
      * unbroken run of digits. Result in WS-NUM-VALUE.
       5100-PARSE-INTEGER.
           MOVE 0                      TO WS-NUM-VALUE
           MOVE 'Y'                    TO WS-NUM-FLAG
           SET WS-NUM-LEADING          TO TRUE
           PERFORM VARYING WS-PTR FROM 1 BY 1 UNTIL WS-PTR > 15
              MOVE WS-NUM-TEXT(WS-PTR:1) TO WS-CHAR
              EVALUATE TRUE
                 WHEN WS-CHAR IS NUMERIC AND NOT WS-NUM-TRAILING
                    SET WS-NUM-IN-DIGITS TO TRUE
                    IF WS-NUM-VALUE > 99999999999999
                       MOVE 'N'        TO WS-NUM-FLAG
                    ELSE
                       COMPUTE WS-NUM-VALUE = WS-NUM-VALUE * 10
                                            + FUNCTION NUMVAL(WS-CHAR)
                    END-IF
                 WHEN WS-CHAR = SPACE OR LOW-VALUE
                    IF WS-NUM-IN-DIGITS
                       SET WS-NUM-TRAILING TO TRUE
                    END-IF
                 WHEN OTHER
                    MOVE 'N'           TO WS-NUM-FLAG
              END-EVALUATE
           END-PERFORM
           IF WS-NUM-LEADING
              MOVE 'N'                 TO WS-NUM-FLAG
           END-IF.

      ******************************************************************
      * Coverage summary: two coverages per line, limit/deductible
       5300-FORMAT-COVERAGES.
           MOVE SPACES                 TO FCOV1O FCOV2O
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PVA-PV-COV-COUNT OR WS-IX > 4
              MOVE SPACES              TO WS-COV-TEXT
              MOVE PVA-PV-C-LIMIT(WS-IX)  TO WS-AMT-EDIT
              STRING PVA-PV-C-TYPE(WS-IX) ' L' WS-AMT-EDIT
                     DELIMITED BY SIZE INTO WS-COV-TEXT
              MOVE PVA-PV-C-DEDUCTIBLE(WS-IX) TO WS-AMT-EDIT
              STRING WS-COV-TEXT(1:19) ' D' WS-AMT-EDIT(4:6)
                     DELIMITED BY SIZE INTO WS-COV-TEXT
              EVALUATE WS-IX
                 WHEN 1
                    MOVE WS-COV-TEXT   TO FCOV1O(1:32)
                 WHEN 2
                    MOVE WS-COV-TEXT   TO FCOV1O(33:32)
                 WHEN 3
                    MOVE WS-COV-TEXT   TO FCOV2O(1:32)
                 WHEN 4
                    MOVE WS-COV-TEXT   TO FCOV2O(33:32)
              END-EVALUATE
           END-PERFORM
           IF PVA-PV-COV-COUNT = 0
              MOVE '(none)'            TO FCOV1O
           END-IF
           IF PVA-PV-COV-COUNT > 4
              COMPUTE WS-COUNT = PVA-PV-COV-COUNT - 4
              MOVE WS-COUNT            TO WS-MORE-EDIT
              MOVE SPACES              TO WS-COV-LINE
              STRING FCOV2O(1:55) ' +' WS-MORE-EDIT ' more'
                     DELIMITED BY SIZE INTO WS-COV-LINE
              MOVE WS-COV-LINE         TO FCOV2O
           END-IF.

      ******************************************************************
      * Risk units: first unit described (vehicle or property)
       5400-FORMAT-RISK-UNITS.
           MOVE SPACES                 TO FRISKO WS-COV-LINE
           EVALUATE TRUE
              WHEN PVA-PV-RU-COUNT = 0
                 MOVE '(none on policy)' TO WS-COV-LINE
              WHEN PVA-PV-R-REG(1) NOT = SPACES
                 STRING PVA-PV-R-TYPE(1) ' ' PVA-PV-R-REG(1) ' '
                        PVA-PV-R-MAKE(1) ' ' PVA-PV-R-MODEL(1)
                        DELIMITED BY SIZE INTO WS-COV-LINE
              WHEN OTHER
                 STRING PVA-PV-R-TYPE(1) ' ' PVA-PV-R-POSTCODE(1) ' '
                        PVA-PV-R-DESC(1)
                        DELIMITED BY SIZE INTO WS-COV-LINE
           END-EVALUATE
           IF PVA-PV-RU-COUNT > 1
              COMPUTE WS-COUNT = PVA-PV-RU-COUNT - 1
              MOVE WS-COUNT            TO WS-MORE-EDIT
              STRING WS-COV-LINE(1:55) ' +' WS-MORE-EDIT ' more'
                     DELIMITED BY SIZE INTO FRISKO
           ELSE
              MOVE WS-COV-LINE         TO FRISKO
           END-IF.

      ******************************************************************
       5500-FORMAT-CLAIM-FLAGS.
           MOVE SPACES                 TO FFLAGSO
           MOVE 1                      TO WS-PTR
           IF CLA-CL-COVERAGE-IN-QSTN = 'Y'
              STRING 'COVQ ' DELIMITED BY SIZE
                     INTO FFLAGSO WITH POINTER WS-PTR
           END-IF
           IF CLA-CL-DUP-SUSPECT-FLAG = 'Y'
              STRING 'DUP ' DELIMITED BY SIZE
                     INTO FFLAGSO WITH POINTER WS-PTR
           END-IF
           IF CLA-CL-SIU-SCORE > 0
              MOVE CLA-CL-SIU-SCORE       TO WS-SIU-EDIT
              STRING 'SIU' WS-SIU-EDIT ' ' DELIMITED BY SIZE
                     INTO FFLAGSO WITH POINTER WS-PTR
           END-IF
           IF CLA-CL-CAT-CODE NOT = SPACES
              STRING 'CAT ' CLA-CL-CAT-CODE DELIMITED BY SIZE
                     INTO FFLAGSO WITH POINTER WS-PTR
           END-IF.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(CLMFNMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
              INSPECT CLMFNMI REPLACING ALL LOW-VALUES BY SPACES
           ELSE
              MOVE SPACES              TO CLMFNMI
           END-IF
           INITIALIZE WS-SCREEN-STATE
           MOVE FPOLNOI                TO WE-E-POLICY-NO
           MOVE FLDATEI                TO WE-E-LOSS-DATE
           MOVE FLTIMEI                TO WE-E-LOSS-TIME
           MOVE FLTYPEI                TO WE-E-LOSS-TYPE
           MOVE FCAUSEI                TO WE-E-LOSS-CAUSE
           MOVE FCOVOVI                TO WE-E-COV-OVERRIDE
           MOVE FDESC1I                TO WE-E-DESC-LINE(1)
           MOVE FDESC2I                TO WE-E-DESC-LINE(2)
           MOVE FDESC3I                TO WE-E-DESC-LINE(3)
           MOVE FADDRI                 TO WE-E-LOSS-ADDR
           MOVE FPCODEI                TO WE-E-POSTCODE
           MOVE FHOWRPI                TO WE-E-HOW-REPORTED
           MOVE FRFNAMI                TO WE-E-RPT-FIRST
           MOVE FRLNAMI                TO WE-E-RPT-LAST
           MOVE FRPHONI                TO WE-E-RPT-PHONE
           MOVE FRRELI                 TO WE-E-RPT-RELATION
           INSPECT WE-E-LOSS-TYPE      CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WE-E-LOSS-CAUSE     CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WE-E-COV-OVERRIDE   CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WE-E-POSTCODE       CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WE-E-HOW-REPORTED   CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WE-E-RPT-RELATION   CONVERTING WS-LOWER TO WS-UPPER.

      ******************************************************************
      * Normalised entry back to the screen so that what PF5 reads is
      * exactly what was verified
       6200-ENTRY-TO-SCREEN.
           MOVE WE-E-POLICY-NO         TO FPOLNOO
           MOVE WE-E-LOSS-DATE         TO FLDATEO
           MOVE WE-E-LOSS-TIME         TO FLTIMEO
           MOVE WE-E-LOSS-TYPE         TO FLTYPEO
           MOVE WE-E-LOSS-CAUSE        TO FCAUSEO
           MOVE WE-E-COV-OVERRIDE      TO FCOVOVO
           MOVE WE-E-DESC-LINE(1)      TO FDESC1O
           MOVE WE-E-DESC-LINE(2)      TO FDESC2O
           MOVE WE-E-DESC-LINE(3)      TO FDESC3O
           MOVE WE-E-LOSS-ADDR         TO FADDRO
           MOVE WE-E-POSTCODE          TO FPCODEO
           MOVE WE-E-HOW-REPORTED      TO FHOWRPO
           MOVE WE-E-RPT-FIRST         TO FRFNAMO
           MOVE WE-E-RPT-LAST          TO FRLNAMO
           MOVE WE-E-RPT-PHONE         TO FRPHONO
           MOVE WE-E-RPT-RELATION      TO FRRELO.

      ******************************************************************
       6100-SEND-MAP.
           MOVE WS-TODAY               TO FDATEO
           IF WS-SEND-ERASE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMFNMO) ERASE CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           ELSE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMFNMO) DATAONLY CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           END-IF.

      ******************************************************************
       6300-SEND-ERROR.
           MOVE WS-ERR-TEXT            TO FMSGO
           MOVE DFHBMASB               TO FMSGA
           IF WS-WARN-TEXT NOT = SPACES
              MOVE WS-WARN-TEXT        TO FWRNO
           END-IF
           IF FPOLNOL NOT = -1 AND FLDATEL NOT = -1
              AND FLTIMEL NOT = -1 AND FLTYPEL NOT = -1
              AND FCAUSEL NOT = -1 AND FCOVOVL NOT = -1
              AND FDESC1L NOT = -1 AND FPCODEL NOT = -1
              AND FHOWRPL NOT = -1 AND FRLNAML NOT = -1
              AND FRPHONL NOT = -1 AND FRRELL NOT = -1
              MOVE -1                  TO FPOLNOL
           END-IF
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       6400-SET-STEP-TEXT.
           EVALUATE TRUE
              WHEN FN-STEP-CREATED
                 MOVE 'Claim created' TO FSTEPO
                 MOVE 'PF4=Inquire on claim  PF12=New FNOL  PF3=Menu'
                                       TO FPFKO
              WHEN FN-STEP-VERIFIED
                 MOVE 'Step 2 of 2: policy verified - PF5 to submit'
                                       TO FSTEPO
                 MOVE 'PF5=Submit claim  ENTER=Re-verify  PF12=Cancel
      -               '  PF3=Menu'     TO FPFKO
              WHEN OTHER
                 MOVE 'Step 1 of 2: enter loss details, ENTER to verify'
                                       TO FSTEPO
                 MOVE 'ENTER=Verify policy  PF12=Cancel  PF3=Menu'
                                       TO FPFKO
           END-EVALUATE.

      ******************************************************************
       8100-LINK-POLVRF.
           INITIALIZE WS-POL-CA
           MOVE 'POLVRF  '             TO PVA-REQUEST-ID
           MOVE WS-USERID              TO PVA-USER-ID
           MOVE WS-TRACE-ID            TO PVA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO PVA-CALLER-PGM
           MOVE WE-V-POLICY-NUM        TO PVA-PV-POLICY-NUMBER
           MOVE WE-E-LOSS-DATE         TO PVA-PV-LOSS-DATE
           MOVE WS-POL-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-POL-PGM)
                     COMMAREA(WS-POL-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN PVA-RC-OK
                 WHEN PVA-RC-WARNING
                    CONTINUE
                 WHEN PVA-RC-NOT-FOUND
                    STRING 'Policy ' WE-E-POLICY-NO
                           ' not found in policy administration'
                           DELIMITED BY SIZE INTO WS-ERR-TEXT
                    MOVE 01            TO WS-ERR-FIELD
                    PERFORM 3900-FLAG-FIELD
                 WHEN PVA-RETURN-CODE >= 90
                    MOVE PVA-REASON-CODE TO ERA-ER-KEY
                    MOVE PVA-MESSAGE   TO WS-ERR-TEXT
                    PERFORM 8900-BACK-OUT
                 WHEN OTHER
                    STRING PVA-REASON-CODE ' ' PVA-MESSAGE
                           DELIMITED BY SIZE INTO WS-ERR-TEXT
                    MOVE 01            TO WS-ERR-FIELD
                    PERFORM 3900-FLAG-FIELD
              END-EVALUATE
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-016 Reported date is today
       8200-LINK-CLMNEW.
           INITIALIZE WS-CLM-CA
           MOVE 'CLMNEW  '             TO CLA-REQUEST-ID
           MOVE WS-USERID              TO CLA-USER-ID
           MOVE WS-TRACE-ID            TO CLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CLA-CALLER-PGM
           MOVE FN-V-POLICY-NUM        TO CLA-CL-GENAPP-POL-NUM
           MOVE FN-V-CUSTOMER-NUM      TO CLA-CL-GENAPP-CUST-NUM
           MOVE FN-V-POLICY-TYPE       TO CLA-CL-POLICY-TYPE
           MOVE FN-E-LOSS-TYPE         TO CLA-CL-LOSS-TYPE
           MOVE FN-E-LOSS-DATE         TO CLA-CL-LOSS-DATE
           IF FN-E-LOSS-TIME = SPACES
              MOVE SPACES              TO CLA-CL-LOSS-TIME
           ELSE
              STRING FN-E-LOSS-TIME ':00' DELIMITED BY SIZE
                     INTO CLA-CL-LOSS-TIME
           END-IF
           MOVE WS-TODAY               TO CLA-CL-REPORTED-DATE
           MOVE FN-E-HOW-REPORTED      TO CLA-CL-HOW-REPORTED
           MOVE FN-E-LOSS-CAUSE        TO CLA-CL-LOSS-CAUSE
           PERFORM 8210-BUILD-DESCRIPTION
           MOVE WS-LOSS-DESC           TO CLA-CL-LOSS-DESC
           MOVE FN-E-LOSS-ADDR         TO CLA-CL-LOSS-ADDR1
           MOVE FN-E-POSTCODE          TO CLA-CL-LOSS-POSTCODE
           MOVE FN-V-COV-IN-QSTN       TO CLA-CL-COVERAGE-IN-QSTN
           MOVE FN-E-RPT-FIRST         TO CLA-CL-RPT-FIRST-NAME
           MOVE FN-E-RPT-LAST          TO CLA-CL-RPT-LAST-NAME
           MOVE FN-E-RPT-PHONE         TO CLA-CL-RPT-PHONE
           MOVE FN-E-RPT-RELATION      TO CLA-CL-RPT-RELATION
           MOVE WS-CLM-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-CLM-PGM)
                     COMMAREA(WS-CLM-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN CLA-RC-OK
                 WHEN CLA-RC-WARNING
                    CONTINUE
      * BR-CLM-SFN-017 RC >= 90 backs out the unit of work
                 WHEN CLA-RETURN-CODE >= 90
                    MOVE CLA-REASON-CODE TO ERA-ER-KEY
                    MOVE CLA-MESSAGE   TO WS-ERR-TEXT
                    PERFORM 8900-BACK-OUT
                 WHEN OTHER
                    SET WS-HAS-ERROR   TO TRUE
                    STRING CLA-REASON-CODE ' ' CLA-MESSAGE
                           DELIMITED BY SIZE INTO WS-ERR-TEXT
              END-EVALUATE
           END-IF.

      * Three description lines joined, trailing blanks dropped
       8210-BUILD-DESCRIPTION.
           MOVE SPACES                 TO WS-LOSS-DESC
           MOVE 1                      TO WS-PTR
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
              IF FN-E-DESC-LINE(WS-IX) NOT = SPACES
                 MOVE 0                TO WS-COUNT
                 INSPECT FUNCTION REVERSE(FN-E-DESC-LINE(WS-IX))
                         TALLYING WS-COUNT FOR LEADING SPACES
                 COMPUTE WS-LEN = 64 - WS-COUNT
                 IF WS-PTR > 1
                    STRING ' ' DELIMITED BY SIZE
                           INTO WS-LOSS-DESC WITH POINTER WS-PTR
                 END-IF
                 STRING FN-E-DESC-LINE(WS-IX)(1:WS-LEN)
                        DELIMITED BY SIZE
                        INTO WS-LOSS-DESC WITH POINTER WS-PTR
              END-IF
           END-PERFORM.

      ******************************************************************
      * BR-CLM-SFN-006 Code-table validation through CLMCODVL
       8300-LINK-CODVL.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE WS-USERID              TO CDA-USER-ID
           MOVE WS-TRACE-ID            TO CDA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE WS-CODE-TABLE          TO CDA-CD-TABLE
           MOVE WS-CODE-VALUE          TO CDA-CD-CODE
           MOVE 'N'                    TO WS-CODE-FLAG
           MOVE WS-COD-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-COD-PGM)
                     COMMAREA(WS-COD-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK AND CDA-RC-OK AND CDA-CD-IS-VALID
              MOVE 'Y'                 TO WS-CODE-FLAG
           END-IF.

      ******************************************************************
       8400-LINK-DATES.
           MOVE WS-USERID              TO DTA-USER-ID
           MOVE WS-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE 'N'                    TO WS-DATE-FLAG
           MOVE WS-DTE-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-DTE-PGM)
                     COMMAREA(WS-DTE-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK AND DTA-RC-OK
              AND (DTA-DT-IS-VALID OR DTA-REQUEST-ID = 'DTDIFF  ')
              MOVE 'Y'                 TO WS-DATE-FLAG
           END-IF.

      ******************************************************************
      * LINK RESP check; a failed LINK is logged and treated as RC 91
       8800-CHECK-LINK.
           IF WS-RESP = DFHRESP(NORMAL)
              SET WS-LINK-OK           TO TRUE
           ELSE
              SET WS-LINK-FAILED       TO TRUE
              MOVE WS-RESP             TO WS-RESP-DISP
              MOVE SPACES              TO WS-ERR-TEXT
              STRING 'Service ' WS-LINK-PGM
                     ' unavailable (CICS RESP ' WS-RESP-DISP
                     ') - try again later'
                     DELIMITED BY SIZE INTO WS-ERR-TEXT
              PERFORM 8950-LOG-CICS-ERROR
              MOVE WS-LINK-PGM         TO ERA-ER-KEY
              PERFORM 8900-BACK-OUT
           END-IF.

      ******************************************************************
      * BR-CLM-SFN-017 Back out everything done in this task
       8900-BACK-OUT.
           SET WS-HAS-ERROR            TO TRUE
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           MOVE WS-ERR-TEXT            TO WS-COV-LINE
           MOVE SPACES                 TO WS-ERR-TEXT
           STRING 'Backed out: ' WS-COV-LINE DELIMITED BY SIZE
                  INTO WS-ERR-TEXT
           SET FN-STEP-ENTRY           TO TRUE.

      ******************************************************************
       8950-LOG-CICS-ERROR.
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERA-REQUEST-ID
           MOVE WS-USERID              TO ERA-USER-ID
           MOVE WS-TRACE-ID            TO ERA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ERA-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERA-ER-PROGRAM
           MOVE '8800-CHECK-LINK'      TO ERA-ER-PARAGRAPH
           MOVE WS-RESP                TO ERA-ER-RESP
           MOVE WS-ERR-TEXT            TO ERA-ER-TEXT
           MOVE WS-LINK-PGM            TO ERA-ER-KEY
           MOVE EIBTRNID               TO ERA-ER-TRAN-ID
           EXEC CICS LINK PROGRAM(WS-ERR-PGM)
                     COMMAREA(WS-ERR-CA) LENGTH(4000)
                     RESP(WS-RESP2)
           END-EXEC.

      ******************************************************************
       9000-RETURN.
           MOVE WS-PROGRAM-ID          TO CN-OWNER-PGM
           EXEC CICS RETURN TRANSID(WS-TRANID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
