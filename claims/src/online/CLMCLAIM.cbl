       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCLAIM.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMCLAIM                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim service - first notice of loss and the     *
      *               claim life cycle. FNOL verifies the policy       *
      *               against GenApp, snapshots policy, coverages and  *
      *               risk units, creates the insured and reporter     *
      *               contacts, flags duplicates and catastrophes,     *
      *               segments, assigns and opens the claim with its   *
      *               initial activities. Also inquiry, maintenance    *
      *               with field-level history, close and reopen.      *
      * REQUESTS    : CLMNEW CLMINQ CLMUPD CLMCLS CLMROP               *
      * CALLED BY   : CLMFNLP (FNOL screen), CLMFNLBT (FNOL intake),   *
      *               claim summary / close screens                    *
      * CALLS       : CLMSEQGN CLMERRLG CLMDATES CLMCODVL CLMHIST0     *
      *               CLMAUDCH CLMACTV0 CLMPOLVR CLMCONTC CLMSEGMT     *
      *               CLMASSGN                                         *
      * TABLES      : CLM_CLAIM          (C R U)                       *
      *               CLM_POLICY_SNAPSHOT(C R U)                       *
      *               CLM_POLICY_COVERAGE(C)                           *
      *               CLM_RISK_UNIT      (C)                           *
      *               CLM_CATASTROPHE    (R)                           *
      *               CLM_CLAIM_CONTACT  (R)                           *
      *               CLM_CONTACT        (R)                           *
      *               CLM_EXPOSURE       (R)                           *
      *               CLM_RESERVE_LINE   (R)                           *
      *               CLM_TRANSACTION    (R)                           *
      *               CLM_CHECK          (R)                           *
      *               CLM_ACTIVITY       (R U)                         *
      *               CLM_ACTIVITY_PATTERN (R)                         *
      *               CLM_LITIGATION     (R)                           *
      *               CLM_USER           (R U)                         *
      *               CLM_CONTROL        (R)                           *
      *               GENAPP.CUSTOMER    (R)                           *
      * RULES       : BR-CLM-CLM-001 Loss date valid, not after the    *
      *                              business date, not before 1990;   *
      *                              reported date >= loss date        *
      *               BR-CLM-CLM-002 Loss type and cause from code     *
      *                              tables; cause must be a child of  *
      *                              the loss type                     *
      *               BR-CLM-CLM-003 Loss description required, at     *
      *                              least 10 characters               *
      *               BR-CLM-CLM-004 Unverified (not found) policy is  *
      *                              accepted with COVERAGE_IN_QSTN Y  *
      *               BR-CLM-CLM-005 Loss outside the policy period is *
      *                              rejected unless override 'O'      *
      *                              (or 'Y') is passed                *
      *               BR-CLM-CLM-006 Policy type must agree with loss  *
      *                              type: M AUTO, H PROP, C PROP/LIAB *
      *               BR-CLM-CLM-007 Adverse billing or unverifiable   *
      *                              cover puts coverage in question   *
      *               BR-CLM-CLM-008 Duplicate suspect: same policy,   *
      *                              same cause, loss date +/- 3 days  *
      *               BR-CLM-CLM-009 Catastrophe tag when the loss     *
      *                              postcode area/district and date   *
      *                              fall in an active CLM_CATASTROPHE *
      *               BR-CLM-CLM-010 Reporter needs a relation; non-   *
      *                              insured reporters need a phone    *
      *               BR-CLM-CLM-011 After the claim row exists, only  *
      *                              RC >= 90 from a sub-service fails *
      *                              FNOL; lesser failures are warnings*
      *               BR-CLM-CLM-012 Claim opens in state OPEN on the  *
      *                              business date after segmentation  *
      *                              and assignment                    *
      *               BR-CLM-CLM-013 Optimistic locking on VERSION_NO  *
      *                              for update, close and reopen      *
      *               BR-CLM-CLM-014 Loss type is fixed after FNOL;    *
      *                              changing loss date re-verifies    *
      *                              the policy                        *
      *               BR-CLM-CLM-015 Litigation flag cannot be cleared *
      *                              while a matter is open            *
      *               BR-CLM-CLM-016 Coverage question can be cleared  *
      *                              only on a verified policy         *
      *               BR-CLM-CLM-017 Close reasons COMPLETE DUPLICAT   *
      *                              DENIED WITHDRAW NOPAY             *
      *               BR-CLM-CLM-018 Close needs all exposures closed, *
      *                              no open/pending reserve, no       *
      *                              pending txns or checks, no OPEN   *
      *                              mandatory activity or matter      *
      *               BR-CLM-CLM-019 DUPLICAT / WITHDRAW / NOPAY only  *
      *                              when nothing has been paid        *
      *               BR-CLM-CLM-020 Closing skips the remaining non-  *
      *                              mandatory activities and frees    *
      *                              the adjuster's capacity           *
      *               BR-CLM-CLM-021 Reopen only from CLSD with a      *
      *                              reason; REOPEN_COUNT + 1          *
      *               BR-CLM-CLM-022 Reopen returns to the previous    *
      *                              adjuster if active, else the      *
      *                              assignment rules decide           *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMCLAIM'.
       01  WS-PROGRAM-NAMES.
           05 WS-ERR-PGM               PIC X(8)  VALUE 'CLMERRLG'.
           05 WS-SEQ-PGM               PIC X(8)  VALUE 'CLMSEQGN'.
           05 WS-DTE-PGM               PIC X(8)  VALUE 'CLMDATES'.
           05 WS-COD-PGM               PIC X(8)  VALUE 'CLMCODVL'.
           05 WS-HIS-PGM               PIC X(8)  VALUE 'CLMHIST0'.
           05 WS-AUC-PGM               PIC X(8)  VALUE 'CLMAUDCH'.
           05 WS-ACT-PGM               PIC X(8)  VALUE 'CLMACTV0'.
           05 WS-POL-PGM               PIC X(8)  VALUE 'CLMPOLVR'.
           05 WS-CON-PGM               PIC X(8)  VALUE 'CLMCONTC'.
           05 WS-SEG-PGM               PIC X(8)  VALUE 'CLMSEGMT'.
           05 WS-ASG-PGM               PIC X(8)  VALUE 'CLMASSGN'.

       01  WS-CONSTANTS.
           05 WS-MIN-LOSS-DATE         PIC X(10) VALUE '1990-01-01'.
           05 WS-DUP-WINDOW            PIC S9(4) COMP VALUE 3.
           05 WS-MIN-DESC-LEN          PIC S9(4) COMP VALUE 10.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(10).
           05 WS-POL-DISP              PIC 9(10).
           05 WS-COUNT-DISP            PIC 9(4).
           05 WS-BUS-DATE              PIC X(10).
           05 WS-POLICY-TYPE           PIC X.
           05 WS-CUST-NUM              PIC S9(9) COMP.
           05 WS-COVQ                  PIC X.
           05 WS-CAT-CODE              PIC X(8).
           05 WS-DUP-FROM              PIC X(10).
           05 WS-DUP-TO                PIC X(10).
           05 WS-DUP-COUNT             PIC S9(9) COMP.
           05 WS-DUP-CLAIM-NO          PIC X(12).
           05 WS-DUP-IND               PIC S9(4) COMP.
           05 WS-CAT-POSTCODE          PIC X(8).
           05 WS-AREA                  PIC X(4).
           05 WS-OUTWARD               PIC X(4).
           05 WS-COUNT                 PIC S9(9) COMP.
           05 WS-AMOUNT                PIC S9(11)V99 COMP-3.
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-CHAR-IX               PIC S9(4) COMP.
           05 WS-INSURED-ID            PIC S9(9) COMP.
           05 WS-REPORTER-ID           PIC S9(9) COMP.
           05 WS-PREV-USER             PIC X(8).
           05 WS-VERIFIED-FLAG         PIC X.
           05 WS-OPEN-MATTERS          PIC S9(9) COMP.
           05 WS-EXP-TOTAL             PIC S9(9) COMP.
           05 WS-NULL-IND              PIC S9(4) COMP.
           05 WS-EOF-SW                PIC X.
              88 WS-END-OF-CURSOR                VALUE 'Y'.
           05 WS-CLAIM-ROW-SW          PIC X.
              88 WS-CLAIM-ROW-EXISTS             VALUE 'Y'.
           05 WS-REVERIFY-SW           PIC X.
              88 WS-REVERIFY                     VALUE 'Y'.
           05 WS-RESEGMENT-SW          PIC X.
              88 WS-RESEGMENT                    VALUE 'Y'.
           05 WS-OVERRIDE-SW           PIC X.
              88 WS-OVERRIDE-USED                VALUE 'Y'.
           05 WS-POL-FOUND-SW          PIC X.
              88 WS-POLICY-FOUND                 VALUE 'Y'.

       01  WS-TIME-WORK.
           05 WS-TIME-TEXT             PIC X(8).
           05 WS-TIME-PARTS REDEFINES WS-TIME-TEXT.
              10 WS-T-HH               PIC X(2).
              10 WS-T-SEP1             PIC X.
              10 WS-T-MM               PIC X(2).
              10 WS-T-SEP2             PIC X.
              10 WS-T-SS               PIC X(2).

       01  WS-TRIM.
           05 WS-TRIM-TEXT             PIC X(250).
           05 WS-TRIM-LEN              PIC S9(4) COMP.

       01  WS-CHANGE-LOG.
           05 WS-CHG-COUNT             PIC S9(4) COMP.
           05 WS-CHG-FIELD             PIC X(16).
           05 WS-CHG-OLD               PIC X(40).
           05 WS-CHG-NEW               PIC X(40).
           05 WS-CHG-LIST              PIC X(200).
           05 WS-CHG-PTR               PIC S9(4) COMP.

       01  WS-WARNING.
           05 WS-WARN-REASON           PIC X(8).
           05 WS-WARN-MESSAGE          PIC X(80).
       01  WS-NEW-WARNING.
           05 WS-NW-REASON             PIC X(8).
           05 WS-NW-MESSAGE            PIC X(80).

       01  WS-SUB-RESULT.
           05 WS-SUB-RC                PIC 9(2).
           05 WS-SUB-REASON            PIC X(8).
           05 WS-SUB-MESSAGE           PIC X(80).

       01  WS-CHANGE-TABLE.
           05 WS-CE-ENTRY OCCURS 12 TIMES.
              10 WS-CE-FIELD           PIC X(16).
              10 WS-CE-OLD             PIC X(40).
              10 WS-CE-NEW             PIC X(40).

       01  WS-SIDE-EFFECT.
           05 WS-HIST-EVENT            PIC X(8).
           05 WS-HIST-TEXT             PIC X(120).
           05 WS-AUDIT-EVENT           PIC X(8).
           05 WS-AUDIT-PAYLOAD         PIC X(400).
           05 WS-ACT-EVENT             PIC X(8).
           05 WS-CODE-TABLE            PIC X(8).
           05 WS-CODE-VALUE            PIC X(10).
           05 WS-CODE-PARENT           PIC X(10).

       01  WS-CODE-LISTS.
           05 WS-CLOSE-REASON          PIC X(8).
              88 WS-CLOSE-REASON-OK      VALUE 'COMPLETE' 'DUPLICAT'
                                               'DENIED  ' 'WITHDRAW'
                                               'NOPAY   '.
              88 WS-CLOSE-NEEDS-NO-PAY   VALUE 'DUPLICAT' 'WITHDRAW'
                                               'NOPAY   '.
           05 WS-FAULT                 PIC X(4).
              88 WS-FAULT-OK             VALUE 'UNKN' 'INSD' 'THRD'
                                               'SHRD' 'NOFT'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDPSN END-EXEC.
           EXEC SQL INCLUDE CLMDPCV END-EXEC.
           EXEC SQL INCLUDE CLMDRSK END-EXEC.
           EXEC SQL INCLUDE CLMDCAT END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDEXP END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE GENDCUS END-EXEC.

           EXEC SQL DECLARE CSR-EXPOSURES CURSOR FOR
              SELECT EXPOSURE_ID, EXPOSURE_SEQ, EXPOSURE_TYPE,
                     COVERAGE_TYPE, EXPOSURE_STATE, OPEN_RESERVE,
                     TOTAL_PAID
                FROM CLMDB.CLM_EXPOSURE
               WHERE CLAIM_ID = :CLM-CLAIM-ID
               ORDER BY EXPOSURE_SEQ
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.
       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTC-==.
           03 DTC-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTC-==.
       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDC-==.
           03 CDC-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDC-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HSC-==.
           03 HSC-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HSC-==.
       01  WS-AUC-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ACC-==.
           03 ACC-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==ACC-==.
       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVC-==.
           03 AVC-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVC-==.
       01  WS-POL-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==POC-==.
           03 POC-BODY.
              COPY CLMCPOL REPLACING LEADING ==CA-== BY ==POC-==.
       01  WS-CON-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==COC-==.
           03 COC-BODY.
              COPY CLMCCON REPLACING LEADING ==CA-== BY ==COC-==.
       01  WS-SEG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SGC-==.
           03 SGC-BODY.
              COPY CLMCSEG REPLACING LEADING ==CA-== BY ==SGC-==.
       01  WS-ASG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ASC-==.
           03 ASC-BODY.
              COPY CLMCASG REPLACING LEADING ==CA-== BY ==ASC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCCLM.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'CLMNEW  '
                 PERFORM 2000-CLAIM-NEW
              WHEN 'CLMINQ  '
                 PERFORM 2100-CLAIM-INQUIRE
              WHEN 'CLMUPD  '
                 PERFORM 2200-CLAIM-UPDATE
              WHEN 'CLMCLS  '
                 PERFORM 2300-CLAIM-CLOSE
              WHEN 'CLMROP  '
                 PERFORM 2400-CLAIM-REOPEN
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMCLAIM: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 3950-FINAL-WARNING
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          WS-WARNING
                                          WS-NEW-WARNING
                                          WS-CAT-CODE
           MOVE 'N'                    TO WS-EOF-SW
                                          WS-CLAIM-ROW-SW
                                          WS-REVERIFY-SW
                                          WS-RESEGMENT-SW
                                          WS-OVERRIDE-SW
                                          WS-POL-FOUND-SW
                                          WS-COVQ
           MOVE 0                      TO WS-INSURED-ID
                                          WS-REPORTER-ID
                                          WS-CHG-COUNT
           MOVE SPACES                 TO WS-CHG-LIST
           MOVE 1                      TO WS-CHG-PTR
           MOVE CA-CL-CLAIM-ID         TO WS-KEY-DISP
           MOVE CA-BUS-DATE            TO WS-BUS-DATE
           IF WS-BUS-DATE = SPACES
              EXEC SQL
                 SELECT CHAR(BUSINESS_DATE, ISO)
                   INTO :WS-BUS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = 'SYSTEM  '
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '1000-INITIALISE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF.

      ******************************************************************
      * CLMNEW - first notice of loss. Everything that can reject the
      * request is checked before the first INSERT (BR-CLM-CLM-011).
       2000-CLAIM-NEW.
           PERFORM 3000-VALIDATE-FNOL
           PERFORM 8100-VERIFY-POLICY
           PERFORM 3100-APPLY-VERIFICATION
           PERFORM 3200-CHECK-POLICY-LOB
           MOVE 0                      TO CLM-CLAIM-ID
           PERFORM 4300-CHECK-DUPLICATE
           MOVE CA-CL-LOSS-POSTCODE    TO WS-CAT-POSTCODE
           IF WS-CAT-POSTCODE = SPACES AND POC-PV-RU-COUNT > 0
              MOVE POC-PV-R-POSTCODE (1) TO WS-CAT-POSTCODE
           END-IF
           PERFORM 4400-FIND-CATASTROPHE

           MOVE 'CLM     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQC-SQ-VALUE           TO CLM-CLAIM-ID
                                          CA-CL-CLAIM-ID
                                          WS-KEY-DISP
           MOVE 'CLMNO   '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQC-SQ-FORMATTED       TO CLM-CLAIM-NUMBER
                                          CA-CL-CLAIM-NUMBER

           PERFORM 2010-BUILD-CLAIM-ROW
           PERFORM 4000-INSERT-CLAIM
           SET WS-CLAIM-ROW-EXISTS     TO TRUE
           PERFORM 4010-INSERT-SNAPSHOT
           PERFORM 4020-INSERT-COVERAGES
           PERFORM 4030-INSERT-RISK-UNITS
           PERFORM 2020-SETUP-INSURED
           PERFORM 2030-SETUP-REPORTER
           PERFORM 2040-RECORD-FNOL-FLAGS

           PERFORM 8400-SEGMENT-CLAIM
           MOVE 0                      TO ASC-AS-SEVERITY
           MOVE SPACES                 TO ASC-AS-SEGMENT
           PERFORM 8500-ASSIGN-CLAIM

      * BR-CLM-CLM-012 Draft becomes OPEN once segmented and assigned
           PERFORM 4100-READ-CLAIM-BY-ID
           PERFORM 4200-OPEN-CLAIM
           MOVE 'CLMOPEN '             TO WS-ACT-EVENT
           PERFORM 8600-GENERATE-ACTIVITIES
           IF CLM-DUP-SUSPECT-FLAG = 'Y'
              MOVE 'DUPSUSP '          TO WS-ACT-EVENT
              PERFORM 8600-GENERATE-ACTIVITIES
           END-IF
           IF CLM-COVERAGE-IN-QSTN = 'Y'
              MOVE 'COVQSTN '          TO WS-ACT-EVENT
              PERFORM 8600-GENERATE-ACTIVITIES
           END-IF

           MOVE CLM-GENAPP-POL-NUM     TO WS-POL-DISP
           MOVE 'CLMOPEN '             TO WS-HIST-EVENT
           MOVE SPACES                 TO WS-HIST-TEXT
           STRING 'Claim ' CLM-CLAIM-NUMBER ' opened ('
                  CLM-HOW-REPORTED ') policy ' WS-POL-DISP
                  ' loss ' CLM-LOSS-DATE ' cause ' CLM-LOSS-CAUSE
                  DELIMITED BY SIZE INTO WS-HIST-TEXT
           PERFORM 8300-WRITE-HISTORY
           MOVE 'CLMOPEN '             TO WS-AUDIT-EVENT
           PERFORM 8350-WRITE-AUDIT
           PERFORM 2900-RETURN-CLAIM.

      ******************************************************************
       2010-BUILD-CLAIM-ROW.
           MOVE CA-CL-GENAPP-POL-NUM   TO CLM-GENAPP-POL-NUM
           MOVE WS-CUST-NUM            TO CLM-GENAPP-CUST-NUM
           MOVE WS-POLICY-TYPE         TO CLM-POLICY-TYPE
           MOVE CA-CL-LOSS-TYPE        TO CLM-LOSS-TYPE
           MOVE CA-CL-LOSS-DATE        TO CLM-LOSS-DATE
           MOVE CA-CL-REPORTED-DATE    TO CLM-REPORTED-DATE
           MOVE CA-CL-HOW-REPORTED     TO CLM-HOW-REPORTED
           MOVE CA-CL-LOSS-CAUSE       TO CLM-LOSS-CAUSE
           PERFORM 2015-MOVE-LOSS-DETAIL
           MOVE 'DRFT'                 TO CLM-CLAIM-STATE
           MOVE SPACES                 TO CLM-CLOSE-REASON
                                          CLM-SEGMENT-CODE
                                          CLM-ASSIGNED-USER
                                          CLM-ASSIGNED-GROUP
           MOVE 1                      TO CLM-SEVERITY
                                          CLM-VERSION-NO
           MOVE 'UNKN'                 TO CLM-FAULT-RATING
           MOVE WS-COVQ                TO CLM-COVERAGE-IN-QSTN
           MOVE 0                      TO CLM-SIU-SCORE
                                          CLM-REOPEN-COUNT
                                          CLM-TOTAL-RESERVES
                                          CLM-TOTAL-PAID
                                          CLM-TOTAL-RECOVERED
                                          CLM-TOTAL-INCURRED
           MOVE 'NONE'                 TO CLM-SIU-STATUS
           MOVE 'N'                    TO CLM-LITIGATION-FLAG
                                          CLM-TOTAL-LOSS-FLAG
           MOVE WS-CAT-CODE            TO CLM-CAT-CODE
           IF WS-DUP-COUNT > 0
              MOVE 'Y'                 TO CLM-DUP-SUSPECT-FLAG
           ELSE
              MOVE 'N'                 TO CLM-DUP-SUSPECT-FLAG
           END-IF
           MOVE CA-USER-ID             TO CLM-CREATE-USER
           MOVE -1                     TO CLM-OPEN-DATE-IND
                                          CLM-CLOSE-DATE-IND.

      ******************************************************************
      * Loss time, description, address and postcode with NULL rules
       2015-MOVE-LOSS-DETAIL.
           IF CA-CL-LOSS-TIME = SPACES
              MOVE -1                  TO CLM-LOSS-TIME-IND
              MOVE SPACES              TO CLM-LOSS-TIME
           ELSE
              MOVE 0                   TO CLM-LOSS-TIME-IND
              MOVE CA-CL-LOSS-TIME     TO CLM-LOSS-TIME
           END-IF
           MOVE CA-CL-LOSS-DESC        TO WS-TRIM-TEXT
           PERFORM 5000-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CLM-LOSS-DESC-LEN
           MOVE CA-CL-LOSS-DESC        TO CLM-LOSS-DESC-TEXT
           MOVE CA-CL-LOSS-ADDR1       TO WS-TRIM-TEXT
           PERFORM 5000-TRIM-LENGTH
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CLM-LOSS-ADDR-LINE1-IND
              MOVE 0                   TO CLM-LOSS-ADDR-LINE1-LEN
           ELSE
              MOVE 0                   TO CLM-LOSS-ADDR-LINE1-IND
              MOVE WS-TRIM-LEN         TO CLM-LOSS-ADDR-LINE1-LEN
           END-IF
           MOVE CA-CL-LOSS-ADDR1       TO CLM-LOSS-ADDR-LINE1-TEXT
           MOVE CA-CL-LOSS-POSTCODE    TO CLM-LOSS-POSTCODE
           IF CA-CL-LOSS-POSTCODE = SPACES
              MOVE -1                  TO CLM-LOSS-POSTCODE-IND
           ELSE
              MOVE 0                   TO CLM-LOSS-POSTCODE-IND
           END-IF.

      ******************************************************************
      * Insured contact mirrored from the GenApp customer
       2020-SETUP-INSURED.
           IF CLM-GENAPP-CUST-NUM NOT > 0
              MOVE 'INSNOCUS'          TO WS-NW-REASON
              MOVE 'No GenApp customer - insured contact not created'
                                       TO WS-NW-MESSAGE
              PERFORM 3900-ADD-WARNING
           ELSE
              PERFORM 4600-READ-GENAPP-CUSTOMER
              IF SQLCODE = 100 OR GCU-LASTNAME = SPACES
                 MOVE 'INSNOCUS'       TO WS-NW-REASON
                 MOVE 'GenApp customer missing - insured not created'
                                       TO WS-NW-MESSAGE
                 PERFORM 3900-ADD-WARNING
              ELSE
                 INITIALIZE WS-CON-CA
                 MOVE 'CONNEW  '       TO COC-REQUEST-ID
                 MOVE 'PERS'           TO COC-CO-TYPE
                 MOVE GCU-FIRSTNAME    TO COC-CO-FIRST-NAME
                 MOVE GCU-LASTNAME     TO COC-CO-LAST-NAME
                 MOVE GCU-DATEOFBIRTH  TO COC-CO-DATE-OF-BIRTH
                 STRING GCU-HOUSENUMBER DELIMITED BY SPACE
                        ' '             DELIMITED BY SIZE
                        GCU-HOUSENAME   DELIMITED BY '  '
                        INTO COC-CO-ADDR-LINE1
                 MOVE GCU-POSTCODE     TO COC-CO-POSTCODE
                 IF GCU-PHONEMOBILE NOT = SPACES
                    MOVE GCU-PHONEMOBILE TO COC-CO-PHONE
                 ELSE
                    MOVE GCU-PHONEHOME TO COC-CO-PHONE
                 END-IF
                 MOVE GCU-EMAILADDRESS TO COC-CO-EMAIL
                 MOVE CLM-GENAPP-CUST-NUM TO COC-CO-GENAPP-CUST-NUM
                 MOVE CLM-CLAIM-ID     TO COC-CO-CLAIM-ID
                 PERFORM 8200-CALL-CONTACT
                 IF COC-RETURN-CODE < 12
                    MOVE COC-CO-CONTACT-ID TO WS-INSURED-ID
                    MOVE 'INSURED '    TO COC-CO-ROLE
                    PERFORM 2025-LINK-CONTACT
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
       2025-LINK-CONTACT.
           MOVE 'CONLNK  '             TO COC-REQUEST-ID
           MOVE CLM-CLAIM-ID           TO COC-CO-CLAIM-ID
           MOVE 0                      TO COC-CO-EXPOSURE-ID
           PERFORM 8200-CALL-CONTACT.

      ******************************************************************
       2030-SETUP-REPORTER.
           IF CA-CL-RPT-LAST-NAME NOT = SPACES
              INITIALIZE WS-CON-CA
              IF CA-CL-RPT-RELATION = 'INSURED ' AND WS-INSURED-ID > 0
                 MOVE WS-INSURED-ID    TO COC-CO-CONTACT-ID
              ELSE
                 MOVE 'CONNEW  '       TO COC-REQUEST-ID
                 MOVE 'PERS'           TO COC-CO-TYPE
                 MOVE CA-CL-RPT-FIRST-NAME TO COC-CO-FIRST-NAME
                 MOVE CA-CL-RPT-LAST-NAME  TO COC-CO-LAST-NAME
                 MOVE CA-CL-RPT-PHONE  TO COC-CO-PHONE
                 MOVE CLM-CLAIM-ID     TO COC-CO-CLAIM-ID
                 PERFORM 8200-CALL-CONTACT
              END-IF
              IF COC-RETURN-CODE < 12
                 MOVE COC-CO-CONTACT-ID TO WS-REPORTER-ID
                 MOVE 'REPORTER'       TO COC-CO-ROLE
                 PERFORM 2025-LINK-CONTACT
              END-IF
           END-IF.

      ******************************************************************
       2040-RECORD-FNOL-FLAGS.
           IF CLM-DUP-SUSPECT-FLAG = 'Y'
              MOVE WS-DUP-COUNT        TO WS-COUNT-DISP
              MOVE 'DUPSUSP '          TO WS-HIST-EVENT
              MOVE SPACES              TO WS-HIST-TEXT
              STRING 'Possible duplicate of ' WS-DUP-CLAIM-NO
                     ' (' WS-COUNT-DISP ' claim(s) same policy/cause'
                     ' within 3 days)' DELIMITED BY SIZE
                     INTO WS-HIST-TEXT
              PERFORM 8300-WRITE-HISTORY
              MOVE 'DUPSUSP '          TO WS-NW-REASON
              STRING 'Possible duplicate of claim ' WS-DUP-CLAIM-NO
                     DELIMITED BY SIZE INTO WS-NW-MESSAGE
              PERFORM 3900-ADD-WARNING
           END-IF
           IF CLM-CAT-CODE NOT = SPACES
              MOVE 'CATTAG  '          TO WS-HIST-EVENT
              MOVE SPACES              TO WS-HIST-TEXT
              STRING 'Tagged to catastrophe ' CLM-CAT-CODE
                     ' (postcode ' WS-CAT-POSTCODE ')'
                     DELIMITED BY SIZE INTO WS-HIST-TEXT
              PERFORM 8300-WRITE-HISTORY
           END-IF
           IF WS-OVERRIDE-USED
              MOVE 'COVQSTN '          TO WS-HIST-EVENT
              MOVE SPACES              TO WS-HIST-TEXT
              STRING 'Loss outside policy period accepted on override'
                     ' by ' CA-USER-ID DELIMITED BY SIZE
                     INTO WS-HIST-TEXT
              PERFORM 8300-WRITE-HISTORY
           END-IF.

      ******************************************************************
      * CLMINQ - by claim id, else by claim number
       2100-CLAIM-INQUIRE.
           EVALUATE TRUE
              WHEN CA-CL-CLAIM-ID > 0
                 PERFORM 4100-READ-CLAIM-BY-ID
              WHEN CA-CL-CLAIM-NUMBER NOT = SPACES
                 PERFORM 4110-READ-CLAIM-BY-NUMBER
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'CLMKEYRQ'       TO CA-REASON-CODE
                 MOVE 'Claim id or claim number is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           PERFORM 2900-RETURN-CLAIM.

      ******************************************************************
      * CLMUPD - loss details, fault and flags; field-level history
       2200-CLAIM-UPDATE.
           PERFORM 3300-LOAD-FOR-CHANGE
           IF CLM-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLSD '          TO CA-REASON-CODE
              MOVE 'Closed claims cannot be updated - reopen first'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-CLM-014 Loss type is fixed once the claim exists
           IF CA-CL-LOSS-TYPE NOT = SPACES
              AND CA-CL-LOSS-TYPE NOT = CLM-LOSS-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSTYIM'          TO CA-REASON-CODE
              MOVE 'Loss type cannot be changed after FNOL'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CLM-LOSS-TYPE          TO CA-CL-LOSS-TYPE
           MOVE CLM-REPORTED-DATE      TO CA-CL-REPORTED-DATE
           PERFORM 3400-VALIDATE-UPDATE
           PERFORM 2210-DETECT-CHANGES
           IF WS-CHG-COUNT = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOCHANGE'          TO CA-REASON-CODE
              MOVE 'No claim fields were changed' TO CA-MESSAGE
              PERFORM 2900-RETURN-CLAIM
              PERFORM 9000-RETURN
           END-IF

           MOVE CA-CL-LOSS-DATE        TO CLM-LOSS-DATE
           MOVE CA-CL-LOSS-CAUSE       TO CLM-LOSS-CAUSE
           PERFORM 2015-MOVE-LOSS-DETAIL
           MOVE CA-CL-FAULT-RATING     TO CLM-FAULT-RATING
           MOVE CA-CL-LITIGATION-FLAG  TO CLM-LITIGATION-FLAG
           MOVE CA-CL-TOTAL-LOSS-FLAG  TO CLM-TOTAL-LOSS-FLAG
           MOVE WS-COVQ                TO CLM-COVERAGE-IN-QSTN
           MOVE WS-CAT-CODE            TO CLM-CAT-CODE
           PERFORM 4500-UPDATE-CLAIM
           SET WS-CLAIM-ROW-EXISTS     TO TRUE
           PERFORM 5150-WRITE-CHANGE-HISTORY
           IF WS-REVERIFY
              PERFORM 4510-UPDATE-SNAPSHOT
           END-IF
           IF WS-RESEGMENT
              PERFORM 8400-SEGMENT-CLAIM
           END-IF
           MOVE 'CLMUPD  '             TO WS-AUDIT-EVENT
           PERFORM 8350-WRITE-AUDIT
           PERFORM 2900-RETURN-CLAIM.

      ******************************************************************
      * Compare the request image with the stored claim. Each changed
      * field gets its own history row (old -> new).
       2210-DETECT-CHANGES.
           IF CA-CL-LOSS-DATE NOT = CLM-LOSS-DATE
              MOVE 'LOSS_DATE'         TO WS-CHG-FIELD
              MOVE CLM-LOSS-DATE       TO WS-CHG-OLD
              MOVE CA-CL-LOSS-DATE     TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           IF CLM-LOSS-TIME-IND < 0
              MOVE SPACES              TO CLM-LOSS-TIME
           END-IF
           IF CA-CL-LOSS-TIME NOT = CLM-LOSS-TIME
              MOVE 'LOSS_TIME'         TO WS-CHG-FIELD
              MOVE CLM-LOSS-TIME       TO WS-CHG-OLD
              MOVE CA-CL-LOSS-TIME     TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           IF CA-CL-LOSS-CAUSE NOT = CLM-LOSS-CAUSE
              MOVE 'LOSS_CAUSE'        TO WS-CHG-FIELD
              MOVE CLM-LOSS-CAUSE      TO WS-CHG-OLD
              MOVE CA-CL-LOSS-CAUSE    TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
              SET WS-RESEGMENT         TO TRUE
           END-IF
           MOVE SPACES                 TO WS-TRIM-TEXT
           IF CLM-LOSS-DESC-LEN > 0
              MOVE CLM-LOSS-DESC-TEXT(1:CLM-LOSS-DESC-LEN)
                                       TO WS-TRIM-TEXT
           END-IF
           IF CA-CL-LOSS-DESC NOT = WS-TRIM-TEXT
              MOVE 'LOSS_DESC'         TO WS-CHG-FIELD
              MOVE WS-TRIM-TEXT(1:40)  TO WS-CHG-OLD
              MOVE CA-CL-LOSS-DESC(1:40) TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           MOVE SPACES                 TO WS-TRIM-TEXT
           IF CLM-LOSS-ADDR-LINE1-IND >= 0
              AND CLM-LOSS-ADDR-LINE1-LEN > 0
              MOVE CLM-LOSS-ADDR-LINE1-TEXT(1:CLM-LOSS-ADDR-LINE1-LEN)
                                       TO WS-TRIM-TEXT
           END-IF
           IF CA-CL-LOSS-ADDR1 NOT = WS-TRIM-TEXT(1:60)
              MOVE 'LOSS_ADDR'         TO WS-CHG-FIELD
              MOVE WS-TRIM-TEXT(1:40)  TO WS-CHG-OLD
              MOVE CA-CL-LOSS-ADDR1(1:40) TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           IF CLM-LOSS-POSTCODE-IND < 0
              MOVE SPACES              TO CLM-LOSS-POSTCODE
           END-IF
           IF CA-CL-LOSS-POSTCODE NOT = CLM-LOSS-POSTCODE
              MOVE 'LOSS_POSTCODE'     TO WS-CHG-FIELD
              MOVE CLM-LOSS-POSTCODE   TO WS-CHG-OLD
              MOVE CA-CL-LOSS-POSTCODE TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           IF CA-CL-FAULT-RATING NOT = CLM-FAULT-RATING
              MOVE 'FAULT_RATING'      TO WS-CHG-FIELD
              MOVE CLM-FAULT-RATING    TO WS-CHG-OLD
              MOVE CA-CL-FAULT-RATING  TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           IF CA-CL-LITIGATION-FLAG NOT = CLM-LITIGATION-FLAG
              MOVE 'LITIGATION_FLAG'   TO WS-CHG-FIELD
              MOVE CLM-LITIGATION-FLAG TO WS-CHG-OLD
              MOVE CA-CL-LITIGATION-FLAG TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
              SET WS-RESEGMENT         TO TRUE
           END-IF
           IF CA-CL-TOTAL-LOSS-FLAG NOT = CLM-TOTAL-LOSS-FLAG
              MOVE 'TOTAL_LOSS_FLAG'   TO WS-CHG-FIELD
              MOVE CLM-TOTAL-LOSS-FLAG TO WS-CHG-OLD
              MOVE CA-CL-TOTAL-LOSS-FLAG TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
              SET WS-RESEGMENT         TO TRUE
           END-IF
           IF WS-COVQ NOT = CLM-COVERAGE-IN-QSTN
              MOVE 'COVERAGE_IN_QSTN'  TO WS-CHG-FIELD
              MOVE CLM-COVERAGE-IN-QSTN TO WS-CHG-OLD
              MOVE WS-COVQ             TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
           END-IF
           IF WS-CAT-CODE NOT = CLM-CAT-CODE
              MOVE 'CAT_CODE'          TO WS-CHG-FIELD
              MOVE CLM-CAT-CODE        TO WS-CHG-OLD
              MOVE WS-CAT-CODE         TO WS-CHG-NEW
              PERFORM 5100-NOTE-CHANGE
              SET WS-RESEGMENT         TO TRUE
           END-IF.

      ******************************************************************
      * CLMCLS - close the claim
       2300-CLAIM-CLOSE.
           PERFORM 3300-LOAD-FOR-CHANGE
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMNOTOP'          TO CA-REASON-CODE
              STRING 'Claim is ' CLM-CLAIM-STATE
                     ' - only OPEN claims can be closed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-CLM-017 Published close reasons only
           MOVE CA-CL-CLOSE-REASON     TO WS-CLOSE-REASON
           IF NOT WS-CLOSE-REASON-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLSRSNIV'          TO CA-REASON-CODE
              MOVE 'Close reason must be COMPLETE DUPLICAT DENIED WITH
      -            'DRAW or NOPAY'     TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-CLM-019 Nothing may have been paid on these closures
           IF WS-CLOSE-NEEDS-NO-PAY AND CLM-TOTAL-PAID NOT = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'PAIDCLS '          TO CA-REASON-CODE
              STRING 'Payments exist - cannot close as '
                     WS-CLOSE-REASON DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4700-CHECK-CLOSE-BLOCKERS

      * BR-CLM-CLM-020 Leftover optional work is skipped, not deleted
           PERFORM 4720-SKIP-OPEN-ACTIVITIES
           MOVE 'CLSD'                 TO CLM-CLAIM-STATE
           MOVE WS-CLOSE-REASON        TO CLM-CLOSE-REASON
           MOVE WS-BUS-DATE            TO CLM-CLOSE-DATE
           PERFORM 4730-CLOSE-CLAIM
           SET WS-CLAIM-ROW-EXISTS     TO TRUE
           IF CLM-ASSIGNED-USER NOT = SPACES
              MOVE CLM-ASSIGNED-USER   TO USR-USER-ID
              PERFORM 4740-RELEASE-CAPACITY
           END-IF
           MOVE 'CLMCLOSE'             TO WS-HIST-EVENT
           MOVE SPACES                 TO WS-HIST-TEXT
           STRING 'Claim ' CLM-CLAIM-NUMBER ' closed '
                  WS-CLOSE-REASON ' on ' WS-BUS-DATE
                  DELIMITED BY SIZE INTO WS-HIST-TEXT
           PERFORM 8300-WRITE-HISTORY
           MOVE 'CLMCLOSE'             TO WS-AUDIT-EVENT
           PERFORM 8350-WRITE-AUDIT
           PERFORM 2900-RETURN-CLAIM.

      ******************************************************************
      * CLMROP - reopen a closed claim
       2400-CLAIM-REOPEN.
           PERFORM 3300-LOAD-FOR-CHANGE
      * BR-CLM-CLM-021 Only closed claims, and only with a reason
           IF CLM-CLAIM-STATE NOT = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMNOTCL'          TO CA-REASON-CODE
              STRING 'Claim is ' CLM-CLAIM-STATE
                     ' - only CLSD claims can be reopened'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CL-REOPEN-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ROPRSNRQ'          TO CA-REASON-CODE
              MOVE 'A reopen reason is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

      * BR-CLM-CLM-022 Previous adjuster keeps the claim when active
           MOVE CLM-ASSIGNED-USER      TO WS-PREV-USER
           MOVE 'N'                    TO USR-ACTIVE-FLAG
           IF WS-PREV-USER NOT = SPACES
              MOVE WS-PREV-USER        TO USR-USER-ID
              PERFORM 4800-READ-USER
           END-IF
           IF USR-ACTIVE-FLAG NOT = 'Y'
              MOVE SPACES              TO CLM-ASSIGNED-USER
                                          CLM-ASSIGNED-GROUP
           END-IF
           MOVE 'OPEN'                 TO CLM-CLAIM-STATE
           MOVE SPACES                 TO CLM-CLOSE-REASON
           ADD 1                       TO CLM-REOPEN-COUNT
           PERFORM 4750-REOPEN-CLAIM
           SET WS-CLAIM-ROW-EXISTS     TO TRUE
           IF USR-ACTIVE-FLAG = 'Y'
              PERFORM 4810-CLAIM-CAPACITY
           ELSE
              INITIALIZE WS-ASG-CA
              PERFORM 8500-ASSIGN-CLAIM
           END-IF

           MOVE 'CLMOPEN '             TO WS-ACT-EVENT
           PERFORM 8600-GENERATE-ACTIVITIES
           MOVE CLM-REOPEN-COUNT       TO WS-COUNT-DISP
           MOVE 'CLMREOPN'             TO WS-HIST-EVENT
           MOVE SPACES                 TO WS-HIST-TEXT
           STRING 'Claim reopened (' CA-CL-REOPEN-REASON
                  ') reopen no. ' WS-COUNT-DISP ' previous owner '
                  WS-PREV-USER DELIMITED BY SIZE INTO WS-HIST-TEXT
           PERFORM 8300-WRITE-HISTORY
           MOVE 'CLMREOPN'             TO WS-AUDIT-EVENT
           PERFORM 8350-WRITE-AUDIT
           PERFORM 2900-RETURN-CLAIM.

      ******************************************************************
      * Re-read the claim and hand back the full picture
       2900-RETURN-CLAIM.
           PERFORM 4100-READ-CLAIM-BY-ID
           PERFORM 2910-FILL-COMMAREA
           PERFORM 4900-READ-CLAIM-CONTACTS
           PERFORM 4950-LIST-EXPOSURES.

      ******************************************************************
       2910-FILL-COMMAREA.
           MOVE CLM-CLAIM-ID           TO CA-CL-CLAIM-ID
           MOVE CLM-CLAIM-NUMBER       TO CA-CL-CLAIM-NUMBER
           MOVE CLM-GENAPP-POL-NUM     TO CA-CL-GENAPP-POL-NUM
           MOVE CLM-GENAPP-CUST-NUM    TO CA-CL-GENAPP-CUST-NUM
           MOVE CLM-POLICY-TYPE        TO CA-CL-POLICY-TYPE
           MOVE CLM-LOSS-TYPE          TO CA-CL-LOSS-TYPE
           MOVE CLM-LOSS-DATE          TO CA-CL-LOSS-DATE
           IF CLM-LOSS-TIME-IND < 0
              MOVE SPACES              TO CA-CL-LOSS-TIME
           ELSE
              MOVE CLM-LOSS-TIME       TO CA-CL-LOSS-TIME
           END-IF
           MOVE CLM-REPORTED-DATE      TO CA-CL-REPORTED-DATE
           MOVE CLM-HOW-REPORTED       TO CA-CL-HOW-REPORTED
           MOVE CLM-LOSS-CAUSE         TO CA-CL-LOSS-CAUSE
           MOVE SPACES                 TO CA-CL-LOSS-DESC
                                          CA-CL-LOSS-ADDR1
           IF CLM-LOSS-DESC-LEN > 0
              MOVE CLM-LOSS-DESC-TEXT(1:CLM-LOSS-DESC-LEN)
                                       TO CA-CL-LOSS-DESC
           END-IF
           IF CLM-LOSS-ADDR-LINE1-IND >= 0
              AND CLM-LOSS-ADDR-LINE1-LEN > 0
              MOVE CLM-LOSS-ADDR-LINE1-TEXT(1:CLM-LOSS-ADDR-LINE1-LEN)
                                       TO CA-CL-LOSS-ADDR1
           END-IF
           IF CLM-LOSS-POSTCODE-IND < 0
              MOVE SPACES              TO CA-CL-LOSS-POSTCODE
           ELSE
              MOVE CLM-LOSS-POSTCODE   TO CA-CL-LOSS-POSTCODE
           END-IF
           MOVE CLM-CLAIM-STATE        TO CA-CL-STATE
           MOVE CLM-CLOSE-REASON       TO CA-CL-CLOSE-REASON
           MOVE CLM-SEGMENT-CODE       TO CA-CL-SEGMENT
           MOVE CLM-SEVERITY           TO CA-CL-SEVERITY
           MOVE CLM-FAULT-RATING       TO CA-CL-FAULT-RATING
           MOVE CLM-COVERAGE-IN-QSTN   TO CA-CL-COVERAGE-IN-QSTN
           MOVE CLM-SIU-SCORE          TO CA-CL-SIU-SCORE
           MOVE CLM-SIU-STATUS         TO CA-CL-SIU-STATUS
           MOVE CLM-LITIGATION-FLAG    TO CA-CL-LITIGATION-FLAG
           MOVE CLM-CAT-CODE           TO CA-CL-CAT-CODE
           MOVE CLM-TOTAL-LOSS-FLAG    TO CA-CL-TOTAL-LOSS-FLAG
           MOVE CLM-DUP-SUSPECT-FLAG   TO CA-CL-DUP-SUSPECT-FLAG
           MOVE CLM-ASSIGNED-USER      TO CA-CL-ASSIGNED-USER
           MOVE CLM-ASSIGNED-GROUP     TO CA-CL-ASSIGNED-GROUP
           MOVE CLM-TOTAL-RESERVES     TO CA-CL-TOTAL-RESERVES
           MOVE CLM-TOTAL-PAID         TO CA-CL-TOTAL-PAID
           MOVE CLM-TOTAL-RECOVERED    TO CA-CL-TOTAL-RECOVERED
           MOVE CLM-TOTAL-INCURRED     TO CA-CL-TOTAL-INCURRED
           MOVE CLM-VERSION-NO         TO CA-CL-VERSION-NO.

      ******************************************************************
       3000-VALIDATE-FNOL.
           IF CA-CL-GENAPP-POL-NUM NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'POLNUMRQ'          TO CA-REASON-CODE
              MOVE 'Policy number is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CL-REPORTED-DATE = SPACES
              MOVE WS-BUS-DATE         TO CA-CL-REPORTED-DATE
           END-IF
           PERFORM 3010-VALIDATE-LOSS-DATE
           PERFORM 3020-VALIDATE-LOSS-TIME

      * BR-CLM-CLM-002 Loss type from its typelist
           MOVE 'LOSSTYPE'             TO WS-CODE-TABLE
           MOVE CA-CL-LOSS-TYPE        TO WS-CODE-VALUE
           MOVE SPACES                 TO WS-CODE-PARENT
           PERFORM 8050-VALIDATE-CODE
           IF NOT CDC-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSTYIV'          TO CA-REASON-CODE
              STRING 'Loss type ' CA-CL-LOSS-TYPE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3030-VALIDATE-CAUSE

           IF CA-CL-HOW-REPORTED = SPACES
              MOVE 'PHON'              TO CA-CL-HOW-REPORTED
           END-IF
           MOVE 'HOWRPTD '             TO WS-CODE-TABLE
           MOVE CA-CL-HOW-REPORTED     TO WS-CODE-VALUE
           MOVE SPACES                 TO WS-CODE-PARENT
           PERFORM 8050-VALIDATE-CODE
           IF NOT CDC-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HOWRPTIV'          TO CA-REASON-CODE
              STRING 'How reported ' CA-CL-HOW-REPORTED
                     ' is not valid' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3040-VALIDATE-DESCRIPTION
           PERFORM 3050-VALIDATE-POSTCODE

      * BR-CLM-CLM-010 A reporter must say who they are to the insured
           IF CA-CL-RPT-LAST-NAME NOT = SPACES
              MOVE 'RPTRELN '          TO WS-CODE-TABLE
              MOVE CA-CL-RPT-RELATION  TO WS-CODE-VALUE
              MOVE SPACES              TO WS-CODE-PARENT
              PERFORM 8050-VALIDATE-CODE
              IF CA-CL-RPT-RELATION = SPACES
                 OR NOT CDC-CD-IS-VALID
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RPTRELIV'       TO CA-REASON-CODE
                 MOVE 'Reporter relation is missing or not valid'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              IF CA-CL-RPT-RELATION NOT = 'INSURED '
                 AND CA-CL-RPT-PHONE = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RPTPHNRQ'       TO CA-REASON-CODE
                 MOVE 'A phone number is required for the reporter'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           IF CA-CL-COVERAGE-IN-QSTN NOT = SPACE AND NOT = 'N'
              AND NOT = 'Y' AND NOT = 'O'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'COVQIV  '          TO CA-REASON-CODE
              MOVE 'Coverage-in-question must be blank, N, Y or O'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-CLM-001 Loss date window and reported-date ordering
       3010-VALIDATE-LOSS-DATE.
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTC-REQUEST-ID
           MOVE CA-CL-LOSS-DATE        TO DTC-DT-DATE-1
           PERFORM 8060-CALL-DATES
           IF NOT DTC-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSDTIV'          TO CA-REASON-CODE
              STRING 'Loss date ' CA-CL-LOSS-DATE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CL-LOSS-DATE > WS-BUS-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSDTFU'          TO CA-REASON-CODE
              MOVE 'Loss date cannot be in the future' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CL-LOSS-DATE < WS-MIN-LOSS-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSDTOL'          TO CA-REASON-CODE
              MOVE 'Loss date cannot be before 1990-01-01'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTC-REQUEST-ID
           MOVE CA-CL-REPORTED-DATE    TO DTC-DT-DATE-1
           PERFORM 8060-CALL-DATES
           IF NOT DTC-DT-IS-VALID
              OR CA-CL-REPORTED-DATE > WS-BUS-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RPTDTIV '          TO CA-REASON-CODE
              MOVE 'Reported date is invalid or in the future'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CL-REPORTED-DATE < CA-CL-LOSS-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RPTBFLOS'          TO CA-REASON-CODE
              MOVE 'Reported date cannot be before the loss date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Loss time HH:MM[:SS] or HH.MM.SS; stored as Db2 TIME hh.mm.ss
       3020-VALIDATE-LOSS-TIME.
           IF CA-CL-LOSS-TIME NOT = SPACES
              MOVE CA-CL-LOSS-TIME     TO WS-TIME-TEXT
              INSPECT WS-TIME-TEXT REPLACING ALL ':' BY '.'
              IF WS-T-SS = SPACES
                 MOVE '.'              TO WS-T-SEP2
                 MOVE '00'             TO WS-T-SS
              END-IF
              IF WS-T-HH NOT NUMERIC OR WS-T-MM NOT NUMERIC
                 OR WS-T-SS NOT NUMERIC
                 OR WS-T-SEP1 NOT = '.' OR WS-T-SEP2 NOT = '.'
                 OR WS-T-HH > '23' OR WS-T-MM > '59'
                 OR WS-T-SS > '59'
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'LOSSTMIV'       TO CA-REASON-CODE
                 MOVE 'Loss time must be HH:MM:SS (00:00:00-23:59:59)'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE WS-TIME-TEXT        TO CA-CL-LOSS-TIME
           END-IF.

      ******************************************************************
      * BR-CLM-CLM-002 The cause must sit under the claim's loss type
       3030-VALIDATE-CAUSE.
           MOVE 'LOSSCAUS'             TO WS-CODE-TABLE
           MOVE CA-CL-LOSS-CAUSE       TO WS-CODE-VALUE
           MOVE CA-CL-LOSS-TYPE        TO WS-CODE-PARENT
           PERFORM 8050-VALIDATE-CODE
           IF CA-CL-LOSS-CAUSE = SPACES OR NOT CDC-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CAUSEIV '          TO CA-REASON-CODE
              STRING 'Loss cause ' CA-CL-LOSS-CAUSE
                     ' is not valid for loss type ' CA-CL-LOSS-TYPE
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-CLM-003 A meaningful description is required
       3040-VALIDATE-DESCRIPTION.
           MOVE CA-CL-LOSS-DESC        TO WS-TRIM-TEXT
           PERFORM 5000-TRIM-LENGTH
           IF WS-TRIM-LEN < WS-MIN-DESC-LEN
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSDSRQ'          TO CA-REASON-CODE
              MOVE 'Loss description required (at least 10 chars)'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3050-VALIDATE-POSTCODE.
           MOVE FUNCTION UPPER-CASE(CA-CL-LOSS-POSTCODE)
                                       TO CA-CL-LOSS-POSTCODE
           IF CA-CL-LOSS-POSTCODE NOT = SPACES
              AND CA-CL-LOSS-POSTCODE(1:1) NOT ALPHABETIC
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PCODEINV'          TO CA-REASON-CODE
              MOVE 'Loss postcode is not a valid UK postcode'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Interpret the POLVRF result for FNOL / loss-date change
       3100-APPLY-VERIFICATION.
           MOVE 'N'                    TO WS-COVQ
                                          WS-OVERRIDE-SW
           EVALUATE TRUE
      * BR-CLM-CLM-004 Policy unknown to GenApp: allowed, in question
              WHEN POC-RC-NOT-FOUND
                 MOVE 'N'              TO WS-POL-FOUND-SW
                 MOVE 'Y'              TO WS-COVQ
                 MOVE CA-CL-POLICY-TYPE TO WS-POLICY-TYPE
                 MOVE CA-CL-GENAPP-CUST-NUM TO WS-CUST-NUM
                 IF WS-POLICY-TYPE NOT = 'M' AND NOT = 'H'
                    AND NOT = 'C'
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'POLTYPRQ'    TO CA-REASON-CODE
                    MOVE 'Policy not in GenApp - policy type M, H or C
      -                  ' must be given'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
                 MOVE 'POLUNVER'       TO WS-NW-REASON
                 MOVE 'Policy not found in GenApp - claim taken as cov
      -               'erage in question'
                                       TO WS-NW-MESSAGE
                 PERFORM 3900-ADD-WARNING
              WHEN POC-RETURN-CODE >= 12
                 MOVE POC-RETURN-CODE  TO CA-RETURN-CODE
                 MOVE POC-REASON-CODE  TO CA-REASON-CODE
                 MOVE POC-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
      * BR-CLM-CLM-005 Out-of-period loss needs an explicit override
              WHEN POC-PV-STATUS-AT-LOSS = 'OUTP'
                 SET WS-POLICY-FOUND   TO TRUE
                 IF CA-CL-COVERAGE-IN-QSTN = 'O' OR = 'Y'
                    MOVE 'Y'           TO WS-COVQ
                    SET WS-OVERRIDE-USED TO TRUE
                    MOVE 'COVQOVRD'    TO WS-NW-REASON
                    MOVE POC-MESSAGE   TO WS-NW-MESSAGE
                    PERFORM 3900-ADD-WARNING
                 ELSE
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'LOSSOUTP'    TO CA-REASON-CODE
                    MOVE POC-MESSAGE   TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
      * BR-CLM-CLM-007 Unverifiable cover or adverse billing
              WHEN NOT POC-PV-IS-VERIFIED
              WHEN POC-PV-STATUS-AT-LOSS = 'BILL'
                 SET WS-POLICY-FOUND   TO TRUE
                 MOVE 'Y'              TO WS-COVQ
                 MOVE POC-REASON-CODE  TO WS-NW-REASON
                 MOVE POC-MESSAGE      TO WS-NW-MESSAGE
                 PERFORM 3900-ADD-WARNING
              WHEN OTHER
                 SET WS-POLICY-FOUND   TO TRUE
                 IF POC-RC-WARNING
                    MOVE POC-REASON-CODE TO WS-NW-REASON
                    MOVE POC-MESSAGE   TO WS-NW-MESSAGE
                    PERFORM 3900-ADD-WARNING
                 END-IF
           END-EVALUATE
           IF WS-POLICY-FOUND
              MOVE POC-PV-POLICY-TYPE  TO WS-POLICY-TYPE
              MOVE POC-PV-CUSTOMER-NUM TO WS-CUST-NUM
           END-IF
           IF CA-CL-COVERAGE-IN-QSTN = 'Y'
              MOVE 'Y'                 TO WS-COVQ
           END-IF.

      ******************************************************************
      * BR-CLM-CLM-006 Policy line must be able to cover the loss type
       3200-CHECK-POLICY-LOB.
           EVALUATE TRUE
              WHEN WS-POLICY-TYPE = 'M' AND CA-CL-LOSS-TYPE = 'AUTO'
              WHEN WS-POLICY-TYPE = 'H' AND CA-CL-LOSS-TYPE = 'PROP'
              WHEN WS-POLICY-TYPE = 'C' AND CA-CL-LOSS-TYPE = 'PROP'
              WHEN WS-POLICY-TYPE = 'C' AND CA-CL-LOSS-TYPE = 'LIAB'
                 CONTINUE
              WHEN WS-POLICY-TYPE = 'E'
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'LIFEPOL '       TO CA-REASON-CODE
                 MOVE 'Endowment policies are not claimable in P&C'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'POLTYPMM'       TO CA-REASON-CODE
                 STRING 'Loss type ' CA-CL-LOSS-TYPE
                        ' is not covered by policy type '
                        WS-POLICY-TYPE DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-CLM-013 Load the claim and check the caller's version
       3300-LOAD-FOR-CHANGE.
           IF CA-CL-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMIDRQ '          TO CA-REASON-CODE
              MOVE 'Claim id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM-BY-ID
           IF CA-CL-VERSION-NO NOT = CLM-VERSION-NO
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Claim changed by another user - refresh and retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3400-VALIDATE-UPDATE.
           IF CA-CL-LOSS-DATE NOT = CLM-LOSS-DATE
              PERFORM 3010-VALIDATE-LOSS-DATE
      * BR-CLM-CLM-014 A new loss date means a new policy check
              SET WS-REVERIFY          TO TRUE
           END-IF
           PERFORM 3020-VALIDATE-LOSS-TIME
           IF CA-CL-LOSS-CAUSE NOT = CLM-LOSS-CAUSE
              PERFORM 3030-VALIDATE-CAUSE
           END-IF
           PERFORM 3040-VALIDATE-DESCRIPTION
           PERFORM 3050-VALIDATE-POSTCODE
           MOVE CA-CL-FAULT-RATING     TO WS-FAULT
           IF NOT WS-FAULT-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FAULTIV '          TO CA-REASON-CODE
              MOVE 'Fault rating must be UNKN INSD THRD SHRD or NOFT'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF (CA-CL-LITIGATION-FLAG NOT = 'Y' AND NOT = 'N')
              OR (CA-CL-TOTAL-LOSS-FLAG NOT = 'Y' AND NOT = 'N')
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FLAGIV  '          TO CA-REASON-CODE
              MOVE 'Litigation and total loss flags must be Y or N'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-CLM-015 Open matters keep the litigation flag on
           IF CA-CL-LITIGATION-FLAG = 'N'
              AND CLM-LITIGATION-FLAG = 'Y'
              PERFORM 4710-COUNT-OPEN-MATTERS
              IF WS-OPEN-MATTERS > 0
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'LITOPEN '       TO CA-REASON-CODE
                 MOVE 'Litigation flag cannot be cleared while a matter
      -               ' is open'       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF

           IF WS-REVERIFY
              MOVE CLM-GENAPP-POL-NUM  TO CA-CL-GENAPP-POL-NUM
              MOVE CLM-POLICY-TYPE     TO CA-CL-POLICY-TYPE
              MOVE CLM-GENAPP-CUST-NUM TO CA-CL-GENAPP-CUST-NUM
              PERFORM 8100-VERIFY-POLICY
              PERFORM 3100-APPLY-VERIFICATION
              PERFORM 3200-CHECK-POLICY-LOB
           ELSE
              PERFORM 3410-COVERAGE-QUESTION
           END-IF

           MOVE CLM-CAT-CODE           TO WS-CAT-CODE
           IF WS-REVERIFY
              OR CA-CL-LOSS-POSTCODE NOT = CLM-LOSS-POSTCODE
              MOVE CA-CL-LOSS-POSTCODE TO WS-CAT-POSTCODE
              MOVE SPACES              TO WS-CAT-CODE
              PERFORM 4400-FIND-CATASTROPHE
           END-IF.

      ******************************************************************
      * BR-CLM-CLM-016 Adjuster may raise the coverage question at any
      * time but may clear it only when the policy snapshot is verified
       3410-COVERAGE-QUESTION.
           MOVE CLM-COVERAGE-IN-QSTN   TO WS-COVQ
           EVALUATE CA-CL-COVERAGE-IN-QSTN
              WHEN 'Y'
                 MOVE 'Y'              TO WS-COVQ
              WHEN 'N'
                 IF CLM-COVERAGE-IN-QSTN = 'Y'
                    PERFORM 4520-READ-SNAPSHOT-FLAG
                    IF WS-VERIFIED-FLAG NOT = 'Y'
                       SET CA-RC-STATE TO TRUE
                       MOVE 'COVQUNVR' TO CA-REASON-CODE
                       MOVE 'Policy unverified - coverage question cann
      -                     'ot be cleared'
                                       TO CA-MESSAGE
                       PERFORM 9000-RETURN
                    END-IF
                    MOVE 'N'           TO WS-COVQ
                 END-IF
           END-EVALUATE.

      ******************************************************************
      * First warning raised is the one returned
       3900-ADD-WARNING.
           IF WS-WARN-REASON = SPACES
              MOVE WS-NW-REASON        TO WS-WARN-REASON
              MOVE WS-NW-MESSAGE       TO WS-WARN-MESSAGE
           END-IF
           MOVE SPACES                 TO WS-NEW-WARNING.

      ******************************************************************
       3950-FINAL-WARNING.
           IF CA-RC-OK AND WS-WARN-REASON NOT = SPACES
              SET CA-RC-WARNING        TO TRUE
              MOVE WS-WARN-REASON      TO CA-REASON-CODE
              MOVE WS-WARN-MESSAGE     TO CA-MESSAGE
           END-IF.

      ******************************************************************
       4000-INSERT-CLAIM.
           EXEC SQL
              INSERT INTO CLMDB.CLM_CLAIM
                    (CLAIM_ID, CLAIM_NUMBER, GENAPP_POL_NUM,
                     GENAPP_CUST_NUM, POLICY_TYPE, LOSS_TYPE,
                     LOSS_DATE, LOSS_TIME, REPORTED_DATE,
                     HOW_REPORTED, LOSS_CAUSE, LOSS_DESC,
                     LOSS_ADDR_LINE1, LOSS_POSTCODE, CLAIM_STATE,
                     CLOSE_REASON, SEGMENT_CODE, SEVERITY,
                     FAULT_RATING, COVERAGE_IN_QSTN, SIU_SCORE,
                     SIU_STATUS, LITIGATION_FLAG, CAT_CODE,
                     TOTAL_LOSS_FLAG, DUP_SUSPECT_FLAG, ASSIGNED_USER,
                     ASSIGNED_GROUP, TOTAL_RESERVES, TOTAL_PAID,
                     TOTAL_RECOVERED, TOTAL_INCURRED, OPEN_DATE,
                     CLOSE_DATE, REOPEN_COUNT, CREATE_USER,
                     CREATE_TS, UPDATE_TS, VERSION_NO)
              VALUES (:CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                      :CLM-GENAPP-POL-NUM, :CLM-GENAPP-CUST-NUM,
                      :CLM-POLICY-TYPE, :CLM-LOSS-TYPE,
                      :CLM-LOSS-DATE,
                      :CLM-LOSS-TIME :CLM-LOSS-TIME-IND,
                      :CLM-REPORTED-DATE, :CLM-HOW-REPORTED,
                      :CLM-LOSS-CAUSE, :CLM-LOSS-DESC,
                      :CLM-LOSS-ADDR-LINE1 :CLM-LOSS-ADDR-LINE1-IND,
                      :CLM-LOSS-POSTCODE :CLM-LOSS-POSTCODE-IND,
                      :CLM-CLAIM-STATE, :CLM-CLOSE-REASON,
                      :CLM-SEGMENT-CODE, :CLM-SEVERITY,
                      :CLM-FAULT-RATING, :CLM-COVERAGE-IN-QSTN,
                      :CLM-SIU-SCORE, :CLM-SIU-STATUS,
                      :CLM-LITIGATION-FLAG, :CLM-CAT-CODE,
                      :CLM-TOTAL-LOSS-FLAG, :CLM-DUP-SUSPECT-FLAG,
                      :CLM-ASSIGNED-USER, :CLM-ASSIGNED-GROUP,
                      :CLM-TOTAL-RESERVES, :CLM-TOTAL-PAID,
                      :CLM-TOTAL-RECOVERED, :CLM-TOTAL-INCURRED,
                      :CLM-OPEN-DATE :CLM-OPEN-DATE-IND,
                      :CLM-CLOSE-DATE :CLM-CLOSE-DATE-IND,
                      :CLM-REOPEN-COUNT, :CLM-CREATE-USER,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP,
                      :CLM-VERSION-NO)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 STRING 'Claim number ' CLM-CLAIM-NUMBER
                        ' already exists - sequence out of step'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-INSERT-CLAIM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Policy as it stood at FNOL - never refreshed from GenApp later
      * except when the loss date itself is corrected
       4010-INSERT-SNAPSHOT.
           PERFORM 4015-BUILD-SNAPSHOT
           EXEC SQL
              INSERT INTO CLMDB.CLM_POLICY_SNAPSHOT
                    (CLAIM_ID, POLICY_NUMBER, POLICY_TYPE, EFF_DATE,
                     EXP_DATE, VERIFIED_FLAG, INSURED_NAME,
                     PRODUCER_CODE, STATUS_AT_LOSS, BILLING_STATUS,
                     SNAPSHOT_TS)
              VALUES (:PSN-CLAIM-ID, :PSN-POLICY-NUMBER,
                      :PSN-POLICY-TYPE, :PSN-EFF-DATE, :PSN-EXP-DATE,
                      :PSN-VERIFIED-FLAG, :PSN-INSURED-NAME,
                      :PSN-PRODUCER-CODE, :PSN-STATUS-AT-LOSS,
                      :PSN-BILLING-STATUS, CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4010-INSERT-SNAPSHOT' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4015-BUILD-SNAPSHOT.
           MOVE CLM-CLAIM-ID           TO PSN-CLAIM-ID
           MOVE CLM-GENAPP-POL-NUM     TO PSN-POLICY-NUMBER
           MOVE WS-POLICY-TYPE         TO PSN-POLICY-TYPE
           IF WS-POLICY-FOUND
              MOVE POC-PV-EFF-DATE     TO PSN-EFF-DATE
              MOVE POC-PV-EXP-DATE     TO PSN-EXP-DATE
              MOVE POC-PV-VERIFIED     TO PSN-VERIFIED-FLAG
              MOVE POC-PV-INSURED-NAME TO WS-TRIM-TEXT
              MOVE POC-PV-PRODUCER-CODE TO PSN-PRODUCER-CODE
              MOVE POC-PV-STATUS-AT-LOSS TO PSN-STATUS-AT-LOSS
              MOVE POC-PV-BILLING-STATUS TO PSN-BILLING-STATUS
           ELSE
              MOVE SPACES              TO PSN-EFF-DATE
              MOVE SPACES              TO PSN-EXP-DATE
              MOVE 'N'                 TO PSN-VERIFIED-FLAG
              MOVE 'UNVERIFIED - POLICY NOT IN GENAPP'
                                       TO WS-TRIM-TEXT
              MOVE 0                   TO PSN-PRODUCER-CODE
              MOVE 'NFND'              TO PSN-STATUS-AT-LOSS
              MOVE 'UNKN'              TO PSN-BILLING-STATUS
           END-IF
           IF PSN-EFF-DATE = SPACES
              MOVE CA-CL-LOSS-DATE     TO PSN-EFF-DATE
           END-IF
           IF PSN-EXP-DATE = SPACES
              MOVE CA-CL-LOSS-DATE     TO PSN-EXP-DATE
           END-IF
           PERFORM 5000-TRIM-LENGTH
           IF WS-TRIM-LEN = 0
              MOVE 'UNKNOWN'           TO WS-TRIM-TEXT
              MOVE 7                   TO WS-TRIM-LEN
           END-IF
           IF WS-TRIM-LEN > 60
              MOVE 60                  TO WS-TRIM-LEN
           END-IF
           MOVE WS-TRIM-LEN            TO PSN-INSURED-NAME-LEN
           MOVE WS-TRIM-TEXT(1:60)     TO PSN-INSURED-NAME-TEXT.

      ******************************************************************
       4020-INSERT-COVERAGES.
           IF WS-POLICY-FOUND
              PERFORM VARYING WS-IX FROM 1 BY 1
                      UNTIL WS-IX > POC-PV-COV-COUNT
                 MOVE CLM-CLAIM-ID     TO PCV-CLAIM-ID
                 MOVE WS-IX            TO PCV-COV-SEQ
                 MOVE POC-PV-C-TYPE (WS-IX)   TO PCV-COVERAGE-TYPE
                 MOVE POC-PV-C-RU-SEQ (WS-IX) TO PCV-RISK-UNIT-SEQ
                 MOVE POC-PV-C-LIMIT (WS-IX)  TO PCV-LIMIT-AMOUNT
                 MOVE POC-PV-C-DEDUCTIBLE (WS-IX)
                                       TO PCV-DEDUCTIBLE-AMOUNT
                 EXEC SQL
                    INSERT INTO CLMDB.CLM_POLICY_COVERAGE
                          (CLAIM_ID, COV_SEQ, COVERAGE_TYPE,
                           RISK_UNIT_SEQ, LIMIT_AMOUNT,
                           DEDUCTIBLE_AMOUNT)
                    VALUES (:PCV-CLAIM-ID, :PCV-COV-SEQ,
                            :PCV-COVERAGE-TYPE, :PCV-RISK-UNIT-SEQ,
                            :PCV-LIMIT-AMOUNT, :PCV-DEDUCTIBLE-AMOUNT)
                 END-EXEC
                 IF SQLCODE NOT = 0
                    MOVE '4020-INSERT-COVERAGES' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
                 END-IF
              END-PERFORM
           END-IF.

      ******************************************************************
       4030-INSERT-RISK-UNITS.
           IF WS-POLICY-FOUND
              PERFORM VARYING WS-IX FROM 1 BY 1
                      UNTIL WS-IX > POC-PV-RU-COUNT
                 MOVE CLM-CLAIM-ID     TO RSK-CLAIM-ID
                 MOVE WS-IX            TO RSK-RISK-UNIT-SEQ
                 MOVE POC-PV-R-TYPE (WS-IX)  TO RSK-RISK-UNIT-TYPE
                 MOVE POC-PV-R-DESC (WS-IX)  TO WS-TRIM-TEXT
                 PERFORM 5000-TRIM-LENGTH
                 IF WS-TRIM-LEN = 0
                    MOVE POC-PV-R-TYPE (WS-IX) TO WS-TRIM-TEXT
                    MOVE 4             TO WS-TRIM-LEN
                 END-IF
                 MOVE WS-TRIM-LEN      TO RSK-DESCRIPTION-LEN
                 MOVE WS-TRIM-TEXT(1:60) TO RSK-DESCRIPTION-TEXT
                 MOVE POC-PV-R-REG (WS-IX)   TO RSK-VEHICLE-REG
                 MOVE POC-PV-R-MAKE (WS-IX)  TO RSK-VEHICLE-MAKE
                 MOVE POC-PV-R-MODEL (WS-IX) TO RSK-VEHICLE-MODEL
                 MOVE POC-PV-R-POSTCODE (WS-IX)
                                       TO RSK-PROPERTY-POSTCODE
                 MOVE POC-PV-R-VALUE (WS-IX) TO RSK-INSURED-VALUE
                 EXEC SQL
                    INSERT INTO CLMDB.CLM_RISK_UNIT
                          (CLAIM_ID, RISK_UNIT_SEQ, RISK_UNIT_TYPE,
                           DESCRIPTION, VEHICLE_REG, VEHICLE_MAKE,
                           VEHICLE_MODEL, PROPERTY_POSTCODE,
                           INSURED_VALUE)
                    VALUES (:RSK-CLAIM-ID, :RSK-RISK-UNIT-SEQ,
                            :RSK-RISK-UNIT-TYPE, :RSK-DESCRIPTION,
                            :RSK-VEHICLE-REG, :RSK-VEHICLE-MAKE,
                            :RSK-VEHICLE-MODEL,
                            :RSK-PROPERTY-POSTCODE,
                            :RSK-INSURED-VALUE)
                 END-EXEC
                 IF SQLCODE NOT = 0
                    MOVE '4030-INSERT-RISK-UNITS' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
                 END-IF
              END-PERFORM
           END-IF.

      ******************************************************************
       4100-READ-CLAIM-BY-ID.
           MOVE CA-CL-CLAIM-ID         TO CLM-CLAIM-ID
                                          WS-KEY-DISP
           EXEC SQL
              SELECT CLAIM_ID, CLAIM_NUMBER, GENAPP_POL_NUM,
                     GENAPP_CUST_NUM, POLICY_TYPE, LOSS_TYPE,
                     LOSS_DATE, LOSS_TIME, REPORTED_DATE,
                     HOW_REPORTED, LOSS_CAUSE, LOSS_DESC,
                     LOSS_ADDR_LINE1, LOSS_POSTCODE, CLAIM_STATE,
                     CLOSE_REASON, SEGMENT_CODE, SEVERITY,
                     FAULT_RATING, COVERAGE_IN_QSTN, SIU_SCORE,
                     SIU_STATUS, LITIGATION_FLAG, CAT_CODE,
                     TOTAL_LOSS_FLAG, DUP_SUSPECT_FLAG, ASSIGNED_USER,
                     ASSIGNED_GROUP, TOTAL_RESERVES, TOTAL_PAID,
                     TOTAL_RECOVERED, TOTAL_INCURRED, OPEN_DATE,
                     CLOSE_DATE, REOPEN_COUNT, VERSION_NO
                INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                     :CLM-GENAPP-POL-NUM, :CLM-GENAPP-CUST-NUM,
                     :CLM-POLICY-TYPE, :CLM-LOSS-TYPE, :CLM-LOSS-DATE,
                     :CLM-LOSS-TIME :CLM-LOSS-TIME-IND,
                     :CLM-REPORTED-DATE, :CLM-HOW-REPORTED,
                     :CLM-LOSS-CAUSE, :CLM-LOSS-DESC,
                     :CLM-LOSS-ADDR-LINE1 :CLM-LOSS-ADDR-LINE1-IND,
                     :CLM-LOSS-POSTCODE :CLM-LOSS-POSTCODE-IND,
                     :CLM-CLAIM-STATE, :CLM-CLOSE-REASON,
                     :CLM-SEGMENT-CODE, :CLM-SEVERITY,
                     :CLM-FAULT-RATING, :CLM-COVERAGE-IN-QSTN,
                     :CLM-SIU-SCORE, :CLM-SIU-STATUS,
                     :CLM-LITIGATION-FLAG, :CLM-CAT-CODE,
                     :CLM-TOTAL-LOSS-FLAG, :CLM-DUP-SUSPECT-FLAG,
                     :CLM-ASSIGNED-USER, :CLM-ASSIGNED-GROUP,
                     :CLM-TOTAL-RESERVES, :CLM-TOTAL-PAID,
                     :CLM-TOTAL-RECOVERED, :CLM-TOTAL-INCURRED,
                     :CLM-OPEN-DATE :CLM-OPEN-DATE-IND,
                     :CLM-CLOSE-DATE :CLM-CLOSE-DATE-IND,
                     :CLM-REOPEN-COUNT, :CLM-VERSION-NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           PERFORM 4150-CHECK-CLAIM-READ.

      ******************************************************************
       4110-READ-CLAIM-BY-NUMBER.
           MOVE CA-CL-CLAIM-NUMBER     TO CLM-CLAIM-NUMBER
           EXEC SQL
              SELECT CLAIM_ID
                INTO :CLM-CLAIM-ID
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_NUMBER = :CLM-CLAIM-NUMBER
           END-EXEC
           PERFORM 4150-CHECK-CLAIM-READ
           MOVE CLM-CLAIM-ID           TO CA-CL-CLAIM-ID
           PERFORM 4100-READ-CLAIM-BY-ID.

      ******************************************************************
       4150-CHECK-CLAIM-READ.
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 IF CA-CL-CLAIM-ID > 0
                    STRING 'Claim ' WS-KEY-DISP ' not found'
                           DELIMITED BY SIZE INTO CA-MESSAGE
                 ELSE
                    STRING 'Claim ' CA-CL-CLAIM-NUMBER ' not found'
                           DELIMITED BY SIZE INTO CA-MESSAGE
                 END-IF
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Claim row locked - retry' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4150-CHECK-CLAIM-READ' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-OPEN-CLAIM.
           MOVE 'OPEN'                 TO CLM-CLAIM-STATE
           MOVE WS-BUS-DATE            TO CLM-OPEN-DATE
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET CLAIM_STATE = 'OPEN',
                     OPEN_DATE   = :CLM-OPEN-DATE,
                     UPDATE_TS   = CURRENT TIMESTAMP,
                     VERSION_NO  = VERSION_NO + 1
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
           END-EXEC
           MOVE '4200-OPEN-CLAIM'      TO ERC-ER-PARAGRAPH
           PERFORM 4990-CHECK-UPDATE.

      ******************************************************************
      * BR-CLM-CLM-008 Same policy, same cause, loss within +/- 3 days;
      * claims already closed as duplicates do not count
       4300-CHECK-DUPLICATE.
           MOVE 0                      TO WS-DUP-COUNT
           MOVE SPACES                 TO WS-DUP-CLAIM-NO
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTC-REQUEST-ID
           MOVE CA-CL-LOSS-DATE        TO DTC-DT-DATE-1
           COMPUTE DTC-DT-NUMBER = 0 - WS-DUP-WINDOW
           PERFORM 8060-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO WS-DUP-FROM
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTC-REQUEST-ID
           MOVE CA-CL-LOSS-DATE        TO DTC-DT-DATE-1
           MOVE WS-DUP-WINDOW          TO DTC-DT-NUMBER
           PERFORM 8060-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO WS-DUP-TO
           MOVE CA-CL-LOSS-CAUSE       TO CLM-LOSS-CAUSE
           EXEC SQL
              SELECT COUNT(*), MIN(CLAIM_NUMBER)
                INTO :WS-DUP-COUNT, :WS-DUP-CLAIM-NO :WS-DUP-IND
                FROM CLMDB.CLM_CLAIM
               WHERE GENAPP_POL_NUM = :CA-CL-GENAPP-POL-NUM
                 AND LOSS_DATE BETWEEN :WS-DUP-FROM AND :WS-DUP-TO
                 AND LOSS_CAUSE     = :CLM-LOSS-CAUSE
                 AND CLOSE_REASON  <> 'DUPLICAT'
                 AND CLAIM_ID      <> :CLM-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-CHECK-DUPLICATE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-CLM-009 Postcode area (letters) or district (outward
      * code) inside an active catastrophe window on the loss date
       4400-FIND-CATASTROPHE.
           IF WS-CAT-POSTCODE NOT = SPACES
              PERFORM 5200-DERIVE-POSTCODE-AREA
              EXEC SQL
                 SELECT CAT_CODE
                   INTO :CAT-CAT-CODE
                   FROM CLMDB.CLM_CATASTROPHE
                  WHERE ACTIVE_FLAG   = 'Y'
                    AND POSTCODE_AREA IN (:WS-AREA, :WS-OUTWARD)
                    AND START_DATE   <= :CA-CL-LOSS-DATE
                    AND END_DATE     >= :CA-CL-LOSS-DATE
                  ORDER BY START_DATE DESC, CAT_CODE
                  FETCH FIRST 1 ROW ONLY
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE CAT-CAT-CODE  TO WS-CAT-CODE
                 WHEN 100
                    MOVE SPACES        TO WS-CAT-CODE
                 WHEN OTHER
                    MOVE '4400-FIND-CATASTROPHE' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
       4500-UPDATE-CLAIM.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET LOSS_DATE        = :CLM-LOSS-DATE,
                     LOSS_TIME        = :CLM-LOSS-TIME
                                        :CLM-LOSS-TIME-IND,
                     LOSS_CAUSE       = :CLM-LOSS-CAUSE,
                     LOSS_DESC        = :CLM-LOSS-DESC,
                     LOSS_ADDR_LINE1  = :CLM-LOSS-ADDR-LINE1
                                        :CLM-LOSS-ADDR-LINE1-IND,
                     LOSS_POSTCODE    = :CLM-LOSS-POSTCODE
                                        :CLM-LOSS-POSTCODE-IND,
                     FAULT_RATING     = :CLM-FAULT-RATING,
                     LITIGATION_FLAG  = :CLM-LITIGATION-FLAG,
                     TOTAL_LOSS_FLAG  = :CLM-TOTAL-LOSS-FLAG,
                     COVERAGE_IN_QSTN = :CLM-COVERAGE-IN-QSTN,
                     CAT_CODE         = :CLM-CAT-CODE,
                     UPDATE_TS        = CURRENT TIMESTAMP,
                     VERSION_NO       = VERSION_NO + 1
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
           END-EXEC
           MOVE '4500-UPDATE-CLAIM'    TO ERC-ER-PARAGRAPH
           PERFORM 4990-CHECK-UPDATE.

      ******************************************************************
       4510-UPDATE-SNAPSHOT.
           PERFORM 4015-BUILD-SNAPSHOT
           EXEC SQL
              UPDATE CLMDB.CLM_POLICY_SNAPSHOT
                 SET EFF_DATE       = :PSN-EFF-DATE,
                     EXP_DATE       = :PSN-EXP-DATE,
                     VERIFIED_FLAG  = :PSN-VERIFIED-FLAG,
                     STATUS_AT_LOSS = :PSN-STATUS-AT-LOSS,
                     BILLING_STATUS = :PSN-BILLING-STATUS,
                     SNAPSHOT_TS    = CURRENT TIMESTAMP
               WHERE CLAIM_ID = :PSN-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4510-UPDATE-SNAPSHOT' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4520-READ-SNAPSHOT-FLAG.
           EXEC SQL
              SELECT VERIFIED_FLAG
                INTO :WS-VERIFIED-FLAG
                FROM CLMDB.CLM_POLICY_SNAPSHOT
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 'N'              TO WS-VERIFIED-FLAG
              WHEN OTHER
                 MOVE '4520-READ-SNAPSHOT-FLAG' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4600-READ-GENAPP-CUSTOMER.
           MOVE CLM-GENAPP-CUST-NUM    TO GCU-CUSTOMERNUMBER
           EXEC SQL
              SELECT FIRSTNAME, LASTNAME, DATEOFBIRTH, HOUSENAME,
                     HOUSENUMBER, POSTCODE, PHONEHOME, PHONEMOBILE,
                     EMAILADDRESS
                INTO :GCU-FIRSTNAME    :GCU-FIRSTNAME-IND,
                     :GCU-LASTNAME     :GCU-LASTNAME-IND,
                     :GCU-DATEOFBIRTH  :GCU-DATEOFBIRTH-IND,
                     :GCU-HOUSENAME    :GCU-HOUSENAME-IND,
                     :GCU-HOUSENUMBER  :GCU-HOUSENUMBER-IND,
                     :GCU-POSTCODE     :GCU-POSTCODE-IND,
                     :GCU-PHONEHOME    :GCU-PHONEHOME-IND,
                     :GCU-PHONEMOBILE  :GCU-PHONEMOBILE-IND,
                     :GCU-EMAILADDRESS :GCU-EMAILADDRESS-IND
                FROM GENAPP.CUSTOMER
               WHERE CUSTOMERNUMBER = :GCU-CUSTOMERNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF GCU-FIRSTNAME-IND < 0
                    MOVE SPACES        TO GCU-FIRSTNAME
                 END-IF
                 IF GCU-LASTNAME-IND < 0
                    MOVE SPACES        TO GCU-LASTNAME
                 END-IF
                 IF GCU-DATEOFBIRTH-IND < 0
                    MOVE SPACES        TO GCU-DATEOFBIRTH
                 END-IF
                 IF GCU-HOUSENAME-IND < 0
                    MOVE SPACES        TO GCU-HOUSENAME
                 END-IF
                 IF GCU-HOUSENUMBER-IND < 0
                    MOVE SPACES        TO GCU-HOUSENUMBER
                 END-IF
                 IF GCU-POSTCODE-IND < 0
                    MOVE SPACES        TO GCU-POSTCODE
                 END-IF
                 IF GCU-PHONEHOME-IND < 0
                    MOVE SPACES        TO GCU-PHONEHOME
                 END-IF
                 IF GCU-PHONEMOBILE-IND < 0
                    MOVE SPACES        TO GCU-PHONEMOBILE
                 END-IF
                 IF GCU-EMAILADDRESS-IND < 0
                    MOVE SPACES        TO GCU-EMAILADDRESS
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4600-READ-GENAPP-CUSTOMER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-CLM-018 Everything financial and procedural must be
      * finished before the claim can close
       4700-CHECK-CLOSE-BLOCKERS.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_EXPOSURE
               WHERE CLAIM_ID       = :CLM-CLAIM-ID
                 AND EXPOSURE_STATE <> 'CLSD'
           END-EXEC
           MOVE 'EXPOPEN '             TO WS-NW-REASON
           MOVE 'exposure(s) still open' TO WS-NW-MESSAGE
           PERFORM 4790-BLOCK-IF-COUNT

           EXEC SQL
              SELECT COALESCE(SUM(OPEN_RESERVE + PENDING_APPROVAL), 0)
                INTO :WS-AMOUNT
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4700-CHECK-CLOSE-BLOCKERS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-AMOUNT NOT = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'RSVOPEN '          TO CA-REASON-CODE
              MOVE 'Open or pending reserve remains - zero it first'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_TRANSACTION
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND TXN_STATUS IN ('PEND', 'PAPR')
           END-EXEC
           MOVE 'TXNPEND '             TO WS-NW-REASON
           MOVE 'transaction(s) awaiting approval' TO WS-NW-MESSAGE
           PERFORM 4790-BLOCK-IF-COUNT

           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_CHECK
               WHERE CLAIM_ID     = :CLM-CLAIM-ID
                 AND CHECK_STATUS = 'PEND'
           END-EXEC
           MOVE 'CHKPEND '             TO WS-NW-REASON
           MOVE 'check(s) not yet issued' TO WS-NW-MESSAGE
           PERFORM 4790-BLOCK-IF-COUNT

           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_ACTIVITY A,
                     CLMDB.CLM_ACTIVITY_PATTERN P
               WHERE A.CLAIM_ID        = :CLM-CLAIM-ID
                 AND A.ACTIVITY_STATUS = 'OPEN'
                 AND P.PATTERN_CODE    = A.PATTERN_CODE
                 AND P.MANDATORY_FLAG  = 'Y'
           END-EXEC
           MOVE 'ACTMAND '             TO WS-NW-REASON
           MOVE 'mandatory activity(ies) still open' TO WS-NW-MESSAGE
           PERFORM 4790-BLOCK-IF-COUNT

           PERFORM 4710-COUNT-OPEN-MATTERS
           MOVE WS-OPEN-MATTERS        TO WS-COUNT
           MOVE 'LITOPEN '             TO WS-NW-REASON
           MOVE 'litigation matter(s) still open' TO WS-NW-MESSAGE
           PERFORM 4790-BLOCK-IF-COUNT
           MOVE SPACES                 TO WS-NEW-WARNING.

      ******************************************************************
       4710-COUNT-OPEN-MATTERS.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-OPEN-MATTERS
                FROM CLMDB.CLM_LITIGATION
               WHERE CLAIM_ID      = :CLM-CLAIM-ID
                 AND MATTER_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4710-COUNT-OPEN-MATTERS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4720-SKIP-OPEN-ACTIVITIES.
           EXEC SQL
              UPDATE CLMDB.CLM_ACTIVITY
                 SET ACTIVITY_STATUS = 'SKIP',
                     COMPLETE_TS     = CURRENT TIMESTAMP,
                     COMPLETE_USER   = :CA-USER-ID
               WHERE CLAIM_ID        = :CLM-CLAIM-ID
                 AND ACTIVITY_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4720-SKIP-OPEN-ACTIVITIES' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4730-CLOSE-CLAIM.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET CLAIM_STATE  = 'CLSD',
                     CLOSE_REASON = :CLM-CLOSE-REASON,
                     CLOSE_DATE   = :CLM-CLOSE-DATE,
                     UPDATE_TS    = CURRENT TIMESTAMP,
                     VERSION_NO   = VERSION_NO + 1
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
           END-EXEC
           MOVE '4730-CLOSE-CLAIM'     TO ERC-ER-PARAGRAPH
           PERFORM 4990-CHECK-UPDATE.

      ******************************************************************
      * BR-CLM-CLM-020 Closed claims stop counting against capacity
       4740-RELEASE-CAPACITY.
           EXEC SQL
              UPDATE CLMDB.CLM_USER
                 SET OPEN_CLAIMS = OPEN_CLAIMS - 1
               WHERE USER_ID     = :USR-USER-ID
                 AND OPEN_CLAIMS > 0
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4740-RELEASE-CAPACITY' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4750-REOPEN-CLAIM.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET CLAIM_STATE    = 'OPEN',
                     CLOSE_REASON   = ' ',
                     CLOSE_DATE     = NULL,
                     REOPEN_COUNT   = :CLM-REOPEN-COUNT,
                     ASSIGNED_USER  = :CLM-ASSIGNED-USER,
                     ASSIGNED_GROUP = :CLM-ASSIGNED-GROUP,
                     UPDATE_TS      = CURRENT TIMESTAMP,
                     VERSION_NO     = VERSION_NO + 1
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
           END-EXEC
           MOVE '4750-REOPEN-CLAIM'    TO ERC-ER-PARAGRAPH
           PERFORM 4990-CHECK-UPDATE.

      ******************************************************************
       4790-BLOCK-IF-COUNT.
           IF SQLCODE NOT = 0
              MOVE '4790-BLOCK-IF-COUNT' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              MOVE WS-COUNT            TO WS-COUNT-DISP
              SET CA-RC-STATE          TO TRUE
              MOVE WS-NW-REASON        TO CA-REASON-CODE
              STRING 'Cannot close: ' WS-COUNT-DISP ' '
                     WS-NW-MESSAGE DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4800-READ-USER.
           EXEC SQL
              SELECT ACTIVE_FLAG, GROUP_CODE
                INTO :USR-ACTIVE-FLAG, :USR-GROUP-CODE
                FROM CLMDB.CLM_USER
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 'N'              TO USR-ACTIVE-FLAG
              WHEN OTHER
                 MOVE '4800-READ-USER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4810-CLAIM-CAPACITY.
           EXEC SQL
              UPDATE CLMDB.CLM_USER
                 SET OPEN_CLAIMS    = OPEN_CLAIMS + 1,
                     LAST_ASSIGN_TS = CURRENT TIMESTAMP
               WHERE USER_ID = :USR-USER-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4810-CLAIM-CAPACITY' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4900-READ-CLAIM-CONTACTS.
           EXEC SQL
              SELECT MIN(CONTACT_ID)
                INTO :CON-CONTACT-ID :WS-NULL-IND
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID     = :CLM-CLAIM-ID
                 AND CONTACT_ROLE = 'INSURED'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-READ-CLAIM-CONTACTS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-NULL-IND < 0
              MOVE 0                   TO CA-CL-INSURED-CONTACT-ID
           ELSE
              MOVE CON-CONTACT-ID      TO CA-CL-INSURED-CONTACT-ID
           END-IF

           EXEC SQL
              SELECT C.FIRST_NAME, C.LAST_NAME, C.PHONE_NUMBER
                INTO :CON-FIRST-NAME   :CON-FIRST-NAME-IND,
                     :CON-LAST-NAME    :CON-LAST-NAME-IND,
                     :CON-PHONE-NUMBER :CON-PHONE-NUMBER-IND
                FROM CLMDB.CLM_CLAIM_CONTACT L,
                     CLMDB.CLM_CONTACT C
               WHERE L.CLAIM_ID     = :CLM-CLAIM-ID
                 AND L.CONTACT_ROLE = 'REPORTER'
                 AND C.CONTACT_ID   = L.CONTACT_ID
               ORDER BY L.ADDED_TS
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE SPACES           TO CA-CL-RPT-FIRST-NAME
                                          CA-CL-RPT-LAST-NAME
                                          CA-CL-RPT-PHONE
                 IF CON-FIRST-NAME-IND >= 0
                    AND CON-FIRST-NAME-LEN > 0
                    MOVE CON-FIRST-NAME-TEXT(1:CON-FIRST-NAME-LEN)
                                       TO CA-CL-RPT-FIRST-NAME
                 END-IF
                 IF CON-LAST-NAME-IND >= 0
                    AND CON-LAST-NAME-LEN > 0
                    MOVE CON-LAST-NAME-TEXT(1:CON-LAST-NAME-LEN)
                                       TO CA-CL-RPT-LAST-NAME
                 END-IF
                 IF CON-PHONE-NUMBER-IND >= 0
                    MOVE CON-PHONE-NUMBER TO CA-CL-RPT-PHONE
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4900-READ-CLAIM-CONTACTS' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Exposure summary: first 10 by sequence, total count returned
       4950-LIST-EXPOSURES.
           MOVE 0                      TO CA-CL-EXP-COUNT
                                          WS-EXP-TOTAL
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN CSR-EXPOSURES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4950-LIST-EXPOSURES' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-END-OF-CURSOR
              EXEC SQL
                 FETCH CSR-EXPOSURES
                  INTO :EXP-EXPOSURE-ID, :EXP-EXPOSURE-SEQ,
                       :EXP-EXPOSURE-TYPE, :EXP-COVERAGE-TYPE,
                       :EXP-EXPOSURE-STATE, :EXP-OPEN-RESERVE,
                       :EXP-TOTAL-PAID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-EXP-TOTAL
                    IF CA-CL-EXP-COUNT < 10
                       ADD 1           TO CA-CL-EXP-COUNT
                       MOVE CA-CL-EXP-COUNT TO WS-IX
                       MOVE EXP-EXPOSURE-ID
                                 TO CA-CL-X-EXPOSURE-ID (WS-IX)
                       MOVE EXP-EXPOSURE-SEQ  TO CA-CL-X-SEQ (WS-IX)
                       MOVE EXP-EXPOSURE-TYPE TO CA-CL-X-TYPE (WS-IX)
                       MOVE EXP-COVERAGE-TYPE
                                 TO CA-CL-X-COVERAGE (WS-IX)
                       MOVE EXP-EXPOSURE-STATE
                                 TO CA-CL-X-STATE (WS-IX)
                       MOVE EXP-OPEN-RESERVE
                                 TO CA-CL-X-OPEN-RESERVE (WS-IX)
                       MOVE EXP-TOTAL-PAID TO CA-CL-X-PAID (WS-IX)
                    END-IF
                 WHEN 100
                    SET WS-END-OF-CURSOR TO TRUE
                 WHEN OTHER
                    EXEC SQL CLOSE CSR-EXPOSURES END-EXEC
                    MOVE '4950-LIST-EXPOSURES' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-EXPOSURES END-EXEC
           IF WS-EXP-TOTAL > 10
              MOVE WS-EXP-TOTAL        TO WS-COUNT-DISP
              MOVE 'EXPTRUNC'          TO WS-NW-REASON
              STRING 'First 10 of ' WS-COUNT-DISP ' exposures shown'
                     DELIMITED BY SIZE INTO WS-NW-MESSAGE
              PERFORM 3900-ADD-WARNING
           END-IF.

      ******************************************************************
      * Shared outcome of an optimistic-lock UPDATE on CLM_CLAIM
       4990-CHECK-UPDATE.
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 0
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Claim changed by another user - refresh and retr
      -               'y'              TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = 0
                 ADD 1                 TO CLM-VERSION-NO
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Claim row locked - retry' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       5000-TRIM-LENGTH.
           MOVE 0                      TO WS-TRIM-LEN
           IF WS-TRIM-TEXT NOT = SPACES
              INSPECT FUNCTION REVERSE(WS-TRIM-TEXT)
                      TALLYING WS-TRIM-LEN FOR LEADING SPACES
              COMPUTE WS-TRIM-LEN = LENGTH OF WS-TRIM-TEXT
                                  - WS-TRIM-LEN
           END-IF.

      ******************************************************************
      * Changes are held until the UPDATE succeeds, then written out
       5100-NOTE-CHANGE.
           IF WS-CHG-COUNT < 12
              ADD 1                    TO WS-CHG-COUNT
              MOVE WS-CHG-FIELD        TO WS-CE-FIELD (WS-CHG-COUNT)
              MOVE WS-CHG-OLD          TO WS-CE-OLD (WS-CHG-COUNT)
              MOVE WS-CHG-NEW          TO WS-CE-NEW (WS-CHG-COUNT)
              STRING WS-CHG-FIELD DELIMITED BY SPACE
                     ';'          DELIMITED BY SIZE
                     INTO WS-CHG-LIST WITH POINTER WS-CHG-PTR
           END-IF.

      ******************************************************************
       5150-WRITE-CHANGE-HISTORY.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CHG-COUNT
              MOVE 'CLMUPD  '          TO WS-HIST-EVENT
              MOVE SPACES              TO WS-HIST-TEXT
              STRING WS-CE-FIELD (WS-IX) DELIMITED BY SPACE
                     ': '                DELIMITED BY SIZE
                     WS-CE-OLD (WS-IX)   DELIMITED BY '  '
                     ' -> '              DELIMITED BY SIZE
                     WS-CE-NEW (WS-IX)   DELIMITED BY '  '
                     INTO WS-HIST-TEXT
              PERFORM 8300-WRITE-HISTORY
           END-PERFORM.

      ******************************************************************
      * Area = leading letters (1-2), outward = text before the space
       5200-DERIVE-POSTCODE-AREA.
           MOVE SPACES                 TO WS-AREA
                                          WS-OUTWARD
           MOVE FUNCTION UPPER-CASE(WS-CAT-POSTCODE) TO WS-CAT-POSTCODE
           MOVE WS-CAT-POSTCODE(1:1)   TO WS-AREA(1:1)
           IF WS-CAT-POSTCODE(2:1) IS ALPHABETIC
              AND WS-CAT-POSTCODE(2:1) NOT = SPACE
              MOVE WS-CAT-POSTCODE(2:1) TO WS-AREA(2:1)
           END-IF
           PERFORM VARYING WS-CHAR-IX FROM 1 BY 1
                   UNTIL WS-CHAR-IX > 4
                      OR WS-CAT-POSTCODE(WS-CHAR-IX:1) = SPACE
              MOVE WS-CAT-POSTCODE(WS-CHAR-IX:1)
                                       TO WS-OUTWARD(WS-CHAR-IX:1)
           END-PERFORM.

      ******************************************************************
       8000-NEXT-SEQUENCE.
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE 1                      TO SQC-SQ-COUNT
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 0                      TO SQC-RETURN-CODE
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * CODVAL for plain typelists, CODCHD when a parent must match
       8050-VALIDATE-CODE.
           INITIALIZE WS-COD-CA
           IF WS-CODE-PARENT = SPACES
              MOVE 'CODVAL  '          TO CDC-REQUEST-ID
           ELSE
              MOVE 'CODCHD  '          TO CDC-REQUEST-ID
              MOVE WS-CODE-PARENT      TO CDC-CD-DESC(1:10)
           END-IF
           MOVE WS-CODE-TABLE          TO CDC-CD-TABLE
           MOVE WS-CODE-VALUE          TO CDC-CD-CODE
           MOVE CA-USER-ID             TO CDC-USER-ID
           MOVE WS-BUS-DATE            TO CDC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CDC-CALLER-PGM
           CALL WS-COD-PGM USING WS-COD-CA
           IF CDC-RETURN-CODE >= 90
              MOVE CDC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE CDC-REASON-CODE     TO CA-REASON-CODE
              MOVE CDC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8060-CALL-DATES.
           MOVE CA-USER-ID             TO DTC-USER-ID
           MOVE WS-BUS-DATE            TO DTC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 90
              OR (DTC-RETURN-CODE >= 12
                  AND DTC-REQUEST-ID NOT = 'DTVALD  ')
              MOVE DTC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTC-REASON-CODE     TO CA-REASON-CODE
              MOVE DTC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8100-VERIFY-POLICY.
           INITIALIZE WS-POL-CA
           MOVE 'POLVRF  '             TO POC-REQUEST-ID
           MOVE CA-USER-ID             TO POC-USER-ID
           MOVE WS-BUS-DATE            TO POC-BUS-DATE
           MOVE CA-TRACE-ID            TO POC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO POC-CALLER-PGM
           MOVE CA-CL-GENAPP-POL-NUM   TO POC-PV-POLICY-NUMBER
           MOVE CA-CL-LOSS-DATE        TO POC-PV-LOSS-DATE
           CALL WS-POL-PGM USING WS-POL-CA.

      ******************************************************************
       8200-CALL-CONTACT.
           MOVE CA-USER-ID             TO COC-USER-ID
           MOVE WS-BUS-DATE            TO COC-BUS-DATE
           MOVE CA-TRACE-ID            TO COC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO COC-CALLER-PGM
           CALL WS-CON-PGM USING WS-CON-CA
           MOVE COC-RETURN-CODE        TO WS-SUB-RC
           MOVE COC-REASON-CODE        TO WS-SUB-REASON
           MOVE COC-MESSAGE            TO WS-SUB-MESSAGE
           PERFORM 8900-SUB-SERVICE-RESULT.

      ******************************************************************
       8300-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HSC-REQUEST-ID
           MOVE CA-USER-ID             TO HSC-USER-ID
           MOVE WS-BUS-DATE            TO HSC-BUS-DATE
           MOVE CA-TRACE-ID            TO HSC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HSC-CALLER-PGM
                                          HSC-HI-PROGRAM
           MOVE CLM-CLAIM-ID           TO HSC-HI-CLAIM-ID
           MOVE WS-HIST-EVENT          TO HSC-HI-EVENT-TYPE
           MOVE WS-HIST-TEXT           TO HSC-HI-DESCRIPTION
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HSC-RETURN-CODE        TO WS-SUB-RC
           MOVE HSC-REASON-CODE        TO WS-SUB-REASON
           MOVE HSC-MESSAGE            TO WS-SUB-MESSAGE
           PERFORM 8900-SUB-SERVICE-RESULT.

      ******************************************************************
       8350-WRITE-AUDIT.
           MOVE CLM-GENAPP-POL-NUM     TO WS-POL-DISP
           MOVE SPACES                 TO WS-AUDIT-PAYLOAD
           STRING 'CLAIM=' CLM-CLAIM-NUMBER
                  ';EVENT=' WS-AUDIT-EVENT
                  ';STATE=' CLM-CLAIM-STATE
                  ';POLICY=' WS-POL-DISP
                  ';LOSS=' CLM-LOSS-DATE
                  ';CAUSE=' CLM-LOSS-CAUSE
                  ';COVQ=' CLM-COVERAGE-IN-QSTN
                  ';CLOSE=' CLM-CLOSE-REASON
                  ';REOPEN=' CA-CL-REOPEN-REASON
                  ';OWNER=' CLM-ASSIGNED-USER
                  ';BUSDATE=' WS-BUS-DATE
                  ';USER=' CA-USER-ID
                  ';CHANGES=' WS-CHG-LIST
                  DELIMITED BY SIZE INTO WS-AUDIT-PAYLOAD
           INITIALIZE WS-AUC-CA
           MOVE 'AUCADD  '             TO ACC-REQUEST-ID
           MOVE CA-USER-ID             TO ACC-USER-ID
           MOVE WS-BUS-DATE            TO ACC-BUS-DATE
           MOVE CA-TRACE-ID            TO ACC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ACC-CALLER-PGM
                                          ACC-AC-PROGRAM
           MOVE CLM-CLAIM-ID           TO ACC-AC-CLAIM-ID
           MOVE WS-AUDIT-EVENT         TO ACC-AC-EVENT-CODE
           MOVE WS-AUDIT-PAYLOAD       TO ACC-AC-PAYLOAD
           CALL WS-AUC-PGM USING WS-AUC-CA
           MOVE ACC-RETURN-CODE        TO WS-SUB-RC
           MOVE ACC-REASON-CODE        TO WS-SUB-REASON
           MOVE ACC-MESSAGE            TO WS-SUB-MESSAGE
           PERFORM 8900-SUB-SERVICE-RESULT.

      ******************************************************************
       8400-SEGMENT-CLAIM.
           INITIALIZE WS-SEG-CA
           MOVE 'SEGCLM  '             TO SGC-REQUEST-ID
           MOVE CA-USER-ID             TO SGC-USER-ID
           MOVE WS-BUS-DATE            TO SGC-BUS-DATE
           MOVE CA-TRACE-ID            TO SGC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SGC-CALLER-PGM
           MOVE CLM-CLAIM-ID           TO SGC-SG-CLAIM-ID
           CALL WS-SEG-PGM USING WS-SEG-CA
           MOVE SGC-RETURN-CODE        TO WS-SUB-RC
           MOVE SGC-REASON-CODE        TO WS-SUB-REASON
           MOVE SGC-MESSAGE            TO WS-SUB-MESSAGE
           PERFORM 8900-SUB-SERVICE-RESULT.

      ******************************************************************
       8500-ASSIGN-CLAIM.
           MOVE 'ASGCLM  '             TO ASC-REQUEST-ID
           MOVE CA-USER-ID             TO ASC-USER-ID
           MOVE WS-BUS-DATE            TO ASC-BUS-DATE
           MOVE CA-TRACE-ID            TO ASC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ASC-CALLER-PGM
           MOVE CLM-CLAIM-ID           TO ASC-AS-CLAIM-ID
           MOVE 0                      TO ASC-AS-EXPOSURE-ID
           MOVE SPACES                 TO ASC-AS-TARGET-USER
                                          ASC-AS-REGION
                                          ASC-AS-LOSS-TYPE
           CALL WS-ASG-PGM USING WS-ASG-CA
           MOVE ASC-RETURN-CODE        TO WS-SUB-RC
           MOVE ASC-REASON-CODE        TO WS-SUB-REASON
           MOVE ASC-MESSAGE            TO WS-SUB-MESSAGE
           PERFORM 8900-SUB-SERVICE-RESULT.

      ******************************************************************
       8600-GENERATE-ACTIVITIES.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTGEN  '             TO AVC-REQUEST-ID
           MOVE CA-USER-ID             TO AVC-USER-ID
           MOVE WS-BUS-DATE            TO AVC-BUS-DATE
           MOVE CA-TRACE-ID            TO AVC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AVC-CALLER-PGM
           MOVE CLM-CLAIM-ID           TO AVC-AV-CLAIM-ID
           MOVE WS-ACT-EVENT           TO AVC-AV-EVENT
           CALL WS-ACT-PGM USING WS-ACT-CA
           MOVE AVC-RETURN-CODE        TO WS-SUB-RC
           MOVE AVC-REASON-CODE        TO WS-SUB-REASON
           MOVE AVC-MESSAGE            TO WS-SUB-MESSAGE
           PERFORM 8900-SUB-SERVICE-RESULT.

      ******************************************************************
      * BR-CLM-CLM-011 Once the claim row is written only an RC >= 90
      * (environment failure, caller rolls back) aborts the request;
      * business refusals from sub-services become warnings.
       8900-SUB-SERVICE-RESULT.
           EVALUATE TRUE
              WHEN WS-SUB-RC = 0
                 CONTINUE
              WHEN WS-SUB-RC >= 90
              WHEN WS-SUB-RC >= 12 AND NOT WS-CLAIM-ROW-EXISTS
                 MOVE WS-SUB-RC        TO CA-RETURN-CODE
                 MOVE WS-SUB-REASON    TO CA-REASON-CODE
                 MOVE WS-SUB-MESSAGE   TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE WS-SUB-REASON    TO WS-NW-REASON
                 MOVE WS-SUB-MESSAGE   TO WS-NW-MESSAGE
                 PERFORM 3900-ADD-WARNING
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
           STRING 'CLMCLAIM Db2 error SQLCODE ' WS-SQLCODE-DISP
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
