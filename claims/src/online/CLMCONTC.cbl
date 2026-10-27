       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCONTC.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMCONTC                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Contact (address book) service. Creates people   *
      *               and companies with duplicate detection, reads    *
      *               and maintains them, links them to claims in a    *
      *               role, and screens names against the sanctions    *
      *               list held in code table SANCTION.                *
      * REQUESTS    : CONNEW CONINQ CONUPD CONLNK CONSAN               *
      * CALLED BY   : CLMCLAIM (FNOL), CLMEXPOS, contact screens,      *
      *               payment services (sanction re-screen)            *
      * CALLS       : CLMSEQGN CLMDATES CLMCODVL CLMHIST0 CLMERRLG     *
      * TABLES      : CLM_CONTACT        (C R U)                       *
      *               CLM_CLAIM_CONTACT  (C R)                         *
      *               CLM_CLAIM          (R)                           *
      *               CLM_EXPOSURE       (R)                           *
      *               CLM_CODE_TABLE     (R)                           *
      * RULES       : BR-CLM-CON-001 Type PERS needs a last name, COMP *
      *                              needs a company name              *
      *               BR-CLM-CON-002 Date of birth must be valid and   *
      *                              not after the business date       *
      *               BR-CLM-CON-003 Postcode required for companies   *
      *                              and with an address; when given   *
      *                              it must start with a letter       *
      *               BR-CLM-CON-004 E-mail must contain one '@' with  *
      *                              a '.' in the domain               *
      *               BR-CLM-CON-005 A GenApp customer already held is *
      *                              reused (match on customer number) *
      *               BR-CLM-CON-006 Person dedupe: last name +        *
      *                              postcode + date of birth (first   *
      *                              name when DOB is not given)       *
      *               BR-CLM-CON-007 Company dedupe: company name +    *
      *                              postcode                          *
      *               BR-CLM-CON-008 New and renamed contacts are      *
      *                              sanctions screened immediately    *
      *               BR-CLM-CON-009 Contacts mirrored from GenApp are *
      *                              mastered there: name and DOB are  *
      *                              read-only in claims               *
      *               BR-CLM-CON-010 Roles: INSURED CLAIMANT REPORTER  *
      *                              WITNESS ATTORNEY VENDOR DOCTOR    *
      *                              ADVERSE                           *
      *               BR-CLM-CON-011 Only one INSURED per claim        *
      *               BR-CLM-CON-012 VENDOR role needs a vendor type   *
      *               BR-CLM-CON-013 No new links on a closed claim    *
      *               BR-CLM-CON-014 Sanctions match = upper-case name *
      *                              with spaces removed, either name  *
      *                              order -> HIT, else CLER           *
      *               BR-CLM-CON-015 A HIT writes a history row on     *
      *                              every claim the contact is on and *
      *                              returns warning SANCHIT           *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMCONTC'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(10).
           05 WS-BUS-DATE              PIC X(10).
           05 WS-AT-COUNT              PIC S9(4) COMP.
           05 WS-AT-POS                PIC S9(4) COMP.
           05 WS-DOT-COUNT             PIC S9(4) COMP.
           05 WS-I                     PIC S9(4) COMP.
           05 WS-O                     PIC S9(4) COMP.
           05 WS-UPPER-LAST            PIC X(40).
           05 WS-UPPER-FIRST           PIC X(30).
           05 WS-UPPER-COMPANY         PIC X(60).
           05 WS-OTHER-ID              PIC S9(9) COMP.
           05 WS-OTHER-IND             PIC S9(4) COMP.
           05 WS-CLAIM-STATE           PIC X(4).
           05 WS-COUNT                 PIC S9(9) COMP.
           05 WS-NAME-CHANGED          PIC X     VALUE 'N'.
              88 WS-NAME-WAS-CHANGED             VALUE 'Y'.
           05 WS-HIT-SW                PIC X     VALUE 'N'.
              88 WS-SANCTION-HIT                 VALUE 'Y'.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-END-OF-CURSOR                VALUE 'Y'.
           05 WS-HIT-ENTRY             PIC X(10).
           05 WS-HIST-COUNT            PIC S9(4) COMP.

       01  WS-TRIM.
           05 WS-TRIM-TEXT             PIC X(100).
           05 WS-TRIM-LEN              PIC S9(4) COMP.

       01  WS-NORM.
           05 WS-NORM-IN               PIC X(100).
           05 WS-NORM-OUT              PIC X(100).
           05 WS-NORM-NAME-1           PIC X(100).
           05 WS-NORM-NAME-2           PIC X(100).
           05 WS-NORM-LIST             PIC X(100).

       01  WS-VALID-ROLE               PIC X(8).
           88 WS-ROLE-OK               VALUE 'INSURED ' 'CLAIMANT'
                                             'REPORTER' 'WITNESS '
                                             'ATTORNEY' 'VENDOR  '
                                             'DOCTOR  ' 'ADVERSE '.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDCCR END-EXEC.
           EXEC SQL INCLUDE CLMDCOD END-EXEC.

           EXEC SQL DECLARE CSR-SANCTION CURSOR FOR
              SELECT CODE_VALUE, DESCRIPTION
                FROM CLMDB.CLM_CODE_TABLE
               WHERE TABLE_NAME  = 'SANCTION'
                 AND ACTIVE_FLAG = 'Y'
               ORDER BY CODE_VALUE
               FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-LINKED-CLAIMS CURSOR FOR
              SELECT DISTINCT CLAIM_ID
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CONTACT_ID = :CON-CONTACT-ID
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

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCCON.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'CONNEW  '
                 PERFORM 2000-CONTACT-NEW
              WHEN 'CONINQ  '
                 PERFORM 2100-CONTACT-INQUIRE
              WHEN 'CONUPD  '
                 PERFORM 2200-CONTACT-UPDATE
              WHEN 'CONLNK  '
                 PERFORM 2300-CONTACT-LINK
              WHEN 'CONSAN  '
                 PERFORM 2400-SANCTION-SCREEN
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMCONTC: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO CA-CO-EXISTING
                                          WS-NAME-CHANGED
                                          WS-HIT-SW
           MOVE CA-CO-CONTACT-ID       TO WS-KEY-DISP
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
       2000-CONTACT-NEW.
           PERFORM 3000-VALIDATE-CONTACT
           PERFORM 4500-FIND-DUPLICATE
           IF CA-CO-EXISTING = 'Y'
              PERFORM 9000-RETURN
           END-IF

           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE 'CON     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           PERFORM 8000-CALL-SEQUENCE
           MOVE SQC-SQ-VALUE           TO CA-CO-CONTACT-ID
           MOVE CA-CO-CONTACT-ID       TO WS-KEY-DISP

           PERFORM 4800-BUILD-HOST-ROW
           MOVE 'UNCK'                 TO CON-SANCTION-STATUS
           PERFORM 4000-INSERT-CONTACT
      * BR-CLM-CON-008 Every new contact is screened before first use
           PERFORM 2400-SANCTION-SCREEN.

      ******************************************************************
       2100-CONTACT-INQUIRE.
           PERFORM 4100-READ-CONTACT
           MOVE CON-CONTACT-TYPE       TO CA-CO-TYPE
           MOVE SPACES                 TO CA-CO-FIRST-NAME
                                          CA-CO-LAST-NAME
                                          CA-CO-COMPANY-NAME
                                          CA-CO-ADDR-LINE1
                                          CA-CO-CITY
                                          CA-CO-EMAIL
           IF CON-FIRST-NAME-IND >= 0 AND CON-FIRST-NAME-LEN > 0
              MOVE CON-FIRST-NAME-TEXT(1:CON-FIRST-NAME-LEN)
                                       TO CA-CO-FIRST-NAME
           END-IF
           IF CON-LAST-NAME-IND >= 0 AND CON-LAST-NAME-LEN > 0
              MOVE CON-LAST-NAME-TEXT(1:CON-LAST-NAME-LEN)
                                       TO CA-CO-LAST-NAME
           END-IF
           IF CON-COMPANY-NAME-IND >= 0 AND CON-COMPANY-NAME-LEN > 0
              MOVE CON-COMPANY-NAME-TEXT(1:CON-COMPANY-NAME-LEN)
                                       TO CA-CO-COMPANY-NAME
           END-IF
           IF CON-DATE-OF-BIRTH-IND >= 0
              MOVE CON-DATE-OF-BIRTH   TO CA-CO-DATE-OF-BIRTH
           ELSE
              MOVE SPACES              TO CA-CO-DATE-OF-BIRTH
           END-IF
           IF CON-ADDRESS-LINE1-IND >= 0 AND CON-ADDRESS-LINE1-LEN > 0
              MOVE CON-ADDRESS-LINE1-TEXT(1:CON-ADDRESS-LINE1-LEN)
                                       TO CA-CO-ADDR-LINE1
           END-IF
           IF CON-CITY-IND >= 0 AND CON-CITY-LEN > 0
              MOVE CON-CITY-TEXT(1:CON-CITY-LEN) TO CA-CO-CITY
           END-IF
           IF CON-POSTCODE-IND >= 0
              MOVE CON-POSTCODE        TO CA-CO-POSTCODE
           ELSE
              MOVE SPACES              TO CA-CO-POSTCODE
           END-IF
           IF CON-PHONE-NUMBER-IND >= 0
              MOVE CON-PHONE-NUMBER    TO CA-CO-PHONE
           ELSE
              MOVE SPACES              TO CA-CO-PHONE
           END-IF
           IF CON-EMAIL-ADDRESS-IND >= 0 AND CON-EMAIL-ADDRESS-LEN > 0
              MOVE CON-EMAIL-ADDRESS-TEXT(1:CON-EMAIL-ADDRESS-LEN)
                                       TO CA-CO-EMAIL
           END-IF
           MOVE CON-GENAPP-CUST-NUM    TO CA-CO-GENAPP-CUST-NUM
           MOVE CON-VENDOR-TYPE        TO CA-CO-VENDOR-TYPE
           MOVE CON-PREFERRED-FLAG     TO CA-CO-PREFERRED
           MOVE CON-SANCTION-STATUS    TO CA-CO-SANCTION-STATUS.

      ******************************************************************
       2200-CONTACT-UPDATE.
           PERFORM 4100-READ-CONTACT
           PERFORM 3000-VALIDATE-CONTACT
           PERFORM 3200-CHECK-NAME-CHANGE
      * BR-CLM-CON-009 GenApp-mastered identity cannot change here
           IF CON-GENAPP-CUST-NUM > 0
              AND WS-NAME-WAS-CHANGED
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GENAPPMS'          TO CA-REASON-CODE
              MOVE 'Insured identity is mastered in GenApp'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CO-GENAPP-CUST-NUM = 0
              MOVE CON-GENAPP-CUST-NUM TO CA-CO-GENAPP-CUST-NUM
           END-IF
           IF CA-CO-GENAPP-CUST-NUM NOT = CON-GENAPP-CUST-NUM
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GENAPPMS'          TO CA-REASON-CODE
              MOVE 'GenApp customer link cannot be changed'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CON-SANCTION-STATUS    TO CA-CO-SANCTION-STATUS
           PERFORM 4800-BUILD-HOST-ROW
           IF WS-NAME-WAS-CHANGED
              MOVE 'UNCK'              TO CON-SANCTION-STATUS
           ELSE
              MOVE CA-CO-SANCTION-STATUS TO CON-SANCTION-STATUS
           END-IF
           PERFORM 4200-UPDATE-CONTACT
      * BR-CLM-CON-008 A renamed contact is screened again
           IF WS-NAME-WAS-CHANGED
              PERFORM 2400-SANCTION-SCREEN
           END-IF.

      ******************************************************************
       2300-CONTACT-LINK.
           MOVE CA-CO-ROLE             TO WS-VALID-ROLE
      * BR-CLM-CON-010 Only the published contact roles may be used
           IF NOT WS-ROLE-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ROLEINV '          TO CA-REASON-CODE
              STRING 'Contact role ' CA-CO-ROLE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CONTACT
           PERFORM 4300-READ-CLAIM-STATE
      * BR-CLM-CON-013 Closed claims cannot gain new parties
           IF WS-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLSD '          TO CA-REASON-CODE
              MOVE 'Claim is closed - reopen it to add contacts'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-CO-EXPOSURE-ID > 0
              PERFORM 4350-CHECK-EXPOSURE
           END-IF
      * BR-CLM-CON-012 A vendor must carry a vendor type
           IF CA-CO-ROLE = 'VENDOR  '
              AND CON-VENDOR-TYPE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'VENDTYRQ'          TO CA-REASON-CODE
              MOVE 'Contact has no vendor type - cannot be VENDOR'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-CON-011 One insured per claim
           IF CA-CO-ROLE = 'INSURED '
              PERFORM 4400-CHECK-SINGLE-INSURED
           END-IF
           PERFORM 4600-INSERT-LINK
           IF CA-RC-OK
              INITIALIZE WS-HIS-CA
              MOVE CA-CO-CLAIM-ID      TO HSC-HI-CLAIM-ID
              MOVE 'CONLINK '          TO HSC-HI-EVENT-TYPE
              STRING 'Contact ' WS-KEY-DISP ' linked as '
                     CA-CO-ROLE DELIMITED BY SIZE
                     INTO HSC-HI-DESCRIPTION
              PERFORM 8300-CALL-HISTORY
           END-IF
           IF CON-SANCTION-STATUS = 'HIT ' AND CA-RC-OK
              SET CA-RC-WARNING        TO TRUE
              MOVE 'SANCHIT '          TO CA-REASON-CODE
              MOVE 'Linked contact is a sanctions list match'
                                       TO CA-MESSAGE
           END-IF
           MOVE CON-SANCTION-STATUS    TO CA-CO-SANCTION-STATUS.

      ******************************************************************
       2400-SANCTION-SCREEN.
           PERFORM 4100-READ-CONTACT
           PERFORM 5300-BUILD-SCREEN-NAMES
           MOVE 'N'                    TO WS-HIT-SW
                                          WS-EOF-SW
           MOVE SPACES                 TO WS-HIT-ENTRY
           EXEC SQL OPEN CSR-SANCTION END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2400-SANCTION-SCREEN' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-END-OF-CURSOR OR WS-SANCTION-HIT
              EXEC SQL
                 FETCH CSR-SANCTION
                  INTO :COD-CODE-VALUE, :COD-DESCRIPTION
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE SPACES        TO WS-NORM-IN
                    IF COD-DESCRIPTION-LEN > 0
                       MOVE COD-DESCRIPTION-TEXT(1:COD-DESCRIPTION-LEN)
                                       TO WS-NORM-IN
                    END-IF
                    PERFORM 5200-NORMALISE-NAME
                    MOVE WS-NORM-OUT   TO WS-NORM-LIST
      * BR-CLM-CON-014 Normalised exact match, either name order
                    IF WS-NORM-LIST NOT = SPACES
                       AND (WS-NORM-LIST = WS-NORM-NAME-1
                            OR WS-NORM-LIST = WS-NORM-NAME-2)
                       SET WS-SANCTION-HIT TO TRUE
                       MOVE COD-CODE-VALUE TO WS-HIT-ENTRY
                    END-IF
                 WHEN 100
                    SET WS-END-OF-CURSOR TO TRUE
                 WHEN OTHER
                    EXEC SQL CLOSE CSR-SANCTION END-EXEC
                    MOVE '2400-SANCTION-SCREEN' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-SANCTION END-EXEC

           IF WS-SANCTION-HIT
              MOVE 'HIT '              TO CON-SANCTION-STATUS
           ELSE
              MOVE 'CLER'              TO CON-SANCTION-STATUS
           END-IF
           PERFORM 4700-UPDATE-SANCTION-STATUS
           MOVE CON-SANCTION-STATUS    TO CA-CO-SANCTION-STATUS
           IF WS-SANCTION-HIT
              PERFORM 2450-RECORD-SANCTION-HIT
           END-IF.

      ******************************************************************
      * BR-CLM-CON-015 A hit is written to the history of every claim
      * the contact is on (or the claim in hand) and returned as RC 04
       2450-RECORD-SANCTION-HIT.
           MOVE 0                      TO WS-HIST-COUNT
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN CSR-LINKED-CLAIMS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2450-RECORD-SANCTION-HIT' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-END-OF-CURSOR
              EXEC SQL
                 FETCH CSR-LINKED-CLAIMS INTO :CCR-CLAIM-ID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 2460-WRITE-HIT-HISTORY
                 WHEN 100
                    SET WS-END-OF-CURSOR TO TRUE
                 WHEN OTHER
                    EXEC SQL CLOSE CSR-LINKED-CLAIMS END-EXEC
                    MOVE '2450-RECORD-SANCTION-HIT'
                                       TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-LINKED-CLAIMS END-EXEC
           IF WS-HIST-COUNT = 0 AND CA-CO-CLAIM-ID > 0
              MOVE CA-CO-CLAIM-ID      TO CCR-CLAIM-ID
              PERFORM 2460-WRITE-HIT-HISTORY
           END-IF
           SET CA-RC-WARNING           TO TRUE
           MOVE 'SANCHIT '             TO CA-REASON-CODE
           STRING 'Sanctions list match on entry ' WS-HIT-ENTRY
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       2460-WRITE-HIT-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE CCR-CLAIM-ID           TO HSC-HI-CLAIM-ID
           MOVE 'SANCHIT '             TO HSC-HI-EVENT-TYPE
           STRING 'Contact ' WS-KEY-DISP
                  ' matched sanctions entry ' WS-HIT-ENTRY
                  ' - payments must be held'
                  DELIMITED BY SIZE INTO HSC-HI-DESCRIPTION
           PERFORM 8300-CALL-HISTORY
           ADD 1                       TO WS-HIST-COUNT.

      ******************************************************************
       3000-VALIDATE-CONTACT.
           IF CA-CO-TYPE = SPACES
              IF CA-CO-LAST-NAME = SPACES
                 AND CA-CO-COMPANY-NAME NOT = SPACES
                 MOVE 'COMP'           TO CA-CO-TYPE
              ELSE
                 MOVE 'PERS'           TO CA-CO-TYPE
              END-IF
           END-IF
      * BR-CLM-CON-001 Mandatory name by contact type
           EVALUATE CA-CO-TYPE
              WHEN 'PERS'
                 IF CA-CO-LAST-NAME = SPACES
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'LNAMERQ '    TO CA-REASON-CODE
                    MOVE 'Last name is required for a person'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 'COMP'
                 IF CA-CO-COMPANY-NAME = SPACES
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'CNAMERQ '    TO CA-REASON-CODE
                    MOVE 'Company name is required for a company'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
                 IF CA-CO-DATE-OF-BIRTH NOT = SPACES
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'DOBCOMP '    TO CA-REASON-CODE
                    MOVE 'A company cannot have a date of birth'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'CONTYPIV'       TO CA-REASON-CODE
                 MOVE 'Contact type must be PERS or COMP'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE

      * BR-CLM-CON-002 Date of birth valid and not in the future
           IF CA-CO-DATE-OF-BIRTH NOT = SPACES
              INITIALIZE WS-DTE-CA
              MOVE 'DTVALD  '          TO DTC-REQUEST-ID
              MOVE CA-CO-DATE-OF-BIRTH TO DTC-DT-DATE-1
              PERFORM 8100-CALL-DATES
              IF NOT DTC-DT-IS-VALID
                 OR CA-CO-DATE-OF-BIRTH > WS-BUS-DATE
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DOBINV  '       TO CA-REASON-CODE
                 MOVE 'Date of birth is invalid or in the future'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF

      * BR-CLM-CON-003 Postcode needed for companies and addresses;
      * a phone-only person (witness, reporter) may have none
           IF CA-CO-POSTCODE = SPACES
              IF CA-CO-TYPE = 'COMP'
                 OR CA-CO-ADDR-LINE1 NOT = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PCODERQ '       TO CA-REASON-CODE
                 MOVE 'Postcode is required with an address'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           ELSE
              IF CA-CO-POSTCODE(1:1) NOT ALPHABETIC
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PCODEINV'       TO CA-REASON-CODE
                 MOVE 'Postcode is not a valid UK postcode'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           MOVE FUNCTION UPPER-CASE(CA-CO-POSTCODE) TO CA-CO-POSTCODE

           IF CA-CO-EMAIL NOT = SPACES
              PERFORM 3100-VALIDATE-EMAIL
           END-IF

           IF CA-CO-VENDOR-TYPE NOT = SPACES
              INITIALIZE WS-COD-CA
              MOVE 'CODVAL  '          TO CDC-REQUEST-ID
              MOVE 'VENDTYPE'          TO CDC-CD-TABLE
              MOVE CA-CO-VENDOR-TYPE   TO CDC-CD-CODE
              PERFORM 8200-CALL-CODES
              IF NOT CDC-CD-IS-VALID
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'VENDTYIV'       TO CA-REASON-CODE
                 STRING 'Vendor type ' CA-CO-VENDOR-TYPE
                        ' is not valid' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           IF CA-CO-PREFERRED = SPACES
              MOVE 'N'                 TO CA-CO-PREFERRED
           END-IF
           IF CA-CO-PREFERRED NOT = 'Y' AND NOT = 'N'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PREFFLIV'          TO CA-REASON-CODE
              MOVE 'Preferred flag must be Y or N' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-CON-004 Exactly one '@', a '.' after it, not at the end
       3100-VALIDATE-EMAIL.
           MOVE 0                      TO WS-AT-COUNT
                                          WS-AT-POS
                                          WS-DOT-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
              EVALUATE CA-CO-EMAIL(WS-I:1)
                 WHEN '@'
                    ADD 1              TO WS-AT-COUNT
                    MOVE WS-I          TO WS-AT-POS
                 WHEN '.'
                    IF WS-AT-POS > 0 AND WS-I > WS-AT-POS + 1
                       ADD 1           TO WS-DOT-COUNT
                    END-IF
              END-EVALUATE
           END-PERFORM
           IF WS-AT-COUNT NOT = 1
              OR WS-AT-POS = 1
              OR WS-DOT-COUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EMAILINV'          TO CA-REASON-CODE
              MOVE 'E-mail address is not well formed'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3200-CHECK-NAME-CHANGE.
           MOVE 'N'                    TO WS-NAME-CHANGED
           MOVE SPACES                 TO WS-TRIM-TEXT
           IF CON-LAST-NAME-IND >= 0 AND CON-LAST-NAME-LEN > 0
              MOVE CON-LAST-NAME-TEXT(1:CON-LAST-NAME-LEN)
                                       TO WS-TRIM-TEXT
           END-IF
           IF WS-TRIM-TEXT(1:40) NOT = CA-CO-LAST-NAME
              SET WS-NAME-WAS-CHANGED  TO TRUE
           END-IF
           MOVE SPACES                 TO WS-TRIM-TEXT
           IF CON-FIRST-NAME-IND >= 0 AND CON-FIRST-NAME-LEN > 0
              MOVE CON-FIRST-NAME-TEXT(1:CON-FIRST-NAME-LEN)
                                       TO WS-TRIM-TEXT
           END-IF
           IF WS-TRIM-TEXT(1:30) NOT = CA-CO-FIRST-NAME
              SET WS-NAME-WAS-CHANGED  TO TRUE
           END-IF
           MOVE SPACES                 TO WS-TRIM-TEXT
           IF CON-COMPANY-NAME-IND >= 0 AND CON-COMPANY-NAME-LEN > 0
              MOVE CON-COMPANY-NAME-TEXT(1:CON-COMPANY-NAME-LEN)
                                       TO WS-TRIM-TEXT
           END-IF
           IF WS-TRIM-TEXT(1:60) NOT = CA-CO-COMPANY-NAME
              SET WS-NAME-WAS-CHANGED  TO TRUE
           END-IF
           IF CON-DATE-OF-BIRTH-IND < 0
              MOVE SPACES              TO CON-DATE-OF-BIRTH
           END-IF
           IF CON-GENAPP-CUST-NUM > 0
              AND CON-DATE-OF-BIRTH NOT = CA-CO-DATE-OF-BIRTH
              SET WS-NAME-WAS-CHANGED  TO TRUE
           END-IF.

      ******************************************************************
       4000-INSERT-CONTACT.
           EXEC SQL
              INSERT INTO CLMDB.CLM_CONTACT
                    (CONTACT_ID, CONTACT_TYPE, FIRST_NAME, LAST_NAME,
                     COMPANY_NAME, DATE_OF_BIRTH, ADDRESS_LINE1, CITY,
                     POSTCODE, PHONE_NUMBER, EMAIL_ADDRESS,
                     GENAPP_CUST_NUM, VENDOR_TYPE, PREFERRED_FLAG,
                     SANCTION_STATUS, CREATE_TS, UPDATE_TS)
              VALUES (:CON-CONTACT-ID, :CON-CONTACT-TYPE,
                      :CON-FIRST-NAME    :CON-FIRST-NAME-IND,
                      :CON-LAST-NAME     :CON-LAST-NAME-IND,
                      :CON-COMPANY-NAME  :CON-COMPANY-NAME-IND,
                      :CON-DATE-OF-BIRTH :CON-DATE-OF-BIRTH-IND,
                      :CON-ADDRESS-LINE1 :CON-ADDRESS-LINE1-IND,
                      :CON-CITY          :CON-CITY-IND,
                      :CON-POSTCODE      :CON-POSTCODE-IND,
                      :CON-PHONE-NUMBER  :CON-PHONE-NUMBER-IND,
                      :CON-EMAIL-ADDRESS :CON-EMAIL-ADDRESS-IND,
                      :CON-GENAPP-CUST-NUM, :CON-VENDOR-TYPE,
                      :CON-PREFERRED-FLAG, :CON-SANCTION-STATUS,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE 'Contact id already exists - sequence out of step'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-INSERT-CONTACT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-READ-CONTACT.
           IF CA-CO-CONTACT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CONIDRQ '          TO CA-REASON-CODE
              MOVE 'Contact id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-CO-CONTACT-ID       TO CON-CONTACT-ID
                                          WS-KEY-DISP
           EXEC SQL
              SELECT CONTACT_TYPE, FIRST_NAME, LAST_NAME,
                     COMPANY_NAME, DATE_OF_BIRTH, ADDRESS_LINE1,
                     CITY, POSTCODE, PHONE_NUMBER, EMAIL_ADDRESS,
                     GENAPP_CUST_NUM, VENDOR_TYPE, PREFERRED_FLAG,
                     SANCTION_STATUS
                INTO :CON-CONTACT-TYPE,
                     :CON-FIRST-NAME    :CON-FIRST-NAME-IND,
                     :CON-LAST-NAME     :CON-LAST-NAME-IND,
                     :CON-COMPANY-NAME  :CON-COMPANY-NAME-IND,
                     :CON-DATE-OF-BIRTH :CON-DATE-OF-BIRTH-IND,
                     :CON-ADDRESS-LINE1 :CON-ADDRESS-LINE1-IND,
                     :CON-CITY          :CON-CITY-IND,
                     :CON-POSTCODE      :CON-POSTCODE-IND,
                     :CON-PHONE-NUMBER  :CON-PHONE-NUMBER-IND,
                     :CON-EMAIL-ADDRESS :CON-EMAIL-ADDRESS-IND,
                     :CON-GENAPP-CUST-NUM, :CON-VENDOR-TYPE,
                     :CON-PREFERRED-FLAG, :CON-SANCTION-STATUS
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :CON-CONTACT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CONNOTFD'       TO CA-REASON-CODE
                 STRING 'Contact ' WS-KEY-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CONTACT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-UPDATE-CONTACT.
           EXEC SQL
              UPDATE CLMDB.CLM_CONTACT
                 SET CONTACT_TYPE    = :CON-CONTACT-TYPE,
                     FIRST_NAME      = :CON-FIRST-NAME
                                       :CON-FIRST-NAME-IND,
                     LAST_NAME       = :CON-LAST-NAME
                                       :CON-LAST-NAME-IND,
                     COMPANY_NAME    = :CON-COMPANY-NAME
                                       :CON-COMPANY-NAME-IND,
                     DATE_OF_BIRTH   = :CON-DATE-OF-BIRTH
                                       :CON-DATE-OF-BIRTH-IND,
                     ADDRESS_LINE1   = :CON-ADDRESS-LINE1
                                       :CON-ADDRESS-LINE1-IND,
                     CITY            = :CON-CITY :CON-CITY-IND,
                     POSTCODE        = :CON-POSTCODE
                                       :CON-POSTCODE-IND,
                     PHONE_NUMBER    = :CON-PHONE-NUMBER
                                       :CON-PHONE-NUMBER-IND,
                     EMAIL_ADDRESS   = :CON-EMAIL-ADDRESS
                                       :CON-EMAIL-ADDRESS-IND,
                     VENDOR_TYPE     = :CON-VENDOR-TYPE,
                     PREFERRED_FLAG  = :CON-PREFERRED-FLAG,
                     SANCTION_STATUS = :CON-SANCTION-STATUS,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE CONTACT_ID = :CON-CONTACT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-UPDATE-CONTACT' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-READ-CLAIM-STATE.
           IF CA-CO-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLMIDRQ '          TO CA-REASON-CODE
              MOVE 'Claim id is required to link a contact'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT CLAIM_STATE
                INTO :WS-CLAIM-STATE
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-CO-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE 'Claim not found for contact link'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4300-READ-CLAIM-STATE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4350-CHECK-EXPOSURE.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :CA-CO-EXPOSURE-ID
                 AND CLAIM_ID    = :CA-CO-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4350-CHECK-EXPOSURE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPNOTCL'          TO CA-REASON-CODE
              MOVE 'Exposure does not belong to this claim'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4400-CHECK-SINGLE-INSURED.
           EXEC SQL
              SELECT MIN(CONTACT_ID)
                INTO :WS-OTHER-ID :WS-OTHER-IND
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID     = :CA-CO-CLAIM-ID
                 AND CONTACT_ROLE = 'INSURED'
                 AND CONTACT_ID  <> :CA-CO-CONTACT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-CHECK-SINGLE-INSURED' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-OTHER-IND >= 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'INSEXIST'          TO CA-REASON-CODE
              MOVE WS-OTHER-ID         TO WS-KEY-DISP
              STRING 'Claim already has insured contact '
                     WS-KEY-DISP DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Known-duplicate search. Every match returns the lowest contact
      * id so repeated FNOLs converge on the same address-book entry.
       4500-FIND-DUPLICATE.
           MOVE FUNCTION UPPER-CASE(CA-CO-LAST-NAME)  TO WS-UPPER-LAST
           MOVE FUNCTION UPPER-CASE(CA-CO-FIRST-NAME) TO WS-UPPER-FIRST
           MOVE FUNCTION UPPER-CASE(CA-CO-COMPANY-NAME)
                                       TO WS-UPPER-COMPANY
           MOVE CA-CO-POSTCODE         TO CON-POSTCODE
           EVALUATE TRUE
      * BR-CLM-CON-005 GenApp customer already mirrored as a contact
              WHEN CA-CO-GENAPP-CUST-NUM > 0
                 EXEC SQL
                    SELECT MIN(CONTACT_ID)
                      INTO :WS-OTHER-ID :WS-OTHER-IND
                      FROM CLMDB.CLM_CONTACT
                     WHERE GENAPP_CUST_NUM = :CA-CO-GENAPP-CUST-NUM
                 END-EXEC
      * BR-CLM-CON-007 Company: name + postcode
              WHEN CA-CO-TYPE = 'COMP'
                 EXEC SQL
                    SELECT MIN(CONTACT_ID)
                      INTO :WS-OTHER-ID :WS-OTHER-IND
                      FROM CLMDB.CLM_CONTACT
                     WHERE CONTACT_TYPE        = 'COMP'
                       AND UPPER(COMPANY_NAME) = :WS-UPPER-COMPANY
                       AND POSTCODE            = :CON-POSTCODE
                 END-EXEC
      * BR-CLM-CON-006 Person: last name + postcode + date of birth
              WHEN CA-CO-DATE-OF-BIRTH NOT = SPACES
                 EXEC SQL
                    SELECT MIN(CONTACT_ID)
                      INTO :WS-OTHER-ID :WS-OTHER-IND
                      FROM CLMDB.CLM_CONTACT
                     WHERE CONTACT_TYPE     = 'PERS'
                       AND UPPER(LAST_NAME) = :WS-UPPER-LAST
                       AND POSTCODE         = :CON-POSTCODE
                       AND DATE_OF_BIRTH    = :CA-CO-DATE-OF-BIRTH
                 END-EXEC
              WHEN OTHER
                 EXEC SQL
                    SELECT MIN(CONTACT_ID)
                      INTO :WS-OTHER-ID :WS-OTHER-IND
                      FROM CLMDB.CLM_CONTACT
                     WHERE CONTACT_TYPE      = 'PERS'
                       AND UPPER(LAST_NAME)  = :WS-UPPER-LAST
                       AND UPPER(FIRST_NAME) = :WS-UPPER-FIRST
                       AND POSTCODE          = :CON-POSTCODE
                       AND DATE_OF_BIRTH IS NULL
                 END-EXEC
           END-EVALUATE
           IF SQLCODE NOT = 0
              MOVE '4500-FIND-DUPLICATE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-OTHER-IND >= 0
              MOVE WS-OTHER-ID         TO CA-CO-CONTACT-ID
              MOVE 'Y'                 TO CA-CO-EXISTING
              PERFORM 4100-READ-CONTACT
              MOVE CON-SANCTION-STATUS TO CA-CO-SANCTION-STATUS
           ELSE
              MOVE 'N'                 TO CA-CO-EXISTING
           END-IF.

      ******************************************************************
       4600-INSERT-LINK.
           MOVE CA-CO-CLAIM-ID         TO CCR-CLAIM-ID
           MOVE CA-CO-CONTACT-ID       TO CCR-CONTACT-ID
           MOVE CA-CO-ROLE             TO CCR-CONTACT-ROLE
           MOVE CA-CO-EXPOSURE-ID      TO CCR-EXPOSURE-ID
           EXEC SQL
              INSERT INTO CLMDB.CLM_CLAIM_CONTACT
                    (CLAIM_ID, CONTACT_ID, CONTACT_ROLE, EXPOSURE_ID,
                     ADDED_TS)
              VALUES (:CCR-CLAIM-ID, :CCR-CONTACT-ID,
                      :CCR-CONTACT-ROLE, :CCR-EXPOSURE-ID,
                      CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'LNKEXIST'       TO CA-REASON-CODE
                 MOVE 'Contact already linked to claim in this role'
                                       TO CA-MESSAGE
              WHEN OTHER
                 MOVE '4600-INSERT-LINK' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4700-UPDATE-SANCTION-STATUS.
           EXEC SQL
              UPDATE CLMDB.CLM_CONTACT
                 SET SANCTION_STATUS = :CON-SANCTION-STATUS,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE CONTACT_ID = :CON-CONTACT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4700-UPDATE-SANCTION-STATUS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Commarea -> DCLGEN host row; blank optional columns become NULL
       4800-BUILD-HOST-ROW.
           MOVE CA-CO-CONTACT-ID       TO CON-CONTACT-ID
           MOVE CA-CO-TYPE             TO CON-CONTACT-TYPE
           MOVE CA-CO-GENAPP-CUST-NUM  TO CON-GENAPP-CUST-NUM
           MOVE CA-CO-VENDOR-TYPE      TO CON-VENDOR-TYPE
           MOVE CA-CO-PREFERRED        TO CON-PREFERRED-FLAG

           MOVE CA-CO-FIRST-NAME       TO WS-TRIM-TEXT
           PERFORM 5100-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CON-FIRST-NAME-LEN
           MOVE CA-CO-FIRST-NAME       TO CON-FIRST-NAME-TEXT
           MOVE 0                      TO CON-FIRST-NAME-IND
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CON-FIRST-NAME-IND
           END-IF

           MOVE CA-CO-LAST-NAME        TO WS-TRIM-TEXT
           PERFORM 5100-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CON-LAST-NAME-LEN
           MOVE CA-CO-LAST-NAME        TO CON-LAST-NAME-TEXT
           MOVE 0                      TO CON-LAST-NAME-IND
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CON-LAST-NAME-IND
           END-IF

           MOVE CA-CO-COMPANY-NAME     TO WS-TRIM-TEXT
           PERFORM 5100-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CON-COMPANY-NAME-LEN
           MOVE CA-CO-COMPANY-NAME     TO CON-COMPANY-NAME-TEXT
           MOVE 0                      TO CON-COMPANY-NAME-IND
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CON-COMPANY-NAME-IND
           END-IF

           MOVE CA-CO-ADDR-LINE1       TO WS-TRIM-TEXT
           PERFORM 5100-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CON-ADDRESS-LINE1-LEN
           MOVE CA-CO-ADDR-LINE1       TO CON-ADDRESS-LINE1-TEXT
           MOVE 0                      TO CON-ADDRESS-LINE1-IND
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CON-ADDRESS-LINE1-IND
           END-IF

           MOVE CA-CO-CITY             TO WS-TRIM-TEXT
           PERFORM 5100-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CON-CITY-LEN
           MOVE CA-CO-CITY             TO CON-CITY-TEXT
           MOVE 0                      TO CON-CITY-IND
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CON-CITY-IND
           END-IF

           MOVE CA-CO-EMAIL            TO WS-TRIM-TEXT
           PERFORM 5100-TRIM-LENGTH
           MOVE WS-TRIM-LEN            TO CON-EMAIL-ADDRESS-LEN
           MOVE CA-CO-EMAIL            TO CON-EMAIL-ADDRESS-TEXT
           MOVE 0                      TO CON-EMAIL-ADDRESS-IND
           IF WS-TRIM-LEN = 0
              MOVE -1                  TO CON-EMAIL-ADDRESS-IND
           END-IF

           MOVE CA-CO-DATE-OF-BIRTH    TO CON-DATE-OF-BIRTH
           MOVE 0                      TO CON-DATE-OF-BIRTH-IND
           IF CA-CO-DATE-OF-BIRTH = SPACES
              MOVE -1                  TO CON-DATE-OF-BIRTH-IND
           END-IF
           MOVE CA-CO-POSTCODE         TO CON-POSTCODE
           MOVE 0                      TO CON-POSTCODE-IND
           IF CA-CO-POSTCODE = SPACES
              MOVE -1                  TO CON-POSTCODE-IND
           END-IF
           MOVE CA-CO-PHONE            TO CON-PHONE-NUMBER
           MOVE 0                      TO CON-PHONE-NUMBER-IND
           IF CA-CO-PHONE = SPACES
              MOVE -1                  TO CON-PHONE-NUMBER-IND
           END-IF.

      ******************************************************************
       5100-TRIM-LENGTH.
           MOVE 0                      TO WS-TRIM-LEN
           IF WS-TRIM-TEXT NOT = SPACES
              INSPECT FUNCTION REVERSE(WS-TRIM-TEXT)
                      TALLYING WS-TRIM-LEN FOR LEADING SPACES
              COMPUTE WS-TRIM-LEN = LENGTH OF WS-TRIM-TEXT
                                  - WS-TRIM-LEN
           END-IF.

      ******************************************************************
      * Upper-case and squeeze out every space: 'Mc Donald  Ltd'
      * becomes 'MCDONALDLTD' so spacing never defeats the screen.
       5200-NORMALISE-NAME.
           MOVE SPACES                 TO WS-NORM-OUT
           MOVE FUNCTION UPPER-CASE(WS-NORM-IN) TO WS-NORM-IN
           MOVE 0                      TO WS-O
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
              IF WS-NORM-IN(WS-I:1) NOT = SPACE
                 ADD 1                 TO WS-O
                 MOVE WS-NORM-IN(WS-I:1) TO WS-NORM-OUT(WS-O:1)
              END-IF
           END-PERFORM.

      ******************************************************************
       5300-BUILD-SCREEN-NAMES.
           MOVE SPACES                 TO WS-NORM-NAME-1
                                          WS-NORM-NAME-2
           IF CON-CONTACT-TYPE = 'COMP'
              MOVE SPACES              TO WS-NORM-IN
              IF CON-COMPANY-NAME-IND >= 0 AND CON-COMPANY-NAME-LEN > 0
                 MOVE CON-COMPANY-NAME-TEXT(1:CON-COMPANY-NAME-LEN)
                                       TO WS-NORM-IN
              END-IF
              PERFORM 5200-NORMALISE-NAME
              MOVE WS-NORM-OUT         TO WS-NORM-NAME-1
                                          WS-NORM-NAME-2
           ELSE
              MOVE SPACES              TO WS-UPPER-FIRST
                                          WS-UPPER-LAST
              IF CON-FIRST-NAME-IND >= 0 AND CON-FIRST-NAME-LEN > 0
                 MOVE CON-FIRST-NAME-TEXT(1:CON-FIRST-NAME-LEN)
                                       TO WS-UPPER-FIRST
              END-IF
              IF CON-LAST-NAME-IND >= 0 AND CON-LAST-NAME-LEN > 0
                 MOVE CON-LAST-NAME-TEXT(1:CON-LAST-NAME-LEN)
                                       TO WS-UPPER-LAST
              END-IF
              MOVE SPACES              TO WS-NORM-IN
              STRING WS-UPPER-FIRST WS-UPPER-LAST
                     DELIMITED BY SIZE INTO WS-NORM-IN
              PERFORM 5200-NORMALISE-NAME
              MOVE WS-NORM-OUT         TO WS-NORM-NAME-1
              MOVE SPACES              TO WS-NORM-IN
              STRING WS-UPPER-LAST WS-UPPER-FIRST
                     DELIMITED BY SIZE INTO WS-NORM-IN
              PERFORM 5200-NORMALISE-NAME
              MOVE WS-NORM-OUT         TO WS-NORM-NAME-2
           END-IF.

      ******************************************************************
       8000-CALL-SEQUENCE.
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8100-CALL-DATES.
           MOVE CA-USER-ID             TO DTC-USER-ID
           MOVE WS-BUS-DATE            TO DTC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA.

      ******************************************************************
       8200-CALL-CODES.
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
       8300-CALL-HISTORY.
           MOVE 'HISADD  '             TO HSC-REQUEST-ID
           MOVE CA-USER-ID             TO HSC-USER-ID
           MOVE WS-BUS-DATE            TO HSC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HSC-CALLER-PGM
                                          HSC-HI-PROGRAM
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
           STRING 'CLMCONTC Db2 error SQLCODE ' WS-SQLCODE-DISP
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
