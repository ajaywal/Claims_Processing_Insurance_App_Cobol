       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMPOLVR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMPOLVR                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Policy verification against IBM GenApp. Reads    *
      *               the policy, its customer and its line table      *
      *               (MOTOR / HOUSE / COMMERCIAL), decides whether    *
      *               the policy was in force on the loss date, builds *
      *               the claim-time coverage and risk unit picture    *
      *               and reports the latest billing status received   *
      *               from BillingCore (BILSTAT events).               *
      * REQUESTS    : POLVRF                                           *
      * CALLED BY   : CLMCLAIM (CLMNEW, CLMUPD), FNOL batch intake     *
      * CALLS       : CLMDATES CLMMONEY CLMERRLG                       *
      * TABLES      : GENAPP.POLICY      (R)                           *
      *               GENAPP.CUSTOMER    (R)                           *
      *               GENAPP.MOTOR       (R)                           *
      *               GENAPP.HOUSE       (R)                           *
      *               GENAPP.COMMERCIAL  (R)                           *
      *               CLM_EVENT_QUEUE    (R)                           *
      * RULES       : BR-CLM-POL-001 Policy is in force when           *
      *                              ISSUEDATE <= loss <= EXPIRYDATE   *
      *               BR-CLM-POL-002 Missing EXPIRYDATE = open-ended;  *
      *                              missing ISSUEDATE = unverifiable  *
      *               BR-CLM-POL-003 Endowment is not a P&C policy:    *
      *                              VERIFIED 'N' reason LIFEPOL       *
      *               BR-CLM-POL-004 Motor COLL/COMP limit = vehicle   *
      *                              value, deductibles 250 / 100      *
      *               BR-CLM-POL-005 Motor third-party limits fixed:   *
      *                              LIAB_BI 5,000,000 and LIAB_PD     *
      *                              1,000,000                         *
      *               BR-CLM-POL-006 Physical damage cover needs an    *
      *                              insured value greater than zero   *
      *               BR-CLM-POL-007 House DWELLING limit = value,     *
      *                              deductible 500                    *
      *               BR-CLM-POL-008 House CONTENTS limit = 30% of     *
      *                              dwelling value, deductible 250    *
      *               BR-CLM-POL-009 Commercial perils count only when *
      *                              their premium is greater than 0   *
      *               BR-CLM-POL-010 Commercial BLDG sum insured =     *
      *                              premium x peril multiplier, max   *
      *                              10,000,000; flood raises excess   *
      *               BR-CLM-POL-011 Commercial BUSINT limit = 25% of  *
      *                              BLDG, minimum 25,000              *
      *               BR-CLM-POL-012 Verified only when at least one   *
      *                              coverage could be built           *
      *               BR-CLM-POL-013 Billing status = latest inbound   *
      *                              BILSTAT for the policy, else UNKN *
      *               BR-CLM-POL-014 DLNQ / CANC billing on an in-     *
      *                              force policy -> STATUS 'BILL' and *
      *                              warning RC 04                     *
      *               BR-CLM-POL-015 First warning raised wins; later  *
      *                              warnings never overwrite it       *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMPOLVR'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'CLMMONEY'.

       01  WS-CONSTANTS.
           05 WS-DED-COLL              PIC S9(11)V99 COMP-3 VALUE 250.
           05 WS-DED-COMP              PIC S9(11)V99 COMP-3 VALUE 100.
           05 WS-LIM-LIAB-BI           PIC S9(11)V99 COMP-3
                                                    VALUE 5000000.
           05 WS-LIM-LIAB-PD           PIC S9(11)V99 COMP-3
                                                    VALUE 1000000.
           05 WS-DED-DWELLING          PIC S9(11)V99 COMP-3 VALUE 500.
           05 WS-DED-CONTENTS          PIC S9(11)V99 COMP-3 VALUE 250.
           05 WS-CONTENTS-BPS          PIC S9(4) COMP VALUE 3000.
           05 WS-MULT-FIRE             PIC S9(4) COMP VALUE 400.
           05 WS-MULT-FLOOD            PIC S9(4) COMP VALUE 250.
           05 WS-MULT-WEATHER          PIC S9(4) COMP VALUE 300.
           05 WS-MULT-CRIME            PIC S9(4) COMP VALUE 200.
           05 WS-BLDG-MAX              PIC S9(11)V99 COMP-3
                                                    VALUE 10000000.
           05 WS-DED-BLDG              PIC S9(11)V99 COMP-3 VALUE 1000.
           05 WS-DED-BLDG-FLOOD        PIC S9(11)V99 COMP-3 VALUE 2500.
           05 WS-BUSINT-BPS            PIC S9(4) COMP VALUE 2500.
           05 WS-BUSINT-MIN            PIC S9(11)V99 COMP-3
                                                    VALUE 25000.
           05 WS-MAX-COV               PIC S9(4) COMP VALUE 10.
           05 WS-MAX-RU                PIC S9(4) COMP VALUE 5.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-POL-KEY-NUM           PIC 9(10).
           05 WS-POL-KEY               PIC X(20).
           05 WS-INSURED-NAME          PIC X(60).
           05 WS-RU-DESC               PIC X(60).
           05 WS-BEDROOMS-DISP         PIC Z9.
           05 WS-YEAR-TEXT             PIC X(4).
           05 WS-ISSUE-NULL            PIC X     VALUE 'N'.
           05 WS-EXPIRY-NULL           PIC X     VALUE 'N'.
           05 WS-LINE-FOUND            PIC X     VALUE 'N'.
              88 WS-LINE-WAS-FOUND               VALUE 'Y'.
           05 WS-VALUE                 PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-BLDG-LIMIT            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-BUSINT-LIMIT          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-BLDG-DED              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PERIL-COUNT           PIC S9(4) COMP VALUE 0.
           05 WS-FLOOD-SW              PIC X     VALUE 'N'.
              88 WS-FLOOD-COVERED                VALUE 'Y'.

       01  WS-NEW-COV.
           05 WS-NC-TYPE               PIC X(8).
           05 WS-NC-RU-SEQ             PIC S9(4) COMP.
           05 WS-NC-LIMIT              PIC S9(11)V99 COMP-3.
           05 WS-NC-DEDUCTIBLE         PIC S9(11)V99 COMP-3.

       01  WS-WARNING.
           05 WS-W-REASON              PIC X(8).
           05 WS-W-MESSAGE             PIC X(80).

       01  WS-BILSTAT-PAYLOAD.
           05 WS-BS-POLICY-NO          PIC X(10).
           05 WS-BS-STATUS             PIC X(4).
              88 WS-BS-KNOWN             VALUE 'CURR' 'PDUE'
                                               'DLNQ' 'CANC'.
              88 WS-BS-ADVERSE           VALUE 'DLNQ' 'CANC'.
           05 WS-BS-PAST-DUE           PIC X(14).
           05 WS-BS-AS-OF              PIC X(10).
           05 FILLER                   PIC X(962).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE GENDPOL END-EXEC.
           EXEC SQL INCLUDE GENDCUS END-EXEC.
           EXEC SQL INCLUDE GENDMOT END-EXEC.
           EXEC SQL INCLUDE GENDHOU END-EXEC.
           EXEC SQL INCLUDE GENDCOM END-EXEC.
           EXEC SQL INCLUDE CLMDEVQ END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTC-==.
           03 DTC-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTC-==.

       01  WS-MNY-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==MNC-==.
           03 MNC-BODY.
              COPY CLMCMNY REPLACING LEADING ==CA-== BY ==MNC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCPOL.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'POLVRF  '
                 PERFORM 2000-VERIFY-POLICY
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMPOLVR: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO CA-PV-VERIFIED
           MOVE SPACES                 TO CA-PV-POLICY-TYPE
                                          CA-PV-EFF-DATE
                                          CA-PV-EXP-DATE
                                          CA-PV-INSURED-NAME
                                          CA-PV-STATUS-AT-LOSS
           MOVE 'UNKN'                 TO CA-PV-BILLING-STATUS
           MOVE 0                      TO CA-PV-CUSTOMER-NUM
                                          CA-PV-PRODUCER-CODE
                                          CA-PV-COV-COUNT
                                          CA-PV-RU-COUNT
           MOVE SPACES                 TO WS-WARNING
           MOVE 'N'                    TO WS-LINE-FOUND
                                          WS-ISSUE-NULL
                                          WS-EXPIRY-NULL
                                          WS-FLOOD-SW
           MOVE CA-PV-POLICY-NUMBER    TO WS-POL-KEY-NUM.

      ******************************************************************
       2000-VERIFY-POLICY.
           PERFORM 3000-VALIDATE-REQUEST
           PERFORM 4000-READ-POLICY
           PERFORM 4100-READ-CUSTOMER

           EVALUATE CA-PV-POLICY-TYPE
      * BR-CLM-POL-003 Endowment is life business, never verified here
              WHEN 'E'
                 MOVE 'LIFE'           TO CA-PV-STATUS-AT-LOSS
                 MOVE 'LIFEPOL '       TO WS-W-REASON
                 MOVE 'Endowment policy is not a P&C policy'
                                       TO WS-W-MESSAGE
                 PERFORM 3100-SET-WARNING
                 PERFORM 9000-RETURN
              WHEN 'M'
                 PERFORM 4200-READ-MOTOR
                 IF WS-LINE-WAS-FOUND
                    PERFORM 5100-BUILD-MOTOR
                 END-IF
              WHEN 'H'
                 PERFORM 4300-READ-HOUSE
                 IF WS-LINE-WAS-FOUND
                    PERFORM 5200-BUILD-HOUSE
                 END-IF
              WHEN 'C'
                 PERFORM 4400-READ-COMMERCIAL
                 IF WS-LINE-WAS-FOUND
                    PERFORM 5300-BUILD-COMMERCIAL
                 END-IF
              WHEN OTHER
                 MOVE 'UNKN'           TO CA-PV-STATUS-AT-LOSS
                 MOVE 'POLTYPUK'       TO WS-W-REASON
                 STRING 'GenApp policy type ' CA-PV-POLICY-TYPE
                        ' is not recognised' DELIMITED BY SIZE
                        INTO WS-W-MESSAGE
                 PERFORM 3100-SET-WARNING
                 PERFORM 9000-RETURN
           END-EVALUATE

           IF NOT WS-LINE-WAS-FOUND
              MOVE 'LINENTFD'          TO WS-W-REASON
              MOVE 'Policy line details missing in GenApp'
                                       TO WS-W-MESSAGE
              PERFORM 3100-SET-WARNING
           END-IF

           PERFORM 5000-DETERMINE-STATUS
           PERFORM 4500-READ-BILLING-STATUS
           PERFORM 5400-APPLY-BILLING-STATUS.

      ******************************************************************
       3000-VALIDATE-REQUEST.
           IF CA-PV-POLICY-NUMBER NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'POLNUMRQ'          TO CA-REASON-CODE
              MOVE 'Policy number is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTC-REQUEST-ID
           MOVE CA-PV-LOSS-DATE        TO DTC-DT-DATE-1
           PERFORM 8100-CALL-DATES
           IF NOT DTC-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LOSSDTIV'          TO CA-REASON-CODE
              STRING 'Loss date ' CA-PV-LOSS-DATE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-POL-015 Only the first warning is reported; a later
      * warning never masks the one that explains the real problem.
       3100-SET-WARNING.
           IF CA-RC-OK
              SET CA-RC-WARNING        TO TRUE
              MOVE WS-W-REASON         TO CA-REASON-CODE
              MOVE WS-W-MESSAGE        TO CA-MESSAGE
           END-IF
           MOVE SPACES                 TO WS-WARNING.

      ******************************************************************
       4000-READ-POLICY.
           MOVE CA-PV-POLICY-NUMBER    TO GPO-POLICYNUMBER
           EXEC SQL
              SELECT CUSTOMERNUMBER, ISSUEDATE, EXPIRYDATE,
                     POLICYTYPE, BROKERID
                INTO :GPO-CUSTOMERNUMBER,
                     :GPO-ISSUEDATE    :GPO-ISSUEDATE-IND,
                     :GPO-EXPIRYDATE   :GPO-EXPIRYDATE-IND,
                     :GPO-POLICYTYPE   :GPO-POLICYTYPE-IND,
                     :GPO-BROKERID     :GPO-BROKERID-IND
                FROM GENAPP.POLICY
               WHERE POLICYNUMBER = :GPO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'POLNOTFD'       TO CA-REASON-CODE
                 MOVE 'NFND'           TO CA-PV-STATUS-AT-LOSS
                 MOVE 'Policy not found in GenApp'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'GenApp policy row locked - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-POLICY' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE

           MOVE GPO-CUSTOMERNUMBER     TO CA-PV-CUSTOMER-NUM
           IF GPO-POLICYTYPE-IND < 0
              MOVE SPACES              TO CA-PV-POLICY-TYPE
           ELSE
              MOVE GPO-POLICYTYPE      TO CA-PV-POLICY-TYPE
           END-IF
           IF GPO-BROKERID-IND < 0
              MOVE 0                   TO CA-PV-PRODUCER-CODE
           ELSE
              MOVE GPO-BROKERID        TO CA-PV-PRODUCER-CODE
           END-IF
      * BR-CLM-POL-002 Null dates: no issue date cannot be verified,
      * no expiry date is treated as continuous (open-ended) cover
           IF GPO-ISSUEDATE-IND < 0
              MOVE 'Y'                 TO WS-ISSUE-NULL
              MOVE SPACES              TO CA-PV-EFF-DATE
           ELSE
              MOVE 'N'                 TO WS-ISSUE-NULL
              MOVE GPO-ISSUEDATE       TO CA-PV-EFF-DATE
           END-IF
           IF GPO-EXPIRYDATE-IND < 0
              MOVE 'Y'                 TO WS-EXPIRY-NULL
              MOVE '9999-12-31'        TO CA-PV-EXP-DATE
           ELSE
              MOVE 'N'                 TO WS-EXPIRY-NULL
              MOVE GPO-EXPIRYDATE      TO CA-PV-EXP-DATE
           END-IF.

      ******************************************************************
       4100-READ-CUSTOMER.
           MOVE GPO-CUSTOMERNUMBER     TO GCU-CUSTOMERNUMBER
           EXEC SQL
              SELECT FIRSTNAME, LASTNAME
                INTO :GCU-FIRSTNAME :GCU-FIRSTNAME-IND,
                     :GCU-LASTNAME  :GCU-LASTNAME-IND
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
                 MOVE SPACES           TO WS-INSURED-NAME
                 IF GCU-FIRSTNAME = SPACES
                    MOVE GCU-LASTNAME  TO WS-INSURED-NAME
                 ELSE
                    STRING GCU-FIRSTNAME DELIMITED BY '  '
                           ' '           DELIMITED BY SIZE
                           GCU-LASTNAME  DELIMITED BY '  '
                           INTO WS-INSURED-NAME
                 END-IF
                 MOVE WS-INSURED-NAME  TO CA-PV-INSURED-NAME
              WHEN 100
                 MOVE 'UNKNOWN CUSTOMER' TO CA-PV-INSURED-NAME
                 MOVE 'CUSNOTFD'       TO WS-W-REASON
                 MOVE 'Policy customer not found in GenApp'
                                       TO WS-W-MESSAGE
                 PERFORM 3100-SET-WARNING
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'GenApp customer row locked - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CUSTOMER' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-MOTOR.
           MOVE CA-PV-POLICY-NUMBER    TO GMO-POLICYNUMBER
           EXEC SQL
              SELECT MAKE, MODEL, VALUE, REGNUMBER, COLOUR,
                     YEAROFMANUFACTURE
                INTO :GMO-MAKE   :GMO-MAKE-IND,
                     :GMO-MODEL  :GMO-MODEL-IND,
                     :GMO-VALUE  :GMO-VALUE-IND,
                     :GMO-REGNUMBER :GMO-REGNUMBER-IND,
                     :GMO-COLOUR :GMO-COLOUR-IND,
                     :GMO-YEAROFMANUFACTURE
                                 :GMO-YEAROFMANUFACTURE-IND
                FROM GENAPP.MOTOR
               WHERE POLICYNUMBER = :GMO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-LINE-WAS-FOUND TO TRUE
                 IF GMO-MAKE-IND < 0
                    MOVE SPACES        TO GMO-MAKE
                 END-IF
                 IF GMO-MODEL-IND < 0
                    MOVE SPACES        TO GMO-MODEL
                 END-IF
                 IF GMO-VALUE-IND < 0
                    MOVE 0             TO GMO-VALUE
                 END-IF
                 IF GMO-REGNUMBER-IND < 0
                    MOVE SPACES        TO GMO-REGNUMBER
                 END-IF
                 IF GMO-COLOUR-IND < 0
                    MOVE SPACES        TO GMO-COLOUR
                 END-IF
                 IF GMO-YEAROFMANUFACTURE-IND < 0
                    MOVE SPACES        TO GMO-YEAROFMANUFACTURE
                 END-IF
              WHEN 100
                 MOVE 'N'              TO WS-LINE-FOUND
              WHEN OTHER
                 MOVE '4200-READ-MOTOR' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-READ-HOUSE.
           MOVE CA-PV-POLICY-NUMBER    TO GHO-POLICYNUMBER
           EXEC SQL
              SELECT PROPERTYTYPE, BEDROOMS, VALUE, HOUSENAME,
                     HOUSENUMBER, POSTCODE
                INTO :GHO-PROPERTYTYPE :GHO-PROPERTYTYPE-IND,
                     :GHO-BEDROOMS     :GHO-BEDROOMS-IND,
                     :GHO-VALUE        :GHO-VALUE-IND,
                     :GHO-HOUSENAME    :GHO-HOUSENAME-IND,
                     :GHO-HOUSENUMBER  :GHO-HOUSENUMBER-IND,
                     :GHO-POSTCODE     :GHO-POSTCODE-IND
                FROM GENAPP.HOUSE
               WHERE POLICYNUMBER = :GHO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-LINE-WAS-FOUND TO TRUE
                 IF GHO-PROPERTYTYPE-IND < 0
                    MOVE SPACES        TO GHO-PROPERTYTYPE
                 END-IF
                 IF GHO-BEDROOMS-IND < 0
                    MOVE 0             TO GHO-BEDROOMS
                 END-IF
                 IF GHO-VALUE-IND < 0
                    MOVE 0             TO GHO-VALUE
                 END-IF
                 IF GHO-HOUSENAME-IND < 0
                    MOVE SPACES        TO GHO-HOUSENAME
                 END-IF
                 IF GHO-HOUSENUMBER-IND < 0
                    MOVE SPACES        TO GHO-HOUSENUMBER
                 END-IF
                 IF GHO-POSTCODE-IND < 0
                    MOVE SPACES        TO GHO-POSTCODE
                 END-IF
              WHEN 100
                 MOVE 'N'              TO WS-LINE-FOUND
              WHEN OTHER
                 MOVE '4300-READ-HOUSE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-READ-COMMERCIAL.
           MOVE CA-PV-POLICY-NUMBER    TO GCO-POLICYNUMBER
           EXEC SQL
              SELECT ADDRESS, ZIPCODE, PROPERTYTYPE,
                     FIREPREMIUM, CRIMEPREMIUM, FLOODPREMIUM,
                     WEATHERPREMIUM
                INTO :GCO-ADDRESS        :GCO-ADDRESS-IND,
                     :GCO-ZIPCODE        :GCO-ZIPCODE-IND,
                     :GCO-PROPERTYTYPE   :GCO-PROPERTYTYPE-IND,
                     :GCO-FIREPREMIUM    :GCO-FIREPREMIUM-IND,
                     :GCO-CRIMEPREMIUM   :GCO-CRIMEPREMIUM-IND,
                     :GCO-FLOODPREMIUM   :GCO-FLOODPREMIUM-IND,
                     :GCO-WEATHERPREMIUM :GCO-WEATHERPREMIUM-IND
                FROM GENAPP.COMMERCIAL
               WHERE POLICYNUMBER = :GCO-POLICYNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-LINE-WAS-FOUND TO TRUE
                 IF GCO-ADDRESS-IND < 0
                    MOVE SPACES        TO GCO-ADDRESS
                 END-IF
                 IF GCO-ZIPCODE-IND < 0
                    MOVE SPACES        TO GCO-ZIPCODE
                 END-IF
                 IF GCO-PROPERTYTYPE-IND < 0
                    MOVE SPACES        TO GCO-PROPERTYTYPE
                 END-IF
                 IF GCO-FIREPREMIUM-IND < 0
                    MOVE 0             TO GCO-FIREPREMIUM
                 END-IF
                 IF GCO-CRIMEPREMIUM-IND < 0
                    MOVE 0             TO GCO-CRIMEPREMIUM
                 END-IF
                 IF GCO-FLOODPREMIUM-IND < 0
                    MOVE 0             TO GCO-FLOODPREMIUM
                 END-IF
                 IF GCO-WEATHERPREMIUM-IND < 0
                    MOVE 0             TO GCO-WEATHERPREMIUM
                 END-IF
              WHEN 100
                 MOVE 'N'              TO WS-LINE-FOUND
              WHEN OTHER
                 MOVE '4400-READ-COMMERCIAL' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-POL-013 Billing status is the latest inbound BILSTAT
      * event delivered by BillingCore for this policy; EVENT_KEY is
      * the 10-digit zero-padded policy number (EVENTS.md payload).
       4500-READ-BILLING-STATUS.
           MOVE CA-PV-POLICY-NUMBER    TO WS-POL-KEY-NUM
           MOVE SPACES                 TO WS-POL-KEY
           MOVE WS-POL-KEY-NUM         TO WS-POL-KEY(1:10)
           MOVE WS-POL-KEY             TO EVQ-EVENT-KEY
           EXEC SQL
              SELECT PAYLOAD
                INTO :EVQ-PAYLOAD
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_TYPE   = 'BILSTAT'
                 AND DIRECTION    = 'I'
                 AND EVENT_KEY    = :EVQ-EVENT-KEY
                 AND EVENT_STATUS <> 'ERRR'
               ORDER BY CREATE_TS DESC, EVENT_ID DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE SPACES           TO WS-BILSTAT-PAYLOAD
                 IF EVQ-PAYLOAD-LEN > 0
                    MOVE EVQ-PAYLOAD-TEXT(1:EVQ-PAYLOAD-LEN)
                                       TO WS-BILSTAT-PAYLOAD
                 END-IF
                 IF WS-BS-POLICY-NO = WS-POL-KEY(1:10)
                    AND WS-BS-KNOWN
                    MOVE WS-BS-STATUS  TO CA-PV-BILLING-STATUS
                 ELSE
                    MOVE 'UNKN'        TO CA-PV-BILLING-STATUS
                 END-IF
              WHEN 100
                 MOVE 'UNKN'           TO CA-PV-BILLING-STATUS
              WHEN -911
              WHEN -913
                 MOVE 'UNKN'           TO CA-PV-BILLING-STATUS
                 MOVE 'BILLLOCK'       TO WS-W-REASON
                 MOVE 'Billing status unavailable (queue locked)'
                                       TO WS-W-MESSAGE
                 PERFORM 3100-SET-WARNING
              WHEN OTHER
                 MOVE '4500-READ-BILLING-STATUS' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       5000-DETERMINE-STATUS.
           IF WS-ISSUE-NULL = 'Y'
              MOVE 'NODT'              TO CA-PV-STATUS-AT-LOSS
              MOVE 'N'                 TO CA-PV-VERIFIED
              MOVE 'POLNODTE'          TO WS-W-REASON
              MOVE 'Policy has no issue date - cannot verify'
                                       TO WS-W-MESSAGE
              PERFORM 3100-SET-WARNING
           ELSE
      * BR-CLM-POL-001 In force when issue <= loss <= expiry (ISO
      * dates compare correctly as text; no arithmetic involved)
              IF CA-PV-LOSS-DATE >= CA-PV-EFF-DATE
                 AND CA-PV-LOSS-DATE <= CA-PV-EXP-DATE
                 MOVE 'INFC'           TO CA-PV-STATUS-AT-LOSS
      * BR-CLM-POL-012 Verified only with at least one coverage
                 IF CA-PV-COV-COUNT > 0
                    MOVE 'Y'           TO CA-PV-VERIFIED
                 ELSE
                    MOVE 'N'           TO CA-PV-VERIFIED
                    MOVE 'NOCOVER '    TO WS-W-REASON
                    MOVE 'No coverage could be derived from GenApp'
                                       TO WS-W-MESSAGE
                    PERFORM 3100-SET-WARNING
                 END-IF
              ELSE
                 MOVE 'OUTP'           TO CA-PV-STATUS-AT-LOSS
                 MOVE 'N'              TO CA-PV-VERIFIED
                 MOVE 'LOSSOUTP'       TO WS-W-REASON
                 STRING 'Loss date outside policy period '
                        CA-PV-EFF-DATE ' to ' CA-PV-EXP-DATE
                        DELIMITED BY SIZE INTO WS-W-MESSAGE
                 PERFORM 3100-SET-WARNING
              END-IF
           END-IF.

      ******************************************************************
       5100-BUILD-MOTOR.
           MOVE GMO-VALUE              TO WS-VALUE
           MOVE SPACES                 TO WS-RU-DESC
           MOVE GMO-YEAROFMANUFACTURE(1:4) TO WS-YEAR-TEXT
           STRING GMO-MAKE    DELIMITED BY '  '
                  ' '         DELIMITED BY SIZE
                  GMO-MODEL   DELIMITED BY '  '
                  ' '         DELIMITED BY SIZE
                  GMO-COLOUR  DELIMITED BY '  '
                  ' '         DELIMITED BY SIZE
                  WS-YEAR-TEXT DELIMITED BY SPACE
                  INTO WS-RU-DESC
           ADD 1                       TO CA-PV-RU-COUNT
           MOVE 'VEH '                 TO CA-PV-R-TYPE (1)
           MOVE WS-RU-DESC             TO CA-PV-R-DESC (1)
           MOVE GMO-REGNUMBER          TO CA-PV-R-REG (1)
           MOVE GMO-MAKE               TO CA-PV-R-MAKE (1)
           MOVE GMO-MODEL              TO CA-PV-R-MODEL (1)
           MOVE SPACES                 TO CA-PV-R-POSTCODE (1)
           MOVE WS-VALUE               TO CA-PV-R-VALUE (1)

      * BR-CLM-POL-006 Own-damage cover only with a known value
           IF WS-VALUE > 0
      * BR-CLM-POL-004 Collision and comprehensive at vehicle value
              MOVE 'COLL    '          TO WS-NC-TYPE
              MOVE 1                   TO WS-NC-RU-SEQ
              MOVE WS-VALUE            TO WS-NC-LIMIT
              MOVE WS-DED-COLL         TO WS-NC-DEDUCTIBLE
              PERFORM 5800-ADD-COVERAGE
              MOVE 'COMP    '          TO WS-NC-TYPE
              MOVE WS-DED-COMP         TO WS-NC-DEDUCTIBLE
              PERFORM 5800-ADD-COVERAGE
           ELSE
              MOVE 'NOVALUE '          TO WS-W-REASON
              MOVE 'Vehicle value unknown - no own-damage cover'
                                       TO WS-W-MESSAGE
              PERFORM 3100-SET-WARNING
           END-IF
      * BR-CLM-POL-005 Third-party liability limits are fixed
           MOVE 'LIAB_BI '             TO WS-NC-TYPE
           MOVE 1                      TO WS-NC-RU-SEQ
           MOVE WS-LIM-LIAB-BI         TO WS-NC-LIMIT
           MOVE 0                      TO WS-NC-DEDUCTIBLE
           PERFORM 5800-ADD-COVERAGE
           MOVE 'LIAB_PD '             TO WS-NC-TYPE
           MOVE WS-LIM-LIAB-PD         TO WS-NC-LIMIT
           PERFORM 5800-ADD-COVERAGE.

      ******************************************************************
       5200-BUILD-HOUSE.
           MOVE GHO-VALUE              TO WS-VALUE
           MOVE GHO-BEDROOMS           TO WS-BEDROOMS-DISP
           MOVE SPACES                 TO WS-RU-DESC
           STRING GHO-PROPERTYTYPE  DELIMITED BY '  '
                  ', '              DELIMITED BY SIZE
                  WS-BEDROOMS-DISP  DELIMITED BY SIZE
                  ' BED, '          DELIMITED BY SIZE
                  GHO-HOUSENUMBER   DELIMITED BY SPACE
                  ' '               DELIMITED BY SIZE
                  GHO-HOUSENAME     DELIMITED BY '  '
                  INTO WS-RU-DESC
           ADD 1                       TO CA-PV-RU-COUNT
           MOVE 'DWEL'                 TO CA-PV-R-TYPE (1)
           MOVE WS-RU-DESC             TO CA-PV-R-DESC (1)
           MOVE SPACES                 TO CA-PV-R-REG (1)
                                          CA-PV-R-MAKE (1)
                                          CA-PV-R-MODEL (1)
           MOVE GHO-POSTCODE           TO CA-PV-R-POSTCODE (1)
           MOVE WS-VALUE               TO CA-PV-R-VALUE (1)

      * BR-CLM-POL-006 Property cover only with a known value
           IF WS-VALUE NOT > 0
              MOVE 'NOVALUE '          TO WS-W-REASON
              MOVE 'Property value unknown - no cover derived'
                                       TO WS-W-MESSAGE
              PERFORM 3100-SET-WARNING
           ELSE
      * BR-CLM-POL-007 Buildings cover at declared value
              MOVE 'DWELLING'          TO WS-NC-TYPE
              MOVE 1                   TO WS-NC-RU-SEQ
              MOVE WS-VALUE            TO WS-NC-LIMIT
              MOVE WS-DED-DWELLING     TO WS-NC-DEDUCTIBLE
              PERFORM 5800-ADD-COVERAGE
      * BR-CLM-POL-008 Contents sum insured is 30% of dwelling value
              INITIALIZE WS-MNY-CA
              MOVE 'MNRATE  '          TO MNC-REQUEST-ID
              MOVE WS-VALUE            TO MNC-MN-TOTAL
              MOVE WS-CONTENTS-BPS     TO MNC-MN-RATE-BPS
              PERFORM 8200-CALL-MONEY
              MOVE 'CONTENTS'          TO WS-NC-TYPE
              MOVE MNC-MN-RESULT       TO WS-NC-LIMIT
              MOVE WS-DED-CONTENTS     TO WS-NC-DEDUCTIBLE
              PERFORM 5800-ADD-COVERAGE
           END-IF.

      ******************************************************************
       5300-BUILD-COMMERCIAL.
           MOVE 0                      TO WS-BLDG-LIMIT
                                          WS-PERIL-COUNT
           MOVE 'N'                    TO WS-FLOOD-SW
      * BR-CLM-POL-009 A peril is covered only when it carries premium
      * BR-CLM-POL-010 Sum insured = premium x peril multiplier
           IF GCO-FIREPREMIUM > 0
              COMPUTE WS-BLDG-LIMIT = WS-BLDG-LIMIT
                                    + GCO-FIREPREMIUM * WS-MULT-FIRE
              ADD 1                    TO WS-PERIL-COUNT
           END-IF
           IF GCO-FLOODPREMIUM > 0
              COMPUTE WS-BLDG-LIMIT = WS-BLDG-LIMIT
                                    + GCO-FLOODPREMIUM * WS-MULT-FLOOD
              ADD 1                    TO WS-PERIL-COUNT
              SET WS-FLOOD-COVERED     TO TRUE
           END-IF
           IF GCO-WEATHERPREMIUM > 0
              COMPUTE WS-BLDG-LIMIT = WS-BLDG-LIMIT
                                + GCO-WEATHERPREMIUM * WS-MULT-WEATHER
              ADD 1                    TO WS-PERIL-COUNT
           END-IF
           IF GCO-CRIMEPREMIUM > 0
              COMPUTE WS-BLDG-LIMIT = WS-BLDG-LIMIT
                                    + GCO-CRIMEPREMIUM * WS-MULT-CRIME
              ADD 1                    TO WS-PERIL-COUNT
           END-IF
           IF WS-PERIL-COUNT = 0
              MOVE 'NOPERIL '          TO WS-W-REASON
              MOVE 'Commercial policy has no premium-bearing peril'
                                       TO WS-W-MESSAGE
              PERFORM 3100-SET-WARNING
           ELSE
              IF WS-BLDG-LIMIT > WS-BLDG-MAX
                 MOVE WS-BLDG-MAX      TO WS-BLDG-LIMIT
              END-IF
              IF WS-FLOOD-COVERED
                 MOVE WS-DED-BLDG-FLOOD TO WS-BLDG-DED
              ELSE
                 MOVE WS-DED-BLDG      TO WS-BLDG-DED
              END-IF

              MOVE SPACES              TO WS-RU-DESC
              STRING GCO-PROPERTYTYPE(1:25) DELIMITED BY '  '
                     ', '                   DELIMITED BY SIZE
                     GCO-ADDRESS(1:33)      DELIMITED BY '  '
                     INTO WS-RU-DESC
              ADD 1                    TO CA-PV-RU-COUNT
              MOVE 'BLDG'              TO CA-PV-R-TYPE (1)
              MOVE WS-RU-DESC          TO CA-PV-R-DESC (1)
              MOVE SPACES              TO CA-PV-R-REG (1)
                                          CA-PV-R-MAKE (1)
                                          CA-PV-R-MODEL (1)
              MOVE GCO-ZIPCODE         TO CA-PV-R-POSTCODE (1)
              MOVE WS-BLDG-LIMIT       TO CA-PV-R-VALUE (1)

              MOVE 'BLDG    '          TO WS-NC-TYPE
              MOVE 1                   TO WS-NC-RU-SEQ
              MOVE WS-BLDG-LIMIT       TO WS-NC-LIMIT
              MOVE WS-BLDG-DED         TO WS-NC-DEDUCTIBLE
              PERFORM 5800-ADD-COVERAGE

      * BR-CLM-POL-011 Business interruption is 25% of buildings,
      * never less than the 25,000 minimum indemnity
              INITIALIZE WS-MNY-CA
              MOVE 'MNRATE  '          TO MNC-REQUEST-ID
              MOVE WS-BLDG-LIMIT       TO MNC-MN-TOTAL
              MOVE WS-BUSINT-BPS       TO MNC-MN-RATE-BPS
              PERFORM 8200-CALL-MONEY
              MOVE MNC-MN-RESULT       TO WS-BUSINT-LIMIT
              IF WS-BUSINT-LIMIT < WS-BUSINT-MIN
                 MOVE WS-BUSINT-MIN    TO WS-BUSINT-LIMIT
              END-IF
              MOVE 'BUSINT  '          TO WS-NC-TYPE
              MOVE WS-BUSINT-LIMIT     TO WS-NC-LIMIT
              MOVE 0                   TO WS-NC-DEDUCTIBLE
              PERFORM 5800-ADD-COVERAGE
           END-IF.

      ******************************************************************
      * BR-CLM-POL-014 Adverse billing on an otherwise in-force policy
      * flags the claim for coverage review but does not block FNOL
       5400-APPLY-BILLING-STATUS.
           MOVE CA-PV-BILLING-STATUS   TO WS-BS-STATUS
           IF CA-PV-STATUS-AT-LOSS = 'INFC'
              AND WS-BS-ADVERSE
              MOVE 'BILL'              TO CA-PV-STATUS-AT-LOSS
              MOVE 'BILLWARN'          TO WS-W-REASON
              STRING 'Billing status ' CA-PV-BILLING-STATUS
                     ' - premium not current at loss'
                     DELIMITED BY SIZE INTO WS-W-MESSAGE
              PERFORM 3100-SET-WARNING
           END-IF.

      ******************************************************************
       5800-ADD-COVERAGE.
           IF CA-PV-COV-COUNT < WS-MAX-COV
              ADD 1                    TO CA-PV-COV-COUNT
              MOVE WS-NC-TYPE   TO CA-PV-C-TYPE (CA-PV-COV-COUNT)
              MOVE WS-NC-RU-SEQ TO CA-PV-C-RU-SEQ (CA-PV-COV-COUNT)
              MOVE WS-NC-LIMIT  TO CA-PV-C-LIMIT (CA-PV-COV-COUNT)
              MOVE WS-NC-DEDUCTIBLE
                                TO CA-PV-C-DEDUCTIBLE (CA-PV-COV-COUNT)
           END-IF.

      ******************************************************************
       8100-CALL-DATES.
           MOVE CA-USER-ID             TO DTC-USER-ID
           MOVE CA-BUS-DATE            TO DTC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 12
              AND DTC-REQUEST-ID NOT = 'DTVALD  '
              MOVE DTC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTC-REASON-CODE     TO CA-REASON-CODE
              MOVE DTC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8200-CALL-MONEY.
           MOVE CA-USER-ID             TO MNC-USER-ID
           MOVE CA-BUS-DATE            TO MNC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO MNC-CALLER-PGM
           CALL WS-MNY-PGM USING WS-MNY-CA
           IF MNC-RETURN-CODE >= 12
              MOVE MNC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE MNC-REASON-CODE     TO CA-REASON-CODE
              MOVE MNC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'CLMPOLVR Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-POL-KEY-NUM         TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
