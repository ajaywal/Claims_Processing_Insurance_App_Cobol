       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSRVRQ.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMSRVRQ                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Vendor service requests (appraisal, legal,       *
      *               repair, rental, tow). Picks the best preferred   *
      *               vendor when none is named, records the quote and *
      *               on completion pays the vendor invoice through    *
      *               CLMFINTX against the exposure.                   *
      * REQUESTS    : SRQNEW SRQQTE SRQCMP SRQCAN                      *
      * CALLED BY   : exposure / service screens, FNOL auto-tow        *
      * CALLS       : CLMSEQGN CLMCONTC CLMFINTX CLMHIST0 CLMERRLG     *
      * TABLES      : CLM_SERVICE_REQUEST (C R U)  CLM_CLAIM (R)       *
      *               CLM_EXPOSURE (R)  CLM_CONTACT (R)                *
      *               CLM_CLAIM_CONTACT (R)  CLM_CONTROL (R)           *
      * RULES       : BR-CLM-SRQ-001 Service type APPRAISL, LEGAL,     *
      *                              REPAIR, RENTAL or TOW             *
      *               BR-CLM-SRQ-002 Requests need an OPEN claim and   *
      *                              an open exposure of that claim    *
      *               BR-CLM-SRQ-003 Auto-pick: preferred vendors of   *
      *                              the service type, highest rating, *
      *                              loss postcode area match first    *
      *               BR-CLM-SRQ-004 Named vendor must be of the       *
      *                              service type and sanction-clear   *
      *               BR-CLM-SRQ-005 One active request per exposure   *
      *                              and service type                  *
      *               BR-CLM-SRQ-006 Quote / invoice amounts > 0; only *
      *                              RQST or QUOT requests change      *
      *               BR-CLM-SRQ-007 Invoice pays the vendor: cost     *
      *                              type EXAO for appraisal / legal,  *
      *                              CLMC for repair / rental / tow    *
      *               BR-CLM-SRQ-008 Invoice more than 10% over the    *
      *                              quote completes with a warning    *
      *               BR-CLM-SRQ-009 The vendor is linked to the claim *
      *                              in role VENDOR                    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMSRVRQ'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-CON-PGM                  PIC X(8)  VALUE 'CLMCONTC'.
       01  WS-FIN-PGM                  PIC X(8)  VALUE 'CLMFINTX'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-SERVICE-TYPE          PIC X(8)  VALUE SPACES.
              88 WS-VALID-SERVICE      VALUE 'APPRAISL' 'LEGAL   '
                                             'REPAIR  ' 'RENTAL  '
                                             'TOW     '.
              88 WS-EXPENSE-SERVICE    VALUE 'APPRAISL' 'LEGAL   '.
           05 WS-COST-TYPE             PIC X(4)  VALUE SPACES.
           05 WS-PC-IN                 PIC X(8)  VALUE SPACES.
           05 WS-PC-AREA               PIC X(2)  VALUE SPACES.
           05 WS-LOSS-AREA             PIC X(2)  VALUE SPACES.
           05 WS-PC-IX                 PIC S9(4) COMP VALUE 0.
           05 WS-BEST-ID               PIC S9(9) COMP VALUE 0.
           05 WS-AREA-ID               PIC S9(9) COMP VALUE 0.
           05 WS-EXP-CLAIM-ID          PIC S9(9) COMP VALUE 0.
           05 WS-EXP-STATE             PIC X(4)  VALUE SPACES.
           05 WS-QUOTE-CEILING         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMT-DISP              PIC Z(10)9.99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-HIS-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-OVER-QUOTE            PIC X     VALUE 'N'.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-DONE                   VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.
           05 WS-CSR-VND-SW            PIC X     VALUE 'N'.
              88 WS-CSR-VND-OPEN                 VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDSRQ END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CSR-VENDORS CURSOR FOR
              SELECT CONTACT_ID, POSTCODE, VENDOR_RATING
                FROM CLMDB.CLM_CONTACT
               WHERE VENDOR_TYPE     = :WS-SERVICE-TYPE
                 AND PREFERRED_FLAG  = 'Y'
                 AND SANCTION_STATUS <> 'HIT '
               ORDER BY VENDOR_RATING DESC, CONTACT_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-SVC-RESULT.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SVC-==.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

       01  WS-CON-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==COA-==.
           03 COA-BODY.
              COPY CLMCCON REPLACING LEADING ==CA-== BY ==COA-==.

       01  WS-FIN-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==FTA-==.
           03 FTA-BODY.
              COPY CLMCFIN REPLACING LEADING ==CA-== BY ==FTA-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCSRQ.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'SRQNEW  '
                 PERFORM 2100-NEW-REQUEST
              WHEN 'SRQQTE  '
                 PERFORM 2200-RECORD-QUOTE
              WHEN 'SRQCMP  '
                 PERFORM 2300-COMPLETE-REQUEST
              WHEN 'SRQCAN  '
                 PERFORM 2400-CANCEL-REQUEST
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMSRVRQ: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
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
      * SRQNEW
       2100-NEW-REQUEST.
           PERFORM 3100-VALIDATE-NEW
           IF CA-SR-VENDOR-ID > 0
              PERFORM 3200-CHECK-NAMED-VENDOR
           ELSE
              PERFORM 5100-PICK-VENDOR
           END-IF
      * BR-CLM-SRQ-005 One active request per exposure and type
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_SERVICE_REQUEST
               WHERE EXPOSURE_ID  = :CA-SR-EXPOSURE-ID
                 AND SERVICE_TYPE = :WS-SERVICE-TYPE
                 AND SR_STATUS IN ('RQST', 'QUOT')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-NEW-REQUEST'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'SRQACTIV'          TO CA-REASON-CODE
              STRING 'An active ' WS-SERVICE-TYPE
                     ' request already exists on this exposure'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8000-NEXT-SR-ID
           MOVE CA-SR-CLAIM-ID         TO SRQ-CLAIM-ID
           MOVE CA-SR-EXPOSURE-ID      TO SRQ-EXPOSURE-ID
           MOVE CA-SR-VENDOR-ID        TO SRQ-VENDOR-CONTACT-ID
           MOVE WS-SERVICE-TYPE        TO SRQ-SERVICE-TYPE
           MOVE 'RQST'                 TO SRQ-SR-STATUS
           MOVE WS-BUS-DATE            TO SRQ-REQUEST-DATE
           EXEC SQL
              INSERT INTO CLMDB.CLM_SERVICE_REQUEST
                     (SR_ID, CLAIM_ID, EXPOSURE_ID, VENDOR_CONTACT_ID,
                      SERVICE_TYPE, SR_STATUS, REQUEST_DATE,
                      QUOTE_AMOUNT, INVOICE_AMOUNT, COMPLETED_DATE,
                      PAYMENT_TXN_ID, UPDATE_TS)
              VALUES (:SRQ-SR-ID, :SRQ-CLAIM-ID, :SRQ-EXPOSURE-ID,
                      :SRQ-VENDOR-CONTACT-ID, :SRQ-SERVICE-TYPE,
                      :SRQ-SR-STATUS, :SRQ-REQUEST-DATE, 0, 0, NULL,
                      0, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE SRQ-SR-ID        TO WS-ID-DISP
                 STRING 'Service request ' WS-ID-DISP
                        ' already exists' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2100-NEW-REQUEST' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           PERFORM 8100-LINK-VENDOR
           MOVE SRQ-SR-ID              TO CA-SR-SR-ID
           MOVE 'RQST'                 TO CA-SR-STATUS
           MOVE 'SRQNEW  '             TO WS-HIS-EVENT
           MOVE CA-SR-VENDOR-ID        TO WS-ID-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING WS-SERVICE-TYPE ' requested from vendor '
                  WS-ID-DISP DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           MOVE SRQ-SR-ID              TO WS-ID-DISP
           STRING 'Service request ' WS-ID-DISP ' raised'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * SRQQTE - a re-quote replaces the previous quote
       2200-RECORD-QUOTE.
           PERFORM 3300-LOAD-ACTIVE-REQUEST
           IF CA-SR-QUOTE-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'QUOTEAMT'          TO CA-REASON-CODE
              MOVE 'Quote amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              UPDATE CLMDB.CLM_SERVICE_REQUEST
                 SET SR_STATUS    = 'QUOT',
                     QUOTE_AMOUNT = :CA-SR-QUOTE-AMOUNT,
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE SR_ID        = :SRQ-SR-ID
                 AND UPDATE_TS    = :SRQ-UPDATE-TS
           END-EXEC
           PERFORM 4200-CHECK-UPDATE
           MOVE 'QUOT'                 TO CA-SR-STATUS
           MOVE CA-SR-QUOTE-AMOUNT     TO WS-AMT-DISP
           MOVE 'SRQQTE  '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING SRQ-SERVICE-TYPE ' quote received ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           STRING 'Quote of ' WS-AMT-DISP ' recorded'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * SRQCMP - complete and pay the vendor invoice
       2300-COMPLETE-REQUEST.
           PERFORM 3300-LOAD-ACTIVE-REQUEST
           IF CA-SR-INVOICE-AMOUNT NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INVCAMT '          TO CA-REASON-CODE
              MOVE 'Invoice amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-SRQ-008 Flag invoices more than 10% over the quote
           MOVE 'N'                    TO WS-OVER-QUOTE
           IF SRQ-QUOTE-AMOUNT > 0
              COMPUTE WS-QUOTE-CEILING ROUNDED
                    = SRQ-QUOTE-AMOUNT * 1.10
              IF CA-SR-INVOICE-AMOUNT > WS-QUOTE-CEILING
                 MOVE 'Y'              TO WS-OVER-QUOTE
              END-IF
           END-IF
      * BR-CLM-SRQ-007 Expense (EXAO) or claim cost (CLMC)
           MOVE SRQ-SERVICE-TYPE       TO WS-SERVICE-TYPE
           IF WS-EXPENSE-SERVICE
              MOVE 'EXAO'              TO WS-COST-TYPE
           ELSE
              MOVE 'CLMC'              TO WS-COST-TYPE
           END-IF
           PERFORM 8200-PAY-VENDOR
           EXEC SQL
              UPDATE CLMDB.CLM_SERVICE_REQUEST
                 SET SR_STATUS      = 'CMPL',
                     INVOICE_AMOUNT = :CA-SR-INVOICE-AMOUNT,
                     COMPLETED_DATE = :WS-BUS-DATE,
                     PAYMENT_TXN_ID = :CA-SR-PAYMENT-TXN-ID,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE SR_ID          = :SRQ-SR-ID
                 AND UPDATE_TS      = :SRQ-UPDATE-TS
           END-EXEC
           PERFORM 4200-CHECK-UPDATE
           MOVE 'CMPL'                 TO CA-SR-STATUS
           MOVE CA-SR-INVOICE-AMOUNT   TO WS-AMT-DISP
           MOVE CA-SR-PAYMENT-TXN-ID   TO WS-ID-DISP
           MOVE 'SRQCMP  '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING SRQ-SERVICE-TYPE ' completed, invoice '
                  WS-AMT-DISP ' paid by txn ' WS-ID-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           IF WS-OVER-QUOTE = 'Y'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'OVERQUOT'          TO CA-REASON-CODE
              STRING 'Completed - invoice ' WS-AMT-DISP
                     ' is over 10% above the quote'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              IF FTA-RC-WARNING
                 SET CA-RC-WARNING     TO TRUE
                 MOVE FTA-REASON-CODE  TO CA-REASON-CODE
                 MOVE FTA-MESSAGE      TO CA-MESSAGE
              ELSE
                 STRING 'Completed - invoice ' WS-AMT-DISP ' paid'
                        DELIMITED BY SIZE INTO CA-MESSAGE
              END-IF
           END-IF.

      ******************************************************************
      * SRQCAN
       2400-CANCEL-REQUEST.
           PERFORM 3300-LOAD-ACTIVE-REQUEST
           EXEC SQL
              UPDATE CLMDB.CLM_SERVICE_REQUEST
                 SET SR_STATUS = 'CANC',
                     UPDATE_TS = CURRENT TIMESTAMP
               WHERE SR_ID     = :SRQ-SR-ID
                 AND UPDATE_TS = :SRQ-UPDATE-TS
           END-EXEC
           PERFORM 4200-CHECK-UPDATE
           MOVE 'CANC'                 TO CA-SR-STATUS
           MOVE 'SRQCAN  '             TO WS-HIS-EVENT
           MOVE SRQ-SR-ID              TO WS-ID-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING SRQ-SERVICE-TYPE ' request ' WS-ID-DISP
                  ' cancelled' DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8300-WRITE-HISTORY
           STRING 'Service request ' WS-ID-DISP ' cancelled'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       3100-VALIDATE-NEW.
           IF CA-SR-CLAIM-ID NOT > 0 OR CA-SR-EXPOSURE-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'KEYREQ  '          TO CA-REASON-CODE
              MOVE 'Claim id and exposure id are required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-SRQ-001 Service type
           MOVE CA-SR-SERVICE-TYPE     TO WS-SERVICE-TYPE
           IF NOT WS-VALID-SERVICE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADSVCTY'          TO CA-REASON-CODE
              STRING 'Service type ' CA-SR-SERVICE-TYPE
                     ' must be APPRAISL LEGAL REPAIR RENTAL or TOW'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-SRQ-002 OPEN claim, open exposure of the claim
           EXEC SQL
              SELECT CLAIM_NUMBER, CLAIM_STATE, LOSS_POSTCODE
                INTO :CLM-CLAIM-NUMBER, :CLM-CLAIM-STATE,
                     :CLM-LOSS-POSTCODE :CLM-LOSS-POSTCODE-IND
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-SR-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-SR-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3100-VALIDATE-NEW' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMNOTOP'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ', not OPEN'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT CLAIM_ID, EXPOSURE_STATE
                INTO :WS-EXP-CLAIM-ID, :WS-EXP-STATE
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :CA-SR-EXPOSURE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO WS-EXP-CLAIM-ID
              WHEN OTHER
                 MOVE '3100-VALIDATE-NEW' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-EXP-CLAIM-ID NOT = CA-SR-CLAIM-ID
              OR WS-EXP-STATE = 'CLSD'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPNOTOP'          TO CA-REASON-CODE
              MOVE CA-SR-EXPOSURE-ID   TO WS-ID-DISP
              STRING 'Exposure ' WS-ID-DISP
                     ' is not an open exposure of this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-SRQ-004 Named vendor: right type, sanction-clear
       3200-CHECK-NAMED-VENDOR.
           EXEC SQL
              SELECT VENDOR_TYPE, SANCTION_STATUS
                INTO :CON-VENDOR-TYPE, :CON-SANCTION-STATUS
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :CA-SR-VENDOR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'VNDNOTFD'       TO CA-REASON-CODE
                 MOVE CA-SR-VENDOR-ID  TO WS-ID-DISP
                 STRING 'Vendor contact ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3200-CHECK-NAMED-VENDOR' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF CON-VENDOR-TYPE NOT = WS-SERVICE-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'VNDTYPE '          TO CA-REASON-CODE
              STRING 'Vendor type ' CON-VENDOR-TYPE
                     ' does not provide ' WS-SERVICE-TYPE
                     DELIMITED BY SIZE INTO CA-MESSAGE
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
      * BR-CLM-SRQ-006 Only RQST / QUOT requests can change
       3300-LOAD-ACTIVE-REQUEST.
           IF CA-SR-SR-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SRIDREQ '          TO CA-REASON-CODE
              MOVE 'Service request id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT SR_ID, CLAIM_ID, EXPOSURE_ID, VENDOR_CONTACT_ID,
                     SERVICE_TYPE, SR_STATUS, QUOTE_AMOUNT, UPDATE_TS
                INTO :SRQ-SR-ID, :SRQ-CLAIM-ID, :SRQ-EXPOSURE-ID,
                     :SRQ-VENDOR-CONTACT-ID, :SRQ-SERVICE-TYPE,
                     :SRQ-SR-STATUS, :SRQ-QUOTE-AMOUNT,
                     :SRQ-UPDATE-TS
                FROM CLMDB.CLM_SERVICE_REQUEST
               WHERE SR_ID = :CA-SR-SR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'SRQNOTFD'       TO CA-REASON-CODE
                 MOVE CA-SR-SR-ID      TO WS-ID-DISP
                 STRING 'Service request ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3300-LOAD-ACTIVE-REQUEST' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF SRQ-SR-STATUS NOT = 'RQST' AND NOT = 'QUOT'
              SET CA-RC-STATE          TO TRUE
              MOVE 'SRQNOTAC'          TO CA-REASON-CODE
              MOVE SRQ-SR-ID           TO WS-ID-DISP
              STRING 'Service request ' WS-ID-DISP ' is '
                     SRQ-SR-STATUS ' - no further changes'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SRQ-CLAIM-ID           TO CA-SR-CLAIM-ID
           MOVE SRQ-EXPOSURE-ID        TO CA-SR-EXPOSURE-ID
           MOVE SRQ-VENDOR-CONTACT-ID  TO CA-SR-VENDOR-ID
           MOVE SRQ-SERVICE-TYPE       TO CA-SR-SERVICE-TYPE.

      ******************************************************************
      * UPDATE_TS acts as the row version for service requests
       4200-CHECK-UPDATE.
           IF SQLCODE NOT = 0
              MOVE '4200-CHECK-UPDATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Service request changed by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-SRQ-003 Highest-rated preferred vendor of the type,
      * preferring one in the loss postcode area
       5100-PICK-VENDOR.
           MOVE SPACES                 TO WS-LOSS-AREA
           IF CLM-LOSS-POSTCODE-IND = 0
              MOVE CLM-LOSS-POSTCODE   TO WS-PC-IN
              PERFORM 5200-POSTCODE-AREA
              MOVE WS-PC-AREA          TO WS-LOSS-AREA
           END-IF
           MOVE 0                      TO WS-BEST-ID
                                          WS-AREA-ID
           EXEC SQL OPEN CSR-VENDORS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5100-PICK-VENDOR'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CSR-VND-OPEN         TO TRUE
           SET WS-FETCH-MORE           TO TRUE
           PERFORM UNTIL WS-FETCH-DONE
              EXEC SQL
                 FETCH CSR-VENDORS
                  INTO :CON-CONTACT-ID,
                       :CON-POSTCODE :CON-POSTCODE-IND,
                       :CON-VENDOR-RATING
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 5110-RANK-VENDOR
                 WHEN 100
                    SET WS-FETCH-DONE  TO TRUE
                 WHEN OTHER
                    MOVE '5100-PICK-VENDOR' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-VENDORS END-EXEC
           MOVE 'N'                    TO WS-CSR-VND-SW
           EVALUATE TRUE
              WHEN WS-AREA-ID > 0
                 MOVE WS-AREA-ID       TO CA-SR-VENDOR-ID
              WHEN WS-BEST-ID > 0
                 MOVE WS-BEST-ID       TO CA-SR-VENDOR-ID
              WHEN OTHER
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'NOVENDOR'       TO CA-REASON-CODE
                 STRING 'No preferred ' WS-SERVICE-TYPE
                        ' vendor is available'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
      * Rows arrive best rating first: keep the first overall and the
      * first whose postcode area matches the loss
       5110-RANK-VENDOR.
           IF WS-BEST-ID = 0
              MOVE CON-CONTACT-ID      TO WS-BEST-ID
           END-IF
           IF WS-AREA-ID = 0 AND WS-LOSS-AREA NOT = SPACES
              AND CON-POSTCODE-IND = 0
              MOVE CON-POSTCODE        TO WS-PC-IN
              PERFORM 5200-POSTCODE-AREA
              IF WS-PC-AREA = WS-LOSS-AREA
                 MOVE CON-CONTACT-ID   TO WS-AREA-ID
                 SET WS-FETCH-DONE     TO TRUE
              END-IF
           END-IF.

      ******************************************************************
      * UK postcode area = the leading letters (1 or 2) of WS-PC-IN
       5200-POSTCODE-AREA.
           MOVE SPACES                 TO WS-PC-AREA
           PERFORM VARYING WS-PC-IX FROM 1 BY 1
                   UNTIL WS-PC-IX > 2
                      OR WS-PC-IN(WS-PC-IX:1) IS NOT ALPHABETIC
                      OR WS-PC-IN(WS-PC-IX:1) = SPACE
              MOVE WS-PC-IN(WS-PC-IX:1) TO WS-PC-AREA(WS-PC-IX:1)
           END-PERFORM.

      ******************************************************************
       8000-NEXT-SR-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'SRQ     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE SQA-SQ-VALUE           TO SRQ-SR-ID.

      ******************************************************************
      * BR-CLM-SRQ-009 Vendor linked to the claim as VENDOR (once)
       8100-LINK-VENDOR.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID     = :CA-SR-CLAIM-ID
                 AND CONTACT_ID   = :CA-SR-VENDOR-ID
                 AND CONTACT_ROLE = 'VENDOR'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '8100-LINK-VENDOR'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0
              INITIALIZE WS-CON-CA
              MOVE 'CONLNK  '          TO COA-REQUEST-ID
              MOVE CA-USER-ID          TO COA-USER-ID
              MOVE WS-BUS-DATE         TO COA-BUS-DATE
              MOVE WS-PROGRAM-ID       TO COA-CALLER-PGM
              MOVE CA-SR-VENDOR-ID     TO COA-CO-CONTACT-ID
              MOVE CA-SR-CLAIM-ID      TO COA-CO-CLAIM-ID
              MOVE CA-SR-EXPOSURE-ID   TO COA-CO-EXPOSURE-ID
              MOVE 'VENDOR  '          TO COA-CO-ROLE
              CALL WS-CON-PGM USING WS-CON-CA
              MOVE COA-HEADER          TO SVC-HEADER
              PERFORM 8900-CHECK-CALLEE
           END-IF.

      ******************************************************************
      * Invoice payment to the vendor; CLMFINTX applies authority and
      * may leave it pending approval (RC 04, txn returned)
       8200-PAY-VENDOR.
           INITIALIZE WS-FIN-CA
           MOVE 'PAYCRT  '             TO FTA-REQUEST-ID
           MOVE CA-USER-ID             TO FTA-USER-ID
           MOVE WS-BUS-DATE            TO FTA-BUS-DATE
           MOVE CA-TRACE-ID            TO FTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO FTA-CALLER-PGM
           MOVE SRQ-CLAIM-ID           TO FTA-FT-CLAIM-ID
           MOVE SRQ-EXPOSURE-ID        TO FTA-FT-EXPOSURE-ID
           MOVE WS-COST-TYPE           TO FTA-FT-COST-TYPE
           MOVE SRQ-SERVICE-TYPE       TO FTA-FT-COST-CATEGORY
           MOVE CA-SR-INVOICE-AMOUNT   TO FTA-FT-AMOUNT
           MOVE 'FINL'                 TO FTA-FT-PAYMENT-TYPE
           MOVE SRQ-VENDOR-CONTACT-ID  TO FTA-FT-PAYEE-CONTACT-ID
           MOVE WS-BUS-DATE            TO FTA-FT-SCHEDULED-DATE
           CALL WS-FIN-PGM USING WS-FIN-CA
           MOVE FTA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE FTA-FT-TXN-ID          TO CA-SR-PAYMENT-TXN-ID.

      ******************************************************************
       8300-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-SR-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HIA-HEADER             TO SVC-HEADER
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
           IF WS-CSR-VND-OPEN
              EXEC SQL CLOSE CSR-VENDORS END-EXEC
           END-IF
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Service data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMSRVRQ Db2 error SQLCODE ' WS-SQLCODE-DISP
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
              MOVE CA-SR-CLAIM-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
