       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMDOCRQ.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMDOCRQ                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim document requests. Requests a document     *
      *               from a claim contact, records receipt against a  *
      *               document management (DMS) reference and lists    *
      *               the claim's documents. Receipt of a police       *
      *               report withdraws the SIU "no police reference"   *
      *               indicator.                                       *
      * REQUESTS    : DOCREQ DOCRCV DOCLST                             *
      * CALLED BY   : claim screens, correspondence batch, FNOL intake *
      * CALLS       : CLMSEQGN CLMCODVL CLMHIST0 CLMERRLG              *
      * TABLES      : CLM_DOCUMENT (C R U)  CLM_CLAIM (R U)            *
      *               CLM_CLAIM_CONTACT (R)  CLM_SIU_SCORE (R D)       *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-DOC-001 Document type must be an active   *
      *                              DOCTYPE code                      *
      *               BR-CLM-DOC-002 Recipient must be a contact on    *
      *                              the claim                         *
      *               BR-CLM-DOC-003 One outstanding request per type  *
      *                              and recipient                     *
      *               BR-CLM-DOC-004 Requests cannot be raised on a    *
      *                              closed claim                      *
      *               BR-CLM-DOC-005 Receipt requires a DMS reference  *
      *               BR-CLM-DOC-006 Only a requested (RQST) document  *
      *                              can be received; unsolicited      *
      *                              documents are registered as RCVD  *
      *               BR-CLM-DOC-007 Receiving a police report removes *
      *                              the NOPOLREF SIU score, lowers    *
      *                              the claim SIU score and records   *
      *                              history                           *
      *               BR-CLM-DOC-008 Document name defaults to the     *
      *                              type description                  *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMDOCRQ'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.

       01  WS-CONSTANTS.
           05 WS-POLICE-REPORT         PIC X(8)  VALUE 'POLICERP'.
           05 WS-NOPOLREF-RULE         PIC X(8)  VALUE 'NOPOLREF'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-ROW-IX                PIC S9(4) COMP VALUE 0.
           05 WS-DOC-NAME              PIC X(60) VALUE SPACES.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-PTS-DISP              PIC ZZ9.
           05 WS-COUNT-DISP            PIC Z9.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-HIS-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-DONE                   VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.
           05 WS-CSR-DOC-SW            PIC X     VALUE 'N'.
              88 WS-CSR-DOC-OPEN                 VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDDOC END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDSSC END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CSR-DOCS CURSOR FOR
              SELECT DOC_ID, DOC_TYPE, DOC_NAME, DOC_STATUS,
                     REQUEST_DATE
                FROM CLMDB.CLM_DOCUMENT
               WHERE CLAIM_ID = :CA-DC-CLAIM-ID
                 AND (:CA-DC-STATUS = ' '
                      OR DOC_STATUS = :CA-DC-STATUS)
               ORDER BY CASE DOC_STATUS WHEN 'RQST' THEN 0
                        ELSE 1 END,
                        REQUEST_DATE DESC, DOC_ID DESC
               FETCH FIRST 11 ROWS ONLY
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

       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCDOC.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'DOCREQ  '
                 PERFORM 2100-REQUEST-DOCUMENT
              WHEN 'DOCRCV  '
                 PERFORM 2200-RECEIVE-DOCUMENT
              WHEN 'DOCLST  '
                 PERFORM 2300-LIST-DOCUMENTS
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMDOCRQ: unknown request '
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
      * DOCREQ
       2100-REQUEST-DOCUMENT.
           PERFORM 3100-VALIDATE-TYPE
           PERFORM 4100-READ-CLAIM
      * BR-CLM-DOC-004 No requests on a closed claim
           IF CLM-CLAIM-STATE = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMCLOSD'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is closed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3200-CHECK-RECIPIENT
      * BR-CLM-DOC-003 One outstanding request per type / recipient
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_DOCUMENT
               WHERE CLAIM_ID     = :CA-DC-CLAIM-ID
                 AND DOC_TYPE     = :CA-DC-DOC-TYPE
                 AND RECIPIENT_ID = :CA-DC-RECIPIENT-ID
                 AND DOC_STATUS   = 'RQST'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-REQUEST-DOCUMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DOCOUTST'          TO CA-REASON-CODE
              STRING 'A ' CA-DC-DOC-TYPE ' request is already '
                     'outstanding for this recipient'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'RQST'                 TO DOC-DOC-STATUS
           MOVE SPACES                 TO DOC-DMS-REF
           MOVE -1                     TO DOC-RECEIVED-DATE-IND
           PERFORM 2900-INSERT-DOCUMENT
           MOVE 'DOCREQ  '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           MOVE CA-DC-RECIPIENT-ID     TO WS-ID-DISP
           STRING 'Requested ' CA-DC-DOC-TYPE ' '
                  WS-DOC-NAME DELIMITED BY '  '
                  ' from contact ' WS-ID-DISP DELIMITED BY SIZE
                  INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           MOVE DOC-DOC-ID             TO WS-ID-DISP
           STRING 'Document request ' WS-ID-DISP ' raised'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * DOCRCV
       2200-RECEIVE-DOCUMENT.
      * BR-CLM-DOC-005 A DMS reference is mandatory on receipt
           IF CA-DC-DMS-REF = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DMSREQ  '          TO CA-REASON-CODE
              MOVE 'DMS reference is required to receive a document'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-DOC-006 Requested documents move RQST -> RCVD;
      *                unsolicited documents are registered directly
           IF CA-DC-DOC-ID > 0
              PERFORM 4200-READ-DOCUMENT
              IF DOC-DOC-STATUS NOT = 'RQST'
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'DOCNOTRQ'       TO CA-REASON-CODE
                 MOVE DOC-DOC-ID       TO WS-ID-DISP
                 STRING 'Document ' WS-ID-DISP ' is '
                        DOC-DOC-STATUS ', not RQST'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE DOC-CLAIM-ID        TO CA-DC-CLAIM-ID
              MOVE DOC-DOC-TYPE        TO CA-DC-DOC-TYPE
              PERFORM 4100-READ-CLAIM
              PERFORM 4300-MARK-RECEIVED
           ELSE
              PERFORM 3100-VALIDATE-TYPE
              PERFORM 4100-READ-CLAIM
              IF CA-DC-RECIPIENT-ID > 0
                 PERFORM 3200-CHECK-RECIPIENT
              END-IF
              MOVE 'RCVD'              TO DOC-DOC-STATUS
              MOVE CA-DC-DMS-REF       TO DOC-DMS-REF
              MOVE WS-BUS-DATE         TO DOC-RECEIVED-DATE
              MOVE 0                   TO DOC-RECEIVED-DATE-IND
              PERFORM 2900-INSERT-DOCUMENT
           END-IF
           MOVE 'RCVD'                 TO CA-DC-STATUS
           MOVE DOC-DOC-ID             TO CA-DC-DOC-ID
           MOVE 'DOCRCV  '             TO WS-HIS-EVENT
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Received ' CA-DC-DOC-TYPE ' DMS '
                  CA-DC-DMS-REF DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY
           IF CA-DC-DOC-TYPE = WS-POLICE-REPORT
              PERFORM 2210-RESET-NOPOLREF
           END-IF
           MOVE DOC-DOC-ID             TO WS-ID-DISP
           STRING 'Document ' WS-ID-DISP ' received'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-CLM-DOC-007 A police report withdraws the NOPOLREF SIU
      * indicator: score row removed, claim score lowered, history
       2210-RESET-NOPOLREF.
           MOVE CA-DC-CLAIM-ID         TO SSC-CLAIM-ID
           MOVE WS-NOPOLREF-RULE       TO SSC-RULE-CODE
           EXEC SQL
              SELECT POINTS
                INTO :SSC-POINTS
                FROM CLMDB.CLM_SIU_SCORE
               WHERE CLAIM_ID  = :SSC-CLAIM-ID
                 AND RULE_CODE = :SSC-RULE-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO SSC-POINTS
              WHEN OTHER
                 MOVE '2210-RESET-NOPOLREF' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF SSC-POINTS > 0
              EXEC SQL
                 DELETE FROM CLMDB.CLM_SIU_SCORE
                  WHERE CLAIM_ID  = :SSC-CLAIM-ID
                    AND RULE_CODE = :SSC-RULE-CODE
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '2210-RESET-NOPOLREF' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              EXEC SQL
                 UPDATE CLMDB.CLM_CLAIM
                    SET SIU_SCORE  = CASE
                                     WHEN SIU_SCORE > :SSC-POINTS
                                     THEN SIU_SCORE - :SSC-POINTS
                                     ELSE 0 END,
                        UPDATE_TS  = CURRENT TIMESTAMP,
                        VERSION_NO = VERSION_NO + 1
                  WHERE CLAIM_ID   = :CLM-CLAIM-ID
                    AND VERSION_NO = :CLM-VERSION-NO
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '2210-RESET-NOPOLREF' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              IF SQLERRD(3) = 0
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Claim changed by another user - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE 'SIURESET'          TO WS-HIS-EVENT
              MOVE SSC-POINTS          TO WS-PTS-DISP
              MOVE SPACES              TO WS-HIS-DESC
              STRING 'Police report received - SIU rule '
                     WS-NOPOLREF-RULE ' withdrawn (-' WS-PTS-DISP
                     ' points)' DELIMITED BY SIZE INTO WS-HIS-DESC
              PERFORM 8100-WRITE-HISTORY
           END-IF.

      ******************************************************************
      * DOCLST - outstanding requests first, newest first
       2300-LIST-DOCUMENTS.
           IF CA-DC-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required to list documents'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM
           MOVE 0                      TO CA-DC-ROW-COUNT
           PERFORM VARYING WS-ROW-IX FROM 1 BY 1 UNTIL WS-ROW-IX > 10
              MOVE 0                   TO CA-DC-R-ID(WS-ROW-IX)
              MOVE SPACES              TO CA-DC-R-TYPE(WS-ROW-IX)
                                          CA-DC-R-NAME(WS-ROW-IX)
                                          CA-DC-R-STATUS(WS-ROW-IX)
                                          CA-DC-R-REQUESTED(WS-ROW-IX)
           END-PERFORM
           EXEC SQL OPEN CSR-DOCS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-LIST-DOCUMENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CSR-DOC-OPEN         TO TRUE
           SET WS-FETCH-MORE           TO TRUE
           PERFORM UNTIL WS-FETCH-DONE
              EXEC SQL
                 FETCH CSR-DOCS
                  INTO :DOC-DOC-ID, :DOC-DOC-TYPE, :DOC-DOC-NAME,
                       :DOC-DOC-STATUS, :DOC-REQUEST-DATE
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF CA-DC-ROW-COUNT = 10
                       SET CA-RC-WARNING TO TRUE
                       MOVE 'MOREROWS' TO CA-REASON-CODE
                       MOVE 'More than 10 documents - filter by status'
                                       TO CA-MESSAGE
                       SET WS-FETCH-DONE TO TRUE
                    ELSE
                       PERFORM 2310-MOVE-DOC-ROW
                    END-IF
                 WHEN 100
                    SET WS-FETCH-DONE  TO TRUE
                 WHEN OTHER
                    MOVE '2300-LIST-DOCUMENTS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-DOCS END-EXEC
           MOVE 'N'                    TO WS-CSR-DOC-SW
           IF CA-RC-OK
              MOVE CA-DC-ROW-COUNT     TO WS-COUNT-DISP
              STRING WS-COUNT-DISP ' documents listed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       2310-MOVE-DOC-ROW.
           ADD 1                       TO CA-DC-ROW-COUNT
           MOVE CA-DC-ROW-COUNT        TO WS-ROW-IX
           MOVE DOC-DOC-ID             TO CA-DC-R-ID(WS-ROW-IX)
           MOVE DOC-DOC-TYPE           TO CA-DC-R-TYPE(WS-ROW-IX)
           MOVE DOC-DOC-NAME-TEXT(1:DOC-DOC-NAME-LEN)
                                       TO CA-DC-R-NAME(WS-ROW-IX)
           MOVE DOC-DOC-STATUS         TO CA-DC-R-STATUS(WS-ROW-IX)
           MOVE DOC-REQUEST-DATE       TO CA-DC-R-REQUESTED(WS-ROW-IX).

      ******************************************************************
      * Shared insert for a request or an unsolicited receipt; the
      * caller sets status, DMS reference and received-date indicator
       2900-INSERT-DOCUMENT.
           PERFORM 8000-NEXT-DOC-ID
           MOVE CA-DC-CLAIM-ID         TO DOC-CLAIM-ID
           MOVE CA-DC-DOC-TYPE         TO DOC-DOC-TYPE
           MOVE WS-DOC-NAME            TO DOC-DOC-NAME-TEXT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DOC-NAME TRAILING))
                                       TO DOC-DOC-NAME-LEN
           MOVE CA-DC-RECIPIENT-ID     TO DOC-RECIPIENT-ID
           MOVE WS-BUS-DATE            TO DOC-REQUEST-DATE
           EXEC SQL
              INSERT INTO CLMDB.CLM_DOCUMENT
                     (DOC_ID, CLAIM_ID, DOC_TYPE, DOC_NAME,
                      DOC_STATUS, RECIPIENT_ID, DMS_REF,
                      REQUEST_DATE, RECEIVED_DATE, CREATE_TS)
              VALUES (:DOC-DOC-ID, :DOC-CLAIM-ID, :DOC-DOC-TYPE,
                      :DOC-DOC-NAME, :DOC-DOC-STATUS,
                      :DOC-RECIPIENT-ID, :DOC-DMS-REF,
                      :DOC-REQUEST-DATE,
                      :DOC-RECEIVED-DATE :DOC-RECEIVED-DATE-IND,
                      CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE DOC-DOC-ID       TO CA-DC-DOC-ID
                 MOVE DOC-DOC-STATUS   TO CA-DC-STATUS
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE DOC-DOC-ID       TO WS-ID-DISP
                 STRING 'Document id ' WS-ID-DISP ' already exists'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2900-INSERT-DOCUMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-DOC-001 Type must be valid; BR-CLM-DOC-008 name default
       3100-VALIDATE-TYPE.
           IF CA-DC-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for a document'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-DC-DOC-TYPE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DOCTYREQ'          TO CA-REASON-CODE
              MOVE 'Document type is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE WS-BUS-DATE            TO CDA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE 'DOCTYPE '             TO CDA-CD-TABLE
           MOVE CA-DC-DOC-TYPE         TO CDA-CD-CODE
           CALL WS-COD-PGM USING WS-COD-CA
           MOVE CDA-HEADER             TO SVC-HEADER
           IF SVC-RETURN-CODE NOT = 08
              PERFORM 8900-CHECK-CALLEE
           END-IF
           IF NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADDOCTY'          TO CA-REASON-CODE
              STRING 'Document type ' CA-DC-DOC-TYPE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-DC-DOC-NAME NOT = SPACES
              MOVE CA-DC-DOC-NAME      TO WS-DOC-NAME
           ELSE
              MOVE CDA-CD-DESC         TO WS-DOC-NAME
           END-IF.

      ******************************************************************
      * BR-CLM-DOC-002 Recipient must be a contact on the claim
       3200-CHECK-RECIPIENT.
           IF CA-DC-RECIPIENT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RCPTREQ '          TO CA-REASON-CODE
              MOVE 'Recipient contact is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID   = :CA-DC-CLAIM-ID
                 AND CONTACT_ID = :CA-DC-RECIPIENT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3200-CHECK-RECIPIENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RCPTNOCL'          TO CA-REASON-CODE
              MOVE CA-DC-RECIPIENT-ID  TO WS-ID-DISP
              STRING 'Contact ' WS-ID-DISP
                     ' is not a party to this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4100-READ-CLAIM.
           EXEC SQL
              SELECT CLAIM_ID, CLAIM_NUMBER, CLAIM_STATE, VERSION_NO
                INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                     :CLM-CLAIM-STATE, :CLM-VERSION-NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-DC-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-DC-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-DOCUMENT.
           EXEC SQL
              SELECT DOC_ID, CLAIM_ID, DOC_TYPE, DOC_STATUS,
                     RECIPIENT_ID
                INTO :DOC-DOC-ID, :DOC-CLAIM-ID, :DOC-DOC-TYPE,
                     :DOC-DOC-STATUS, :DOC-RECIPIENT-ID
                FROM CLMDB.CLM_DOCUMENT
               WHERE DOC_ID = :CA-DC-DOC-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'DOCNOTFD'       TO CA-REASON-CODE
                 MOVE CA-DC-DOC-ID     TO WS-ID-DISP
                 STRING 'Document ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-READ-DOCUMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-MARK-RECEIVED.
           EXEC SQL
              UPDATE CLMDB.CLM_DOCUMENT
                 SET DOC_STATUS    = 'RCVD',
                     DMS_REF       = :CA-DC-DMS-REF,
                     RECEIVED_DATE = :WS-BUS-DATE
               WHERE DOC_ID        = :DOC-DOC-ID
                 AND DOC_STATUS    = 'RQST'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-MARK-RECEIVED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Document was updated by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8000-NEXT-DOC-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'DOC     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE SQA-SQ-VALUE           TO DOC-DOC-ID.

      ******************************************************************
       8100-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-DC-CLAIM-ID         TO HIA-HI-CLAIM-ID
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
           IF WS-CSR-DOC-OPEN
              EXEC SQL CLOSE CSR-DOCS END-EXEC
           END-IF
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Document data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMDOCRQ Db2 error SQLCODE ' WS-SQLCODE-DISP
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
              MOVE CA-DC-CLAIM-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
