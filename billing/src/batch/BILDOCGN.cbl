       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDOCGN.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDOCGN                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Document extract. Requested documents (status    *
      *               RQST) are resolved to their recipient and        *
      *               reference data and written to the fixed-length   *
      *               print-vendor file, then marked SENT. Requests    *
      *               that cannot be addressed are marked ERRR.        *
      * INPUT       : SYSIN   control card            (BILINVC1)       *
      *               CHKPT   checkpoint / restart     (BILINVK1)      *
      * OUTPUT      : PRTVEND print-vendor file        (BILDOCP1)      *
      *               CHKPT, RPTOUT run report (BILINVH1, BILDOCR1)    *
      * CALLS       : BILDATES BILERRLG                                *
      * TABLES      : BIL_DOCUMENT_REQUEST  (R U)                      *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_ACCOUNT_CONTACT   (R)                        *
      *               BIL_INVOICE           (R)                        *
      *               BIL_DELINQ_PROCESS    (R)                        *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_CONTROL           (R)                        *
      * RESTART     : the vendor file is rebuilt in full: requests     *
      *               already marked SENT since the original run start *
      *               (CK-RUN-START-TS) are written again, then the    *
      *               remaining RQST requests                          *
      * RULES       : BR-BIL-DOC-001 Only RQST requests dated on or    *
      *                              before the business date go out   *
      *               BR-BIL-DOC-002 Recipient is the account's        *
      *                              primary contact: company name for *
      *                              company contacts, else first and  *
      *                              last name                         *
      *               BR-BIL-DOC-003 A request without a primary       *
      *                              contact address is marked ERRR    *
      *               BR-BIL-DOC-004 E-mail delivery needs an e-mail   *
      *                              address on the primary contact    *
      *               BR-BIL-DOC-005 Invoice documents carry invoice   *
      *                              number, amount due and due date;  *
      *                              delinquency documents carry the   *
      *                              policy number and past due        *
      *               BR-BIL-DOC-006 A request is written only after   *
      *                              it is marked SENT from RQST       *
      *               BR-BIL-DOC-007 Trailer carries the record count, *
      *                              last document id and amount total *
      *                              for vendor reconciliation         *
      * RETURN CODE : 0 ok, 4 requests in error, 16 abended            *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTROL-CARD ASSIGN TO SYSIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CC-STATUS.
           SELECT OPTIONAL CHKPT-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CK-STATUS.
           SELECT VENDOR-FILE ASSIGN TO PRTVEND
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-PV-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTROL-CARD
           RECORDING MODE IS F.
           COPY BILINVC1.
       FD  CHKPT-FILE
           RECORDING MODE IS F.
           COPY BILINVK1.
       FD  VENDOR-FILE
           RECORDING MODE IS F.
       01  VENDOR-RECORD               PIC X(500).
       FD  REPORT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDOCGN'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'BILBATCH'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-FILE-STATUS.
           05 WS-CC-STATUS             PIC XX    VALUE '00'.
           05 WS-CK-STATUS             PIC XX    VALUE '00'.
           05 WS-PV-STATUS             PIC XX    VALUE '00'.
           05 WS-RP-STATUS             PIC XX    VALUE '00'.

       01  WS-RUN.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-START-TS          PIC X(26) VALUE SPACES.
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LAST-DOC-ID           PIC S9(9) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-RESTART-FLAG          PIC X     VALUE 'N'.
              88 WS-IS-RESTART                   VALUE 'Y'.
           05 WS-RUN-FLAG              PIC X     VALUE 'N'.
              88 WS-RUN-ENDED                    VALUE 'Y'.
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-END-OF-DOCS                  VALUE 'Y'.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.
           05 WS-ABEND-TEXT            PIC X(100) VALUE SPACES.
           05 WS-ERROR-TEXT            PIC X(87) VALUE SPACES.
           05 WS-NAME-WORK             PIC X(60) VALUE SPACES.
           05 WS-REF-AMOUNT            PIC S9(11)V99 COMP-3 VALUE 0.

       01  WS-TOTALS.
           05 WS-T-READ                PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-WRITTEN             PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-REWRITTEN           PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-ERRORS              PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-RACED               PIC S9(9) COMP-3 VALUE 0.
           05 WS-T-AMOUNT              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-T-COMMITS             PIC S9(9) COMP-3 VALUE 0.

       01  WS-DOCTYPE-TABLE.
           05 WS-DT-COUNT              PIC S9(4) COMP VALUE 0.
           05 WS-DT OCCURS 20 TIMES INDEXED BY DX.
              10 WS-DT-TYPE            PIC X(8).
              10 WS-DT-SENT            PIC S9(9) COMP-3.
              10 WS-DT-ERRORS          PIC S9(9) COMP-3.

       01  WS-WORK.
           05 WS-PAGE                  PIC S9(4) COMP VALUE 0.
           05 WS-LINES                 PIC S9(4) COMP VALUE 99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-CNT-DISP              PIC Z(8)9.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURRENT-TS            PIC X(26).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDDOC END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDACN END-EXEC.
           EXEC SQL INCLUDE BILDINV END-EXEC.
           EXEC SQL INCLUDE BILDDLP END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-DOC-001 Requested documents up to the business date,
      * plus on restart those already sent by the original run
           EXEC SQL
              DECLARE C-DOCS CURSOR WITH HOLD FOR
               SELECT D.DOC_ID, D.ACCOUNT_ID, D.DOC_TYPE, D.REF_TYPE,
                      D.REF_ID, D.DELIVERY_METHOD, D.DOC_STATUS,
                      D.REQUEST_DATE, A.ACCOUNT_NUMBER
                 FROM BILDB.BIL_DOCUMENT_REQUEST D
                 JOIN BILDB.BIL_ACCOUNT A
                   ON A.ACCOUNT_ID = D.ACCOUNT_ID
                WHERE D.REQUEST_DATE <= :WS-BUS-DATE
                  AND (D.DOC_STATUS = 'RQST'
                       OR (D.DOC_STATUS = 'SENT'
                           AND :WS-RESTART-FLAG = 'Y'
                           AND D.SENT_TS >= :WS-RUN-START-TS))
                ORDER BY D.DOC_ID
                FOR FETCH ONLY
           END-EXEC.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTC-==.
           03 DTC-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILDOCP1.
           COPY BILINVH1.
           COPY BILDOCR1.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF NOT WS-RUN-ENDED
              PERFORM 2000-EXTRACT-DOCUMENTS
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN INPUT  CONTROL-CARD
                OUTPUT REPORT-FILE
           IF WS-CC-STATUS NOT = '00' OR WS-RP-STATUS NOT = '00'
              DISPLAY 'BILDOCGN OPEN FAILED SYSIN=' WS-CC-STATUS
                      ' RPTOUT=' WS-RP-STATUS
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE WS-PROGRAM-ID          TO RH-PROGRAM-ID
           MOVE 'DOCUMENT EXTRACT TO PRINT VENDOR' TO RH-TITLE
           PERFORM 1100-READ-CONTROL-CARD
           PERFORM 1200-RESOLVE-BUS-DATE
           MOVE WS-BUS-DATE            TO RH-BUS-DATE
           MOVE WS-RUN-DATE            TO RH-RUN-DATE
           PERFORM 1300-READ-CHECKPOINT
           IF NOT WS-RUN-ENDED
              PERFORM 1400-OPEN-VENDOR-FILE
           END-IF.

      ******************************************************************
       1100-READ-CONTROL-CARD.
           MOVE SPACES                 TO CC-RECORD
           READ CONTROL-CARD
              AT END
                 MOVE SPACES           TO CC-RECORD
           END-READ
           CLOSE CONTROL-CARD
           IF CC-COMMIT-FREQ NOT = SPACES
              IF CC-COMMIT-FREQ IS NUMERIC AND CC-COMMIT-FREQ-N > 0
                 MOVE CC-COMMIT-FREQ-N TO WS-COMMIT-FREQ
              ELSE
                 STRING 'INVALID COMMIT FREQUENCY ' CC-COMMIT-FREQ
                        DELIMITED BY SIZE INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
           END-IF.

      ******************************************************************
       1200-RESOLVE-BUS-DATE.
           EXEC SQL
              SELECT BUSINESS_DATE, CHAR(CURRENT DATE, ISO),
                     CURRENT TIMESTAMP
                INTO :CTL-BUSINESS-DATE, :WS-RUN-DATE,
                     :WS-RUN-START-TS
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = 'SYSTEM  '
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE 'SYSTEM'            TO WS-ERR-KEY
              MOVE '1200-RESOLVE-BUS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF CC-BUS-DATE = SPACES
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           ELSE
              MOVE CC-BUS-DATE         TO WS-BUS-DATE
           END-IF
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTC-REQUEST-ID
           MOVE WS-BATCH-USER          TO DTC-USER-ID
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTC-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 90
              MOVE DTC-MESSAGE         TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF NOT DTC-DT-IS-VALID
              STRING 'INVALID BUSINESS DATE ' WS-BUS-DATE
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * A restart keeps the original run start so that everything it
      * already sent is written again to the rebuilt vendor file
       1300-READ-CHECKPOINT.
           OPEN INPUT CHKPT-FILE
           IF WS-CK-STATUS = '00'
              READ CHKPT-FILE
                 AT END
                    MOVE SPACES        TO CK-RECORD
              END-READ
              CLOSE CHKPT-FILE
           ELSE
              MOVE SPACES              TO CK-RECORD
           END-IF
           IF CK-PROGRAM-ID = WS-PROGRAM-ID
              AND CK-BUS-DATE = WS-BUS-DATE
              AND NOT CC-RESTART-NONE
              EVALUATE TRUE
                 WHEN CK-RUNNING OR CK-FAILED
                    SET WS-IS-RESTART  TO TRUE
                    MOVE CK-RUN-START-TS TO WS-RUN-START-TS
                    MOVE CK-COMMITS    TO WS-T-COMMITS
                    STRING 'RESTART - REBUILDING EXTRACT FROM RUN '
                           'STARTED ' WS-RUN-START-TS
                           DELIMITED BY SIZE INTO RH-TEXT
                    PERFORM 7200-WRITE-TEXT
                 WHEN CK-COMPLETE AND NOT CC-RESTART-FORCE
                    MOVE 'BUSINESS DATE ALREADY COMPLETE - NOTHING DONE'
                                       TO RH-TEXT
                    PERFORM 7200-WRITE-TEXT
                    MOVE 4             TO WS-MAX-RC
                    SET WS-RUN-ENDED   TO TRUE
              END-EVALUATE
           END-IF.

      ******************************************************************
       1400-OPEN-VENDOR-FILE.
           OPEN OUTPUT VENDOR-FILE
           IF WS-PV-STATUS NOT = '00'
              STRING 'PRTVEND OPEN FAILED STATUS ' WS-PV-STATUS
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE WS-BUS-DATE            TO PV-H-BUS-DATE
           MOVE WS-RUN-START-TS        TO PV-H-CREATED-TS
           MOVE 1                      TO PV-H-SEGMENT
           WRITE VENDOR-RECORD FROM PV-HEADER-REC
           PERFORM 7500-CHECK-VENDOR-WRITE.

      ******************************************************************
       2000-EXTRACT-DOCUMENTS.
           EXEC SQL OPEN C-DOCS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-EXTRACT-DOCUMENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4000-FETCH-DOC
           PERFORM UNTIL WS-END-OF-DOCS
              ADD 1                    TO WS-T-READ
              PERFORM 2100-PROCESS-DOC
              PERFORM 4000-FETCH-DOC
           END-PERFORM
           EXEC SQL CLOSE C-DOCS END-EXEC
           SET WS-CURSOR-CLOSED        TO TRUE.

      ******************************************************************
       2100-PROCESS-DOC.
           MOVE SPACES                 TO WS-ERROR-TEXT
           PERFORM 5000-FIND-DOC-TYPE
           PERFORM 4100-READ-PRIMARY-CONTACT
           IF WS-ERROR-TEXT = SPACES
              PERFORM 3000-VALIDATE-RECIPIENT
           END-IF
           IF WS-ERROR-TEXT = SPACES
              PERFORM 4200-READ-REFERENCE
           END-IF
           IF WS-ERROR-TEXT NOT = SPACES
              IF DOC-DOC-STATUS = 'RQST'
                 PERFORM 4400-MARK-ERROR
              END-IF
              ADD 1                    TO WS-T-ERRORS
                                          WS-DT-ERRORS(DX)
              PERFORM 7300-WRITE-EXCEPTION
           ELSE
              PERFORM 2200-SEND-DOC
           END-IF
           MOVE DOC-DOC-ID             TO WS-LAST-DOC-ID
           ADD 1                       TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 7100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
      * BR-BIL-DOC-006 Marked SENT first; written only if it still was
      * RQST (requests re-sent on restart are already SENT)
       2200-SEND-DOC.
           IF DOC-DOC-STATUS = 'RQST'
              PERFORM 4300-MARK-SENT
              IF SQLERRD(3) = 0
                 ADD 1                 TO WS-T-RACED
                 MOVE 'REQUEST CHANGED BY ANOTHER TASK - NOT SENT'
                                       TO WS-ERROR-TEXT
                 PERFORM 7300-WRITE-EXCEPTION
                 EXIT PARAGRAPH
              END-IF
           ELSE
              ADD 1                    TO WS-T-REWRITTEN
           END-IF
           PERFORM 5100-BUILD-DETAIL
           WRITE VENDOR-RECORD FROM PV-DETAIL-REC
           PERFORM 7500-CHECK-VENDOR-WRITE
           ADD 1                       TO WS-T-WRITTEN
                                          WS-DT-SENT(DX)
           ADD WS-REF-AMOUNT           TO WS-T-AMOUNT.

      ******************************************************************
       3000-VALIDATE-RECIPIENT.
      * BR-BIL-DOC-003 Postal address required for every document
           IF ACN-ADDRESS-LINE1-LEN = 0 OR ACN-POSTCODE = SPACES
              MOVE 'PRIMARY CONTACT HAS NO POSTAL ADDRESS'
                                       TO WS-ERROR-TEXT
           END-IF
      * BR-BIL-DOC-004 E-mail delivery needs an address
           IF DOC-DELIVERY-METHOD = 'EMAL'
              AND (ACN-EMAIL-ADDRESS-IND < 0
                   OR ACN-EMAIL-ADDRESS-LEN = 0)
              MOVE 'E-MAIL DELIVERY BUT NO E-MAIL ADDRESS ON FILE'
                                       TO WS-ERROR-TEXT
           END-IF.

      ******************************************************************
       4000-FETCH-DOC.
           EXEC SQL
              FETCH C-DOCS
               INTO :DOC-DOC-ID, :DOC-ACCOUNT-ID, :DOC-DOC-TYPE,
                    :DOC-REF-TYPE, :DOC-REF-ID,
                    :DOC-DELIVERY-METHOD, :DOC-DOC-STATUS,
                    :DOC-REQUEST-DATE, :ACT-ACCOUNT-NUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE DOC-DOC-ID       TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
              WHEN 100
                 SET WS-END-OF-DOCS    TO TRUE
              WHEN OTHER
                 MOVE '4000-FETCH-DOC' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DOC-002 Primary contact is the recipient
       4100-READ-PRIMARY-CONTACT.
           MOVE DOC-ACCOUNT-ID         TO ACN-ACCOUNT-ID
           EXEC SQL
              SELECT CONTACT_TYPE, FIRST_NAME, LAST_NAME,
                     COMPANY_NAME, ADDRESS_LINE1, ADDRESS_LINE2,
                     CITY, POSTCODE, COUNTRY_CODE, EMAIL_ADDRESS
                INTO :ACN-CONTACT-TYPE,
                     :ACN-FIRST-NAME :ACN-FIRST-NAME-IND,
                     :ACN-LAST-NAME :ACN-LAST-NAME-IND,
                     :ACN-COMPANY-NAME :ACN-COMPANY-NAME-IND,
                     :ACN-ADDRESS-LINE1,
                     :ACN-ADDRESS-LINE2 :ACN-ADDRESS-LINE2-IND,
                     :ACN-CITY :ACN-CITY-IND,
                     :ACN-POSTCODE, :ACN-COUNTRY-CODE,
                     :ACN-EMAIL-ADDRESS :ACN-EMAIL-ADDRESS-IND
                FROM BILDB.BIL_ACCOUNT_CONTACT
               WHERE ACCOUNT_ID   = :ACN-ACCOUNT-ID
                 AND PRIMARY_FLAG = 'Y'
               ORDER BY CONTACT_SEQ
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 PERFORM 4110-APPLY-NULLS
              WHEN 100
                 MOVE 'ACCOUNT HAS NO PRIMARY CONTACT'
                                       TO WS-ERROR-TEXT
              WHEN OTHER
                 MOVE '4100-READ-PRIMARY-CONTACT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4110-APPLY-NULLS.
           IF ACN-FIRST-NAME-IND < 0
              MOVE 0                   TO ACN-FIRST-NAME-LEN
           END-IF
           IF ACN-LAST-NAME-IND < 0
              MOVE 0                   TO ACN-LAST-NAME-LEN
           END-IF
           IF ACN-COMPANY-NAME-IND < 0
              MOVE 0                   TO ACN-COMPANY-NAME-LEN
           END-IF
           IF ACN-ADDRESS-LINE2-IND < 0
              MOVE 0                   TO ACN-ADDRESS-LINE2-LEN
           END-IF
           IF ACN-CITY-IND < 0
              MOVE 0                   TO ACN-CITY-LEN
           END-IF
           IF ACN-EMAIL-ADDRESS-IND < 0
              MOVE 0                   TO ACN-EMAIL-ADDRESS-LEN
           END-IF.

      ******************************************************************
      * BR-BIL-DOC-005 Reference data per document reference type
       4200-READ-REFERENCE.
           MOVE 0                      TO WS-REF-AMOUNT
                                          PPR-GENAPP-POL-NUM
           MOVE SPACES                 TO INV-INVOICE-NUMBER
                                          INV-DUE-DATE
           EVALUATE DOC-REF-TYPE
              WHEN 'INV '
                 MOVE DOC-REF-ID       TO INV-INVOICE-ID
                 EXEC SQL
                    SELECT INVOICE_NUMBER, AMOUNT_DUE, DUE_DATE
                      INTO :INV-INVOICE-NUMBER, :INV-AMOUNT-DUE,
                           :INV-DUE-DATE
                      FROM BILDB.BIL_INVOICE
                     WHERE INVOICE_ID = :INV-INVOICE-ID
                 END-EXEC
                 EVALUATE SQLCODE
                    WHEN 0
                       MOVE INV-AMOUNT-DUE TO WS-REF-AMOUNT
                    WHEN 100
                       MOVE 'REFERENCED INVOICE NOT FOUND'
                                       TO WS-ERROR-TEXT
                    WHEN OTHER
                       MOVE '4200-READ-REFERENCE' TO WS-ERR-PARA
                       PERFORM 9900-DB2-ERROR
                 END-EVALUATE
              WHEN 'DLP '
                 MOVE DOC-REF-ID       TO DLP-DP-ID
                 EXEC SQL
                    SELECT P.GENAPP_POL_NUM, D.PASTDUE_AMOUNT
                      INTO :PPR-GENAPP-POL-NUM, :DLP-PASTDUE-AMOUNT
                      FROM BILDB.BIL_DELINQ_PROCESS D
                      JOIN BILDB.BIL_POLICY_PERIOD P
                        ON P.PP_ID = D.PP_ID
                     WHERE D.DP_ID = :DLP-DP-ID
                 END-EXEC
                 EVALUATE SQLCODE
                    WHEN 0
                       MOVE DLP-PASTDUE-AMOUNT TO WS-REF-AMOUNT
                       MOVE DLP-DP-ID  TO WS-ID-DISP
                       MOVE WS-ID-DISP TO INV-INVOICE-NUMBER
                    WHEN 100
                       MOVE 'REFERENCED DELINQUENCY PROCESS NOT FOUND'
                                       TO WS-ERROR-TEXT
                    WHEN OTHER
                       MOVE '4200-READ-REFERENCE' TO WS-ERR-PARA
                       PERFORM 9900-DB2-ERROR
                 END-EVALUATE
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.

      ******************************************************************
       4300-MARK-SENT.
           EXEC SQL
              UPDATE BILDB.BIL_DOCUMENT_REQUEST
                 SET DOC_STATUS = 'SENT',
                     SENT_TS    = CURRENT TIMESTAMP
               WHERE DOC_ID     = :DOC-DOC-ID
                 AND DOC_STATUS = 'RQST'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4300-MARK-SENT'    TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-MARK-ERROR.
           EXEC SQL
              UPDATE BILDB.BIL_DOCUMENT_REQUEST
                 SET DOC_STATUS = 'ERRR'
               WHERE DOC_ID     = :DOC-DOC-ID
                 AND DOC_STATUS = 'RQST'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4400-MARK-ERROR'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       5000-FIND-DOC-TYPE.
           SET DX                      TO 1
           SEARCH WS-DT
              AT END
                 PERFORM 5010-ADD-DOC-TYPE
              WHEN DX > WS-DT-COUNT
                 PERFORM 5010-ADD-DOC-TYPE
              WHEN WS-DT-TYPE(DX) = DOC-DOC-TYPE
                 CONTINUE
           END-SEARCH.

      ******************************************************************
      * Types beyond 20 share the last slot as OTHER
       5010-ADD-DOC-TYPE.
           IF WS-DT-COUNT < 20
              ADD 1                    TO WS-DT-COUNT
              SET DX                   TO WS-DT-COUNT
              MOVE DOC-DOC-TYPE        TO WS-DT-TYPE(DX)
              MOVE 0                   TO WS-DT-SENT(DX)
                                          WS-DT-ERRORS(DX)
           ELSE
              SET DX                   TO 20
              MOVE 'OTHER   '          TO WS-DT-TYPE(DX)
           END-IF.

      ******************************************************************
       5100-BUILD-DETAIL.
           MOVE DOC-DOC-ID             TO PV-D-DOC-ID
           MOVE DOC-DOC-TYPE           TO PV-D-DOC-TYPE
           MOVE DOC-DELIVERY-METHOD    TO PV-D-DELIVERY
           MOVE DOC-REQUEST-DATE       TO PV-D-REQUEST-DATE
           MOVE ACT-ACCOUNT-NUMBER     TO PV-D-ACCOUNT-NUMBER
           PERFORM 5200-FORMAT-RECIPIENT
           MOVE WS-NAME-WORK           TO PV-D-RECIPIENT
           MOVE SPACES                 TO PV-D-ADDR-LINE1
                                          PV-D-ADDR-LINE2
                                          PV-D-CITY
                                          PV-D-EMAIL
           MOVE ACN-ADDRESS-LINE1-TEXT(1:ACN-ADDRESS-LINE1-LEN)
                                       TO PV-D-ADDR-LINE1
           IF ACN-ADDRESS-LINE2-LEN > 0
              MOVE ACN-ADDRESS-LINE2-TEXT(1:ACN-ADDRESS-LINE2-LEN)
                                       TO PV-D-ADDR-LINE2
           END-IF
           IF ACN-CITY-LEN > 0
              MOVE ACN-CITY-TEXT(1:ACN-CITY-LEN) TO PV-D-CITY
           END-IF
           IF ACN-EMAIL-ADDRESS-LEN > 0
              MOVE ACN-EMAIL-ADDRESS-TEXT(1:ACN-EMAIL-ADDRESS-LEN)
                                       TO PV-D-EMAIL
           END-IF
           MOVE ACN-POSTCODE           TO PV-D-POSTCODE
           MOVE ACN-COUNTRY-CODE       TO PV-D-COUNTRY
           MOVE DOC-REF-TYPE           TO PV-D-REF-TYPE
           MOVE INV-INVOICE-NUMBER     TO PV-D-REF-NUMBER
           MOVE PPR-GENAPP-POL-NUM     TO PV-D-POLICY-NO
           IF WS-REF-AMOUNT < 0
              COMPUTE PV-D-AMOUNT = 0 - WS-REF-AMOUNT
              MOVE '-'                 TO PV-D-AMOUNT-SIGN
           ELSE
              MOVE WS-REF-AMOUNT       TO PV-D-AMOUNT
              MOVE '+'                 TO PV-D-AMOUNT-SIGN
           END-IF
           MOVE INV-DUE-DATE           TO PV-D-DUE-DATE.

      ******************************************************************
      * BR-BIL-DOC-002 Company name for company contacts, else names
       5200-FORMAT-RECIPIENT.
           MOVE SPACES                 TO WS-NAME-WORK
           IF ACN-CONTACT-TYPE = 'C' AND ACN-COMPANY-NAME-LEN > 0
              MOVE ACN-COMPANY-NAME-TEXT(1:ACN-COMPANY-NAME-LEN)
                                       TO WS-NAME-WORK
           ELSE
              EVALUATE TRUE
                 WHEN ACN-FIRST-NAME-LEN > 0
                  AND ACN-LAST-NAME-LEN > 0
                    STRING ACN-FIRST-NAME-TEXT(1:ACN-FIRST-NAME-LEN)
                           ' '
                           ACN-LAST-NAME-TEXT(1:ACN-LAST-NAME-LEN)
                           DELIMITED BY SIZE INTO WS-NAME-WORK
                 WHEN ACN-LAST-NAME-LEN > 0
                    MOVE ACN-LAST-NAME-TEXT(1:ACN-LAST-NAME-LEN)
                                       TO WS-NAME-WORK
                 WHEN ACN-COMPANY-NAME-LEN > 0
                    MOVE ACN-COMPANY-NAME-TEXT(1:ACN-COMPANY-NAME-LEN)
                                       TO WS-NAME-WORK
                 WHEN OTHER
                    MOVE 'THE ACCOUNT HOLDER' TO WS-NAME-WORK
              END-EVALUATE
           END-IF.

      ******************************************************************
       7000-NEW-PAGE.
           ADD 1                       TO WS-PAGE
           MOVE WS-PAGE                TO RH-PAGE
           WRITE RPT-RECORD FROM RH-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           WRITE RPT-RECORD FROM R4-EXC-HEAD AFTER ADVANCING 2 LINES
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 1 LINE
           MOVE 5                      TO WS-LINES.

      ******************************************************************
      * The vendor file is flushed by the commit point sequence: the
      * checkpoint is only advanced after Db2 has committed
       7100-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7100-COMMIT-CHECKPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-T-COMMITS
           MOVE 0                      TO WS-SINCE-COMMIT
           SET CK-RUNNING              TO TRUE
           PERFORM 7110-WRITE-CHECKPOINT.

      ******************************************************************
       7110-WRITE-CHECKPOINT.
           EXEC SQL
              SET :WS-CURRENT-TS = CURRENT TIMESTAMP
           END-EXEC
           MOVE WS-PROGRAM-ID          TO CK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CK-BUS-DATE
           MOVE WS-LAST-DOC-ID         TO CK-LAST-KEY-1
           MOVE 0                      TO CK-LAST-KEY-2
           MOVE WS-T-WRITTEN           TO CK-UNITS-DONE
           MOVE WS-T-COMMITS           TO CK-COMMITS
           MOVE WS-CURRENT-TS          TO CK-TIMESTAMP
           MOVE WS-RUN-START-TS        TO CK-RUN-START-TS
           OPEN OUTPUT CHKPT-FILE
           WRITE CK-RECORD
           IF WS-CK-STATUS NOT = '00'
              MOVE 'CHECKPOINT WRITE FAILED' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           CLOSE CHKPT-FILE.

      ******************************************************************
       7200-WRITE-TEXT.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           WRITE RPT-RECORD FROM RH-TEXT-LINE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES
           MOVE SPACES                 TO RH-TEXT.

      ******************************************************************
       7300-WRITE-EXCEPTION.
           IF WS-LINES > 55
              PERFORM 7000-NEW-PAGE
           END-IF
           MOVE DOC-DOC-ID             TO R4-DOC-ID
           MOVE ACT-ACCOUNT-NUMBER     TO R4-ACCOUNT-NUMBER
           MOVE DOC-DOC-TYPE           TO R4-DOC-TYPE
           MOVE DOC-DELIVERY-METHOD    TO R4-DELIVERY
           MOVE WS-ERROR-TEXT          TO R4-REASON
           WRITE RPT-RECORD FROM R4-EXC-LINE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINES.

      ******************************************************************
       7400-WRITE-TOTALS.
           MOVE 99                     TO WS-LINES
           PERFORM 7200-WRITE-TEXT
           WRITE RPT-RECORD FROM R4-TYPE-HEAD AFTER ADVANCING 2 LINES
           PERFORM VARYING DX FROM 1 BY 1 UNTIL DX > WS-DT-COUNT
              MOVE WS-DT-TYPE(DX)      TO R4-T-DOC-TYPE
              MOVE WS-DT-SENT(DX)      TO R4-T-SENT
              MOVE WS-DT-ERRORS(DX)    TO R4-T-ERRORS
              WRITE RPT-RECORD FROM R4-TYPE-LINE
                    AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE RPT-RECORD FROM RH-RULE-LINE AFTER ADVANCING 2 LINES
           MOVE 'REQUESTS READ'        TO RH-TOT-LABEL
           MOVE WS-T-READ              TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'RECORDS WRITTEN / AMOUNT' TO RH-TOT-LABEL
           MOVE WS-T-WRITTEN           TO RH-TOT-COUNT
           MOVE WS-T-AMOUNT            TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'OF WHICH RE-WRITTEN ON RESTART' TO RH-TOT-LABEL
           MOVE WS-T-REWRITTEN         TO RH-TOT-COUNT
           MOVE 0                      TO RH-TOT-AMOUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'REQUESTS IN ERROR (ERRR)' TO RH-TOT-LABEL
           MOVE WS-T-ERRORS            TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'REQUESTS CHANGED DURING RUN' TO RH-TOT-LABEL
           MOVE WS-T-RACED             TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE
           MOVE 'COMMITS (INCLUDING PRIOR RUNS TODAY)' TO RH-TOT-LABEL
           MOVE WS-T-COMMITS           TO RH-TOT-COUNT
           PERFORM 7410-WRITE-TOTAL-LINE.

      ******************************************************************
       7410-WRITE-TOTAL-LINE.
           WRITE RPT-RECORD FROM RH-TOTAL-LINE AFTER ADVANCING 1 LINE.

      ******************************************************************
       7500-CHECK-VENDOR-WRITE.
           IF WS-PV-STATUS NOT = '00'
              STRING 'PRTVEND WRITE FAILED STATUS ' WS-PV-STATUS
                     DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-BIL-DOC-007 Trailer for vendor reconciliation
       9000-RETURN.
           IF NOT WS-RUN-ENDED
              MOVE WS-T-WRITTEN        TO PV-T-RECORD-COUNT
              MOVE WS-LAST-DOC-ID      TO PV-T-LAST-DOC-ID
              IF WS-T-AMOUNT < 0
                 COMPUTE PV-T-AMOUNT = 0 - WS-T-AMOUNT
                 MOVE '-'              TO PV-T-AMOUNT-SIGN
              ELSE
                 MOVE WS-T-AMOUNT      TO PV-T-AMOUNT
                 MOVE '+'              TO PV-T-AMOUNT-SIGN
              END-IF
              WRITE VENDOR-RECORD FROM PV-TRAILER-REC
              PERFORM 7500-CHECK-VENDOR-WRITE
              CLOSE VENDOR-FILE
              EXEC SQL COMMIT END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '9000-RETURN'    TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              ADD 1                    TO WS-T-COMMITS
              SET CK-COMPLETE          TO TRUE
              PERFORM 7110-WRITE-CHECKPOINT
              PERFORM 7400-WRITE-TOTALS
           END-IF
           IF WS-T-ERRORS > 0 OR WS-T-RACED > 0
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF
           MOVE WS-MAX-RC              TO WS-CNT-DISP
           STRING 'BILDOCGN ENDED - RETURN CODE ' WS-CNT-DISP
                  DELIMITED BY SIZE INTO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           CLOSE REPORT-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Abend: the partial vendor file must not be transmitted; the
      * restart rebuilds it from the original run start
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE C-DOCS END-EXEC
              SET WS-CURSOR-CLOSED     TO TRUE
           END-IF
           DISPLAY 'BILDOCGN ABEND: ' WS-ABEND-TEXT
           STRING 'RUN ABENDED - ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           MOVE 'PRTVEND IS INCOMPLETE - DO NOT TRANSMIT; RESTART'
                                       TO RH-TEXT
           PERFORM 7200-WRITE-TEXT
           CLOSE REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           STRING 'Db2 SQLCODE ' WS-SQLCODE-DISP ' IN ' WS-ERR-PARA
                  ' KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO WS-ABEND-TEXT
           IF SQLCODE NOT = -911 AND SQLCODE NOT = -913
              INITIALIZE WS-ERR-CA
              MOVE 'ERRLOG  '          TO ERC-REQUEST-ID
              MOVE WS-BATCH-USER       TO ERC-USER-ID
              MOVE WS-PROGRAM-ID       TO ERC-CALLER-PGM
              MOVE WS-PROGRAM-ID       TO ERC-ER-PROGRAM
              MOVE WS-ERR-PARA         TO ERC-ER-PARAGRAPH
              MOVE SQLCODE             TO ERC-ER-SQLCODE
              MOVE WS-ABEND-TEXT       TO ERC-ER-TEXT
              MOVE WS-ERR-KEY          TO ERC-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9800-ABEND.
