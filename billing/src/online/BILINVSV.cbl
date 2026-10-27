       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILINVSV.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILINVSV                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Invoice service. Inquiry of an invoice with its  *
      *               invoice items, list of an account's latest       *
      *               invoices, ad-hoc invoicing of planned items up   *
      *               to a cut-off date and re-send of an invoice      *
      *               document.                                        *
      * REQUESTS    : INVINQ  invoice header + items as rows           *
      *               INVLST  newest 20 invoices of an account         *
      *               INVADH  ad-hoc invoice(s) for an account         *
      *               INVRSN  re-send invoice document                 *
      * CALLED BY   : BILINVQP (screen), BILINVB0 family, API adapter  *
      * CALLS       : BILSEQGN BILDATES BILHOLD0 BILGLPST BILACCT0     *
      *               BILERRLG                                         *
      * TABLES      : BIL_INVOICE           (C R U)                    *
      *               BIL_INVOICE_ITEM      (R U)                      *
      *               BIL_CHARGE            (R)                        *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_DOCUMENT_REQUEST  (C R)                      *
      *               BIL_CONTROL           (R)                        *
      * RULES       : BR-BIL-INV-001 Invoice is located by number when *
      *                              given, otherwise by surrogate id  *
      *               BR-BIL-INV-002 Items list in due-date / install  *
      *                              order; > 20 items is a warning    *
      *               BR-BIL-INV-003 Account list is the newest 20 by  *
      *                              bill date then invoice id         *
      *               BR-BIL-INV-004 Ad-hoc invoicing only for an      *
      *                              active (ACTV) account             *
      *               BR-BIL-INV-005 An active INVC hold blocks ad-hoc *
      *                              invoicing                         *
      *               BR-BIL-INV-006 Cut-off defaults to business date *
      *                              and may not be more than 366 days *
      *                              ahead of it                       *
      *               BR-BIL-INV-007 One invoice per invoice stream,   *
      *                              covering PLAN items with event    *
      *                              date on or before the cut-off     *
      *               BR-BIL-INV-008 Invoice due date is the earliest  *
      *                              item due date, never before the   *
      *                              bill date                         *
      *               BR-BIL-INV-009 Prior balance is the open amount  *
      *                              of already billed (BILD/DUE)      *
      *                              items, carried on the first       *
      *                              invoice of the run only           *
      *               BR-BIL-INV-010 Amount due = invoice amount +     *
      *                              prior balance                     *
      *               BR-BIL-INV-011 Billing posts DR BILLED / CR      *
      *                              UNBILLED; a credit invoice posts  *
      *                              the reverse                       *
      *               BR-BIL-INV-012 Every invoice raises a document   *
      *                              request on the account's delivery *
      *                              method                            *
      *               BR-BIL-INV-013 Items are only billed if still    *
      *                              PLAN; a count mismatch is a       *
      *                              concurrent change (conflict)      *
      *               BR-BIL-INV-014 Re-send not allowed for PLAN or   *
      *                              CANC invoices                     *
      *               BR-BIL-INV-015 Re-send rejected while an earlier *
      *                              request is still RQST             *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILINVSV'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-MAX-ROWS              PIC S9(4) COMP VALUE 20.
           05 WS-MAX-CUTOFF-DAYS       PIC S9(4) COMP VALUE 366.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-CUTOFF-DATE           PIC X(10) VALUE SPACES.
           05 WS-DUE-DATE              PIC X(10) VALUE SPACES.
           05 WS-ROW                   PIC S9(4) COMP VALUE 0.
           05 WS-ITEMS-SEEN            PIC S9(4) COMP VALUE 0.
           05 WS-INVOICES-MADE         PIC S9(4) COMP VALUE 0.
           05 WS-ITEM-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-PENDING-DOCS          PIC S9(9) COMP VALUE 0.
           05 WS-STREAM-ID             PIC S9(9) COMP VALUE 0.
           05 WS-STREAM-AMOUNT         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-MIN-DUE-DATE          PIC X(10) VALUE SPACES.
           05 WS-PRIOR-BALANCE         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-INV-PAID              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-GL-AMOUNT             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DOC-TYPE              PIC X(8)  VALUE SPACES.
           05 WS-DOC-ID                PIC S9(9) COMP VALUE 0.
           05 WS-INST-DISP             PIC 99.
           05 WS-COUNT-DISP            PIC ZZ9.
           05 WS-POLNO-DISP            PIC 9(10).
           05 WS-ID-DISP               PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.
           05 WS-OPEN-CURSOR           PIC X(8)  VALUE SPACES.
           05 WS-FIRST-INVOICE         PIC X     VALUE 'Y'.
              88 WS-IS-FIRST-INVOICE             VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDINV END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDDOC END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * Items of one invoice with their charge pattern and policy
           EXEC SQL
              DECLARE C-INV-ITEMS CURSOR FOR
               SELECT I.ITEM_ID, I.INSTALL_NUMBER, I.EVENT_DATE,
                      I.DUE_DATE, I.ITEM_AMOUNT,
                      I.PAID_AMOUNT + I.WRITEOFF_AMOUNT,
                      I.ITEM_STATUS, C.CHARGE_PATTERN,
                      P.GENAPP_POL_NUM
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_CHARGE C
                   ON C.CHARGE_ID = I.CHARGE_ID
                 JOIN BILDB.BIL_POLICY_PERIOD P
                   ON P.PP_ID = I.PP_ID
                WHERE I.INVOICE_ID = :INV-INVOICE-ID
                ORDER BY I.DUE_DATE, I.INSTALL_NUMBER, I.ITEM_ID
                FOR FETCH ONLY
           END-EXEC.

      * Newest invoices of an account with the amount settled so far
           EXEC SQL
              DECLARE C-ACCT-INVS CURSOR FOR
               SELECT V.INVOICE_ID, V.INVOICE_NUMBER, V.BILL_DATE,
                      V.DUE_DATE, V.INVOICE_AMOUNT, V.INVOICE_STATUS,
                      V.ADHOC_FLAG, V.ITEM_COUNT,
                      COALESCE((SELECT SUM(I.PAID_AMOUNT
                                         + I.WRITEOFF_AMOUNT)
                                  FROM BILDB.BIL_INVOICE_ITEM I
                                 WHERE I.INVOICE_ID = V.INVOICE_ID),
                               0)
                 FROM BILDB.BIL_INVOICE V
                WHERE V.ACCOUNT_ID = :INV-ACCOUNT-ID
                ORDER BY V.BILL_DATE DESC, V.INVOICE_ID DESC
                FETCH FIRST 20 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

      * Streams of an account with planned items up to the cut-off
           EXEC SQL
              DECLARE C-ADH-STREAMS CURSOR FOR
               SELECT STREAM_ID, COUNT(*), SUM(ITEM_AMOUNT),
                      MIN(DUE_DATE)
                 FROM BILDB.BIL_INVOICE_ITEM
                WHERE ACCOUNT_ID  = :ITM-ACCOUNT-ID
                  AND ITEM_STATUS = 'PLAN'
                  AND EVENT_DATE <= :WS-CUTOFF-DATE
                GROUP BY STREAM_ID
                ORDER BY STREAM_ID
                FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTC-==.
           03 DTC-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTC-==.
       01  WS-HLD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==HLC-==.
           03 HLC-BODY.
              COPY BILCHLD REPLACING LEADING ==CA-== BY ==HLC-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLC-==.
           03 GLC-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLC-==.
       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACC-==.
           03 ACC-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCINV.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'INVINQ  '
                 PERFORM 2000-INVOICE-INQUIRY
              WHEN 'INVLST  '
                 PERFORM 2100-INVOICE-LIST
              WHEN 'INVADH  '
                 PERFORM 2200-ADHOC-INVOICE
              WHEN 'INVRSN  '
                 PERFORM 2300-RESEND-INVOICE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILINVSV: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-IV-ROW-COUNT
           MOVE 0                      TO WS-ROW
           SET WS-CURSOR-CLOSED        TO TRUE
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO CA-BUS-DATE
           END-IF
           MOVE CA-BUS-DATE            TO WS-BUS-DATE.

      ******************************************************************
      * INVINQ - one invoice with its items as rows
       2000-INVOICE-INQUIRY.
           PERFORM 3000-VALIDATE-INVOICE-KEY
           PERFORM 4000-READ-INVOICE
           PERFORM 2010-MOVE-INVOICE-HEADER
           PERFORM 4010-LIST-INVOICE-ITEMS.

      ******************************************************************
       2010-MOVE-INVOICE-HEADER.
           MOVE INV-INVOICE-ID         TO CA-IV-INVOICE-ID
           MOVE INV-INVOICE-NUMBER     TO CA-IV-INVOICE-NUMBER
           MOVE INV-ACCOUNT-ID         TO CA-IV-ACCOUNT-ID
           MOVE INV-BILL-DATE          TO CA-IV-BILL-DATE
           MOVE INV-DUE-DATE           TO CA-IV-DUE-DATE
           MOVE INV-INVOICE-AMOUNT     TO CA-IV-AMOUNT
           MOVE INV-AMOUNT-DUE         TO CA-IV-AMOUNT-DUE
           MOVE INV-INVOICE-STATUS     TO CA-IV-STATUS.

      ******************************************************************
      * INVLST - newest invoices of an account
       2100-INVOICE-LIST.
           IF CA-IV-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACCTREQ '          TO CA-REASON-CODE
              MOVE 'Account id is required for an invoice list'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-ACCOUNT
           PERFORM 4300-CALC-PRIOR-BALANCE
           MOVE WS-PRIOR-BALANCE       TO CA-IV-AMOUNT-DUE
           MOVE ACT-ACCOUNT-STATUS     TO CA-IV-STATUS
           PERFORM 4020-LIST-ACCOUNT-INVOICES
           IF CA-IV-ROW-COUNT = 0
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'NOINVS  '          TO CA-REASON-CODE
              MOVE 'Account has no invoices'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * INVADH - ad-hoc invoice for planned items up to a cut-off
       2200-ADHOC-INVOICE.
           IF CA-IV-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACCTREQ '          TO CA-REASON-CODE
              MOVE 'Account id is required for ad-hoc invoicing'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-ACCOUNT
      * BR-BIL-INV-004 Ad-hoc invoicing only for an active account
           IF ACT-ACCOUNT-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTNOTAC'          TO CA-REASON-CODE
              STRING 'Account status ' ACT-ACCOUNT-STATUS
                     ' does not allow invoicing'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3200-VALIDATE-CUTOFF
           PERFORM 8100-CHECK-INVOICE-HOLD
      * BR-BIL-INV-005 An active INVC hold blocks ad-hoc invoicing
           IF HLC-HL-IS-ACTIVE
              SET CA-RC-STATE          TO TRUE
              MOVE 'INVCHOLD'          TO CA-REASON-CODE
              MOVE 'Account has an active invoice hold (INVC)'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4300-CALC-PRIOR-BALANCE
           MOVE 'Y'                    TO WS-FIRST-INVOICE
           MOVE 0                      TO WS-INVOICES-MADE
           MOVE CA-IV-ACCOUNT-ID       TO ITM-ACCOUNT-ID
           PERFORM 4200-OPEN-ADHOC-STREAMS
           PERFORM 4210-FETCH-ADHOC-STREAM
           PERFORM UNTIL SQLCODE = 100
              IF WS-INVOICES-MADE >= WS-MAX-ROWS
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'MORESTRM'       TO CA-REASON-CODE
                 MOVE 'More than 20 streams - run INVADH again'
                                       TO CA-MESSAGE
                 MOVE 100              TO SQLCODE
              ELSE
                 PERFORM 2210-BILL-ONE-STREAM
                 PERFORM 4210-FETCH-ADHOC-STREAM
              END-IF
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR
           IF WS-INVOICES-MADE = 0
              SET CA-RC-NOT-FOUND      TO TRUE
              MOVE 'NOITEMS '          TO CA-REASON-CODE
              STRING 'No planned items on or before '
                     WS-CUTOFF-DATE DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8400-REFRESH-BALANCES
           MOVE CA-IV-R-ID(1)          TO INV-INVOICE-ID
           MOVE SPACES                 TO INV-INVOICE-NUMBER
           PERFORM 4000-READ-INVOICE
           PERFORM 2010-MOVE-INVOICE-HEADER
           MOVE WS-INVOICES-MADE       TO CA-IV-ROW-COUNT.

      ******************************************************************
      * BR-BIL-INV-007 One invoice per stream for its PLAN items
       2210-BILL-ONE-STREAM.
           MOVE 'INV     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQC-SQ-VALUE           TO INV-INVOICE-ID
           MOVE 'INVNO   '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQC-SQ-FORMATTED       TO INV-INVOICE-NUMBER
           PERFORM 5000-DERIVE-DUE-DATE
           MOVE CA-IV-ACCOUNT-ID       TO INV-ACCOUNT-ID
           MOVE WS-STREAM-ID           TO INV-STREAM-ID
           MOVE WS-BUS-DATE            TO INV-BILL-DATE
           MOVE WS-DUE-DATE            TO INV-DUE-DATE
           MOVE WS-STREAM-AMOUNT       TO INV-INVOICE-AMOUNT
           MOVE WS-ITEM-COUNT          TO INV-ITEM-COUNT
           PERFORM 5100-CALC-AMOUNT-DUE
           MOVE 'BILD'                 TO INV-INVOICE-STATUS
           MOVE 'Y'                    TO INV-ADHOC-FLAG
           MOVE 0                      TO INV-DOC-ID
           PERFORM 4400-INSERT-INVOICE
           PERFORM 4410-BILL-STREAM-ITEMS
           PERFORM 8200-POST-INVOICE-LEDGER
           MOVE 'INVOICE '             TO WS-DOC-TYPE
           PERFORM 4420-INSERT-DOC-REQUEST
           PERFORM 4430-SET-INVOICE-DOC
           ADD 1                       TO WS-INVOICES-MADE
           MOVE WS-INVOICES-MADE       TO WS-ROW
           MOVE INV-INVOICE-ID         TO CA-IV-R-ID(WS-ROW)
           MOVE INV-INVOICE-NUMBER     TO CA-IV-R-NUMBER(WS-ROW)
           MOVE INV-BILL-DATE          TO CA-IV-R-DATE(WS-ROW)
           MOVE INV-DUE-DATE           TO CA-IV-R-DUE(WS-ROW)
           MOVE INV-INVOICE-AMOUNT     TO CA-IV-R-AMOUNT(WS-ROW)
           MOVE 0                      TO CA-IV-R-PAID(WS-ROW)
           MOVE INV-INVOICE-STATUS     TO CA-IV-R-STATUS(WS-ROW)
           MOVE WS-ITEM-COUNT          TO WS-COUNT-DISP
           STRING 'ADHOC ' WS-COUNT-DISP ' ITEMS'
                  DELIMITED BY SIZE INTO CA-IV-R-DESC(WS-ROW).

      ******************************************************************
      * INVRSN - re-send the invoice document
       2300-RESEND-INVOICE.
           PERFORM 3000-VALIDATE-INVOICE-KEY
           PERFORM 4000-READ-INVOICE
      * BR-BIL-INV-014 Re-send not allowed for PLAN or CANC invoices
           IF INV-INVOICE-STATUS = 'PLAN' OR 'CANC'
              SET CA-RC-STATE          TO TRUE
              MOVE 'INVSTATE'          TO CA-REASON-CODE
              STRING 'Invoice ' INV-INVOICE-NUMBER ' status '
                     INV-INVOICE-STATUS ' cannot be re-sent'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4030-COUNT-PENDING-DOCS
      * BR-BIL-INV-015 One outstanding document request at a time
           IF WS-PENDING-DOCS > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DOCPEND '          TO CA-REASON-CODE
              STRING 'Invoice ' INV-INVOICE-NUMBER
                     ' already has a document request pending'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE INV-ACCOUNT-ID         TO CA-IV-ACCOUNT-ID
           PERFORM 4100-READ-ACCOUNT
           MOVE 'INVCOPY '             TO WS-DOC-TYPE
           PERFORM 4420-INSERT-DOC-REQUEST
           PERFORM 4430-SET-INVOICE-DOC
           PERFORM 2010-MOVE-INVOICE-HEADER
           MOVE WS-DOC-ID              TO WS-ID-DISP
           STRING 'Re-send requested, document ' WS-ID-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-BIL-INV-001 Number first, otherwise surrogate id
       3000-VALIDATE-INVOICE-KEY.
           IF CA-IV-INVOICE-NUMBER = SPACES OR LOW-VALUES
              IF CA-IV-INVOICE-ID NOT > 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'INVKEY  '       TO CA-REASON-CODE
                 MOVE 'Invoice number or invoice id is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE SPACES              TO INV-INVOICE-NUMBER
              MOVE CA-IV-INVOICE-ID    TO INV-INVOICE-ID
           ELSE
              MOVE CA-IV-INVOICE-NUMBER TO INV-INVOICE-NUMBER
              MOVE 0                   TO INV-INVOICE-ID
           END-IF.

      ******************************************************************
      * BR-BIL-INV-006 Cut-off defaults to the business date and may
      * not lie more than 366 days ahead of it
       3200-VALIDATE-CUTOFF.
           IF CA-IV-CUTOFF-DATE = SPACES OR LOW-VALUES
              MOVE WS-BUS-DATE         TO CA-IV-CUTOFF-DATE
           END-IF
           INITIALIZE DTC-BODY
           MOVE 'DTVALD  '             TO DTC-REQUEST-ID
           MOVE CA-IV-CUTOFF-DATE      TO DTC-DT-DATE-1
           PERFORM 8300-CALL-DATES
           IF NOT DTC-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CUTOFFDT'          TO CA-REASON-CODE
              STRING 'Cut-off date ' CA-IV-CUTOFF-DATE
                     ' is not a valid date'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE DTC-BODY
           MOVE 'DTDIFF  '             TO DTC-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTC-DT-DATE-1
           MOVE CA-IV-CUTOFF-DATE      TO DTC-DT-DATE-2
           PERFORM 8300-CALL-DATES
           IF DTC-DT-RESULT-NUM > WS-MAX-CUTOFF-DAYS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CUTOFFHI'          TO CA-REASON-CODE
              MOVE 'Cut-off more than 366 days after business date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-IV-CUTOFF-DATE      TO WS-CUTOFF-DATE.

      ******************************************************************
       4000-READ-INVOICE.
           IF INV-INVOICE-NUMBER NOT = SPACES
              EXEC SQL
                 SELECT INVOICE_ID, INVOICE_NUMBER, ACCOUNT_ID,
                        STREAM_ID, BILL_DATE, DUE_DATE,
                        INVOICE_AMOUNT, AMOUNT_DUE, PRIOR_BALANCE,
                        INVOICE_STATUS, ADHOC_FLAG, ITEM_COUNT,
                        DOC_ID
                   INTO :INV-INVOICE-ID, :INV-INVOICE-NUMBER,
                        :INV-ACCOUNT-ID, :INV-STREAM-ID,
                        :INV-BILL-DATE, :INV-DUE-DATE,
                        :INV-INVOICE-AMOUNT, :INV-AMOUNT-DUE,
                        :INV-PRIOR-BALANCE, :INV-INVOICE-STATUS,
                        :INV-ADHOC-FLAG, :INV-ITEM-COUNT,
                        :INV-DOC-ID
                   FROM BILDB.BIL_INVOICE
                  WHERE INVOICE_NUMBER = :INV-INVOICE-NUMBER
              END-EXEC
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
           ELSE
              EXEC SQL
                 SELECT INVOICE_ID, INVOICE_NUMBER, ACCOUNT_ID,
                        STREAM_ID, BILL_DATE, DUE_DATE,
                        INVOICE_AMOUNT, AMOUNT_DUE, PRIOR_BALANCE,
                        INVOICE_STATUS, ADHOC_FLAG, ITEM_COUNT,
                        DOC_ID
                   INTO :INV-INVOICE-ID, :INV-INVOICE-NUMBER,
                        :INV-ACCOUNT-ID, :INV-STREAM-ID,
                        :INV-BILL-DATE, :INV-DUE-DATE,
                        :INV-INVOICE-AMOUNT, :INV-AMOUNT-DUE,
                        :INV-PRIOR-BALANCE, :INV-INVOICE-STATUS,
                        :INV-ADHOC-FLAG, :INV-ITEM-COUNT,
                        :INV-DOC-ID
                   FROM BILDB.BIL_INVOICE
                  WHERE INVOICE_ID = :INV-INVOICE-ID
              END-EXEC
              MOVE INV-INVOICE-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
           END-IF
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INVNOTFD'       TO CA-REASON-CODE
                 STRING 'Invoice ' WS-ERR-KEY ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-INVOICE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-INV-002 Items in due-date / install order, max 20 rows
       4010-LIST-INVOICE-ITEMS.
           MOVE 0                      TO WS-ITEMS-SEEN
           EXEC SQL OPEN C-INV-ITEMS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4010-LIST-INVOICE-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'ITEMS   '             TO WS-OPEN-CURSOR
           PERFORM UNTIL SQLCODE = 100
              EXEC SQL
                 FETCH C-INV-ITEMS
                  INTO :ITM-ITEM-ID, :ITM-INSTALL-NUMBER,
                       :ITM-EVENT-DATE, :ITM-DUE-DATE,
                       :ITM-ITEM-AMOUNT, :ITM-PAID-AMOUNT,
                       :ITM-ITEM-STATUS, :CHG-CHARGE-PATTERN,
                       :PPR-GENAPP-POL-NUM
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-ITEMS-SEEN
                    IF WS-ITEMS-SEEN NOT > WS-MAX-ROWS
                       PERFORM 4011-MOVE-ITEM-ROW
                    END-IF
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '4010-LIST-INVOICE-ITEMS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR
           IF WS-ITEMS-SEEN > WS-MAX-ROWS
              SET CA-RC-WARNING        TO TRUE
              MOVE 'MOREROWS'          TO CA-REASON-CODE
              MOVE WS-ITEMS-SEEN       TO WS-COUNT-DISP
              STRING 'First 20 of ' WS-COUNT-DISP ' items shown'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       4011-MOVE-ITEM-ROW.
           ADD 1                       TO CA-IV-ROW-COUNT
           MOVE CA-IV-ROW-COUNT        TO WS-ROW
           MOVE ITM-ITEM-ID            TO CA-IV-R-ID(WS-ROW)
           MOVE PPR-GENAPP-POL-NUM     TO WS-POLNO-DISP
           MOVE WS-POLNO-DISP          TO CA-IV-R-NUMBER(WS-ROW)
           MOVE ITM-EVENT-DATE         TO CA-IV-R-DATE(WS-ROW)
           MOVE ITM-DUE-DATE           TO CA-IV-R-DUE(WS-ROW)
           MOVE ITM-ITEM-AMOUNT        TO CA-IV-R-AMOUNT(WS-ROW)
           MOVE ITM-PAID-AMOUNT        TO CA-IV-R-PAID(WS-ROW)
           MOVE ITM-ITEM-STATUS        TO CA-IV-R-STATUS(WS-ROW)
           MOVE ITM-INSTALL-NUMBER     TO WS-INST-DISP
           STRING CHG-CHARGE-PATTERN DELIMITED BY SPACE
                  ' INST ' WS-INST-DISP DELIMITED BY SIZE
                  INTO CA-IV-R-DESC(WS-ROW).

      ******************************************************************
      * BR-BIL-INV-003 Newest 20 by bill date, then invoice id
       4020-LIST-ACCOUNT-INVOICES.
           MOVE CA-IV-ACCOUNT-ID       TO INV-ACCOUNT-ID
           EXEC SQL OPEN C-ACCT-INVS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4020-LIST-ACCOUNT-INVOICES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'ACCTINVS'             TO WS-OPEN-CURSOR
           PERFORM UNTIL SQLCODE = 100
              EXEC SQL
                 FETCH C-ACCT-INVS
                  INTO :INV-INVOICE-ID, :INV-INVOICE-NUMBER,
                       :INV-BILL-DATE, :INV-DUE-DATE,
                       :INV-INVOICE-AMOUNT, :INV-INVOICE-STATUS,
                       :INV-ADHOC-FLAG, :INV-ITEM-COUNT,
                       :WS-INV-PAID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 4021-MOVE-INVOICE-ROW
                 WHEN 100
                    CONTINUE
                 WHEN OTHER
                    MOVE '4020-LIST-ACCOUNT-INVOICES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR.

      ******************************************************************
       4021-MOVE-INVOICE-ROW.
           ADD 1                       TO CA-IV-ROW-COUNT
           MOVE CA-IV-ROW-COUNT        TO WS-ROW
           MOVE INV-INVOICE-ID         TO CA-IV-R-ID(WS-ROW)
           MOVE INV-INVOICE-NUMBER     TO CA-IV-R-NUMBER(WS-ROW)
           MOVE INV-BILL-DATE          TO CA-IV-R-DATE(WS-ROW)
           MOVE INV-DUE-DATE           TO CA-IV-R-DUE(WS-ROW)
           MOVE INV-INVOICE-AMOUNT     TO CA-IV-R-AMOUNT(WS-ROW)
           MOVE WS-INV-PAID            TO CA-IV-R-PAID(WS-ROW)
           MOVE INV-INVOICE-STATUS     TO CA-IV-R-STATUS(WS-ROW)
           MOVE INV-ITEM-COUNT         TO WS-COUNT-DISP
           IF INV-ADHOC-FLAG = 'Y'
              STRING 'ADHOC ' WS-COUNT-DISP ' ITEMS'
                     DELIMITED BY SIZE INTO CA-IV-R-DESC(WS-ROW)
           ELSE
              STRING 'CYCLE ' WS-COUNT-DISP ' ITEMS'
                     DELIMITED BY SIZE INTO CA-IV-R-DESC(WS-ROW)
           END-IF.

      ******************************************************************
       4030-COUNT-PENDING-DOCS.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-PENDING-DOCS
                FROM BILDB.BIL_DOCUMENT_REQUEST
               WHERE REF_TYPE   = 'INV '
                 AND REF_ID     = :INV-INVOICE-ID
                 AND DOC_STATUS = 'RQST'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4030-COUNT-PENDING-DOCS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4100-READ-ACCOUNT.
           MOVE CA-IV-ACCOUNT-ID       TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT ACCOUNT_NUMBER, ACCOUNT_STATUS, DELIVERY_METHOD,
                     COLLECTION_STATUS
                INTO :ACT-ACCOUNT-NUMBER, :ACT-ACCOUNT-STATUS,
                     :ACT-DELIVERY-METHOD, :ACT-COLLECTION-STATUS
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 MOVE ACT-ACCOUNT-ID   TO WS-ID-DISP
                 STRING 'Billing account ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE ACT-ACCOUNT-ID   TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4100-READ-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-OPEN-ADHOC-STREAMS.
           EXEC SQL OPEN C-ADH-STREAMS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-OPEN-ADHOC-STREAMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'ADHSTRM '             TO WS-OPEN-CURSOR.

      ******************************************************************
       4210-FETCH-ADHOC-STREAM.
           EXEC SQL
              FETCH C-ADH-STREAMS
               INTO :WS-STREAM-ID, :WS-ITEM-COUNT,
                    :WS-STREAM-AMOUNT, :WS-MIN-DUE-DATE
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4210-FETCH-ADHOC-STREAM' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Closes whichever cursor is open; called on every exit path
       4290-CLOSE-CURSOR.
           IF WS-CURSOR-OPEN
              EVALUATE WS-OPEN-CURSOR
                 WHEN 'ITEMS   '
                    EXEC SQL CLOSE C-INV-ITEMS END-EXEC
                 WHEN 'ACCTINVS'
                    EXEC SQL CLOSE C-ACCT-INVS END-EXEC
                 WHEN 'ADHSTRM '
                    EXEC SQL CLOSE C-ADH-STREAMS END-EXEC
              END-EVALUATE
              SET WS-CURSOR-CLOSED     TO TRUE
              MOVE SPACES              TO WS-OPEN-CURSOR
           END-IF.

      ******************************************************************
      * BR-BIL-INV-009 Prior balance = open amount of billed items
       4300-CALC-PRIOR-BALANCE.
           MOVE CA-IV-ACCOUNT-ID       TO ITM-ACCOUNT-ID
           EXEC SQL
              SELECT COALESCE(SUM(ITEM_AMOUNT - PAID_AMOUNT
                                  - WRITEOFF_AMOUNT), 0)
                INTO :WS-PRIOR-BALANCE
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE ACCOUNT_ID  = :ITM-ACCOUNT-ID
                 AND ITEM_STATUS IN ('BILD', 'DUE ')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-CALC-PRIOR-BALANCE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-INSERT-INVOICE.
           EXEC SQL
              INSERT INTO BILDB.BIL_INVOICE
                     (INVOICE_ID, INVOICE_NUMBER, ACCOUNT_ID,
                      STREAM_ID, BILL_DATE, DUE_DATE, INVOICE_AMOUNT,
                      AMOUNT_DUE, PRIOR_BALANCE, INVOICE_STATUS,
                      ADHOC_FLAG, ITEM_COUNT, DOC_ID, CREATE_TS,
                      UPDATE_TS)
              VALUES (:INV-INVOICE-ID, :INV-INVOICE-NUMBER,
                      :INV-ACCOUNT-ID, :INV-STREAM-ID,
                      :INV-BILL-DATE, :INV-DUE-DATE,
                      :INV-INVOICE-AMOUNT, :INV-AMOUNT-DUE,
                      :INV-PRIOR-BALANCE, :INV-INVOICE-STATUS,
                      :INV-ADHOC-FLAG, :INV-ITEM-COUNT, :INV-DOC-ID,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4400-INSERT-INVOICE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-INV-013 Only still-PLAN items are billed; a count that
      * differs from the cursor total means another task got there
       4410-BILL-STREAM-ITEMS.
           MOVE INV-INVOICE-ID         TO ITM-INVOICE-ID
           MOVE WS-STREAM-ID           TO ITM-STREAM-ID
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET INVOICE_ID  = :ITM-INVOICE-ID,
                     ITEM_STATUS = 'BILD',
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID  = :ITM-ACCOUNT-ID
                 AND STREAM_ID   = :ITM-STREAM-ID
                 AND ITEM_STATUS = 'PLAN'
                 AND EVENT_DATE <= :WS-CUTOFF-DATE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4410-BILL-STREAM-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) NOT = WS-ITEM-COUNT
              PERFORM 4290-CLOSE-CURSOR
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'ITEMCHNG'          TO CA-REASON-CODE
              MOVE 'Invoice items changed during invoicing - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-INV-012 Document request on the account delivery method
       4420-INSERT-DOC-REQUEST.
           MOVE 'DOC     '             TO SQC-SQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQC-SQ-VALUE           TO DOC-DOC-ID
           MOVE INV-ACCOUNT-ID         TO DOC-ACCOUNT-ID
           MOVE WS-DOC-TYPE            TO DOC-DOC-TYPE
           MOVE 'INV '                 TO DOC-REF-TYPE
           MOVE INV-INVOICE-ID         TO DOC-REF-ID
           MOVE ACT-DELIVERY-METHOD    TO DOC-DELIVERY-METHOD
           MOVE 'RQST'                 TO DOC-DOC-STATUS
           MOVE WS-BUS-DATE            TO DOC-REQUEST-DATE
           EXEC SQL
              INSERT INTO BILDB.BIL_DOCUMENT_REQUEST
                     (DOC_ID, ACCOUNT_ID, DOC_TYPE, REF_TYPE, REF_ID,
                      DELIVERY_METHOD, DOC_STATUS, REQUEST_DATE)
              VALUES (:DOC-DOC-ID, :DOC-ACCOUNT-ID, :DOC-DOC-TYPE,
                      :DOC-REF-TYPE, :DOC-REF-ID,
                      :DOC-DELIVERY-METHOD, :DOC-DOC-STATUS,
                      :DOC-REQUEST-DATE)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4420-INSERT-DOC-REQUEST' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE DOC-DOC-ID             TO WS-DOC-ID.

      ******************************************************************
       4430-SET-INVOICE-DOC.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE
                 SET DOC_ID    = :WS-DOC-ID,
                     UPDATE_TS = CURRENT TIMESTAMP
               WHERE INVOICE_ID = :INV-INVOICE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE INV-INVOICE-NUMBER  TO WS-ERR-KEY
              MOVE '4430-SET-INVOICE-DOC' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-DOC-ID              TO INV-DOC-ID.

      ******************************************************************
       4900-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE 'SYSTEM'            TO WS-ERR-KEY
              MOVE '4900-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-INV-008 Earliest item due date, never before bill date
       5000-DERIVE-DUE-DATE.
           IF WS-MIN-DUE-DATE < WS-BUS-DATE
              MOVE WS-BUS-DATE         TO WS-DUE-DATE
           ELSE
              MOVE WS-MIN-DUE-DATE     TO WS-DUE-DATE
           END-IF.

      ******************************************************************
      * BR-BIL-INV-010 Amount due = invoice amount + prior balance;
      * the prior balance is only carried by the first invoice
       5100-CALC-AMOUNT-DUE.
           IF WS-IS-FIRST-INVOICE
              MOVE WS-PRIOR-BALANCE    TO INV-PRIOR-BALANCE
              MOVE 'N'                 TO WS-FIRST-INVOICE
           ELSE
              MOVE 0                   TO INV-PRIOR-BALANCE
           END-IF
           COMPUTE INV-AMOUNT-DUE = INV-INVOICE-AMOUNT
                                  + INV-PRIOR-BALANCE.

      ******************************************************************
       8000-NEXT-SEQUENCE.
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE 1                      TO SQC-SQ-COUNT
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE CA-TRACE-ID            TO SQC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 4290-CLOSE-CURSOR
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8100-CHECK-INVOICE-HOLD.
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLC-REQUEST-ID
           MOVE CA-USER-ID             TO HLC-USER-ID
           MOVE WS-BUS-DATE            TO HLC-BUS-DATE
           MOVE CA-TRACE-ID            TO HLC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HLC-CALLER-PGM
           MOVE CA-IV-ACCOUNT-ID       TO HLC-HL-ACCOUNT-ID
           MOVE 'INVC'                 TO HLC-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLC-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           IF HLC-RETURN-CODE >= 12
              MOVE HLC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HLC-REASON-CODE     TO CA-REASON-CODE
              MOVE HLC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-INV-011 DR BILLED / CR UNBILLED; credit reverses sides
       8200-POST-INVOICE-LEDGER.
           IF INV-INVOICE-AMOUNT = 0
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLC-REQUEST-ID
           MOVE CA-USER-ID             TO GLC-USER-ID
           MOVE WS-BUS-DATE            TO GLC-BUS-DATE
           MOVE CA-TRACE-ID            TO GLC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLC-CALLER-PGM
           MOVE 'INVBILL '             TO GLC-GL-TXN-TYPE
           MOVE INV-ACCOUNT-ID         TO GLC-GL-ACCOUNT-ID
           MOVE 'INV '                 TO GLC-GL-REF-TYPE
           MOVE INV-INVOICE-ID         TO GLC-GL-REF-ID
           MOVE 2                      TO GLC-GL-LINE-COUNT
           IF INV-INVOICE-AMOUNT > 0
              MOVE INV-INVOICE-AMOUNT  TO WS-GL-AMOUNT
              MOVE 'BILLED  '          TO GLC-GL-T-ACCOUNT(1)
              MOVE 'UNBILLED'          TO GLC-GL-T-ACCOUNT(2)
           ELSE
              COMPUTE WS-GL-AMOUNT = 0 - INV-INVOICE-AMOUNT
              MOVE 'UNBILLED'          TO GLC-GL-T-ACCOUNT(1)
              MOVE 'BILLED  '          TO GLC-GL-T-ACCOUNT(2)
           END-IF
           MOVE 'D'                    TO GLC-GL-DR-CR(1)
           MOVE 'C'                    TO GLC-GL-DR-CR(2)
           MOVE WS-GL-AMOUNT           TO GLC-GL-AMOUNT(1)
                                          GLC-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLC-RETURN-CODE >= 12
              MOVE GLC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE GLC-REASON-CODE     TO CA-REASON-CODE
              MOVE GLC-MESSAGE         TO CA-MESSAGE
              PERFORM 4290-CLOSE-CURSOR
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8300-CALL-DATES.
           MOVE CA-USER-ID             TO DTC-USER-ID
           MOVE WS-BUS-DATE            TO DTC-BUS-DATE
           MOVE CA-TRACE-ID            TO DTC-TRACE-ID
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
      * Refresh billed / due / unbilled totals after invoicing
       8400-REFRESH-BALANCES.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTBAL '             TO ACC-REQUEST-ID
           MOVE CA-USER-ID             TO ACC-USER-ID
           MOVE WS-BUS-DATE            TO ACC-BUS-DATE
           MOVE CA-TRACE-ID            TO ACC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ACC-CALLER-PGM
           MOVE CA-IV-ACCOUNT-ID       TO ACC-AC-ACCOUNT-ID
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF ACC-RETURN-CODE >= 12
              MOVE ACC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE ACC-REASON-CODE     TO CA-REASON-CODE
              MOVE ACC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           EVALUATE SQLCODE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 STRING 'Duplicate key ' WS-ERR-KEY
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Invoice data locked - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DB2ERR  '       TO CA-REASON-CODE
                 STRING 'BILINVSV Db2 error SQLCODE ' WS-SQLCODE-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 INITIALIZE ERC-HEADER
                 MOVE 'ERRLOG  '       TO ERC-REQUEST-ID
                 MOVE CA-USER-ID       TO ERC-USER-ID
                 MOVE WS-PROGRAM-ID    TO ERC-CALLER-PGM
                 MOVE WS-PROGRAM-ID    TO ERC-ER-PROGRAM
                 MOVE WS-ERR-PARA      TO ERC-ER-PARAGRAPH
                 MOVE SQLCODE          TO ERC-ER-SQLCODE
                 MOVE CA-MESSAGE       TO ERC-ER-TEXT
                 MOVE WS-ERR-KEY       TO ERC-ER-KEY
                 CALL WS-ERR-PGM USING WS-ERR-CA
           END-EVALUATE
           PERFORM 4290-CLOSE-CURSOR
           PERFORM 9000-RETURN.
