       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILACCT0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILACCT0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Billing account service. Owns the account, its   *
      *               primary payer contact and its invoice stream,    *
      *               and is the only program that maintains the      *
      *               account balance buckets.                         *
      * REQUESTS    : ACCTADD  create account + payer + invoice stream *
      *               ACCTINQ  inquire by ACCOUNT-ID / ACCOUNT-NUMBER  *
      *               ACCTFND  find by GENAPP-CUST-NUM                 *
      *               ACCTUPD  update mutable fields (VERSION-NO)      *
      *               ACCTCLS  close account                           *
      *               ACCTBAL  recompute balances from invoice items   *
      * CALLED BY   : BIL account screens, BILINSTR, BILEVTDR          *
      *               (POLISSUE for a new customer), batch drivers     *
      * CALLS       : BILSEQGN, BILAUDIT, BILERRLG                     *
      * TABLES      : BIL_ACCOUNT            (C R U)                   *
      *               BIL_ACCOUNT_CONTACT    (C R U)                   *
      *               BIL_INVOICE_STREAM     (C R U)                   *
      *               BIL_PAYMENT_INSTRUMENT (R U)                     *
      *               BIL_DELINQ_PLAN        (R)                       *
      *               BIL_POLICY_PERIOD      (R)                       *
      *               BIL_INVOICE_ITEM       (R)                       *
      *               BIL_PAYMENT_RECEIPT    (R)                       *
      *               BIL_DELINQ_PROCESS     (R)                       *
      *               BIL_DISBURSEMENT       (R)                       *
      *               BIL_CONTROL            (R)                       *
      *               GENAPP.CUSTOMER        (R)                       *
      * RULES       : BR-BIL-ACT-001 Account type is PERS or COMM      *
      *               BR-BIL-ACT-002 Invoice day 1..28 (default 1) so  *
      *                              every month has the day           *
      *               BR-BIL-ACT-003 Delinquency plan must exist and   *
      *                              be active                         *
      *               BR-BIL-ACT-004 PERS payer needs first and last   *
      *                              name; COMM payer needs company    *
      *               BR-BIL-ACT-005 UK postcode format, stored as     *
      *                              outward + space + inward          *
      *               BR-BIL-ACT-006 Account number from BILSEQGN      *
      *                              ACTNO, never derived              *
      *               BR-BIL-ACT-007 One open account per GenApp       *
      *                              customer (duplicate RC 16)        *
      *               BR-BIL-ACT-008 A GenApp customer number must     *
      *                              exist in GENAPP.CUSTOMER          *
      *               BR-BIL-ACT-009 New account gets a primary payer  *
      *                              (PAYR) and a monthly invoice      *
      *                              stream anchored on invoice day    *
      *               BR-BIL-ACT-010 Account name defaults from payer  *
      *               BR-BIL-ACT-011 Autopay cannot be set at create   *
      *               BR-BIL-ACT-012 Update/close need matching        *
      *                              VERSION-NO (optimistic lock)      *
      *               BR-BIL-ACT-013 Field-level audit of name,        *
      *                              invoice day, delivery, plan,      *
      *                              autopay and payer address         *
      *               BR-BIL-ACT-014 Invoice day change re-anchors     *
      *                              the active invoice stream         *
      *               BR-BIL-ACT-015 Autopay ON needs an active ACH or *
      *                              CARD instrument of this account   *
      *               BR-BIL-ACT-016 EMAL / BOTH delivery needs an     *
      *                              e-mail address on the payer       *
      *               BR-BIL-ACT-017 Close only with no in-force       *
      *                              period, zero balances, no open    *
      *                              delinquency, no pending disb.     *
      *               BR-BIL-ACT-018 Close turns autopay off, closes   *
      *                              streams, deactivates instruments  *
      *               BR-BIL-ACT-019 Balance buckets derived from open *
      *                              item amounts by invoice / due     *
      *                              date; unapplied from receipts     *
      *               BR-BIL-ACT-020 Recompute keeps VERSION-NO; any   *
      *                              drift from stored is audited      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS PC-LETTER   IS 'A' THRU 'I' 'J' THRU 'R' 'S' THRU 'Z'
           CLASS PC-INWARD   IS 'A' 'B' 'D' THRU 'H' 'J' 'L' 'N'
                                'P' THRU 'R' 'S' THRU 'U' 'W' THRU 'Z'
           CLASS PHONE-CHARS IS '0' THRU '9' ' ' '+' '-' '(' ')'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILACCT0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-MAX-PERIODS           PIC S9(4) COMP VALUE 10.
           05 WS-PAYER-ROLE            PIC X(4)  VALUE 'PAYR'.
           05 WS-MONTHLY               PIC X(1)  VALUE 'M'.
           05 WS-AUD-ENTITY            PIC X(8)  VALUE 'ACCOUNT '.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-SAVE-SQLCODE          PIC S9(9) COMP.
           05 WS-COUNT                PIC S9(9) COMP.
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-EXPECT-VERSION        PIC S9(9) COMP.
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-SEQ-VALUE             PIC S9(9) COMP.
           05 WS-SEQ-FORMATTED         PIC X(12).
           05 WS-NUM-DISP              PIC 9(9).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-ACCT-TYPE             PIC X(4).
              88 WS-TYPE-PERSONAL      VALUE 'PERS'.
              88 WS-TYPE-COMMERCIAL    VALUE 'COMM'.
              88 WS-TYPE-VALID         VALUE 'PERS' 'COMM'.
           05 WS-BILL-LEVEL            PIC X(4).
              88 WS-LEVEL-VALID        VALUE 'ACCT' 'PLCY'.
           05 WS-DELIVERY              PIC X(4).
              88 WS-DELIVERY-VALID     VALUE 'MAIL' 'EMAL' 'BOTH'.
              88 WS-DELIVERY-EMAIL     VALUE 'EMAL' 'BOTH'.
           05 WS-AUTOPAY               PIC X(1).
              88 WS-AUTOPAY-ON         VALUE 'Y'.
              88 WS-AUTOPAY-VALID      VALUE 'Y' 'N'.
           05 WS-CONTACT-TYPE          PIC X(1).
              88 WS-CONTACT-PERSON     VALUE 'P'.
              88 WS-CONTACT-COMPANY    VALUE 'C'.
           05 WS-EFFECTIVE-EMAIL       PIC X(100).
           05 WS-CONTACT-CHANGED       PIC X(1).
              88 WS-CONTACT-IS-CHANGED VALUE 'Y'.
           05 WS-ANCHOR-CHANGED        PIC X(1).
              88 WS-ANCHOR-IS-CHANGED  VALUE 'Y'.

      * Snapshot of the row before an update, for field-level audit
       01  WS-BEFORE.
           05 WS-BF-NAME               PIC X(60).
           05 WS-BF-INVOICE-DAY        PIC S9(4) COMP.
           05 WS-BF-DELIVERY           PIC X(4).
           05 WS-BF-PLAN               PIC X(8).
           05 WS-BF-AUTOPAY            PIC X(1).
           05 WS-BF-AUTOPAY-INSTR      PIC S9(9) COMP.
           05 WS-BF-BILL-LEVEL         PIC X(4).
           05 WS-BF-ADDR1              PIC X(60).
           05 WS-BF-POSTCODE           PIC X(8).
           05 WS-BF-EMAIL              PIC X(100).
           05 WS-BF-PHONE              PIC X(20).

      * Balance buckets (recomputed) and the values found on the row
       01  WS-BALANCES.
           05 WS-UNBILLED              PIC S9(11)V99 COMP-3.
           05 WS-BILLED                PIC S9(11)V99 COMP-3.
           05 WS-DUE                   PIC S9(11)V99 COMP-3.
           05 WS-UNAPPLIED             PIC S9(11)V99 COMP-3.
           05 WS-OLD-UNBILLED          PIC S9(11)V99 COMP-3.
           05 WS-OLD-BILLED            PIC S9(11)V99 COMP-3.
           05 WS-OLD-DUE               PIC S9(11)V99 COMP-3.
           05 WS-OLD-UNAPPLIED         PIC S9(11)V99 COMP-3.

      * Trailing-length helper for VARCHAR host variables
       01  WS-TRIM.
           05 WS-TL-SRC                PIC X(100).
           05 WS-TL-LEN                PIC S9(4) COMP.

      * Postcode work area: shape string uses A letter, 9 digit, ? other
       01  WS-POSTCODE-WORK.
           05 WS-PC-IN                 PIC X(8).
           05 WS-PC-COMPACT            PIC X(8).
           05 WS-PC-SHAPE              PIC X(8).
           05 WS-PC-LEN                PIC S9(4) COMP.
           05 WS-PC-OUT-LEN            PIC S9(4) COMP.
           05 WS-PC-GAPS               PIC S9(4) COMP.
           05 WS-PC-CHAR               PIC X(1).
           05 WS-PC-PREV               PIC X(1).
           05 WS-PC-OUTWARD            PIC X(4).
              88 WS-PC-OUTWARD-OK      VALUE 'A9  ' 'A99 ' 'A9A '
                                             'AA9 ' 'AA99' 'AA9A'.
           05 WS-PC-INWARD             PIC X(3).
           05 WS-PC-K                  PIC S9(4) COMP.

       01  WS-EMAIL-WORK.
           05 WS-EM-AT-COUNT           PIC S9(4) COMP.
           05 WS-EM-AT-POS             PIC S9(4) COMP.
           05 WS-EM-DOT-COUNT          PIC S9(4) COMP.
           05 WS-EM-SPACES             PIC S9(4) COMP.

      * Audit request built by the handlers, written by 8100
       01  WS-AUDIT-REQ.
           05 WS-AU-ACTION             PIC X(4).
           05 WS-AU-FIELD              PIC X(18).
           05 WS-AU-BEFORE             PIC X(100).
           05 WS-AU-AFTER              PIC X(100).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDACN END-EXEC.
           EXEC SQL INCLUDE BILDIST END-EXEC.
           EXEC SQL INCLUDE BILDPIN END-EXEC.
           EXEC SQL INCLUDE BILDDPL END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE GENDCUS END-EXEC.

           EXEC SQL DECLARE PERIOD-CSR CURSOR FOR
              SELECT PP_ID, GENAPP_POL_NUM, PRODUCT_CODE,
                     PP_STATUS, EXP_DATE
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
               ORDER BY EXP_DATE DESC, PP_ID DESC
               FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCACT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'ACCTADD '
                 PERFORM 2000-ACCOUNT-ADD
              WHEN 'ACCTINQ '
                 PERFORM 2100-ACCOUNT-INQUIRE
              WHEN 'ACCTFND '
                 PERFORM 2200-ACCOUNT-FIND
              WHEN 'ACCTUPD '
                 PERFORM 2300-ACCOUNT-UPDATE
              WHEN 'ACCTCLS '
                 PERFORM 2400-ACCOUNT-CLOSE
              WHEN 'ACCTBAL '
                 PERFORM 2500-ACCOUNT-BALANCE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILACCT0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          WS-ERR-KEY
           PERFORM 1100-GET-BUS-DATE.

      ******************************************************************
       1100-GET-BUS-DATE.
           IF CA-BUS-DATE NOT = SPACES
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM BILDB.BIL_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE 'SYSTEM'         TO WS-ERR-KEY
                 MOVE '1100-GET-BUS-DATE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * ACCTADD - validate everything, then account, payer, stream
      ******************************************************************
       2000-ACCOUNT-ADD.
           PERFORM 3100-VALIDATE-ACCOUNT-TYPE
           PERFORM 3110-VALIDATE-BILLING-LEVEL
           IF CA-AC-INVOICE-DAY = 0
              MOVE 1                   TO CA-AC-INVOICE-DAY
           END-IF
           PERFORM 3150-VALIDATE-INVOICE-DAY
           MOVE CA-AC-DELINQ-PLAN      TO DPL-PLAN-CODE
           PERFORM 3250-VALIDATE-DELINQ-PLAN
      * BR-BIL-ACT-011 Autopay needs an instrument, which cannot exist
      * before the account does
           IF CA-AC-AUTOPAY-FLAG = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUTOPNEW'          TO CA-REASON-CODE
              MOVE 'Autopay can only be set once an instrument is added'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3300-VALIDATE-CONTACT
           MOVE CA-AC-EMAIL            TO WS-EFFECTIVE-EMAIL
           IF CA-AC-DELIVERY-METHOD = SPACES
              MOVE 'MAIL'              TO CA-AC-DELIVERY-METHOD
           END-IF
           PERFORM 3200-VALIDATE-DELIVERY
      * BR-BIL-ACT-010 Account name defaults from the payer
           IF CA-AC-ACCOUNT-NAME = SPACES
              IF WS-TYPE-PERSONAL
                 STRING CA-AC-FIRST-NAME DELIMITED BY '  '
                        ' '              DELIMITED BY SIZE
                        CA-AC-LAST-NAME  DELIMITED BY '  '
                        INTO CA-AC-ACCOUNT-NAME
              ELSE
                 MOVE CA-AC-COMPANY-NAME TO CA-AC-ACCOUNT-NAME
              END-IF
           END-IF
           PERFORM 3400-CHECK-GENAPP-CUSTOMER

      * BR-BIL-ACT-006 Surrogate key and account number from BILSEQGN
           MOVE 'ACT     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE WS-SEQ-VALUE           TO ACT-ACCOUNT-ID
           MOVE 'ACTNO   '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE WS-SEQ-FORMATTED(1:10) TO ACT-ACCOUNT-NUMBER
           MOVE ACT-ACCOUNT-NUMBER     TO WS-ERR-KEY

           PERFORM 4100-INSERT-ACCOUNT
      * BR-BIL-ACT-009 Primary payer contact and monthly stream
           PERFORM 4150-INSERT-PAYER
           MOVE 'IST     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE WS-SEQ-VALUE           TO IST-STREAM-ID
           PERFORM 4200-INSERT-STREAM

           MOVE 'ADD '                 TO WS-AU-ACTION
           MOVE SPACES                 TO WS-AU-FIELD
                                          WS-AU-BEFORE
           STRING ACT-ACCOUNT-NUMBER ' ' CA-AC-ACCOUNT-TYPE ' '
                  CA-AC-ACCOUNT-NAME DELIMITED BY SIZE
                  INTO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT

           MOVE ACT-ACCOUNT-ID         TO CA-AC-ACCOUNT-ID
           MOVE ACT-ACCOUNT-NUMBER     TO CA-AC-ACCOUNT-NUMBER
           MOVE IST-STREAM-ID          TO CA-AC-STREAM-ID
           MOVE 'ACTV'                 TO CA-AC-STATUS
           MOVE 'NORM'                 TO CA-AC-COLLECTION-STATUS
           MOVE 'N'                    TO CA-AC-AUTOPAY-FLAG
           MOVE 0                      TO CA-AC-AUTOPAY-INSTR-ID
                                          CA-AC-UNAPPLIED-AMT
                                          CA-AC-BILLED-AMT
                                          CA-AC-DUE-AMT
                                          CA-AC-UNBILLED-AMT
                                          CA-AC-PERIOD-COUNT
           MOVE WS-BUS-DATE            TO CA-AC-OPEN-DATE
           MOVE 1                      TO CA-AC-VERSION-NO
           STRING 'Account ' ACT-ACCOUNT-NUMBER ' created'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * ACCTINQ - by ACCOUNT-ID when given, otherwise ACCOUNT-NUMBER
      ******************************************************************
       2100-ACCOUNT-INQUIRE.
           PERFORM 4000-READ-ACCOUNT
           PERFORM 2150-RETURN-ACCOUNT-VIEW.

      ******************************************************************
      * Account, payer, stream and up to 10 periods into the COMMAREA
       2150-RETURN-ACCOUNT-VIEW.
           PERFORM 5100-MOVE-ACCOUNT-TO-CA
           PERFORM 4250-READ-PAYER
           PERFORM 5150-MOVE-PAYER-TO-CA
           PERFORM 4350-READ-ACTIVE-STREAM
           PERFORM 4300-LOAD-PERIODS.

      ******************************************************************
      * ACCTFND - the open account of a GenApp customer, else the most
      * recently created closed one
      ******************************************************************
       2200-ACCOUNT-FIND.
           IF CA-AC-GENAPP-CUST-NUM NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CUSTNUM '          TO CA-REASON-CODE
              MOVE 'GenApp customer number is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-AC-GENAPP-CUST-NUM  TO ACT-GENAPP-CUST-NUM
                                          WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT ACCOUNT_ID
                INTO :ACT-ACCOUNT-ID
                FROM BILDB.BIL_ACCOUNT
               WHERE GENAPP_CUST_NUM = :ACT-GENAPP-CUST-NUM
               ORDER BY CASE ACCOUNT_STATUS WHEN 'CLSD' THEN 1
                                            ELSE 0 END,
                        ACCOUNT_ID DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 STRING 'No billing account for GenApp customer '
                        WS-NUM-DISP DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2200-ACCOUNT-FIND' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE ACT-ACCOUNT-ID         TO CA-AC-ACCOUNT-ID
           PERFORM 4000-READ-ACCOUNT
           PERFORM 2150-RETURN-ACCOUNT-VIEW.

      ******************************************************************
      * ACCTUPD - spaces / zero in a field means "leave unchanged"
      ******************************************************************
       2300-ACCOUNT-UPDATE.
           PERFORM 4000-READ-ACCOUNT
           PERFORM 3500-CHECK-OPEN-AND-VERSION
           PERFORM 4250-READ-PAYER
           PERFORM 5200-SNAPSHOT-BEFORE

           IF CA-AC-ACCOUNT-NAME NOT = SPACES
              MOVE CA-AC-ACCOUNT-NAME  TO WS-TL-SRC
              PERFORM 5900-TRAILING-LENGTH
              MOVE WS-TL-LEN           TO ACT-ACCOUNT-NAME-LEN
              MOVE CA-AC-ACCOUNT-NAME  TO ACT-ACCOUNT-NAME-TEXT
           END-IF
           IF CA-AC-BILLING-LEVEL NOT = SPACES
              PERFORM 3110-VALIDATE-BILLING-LEVEL
              MOVE CA-AC-BILLING-LEVEL TO ACT-BILLING-LEVEL
           END-IF
           MOVE 'N'                    TO WS-ANCHOR-CHANGED
           IF CA-AC-INVOICE-DAY NOT = 0
              PERFORM 3150-VALIDATE-INVOICE-DAY
              IF CA-AC-INVOICE-DAY NOT = ACT-INVOICE-DAY
                 MOVE 'Y'              TO WS-ANCHOR-CHANGED
                 MOVE CA-AC-INVOICE-DAY TO ACT-INVOICE-DAY
              END-IF
           END-IF
           IF CA-AC-DELINQ-PLAN NOT = SPACES
              AND CA-AC-DELINQ-PLAN NOT = ACT-DELINQ-PLAN-CODE
              MOVE CA-AC-DELINQ-PLAN   TO DPL-PLAN-CODE
              PERFORM 3250-VALIDATE-DELINQ-PLAN
              MOVE CA-AC-DELINQ-PLAN   TO ACT-DELINQ-PLAN-CODE
           END-IF

           PERFORM 2350-APPLY-PAYER-CHANGES

           IF CA-AC-DELIVERY-METHOD NOT = SPACES
              MOVE CA-AC-DELIVERY-METHOD TO ACT-DELIVERY-METHOD
           END-IF
           MOVE ACT-DELIVERY-METHOD    TO CA-AC-DELIVERY-METHOD
           MOVE SPACES                 TO WS-EFFECTIVE-EMAIL
           IF ACN-EMAIL-ADDRESS-IND NOT < 0
              MOVE ACN-EMAIL-ADDRESS-TEXT TO WS-EFFECTIVE-EMAIL
           END-IF
           PERFORM 3200-VALIDATE-DELIVERY

           PERFORM 2360-APPLY-AUTOPAY

           PERFORM 4400-UPDATE-ACCOUNT
           IF WS-CONTACT-IS-CHANGED
              PERFORM 4450-UPDATE-PAYER
           END-IF
      * BR-BIL-ACT-014 New invoice day re-anchors the active stream
           IF WS-ANCHOR-IS-CHANGED
              PERFORM 4500-REANCHOR-STREAM
           END-IF
           PERFORM 2390-AUDIT-CHANGES

           PERFORM 4000-READ-ACCOUNT
           PERFORM 2150-RETURN-ACCOUNT-VIEW
           IF CA-RC-OK
              STRING 'Account ' ACT-ACCOUNT-NUMBER ' updated'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * Payer address / e-mail / phone changes (spaces = unchanged)
       2350-APPLY-PAYER-CHANGES.
           MOVE 'N'                    TO WS-CONTACT-CHANGED
           IF CA-AC-ADDR-LINE1 NOT = SPACES
              IF CA-AC-POSTCODE = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'POSTCODE'       TO CA-REASON-CODE
                 MOVE 'Postcode is required with a new address'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              PERFORM 3350-VALIDATE-POSTCODE
              MOVE 'Y'                 TO WS-CONTACT-CHANGED
              MOVE CA-AC-ADDR-LINE1    TO WS-TL-SRC
              PERFORM 5900-TRAILING-LENGTH
              MOVE WS-TL-LEN           TO ACN-ADDRESS-LINE1-LEN
              MOVE CA-AC-ADDR-LINE1    TO ACN-ADDRESS-LINE1-TEXT
              MOVE CA-AC-ADDR-LINE2    TO WS-TL-SRC
              PERFORM 5900-TRAILING-LENGTH
              MOVE WS-TL-LEN           TO ACN-ADDRESS-LINE2-LEN
              MOVE CA-AC-ADDR-LINE2    TO ACN-ADDRESS-LINE2-TEXT
              MOVE 0                   TO ACN-ADDRESS-LINE2-IND
              IF WS-TL-LEN = 0
                 MOVE -1               TO ACN-ADDRESS-LINE2-IND
              END-IF
              MOVE CA-AC-CITY          TO WS-TL-SRC
              PERFORM 5900-TRAILING-LENGTH
              MOVE WS-TL-LEN           TO ACN-CITY-LEN
              MOVE CA-AC-CITY          TO ACN-CITY-TEXT
              MOVE 0                   TO ACN-CITY-IND
              IF WS-TL-LEN = 0
                 MOVE -1               TO ACN-CITY-IND
              END-IF
              MOVE CA-AC-POSTCODE      TO ACN-POSTCODE
           END-IF
           IF CA-AC-EMAIL NOT = SPACES
              PERFORM 3360-VALIDATE-EMAIL
              MOVE 'Y'                 TO WS-CONTACT-CHANGED
              MOVE CA-AC-EMAIL         TO WS-TL-SRC
              PERFORM 5900-TRAILING-LENGTH
              MOVE WS-TL-LEN           TO ACN-EMAIL-ADDRESS-LEN
              MOVE CA-AC-EMAIL         TO ACN-EMAIL-ADDRESS-TEXT
              MOVE 0                   TO ACN-EMAIL-ADDRESS-IND
           END-IF
           IF CA-AC-PHONE NOT = SPACES
              PERFORM 3370-VALIDATE-PHONE
              MOVE 'Y'                 TO WS-CONTACT-CHANGED
              MOVE CA-AC-PHONE         TO ACN-PHONE-NUMBER
              MOVE 0                   TO ACN-PHONE-NUMBER-IND
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-015 Autopay ON needs an active ACH / CARD instrument
      * owned by this account; OFF clears the instrument link
       2360-APPLY-AUTOPAY.
           IF CA-AC-AUTOPAY-FLAG = SPACES
              MOVE ACT-AUTOPAY-FLAG    TO CA-AC-AUTOPAY-FLAG
           END-IF
           MOVE CA-AC-AUTOPAY-FLAG     TO WS-AUTOPAY
           IF NOT WS-AUTOPAY-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUTOPFLG'          TO CA-REASON-CODE
              MOVE 'Autopay flag must be Y or N' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-AUTOPAY-ON
              IF CA-AC-AUTOPAY-INSTR-ID = 0
                 MOVE ACT-AUTOPAY-INSTR-ID TO CA-AC-AUTOPAY-INSTR-ID
              END-IF
              PERFORM 3450-VALIDATE-AUTOPAY-INSTR
              MOVE 'Y'                 TO ACT-AUTOPAY-FLAG
              MOVE CA-AC-AUTOPAY-INSTR-ID TO ACT-AUTOPAY-INSTR-ID
           ELSE
              MOVE 'N'                 TO ACT-AUTOPAY-FLAG
              MOVE 0                   TO ACT-AUTOPAY-INSTR-ID
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-013 One audit row per changed field
       2390-AUDIT-CHANGES.
           MOVE 'UPD '                 TO WS-AU-ACTION
           IF ACT-ACCOUNT-NAME-TEXT NOT = WS-BF-NAME
              MOVE 'ACCOUNT_NAME'      TO WS-AU-FIELD
              MOVE WS-BF-NAME          TO WS-AU-BEFORE
              MOVE ACT-ACCOUNT-NAME-TEXT TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF ACT-INVOICE-DAY NOT = WS-BF-INVOICE-DAY
              MOVE 'INVOICE_DAY'       TO WS-AU-FIELD
              MOVE WS-BF-INVOICE-DAY   TO WS-NUM-DISP
              MOVE WS-NUM-DISP(8:2)    TO WS-AU-BEFORE
              MOVE ACT-INVOICE-DAY     TO WS-NUM-DISP
              MOVE WS-NUM-DISP(8:2)    TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF ACT-DELIVERY-METHOD NOT = WS-BF-DELIVERY
              MOVE 'DELIVERY_METHOD'   TO WS-AU-FIELD
              MOVE WS-BF-DELIVERY      TO WS-AU-BEFORE
              MOVE ACT-DELIVERY-METHOD TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF ACT-DELINQ-PLAN-CODE NOT = WS-BF-PLAN
              MOVE 'DELINQ_PLAN_CODE'  TO WS-AU-FIELD
              MOVE WS-BF-PLAN          TO WS-AU-BEFORE
              MOVE ACT-DELINQ-PLAN-CODE TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF ACT-BILLING-LEVEL NOT = WS-BF-BILL-LEVEL
              MOVE 'BILLING_LEVEL'     TO WS-AU-FIELD
              MOVE WS-BF-BILL-LEVEL    TO WS-AU-BEFORE
              MOVE ACT-BILLING-LEVEL   TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF ACT-AUTOPAY-FLAG NOT = WS-BF-AUTOPAY
              OR ACT-AUTOPAY-INSTR-ID NOT = WS-BF-AUTOPAY-INSTR
              MOVE 'AUTOPAY'           TO WS-AU-FIELD
              MOVE WS-BF-AUTOPAY-INSTR TO WS-NUM-DISP
              STRING WS-BF-AUTOPAY ' INSTR ' WS-NUM-DISP
                     DELIMITED BY SIZE INTO WS-AU-BEFORE
              MOVE ACT-AUTOPAY-INSTR-ID TO WS-NUM-DISP
              STRING ACT-AUTOPAY-FLAG ' INSTR ' WS-NUM-DISP
                     DELIMITED BY SIZE INTO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF WS-CONTACT-IS-CHANGED
              PERFORM 2395-AUDIT-PAYER-CHANGES
           END-IF.

      ******************************************************************
       2395-AUDIT-PAYER-CHANGES.
           IF ACN-ADDRESS-LINE1-TEXT NOT = WS-BF-ADDR1
              MOVE 'PAYER_ADDRESS1'    TO WS-AU-FIELD
              MOVE WS-BF-ADDR1         TO WS-AU-BEFORE
              MOVE ACN-ADDRESS-LINE1-TEXT TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF ACN-POSTCODE NOT = WS-BF-POSTCODE
              MOVE 'PAYER_POSTCODE'    TO WS-AU-FIELD
              MOVE WS-BF-POSTCODE      TO WS-AU-BEFORE
              MOVE ACN-POSTCODE        TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF CA-AC-EMAIL NOT = SPACES
              AND CA-AC-EMAIL NOT = WS-BF-EMAIL
              MOVE 'PAYER_EMAIL'       TO WS-AU-FIELD
              MOVE WS-BF-EMAIL         TO WS-AU-BEFORE
              MOVE CA-AC-EMAIL         TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF CA-AC-PHONE NOT = SPACES
              AND CA-AC-PHONE NOT = WS-BF-PHONE
              MOVE 'PAYER_PHONE'       TO WS-AU-FIELD
              MOVE WS-BF-PHONE         TO WS-AU-BEFORE
              MOVE CA-AC-PHONE         TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF.

      ******************************************************************
      * ACCTCLS - state rules first, then close the account and its
      * dependants in the caller's unit of work
      ******************************************************************
       2400-ACCOUNT-CLOSE.
           PERFORM 4000-READ-ACCOUNT
           PERFORM 3500-CHECK-OPEN-AND-VERSION
      * BR-BIL-ACT-017 Closure preconditions
           PERFORM 4600-COUNT-INFORCE-PERIODS
           IF WS-COUNT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'PPINFORC'          TO CA-REASON-CODE
              MOVE WS-COUNT            TO WS-NUM-DISP
              STRING 'Account has ' WS-NUM-DISP
                     ' in-force policy period(s)'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 5000-RECOMPUTE-BALANCES
           IF WS-UNBILLED NOT = 0 OR WS-BILLED NOT = 0
              OR WS-DUE NOT = 0 OR WS-UNAPPLIED NOT = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'BALNZERO'          TO CA-REASON-CODE
              MOVE 'Account balances must all be zero to close'
                                       TO CA-MESSAGE
              PERFORM 5300-MOVE-BALANCES-TO-CA
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4610-COUNT-OPEN-DELINQUENCY
           IF WS-COUNT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'DLQOPEN '          TO CA-REASON-CODE
              MOVE 'Account has an open delinquency process'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4620-COUNT-PENDING-DISB
           IF WS-COUNT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'DSBPEND '          TO CA-REASON-CODE
              MOVE 'Account has a pending disbursement'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

      * BR-BIL-ACT-018 Closing switches off everything that bills
           PERFORM 4650-CLOSE-ACCOUNT-ROW
           PERFORM 4660-CLOSE-STREAMS
           PERFORM 4670-DEACTIVATE-INSTRUMENTS

           MOVE 'CLS '                 TO WS-AU-ACTION
           MOVE 'ACCOUNT_STATUS'       TO WS-AU-FIELD
           MOVE ACT-ACCOUNT-STATUS     TO WS-AU-BEFORE
           MOVE 'CLSD'                 TO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT
           IF ACT-AUTOPAY-FLAG = 'Y'
              MOVE 'AUTOPAY'           TO WS-AU-FIELD
              MOVE 'Y'                 TO WS-AU-BEFORE
              MOVE 'N'                 TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF

           PERFORM 4000-READ-ACCOUNT
           PERFORM 5100-MOVE-ACCOUNT-TO-CA
           STRING 'Account ' ACT-ACCOUNT-NUMBER ' closed'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * ACCTBAL - recompute, store, return
      ******************************************************************
       2500-ACCOUNT-BALANCE.
           PERFORM 4000-READ-ACCOUNT
           PERFORM 5000-RECOMPUTE-BALANCES
           PERFORM 5100-MOVE-ACCOUNT-TO-CA
           PERFORM 5300-MOVE-BALANCES-TO-CA.

      ******************************************************************
      * BR-BIL-ACT-001 Account type; the payer type follows from it
       3100-VALIDATE-ACCOUNT-TYPE.
           MOVE CA-AC-ACCOUNT-TYPE     TO WS-ACCT-TYPE
           IF NOT WS-TYPE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACTTYPE '          TO CA-REASON-CODE
              MOVE 'Account type must be PERS or COMM' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-TYPE-PERSONAL
              SET WS-CONTACT-PERSON    TO TRUE
           ELSE
              SET WS-CONTACT-COMPANY   TO TRUE
           END-IF
           IF CA-AC-CONTACT-TYPE NOT = SPACE
              AND CA-AC-CONTACT-TYPE NOT = WS-CONTACT-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CNTTYPE '          TO CA-REASON-CODE
              STRING 'Payer type must be ' WS-CONTACT-TYPE
                     ' for a ' WS-ACCT-TYPE ' account'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-CONTACT-TYPE        TO CA-AC-CONTACT-TYPE.

      ******************************************************************
       3110-VALIDATE-BILLING-LEVEL.
           IF CA-AC-BILLING-LEVEL = SPACES
              MOVE 'ACCT'              TO CA-AC-BILLING-LEVEL
           END-IF
           MOVE CA-AC-BILLING-LEVEL    TO WS-BILL-LEVEL
           IF NOT WS-LEVEL-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BILLLVL '          TO CA-REASON-CODE
              MOVE 'Billing level must be ACCT or PLCY' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-002 Invoice day 1..28
       3150-VALIDATE-INVOICE-DAY.
           IF CA-AC-INVOICE-DAY < 1 OR CA-AC-INVOICE-DAY > 28
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INVDAY  '          TO CA-REASON-CODE
              MOVE 'Invoice day must be between 1 and 28'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-016 E-mail delivery needs an address to send to
       3200-VALIDATE-DELIVERY.
           MOVE CA-AC-DELIVERY-METHOD  TO WS-DELIVERY
           IF NOT WS-DELIVERY-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DELIVERY'          TO CA-REASON-CODE
              MOVE 'Delivery method must be MAIL, EMAL or BOTH'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-DELIVERY-EMAIL AND WS-EFFECTIVE-EMAIL = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOEMAIL '          TO CA-REASON-CODE
              MOVE 'E-mail delivery needs a payer e-mail address'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-003 Delinquency plan must exist and be active
       3250-VALIDATE-DELINQ-PLAN.
           IF DPL-PLAN-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DLQPLAN '          TO CA-REASON-CODE
              MOVE 'Delinquency plan is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT ACTIVE_FLAG
                INTO :DPL-ACTIVE-FLAG
                FROM BILDB.BIL_DELINQ_PLAN
               WHERE PLAN_CODE = :DPL-PLAN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF DPL-ACTIVE-FLAG NOT = 'Y'
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'DLQPLINA'    TO CA-REASON-CODE
                    STRING 'Delinquency plan ' DPL-PLAN-CODE
                           ' is not active' DELIMITED BY SIZE
                           INTO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DLQPLNF '       TO CA-REASON-CODE
                 STRING 'Delinquency plan ' DPL-PLAN-CODE
                        ' does not exist' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE DPL-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '3250-VALIDATE-DELINQ-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ACT-004 Payer naming depends on the account type
       3300-VALIDATE-CONTACT.
           IF WS-CONTACT-PERSON
              IF CA-AC-FIRST-NAME = SPACES
                 OR CA-AC-LAST-NAME = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PAYRNAME'       TO CA-REASON-CODE
                 MOVE 'Personal payer needs first and last name'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           ELSE
              IF CA-AC-COMPANY-NAME = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PAYRCOMP'       TO CA-REASON-CODE
                 MOVE 'Commercial payer needs a company name'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           IF CA-AC-ADDR-LINE1 = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ADDRESS '          TO CA-REASON-CODE
              MOVE 'Payer address line 1 is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3350-VALIDATE-POSTCODE
           IF CA-AC-EMAIL NOT = SPACES
              PERFORM 3360-VALIDATE-EMAIL
           END-IF
           IF CA-AC-PHONE NOT = SPACES
              PERFORM 3370-VALIDATE-PHONE
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-005 UK postcode: outward A9 A99 A9A AA9 AA99 AA9A,
      * inward digit + two letters from the Royal Mail inward set
      * (never C I K M O V). Stored upper case with one space.
       3350-VALIDATE-POSTCODE.
           MOVE FUNCTION UPPER-CASE(CA-AC-POSTCODE) TO WS-PC-IN
           MOVE SPACES                 TO WS-PC-COMPACT WS-PC-SHAPE
           MOVE 0                      TO WS-PC-LEN WS-PC-GAPS
           MOVE SPACE                  TO WS-PC-PREV
           PERFORM VARYING WS-PC-K FROM 1 BY 1 UNTIL WS-PC-K > 8
              MOVE WS-PC-IN(WS-PC-K:1) TO WS-PC-CHAR
              IF WS-PC-CHAR NOT = SPACE
                 IF WS-PC-PREV = SPACE AND WS-PC-LEN > 0
                    ADD 1              TO WS-PC-GAPS
                 END-IF
                 ADD 1                 TO WS-PC-LEN
                 MOVE WS-PC-CHAR       TO WS-PC-COMPACT(WS-PC-LEN:1)
                 EVALUATE TRUE
                    WHEN WS-PC-CHAR IS NUMERIC
                       MOVE '9'        TO WS-PC-SHAPE(WS-PC-LEN:1)
                    WHEN WS-PC-CHAR IS PC-LETTER
                       MOVE 'A'        TO WS-PC-SHAPE(WS-PC-LEN:1)
                    WHEN OTHER
                       MOVE '?'        TO WS-PC-SHAPE(WS-PC-LEN:1)
                 END-EVALUATE
              END-IF
              MOVE WS-PC-CHAR          TO WS-PC-PREV
           END-PERFORM
           IF WS-PC-LEN < 5 OR WS-PC-LEN > 7 OR WS-PC-GAPS > 1
              PERFORM 3355-REJECT-POSTCODE
           END-IF
           COMPUTE WS-PC-OUT-LEN = WS-PC-LEN - 3
           MOVE SPACES                 TO WS-PC-OUTWARD
           MOVE WS-PC-SHAPE(1:WS-PC-OUT-LEN) TO WS-PC-OUTWARD
           MOVE WS-PC-COMPACT(WS-PC-OUT-LEN + 1:3) TO WS-PC-INWARD
           IF NOT WS-PC-OUTWARD-OK
              OR WS-PC-SHAPE(WS-PC-OUT-LEN + 1:3) NOT = '9AA'
              OR WS-PC-INWARD(2:2) IS NOT PC-INWARD
              PERFORM 3355-REJECT-POSTCODE
           END-IF
           MOVE SPACES                 TO CA-AC-POSTCODE
           STRING WS-PC-COMPACT(1:WS-PC-OUT-LEN) ' ' WS-PC-INWARD
                  DELIMITED BY SIZE INTO CA-AC-POSTCODE.

      ******************************************************************
       3355-REJECT-POSTCODE.
           SET CA-RC-VALIDATION        TO TRUE
           MOVE 'POSTCODE'             TO CA-REASON-CODE
           STRING 'Postcode ' CA-AC-POSTCODE
                  ' is not a valid UK postcode'
                  DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
      * E-mail: one @ not in first position, a dot after it, no
      * embedded spaces
       3360-VALIDATE-EMAIL.
           MOVE CA-AC-EMAIL            TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE 0                      TO WS-EM-AT-COUNT
                                          WS-EM-AT-POS
                                          WS-EM-DOT-COUNT
                                          WS-EM-SPACES
           INSPECT WS-TL-SRC(1:WS-TL-LEN) TALLYING
                   WS-EM-AT-COUNT FOR ALL '@'
                   WS-EM-SPACES   FOR ALL SPACE
           INSPECT WS-TL-SRC(1:WS-TL-LEN) TALLYING
                   WS-EM-AT-POS FOR CHARACTERS BEFORE INITIAL '@'
           IF WS-EM-AT-COUNT = 1 AND WS-EM-AT-POS > 0
              AND WS-EM-AT-POS + 3 < WS-TL-LEN
              INSPECT WS-TL-SRC(WS-EM-AT-POS + 2:
                                WS-TL-LEN - WS-EM-AT-POS - 2)
                      TALLYING WS-EM-DOT-COUNT FOR ALL '.'
           END-IF
           IF WS-EM-AT-COUNT NOT = 1 OR WS-EM-AT-POS = 0
              OR WS-EM-DOT-COUNT = 0 OR WS-EM-SPACES > 0
              OR WS-TL-SRC(WS-TL-LEN:1) = '.'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EMAIL   '          TO CA-REASON-CODE
              MOVE 'Payer e-mail address is not valid' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3370-VALIDATE-PHONE.
           IF CA-AC-PHONE IS NOT PHONE-CHARS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PHONE   '          TO CA-REASON-CODE
              MOVE 'Phone may hold digits, space, + - ( ) only'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-008 / BR-BIL-ACT-007 GenApp customer must exist and
      * may own only one open billing account
       3400-CHECK-GENAPP-CUSTOMER.
           IF CA-AC-GENAPP-CUST-NUM < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CUSTNUM '          TO CA-REASON-CODE
              MOVE 'GenApp customer number cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-AC-GENAPP-CUST-NUM  TO ACT-GENAPP-CUST-NUM
           IF CA-AC-GENAPP-CUST-NUM > 0
              MOVE CA-AC-GENAPP-CUST-NUM TO GCU-CUSTOMERNUMBER
                                            WS-NUM-DISP
              MOVE WS-NUM-DISP         TO WS-ERR-KEY
              EXEC SQL
                 SELECT CUSTOMERNUMBER
                   INTO :GCU-CUSTOMERNUMBER
                   FROM GENAPP.CUSTOMER
                  WHERE CUSTOMERNUMBER = :GCU-CUSTOMERNUMBER
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    CONTINUE
                 WHEN 100
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'CUSTNOTF'    TO CA-REASON-CODE
                    STRING 'GenApp customer ' WS-NUM-DISP
                           ' does not exist' DELIMITED BY SIZE
                           INTO CA-MESSAGE
                    PERFORM 9000-RETURN
                 WHEN OTHER
                    MOVE '3400-CHECK-GENAPP-CUSTOMER' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
              EXEC SQL
                 SELECT COUNT(*)
                   INTO :WS-COUNT
                   FROM BILDB.BIL_ACCOUNT
                  WHERE GENAPP_CUST_NUM = :ACT-GENAPP-CUST-NUM
                    AND ACCOUNT_STATUS <> 'CLSD'
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '3400-CHECK-GENAPP-CUSTOMER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              IF WS-COUNT > 0
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPCUST '       TO CA-REASON-CODE
                 STRING 'GenApp customer ' WS-NUM-DISP
                        ' already has an open billing account'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-015 The autopay instrument
       3450-VALIDATE-AUTOPAY-INSTR.
           IF CA-AC-AUTOPAY-INSTR-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUTOPINS'          TO CA-REASON-CODE
              MOVE 'Autopay needs a payment instrument id'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-AC-AUTOPAY-INSTR-ID TO PIN-INSTR-ID
           EXEC SQL
              SELECT ACCOUNT_ID, INSTR_TYPE, INSTR_STATUS
                INTO :PIN-ACCOUNT-ID, :PIN-INSTR-TYPE,
                     :PIN-INSTR-STATUS
                FROM BILDB.BIL_PAYMENT_INSTRUMENT
               WHERE INSTR_ID = :PIN-INSTR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'INSNOTFD'       TO CA-REASON-CODE
                 MOVE 'Autopay instrument does not exist'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3450-VALIDATE-AUTOPAY-INSTR' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           EVALUATE TRUE
              WHEN PIN-ACCOUNT-ID NOT = ACT-ACCOUNT-ID
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'INSOWNER'       TO CA-REASON-CODE
                 MOVE 'Autopay instrument belongs to another account'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN PIN-INSTR-STATUS NOT = 'ACTV'
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'INSINACT'       TO CA-REASON-CODE
                 MOVE 'Autopay instrument is not active'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN PIN-INSTR-TYPE NOT = 'ACH ' AND NOT = 'CARD'
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'INSTYPE '       TO CA-REASON-CODE
                 MOVE 'Autopay needs an ACH or CARD instrument'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ACT-012 Closed accounts are read-only; the caller's
      * VERSION-NO must match the row
       3500-CHECK-OPEN-AND-VERSION.
           IF ACT-ACCOUNT-STATUS = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTCLSD '          TO CA-REASON-CODE
              STRING 'Account ' ACT-ACCOUNT-NUMBER ' is closed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-AC-VERSION-NO NOT = ACT-VERSION-NO
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Account changed by another user - refresh'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE ACT-VERSION-NO         TO WS-EXPECT-VERSION.

      ******************************************************************
      * Read by ACCOUNT-ID, or by ACCOUNT-NUMBER when the id is zero
       4000-READ-ACCOUNT.
           IF CA-AC-ACCOUNT-ID = 0 AND CA-AC-ACCOUNT-NUMBER = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACTKEY  '          TO CA-REASON-CODE
              MOVE 'Account id or account number is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE DCLBIL-ACCOUNT
           MOVE CA-AC-ACCOUNT-ID       TO ACT-ACCOUNT-ID
           MOVE CA-AC-ACCOUNT-NUMBER   TO ACT-ACCOUNT-NUMBER
           IF CA-AC-ACCOUNT-ID NOT = 0
              MOVE CA-AC-ACCOUNT-ID    TO WS-NUM-DISP
              MOVE WS-NUM-DISP         TO WS-ERR-KEY
           ELSE
              MOVE CA-AC-ACCOUNT-NUMBER TO WS-ERR-KEY
              PERFORM 4010-RESOLVE-ACCOUNT-NUMBER
           END-IF
           EXEC SQL
              SELECT ACCOUNT_ID, ACCOUNT_NUMBER, GENAPP_CUST_NUM,
                     ACCOUNT_NAME, ACCOUNT_TYPE, BILLING_LEVEL,
                     INVOICE_DAY, DELIVERY_METHOD, DELINQ_PLAN_CODE,
                     AUTOPAY_FLAG, AUTOPAY_INSTR_ID, ACCOUNT_STATUS,
                     COLLECTION_STATUS, UNAPPLIED_AMT, BILLED_AMT,
                     DUE_AMT, UNBILLED_AMT, OPEN_DATE, CLOSE_DATE,
                     VERSION_NO
                INTO :ACT-ACCOUNT-ID, :ACT-ACCOUNT-NUMBER,
                     :ACT-GENAPP-CUST-NUM, :ACT-ACCOUNT-NAME,
                     :ACT-ACCOUNT-TYPE, :ACT-BILLING-LEVEL,
                     :ACT-INVOICE-DAY, :ACT-DELIVERY-METHOD,
                     :ACT-DELINQ-PLAN-CODE, :ACT-AUTOPAY-FLAG,
                     :ACT-AUTOPAY-INSTR-ID, :ACT-ACCOUNT-STATUS,
                     :ACT-COLLECTION-STATUS, :ACT-UNAPPLIED-AMT,
                     :ACT-BILLED-AMT, :ACT-DUE-AMT,
                     :ACT-UNBILLED-AMT, :ACT-OPEN-DATE,
                     :ACT-CLOSE-DATE :ACT-CLOSE-DATE-IND,
                     :ACT-VERSION-NO
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 PERFORM 4090-ACCOUNT-NOT-FOUND
              WHEN OTHER
                 MOVE '4000-READ-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE ACT-ACCOUNT-NUMBER     TO WS-ERR-KEY.

      ******************************************************************
      * Account number -> id through unique index XACT01
       4010-RESOLVE-ACCOUNT-NUMBER.
           EXEC SQL
              SELECT ACCOUNT_ID
                INTO :ACT-ACCOUNT-ID
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_NUMBER = :ACT-ACCOUNT-NUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 PERFORM 4090-ACCOUNT-NOT-FOUND
              WHEN OTHER
                 MOVE '4010-RESOLVE-ACCOUNT-NUMBER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4090-ACCOUNT-NOT-FOUND.
           SET CA-RC-NOT-FOUND         TO TRUE
           MOVE 'ACTNOTFD'             TO CA-REASON-CODE
           STRING 'Billing account ' WS-ERR-KEY ' not found'
                  DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       4100-INSERT-ACCOUNT.
           MOVE CA-AC-ACCOUNT-NAME     TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACT-ACCOUNT-NAME-LEN
           MOVE CA-AC-ACCOUNT-NAME     TO ACT-ACCOUNT-NAME-TEXT
           MOVE CA-AC-ACCOUNT-TYPE     TO ACT-ACCOUNT-TYPE
           MOVE CA-AC-BILLING-LEVEL    TO ACT-BILLING-LEVEL
           MOVE CA-AC-INVOICE-DAY      TO ACT-INVOICE-DAY
           MOVE CA-AC-DELIVERY-METHOD  TO ACT-DELIVERY-METHOD
           MOVE CA-AC-DELINQ-PLAN      TO ACT-DELINQ-PLAN-CODE
           MOVE WS-BUS-DATE            TO ACT-OPEN-DATE
           MOVE CA-USER-ID             TO ACT-CREATE-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_ACCOUNT
                     (ACCOUNT_ID, ACCOUNT_NUMBER, GENAPP_CUST_NUM,
                      ACCOUNT_NAME, ACCOUNT_TYPE, BILLING_LEVEL,
                      INVOICE_DAY, DELIVERY_METHOD, DELINQ_PLAN_CODE,
                      AUTOPAY_FLAG, AUTOPAY_INSTR_ID, ACCOUNT_STATUS,
                      COLLECTION_STATUS, UNAPPLIED_AMT, BILLED_AMT,
                      DUE_AMT, UNBILLED_AMT, CURRENCY_CODE,
                      OPEN_DATE, CLOSE_DATE, CREATE_USER, CREATE_TS,
                      UPDATE_TS, VERSION_NO)
              VALUES (:ACT-ACCOUNT-ID, :ACT-ACCOUNT-NUMBER,
                      :ACT-GENAPP-CUST-NUM, :ACT-ACCOUNT-NAME,
                      :ACT-ACCOUNT-TYPE, :ACT-BILLING-LEVEL,
                      :ACT-INVOICE-DAY, :ACT-DELIVERY-METHOD,
                      :ACT-DELINQ-PLAN-CODE, 'N', 0, 'ACTV',
                      'NORM', 0, 0, 0, 0, 'GBP',
                      :ACT-OPEN-DATE, NULL, :ACT-CREATE-USER,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP, 1)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPACTNO'       TO CA-REASON-CODE
                 STRING 'Account number ' ACT-ACCOUNT-NUMBER
                        ' already exists - check sequence ACTNO'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-INSERT-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Primary payer: contact 1, role PAYR, nullable columns set from
      * the COMMAREA (spaces -> NULL)
       4150-INSERT-PAYER.
           INITIALIZE DCLBIL-ACCOUNT-CONTACT
           MOVE ACT-ACCOUNT-ID         TO ACN-ACCOUNT-ID
           MOVE 1                      TO ACN-CONTACT-SEQ
           MOVE WS-PAYER-ROLE          TO ACN-CONTACT-ROLE
           MOVE CA-AC-CONTACT-TYPE     TO ACN-CONTACT-TYPE
           MOVE 'Y'                    TO ACN-PRIMARY-FLAG
           MOVE 'GB'                   TO ACN-COUNTRY-CODE
           MOVE CA-AC-POSTCODE         TO ACN-POSTCODE

           MOVE CA-AC-FIRST-NAME       TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-FIRST-NAME-LEN
           MOVE CA-AC-FIRST-NAME       TO ACN-FIRST-NAME-TEXT
           MOVE 0                      TO ACN-FIRST-NAME-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO ACN-FIRST-NAME-IND
           END-IF

           MOVE CA-AC-LAST-NAME        TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-LAST-NAME-LEN
           MOVE CA-AC-LAST-NAME        TO ACN-LAST-NAME-TEXT
           MOVE 0                      TO ACN-LAST-NAME-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO ACN-LAST-NAME-IND
           END-IF

           MOVE CA-AC-COMPANY-NAME     TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-COMPANY-NAME-LEN
           MOVE CA-AC-COMPANY-NAME     TO ACN-COMPANY-NAME-TEXT
           MOVE 0                      TO ACN-COMPANY-NAME-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO ACN-COMPANY-NAME-IND
           END-IF

           MOVE CA-AC-ADDR-LINE1       TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-ADDRESS-LINE1-LEN
           MOVE CA-AC-ADDR-LINE1       TO ACN-ADDRESS-LINE1-TEXT

           MOVE CA-AC-ADDR-LINE2       TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-ADDRESS-LINE2-LEN
           MOVE CA-AC-ADDR-LINE2       TO ACN-ADDRESS-LINE2-TEXT
           MOVE 0                      TO ACN-ADDRESS-LINE2-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO ACN-ADDRESS-LINE2-IND
           END-IF

           MOVE CA-AC-CITY             TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-CITY-LEN
           MOVE CA-AC-CITY             TO ACN-CITY-TEXT
           MOVE 0                      TO ACN-CITY-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO ACN-CITY-IND
           END-IF

           MOVE CA-AC-EMAIL            TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO ACN-EMAIL-ADDRESS-LEN
           MOVE CA-AC-EMAIL            TO ACN-EMAIL-ADDRESS-TEXT
           MOVE 0                      TO ACN-EMAIL-ADDRESS-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO ACN-EMAIL-ADDRESS-IND
           END-IF

           MOVE CA-AC-PHONE            TO ACN-PHONE-NUMBER
           MOVE 0                      TO ACN-PHONE-NUMBER-IND
           IF CA-AC-PHONE = SPACES
              MOVE -1                  TO ACN-PHONE-NUMBER-IND
           END-IF

           EXEC SQL
              INSERT INTO BILDB.BIL_ACCOUNT_CONTACT
                     (ACCOUNT_ID, CONTACT_SEQ, CONTACT_ROLE,
                      CONTACT_TYPE, FIRST_NAME, LAST_NAME,
                      COMPANY_NAME, ADDRESS_LINE1, ADDRESS_LINE2,
                      CITY, POSTCODE, COUNTRY_CODE, EMAIL_ADDRESS,
                      PHONE_NUMBER, PRIMARY_FLAG, UPDATE_TS)
              VALUES (:ACN-ACCOUNT-ID, :ACN-CONTACT-SEQ,
                      :ACN-CONTACT-ROLE, :ACN-CONTACT-TYPE,
                      :ACN-FIRST-NAME :ACN-FIRST-NAME-IND,
                      :ACN-LAST-NAME :ACN-LAST-NAME-IND,
                      :ACN-COMPANY-NAME :ACN-COMPANY-NAME-IND,
                      :ACN-ADDRESS-LINE1,
                      :ACN-ADDRESS-LINE2 :ACN-ADDRESS-LINE2-IND,
                      :ACN-CITY :ACN-CITY-IND,
                      :ACN-POSTCODE, :ACN-COUNTRY-CODE,
                      :ACN-EMAIL-ADDRESS :ACN-EMAIL-ADDRESS-IND,
                      :ACN-PHONE-NUMBER :ACN-PHONE-NUMBER-IND,
                      :ACN-PRIMARY-FLAG, CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4150-INSERT-PAYER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4200-INSERT-STREAM.
           MOVE ACT-ACCOUNT-ID         TO IST-ACCOUNT-ID
           MOVE WS-MONTHLY             TO IST-PERIODICITY
           MOVE ACT-INVOICE-DAY        TO IST-ANCHOR-DAY
           MOVE 'ACTV'                 TO IST-STREAM-STATUS
           EXEC SQL
              INSERT INTO BILDB.BIL_INVOICE_STREAM
                     (STREAM_ID, ACCOUNT_ID, PERIODICITY,
                      ANCHOR_DAY, STREAM_STATUS)
              VALUES (:IST-STREAM-ID, :IST-ACCOUNT-ID,
                      :IST-PERIODICITY, :IST-ANCHOR-DAY,
                      :IST-STREAM-STATUS)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-INSERT-STREAM' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Primary payer; an account without one is returned with a
      * blank contact (data repair case), not as an error
       4250-READ-PAYER.
           INITIALIZE DCLBIL-ACCOUNT-CONTACT
           MOVE -1                     TO ACN-FIRST-NAME-IND
                                          ACN-LAST-NAME-IND
                                          ACN-COMPANY-NAME-IND
                                          ACN-ADDRESS-LINE2-IND
                                          ACN-CITY-IND
                                          ACN-EMAIL-ADDRESS-IND
                                          ACN-PHONE-NUMBER-IND
           MOVE ACT-ACCOUNT-ID         TO ACN-ACCOUNT-ID
           MOVE WS-PAYER-ROLE          TO ACN-CONTACT-ROLE
           EXEC SQL
              SELECT CONTACT_SEQ, CONTACT_TYPE, FIRST_NAME,
                     LAST_NAME, COMPANY_NAME, ADDRESS_LINE1,
                     ADDRESS_LINE2, CITY, POSTCODE, EMAIL_ADDRESS,
                     PHONE_NUMBER
                INTO :ACN-CONTACT-SEQ, :ACN-CONTACT-TYPE,
                     :ACN-FIRST-NAME :ACN-FIRST-NAME-IND,
                     :ACN-LAST-NAME :ACN-LAST-NAME-IND,
                     :ACN-COMPANY-NAME :ACN-COMPANY-NAME-IND,
                     :ACN-ADDRESS-LINE1,
                     :ACN-ADDRESS-LINE2 :ACN-ADDRESS-LINE2-IND,
                     :ACN-CITY :ACN-CITY-IND,
                     :ACN-POSTCODE,
                     :ACN-EMAIL-ADDRESS :ACN-EMAIL-ADDRESS-IND,
                     :ACN-PHONE-NUMBER :ACN-PHONE-NUMBER-IND
                FROM BILDB.BIL_ACCOUNT_CONTACT
               WHERE ACCOUNT_ID   = :ACN-ACCOUNT-ID
                 AND CONTACT_ROLE = :ACN-CONTACT-ROLE
                 AND PRIMARY_FLAG = 'Y'
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO ACN-CONTACT-SEQ
              WHEN OTHER
                 MOVE '4250-READ-PAYER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Up to 10 periods, latest expiry first; more -> warning RC 04
       4300-LOAD-PERIODS.
           MOVE 0                      TO CA-AC-PERIOD-COUNT
           EXEC SQL OPEN PERIOD-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-LOAD-PERIODS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4310-FETCH-PERIOD
           PERFORM UNTIL SQLCODE = 100
                      OR CA-AC-PERIOD-COUNT = WS-MAX-PERIODS
              ADD 1                    TO CA-AC-PERIOD-COUNT
              MOVE CA-AC-PERIOD-COUNT  TO WS-IX
              MOVE PPR-PP-ID           TO CA-AC-PP-ID(WS-IX)
              MOVE PPR-GENAPP-POL-NUM  TO CA-AC-PP-POL-NUM(WS-IX)
              MOVE PPR-PRODUCT-CODE    TO CA-AC-PP-PRODUCT(WS-IX)
              MOVE PPR-PP-STATUS       TO CA-AC-PP-STATUS(WS-IX)
              MOVE PPR-EXP-DATE        TO CA-AC-PP-EXP-DATE(WS-IX)
              PERFORM 4310-FETCH-PERIOD
           END-PERFORM
           IF CA-AC-PERIOD-COUNT = WS-MAX-PERIODS
              PERFORM 4310-FETCH-PERIOD
              IF SQLCODE = 0
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'MOREPPS '       TO CA-REASON-CODE
                 MOVE 'More than 10 policy periods - latest shown'
                                       TO CA-MESSAGE
              END-IF
           END-IF
           EXEC SQL CLOSE PERIOD-CSR END-EXEC.

      ******************************************************************
       4310-FETCH-PERIOD.
           EXEC SQL
              FETCH PERIOD-CSR
               INTO :PPR-PP-ID, :PPR-GENAPP-POL-NUM,
                    :PPR-PRODUCT-CODE, :PPR-PP-STATUS,
                    :PPR-EXP-DATE
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE SQLCODE             TO WS-SAVE-SQLCODE
              EXEC SQL CLOSE PERIOD-CSR END-EXEC
              MOVE WS-SAVE-SQLCODE     TO SQLCODE
              MOVE '4310-FETCH-PERIOD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4350-READ-ACTIVE-STREAM.
           MOVE 0                      TO CA-AC-STREAM-ID
           MOVE ACT-ACCOUNT-ID         TO IST-ACCOUNT-ID
           EXEC SQL
              SELECT STREAM_ID
                INTO :IST-STREAM-ID
                FROM BILDB.BIL_INVOICE_STREAM
               WHERE ACCOUNT_ID    = :IST-ACCOUNT-ID
                 AND STREAM_STATUS = 'ACTV'
               ORDER BY STREAM_ID
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE IST-STREAM-ID    TO CA-AC-STREAM-ID
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4350-READ-ACTIVE-STREAM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ACT-012 Optimistic lock: zero rows updated = stale
       4400-UPDATE-ACCOUNT.
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET ACCOUNT_NAME     = :ACT-ACCOUNT-NAME,
                     BILLING_LEVEL    = :ACT-BILLING-LEVEL,
                     INVOICE_DAY      = :ACT-INVOICE-DAY,
                     DELIVERY_METHOD  = :ACT-DELIVERY-METHOD,
                     DELINQ_PLAN_CODE = :ACT-DELINQ-PLAN-CODE,
                     AUTOPAY_FLAG     = :ACT-AUTOPAY-FLAG,
                     AUTOPAY_INSTR_ID = :ACT-AUTOPAY-INSTR-ID,
                     UPDATE_TS        = CURRENT TIMESTAMP,
                     VERSION_NO       = VERSION_NO + 1
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
                 AND VERSION_NO = :WS-EXPECT-VERSION
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-UPDATE-ACCOUNT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              PERFORM 4490-STALE-VERSION
           END-IF.

      ******************************************************************
       4450-UPDATE-PAYER.
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT_CONTACT
                 SET ADDRESS_LINE1 = :ACN-ADDRESS-LINE1,
                     ADDRESS_LINE2 = :ACN-ADDRESS-LINE2
                                     :ACN-ADDRESS-LINE2-IND,
                     CITY          = :ACN-CITY :ACN-CITY-IND,
                     POSTCODE      = :ACN-POSTCODE,
                     EMAIL_ADDRESS = :ACN-EMAIL-ADDRESS
                                     :ACN-EMAIL-ADDRESS-IND,
                     PHONE_NUMBER  = :ACN-PHONE-NUMBER
                                     :ACN-PHONE-NUMBER-IND,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID  = :ACT-ACCOUNT-ID
                 AND CONTACT_SEQ = :ACN-CONTACT-SEQ
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PAYRNOTF'       TO CA-REASON-CODE
                 MOVE 'Account has no primary payer to update'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4450-UPDATE-PAYER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4490-STALE-VERSION.
           SET CA-RC-CONFLICT          TO TRUE
           MOVE 'STALEVER'             TO CA-REASON-CODE
           MOVE 'Account changed by another user - refresh'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
      * BR-BIL-ACT-014 Monthly streams follow the account invoice day
       4500-REANCHOR-STREAM.
           MOVE ACT-ACCOUNT-ID         TO IST-ACCOUNT-ID
           MOVE ACT-INVOICE-DAY        TO IST-ANCHOR-DAY
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_STREAM
                 SET ANCHOR_DAY    = :IST-ANCHOR-DAY
               WHERE ACCOUNT_ID    = :IST-ACCOUNT-ID
                 AND PERIODICITY   = 'M'
                 AND STREAM_STATUS = 'ACTV'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4500-REANCHOR-STREAM' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4600-COUNT-INFORCE-PERIODS.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
                 AND PP_STATUS  = 'INFC'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4600-COUNT-INFORCE-PERIODS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4610-COUNT-OPEN-DELINQUENCY.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_DELINQ_PROCESS
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
                 AND DP_STATUS  = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4610-COUNT-OPEN-DELINQUENCY' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Pending = created or approved but not yet sent or voided
       4620-COUNT-PENDING-DISB.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_DISBURSEMENT
               WHERE ACCOUNT_ID  = :ACT-ACCOUNT-ID
                 AND DISB_STATUS IN ('PEND', 'APPR')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4620-COUNT-PENDING-DISB' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4650-CLOSE-ACCOUNT-ROW.
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET ACCOUNT_STATUS   = 'CLSD',
                     CLOSE_DATE       = :WS-BUS-DATE,
                     AUTOPAY_FLAG     = 'N',
                     AUTOPAY_INSTR_ID = 0,
                     UPDATE_TS        = CURRENT TIMESTAMP,
                     VERSION_NO       = VERSION_NO + 1
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
                 AND VERSION_NO = :WS-EXPECT-VERSION
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4650-CLOSE-ACCOUNT-ROW' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              PERFORM 4490-STALE-VERSION
           END-IF.

      ******************************************************************
       4660-CLOSE-STREAMS.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_STREAM
                 SET STREAM_STATUS = 'CLSD'
               WHERE ACCOUNT_ID    = :ACT-ACCOUNT-ID
                 AND STREAM_STATUS = 'ACTV'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4660-CLOSE-STREAMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4670-DEACTIVATE-INSTRUMENTS.
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_INSTRUMENT
                 SET INSTR_STATUS = 'INAC'
               WHERE ACCOUNT_ID   = :ACT-ACCOUNT-ID
                 AND INSTR_STATUS = 'ACTV'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4670-DEACTIVATE-INSTRUMENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-ACT-019 Balance buckets from open item amounts
      *   open amount = ITEM_AMOUNT - PAID_AMOUNT - WRITEOFF_AMOUNT
      *   UNBILLED  item not yet on an invoice (INVOICE_ID = 0)
      *   BILLED    invoiced, due date after the business date
      *   DUE       invoiced, due on or before the business date
      *   UNAPPLIED cash received for the account not yet distributed
       5000-RECOMPUTE-BALANCES.
           MOVE ACT-UNBILLED-AMT       TO WS-OLD-UNBILLED
           MOVE ACT-BILLED-AMT         TO WS-OLD-BILLED
           MOVE ACT-DUE-AMT            TO WS-OLD-DUE
           MOVE ACT-UNAPPLIED-AMT      TO WS-OLD-UNAPPLIED
           EXEC SQL
              SELECT COALESCE(SUM(CASE WHEN INVOICE_ID = 0
                         THEN ITEM_AMOUNT - PAID_AMOUNT
                              - WRITEOFF_AMOUNT ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN INVOICE_ID <> 0
                                        AND DUE_DATE > :WS-BUS-DATE
                         THEN ITEM_AMOUNT - PAID_AMOUNT
                              - WRITEOFF_AMOUNT ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN INVOICE_ID <> 0
                                        AND DUE_DATE <= :WS-BUS-DATE
                         THEN ITEM_AMOUNT - PAID_AMOUNT
                              - WRITEOFF_AMOUNT ELSE 0 END), 0)
                INTO :WS-UNBILLED, :WS-BILLED, :WS-DUE
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5000-RECOMPUTE-BALANCES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              SELECT COALESCE(SUM(UNAPPLIED_AMOUNT), 0)
                INTO :WS-UNAPPLIED
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5000-RECOMPUTE-BALANCES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
      * BR-BIL-ACT-020 Derived figures: VERSION_NO is not bumped so a
      * user holding the account on screen is not made stale
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET UNBILLED_AMT  = :WS-UNBILLED,
                     BILLED_AMT    = :WS-BILLED,
                     DUE_AMT       = :WS-DUE,
                     UNAPPLIED_AMT = :WS-UNAPPLIED,
                     UPDATE_TS     = CURRENT TIMESTAMP
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5000-RECOMPUTE-BALANCES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-UNBILLED            TO ACT-UNBILLED-AMT
           MOVE WS-BILLED              TO ACT-BILLED-AMT
           MOVE WS-DUE                 TO ACT-DUE-AMT
           MOVE WS-UNAPPLIED           TO ACT-UNAPPLIED-AMT
           PERFORM 5050-AUDIT-BALANCE-DRIFT.

      ******************************************************************
      * BR-BIL-ACT-020 A stored bucket that disagrees with the ledger
      * of items is a reconciliation break and is audited
       5050-AUDIT-BALANCE-DRIFT.
           MOVE 'BAL '                 TO WS-AU-ACTION
           IF WS-UNBILLED NOT = WS-OLD-UNBILLED
              MOVE 'UNBILLED_AMT'      TO WS-AU-FIELD
              MOVE WS-OLD-UNBILLED     TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-BEFORE
              MOVE WS-UNBILLED         TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF WS-BILLED NOT = WS-OLD-BILLED
              MOVE 'BILLED_AMT'        TO WS-AU-FIELD
              MOVE WS-OLD-BILLED       TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-BEFORE
              MOVE WS-BILLED           TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF WS-DUE NOT = WS-OLD-DUE
              MOVE 'DUE_AMT'           TO WS-AU-FIELD
              MOVE WS-OLD-DUE          TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-BEFORE
              MOVE WS-DUE              TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF WS-UNAPPLIED NOT = WS-OLD-UNAPPLIED
              MOVE 'UNAPPLIED_AMT'     TO WS-AU-FIELD
              MOVE WS-OLD-UNAPPLIED    TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-BEFORE
              MOVE WS-UNAPPLIED        TO WS-AMT-DISP
              MOVE WS-AMT-DISP         TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF.

      ******************************************************************
       5100-MOVE-ACCOUNT-TO-CA.
           MOVE ACT-ACCOUNT-ID         TO CA-AC-ACCOUNT-ID
           MOVE ACT-ACCOUNT-NUMBER     TO CA-AC-ACCOUNT-NUMBER
           MOVE ACT-GENAPP-CUST-NUM    TO CA-AC-GENAPP-CUST-NUM
           MOVE SPACES                 TO CA-AC-ACCOUNT-NAME
           IF ACT-ACCOUNT-NAME-LEN > 0
              MOVE ACT-ACCOUNT-NAME-TEXT(1:ACT-ACCOUNT-NAME-LEN)
                                       TO CA-AC-ACCOUNT-NAME
           END-IF
           MOVE ACT-ACCOUNT-TYPE       TO CA-AC-ACCOUNT-TYPE
           MOVE ACT-BILLING-LEVEL      TO CA-AC-BILLING-LEVEL
           MOVE ACT-INVOICE-DAY        TO CA-AC-INVOICE-DAY
           MOVE ACT-DELIVERY-METHOD    TO CA-AC-DELIVERY-METHOD
           MOVE ACT-DELINQ-PLAN-CODE   TO CA-AC-DELINQ-PLAN
           MOVE ACT-AUTOPAY-FLAG       TO CA-AC-AUTOPAY-FLAG
           MOVE ACT-AUTOPAY-INSTR-ID   TO CA-AC-AUTOPAY-INSTR-ID
           MOVE ACT-ACCOUNT-STATUS     TO CA-AC-STATUS
           MOVE ACT-COLLECTION-STATUS  TO CA-AC-COLLECTION-STATUS
           MOVE ACT-UNAPPLIED-AMT      TO CA-AC-UNAPPLIED-AMT
           MOVE ACT-BILLED-AMT         TO CA-AC-BILLED-AMT
           MOVE ACT-DUE-AMT            TO CA-AC-DUE-AMT
           MOVE ACT-UNBILLED-AMT       TO CA-AC-UNBILLED-AMT
           MOVE ACT-OPEN-DATE          TO CA-AC-OPEN-DATE
           MOVE ACT-VERSION-NO         TO CA-AC-VERSION-NO.

      ******************************************************************
       5150-MOVE-PAYER-TO-CA.
           INITIALIZE CA-AC-CONTACT
           IF ACN-CONTACT-SEQ = 0
              EXIT PARAGRAPH
           END-IF
           MOVE ACN-CONTACT-TYPE       TO CA-AC-CONTACT-TYPE
           IF ACN-FIRST-NAME-IND NOT < 0
              MOVE ACN-FIRST-NAME-TEXT TO CA-AC-FIRST-NAME
           END-IF
           IF ACN-LAST-NAME-IND NOT < 0
              MOVE ACN-LAST-NAME-TEXT  TO CA-AC-LAST-NAME
           END-IF
           IF ACN-COMPANY-NAME-IND NOT < 0
              MOVE ACN-COMPANY-NAME-TEXT TO CA-AC-COMPANY-NAME
           END-IF
           MOVE ACN-ADDRESS-LINE1-TEXT TO CA-AC-ADDR-LINE1
           IF ACN-ADDRESS-LINE2-IND NOT < 0
              MOVE ACN-ADDRESS-LINE2-TEXT TO CA-AC-ADDR-LINE2
           END-IF
           IF ACN-CITY-IND NOT < 0
              MOVE ACN-CITY-TEXT       TO CA-AC-CITY
           END-IF
           MOVE ACN-POSTCODE           TO CA-AC-POSTCODE
           IF ACN-EMAIL-ADDRESS-IND NOT < 0
              MOVE ACN-EMAIL-ADDRESS-TEXT TO CA-AC-EMAIL
           END-IF
           IF ACN-PHONE-NUMBER-IND NOT < 0
              MOVE ACN-PHONE-NUMBER    TO CA-AC-PHONE
           END-IF.

      ******************************************************************
       5200-SNAPSHOT-BEFORE.
           MOVE ACT-ACCOUNT-NAME-TEXT  TO WS-BF-NAME
           MOVE ACT-INVOICE-DAY        TO WS-BF-INVOICE-DAY
           MOVE ACT-DELIVERY-METHOD    TO WS-BF-DELIVERY
           MOVE ACT-DELINQ-PLAN-CODE   TO WS-BF-PLAN
           MOVE ACT-AUTOPAY-FLAG       TO WS-BF-AUTOPAY
           MOVE ACT-AUTOPAY-INSTR-ID   TO WS-BF-AUTOPAY-INSTR
           MOVE ACT-BILLING-LEVEL      TO WS-BF-BILL-LEVEL
           MOVE ACN-ADDRESS-LINE1-TEXT TO WS-BF-ADDR1
           MOVE ACN-POSTCODE           TO WS-BF-POSTCODE
           MOVE SPACES                 TO WS-BF-EMAIL WS-BF-PHONE
           IF ACN-EMAIL-ADDRESS-IND NOT < 0
              MOVE ACN-EMAIL-ADDRESS-TEXT TO WS-BF-EMAIL
           END-IF
           IF ACN-PHONE-NUMBER-IND NOT < 0
              MOVE ACN-PHONE-NUMBER    TO WS-BF-PHONE
           END-IF.

      ******************************************************************
       5300-MOVE-BALANCES-TO-CA.
           MOVE WS-UNBILLED            TO CA-AC-UNBILLED-AMT
           MOVE WS-BILLED              TO CA-AC-BILLED-AMT
           MOVE WS-DUE                 TO CA-AC-DUE-AMT
           MOVE WS-UNAPPLIED           TO CA-AC-UNAPPLIED-AMT.

      ******************************************************************
      * Length of WS-TL-SRC without trailing spaces (0 when blank)
       5900-TRAILING-LENGTH.
           MOVE 100                    TO WS-TL-LEN
           PERFORM UNTIL WS-TL-LEN = 0
                      OR WS-TL-SRC(WS-TL-LEN:1) NOT = SPACE
              SUBTRACT 1               FROM WS-TL-LEN
           END-PERFORM.

      ******************************************************************
      * Next value of WS-SEQ-NAME; anything but OK / WARNING is passed
      * back to our caller unchanged
       8000-NEXT-SEQUENCE.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE WS-SEQ-NAME            TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF NOT SQA-RC-OK AND NOT SQA-RC-WARNING
              MOVE SQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQA-REASON-CODE     TO CA-REASON-CODE
              MOVE SQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SQA-SQ-VALUE           TO WS-SEQ-VALUE
           MOVE SQA-SQ-FORMATTED       TO WS-SEQ-FORMATTED.

      ******************************************************************
      * Audit row for the current account; failure to audit fails the
      * request so no unaudited change is ever committed
       8100-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE WS-AUD-ENTITY          TO AUA-AU-ENTITY
           MOVE ACT-ACCOUNT-NUMBER     TO AUA-AU-KEY
           MOVE WS-AU-ACTION           TO AUA-AU-ACTION
           MOVE WS-AU-FIELD            TO AUA-AU-FIELD
           MOVE WS-AU-BEFORE           TO AUA-AU-BEFORE
           MOVE WS-AU-AFTER            TO AUA-AU-AFTER
           MOVE WS-PROGRAM-ID          TO AUA-AU-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF NOT AUA-RC-OK AND NOT AUA-RC-WARNING
              MOVE AUA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AUA-REASON-CODE     TO CA-REASON-CODE
              MOVE AUA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO WS-AU-BEFORE WS-AU-AFTER.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
      * -911/-913 are reported as DEADLOCK for the caller to retry;
      * anything else is logged through BILERRLG
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Account data locked - retry' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           MOVE SPACES                 TO CA-MESSAGE
           STRING 'BILACCT0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
