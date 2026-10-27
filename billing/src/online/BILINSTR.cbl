       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILINSTR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILINSTR                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Payment instrument service. Instruments are      *
      *               tokenised: only the vault token, last four       *
      *               digits, expiry and sort code are held. Tracks    *
      *               collection failures and withdraws an instrument  *
      *               (and account autopay) after repeated failures.   *
      * REQUESTS    : INSADD   add instrument to an open account       *
      *               INSINQ   inquire by INSTR-ID                     *
      *               INSDEA   deactivate                              *
      *               INSFAIL  record a failed collection              *
      * CALLED BY   : BIL account screens, BILPAYRC (returned drafts), *
      *               ACH draft batch                                  *
      * CALLS       : BILSEQGN, BILAUDIT, BILDATES, BILACCT0,          *
      *               BILERRLG                                         *
      * TABLES      : BIL_PAYMENT_INSTRUMENT (C R U)                   *
      *               BIL_ACCOUNT            (R)                       *
      *               BIL_TROUBLE_TICKET     (C)                       *
      *               BIL_CONTROL            (R)                       *
      * RULES       : BR-BIL-PIN-001 Type is ACH, CARD, CHEQ or CASH   *
      *               BR-BIL-PIN-002 ACH and CARD need a vault token;  *
      *                              CHEQ and CASH carry no token,     *
      *                              card or bank details              *
      *               BR-BIL-PIN-003 Raw card / bank numbers are never *
      *                              accepted: an all-digit or short   *
      *                              token is rejected                 *
      *               BR-BIL-PIN-004 CARD needs last four and an       *
      *                              expiry MMYY not before the        *
      *                              business month                    *
      *               BR-BIL-PIN-005 ACH needs a 6-digit sort code and *
      *                              the account's last four digits    *
      *               BR-BIL-PIN-006 ACH and CARD need a holder name   *
      *               BR-BIL-PIN-007 Instruments are added only to an  *
      *                              open account                      *
      *               BR-BIL-PIN-008 A token is active at most once    *
      *                              per account (RC 16)               *
      *               BR-BIL-PIN-009 Only an active instrument can be  *
      *                              deactivated or fail (RC 24)       *
      *               BR-BIL-PIN-010 Withdrawing the autopay           *
      *                              instrument turns autopay off      *
      *                              through BILACCT0 (audited there)  *
      *               BR-BIL-PIN-011 Third failure deactivates the     *
      *                              instrument (RC 04 INSDEACT)       *
      *               BR-BIL-PIN-012 Third failure raises a priority 2 *
      *                              trouble ticket due in two         *
      *                              business days                     *
      *               BR-BIL-PIN-013 Every instrument change audited   *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILINSTR'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-MAX-FAILURES          PIC S9(4) COMP VALUE 3.
           05 WS-MIN-TOKEN-LEN         PIC S9(4) COMP VALUE 16.
           05 WS-TICKET-DAYS           PIC S9(5) COMP-3 VALUE 2.
           05 WS-AUD-ENTITY            PIC X(8)  VALUE 'PAYINSTR'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-BUS-DATE-R REDEFINES WS-BUS-DATE.
              10 WS-BUS-YYYY           PIC 9(4).
              10 FILLER                PIC X.
              10 WS-BUS-MM             PIC 9(2).
              10 FILLER                PIC X(3).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-COUNT                 PIC S9(9) COMP.
           05 WS-NUM-DISP              PIC 9(9).
           05 WS-OLD-FAIL-COUNT        PIC S9(4) COMP.
           05 WS-FAIL-DISP             PIC 9(2).
           05 WS-TOKEN-LEN             PIC S9(4) COMP.
           05 WS-TOKEN-DIGITS          PIC S9(4) COMP.
           05 WS-INSTR-TYPE            PIC X(4).
              88 WS-TYPE-ACH           VALUE 'ACH '.
              88 WS-TYPE-CARD          VALUE 'CARD'.
              88 WS-TYPE-ELECTRONIC    VALUE 'ACH ' 'CARD'.
              88 WS-TYPE-VALID         VALUE 'ACH ' 'CARD' 'CHEQ'
                                             'CASH'.
           05 WS-EXPIRY.
              10 WS-EXP-MM             PIC 9(2).
              10 WS-EXP-YY             PIC 9(2).
           05 WS-EXP-YYYYMM            PIC 9(6).
           05 WS-BUS-YYYYMM            PIC 9(6).
           05 WS-DEACTIVATED           PIC X(1).
              88 WS-WAS-DEACTIVATED    VALUE 'Y'.
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-SEQ-VALUE             PIC S9(9) COMP.
           05 WS-ACT-REQUEST           PIC X(8).
           05 WS-ACT-VERSION           PIC S9(9) COMP.

       01  WS-TRIM.
           05 WS-TL-SRC                PIC X(100).
           05 WS-TL-LEN                PIC S9(4) COMP.

       01  WS-AUDIT-REQ.
           05 WS-AU-ACTION             PIC X(4).
           05 WS-AU-FIELD              PIC X(18).
           05 WS-AU-BEFORE             PIC X(100).
           05 WS-AU-AFTER              PIC X(100).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPIN END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDTTK END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCPIN.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'INSADD  '
                 PERFORM 2000-INSTRUMENT-ADD
              WHEN 'INSINQ  '
                 PERFORM 2100-INSTRUMENT-INQUIRE
              WHEN 'INSDEA  '
                 PERFORM 2200-INSTRUMENT-DEACTIVATE
              WHEN 'INSFAIL '
                 PERFORM 2300-INSTRUMENT-FAILURE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILINSTR: unknown request '
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
           MOVE 'N'                    TO WS-DEACTIVATED
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
                 MOVE '1000-INITIALISE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * INSADD
      ******************************************************************
       2000-INSTRUMENT-ADD.
           PERFORM 3000-VALIDATE-ACCOUNT
           MOVE CA-PI-TYPE             TO WS-INSTR-TYPE
      * BR-BIL-PIN-001 Instrument type
           IF NOT WS-TYPE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INSTYPE '          TO CA-REASON-CODE
              MOVE 'Instrument type must be ACH, CARD, CHEQ or CASH'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-TYPE-ELECTRONIC
              PERFORM 3100-VALIDATE-TOKEN
              PERFORM 3150-VALIDATE-LAST-FOUR
      * BR-BIL-PIN-006 Holder name on electronic instruments
              IF CA-PI-HOLDER-NAME = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'HOLDER  '       TO CA-REASON-CODE
                 MOVE 'Account / card holder name is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           ELSE
      * BR-BIL-PIN-002 Paper instruments hold no electronic details
              IF CA-PI-TOKEN-REF NOT = SPACES
                 OR CA-PI-LAST-FOUR NOT = SPACES
                 OR CA-PI-EXPIRY-MMYY NOT = SPACES
                 OR CA-PI-SORT-CODE NOT = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PAPERDTL'       TO CA-REASON-CODE
                 STRING CA-PI-TYPE ' instrument cannot carry token,'
                        ' card or bank details' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           EVALUATE TRUE
              WHEN WS-TYPE-CARD
                 PERFORM 3200-VALIDATE-CARD-EXPIRY
                 IF CA-PI-SORT-CODE NOT = SPACES
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'SORTCODE'    TO CA-REASON-CODE
                    MOVE 'Sort code is not valid on a card'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN WS-TYPE-ACH
      * BR-BIL-PIN-005 UK bank instrument needs a 6-digit sort code
                 IF CA-PI-SORT-CODE IS NOT NUMERIC
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'SORTCODE'    TO CA-REASON-CODE
                    MOVE 'ACH instrument needs a 6-digit sort code'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
                 IF CA-PI-EXPIRY-MMYY NOT = SPACES
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'EXPIRY  '    TO CA-REASON-CODE
                    MOVE 'Expiry is not valid on a bank instrument'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
           END-EVALUATE
           IF WS-TYPE-ELECTRONIC
              PERFORM 3300-CHECK-DUPLICATE-TOKEN
           END-IF

           MOVE 'PIN     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE WS-SEQ-VALUE           TO PIN-INSTR-ID
           PERFORM 4100-INSERT-INSTRUMENT

           MOVE 'ADD '                 TO WS-AU-ACTION
           MOVE SPACES                 TO WS-AU-FIELD WS-AU-BEFORE
           STRING CA-PI-TYPE ' ****' CA-PI-LAST-FOUR ' '
                  CA-PI-EXPIRY-MMYY ' ' CA-PI-SORT-CODE
                  DELIMITED BY SIZE INTO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT

           MOVE PIN-INSTR-ID           TO CA-PI-INSTR-ID
           MOVE 'ACTV'                 TO CA-PI-STATUS
           MOVE 0                      TO CA-PI-FAIL-COUNT
           MOVE PIN-INSTR-ID           TO WS-NUM-DISP
           STRING 'Instrument ' WS-NUM-DISP ' added'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * INSINQ
      ******************************************************************
       2100-INSTRUMENT-INQUIRE.
           PERFORM 4000-READ-INSTRUMENT
           PERFORM 5100-MOVE-INSTRUMENT-TO-CA.

      ******************************************************************
      * INSDEA
      ******************************************************************
       2200-INSTRUMENT-DEACTIVATE.
           PERFORM 4000-READ-INSTRUMENT
           PERFORM 3400-CHECK-ACTIVE
           MOVE 'INAC'                 TO PIN-INSTR-STATUS
           MOVE PIN-FAIL-COUNT         TO WS-OLD-FAIL-COUNT
           PERFORM 4200-UPDATE-INSTRUMENT
           MOVE 'UPD '                 TO WS-AU-ACTION
           MOVE 'INSTR_STATUS'         TO WS-AU-FIELD
           MOVE 'ACTV'                 TO WS-AU-BEFORE
           MOVE 'INAC'                 TO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT
           PERFORM 5000-RELEASE-AUTOPAY
           PERFORM 5100-MOVE-INSTRUMENT-TO-CA
           MOVE PIN-INSTR-ID           TO WS-NUM-DISP
           STRING 'Instrument ' WS-NUM-DISP ' deactivated'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * INSFAIL - a returned draft / declined card
      ******************************************************************
       2300-INSTRUMENT-FAILURE.
           PERFORM 4000-READ-INSTRUMENT
           PERFORM 3400-CHECK-ACTIVE
           MOVE PIN-FAIL-COUNT         TO WS-OLD-FAIL-COUNT
           ADD 1                       TO PIN-FAIL-COUNT
      * BR-BIL-PIN-011 Third failure withdraws the instrument
           IF PIN-FAIL-COUNT >= WS-MAX-FAILURES
              MOVE 'INAC'              TO PIN-INSTR-STATUS
              MOVE 'Y'                 TO WS-DEACTIVATED
           END-IF
           PERFORM 4200-UPDATE-INSTRUMENT

           MOVE 'UPD '                 TO WS-AU-ACTION
           MOVE 'FAIL_COUNT'           TO WS-AU-FIELD
           MOVE WS-OLD-FAIL-COUNT      TO WS-FAIL-DISP
           MOVE WS-FAIL-DISP           TO WS-AU-BEFORE
           MOVE PIN-FAIL-COUNT         TO WS-FAIL-DISP
           MOVE WS-FAIL-DISP           TO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT

           MOVE PIN-INSTR-ID           TO WS-NUM-DISP
           IF WS-WAS-DEACTIVATED
              MOVE 'INSTR_STATUS'      TO WS-AU-FIELD
              MOVE 'ACTV'              TO WS-AU-BEFORE
              MOVE 'INAC'              TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
              PERFORM 5000-RELEASE-AUTOPAY
              PERFORM 5200-RAISE-TROUBLE-TICKET
              SET CA-RC-WARNING        TO TRUE
              MOVE 'INSDEACT'          TO CA-REASON-CODE
              STRING 'Instrument ' WS-NUM-DISP
                     ' deactivated after 3 failures'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              MOVE PIN-FAIL-COUNT      TO WS-FAIL-DISP
              STRING 'Instrument ' WS-NUM-DISP ' failure '
                     WS-FAIL-DISP ' recorded'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           PERFORM 5100-MOVE-INSTRUMENT-TO-CA.

      ******************************************************************
      * BR-BIL-PIN-007 Owning account must exist and be open
       3000-VALIDATE-ACCOUNT.
           IF CA-PI-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACTKEY  '          TO CA-REASON-CODE
              MOVE 'Account id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PI-ACCOUNT-ID       TO ACT-ACCOUNT-ID WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT ACCOUNT_STATUS
                INTO :ACT-ACCOUNT-STATUS
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 STRING 'Billing account ' WS-NUM-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3000-VALIDATE-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF ACT-ACCOUNT-STATUS = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTCLSD '          TO CA-REASON-CODE
              STRING 'Billing account ' WS-NUM-DISP ' is closed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PIN-002 / BR-BIL-PIN-003 A vault token is required and
      * must not be a raw PAN or bank account number: tokens issued by
      * the vault are at least 16 characters and never all digits
       3100-VALIDATE-TOKEN.
           IF CA-PI-TOKEN-REF = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TOKENREQ'          TO CA-REASON-CODE
              MOVE 'Token reference from the card/bank vault required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PI-TOKEN-REF        TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO WS-TOKEN-LEN
           MOVE 0                      TO WS-TOKEN-DIGITS
           INSPECT CA-PI-TOKEN-REF(1:WS-TOKEN-LEN) TALLYING
                   WS-TOKEN-DIGITS FOR ALL '0' '1' '2' '3' '4'
                                           '5' '6' '7' '8' '9' ' '
           IF WS-TOKEN-DIGITS = WS-TOKEN-LEN
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RAWPAN  '          TO CA-REASON-CODE
              MOVE 'Raw card or bank numbers are not accepted'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-TOKEN-LEN < WS-MIN-TOKEN-LEN
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TOKENFMT'          TO CA-REASON-CODE
              MOVE 'Token reference is not a vault token'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PIN-004 / BR-BIL-PIN-005 Exactly four digits are kept
       3150-VALIDATE-LAST-FOUR.
           IF CA-PI-LAST-FOUR IS NOT NUMERIC
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'LASTFOUR'          TO CA-REASON-CODE
              MOVE 'Last four digits must be 4 numeric digits'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PIN-004 A card is valid to the end of its expiry month
       3200-VALIDATE-CARD-EXPIRY.
           MOVE CA-PI-EXPIRY-MMYY      TO WS-EXPIRY
           IF WS-EXPIRY IS NOT NUMERIC
              OR WS-EXP-MM < 1 OR WS-EXP-MM > 12
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPIRY  '          TO CA-REASON-CODE
              MOVE 'Card expiry must be MMYY with month 01-12'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-EXP-YYYYMM = (2000 + WS-EXP-YY) * 100 + WS-EXP-MM
           COMPUTE WS-BUS-YYYYMM = WS-BUS-YYYY * 100 + WS-BUS-MM
           IF WS-EXP-YYYYMM < WS-BUS-YYYYMM
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CARDEXPD'          TO CA-REASON-CODE
              STRING 'Card expired ' WS-EXP-MM '/' WS-EXP-YY
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PIN-008 Same token active only once on an account
       3300-CHECK-DUPLICATE-TOKEN.
           MOVE CA-PI-ACCOUNT-ID       TO PIN-ACCOUNT-ID
           MOVE CA-PI-TOKEN-REF        TO PIN-TOKEN-REF
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_PAYMENT_INSTRUMENT
               WHERE ACCOUNT_ID   = :PIN-ACCOUNT-ID
                 AND TOKEN_REF    = :PIN-TOKEN-REF
                 AND INSTR_STATUS = 'ACTV'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3300-CHECK-DUPLICATE-TOKEN' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'DUPTOKEN'          TO CA-REASON-CODE
              MOVE 'This instrument is already active on the account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PIN-009 State check for deactivate / failure
       3400-CHECK-ACTIVE.
           IF PIN-INSTR-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'INSINACT'          TO CA-REASON-CODE
              MOVE PIN-INSTR-ID        TO WS-NUM-DISP
              STRING 'Instrument ' WS-NUM-DISP ' is not active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-INSTRUMENT.
           IF CA-PI-INSTR-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'INSKEY  '          TO CA-REASON-CODE
              MOVE 'Instrument id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE DCLBIL-PAYMENT-INSTRUMENT
           MOVE CA-PI-INSTR-ID         TO PIN-INSTR-ID WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT ACCOUNT_ID, INSTR_TYPE, TOKEN_REF, LAST_FOUR,
                     EXPIRY_MMYY, SORT_CODE, HOLDER_NAME,
                     INSTR_STATUS, FAIL_COUNT
                INTO :PIN-ACCOUNT-ID, :PIN-INSTR-TYPE,
                     :PIN-TOKEN-REF, :PIN-LAST-FOUR,
                     :PIN-EXPIRY-MMYY, :PIN-SORT-CODE,
                     :PIN-HOLDER-NAME :PIN-HOLDER-NAME-IND,
                     :PIN-INSTR-STATUS, :PIN-FAIL-COUNT
                FROM BILDB.BIL_PAYMENT_INSTRUMENT
               WHERE INSTR_ID = :PIN-INSTR-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INSNOTFD'       TO CA-REASON-CODE
                 STRING 'Instrument ' WS-NUM-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-INSTRUMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-INSTRUMENT.
           MOVE CA-PI-ACCOUNT-ID       TO PIN-ACCOUNT-ID
           MOVE CA-PI-TYPE             TO PIN-INSTR-TYPE
           MOVE CA-PI-TOKEN-REF        TO PIN-TOKEN-REF
           MOVE CA-PI-LAST-FOUR        TO PIN-LAST-FOUR
           MOVE CA-PI-EXPIRY-MMYY      TO PIN-EXPIRY-MMYY
           MOVE CA-PI-SORT-CODE        TO PIN-SORT-CODE
           MOVE CA-PI-HOLDER-NAME      TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO PIN-HOLDER-NAME-LEN
           MOVE CA-PI-HOLDER-NAME      TO PIN-HOLDER-NAME-TEXT
           MOVE 0                      TO PIN-HOLDER-NAME-IND
           IF WS-TL-LEN = 0
              MOVE -1                  TO PIN-HOLDER-NAME-IND
           END-IF
           EXEC SQL
              INSERT INTO BILDB.BIL_PAYMENT_INSTRUMENT
                     (INSTR_ID, ACCOUNT_ID, INSTR_TYPE, TOKEN_REF,
                      LAST_FOUR, EXPIRY_MMYY, SORT_CODE, HOLDER_NAME,
                      INSTR_STATUS, FAIL_COUNT, CREATE_TS)
              VALUES (:PIN-INSTR-ID, :PIN-ACCOUNT-ID,
                      :PIN-INSTR-TYPE, :PIN-TOKEN-REF,
                      :PIN-LAST-FOUR, :PIN-EXPIRY-MMYY,
                      :PIN-SORT-CODE,
                      :PIN-HOLDER-NAME :PIN-HOLDER-NAME-IND,
                      'ACTV', 0, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE 'Instrument id already used - check sequence PIN'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-INSERT-INSTRUMENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * The fail count read is the concurrency guard: two failures
      * posted at once cannot both be counted from the same base
       4200-UPDATE-INSTRUMENT.
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_INSTRUMENT
                 SET INSTR_STATUS = :PIN-INSTR-STATUS,
                     FAIL_COUNT   = :PIN-FAIL-COUNT
               WHERE INSTR_ID     = :PIN-INSTR-ID
                 AND INSTR_STATUS = 'ACTV'
                 AND FAIL_COUNT   = :WS-OLD-FAIL-COUNT
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-UPDATE-INSTRUMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Instrument changed by another process - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PIN-010 When this instrument funds autopay, autopay is
      * switched off through the account service, which audits it.
      * A closed account already has autopay off.
       5000-RELEASE-AUTOPAY.
           MOVE PIN-ACCOUNT-ID         TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT AUTOPAY_FLAG, AUTOPAY_INSTR_ID, ACCOUNT_STATUS
                INTO :ACT-AUTOPAY-FLAG, :ACT-AUTOPAY-INSTR-ID,
                     :ACT-ACCOUNT-STATUS
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5000-RELEASE-AUTOPAY' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF ACT-AUTOPAY-FLAG NOT = 'Y'
              OR ACT-AUTOPAY-INSTR-ID NOT = PIN-INSTR-ID
              OR ACT-ACCOUNT-STATUS = 'CLSD'
              EXIT PARAGRAPH
           END-IF
           MOVE 'ACCTINQ '             TO WS-ACT-REQUEST
           PERFORM 8200-PREPARE-ACCOUNT-CALL
           CALL WS-ACT-PGM USING WS-ACT-CA
           PERFORM 8250-CHECK-ACCOUNT-CALL
           MOVE ACA-AC-VERSION-NO      TO WS-ACT-VERSION
           MOVE 'ACCTUPD '             TO WS-ACT-REQUEST
           PERFORM 8200-PREPARE-ACCOUNT-CALL
           MOVE WS-ACT-VERSION         TO ACA-AC-VERSION-NO
           MOVE 'N'                    TO ACA-AC-AUTOPAY-FLAG
           CALL WS-ACT-PGM USING WS-ACT-CA
           PERFORM 8250-CHECK-ACCOUNT-CALL.

      ******************************************************************
       5100-MOVE-INSTRUMENT-TO-CA.
           MOVE PIN-INSTR-ID           TO CA-PI-INSTR-ID
           MOVE PIN-ACCOUNT-ID         TO CA-PI-ACCOUNT-ID
           MOVE PIN-INSTR-TYPE         TO CA-PI-TYPE
           MOVE PIN-TOKEN-REF          TO CA-PI-TOKEN-REF
           MOVE PIN-LAST-FOUR          TO CA-PI-LAST-FOUR
           MOVE PIN-EXPIRY-MMYY        TO CA-PI-EXPIRY-MMYY
           MOVE PIN-SORT-CODE          TO CA-PI-SORT-CODE
           MOVE SPACES                 TO CA-PI-HOLDER-NAME
           IF PIN-HOLDER-NAME-IND NOT < 0
              MOVE PIN-HOLDER-NAME-TEXT TO CA-PI-HOLDER-NAME
           END-IF
           MOVE PIN-INSTR-STATUS       TO CA-PI-STATUS
           MOVE PIN-FAIL-COUNT         TO CA-PI-FAIL-COUNT.

      ******************************************************************
      * BR-BIL-PIN-012 Ticket for the collections team, due two
      * business days out
       5200-RAISE-TROUBLE-TICKET.
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           PERFORM 8300-PREPARE-DATE-CALL
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-1
           MOVE WS-TICKET-DAYS         TO DTA-DT-NUMBER
           CALL WS-DTE-PGM USING WS-DTE-CA
           PERFORM 8350-CHECK-DATE-CALL
           MOVE DTA-DT-RESULT-DATE     TO TTK-DUE-DATE
           INITIALIZE WS-DTE-CA
           MOVE 'DTBUSD  '             TO DTA-REQUEST-ID
           PERFORM 8300-PREPARE-DATE-CALL
           MOVE TTK-DUE-DATE           TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           PERFORM 8350-CHECK-DATE-CALL
           MOVE DTA-DT-RESULT-DATE     TO TTK-DUE-DATE

           MOVE 'TTK     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE WS-SEQ-VALUE           TO TTK-TICKET-ID
           MOVE PIN-ACCOUNT-ID         TO TTK-ACCOUNT-ID
           MOVE 'INSTRFLD'             TO TTK-TICKET-TYPE
           MOVE '2'                    TO TTK-PRIORITY
           MOVE SPACES                 TO TTK-SUBJECT-TEXT
           MOVE PIN-INSTR-ID           TO WS-NUM-DISP
           STRING PIN-INSTR-TYPE ' instrument ' WS-NUM-DISP
                  ' deactivated after 3 failed collections'
                  DELIMITED BY SIZE INTO TTK-SUBJECT-TEXT
           MOVE TTK-SUBJECT-TEXT       TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO TTK-SUBJECT-LEN
           EXEC SQL
              INSERT INTO BILDB.BIL_TROUBLE_TICKET
                     (TICKET_ID, ACCOUNT_ID, TICKET_TYPE, PRIORITY,
                      SUBJECT, TICKET_STATUS, ASSIGNED_USER, HOLD_ID,
                      DUE_DATE, CREATE_TS, CLOSE_TS)
              VALUES (:TTK-TICKET-ID, :TTK-ACCOUNT-ID,
                      :TTK-TICKET-TYPE, :TTK-PRIORITY,
                      :TTK-SUBJECT, 'OPEN', ' ', 0,
                      :TTK-DUE-DATE, CURRENT TIMESTAMP, NULL)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5200-RAISE-TROUBLE-TICKET' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       5900-TRAILING-LENGTH.
           MOVE 100                    TO WS-TL-LEN
           PERFORM UNTIL WS-TL-LEN = 0
                      OR WS-TL-SRC(WS-TL-LEN:1) NOT = SPACE
              SUBTRACT 1               FROM WS-TL-LEN
           END-PERFORM.

      ******************************************************************
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
           MOVE SQA-SQ-VALUE           TO WS-SEQ-VALUE.

      ******************************************************************
      * BR-BIL-PIN-013 Audit keyed by instrument id
       8100-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE WS-AUD-ENTITY          TO AUA-AU-ENTITY
           MOVE PIN-INSTR-ID           TO WS-NUM-DISP
           MOVE WS-NUM-DISP            TO AUA-AU-KEY
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
      * Header for a BILACCT0 call of request WS-ACT-REQUEST.
      * Body fields left blank / zero mean "unchanged" to ACCTUPD.
       8200-PREPARE-ACCOUNT-CALL.
           INITIALIZE WS-ACT-CA
           MOVE WS-ACT-REQUEST         TO ACA-REQUEST-ID
           MOVE CA-USER-ID             TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE CA-TRACE-ID            TO ACA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE PIN-ACCOUNT-ID         TO ACA-AC-ACCOUNT-ID.

      ******************************************************************
       8250-CHECK-ACCOUNT-CALL.
           IF NOT ACA-RC-OK AND NOT ACA-RC-WARNING
              MOVE ACA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE ACA-REASON-CODE     TO CA-REASON-CODE
              MOVE ACA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8300-PREPARE-DATE-CALL.
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE CA-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM.

      ******************************************************************
       8350-CHECK-DATE-CALL.
           IF NOT DTA-RC-OK
              MOVE DTA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTA-REASON-CODE     TO CA-REASON-CODE
              MOVE DTA-MESSAGE         TO CA-MESSAGE
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
              MOVE 'Instrument data locked - retry' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           MOVE SPACES                 TO CA-MESSAGE
           STRING 'BILINSTR Db2 error SQLCODE ' WS-SQLCODE-DISP
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
