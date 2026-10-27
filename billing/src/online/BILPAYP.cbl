       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILPAYP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILPAYP                                          *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * FUNCTION    : Payment entry (transaction BPAY, map BILPYM).    *
      *               Two-step: ENTER validates the receipt (account   *
      *               or invoice number, amount, method, reference,    *
      *               received date), shows the payer and what is      *
      *               owed and protects the fields; PF5 then posts it  *
      *               through BILPAYRC PAYRCV and shows the money id,  *
      *               the amount distributed to items and the amount   *
      *               left unapplied. PF12 returns to amend.           *
      * REQUESTS    : n/a (screen) - LINKs BILPAYRC PAYRCV,            *
      *               BILACCT0 ACCTINQ, BILINVSV INVINQ,               *
      *               BILCODVL (PAYMTHD), BILDATES DTVALD              *
      * CALLED BY   : terminal (BPAY), XCTL from BILMNUP / BILACIP /   *
      *               BILIVMP                                          *
      * CALLS       : LINK BILPAYRC BILACCT0 BILINVSV BILCODVL         *
      *               BILDATES BILERRLG; XCTL BILMNUP                  *
      * TABLES      : none (all data through services)                 *
      * RULES       : BR-BIL-PYE-001 An account or an invoice number   *
      *                              is required; when both are keyed  *
      *                              the invoice must be the account's *
      *               BR-BIL-PYE-002 Amount: digits, optional commas,  *
      *                              at most 2 decimals, above zero    *
      *               BR-BIL-PYE-003 Online receipts are limited to    *
      *                              250,000.00; larger go to treasury *
      *               BR-BIL-PYE-004 Method must be an active PAYMTHD  *
      *                              code                              *
      *               BR-BIL-PYE-005 A reference is required for every *
      *                              method except CASH                *
      *               BR-BIL-PYE-006 Received date, when keyed, must   *
      *                              be a valid date (blank = business *
      *                              date)                             *
      *               BR-BIL-PYE-007 Posting needs a validated screen  *
      *                              (PF5 only in the confirm step)    *
      *               BR-BIL-PYE-008 A posted screen cannot be posted  *
      *                              again (double-post guard)         *
      *               BR-BIL-PYE-009 Paying more than is owed is       *
      *                              allowed but warned (stays         *
      *                              unapplied)                        *
      *               BR-BIL-PYE-010 Service RC >= 90 rolls back the   *
      *                              task (SYNCPOINT ROLLBACK)         *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILPAYP'.
       01  WS-TRAN-ID                  PIC X(4)  VALUE 'BPAY'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'BILMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'BILPYM'.
       01  WS-PAY-PGM                  PIC X(8)  VALUE 'BILPAYRC'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-INV-PGM                  PIC X(8)  VALUE 'BILINVSV'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'BILCODVL'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-RECEIPT-SOURCE           PIC X(4)  VALUE 'CSR '.
       01  WS-ONLINE-LIMIT             PIC S9(11)V99 COMP-3
                                                 VALUE 250000.00.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP.
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-TARGET-PGM            PIC X(8).
           05 WS-ERR-TEXT              PIC X(79).
           05 WS-MSG                   PIC X(79).
           05 WS-OWED                  PIC S9(11)V99 COMP-3.
           05 WS-INV-ACCOUNT-ID        PIC S9(9) COMP.
           05 WS-AMT-EDIT              PIC -ZZZ,ZZZ,ZZ9.99.
           05 WS-ID-EDIT               PIC Z(9)9.
           05 WS-DX                    PIC S9(4) COMP.
           05 WS-COUNT-EDIT            PIC Z9.

      * Amount parsing (BR-BIL-PYE-002)
       01  WS-AMT-PARSE.
           05 WS-AMT-TEXT              PIC X(13).
           05 WS-AMT-CHAR              PIC X.
              88 WS-AMT-DIGIT           VALUE '0' THRU '9'.
           05 WS-CX                    PIC S9(4) COMP.
           05 WS-DIGIT                 PIC 9.
           05 WS-AMT-CENTS             PIC S9(13) COMP-3.
           05 WS-DECIMALS              PIC S9(4) COMP.
           05 WS-DIGITS                PIC S9(4) COMP.
           05 WS-POINT-SEEN            PIC X.
           05 WS-AMT-BAD               PIC X.
           05 WS-TRAILING              PIC X.
           05 WS-AMOUNT                PIC S9(11)V99 COMP-3.

       01  WS-DIST-LINE.
           05 WDL-ITEM-ID              PIC Z(8)9.
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 WDL-INVOICE-NO           PIC X(12).
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 WDL-AMOUNT               PIC ZZZ,ZZZ,ZZ9.99-.

       01  WS-SCR-CA.
           COPY BILMNUC1.

       01  WS-PAY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PYA-==.
           03 PYA-BODY.
              COPY BILCPAY REPLACING LEADING ==CA-== BY ==PYA-==.

       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.

       01  WS-INV-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==IVA-==.
           03 IVA-BODY.
              COPY BILCINV REPLACING LEADING ==CA-== BY ==IVA-==.

       01  WS-COD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY BILCCOD REPLACING LEADING ==CA-== BY ==CDA-==.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILMSET.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(256).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE TRUE
              WHEN EIBCALEN = 0
              WHEN SCR-OWNER-PGM NOT = WS-PROGRAM-ID
                 PERFORM 2000-FIRST-DISPLAY
              WHEN EIBAID = DFHPF3
                 MOVE 'BILMNUP'        TO WS-TARGET-PGM
                 PERFORM 8900-XCTL
              WHEN EIBAID = DFHCLEAR
                 PERFORM 2000-FIRST-DISPLAY
              WHEN EIBAID = DFHPF12
                 PERFORM 2400-AMEND-OR-CLEAR
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-VALIDATE-ENTRY
              WHEN EIBAID = DFHPF5
                 PERFORM 2200-POST-PAYMENT
              WHEN OTHER
                 MOVE 'That key is not active on this screen'
                                       TO WS-MSG
                 PERFORM 7200-SEND-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           IF EIBCALEN > 0
              MOVE DFHCOMMAREA(1:EIBCALEN) TO WS-SCR-CA
           END-IF
           IF EIBCALEN = 0 OR NOT SCR-VALID
              INITIALIZE WS-SCR-CA
              SET SCR-VALID            TO TRUE
              EXEC CICS ASSIGN USERID(SCR-USER-ID)
                        RESP(WS-RESP)
              END-EXEC
           END-IF
           MOVE SPACES                 TO WS-MSG
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
                     RESP(WS-RESP)
           END-EXEC.

      ******************************************************************
       2000-FIRST-DISPLAY.
           MOVE SPACES                 TO SCR-WORK-AREA
           SET SCR-PY-ENTRY            TO TRUE
           MOVE SCR-ACCOUNT-NUMBER     TO SCR-PY-ACCOUNT-NUMBER
           MOVE 0                      TO SCR-PY-AMOUNT
                                          SCR-PY-MONEY-ID
           MOVE LOW-VALUES             TO BILPYMO
           MOVE SCR-PY-ACCOUNT-NUMBER  TO PYMACCTO
           MOVE 'Key the payment and press ENTER to validate'
                                       TO WS-MSG
           IF SCR-PY-ACCOUNT-NUMBER = SPACES
              MOVE -1                  TO PYMACCTL
           ELSE
              MOVE -1                  TO PYMAMTL
           END-IF
           PERFORM 7000-SEND-MAP.

      ******************************************************************
      * Step 1: validate and show what will be posted
       2100-VALIDATE-ENTRY.
           IF SCR-PY-POSTED
              MOVE 'Payment posted - PF12 for a new payment' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF SCR-PY-CONFIRM
              MOVE 'Press PF5 to post, or PF12 to amend' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 6000-RECEIVE-MAP
           PERFORM 2150-CAPTURE-FIELDS
           MOVE LOW-VALUES             TO BILPYMO
           PERFORM 3000-VALIDATE-FIELDS
           PERFORM 3500-RESOLVE-PAYER
           SET SCR-PY-CONFIRM          TO TRUE
           MOVE SCR-PY-ACCOUNT-NUMBER  TO SCR-ACCOUNT-NUMBER
           PERFORM 5000-SHOW-ENTRY
           PERFORM 5100-PROTECT-ENTRY
           IF WS-MSG = SPACES
              MOVE 'Check the details - PF5 to post, PF12 to amend'
                                       TO WS-MSG
           END-IF
           PERFORM 7000-SEND-MAP.

      ******************************************************************
       2150-CAPTURE-FIELDS.
           IF PYMACCTL > 0 OR PYMACCTF = DFHBMEOF
              MOVE PYMACCTI            TO SCR-PY-ACCOUNT-NUMBER
           END-IF
           IF PYMINVNL > 0 OR PYMINVNF = DFHBMEOF
              MOVE PYMINVNI            TO SCR-PY-INVOICE-NUMBER
           END-IF
           IF PYMAMTL > 0 OR PYMAMTF = DFHBMEOF
              MOVE PYMAMTI             TO WS-AMT-TEXT
           ELSE
              MOVE SPACES              TO WS-AMT-TEXT
           END-IF
           IF PYMMETHL > 0 OR PYMMETHF = DFHBMEOF
              MOVE PYMMETHI            TO SCR-PY-METHOD
           END-IF
           IF PYMREFL > 0 OR PYMREFF = DFHBMEOF
              MOVE PYMREFI             TO SCR-PY-REF
           END-IF
           IF PYMRDATL > 0 OR PYMRDATF = DFHBMEOF
              MOVE PYMRDATI            TO SCR-PY-RCVD-DATE
           END-IF
           INSPECT SCR-PY-ACCOUNT-NUMBER REPLACING ALL LOW-VALUE
                   BY SPACE
           INSPECT SCR-PY-INVOICE-NUMBER REPLACING ALL LOW-VALUE
                   BY SPACE
           INSPECT WS-AMT-TEXT REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SCR-PY-METHOD REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SCR-PY-REF REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SCR-PY-RCVD-DATE REPLACING ALL LOW-VALUE BY SPACE.

      ******************************************************************
      * Step 2: post through BILPAYRC
       2200-POST-PAYMENT.
      * BR-BIL-PYE-008 Double-post guard
           IF SCR-PY-POSTED
              MOVE SCR-PY-MONEY-ID     TO WS-ID-EDIT
              STRING 'Already posted as money id ' WS-ID-EDIT
                     DELIMITED BY SIZE INTO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PYE-007 PF5 only after ENTER has validated the screen
           IF NOT SCR-PY-CONFIRM
              MOVE 'Press ENTER to validate the payment first'
                                       TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8000-LINK-PAYRCV
           MOVE LOW-VALUES             TO BILPYMO
           IF PYA-RETURN-CODE < 90
              PERFORM 8100-LINK-ACCTINQ
           END-IF
           EVALUATE TRUE
              WHEN PYA-RC-OK
              WHEN PYA-RC-WARNING
                 SET SCR-PY-POSTED     TO TRUE
                 MOVE PYA-PY-MONEY-ID     TO SCR-PY-MONEY-ID
                 PERFORM 5000-SHOW-ENTRY
                 PERFORM 5100-PROTECT-ENTRY
                 PERFORM 5200-SHOW-RESULT
                 IF PYA-RC-WARNING
                    MOVE PYA-MESSAGE   TO WS-MSG
                 ELSE
                    MOVE SCR-PY-MONEY-ID TO WS-ID-EDIT
                    STRING 'Payment posted, money id ' WS-ID-EDIT
                           ' - PF12 for the next payment'
                           DELIMITED BY SIZE INTO WS-MSG
                 END-IF
      * BR-BIL-PYE-010 Failed unit of work is rolled back
              WHEN PYA-RETURN-CODE >= 90
                 PERFORM 8800-ROLLBACK
                 SET SCR-PY-ENTRY      TO TRUE
                 PERFORM 5000-SHOW-ENTRY
                 STRING 'Not posted: ' PYA-MESSAGE
                        DELIMITED BY SIZE INTO WS-MSG
                 MOVE -1               TO PYMAMTL
              WHEN OTHER
                 SET SCR-PY-ENTRY      TO TRUE
                 PERFORM 5000-SHOW-ENTRY
                 STRING 'Not posted (' PYA-REASON-CODE '): '
                        PYA-MESSAGE DELIMITED BY SIZE INTO WS-MSG
                 PERFORM 5300-CURSOR-FOR-REASON
           END-EVALUATE
           PERFORM 7000-SEND-MAP.

      ******************************************************************
      * PF12: from confirm back to amend; otherwise a fresh screen
       2400-AMEND-OR-CLEAR.
           IF SCR-PY-CONFIRM
              SET SCR-PY-ENTRY         TO TRUE
              MOVE LOW-VALUES          TO BILPYMO
              PERFORM 5000-SHOW-ENTRY
              MOVE -1                  TO PYMAMTL
              MOVE 'Amend the payment and press ENTER' TO WS-MSG
              PERFORM 7000-SEND-MAP
           ELSE
              MOVE SPACES              TO SCR-ACCOUNT-NUMBER
              PERFORM 2000-FIRST-DISPLAY
           END-IF.

      ******************************************************************
       3000-VALIDATE-FIELDS.
      * BR-BIL-PYE-001 Account or invoice
           IF SCR-PY-ACCOUNT-NUMBER = SPACES
              AND SCR-PY-INVOICE-NUMBER = SPACES
              MOVE 'Enter an account number or an invoice number'
                                       TO WS-MSG
              MOVE -1                  TO PYMACCTL
              PERFORM 3900-FIELD-ERROR
           END-IF
           PERFORM 3100-PARSE-AMOUNT
           IF WS-AMT-BAD = 'Y'
              MOVE 'Amount must be a number with up to 2 decimals'
                                       TO WS-MSG
              MOVE -1                  TO PYMAMTL
              PERFORM 3900-FIELD-ERROR
           END-IF
           IF WS-AMOUNT NOT > 0
              MOVE 'Amount must be greater than zero' TO WS-MSG
              MOVE -1                  TO PYMAMTL
              PERFORM 3900-FIELD-ERROR
           END-IF
      * BR-BIL-PYE-003 Online limit
           IF WS-AMOUNT > WS-ONLINE-LIMIT
              MOVE 'Over 250,000.00 - route the receipt to treasury'
                                       TO WS-MSG
              MOVE -1                  TO PYMAMTL
              PERFORM 3900-FIELD-ERROR
           END-IF
           MOVE WS-AMOUNT              TO SCR-PY-AMOUNT
      * BR-BIL-PYE-004 Method from the code table
           PERFORM 8300-LINK-CODVL
           IF NOT CDA-CD-IS-VALID
              STRING 'Payment method ' SCR-PY-METHOD
                     ' is not a valid method'
                     DELIMITED BY SIZE INTO WS-MSG
              MOVE -1                  TO PYMMETHL
              PERFORM 3900-FIELD-ERROR
           END-IF
      * BR-BIL-PYE-005 Reference for traceable methods
           IF SCR-PY-METHOD NOT = 'CASH' AND SCR-PY-REF = SPACES
              MOVE 'A reference is required for this method'
                                       TO WS-MSG
              MOVE -1                  TO PYMREFL
              PERFORM 3900-FIELD-ERROR
           END-IF
      * BR-BIL-PYE-006 Received date
           IF SCR-PY-RCVD-DATE NOT = SPACES
              PERFORM 8400-LINK-DTVALD
              IF NOT DTA-DT-IS-VALID
                 MOVE 'Received date must be a valid yyyy-mm-dd date'
                                       TO WS-MSG
                 MOVE -1               TO PYMRDATL
                 PERFORM 3900-FIELD-ERROR
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-PYE-002 Digits, commas ignored, one point, 2 decimals
       3100-PARSE-AMOUNT.
           MOVE 'N'                    TO WS-AMT-BAD WS-POINT-SEEN
                                          WS-TRAILING
           MOVE 0                      TO WS-AMT-CENTS WS-DECIMALS
                                          WS-DIGITS WS-AMOUNT
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > LENGTH OF WS-AMT-TEXT
                      OR WS-AMT-BAD = 'Y'
              MOVE WS-AMT-TEXT(WS-CX:1) TO WS-AMT-CHAR
              EVALUATE TRUE
                 WHEN WS-AMT-CHAR = SPACE
                    IF WS-DIGITS > 0
                       MOVE 'Y'        TO WS-TRAILING
                    END-IF
                 WHEN WS-TRAILING = 'Y'
                    MOVE 'Y'           TO WS-AMT-BAD
                 WHEN WS-AMT-DIGIT
                    MOVE WS-AMT-CHAR   TO WS-DIGIT
                    COMPUTE WS-AMT-CENTS = WS-AMT-CENTS * 10 + WS-DIGIT
                    ADD 1              TO WS-DIGITS
                    IF WS-POINT-SEEN = 'Y'
                       ADD 1           TO WS-DECIMALS
                    END-IF
                 WHEN WS-AMT-CHAR = ','
                    IF WS-POINT-SEEN = 'Y'
                       MOVE 'Y'        TO WS-AMT-BAD
                    END-IF
                 WHEN WS-AMT-CHAR = '.'
                    IF WS-POINT-SEEN = 'Y'
                       MOVE 'Y'        TO WS-AMT-BAD
                    END-IF
                    MOVE 'Y'           TO WS-POINT-SEEN
                 WHEN OTHER
                    MOVE 'Y'           TO WS-AMT-BAD
              END-EVALUATE
           END-PERFORM
           IF WS-DIGITS = 0 OR WS-DECIMALS > 2
              MOVE 'Y'                 TO WS-AMT-BAD
           END-IF
           IF WS-AMT-BAD = 'N'
              PERFORM UNTIL WS-DECIMALS = 2
                 COMPUTE WS-AMT-CENTS = WS-AMT-CENTS * 10
                 ADD 1                 TO WS-DECIMALS
              END-PERFORM
              COMPUTE WS-AMOUNT = WS-AMT-CENTS / 100
           END-IF.

      ******************************************************************
      * Payer lookup for the confirmation step
       3500-RESOLVE-PAYER.
           MOVE 0                      TO WS-INV-ACCOUNT-ID
           IF SCR-PY-INVOICE-NUMBER NOT = SPACES
              PERFORM 8200-LINK-INVINQ
              IF NOT IVA-RC-OK
                 MOVE SPACES           TO WS-MSG
                 STRING 'Invoice ' SCR-PY-INVOICE-NUMBER ': '
                        IVA-MESSAGE DELIMITED BY SIZE INTO WS-MSG
                 MOVE -1               TO PYMINVNL
                 PERFORM 3900-FIELD-ERROR
              END-IF
              MOVE IVA-IV-ACCOUNT-ID      TO WS-INV-ACCOUNT-ID
           END-IF
           PERFORM 8100-LINK-ACCTINQ
           IF NOT (ACA-RC-OK OR ACA-RC-WARNING)
              MOVE SPACES              TO WS-MSG
              STRING 'Account: ' ACA-MESSAGE
                     DELIMITED BY SIZE INTO WS-MSG
              MOVE -1                  TO PYMACCTL
              PERFORM 3900-FIELD-ERROR
           END-IF
      * BR-BIL-PYE-001 A keyed invoice must belong to the keyed account
           IF WS-INV-ACCOUNT-ID > 0
              AND WS-INV-ACCOUNT-ID NOT = ACA-AC-ACCOUNT-ID
              MOVE 'Invoice belongs to a different account'
                                       TO WS-MSG
              MOVE -1                  TO PYMINVNL
              PERFORM 3900-FIELD-ERROR
           END-IF
           MOVE ACA-AC-ACCOUNT-NUMBER  TO SCR-PY-ACCOUNT-NUMBER
           MOVE ACA-AC-ACCOUNT-ID      TO SCR-ACCOUNT-ID
           IF ACA-AC-STATUS NOT = 'ACTV'
              STRING 'Account status ' ACA-AC-STATUS
                     ' - payment may be held in suspense'
                     DELIMITED BY SIZE INTO WS-MSG
           END-IF
      * BR-BIL-PYE-009 Overpayment warning
           COMPUTE WS-OWED = ACA-AC-BILLED-AMT + ACA-AC-DUE-AMT
                           - ACA-AC-UNAPPLIED-AMT
           IF SCR-PY-AMOUNT > WS-OWED AND WS-MSG = SPACES
              MOVE 'Amount exceeds the billed balance - rest unapplied'
                                       TO WS-MSG
           END-IF.

      ******************************************************************
      * Re-show the entry with the error; stay in the entry step
       3900-FIELD-ERROR.
           SET SCR-PY-ENTRY            TO TRUE
           PERFORM 5000-SHOW-ENTRY
           MOVE WS-AMT-TEXT            TO PYMAMTO
           PERFORM 7000-SEND-MAP
           PERFORM 9000-RETURN.

      ******************************************************************
       5000-SHOW-ENTRY.
           MOVE SCR-PY-ACCOUNT-NUMBER  TO PYMACCTO
           MOVE SCR-PY-INVOICE-NUMBER  TO PYMINVNO
           MOVE SCR-PY-AMOUNT          TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO PYMAMTO
           MOVE SCR-PY-METHOD          TO PYMMETHO
           MOVE SCR-PY-REF             TO PYMREFO
           MOVE SCR-PY-RCVD-DATE       TO PYMRDATO
           IF SCR-PY-CONFIRM OR SCR-PY-POSTED
              MOVE ACA-AC-ACCOUNT-NAME TO PYMANAMO
              MOVE ACA-AC-DUE-AMT      TO WS-AMT-EDIT
              MOVE WS-AMT-EDIT         TO PYMADUEO
              IF WS-INV-ACCOUNT-ID > 0
                 MOVE IVA-IV-AMOUNT-DUE   TO WS-AMT-EDIT
                 MOVE WS-AMT-EDIT      TO PYMIDUEO
              END-IF
           END-IF.

      ******************************************************************
       5100-PROTECT-ENTRY.
           MOVE DFHBMPRO               TO PYMACCTA PYMINVNA PYMAMTA
                                          PYMMETHA PYMREFA PYMRDATA.

      ******************************************************************
       5200-SHOW-RESULT.
           MOVE PYA-PY-MONEY-ID           TO WS-ID-EDIT
           MOVE WS-ID-EDIT             TO PYMMNIDO
           MOVE PYA-PY-STATUS             TO PYMSTATO
           MOVE PYA-PY-DISTRIBUTED        TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO PYMDISTO
           MOVE PYA-PY-UNAPPLIED          TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO PYMUNAPO
           PERFORM VARYING WS-DX FROM 1 BY 1
                   UNTIL WS-DX > 6 OR WS-DX > PYA-PY-DIST-COUNT
              MOVE PYA-PY-D-ITEM-ID(WS-DX) TO WDL-ITEM-ID
              MOVE PYA-PY-D-INVOICE-NO(WS-DX) TO WDL-INVOICE-NO
              MOVE PYA-PY-D-AMOUNT(WS-DX) TO WDL-AMOUNT
              MOVE WS-DIST-LINE        TO PYMDLNO(WS-DX)
           END-PERFORM
           IF PYA-PY-DIST-COUNT > 6
              MOVE PYA-PY-DIST-COUNT      TO WS-COUNT-EDIT
              MOVE SPACES              TO PYMDLNO(6)
              STRING '... ' WS-COUNT-EDIT ' items paid in total'
                     DELIMITED BY SIZE INTO PYMDLNO(6)
           END-IF.

      ******************************************************************
      * Put the cursor on the field the service rejected
       5300-CURSOR-FOR-REASON.
           EVALUATE PYA-REASON-CODE
              WHEN 'ACTNOTFD'
              WHEN 'ACCTREQD'
              WHEN 'ACCTCLSD'
                 MOVE -1               TO PYMACCTL
              WHEN 'BADMETH '
                 MOVE -1               TO PYMMETHL
              WHEN 'DUPPYMT '
                 MOVE -1               TO PYMREFL
              WHEN 'BADDATE '
              WHEN 'FUTDATE '
                 MOVE -1               TO PYMRDATL
              WHEN OTHER
                 MOVE -1               TO PYMAMTL
           END-EVALUATE.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(BILPYMI)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
              WHEN DFHRESP(NORMAL)
                 CONTINUE
              WHEN DFHRESP(MAPFAIL)
                 MOVE LOW-VALUES       TO BILPYMI
              WHEN OTHER
                 MOVE '6000-RECEIVE-MAP' TO ERC-ER-PARAGRAPH
                 PERFORM 9800-CICS-ERROR
           END-EVALUATE.

      ******************************************************************
       7000-SEND-MAP.
           MOVE WS-TODAY               TO PYMDATEO
           MOVE WS-MSG                 TO PYMMSGO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILPYMO) ERASE CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7000-SEND-MAP'     TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-OWNER-PGM.

      ******************************************************************
       7200-SEND-MESSAGE.
           MOVE LOW-VALUES             TO BILPYMO
           MOVE WS-MSG                 TO PYMMSGO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILPYMO) DATAONLY
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7200-SEND-MESSAGE' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF.

      ******************************************************************
       8000-LINK-PAYRCV.
           INITIALIZE WS-PAY-CA
           MOVE 'PAYRCV  '             TO PYA-REQUEST-ID
           MOVE SCR-USER-ID            TO PYA-USER-ID
           MOVE WS-PROGRAM-ID          TO PYA-CALLER-PGM
           MOVE EIBTRNID               TO PYA-TRACE-ID
           MOVE SCR-ACCOUNT-ID         TO PYA-PY-ACCOUNT-ID
           MOVE SCR-PY-ACCOUNT-NUMBER  TO PYA-PY-ACCOUNT-NUMBER
           MOVE SCR-PY-INVOICE-NUMBER  TO PYA-PY-INVOICE-NUMBER
           MOVE SCR-PY-AMOUNT          TO PYA-PY-AMOUNT
           MOVE SCR-PY-METHOD          TO PYA-PY-METHOD
           MOVE SCR-PY-REF             TO PYA-PY-REF-NUMBER
           MOVE WS-RECEIPT-SOURCE      TO PYA-PY-SOURCE
           MOVE SCR-PY-RCVD-DATE       TO PYA-PY-RECEIVED-DATE
           EXEC CICS LINK PROGRAM(WS-PAY-PGM)
                     COMMAREA(WS-PAY-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8000-LINK-PAYRCV'  TO ERC-ER-PARAGRAPH
              PERFORM 8800-ROLLBACK
              PERFORM 9800-CICS-ERROR
           END-IF.

      ******************************************************************
       8100-LINK-ACCTINQ.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTINQ '             TO ACA-REQUEST-ID
           MOVE SCR-USER-ID            TO ACA-USER-ID
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE EIBTRNID               TO ACA-TRACE-ID
           IF WS-INV-ACCOUNT-ID > 0
              AND SCR-PY-ACCOUNT-NUMBER = SPACES
              MOVE WS-INV-ACCOUNT-ID   TO ACA-AC-ACCOUNT-ID
           ELSE
              MOVE SCR-PY-ACCOUNT-NUMBER TO ACA-AC-ACCOUNT-NUMBER
           END-IF
           EXEC CICS LINK PROGRAM(WS-ACT-PGM)
                     COMMAREA(WS-ACT-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8100-LINK-ACCTINQ' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           IF ACA-RETURN-CODE >= 90
              PERFORM 8800-ROLLBACK
           END-IF.

      ******************************************************************
       8200-LINK-INVINQ.
           INITIALIZE WS-INV-CA
           MOVE 'INVINQ  '             TO IVA-REQUEST-ID
           MOVE SCR-USER-ID            TO IVA-USER-ID
           MOVE WS-PROGRAM-ID          TO IVA-CALLER-PGM
           MOVE EIBTRNID               TO IVA-TRACE-ID
           MOVE SCR-PY-INVOICE-NUMBER  TO IVA-IV-INVOICE-NUMBER
           EXEC CICS LINK PROGRAM(WS-INV-PGM)
                     COMMAREA(WS-INV-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8200-LINK-INVINQ'  TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           IF IVA-RETURN-CODE >= 90
              PERFORM 8800-ROLLBACK
           END-IF.

      ******************************************************************
       8300-LINK-CODVL.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE SCR-USER-ID            TO CDA-USER-ID
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE 'PAYMTHD '             TO CDA-CD-TABLE
           MOVE SCR-PY-METHOD          TO CDA-CD-CODE
           EXEC CICS LINK PROGRAM(WS-COD-PGM)
                     COMMAREA(WS-COD-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8300-LINK-CODVL'   TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF.

      ******************************************************************
       8400-LINK-DTVALD.
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE SCR-USER-ID            TO DTA-USER-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE SCR-PY-RCVD-DATE       TO DTA-DT-DATE-1
           EXEC CICS LINK PROGRAM(WS-DTE-PGM)
                     COMMAREA(WS-DTE-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8400-LINK-DTVALD'  TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF.

      ******************************************************************
       8800-ROLLBACK.
           EXEC CICS SYNCPOINT ROLLBACK
                     RESP(WS-RESP)
           END-EXEC.

      ******************************************************************
       8900-XCTL.
           EXEC CICS XCTL PROGRAM(WS-TARGET-PGM)
                     COMMAREA(WS-SCR-CA)
                     LENGTH(LENGTH OF WS-SCR-CA)
                     RESP(WS-RESP)
           END-EXEC
           MOVE '8900-XCTL'            TO ERC-ER-PARAGRAPH
           PERFORM 9850-LOG-CICS-ERROR
           STRING 'Screen ' WS-TARGET-PGM ' is not available'
                  DELIMITED BY SIZE INTO WS-MSG
           PERFORM 7200-SEND-MESSAGE.

      ******************************************************************
       9000-RETURN.
           EXEC CICS RETURN TRANSID(WS-TRAN-ID)
                     COMMAREA(WS-SCR-CA)
                     LENGTH(LENGTH OF WS-SCR-CA)
           END-EXEC.

      ******************************************************************
       9800-CICS-ERROR.
           PERFORM 9850-LOG-CICS-ERROR
           EXEC CICS SEND TEXT FROM(WS-ERR-TEXT)
                     LENGTH(LENGTH OF WS-ERR-TEXT)
                     ERASE FREEKB
                     NOHANDLE
           END-EXEC
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
       9850-LOG-CICS-ERROR.
           MOVE WS-RESP                TO ERC-ER-RESP
           MOVE SPACES                 TO WS-ERR-TEXT
           STRING 'BILPAYP CICS error in ' ERC-ER-PARAGRAPH
                  DELIMITED BY SIZE INTO WS-ERR-TEXT
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE SCR-USER-ID            TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE 0                      TO ERC-ER-SQLCODE
           MOVE WS-ERR-TEXT            TO ERC-ER-TEXT
           MOVE SCR-PY-ACCOUNT-NUMBER  TO ERC-ER-KEY
           MOVE EIBTRNID               TO ERC-ER-TRAN-ID
           EXEC CICS LINK PROGRAM(WS-ERR-PGM)
                     COMMAREA(WS-ERR-CA)
                     LENGTH(4000)
                     NOHANDLE
           END-EXEC.
