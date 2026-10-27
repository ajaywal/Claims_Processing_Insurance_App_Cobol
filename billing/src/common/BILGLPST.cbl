       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILGLPST.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILGLPST                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Double-entry ledger posting. Every money         *
      *               movement in billing (charges, billing, cash,     *
      *               distribution, write-off, commission,             *
      *               disbursement) is recorded as one balanced        *
      *               BIL_TRANSACTION with 2..8 BIL_LEDGER_LINE rows   *
      *               against the billing T-accounts. Reversals post   *
      *               a mirror image linked to the original.           *
      * REQUESTS    : GLPOST  post a balanced transaction              *
      *               GLREVS  reverse CA-GL-REVERSAL-OF (DR/CR swap)   *
      * CALLED BY   : charge, invoice, payment, distribution, write-   *
      *               off, commission and disbursement services        *
      * CALLS       : BILSEQGN, BILERRLG                               *
      * TABLES      : BIL_CONTROL (R), BIL_TRANSACTION (C R),          *
      *               BIL_LEDGER_LINE (C R)                            *
      * RULES       : BR-BIL-GLP-001 Transaction type and reference    *
      *                              type are mandatory                *
      *               BR-BIL-GLP-002 A transaction has 1 to 8 lines    *
      *                              and at least one debit and one    *
      *                              credit                            *
      *               BR-BIL-GLP-003 Lines post only to the billing    *
      *                              chart of T-accounts               *
      *               BR-BIL-GLP-004 Line side is D or C and the       *
      *                              amount is greater than zero       *
      *               BR-BIL-GLP-005 Account-level T-accounts need an  *
      *                              account id; commission T-accounts *
      *                              need a producer id                *
      *               BR-BIL-GLP-006 Total debits must equal total     *
      *                              credits                           *
      *               BR-BIL-GLP-007 Postings carry the business date, *
      *                              not the system clock              *
      *               BR-BIL-GLP-008 GLPOST may not set REVERSAL-OF;   *
      *                              reversals go through GLREVS       *
      *               BR-BIL-GLP-009 A reversal mirrors the original   *
      *                              lines with debit and credit       *
      *                              swapped, on today's business date *
      *               BR-BIL-GLP-010 A reversal cannot be reversed     *
      *               BR-BIL-GLP-011 A transaction can be reversed     *
      *                              only once; the original is locked *
      *                              while the check is made           *
      *               BR-BIL-GLP-012 A reversal requested for an       *
      *                              account must belong to it         *
      *               BR-BIL-GLP-013 Transaction keys come from        *
      *                              sequence TXN                      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILGLPST'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.

      * BR-BIL-GLP-003 Chart of T-accounts with owner class
      *   A = account-level, P = producer-level, G = general ledger
       01  WS-TACCT-DATA.
           05 FILLER                   PIC X(9) VALUE 'CASH    G'.
           05 FILLER                   PIC X(9) VALUE 'UNAPPLD A'.
           05 FILLER                   PIC X(9) VALUE 'UNBILLEDA'.
           05 FILLER                   PIC X(9) VALUE 'BILLED  A'.
           05 FILLER                   PIC X(9) VALUE 'DUE     A'.
           05 FILLER                   PIC X(9) VALUE 'PREMREV G'.
           05 FILLER                   PIC X(9) VALUE 'FEEREV  G'.
           05 FILLER                   PIC X(9) VALUE 'SUSPENSEG'.
           05 FILLER                   PIC X(9) VALUE 'WRITEOFFG'.
           05 FILLER                   PIC X(9) VALUE 'COMMRSV P'.
           05 FILLER                   PIC X(9) VALUE 'COMMPAY P'.
           05 FILLER                   PIC X(9) VALUE 'DISBPAY G'.
           05 FILLER                   PIC X(9) VALUE 'CLMPAY  G'.
       01  WS-TACCT-TABLE REDEFINES WS-TACCT-DATA.
           05 WS-TACCT-ENTRY OCCURS 13 TIMES.
              10 WS-TACCT-NAME         PIC X(8).
              10 WS-TACCT-CLASS        PIC X.
       01  WS-TACCT-COUNT              PIC S9(4) COMP VALUE 13.

       01  WS-WORK.
           05 WS-LX                    PIC S9(4) COMP VALUE 0.
           05 WS-TX                    PIC S9(4) COMP VALUE 0.
           05 WS-TACCT-CLASS-FOUND     PIC X     VALUE SPACE.
           05 WS-DR-TOTAL              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CR-TOTAL              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-DR-LINES              PIC S9(4) COMP VALUE 0.
           05 WS-CR-LINES              PIC S9(4) COMP VALUE 0.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-REVERSAL-OF           PIC S9(9) COMP VALUE 0.
           05 WS-PRIOR-REVERSALS       PIC S9(9) COMP VALUE 0.
           05 WS-ORIG-TXN-ID           PIC S9(9) COMP VALUE 0.
           05 WS-SAVE-SQLCODE          PIC S9(9) COMP VALUE 0.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-AMT-DISP-1            PIC -Z(10)9.99.
           05 WS-AMT-DISP-2            PIC -Z(10)9.99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-LINE-DISP             PIC 9.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE BILDTXN END-EXEC.
           EXEC SQL INCLUDE BILDLGL END-EXEC.

           EXEC SQL DECLARE CSR-ORIG-LINES CURSOR FOR
              SELECT LINE_SEQ, T_ACCOUNT, DR_CR, LINE_AMOUNT
                FROM BILDB.BIL_LEDGER_LINE
               WHERE TXN_ID = :WS-ORIG-TXN-ID
               ORDER BY LINE_SEQ
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCGLP.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'GLPOST  '
                 PERFORM 2000-POST
              WHEN 'GLREVS  '
                 PERFORM 2100-REVERSE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILGLPST: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-GL-TXN-ID
                                          WS-DR-TOTAL WS-CR-TOTAL
                                          WS-DR-LINES WS-CR-LINES
                                          WS-REVERSAL-OF
           IF CA-USER-ID = SPACES
              MOVE 'BATCH   '          TO CA-USER-ID
           END-IF.

      ******************************************************************
       2000-POST.
           PERFORM 3000-VALIDATE-HEADER
      * BR-BIL-GLP-008 Reversals only through GLREVS
           IF CA-GL-REVERSAL-OF NOT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLUSEREV'          TO CA-REASON-CODE
              MOVE 'Use request GLREVS to post a reversal'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3100-VALIDATE-LINES
           PERFORM 3200-CHECK-BALANCE
           PERFORM 3300-GET-BUSINESS-DATE
           PERFORM 8000-GET-TXN-ID
           PERFORM 4000-INSERT-TRANSACTION
           PERFORM 4100-INSERT-LINES.

      ******************************************************************
       2100-REVERSE.
           IF CA-GL-REVERSAL-OF <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLREVREQ'          TO CA-REASON-CODE
              MOVE 'Transaction id to reverse is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-GL-REVERSAL-OF      TO WS-ORIG-TXN-ID
           PERFORM 4200-READ-ORIGINAL
      * BR-BIL-GLP-010 No reversal of a reversal
           IF TXN-REVERSAL-OF-TXN NOT = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'GLREVREV'          TO CA-REASON-CODE
              MOVE WS-ORIG-TXN-ID      TO WS-ID-DISP
              STRING 'Transaction ' WS-ID-DISP
                     ' is itself a reversal and cannot be reversed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-GLP-012 Reversal must belong to the stated account
           IF CA-GL-ACCOUNT-ID NOT = 0
           AND CA-GL-ACCOUNT-ID NOT = TXN-ACCOUNT-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLACCTMM'          TO CA-REASON-CODE
              MOVE 'Transaction to reverse belongs to another account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4300-CHECK-NOT-REVERSED
           PERFORM 5000-BUILD-REVERSAL-HEADER
           PERFORM 4400-LOAD-MIRROR-LINES
           MOVE WS-ORIG-TXN-ID         TO WS-REVERSAL-OF
           PERFORM 3100-VALIDATE-LINES
           PERFORM 3200-CHECK-BALANCE
      * BR-BIL-GLP-009 Reversal posts on today's business date
           PERFORM 3300-GET-BUSINESS-DATE
           PERFORM 8000-GET-TXN-ID
           PERFORM 4000-INSERT-TRANSACTION
           PERFORM 4100-INSERT-LINES.

      ******************************************************************
      * BR-BIL-GLP-001 Header validation
       3000-VALIDATE-HEADER.
           EVALUATE TRUE
              WHEN CA-GL-TXN-TYPE = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'GLTXNTYP'       TO CA-REASON-CODE
                 MOVE 'Transaction type is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN CA-GL-REF-TYPE = SPACES
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'GLREFTYP'       TO CA-REASON-CODE
                 MOVE 'Reference type is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN CA-GL-ACCOUNT-ID < 0
                OR CA-GL-PRODUCER-ID < 0
                OR CA-GL-REF-ID < 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'GLNEGID '       TO CA-REASON-CODE
                 MOVE 'Account, producer and reference ids cannot be'
                    & ' negative'      TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.

      ******************************************************************
       3100-VALIDATE-LINES.
      * BR-BIL-GLP-002 1..8 lines
           IF CA-GL-LINE-COUNT < 1 OR CA-GL-LINE-COUNT > 8
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLLINECT'          TO CA-REASON-CODE
              MOVE 'A posting must have 1 to 8 ledger lines'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-DR-TOTAL WS-CR-TOTAL
                                          WS-DR-LINES WS-CR-LINES
           PERFORM 3110-VALIDATE-LINE
                   VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > CA-GL-LINE-COUNT.

      ******************************************************************
       3110-VALIDATE-LINE.
           MOVE WS-LX                  TO WS-LINE-DISP
           PERFORM 5100-FIND-TACCOUNT
      * BR-BIL-GLP-003 Known T-account
           IF WS-TACCT-CLASS-FOUND = SPACE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLTACCT '          TO CA-REASON-CODE
              STRING 'Line ' WS-LINE-DISP ' T-account '
                     CA-GL-T-ACCOUNT(WS-LX) ' is not in the chart'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-GLP-004 Side and amount
           IF CA-GL-DR-CR(WS-LX) NOT = 'D'
           AND CA-GL-DR-CR(WS-LX) NOT = 'C'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLDRCR  '          TO CA-REASON-CODE
              STRING 'Line ' WS-LINE-DISP ' side must be D or C'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-GL-AMOUNT(WS-LX) NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLAMOUNT'          TO CA-REASON-CODE
              STRING 'Line ' WS-LINE-DISP
                     ' amount must be greater than zero'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-GLP-005 Owner of sub-ledger T-accounts
           IF WS-TACCT-CLASS-FOUND = 'A' AND CA-GL-ACCOUNT-ID = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLNOACCT'          TO CA-REASON-CODE
              STRING 'Line ' WS-LINE-DISP ' T-account '
                     CA-GL-T-ACCOUNT(WS-LX) ' needs an account id'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-TACCT-CLASS-FOUND = 'P' AND CA-GL-PRODUCER-ID = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLNOPROD'          TO CA-REASON-CODE
              STRING 'Line ' WS-LINE-DISP ' T-account '
                     CA-GL-T-ACCOUNT(WS-LX) ' needs a producer id'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-GL-DR-CR(WS-LX) = 'D'
              ADD CA-GL-AMOUNT(WS-LX)  TO WS-DR-TOTAL
              ADD 1                    TO WS-DR-LINES
           ELSE
              ADD CA-GL-AMOUNT(WS-LX)  TO WS-CR-TOTAL
              ADD 1                    TO WS-CR-LINES
           END-IF.

      ******************************************************************
       3200-CHECK-BALANCE.
      * BR-BIL-GLP-002 Both sides present
           IF WS-DR-LINES = 0 OR WS-CR-LINES = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLONESID'          TO CA-REASON-CODE
              MOVE 'A posting needs at least one debit and one credit'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-GLP-006 Debits equal credits
           IF WS-DR-TOTAL NOT = WS-CR-TOTAL
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'GLUNBAL '          TO CA-REASON-CODE
              MOVE WS-DR-TOTAL         TO WS-AMT-DISP-1
              MOVE WS-CR-TOTAL         TO WS-AMT-DISP-2
              STRING 'Posting out of balance DR ' WS-AMT-DISP-1
                     ' CR ' WS-AMT-DISP-2
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-GLP-007 Business date from the header or BIL_CONTROL
       3300-GET-BUSINESS-DATE.
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
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE CTL-BUSINESS-DATE TO WS-BUS-DATE
                                              CA-BUS-DATE
                 WHEN 100
                    SET CA-RC-NOT-FOUND TO TRUE
                    MOVE 'CTLNOTFD'    TO CA-REASON-CODE
                    MOVE 'BIL_CONTROL SYSTEM row missing'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 WHEN OTHER
                    MOVE '3300-GET-BUSINESS-DATE'
                                       TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
       4000-INSERT-TRANSACTION.
           MOVE SQC-SQ-VALUE           TO TXN-TXN-ID
                                          CA-GL-TXN-ID
           MOVE CA-GL-ACCOUNT-ID       TO TXN-ACCOUNT-ID
           MOVE CA-GL-PRODUCER-ID      TO TXN-PRODUCER-ID
           MOVE CA-GL-TXN-TYPE         TO TXN-TXN-TYPE
           MOVE WS-DR-TOTAL            TO TXN-TXN-AMOUNT
           MOVE CA-GL-REF-TYPE         TO TXN-REF-TYPE
           MOVE CA-GL-REF-ID           TO TXN-REF-ID
           MOVE WS-BUS-DATE            TO TXN-BUSINESS-DATE
           MOVE WS-REVERSAL-OF         TO TXN-REVERSAL-OF-TXN
           MOVE CA-USER-ID             TO TXN-CREATE-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_TRANSACTION
                 (TXN_ID, ACCOUNT_ID, PRODUCER_ID, TXN_TYPE,
                  TXN_AMOUNT, REF_TYPE, REF_ID, BUSINESS_DATE,
                  REVERSAL_OF_TXN, CREATE_USER, CREATE_TS)
              VALUES
                 (:TXN-TXN-ID, :TXN-ACCOUNT-ID, :TXN-PRODUCER-ID,
                  :TXN-TXN-TYPE, :TXN-TXN-AMOUNT, :TXN-REF-TYPE,
                  :TXN-REF-ID, :TXN-BUSINESS-DATE,
                  :TXN-REVERSAL-OF-TXN, :TXN-CREATE-USER,
                  CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'GLDUPTXN'       TO CA-REASON-CODE
                 MOVE 'Transaction id already used - check sequence'
                    & ' TXN'           TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-INSERT-TRANSACTION' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-LINES.
           MOVE TXN-TXN-ID             TO LGL-TXN-ID
           MOVE CA-GL-ACCOUNT-ID       TO LGL-ACCOUNT-ID
           MOVE WS-BUS-DATE            TO LGL-BUSINESS-DATE
           PERFORM 4110-INSERT-LINE
                   VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > CA-GL-LINE-COUNT.

       4110-INSERT-LINE.
           MOVE WS-LX                  TO LGL-LINE-SEQ
           MOVE CA-GL-T-ACCOUNT(WS-LX) TO LGL-T-ACCOUNT
           MOVE CA-GL-DR-CR(WS-LX)     TO LGL-DR-CR
           MOVE CA-GL-AMOUNT(WS-LX)    TO LGL-LINE-AMOUNT
           EXEC SQL
              INSERT INTO BILDB.BIL_LEDGER_LINE
                 (TXN_ID, LINE_SEQ, T_ACCOUNT, DR_CR,
                  LINE_AMOUNT, ACCOUNT_ID, BUSINESS_DATE)
              VALUES
                 (:LGL-TXN-ID, :LGL-LINE-SEQ, :LGL-T-ACCOUNT,
                  :LGL-DR-CR, :LGL-LINE-AMOUNT, :LGL-ACCOUNT-ID,
                  :LGL-BUSINESS-DATE)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4110-INSERT-LINE'  TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-GLP-011 Lock the original so two reversers serialise
       4200-READ-ORIGINAL.
           MOVE WS-ORIG-TXN-ID         TO TXN-TXN-ID
           EXEC SQL
              SELECT ACCOUNT_ID, PRODUCER_ID, TXN_TYPE, TXN_AMOUNT,
                     REF_TYPE, REF_ID, BUSINESS_DATE,
                     REVERSAL_OF_TXN
                INTO :TXN-ACCOUNT-ID, :TXN-PRODUCER-ID,
                     :TXN-TXN-TYPE, :TXN-TXN-AMOUNT,
                     :TXN-REF-TYPE, :TXN-REF-ID,
                     :TXN-BUSINESS-DATE, :TXN-REVERSAL-OF-TXN
                FROM BILDB.BIL_TRANSACTION
               WHERE TXN_ID = :TXN-TXN-ID
                WITH RS USE AND KEEP EXCLUSIVE LOCKS
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'GLTXNNF '       TO CA-REASON-CODE
                 MOVE WS-ORIG-TXN-ID   TO WS-ID-DISP
                 STRING 'Transaction ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Transaction locked by another user - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-READ-ORIGINAL' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-GLP-011 Only one reversal per transaction
       4300-CHECK-NOT-REVERSED.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-PRIOR-REVERSALS
                FROM BILDB.BIL_TRANSACTION
               WHERE REVERSAL_OF_TXN = :WS-ORIG-TXN-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-CHECK-NOT-REVERSED' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-PRIOR-REVERSALS > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'GLALRREV'          TO CA-REASON-CODE
              MOVE WS-ORIG-TXN-ID      TO WS-ID-DISP
              STRING 'Transaction ' WS-ID-DISP
                     ' has already been reversed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-GLP-009 Mirror lines: same T-account, side swapped
       4400-LOAD-MIRROR-LINES.
           MOVE 0                      TO CA-GL-LINE-COUNT
           EXEC SQL OPEN CSR-ORIG-LINES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-LOAD-MIRROR-LINES' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4410-FETCH-LINE UNTIL SQLCODE = 100
           EXEC SQL CLOSE CSR-ORIG-LINES END-EXEC.

       4410-FETCH-LINE.
           EXEC SQL
              FETCH CSR-ORIG-LINES
               INTO :LGL-LINE-SEQ, :LGL-T-ACCOUNT, :LGL-DR-CR,
                    :LGL-LINE-AMOUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CA-GL-LINE-COUNT >= 8
                    EXEC SQL CLOSE CSR-ORIG-LINES END-EXEC
                    SET CA-RC-STATE    TO TRUE
                    MOVE 'GLTOOMNY'    TO CA-REASON-CODE
                    MOVE 'Original has more than 8 lines - reverse'
                       & ' manually'   TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
                 ADD 1                 TO CA-GL-LINE-COUNT
                 MOVE CA-GL-LINE-COUNT TO WS-LX
                 MOVE LGL-T-ACCOUNT    TO CA-GL-T-ACCOUNT(WS-LX)
                 MOVE LGL-LINE-AMOUNT  TO CA-GL-AMOUNT(WS-LX)
                 IF LGL-DR-CR = 'D'
                    MOVE 'C'           TO CA-GL-DR-CR(WS-LX)
                 ELSE
                    MOVE 'D'           TO CA-GL-DR-CR(WS-LX)
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE SQLCODE          TO WS-SAVE-SQLCODE
                 EXEC SQL CLOSE CSR-ORIG-LINES END-EXEC
                 MOVE WS-SAVE-SQLCODE  TO SQLCODE
                 MOVE '4410-FETCH-LINE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Reversal header: owner ids always from the original; the
      * caller may supply its own transaction type and reference
       5000-BUILD-REVERSAL-HEADER.
           MOVE TXN-ACCOUNT-ID         TO CA-GL-ACCOUNT-ID
           MOVE TXN-PRODUCER-ID        TO CA-GL-PRODUCER-ID
           IF CA-GL-TXN-TYPE = SPACES
              MOVE TXN-TXN-TYPE        TO CA-GL-TXN-TYPE
           END-IF
           IF CA-GL-REF-TYPE = SPACES
              MOVE TXN-REF-TYPE        TO CA-GL-REF-TYPE
              MOVE TXN-REF-ID          TO CA-GL-REF-ID
           END-IF.

      ******************************************************************
       5100-FIND-TACCOUNT.
           MOVE SPACE                  TO WS-TACCT-CLASS-FOUND
           PERFORM VARYING WS-TX FROM 1 BY 1
                   UNTIL WS-TX > WS-TACCT-COUNT
                      OR WS-TACCT-CLASS-FOUND NOT = SPACE
              IF WS-TACCT-NAME(WS-TX) = CA-GL-T-ACCOUNT(WS-LX)
                 MOVE WS-TACCT-CLASS(WS-TX) TO WS-TACCT-CLASS-FOUND
              END-IF
           END-PERFORM.

      ******************************************************************
      * BR-BIL-GLP-013 Transaction key from sequence TXN
       8000-GET-TXN-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'TXN     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE > 4
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
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
           STRING 'BILGLPST Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE SPACES                 TO ERC-ER-KEY
           STRING CA-GL-TXN-TYPE '/' CA-GL-REF-TYPE
                  DELIMITED BY SIZE INTO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
