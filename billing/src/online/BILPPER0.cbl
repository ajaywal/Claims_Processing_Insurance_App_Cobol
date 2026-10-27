       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILPPER0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILPPER0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Policy period and billing instruction service.   *
      *               Creates new-business and renewal periods with    *
      *               their premium, tax and installment-fee charges,  *
      *               bills mid-term changes pro rata, cancels with a  *
      *               pro-rata return-premium credit, reinstates,      *
      *               changes the payment plan and returns a period    *
      *               with its invoice item schedule.                  *
      * REQUESTS    : PPNEW   new business / renewal period            *
      *               PPCHG   mid-term premium change                  *
      *               PPCAN   cancellation                             *
      *               PPRIN   reinstatement                            *
      *               PPINQ   inquiry with item schedule               *
      *               PPPLN   payment plan change                      *
      * CALLED BY   : BILEVTDR (POLISSUE POLCHNG POLCANC), BILPPRP     *
      *               screen, BILDLQSV (cancel for non-payment)        *
      * CALLS       : BILSEQGN BILDATES BILMONEY BILISCHD BILCOMSV     *
      *               BILGLPST BILAUDIT BILAUTHZ BILPROD0 BILCODVL     *
      *               BILACCT0 BILERRLG                                *
      * TABLES      : BIL_POLICY_PERIOD     (C R U)                    *
      *               BIL_CHARGE            (C R U)                    *
      *               BIL_INVOICE_ITEM      (R U)                      *
      *               BIL_COMMISSION_ITEM   (R)                        *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_INVOICE_STREAM    (R)                        *
      *               BIL_PAYMENT_PLAN      (R)                        *
      *               BIL_DELINQ_PLAN       (R)                        *
      *               BIL_CONTROL           (R)                        *
      * RULES       : BR-BIL-PPR-001 A new period needs an existing,   *
      *                              active (ACTV) account             *
      *               BR-BIL-PPR-002 Product is M, H, C or E; billing  *
      *                              method DB (direct) or AB (agency) *
      *               BR-BIL-PPR-003 Expiry after effective date and   *
      *                              the term at most 18 months        *
      *               BR-BIL-PPR-004 Payment plan must exist and be    *
      *                              active                            *
      *               BR-BIL-PPR-005 Producer resolved from the GenApp *
      *                              broker id must be ACTV; agency    *
      *                              bill needs an agency-bill         *
      *                              producer                          *
      *               BR-BIL-PPR-006 Term = previous term + 1 for the  *
      *                              policy: first term NEWBIZ, later  *
      *                              RENEWAL; no overlap with a prior  *
      *                              term that is not cancelled        *
      *               BR-BIL-PPR-007 Period bills on the account's     *
      *                              active stream of the plan's       *
      *                              frequency, else its first active  *
      *                              stream                            *
      *               BR-BIL-PPR-008 Insurance Premium Tax is 12% of   *
      *                              premium, charged as TAX           *
      *               BR-BIL-PPR-009 Installment fee = plan fee x      *
      *                              (installments - 1)                *
      *               BR-BIL-PPR-010 Charges post DR UNBILLED / CR     *
      *                              PREMREV (premium) and CR FEEREV   *
      *                              (tax, fees); credits the reverse  *
      *               BR-BIL-PPR-011 Commission is reserved on every   *
      *                              premium item of a producer-sold   *
      *                              period and released on items      *
      *                              that are cancelled                *
      *               BR-BIL-PPR-012 Mid-term change: the full-term    *
      *                              delta is pro-rated from the       *
      *                              change date to expiry (POLCHG)    *
      *               BR-BIL-PPR-013 Change and cancel dates fall      *
      *                              within the period                 *
      *               BR-BIL-PPR-014 Cancel only from INFC with a      *
      *                              valid CANCRSN reason; unearned    *
      *                              premium and its tax are returned  *
      *                              pro rata as CANCEL credits        *
      *               BR-BIL-PPR-015 Planned installments wholly       *
      *                              offset by the credit are          *
      *                              cancelled and their installment   *
      *                              fees waived                       *
      *               BR-BIL-PPR-016 An account left in credit by a    *
      *                              cancellation is flagged (RC 04    *
      *                              ACCTCRED) for the batch unapplied *
      *                              / disbursement path               *
      *               BR-BIL-PPR-017 Reinstatement only from CANC      *
      *                              within 30 days of the cancel      *
      *                              date: cancel credits reversed     *
      *                              (REINST) plus the delinquency     *
      *                              plan REINSFEE                     *
      *               BR-BIL-PPR-018 Plan change only for INFC to a    *
      *                              different active plan, within     *
      *                              PLANCHG authority for the         *
      *                              unbilled amount, audited          *
      *               BR-BIL-PPR-019 Period updates are guarded by     *
      *                              VERSION_NO                        *
      *               BR-BIL-PPR-020 A period is found by PP-ID, else  *
      *                              policy number + term (latest term *
      *                              when the term is zero)            *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILPPER0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'BILMONEY'.
       01  WS-SCH-PGM                  PIC X(8)  VALUE 'BILISCHD'.
       01  WS-CMS-PGM                  PIC X(8)  VALUE 'BILCOMSV'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-AUT-PGM                  PIC X(8)  VALUE 'BILAUTHZ'.
       01  WS-PRD-PGM                  PIC X(8)  VALUE 'BILPROD0'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'BILCODVL'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-IPT-RATE-BPS          PIC S9(4) COMP VALUE 1200.
           05 WS-MAX-TERM-MONTHS       PIC S9(4) COMP VALUE 18.
           05 WS-REINSTATE-DAYS        PIC S9(4) COMP VALUE 30.
           05 WS-MAX-ITEMS             PIC S9(4) COMP VALUE 24.
           05 WS-BATCH-SIZE            PIC S9(4) COMP VALUE 50.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-BILLING-INSTR         PIC X(8)  VALUE SPACES.
           05 WS-MAX-TERM              PIC S9(4) COMP VALUE 0.
           05 WS-PREMIUM               PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-TAX                   PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-FEE                   PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-WAIVED                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-UNBILLED              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-NET-BALANCE           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PREMIUM-CHARGE-ID     PIC S9(9) COMP VALUE 0.
           05 WS-DAYS-PART             PIC S9(5) COMP-3 VALUE 0.
           05 WS-DAYS-WHOLE            PIC S9(5) COMP-3 VALUE 0.
           05 WS-OLD-PLAN              PIC X(8)  VALUE SPACES.
           05 WS-OLD-STATUS            PIC X(4)  VALUE SPACES.
           05 WS-PRIOR-EXP-DATE        PIC X(10) VALUE SPACES.
           05 WS-PRIOR-STATUS          PIC X(4)  VALUE SPACES.
           05 WS-OLD-CANCEL-DATE       PIC X(10) VALUE SPACES.
           05 WS-LAST-ITEM-ID          PIC S9(9) COMP VALUE 0.
           05 WS-IX                    PIC S9(4) COMP VALUE 0.
           05 WS-ROWS                  PIC S9(4) COMP VALUE 0.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-POL-DISP              PIC 9(10).
           05 WS-TERM-DISP             PIC 9(3).
           05 WS-AMT-DISP              PIC -(9)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-OPEN-CURSOR           PIC X(8)  VALUE SPACES.
              88 WS-NO-CURSOR                    VALUE SPACES.

      * Date / money helper fields
       01  WS-CALC.
           05 WS-D-IN                  PIC X(10) VALUE SPACES.
           05 WS-D-IN-2                PIC X(10) VALUE SPACES.
           05 WS-D-OUT                 PIC X(10) VALUE SPACES.
           05 WS-D-NUM                 PIC S9(5) COMP-3 VALUE 0.
           05 WS-D-RESULT              PIC S9(7) COMP-3 VALUE 0.
           05 WS-MN-TOTAL              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-MN-RESULT             PIC S9(11)V99 COMP-3 VALUE 0.

      * Charge being created (8500-CREATE-CHARGE)
       01  WS-NEW-CHARGE.
           05 WS-CH-PATTERN            PIC X(8).
           05 WS-CH-INSTR              PIC X(8).
           05 WS-CH-AMOUNT             PIC S9(11)V99 COMP-3.
           05 WS-CH-EFF-DATE           PIC X(10).
           05 WS-CH-ONE-TIME           PIC X.

      * Ledger components (8300-POST-CHARGE-LEDGER)
       01  WS-LEDGER.
           05 WS-GL-TXN-TYPE           PIC X(8)  VALUE SPACES.
           05 WS-GL-PREMREV            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-GL-FEEREV             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-GL-NET                PIC S9(11)V99 COMP-3 VALUE 0.

      * Audit image (8400-WRITE-AUDIT)
       01  WS-AUDIT.
           05 WS-AU-ACTION             PIC X(4)  VALUE SPACES.
           05 WS-AU-FIELD              PIC X(18) VALUE SPACES.
           05 WS-AU-BEFORE             PIC X(100) VALUE SPACES.
           05 WS-AU-AFTER              PIC X(100) VALUE SPACES.

       01  WS-CALLEE.
           05 WS-CALLEE-RC             PIC 9(2).
           05 WS-CALLEE-REASON         PIC X(8).
           05 WS-CALLEE-MESSAGE        PIC X(80).

      * Ids / dates loaded from a cursor before services are called
       01  WS-ID-TABLE.
           05 WS-ID-COUNT              PIC S9(4) COMP VALUE 0.
           05 WS-ID-ENTRY OCCURS 50 TIMES.
              10 WS-ID-VALUE           PIC S9(9) COMP.
              10 WS-ID-PATTERN         PIC X(8).
              10 WS-ID-AMOUNT          PIC S9(11)V99 COMP-3.
              10 WS-ID-DATE            PIC X(10).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDIST END-EXEC.
           EXEC SQL INCLUDE BILDPPL END-EXEC.
           EXEC SQL INCLUDE BILDDPL END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * Item schedule of a period for the response
           EXEC SQL
              DECLARE C-ITEMS CURSOR FOR
               SELECT ITEM_ID, ITEM_TYPE, INSTALL_NUMBER,
                      EVENT_DATE, DUE_DATE, ITEM_AMOUNT,
                      PAID_AMOUNT, ITEM_STATUS
                 FROM BILDB.BIL_INVOICE_ITEM
                WHERE PP_ID        = :PPR-PP-ID
                  AND ITEM_STATUS <> 'CANC'
                ORDER BY DUE_DATE, PRIORITY_RANK, ITEM_ID
                FOR FETCH ONLY
           END-EXEC.

      * Planned premium items still without commission
           EXEC SQL
              DECLARE C-UNCOMM CURSOR FOR
               SELECT I.ITEM_ID
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_CHARGE C
                   ON C.CHARGE_ID = I.CHARGE_ID
                WHERE I.PP_ID          = :PPR-PP-ID
                  AND I.ITEM_STATUS    = 'PLAN'
                  AND I.ITEM_ID        > :WS-LAST-ITEM-ID
                  AND C.CHARGE_PATTERN = 'PREMIUM'
                  AND NOT EXISTS
                      (SELECT 1
                         FROM BILDB.BIL_COMMISSION_ITEM M
                        WHERE M.ITEM_ID   = I.ITEM_ID
                          AND M.CI_STATE IN ('RSRV', 'EARN'))
                ORDER BY I.ITEM_ID
                FETCH FIRST 50 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

      * Cancelled items still carrying live commission
           EXEC SQL
              DECLARE C-RELEASE CURSOR FOR
               SELECT I.ITEM_ID
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_COMMISSION_ITEM M
                   ON M.ITEM_ID = I.ITEM_ID
                WHERE I.PP_ID       = :PPR-PP-ID
                  AND I.ITEM_STATUS = 'CANC'
                  AND I.ITEM_ID     > :WS-LAST-ITEM-ID
                  AND M.CI_STATE IN ('RSRV', 'EARN')
                ORDER BY I.ITEM_ID
                FETCH FIRST 50 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-PPR-015 Due dates whose planned premium and tax net to
      * zero once the cancellation credit has been spread
           EXEC SQL
              DECLARE C-NETZERO CURSOR FOR
               SELECT I.DUE_DATE
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_CHARGE C
                   ON C.CHARGE_ID = I.CHARGE_ID
                WHERE I.PP_ID          = :PPR-PP-ID
                  AND I.ITEM_STATUS    = 'PLAN'
                  AND C.CHARGE_PATTERN IN ('PREMIUM', 'TAX')
                GROUP BY I.DUE_DATE
               HAVING SUM(I.ITEM_AMOUNT) = 0
                ORDER BY I.DUE_DATE
                FETCH FIRST 50 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

      * Cancellation credits not yet reversed
           EXEC SQL
              DECLARE C-CANCHG CURSOR FOR
               SELECT CHARGE_ID, CHARGE_PATTERN, CHARGE_AMOUNT
                 FROM BILDB.BIL_CHARGE
                WHERE PP_ID          = :PPR-PP-ID
                  AND BILLING_INSTR  = 'CANCEL'
                  AND CHARGE_STATUS <> 'REVS'
                ORDER BY CHARGE_ID
                FETCH FIRST 50 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-MNY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY BILCMNY REPLACING LEADING ==CA-== BY ==MNA-==.
       01  WS-SCH-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SCA-==.
           03 SCA-BODY.
              COPY BILCSCH REPLACING LEADING ==CA-== BY ==SCA-==.
       01  WS-CMS-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CMA-==.
           03 CMA-BODY.
              COPY BILCCMS REPLACING LEADING ==CA-== BY ==CMA-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.
       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.
       01  WS-AUT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ATA-==.
           03 ATA-BODY.
              COPY BILCAUT REPLACING LEADING ==CA-== BY ==ATA-==.
       01  WS-PRD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PRA-==.
           03 PRA-BODY.
              COPY BILCPRD REPLACING LEADING ==CA-== BY ==PRA-==.
       01  WS-COD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY BILCCOD REPLACING LEADING ==CA-== BY ==CDA-==.
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
              COPY BILCPPR.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'PPNEW   '
                 PERFORM 2000-NEW-PERIOD
              WHEN 'PPCHG   '
                 PERFORM 2100-CHANGE-PERIOD
              WHEN 'PPCAN   '
                 PERFORM 2200-CANCEL-PERIOD
              WHEN 'PPRIN   '
                 PERFORM 2300-REINSTATE-PERIOD
              WHEN 'PPINQ   '
                 PERFORM 2400-INQUIRE-PERIOD
              WHEN 'PPPLN   '
                 PERFORM 2500-CHANGE-PLAN
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILPPER0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-PP-ITEM-COUNT
                                          CA-PP-CHARGE-ID
           MOVE SPACES                 TO WS-OPEN-CURSOR
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO CA-BUS-DATE
           END-IF
           MOVE CA-BUS-DATE            TO WS-BUS-DATE.

      ******************************************************************
      * PPNEW - new business or renewal period
       2000-NEW-PERIOD.
           PERFORM 3000-VALIDATE-NEW-PERIOD
           PERFORM 2010-INSERT-PERIOD
      * Premium
           MOVE CA-PP-TOTAL-PREMIUM    TO WS-PREMIUM
           MOVE 'PREMIUM '             TO WS-CH-PATTERN
           MOVE WS-PREMIUM             TO WS-CH-AMOUNT
           PERFORM 2020-ADD-PERIOD-CHARGE
           MOVE CHG-CHARGE-ID          TO WS-PREMIUM-CHARGE-ID
      * BR-BIL-PPR-008 Insurance Premium Tax at 12%
           MOVE WS-PREMIUM             TO WS-MN-TOTAL
           PERFORM 8210-APPLY-IPT
           MOVE WS-MN-RESULT           TO WS-TAX
           IF WS-TAX NOT = 0
              MOVE 'TAX     '          TO WS-CH-PATTERN
              MOVE WS-TAX              TO WS-CH-AMOUNT
              PERFORM 2020-ADD-PERIOD-CHARGE
           END-IF
      * BR-BIL-PPR-009 Installment fee on installments 2..N
           MOVE 0                      TO WS-FEE
           IF PPL-NUM-INSTALLMENTS > 1 AND PPL-INSTALL-FEE > 0
              COMPUTE WS-FEE = PPL-INSTALL-FEE
                             * (PPL-NUM-INSTALLMENTS - 1)
              MOVE 'INSTFEE '          TO WS-CH-PATTERN
              MOVE WS-FEE              TO WS-CH-AMOUNT
              PERFORM 2020-ADD-PERIOD-CHARGE
           END-IF
           MOVE 'PPNEW   '             TO WS-GL-TXN-TYPE
           MOVE WS-PREMIUM             TO WS-GL-PREMREV
           COMPUTE WS-GL-FEEREV = WS-TAX + WS-FEE
           PERFORM 8300-POST-CHARGE-LEDGER
           PERFORM 8600-RESERVE-COMMISSIONS
           PERFORM 8700-REFRESH-BALANCES
           MOVE 'ADD '                 TO WS-AU-ACTION
           MOVE SPACES                 TO WS-AU-FIELD WS-AU-BEFORE
                                          WS-AU-AFTER
           MOVE WS-PREMIUM             TO WS-AMT-DISP
           STRING WS-BILLING-INSTR ' ' PPR-PAYMENT-PLAN-CODE ' '
                  WS-AMT-DISP DELIMITED BY SIZE INTO WS-AU-AFTER
           PERFORM 8400-WRITE-AUDIT
           MOVE WS-TAX                 TO CA-PP-TAX-AMOUNT
           MOVE WS-PREMIUM-CHARGE-ID   TO CA-PP-CHARGE-ID
           PERFORM 2900-RETURN-PERIOD.

      ******************************************************************
       2010-INSERT-PERIOD.
           PERFORM 8000-NEXT-SEQUENCE-PPR
           MOVE SQA-SQ-VALUE           TO PPR-PP-ID
           MOVE CA-PP-ACCOUNT-ID       TO PPR-ACCOUNT-ID
           MOVE CA-PP-GENAPP-POL-NUM   TO PPR-GENAPP-POL-NUM
           COMPUTE PPR-TERM-NUMBER = WS-MAX-TERM + 1
           MOVE CA-PP-PRODUCT-CODE     TO PPR-PRODUCT-CODE
           MOVE CA-PP-EFF-DATE         TO PPR-EFF-DATE
           MOVE CA-PP-EXP-DATE         TO PPR-EXP-DATE
           MOVE CA-PP-PLAN-CODE        TO PPR-PAYMENT-PLAN-CODE
           MOVE CA-PP-BILLING-METHOD   TO PPR-BILLING-METHOD
           MOVE IST-STREAM-ID          TO PPR-STREAM-ID
           MOVE CA-PP-TOTAL-PREMIUM    TO PPR-TOTAL-PREMIUM
           MOVE 'INFC'                 TO PPR-PP-STATUS
           MOVE 'OPEN'                 TO PPR-CLOSURE-STATUS
           MOVE 1                      TO PPR-VERSION-NO
           PERFORM 4110-INSERT-PERIOD.

      ******************************************************************
      * Charge on the period with the request's billing instruction
       2020-ADD-PERIOD-CHARGE.
           MOVE WS-BILLING-INSTR       TO WS-CH-INSTR
           MOVE PPR-EFF-DATE           TO WS-CH-EFF-DATE
           MOVE 'N'                    TO WS-CH-ONE-TIME
           PERFORM 8500-CREATE-CHARGE.

      ******************************************************************
      * PPCHG - mid-term premium change
       2100-CHANGE-PERIOD.
           PERFORM 3050-RESOLVE-PERIOD
           PERFORM 3100-REQUIRE-IN-FORCE
           IF CA-PP-CHANGE-AMOUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CHGAMT  '          TO CA-REASON-CODE
              MOVE 'Change amount (full-term premium delta) required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3200-VALIDATE-PERIOD-DATE
      * BR-BIL-PPR-012 Pro-rate the delta from change date to expiry
           MOVE CA-PP-CHANGE-EFF-DATE  TO WS-D-IN
           MOVE PPR-EXP-DATE           TO WS-D-IN-2
           PERFORM 8140-DIFF-DAYS
           MOVE WS-D-RESULT            TO WS-DAYS-PART
           MOVE PPR-EFF-DATE           TO WS-D-IN
           PERFORM 8140-DIFF-DAYS
           MOVE WS-D-RESULT            TO WS-DAYS-WHOLE
           MOVE CA-PP-CHANGE-AMOUNT    TO WS-MN-TOTAL
           PERFORM 8220-PRORATE
           MOVE WS-MN-RESULT           TO WS-PREMIUM
           IF WS-PREMIUM = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'CHGNIL  '          TO CA-REASON-CODE
              MOVE 'Pro-rated change is zero - nothing billed'
                                       TO CA-MESSAGE
              PERFORM 2900-RETURN-PERIOD
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-PREMIUM             TO WS-MN-TOTAL
           PERFORM 8210-APPLY-IPT
           MOVE WS-MN-RESULT           TO WS-TAX
           MOVE 'POLCHG  '             TO WS-CH-INSTR
           MOVE CA-PP-CHANGE-EFF-DATE  TO WS-CH-EFF-DATE
           MOVE 'N'                    TO WS-CH-ONE-TIME
           MOVE 'PREMIUM '             TO WS-CH-PATTERN
           MOVE WS-PREMIUM             TO WS-CH-AMOUNT
           PERFORM 8500-CREATE-CHARGE
           MOVE CHG-CHARGE-ID          TO WS-PREMIUM-CHARGE-ID
           IF WS-TAX NOT = 0
              MOVE 'TAX     '          TO WS-CH-PATTERN
              MOVE WS-TAX              TO WS-CH-AMOUNT
              PERFORM 8500-CREATE-CHARGE
           END-IF
           MOVE PPR-TOTAL-PREMIUM      TO WS-AMT-DISP
           MOVE WS-AMT-DISP            TO WS-AU-BEFORE
           ADD WS-PREMIUM              TO PPR-TOTAL-PREMIUM
           PERFORM 4200-UPDATE-PERIOD
           MOVE 'POLCHG  '             TO WS-GL-TXN-TYPE
           MOVE WS-PREMIUM             TO WS-GL-PREMREV
           MOVE WS-TAX                 TO WS-GL-FEEREV
           PERFORM 8300-POST-CHARGE-LEDGER
           PERFORM 8600-RESERVE-COMMISSIONS
           PERFORM 8700-REFRESH-BALANCES
           MOVE 'UPD '                 TO WS-AU-ACTION
           MOVE 'TOTAL_PREMIUM'        TO WS-AU-FIELD
           MOVE PPR-TOTAL-PREMIUM      TO WS-AMT-DISP
           MOVE WS-AMT-DISP            TO WS-AU-AFTER
           PERFORM 8400-WRITE-AUDIT
           MOVE WS-TAX                 TO CA-PP-TAX-AMOUNT
           MOVE WS-PREMIUM-CHARGE-ID   TO CA-PP-CHARGE-ID
           PERFORM 2900-RETURN-PERIOD.

      ******************************************************************
      * PPCAN - cancellation with pro-rata return premium
       2200-CANCEL-PERIOD.
           PERFORM 3050-RESOLVE-PERIOD
           PERFORM 3100-REQUIRE-IN-FORCE
           PERFORM 3300-VALIDATE-CANCEL-REASON
           PERFORM 3200-VALIDATE-PERIOD-DATE
           IF CA-PP-CHANGE-EFF-DATE >= PPR-EXP-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CANDATE '          TO CA-REASON-CODE
              MOVE 'Cancel date must be before the expiry date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PPR-014 Unearned premium from cancel date to expiry
           MOVE CA-PP-CHANGE-EFF-DATE  TO WS-D-IN
           MOVE PPR-EXP-DATE           TO WS-D-IN-2
           PERFORM 8140-DIFF-DAYS
           MOVE WS-D-RESULT            TO WS-DAYS-PART
           MOVE PPR-EFF-DATE           TO WS-D-IN
           PERFORM 8140-DIFF-DAYS
           MOVE WS-D-RESULT            TO WS-DAYS-WHOLE
           MOVE PPR-TOTAL-PREMIUM      TO WS-MN-TOTAL
           PERFORM 8220-PRORATE
           MOVE WS-MN-RESULT           TO WS-PREMIUM
           MOVE WS-PREMIUM             TO WS-MN-TOTAL
           PERFORM 8210-APPLY-IPT
           MOVE WS-MN-RESULT           TO WS-TAX
           MOVE 0                      TO WS-WAIVED
           IF WS-PREMIUM > 0
              MOVE 'CANCEL  '          TO WS-CH-INSTR
              MOVE CA-PP-CHANGE-EFF-DATE TO WS-CH-EFF-DATE
              MOVE 'N'                 TO WS-CH-ONE-TIME
              MOVE 'PREMIUM '          TO WS-CH-PATTERN
              COMPUTE WS-CH-AMOUNT = 0 - WS-PREMIUM
              PERFORM 8500-CREATE-CHARGE
              MOVE CHG-CHARGE-ID       TO WS-PREMIUM-CHARGE-ID
              IF WS-TAX > 0
                 MOVE 'TAX     '       TO WS-CH-PATTERN
                 COMPUTE WS-CH-AMOUNT = 0 - WS-TAX
                 PERFORM 8500-CREATE-CHARGE
              END-IF
              PERFORM 2210-CANCEL-OFFSET-INSTALMENTS
           END-IF
           MOVE PPR-PP-STATUS          TO WS-OLD-STATUS
           MOVE 'CANC'                 TO PPR-PP-STATUS
           MOVE CA-PP-CHANGE-EFF-DATE  TO PPR-CANCEL-DATE
           MOVE CA-PP-CANCEL-REASON    TO PPR-CANCEL-REASON
           MOVE 0                      TO PPR-CANCEL-DATE-IND
                                          PPR-CANCEL-REASON-IND
           SUBTRACT WS-PREMIUM         FROM PPR-TOTAL-PREMIUM
           PERFORM 4200-UPDATE-PERIOD
           PERFORM 8610-RELEASE-COMMISSIONS
           PERFORM 8600-RESERVE-COMMISSIONS
           MOVE 'PPCAN   '             TO WS-GL-TXN-TYPE
           COMPUTE WS-GL-PREMREV = 0 - WS-PREMIUM
           COMPUTE WS-GL-FEEREV = 0 - WS-TAX - WS-WAIVED
           PERFORM 8300-POST-CHARGE-LEDGER
           PERFORM 8700-REFRESH-BALANCES
           MOVE 'STAT'                 TO WS-AU-ACTION
           MOVE 'PP_STATUS'            TO WS-AU-FIELD
           MOVE WS-OLD-STATUS          TO WS-AU-BEFORE
           MOVE SPACES                 TO WS-AU-AFTER
           STRING 'CANC ' CA-PP-CANCEL-REASON ' '
                  CA-PP-CHANGE-EFF-DATE DELIMITED BY SIZE
                  INTO WS-AU-AFTER
           PERFORM 8400-WRITE-AUDIT
           COMPUTE CA-PP-TAX-AMOUNT = 0 - WS-TAX
           COMPUTE CA-PP-CHANGE-AMOUNT = 0 - WS-PREMIUM
           MOVE WS-PREMIUM-CHARGE-ID   TO CA-PP-CHARGE-ID
           PERFORM 2900-RETURN-PERIOD
      * BR-BIL-PPR-016 Flag an account the cancellation left in credit
           COMPUTE WS-NET-BALANCE = ACA-AC-BILLED-AMT + ACA-AC-DUE-AMT
                                  + ACA-AC-UNBILLED-AMT
                                  - ACA-AC-UNAPPLIED-AMT
           IF WS-NET-BALANCE < 0 AND CA-RC-OK
              SET CA-RC-WARNING        TO TRUE
              MOVE 'ACCTCRED'          TO CA-REASON-CODE
              COMPUTE WS-AMT-DISP = 0 - WS-NET-BALANCE
              STRING 'Account in credit ' WS-AMT-DISP
                     ' - refund handled by batch'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-BIL-PPR-015 Cancel installments the credit wholly offsets,
      * with their installment fees (waived)
       2210-CANCEL-OFFSET-INSTALMENTS.
           MOVE 'NETZERO '             TO WS-OPEN-CURSOR
           EXEC SQL OPEN C-NETZERO END-EXEC
           PERFORM 4500-CHECK-OPEN
           MOVE 0                      TO WS-ID-COUNT
           PERFORM UNTIL WS-ID-COUNT >= WS-BATCH-SIZE
              EXEC SQL
                 FETCH C-NETZERO INTO :ITM-DUE-DATE
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-ID-COUNT
                    MOVE ITM-DUE-DATE  TO WS-ID-DATE(WS-ID-COUNT)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '2210-CANCEL-OFFSET' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4590-CLOSE-CURSOR
           PERFORM VARYING WS-IX FROM 1 BY 1
                     UNTIL WS-IX > WS-ID-COUNT
              MOVE WS-ID-DATE(WS-IX)   TO ITM-DUE-DATE
              PERFORM 4300-CANCEL-DUE-DATE-ITEMS
           END-PERFORM.

      ******************************************************************
      * PPRIN - reinstatement within the window after cancellation
       2300-REINSTATE-PERIOD.
           PERFORM 3050-RESOLVE-PERIOD
           IF PPR-PP-STATUS NOT = 'CANC'
              SET CA-RC-STATE          TO TRUE
              MOVE 'PPNOTCAN'          TO CA-REASON-CODE
              STRING 'Period status ' PPR-PP-STATUS
                     ' cannot be reinstated' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PPR-017 Only within 30 days of the cancel date
           MOVE PPR-CANCEL-DATE        TO WS-D-IN
           MOVE WS-REINSTATE-DAYS      TO WS-D-NUM
           PERFORM 8110-ADD-DAYS
           IF WS-BUS-DATE > WS-D-OUT
              SET CA-RC-STATE          TO TRUE
              MOVE 'RINWINDW'          TO CA-REASON-CODE
              STRING 'Reinstatement window closed on ' WS-D-OUT
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE PPR-PP-STATUS          TO WS-OLD-STATUS
           MOVE PPR-CANCEL-DATE        TO WS-OLD-CANCEL-DATE
           PERFORM 4400-LOAD-CANCEL-CHARGES
      * Back in force first: recurring charges need an INFC period
           MOVE 'INFC'                 TO PPR-PP-STATUS
           MOVE -1                     TO PPR-CANCEL-DATE-IND
                                          PPR-CANCEL-REASON-IND
           MOVE 0                      TO WS-PREMIUM WS-TAX WS-FEE
           PERFORM VARYING WS-IX FROM 1 BY 1
                     UNTIL WS-IX > WS-ID-COUNT
              IF WS-ID-PATTERN(WS-IX) = 'PREMIUM '
                 SUBTRACT WS-ID-AMOUNT(WS-IX) FROM WS-PREMIUM
              ELSE
                 SUBTRACT WS-ID-AMOUNT(WS-IX) FROM WS-TAX
              END-IF
           END-PERFORM
           ADD WS-PREMIUM              TO PPR-TOTAL-PREMIUM
           PERFORM 4200-UPDATE-PERIOD
           PERFORM VARYING WS-IX FROM 1 BY 1
                     UNTIL WS-IX > WS-ID-COUNT
              PERFORM 2310-REVERSE-CANCEL-CHARGE
           END-PERFORM
           PERFORM 4130-READ-ACCOUNT
           PERFORM 4160-READ-DELINQ-PLAN
           IF DPL-REINSTATE-FEE > 0
              MOVE DPL-REINSTATE-FEE   TO WS-FEE
              MOVE 'REINSFEE'          TO WS-CH-PATTERN
              MOVE 'REINST  '          TO WS-CH-INSTR
              MOVE WS-BUS-DATE         TO WS-CH-EFF-DATE
              MOVE 'Y'                 TO WS-CH-ONE-TIME
              MOVE WS-FEE              TO WS-CH-AMOUNT
              PERFORM 8500-CREATE-CHARGE
           END-IF
           MOVE 'PPRIN   '             TO WS-GL-TXN-TYPE
           MOVE WS-PREMIUM             TO WS-GL-PREMREV
           COMPUTE WS-GL-FEEREV = WS-TAX + WS-FEE
           PERFORM 8300-POST-CHARGE-LEDGER
           PERFORM 8600-RESERVE-COMMISSIONS
           PERFORM 8700-REFRESH-BALANCES
           MOVE 'STAT'                 TO WS-AU-ACTION
           MOVE 'PP_STATUS'            TO WS-AU-FIELD
           MOVE SPACES                 TO WS-AU-BEFORE
           STRING WS-OLD-STATUS ' ' WS-OLD-CANCEL-DATE
                  DELIMITED BY SIZE INTO WS-AU-BEFORE
           MOVE 'INFC'                 TO WS-AU-AFTER
           PERFORM 8400-WRITE-AUDIT
           MOVE WS-TAX                 TO CA-PP-TAX-AMOUNT
           MOVE WS-PREMIUM             TO CA-PP-CHANGE-AMOUNT
           PERFORM 2900-RETURN-PERIOD.

      ******************************************************************
       2310-REVERSE-CANCEL-CHARGE.
           MOVE WS-ID-VALUE(WS-IX)     TO CHG-CHARGE-ID
           PERFORM 4410-MARK-CHARGE-REVERSED
           MOVE WS-ID-PATTERN(WS-IX)   TO WS-CH-PATTERN
           COMPUTE WS-CH-AMOUNT = 0 - WS-ID-AMOUNT(WS-IX)
           MOVE 'REINST  '             TO WS-CH-INSTR
           MOVE WS-BUS-DATE            TO WS-CH-EFF-DATE
           MOVE 'N'                    TO WS-CH-ONE-TIME
           PERFORM 8500-CREATE-CHARGE
           IF WS-ID-PATTERN(WS-IX) = 'PREMIUM '
              MOVE CHG-CHARGE-ID       TO CA-PP-CHARGE-ID
           END-IF.

      ******************************************************************
      * PPINQ - period with its item schedule
       2400-INQUIRE-PERIOD.
           PERFORM 3050-RESOLVE-PERIOD
           PERFORM 4600-SUM-TAX
           MOVE WS-TAX                 TO CA-PP-TAX-AMOUNT
           PERFORM 2900-RETURN-PERIOD.

      ******************************************************************
      * PPPLN - move unbilled items onto another payment plan
       2500-CHANGE-PLAN.
           PERFORM 3050-RESOLVE-PERIOD
           PERFORM 3100-REQUIRE-IN-FORCE
           IF CA-PP-PLAN-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PLANREQ '          TO CA-REASON-CODE
              MOVE 'New payment plan is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PPR-018 A different, active plan
           IF CA-PP-PLAN-CODE = PPR-PAYMENT-PLAN-CODE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PLANSAME'          TO CA-REASON-CODE
              MOVE 'Period is already on this payment plan'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PP-PLAN-CODE        TO PPL-PLAN-CODE
           PERFORM 4150-READ-PLAN
           PERFORM 4610-SUM-UNBILLED
           PERFORM 8800-CHECK-PLAN-AUTHORITY
           MOVE PPR-PAYMENT-PLAN-CODE  TO WS-OLD-PLAN
           INITIALIZE WS-SCH-CA
           MOVE 'SCHRSC  '             TO SCA-REQUEST-ID
           MOVE PPR-PP-ID              TO SCA-SC-PP-ID
           MOVE CA-PP-PLAN-CODE        TO SCA-SC-NEW-PLAN
           PERFORM 8510-CALL-SCHEDULER
           MOVE CA-PP-PLAN-CODE        TO PPR-PAYMENT-PLAN-CODE
           PERFORM 4200-UPDATE-PERIOD
           PERFORM 8610-RELEASE-COMMISSIONS
           PERFORM 8600-RESERVE-COMMISSIONS
           MOVE 'UPD '                 TO WS-AU-ACTION
           MOVE 'PAYMENT_PLAN_CODE'    TO WS-AU-FIELD
           MOVE WS-OLD-PLAN            TO WS-AU-BEFORE
           MOVE CA-PP-PLAN-CODE        TO WS-AU-AFTER
           PERFORM 8400-WRITE-AUDIT
           PERFORM 2900-RETURN-PERIOD.

      ******************************************************************
      * Period header and up to 24 live items back to the caller
       2900-RETURN-PERIOD.
           MOVE PPR-PP-ID              TO CA-PP-PP-ID
           MOVE PPR-ACCOUNT-ID         TO CA-PP-ACCOUNT-ID
           MOVE PPR-GENAPP-POL-NUM     TO CA-PP-GENAPP-POL-NUM
           MOVE PPR-TERM-NUMBER        TO CA-PP-TERM-NUMBER
           MOVE PPR-PRODUCT-CODE       TO CA-PP-PRODUCT-CODE
           MOVE PPR-EFF-DATE           TO CA-PP-EFF-DATE
           MOVE PPR-EXP-DATE           TO CA-PP-EXP-DATE
           MOVE PPR-PAYMENT-PLAN-CODE  TO CA-PP-PLAN-CODE
           MOVE PPR-BILLING-METHOD     TO CA-PP-BILLING-METHOD
           MOVE PPR-TOTAL-PREMIUM      TO CA-PP-TOTAL-PREMIUM
           MOVE PPR-PP-STATUS          TO CA-PP-STATUS
           IF PPR-CANCEL-REASON-IND < 0
              MOVE SPACES              TO CA-PP-CANCEL-REASON
           ELSE
              MOVE PPR-CANCEL-REASON   TO CA-PP-CANCEL-REASON
           END-IF
           PERFORM 4700-LOAD-ITEM-SCHEDULE.

      ******************************************************************
      * BR-BIL-PPR-001..007 New period validation
       3000-VALIDATE-NEW-PERIOD.
           IF CA-PP-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACCTREQ '          TO CA-REASON-CODE
              MOVE 'Billing account id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4130-READ-ACCOUNT
      * BR-BIL-PPR-001 Account must be active
           IF ACT-ACCOUNT-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTNACTV'          TO CA-REASON-CODE
              STRING 'Account status ' ACT-ACCOUNT-STATUS
                     ' cannot take a new policy period'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PP-GENAPP-POL-NUM NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'POLREQ  '          TO CA-REASON-CODE
              MOVE 'GenApp policy number is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PPR-002 Product and billing method
           IF CA-PP-PRODUCT-CODE NOT = 'M' AND NOT = 'H'
                                 AND NOT = 'C' AND NOT = 'E'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRODCODE'          TO CA-REASON-CODE
              MOVE 'Product must be M, H, C or E'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PP-BILLING-METHOD NOT = 'DB'
              AND CA-PP-BILLING-METHOD NOT = 'AB'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BILLMETH'          TO CA-REASON-CODE
              MOVE 'Billing method must be DB or AB'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PP-TOTAL-PREMIUM NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PREMAMT '          TO CA-REASON-CODE
              MOVE 'Total premium must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3010-VALIDATE-TERM-DATES
      * BR-BIL-PPR-004 Plan exists and is active
           MOVE CA-PP-PLAN-CODE        TO PPL-PLAN-CODE
           PERFORM 4150-READ-PLAN
           PERFORM 3020-RESOLVE-PRODUCER
           PERFORM 4140-SELECT-STREAM
           PERFORM 3030-DETERMINE-TERM.

      ******************************************************************
      * BR-BIL-PPR-003 Valid dates, expiry after effective, <= 18 mths
       3010-VALIDATE-TERM-DATES.
           MOVE CA-PP-EFF-DATE         TO WS-D-IN
           PERFORM 8130-VALIDATE-DATE
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EFFDATE '          TO CA-REASON-CODE
              MOVE 'Effective date is not a valid date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PP-EXP-DATE         TO WS-D-IN
           PERFORM 8130-VALIDATE-DATE
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPDATE '          TO CA-REASON-CODE
              MOVE 'Expiry date is not a valid date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PP-EXP-DATE NOT > CA-PP-EFF-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PERDATES'          TO CA-REASON-CODE
              MOVE 'Expiry date must be after the effective date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PP-EFF-DATE         TO WS-D-IN
           MOVE WS-MAX-TERM-MONTHS     TO WS-D-NUM
           PERFORM 8120-ADD-MONTHS
           IF CA-PP-EXP-DATE > WS-D-OUT
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PERDLONG'          TO CA-REASON-CODE
              MOVE 'Policy period may not exceed 18 months'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PPR-005 Producer from the GenApp broker id
       3020-RESOLVE-PRODUCER.
           MOVE 0                      TO PPR-PRODUCER-ID
           IF CA-PP-PRODUCER-CODE NOT = 0
              INITIALIZE WS-PRD-CA
              MOVE 'PRDFND  '          TO PRA-REQUEST-ID
              MOVE CA-USER-ID          TO PRA-USER-ID
              MOVE WS-BUS-DATE         TO PRA-BUS-DATE
              MOVE CA-TRACE-ID         TO PRA-TRACE-ID
              MOVE WS-PROGRAM-ID       TO PRA-CALLER-PGM
              MOVE CA-PP-PRODUCER-CODE TO PRA-PR-PRODUCER-CODE
              CALL WS-PRD-PGM USING WS-PRD-CA
              IF PRA-RC-NOT-FOUND
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PRDNOTFD'       TO CA-REASON-CODE
                 MOVE PRA-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE PRA-RETURN-CODE     TO WS-CALLEE-RC
              MOVE PRA-REASON-CODE     TO WS-CALLEE-REASON
              MOVE PRA-MESSAGE         TO WS-CALLEE-MESSAGE
              PERFORM 8900-CHECK-CALLEE
              IF PRA-PR-STATUS NOT = 'ACTV'
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'PRDINACT'       TO CA-REASON-CODE
                 STRING 'Producer status ' PRA-PR-STATUS
                        ' cannot write business' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE PRA-PR-PRODUCER-ID  TO PPR-PRODUCER-ID
           END-IF
           IF CA-PP-BILLING-METHOD = 'AB'
              IF PPR-PRODUCER-ID = 0
                 OR PRA-PR-AGENCY-BILL NOT = 'Y'
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'ABNOPROD'       TO CA-REASON-CODE
                 MOVE 'Agency bill needs an agency-bill producer'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-PPR-006 Next term for the policy; NEWBIZ or RENEWAL;
      * the new term may not overlap a prior term still in force
       3030-DETERMINE-TERM.
           MOVE CA-PP-GENAPP-POL-NUM   TO PPR-GENAPP-POL-NUM
           EXEC SQL
              SELECT COALESCE(MAX(TERM_NUMBER), 0)
                INTO :WS-MAX-TERM
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE GENAPP_POL_NUM = :PPR-GENAPP-POL-NUM
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CA-PP-GENAPP-POL-NUM TO WS-POL-DISP
              MOVE WS-POL-DISP         TO WS-ERR-KEY
              MOVE '3030-DETERMINE-TERM' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-MAX-TERM = 0
              MOVE 'NEWBIZ  '          TO WS-BILLING-INSTR
           ELSE
              MOVE 'RENEWAL '          TO WS-BILLING-INSTR
              MOVE WS-MAX-TERM         TO PPR-TERM-NUMBER
              EXEC SQL
                 SELECT EXP_DATE, PP_STATUS
                   INTO :WS-PRIOR-EXP-DATE, :WS-PRIOR-STATUS
                   FROM BILDB.BIL_POLICY_PERIOD
                  WHERE GENAPP_POL_NUM = :PPR-GENAPP-POL-NUM
                    AND TERM_NUMBER    = :PPR-TERM-NUMBER
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE CA-PP-GENAPP-POL-NUM TO WS-POL-DISP
                 MOVE WS-POL-DISP      TO WS-ERR-KEY
                 MOVE '3030-DETERMINE-TERM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              IF WS-PRIOR-STATUS NOT = 'CANC'
                 AND CA-PP-EFF-DATE < WS-PRIOR-EXP-DATE
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'PPOVRLAP'       TO CA-REASON-CODE
                 STRING 'New term overlaps prior term ending '
                        WS-PRIOR-EXP-DATE DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-PPR-020 By PP-ID, else policy + term (0 = latest)
       3050-RESOLVE-PERIOD.
           EVALUATE TRUE
              WHEN CA-PP-PP-ID > 0
                 MOVE CA-PP-PP-ID      TO PPR-PP-ID
              WHEN CA-PP-GENAPP-POL-NUM > 0
                 PERFORM 4010-FIND-PERIOD-BY-POLICY
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PPKEYREQ'       TO CA-REASON-CODE
                 MOVE 'Period id or GenApp policy number is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           PERFORM 4000-READ-PERIOD.

      ******************************************************************
       3100-REQUIRE-IN-FORCE.
           IF PPR-PP-STATUS NOT = 'INFC'
              SET CA-RC-STATE          TO TRUE
              MOVE 'PPNOTINF'          TO CA-REASON-CODE
              STRING 'Period status ' PPR-PP-STATUS
                     ' does not allow ' CA-REQUEST-ID
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PPR-013 Change / cancel date valid and inside the term
       3200-VALIDATE-PERIOD-DATE.
           MOVE CA-PP-CHANGE-EFF-DATE  TO WS-D-IN
           PERFORM 8130-VALIDATE-DATE
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CHGDATE '          TO CA-REASON-CODE
              MOVE 'Change effective date is not a valid date'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-PP-CHANGE-EFF-DATE < PPR-EFF-DATE
              OR CA-PP-CHANGE-EFF-DATE > PPR-EXP-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CHGDTRNG'          TO CA-REASON-CODE
              STRING 'Date must be within ' PPR-EFF-DATE ' and '
                     PPR-EXP-DATE DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PPR-014 Cancel reason is an active CANCRSN code
       3300-VALIDATE-CANCEL-REASON.
           IF CA-PP-CANCEL-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CANRSREQ'          TO CA-REASON-CODE
              MOVE 'Cancellation reason is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE WS-BUS-DATE            TO CDA-BUS-DATE
           MOVE CA-TRACE-ID            TO CDA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE 'CANCRSN '             TO CDA-CD-TABLE
           MOVE CA-PP-CANCEL-REASON    TO CDA-CD-CODE
           CALL WS-COD-PGM USING WS-COD-CA
           IF CDA-RC-NOT-FOUND OR NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CANRSN  '          TO CA-REASON-CODE
              STRING 'Cancel reason ' CA-PP-CANCEL-REASON
                     ' is not a valid code' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CDA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE CDA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE CDA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       4000-READ-PERIOD.
           EXEC SQL
              SELECT ACCOUNT_ID, GENAPP_POL_NUM, TERM_NUMBER,
                     PRODUCT_CODE, EFF_DATE, EXP_DATE,
                     PAYMENT_PLAN_CODE, BILLING_METHOD, PRODUCER_ID,
                     STREAM_ID, TOTAL_PREMIUM, PP_STATUS,
                     CANCEL_DATE, CANCEL_REASON, CLOSURE_STATUS,
                     VERSION_NO
                INTO :PPR-ACCOUNT-ID, :PPR-GENAPP-POL-NUM,
                     :PPR-TERM-NUMBER, :PPR-PRODUCT-CODE,
                     :PPR-EFF-DATE, :PPR-EXP-DATE,
                     :PPR-PAYMENT-PLAN-CODE, :PPR-BILLING-METHOD,
                     :PPR-PRODUCER-ID, :PPR-STREAM-ID,
                     :PPR-TOTAL-PREMIUM, :PPR-PP-STATUS,
                     :PPR-CANCEL-DATE :PPR-CANCEL-DATE-IND,
                     :PPR-CANCEL-REASON :PPR-CANCEL-REASON-IND,
                     :PPR-CLOSURE-STATUS, :PPR-VERSION-NO
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE PP_ID = :PPR-PP-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PPNOTFD '       TO CA-REASON-CODE
                 MOVE PPR-PP-ID        TO WS-ID-DISP
                 STRING 'Policy period ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE PPR-PP-ID        TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4000-READ-PERIOD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4010-FIND-PERIOD-BY-POLICY.
           MOVE CA-PP-GENAPP-POL-NUM   TO PPR-GENAPP-POL-NUM
           MOVE CA-PP-TERM-NUMBER      TO PPR-TERM-NUMBER
           EXEC SQL
              SELECT PP_ID
                INTO :PPR-PP-ID
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE GENAPP_POL_NUM = :PPR-GENAPP-POL-NUM
                 AND TERM_NUMBER    =
                     CASE WHEN :PPR-TERM-NUMBER > 0
                          THEN :PPR-TERM-NUMBER
                          ELSE (SELECT MAX(P2.TERM_NUMBER)
                                  FROM BILDB.BIL_POLICY_PERIOD P2
                                 WHERE P2.GENAPP_POL_NUM =
                                       :PPR-GENAPP-POL-NUM)
                     END
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PPNOTFD '       TO CA-REASON-CODE
                 MOVE CA-PP-GENAPP-POL-NUM TO WS-POL-DISP
                 MOVE CA-PP-TERM-NUMBER TO WS-TERM-DISP
                 STRING 'No period for policy ' WS-POL-DISP
                        ' term ' WS-TERM-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE CA-PP-GENAPP-POL-NUM TO WS-POL-DISP
                 MOVE WS-POL-DISP      TO WS-ERR-KEY
                 MOVE '4010-FIND-PERIOD-BY-POLICY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4110-INSERT-PERIOD.
           EXEC SQL
              INSERT INTO BILDB.BIL_POLICY_PERIOD
                     (PP_ID, ACCOUNT_ID, GENAPP_POL_NUM, TERM_NUMBER,
                      PRODUCT_CODE, EFF_DATE, EXP_DATE,
                      PAYMENT_PLAN_CODE, BILLING_METHOD, PRODUCER_ID,
                      STREAM_ID, TOTAL_PREMIUM, PP_STATUS,
                      CANCEL_DATE, CANCEL_REASON, CLOSURE_STATUS,
                      CREATE_TS, UPDATE_TS, VERSION_NO)
              VALUES (:PPR-PP-ID, :PPR-ACCOUNT-ID,
                      :PPR-GENAPP-POL-NUM, :PPR-TERM-NUMBER,
                      :PPR-PRODUCT-CODE, :PPR-EFF-DATE,
                      :PPR-EXP-DATE, :PPR-PAYMENT-PLAN-CODE,
                      :PPR-BILLING-METHOD, :PPR-PRODUCER-ID,
                      :PPR-STREAM-ID, :PPR-TOTAL-PREMIUM,
                      :PPR-PP-STATUS, NULL, NULL,
                      :PPR-CLOSURE-STATUS, CURRENT TIMESTAMP,
                      CURRENT TIMESTAMP, :PPR-VERSION-NO)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PPR-GENAPP-POL-NUM  TO WS-POL-DISP
              MOVE WS-POL-DISP         TO WS-ERR-KEY
              MOVE '4110-INSERT-PERIOD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE -1                     TO PPR-CANCEL-DATE-IND
                                          PPR-CANCEL-REASON-IND.

      ******************************************************************
       4130-READ-ACCOUNT.
           IF CA-REQUEST-ID = 'PPNEW   '
              MOVE CA-PP-ACCOUNT-ID    TO ACT-ACCOUNT-ID
           ELSE
              MOVE PPR-ACCOUNT-ID      TO ACT-ACCOUNT-ID
           END-IF
           EXEC SQL
              SELECT ACCOUNT_STATUS, DELINQ_PLAN_CODE
                INTO :ACT-ACCOUNT-STATUS, :ACT-DELINQ-PLAN-CODE
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
                 STRING 'Account ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE ACT-ACCOUNT-ID   TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4130-READ-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-PPR-007 Stream of the plan's frequency, else the first
      * active stream of the account
       4140-SELECT-STREAM.
           MOVE CA-PP-ACCOUNT-ID       TO IST-ACCOUNT-ID
           MOVE PPL-FREQUENCY          TO IST-PERIODICITY
           EXEC SQL
              SELECT STREAM_ID
                INTO :IST-STREAM-ID
                FROM BILDB.BIL_INVOICE_STREAM
               WHERE ACCOUNT_ID    = :IST-ACCOUNT-ID
                 AND STREAM_STATUS = 'ACTV'
               ORDER BY CASE WHEN PERIODICITY = :IST-PERIODICITY
                             THEN 0 ELSE 1 END,
                        STREAM_ID
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'NOSTREAM'       TO CA-REASON-CODE
                 MOVE 'Account has no active invoice stream'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE IST-ACCOUNT-ID   TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4140-SELECT-STREAM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4150-READ-PLAN.
           EXEC SQL
              SELECT NUM_INSTALLMENTS, FREQUENCY, INSTALL_FEE,
                     ACTIVE_FLAG
                INTO :PPL-NUM-INSTALLMENTS, :PPL-FREQUENCY,
                     :PPL-INSTALL-FEE, :PPL-ACTIVE-FLAG
                FROM BILDB.BIL_PAYMENT_PLAN
               WHERE PLAN_CODE = :PPL-PLAN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PLANNOTF'       TO CA-REASON-CODE
                 STRING 'Payment plan ' PPL-PLAN-CODE ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE PPL-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '4150-READ-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF PPL-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PLANINAC'          TO CA-REASON-CODE
              STRING 'Payment plan ' PPL-PLAN-CODE ' is not active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4160-READ-DELINQ-PLAN.
           MOVE ACT-DELINQ-PLAN-CODE   TO DPL-PLAN-CODE
           EXEC SQL
              SELECT REINSTATE_FEE
                INTO :DPL-REINSTATE-FEE
                FROM BILDB.BIL_DELINQ_PLAN
               WHERE PLAN_CODE = :DPL-PLAN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DPLNOTFD'       TO CA-REASON-CODE
                 STRING 'Delinquency plan ' DPL-PLAN-CODE
                        ' not found' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE DPL-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '4160-READ-DELINQ-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-PPR-019 Optimistic lock on VERSION_NO
       4200-UPDATE-PERIOD.
           EXEC SQL
              UPDATE BILDB.BIL_POLICY_PERIOD
                 SET PAYMENT_PLAN_CODE = :PPR-PAYMENT-PLAN-CODE,
                     TOTAL_PREMIUM     = :PPR-TOTAL-PREMIUM,
                     PP_STATUS         = :PPR-PP-STATUS,
                     CANCEL_DATE       =
                        :PPR-CANCEL-DATE :PPR-CANCEL-DATE-IND,
                     CANCEL_REASON     =
                        :PPR-CANCEL-REASON :PPR-CANCEL-REASON-IND,
                     UPDATE_TS         = CURRENT TIMESTAMP,
                     VERSION_NO        = VERSION_NO + 1
               WHERE PP_ID      = :PPR-PP-ID
                 AND VERSION_NO = :PPR-VERSION-NO
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PPR-PP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4200-UPDATE-PERIOD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Policy period changed by another task - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           ADD 1                       TO PPR-VERSION-NO.

      ******************************************************************
      * Cancel the planned premium, tax, credit and installment-fee
      * items of one due date; the fee part is waived
       4300-CANCEL-DUE-DATE-ITEMS.
           MOVE PPR-PP-ID              TO ITM-PP-ID
           EXEC SQL
              SELECT COALESCE(SUM(I.ITEM_AMOUNT), 0)
                INTO :WS-FEE
                FROM BILDB.BIL_INVOICE_ITEM I
                JOIN BILDB.BIL_CHARGE C
                  ON C.CHARGE_ID = I.CHARGE_ID
               WHERE I.PP_ID          = :ITM-PP-ID
                 AND I.ITEM_STATUS    = 'PLAN'
                 AND I.DUE_DATE       = :ITM-DUE-DATE
                 AND C.CHARGE_PATTERN = 'INSTFEE'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ITM-DUE-DATE        TO WS-ERR-KEY
              MOVE '4300-CANCEL-DUE-DATE-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD WS-FEE                  TO WS-WAIVED
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET ITEM_STATUS = 'CANC',
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE PP_ID       = :ITM-PP-ID
                 AND ITEM_STATUS = 'PLAN'
                 AND DUE_DATE    = :ITM-DUE-DATE
                 AND CHARGE_ID IN
                     (SELECT C.CHARGE_ID
                        FROM BILDB.BIL_CHARGE C
                       WHERE C.PP_ID = :ITM-PP-ID
                         AND C.CHARGE_PATTERN IN
                             ('PREMIUM', 'TAX', 'INSTFEE'))
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ITM-DUE-DATE        TO WS-ERR-KEY
              MOVE '4300-CANCEL-DUE-DATE-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-LOAD-CANCEL-CHARGES.
           MOVE 'CANCHG  '             TO WS-OPEN-CURSOR
           EXEC SQL OPEN C-CANCHG END-EXEC
           PERFORM 4500-CHECK-OPEN
           MOVE 0                      TO WS-ID-COUNT
           PERFORM UNTIL WS-ID-COUNT >= WS-BATCH-SIZE
              EXEC SQL
                 FETCH C-CANCHG
                  INTO :CHG-CHARGE-ID, :CHG-CHARGE-PATTERN,
                       :CHG-CHARGE-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-ID-COUNT
                    MOVE CHG-CHARGE-ID TO WS-ID-VALUE(WS-ID-COUNT)
                    MOVE CHG-CHARGE-PATTERN
                                  TO WS-ID-PATTERN(WS-ID-COUNT)
                    MOVE CHG-CHARGE-AMOUNT
                                  TO WS-ID-AMOUNT(WS-ID-COUNT)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '4400-LOAD-CANCEL-CHARGES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4590-CLOSE-CURSOR.

      ******************************************************************
       4410-MARK-CHARGE-REVERSED.
           EXEC SQL
              UPDATE BILDB.BIL_CHARGE
                 SET CHARGE_STATUS = 'REVS'
               WHERE CHARGE_ID      = :CHG-CHARGE-ID
                 AND CHARGE_STATUS <> 'REVS'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CHG-CHARGE-ID       TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4410-MARK-CHARGE-REVERSED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'CHGCHNGD'          TO CA-REASON-CODE
              MOVE 'Cancellation charge reversed by another task'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * WS-OPEN-CURSOR names the cursor just opened
       4500-CHECK-OPEN.
           IF SQLCODE NOT = 0
              MOVE WS-OPEN-CURSOR      TO WS-ERR-KEY
              MOVE SPACES              TO WS-OPEN-CURSOR
              MOVE '4500-CHECK-OPEN'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4590-CLOSE-CURSOR.
           EVALUATE WS-OPEN-CURSOR
              WHEN 'ITEMS   '
                 EXEC SQL CLOSE C-ITEMS END-EXEC
              WHEN 'UNCOMM  '
                 EXEC SQL CLOSE C-UNCOMM END-EXEC
              WHEN 'RELEASE '
                 EXEC SQL CLOSE C-RELEASE END-EXEC
              WHEN 'NETZERO '
                 EXEC SQL CLOSE C-NETZERO END-EXEC
              WHEN 'CANCHG  '
                 EXEC SQL CLOSE C-CANCHG END-EXEC
           END-EVALUATE
           MOVE SPACES                 TO WS-OPEN-CURSOR.

      ******************************************************************
       4600-SUM-TAX.
           MOVE PPR-PP-ID              TO CHG-PP-ID
           EXEC SQL
              SELECT COALESCE(SUM(CHARGE_AMOUNT), 0)
                INTO :WS-TAX
                FROM BILDB.BIL_CHARGE
               WHERE PP_ID          = :CHG-PP-ID
                 AND CHARGE_PATTERN = 'TAX'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PPR-PP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4600-SUM-TAX'      TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4610-SUM-UNBILLED.
           MOVE PPR-PP-ID              TO ITM-PP-ID
           EXEC SQL
              SELECT COALESCE(SUM(ITEM_AMOUNT), 0)
                INTO :WS-UNBILLED
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE PP_ID       = :ITM-PP-ID
                 AND ITEM_STATUS = 'PLAN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE PPR-PP-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4610-SUM-UNBILLED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Up to 24 live items; more is a warning on an otherwise clean
      * response
       4700-LOAD-ITEM-SCHEDULE.
           MOVE 0                      TO CA-PP-ITEM-COUNT
           MOVE 'ITEMS   '             TO WS-OPEN-CURSOR
           EXEC SQL OPEN C-ITEMS END-EXEC
           PERFORM 4500-CHECK-OPEN
           PERFORM UNTIL CA-PP-ITEM-COUNT > WS-MAX-ITEMS
              EXEC SQL
                 FETCH C-ITEMS
                  INTO :ITM-ITEM-ID, :ITM-ITEM-TYPE,
                       :ITM-INSTALL-NUMBER, :ITM-EVENT-DATE,
                       :ITM-DUE-DATE, :ITM-ITEM-AMOUNT,
                       :ITM-PAID-AMOUNT, :ITM-ITEM-STATUS
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    IF CA-PP-ITEM-COUNT = WS-MAX-ITEMS
                       IF CA-RC-OK
                          SET CA-RC-WARNING TO TRUE
                          MOVE 'ITMTRUNC' TO CA-REASON-CODE
                          MOVE 'More than 24 items - first 24 shown'
                                       TO CA-MESSAGE
                       END-IF
                       EXIT PERFORM
                    END-IF
                    ADD 1              TO CA-PP-ITEM-COUNT
                    MOVE CA-PP-ITEM-COUNT TO WS-IX
                    MOVE ITM-ITEM-ID   TO CA-PP-IT-ITEM-ID(WS-IX)
                    MOVE ITM-ITEM-TYPE TO CA-PP-IT-TYPE(WS-IX)
                    MOVE ITM-INSTALL-NUMBER
                                       TO CA-PP-IT-INSTALL-NO(WS-IX)
                    MOVE ITM-EVENT-DATE
                                       TO CA-PP-IT-EVENT-DATE(WS-IX)
                    MOVE ITM-DUE-DATE  TO CA-PP-IT-DUE-DATE(WS-IX)
                    MOVE ITM-ITEM-AMOUNT TO CA-PP-IT-AMOUNT(WS-IX)
                    MOVE ITM-PAID-AMOUNT TO CA-PP-IT-PAID(WS-IX)
                    MOVE ITM-ITEM-STATUS TO CA-PP-IT-STATUS(WS-IX)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '4700-LOAD-ITEM-SCHEDULE' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4590-CLOSE-CURSOR.

      ******************************************************************
      * Item ids for commission work, loaded with the cursor closed
      * again before BILCOMSV is called (WS-OPEN-CURSOR picks it)
       4800-LOAD-ITEM-IDS.
           MOVE 0                      TO WS-ID-COUNT
           IF WS-OPEN-CURSOR = 'UNCOMM  '
              EXEC SQL OPEN C-UNCOMM END-EXEC
           ELSE
              EXEC SQL OPEN C-RELEASE END-EXEC
           END-IF
           PERFORM 4500-CHECK-OPEN
           PERFORM UNTIL WS-ID-COUNT >= WS-BATCH-SIZE
              IF WS-OPEN-CURSOR = 'UNCOMM  '
                 EXEC SQL FETCH C-UNCOMM INTO :ITM-ITEM-ID END-EXEC
              ELSE
                 EXEC SQL FETCH C-RELEASE INTO :ITM-ITEM-ID END-EXEC
              END-IF
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-ID-COUNT
                    MOVE ITM-ITEM-ID   TO WS-ID-VALUE(WS-ID-COUNT)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '4800-LOAD-ITEM-IDS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4590-CLOSE-CURSOR.

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
              MOVE CTL-CONTROL-KEY     TO WS-ERR-KEY
              MOVE '4900-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       8000-NEXT-SEQUENCE-PPR.
           MOVE 'PPR     '             TO SQA-SQ-NAME
           PERFORM 8010-NEXT-SEQUENCE.

      ******************************************************************
       8010-NEXT-SEQUENCE.
           MOVE SQA-SQ-NAME            TO WS-ERR-KEY
           INITIALIZE WS-SEQ-CA
           MOVE WS-ERR-KEY             TO SQA-SQ-NAME
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE SQA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE SQA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8100-CALL-DATES.
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE CA-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           MOVE DTA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE DTA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE DTA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8110-ADD-DAYS.
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           MOVE WS-D-IN                TO DTA-DT-DATE-1
           MOVE WS-D-NUM               TO DTA-DT-NUMBER
           PERFORM 8100-CALL-DATES
           MOVE DTA-DT-RESULT-DATE     TO WS-D-OUT.

      ******************************************************************
       8120-ADD-MONTHS.
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDM  '             TO DTA-REQUEST-ID
           MOVE WS-D-IN                TO DTA-DT-DATE-1
           MOVE WS-D-NUM               TO DTA-DT-NUMBER
           PERFORM 8100-CALL-DATES
           MOVE DTA-DT-RESULT-DATE     TO WS-D-OUT.

      ******************************************************************
      * An invalid date is RC 12 from BILDATES; the caller tests the
      * valid flag, so only Db2/system failures are passed up here
       8130-VALIDATE-DATE.
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE WS-D-IN                TO DTA-DT-DATE-1
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE CA-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 90
              MOVE DTA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTA-REASON-CODE     TO CA-REASON-CODE
              MOVE DTA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Days from WS-D-IN to WS-D-IN-2
       8140-DIFF-DAYS.
           INITIALIZE WS-DTE-CA
           MOVE 'DTDIFF  '             TO DTA-REQUEST-ID
           MOVE WS-D-IN                TO DTA-DT-DATE-1
           MOVE WS-D-IN-2              TO DTA-DT-DATE-2
           PERFORM 8100-CALL-DATES
           MOVE DTA-DT-RESULT-NUM      TO WS-D-RESULT.

      ******************************************************************
       8200-CALL-MONEY.
           MOVE CA-USER-ID             TO MNA-USER-ID
           MOVE WS-BUS-DATE            TO MNA-BUS-DATE
           MOVE CA-TRACE-ID            TO MNA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO MNA-CALLER-PGM
           CALL WS-MNY-PGM USING WS-MNY-CA
           MOVE MNA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE MNA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE MNA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE
           MOVE MNA-MN-RESULT          TO WS-MN-RESULT.

      ******************************************************************
      * BR-BIL-PPR-008 IPT = WS-MN-TOTAL x 12%
       8210-APPLY-IPT.
           INITIALIZE WS-MNY-CA
           MOVE 'MNRATE  '             TO MNA-REQUEST-ID
           MOVE WS-MN-TOTAL            TO MNA-MN-TOTAL
           MOVE WS-IPT-RATE-BPS        TO MNA-MN-RATE-BPS
           PERFORM 8200-CALL-MONEY.

      ******************************************************************
       8220-PRORATE.
           INITIALIZE WS-MNY-CA
           MOVE 'MNPRORA '             TO MNA-REQUEST-ID
           MOVE WS-MN-TOTAL            TO MNA-MN-TOTAL
           MOVE WS-DAYS-PART           TO MNA-MN-DAYS-PART
           MOVE WS-DAYS-WHOLE          TO MNA-MN-DAYS-WHOLE
           PERFORM 8200-CALL-MONEY.

      ******************************************************************
      * BR-BIL-PPR-010 Revenue lines per component, credited when
      * positive and debited when negative, balanced by one UNBILLED
      * line for the net amount
       8300-POST-CHARGE-LEDGER.
           COMPUTE WS-GL-NET = WS-GL-PREMREV + WS-GL-FEEREV
           IF WS-GL-PREMREV = 0 AND WS-GL-FEEREV = 0
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE CA-TRACE-ID            TO GLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE WS-GL-TXN-TYPE         TO GLA-GL-TXN-TYPE
           MOVE PPR-ACCOUNT-ID         TO GLA-GL-ACCOUNT-ID
           MOVE PPR-PRODUCER-ID        TO GLA-GL-PRODUCER-ID
           MOVE 'PP  '                 TO GLA-GL-REF-TYPE
           MOVE PPR-PP-ID              TO GLA-GL-REF-ID
           MOVE 0                      TO GLA-GL-LINE-COUNT
           IF WS-GL-NET NOT = 0
              ADD 1                    TO GLA-GL-LINE-COUNT
              MOVE GLA-GL-LINE-COUNT   TO WS-IX
              MOVE 'UNBILLED'          TO GLA-GL-T-ACCOUNT(WS-IX)
              IF WS-GL-NET > 0
                 MOVE 'D'              TO GLA-GL-DR-CR(WS-IX)
                 MOVE WS-GL-NET        TO GLA-GL-AMOUNT(WS-IX)
              ELSE
                 MOVE 'C'              TO GLA-GL-DR-CR(WS-IX)
                 COMPUTE GLA-GL-AMOUNT(WS-IX) = 0 - WS-GL-NET
              END-IF
           END-IF
           IF WS-GL-PREMREV NOT = 0
              ADD 1                    TO GLA-GL-LINE-COUNT
              MOVE GLA-GL-LINE-COUNT   TO WS-IX
              MOVE 'PREMREV '          TO GLA-GL-T-ACCOUNT(WS-IX)
              IF WS-GL-PREMREV > 0
                 MOVE 'C'              TO GLA-GL-DR-CR(WS-IX)
                 MOVE WS-GL-PREMREV    TO GLA-GL-AMOUNT(WS-IX)
              ELSE
                 MOVE 'D'              TO GLA-GL-DR-CR(WS-IX)
                 COMPUTE GLA-GL-AMOUNT(WS-IX) = 0 - WS-GL-PREMREV
              END-IF
           END-IF
           IF WS-GL-FEEREV NOT = 0
              ADD 1                    TO GLA-GL-LINE-COUNT
              MOVE GLA-GL-LINE-COUNT   TO WS-IX
              MOVE 'FEEREV  '          TO GLA-GL-T-ACCOUNT(WS-IX)
              IF WS-GL-FEEREV > 0
                 MOVE 'C'              TO GLA-GL-DR-CR(WS-IX)
                 MOVE WS-GL-FEEREV     TO GLA-GL-AMOUNT(WS-IX)
              ELSE
                 MOVE 'D'              TO GLA-GL-DR-CR(WS-IX)
                 COMPUTE GLA-GL-AMOUNT(WS-IX) = 0 - WS-GL-FEEREV
              END-IF
           END-IF
           CALL WS-GLP-PGM USING WS-GLP-CA
           MOVE GLA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE GLA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE GLA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8400-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE 'POLPER  '             TO AUA-AU-ENTITY
           MOVE PPR-PP-ID              TO WS-ID-DISP
           MOVE WS-ID-DISP             TO AUA-AU-KEY
           MOVE WS-AU-ACTION           TO AUA-AU-ACTION
           MOVE WS-AU-FIELD            TO AUA-AU-FIELD
           MOVE WS-AU-BEFORE           TO AUA-AU-BEFORE
           MOVE WS-AU-AFTER            TO AUA-AU-AFTER
           MOVE WS-PROGRAM-ID          TO AUA-AU-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           MOVE AUA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE AUA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE AUA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Insert a charge from WS-NEW-CHARGE and schedule it
       8500-CREATE-CHARGE.
           MOVE 'CHG     '             TO SQA-SQ-NAME
           PERFORM 8010-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO CHG-CHARGE-ID
           MOVE PPR-PP-ID              TO CHG-PP-ID
           MOVE PPR-ACCOUNT-ID         TO CHG-ACCOUNT-ID
           MOVE WS-CH-PATTERN          TO CHG-CHARGE-PATTERN
           MOVE WS-CH-INSTR            TO CHG-BILLING-INSTR
           MOVE WS-CH-AMOUNT           TO CHG-CHARGE-AMOUNT
           MOVE WS-BUS-DATE            TO CHG-CHARGE-DATE
           MOVE WS-CH-EFF-DATE         TO CHG-EFF-DATE
           MOVE 'OPEN'                 TO CHG-CHARGE-STATUS
           MOVE CA-PP-SOURCE-REF       TO CHG-SOURCE-REF
           MOVE CA-USER-ID             TO CHG-CREATE-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_CHARGE
                     (CHARGE_ID, PP_ID, ACCOUNT_ID, CHARGE_PATTERN,
                      BILLING_INSTR, CHARGE_AMOUNT, CHARGE_DATE,
                      EFF_DATE, CHARGE_STATUS, SOURCE_REF,
                      CREATE_USER, CREATE_TS)
              VALUES (:CHG-CHARGE-ID, :CHG-PP-ID, :CHG-ACCOUNT-ID,
                      :CHG-CHARGE-PATTERN, :CHG-BILLING-INSTR,
                      :CHG-CHARGE-AMOUNT, :CHG-CHARGE-DATE,
                      :CHG-EFF-DATE, :CHG-CHARGE-STATUS,
                      :CHG-SOURCE-REF, :CHG-CREATE-USER,
                      CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CHG-CHARGE-ID       TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '8500-CREATE-CHARGE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           INITIALIZE WS-SCH-CA
           MOVE 'SCHCHG  '             TO SCA-REQUEST-ID
           MOVE CHG-CHARGE-ID          TO SCA-SC-CHARGE-ID
           MOVE WS-CH-ONE-TIME         TO SCA-SC-ONE-TIME
           PERFORM 8510-CALL-SCHEDULER.

      ******************************************************************
       8510-CALL-SCHEDULER.
           MOVE CA-USER-ID             TO SCA-USER-ID
           MOVE WS-BUS-DATE            TO SCA-BUS-DATE
           MOVE CA-TRACE-ID            TO SCA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SCA-CALLER-PGM
           CALL WS-SCH-PGM USING WS-SCH-CA
           MOVE SCA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE SCA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE SCA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * BR-BIL-PPR-011 Reserve commission on uncommissioned premium
      * items, 50 at a time, never revisiting an item id
       8600-RESERVE-COMMISSIONS.
           IF PPR-PRODUCER-ID = 0
              EXIT PARAGRAPH
           END-IF
           MOVE 0                      TO WS-LAST-ITEM-ID
           MOVE WS-BATCH-SIZE          TO WS-ID-COUNT
           PERFORM UNTIL WS-ID-COUNT < WS-BATCH-SIZE
              MOVE 'UNCOMM  '          TO WS-OPEN-CURSOR
              PERFORM 4800-LOAD-ITEM-IDS
              PERFORM VARYING WS-IX FROM 1 BY 1
                        UNTIL WS-IX > WS-ID-COUNT
                 INITIALIZE WS-CMS-CA
                 MOVE 'CMSRSV  '       TO CMA-REQUEST-ID
                 MOVE WS-ID-VALUE(WS-IX) TO CMA-CM-ITEM-ID
                                          WS-LAST-ITEM-ID
                 MOVE PPR-PRODUCER-ID  TO CMA-CM-PRODUCER-ID
                 PERFORM 8630-CALL-RESERVE
              END-PERFORM
           END-PERFORM.

      ******************************************************************
      * BR-BIL-PPR-011 Release commission on cancelled items
       8610-RELEASE-COMMISSIONS.
           MOVE 0                      TO WS-LAST-ITEM-ID
           MOVE WS-BATCH-SIZE          TO WS-ID-COUNT
           PERFORM UNTIL WS-ID-COUNT < WS-BATCH-SIZE
              MOVE 'RELEASE '          TO WS-OPEN-CURSOR
              PERFORM 4800-LOAD-ITEM-IDS
              PERFORM VARYING WS-IX FROM 1 BY 1
                        UNTIL WS-IX > WS-ID-COUNT
                 INITIALIZE WS-CMS-CA
                 MOVE 'CMSRVS  '       TO CMA-REQUEST-ID
                 MOVE WS-ID-VALUE(WS-IX) TO CMA-CM-ITEM-ID
                                          WS-LAST-ITEM-ID
                 PERFORM 8620-CALL-COMMISSION
              END-PERFORM
           END-PERFORM.

      ******************************************************************
       8620-CALL-COMMISSION.
           MOVE CA-USER-ID             TO CMA-USER-ID
           MOVE WS-BUS-DATE            TO CMA-BUS-DATE
           MOVE CA-TRACE-ID            TO CMA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CMA-CALLER-PGM
           CALL WS-CMS-PGM USING WS-CMS-CA
           MOVE CMA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE CMA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE CMA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * A producer or commission plan made inactive after the period
      * was written (RC 24) stops further reservation, not the change
       8630-CALL-RESERVE.
           MOVE CA-USER-ID             TO CMA-USER-ID
           MOVE WS-BUS-DATE            TO CMA-BUS-DATE
           MOVE CA-TRACE-ID            TO CMA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CMA-CALLER-PGM
           CALL WS-CMS-PGM USING WS-CMS-CA
           IF NOT CMA-RC-STATE
              MOVE CMA-RETURN-CODE     TO WS-CALLEE-RC
              MOVE CMA-REASON-CODE     TO WS-CALLEE-REASON
              MOVE CMA-MESSAGE         TO WS-CALLEE-MESSAGE
              PERFORM 8900-CHECK-CALLEE
           END-IF.

      ******************************************************************
      * Recompute account balances; ACA-AC- amounts are used by PPCAN
       8700-REFRESH-BALANCES.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTBAL '             TO ACA-REQUEST-ID
           MOVE CA-USER-ID             TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE CA-TRACE-ID            TO ACA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE PPR-ACCOUNT-ID         TO ACA-AC-ACCOUNT-ID
           CALL WS-ACT-PGM USING WS-ACT-CA
           MOVE ACA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE ACA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE ACA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * BR-BIL-PPR-018 Plan change within PLANCHG authority
       8800-CHECK-PLAN-AUTHORITY.
           INITIALIZE WS-AUT-CA
           MOVE 'AUTCHK  '             TO ATA-REQUEST-ID
           MOVE CA-USER-ID             TO ATA-USER-ID
           MOVE WS-BUS-DATE            TO ATA-BUS-DATE
           MOVE CA-TRACE-ID            TO ATA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ATA-CALLER-PGM
           MOVE CA-USER-ID             TO ATA-AT-USER
           MOVE 'PLANCHG '             TO ATA-AT-LIMIT-TYPE
           MOVE WS-UNBILLED            TO ATA-AT-AMOUNT
           CALL WS-AUT-PGM USING WS-AUT-CA
           MOVE ATA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE ATA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE ATA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE
           IF NOT ATA-AT-IS-ALLOWED
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'AUTHLIMT'          TO CA-REASON-CODE
              MOVE ATA-AT-LIMIT        TO WS-AMT-DISP
              STRING 'Plan change over your limit ' WS-AMT-DISP
                     ' - refer to ' ATA-AT-APPROVER
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Callee RC 08 here is an inconsistency; RC >= 12 is passed up
      * unchanged with its reason code
       8900-CHECK-CALLEE.
           IF WS-CALLEE-RC >= 08
              MOVE WS-CALLEE-RC        TO CA-RETURN-CODE
              MOVE WS-CALLEE-REASON    TO CA-REASON-CODE
              MOVE WS-CALLEE-MESSAGE   TO CA-MESSAGE
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
                 MOVE 'Policy period data locked - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DB2ERR  '       TO CA-REASON-CODE
                 STRING 'BILPPER0 Db2 error SQLCODE ' WS-SQLCODE-DISP
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
           IF NOT WS-NO-CURSOR
              PERFORM 4590-CLOSE-CURSOR
           END-IF
           PERFORM 9000-RETURN.
