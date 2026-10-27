       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILISCHD.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILISCHD                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Invoice item scheduler. Splits a charge into     *
      *               invoice items following the policy period's      *
      *               payment plan and invoice stream (down payment,   *
      *               installments, one-time fees, credits spread over *
      *               unbilled items) and reschedules the unbilled     *
      *               items of a period onto a new payment plan.       *
      * REQUESTS    : SCHCHG  schedule one charge                      *
      *               SCHRSC  reschedule PLAN items onto a new plan    *
      * CALLED BY   : BILPPER0 (policy periods), BILDLQ family (late   *
      *               and cancellation fees), BILPAY family (NSF fee)  *
      * CALLS       : BILSEQGN BILDATES BILMONEY BILERRLG              *
      * TABLES      : BIL_CHARGE            (R U)                      *
      *               BIL_POLICY_PERIOD     (R)                        *
      *               BIL_PAYMENT_PLAN      (R)                        *
      *               BIL_INVOICE_STREAM    (R)                        *
      *               BIL_INVOICE_ITEM      (C R U)                    *
      *               BIL_CONTROL           (R)                        *
      * RULES       : BR-BIL-SCH-001 A charge is scheduled once: it    *
      *                              must be OPEN and have no items    *
      *               BR-BIL-SCH-002 Recurring positive charges need   *
      *                              an in-force (INFC) period; one-   *
      *                              time fees and credits may follow  *
      *                              a cancelled period                *
      *               BR-BIL-SCH-003 LATEFEE REINSFEE CANCFEE or the   *
      *                              one-time flag bill as one ONET    *
      *                              item on the next invoice date     *
      *               BR-BIL-SCH-004 Next invoice date is the first    *
      *                              stream anchor day on or after the *
      *                              business date                     *
      *               BR-BIL-SCH-005 Down payment DPST: event date =   *
      *                              charge date, due = event date +   *
      *                              DAYS_BILL_BEFORE                  *
      *               BR-BIL-SCH-006 Installments every M=1 Q=3 S=6    *
      *                              A=12 months, the first one        *
      *                              FIRST_INST_MONTHS after period    *
      *                              effective date                    *
      *               BR-BIL-SCH-007 Installment due dates fall on the *
      *                              stream anchor day (clamped to the *
      *                              end of a short month)             *
      *               BR-BIL-SCH-008 Event date = due date minus       *
      *                              DAYS_BILL_BEFORE, never before    *
      *                              the business date                 *
      *               BR-BIL-SCH-009 An installment due after period   *
      *                              expiry merges into the previous   *
      *                              item                              *
      *               BR-BIL-SCH-010 An installment below the plan     *
      *                              MIN_INST_AMOUNT merges into the   *
      *                              previous item                     *
      *               BR-BIL-SCH-011 Priority rank PREMIUM 1, TAX 2,   *
      *                              all fees 5                        *
      *               BR-BIL-SCH-012 INSTFEE is spread over            *
      *                              installments 2..N only - no fee   *
      *                              on the down payment               *
      *               BR-BIL-SCH-013 Mid-term charges (not NEWBIZ or   *
      *                              RENEWAL) spread over the period's *
      *                              remaining planned premium due     *
      *                              dates; none left = next invoice   *
      *               BR-BIL-SCH-014 A credit offsets unbilled items   *
      *                              of the same pattern, latest due   *
      *                              date first; any remainder is one  *
      *                              CRED item on the next invoice     *
      *               BR-BIL-SCH-015 Reschedule preserves each         *
      *                              charge's unbilled total exactly   *
      *               BR-BIL-SCH-016 Reschedule uses only the new      *
      *                              plan's installment dates on or    *
      *                              after the business date           *
      *               BR-BIL-SCH-017 The period's invoice stream must  *
      *                              be ACTV with anchor day 1..31     *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILISCHD'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'BILMONEY'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-MAX-ENTRIES           PIC S9(4) COMP VALUE 24.
           05 WS-MAX-RSC-CHARGES       PIC S9(4) COMP VALUE 60.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-NEXT-INV-DATE         PIC X(10) VALUE SPACES.
           05 WS-FREQ-MONTHS           PIC S9(4) COMP VALUE 0.
           05 WS-INSTALLMENTS          PIC S9(4) COMP VALUE 0.
           05 WS-PRIORITY              PIC S9(4) COMP VALUE 5.
           05 WS-IX                    PIC S9(4) COMP VALUE 0.
           05 WS-JX                    PIC S9(4) COMP VALUE 0.
           05 WS-INST-NO               PIC S9(4) COMP VALUE 0.
           05 WS-MONTHS                PIC S9(4) COMP VALUE 0.
           05 WS-REMAINING             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OFFSET                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SCHED-TOTAL           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-CHARGE-TOTAL          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ITEM-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-INST-DUE              PIC X(10) VALUE SPACES.
           05 WS-INST-EVENT            PIC X(10) VALUE SPACES.
           05 WS-PAST-EXPIRY           PIC X     VALUE 'N'.
              88 WS-IS-PAST-EXPIRY               VALUE 'Y'.
           05 WS-ONE-TIME              PIC X     VALUE 'N'.
              88 WS-IS-ONE-TIME                  VALUE 'Y'.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-AMT-DISP              PIC -(9)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.
           05 WS-OPEN-CURSOR           PIC X(8)  VALUE SPACES.

      * Date helper fields (5100-ALIGN-TO-ANCHOR, 81nn date calls)
       01  WS-DATE-WORK.
           05 WS-D-IN                  PIC X(10) VALUE SPACES.
           05 WS-D-OUT                 PIC X(10) VALUE SPACES.
           05 WS-D-NUM                 PIC S9(5) COMP-3 VALUE 0.
           05 WS-AL-DATE               PIC X(10) VALUE SPACES.
           05 WS-AL-RESULT             PIC X(10) VALUE SPACES.
           05 WS-AL-EOM-DAY            PIC 99    VALUE 0.
           05 WS-AL-DAY                PIC 99    VALUE 0.

      * Work schedule built before any item is inserted
       01  WS-SCHEDULE.
           05 WS-SD-COUNT              PIC S9(4) COMP VALUE 0.
           05 WS-SD-ENTRY OCCURS 24 TIMES.
              10 WS-SD-TYPE            PIC X(4).
              10 WS-SD-INST-NO         PIC S9(4) COMP.
              10 WS-SD-EVENT-DATE      PIC X(10).
              10 WS-SD-DUE-DATE        PIC X(10).
              10 WS-SD-AMOUNT          PIC S9(11)V99 COMP-3.

      * Charges of a period with unbilled items (SCHRSC)
       01  WS-RSC-TABLE.
           05 WS-RS-COUNT              PIC S9(4) COMP VALUE 0.
           05 WS-RS-ENTRY OCCURS 60 TIMES.
              10 WS-RS-CHARGE-ID       PIC S9(9) COMP.
              10 WS-RS-PATTERN         PIC X(8).
              10 WS-RS-AMOUNT          PIC S9(11)V99 COMP-3.

      * Cursor fetch targets
       01  WS-FETCH.
           05 WS-F-DUE-DATE            PIC X(10).
           05 WS-F-EVENT-DATE          PIC X(10).
           05 WS-F-INST-NO             PIC S9(4) COMP.
           05 WS-F-AMOUNT              PIC S9(11)V99 COMP-3.
           05 WS-F-CHARGE-ID           PIC S9(9) COMP.
           05 WS-F-PATTERN             PIC X(8).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDPPL END-EXEC.
           EXEC SQL INCLUDE BILDIST END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * Remaining planned premium due dates of a period (mid-term)
           EXEC SQL
              DECLARE C-REM-DATES CURSOR FOR
               SELECT I.DUE_DATE, MIN(I.EVENT_DATE),
                      MAX(I.INSTALL_NUMBER)
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_CHARGE C
                   ON C.CHARGE_ID = I.CHARGE_ID
                WHERE I.PP_ID          = :CHG-PP-ID
                  AND I.ITEM_STATUS    = 'PLAN'
                  AND I.ITEM_AMOUNT    > 0
                  AND I.DUE_DATE      >= :WS-BUS-DATE
                  AND C.CHARGE_PATTERN = 'PREMIUM'
                GROUP BY I.DUE_DATE
                ORDER BY I.DUE_DATE
                FETCH FIRST 24 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

      * Net unbilled amount per due date for one charge pattern,
      * latest first, for credit offsetting
           EXEC SQL
              DECLARE C-OFFSET CURSOR FOR
               SELECT I.DUE_DATE, MIN(I.EVENT_DATE),
                      MAX(I.INSTALL_NUMBER), SUM(I.ITEM_AMOUNT)
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_CHARGE C
                   ON C.CHARGE_ID = I.CHARGE_ID
                WHERE I.PP_ID          = :CHG-PP-ID
                  AND I.ITEM_STATUS    = 'PLAN'
                  AND C.CHARGE_PATTERN = :CHG-CHARGE-PATTERN
                GROUP BY I.DUE_DATE
               HAVING SUM(I.ITEM_AMOUNT) > 0
                ORDER BY I.DUE_DATE DESC
                FOR FETCH ONLY
           END-EXEC.

      * Unbilled total per charge of a period (reschedule)
           EXEC SQL
              DECLARE C-RSC-CHARGES CURSOR FOR
               SELECT I.CHARGE_ID, C.CHARGE_PATTERN,
                      SUM(I.ITEM_AMOUNT)
                 FROM BILDB.BIL_INVOICE_ITEM I
                 JOIN BILDB.BIL_CHARGE C
                   ON C.CHARGE_ID = I.CHARGE_ID
                WHERE I.PP_ID       = :PPR-PP-ID
                  AND I.ITEM_STATUS = 'PLAN'
                GROUP BY I.CHARGE_ID, C.CHARGE_PATTERN
                ORDER BY I.CHARGE_ID
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
       01  WS-MNY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==MNC-==.
           03 MNC-BODY.
              COPY BILCMNY REPLACING LEADING ==CA-== BY ==MNC-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCSCH.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'SCHCHG  '
                 PERFORM 2000-SCHEDULE-CHARGE
              WHEN 'SCHRSC  '
                 PERFORM 2700-RESCHEDULE-PERIOD
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILISCHD: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-SC-ITEMS-CREATED
                                          CA-SC-AMOUNT-SCHEDULED
           MOVE SPACES                 TO CA-SC-FIRST-DUE-DATE
           SET WS-CURSOR-CLOSED        TO TRUE
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO CA-BUS-DATE
           END-IF
           MOVE CA-BUS-DATE            TO WS-BUS-DATE.

      ******************************************************************
      * SCHCHG - split one charge into invoice items
       2000-SCHEDULE-CHARGE.
           PERFORM 3000-VALIDATE-CHARGE
           PERFORM 5400-SET-PRIORITY
           PERFORM 4110-READ-PERIOD
           PERFORM 3100-VALIDATE-PERIOD
           PERFORM 4120-READ-PLAN
           PERFORM 4130-READ-STREAM
           PERFORM 3200-VALIDATE-PLAN-STREAM
           PERFORM 5000-NEXT-INVOICE-DATE
           MOVE 0                      TO WS-SD-COUNT
           EVALUATE TRUE
              WHEN CHG-CHARGE-AMOUNT < 0
                 PERFORM 2300-SCHEDULE-CREDIT
              WHEN WS-IS-ONE-TIME
                 PERFORM 2200-SCHEDULE-ONE-TIME
              WHEN CHG-CHARGE-PATTERN = 'INSTFEE '
                 PERFORM 2400-SCHEDULE-INSTALL-FEE
              WHEN CHG-BILLING-INSTR = 'NEWBIZ  '
                OR CHG-BILLING-INSTR = 'RENEWAL '
                 PERFORM 2500-SCHEDULE-PLAN
              WHEN OTHER
                 PERFORM 2600-SCHEDULE-REMAINING
           END-EVALUATE
           PERFORM 4500-INSERT-SCHEDULE
           PERFORM 3300-CHECK-BALANCED
           PERFORM 4600-MARK-CHARGE-SCHEDULED.

      ******************************************************************
      * BR-BIL-SCH-003 One-time fee billed in full on the next invoice
       2200-SCHEDULE-ONE-TIME.
           MOVE CHG-CHARGE-AMOUNT      TO WS-REMAINING
           PERFORM 2210-ADD-NEXT-INVOICE-ITEM.

      ******************************************************************
      * Append one item for WS-REMAINING on the next invoice date
       2210-ADD-NEXT-INVOICE-ITEM.
           ADD 1                       TO WS-SD-COUNT
           MOVE WS-SD-COUNT            TO WS-IX
           IF WS-REMAINING < 0
              MOVE 'CRED'              TO WS-SD-TYPE(WS-IX)
           ELSE
              MOVE 'ONET'              TO WS-SD-TYPE(WS-IX)
           END-IF
           MOVE 0                      TO WS-SD-INST-NO(WS-IX)
           MOVE WS-NEXT-INV-DATE       TO WS-SD-EVENT-DATE(WS-IX)
           MOVE WS-NEXT-INV-DATE       TO WS-D-IN
           MOVE PPL-DAYS-BILL-BEFORE   TO WS-D-NUM
           PERFORM 8110-ADD-DAYS
           MOVE WS-D-OUT               TO WS-SD-DUE-DATE(WS-IX)
           MOVE WS-REMAINING           TO WS-SD-AMOUNT(WS-IX).

      ******************************************************************
      * BR-BIL-SCH-014 Credit offsets unbilled items of the same
      * pattern, latest due date first; remainder on next invoice
       2300-SCHEDULE-CREDIT.
           COMPUTE WS-REMAINING = 0 - CHG-CHARGE-AMOUNT
           EXEC SQL OPEN C-OFFSET END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-SCHEDULE-CREDIT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'OFFSET  '             TO WS-OPEN-CURSOR
           PERFORM UNTIL WS-REMAINING = 0
                      OR WS-SD-COUNT >= WS-MAX-ENTRIES - 1
              EXEC SQL
                 FETCH C-OFFSET
                  INTO :WS-F-DUE-DATE, :WS-F-EVENT-DATE,
                       :WS-F-INST-NO, :WS-F-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 2310-ADD-OFFSET
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '2300-SCHEDULE-CREDIT' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR
           IF WS-REMAINING > 0
              COMPUTE WS-REMAINING = 0 - WS-REMAINING
              PERFORM 2210-ADD-NEXT-INVOICE-ITEM
           END-IF.

      ******************************************************************
       2310-ADD-OFFSET.
           IF WS-F-AMOUNT < WS-REMAINING
              MOVE WS-F-AMOUNT         TO WS-OFFSET
           ELSE
              MOVE WS-REMAINING        TO WS-OFFSET
           END-IF
           SUBTRACT WS-OFFSET          FROM WS-REMAINING
           ADD 1                       TO WS-SD-COUNT
           MOVE WS-SD-COUNT            TO WS-IX
           MOVE 'CRED'                 TO WS-SD-TYPE(WS-IX)
           MOVE WS-F-INST-NO           TO WS-SD-INST-NO(WS-IX)
           MOVE WS-F-EVENT-DATE        TO WS-SD-EVENT-DATE(WS-IX)
           MOVE WS-F-DUE-DATE          TO WS-SD-DUE-DATE(WS-IX)
           COMPUTE WS-SD-AMOUNT(WS-IX) = 0 - WS-OFFSET.

      ******************************************************************
      * BR-BIL-SCH-012 Installment fee over installments 2..N only
       2400-SCHEDULE-INSTALL-FEE.
           IF WS-INSTALLMENTS < 2
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FEENOINS'          TO CA-REASON-CODE
              STRING 'Installment fee on single-pay plan '
                     PPL-PLAN-CODE DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CHG-CHARGE-AMOUNT      TO MNC-MN-TOTAL
           COMPUTE MNC-MN-COUNT = WS-INSTALLMENTS - 1
           MOVE 0                      TO MNC-MN-DOWNPAY-PCT
           PERFORM 8200-SPLIT-AMOUNT
           PERFORM VARYING WS-INST-NO FROM 2 BY 1
                     UNTIL WS-INST-NO > WS-INSTALLMENTS
              PERFORM 5200-INSTALLMENT-DATES
              COMPUTE WS-JX = WS-INST-NO - 1
              IF WS-IS-PAST-EXPIRY AND WS-SD-COUNT > 0
                 ADD MNC-MN-PART(WS-JX) TO WS-SD-AMOUNT(WS-SD-COUNT)
              ELSE
                 ADD 1                 TO WS-SD-COUNT
                 MOVE WS-SD-COUNT      TO WS-IX
                 MOVE 'FEE '           TO WS-SD-TYPE(WS-IX)
                 MOVE WS-INST-NO       TO WS-SD-INST-NO(WS-IX)
                 MOVE WS-INST-EVENT    TO WS-SD-EVENT-DATE(WS-IX)
                 MOVE WS-INST-DUE      TO WS-SD-DUE-DATE(WS-IX)
                 MOVE MNC-MN-PART(WS-JX) TO WS-SD-AMOUNT(WS-IX)
              END-IF
           END-PERFORM
           PERFORM 5300-MERGE-SMALL-ITEMS.

      ******************************************************************
      * New business / renewal: down payment + installments per plan
       2500-SCHEDULE-PLAN.
           MOVE CHG-CHARGE-AMOUNT      TO MNC-MN-TOTAL
           MOVE WS-INSTALLMENTS        TO MNC-MN-COUNT
           MOVE PPL-DOWNPAY-PCT        TO MNC-MN-DOWNPAY-PCT
           PERFORM 8200-SPLIT-AMOUNT
      * BR-BIL-SCH-005 Down payment billed from the charge date
           MOVE 1                      TO WS-SD-COUNT
           MOVE 'DPST'                 TO WS-SD-TYPE(1)
           MOVE 1                      TO WS-SD-INST-NO(1)
           MOVE CHG-CHARGE-DATE        TO WS-SD-EVENT-DATE(1)
           MOVE CHG-CHARGE-DATE        TO WS-D-IN
           MOVE PPL-DAYS-BILL-BEFORE   TO WS-D-NUM
           PERFORM 8110-ADD-DAYS
           MOVE WS-D-OUT               TO WS-SD-DUE-DATE(1)
           MOVE MNC-MN-PART(1)         TO WS-SD-AMOUNT(1)
           PERFORM VARYING WS-INST-NO FROM 2 BY 1
                     UNTIL WS-INST-NO > WS-INSTALLMENTS
              PERFORM 5200-INSTALLMENT-DATES
      * BR-BIL-SCH-009 Installment past expiry merges into previous
              IF WS-IS-PAST-EXPIRY
                 ADD MNC-MN-PART(WS-INST-NO)
                                       TO WS-SD-AMOUNT(WS-SD-COUNT)
              ELSE
                 ADD 1                 TO WS-SD-COUNT
                 MOVE WS-SD-COUNT      TO WS-IX
                 MOVE 'INST'           TO WS-SD-TYPE(WS-IX)
                 MOVE WS-INST-NO       TO WS-SD-INST-NO(WS-IX)
                 MOVE WS-INST-EVENT    TO WS-SD-EVENT-DATE(WS-IX)
                 MOVE WS-INST-DUE      TO WS-SD-DUE-DATE(WS-IX)
                 MOVE MNC-MN-PART(WS-INST-NO)
                                       TO WS-SD-AMOUNT(WS-IX)
              END-IF
           END-PERFORM
           PERFORM 5300-MERGE-SMALL-ITEMS.

      ******************************************************************
      * BR-BIL-SCH-013 Mid-term charge over remaining premium dates
       2600-SCHEDULE-REMAINING.
           EXEC SQL OPEN C-REM-DATES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2600-SCHEDULE-REMAINING' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'REMDATES'             TO WS-OPEN-CURSOR
           PERFORM UNTIL WS-SD-COUNT >= WS-MAX-ENTRIES
              EXEC SQL
                 FETCH C-REM-DATES
                  INTO :WS-F-DUE-DATE, :WS-F-EVENT-DATE,
                       :WS-F-INST-NO
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-SD-COUNT
                    MOVE WS-SD-COUNT   TO WS-IX
                    MOVE 'INST'        TO WS-SD-TYPE(WS-IX)
                    MOVE WS-F-INST-NO  TO WS-SD-INST-NO(WS-IX)
                    MOVE WS-F-EVENT-DATE
                                       TO WS-SD-EVENT-DATE(WS-IX)
                    MOVE WS-F-DUE-DATE TO WS-SD-DUE-DATE(WS-IX)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '2600-SCHEDULE-REMAINING' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4290-CLOSE-CURSOR
           IF WS-SD-COUNT = 0
              MOVE CHG-CHARGE-AMOUNT   TO WS-REMAINING
              PERFORM 2210-ADD-NEXT-INVOICE-ITEM
           ELSE
              MOVE CHG-CHARGE-AMOUNT   TO MNC-MN-TOTAL
              MOVE WS-SD-COUNT         TO MNC-MN-COUNT
              MOVE 0                   TO MNC-MN-DOWNPAY-PCT
              PERFORM 8200-SPLIT-AMOUNT
              PERFORM VARYING WS-IX FROM 1 BY 1
                        UNTIL WS-IX > WS-SD-COUNT
                 MOVE MNC-MN-PART(WS-IX) TO WS-SD-AMOUNT(WS-IX)
              END-PERFORM
              PERFORM 5300-MERGE-SMALL-ITEMS
           END-IF.

      ******************************************************************
      * SCHRSC - move every unbilled item of a period onto a new plan
       2700-RESCHEDULE-PERIOD.
           PERFORM 3500-VALIDATE-RESCHEDULE
           PERFORM 4130-READ-STREAM
           MOVE CA-SC-NEW-PLAN         TO PPL-PLAN-CODE
           PERFORM 4125-READ-PLAN-BY-CODE
           PERFORM 3200-VALIDATE-PLAN-STREAM
           PERFORM 5000-NEXT-INVOICE-DATE
           PERFORM 4700-LOAD-RSC-CHARGES
           IF WS-RS-COUNT = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOPLANIT'          TO CA-REASON-CODE
              MOVE 'Period has no unbilled items to reschedule'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM VARYING WS-JX FROM 1 BY 1
                     UNTIL WS-JX > WS-RS-COUNT
              PERFORM 2710-RESCHEDULE-CHARGE
           END-PERFORM.

      ******************************************************************
       2710-RESCHEDULE-CHARGE.
           MOVE WS-RS-CHARGE-ID(WS-JX) TO CHG-CHARGE-ID
           PERFORM 4100-READ-CHARGE
           MOVE WS-RS-AMOUNT(WS-JX)    TO WS-CHARGE-TOTAL
           PERFORM 4710-CANCEL-CHARGE-ITEMS
           IF WS-CHARGE-TOTAL NOT = 0
              PERFORM 5400-SET-PRIORITY
              MOVE 0                   TO WS-SD-COUNT
              MOVE WS-CHARGE-TOTAL     TO WS-REMAINING
              IF WS-IS-ONE-TIME OR WS-CHARGE-TOTAL < 0
                 PERFORM 2210-ADD-NEXT-INVOICE-ITEM
              ELSE
                 PERFORM 2720-FUTURE-PLAN-DATES
              END-IF
              MOVE 0                   TO WS-SCHED-TOTAL
              PERFORM 4500-INSERT-SCHEDULE
      * BR-BIL-SCH-015 Rescheduled total equals the unbilled total
              IF WS-SCHED-TOTAL NOT = WS-CHARGE-TOTAL
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'SCHUNBAL'       TO CA-REASON-CODE
                 MOVE CHG-CHARGE-ID    TO WS-ID-DISP
                 STRING 'Reschedule out of balance for charge '
                        WS-ID-DISP DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-SCH-016 Only new-plan installment dates from today on
       2720-FUTURE-PLAN-DATES.
           PERFORM VARYING WS-INST-NO FROM 2 BY 1
                     UNTIL WS-INST-NO > WS-INSTALLMENTS
                        OR WS-SD-COUNT >= WS-MAX-ENTRIES
              PERFORM 5200-INSTALLMENT-DATES
              IF NOT WS-IS-PAST-EXPIRY
                 AND WS-INST-DUE >= WS-BUS-DATE
                 ADD 1                 TO WS-SD-COUNT
                 MOVE WS-SD-COUNT      TO WS-IX
                 MOVE 'INST'           TO WS-SD-TYPE(WS-IX)
                 IF CHG-CHARGE-PATTERN = 'INSTFEE '
                    MOVE 'FEE '        TO WS-SD-TYPE(WS-IX)
                 END-IF
                 MOVE WS-INST-NO       TO WS-SD-INST-NO(WS-IX)
                 MOVE WS-INST-EVENT    TO WS-SD-EVENT-DATE(WS-IX)
                 MOVE WS-INST-DUE      TO WS-SD-DUE-DATE(WS-IX)
              END-IF
           END-PERFORM
           IF WS-SD-COUNT = 0
              PERFORM 2210-ADD-NEXT-INVOICE-ITEM
           ELSE
              MOVE WS-CHARGE-TOTAL     TO MNC-MN-TOTAL
              MOVE WS-SD-COUNT         TO MNC-MN-COUNT
              MOVE 0                   TO MNC-MN-DOWNPAY-PCT
              PERFORM 8200-SPLIT-AMOUNT
              PERFORM VARYING WS-IX FROM 1 BY 1
                        UNTIL WS-IX > WS-SD-COUNT
                 MOVE MNC-MN-PART(WS-IX) TO WS-SD-AMOUNT(WS-IX)
              END-PERFORM
              PERFORM 5300-MERGE-SMALL-ITEMS
           END-IF.

      ******************************************************************
      * BR-BIL-SCH-001 A charge is scheduled exactly once
       3000-VALIDATE-CHARGE.
           IF CA-SC-CHARGE-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CHGREQ  '          TO CA-REASON-CODE
              MOVE 'Charge id is required for scheduling'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-SC-CHARGE-ID        TO CHG-CHARGE-ID
           PERFORM 4100-READ-CHARGE
           IF CHG-CHARGE-STATUS NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CHGNOTOP'          TO CA-REASON-CODE
              STRING 'Charge status ' CHG-CHARGE-STATUS
                     ' cannot be scheduled' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CHG-CHARGE-AMOUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CHGZERO '          TO CA-REASON-CODE
              MOVE 'Zero charge cannot be scheduled'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4140-COUNT-CHARGE-ITEMS
           IF WS-ITEM-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'CHGSCHED'          TO CA-REASON-CODE
              MOVE 'Charge already has invoice items'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-SCH-002 Recurring positive charges need INFC period
       3100-VALIDATE-PERIOD.
           IF PPR-ACCOUNT-ID NOT = CHG-ACCOUNT-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CHGACCT '          TO CA-REASON-CODE
              MOVE 'Charge account differs from its policy period'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF PPR-PP-STATUS NOT = 'INFC'
              AND CHG-CHARGE-AMOUNT > 0
              AND NOT WS-IS-ONE-TIME
              SET CA-RC-STATE          TO TRUE
              MOVE 'PPNOTINF'          TO CA-REASON-CODE
              STRING 'Period status ' PPR-PP-STATUS
                     ' cannot take recurring charges'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-SCH-006 Plan frequency and installment count
      * BR-BIL-SCH-017 Stream active with a usable anchor day
       3200-VALIDATE-PLAN-STREAM.
           EVALUATE PPL-FREQUENCY
              WHEN 'M'
                 MOVE 1                TO WS-FREQ-MONTHS
              WHEN 'Q'
                 MOVE 3                TO WS-FREQ-MONTHS
              WHEN 'S'
                 MOVE 6                TO WS-FREQ-MONTHS
              WHEN 'A'
                 MOVE 12               TO WS-FREQ-MONTHS
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'PLANFREQ'       TO CA-REASON-CODE
                 STRING 'Plan ' PPL-PLAN-CODE
                        ' has invalid frequency ' PPL-FREQUENCY
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           MOVE PPL-NUM-INSTALLMENTS   TO WS-INSTALLMENTS
           IF WS-INSTALLMENTS < 1 OR WS-INSTALLMENTS > WS-MAX-ENTRIES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PLANINST'          TO CA-REASON-CODE
              STRING 'Plan ' PPL-PLAN-CODE
                     ' installments must be 1 to 24'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF IST-STREAM-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'STRMINAC'          TO CA-REASON-CODE
              MOVE 'Invoice stream of the period is not active'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF IST-ANCHOR-DAY < 1 OR IST-ANCHOR-DAY > 31
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'STRMANCH'          TO CA-REASON-CODE
              MOVE 'Invoice stream anchor day must be 1 to 31'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Defensive: scheduled items must add back to the charge
       3300-CHECK-BALANCED.
           IF WS-SCHED-TOTAL NOT = CHG-CHARGE-AMOUNT
              SET CA-RC-DB2-ERROR      TO TRUE
              MOVE 'SCHUNBAL'          TO CA-REASON-CODE
              MOVE WS-SCHED-TOTAL      TO WS-AMT-DISP
              STRING 'Scheduled ' WS-AMT-DISP
                     ' does not equal charge amount'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3500-VALIDATE-RESCHEDULE.
           IF CA-SC-PP-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PPIDREQ '          TO CA-REASON-CODE
              MOVE 'Policy period id is required to reschedule'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-SC-NEW-PLAN = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PLANREQ '          TO CA-REASON-CODE
              MOVE 'New payment plan is required to reschedule'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-SC-PP-ID            TO CHG-PP-ID
           PERFORM 4110-READ-PERIOD
           IF PPR-PP-STATUS NOT = 'INFC'
              SET CA-RC-STATE          TO TRUE
              MOVE 'PPNOTINF'          TO CA-REASON-CODE
              STRING 'Period status ' PPR-PP-STATUS
                     ' cannot be rescheduled'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4100-READ-CHARGE.
           EXEC SQL
              SELECT PP_ID, ACCOUNT_ID, CHARGE_PATTERN,
                     BILLING_INSTR, CHARGE_AMOUNT, CHARGE_DATE,
                     EFF_DATE, CHARGE_STATUS
                INTO :CHG-PP-ID, :CHG-ACCOUNT-ID,
                     :CHG-CHARGE-PATTERN, :CHG-BILLING-INSTR,
                     :CHG-CHARGE-AMOUNT, :CHG-CHARGE-DATE,
                     :CHG-EFF-DATE, :CHG-CHARGE-STATUS
                FROM BILDB.BIL_CHARGE
               WHERE CHARGE_ID = :CHG-CHARGE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CHGNOTFD'       TO CA-REASON-CODE
                 MOVE CHG-CHARGE-ID    TO WS-ID-DISP
                 STRING 'Charge ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE CHG-CHARGE-ID    TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4100-READ-CHARGE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4110-READ-PERIOD.
           MOVE CHG-PP-ID              TO PPR-PP-ID
           EXEC SQL
              SELECT ACCOUNT_ID, EFF_DATE, EXP_DATE,
                     PAYMENT_PLAN_CODE, STREAM_ID, PP_STATUS
                INTO :PPR-ACCOUNT-ID, :PPR-EFF-DATE, :PPR-EXP-DATE,
                     :PPR-PAYMENT-PLAN-CODE, :PPR-STREAM-ID,
                     :PPR-PP-STATUS
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
                 MOVE '4110-READ-PERIOD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4120-READ-PLAN.
           MOVE PPR-PAYMENT-PLAN-CODE  TO PPL-PLAN-CODE
           PERFORM 4125-READ-PLAN-BY-CODE.

      ******************************************************************
       4125-READ-PLAN-BY-CODE.
           EXEC SQL
              SELECT DOWNPAY_PCT, NUM_INSTALLMENTS, FREQUENCY,
                     INSTALL_FEE, DAYS_BILL_BEFORE,
                     FIRST_INST_MONTHS, MIN_INST_AMOUNT, ACTIVE_FLAG
                INTO :PPL-DOWNPAY-PCT, :PPL-NUM-INSTALLMENTS,
                     :PPL-FREQUENCY, :PPL-INSTALL-FEE,
                     :PPL-DAYS-BILL-BEFORE, :PPL-FIRST-INST-MONTHS,
                     :PPL-MIN-INST-AMOUNT, :PPL-ACTIVE-FLAG
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
                 MOVE '4125-READ-PLAN-BY-CODE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF CA-REQUEST-ID = 'SCHRSC  '
              AND PPL-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PLANINAC'          TO CA-REASON-CODE
              STRING 'Payment plan ' PPL-PLAN-CODE ' is not active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4130-READ-STREAM.
           MOVE PPR-STREAM-ID          TO IST-STREAM-ID
           EXEC SQL
              SELECT PERIODICITY, ANCHOR_DAY, STREAM_STATUS
                INTO :IST-PERIODICITY, :IST-ANCHOR-DAY,
                     :IST-STREAM-STATUS
                FROM BILDB.BIL_INVOICE_STREAM
               WHERE STREAM_ID = :IST-STREAM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'STRMNOTF'       TO CA-REASON-CODE
                 MOVE IST-STREAM-ID    TO WS-ID-DISP
                 STRING 'Invoice stream ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE IST-STREAM-ID    TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4130-READ-STREAM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4140-COUNT-CHARGE-ITEMS.
           MOVE CHG-CHARGE-ID          TO ITM-CHARGE-ID
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-ITEM-COUNT
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE CHARGE_ID = :ITM-CHARGE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CHG-CHARGE-ID       TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4140-COUNT-CHARGE-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4290-CLOSE-CURSOR.
           IF WS-CURSOR-OPEN
              EVALUATE WS-OPEN-CURSOR
                 WHEN 'OFFSET  '
                    EXEC SQL CLOSE C-OFFSET END-EXEC
                 WHEN 'REMDATES'
                    EXEC SQL CLOSE C-REM-DATES END-EXEC
                 WHEN 'RSCCHGS '
                    EXEC SQL CLOSE C-RSC-CHARGES END-EXEC
              END-EVALUATE
              SET WS-CURSOR-CLOSED     TO TRUE
              MOVE SPACES              TO WS-OPEN-CURSOR
           END-IF.

      ******************************************************************
      * Insert every entry of the work schedule as a PLAN item
      * BR-BIL-SCH-011 Priority rank travels on every item
       4500-INSERT-SCHEDULE.
           MOVE 0                      TO WS-SCHED-TOTAL
           PERFORM VARYING WS-IX FROM 1 BY 1
                     UNTIL WS-IX > WS-SD-COUNT
              PERFORM 8000-NEXT-ITEM-ID
              MOVE SQC-SQ-VALUE        TO ITM-ITEM-ID
              MOVE CHG-CHARGE-ID       TO ITM-CHARGE-ID
              MOVE CHG-ACCOUNT-ID      TO ITM-ACCOUNT-ID
              MOVE CHG-PP-ID           TO ITM-PP-ID
              MOVE 0                   TO ITM-INVOICE-ID
              MOVE PPR-STREAM-ID       TO ITM-STREAM-ID
              MOVE WS-SD-TYPE(WS-IX)   TO ITM-ITEM-TYPE
              MOVE WS-SD-INST-NO(WS-IX) TO ITM-INSTALL-NUMBER
              MOVE WS-SD-EVENT-DATE(WS-IX) TO ITM-EVENT-DATE
              MOVE WS-SD-DUE-DATE(WS-IX) TO ITM-DUE-DATE
              MOVE WS-SD-AMOUNT(WS-IX) TO ITM-ITEM-AMOUNT
              MOVE 'PLAN'              TO ITM-ITEM-STATUS
              MOVE WS-PRIORITY         TO ITM-PRIORITY-RANK
              EXEC SQL
                 INSERT INTO BILDB.BIL_INVOICE_ITEM
                        (ITEM_ID, CHARGE_ID, ACCOUNT_ID, PP_ID,
                         INVOICE_ID, STREAM_ID, ITEM_TYPE,
                         INSTALL_NUMBER, EVENT_DATE, DUE_DATE,
                         ITEM_AMOUNT, PAID_AMOUNT, WRITEOFF_AMOUNT,
                         COMMISSION_AMOUNT, ITEM_STATUS,
                         PRIORITY_RANK, UPDATE_TS)
                 VALUES (:ITM-ITEM-ID, :ITM-CHARGE-ID,
                         :ITM-ACCOUNT-ID, :ITM-PP-ID,
                         :ITM-INVOICE-ID, :ITM-STREAM-ID,
                         :ITM-ITEM-TYPE, :ITM-INSTALL-NUMBER,
                         :ITM-EVENT-DATE, :ITM-DUE-DATE,
                         :ITM-ITEM-AMOUNT, 0, 0, 0,
                         :ITM-ITEM-STATUS, :ITM-PRIORITY-RANK,
                         CURRENT TIMESTAMP)
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE ITM-ITEM-ID      TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4500-INSERT-SCHEDULE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              ADD 1                    TO CA-SC-ITEMS-CREATED
              ADD ITM-ITEM-AMOUNT      TO WS-SCHED-TOTAL
                                          CA-SC-AMOUNT-SCHEDULED
              IF CA-SC-FIRST-DUE-DATE = SPACES
                 OR ITM-DUE-DATE < CA-SC-FIRST-DUE-DATE
                 MOVE ITM-DUE-DATE     TO CA-SC-FIRST-DUE-DATE
              END-IF
           END-PERFORM.

      ******************************************************************
       4600-MARK-CHARGE-SCHEDULED.
           EXEC SQL
              UPDATE BILDB.BIL_CHARGE
                 SET CHARGE_STATUS = 'SCHD'
               WHERE CHARGE_ID     = :CHG-CHARGE-ID
                 AND CHARGE_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CHG-CHARGE-ID       TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4600-MARK-CHARGE-SCHEDULED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'CHGCHNGD'          TO CA-REASON-CODE
              MOVE 'Charge changed while it was being scheduled'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * Load charges with unbilled items into WS so no cursor stays
      * open across the item updates and inserts
       4700-LOAD-RSC-CHARGES.
           MOVE 0                      TO WS-RS-COUNT
           EXEC SQL OPEN C-RSC-CHARGES END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4700-LOAD-RSC-CHARGES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'RSCCHGS '             TO WS-OPEN-CURSOR
           PERFORM UNTIL WS-RS-COUNT >= WS-MAX-RSC-CHARGES
              EXEC SQL
                 FETCH C-RSC-CHARGES
                  INTO :WS-F-CHARGE-ID, :WS-F-PATTERN, :WS-F-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-RS-COUNT
                    MOVE WS-F-CHARGE-ID
                                  TO WS-RS-CHARGE-ID(WS-RS-COUNT)
                    MOVE WS-F-PATTERN  TO WS-RS-PATTERN(WS-RS-COUNT)
                    MOVE WS-F-AMOUNT   TO WS-RS-AMOUNT(WS-RS-COUNT)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE '4700-LOAD-RSC-CHARGES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           IF WS-RS-COUNT >= WS-MAX-RSC-CHARGES
              EXEC SQL
                 FETCH C-RSC-CHARGES
                  INTO :WS-F-CHARGE-ID, :WS-F-PATTERN, :WS-F-AMOUNT
              END-EXEC
              IF SQLCODE = 0
                 PERFORM 4290-CLOSE-CURSOR
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RSCLIMIT'       TO CA-REASON-CODE
                 MOVE 'More than 60 charges to reschedule'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           PERFORM 4290-CLOSE-CURSOR.

      ******************************************************************
       4710-CANCEL-CHARGE-ITEMS.
           MOVE CHG-CHARGE-ID          TO ITM-CHARGE-ID
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET ITEM_STATUS = 'CANC',
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE CHARGE_ID   = :ITM-CHARGE-ID
                 AND ITEM_STATUS = 'PLAN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE CHG-CHARGE-ID       TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4710-CANCEL-CHARGE-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

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
      * BR-BIL-SCH-004 First anchor day on or after the business date
       5000-NEXT-INVOICE-DATE.
           MOVE WS-BUS-DATE            TO WS-AL-DATE
           PERFORM 5100-ALIGN-TO-ANCHOR
           IF WS-AL-RESULT < WS-BUS-DATE
              STRING WS-BUS-DATE(1:8) '01' DELIMITED BY SIZE
                     INTO WS-D-IN
              MOVE 1                   TO WS-D-NUM
              PERFORM 8120-ADD-MONTHS
              MOVE WS-D-OUT            TO WS-AL-DATE
              PERFORM 5100-ALIGN-TO-ANCHOR
           END-IF
           MOVE WS-AL-RESULT           TO WS-NEXT-INV-DATE.

      ******************************************************************
      * BR-BIL-SCH-007 Same month as WS-AL-DATE on the anchor day,
      * clamped to the last day of a short month
       5100-ALIGN-TO-ANCHOR.
           MOVE WS-AL-DATE             TO WS-D-IN
           PERFORM 8130-END-OF-MONTH
           MOVE WS-D-OUT(9:2)          TO WS-AL-EOM-DAY
           IF IST-ANCHOR-DAY > WS-AL-EOM-DAY
              MOVE WS-AL-EOM-DAY       TO WS-AL-DAY
           ELSE
              MOVE IST-ANCHOR-DAY      TO WS-AL-DAY
           END-IF
           MOVE WS-AL-DATE             TO WS-AL-RESULT
           MOVE WS-AL-DAY              TO WS-AL-RESULT(9:2).

      ******************************************************************
      * Due and event dates of installment WS-INST-NO (2..N)
      * BR-BIL-SCH-006 First installment FIRST_INST_MONTHS after the
      * period effective date, then every frequency months
      * BR-BIL-SCH-008 Event date = due - DAYS_BILL_BEFORE, not past
       5200-INSTALLMENT-DATES.
           MOVE 'N'                    TO WS-PAST-EXPIRY
           COMPUTE WS-MONTHS = PPL-FIRST-INST-MONTHS
                             + (WS-INST-NO - 2) * WS-FREQ-MONTHS
           MOVE PPR-EFF-DATE           TO WS-D-IN
           MOVE WS-MONTHS              TO WS-D-NUM
           PERFORM 8120-ADD-MONTHS
           MOVE WS-D-OUT               TO WS-AL-DATE
           PERFORM 5100-ALIGN-TO-ANCHOR
           MOVE WS-AL-RESULT           TO WS-INST-DUE
           IF WS-INST-DUE > PPR-EXP-DATE
              MOVE 'Y'                 TO WS-PAST-EXPIRY
           END-IF
           MOVE WS-INST-DUE            TO WS-D-IN
           COMPUTE WS-D-NUM = 0 - PPL-DAYS-BILL-BEFORE
           PERFORM 8110-ADD-DAYS
           MOVE WS-D-OUT               TO WS-INST-EVENT
           IF WS-INST-EVENT < WS-BUS-DATE
              MOVE WS-BUS-DATE         TO WS-INST-EVENT
           END-IF.

      ******************************************************************
      * BR-BIL-SCH-010 Small installments fold into the previous item,
      * walking back from the last so amounts cascade correctly
       5300-MERGE-SMALL-ITEMS.
           IF PPL-MIN-INST-AMOUNT > 0
              PERFORM VARYING WS-IX FROM WS-SD-COUNT BY -1
                        UNTIL WS-IX < 2
                 IF WS-SD-AMOUNT(WS-IX) < PPL-MIN-INST-AMOUNT
                    COMPUTE WS-JX = WS-IX - 1
                    ADD WS-SD-AMOUNT(WS-IX) TO WS-SD-AMOUNT(WS-JX)
                    PERFORM 5310-REMOVE-ENTRY
                 END-IF
              END-PERFORM
           END-IF.

      ******************************************************************
       5310-REMOVE-ENTRY.
           PERFORM VARYING WS-JX FROM WS-IX BY 1
                     UNTIL WS-JX >= WS-SD-COUNT
              MOVE WS-SD-ENTRY(WS-JX + 1) TO WS-SD-ENTRY(WS-JX)
           END-PERFORM
           SUBTRACT 1                  FROM WS-SD-COUNT.

      ******************************************************************
      * BR-BIL-SCH-011 PREMIUM 1, TAX 2, every fee 5
       5400-SET-PRIORITY.
           EVALUATE CHG-CHARGE-PATTERN
              WHEN 'PREMIUM '
                 MOVE 1                TO WS-PRIORITY
              WHEN 'TAX     '
                 MOVE 2                TO WS-PRIORITY
              WHEN OTHER
                 MOVE 5                TO WS-PRIORITY
           END-EVALUATE
           MOVE 'N'                    TO WS-ONE-TIME
           IF (CA-REQUEST-ID = 'SCHCHG  ' AND CA-SC-IS-ONE-TIME)
              OR CHG-CHARGE-PATTERN = 'LATEFEE '
              OR CHG-CHARGE-PATTERN = 'REINSFEE'
              OR CHG-CHARGE-PATTERN = 'CANCFEE '
              MOVE 'Y'                 TO WS-ONE-TIME
           END-IF.

      ******************************************************************
       8000-NEXT-ITEM-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE WS-BUS-DATE            TO SQC-BUS-DATE
           MOVE CA-TRACE-ID            TO SQC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'ITM     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 08
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8100-CALL-DATES.
           MOVE CA-USER-ID             TO DTC-USER-ID
           MOVE WS-BUS-DATE            TO DTC-BUS-DATE
           MOVE CA-TRACE-ID            TO DTC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTC-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTC-RETURN-CODE >= 08
              MOVE DTC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTC-REASON-CODE     TO CA-REASON-CODE
              MOVE DTC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8110-ADD-DAYS.
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTC-REQUEST-ID
           MOVE WS-D-IN                TO DTC-DT-DATE-1
           MOVE WS-D-NUM               TO DTC-DT-NUMBER
           PERFORM 8100-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO WS-D-OUT.

      ******************************************************************
       8120-ADD-MONTHS.
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDM  '             TO DTC-REQUEST-ID
           MOVE WS-D-IN                TO DTC-DT-DATE-1
           MOVE WS-D-NUM               TO DTC-DT-NUMBER
           PERFORM 8100-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO WS-D-OUT.

      ******************************************************************
       8130-END-OF-MONTH.
           INITIALIZE WS-DTE-CA
           MOVE 'DTEOM   '             TO DTC-REQUEST-ID
           MOVE WS-D-IN                TO DTC-DT-DATE-1
           PERFORM 8100-CALL-DATES
           MOVE DTC-DT-RESULT-DATE     TO WS-D-OUT.

      ******************************************************************
      * MNSPLIT: caller sets MNC-MN-TOTAL, -COUNT and -DOWNPAY-PCT
       8200-SPLIT-AMOUNT.
           MOVE 'MNSPLIT '             TO MNC-REQUEST-ID
           MOVE CA-USER-ID             TO MNC-USER-ID
           MOVE WS-BUS-DATE            TO MNC-BUS-DATE
           MOVE CA-TRACE-ID            TO MNC-TRACE-ID
           MOVE WS-PROGRAM-ID          TO MNC-CALLER-PGM
           CALL WS-MNY-PGM USING WS-MNY-CA
           IF MNC-RETURN-CODE >= 08
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
                 MOVE 'Schedule data locked - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DB2ERR  '       TO CA-REASON-CODE
                 STRING 'BILISCHD Db2 error SQLCODE ' WS-SQLCODE-DISP
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
