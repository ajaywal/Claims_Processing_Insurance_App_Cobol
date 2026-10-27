       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILPAYDS.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILPAYDS                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Payment distribution engine. Spreads the         *
      *               unapplied money of one receipt across the open   *
      *               invoice items of its account in the carrier's    *
      *               standard order, books the distribution to the    *
      *               ledger and keeps receipt, item, invoice, account *
      *               and commission balances in step. DSTUNDO backs a *
      *               receipt's distributions out with mirror rows.    *
      * REQUESTS    : DSTAPLY  distribute AMOUNT of MONEY-ID           *
      *               DSTUNDO  reverse every distribution of MONEY-ID  *
      * CALLED BY   : BILPAYRC (receipt, match, reversal)              *
      * CALLS       : BILSEQGN BILGLPST BILCOMSV BILERRLG              *
      * TABLES      : BIL_PAYMENT_RECEIPT (R U)                        *
      *               BIL_DISTRIBUTION    (C R U)                      *
      *               BIL_INVOICE_ITEM    (R U)                        *
      *               BIL_INVOICE         (R U)                        *
      *               BIL_CHARGE          (R)                          *
      *               BIL_ACCOUNT         (R U)                        *
      *               BIL_CONTROL         (R)                          *
      * RULES       : BR-BIL-DST-001 Receipt must exist, belong to the *
      *                              account, and be RCVD or APPL      *
      *               BR-BIL-DST-002 Amount defaults to the receipt's  *
      *                              unapplied money, never above it   *
      *               BR-BIL-DST-003 Target invoice must belong to the *
      *                              account; its items go first       *
      *               BR-BIL-DST-004 Pass 1 past-due (DUE) items,      *
      *                              oldest due date first             *
      *               BR-BIL-DST-005 Pass 2 billed-not-due (BILD)      *
      *                              items, oldest due date first      *
      *               BR-BIL-DST-006 Pass 3 planned items in mode A    *
      *                              only, earliest event date first   *
      *               BR-BIL-DST-007 Within a date premium before tax  *
      *                              before fees (PRIORITY_RANK)       *
      *               BR-BIL-DST-008 Partial payment: an item takes    *
      *                              the lesser of its outstanding and *
      *                              the money left; settled -> CLSD   *
      *               BR-BIL-DST-009 Invoice AMOUNT_DUE follows its    *
      *                              items; nothing due -> PAID        *
      *               BR-BIL-DST-010 PREMIUM-pattern items earn        *
      *                              commission on the amount applied  *
      *               BR-BIL-DST-011 One balanced ledger entry per     *
      *                              batch: UNAPPLD against DUE,       *
      *                              BILLED or UNBILLED by the item's  *
      *                              balance bucket                    *
      *               BR-BIL-DST-012 Undistributed remainder stays     *
      *                              unapplied on receipt and account  *
      *               BR-BIL-DST-013 Undo never deletes: it writes a   *
      *                              negative mirror row per original  *
      *               BR-BIL-DST-014 Restored item status comes from   *
      *                              invoice link and due date         *
      *               BR-BIL-DST-015 Undo reverses earned commission   *
      *               BR-BIL-DST-016 Item, receipt and account updates *
      *                              are guarded against concurrent    *
      *                              change (STALEVER)                 *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILPAYDS'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-CMS-PGM                  PIC X(8)  VALUE 'BILCOMSV'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-TAB-MAX               PIC S9(4) COMP VALUE 100.

       01  WS-SWITCHES.
           05 WS-ITEM-CSR-SW           PIC X     VALUE 'N'.
              88 WS-ITEM-CSR-OPEN                VALUE 'Y'.
           05 WS-PLAN-CSR-SW           PIC X     VALUE 'N'.
              88 WS-PLAN-CSR-OPEN                VALUE 'Y'.
           05 WS-DIST-CSR-SW           PIC X     VALUE 'N'.
              88 WS-DIST-CSR-OPEN                VALUE 'Y'.
           05 WS-FETCH-SW              PIC X     VALUE 'N'.
              88 WS-FETCH-END                    VALUE 'Y'.
              88 WS-FETCH-MORE                   VALUE 'N'.
           05 WS-PASS-SW               PIC X     VALUE 'N'.
              88 WS-PASS-REPEAT                  VALUE 'Y'.
              88 WS-PASS-DONE                    VALUE 'N'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-MONEY-ID              PIC S9(9) COMP.
           05 WS-ACCOUNT-ID            PIC S9(9) COMP.
           05 WS-ACCT-VERSION          PIC S9(9) COMP.
           05 WS-PASS-STATUS           PIC X(4).
           05 WS-PASS-INVOICE-ID       PIC S9(9) COMP.
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-REMAINING             PIC S9(11)V99 COMP-3.
           05 WS-DISTRIBUTED           PIC S9(11)V99 COMP-3.
           05 WS-OUTSTANDING           PIC S9(11)V99 COMP-3.
           05 WS-APPLY                 PIC S9(11)V99 COMP-3.
           05 WS-NEW-PAID              PIC S9(11)V99 COMP-3.
           05 WS-NEW-STATUS            PIC X(4).
           05 WS-BUCKET                PIC X(8).
           05 WS-BKT-DUE               PIC S9(11)V99 COMP-3.
           05 WS-BKT-BILLED            PIC S9(11)V99 COMP-3.
           05 WS-BKT-UNBILLED          PIC S9(11)V99 COMP-3.
           05 WS-UNAPPLD-SIDE          PIC X.
           05 WS-BUCKET-SIDE           PIC X.
           05 WS-TXN-TYPE              PIC X(8).
           05 WS-CMS-REQUEST           PIC X(8).
           05 WS-SIGN                  PIC S9    COMP-3.
           05 WS-RCP-DELTA             PIC S9(11)V99 COMP-3.
           05 WS-DUE-DELTA             PIC S9(11)V99 COMP-3.
           05 WS-BILLED-DELTA          PIC S9(11)V99 COMP-3.
           05 WS-UNBILLED-DELTA        PIC S9(11)V99 COMP-3.
           05 WS-NEXT-DIST-ID          PIC S9(9) COMP.
           05 WS-ORIG-DIST-ID          PIC S9(9) COMP.
           05 WS-INV-ACCOUNT-ID        PIC S9(9) COMP.
           05 WS-AMOUNT-DISP           PIC -(10)9.99.
           05 WS-KEY-DISP              PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-KEY               PIC X(20).

      * Items fetched for one pass; applied after the cursor is closed
      * so the rows being updated are never under an open cursor.
       01  WS-ITEM-TAB.
           05 WS-IT-COUNT              PIC S9(4) COMP.
           05 WS-IT-ENTRY OCCURS 100 TIMES.
              10 WS-IT-ITEM-ID         PIC S9(9) COMP.
              10 WS-IT-INVOICE-ID      PIC S9(9) COMP.
              10 WS-IT-PATTERN         PIC X(8).
              10 WS-IT-STATUS          PIC X(4).
              10 WS-IT-DUE-DATE        PIC X(10).
              10 WS-IT-AMOUNT          PIC S9(11)V99 COMP-3.
              10 WS-IT-PAID            PIC S9(11)V99 COMP-3.
              10 WS-IT-WOFF            PIC S9(11)V99 COMP-3.

       01  WS-DIST-TAB.
           05 WS-DT-COUNT              PIC S9(4) COMP.
           05 WS-DT-ENTRY OCCURS 100 TIMES.
              10 WS-DT-DIST-ID         PIC S9(9) COMP.
              10 WS-DT-ITEM-ID         PIC S9(9) COMP.
              10 WS-DT-AMOUNT          PIC S9(11)V99 COMP-3.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDRCP END-EXEC.
           EXEC SQL INCLUDE BILDDIS END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-DST-004/005/007 due and billed items by due date, rank
           EXEC SQL DECLARE CSR-OPEN-ITEMS CURSOR FOR
              SELECT I.ITEM_ID, I.INVOICE_ID, C.CHARGE_PATTERN,
                     I.ITEM_STATUS, I.DUE_DATE, I.ITEM_AMOUNT,
                     I.PAID_AMOUNT, I.WRITEOFF_AMOUNT
                FROM BILDB.BIL_INVOICE_ITEM I
                JOIN BILDB.BIL_CHARGE C
                  ON C.CHARGE_ID = I.CHARGE_ID
               WHERE I.ACCOUNT_ID  = :WS-ACCOUNT-ID
                 AND I.ITEM_STATUS = :WS-PASS-STATUS
                 AND (:WS-PASS-INVOICE-ID = 0
                      OR I.INVOICE_ID = :WS-PASS-INVOICE-ID)
                 AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                     - I.WRITEOFF_AMOUNT > 0
               ORDER BY I.DUE_DATE, I.PRIORITY_RANK, I.ITEM_ID
               FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-DST-006/007 planned items by event date, rank
           EXEC SQL DECLARE CSR-PLAN-ITEMS CURSOR FOR
              SELECT I.ITEM_ID, I.INVOICE_ID, C.CHARGE_PATTERN,
                     I.ITEM_STATUS, I.DUE_DATE, I.ITEM_AMOUNT,
                     I.PAID_AMOUNT, I.WRITEOFF_AMOUNT
                FROM BILDB.BIL_INVOICE_ITEM I
                JOIN BILDB.BIL_CHARGE C
                  ON C.CHARGE_ID = I.CHARGE_ID
               WHERE I.ACCOUNT_ID  = :WS-ACCOUNT-ID
                 AND I.ITEM_STATUS = 'PLAN'
                 AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                     - I.WRITEOFF_AMOUNT > 0
               ORDER BY I.EVENT_DATE, I.PRIORITY_RANK, I.ITEM_ID
               FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CSR-LIVE-DISTS CURSOR FOR
              SELECT DIST_ID, ITEM_ID, DIST_AMOUNT
                FROM BILDB.BIL_DISTRIBUTION
               WHERE MONEY_ID         = :WS-MONEY-ID
                 AND REVERSED_FLAG    = 'N'
                 AND REVERSED_DIST_ID = 0
               ORDER BY DIST_ID DESC
               FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.

       01  WS-CMS-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CMA-==.
           03 CMA-BODY.
              COPY BILCCMS REPLACING LEADING ==CA-== BY ==CMA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCDST.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'DSTAPLY '
                 PERFORM 2000-DISTRIBUTE
              WHEN 'DSTUNDO '
                 PERFORM 2500-UNDO
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILPAYDS: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-DS-DISTRIBUTED
                                          CA-DS-REMAINING
                                          CA-DS-ITEMS-TOUCHED
                                          CA-DS-ITEMS-CLOSED
                                          WS-DISTRIBUTED
                                          WS-BKT-DUE
                                          WS-BKT-BILLED
                                          WS-BKT-UNBILLED
           MOVE CA-DS-MONEY-ID         TO WS-MONEY-ID
                                          WS-KEY-DISP
           MOVE WS-KEY-DISP            TO WS-ERR-KEY
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-BUS-DATE
           ELSE
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * DSTAPLY - distribute the receipt across the account's items
      ******************************************************************
       2000-DISTRIBUTE.
           PERFORM 3000-VALIDATE-APPLY
           MOVE CA-DS-AMOUNT           TO WS-REMAINING
           IF CA-DS-TARGET-INVOICE-ID > 0
      * BR-BIL-DST-003 The target invoice is settled before anything
      * else, past-due lines first then billed lines
              MOVE CA-DS-TARGET-INVOICE-ID TO WS-PASS-INVOICE-ID
              MOVE 'DUE '              TO WS-PASS-STATUS
              PERFORM 2100-RUN-PASS
              MOVE 'BILD'              TO WS-PASS-STATUS
              PERFORM 2100-RUN-PASS
           END-IF
      * BR-BIL-DST-004 Pass 1 - past-due items across the account
           MOVE 0                      TO WS-PASS-INVOICE-ID
           MOVE 'DUE '                 TO WS-PASS-STATUS
           PERFORM 2100-RUN-PASS
      * BR-BIL-DST-005 Pass 2 - billed but not yet due
           MOVE 'BILD'                 TO WS-PASS-STATUS
           PERFORM 2100-RUN-PASS
      * BR-BIL-DST-006 Pass 3 - planned items only when mode is 'A'
           IF CA-DS-ALL-ITEMS
              MOVE 'PLAN'              TO WS-PASS-STATUS
              PERFORM 2100-RUN-PASS
           END-IF

           IF WS-DISTRIBUTED > 0
              MOVE 'D'                 TO WS-UNAPPLD-SIDE
              MOVE 'C'                 TO WS-BUCKET-SIDE
              MOVE 'PAYDIST '          TO WS-TXN-TYPE
              PERFORM 8100-POST-LEDGER
      * BR-BIL-DST-012 What was not distributed stays unapplied
              COMPUTE WS-RCP-DELTA      = 0 - WS-DISTRIBUTED
              COMPUTE WS-DUE-DELTA      = 0 - WS-BKT-DUE
              COMPUTE WS-BILLED-DELTA   = 0 - WS-BKT-BILLED
              COMPUTE WS-UNBILLED-DELTA = 0 - WS-BKT-UNBILLED
              PERFORM 4700-UPDATE-RECEIPT
              PERFORM 4800-UPDATE-ACCOUNT
           END-IF
           MOVE WS-DISTRIBUTED         TO CA-DS-DISTRIBUTED
           MOVE WS-REMAINING           TO CA-DS-REMAINING
           IF WS-DISTRIBUTED = 0
              MOVE 'NOITEMS '          TO CA-REASON-CODE
              MOVE 'No open items to distribute to - held unapplied'
                                       TO CA-MESSAGE
              SET CA-RC-WARNING        TO TRUE
           END-IF.

      ******************************************************************
      * One pass: load up to WS-TAB-MAX items, apply them, and reload
      * while the table came back full and money remains. Settled
      * items drop out of the cursor predicate, so the reload resumes
      * where the previous block stopped.
       2100-RUN-PASS.
           SET WS-PASS-REPEAT          TO TRUE
           PERFORM UNTIL WS-PASS-DONE
                      OR WS-REMAINING NOT > 0
              PERFORM 4100-LOAD-PASS-ITEMS
              PERFORM 2200-APPLY-ITEM
                 VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-IT-COUNT
                      OR WS-REMAINING NOT > 0
              IF WS-IT-COUNT < WS-TAB-MAX
                 SET WS-PASS-DONE      TO TRUE
              END-IF
           END-PERFORM.

      ******************************************************************
       2200-APPLY-ITEM.
           COMPUTE WS-OUTSTANDING = WS-IT-AMOUNT(WS-IX)
                                  - WS-IT-PAID(WS-IX)
                                  - WS-IT-WOFF(WS-IX)
      * BR-BIL-DST-008 Item takes the lesser of outstanding and money
           IF WS-REMAINING < WS-OUTSTANDING
              MOVE WS-REMAINING        TO WS-APPLY
           ELSE
              MOVE WS-OUTSTANDING      TO WS-APPLY
           END-IF
           COMPUTE WS-NEW-PAID = WS-IT-PAID(WS-IX) + WS-APPLY
           IF WS-NEW-PAID + WS-IT-WOFF(WS-IX) >= WS-IT-AMOUNT(WS-IX)
              MOVE 'CLSD'              TO WS-NEW-STATUS
           ELSE
              MOVE WS-IT-STATUS(WS-IX) TO WS-NEW-STATUS
           END-IF

           MOVE WS-IT-ITEM-ID(WS-IX)   TO ITM-ITEM-ID
           MOVE WS-IT-PAID(WS-IX)      TO ITM-PAID-AMOUNT
           PERFORM 4200-UPDATE-ITEM-PAID

           PERFORM 8000-NEXT-DIST-ID
           MOVE WS-NEXT-DIST-ID        TO DIS-DIST-ID
           MOVE WS-MONEY-ID            TO DIS-MONEY-ID
           MOVE WS-IT-ITEM-ID(WS-IX)   TO DIS-ITEM-ID
           MOVE WS-ACCOUNT-ID          TO DIS-ACCOUNT-ID
           MOVE WS-APPLY               TO DIS-DIST-AMOUNT
           MOVE 'N'                    TO DIS-REVERSED-FLAG
           MOVE 0                      TO DIS-REVERSED-DIST-ID
           PERFORM 4300-INSERT-DISTRIBUTION

           IF WS-IT-INVOICE-ID(WS-IX) > 0
              MOVE WS-IT-INVOICE-ID(WS-IX) TO ITM-INVOICE-ID
              PERFORM 4400-REDUCE-INVOICE
           END-IF

      * BR-BIL-DST-010 Premium earns commission as it is paid
           IF WS-IT-PATTERN(WS-IX) = 'PREMIUM '
              MOVE 'CMSERN  '          TO WS-CMS-REQUEST
              PERFORM 8200-CALL-COMMISSION
           END-IF

           MOVE WS-IT-INVOICE-ID(WS-IX) TO ITM-INVOICE-ID
           MOVE WS-IT-DUE-DATE(WS-IX)  TO ITM-DUE-DATE
           PERFORM 5000-ADD-TO-BUCKET
           SUBTRACT WS-APPLY           FROM WS-REMAINING
           ADD WS-APPLY                TO WS-DISTRIBUTED
           ADD 1                       TO CA-DS-ITEMS-TOUCHED
           IF WS-NEW-PAID + WS-IT-WOFF(WS-IX) >= WS-IT-AMOUNT(WS-IX)
              ADD 1                    TO CA-DS-ITEMS-CLOSED
           END-IF.

      ******************************************************************
      * DSTUNDO - reverse every live distribution of the receipt
      ******************************************************************
       2500-UNDO.
           IF CA-DS-MONEY-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MONEYID '          TO CA-REASON-CODE
              MOVE 'Money id is required to undo distributions'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4000-READ-RECEIPT
           MOVE RCP-ACCOUNT-ID         TO WS-ACCOUNT-ID
           IF RCP-RECEIPT-STATUS = 'RVSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'RCPRVSD '          TO CA-REASON-CODE
              MOVE 'Receipt already reversed - nothing to undo'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           SET WS-PASS-REPEAT          TO TRUE
           PERFORM UNTIL WS-PASS-DONE
              PERFORM 4150-LOAD-LIVE-DISTS
              PERFORM 2600-UNDO-ONE
                 VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-DT-COUNT
              IF WS-DT-COUNT < WS-TAB-MAX
                 SET WS-PASS-DONE      TO TRUE
              END-IF
           END-PERFORM

           IF WS-DISTRIBUTED > 0
              PERFORM 4050-READ-ACCOUNT
              MOVE 'C'                 TO WS-UNAPPLD-SIDE
              MOVE 'D'                 TO WS-BUCKET-SIDE
              MOVE 'PAYUNDO '          TO WS-TXN-TYPE
              PERFORM 8100-POST-LEDGER
              MOVE WS-DISTRIBUTED      TO WS-RCP-DELTA
              MOVE WS-BKT-DUE          TO WS-DUE-DELTA
              MOVE WS-BKT-BILLED       TO WS-BILLED-DELTA
              MOVE WS-BKT-UNBILLED     TO WS-UNBILLED-DELTA
              PERFORM 4700-UPDATE-RECEIPT
              PERFORM 4800-UPDATE-ACCOUNT
           END-IF
           MOVE WS-DISTRIBUTED         TO CA-DS-DISTRIBUTED
           MOVE 0                      TO CA-DS-REMAINING.

      ******************************************************************
      * BR-BIL-DST-013 Mirror row, original flagged, item restored
       2600-UNDO-ONE.
           MOVE WS-DT-ITEM-ID(WS-IX)   TO ITM-ITEM-ID
           MOVE WS-DT-AMOUNT(WS-IX)    TO WS-APPLY
           PERFORM 4250-READ-ITEM
           COMPUTE WS-NEW-PAID = ITM-PAID-AMOUNT - WS-APPLY
           IF WS-NEW-PAID < 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'DISTINTG'          TO CA-REASON-CODE
              MOVE ITM-ITEM-ID         TO WS-KEY-DISP
              STRING 'Item ' WS-KEY-DISP
                     ' paid amount below its distributions'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 5100-RESTORED-STATUS
           PERFORM 4200-UPDATE-ITEM-PAID

           MOVE WS-DT-DIST-ID(WS-IX)   TO WS-ORIG-DIST-ID
           PERFORM 8000-NEXT-DIST-ID
           MOVE WS-NEXT-DIST-ID        TO DIS-DIST-ID
           MOVE WS-MONEY-ID            TO DIS-MONEY-ID
           MOVE ITM-ITEM-ID            TO DIS-ITEM-ID
           MOVE WS-ACCOUNT-ID          TO DIS-ACCOUNT-ID
           COMPUTE DIS-DIST-AMOUNT = 0 - WS-APPLY
           MOVE 'Y'                    TO DIS-REVERSED-FLAG
           MOVE WS-ORIG-DIST-ID        TO DIS-REVERSED-DIST-ID
           PERFORM 4300-INSERT-DISTRIBUTION
           PERFORM 4350-FLAG-ORIGINAL

           IF ITM-INVOICE-ID > 0
              PERFORM 4450-RESTORE-INVOICE
           END-IF
      * BR-BIL-DST-015 Earned commission follows the money back out
           IF CHG-CHARGE-PATTERN = 'PREMIUM '
              MOVE 'CMSRVS  '          TO WS-CMS-REQUEST
              PERFORM 8200-CALL-COMMISSION
           END-IF
           PERFORM 5000-ADD-TO-BUCKET
           ADD WS-APPLY                TO WS-DISTRIBUTED
           ADD 1                       TO CA-DS-ITEMS-TOUCHED.

      ******************************************************************
      * Validation for DSTAPLY
      ******************************************************************
       3000-VALIDATE-APPLY.
           IF CA-DS-MONEY-ID NOT > 0 OR CA-DS-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'KEYREQD '          TO CA-REASON-CODE
              MOVE 'Money id and account id are both required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-DS-MODE = SPACE OR LOW-VALUE
              SET CA-DS-DUE-ONLY       TO TRUE
           END-IF
           IF NOT CA-DS-DUE-ONLY AND NOT CA-DS-ALL-ITEMS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DSTMODE '          TO CA-REASON-CODE
              STRING 'Distribution mode ' CA-DS-MODE
                     ' invalid - use D or A'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-DS-ACCOUNT-ID       TO WS-ACCOUNT-ID
           PERFORM 4000-READ-RECEIPT

      * BR-BIL-DST-001 Receipt belongs to the account and is live
           IF RCP-ACCOUNT-ID NOT = CA-DS-ACCOUNT-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACCTMISM'          TO CA-REASON-CODE
              MOVE 'Receipt is not held on this account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF RCP-RECEIPT-STATUS NOT = 'RCVD'
              AND RCP-RECEIPT-STATUS NOT = 'APPL'
              SET CA-RC-STATE          TO TRUE
              MOVE 'RCPSTAT '          TO CA-REASON-CODE
              STRING 'Receipt status ' RCP-RECEIPT-STATUS
                     ' cannot be distributed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

      * BR-BIL-DST-002 Default to, and never exceed, unapplied money
           IF CA-DS-AMOUNT = 0
              MOVE RCP-UNAPPLIED-AMOUNT TO CA-DS-AMOUNT
           END-IF
           IF CA-DS-AMOUNT < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AMOUNT  '          TO CA-REASON-CODE
              MOVE 'Distribution amount cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-DS-AMOUNT > RCP-UNAPPLIED-AMOUNT
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'OVERAPLY'          TO CA-REASON-CODE
              MOVE RCP-UNAPPLIED-AMOUNT TO WS-AMOUNT-DISP
              STRING 'Amount exceeds unapplied money of '
                     WS-AMOUNT-DISP DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           PERFORM 4050-READ-ACCOUNT
           IF CA-DS-TARGET-INVOICE-ID > 0
              PERFORM 4060-CHECK-TARGET-INVOICE
           END-IF.

      ******************************************************************
      * Db2 access
      ******************************************************************
       4000-READ-RECEIPT.
           MOVE WS-MONEY-ID            TO RCP-MONEY-ID
           EXEC SQL
              SELECT ACCOUNT_ID, RECEIPT_AMOUNT, UNAPPLIED_AMOUNT,
                     RECEIPT_STATUS
                INTO :RCP-ACCOUNT-ID, :RCP-RECEIPT-AMOUNT,
                     :RCP-UNAPPLIED-AMOUNT, :RCP-RECEIPT-STATUS
                FROM BILDB.BIL_PAYMENT_RECEIPT
               WHERE MONEY_ID = :RCP-MONEY-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'RCPNOTFD'       TO CA-REASON-CODE
                 STRING 'Receipt ' WS-ERR-KEY(1:9) ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-RECEIPT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4050-READ-ACCOUNT.
           MOVE WS-ACCOUNT-ID          TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT VERSION_NO, ACCOUNT_STATUS
                INTO :ACT-VERSION-NO, :ACT-ACCOUNT-STATUS
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE ACT-VERSION-NO   TO WS-ACCT-VERSION
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 MOVE 'Billing account not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4050-READ-ACCOUNT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DST-003 Target invoice must be on the same account
       4060-CHECK-TARGET-INVOICE.
           EXEC SQL
              SELECT ACCOUNT_ID
                INTO :WS-INV-ACCOUNT-ID
                FROM BILDB.BIL_INVOICE
               WHERE INVOICE_ID = :CA-DS-TARGET-INVOICE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF WS-INV-ACCOUNT-ID NOT = WS-ACCOUNT-ID
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'INVACCT '    TO CA-REASON-CODE
                    MOVE 'Target invoice belongs to another account'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'INVNOTFD'       TO CA-REASON-CODE
                 MOVE 'Target invoice not found' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4060-CHECK-TARGET-INVOICE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DST-007 Both cursors break date ties on PRIORITY_RANK:
      * premium 1, tax 2, fees 5 (set by BILISCHD)
       4100-LOAD-PASS-ITEMS.
           MOVE 0                      TO WS-IT-COUNT
           SET WS-FETCH-MORE           TO TRUE
           IF WS-PASS-STATUS = 'PLAN'
              EXEC SQL OPEN CSR-PLAN-ITEMS END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4100-LOAD-PASS-ITEMS' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
              SET WS-PLAN-CSR-OPEN     TO TRUE
           ELSE
              EXEC SQL OPEN CSR-OPEN-ITEMS END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4100-LOAD-PASS-ITEMS' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
              SET WS-ITEM-CSR-OPEN     TO TRUE
           END-IF
           PERFORM 4110-FETCH-ITEM
              UNTIL WS-FETCH-END OR WS-IT-COUNT = WS-TAB-MAX
           PERFORM 4190-CLOSE-ITEM-CURSORS.

      ******************************************************************
       4110-FETCH-ITEM.
           IF WS-PLAN-CSR-OPEN
              EXEC SQL
                 FETCH CSR-PLAN-ITEMS
                  INTO :ITM-ITEM-ID, :ITM-INVOICE-ID,
                       :CHG-CHARGE-PATTERN, :ITM-ITEM-STATUS,
                       :ITM-DUE-DATE, :ITM-ITEM-AMOUNT,
                       :ITM-PAID-AMOUNT, :ITM-WRITEOFF-AMOUNT
              END-EXEC
           ELSE
              EXEC SQL
                 FETCH CSR-OPEN-ITEMS
                  INTO :ITM-ITEM-ID, :ITM-INVOICE-ID,
                       :CHG-CHARGE-PATTERN, :ITM-ITEM-STATUS,
                       :ITM-DUE-DATE, :ITM-ITEM-AMOUNT,
                       :ITM-PAID-AMOUNT, :ITM-WRITEOFF-AMOUNT
              END-EXEC
           END-IF
           EVALUATE SQLCODE
              WHEN 0
                 ADD 1                 TO WS-IT-COUNT
                 MOVE ITM-ITEM-ID      TO WS-IT-ITEM-ID(WS-IT-COUNT)
                 MOVE ITM-INVOICE-ID   TO WS-IT-INVOICE-ID(WS-IT-COUNT)
                 MOVE CHG-CHARGE-PATTERN
                                       TO WS-IT-PATTERN(WS-IT-COUNT)
                 MOVE ITM-ITEM-STATUS  TO WS-IT-STATUS(WS-IT-COUNT)
                 MOVE ITM-DUE-DATE     TO WS-IT-DUE-DATE(WS-IT-COUNT)
                 MOVE ITM-ITEM-AMOUNT  TO WS-IT-AMOUNT(WS-IT-COUNT)
                 MOVE ITM-PAID-AMOUNT  TO WS-IT-PAID(WS-IT-COUNT)
                 MOVE ITM-WRITEOFF-AMOUNT
                                       TO WS-IT-WOFF(WS-IT-COUNT)
              WHEN 100
                 SET WS-FETCH-END      TO TRUE
              WHEN OTHER
                 MOVE '4110-FETCH-ITEM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4150-LOAD-LIVE-DISTS.
           MOVE 0                      TO WS-DT-COUNT
           SET WS-FETCH-MORE           TO TRUE
           EXEC SQL OPEN CSR-LIVE-DISTS END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4150-LOAD-LIVE-DISTS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-DIST-CSR-OPEN        TO TRUE
           PERFORM UNTIL WS-FETCH-END OR WS-DT-COUNT = WS-TAB-MAX
              EXEC SQL
                 FETCH CSR-LIVE-DISTS
                  INTO :DIS-DIST-ID, :DIS-ITEM-ID, :DIS-DIST-AMOUNT
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-DT-COUNT
                    MOVE DIS-DIST-ID   TO WS-DT-DIST-ID(WS-DT-COUNT)
                    MOVE DIS-ITEM-ID   TO WS-DT-ITEM-ID(WS-DT-COUNT)
                    MOVE DIS-DIST-AMOUNT
                                       TO WS-DT-AMOUNT(WS-DT-COUNT)
                 WHEN 100
                    SET WS-FETCH-END   TO TRUE
                 WHEN OTHER
                    MOVE '4150-LOAD-LIVE-DISTS' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CSR-LIVE-DISTS END-EXEC
           MOVE 'N'                    TO WS-DIST-CSR-SW.

      ******************************************************************
       4190-CLOSE-ITEM-CURSORS.
           IF WS-PLAN-CSR-OPEN
              EXEC SQL CLOSE CSR-PLAN-ITEMS END-EXEC
              MOVE 'N'                 TO WS-PLAN-CSR-SW
           END-IF
           IF WS-ITEM-CSR-OPEN
              EXEC SQL CLOSE CSR-OPEN-ITEMS END-EXEC
              MOVE 'N'                 TO WS-ITEM-CSR-SW
           END-IF.

      ******************************************************************
      * BR-BIL-DST-016 The item is only changed if PAID_AMOUNT is still
      * what this unit of work read; ITM-PAID-AMOUNT holds that value.
       4200-UPDATE-ITEM-PAID.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET PAID_AMOUNT = :WS-NEW-PAID,
                     ITEM_STATUS = :WS-NEW-STATUS,
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE ITEM_ID     = :ITM-ITEM-ID
                 AND PAID_AMOUNT = :ITM-PAID-AMOUNT
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE ITM-ITEM-ID      TO WS-KEY-DISP
                 STRING 'Invoice item ' WS-KEY-DISP
                        ' changed by another task - retry'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4200-UPDATE-ITEM-PAID' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4250-READ-ITEM.
           EXEC SQL
              SELECT I.INVOICE_ID, C.CHARGE_PATTERN, I.ITEM_STATUS,
                     I.DUE_DATE, I.ITEM_AMOUNT, I.PAID_AMOUNT,
                     I.WRITEOFF_AMOUNT
                INTO :ITM-INVOICE-ID, :CHG-CHARGE-PATTERN,
                     :ITM-ITEM-STATUS, :ITM-DUE-DATE,
                     :ITM-ITEM-AMOUNT, :ITM-PAID-AMOUNT,
                     :ITM-WRITEOFF-AMOUNT
                FROM BILDB.BIL_INVOICE_ITEM I
                JOIN BILDB.BIL_CHARGE C
                  ON C.CHARGE_ID = I.CHARGE_ID
               WHERE I.ITEM_ID = :ITM-ITEM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'DISTINTG'       TO CA-REASON-CODE
                 MOVE ITM-ITEM-ID      TO WS-KEY-DISP
                 STRING 'Distributed item ' WS-KEY-DISP
                        ' no longer exists'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4250-READ-ITEM' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-INSERT-DISTRIBUTION.
           MOVE WS-BUS-DATE            TO DIS-DIST-DATE
           EXEC SQL
              INSERT INTO BILDB.BIL_DISTRIBUTION
                    (DIST_ID, MONEY_ID, ITEM_ID, ACCOUNT_ID,
                     DIST_AMOUNT, DIST_DATE, REVERSED_FLAG,
                     REVERSED_DIST_ID, CREATE_TS)
              VALUES (:DIS-DIST-ID, :DIS-MONEY-ID, :DIS-ITEM-ID,
                      :DIS-ACCOUNT-ID, :DIS-DIST-AMOUNT,
                      :DIS-DIST-DATE, :DIS-REVERSED-FLAG,
                      :DIS-REVERSED-DIST-ID, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPDIST '       TO CA-REASON-CODE
                 MOVE 'Distribution id already used - sequence DIS'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4300-INSERT-DISTRIBUTION' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4350-FLAG-ORIGINAL.
           EXEC SQL
              UPDATE BILDB.BIL_DISTRIBUTION
                 SET REVERSED_FLAG = 'Y'
               WHERE DIST_ID       = :WS-ORIG-DIST-ID
                 AND REVERSED_FLAG = 'N'
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Distribution reversed by another task'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4350-FLAG-ORIGINAL' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DST-009 Invoice due follows its items; PAID at zero
       4400-REDUCE-INVOICE.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE
                 SET AMOUNT_DUE     = AMOUNT_DUE - :WS-APPLY,
                     INVOICE_STATUS =
                        CASE WHEN AMOUNT_DUE - :WS-APPLY <= 0
                             THEN 'PAID'
                             ELSE INVOICE_STATUS END,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE INVOICE_ID     = :ITM-INVOICE-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4400-REDUCE-INVOICE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4450-RESTORE-INVOICE.
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE
                 SET AMOUNT_DUE     = AMOUNT_DUE + :WS-APPLY,
                     INVOICE_STATUS =
                        CASE WHEN INVOICE_STATUS <> 'PAID'
                             THEN INVOICE_STATUS
                             WHEN DUE_DATE <= :WS-BUS-DATE
                             THEN 'DUE '
                             ELSE 'BILD' END,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE INVOICE_ID     = :ITM-INVOICE-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4450-RESTORE-INVOICE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-DST-016 Receipt guarded by the unapplied amount read
       4700-UPDATE-RECEIPT.
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_RECEIPT
                 SET UNAPPLIED_AMOUNT = UNAPPLIED_AMOUNT
                                      + :WS-RCP-DELTA,
                     RECEIPT_STATUS   =
                        CASE WHEN UNAPPLIED_AMOUNT + :WS-RCP-DELTA = 0
                             THEN 'APPL'
                             ELSE 'RCVD' END,
                     UPDATE_TS        = CURRENT TIMESTAMP
               WHERE MONEY_ID         = :RCP-MONEY-ID
                 AND UNAPPLIED_AMOUNT = :RCP-UNAPPLIED-AMOUNT
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Receipt changed by another task - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4700-UPDATE-RECEIPT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DST-012/016 Account balances move with the money,
      * optimistic lock on VERSION_NO
       4800-UPDATE-ACCOUNT.
           MOVE WS-ACCOUNT-ID          TO ACT-ACCOUNT-ID
           MOVE WS-ACCT-VERSION        TO ACT-VERSION-NO
           EXEC SQL
              UPDATE BILDB.BIL_ACCOUNT
                 SET UNAPPLIED_AMT = UNAPPLIED_AMT + :WS-RCP-DELTA,
                     DUE_AMT       = DUE_AMT       + :WS-DUE-DELTA,
                     BILLED_AMT    = BILLED_AMT    + :WS-BILLED-DELTA,
                     UNBILLED_AMT  = UNBILLED_AMT
                                   + :WS-UNBILLED-DELTA,
                     UPDATE_TS     = CURRENT TIMESTAMP,
                     VERSION_NO    = VERSION_NO + 1
               WHERE ACCOUNT_ID    = :ACT-ACCOUNT-ID
                 AND VERSION_NO    = :ACT-VERSION-NO
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) > 0
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Account changed by another task - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4800-UPDATE-ACCOUNT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4900-READ-BUS-DATE.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-READ-BUS-DATE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
      * Calculations
      ******************************************************************
      * BR-BIL-DST-011 The ledger bucket matches the account balance
      * buckets (BILACCT0 ACCTBAL): never invoiced -> UNBILLED,
      * invoiced and due after today -> BILLED, otherwise DUE.
       5000-ADD-TO-BUCKET.
           EVALUATE TRUE
              WHEN ITM-INVOICE-ID = 0
                 ADD WS-APPLY          TO WS-BKT-UNBILLED
              WHEN ITM-DUE-DATE > WS-BUS-DATE
                 ADD WS-APPLY          TO WS-BKT-BILLED
              WHEN OTHER
                 ADD WS-APPLY          TO WS-BKT-DUE
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DST-014 An item reopened by an undo goes back to PLAN
      * when it was never invoiced, DUE when its due date has been
      * reached, BILD otherwise. It stays CLSD only when write-offs
      * alone still settle it.
       5100-RESTORED-STATUS.
           IF WS-NEW-PAID + ITM-WRITEOFF-AMOUNT >= ITM-ITEM-AMOUNT
              MOVE 'CLSD'              TO WS-NEW-STATUS
           ELSE
              EVALUATE TRUE
                 WHEN ITM-INVOICE-ID = 0
                    MOVE 'PLAN'        TO WS-NEW-STATUS
                 WHEN ITM-DUE-DATE <= WS-BUS-DATE
                    MOVE 'DUE '        TO WS-NEW-STATUS
                 WHEN OTHER
                    MOVE 'BILD'        TO WS-NEW-STATUS
              END-EVALUATE
           END-IF.

      ******************************************************************
      * Calls to other services
      ******************************************************************
       8000-NEXT-DIST-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'DIS     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 12
              MOVE SQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQA-REASON-CODE     TO CA-REASON-CODE
              MOVE SQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SQA-SQ-VALUE           TO WS-NEXT-DIST-ID.

      ******************************************************************
      * BR-BIL-DST-011 One balanced entry for the whole batch. Apply:
      * DR UNAPPLD / CR buckets. Undo: DR buckets / CR UNAPPLD.
       8100-POST-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE WS-TXN-TYPE            TO GLA-GL-TXN-TYPE
           MOVE WS-ACCOUNT-ID          TO GLA-GL-ACCOUNT-ID
           MOVE 'RCPT'                 TO GLA-GL-REF-TYPE
           MOVE WS-MONEY-ID            TO GLA-GL-REF-ID
           MOVE 1                      TO GLA-GL-LINE-COUNT
           MOVE 'UNAPPLD '             TO GLA-GL-T-ACCOUNT(1)
           MOVE WS-UNAPPLD-SIDE        TO GLA-GL-DR-CR(1)
           MOVE WS-DISTRIBUTED         TO GLA-GL-AMOUNT(1)
           IF WS-BKT-DUE > 0
              ADD 1                    TO GLA-GL-LINE-COUNT
              MOVE 'DUE     '    TO GLA-GL-T-ACCOUNT(GLA-GL-LINE-COUNT)
              MOVE WS-BUCKET-SIDE TO GLA-GL-DR-CR(GLA-GL-LINE-COUNT)
              MOVE WS-BKT-DUE     TO GLA-GL-AMOUNT(GLA-GL-LINE-COUNT)
           END-IF
           IF WS-BKT-BILLED > 0
              ADD 1                    TO GLA-GL-LINE-COUNT
              MOVE 'BILLED  '    TO GLA-GL-T-ACCOUNT(GLA-GL-LINE-COUNT)
              MOVE WS-BUCKET-SIDE TO GLA-GL-DR-CR(GLA-GL-LINE-COUNT)
              MOVE WS-BKT-BILLED  TO GLA-GL-AMOUNT(GLA-GL-LINE-COUNT)
           END-IF
           IF WS-BKT-UNBILLED > 0
              ADD 1                    TO GLA-GL-LINE-COUNT
              MOVE 'UNBILLED'    TO GLA-GL-T-ACCOUNT(GLA-GL-LINE-COUNT)
              MOVE WS-BUCKET-SIDE TO GLA-GL-DR-CR(GLA-GL-LINE-COUNT)
              MOVE WS-BKT-UNBILLED TO GLA-GL-AMOUNT(GLA-GL-LINE-COUNT)
           END-IF
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLA-RETURN-CODE >= 12
              MOVE GLA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE GLA-REASON-CODE     TO CA-REASON-CODE
              MOVE GLA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * CMSERN / CMSRVS for the item in ITM-/WS-IT- and WS-APPLY. An
      * item with no reserved commission (direct business) is RC 08.
       8200-CALL-COMMISSION.
           INITIALIZE WS-CMS-CA
           MOVE WS-CMS-REQUEST         TO CMA-REQUEST-ID
           MOVE CA-USER-ID             TO CMA-USER-ID
           MOVE WS-BUS-DATE            TO CMA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CMA-CALLER-PGM
           MOVE ITM-ITEM-ID            TO CMA-CM-ITEM-ID
           IF CMA-REQUEST-ID = 'CMSERN  '
              MOVE WS-APPLY            TO CMA-CM-PAID-AMOUNT
           ELSE
              MOVE WS-APPLY            TO CMA-CM-REVERSED-AMOUNT
           END-IF
           CALL WS-CMS-PGM USING WS-CMS-CA
           IF CMA-RETURN-CODE >= 12
              MOVE CMA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE CMA-REASON-CODE     TO CA-REASON-CODE
              MOVE CMA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           PERFORM 4190-CLOSE-ITEM-CURSORS
           IF WS-DIST-CSR-OPEN
              EXEC SQL CLOSE CSR-LIVE-DISTS END-EXEC
              MOVE 'N'                 TO WS-DIST-CSR-SW
           END-IF
           GOBACK.

      ******************************************************************
       9800-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Distribution rows locked - retry the receipt'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           IF SQLCODE = -911 OR SQLCODE = -913
              PERFORM 9800-DEADLOCK
           END-IF
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'BILPAYDS Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
