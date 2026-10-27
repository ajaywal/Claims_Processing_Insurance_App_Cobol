       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILWOFF0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILWOFF0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Write-off service. Writes off the open amount of *
      *               one billed invoice item, sweeps an account's     *
      *               small past-due balances below its delinquency    *
      *               plan threshold, approves write-offs that were    *
      *               held for a higher authority, and reverses or     *
      *               cancels write-offs.                              *
      * REQUESTS    : WOFCRT  create (ITEM-ID), sweep (ACCOUNT-ID      *
      *                       only) or approve a PEND write-off        *
      *                       (WO-ID)                                  *
      *               WOFREV  reverse an approved write-off or cancel  *
      *                       a pending one                            *
      * CALLED BY   : BILWOFP (screen), BILDLQ family (small balance   *
      *               sweep at end of delinquency), API adapter        *
      * CALLS       : BILSEQGN BILCODVL BILAUTHZ BILGLPST BILAUDIT     *
      *               BILACCT0 BILERRLG                                *
      * TABLES      : BIL_WRITEOFF          (C R U)                    *
      *               BIL_INVOICE_ITEM      (R U)                      *
      *               BIL_ACCOUNT           (R)                        *
      *               BIL_DELINQ_PLAN       (R)                        *
      *               BIL_TRANSACTION       (R)                        *
      *               BIL_CONTROL           (R)                        *
      * RULES       : BR-BIL-WOF-001 Write-off reason must be an       *
      *                              active WOREASON code; a sweep     *
      *                              defaults to SMALLBAL              *
      *               BR-BIL-WOF-002 Only billed items (BILD / DUE)    *
      *                              with a positive open amount can   *
      *                              be written off                    *
      *               BR-BIL-WOF-003 Amount must not exceed the open   *
      *                              amount; zero means all of it      *
      *               BR-BIL-WOF-004 Over the user's WRITEOFF limit    *
      *                              the write-off is held PEND for    *
      *                              the approver with no financial    *
      *                              effect                            *
      *               BR-BIL-WOF-005 One pending write-off per item    *
      *               BR-BIL-WOF-006 A pending write-off is approved   *
      *                              by someone other than the         *
      *                              requester, within their limit,    *
      *                              while the item still has the open *
      *                              amount                            *
      *               BR-BIL-WOF-007 Ledger DR WRITEOFF / CR DUE (CR   *
      *                              BILLED while not yet due)         *
      *               BR-BIL-WOF-008 Item is CLSD once paid plus       *
      *                              written off covers its amount     *
      *               BR-BIL-WOF-009 Sweep takes billed items past     *
      *                              their due date whose open amount  *
      *                              is below the delinquency plan     *
      *                              WRITEOFF_THRESHOLD                *
      *               BR-BIL-WOF-010 Reversal needs REVERSAL authority *
      *                              and an approved, unreversed       *
      *                              write-off; a PEND one is simply   *
      *                              cancelled                         *
      *               BR-BIL-WOF-011 Reversal reopens a CLSD item as   *
      *                              DUE when past due, else BILD      *
      *               BR-BIL-WOF-012 Reversal posts the mirror entry   *
      *                              (GLREVS) of the original posting  *
      *               BR-BIL-WOF-013 Items are updated only if paid,   *
      *                              written-off and status are        *
      *                              unchanged since read              *
      *               BR-BIL-WOF-014 Account balances are recomputed   *
      *                              after every financial change      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILWOFF0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'BILCODVL'.
       01  WS-AUT-PGM                  PIC X(8)  VALUE 'BILAUTHZ'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-MAX-SWEEP             PIC S9(4) COMP VALUE 100.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-ACCOUNT-ID            PIC S9(9) COMP VALUE 0.
           05 WS-WO-AMOUNT             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OPEN-AMOUNT           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-NEW-WO-AMOUNT         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OLD-ITEM-STATUS       PIC X(4)  VALUE SPACES.
           05 WS-NEW-ITEM-STATUS       PIC X(4)  VALUE SPACES.
           05 WS-OLD-PAID              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-OLD-WO                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-WO-STATUS             PIC X(4)  VALUE SPACES.
           05 WS-OLD-WO-STATUS         PIC X(4)  VALUE SPACES.
           05 WS-APPROVER              PIC X(8)  VALUE SPACES.
           05 WS-REASON                PIC X(8)  VALUE SPACES.
           05 WS-PEND-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-ORIG-TXN-ID           PIC S9(9) COMP VALUE 0.
           05 WS-STAGE-ACCT            PIC X(8)  VALUE SPACES.
           05 WS-LIMIT-TYPE            PIC X(8)  VALUE SPACES.
           05 WS-GL-TXN-TYPE           PIC X(8)  VALUE SPACES.
           05 WS-GL-DR-ACCT            PIC X(8)  VALUE SPACES.
           05 WS-GL-CR-ACCT            PIC X(8)  VALUE SPACES.
           05 WS-AUD-ACTION            PIC X(4)  VALUE SPACES.
           05 WS-AUD-BEFORE            PIC X(100) VALUE SPACES.
           05 WS-AUD-AFTER             PIC X(100) VALUE SPACES.
           05 WS-IX                    PIC S9(4) COMP VALUE 0.
           05 WS-PEND-HELD             PIC S9(4) COMP VALUE 0.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-AMT-DISP              PIC -(9)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN                  VALUE 'Y'.
              88 WS-CURSOR-CLOSED                VALUE 'N'.

       01  WS-CALLEE.
           05 WS-CALLEE-RC             PIC 9(2).
           05 WS-CALLEE-REASON         PIC X(8).
           05 WS-CALLEE-MESSAGE        PIC X(80).

      * Small-balance items selected by a sweep
       01  WS-SWEEP-TABLE.
           05 WS-SW-COUNT              PIC S9(4) COMP VALUE 0.
           05 WS-SW-ITEM-ID            PIC S9(9) COMP OCCURS 100.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDWOF END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDDPL END-EXEC.
           EXEC SQL INCLUDE BILDTXN END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-WOF-009 Past-due billed items below the threshold with
      * no write-off already waiting for approval
           EXEC SQL
              DECLARE C-SMALL-ITEMS CURSOR FOR
               SELECT I.ITEM_ID
                 FROM BILDB.BIL_INVOICE_ITEM I
                WHERE I.ACCOUNT_ID  = :ACT-ACCOUNT-ID
                  AND I.ITEM_STATUS IN ('BILD', 'DUE ')
                  AND I.DUE_DATE    < :WS-BUS-DATE
                  AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                      - I.WRITEOFF_AMOUNT > 0
                  AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                      - I.WRITEOFF_AMOUNT < :DPL-WRITEOFF-THRESHOLD
                  AND NOT EXISTS
                      (SELECT 1
                         FROM BILDB.BIL_WRITEOFF W
                        WHERE W.ITEM_ID   = I.ITEM_ID
                          AND W.WO_STATUS = 'PEND')
                ORDER BY I.DUE_DATE, I.ITEM_ID
                FETCH FIRST 100 ROWS ONLY
                FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-COD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY BILCCOD REPLACING LEADING ==CA-== BY ==CDA-==.
       01  WS-AUT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ATA-==.
           03 ATA-BODY.
              COPY BILCAUT REPLACING LEADING ==CA-== BY ==ATA-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.
       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.
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
              COPY BILCWOF.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'WOFCRT  '
                 EVALUATE TRUE
                    WHEN CA-WO-WO-ID > 0
                       PERFORM 2100-APPROVE-PENDING
                    WHEN CA-WO-ITEM-ID > 0
                       PERFORM 2000-WRITE-OFF-ITEM
                    WHEN CA-WO-ACCOUNT-ID > 0
                       PERFORM 2200-SWEEP-ACCOUNT
                    WHEN OTHER
                       SET CA-RC-VALIDATION TO TRUE
                       MOVE 'WOKEYREQ'  TO CA-REASON-CODE
                       MOVE 'Write-off needs an item, account or WO id'
                                        TO CA-MESSAGE
                 END-EVALUATE
              WHEN 'WOFREV  '
                 PERFORM 2300-REVERSE-WRITEOFF
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILWOFF0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-WO-ITEMS-WRITTEN
                                          CA-WO-TOTAL-WRITTEN
           SET WS-CURSOR-CLOSED        TO TRUE
           IF CA-BUS-DATE = SPACES OR LOW-VALUES
              PERFORM 4900-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO CA-BUS-DATE
           END-IF
           MOVE CA-BUS-DATE            TO WS-BUS-DATE.

      ******************************************************************
      * WOFCRT by item - write off all or part of one item
       2000-WRITE-OFF-ITEM.
           IF CA-WO-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'WORSNREQ'          TO CA-REASON-CODE
              MOVE 'Write-off reason is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-WO-REASON           TO WS-REASON
           PERFORM 8100-VALIDATE-REASON
           MOVE CA-WO-ITEM-ID          TO ITM-ITEM-ID
           PERFORM 4000-READ-ITEM
           IF CA-WO-ACCOUNT-ID > 0
              AND CA-WO-ACCOUNT-ID NOT = ITM-ACCOUNT-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ITMACCT '          TO CA-REASON-CODE
              MOVE 'Invoice item belongs to another account'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3000-VALIDATE-ITEM-OPEN
      * BR-BIL-WOF-003 Zero = whole open amount, never more than open
           EVALUATE TRUE
              WHEN CA-WO-AMOUNT = 0
                 MOVE WS-OPEN-AMOUNT   TO WS-WO-AMOUNT
              WHEN CA-WO-AMOUNT < 0
                OR CA-WO-AMOUNT > WS-OPEN-AMOUNT
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'WOAMOUNT'       TO CA-REASON-CODE
                 MOVE WS-OPEN-AMOUNT   TO WS-AMT-DISP
                 STRING 'Write-off must be between 0.01 and open '
                        'amount ' WS-AMT-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE CA-WO-AMOUNT     TO WS-WO-AMOUNT
           END-EVALUATE
      * BR-BIL-WOF-005 One pending write-off per item
           PERFORM 4010-COUNT-PENDING
           IF WS-PEND-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'WOPENDNG'          TO CA-REASON-CODE
              MOVE 'Item already has a write-off awaiting approval'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE ITM-ACCOUNT-ID         TO WS-ACCOUNT-ID
           PERFORM 2010-AUTHORISE-AND-RECORD
           PERFORM 8600-REFRESH-BALANCES
           MOVE WOF-WO-ID              TO CA-WO-WO-ID
           MOVE WOF-WO-STATUS          TO CA-WO-STATUS
           MOVE WS-ACCOUNT-ID          TO CA-WO-ACCOUNT-ID
           IF WOF-WO-STATUS = 'PEND'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'WOPENDAP'          TO CA-REASON-CODE
              STRING 'Over authority - write-off pending approval by '
                     WOF-APPROVER-USER DELIMITED BY SIZE
                     INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-BIL-WOF-004 Within limit -> APPR and apply, else PEND
       2010-AUTHORISE-AND-RECORD.
           MOVE 'WRITEOFF'             TO WS-LIMIT-TYPE
           PERFORM 8200-CHECK-AUTHORITY
           IF ATA-AT-IS-ALLOWED
              MOVE 'APPR'              TO WS-WO-STATUS
              MOVE CA-USER-ID          TO WS-APPROVER
           ELSE
              MOVE 'PEND'              TO WS-WO-STATUS
              MOVE ATA-AT-APPROVER     TO WS-APPROVER
           END-IF
           PERFORM 8000-NEXT-WO-ID
           MOVE SQA-SQ-VALUE           TO WOF-WO-ID
           MOVE WS-ACCOUNT-ID          TO WOF-ACCOUNT-ID
           MOVE ITM-ITEM-ID            TO WOF-ITEM-ID
           MOVE WS-WO-AMOUNT           TO WOF-WO-AMOUNT
           MOVE WS-REASON              TO WOF-WO-REASON
           MOVE WS-WO-STATUS           TO WOF-WO-STATUS
           MOVE CA-USER-ID             TO WOF-REQUEST-USER
           MOVE WS-APPROVER            TO WOF-APPROVER-USER
           MOVE WS-BUS-DATE            TO WOF-WO-DATE
           MOVE 'N'                    TO WOF-REVERSED-FLAG
           PERFORM 4100-INSERT-WRITEOFF
           IF WOF-WO-STATUS = 'APPR'
              PERFORM 5000-APPLY-TO-ITEM
              ADD 1                    TO CA-WO-ITEMS-WRITTEN
              ADD WS-WO-AMOUNT         TO CA-WO-TOTAL-WRITTEN
           ELSE
              ADD 1                    TO WS-PEND-HELD
           END-IF
           MOVE 'ADD '                 TO WS-AUD-ACTION
           MOVE SPACES                 TO WS-AUD-BEFORE
           MOVE WS-WO-AMOUNT           TO WS-AMT-DISP
           STRING WOF-WO-STATUS ' ' WS-REASON ' ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-AUD-AFTER
           PERFORM 8400-WRITE-AUDIT.

      ******************************************************************
      * WOFCRT by WO id - approve a write-off held for authority
      * BR-BIL-WOF-006 Different user, within limit, still open
       2100-APPROVE-PENDING.
           MOVE CA-WO-WO-ID            TO WOF-WO-ID
           PERFORM 4200-READ-WRITEOFF
           IF WOF-WO-STATUS NOT = 'PEND'
              SET CA-RC-STATE          TO TRUE
              MOVE 'WONOTPND'          TO CA-REASON-CODE
              STRING 'Write-off status ' WOF-WO-STATUS
                     ' cannot be approved' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WOF-REQUEST-USER = CA-USER-ID
              SET CA-RC-STATE          TO TRUE
              MOVE 'WOSELFAP'          TO CA-REASON-CODE
              MOVE 'A write-off cannot be approved by its requester'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WOF-WO-AMOUNT          TO WS-WO-AMOUNT
           MOVE WOF-ACCOUNT-ID         TO WS-ACCOUNT-ID
           MOVE 'WRITEOFF'             TO WS-LIMIT-TYPE
           PERFORM 8200-CHECK-AUTHORITY
           IF NOT ATA-AT-IS-ALLOWED
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'AUTHLIMT'          TO CA-REASON-CODE
              MOVE ATA-AT-LIMIT        TO WS-AMT-DISP
              STRING 'Approval exceeds your write-off limit '
                     WS-AMT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WOF-ITEM-ID            TO ITM-ITEM-ID
           PERFORM 4000-READ-ITEM
           PERFORM 3000-VALIDATE-ITEM-OPEN
           IF WS-WO-AMOUNT > WS-OPEN-AMOUNT
              SET CA-RC-STATE          TO TRUE
              MOVE 'WOSTALE '          TO CA-REASON-CODE
              MOVE 'Item open amount fell below the pending write-off'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'PEND'                 TO WS-OLD-WO-STATUS
           MOVE 'APPR'                 TO WOF-WO-STATUS
           MOVE CA-USER-ID             TO WOF-APPROVER-USER
           PERFORM 4210-UPDATE-WRITEOFF-STATUS
           PERFORM 5000-APPLY-TO-ITEM
           PERFORM 8600-REFRESH-BALANCES
           MOVE 'STAT'                 TO WS-AUD-ACTION
           MOVE 'PEND'                 TO WS-AUD-BEFORE
           MOVE 'APPR'                 TO WS-AUD-AFTER
           PERFORM 8400-WRITE-AUDIT
           MOVE 1                      TO CA-WO-ITEMS-WRITTEN
           MOVE WS-WO-AMOUNT           TO CA-WO-TOTAL-WRITTEN
           MOVE WOF-WO-STATUS          TO CA-WO-STATUS
           MOVE WOF-ITEM-ID            TO CA-WO-ITEM-ID
           MOVE WS-ACCOUNT-ID          TO CA-WO-ACCOUNT-ID.

      ******************************************************************
      * WOFCRT by account - small balance sweep
       2200-SWEEP-ACCOUNT.
           MOVE CA-WO-ACCOUNT-ID       TO WS-ACCOUNT-ID
           PERFORM 4300-READ-ACCOUNT
           PERFORM 4310-READ-DELINQ-PLAN
           IF DPL-WRITEOFF-THRESHOLD NOT > 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'WONOTHRS'          TO CA-REASON-CODE
              STRING 'Delinquency plan ' DPL-PLAN-CODE
                     ' has no write-off threshold'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-WOF-001 Sweep reason defaults to SMALLBAL
           IF CA-WO-REASON = SPACES
              MOVE 'SMALLBAL'          TO WS-REASON
           ELSE
              MOVE CA-WO-REASON        TO WS-REASON
           END-IF
           PERFORM 8100-VALIDATE-REASON
           PERFORM 4400-LOAD-SMALL-ITEMS
           IF WS-SW-COUNT = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'WONOITEM'          TO CA-REASON-CODE
              MOVE 'No small balances below threshold to write off'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1
                     UNTIL WS-IX > WS-SW-COUNT
              MOVE WS-SW-ITEM-ID(WS-IX) TO ITM-ITEM-ID
              PERFORM 4000-READ-ITEM
              PERFORM 2210-SWEEP-ONE-ITEM
           END-PERFORM
           PERFORM 8600-REFRESH-BALANCES
           MOVE WS-REASON              TO CA-WO-REASON
           IF WS-PEND-HELD > 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'WOPENDAP'          TO CA-REASON-CODE
              MOVE 'Some small balances are pending approval'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * Re-check each item: it may have been paid since the cursor ran
       2210-SWEEP-ONE-ITEM.
           COMPUTE WS-OPEN-AMOUNT = ITM-ITEM-AMOUNT - ITM-PAID-AMOUNT
                                  - ITM-WRITEOFF-AMOUNT
           IF (ITM-ITEM-STATUS = 'BILD' OR ITM-ITEM-STATUS = 'DUE ')
              AND WS-OPEN-AMOUNT > 0
              AND WS-OPEN-AMOUNT < DPL-WRITEOFF-THRESHOLD
              MOVE WS-OPEN-AMOUNT      TO WS-WO-AMOUNT
              PERFORM 2010-AUTHORISE-AND-RECORD
              MOVE WOF-WO-ID           TO CA-WO-WO-ID
              MOVE WOF-WO-STATUS       TO CA-WO-STATUS
           END-IF.

      ******************************************************************
      * WOFREV - reverse an approved write-off / cancel a pending one
       2300-REVERSE-WRITEOFF.
           IF CA-WO-WO-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'WOIDREQ '          TO CA-REASON-CODE
              MOVE 'Write-off id is required for a reversal'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-WO-WO-ID            TO WOF-WO-ID
           PERFORM 4200-READ-WRITEOFF
           MOVE WOF-ACCOUNT-ID         TO WS-ACCOUNT-ID
           MOVE WOF-WO-AMOUNT          TO WS-WO-AMOUNT
           EVALUATE TRUE
              WHEN WOF-WO-STATUS = 'PEND'
                 PERFORM 2310-CANCEL-PENDING
              WHEN WOF-WO-STATUS = 'APPR'
               AND WOF-REVERSED-FLAG = 'N'
                 PERFORM 2320-REVERSE-APPROVED
              WHEN OTHER
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'WONOTREV'       TO CA-REASON-CODE
                 STRING 'Write-off status ' WOF-WO-STATUS
                        ' cannot be reversed' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           MOVE WOF-WO-STATUS          TO CA-WO-STATUS
           MOVE WOF-ITEM-ID            TO CA-WO-ITEM-ID
           MOVE WS-ACCOUNT-ID          TO CA-WO-ACCOUNT-ID
           MOVE WS-WO-AMOUNT           TO CA-WO-AMOUNT.

      ******************************************************************
      * BR-BIL-WOF-010 A pending write-off is cancelled, no postings
       2310-CANCEL-PENDING.
           MOVE 'PEND'                 TO WS-OLD-WO-STATUS
           MOVE 'CANC'                 TO WOF-WO-STATUS
           PERFORM 4210-UPDATE-WRITEOFF-STATUS
           MOVE 'STAT'                 TO WS-AUD-ACTION
           MOVE 'PEND'                 TO WS-AUD-BEFORE
           MOVE 'CANC'                 TO WS-AUD-AFTER
           PERFORM 8400-WRITE-AUDIT.

      ******************************************************************
      * BR-BIL-WOF-010 Reversal of an approved write-off needs
      * REVERSAL authority for its amount
       2320-REVERSE-APPROVED.
           MOVE 'REVERSAL'             TO WS-LIMIT-TYPE
           PERFORM 8200-CHECK-AUTHORITY
           IF NOT ATA-AT-IS-ALLOWED
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'AUTHLIMT'          TO CA-REASON-CODE
              MOVE ATA-AT-LIMIT        TO WS-AMT-DISP
              STRING 'Reversal exceeds your limit ' WS-AMT-DISP
                     ' - refer to ' ATA-AT-APPROVER
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WOF-ITEM-ID            TO ITM-ITEM-ID
           PERFORM 4000-READ-ITEM
           IF ITM-WRITEOFF-AMOUNT < WS-WO-AMOUNT
              SET CA-RC-STATE          TO TRUE
              MOVE 'WOITMCHG'          TO CA-REASON-CODE
              MOVE 'Item write-off total is below this write-off'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE ITM-ITEM-STATUS        TO WS-OLD-ITEM-STATUS
           MOVE ITM-PAID-AMOUNT        TO WS-OLD-PAID
           MOVE ITM-WRITEOFF-AMOUNT    TO WS-OLD-WO
           COMPUTE WS-NEW-WO-AMOUNT = ITM-WRITEOFF-AMOUNT
                                    - WS-WO-AMOUNT
           MOVE ITM-ITEM-STATUS        TO WS-NEW-ITEM-STATUS
      * BR-BIL-WOF-011 Reopen a closed item at its ageing stage
           IF ITM-ITEM-STATUS = 'CLSD'
              IF ITM-DUE-DATE < WS-BUS-DATE
                 MOVE 'DUE '           TO WS-NEW-ITEM-STATUS
              ELSE
                 MOVE 'BILD'           TO WS-NEW-ITEM-STATUS
              END-IF
           END-IF
           PERFORM 4020-UPDATE-ITEM
      * BR-BIL-WOF-012 Mirror posting linked to the original
           PERFORM 4500-FIND-ORIGINAL-TXN
           IF WS-ORIG-TXN-ID = 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'WONOTXN '          TO CA-REASON-CODE
              MOVE 'Original write-off posting not found'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8310-REVERSE-LEDGER
           MOVE 'APPR'                 TO WS-OLD-WO-STATUS
           MOVE 'REVS'                 TO WOF-WO-STATUS
           PERFORM 4210-UPDATE-WRITEOFF-STATUS
           PERFORM 8600-REFRESH-BALANCES
           MOVE 'STAT'                 TO WS-AUD-ACTION
           MOVE 'APPR'                 TO WS-AUD-BEFORE
           MOVE 'REVS'                 TO WS-AUD-AFTER
           PERFORM 8400-WRITE-AUDIT.

      ******************************************************************
      * BR-BIL-WOF-002 Billed item with a positive open amount
       3000-VALIDATE-ITEM-OPEN.
           IF ITM-ITEM-STATUS NOT = 'BILD'
              AND ITM-ITEM-STATUS NOT = 'DUE '
              SET CA-RC-STATE          TO TRUE
              MOVE 'ITMNOTBL'          TO CA-REASON-CODE
              STRING 'Item status ' ITM-ITEM-STATUS
                     ' cannot be written off' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-OPEN-AMOUNT = ITM-ITEM-AMOUNT - ITM-PAID-AMOUNT
                                  - ITM-WRITEOFF-AMOUNT
           IF WS-OPEN-AMOUNT NOT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'ITMNOOPN'          TO CA-REASON-CODE
              MOVE 'Item has no open amount to write off'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-ITEM.
           EXEC SQL
              SELECT ACCOUNT_ID, PP_ID, DUE_DATE, ITEM_AMOUNT,
                     PAID_AMOUNT, WRITEOFF_AMOUNT, ITEM_STATUS
                INTO :ITM-ACCOUNT-ID, :ITM-PP-ID, :ITM-DUE-DATE,
                     :ITM-ITEM-AMOUNT, :ITM-PAID-AMOUNT,
                     :ITM-WRITEOFF-AMOUNT, :ITM-ITEM-STATUS
                FROM BILDB.BIL_INVOICE_ITEM
               WHERE ITEM_ID = :ITM-ITEM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ITMNOTFD'       TO CA-REASON-CODE
                 MOVE ITM-ITEM-ID      TO WS-ID-DISP
                 STRING 'Invoice item ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE ITM-ITEM-ID      TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4000-READ-ITEM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4010-COUNT-PENDING.
           MOVE ITM-ITEM-ID            TO WOF-ITEM-ID
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-PEND-COUNT
                FROM BILDB.BIL_WRITEOFF
               WHERE ITEM_ID   = :WOF-ITEM-ID
                 AND WO_STATUS = 'PEND'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ITM-ITEM-ID         TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4010-COUNT-PENDING' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-WOF-013 Update only an item unchanged since it was read
       4020-UPDATE-ITEM.
           MOVE WS-NEW-WO-AMOUNT       TO ITM-WRITEOFF-AMOUNT
           MOVE WS-NEW-ITEM-STATUS     TO ITM-ITEM-STATUS
           EXEC SQL
              UPDATE BILDB.BIL_INVOICE_ITEM
                 SET WRITEOFF_AMOUNT = :ITM-WRITEOFF-AMOUNT,
                     ITEM_STATUS     = :ITM-ITEM-STATUS,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE ITEM_ID         = :ITM-ITEM-ID
                 AND ITEM_STATUS     = :WS-OLD-ITEM-STATUS
                 AND PAID_AMOUNT     = :WS-OLD-PAID
                 AND WRITEOFF_AMOUNT = :WS-OLD-WO
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE ITM-ITEM-ID         TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4020-UPDATE-ITEM'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Invoice item changed by another task - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4100-INSERT-WRITEOFF.
           EXEC SQL
              INSERT INTO BILDB.BIL_WRITEOFF
                     (WO_ID, ACCOUNT_ID, ITEM_ID, WO_AMOUNT,
                      WO_REASON, WO_STATUS, REQUEST_USER,
                      APPROVER_USER, WO_DATE, REVERSED_FLAG,
                      CREATE_TS)
              VALUES (:WOF-WO-ID, :WOF-ACCOUNT-ID, :WOF-ITEM-ID,
                      :WOF-WO-AMOUNT, :WOF-WO-REASON,
                      :WOF-WO-STATUS, :WOF-REQUEST-USER,
                      :WOF-APPROVER-USER, :WOF-WO-DATE,
                      :WOF-REVERSED-FLAG, CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE WOF-WO-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4100-INSERT-WRITEOFF' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4200-READ-WRITEOFF.
           EXEC SQL
              SELECT ACCOUNT_ID, ITEM_ID, WO_AMOUNT, WO_REASON,
                     WO_STATUS, REQUEST_USER, APPROVER_USER,
                     WO_DATE, REVERSED_FLAG
                INTO :WOF-ACCOUNT-ID, :WOF-ITEM-ID, :WOF-WO-AMOUNT,
                     :WOF-WO-REASON, :WOF-WO-STATUS,
                     :WOF-REQUEST-USER, :WOF-APPROVER-USER,
                     :WOF-WO-DATE, :WOF-REVERSED-FLAG
                FROM BILDB.BIL_WRITEOFF
               WHERE WO_ID = :WOF-WO-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE WOF-WO-REASON    TO WS-REASON
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'WONOTFD '       TO CA-REASON-CODE
                 MOVE WOF-WO-ID        TO WS-ID-DISP
                 STRING 'Write-off ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE WOF-WO-ID        TO WS-ID-DISP
                 MOVE WS-ID-DISP       TO WS-ERR-KEY
                 MOVE '4200-READ-WRITEOFF' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Status change guarded by the status that was read
       4210-UPDATE-WRITEOFF-STATUS.
           IF WOF-WO-STATUS = 'REVS'
              MOVE 'Y'                 TO WOF-REVERSED-FLAG
           END-IF
           EXEC SQL
              UPDATE BILDB.BIL_WRITEOFF
                 SET WO_STATUS     = :WOF-WO-STATUS,
                     APPROVER_USER = :WOF-APPROVER-USER,
                     REVERSED_FLAG = :WOF-REVERSED-FLAG
               WHERE WO_ID     = :WOF-WO-ID
                 AND WO_STATUS = :WS-OLD-WO-STATUS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE WOF-WO-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4210-UPDATE-WRITEOFF-STATUS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Write-off changed by another task - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4300-READ-ACCOUNT.
           MOVE WS-ACCOUNT-ID          TO ACT-ACCOUNT-ID
           EXEC SQL
              SELECT DELINQ_PLAN_CODE, ACCOUNT_STATUS
                INTO :ACT-DELINQ-PLAN-CODE, :ACT-ACCOUNT-STATUS
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
                 MOVE '4300-READ-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4310-READ-DELINQ-PLAN.
           MOVE ACT-DELINQ-PLAN-CODE   TO DPL-PLAN-CODE
           EXEC SQL
              SELECT WRITEOFF_THRESHOLD
                INTO :DPL-WRITEOFF-THRESHOLD
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
                 MOVE '4310-READ-DELINQ-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Item ids are loaded first so no cursor is open while items
      * are updated and other services are called
       4400-LOAD-SMALL-ITEMS.
           MOVE 0                      TO WS-SW-COUNT
           EXEC SQL OPEN C-SMALL-ITEMS END-EXEC
           IF SQLCODE NOT = 0
              MOVE ACT-ACCOUNT-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4400-LOAD-SMALL-ITEMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM UNTIL WS-SW-COUNT >= WS-MAX-SWEEP
              EXEC SQL
                 FETCH C-SMALL-ITEMS
                  INTO :ITM-ITEM-ID
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    ADD 1              TO WS-SW-COUNT
                    MOVE ITM-ITEM-ID   TO WS-SW-ITEM-ID(WS-SW-COUNT)
                 WHEN 100
                    EXIT PERFORM
                 WHEN OTHER
                    MOVE ACT-ACCOUNT-ID TO WS-ID-DISP
                    MOVE WS-ID-DISP    TO WS-ERR-KEY
                    MOVE '4400-LOAD-SMALL-ITEMS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           PERFORM 4490-CLOSE-CURSOR.

      ******************************************************************
       4490-CLOSE-CURSOR.
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE C-SMALL-ITEMS END-EXEC
              SET WS-CURSOR-CLOSED     TO TRUE
           END-IF.

      ******************************************************************
       4500-FIND-ORIGINAL-TXN.
           MOVE 'WOFF'                 TO TXN-REF-TYPE
           MOVE WOF-WO-ID              TO TXN-REF-ID
           EXEC SQL
              SELECT COALESCE(MIN(TXN_ID), 0)
                INTO :WS-ORIG-TXN-ID
                FROM BILDB.BIL_TRANSACTION
               WHERE REF_TYPE        = :TXN-REF-TYPE
                 AND REF_ID          = :TXN-REF-ID
                 AND REVERSAL_OF_TXN = 0
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE WOF-WO-ID           TO WS-ID-DISP
              MOVE WS-ID-DISP          TO WS-ERR-KEY
              MOVE '4500-FIND-ORIGINAL-TXN' TO WS-ERR-PARA
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
      * Apply an approved write-off to the item and post it
      * BR-BIL-WOF-008 Close the item once it is fully settled
       5000-APPLY-TO-ITEM.
           MOVE ITM-ITEM-STATUS        TO WS-OLD-ITEM-STATUS
           MOVE ITM-PAID-AMOUNT        TO WS-OLD-PAID
           MOVE ITM-WRITEOFF-AMOUNT    TO WS-OLD-WO
           COMPUTE WS-NEW-WO-AMOUNT = ITM-WRITEOFF-AMOUNT
                                    + WS-WO-AMOUNT
           MOVE ITM-ITEM-STATUS        TO WS-NEW-ITEM-STATUS
           IF ITM-PAID-AMOUNT + WS-NEW-WO-AMOUNT >= ITM-ITEM-AMOUNT
              MOVE 'CLSD'              TO WS-NEW-ITEM-STATUS
           END-IF
           PERFORM 5100-SET-STAGE-ACCOUNT
           PERFORM 4020-UPDATE-ITEM
      * BR-BIL-WOF-007 DR WRITEOFF / CR DUE or BILLED
           MOVE 'WRITEOFF'             TO WS-GL-DR-ACCT
           MOVE WS-STAGE-ACCT          TO WS-GL-CR-ACCT
           MOVE 'WRITEOFF'             TO WS-GL-TXN-TYPE
           PERFORM 8300-POST-LEDGER.

      ******************************************************************
      * Receivable stage of the item: DUE once due, else BILLED
       5100-SET-STAGE-ACCOUNT.
           IF WS-OLD-ITEM-STATUS = 'DUE '
              MOVE 'DUE     '          TO WS-STAGE-ACCT
           ELSE
              MOVE 'BILLED  '          TO WS-STAGE-ACCT
           END-IF.

      ******************************************************************
       8000-NEXT-WO-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'WOF     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE SQA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE SQA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * BR-BIL-WOF-001 Reason must be an active WOREASON code
       8100-VALIDATE-REASON.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE WS-BUS-DATE            TO CDA-BUS-DATE
           MOVE CA-TRACE-ID            TO CDA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE 'WOFFRSN '             TO CDA-CD-TABLE
           MOVE WS-REASON              TO CDA-CD-CODE
           CALL WS-COD-PGM USING WS-COD-CA
           IF CDA-RC-NOT-FOUND OR NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'WOREASON'          TO CA-REASON-CODE
              STRING 'Write-off reason ' WS-REASON
                     ' is not a valid code' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CDA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE CDA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE CDA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Caller sets WS-LIMIT-TYPE; amount is WS-WO-AMOUNT
       8200-CHECK-AUTHORITY.
           INITIALIZE WS-AUT-CA
           MOVE 'AUTCHK  '             TO ATA-REQUEST-ID
           MOVE CA-USER-ID             TO ATA-USER-ID
           MOVE WS-BUS-DATE            TO ATA-BUS-DATE
           MOVE CA-TRACE-ID            TO ATA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ATA-CALLER-PGM
           MOVE CA-USER-ID             TO ATA-AT-USER
           MOVE WS-LIMIT-TYPE          TO ATA-AT-LIMIT-TYPE
           MOVE WS-WO-AMOUNT           TO ATA-AT-AMOUNT
           CALL WS-AUT-PGM USING WS-AUT-CA
           MOVE ATA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE ATA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE ATA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Two-line posting of WS-WO-AMOUNT for the current write-off
       8300-POST-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE CA-TRACE-ID            TO GLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE WS-GL-TXN-TYPE         TO GLA-GL-TXN-TYPE
           MOVE WS-ACCOUNT-ID          TO GLA-GL-ACCOUNT-ID
           MOVE 0                      TO GLA-GL-PRODUCER-ID
           MOVE 'WOFF'                 TO GLA-GL-REF-TYPE
           MOVE WOF-WO-ID              TO GLA-GL-REF-ID
           MOVE 0                      TO GLA-GL-REVERSAL-OF
           MOVE 2                      TO GLA-GL-LINE-COUNT
           MOVE WS-GL-DR-ACCT          TO GLA-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLA-GL-DR-CR(1)
           MOVE WS-WO-AMOUNT           TO GLA-GL-AMOUNT(1)
           MOVE WS-GL-CR-ACCT          TO GLA-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLA-GL-DR-CR(2)
           MOVE WS-WO-AMOUNT           TO GLA-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           MOVE GLA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE GLA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE GLA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Mirror of the original write-off posting (DR/CR swapped)
       8310-REVERSE-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLREVS  '             TO GLA-REQUEST-ID
           MOVE CA-USER-ID             TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE CA-TRACE-ID            TO GLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE 'WOFFREV '             TO GLA-GL-TXN-TYPE
           MOVE WS-ACCOUNT-ID          TO GLA-GL-ACCOUNT-ID
           MOVE 'WOFF'                 TO GLA-GL-REF-TYPE
           MOVE WOF-WO-ID              TO GLA-GL-REF-ID
           MOVE WS-ORIG-TXN-ID         TO GLA-GL-REVERSAL-OF
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
           MOVE 'WRITEOFF'             TO AUA-AU-ENTITY
           MOVE WOF-WO-ID              TO WS-ID-DISP
           MOVE WS-ID-DISP             TO AUA-AU-KEY
           MOVE WS-AUD-ACTION          TO AUA-AU-ACTION
           MOVE 'WO_STATUS'            TO AUA-AU-FIELD
           MOVE WS-AUD-BEFORE          TO AUA-AU-BEFORE
           MOVE WS-AUD-AFTER           TO AUA-AU-AFTER
           MOVE WS-PROGRAM-ID          TO AUA-AU-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           MOVE AUA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE AUA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE AUA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * BR-BIL-WOF-014 Recompute billed / due / unbilled balances
       8600-REFRESH-BALANCES.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTBAL '             TO ACA-REQUEST-ID
           MOVE CA-USER-ID             TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE CA-TRACE-ID            TO ACA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE WS-ACCOUNT-ID          TO ACA-AC-ACCOUNT-ID
           CALL WS-ACT-PGM USING WS-ACT-CA
           MOVE ACA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE ACA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE ACA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Callee RC 08 at this point is an inconsistency; RC >= 12 is
      * passed up unchanged with its reason code
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
                 MOVE 'Write-off data locked - retry'
                                       TO CA-MESSAGE
              WHEN OTHER
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DB2ERR  '       TO CA-REASON-CODE
                 STRING 'BILWOFF0 Db2 error SQLCODE ' WS-SQLCODE-DISP
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
           PERFORM 4490-CLOSE-CURSOR
           PERFORM 9000-RETURN.
