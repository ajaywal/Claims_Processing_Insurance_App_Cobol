       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILCOMPY.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILCOMPY                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Producer payment run. Sums earned, unpaid        *
      *               commission items per producer, records a         *
      *               producer payment, links the items to it and      *
      *               raises a COMMPAY disbursement through BILDISB0.  *
      * PARM        : 1-4 commit frequency (default 0050)              *
      *               6-15 business date override yyyy-mm-dd           *
      * FILES       : RESTART (I, optional) CHKPT (O) RPTOUT (O)       *
      * CALLS       : BILDISB0 BILSEQGN BILDATES BILERRLG              *
      * TABLES      : BIL_PRODUCER (R U) BIL_COMMISSION_PLAN (R)       *
      *               BIL_COMMISSION_ITEM (R U)                        *
      *               BIL_PRODUCER_PAYMENT (C U) BIL_CONTROL (R)       *
      * RULES       : BR-BIL-CPY-001 Month end pays monthly and weekly *
      *                              producers; other runs weekly only *
      *               BR-BIL-CPY-002 Only ACTV producers are paid      *
      *               BR-BIL-CPY-003 Pay EARN items not yet linked to  *
      *                              a producer payment                *
      *               BR-BIL-CPY-004 Net earned below the plan         *
      *                              PAY_THRESHOLD (or not positive)   *
      *                              is carried forward                *
      *               BR-BIL-CPY-005 Items are linked first and the    *
      *                              amount re-summed from the linked  *
      *                              items, so the payment is exact    *
      *               BR-BIL-CPY-006 Paid by BACS to the producer      *
      *                              mandate: COMMPAY, source COMM,    *
      *                              ref = producer payment id         *
      *               BR-BIL-CPY-007 Payable balance reduced by the    *
      *                              amount paid                       *
      *               BR-BIL-CPY-008 One savepoint per producer        *
      * RETURN CODE : 0 ok, 4 pending approval, 8 rejects, 16 abend    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL RESTART-FILE ASSIGN TO RESTART
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RST-STATUS.
           SELECT CHECKPOINT-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CKP-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RESTART-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RESTART-REC.
           COPY BILDSBK1 REPLACING LEADING ==CKP-== BY ==RST-==.
       FD  CHECKPOINT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHECKPOINT-REC.
           COPY BILDSBK1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILCOMPY'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-DSB-PGM                  PIC X(8)  VALUE 'BILDISB0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.

       01  WS-FILE-STATUS.
           05 WS-RST-STATUS            PIC XX.
      * Checkpoints are appended per commit: the last one counts
           05 WS-RESTART-SAVE          PIC X(80).
           05 WS-CKP-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 50.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-MONTH-END             PIC X     VALUE 'N'.
              88 WS-IS-MONTH-END        VALUE 'Y'.
           05 WS-RESTART-PRD           PIC S9(9) COMP VALUE 0.
           05 WS-RESTART-NUM           PIC 9(10).
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-CURSOR-FLAG           PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN         VALUE 'Y'.
           05 WS-RETURN-CODE           PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-PAGE-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-NUM               PIC 9(10).
           05 WS-ROW-OK                PIC X.
              88 WS-ROW-IS-OK           VALUE 'Y'.
           05 WS-RESULT                PIC X(24).

       01  WS-PRODUCER-WORK.
           05 WS-THRESHOLD             PIC S9(7)V99 COMP-3.
           05 WS-ITEM-COUNT            PIC S9(9) COMP.
           05 WS-EARNED                PIC S9(11)V99 COMP-3.
           05 WS-LINKED-COUNT          PIC S9(9) COMP.
           05 WS-LINKED-AMOUNT         PIC S9(11)V99 COMP-3.

       01  WS-COUNTERS.
           05 WS-CNT-READ              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-NO-EARNINGS       PIC S9(9) COMP VALUE 0.
           05 WS-CNT-CARRIED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-PAID              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-PENDING           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-REJECTED          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-ITEMS-PAID        PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-PAID              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMT-CARRIED           PIC S9(11)V99 COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPRD END-EXEC.
           EXEC SQL INCLUDE BILDPPY END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-CPY-001 / 002 producers due a payment on this run
           EXEC SQL DECLARE PRD-CSR CURSOR WITH HOLD FOR
              SELECT P.PRODUCER_ID, P.PRODUCER_CODE, P.PRODUCER_NAME,
                     P.PAY_CYCLE, C.PAY_THRESHOLD
                FROM BILDB.BIL_PRODUCER P,
                     BILDB.BIL_COMMISSION_PLAN C
               WHERE C.PLAN_CODE       = P.COMM_PLAN_CODE
                 AND P.PRODUCER_STATUS = 'ACTV'
                 AND P.PRODUCER_ID     > :WS-RESTART-PRD
                 AND (P.PAY_CYCLE = 'W'
                      OR (P.PAY_CYCLE = 'M' AND :WS-MONTH-END = 'Y'))
               ORDER BY P.PRODUCER_ID
               FOR FETCH ONLY
           END-EXEC.

       01  WS-DSB-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DBA-==.
           03 DBA-BODY.
              COPY BILCDSB REPLACING LEADING ==CA-== BY ==DBA-==.
       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILCOMR1.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-PRODUCERS
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILCOMPY OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           IF LS-PARM-LEN >= 4 AND LS-PARM-DATA(1:4) IS NUMERIC
              MOVE LS-PARM-DATA(1:4)   TO WS-COMMIT-FREQ
              IF WS-COMMIT-FREQ = 0
                 MOVE 50               TO WS-COMMIT-FREQ
              END-IF
           END-IF
           IF LS-PARM-LEN >= 15 AND LS-PARM-DATA(6:10) NOT = SPACES
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           ELSE
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           MOVE WS-BUS-DATE            TO CPR-H1-BUS-DATE
      * BR-BIL-CPY-001 Month-end cycle decided from the calendar
           INITIALIZE WS-DTE-CA
           MOVE 'DTEOM   '             TO DTA-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-1
           PERFORM 8200-CALL-DATES
           IF DTA-DT-RESULT-DATE = WS-BUS-DATE
              SET WS-IS-MONTH-END      TO TRUE
              MOVE 'CYCLE: MONTHLY+WEEKLY' TO CPR-H1-CYCLE
           ELSE
              MOVE 'CYCLE: WEEKLY'     TO CPR-H1-CYCLE
           END-IF
           PERFORM 1100-READ-RESTART.

       1100-READ-RESTART.
           OPEN INPUT RESTART-FILE
           IF WS-RST-STATUS = '00'
              MOVE SPACES              TO WS-RESTART-SAVE
              PERFORM UNTIL WS-RST-STATUS NOT = '00'
                 READ RESTART-FILE
                 IF WS-RST-STATUS = '00'
                    MOVE RESTART-REC   TO WS-RESTART-SAVE
                 END-IF
              END-PERFORM
              MOVE WS-RESTART-SAVE     TO RESTART-REC
              IF RST-PROGRAM = WS-PROGRAM-ID
                 AND RST-RUN-DATE = WS-BUS-DATE
                 AND RST-RUNNING
                 AND RST-RESTART-KEY(1:10) IS NUMERIC
                 MOVE RST-RESTART-KEY(1:10) TO WS-RESTART-NUM
                 MOVE WS-RESTART-NUM TO WS-RESTART-PRD
                 MOVE SPACES     TO CPR-M-TEXT
                 STRING 'RESTARTING AFTER PRODUCER ID '
                        RST-RESTART-KEY(1:10)
                        DELIMITED BY SIZE INTO CPR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
              END-IF
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
       2000-PROCESS-PRODUCERS.
           EXEC SQL OPEN PRD-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-PRODUCERS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4100-FETCH-PRODUCER
           PERFORM UNTIL WS-EOF
              PERFORM 2100-PROCESS-PRODUCER
              ADD 1                    TO WS-SINCE-COMMIT
              IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
              END-IF
              PERFORM 4100-FETCH-PRODUCER
           END-PERFORM
           EXEC SQL CLOSE PRD-CSR END-EXEC
           MOVE 'N'                    TO WS-CURSOR-FLAG.

       2100-PROCESS-PRODUCER.
           ADD 1                       TO WS-CNT-READ
           MOVE PRD-PRODUCER-ID        TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           INITIALIZE CPR-DETAIL
           MOVE PRD-PRODUCER-CODE      TO CPR-D-PRODUCER-CODE
           MOVE PRD-PRODUCER-NAME-TEXT TO CPR-D-NAME
           MOVE PRD-PAY-CYCLE          TO CPR-D-CYCLE
           MOVE WS-THRESHOLD           TO CPR-D-THRESHOLD
      * BR-BIL-CPY-003 Earned and not yet paid
           PERFORM 4200-SUM-UNPAID
           MOVE WS-ITEM-COUNT          TO CPR-D-ITEMS
           MOVE WS-EARNED              TO CPR-D-EARNED
           IF WS-ITEM-COUNT = 0
              ADD 1                    TO WS-CNT-NO-EARNINGS
              EXIT PARAGRAPH
           END-IF
      * BR-BIL-CPY-004 Below threshold: carry forward
           IF WS-EARNED NOT > 0 OR WS-EARNED < WS-THRESHOLD
              ADD 1                    TO WS-CNT-CARRIED
              ADD WS-EARNED            TO WS-AMT-CARRIED
              MOVE 'CARRIED FORWARD'   TO CPR-D-RESULT
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
      * BR-BIL-CPY-008 Each producer is its own savepoint
           PERFORM 4300-SET-SAVEPOINT
           SET WS-ROW-IS-OK            TO TRUE
           PERFORM 8000-NEXT-PAYMENT-ID
           PERFORM 4400-INSERT-PAYMENT
           PERFORM 4500-LINK-ITEMS
           IF WS-ROW-IS-OK
              PERFORM 8100-CREATE-DISBURSEMENT
           END-IF
           IF WS-ROW-IS-OK
              PERFORM 4600-COMPLETE-PAYMENT
              PERFORM 4700-REDUCE-PAYABLE
           END-IF
           IF WS-ROW-IS-OK
              ADD WS-LINKED-COUNT      TO WS-CNT-ITEMS-PAID
              ADD WS-LINKED-AMOUNT     TO WS-AMT-PAID
              MOVE WS-LINKED-COUNT     TO CPR-D-ITEMS
              MOVE WS-LINKED-AMOUNT    TO CPR-D-EARNED
              MOVE PPY-PRODUCER-PAY-ID TO CPR-D-PAY-ID
              MOVE WS-RESULT           TO CPR-D-RESULT
           ELSE
              PERFORM 4800-ROLLBACK-SAVEPOINT
              ADD 1                    TO WS-CNT-REJECTED
              MOVE WS-RESULT           TO CPR-D-RESULT
              IF WS-RETURN-CODE < 8
                 MOVE 8                TO WS-RETURN-CODE
              END-IF
           END-IF
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
       4000-READ-BUSINESS-DATE.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-BUSINESS-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

       4100-FETCH-PRODUCER.
           EXEC SQL
              FETCH PRD-CSR
               INTO :PRD-PRODUCER-ID, :PRD-PRODUCER-CODE,
                    :PRD-PRODUCER-NAME, :PRD-PAY-CYCLE,
                    :WS-THRESHOLD
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-PRODUCER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4200-SUM-UNPAID.
           EXEC SQL
              SELECT COUNT(*), COALESCE(SUM(EARNED_AMOUNT), 0)
                INTO :WS-ITEM-COUNT, :WS-EARNED
                FROM BILDB.BIL_COMMISSION_ITEM
               WHERE PRODUCER_ID     = :PRD-PRODUCER-ID
                 AND CI_STATE        = 'EARN'
                 AND PRODUCER_PAY_ID = 0
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-SUM-UNPAID'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4300-SET-SAVEPOINT.
           EXEC SQL
              SAVEPOINT COMPY_PRD ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-SET-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4400-INSERT-PAYMENT.
           MOVE PRD-PRODUCER-ID        TO PPY-PRODUCER-ID
           MOVE WS-BUS-DATE            TO PPY-PERIOD-END
           MOVE WS-EARNED              TO PPY-PAY-AMOUNT
           MOVE WS-ITEM-COUNT          TO PPY-ITEM-COUNT
           EXEC SQL
              INSERT INTO BILDB.BIL_PRODUCER_PAYMENT
                   ( PRODUCER_PAY_ID, PRODUCER_ID, PERIOD_END,
                     PAY_AMOUNT, ITEM_COUNT, PAY_STATUS, DISB_ID,
                     CREATE_TS )
              VALUES
                   ( :PPY-PRODUCER-PAY-ID, :PPY-PRODUCER-ID,
                     :PPY-PERIOD-END, :PPY-PAY-AMOUNT,
                     :PPY-ITEM-COUNT, 'PEND', 0, CURRENT TIMESTAMP )
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-INSERT-PAYMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      * BR-BIL-CPY-005 Link, then pay exactly what was linked
       4500-LINK-ITEMS.
           EXEC SQL
              UPDATE BILDB.BIL_COMMISSION_ITEM
                 SET PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID,
                     CI_STATE        = 'PAID',
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE PRODUCER_ID     = :PRD-PRODUCER-ID
                 AND CI_STATE        = 'EARN'
                 AND PRODUCER_PAY_ID = 0
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-LINK-ITEMS'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              SELECT COUNT(*), COALESCE(SUM(EARNED_AMOUNT), 0)
                INTO :WS-LINKED-COUNT, :WS-LINKED-AMOUNT
                FROM BILDB.BIL_COMMISSION_ITEM
               WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-LINK-ITEMS'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-LINKED-AMOUNT NOT > 0
              OR WS-LINKED-AMOUNT < WS-THRESHOLD
              MOVE 'N'                 TO WS-ROW-OK
              MOVE 'REJECT - EARNINGS MOVED' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           IF WS-LINKED-AMOUNT NOT = WS-EARNED
              OR WS-LINKED-COUNT NOT = WS-ITEM-COUNT
              MOVE WS-LINKED-AMOUNT    TO PPY-PAY-AMOUNT
              MOVE WS-LINKED-COUNT     TO PPY-ITEM-COUNT
              EXEC SQL
                 UPDATE BILDB.BIL_PRODUCER_PAYMENT
                    SET PAY_AMOUNT      = :PPY-PAY-AMOUNT,
                        ITEM_COUNT      = :PPY-ITEM-COUNT
                  WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4500-LINK-ITEMS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF.

       4600-COMPLETE-PAYMENT.
           MOVE DBA-DB-DISB-ID         TO PPY-DISB-ID
           EXEC SQL
              UPDATE BILDB.BIL_PRODUCER_PAYMENT
                 SET DISB_ID         = :PPY-DISB-ID,
                     PAY_STATUS      = 'ISSD'
               WHERE PRODUCER_PAY_ID = :PPY-PRODUCER-PAY-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4600-COMPLETE-PAYMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      * BR-BIL-CPY-007 Payable balance down by the amount paid
       4700-REDUCE-PAYABLE.
           EXEC SQL
              UPDATE BILDB.BIL_PRODUCER
                 SET PAYABLE_BALANCE = PAYABLE_BALANCE
                                     - :WS-LINKED-AMOUNT,
                     UPDATE_TS       = CURRENT TIMESTAMP
               WHERE PRODUCER_ID     = :PRD-PRODUCER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -911
              WHEN -913
                 MOVE 'N'              TO WS-ROW-OK
                 MOVE 'REJECT - PRODUCER LOCKED' TO WS-RESULT
              WHEN OTHER
                 MOVE '4700-REDUCE-PAYABLE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4800-ROLLBACK-SAVEPOINT.
           EXEC SQL
              ROLLBACK TO SAVEPOINT COMPY_PRD
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4800-ROLLBACK-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM CPR-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO CPR-H1-PAGE
           WRITE REPORT-REC FROM CPR-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM CPR-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM CPR-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6300-WRITE-TOTAL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM CPR-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE 0                      TO CPR-T-AMOUNT.

       6900-WRITE-CONTROL-TOTALS.
           MOVE '*** CONTROL TOTALS ***' TO CPR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'PRODUCERS IN CYCLE'   TO CPR-T-LABEL
           MOVE WS-CNT-READ            TO CPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'NO UNPAID EARNINGS'   TO CPR-T-LABEL
           MOVE WS-CNT-NO-EARNINGS     TO CPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'CARRIED FORWARD - BELOW THRESHOLD' TO CPR-T-LABEL
           MOVE WS-CNT-CARRIED         TO CPR-T-COUNT
           MOVE WS-AMT-CARRIED         TO CPR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'PAYMENTS RAISED'      TO CPR-T-LABEL
           MOVE WS-CNT-PAID            TO CPR-T-COUNT
           MOVE WS-AMT-PAID            TO CPR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  OF WHICH PENDING APPROVAL' TO CPR-T-LABEL
           MOVE WS-CNT-PENDING         TO CPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMISSION ITEMS PAID' TO CPR-T-LABEL
           MOVE WS-CNT-ITEMS-PAID      TO CPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REJECTED'             TO CPR-T-LABEL
           MOVE WS-CNT-REJECTED        TO CPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO CPR-T-LABEL
           MOVE WS-CNT-COMMITS         TO CPR-T-COUNT
           PERFORM 6300-WRITE-TOTAL.

      ******************************************************************
       7000-COMMIT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-CNT-COMMITS
           MOVE 0                      TO WS-SINCE-COMMIT
           INITIALIZE CHECKPOINT-REC
           MOVE WS-PROGRAM-ID          TO CKP-PROGRAM
           MOVE WS-BUS-DATE            TO CKP-RUN-DATE
           MOVE PRD-PRODUCER-ID        TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO CKP-RESTART-KEY
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           MOVE WS-CNT-PAID            TO CKP-DONE-COUNT
           SET CKP-RUNNING             TO TRUE
           IF WS-EOF
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILCOMPY CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       8000-NEXT-PAYMENT-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE WS-BATCH-USER          TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'PPY     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 12
              MOVE SPACES              TO CPR-M-TEXT
              STRING 'BILSEQGN PPY FAILED ' SQA-REASON-CODE ' '
                     SQA-MESSAGE DELIMITED BY SIZE INTO CPR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           MOVE SQA-SQ-VALUE           TO PPY-PRODUCER-PAY-ID.

      * BR-BIL-CPY-006 COMMPAY to the producer's BACS mandate
       8100-CREATE-DISBURSEMENT.
           INITIALIZE WS-DSB-CA
           MOVE 'DSBCRT  '             TO DBA-REQUEST-ID
           MOVE WS-BATCH-USER          TO DBA-USER-ID
           MOVE WS-BUS-DATE            TO DBA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DBA-CALLER-PGM
           MOVE 0                      TO DBA-DB-ACCOUNT-ID
           MOVE WS-LINKED-AMOUNT       TO DBA-DB-AMOUNT
           MOVE 'COMMPAY '             TO DBA-DB-REASON
           MOVE PRD-PRODUCER-NAME-TEXT TO DBA-DB-PAYEE-NAME
           MOVE 'BACS'                 TO DBA-DB-PAY-METHOD
           MOVE WS-BUS-DATE            TO DBA-DB-DUE-DATE
           MOVE 'COMM'                 TO DBA-DB-SOURCE-SYSTEM
           MOVE PPY-PRODUCER-PAY-ID    TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO DBA-DB-SOURCE-REF
           CALL WS-DSB-PGM USING WS-DSB-CA
           EVALUATE TRUE
              WHEN DBA-RC-OK
                 ADD 1                 TO WS-CNT-PAID
                 MOVE 'PAID - DISB APPR' TO WS-RESULT
              WHEN DBA-RC-WARNING
                 ADD 1                 TO WS-CNT-PAID
                 ADD 1                 TO WS-CNT-PENDING
                 STRING 'PAID - PEND ' DBA-DB-APPROVER
                        DELIMITED BY SIZE INTO WS-RESULT
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
              WHEN DBA-RETURN-CODE >= 90
                 PERFORM 4800-ROLLBACK-SAVEPOINT
                 MOVE SPACES           TO CPR-M-TEXT
                 STRING 'BILDISB0 FAILED ' DBA-REASON-CODE ' '
                        DBA-MESSAGE DELIMITED BY SIZE INTO CPR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
              WHEN OTHER
                 MOVE 'N'              TO WS-ROW-OK
                 STRING 'REJECT ' DBA-REASON-CODE
                        DELIMITED BY SIZE INTO WS-RESULT
                 MOVE SPACES           TO CPR-M-TEXT
                 STRING '  PRODUCER ' WS-ERR-KEY ' ' DBA-MESSAGE
                        DELIMITED BY SIZE INTO CPR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
           END-EVALUATE.

       8200-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              DISPLAY 'BILCOMPY BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       9000-TERMINATE.
           PERFORM 7000-COMMIT
           IF WS-PAGE-COUNT = 0
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILCOMPY COMPLETE PRODUCERS=' WS-CNT-READ
                   ' PAID=' WS-CNT-PAID ' REJ=' WS-CNT-REJECTED
                   ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

       9800-ABEND.
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE PRD-CSR END-EXEC
              MOVE 'N'                 TO WS-CURSOR-FLAG
           END-IF
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO CPR-M-TEXT
           STRING '*** BILCOMPY ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK. LAST KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO CPR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILCOMPY DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA ' KEY ' WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           STRING 'Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9800-ABEND.
