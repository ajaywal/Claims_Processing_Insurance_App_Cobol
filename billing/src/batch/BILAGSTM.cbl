       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILAGSTM.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILAGSTM                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Agency-bill statements. For every agency-bill    *
      *               producer, lists the items billed on its          *
      *               agency-bill (AB) policy periods since the last   *
      *               statement: gross, commission retained by the     *
      *               agency and net due to the carrier. Records the   *
      *               statement and writes it to the statement file.   *
      * PARM        : 1-4 commit frequency (default 0050)              *
      *               6-15 business date override yyyy-mm-dd           *
      * FILES       : RESTART (I, optional) CHKPT (O) STMTOUT (O)      *
      *               RPTOUT (O)                                       *
      * CALLS       : BILSEQGN BILDATES BILERRLG                       *
      * TABLES      : BIL_PRODUCER (R) BIL_POLICY_PERIOD (R)           *
      *               BIL_INVOICE_ITEM (R) BIL_ACCOUNT (R)             *
      *               BIL_AGENCY_STATEMENT (C R) BIL_CONTROL (R)       *
      * RULES       : BR-BIL-AGS-001 Only ACTV producers flagged       *
      *                              AGENCY_BILL_FLAG = Y              *
      *               BR-BIL-AGS-002 Statement holds BILD/DUE items on *
      *                              the producer's AB periods with an *
      *                              event date after the previous     *
      *                              statement, up to business date    *
      *               BR-BIL-AGS-003 Net due = gross - commission      *
      *               BR-BIL-AGS-004 Agency settles 30 days after the  *
      *                              statement date                    *
      *               BR-BIL-AGS-005 No items, no statement            *
      *               BR-BIL-AGS-006 One statement per producer per    *
      *                              day; a rerun skips producers done *
      * RETURN CODE : 0 ok, 4 producers skipped, 16 abend              *
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
           SELECT STATEMENT-FILE ASSIGN TO STMTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-STM-STATUS.
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
       FD  STATEMENT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STATEMENT-REC.
           COPY BILAGSS1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILAGSTM'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.

       01  WS-FILE-STATUS.
           05 WS-RST-STATUS            PIC XX.
      * Checkpoints are appended per commit: the last one counts
           05 WS-RESTART-SAVE          PIC X(80).
           05 WS-CKP-STATUS            PIC XX.
           05 WS-STM-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-SETTLE-DAYS           PIC S9(5) COMP-3 VALUE 30.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.
           05 WS-LOW-DATE              PIC X(10) VALUE '0001-01-01'.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 50.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-DUE-DATE              PIC X(10) VALUE SPACES.
           05 WS-LAST-STMT-DATE        PIC X(10).
           05 WS-RESTART-PRD           PIC S9(9) COMP VALUE 0.
           05 WS-RESTART-NUM           PIC 9(10).
           05 WS-PRD-EOF-FLAG          PIC X     VALUE 'N'.
              88 WS-PRD-EOF             VALUE 'Y'.
           05 WS-ITM-EOF-FLAG          PIC X     VALUE 'N'.
              88 WS-ITM-EOF             VALUE 'Y'.
           05 WS-OPEN-CURSOR           PIC X(3)  VALUE SPACES.
              88 WS-ITM-CSR-OPEN        VALUE 'ITM'.
           05 WS-PRD-CSR-FLAG          PIC X     VALUE 'N'.
              88 WS-PRD-CSR-OPEN        VALUE 'Y'.
           05 WS-RETURN-CODE           PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-PAGE-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-NUM               PIC 9(10).

       01  WS-STATEMENT-WORK.
           05 WS-STMT-ID               PIC S9(9) COMP VALUE 0.
           05 WS-ST-COUNT              PIC S9(9) COMP VALUE 0.
           05 WS-ST-GROSS              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ST-COMMISSION         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ST-NET                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-POL-NUM               PIC S9(9) COMP.

       01  WS-COUNTERS.
           05 WS-CNT-PRODUCERS         PIC S9(9) COMP VALUE 0.
           05 WS-CNT-STATEMENTS        PIC S9(9) COMP VALUE 0.
           05 WS-CNT-NO-ITEMS          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-ALREADY           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-ITEMS             PIC S9(9) COMP VALUE 0.
           05 WS-CNT-RECORDS           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-TOT-GROSS             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-TOT-COMMISSION        PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-TOT-NET               PIC S9(11)V99 COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPRD END-EXEC.
           EXEC SQL INCLUDE BILDITM END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDAGS END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-AGS-001 Agency-bill producers
           EXEC SQL DECLARE AGP-CSR CURSOR WITH HOLD FOR
              SELECT PRODUCER_ID, PRODUCER_CODE, PRODUCER_NAME
                FROM BILDB.BIL_PRODUCER
               WHERE AGENCY_BILL_FLAG = 'Y'
                 AND PRODUCER_STATUS  = 'ACTV'
                 AND PRODUCER_ID      > :WS-RESTART-PRD
               ORDER BY PRODUCER_ID
               FOR FETCH ONLY
           END-EXEC.
      * BR-BIL-AGS-002 Billed items on the producer's AB periods
           EXEC SQL DECLARE AGI-CSR CURSOR FOR
              SELECT I.ITEM_ID, I.ITEM_TYPE, I.INSTALL_NUMBER,
                     I.EVENT_DATE, I.DUE_DATE, I.ITEM_AMOUNT,
                     I.COMMISSION_AMOUNT, P.GENAPP_POL_NUM,
                     A.ACCOUNT_NUMBER
                FROM BILDB.BIL_POLICY_PERIOD P,
                     BILDB.BIL_INVOICE_ITEM  I,
                     BILDB.BIL_ACCOUNT       A
               WHERE P.PRODUCER_ID    = :PRD-PRODUCER-ID
                 AND P.BILLING_METHOD = 'AB'
                 AND I.PP_ID          = P.PP_ID
                 AND I.ITEM_STATUS   IN ('BILD', 'DUE ')
                 AND I.EVENT_DATE     > :WS-LAST-STMT-DATE
                 AND I.EVENT_DATE    <= :WS-BUS-DATE
                 AND A.ACCOUNT_ID     = I.ACCOUNT_ID
               ORDER BY P.GENAPP_POL_NUM, I.EVENT_DATE, I.ITEM_ID
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
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILAGSR1.

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
                       STATEMENT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              OR WS-STM-STATUS NOT = '00'
              DISPLAY 'BILAGSTM OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
                      ' STMTOUT=' WS-STM-STATUS
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
           MOVE WS-BUS-DATE            TO AGR-H1-BUS-DATE
      * BR-BIL-AGS-004 Settlement due date
           INITIALIZE WS-DTE-CA
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-1
           MOVE WS-SETTLE-DAYS         TO DTA-DT-NUMBER
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              DISPLAY 'BILAGSTM BILDATES RC ' DTA-RETURN-CODE ' '
                      DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           MOVE DTA-DT-RESULT-DATE     TO WS-DUE-DATE
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
                 MOVE SPACES     TO AGR-M-TEXT
                 STRING 'RESTARTING AFTER PRODUCER ID '
                        RST-RESTART-KEY(1:10)
                        DELIMITED BY SIZE INTO AGR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
              END-IF
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
       2000-PROCESS-PRODUCERS.
           EXEC SQL OPEN AGP-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-PRODUCERS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-PRD-CSR-OPEN         TO TRUE
           PERFORM 4100-FETCH-PRODUCER
           PERFORM UNTIL WS-PRD-EOF
              PERFORM 2100-PRODUCER-STATEMENT
              ADD 1                    TO WS-SINCE-COMMIT
              IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
              END-IF
              PERFORM 4100-FETCH-PRODUCER
           END-PERFORM
           EXEC SQL CLOSE AGP-CSR END-EXEC
           MOVE 'N'                    TO WS-PRD-CSR-FLAG.

       2100-PRODUCER-STATEMENT.
           ADD 1                       TO WS-CNT-PRODUCERS
           MOVE PRD-PRODUCER-ID        TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           PERFORM 4200-LAST-STATEMENT-DATE
      * BR-BIL-AGS-006 Already stated today
           IF WS-LAST-STMT-DATE = WS-BUS-DATE
              ADD 1                    TO WS-CNT-ALREADY
              MOVE SPACES              TO AGR-M-TEXT
              STRING '  PRODUCER ' WS-ERR-KEY
                     ' ALREADY HAS A STATEMENT DATED ' WS-BUS-DATE
                     DELIMITED BY SIZE INTO AGR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              IF WS-RETURN-CODE < 4
                 MOVE 4                TO WS-RETURN-CODE
              END-IF
              EXIT PARAGRAPH
           END-IF
           MOVE 0                      TO WS-STMT-ID
                                          WS-ST-COUNT
                                          WS-ST-GROSS
                                          WS-ST-COMMISSION
                                          WS-ST-NET
           EXEC SQL OPEN AGI-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-PRODUCER-STATEMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-ITM-CSR-OPEN         TO TRUE
           MOVE 'N'                    TO WS-ITM-EOF-FLAG
           PERFORM 4300-FETCH-ITEM
           PERFORM UNTIL WS-ITM-EOF
              PERFORM 2200-STATEMENT-LINE
              PERFORM 4300-FETCH-ITEM
           END-PERFORM
           EXEC SQL CLOSE AGI-CSR END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR
      * BR-BIL-AGS-005 Nothing billed, nothing stated
           IF WS-ST-COUNT = 0
              ADD 1                    TO WS-CNT-NO-ITEMS
              EXIT PARAGRAPH
           END-IF
           PERFORM 2300-CLOSE-STATEMENT.

      * The statement id is taken on the first item so producers with
      * nothing to state do not consume numbers.
       2200-STATEMENT-LINE.
           IF WS-STMT-ID = 0
              PERFORM 8000-NEXT-STATEMENT-ID
              INITIALIZE STATEMENT-REC
              SET AST-HEADER           TO TRUE
              MOVE WS-STMT-ID          TO AST-H-STMT-ID
              MOVE PRD-PRODUCER-CODE   TO AST-H-PRODUCER-CODE
              MOVE PRD-PRODUCER-NAME-TEXT TO AST-H-PRODUCER-NAME
              MOVE WS-BUS-DATE         TO AST-H-STMT-DATE
              MOVE WS-LAST-STMT-DATE   TO AST-H-PERIOD-FROM
              MOVE WS-DUE-DATE         TO AST-H-DUE-DATE
              PERFORM 3900-WRITE-STATEMENT
           END-IF
           ADD 1                       TO WS-ST-COUNT
           ADD ITM-ITEM-AMOUNT         TO WS-ST-GROSS
           ADD ITM-COMMISSION-AMOUNT   TO WS-ST-COMMISSION
           INITIALIZE STATEMENT-REC
           SET AST-DETAIL              TO TRUE
           MOVE WS-STMT-ID             TO AST-D-STMT-ID
           MOVE ACT-ACCOUNT-NUMBER     TO AST-D-ACCOUNT-NO
           MOVE WS-POL-NUM             TO AST-D-POLICY-NO
           MOVE ITM-ITEM-ID            TO AST-D-ITEM-ID
           MOVE ITM-ITEM-TYPE          TO AST-D-ITEM-TYPE
           MOVE ITM-INSTALL-NUMBER     TO AST-D-INSTALL-NO
           MOVE ITM-EVENT-DATE         TO AST-D-EVENT-DATE
           MOVE ITM-DUE-DATE           TO AST-D-DUE-DATE
           MOVE ITM-ITEM-AMOUNT        TO AST-D-GROSS
           MOVE ITM-COMMISSION-AMOUNT  TO AST-D-COMMISSION
      * BR-BIL-AGS-003 Net due to the carrier per item
           COMPUTE AST-D-NET = ITM-ITEM-AMOUNT - ITM-COMMISSION-AMOUNT
           PERFORM 3900-WRITE-STATEMENT.

       2300-CLOSE-STATEMENT.
      * BR-BIL-AGS-003 Net due = gross - commission
           COMPUTE WS-ST-NET = WS-ST-GROSS - WS-ST-COMMISSION
           INITIALIZE STATEMENT-REC
           SET AST-TRAILER             TO TRUE
           MOVE WS-STMT-ID             TO AST-T-STMT-ID
           MOVE WS-ST-COUNT            TO AST-T-ITEM-COUNT
           MOVE WS-ST-GROSS            TO AST-T-GROSS
           MOVE WS-ST-COMMISSION       TO AST-T-COMMISSION
           MOVE WS-ST-NET              TO AST-T-NET
           PERFORM 3900-WRITE-STATEMENT
           PERFORM 4400-INSERT-STATEMENT
           ADD 1                       TO WS-CNT-STATEMENTS
           ADD WS-ST-COUNT             TO WS-CNT-ITEMS
           ADD WS-ST-GROSS             TO WS-TOT-GROSS
           ADD WS-ST-COMMISSION        TO WS-TOT-COMMISSION
           ADD WS-ST-NET               TO WS-TOT-NET
           INITIALIZE AGR-DETAIL
           MOVE PRD-PRODUCER-CODE      TO AGR-D-PRODUCER-CODE
           MOVE PRD-PRODUCER-NAME-TEXT TO AGR-D-NAME
           MOVE WS-ST-COUNT            TO AGR-D-ITEMS
           MOVE WS-ST-GROSS            TO AGR-D-GROSS
           MOVE WS-ST-COMMISSION       TO AGR-D-COMMISSION
           MOVE WS-ST-NET              TO AGR-D-NET
           MOVE WS-STMT-ID             TO AGR-D-STMT-ID
           MOVE WS-DUE-DATE            TO AGR-D-DUE-DATE
           PERFORM 6000-WRITE-DETAIL.

       3900-WRITE-STATEMENT.
           WRITE STATEMENT-REC
           IF WS-STM-STATUS NOT = '00'
              DISPLAY 'BILAGSTM STMTOUT WRITE FAILED ' WS-STM-STATUS
              PERFORM 9800-ABEND
           END-IF
           ADD 1                       TO WS-CNT-RECORDS.

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
              FETCH AGP-CSR
               INTO :PRD-PRODUCER-ID, :PRD-PRODUCER-CODE,
                    :PRD-PRODUCER-NAME
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-PRD-EOF        TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-PRODUCER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * First statement ever: period runs from the low date
       4200-LAST-STATEMENT-DATE.
           EXEC SQL
              SELECT COALESCE(MAX(STMT_DATE), :WS-LOW-DATE)
                INTO :WS-LAST-STMT-DATE
                FROM BILDB.BIL_AGENCY_STATEMENT
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-LAST-STATEMENT-DATE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4300-FETCH-ITEM.
           EXEC SQL
              FETCH AGI-CSR
               INTO :ITM-ITEM-ID, :ITM-ITEM-TYPE, :ITM-INSTALL-NUMBER,
                    :ITM-EVENT-DATE, :ITM-DUE-DATE, :ITM-ITEM-AMOUNT,
                    :ITM-COMMISSION-AMOUNT, :WS-POL-NUM,
                    :ACT-ACCOUNT-NUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-ITM-EOF        TO TRUE
              WHEN OTHER
                 MOVE '4300-FETCH-ITEM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4400-INSERT-STATEMENT.
           MOVE WS-STMT-ID             TO AGS-STMT-ID
           MOVE PRD-PRODUCER-ID        TO AGS-PRODUCER-ID
           MOVE WS-BUS-DATE            TO AGS-STMT-DATE
           MOVE WS-DUE-DATE            TO AGS-DUE-DATE
           MOVE WS-ST-GROSS            TO AGS-GROSS-AMOUNT
           MOVE WS-ST-COMMISSION       TO AGS-COMMISSION-AMOUNT
           MOVE WS-ST-NET              TO AGS-NET-DUE
           MOVE WS-ST-COUNT            TO AGS-ITEM-COUNT
           EXEC SQL
              INSERT INTO BILDB.BIL_AGENCY_STATEMENT
                   ( STMT_ID, PRODUCER_ID, STMT_DATE, DUE_DATE,
                     GROSS_AMOUNT, COMMISSION_AMOUNT, NET_DUE,
                     PAID_AMOUNT, STMT_STATUS, ITEM_COUNT, CREATE_TS )
              VALUES
                   ( :AGS-STMT-ID, :AGS-PRODUCER-ID, :AGS-STMT-DATE,
                     :AGS-DUE-DATE, :AGS-GROSS-AMOUNT,
                     :AGS-COMMISSION-AMOUNT, :AGS-NET-DUE, 0, 'OPEN',
                     :AGS-ITEM-COUNT, CURRENT TIMESTAMP )
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-INSERT-STATEMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM AGR-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO AGR-H1-PAGE
           WRITE REPORT-REC FROM AGR-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM AGR-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM AGR-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6300-WRITE-TOTAL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM AGR-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE 0                      TO AGR-T-AMOUNT.

       6900-WRITE-CONTROL-TOTALS.
           MOVE '*** CONTROL TOTALS ***' TO AGR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'AGENCY-BILL PRODUCERS READ' TO AGR-T-LABEL
           MOVE WS-CNT-PRODUCERS       TO AGR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'STATEMENTS ISSUED'    TO AGR-T-LABEL
           MOVE WS-CNT-STATEMENTS      TO AGR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'NO BILLED ITEMS'      TO AGR-T-LABEL
           MOVE WS-CNT-NO-ITEMS        TO AGR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'ALREADY STATED TODAY' TO AGR-T-LABEL
           MOVE WS-CNT-ALREADY         TO AGR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'ITEMS STATED - GROSS' TO AGR-T-LABEL
           MOVE WS-CNT-ITEMS           TO AGR-T-COUNT
           MOVE WS-TOT-GROSS           TO AGR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMISSION RETAINED BY AGENCIES' TO AGR-T-LABEL
           MOVE 0                      TO AGR-T-COUNT
           MOVE WS-TOT-COMMISSION      TO AGR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'NET DUE FROM AGENCIES' TO AGR-T-LABEL
           MOVE WS-TOT-NET             TO AGR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'STATEMENT FILE RECORDS' TO AGR-T-LABEL
           MOVE WS-CNT-RECORDS         TO AGR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO AGR-T-LABEL
           MOVE WS-CNT-COMMITS         TO AGR-T-COUNT
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
           MOVE WS-CNT-PRODUCERS       TO CKP-READ-COUNT
           MOVE WS-CNT-STATEMENTS      TO CKP-DONE-COUNT
           SET CKP-RUNNING             TO TRUE
           IF WS-PRD-EOF
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILAGSTM CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       8000-NEXT-STATEMENT-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE WS-BATCH-USER          TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'AGS     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 12
              MOVE SPACES              TO AGR-M-TEXT
              STRING 'BILSEQGN AGS FAILED ' SQA-REASON-CODE ' '
                     SQA-MESSAGE DELIMITED BY SIZE INTO AGR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           MOVE SQA-SQ-VALUE           TO WS-STMT-ID.

      ******************************************************************
       9000-TERMINATE.
           PERFORM 7000-COMMIT
           IF WS-PAGE-COUNT = 0
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 STATEMENT-FILE
                 REPORT-FILE
           DISPLAY 'BILAGSTM COMPLETE PRODUCERS=' WS-CNT-PRODUCERS
                   ' STATEMENTS=' WS-CNT-STATEMENTS
                   ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      * Statement file records written since the last commit are not
      * backed out: the restart re-creates them, so the file from an
      * abended run must be discarded (DISP=(NEW,CATLG,DELETE)).
       9800-ABEND.
           IF WS-ITM-CSR-OPEN
              EXEC SQL CLOSE AGI-CSR END-EXEC
              MOVE SPACES              TO WS-OPEN-CURSOR
           END-IF
           IF WS-PRD-CSR-OPEN
              EXEC SQL CLOSE AGP-CSR END-EXEC
              MOVE 'N'                 TO WS-PRD-CSR-FLAG
           END-IF
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO AGR-M-TEXT
           STRING '*** BILAGSTM ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK. LAST KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO AGR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 STATEMENT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILAGSTM DB2 ERROR ' WS-SQLCODE-DISP ' IN '
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
