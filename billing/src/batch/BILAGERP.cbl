       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILAGERP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILAGERP                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Receivables aging report. Ages the open amount   *
      *               of every invoiced item by days past due into     *
      *               Current / 1-30 / 31-60 / 61-90 / 90+ buckets per *
      *               account, with subtotals by producer within       *
      *               product, grand totals with bucket percentages,   *
      *               and the 20 most delinquent accounts. Writes the  *
      *               132-column report and a CSV extract for finance. *
      *               Read-only: no table is changed.                  *
      * REQUESTS    : n/a (batch)                                      *
      * CALLED BY   : JCL BILEOD step AGERP, or BILAGERP ad hoc        *
      * CALLS       : BILDATES (DTADDD DTDIFF), BILERRLG               *
      * TABLES      : BIL_CONTROL (R) BIL_INVOICE_ITEM (R)             *
      *               BIL_POLICY_PERIOD (R) BIL_ACCOUNT (R)            *
      *               BIL_PRODUCER (R)                                 *
      * FILES       : AGERPT  report 132   (O)                         *
      *               AGECSV  CSV extract  (O)                         *
      *               CHKPT   checkpoint   (I/O)                       *
      * RULES       : BR-BIL-AGE-001 Only billed (BILD) and due (DUE)  *
      *                              items are aged; planned items are *
      *                              not yet receivable                *
      *               BR-BIL-AGE-002 Open amount = item - paid -       *
      *                              written off; zero-open excluded   *
      *               BR-BIL-AGE-003 Bucket by days past due: due      *
      *                              today or later is Current, then   *
      *                              1-30, 31-60, 61-90, over 90       *
      *               BR-BIL-AGE-004 Credit items net into Current     *
      *               BR-BIL-AGE-005 Producer 0 is reported as direct  *
      *                              bill                              *
      *               BR-BIL-AGE-006 Top 20 delinquent ranked by past  *
      *                              due amount, ties by account no    *
      *               BR-BIL-AGE-007 Report and extract totals must    *
      *                              agree or the run ends RC 12       *
      *               BR-BIL-AGE-008 Read-only report restarts from    *
      *                              the beginning; checkpoint proves  *
      *                              completion for the business date  *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE  ASSIGN TO AGERPT
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT CSV-FILE     ASSIGN TO AGECSV
                  FILE STATUS IS WS-CSV-STATUS.
           SELECT CHKPT-FILE   ASSIGN TO CHKPT
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE
           RECORDING MODE IS F.
           COPY BILEODP1.
       FD  CSV-FILE
           RECORDING MODE IS F.
           COPY BILAGEC1.
       FD  CHKPT-FILE
           RECORDING MODE IS F.
           COPY BILEODK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILAGERP'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-COMMIT-FREQ              PIC S9(4) COMP VALUE 200.
       01  WS-TOP-LIMIT                PIC S9(4) COMP VALUE 20.

       01  WS-FILE-STATUS.
           05 WS-RPT-STATUS            PIC XX.
           05 WS-CSV-STATUS            PIC XX.
           05 WS-CHK-STATUS            PIC XX.

       01  WS-FLAGS.
           05 WS-EOF-AGE               PIC X     VALUE 'N'.
              88 AGE-EOF                VALUE 'Y'.
           05 WS-EOF-TOP               PIC X     VALUE 'N'.
              88 TOP-EOF                VALUE 'Y'.
           05 WS-FIRST-ROW             PIC X     VALUE 'Y'.
              88 FIRST-ROW              VALUE 'Y'.

       01  WS-RUN.
           05 WS-RC                    PIC S9(4) COMP VALUE 0.
           05 WS-BUS-DATE             PIC X(10).
           05 WS-DATE-30               PIC X(10).
           05 WS-DATE-60               PIC X(10).
           05 WS-DATE-90               PIC X(10).
           05 WS-RUN-TS                PIC X(21).
           05 WS-ROWS-READ             PIC S9(9) COMP VALUE 0.
           05 WS-ROWS-SINCE-COMMIT     PIC S9(9) COMP VALUE 0.
           05 WS-CSV-WRITTEN           PIC S9(9) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-CNT              PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.
           05 WS-TOP-RANK              PIC S9(4) COMP VALUE 0.
           05 WS-PREV-PRODUCT          PIC X     VALUE LOW-VALUES.
           05 WS-PREV-PRODUCER         PIC S9(9) COMP VALUE -1.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).

      * Accumulators: level 1 producer, 2 product, 3 grand total.
      * Buckets: 1 current, 2 1-30, 3 31-60, 4 61-90, 5 90+, 6 total.
       01  WS-TOTALS.
           05 WS-TOT-LEVEL OCCURS 3 TIMES.
              10 WS-TOT-ACCOUNTS       PIC S9(9) COMP.
              10 WS-TOT-AMT OCCURS 6 TIMES
                                       PIC S9(11)V99 COMP-3.
       01  WS-CSV-TOTAL                PIC S9(11)V99 COMP-3 VALUE 0.
       01  WS-ROW.
           05 WS-ROW-AMT OCCURS 6 TIMES
                                       PIC S9(11)V99 COMP-3.
       01  WS-HOLD-LINE                PIC X(132).
       01  WS-BKT                      PIC S9(4) COMP.
       01  WS-LVL                      PIC S9(4) COMP.
       01  WS-PCT                      PIC S9(3)V99 COMP-3.
       01  WS-DAYS-PAST                PIC S9(7) COMP-3.

       01  WS-HOST.
           05 HV-PRODUCT-CODE          PIC X.
           05 HV-PRODUCER-ID           PIC S9(9) COMP.
           05 HV-PRODUCER-CODE         PIC S9(9) COMP.
           05 HV-AMT OCCURS 5 TIMES    PIC S9(11)V99 COMP-3.
           05 HV-PAST-DUE              PIC S9(11)V99 COMP-3.
           05 HV-OVER-90               PIC S9(11)V99 COMP-3.
           05 HV-OLDEST-DUE            PIC X(10).
           05 HV-ITEM-COUNT            PIC S9(9) COMP.

       01  WS-PRODUCER-LABEL           PIC X(40).
       01  WS-PRODUCT-NAME             PIC X(12).

      * CSV field formatting work area
       01  WS-CSV-WORK.
           05 WS-CSV-PTR               PIC S9(4) COMP.
           05 WS-AMT-EDIT              PIC -(11)9.99.
           05 WS-AMT-LEAD              PIC S9(4) COMP.
           05 WS-CSV-AMT               PIC S9(11)V99 COMP-3.
           05 WS-NUM-EDIT              PIC Z(8)9.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDPRD END-EXEC.

      * BR-BIL-AGE-001 Only billed and due items are aged
      * BR-BIL-AGE-002 Open amount = item - paid - written off
      * BR-BIL-AGE-003 Bucket boundaries are business date -30/-60/-90
      * BR-BIL-AGE-004 Credit (negative open) items fall into Current
           EXEC SQL DECLARE AGECSR CURSOR WITH HOLD FOR
              SELECT P.PRODUCT_CODE, P.PRODUCER_ID,
                     A.ACCOUNT_NUMBER, A.ACCOUNT_NAME,
                     A.COLLECTION_STATUS,
                     SUM(CASE WHEN I.DUE_DATE >= :WS-BUS-DATE
                                OR I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT < 0
                              THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT
                              ELSE 0 END),
                     SUM(CASE WHEN I.DUE_DATE <  :WS-BUS-DATE
                               AND I.DUE_DATE >= :WS-DATE-30
                               AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT > 0
                              THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT
                              ELSE 0 END),
                     SUM(CASE WHEN I.DUE_DATE <  :WS-DATE-30
                               AND I.DUE_DATE >= :WS-DATE-60
                               AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT > 0
                              THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT
                              ELSE 0 END),
                     SUM(CASE WHEN I.DUE_DATE <  :WS-DATE-60
                               AND I.DUE_DATE >= :WS-DATE-90
                               AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT > 0
                              THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT
                              ELSE 0 END),
                     SUM(CASE WHEN I.DUE_DATE <  :WS-DATE-90
                               AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT > 0
                              THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT
                              ELSE 0 END),
                     COUNT(*)
                FROM BILDB.BIL_INVOICE_ITEM  I
                JOIN BILDB.BIL_POLICY_PERIOD P ON P.PP_ID = I.PP_ID
                JOIN BILDB.BIL_ACCOUNT       A
                  ON A.ACCOUNT_ID = I.ACCOUNT_ID
               WHERE I.ITEM_STATUS IN ('BILD', 'DUE ')
                 AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                     - I.WRITEOFF_AMOUNT <> 0
               GROUP BY P.PRODUCT_CODE, P.PRODUCER_ID,
                        A.ACCOUNT_NUMBER, A.ACCOUNT_NAME,
                        A.COLLECTION_STATUS
               ORDER BY P.PRODUCT_CODE, P.PRODUCER_ID,
                        A.ACCOUNT_NUMBER
               FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-AGE-006 Top delinquent accounts by past-due amount
           EXEC SQL DECLARE TOPCSR CURSOR FOR
              SELECT A.ACCOUNT_NUMBER, A.ACCOUNT_NAME,
                     A.COLLECTION_STATUS,
                     SUM(I.ITEM_AMOUNT - I.PAID_AMOUNT
                         - I.WRITEOFF_AMOUNT),
                     SUM(CASE WHEN I.DUE_DATE < :WS-DATE-90
                              THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                   - I.WRITEOFF_AMOUNT
                              ELSE 0 END),
                     CHAR(MIN(I.DUE_DATE), ISO),
                     COUNT(*)
                FROM BILDB.BIL_INVOICE_ITEM I
                JOIN BILDB.BIL_ACCOUNT      A
                  ON A.ACCOUNT_ID = I.ACCOUNT_ID
               WHERE I.ITEM_STATUS IN ('BILD', 'DUE ')
                 AND I.DUE_DATE   < :WS-BUS-DATE
                 AND I.ITEM_AMOUNT - I.PAID_AMOUNT
                     - I.WRITEOFF_AMOUNT > 0
               GROUP BY A.ACCOUNT_NUMBER, A.ACCOUNT_NAME,
                        A.COLLECTION_STATUS
               ORDER BY 4 DESC, 1
               FETCH FIRST 20 ROWS ONLY
               FOR FETCH ONLY
           END-EXEC.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

      * Report lines
       01  RPT-HEAD-1.
           05 FILLER                   PIC X(10) VALUE 'BILAGERP'.
           05 FILLER                   PIC X(20) VALUE SPACES.
           05 FILLER                   PIC X(46) VALUE
              'BILLINGCORE  RECEIVABLES AGING REPORT'.
           05 FILLER                   PIC X(15) VALUE
              'BUSINESS DATE '.
           05 RH1-BUS-DATE             PIC X(10).
           05 FILLER                   PIC X(21) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 RH1-PAGE                 PIC ZZZ9.
       01  RPT-HEAD-2.
           05 FILLER                   PIC X(10) VALUE 'RUN'.
           05 RH2-RUN-TS               PIC X(21).
           05 FILLER                   PIC X(30) VALUE SPACES.
           05 RH2-SECTION              PIC X(40).
       01  RPT-HEAD-3.
           05 FILLER                   PIC X(39) VALUE
              'ACCOUNT    ACCOUNT NAME           COLL'.
           05 FILLER                   PIC X(15) VALUE
              '        CURRENT'.
           05 FILLER                   PIC X(15) VALUE
              '      1-30 DAYS'.
           05 FILLER                   PIC X(15) VALUE
              '     31-60 DAYS'.
           05 FILLER                   PIC X(15) VALUE
              '     61-90 DAYS'.
           05 FILLER                   PIC X(15) VALUE
              '        OVER 90'.
           05 FILLER                   PIC X(15) VALUE
              '          TOTAL'.
       01  RPT-BREAK-LINE.
           05 RBK-LABEL                PIC X(12).
           05 RBK-VALUE                PIC X(60).
       01  RPT-DETAIL.
           05 RD-ACCOUNT               PIC X(10).
           05 FILLER                   PIC X     VALUE SPACE.
           05 RD-NAME                  PIC X(22).
           05 FILLER                   PIC X     VALUE SPACE.
           05 RD-COLL                  PIC X(4).
           05 FILLER                   PIC X     VALUE SPACE.
           05 RD-AMT OCCURS 6 TIMES    PIC ZZZ,ZZZ,ZZ9.99-.
       01  RPT-TOTAL.
           05 RT-LABEL                 PIC X(26).
           05 RT-ACCOUNTS              PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(6)  VALUE SPACES.
           05 RT-AMT OCCURS 6 TIMES    PIC ZZZ,ZZZ,ZZ9.99-.
       01  RPT-PCT-LINE.
           05 FILLER                   PIC X(39) VALUE
              '   PERCENT OF TOTAL'.
           05 RP-ENTRY OCCURS 6 TIMES.
              10 FILLER                PIC X(8)  VALUE SPACES.
              10 RP-PCT                PIC ZZ9.99-.
       01  RPT-TOP-HEAD.
           05 FILLER                   PIC X(48) VALUE
              'RANK ACCOUNT    ACCOUNT NAME'.
           05 FILLER                   PIC X(31) VALUE
              'COLL  OLDEST DUE  DAYS  ITEMS'.
           05 FILLER                   PIC X(28) VALUE
              '     PAST DUE        OVER 90'.
       01  RPT-TOP-LINE.
           05 RTL-RANK                 PIC ZZ9.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RTL-ACCOUNT              PIC X(10).
           05 FILLER                   PIC X     VALUE SPACE.
           05 RTL-NAME                 PIC X(31).
           05 FILLER                   PIC X     VALUE SPACE.
           05 RTL-COLL                 PIC X(4).
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RTL-OLDEST               PIC X(10).
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RTL-DAYS                 PIC ZZZ9.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RTL-ITEMS                PIC ZZZZ9.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RTL-PAST-DUE             PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X     VALUE SPACE.
           05 RTL-OVER-90              PIC Z,ZZZ,ZZ9.99-.
       01  RPT-CONTROL-LINE.
           05 RCL-LABEL                PIC X(40).
           05 RCL-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 RCL-AMOUNT               PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-AGING-SECTION
           PERFORM 2500-TOP-DELINQUENT
           PERFORM 2800-RUN-CONTROLS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE  TO WS-RUN-TS
           INITIALIZE WS-TOTALS
           PERFORM 4000-READ-CONTROL
           PERFORM 1100-BUCKET-DATES
           PERFORM 6000-READ-CHECKPOINT
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILAGERP AGERPT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           OPEN OUTPUT CSV-FILE
           IF WS-CSV-STATUS NOT = '00'
              DISPLAY 'BILAGERP AGECSV OPEN FAILED ' WS-CSV-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-BUS-DATE            TO RH1-BUS-DATE
           MOVE WS-RUN-TS              TO RH2-RUN-TS
           MOVE 'AGING BY PRODUCT AND PRODUCER' TO RH2-SECTION
           MOVE SPACES                 TO CSV-RECORD
           STRING 'BUS_DATE,PRODUCT,PRODUCER_CODE,ACCOUNT_NUMBER,'
                  'COLL_STATUS,CURRENT,D1_30,D31_60,D61_90,'
                  'D90_PLUS,TOTAL'
                  DELIMITED BY SIZE INTO CSV-RECORD
           PERFORM 7300-WRITE-CSV.

      ******************************************************************
      * BR-BIL-AGE-003 Bucket boundaries are derived through BILDATES
      * so the calendar rules live in one place.
       1100-BUCKET-DATES.
           MOVE -30                    TO DTA-DT-NUMBER
           PERFORM 8000-ADD-DAYS
           MOVE DTA-DT-RESULT-DATE     TO WS-DATE-30
           MOVE -60                    TO DTA-DT-NUMBER
           PERFORM 8000-ADD-DAYS
           MOVE DTA-DT-RESULT-DATE     TO WS-DATE-60
           MOVE -90                    TO DTA-DT-NUMBER
           PERFORM 8000-ADD-DAYS
           MOVE DTA-DT-RESULT-DATE     TO WS-DATE-90.

      ******************************************************************
       2000-AGING-SECTION.
           EXEC SQL OPEN AGECSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-AGING-SECTION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-AGING
           PERFORM UNTIL AGE-EOF
              PERFORM 2100-PROCESS-ROW
              PERFORM 4100-FETCH-AGING
           END-PERFORM
           EXEC SQL CLOSE AGECSR END-EXEC
           IF NOT FIRST-ROW
              PERFORM 2300-PRODUCER-BREAK
              PERFORM 2400-PRODUCT-BREAK
           END-IF
           PERFORM 7200-PRINT-GRAND-TOTAL.

      ******************************************************************
       2100-PROCESS-ROW.
           ADD 1                       TO WS-ROWS-READ
                                          WS-ROWS-SINCE-COMMIT
           IF FIRST-ROW
              MOVE 'N'                 TO WS-FIRST-ROW
              PERFORM 2200-START-PRODUCT
              PERFORM 2250-START-PRODUCER
           ELSE
              IF HV-PRODUCT-CODE NOT = WS-PREV-PRODUCT
                 PERFORM 2300-PRODUCER-BREAK
                 PERFORM 2400-PRODUCT-BREAK
                 PERFORM 2200-START-PRODUCT
                 PERFORM 2250-START-PRODUCER
              ELSE
                 IF HV-PRODUCER-ID NOT = WS-PREV-PRODUCER
                    PERFORM 2300-PRODUCER-BREAK
                    PERFORM 2250-START-PRODUCER
                 END-IF
              END-IF
           END-IF
           PERFORM 5000-ROW-AMOUNTS
           PERFORM 7100-PRINT-DETAIL
           PERFORM 7400-CSV-DETAIL
           IF WS-ROWS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 6100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
       2200-START-PRODUCT.
           MOVE HV-PRODUCT-CODE        TO WS-PREV-PRODUCT
           PERFORM 5200-PRODUCT-NAME
           MOVE 99                     TO WS-LINE-CNT
           MOVE 'PRODUCT:'             TO RBK-LABEL
           MOVE SPACES                 TO RBK-VALUE
           STRING HV-PRODUCT-CODE ' - ' WS-PRODUCT-NAME
                  DELIMITED BY SIZE INTO RBK-VALUE
           PERFORM 7000-PRINT-HEADINGS
           MOVE RPT-BREAK-LINE         TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
      * BR-BIL-AGE-005 Producer 0 is direct bill (no producer row)
       2250-START-PRODUCER.
           MOVE HV-PRODUCER-ID         TO WS-PREV-PRODUCER
           MOVE 0                      TO HV-PRODUCER-CODE
           IF HV-PRODUCER-ID = 0
              MOVE 'DIRECT BILL - NO PRODUCER' TO WS-PRODUCER-LABEL
           ELSE
              PERFORM 4200-READ-PRODUCER
           END-IF
           MOVE 'PRODUCER:'            TO RBK-LABEL
           MOVE WS-PRODUCER-LABEL      TO RBK-VALUE
           IF WS-LINE-CNT + 3 > WS-LINES-PER-PAGE
              PERFORM 7000-PRINT-HEADINGS
           END-IF
           MOVE SPACES                 TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE RPT-BREAK-LINE         TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       2300-PRODUCER-BREAK.
           MOVE 1                      TO WS-LVL
           MOVE '  PRODUCER TOTAL'     TO RT-LABEL
           PERFORM 7150-PRINT-TOTAL-LINE.

      ******************************************************************
       2400-PRODUCT-BREAK.
           MOVE 2                      TO WS-LVL
           MOVE SPACES                 TO RT-LABEL
           STRING '  PRODUCT ' WS-PREV-PRODUCT ' TOTAL'
                  DELIMITED BY SIZE INTO RT-LABEL
           PERFORM 7150-PRINT-TOTAL-LINE.

      ******************************************************************
       2500-TOP-DELINQUENT.
           MOVE 'TOP 20 DELINQUENT ACCOUNTS' TO RH2-SECTION
           MOVE 99                     TO WS-LINE-CNT
           PERFORM 7000-PRINT-HEADINGS
           MOVE RPT-TOP-HEAD           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           EXEC SQL OPEN TOPCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-TOP-DELINQUENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4300-FETCH-TOP
           PERFORM UNTIL TOP-EOF OR WS-TOP-RANK >= WS-TOP-LIMIT
              ADD 1                    TO WS-TOP-RANK
              PERFORM 2550-PRINT-TOP-ROW
              PERFORM 4300-FETCH-TOP
           END-PERFORM
           EXEC SQL CLOSE TOPCSR END-EXEC
           IF WS-TOP-RANK = 0
              MOVE '     NO ACCOUNTS PAST DUE' TO PRT-RECORD
              PERFORM 7900-PRINT-LINE
           END-IF.

      ******************************************************************
       2550-PRINT-TOP-ROW.
           MOVE HV-OLDEST-DUE          TO DTA-DT-DATE-1
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-2
           PERFORM 8100-DAYS-BETWEEN
           MOVE DTA-DT-RESULT-NUM      TO WS-DAYS-PAST
           MOVE WS-TOP-RANK            TO RTL-RANK
           MOVE ACT-ACCOUNT-NUMBER     TO RTL-ACCOUNT
           MOVE SPACES                 TO RTL-NAME
           MOVE ACT-ACCOUNT-NAME-TEXT(1:ACT-ACCOUNT-NAME-LEN)
                                       TO RTL-NAME
           MOVE ACT-COLLECTION-STATUS  TO RTL-COLL
           MOVE HV-OLDEST-DUE          TO RTL-OLDEST
           MOVE WS-DAYS-PAST           TO RTL-DAYS
           MOVE HV-ITEM-COUNT          TO RTL-ITEMS
           MOVE HV-PAST-DUE            TO RTL-PAST-DUE
           MOVE HV-OVER-90             TO RTL-OVER-90
           MOVE RPT-TOP-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
      * BR-BIL-AGE-007 The CSV extract must carry exactly the grand
      * total printed on the report.
       2800-RUN-CONTROLS.
           MOVE 'RUN CONTROLS'         TO RH2-SECTION
           MOVE 99                     TO WS-LINE-CNT
           PERFORM 7000-PRINT-HEADINGS
           MOVE 'ACCOUNT ROWS AGED'    TO RCL-LABEL
           MOVE WS-ROWS-READ           TO RCL-COUNT
           MOVE WS-TOT-AMT(3, 6)       TO RCL-AMOUNT
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE 'CSV DETAIL RECORDS WRITTEN' TO RCL-LABEL
           COMPUTE RCL-COUNT = WS-CSV-WRITTEN - 1
           MOVE WS-CSV-TOTAL           TO RCL-AMOUNT
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE 'DELINQUENT ACCOUNTS LISTED' TO RCL-LABEL
           MOVE WS-TOP-RANK            TO RCL-COUNT
           MOVE 0                      TO RCL-AMOUNT
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           IF WS-CSV-TOTAL NOT = WS-TOT-AMT(3, 6)
              OR WS-CSV-WRITTEN - 1 NOT = WS-ROWS-READ
              MOVE '*** CSV EXTRACT DOES NOT AGREE TO REPORT TOTALS'
                                       TO PRT-RECORD
              PERFORM 7900-PRINT-LINE
              MOVE 12                  TO WS-RC
           ELSE
              IF WS-ROWS-READ = 0
                 MOVE '*** WARNING - NO OPEN INVOICED ITEMS FOUND'
                                       TO PRT-RECORD
                 PERFORM 7900-PRINT-LINE
                 MOVE 4                TO WS-RC
              END-IF
           END-IF.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT CHAR(BUSINESS_DATE, ISO), BATCH_STATUS
                INTO :CTL-BUSINESS-DATE, :CTL-BATCH-STATUS
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-CONTROL' TO WS-ERR-PARA
              MOVE CTL-CONTROL-KEY     TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
       4100-FETCH-AGING.
           EXEC SQL
              FETCH AGECSR
               INTO :HV-PRODUCT-CODE, :HV-PRODUCER-ID,
                    :ACT-ACCOUNT-NUMBER, :ACT-ACCOUNT-NAME,
                    :ACT-COLLECTION-STATUS,
                    :HV-AMT(1), :HV-AMT(2), :HV-AMT(3),
                    :HV-AMT(4), :HV-AMT(5), :HV-ITEM-COUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET AGE-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-AGING' TO WS-ERR-PARA
                 MOVE ACT-ACCOUNT-NUMBER TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-PRODUCER.
           MOVE HV-PRODUCER-ID         TO PRD-PRODUCER-ID
           EXEC SQL
              SELECT PRODUCER_CODE, PRODUCER_NAME
                INTO :PRD-PRODUCER-CODE, :PRD-PRODUCER-NAME
                FROM BILDB.BIL_PRODUCER
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE PRD-PRODUCER-CODE TO HV-PRODUCER-CODE
                                           WS-NUM-EDIT
                 MOVE SPACES           TO WS-PRODUCER-LABEL
                 STRING WS-NUM-EDIT ' '
                        PRD-PRODUCER-NAME-TEXT(1:PRD-PRODUCER-NAME-LEN)
                        DELIMITED BY SIZE INTO WS-PRODUCER-LABEL
              WHEN 100
                 MOVE HV-PRODUCER-ID   TO WS-NUM-EDIT
                 MOVE SPACES           TO WS-PRODUCER-LABEL
                 STRING WS-NUM-EDIT ' *** PRODUCER NOT ON FILE'
                        DELIMITED BY SIZE INTO WS-PRODUCER-LABEL
                 IF WS-RC < 4
                    MOVE 4             TO WS-RC
                 END-IF
              WHEN OTHER
                 MOVE '4200-READ-PRODUCER' TO WS-ERR-PARA
                 MOVE HV-PRODUCER-ID   TO WS-NUM-EDIT
                 MOVE WS-NUM-EDIT      TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-FETCH-TOP.
           EXEC SQL
              FETCH TOPCSR
               INTO :ACT-ACCOUNT-NUMBER, :ACT-ACCOUNT-NAME,
                    :ACT-COLLECTION-STATUS,
                    :HV-PAST-DUE, :HV-OVER-90, :HV-OLDEST-DUE,
                    :HV-ITEM-COUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET TOP-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4300-FETCH-TOP' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Bucket amounts for one account row; total is bucket sum and
      * the row is added into producer, product and grand totals.
       5000-ROW-AMOUNTS.
           MOVE 0                      TO WS-ROW-AMT(6)
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 5
              MOVE HV-AMT(WS-BKT)      TO WS-ROW-AMT(WS-BKT)
              ADD HV-AMT(WS-BKT)       TO WS-ROW-AMT(6)
           END-PERFORM
           PERFORM VARYING WS-LVL FROM 1 BY 1 UNTIL WS-LVL > 3
              ADD 1                    TO WS-TOT-ACCOUNTS(WS-LVL)
              PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
                 ADD WS-ROW-AMT(WS-BKT)
                                       TO WS-TOT-AMT(WS-LVL, WS-BKT)
              END-PERFORM
           END-PERFORM.

      ******************************************************************
       5100-BUCKET-PERCENT.
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
              IF WS-TOT-AMT(3, 6) = 0
                 MOVE 0                TO WS-PCT
              ELSE
                 COMPUTE WS-PCT ROUNDED =
                    WS-TOT-AMT(3, WS-BKT) * 100 / WS-TOT-AMT(3, 6)
              END-IF
              MOVE WS-PCT              TO RP-PCT(WS-BKT)
           END-PERFORM.

      ******************************************************************
       5200-PRODUCT-NAME.
           EVALUATE HV-PRODUCT-CODE
              WHEN 'M'  MOVE 'MOTOR'        TO WS-PRODUCT-NAME
              WHEN 'H'  MOVE 'HOUSE'        TO WS-PRODUCT-NAME
              WHEN 'C'  MOVE 'COMMERCIAL'   TO WS-PRODUCT-NAME
              WHEN 'E'  MOVE 'ENDOWMENT'    TO WS-PRODUCT-NAME
              WHEN OTHER
                 MOVE 'UNKNOWN'             TO WS-PRODUCT-NAME
                 IF WS-RC < 4
                    MOVE 4                  TO WS-RC
                 END-IF
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-AGE-008 A previous incomplete run for the same date is
      * reported; the report is regenerated in full (read-only job).
       6000-READ-CHECKPOINT.
           OPEN INPUT CHKPT-FILE
           IF WS-CHK-STATUS = '00'
              READ CHKPT-FILE
              IF WS-CHK-STATUS = '00'
                 AND CHK-PROGRAM-ID = WS-PROGRAM-ID
                 AND CHK-BUS-DATE = WS-BUS-DATE
                 AND CHK-RUNNING
                 DISPLAY 'BILAGERP RESTART - PRIOR RUN STOPPED AFTER '
                         CHK-ROWS-DONE ' ROWS AT ' CHK-RESTART-KEY
                         ' - REPORT REGENERATED IN FULL'
              END-IF
              CLOSE CHKPT-FILE
           END-IF
           MOVE SPACES                 TO CHK-RESTART-KEY
           SET CHK-RUNNING             TO TRUE
           PERFORM 6200-WRITE-CHECKPOINT.

      ******************************************************************
       6100-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '6100-COMMIT-CHECKPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-ROWS-SINCE-COMMIT
           MOVE ACT-ACCOUNT-NUMBER     TO CHK-RESTART-KEY
           PERFORM 6200-WRITE-CHECKPOINT.

      ******************************************************************
       6200-WRITE-CHECKPOINT.
           MOVE WS-PROGRAM-ID          TO CHK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CHK-BUS-DATE
           MOVE WS-ROWS-READ           TO CHK-ROWS-DONE
           MOVE FUNCTION CURRENT-DATE  TO CHK-TIMESTAMP
           OPEN OUTPUT CHKPT-FILE
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'BILAGERP CHKPT OPEN FAILED ' WS-CHK-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           WRITE CHK-RECORD
           CLOSE CHKPT-FILE.

      ******************************************************************
       7000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO RH1-PAGE
           WRITE PRT-RECORD FROM RPT-HEAD-1 AFTER ADVANCING PAGE
           WRITE PRT-RECORD FROM RPT-HEAD-2 AFTER ADVANCING 1 LINE
           IF RH2-SECTION(1:5) = 'AGING'
              WRITE PRT-RECORD FROM RPT-HEAD-3 AFTER ADVANCING 2 LINES
           END-IF
           MOVE SPACES                 TO PRT-RECORD
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE
           MOVE 5                      TO WS-LINE-CNT.

      ******************************************************************
       7100-PRINT-DETAIL.
           MOVE ACT-ACCOUNT-NUMBER     TO RD-ACCOUNT
           MOVE SPACES                 TO RD-NAME
           MOVE ACT-ACCOUNT-NAME-TEXT(1:ACT-ACCOUNT-NAME-LEN)
                                       TO RD-NAME
           MOVE ACT-COLLECTION-STATUS  TO RD-COLL
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
              MOVE WS-ROW-AMT(WS-BKT)  TO RD-AMT(WS-BKT)
           END-PERFORM
           MOVE RPT-DETAIL             TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
      * Print totals at WS-LVL, then clear that level for the next group
       7150-PRINT-TOTAL-LINE.
           MOVE WS-TOT-ACCOUNTS(WS-LVL) TO RT-ACCOUNTS
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
              MOVE WS-TOT-AMT(WS-LVL, WS-BKT) TO RT-AMT(WS-BKT)
              MOVE 0                   TO WS-TOT-AMT(WS-LVL, WS-BKT)
           END-PERFORM
           MOVE 0                      TO WS-TOT-ACCOUNTS(WS-LVL)
           MOVE RPT-TOTAL              TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE SPACES                 TO RT-LABEL.

      ******************************************************************
       7200-PRINT-GRAND-TOTAL.
           IF WS-LINE-CNT + 4 > WS-LINES-PER-PAGE
              PERFORM 7000-PRINT-HEADINGS
           END-IF
           MOVE SPACES                 TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE 'GRAND TOTAL  ACCOUNTS'  TO RT-LABEL
           MOVE WS-TOT-ACCOUNTS(3)     TO RT-ACCOUNTS
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
              MOVE WS-TOT-AMT(3, WS-BKT) TO RT-AMT(WS-BKT)
           END-PERFORM
           MOVE RPT-TOTAL              TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           PERFORM 5100-BUCKET-PERCENT
           MOVE RPT-PCT-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       7300-WRITE-CSV.
           WRITE CSV-RECORD
           IF WS-CSV-STATUS NOT = '00'
              DISPLAY 'BILAGERP AGECSV WRITE FAILED ' WS-CSV-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           ADD 1                       TO WS-CSV-WRITTEN.

      ******************************************************************
       7400-CSV-DETAIL.
           MOVE SPACES                 TO CSV-RECORD
           MOVE 1                      TO WS-CSV-PTR
           MOVE HV-PRODUCER-CODE       TO WS-NUM-EDIT
           STRING WS-BUS-DATE ',' HV-PRODUCT-CODE ','
                  FUNCTION TRIM(WS-NUM-EDIT) ','
                  ACT-ACCOUNT-NUMBER ',' ACT-COLLECTION-STATUS
                  DELIMITED BY SIZE
                  INTO CSV-RECORD WITH POINTER WS-CSV-PTR
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 6
              MOVE WS-ROW-AMT(WS-BKT)  TO WS-CSV-AMT
              PERFORM 7450-CSV-AMOUNT
           END-PERFORM
           ADD WS-ROW-AMT(6)           TO WS-CSV-TOTAL
           PERFORM 7300-WRITE-CSV.

      ******************************************************************
      * Append ",<amount>" with the edited amount left-trimmed
       7450-CSV-AMOUNT.
           MOVE WS-CSV-AMT             TO WS-AMT-EDIT
           MOVE 0                      TO WS-AMT-LEAD
           INSPECT WS-AMT-EDIT TALLYING WS-AMT-LEAD
                   FOR LEADING SPACES
           STRING ',' WS-AMT-EDIT(WS-AMT-LEAD + 1:)
                  DELIMITED BY SIZE
                  INTO CSV-RECORD WITH POINTER WS-CSV-PTR.

      ******************************************************************
       7900-PRINT-LINE.
           IF WS-LINE-CNT >= WS-LINES-PER-PAGE
              MOVE PRT-RECORD          TO WS-HOLD-LINE
              PERFORM 7000-PRINT-HEADINGS
              MOVE WS-HOLD-LINE        TO PRT-RECORD
           END-IF
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-CNT.

      ******************************************************************
       8000-ADD-DAYS.
           INITIALIZE DTA-HEADER
           MOVE 'DTADDD  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
                                          DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
                                          DTA-DT-DATE-1
           PERFORM 8900-CALL-BILDATES.

      ******************************************************************
       8100-DAYS-BETWEEN.
           INITIALIZE DTA-HEADER
           MOVE 'DTDIFF  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
                                          DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           PERFORM 8900-CALL-BILDATES.

      ******************************************************************
       8900-CALL-BILDATES.
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              DISPLAY 'BILAGERP BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-REASON-CODE ' '
                      DTA-MESSAGE
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           IF WS-RC < 16
              EXEC SQL COMMIT END-EXEC
              MOVE SPACES              TO CHK-RESTART-KEY
              SET CHK-COMPLETE         TO TRUE
              PERFORM 6200-WRITE-CHECKPOINT
           END-IF
           CLOSE REPORT-FILE CSV-FILE
           DISPLAY 'BILAGERP ENDED RC ' WS-RC ' ROWS ' WS-ROWS-READ
           MOVE WS-RC                  TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILAGERP DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID
                                          ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'BILAGERP Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL ROLLBACK END-EXEC
           SET CHK-FAILED              TO TRUE
           PERFORM 6200-WRITE-CHECKPOINT
           MOVE 16                     TO WS-RC
           PERFORM 9000-RETURN.
