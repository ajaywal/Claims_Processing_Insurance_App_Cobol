       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILRECON.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILRECON                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Ledger reconciliation. Builds the trial balance  *
      *               per T-account (DR - CR) for the business date    *
      *               and cumulative to date, proves the ledger nets   *
      *               to zero, lists transactions that do not balance  *
      *               or carry no lines, and reconciles each           *
      *               sub-ledger to its control T-account:             *
      *                 UNBILLED / BILLED / DUE  <- invoice items      *
      *                 UNAPPLD                  <- account unapplied  *
      *                 COMMPAY                  <- producer payable   *
      *                 SUSPENSE                 <- open suspense      *
      *               Any break ends the step RC 8. Read-only.         *
      * REQUESTS    : n/a (batch)                                      *
      * CALLED BY   : JCL BILEOD step RECON                            *
      * CALLS       : BILERRLG                                         *
      * TABLES      : BIL_CONTROL (R) BIL_LEDGER_LINE (R)              *
      *               BIL_TRANSACTION (R) BIL_INVOICE_ITEM (R)         *
      *               BIL_ACCOUNT (R) BIL_PRODUCER (R)                 *
      *               BIL_SUSPENSE (R)                                 *
      * FILES       : RECRPT  report 132  (O)                          *
      *               CHKPT   checkpoint  (I/O)                        *
      * RULES       : BR-BIL-REC-001 Ledger proves to zero: total DR = *
      *                              total CR for the day and to date  *
      *               BR-BIL-REC-002 Every transaction of the day must *
      *                              balance and carry ledger lines    *
      *               BR-BIL-REC-003 Only chart-of-accounts T-accounts *
      *                              may carry ledger lines            *
      *               BR-BIL-REC-004 Sub-ledger compared on the normal *
      *                              side: asset DR-CR, liability CR-DR*
      *               BR-BIL-REC-005 Item sub-ledgers by item status:  *
      *                              PLAN = UNBILLED, BILD = BILLED,   *
      *                              DUE = DUE (CLSD/CANC carry none)  *
      *               BR-BIL-REC-006 Zero tolerance - a one-penny      *
      *                              difference is a break (RC 8)      *
      *               BR-BIL-REC-007 Stale account balance caches are  *
      *                              warnings (RC 4), not breaks       *
      *               BR-BIL-REC-008 Read-only: restart reruns in full *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE  ASSIGN TO RECRPT
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT CHKPT-FILE   ASSIGN TO CHKPT
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE
           RECORDING MODE IS F.
           COPY BILEODP1.
       FD  CHKPT-FILE
           RECORDING MODE IS F.
           COPY BILEODK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILRECON'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-COMMIT-FREQ              PIC S9(4) COMP VALUE 200.
       01  WS-MAX-LISTED               PIC S9(4) COMP VALUE 50.

       01  WS-FILE-STATUS.
           05 WS-RPT-STATUS            PIC XX.
           05 WS-CHK-STATUS            PIC XX.

       01  WS-FLAGS.
           05 WS-EOF                   PIC X     VALUE 'N'.
              88 CSR-EOF                VALUE 'Y'.

       01  WS-RUN.
           05 WS-RC                    PIC S9(4) COMP VALUE 0.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-RUN-TS                PIC X(21).
           05 WS-SECTION               PIC X(20).
           05 WS-ROWS-READ             PIC S9(9) COMP VALUE 0.
           05 WS-ROWS-SINCE-COMMIT     PIC S9(9) COMP VALUE 0.
           05 WS-BREAKS                PIC S9(9) COMP VALUE 0.
           05 WS-WARNINGS              PIC S9(9) COMP VALUE 0.
           05 WS-LISTED                PIC S9(9) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-CNT              PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-HOLD-LINE             PIC X(132).

      * Chart of accounts: T-account, normal side, sub-ledger slot
       01  WS-COA-VALUES.
           05 FILLER PIC X(10) VALUE 'CASH    D0'.
           05 FILLER PIC X(10) VALUE 'UNAPPLD C4'.
           05 FILLER PIC X(10) VALUE 'UNBILLEDD1'.
           05 FILLER PIC X(10) VALUE 'BILLED  D2'.
           05 FILLER PIC X(10) VALUE 'DUE     D3'.
           05 FILLER PIC X(10) VALUE 'PREMREV C0'.
           05 FILLER PIC X(10) VALUE 'FEEREV  C0'.
           05 FILLER PIC X(10) VALUE 'SUSPENSEC6'.
           05 FILLER PIC X(10) VALUE 'WRITEOFFD0'.
           05 FILLER PIC X(10) VALUE 'COMMRSV C0'.
           05 FILLER PIC X(10) VALUE 'COMMPAY C5'.
           05 FILLER PIC X(10) VALUE 'DISBPAY C0'.
           05 FILLER PIC X(10) VALUE 'CLMPAY  C0'.
       01  WS-COA REDEFINES WS-COA-VALUES.
           05 WS-COA-ENTRY OCCURS 13 TIMES INDEXED BY COA-IX.
              10 WS-COA-TACCT          PIC X(8).
              10 WS-COA-NORMAL         PIC X.
              10 WS-COA-SUBLDG         PIC 9.
       01  WS-COA-COUNT                PIC S9(4) COMP VALUE 13.

      * Trial balance per T-account (index matches the chart above)
       01  WS-TB.
           05 WS-TB-ENTRY OCCURS 13 TIMES.
              10 WS-TB-DAY-DR          PIC S9(13)V99 COMP-3.
              10 WS-TB-DAY-CR          PIC S9(13)V99 COMP-3.
              10 WS-TB-CUM-DR          PIC S9(13)V99 COMP-3.
              10 WS-TB-CUM-CR          PIC S9(13)V99 COMP-3.
       01  WS-TB-TOTALS.
           05 WS-TOT-DAY-DR            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TOT-DAY-CR            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TOT-CUM-DR            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TOT-CUM-CR            PIC S9(13)V99 COMP-3 VALUE 0.

      * Sub-ledger slots 1 UNBILLED 2 BILLED 3 DUE 4 UNAPPLD
      *                 5 COMMPAY  6 SUSPENSE
       01  WS-SUBLEDGER.
           05 WS-SL-ENTRY OCCURS 6 TIMES.
              10 WS-SL-SOURCE-AMT      PIC S9(13)V99 COMP-3.
              10 WS-SL-LEDGER-AMT      PIC S9(13)V99 COMP-3.
              10 WS-SL-SOURCE-NAME     PIC X(30).
       01  WS-SL                       PIC S9(4) COMP.
       01  WS-TB-IX                    PIC S9(4) COMP.
       01  WS-DIFF                     PIC S9(13)V99 COMP-3.
       01  WS-BALANCE                  PIC S9(13)V99 COMP-3.

       01  WS-HOST.
           05 HV-DAY-DR                PIC S9(13)V99 COMP-3.
           05 HV-DAY-CR                PIC S9(13)V99 COMP-3.
           05 HV-CUM-DR                PIC S9(13)V99 COMP-3.
           05 HV-CUM-CR                PIC S9(13)V99 COMP-3.
           05 HV-LINE-COUNT            PIC S9(9) COMP.
           05 HV-AMT-1                 PIC S9(13)V99 COMP-3.
           05 HV-AMT-2                 PIC S9(13)V99 COMP-3.
           05 HV-AMT-3                 PIC S9(13)V99 COMP-3.
           05 HV-ITEM-UNB              PIC S9(13)V99 COMP-3.
           05 HV-ITEM-BIL              PIC S9(13)V99 COMP-3.
           05 HV-ITEM-DUE              PIC S9(13)V99 COMP-3.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE BILDLGL END-EXEC.
           EXEC SQL INCLUDE BILDTXN END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.

           EXEC SQL DECLARE TBCSR CURSOR WITH HOLD FOR
              SELECT T_ACCOUNT,
                     SUM(CASE WHEN BUSINESS_DATE = :WS-BUS-DATE
                               AND DR_CR = 'D'
                              THEN LINE_AMOUNT ELSE 0 END),
                     SUM(CASE WHEN BUSINESS_DATE = :WS-BUS-DATE
                               AND DR_CR = 'C'
                              THEN LINE_AMOUNT ELSE 0 END),
                     SUM(CASE WHEN DR_CR = 'D'
                              THEN LINE_AMOUNT ELSE 0 END),
                     SUM(CASE WHEN DR_CR = 'C'
                              THEN LINE_AMOUNT ELSE 0 END),
                     COUNT(*)
                FROM BILDB.BIL_LEDGER_LINE
               WHERE BUSINESS_DATE <= :WS-BUS-DATE
               GROUP BY T_ACCOUNT
               ORDER BY T_ACCOUNT
               FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-REC-002 Transactions of the day that do not balance
           EXEC SQL DECLARE UBCSR CURSOR WITH HOLD FOR
              SELECT L.TXN_ID, T.TXN_TYPE, T.ACCOUNT_ID,
                     SUM(CASE WHEN L.DR_CR = 'D'
                              THEN L.LINE_AMOUNT ELSE 0 END),
                     SUM(CASE WHEN L.DR_CR = 'C'
                              THEN L.LINE_AMOUNT ELSE 0 END)
                FROM BILDB.BIL_LEDGER_LINE L
                JOIN BILDB.BIL_TRANSACTION T ON T.TXN_ID = L.TXN_ID
               WHERE L.BUSINESS_DATE = :WS-BUS-DATE
               GROUP BY L.TXN_ID, T.TXN_TYPE, T.ACCOUNT_ID
              HAVING SUM(CASE WHEN L.DR_CR = 'D'
                              THEN L.LINE_AMOUNT ELSE 0 END)
                  <> SUM(CASE WHEN L.DR_CR = 'C'
                              THEN L.LINE_AMOUNT ELSE 0 END)
               ORDER BY L.TXN_ID
               FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-REC-002 Transactions of the day with no ledger lines
           EXEC SQL DECLARE NLCSR CURSOR WITH HOLD FOR
              SELECT T.TXN_ID, T.TXN_TYPE, T.ACCOUNT_ID, T.TXN_AMOUNT
                FROM BILDB.BIL_TRANSACTION T
               WHERE T.BUSINESS_DATE = :WS-BUS-DATE
                 AND NOT EXISTS
                     (SELECT 1 FROM BILDB.BIL_LEDGER_LINE L
                       WHERE L.TXN_ID = T.TXN_ID)
               ORDER BY T.TXN_ID
               FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-REC-007 Account balance caches against their items
           EXEC SQL DECLARE ABCSR CURSOR WITH HOLD FOR
              SELECT A.ACCOUNT_NUMBER,
                     A.UNBILLED_AMT, A.BILLED_AMT, A.DUE_AMT,
                     COALESCE(X.UNB, 0), COALESCE(X.BIL, 0),
                     COALESCE(X.DUE, 0)
                FROM BILDB.BIL_ACCOUNT A
                LEFT OUTER JOIN
                     (SELECT I.ACCOUNT_ID,
                             SUM(CASE WHEN I.ITEM_STATUS = 'PLAN'
                                 THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                      - I.WRITEOFF_AMOUNT
                                 ELSE 0 END) AS UNB,
                             SUM(CASE WHEN I.ITEM_STATUS = 'BILD'
                                 THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                      - I.WRITEOFF_AMOUNT
                                 ELSE 0 END) AS BIL,
                             SUM(CASE WHEN I.ITEM_STATUS = 'DUE '
                                 THEN I.ITEM_AMOUNT - I.PAID_AMOUNT
                                      - I.WRITEOFF_AMOUNT
                                 ELSE 0 END) AS DUE
                        FROM BILDB.BIL_INVOICE_ITEM I
                       GROUP BY I.ACCOUNT_ID) AS X
                  ON X.ACCOUNT_ID = A.ACCOUNT_ID
               WHERE A.UNBILLED_AMT <> COALESCE(X.UNB, 0)
                  OR A.BILLED_AMT   <> COALESCE(X.BIL, 0)
                  OR A.DUE_AMT      <> COALESCE(X.DUE, 0)
               ORDER BY A.ACCOUNT_NUMBER
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

      * Report lines
       01  RPT-HEAD-1.
           05 FILLER                   PIC X(10) VALUE 'BILRECON'.
           05 FILLER                   PIC X(20) VALUE SPACES.
           05 FILLER                   PIC X(46) VALUE
              'BILLINGCORE  LEDGER RECONCILIATION'.
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
       01  RPT-TB-HEAD.
           05 FILLER                   PIC X(14) VALUE
              'T-ACCOUNT  N'.
           05 FILLER                   PIC X(19) VALUE
              '          DAY DEBIT'.
           05 FILLER                   PIC X(19) VALUE
              '         DAY CREDIT'.
           05 FILLER                   PIC X(19) VALUE
              '       DAY MOVEMENT'.
           05 FILLER                   PIC X(19) VALUE
              '      TO-DATE DEBIT'.
           05 FILLER                   PIC X(19) VALUE
              '     TO-DATE CREDIT'.
           05 FILLER                   PIC X(19) VALUE
              '     BALANCE DR-CR'.
       01  RPT-TB-LINE.
           05 RTB-TACCT                PIC X(8).
           05 FILLER                   PIC X(3)  VALUE SPACES.
           05 RTB-NORMAL               PIC X.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RTB-AMT OCCURS 6 TIMES.
              10 RTB-VALUE             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  RPT-SL-HEAD.
           05 FILLER                   PIC X(42) VALUE
              'T-ACCOUNT SUB-LEDGER SOURCE'.
           05 FILLER                   PIC X(19) VALUE
              '     LEDGER BALANCE'.
           05 FILLER                   PIC X(19) VALUE
              '   SUB-LEDGER TOTAL'.
           05 FILLER                   PIC X(19) VALUE
              '         DIFFERENCE'.
       01  RPT-SL-LINE.
           05 RSL-TACCT                PIC X(10).
           05 RSL-SOURCE               PIC X(32).
           05 RSL-LEDGER               PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 RSL-SOURCE-AMT           PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 RSL-DIFF                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RSL-FLAG                 PIC X(12).
       01  RPT-EXC-LINE.
           05 REX-TYPE                 PIC X(10).
           05 REX-KEY                  PIC X(12).
           05 REX-TEXT                 PIC X(20).
           05 REX-AMT OCCURS 3 TIMES.
              10 REX-VALUE             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 REX-NOTE                 PIC X(26).
       01  RPT-MSG-LINE.
           05 FILLER                   PIC X(4)  VALUE '*** '.
           05 RMS-TEXT                 PIC X(80).
       01  RPT-CONTROL-LINE.
           05 RCL-LABEL                PIC X(40).
           05 RCL-COUNT                PIC ZZZ,ZZZ,ZZ9.
       01  WS-NUM-EDIT                 PIC Z(8)9.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-TRIAL-BALANCE
           PERFORM 2200-LEDGER-PROOF
           PERFORM 2300-UNBALANCED-TXNS
           PERFORM 2400-TXNS-WITHOUT-LINES
           PERFORM 2500-SUBLEDGER-RECON
           PERFORM 2700-ACCOUNT-CACHE-CHECK
           PERFORM 2900-RUN-SUMMARY
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE  TO WS-RUN-TS
           INITIALIZE WS-TB WS-SUBLEDGER
           MOVE 'ITEMS PLANNED (PLAN)' TO WS-SL-SOURCE-NAME(1)
           MOVE 'ITEMS BILLED (BILD)'  TO WS-SL-SOURCE-NAME(2)
           MOVE 'ITEMS DUE (DUE)'      TO WS-SL-SOURCE-NAME(3)
           MOVE 'ACCOUNT UNAPPLIED CASH' TO WS-SL-SOURCE-NAME(4)
           MOVE 'PRODUCER PAYABLE BALANCE' TO WS-SL-SOURCE-NAME(5)
           MOVE 'OPEN SUSPENSE ITEMS'  TO WS-SL-SOURCE-NAME(6)
           PERFORM 4000-READ-CONTROL
           PERFORM 6000-READ-CHECKPOINT
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILRECON RECRPT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-BUS-DATE            TO RH1-BUS-DATE
           MOVE WS-RUN-TS              TO RH2-RUN-TS.

      ******************************************************************
       2000-TRIAL-BALANCE.
           MOVE 'TRIAL BALANCE'        TO WS-SECTION
           MOVE 'TRIAL BALANCE BY T-ACCOUNT' TO RH2-SECTION
           PERFORM 7000-PRINT-HEADINGS
           MOVE RPT-TB-HEAD            TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           EXEC SQL OPEN TBCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-TRIAL-BALANCE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF
           PERFORM 4100-FETCH-TB
           PERFORM UNTIL CSR-EOF
              PERFORM 2100-POST-TB-ROW
              PERFORM 4100-FETCH-TB
           END-PERFORM
           EXEC SQL CLOSE TBCSR END-EXEC
           PERFORM VARYING WS-TB-IX FROM 1 BY 1
                   UNTIL WS-TB-IX > WS-COA-COUNT
              PERFORM 7100-PRINT-TB-LINE
           END-PERFORM
           MOVE 'TOTAL'                TO RTB-TACCT
           MOVE SPACE                  TO RTB-NORMAL
           MOVE WS-TOT-DAY-DR          TO RTB-VALUE(1)
           MOVE WS-TOT-DAY-CR          TO RTB-VALUE(2)
           COMPUTE RTB-VALUE(3) = WS-TOT-DAY-DR - WS-TOT-DAY-CR
           MOVE WS-TOT-CUM-DR          TO RTB-VALUE(4)
           MOVE WS-TOT-CUM-CR          TO RTB-VALUE(5)
           COMPUTE RTB-VALUE(6) = WS-TOT-CUM-DR - WS-TOT-CUM-CR
           MOVE SPACES                 TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE RPT-TB-LINE            TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           PERFORM 6100-COMMIT-CHECKPOINT.

      ******************************************************************
      * BR-BIL-REC-003 A T-account outside the chart is a break; its
      * amounts still count in the proof totals.
       2100-POST-TB-ROW.
           ADD 1                       TO WS-ROWS-READ
           ADD HV-DAY-DR               TO WS-TOT-DAY-DR
           ADD HV-DAY-CR               TO WS-TOT-DAY-CR
           ADD HV-CUM-DR               TO WS-TOT-CUM-DR
           ADD HV-CUM-CR               TO WS-TOT-CUM-CR
           SET COA-IX                  TO 1
           SEARCH WS-COA-ENTRY
              AT END
                 ADD 1                 TO WS-BREAKS
                 MOVE SPACES           TO RMS-TEXT
                 STRING 'T-ACCOUNT ' LGL-T-ACCOUNT
                        ' IS NOT IN THE CHART OF ACCOUNTS'
                        DELIMITED BY SIZE INTO RMS-TEXT
                 MOVE RPT-MSG-LINE     TO PRT-RECORD
                 PERFORM 7900-PRINT-LINE
              WHEN WS-COA-TACCT(COA-IX) = LGL-T-ACCOUNT
                 SET WS-TB-IX          TO COA-IX
                 MOVE HV-DAY-DR        TO WS-TB-DAY-DR(WS-TB-IX)
                 MOVE HV-DAY-CR        TO WS-TB-DAY-CR(WS-TB-IX)
                 MOVE HV-CUM-DR        TO WS-TB-CUM-DR(WS-TB-IX)
                 MOVE HV-CUM-CR        TO WS-TB-CUM-CR(WS-TB-IX)
           END-SEARCH.

      ******************************************************************
      * BR-BIL-REC-001 Ledger proves to zero for the day and to date
       2200-LEDGER-PROOF.
           MOVE 'LEDGER PROOF'         TO WS-SECTION
           MOVE SPACES                 TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE SPACES                 TO RMS-TEXT
           IF WS-TOT-DAY-DR = WS-TOT-DAY-CR
              MOVE 'DAY LEDGER BALANCES - DEBITS EQUAL CREDITS'
                                       TO RMS-TEXT
           ELSE
              ADD 1                    TO WS-BREAKS
              MOVE 'BREAK - DAY LEDGER DOES NOT BALANCE' TO RMS-TEXT
           END-IF
           MOVE RPT-MSG-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE SPACES                 TO RMS-TEXT
           IF WS-TOT-CUM-DR = WS-TOT-CUM-CR
              MOVE 'CUMULATIVE LEDGER PROVES TO ZERO' TO RMS-TEXT
           ELSE
              ADD 1                    TO WS-BREAKS
              MOVE 'BREAK - CUMULATIVE LEDGER DOES NOT PROVE TO ZERO'
                                       TO RMS-TEXT
           END-IF
           MOVE RPT-MSG-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       2300-UNBALANCED-TXNS.
           MOVE 'UNBALANCED TXNS'      TO WS-SECTION
           MOVE 'UNBALANCED TRANSACTIONS OF THE DAY' TO RH2-SECTION
           PERFORM 7000-PRINT-HEADINGS
           MOVE 0                      TO WS-LISTED
           EXEC SQL OPEN UBCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-UNBALANCED-TXNS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF
           PERFORM 4200-FETCH-UNBALANCED
           PERFORM UNTIL CSR-EOF
              ADD 1                    TO WS-BREAKS WS-LISTED
                                          WS-ROWS-SINCE-COMMIT
              IF WS-LISTED <= WS-MAX-LISTED
                 MOVE 'UNBALANCED'     TO REX-TYPE
                 MOVE LGL-TXN-ID       TO WS-NUM-EDIT
                 MOVE WS-NUM-EDIT      TO REX-KEY
                 MOVE TXN-TXN-TYPE     TO REX-TEXT
                 MOVE HV-DAY-DR        TO REX-VALUE(1)
                 MOVE HV-DAY-CR        TO REX-VALUE(2)
                 COMPUTE REX-VALUE(3) = HV-DAY-DR - HV-DAY-CR
                 MOVE TXN-ACCOUNT-ID   TO WS-NUM-EDIT
                 MOVE SPACES           TO REX-NOTE
                 STRING 'ACCOUNT ID ' WS-NUM-EDIT
                        DELIMITED BY SIZE INTO REX-NOTE
                 MOVE RPT-EXC-LINE     TO PRT-RECORD
                 PERFORM 7900-PRINT-LINE
              END-IF
              IF WS-ROWS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 6100-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4200-FETCH-UNBALANCED
           END-PERFORM
           EXEC SQL CLOSE UBCSR END-EXEC
           PERFORM 7200-PRINT-LIST-FOOT.

      ******************************************************************
       2400-TXNS-WITHOUT-LINES.
           MOVE 'TXNS NO LINES'        TO WS-SECTION
           MOVE 0                      TO WS-LISTED
           MOVE SPACES                 TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           EXEC SQL OPEN NLCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2400-TXNS-WITHOUT-LINES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF
           PERFORM 4300-FETCH-NO-LINES
           PERFORM UNTIL CSR-EOF
              ADD 1                    TO WS-BREAKS WS-LISTED
                                          WS-ROWS-SINCE-COMMIT
              IF WS-LISTED <= WS-MAX-LISTED
                 MOVE 'NO LINES'       TO REX-TYPE
                 MOVE TXN-TXN-ID       TO WS-NUM-EDIT
                 MOVE WS-NUM-EDIT      TO REX-KEY
                 MOVE TXN-TXN-TYPE     TO REX-TEXT
                 MOVE TXN-TXN-AMOUNT   TO REX-VALUE(1)
                 MOVE 0                TO REX-VALUE(2) REX-VALUE(3)
                 MOVE 'TRANSACTION NOT POSTED' TO REX-NOTE
                 MOVE RPT-EXC-LINE     TO PRT-RECORD
                 PERFORM 7900-PRINT-LINE
              END-IF
              IF WS-ROWS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 6100-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4300-FETCH-NO-LINES
           END-PERFORM
           EXEC SQL CLOSE NLCSR END-EXEC
           PERFORM 7200-PRINT-LIST-FOOT.

      ******************************************************************
      * BR-BIL-REC-004 Compare on the T-account's normal side
      * BR-BIL-REC-006 Zero tolerance
       2500-SUBLEDGER-RECON.
           MOVE 'SUBLEDGERS'           TO WS-SECTION
           MOVE 'SUB-LEDGER TO CONTROL ACCOUNT RECONCILIATION'
                                       TO RH2-SECTION
           PERFORM 7000-PRINT-HEADINGS
           MOVE RPT-SL-HEAD            TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           PERFORM 4400-ITEM-SUBLEDGERS
           PERFORM 4500-CASH-SUBLEDGERS
           PERFORM VARYING WS-TB-IX FROM 1 BY 1
                   UNTIL WS-TB-IX > WS-COA-COUNT
              IF WS-COA-SUBLDG(WS-TB-IX) > 0
                 MOVE WS-COA-SUBLDG(WS-TB-IX) TO WS-SL
                 PERFORM 5000-NORMAL-BALANCE
                 MOVE WS-BALANCE       TO WS-SL-LEDGER-AMT(WS-SL)
                 PERFORM 2600-PRINT-SUBLEDGER
              END-IF
           END-PERFORM
           PERFORM 6100-COMMIT-CHECKPOINT.

      ******************************************************************
       2600-PRINT-SUBLEDGER.
           COMPUTE WS-DIFF = WS-SL-LEDGER-AMT(WS-SL)
                           - WS-SL-SOURCE-AMT(WS-SL)
           MOVE WS-COA-TACCT(WS-TB-IX) TO RSL-TACCT
           MOVE WS-SL-SOURCE-NAME(WS-SL) TO RSL-SOURCE
           MOVE WS-SL-LEDGER-AMT(WS-SL) TO RSL-LEDGER
           MOVE WS-SL-SOURCE-AMT(WS-SL) TO RSL-SOURCE-AMT
           MOVE WS-DIFF                TO RSL-DIFF
           IF WS-DIFF = 0
              MOVE 'AGREED'            TO RSL-FLAG
           ELSE
              MOVE '*** BREAK'         TO RSL-FLAG
              ADD 1                    TO WS-BREAKS
           END-IF
           MOVE RPT-SL-LINE            TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       2700-ACCOUNT-CACHE-CHECK.
           MOVE 'ACCOUNT CACHES'       TO WS-SECTION
           MOVE 'ACCOUNT BALANCE EXCEPTIONS (WARNINGS)'
                                       TO RH2-SECTION
           PERFORM 7000-PRINT-HEADINGS
           MOVE 0                      TO WS-LISTED
           EXEC SQL OPEN ABCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2700-ACCOUNT-CACHE-CHECK' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-EOF
           PERFORM 4600-FETCH-ACCOUNT-CACHE
           PERFORM UNTIL CSR-EOF
              ADD 1                    TO WS-WARNINGS WS-LISTED
                                          WS-ROWS-SINCE-COMMIT
              IF WS-LISTED <= WS-MAX-LISTED
                 PERFORM 2750-PRINT-ACCOUNT-CACHE
              END-IF
              IF WS-ROWS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 6100-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4600-FETCH-ACCOUNT-CACHE
           END-PERFORM
           EXEC SQL CLOSE ABCSR END-EXEC
           PERFORM 7200-PRINT-LIST-FOOT.

      ******************************************************************
       2750-PRINT-ACCOUNT-CACHE.
           MOVE 'ACCOUNT'              TO REX-TYPE
           MOVE ACT-ACCOUNT-NUMBER     TO REX-KEY
           MOVE 'UNBILLED/BILLED/DUE'  TO REX-TEXT
           COMPUTE REX-VALUE(1) = ACT-UNBILLED-AMT - HV-ITEM-UNB
           COMPUTE REX-VALUE(2) = ACT-BILLED-AMT   - HV-ITEM-BIL
           COMPUTE REX-VALUE(3) = ACT-DUE-AMT      - HV-ITEM-DUE
           MOVE 'CACHE MINUS ITEMS'    TO REX-NOTE
           MOVE RPT-EXC-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       2900-RUN-SUMMARY.
           MOVE 'RUN SUMMARY'          TO RH2-SECTION
           PERFORM 7000-PRINT-HEADINGS
           MOVE 'T-ACCOUNTS WITH LEDGER LINES' TO RCL-LABEL
           MOVE WS-ROWS-READ           TO RCL-COUNT
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE 'RECONCILIATION BREAKS' TO RCL-LABEL
           MOVE WS-BREAKS              TO RCL-COUNT
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE 'ACCOUNT CACHE WARNINGS' TO RCL-LABEL
           MOVE WS-WARNINGS            TO RCL-COUNT
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE
           MOVE SPACES                 TO RMS-TEXT
           EVALUATE TRUE
              WHEN WS-BREAKS > 0
                 MOVE 8                TO WS-RC
                 MOVE 'LEDGER NOT RECONCILED - INVESTIGATE BREAKS'
                                       TO RMS-TEXT
              WHEN WS-WARNINGS > 0
                 MOVE 4                TO WS-RC
                 MOVE 'LEDGER RECONCILED - ACCOUNT CACHES NEED ACCTBAL'
                                       TO RMS-TEXT
              WHEN OTHER
                 MOVE 'LEDGER RECONCILED - NO EXCEPTIONS' TO RMS-TEXT
           END-EVALUATE
           MOVE RPT-MSG-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT CHAR(BUSINESS_DATE, ISO)
                INTO :CTL-BUSINESS-DATE
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
       4100-FETCH-TB.
           EXEC SQL
              FETCH TBCSR
               INTO :LGL-T-ACCOUNT, :HV-DAY-DR, :HV-DAY-CR,
                    :HV-CUM-DR, :HV-CUM-CR, :HV-LINE-COUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CSR-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-TB'  TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-FETCH-UNBALANCED.
           EXEC SQL
              FETCH UBCSR
               INTO :LGL-TXN-ID, :TXN-TXN-TYPE, :TXN-ACCOUNT-ID,
                    :HV-DAY-DR, :HV-DAY-CR
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CSR-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4200-FETCH-UNBALANCED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-FETCH-NO-LINES.
           EXEC SQL
              FETCH NLCSR
               INTO :TXN-TXN-ID, :TXN-TXN-TYPE, :TXN-ACCOUNT-ID,
                    :TXN-TXN-AMOUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CSR-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4300-FETCH-NO-LINES' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-REC-005 Item sub-ledgers by item status
       4400-ITEM-SUBLEDGERS.
           EXEC SQL
              SELECT COALESCE(SUM(CASE WHEN ITEM_STATUS = 'PLAN'
                         THEN ITEM_AMOUNT - PAID_AMOUNT
                              - WRITEOFF_AMOUNT ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN ITEM_STATUS = 'BILD'
                         THEN ITEM_AMOUNT - PAID_AMOUNT
                              - WRITEOFF_AMOUNT ELSE 0 END), 0),
                     COALESCE(SUM(CASE WHEN ITEM_STATUS = 'DUE '
                         THEN ITEM_AMOUNT - PAID_AMOUNT
                              - WRITEOFF_AMOUNT ELSE 0 END), 0)
                INTO :HV-AMT-1, :HV-AMT-2, :HV-AMT-3
                FROM BILDB.BIL_INVOICE_ITEM
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-ITEM-SUBLEDGERS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE HV-AMT-1               TO WS-SL-SOURCE-AMT(1)
           MOVE HV-AMT-2               TO WS-SL-SOURCE-AMT(2)
           MOVE HV-AMT-3               TO WS-SL-SOURCE-AMT(3).

      ******************************************************************
       4500-CASH-SUBLEDGERS.
           EXEC SQL
              SELECT COALESCE(SUM(UNAPPLIED_AMT), 0)
                INTO :HV-AMT-1
                FROM BILDB.BIL_ACCOUNT
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-CASH-SUBLEDGERS' TO WS-ERR-PARA
              MOVE 'BIL_ACCOUNT'       TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              SELECT COALESCE(SUM(PAYABLE_BALANCE), 0)
                INTO :HV-AMT-2
                FROM BILDB.BIL_PRODUCER
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-CASH-SUBLEDGERS' TO WS-ERR-PARA
              MOVE 'BIL_PRODUCER'      TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              SELECT COALESCE(SUM(SUSP_AMOUNT), 0)
                INTO :HV-AMT-3
                FROM BILDB.BIL_SUSPENSE
               WHERE SUSP_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-CASH-SUBLEDGERS' TO WS-ERR-PARA
              MOVE 'BIL_SUSPENSE'      TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE HV-AMT-1               TO WS-SL-SOURCE-AMT(4)
           MOVE HV-AMT-2               TO WS-SL-SOURCE-AMT(5)
           MOVE HV-AMT-3               TO WS-SL-SOURCE-AMT(6).

      ******************************************************************
       4600-FETCH-ACCOUNT-CACHE.
           EXEC SQL
              FETCH ABCSR
               INTO :ACT-ACCOUNT-NUMBER, :ACT-UNBILLED-AMT,
                    :ACT-BILLED-AMT, :ACT-DUE-AMT,
                    :HV-ITEM-UNB, :HV-ITEM-BIL, :HV-ITEM-DUE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CSR-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4600-FETCH-ACCOUNT-CACHE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-REC-004 Balance on the normal side of WS-TB-IX
       5000-NORMAL-BALANCE.
           IF WS-COA-NORMAL(WS-TB-IX) = 'D'
              COMPUTE WS-BALANCE = WS-TB-CUM-DR(WS-TB-IX)
                                 - WS-TB-CUM-CR(WS-TB-IX)
           ELSE
              COMPUTE WS-BALANCE = WS-TB-CUM-CR(WS-TB-IX)
                                 - WS-TB-CUM-DR(WS-TB-IX)
           END-IF.

      ******************************************************************
      * BR-BIL-REC-008 Prior incomplete run is noted; rerun in full
       6000-READ-CHECKPOINT.
           OPEN INPUT CHKPT-FILE
           IF WS-CHK-STATUS = '00'
              READ CHKPT-FILE
              IF WS-CHK-STATUS = '00'
                 AND CHK-PROGRAM-ID = WS-PROGRAM-ID
                 AND CHK-BUS-DATE = WS-BUS-DATE
                 AND NOT CHK-COMPLETE
                 DISPLAY 'BILRECON RESTART - PRIOR RUN STOPPED IN '
                         CHK-RESTART-KEY ' - RECONCILIATION RERUN'
              END-IF
              CLOSE CHKPT-FILE
           END-IF
           MOVE 'START'                TO WS-SECTION
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
           PERFORM 6200-WRITE-CHECKPOINT.

      ******************************************************************
       6200-WRITE-CHECKPOINT.
           MOVE WS-PROGRAM-ID          TO CHK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CHK-BUS-DATE
           MOVE WS-SECTION             TO CHK-RESTART-KEY
           MOVE WS-ROWS-READ           TO CHK-ROWS-DONE
           MOVE FUNCTION CURRENT-DATE  TO CHK-TIMESTAMP
           OPEN OUTPUT CHKPT-FILE
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'BILRECON CHKPT OPEN FAILED ' WS-CHK-STATUS
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
           MOVE SPACES                 TO PRT-RECORD
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE
           MOVE 3                      TO WS-LINE-CNT.

      ******************************************************************
       7100-PRINT-TB-LINE.
           MOVE WS-COA-TACCT(WS-TB-IX) TO RTB-TACCT
           MOVE WS-COA-NORMAL(WS-TB-IX) TO RTB-NORMAL
           MOVE WS-TB-DAY-DR(WS-TB-IX) TO RTB-VALUE(1)
           MOVE WS-TB-DAY-CR(WS-TB-IX) TO RTB-VALUE(2)
           COMPUTE RTB-VALUE(3) = WS-TB-DAY-DR(WS-TB-IX)
                                - WS-TB-DAY-CR(WS-TB-IX)
           MOVE WS-TB-CUM-DR(WS-TB-IX) TO RTB-VALUE(4)
           MOVE WS-TB-CUM-CR(WS-TB-IX) TO RTB-VALUE(5)
           COMPUTE RTB-VALUE(6) = WS-TB-CUM-DR(WS-TB-IX)
                                - WS-TB-CUM-CR(WS-TB-IX)
           MOVE RPT-TB-LINE            TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       7200-PRINT-LIST-FOOT.
           MOVE SPACES                 TO RMS-TEXT
           MOVE WS-LISTED              TO WS-NUM-EDIT
           EVALUATE TRUE
              WHEN WS-LISTED = 0
                 STRING WS-SECTION ': NONE'
                        DELIMITED BY '  ' INTO RMS-TEXT
              WHEN WS-LISTED > WS-MAX-LISTED
                 STRING WS-SECTION ': ' WS-NUM-EDIT
                        ' FOUND, FIRST 50 LISTED'
                        DELIMITED BY SIZE INTO RMS-TEXT
              WHEN OTHER
                 STRING WS-SECTION ': ' WS-NUM-EDIT ' FOUND'
                        DELIMITED BY SIZE INTO RMS-TEXT
           END-EVALUATE
           MOVE RPT-MSG-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

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
       9000-RETURN.
           IF WS-RC < 16
              EXEC SQL COMMIT END-EXEC
              MOVE 'END'               TO WS-SECTION
              SET CHK-COMPLETE         TO TRUE
              PERFORM 6200-WRITE-CHECKPOINT
           END-IF
           CLOSE REPORT-FILE
           DISPLAY 'BILRECON ENDED RC ' WS-RC ' BREAKS ' WS-BREAKS
                   ' WARNINGS ' WS-WARNINGS
           MOVE WS-RC                  TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILRECON DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID
                                          ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'BILRECON Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' section ' WS-SECTION
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL ROLLBACK END-EXEC
           SET CHK-FAILED              TO TRUE
           PERFORM 6200-WRITE-CHECKPOINT
           MOVE 16                     TO WS-RC
           PERFORM 9000-RETURN.
