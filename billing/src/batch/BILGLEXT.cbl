       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILGLEXT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILGLEXT                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : General-ledger extract for finance. Summarises   *
      *               the business date's ledger lines by T-account    *
      *               and product into the fixed GL interface file     *
      *               (header, one detail per GL account / cost        *
      *               centre, trailer with record count and DR, CR     *
      *               and account-number hash totals) and prints a     *
      *               control report. Read-only: nothing is marked as  *
      *               extracted; the file is rebuilt on every run.     *
      * REQUESTS    : n/a (batch)                                      *
      * CALLED BY   : JCL BILEOD step GLEXT                            *
      * CALLS       : BILERRLG                                         *
      * TABLES      : BIL_CONTROL (R) BIL_LEDGER_LINE (R)              *
      *               BIL_TRANSACTION (R) BIL_DISTRIBUTION (R)         *
      *               BIL_WRITEOFF (R) BIL_INVOICE_ITEM (R)            *
      *               BIL_CHARGE (R) BIL_COMMISSION_ITEM (R)           *
      *               BIL_POLICY_PERIOD (R)                            *
      * FILES       : GLEXT   GL interface FB 120 (O)                  *
      *               GLXRPT  report 132          (O)                  *
      *               CHKPT   checkpoint          (I/O)                *
      * RULES       : BR-BIL-GLX-001 Extract only while the day is     *
      *                              closed to online (BATCH_STATUS B) *
      *               BR-BIL-GLX-002 Product derived from the posting  *
      *                              reference (item, distribution,    *
      *                              write-off, charge, commission     *
      *                              item, period); none is corporate  *
      *                              cost centre CORP                  *
      *               BR-BIL-GLX-003 T-account maps to a 6-digit GL    *
      *                              account; unmapped lines go to GL  *
      *                              suspense 199999 with RC 4         *
      *               BR-BIL-GLX-004 Gross DR and CR are extracted;    *
      *                              net carries its own sign          *
      *               BR-BIL-GLX-005 Extract must balance (DR = CR)    *
      *                              or the step ends RC 8             *
      *               BR-BIL-GLX-006 Trailer hash = sum of detail GL   *
      *                              account numbers                   *
      *               BR-BIL-GLX-007 A day with no ledger lines still  *
      *                              sends header and trailer (RC 4)   *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE      ASSIGN TO GLEXT
                  FILE STATUS IS WS-GL-STATUS.
           SELECT REPORT-FILE  ASSIGN TO GLXRPT
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT CHKPT-FILE   ASSIGN TO CHKPT
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE
           RECORDING MODE IS F.
           COPY BILGLEG1.
       FD  REPORT-FILE
           RECORDING MODE IS F.
           COPY BILEODP1.
       FD  CHKPT-FILE
           RECORDING MODE IS F.
           COPY BILEODK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILGLEXT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-COMMIT-FREQ              PIC S9(4) COMP VALUE 200.
       01  WS-COMPANY-CODE             PIC X(4)  VALUE 'GB01'.
       01  WS-SUSPENSE-GL              PIC 9(6)  VALUE 199999.

       01  WS-FILE-STATUS.
           05 WS-GL-STATUS             PIC XX.
           05 WS-RPT-STATUS            PIC XX.
           05 WS-CHK-STATUS            PIC XX.

       01  WS-FLAGS.
           05 WS-EOF                   PIC X     VALUE 'N'.
              88 CSR-EOF                VALUE 'Y'.

       01  WS-RUN.
           05 WS-RC                    PIC S9(4) COMP VALUE 0.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-RUN-TS                PIC X(21).
           05 WS-DB2-TS                PIC X(26).
           05 WS-BATCH-REF             PIC X(12).
           05 WS-ROWS-READ             PIC S9(9) COMP VALUE 0.
           05 WS-ROWS-SINCE-COMMIT     PIC S9(9) COMP VALUE 0.
           05 WS-UNMAPPED              PIC S9(9) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-CNT              PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ERR-SQLCODE           PIC S9(9) COMP VALUE 0.
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-DTL-COUNT             PIC S9(9) COMP VALUE 0.
           05 WS-REC-COUNT             PIC S9(9) COMP VALUE 0.
           05 WS-LINE-TOTAL            PIC S9(9) COMP VALUE 0.
           05 WS-DR-TOTAL              PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-CR-TOTAL              PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-ACCT-HASH             PIC S9(15) COMP-3 VALUE 0.
           05 WS-NET                   PIC S9(13)V99 COMP-3.

      * BR-BIL-GLX-003 T-account to GL account (and natural side)
       01  WS-GLMAP-VALUES.
           05 FILLER PIC X(14) VALUE 'CASH    110100'.
           05 FILLER PIC X(14) VALUE 'UNBILLED120100'.
           05 FILLER PIC X(14) VALUE 'BILLED  120200'.
           05 FILLER PIC X(14) VALUE 'DUE     120300'.
           05 FILLER PIC X(14) VALUE 'UNAPPLD 210300'.
           05 FILLER PIC X(14) VALUE 'SUSPENSE210900'.
           05 FILLER PIC X(14) VALUE 'COMMRSV 220100'.
           05 FILLER PIC X(14) VALUE 'COMMPAY 220200'.
           05 FILLER PIC X(14) VALUE 'DISBPAY 230100'.
           05 FILLER PIC X(14) VALUE 'CLMPAY  230200'.
           05 FILLER PIC X(14) VALUE 'PREMREV 410100'.
           05 FILLER PIC X(14) VALUE 'FEEREV  420100'.
           05 FILLER PIC X(14) VALUE 'WRITEOFF610100'.
       01  WS-GLMAP REDEFINES WS-GLMAP-VALUES.
           05 WS-GLMAP-ENTRY OCCURS 13 TIMES INDEXED BY GLM-IX.
              10 WS-GLMAP-TACCT        PIC X(8).
              10 WS-GLMAP-GL           PIC 9(6).

      * BR-BIL-GLX-002 Product to cost centre
       01  WS-CC-VALUES.
           05 FILLER PIC X(5) VALUE 'MMOTR'.
           05 FILLER PIC X(5) VALUE 'HHOME'.
           05 FILLER PIC X(5) VALUE 'CCOMM'.
           05 FILLER PIC X(5) VALUE 'EENDW'.
       01  WS-CC REDEFINES WS-CC-VALUES.
           05 WS-CC-ENTRY OCCURS 4 TIMES INDEXED BY CC-IX.
              10 WS-CC-PRODUCT         PIC X.
              10 WS-CC-CENTRE          PIC X(4).

       01  WS-HOST.
           05 HV-PRODUCT-CODE          PIC X.
           05 HV-DR-AMOUNT             PIC S9(13)V99 COMP-3.
           05 HV-CR-AMOUNT             PIC S9(13)V99 COMP-3.
           05 HV-LINE-COUNT            PIC S9(9) COMP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE BILDLGL END-EXEC.

      * BR-BIL-GLX-002 Product from the transaction's reference:
      * ITEM / DIST / WOFF reach the item's period, CHRG the charge's
      * period, CMI the commission item's period, PPER the period
      * itself; RCPT, INV, DISB and the rest are corporate.
           EXEC SQL DECLARE GLCSR CURSOR WITH HOLD FOR
              SELECT X.T_ACCOUNT, X.PRODUCT_CODE,
                     SUM(CASE WHEN X.DR_CR = 'D'
                              THEN X.LINE_AMOUNT ELSE 0 END),
                     SUM(CASE WHEN X.DR_CR = 'C'
                              THEN X.LINE_AMOUNT ELSE 0 END),
                     COUNT(*)
                FROM (SELECT L.T_ACCOUNT, L.DR_CR, L.LINE_AMOUNT,
                             COALESCE(P.PRODUCT_CODE, '*')
                                AS PRODUCT_CODE
                        FROM BILDB.BIL_LEDGER_LINE L
                        JOIN BILDB.BIL_TRANSACTION T
                          ON T.TXN_ID = L.TXN_ID
                        LEFT OUTER JOIN BILDB.BIL_DISTRIBUTION D
                          ON T.REF_TYPE = 'DIST'
                         AND D.DIST_ID  = T.REF_ID
                        LEFT OUTER JOIN BILDB.BIL_WRITEOFF W
                          ON T.REF_TYPE = 'WOFF'
                         AND W.WO_ID    = T.REF_ID
                        LEFT OUTER JOIN BILDB.BIL_INVOICE_ITEM I
                          ON I.ITEM_ID  = CASE T.REF_TYPE
                                            WHEN 'ITEM' THEN T.REF_ID
                                            WHEN 'DIST' THEN D.ITEM_ID
                                            WHEN 'WOFF' THEN W.ITEM_ID
                                          END
                        LEFT OUTER JOIN BILDB.BIL_CHARGE C
                          ON T.REF_TYPE  = 'CHRG'
                         AND C.CHARGE_ID = T.REF_ID
                        LEFT OUTER JOIN BILDB.BIL_COMMISSION_ITEM M
                          ON T.REF_TYPE  = 'CMI '
                         AND M.CI_ID     = T.REF_ID
                        LEFT OUTER JOIN BILDB.BIL_POLICY_PERIOD P
                          ON P.PP_ID = COALESCE(I.PP_ID, C.PP_ID,
                                M.PP_ID,
                                CASE WHEN T.REF_TYPE = 'PPER'
                                     THEN T.REF_ID END)
                       WHERE L.BUSINESS_DATE = :WS-BUS-DATE) AS X
               GROUP BY X.T_ACCOUNT, X.PRODUCT_CODE
               ORDER BY X.T_ACCOUNT, X.PRODUCT_CODE
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       01  RPT-HEAD-1.
           05 FILLER                   PIC X(10) VALUE 'BILGLEXT'.
           05 FILLER                   PIC X(20) VALUE SPACES.
           05 FILLER                   PIC X(46) VALUE
              'BILLINGCORE  GENERAL LEDGER EXTRACT'.
           05 FILLER                   PIC X(15) VALUE
              'BUSINESS DATE '.
           05 RH1-BUS-DATE             PIC X(10).
           05 FILLER                   PIC X(21) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 RH1-PAGE                 PIC ZZZ9.
       01  RPT-HEAD-2.
           05 FILLER                   PIC X(10) VALUE 'BATCH REF'.
           05 RH2-BATCH-REF            PIC X(12).
           05 FILLER                   PIC X(8)  VALUE SPACES.
           05 FILLER                   PIC X(10) VALUE 'RUN'.
           05 RH2-RUN-TS               PIC X(21).
       01  RPT-HEAD-3.
           05 FILLER                   PIC X(30) VALUE
              'GL ACCT  CC    T-ACCOUNT  PROD'.
           05 FILLER                   PIC X(19) VALUE
              '              DEBIT'.
           05 FILLER                   PIC X(19) VALUE
              '             CREDIT'.
           05 FILLER                   PIC X(19) VALUE
              '                NET'.
           05 FILLER                   PIC X(10) VALUE
              '     LINES'.
       01  RPT-DETAIL.
           05 RD-GL                    PIC 9(6).
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 RD-CC                    PIC X(4).
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RD-TACCT                 PIC X(8).
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 RD-PRODUCT               PIC X.
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 RD-DR                    PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 RD-CR                    PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 RD-NET                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 RD-LINES                 PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 RD-NOTE                  PIC X(20).
       01  RPT-CONTROL-LINE.
           05 RCL-LABEL                PIC X(40).
           05 RCL-VALUE                PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  RPT-MSG-LINE.
           05 FILLER                   PIC X(4)  VALUE '*** '.
           05 RMS-TEXT                 PIC X(80).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EXTRACT
           PERFORM 2500-WRITE-TRAILER
           PERFORM 2800-RUN-CONTROLS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           MOVE FUNCTION CURRENT-DATE  TO WS-RUN-TS
           PERFORM 4000-READ-CONTROL
           PERFORM 3000-VALIDATE-WINDOW
           PERFORM 6000-READ-CHECKPOINT
           OPEN OUTPUT GL-FILE
           IF WS-GL-STATUS NOT = '00'
              DISPLAY 'BILGLEXT GLEXT OPEN FAILED ' WS-GL-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILGLEXT GLXRPT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO WS-BATCH-REF
           STRING 'BILGL' WS-BUS-DATE(1:4) WS-BUS-DATE(6:2)
                  WS-BUS-DATE(9:2) DELIMITED BY SIZE
                  INTO WS-BATCH-REF
           MOVE WS-BUS-DATE            TO RH1-BUS-DATE
           MOVE WS-BATCH-REF           TO RH2-BATCH-REF
           MOVE WS-RUN-TS              TO RH2-RUN-TS
           PERFORM 7000-PRINT-HEADINGS
           PERFORM 2050-WRITE-HEADER.

      ******************************************************************
       2000-EXTRACT.
           EXEC SQL OPEN GLCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-EXTRACT'      TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-GL
           PERFORM UNTIL CSR-EOF
              PERFORM 2100-BUILD-DETAIL
              PERFORM 4100-FETCH-GL
           END-PERFORM
           EXEC SQL CLOSE GLCSR END-EXEC.

      ******************************************************************
       2050-WRITE-HEADER.
           MOVE SPACES                 TO GLI-RECORD
           SET GLI-HEADER              TO TRUE
           MOVE 'BILLING '             TO GLI-H-SOURCE
           MOVE WS-COMPANY-CODE        TO GLI-H-COMPANY
           MOVE WS-BUS-DATE            TO GLI-H-BUS-DATE
           MOVE CTL-CURRENCY-CODE      TO GLI-H-CURRENCY
           MOVE WS-DB2-TS              TO GLI-H-CREATED-TS
           MOVE WS-BATCH-REF           TO GLI-H-BATCH-REF
           PERFORM 7300-WRITE-GL.

      ******************************************************************
      * BR-BIL-GLX-004 Gross amounts out, net with explicit sign
       2100-BUILD-DETAIL.
           ADD 1                       TO WS-ROWS-READ
                                          WS-ROWS-SINCE-COMMIT
           MOVE SPACES                 TO GLI-RECORD
                                          RD-NOTE
           SET GLI-DETAIL              TO TRUE
           MOVE WS-COMPANY-CODE        TO GLI-D-COMPANY
           PERFORM 5000-MAP-GL-ACCOUNT
           PERFORM 5100-MAP-COST-CENTRE
           MOVE LGL-T-ACCOUNT          TO GLI-D-T-ACCOUNT
           MOVE HV-PRODUCT-CODE        TO GLI-D-PRODUCT
           MOVE HV-DR-AMOUNT           TO GLI-D-DR-AMOUNT
           MOVE HV-CR-AMOUNT           TO GLI-D-CR-AMOUNT
           COMPUTE WS-NET = HV-DR-AMOUNT - HV-CR-AMOUNT
           MOVE WS-NET                 TO GLI-D-NET-AMOUNT
           IF WS-NET < 0
              MOVE '-'                 TO GLI-D-NET-SIGN
           ELSE
              MOVE '+'                 TO GLI-D-NET-SIGN
           END-IF
           MOVE HV-LINE-COUNT          TO GLI-D-LINE-COUNT
           MOVE WS-BUS-DATE            TO GLI-D-BUS-DATE
           PERFORM 7300-WRITE-GL
           ADD 1                       TO WS-DTL-COUNT
           ADD HV-DR-AMOUNT            TO WS-DR-TOTAL
           ADD HV-CR-AMOUNT            TO WS-CR-TOTAL
           ADD HV-LINE-COUNT           TO WS-LINE-TOTAL
      * BR-BIL-GLX-006 Hash total of GL account numbers
           ADD GLI-D-GL-ACCOUNT        TO WS-ACCT-HASH
           PERFORM 7100-PRINT-DETAIL
           IF WS-ROWS-SINCE-COMMIT >= WS-COMMIT-FREQ
              PERFORM 6100-COMMIT-CHECKPOINT
           END-IF.

      ******************************************************************
       2500-WRITE-TRAILER.
           MOVE SPACES                 TO GLI-RECORD
           SET GLI-TRAILER             TO TRUE
           MOVE WS-DTL-COUNT           TO GLI-T-RECORD-COUNT
           MOVE WS-DR-TOTAL            TO GLI-T-DR-TOTAL
           MOVE WS-CR-TOTAL            TO GLI-T-CR-TOTAL
           MOVE WS-ACCT-HASH           TO GLI-T-ACCOUNT-HASH
           MOVE WS-LINE-TOTAL          TO GLI-T-LINE-COUNT
           PERFORM 7300-WRITE-GL.

      ******************************************************************
       2800-RUN-CONTROLS.
           PERFORM 7000-PRINT-HEADINGS
           MOVE 'GL INTERFACE RECORDS WRITTEN (INCL H/T)' TO RCL-LABEL
           MOVE WS-REC-COUNT           TO RCL-VALUE
           PERFORM 7200-PRINT-CONTROL
           MOVE 'DETAIL RECORDS'       TO RCL-LABEL
           MOVE WS-DTL-COUNT           TO RCL-VALUE
           PERFORM 7200-PRINT-CONTROL
           MOVE 'LEDGER LINES SUMMARISED' TO RCL-LABEL
           MOVE WS-LINE-TOTAL          TO RCL-VALUE
           PERFORM 7200-PRINT-CONTROL
           MOVE 'TOTAL DEBITS'         TO RCL-LABEL
           MOVE WS-DR-TOTAL            TO RCL-VALUE
           PERFORM 7200-PRINT-CONTROL
           MOVE 'TOTAL CREDITS'        TO RCL-LABEL
           MOVE WS-CR-TOTAL            TO RCL-VALUE
           PERFORM 7200-PRINT-CONTROL
           MOVE 'GL ACCOUNT HASH TOTAL' TO RCL-LABEL
           MOVE WS-ACCT-HASH           TO RCL-VALUE
           PERFORM 7200-PRINT-CONTROL
           MOVE SPACES                 TO RMS-TEXT
      * BR-BIL-GLX-005 Extract must balance
      * BR-BIL-GLX-007 Empty day still delivers header and trailer
           EVALUATE TRUE
              WHEN WS-DR-TOTAL NOT = WS-CR-TOTAL
                 MOVE 8                TO WS-RC
                 MOVE 'EXTRACT OUT OF BALANCE - FINANCE MUST NOT LOAD'
                                       TO RMS-TEXT
              WHEN WS-DTL-COUNT = 0
                 MOVE 4                TO WS-RC
                 MOVE 'NO LEDGER LINES FOR THE BUSINESS DATE'
                                       TO RMS-TEXT
              WHEN WS-UNMAPPED > 0
                 MOVE 4                TO WS-RC
                 MOVE 'UNMAPPED T-ACCOUNTS POSTED TO GL SUSPENSE 199999'
                                       TO RMS-TEXT
              WHEN OTHER
                 MOVE 'EXTRACT BALANCED' TO RMS-TEXT
           END-EVALUATE
           MOVE RPT-MSG-LINE           TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
      * BR-BIL-GLX-001 Day must be closed to online posting
       3000-VALIDATE-WINDOW.
           IF CTL-BATCH-STATUS NOT = 'B'
              DISPLAY 'BILGLEXT BATCH_STATUS IS ' CTL-BATCH-STATUS
                      ' - EXTRACT REQUIRES END-OF-DAY STATUS B'
              MOVE 12                  TO WS-RC
              MOVE 'BILGLEXT BATCH_STATUS not B - extract refused'
                                       TO ERC-ER-TEXT
              MOVE '3000-VALIDATE-WINDOW' TO WS-ERR-PARA
              MOVE CTL-CONTROL-KEY     TO WS-ERR-KEY
              PERFORM 8000-LOG-ERROR
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT CHAR(BUSINESS_DATE, ISO), BATCH_STATUS,
                     CURRENCY_CODE, CHAR(CURRENT TIMESTAMP)
                INTO :CTL-BUSINESS-DATE, :CTL-BATCH-STATUS,
                     :CTL-CURRENCY-CODE, :WS-DB2-TS
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
       4100-FETCH-GL.
           EXEC SQL
              FETCH GLCSR
               INTO :LGL-T-ACCOUNT, :HV-PRODUCT-CODE,
                    :HV-DR-AMOUNT, :HV-CR-AMOUNT, :HV-LINE-COUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CSR-EOF           TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-GL'  TO WS-ERR-PARA
                 MOVE LGL-T-ACCOUNT    TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-GLX-003 Unmapped T-account goes to GL suspense
       5000-MAP-GL-ACCOUNT.
           SET GLM-IX                  TO 1
           SEARCH WS-GLMAP-ENTRY
              AT END
                 MOVE WS-SUSPENSE-GL   TO GLI-D-GL-ACCOUNT
                 MOVE 'UNMAPPED T-ACCOUNT' TO RD-NOTE
                 ADD 1                 TO WS-UNMAPPED
              WHEN WS-GLMAP-TACCT(GLM-IX) = LGL-T-ACCOUNT
                 MOVE WS-GLMAP-GL(GLM-IX) TO GLI-D-GL-ACCOUNT
           END-SEARCH.

      ******************************************************************
      * BR-BIL-GLX-002 No product reference -> corporate cost centre
       5100-MAP-COST-CENTRE.
           SET CC-IX                   TO 1
           SEARCH WS-CC-ENTRY
              AT END
                 MOVE 'CORP'           TO GLI-D-COST-CENTRE
              WHEN WS-CC-PRODUCT(CC-IX) = HV-PRODUCT-CODE
                 MOVE WS-CC-CENTRE(CC-IX) TO GLI-D-COST-CENTRE
           END-SEARCH.

      ******************************************************************
       6000-READ-CHECKPOINT.
           OPEN INPUT CHKPT-FILE
           IF WS-CHK-STATUS = '00'
              READ CHKPT-FILE
              IF WS-CHK-STATUS = '00'
                 AND CHK-PROGRAM-ID = WS-PROGRAM-ID
                 AND CHK-BUS-DATE = WS-BUS-DATE
                 DISPLAY 'BILGLEXT PRIOR RUN FOR ' WS-BUS-DATE
                         ' STATUS ' CHK-RUN-STATUS
                         ' - GL FILE REBUILT IN FULL'
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
           MOVE SPACES                 TO CHK-RESTART-KEY
           STRING LGL-T-ACCOUNT HV-PRODUCT-CODE
                  DELIMITED BY SIZE INTO CHK-RESTART-KEY
           PERFORM 6200-WRITE-CHECKPOINT.

      ******************************************************************
       6200-WRITE-CHECKPOINT.
           MOVE WS-PROGRAM-ID          TO CHK-PROGRAM-ID
           MOVE WS-BUS-DATE            TO CHK-BUS-DATE
           MOVE WS-ROWS-READ           TO CHK-ROWS-DONE
           MOVE FUNCTION CURRENT-DATE  TO CHK-TIMESTAMP
           OPEN OUTPUT CHKPT-FILE
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'BILGLEXT CHKPT OPEN FAILED ' WS-CHK-STATUS
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
           WRITE PRT-RECORD FROM RPT-HEAD-3 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO PRT-RECORD
           WRITE PRT-RECORD AFTER ADVANCING 1 LINE
           MOVE 5                      TO WS-LINE-CNT.

      ******************************************************************
       7100-PRINT-DETAIL.
           MOVE GLI-D-GL-ACCOUNT       TO RD-GL
           MOVE GLI-D-COST-CENTRE      TO RD-CC
           MOVE LGL-T-ACCOUNT          TO RD-TACCT
           MOVE HV-PRODUCT-CODE        TO RD-PRODUCT
           MOVE HV-DR-AMOUNT           TO RD-DR
           MOVE HV-CR-AMOUNT           TO RD-CR
           MOVE WS-NET                 TO RD-NET
           MOVE HV-LINE-COUNT          TO RD-LINES
           MOVE RPT-DETAIL             TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       7200-PRINT-CONTROL.
           MOVE RPT-CONTROL-LINE       TO PRT-RECORD
           PERFORM 7900-PRINT-LINE.

      ******************************************************************
       7300-WRITE-GL.
           WRITE GLI-RECORD
           IF WS-GL-STATUS NOT = '00'
              DISPLAY 'BILGLEXT GLEXT WRITE FAILED ' WS-GL-STATUS
              MOVE 16                  TO WS-RC
              PERFORM 9000-RETURN
           END-IF
           ADD 1                       TO WS-REC-COUNT.

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
       8000-LOG-ERROR.
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID
                                          ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE WS-ERR-SQLCODE         TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA.

      ******************************************************************
       9000-RETURN.
           IF WS-RC < 12
              EXEC SQL COMMIT END-EXEC
              MOVE SPACES              TO CHK-RESTART-KEY
              SET CHK-COMPLETE         TO TRUE
              PERFORM 6200-WRITE-CHECKPOINT
           END-IF
           CLOSE GL-FILE REPORT-FILE
           DISPLAY 'BILGLEXT ENDED RC ' WS-RC ' DETAILS ' WS-DTL-COUNT
           MOVE WS-RC                  TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
                                          WS-ERR-SQLCODE
           DISPLAY 'BILGLEXT DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA
           MOVE SPACES                 TO ERC-ER-TEXT
           STRING 'BILGLEXT Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           PERFORM 8000-LOG-ERROR
           EXEC SQL ROLLBACK END-EXEC
           SET CHK-FAILED              TO TRUE
           PERFORM 6200-WRITE-CHECKPOINT
           MOVE 16                     TO WS-RC
           PERFORM 9000-RETURN.
