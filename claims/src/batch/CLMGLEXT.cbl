       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMGLEXT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMGLEXT                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Claims financial extract for the general ledger. *
      *               Summarises the business day's submitted and      *
      *               approved claim transactions by policy type, cost *
      *               type and transaction type into one balanced GL   *
      *               journal (paid loss, paid ALAE, case reserve      *
      *               change, recoveries, recovery reserve change)     *
      *               with header and trailer hash totals.             *
      *               PARM: optional 'RERUN' to re-extract a date.     *
      * CALLS       : CLMERRLG (ERRLOG)                                *
      * FILES       : GLOUT   GL journal interface file (CLMGLEJ1)     *
      *               RPTOUT  132-column run report                    *
      *               CHKPT   checkpoint record (CLMGLEC1)             *
      * TABLES      : CLM_TRANSACTION  (R)                             *
      *               CLM_RESERVE_LINE (R)                             *
      *               CLM_CLAIM        (R)                             *
      *               CLM_CONTROL      (R U C) rows SYSTEM, GLEXTRCT   *
      * RULES       : BR-CLM-GLX-001 The day's financials are posted   *
      *                              (SUBM) transactions dated on the  *
      *                              business date plus older PAPR     *
      *                              ones approved to SUBM on it       *
      *               BR-CLM-GLX-002 GL accounts come from the posting *
      *                              table by transaction type and     *
      *                              cost type; an unmapped pair posts *
      *                              to suspense with RC 4             *
      *               BR-CLM-GLX-003 A negative amount (reserve take-  *
      *                              down, void) swaps debit / credit  *
      *               BR-CLM-GLX-004 Policy type is the cost centre    *
      *               BR-CLM-GLX-005 Journal must balance; otherwise   *
      *                              RC 12 and the date is not marked  *
      *                              extracted                         *
      *               BR-CLM-GLX-006 One extract per business date     *
      *                              unless PARM RERUN or the previous *
      *                              run for the date did not finish   *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE      ASSIGN TO GLOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-GL-STATUS.
           SELECT RPT-FILE     ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL CHK-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE
           RECORDING MODE IS F.
           COPY CLMGLEJ1.
       FD  RPT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).
       FD  CHK-FILE
           RECORDING MODE IS F.
           COPY CLMGLEC1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMGLEXT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-GL-CONTROL-KEY           PIC X(8)  VALUE 'GLEXTRCT'.

       01  WS-FILE-STATUS.
           05 WS-GL-STATUS             PIC XX    VALUE '00'.
           05 WS-RPT-STATUS            PIC XX    VALUE '00'.
           05 WS-CHK-STATUS            PIC XX    VALUE '00'.

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-RERUN-SW              PIC X     VALUE 'N'.
              88 WS-RERUN               VALUE 'Y'.
           05 WS-CTL-ROW-SW            PIC X     VALUE 'N'.
              88 WS-CTL-ROW-EXISTS      VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-TS                PIC X(26) VALUE SPACES.
           05 WS-LAST-EXTRACT          PIC X(10) VALUE SPACES.
           05 WS-JOURNAL-ID            PIC X(16) VALUE SPACES.
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-UNCOMMITTED           PIC S9(4) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.

       01  WS-TOTALS.
           05 WS-ROWS-READ             PIC S9(7) COMP-3 VALUE 0.
           05 WS-TXNS-READ             PIC S9(9) COMP-3 VALUE 0.
           05 WS-LINES-WRITTEN         PIC S9(7) COMP-3 VALUE 0.
           05 WS-TOTAL-DEBITS          PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-CREDITS         PIC S9(15)V99 COMP-3 VALUE 0.
           05 WS-ACCOUNT-HASH          PIC S9(15) COMP-3 VALUE 0.
           05 WS-SUSPENSE-ROWS         PIC S9(7) COMP-3 VALUE 0.
           05 WS-ZERO-ROWS             PIC S9(7) COMP-3 VALUE 0.

      * Fetched summary row
       01  WS-SUMMARY.
           05 WS-S-POLICY-TYPE         PIC X.
           05 WS-S-COST-TYPE           PIC X(4).
           05 WS-S-TXN-TYPE            PIC X(4).
           05 WS-S-AMOUNT              PIC S9(13)V99 COMP-3.
           05 WS-S-COUNT               PIC S9(9) COMP.

       01  WS-POSTING.
           05 WS-PST-BUCKET            PIC X(6).
           05 WS-PST-DR-ACCT           PIC X(6).
           05 WS-PST-CR-ACCT           PIC X(6).
           05 WS-PST-DESC              PIC X(20).
           05 WS-PST-CCTR              PIC X(4).
           05 WS-PST-CCTR-NAME         PIC X(10).
           05 WS-PST-ABS-AMOUNT        PIC S9(13)V99 COMP-3.
           05 WS-LINE-ACCOUNT          PIC X(6).
           05 WS-LINE-ACCOUNT-N REDEFINES WS-LINE-ACCOUNT
                                       PIC 9(6).
           05 WS-LINE-DR-CR            PIC X(2).

      * BR-CLM-GLX-002 Posting table: transaction type x cost type
      * as written by CLMFINTX. '*' cost type is the default for the
      * transaction type. PAY payment, DED deductible withheld from a
      * payment (credits paid loss), RSV reserve change, RCV recovery
      * receipt, RRSV recovery reserve change. Cost types: CLMC claim
      * cost (indemnity), EXAO adjusting & other, EXDC defence and
      * cost containment (both ALAE).
       01  WS-GL-RULE-VALUES.
           05 FILLER PIC X(46) VALUE
              'PAY CLMCPDLOSS510100210100PAID LOSS           '.
           05 FILLER PIC X(46) VALUE
              'PAY EXAOPDAOEX520100210100PAID ALAE A&O       '.
           05 FILLER PIC X(46) VALUE
              'PAY EXDCPDDCCX520200210100PAID ALAE DCC       '.
           05 FILLER PIC X(46) VALUE
              'DED *   DEDWHD210100510100DEDUCTIBLE WITHHELD '.
           05 FILLER PIC X(46) VALUE
              'RSV CLMCRSVLOS511100230100CASE RSV CHG LOSS   '.
           05 FILLER PIC X(46) VALUE
              'RSV EXAORSVAOX521100230200CASE RSV CHG A&O    '.
           05 FILLER PIC X(46) VALUE
              'RSV EXDCRSVDCC521200230300CASE RSV CHG DCC    '.
           05 FILLER PIC X(46) VALUE
              'RCV *   RECOVR210100515100RECOVERIES RECEIVED '.
           05 FILLER PIC X(46) VALUE
              'RRSV*   RCVRSV131100516100RECOVERY RSV CHANGE '.
       01  WS-GL-RULE-TABLE REDEFINES WS-GL-RULE-VALUES.
           05 WS-GL-RULE OCCURS 9 TIMES INDEXED BY GX.
              10 WS-GR-TXN-TYPE        PIC X(4).
              10 WS-GR-COST-TYPE       PIC X(4).
              10 WS-GR-BUCKET          PIC X(6).
              10 WS-GR-DR-ACCT         PIC X(6).
              10 WS-GR-CR-ACCT         PIC X(6).
              10 WS-GR-DESC            PIC X(20).
       01  WS-SUSPENSE-ACCT            PIC X(6)  VALUE '999900'.
       01  WS-CLEARING-ACCT            PIC X(6)  VALUE '210100'.

      * BR-CLM-GLX-004 Policy type -> GL cost centre
       01  WS-CCTR-VALUES.
           05 FILLER PIC X(15) VALUE 'MC100MOTOR     '.
           05 FILLER PIC X(15) VALUE 'HC200HOUSEHOLD '.
           05 FILLER PIC X(15) VALUE 'CC300COMMERCIAL'.
           05 FILLER PIC X(15) VALUE 'EC400ENDOWMENT '.
       01  WS-CCTR-TABLE REDEFINES WS-CCTR-VALUES.
           05 WS-CCTR OCCURS 4 TIMES INDEXED BY CX.
              10 WS-CC-POLICY-TYPE     PIC X.
              10 WS-CC-CODE            PIC X(4).
              10 WS-CC-NAME            PIC X(10).

      * Totals by GL bucket for the report (row 10 = suspense)
       01  WS-BUCKET-TOTALS.
           05 WS-BT OCCURS 10 TIMES.
              10 WS-BT-COUNT           PIC S9(9) COMP-3.
              10 WS-BT-AMOUNT          PIC S9(15)V99 COMP-3.
       01  WS-BX                       PIC S9(4) COMP VALUE 0.

       01  WS-WORK.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

      * BR-CLM-GLX-001 Day's postable transactions, summarised
           EXEC SQL DECLARE GLCSR CURSOR WITH HOLD FOR
                SELECT C.POLICY_TYPE, R.COST_TYPE, T.TXN_TYPE,
                       SUM(T.TXN_AMOUNT), COUNT(*)
                  FROM CLMDB.CLM_TRANSACTION T
                  JOIN CLMDB.CLM_RESERVE_LINE R
                    ON R.RL_ID = T.RL_ID
                  JOIN CLMDB.CLM_CLAIM C
                    ON C.CLAIM_ID = T.CLAIM_ID
                 WHERE T.TXN_STATUS = 'SUBM'
                   AND (T.BUSINESS_DATE = :WS-BUS-DATE
                        OR (T.BUSINESS_DATE < :WS-BUS-DATE
                            AND T.APPROVED_TS IS NOT NULL
                            AND DATE(T.APPROVED_TS) = :WS-BUS-DATE))
                 GROUP BY C.POLICY_TYPE, R.COST_TYPE, T.TXN_TYPE
                 ORDER BY C.POLICY_TYPE, R.COST_TYPE, T.TXN_TYPE
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY CLMGLER1.

       LINKAGE SECTION.
       01  LK-PARM.
           05 LK-PARM-LEN              PIC S9(4) COMP.
           05 LK-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EXTRACT
           PERFORM 3000-CHECK-BALANCE
           PERFORM 4300-MARK-EXTRACTED
           PERFORM 7000-COMMIT
           MOVE 'C'                    TO GLC-RUN-STATUS
           PERFORM 7100-WRITE-CHECKPOINT
           PERFORM 6500-PRINT-TOTALS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'CLMGLEXT RPTOUT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           IF LK-PARM-LEN > 0
              IF LK-PARM-DATA(1:5) = 'RERUN'
                 SET WS-RERUN          TO TRUE
              ELSE
                 MOVE 'Invalid PARM - only RERUN is accepted'
                                       TO GLR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
              END-IF
           END-IF
           PERFORM 4000-READ-CONTROL
           MOVE WS-BUS-DATE            TO GLR-H1-BUS-DATE
           MOVE WS-RUN-TS(1:19)        TO GLR-H1-RUN-TS
           PERFORM 1100-READ-CHECKPOINT
           PERFORM 1200-CHECK-ALREADY-EXTRACTED
           STRING 'CLMGL' WS-BUS-DATE(1:4) WS-BUS-DATE(6:2)
                  WS-BUS-DATE(9:2) '001'
                  DELIMITED BY SIZE INTO WS-JOURNAL-ID
           OPEN OUTPUT GL-FILE
           IF WS-GL-STATUS NOT = '00'
              MOVE 'GLOUT cannot be opened' TO GLR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE SPACES                 TO GLJ-HEADER-REC
           MOVE 'H'                    TO GLJ-REC-TYPE
           MOVE 'CLAIMS  '             TO GLJ-H-SOURCE
           MOVE WS-JOURNAL-ID          TO GLJ-H-JOURNAL-ID
           MOVE WS-BUS-DATE            TO GLJ-H-JOURNAL-DATE
           STRING WS-BUS-DATE(1:4) WS-BUS-DATE(6:2)
                  DELIMITED BY SIZE INTO GLJ-H-PERIOD
           MOVE CTL-CURRENCY-CODE      TO GLJ-H-CURRENCY
           MOVE WS-RERUN-SW            TO GLJ-H-RERUN-FLAG
           WRITE GLJ-HEADER-REC
           INITIALIZE WS-BUCKET-TOTALS.

      ******************************************************************
      * An unfinished run for the same date is simply run again: the
      * extract posts nothing to Db2 until the journal balances.
       1100-READ-CHECKPOINT.
           OPEN INPUT CHK-FILE
           IF WS-CHK-STATUS = '00'
              READ CHK-FILE
                 AT END
                    CONTINUE
                 NOT AT END
                    IF GLC-PROGRAM = WS-PROGRAM-ID
                    AND GLC-RUN-ACTIVE
                    AND GLC-BUS-DATE = WS-BUS-DATE
                       SET WS-RERUN    TO TRUE
                       STRING 'RESTART: previous extract for this '
                              'date did not complete - re-extracting'
                              DELIMITED BY SIZE INTO GLR-MSG-TEXT
                       PERFORM 6100-WRITE-MSG
                    END-IF
              END-READ
              CLOSE CHK-FILE
           END-IF
           OPEN OUTPUT CHK-FILE
           IF WS-CHK-STATUS NOT = '00'
              MOVE 'CHKPT file cannot be opened for output'
                                       TO GLR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           INITIALIZE GLC-CHECKPOINT-REC
           PERFORM 7100-WRITE-CHECKPOINT.

      ******************************************************************
      * BR-CLM-GLX-006 One extract per business date
       1200-CHECK-ALREADY-EXTRACTED.
           MOVE WS-GL-CONTROL-KEY      TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :WS-LAST-EXTRACT
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-CTL-ROW-EXISTS TO TRUE
              WHEN 100
                 MOVE SPACES           TO WS-LAST-EXTRACT
              WHEN OTHER
                 MOVE '1200-CHECK-EXTRACTED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-LAST-EXTRACT NOT = SPACES
           AND WS-LAST-EXTRACT >= WS-BUS-DATE
              IF WS-RERUN
                 STRING 'RERUN: business date ' WS-BUS-DATE
                        ' re-extracted - GL must reverse the earlier'
                        ' journal' DELIMITED BY SIZE
                        INTO GLR-MSG-TEXT
                 PERFORM 6100-WRITE-MSG
                 MOVE 4                TO WS-MAX-RC
              ELSE
                 STRING 'Business date ' WS-BUS-DATE
                        ' already extracted (last ' WS-LAST-EXTRACT
                        ') - use PARM RERUN to repeat'
                        DELIMITED BY SIZE INTO GLR-MSG-TEXT
                 PERFORM 6100-WRITE-MSG
                 MOVE 8                TO WS-MAX-RC
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
       2000-EXTRACT.
           MOVE GLR-COL-HEAD           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           EXEC SQL OPEN GLCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-EXTRACT'      TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-SUMMARY
           PERFORM UNTIL WS-EOF
              ADD 1                    TO WS-ROWS-READ
              ADD WS-S-COUNT           TO WS-TXNS-READ
              PERFORM 2100-POST-SUMMARY
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
                 PERFORM 7100-WRITE-CHECKPOINT
              END-IF
              PERFORM 4100-FETCH-SUMMARY
           END-PERFORM
           EXEC SQL CLOSE GLCSR END-EXEC
           MOVE SPACES                 TO GLJ-TRAILER-REC
           MOVE 'T'                    TO GLJ-REC-TYPE
           MOVE WS-LINES-WRITTEN       TO GLJ-T-LINE-COUNT
           MOVE WS-TOTAL-DEBITS        TO GLJ-T-TOTAL-DEBITS
           MOVE WS-TOTAL-CREDITS       TO GLJ-T-TOTAL-CREDITS
           MOVE WS-ACCOUNT-HASH        TO GLJ-T-ACCOUNT-HASH
           WRITE GLJ-TRAILER-REC
           CLOSE GL-FILE.

      ******************************************************************
       2100-POST-SUMMARY.
           PERFORM 5000-RESOLVE-POSTING
           MOVE WS-S-POLICY-TYPE       TO GLR-D-POL-TYPE
           MOVE WS-PST-CCTR            TO GLR-D-CCTR
           MOVE WS-S-COST-TYPE         TO GLR-D-COST-TYPE
           MOVE WS-S-TXN-TYPE          TO GLR-D-TXN-TYPE
           MOVE WS-PST-BUCKET          TO GLR-D-BUCKET
           MOVE WS-S-COUNT             TO GLR-D-TXN-COUNT
           MOVE WS-S-AMOUNT            TO GLR-D-AMOUNT
           MOVE SPACES                 TO GLR-D-NOTE
           ADD WS-S-COUNT              TO WS-BT-COUNT(WS-BX)
           ADD WS-S-AMOUNT             TO WS-BT-AMOUNT(WS-BX)
           IF WS-S-AMOUNT = 0
              ADD 1                    TO WS-ZERO-ROWS
              MOVE SPACES              TO GLR-D-DR-ACCT GLR-D-CR-ACCT
              MOVE 'Nets to zero - no journal lines'
                                       TO GLR-D-NOTE
           ELSE
      * BR-CLM-GLX-003 Negative movements reverse the posting sides
              IF WS-S-AMOUNT < 0
                 COMPUTE WS-PST-ABS-AMOUNT = 0 - WS-S-AMOUNT
                 MOVE WS-PST-CR-ACCT   TO GLR-D-DR-ACCT
                 MOVE WS-PST-DR-ACCT   TO GLR-D-CR-ACCT
                 MOVE 'Reversed sides (negative movement)'
                                       TO GLR-D-NOTE
              ELSE
                 MOVE WS-S-AMOUNT      TO WS-PST-ABS-AMOUNT
                 MOVE WS-PST-DR-ACCT   TO GLR-D-DR-ACCT
                 MOVE WS-PST-CR-ACCT   TO GLR-D-CR-ACCT
              END-IF
              MOVE GLR-D-DR-ACCT       TO WS-LINE-ACCOUNT
              MOVE 'DR'                TO WS-LINE-DR-CR
              PERFORM 2200-WRITE-JOURNAL-LINE
              MOVE GLR-D-CR-ACCT       TO WS-LINE-ACCOUNT
              MOVE 'CR'                TO WS-LINE-DR-CR
              PERFORM 2200-WRITE-JOURNAL-LINE
           END-IF
           IF WS-PST-BUCKET = 'SUSPNS'
              MOVE 'UNMAPPED TXN/COST TYPE - POSTED TO SUSPENSE'
                                       TO GLR-D-NOTE
           END-IF
           MOVE GLR-DET-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       2200-WRITE-JOURNAL-LINE.
           ADD 1                       TO WS-LINES-WRITTEN
           MOVE SPACES                 TO GLJ-DETAIL-REC
           MOVE 'D'                    TO GLJ-REC-TYPE
           MOVE WS-JOURNAL-ID          TO GLJ-D-JOURNAL-ID
           MOVE WS-LINES-WRITTEN       TO GLJ-D-LINE-NO
           MOVE WS-LINE-ACCOUNT        TO GLJ-D-ACCOUNT
           MOVE WS-PST-CCTR            TO GLJ-D-COST-CENTRE
           MOVE WS-LINE-DR-CR          TO GLJ-D-DR-CR
           MOVE WS-PST-ABS-AMOUNT      TO GLJ-D-AMOUNT
           MOVE CTL-CURRENCY-CODE      TO GLJ-D-CURRENCY
           MOVE WS-PST-BUCKET          TO GLJ-D-BUCKET
           STRING WS-PST-DESC DELIMITED BY '  '
                  ' ' WS-PST-CCTR-NAME DELIMITED BY SIZE
                  ' ' WS-S-COST-TYPE DELIMITED BY SIZE
                  INTO GLJ-D-DESCRIPTION
           MOVE WS-S-COUNT             TO GLJ-D-TXN-COUNT
           WRITE GLJ-DETAIL-REC
           IF WS-GL-STATUS NOT = '00'
              MOVE 'GLOUT write failed' TO GLR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           IF WS-LINE-DR-CR = 'DR'
              ADD WS-PST-ABS-AMOUNT    TO WS-TOTAL-DEBITS
           ELSE
              ADD WS-PST-ABS-AMOUNT    TO WS-TOTAL-CREDITS
           END-IF
           IF WS-LINE-ACCOUNT-N IS NUMERIC
              ADD WS-LINE-ACCOUNT-N    TO WS-ACCOUNT-HASH
           END-IF.

      ******************************************************************
      * BR-CLM-GLX-005 Debits must equal credits
       3000-CHECK-BALANCE.
           IF WS-TOTAL-DEBITS NOT = WS-TOTAL-CREDITS
              MOVE 'JOURNAL OUT OF BALANCE - DATE NOT MARKED EXTRACTED'
                                       TO GLR-MSG-TEXT
              PERFORM 6100-WRITE-MSG
              PERFORM 6500-PRINT-TOTALS
              EXEC SQL ROLLBACK END-EXEC
              MOVE 12                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           IF WS-SUSPENSE-ROWS > 0 AND WS-MAX-RC < 4
              MOVE 4                   TO WS-MAX-RC
           END-IF.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE, CURRENCY_CODE, CURRENT TIMESTAMP
                INTO :CTL-BUSINESS-DATE, :CTL-CURRENCY-CODE,
                     :WS-RUN-TS
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
       4100-FETCH-SUMMARY.
           EXEC SQL
              FETCH GLCSR
               INTO :WS-S-POLICY-TYPE, :WS-S-COST-TYPE,
                    :WS-S-TXN-TYPE, :WS-S-AMOUNT, :WS-S-COUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-SUMMARY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-MARK-EXTRACTED.
           MOVE WS-GL-CONTROL-KEY      TO CTL-CONTROL-KEY
           IF WS-CTL-ROW-EXISTS
              EXEC SQL
                 UPDATE CLMDB.CLM_CONTROL
                    SET PRIOR_BUS_DATE = BUSINESS_DATE,
                        BUSINESS_DATE  = :WS-BUS-DATE,
                        LAST_EOD_TS    = CURRENT TIMESTAMP,
                        UPDATE_TS      = CURRENT TIMESTAMP
                  WHERE CONTROL_KEY    = :CTL-CONTROL-KEY
              END-EXEC
           ELSE
              EXEC SQL
                 INSERT INTO CLMDB.CLM_CONTROL
                        (CONTROL_KEY, BUSINESS_DATE, PRIOR_BUS_DATE,
                         BATCH_STATUS, LAST_EOD_TS, CURRENCY_CODE,
                         UPDATE_TS)
                 VALUES (:CTL-CONTROL-KEY, :WS-BUS-DATE,
                         :WS-BUS-DATE, 'O', CURRENT TIMESTAMP,
                         :CTL-CURRENCY-CODE, CURRENT TIMESTAMP)
              END-EXEC
           END-IF
           IF SQLCODE NOT = 0
              MOVE '4300-MARK-EXTRACTED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-GLX-002 Exact cost type first, then the '*' default
       5000-RESOLVE-POSTING.
           MOVE 'SUSPNS'               TO WS-PST-BUCKET
           MOVE WS-SUSPENSE-ACCT       TO WS-PST-DR-ACCT
           MOVE WS-CLEARING-ACCT       TO WS-PST-CR-ACCT
           MOVE 'UNMAPPED CLAIM TXN'   TO WS-PST-DESC
           MOVE 10                     TO WS-BX
           SET GX                      TO 1
           SEARCH WS-GL-RULE
              AT END
                 SET GX                TO 1
                 SEARCH WS-GL-RULE
                    AT END
                       ADD 1           TO WS-SUSPENSE-ROWS
                    WHEN WS-GR-TXN-TYPE(GX) = WS-S-TXN-TYPE
                     AND WS-GR-COST-TYPE(GX) = '*'
                       PERFORM 5010-TAKE-RULE
                 END-SEARCH
              WHEN WS-GR-TXN-TYPE(GX) = WS-S-TXN-TYPE
               AND WS-GR-COST-TYPE(GX) = WS-S-COST-TYPE
                 PERFORM 5010-TAKE-RULE
           END-SEARCH
           MOVE 'C999'                 TO WS-PST-CCTR
           MOVE 'UNKNOWN'              TO WS-PST-CCTR-NAME
           SET CX                      TO 1
           SEARCH WS-CCTR
              AT END
                 CONTINUE
              WHEN WS-CC-POLICY-TYPE(CX) = WS-S-POLICY-TYPE
                 MOVE WS-CC-CODE(CX)   TO WS-PST-CCTR
                 MOVE WS-CC-NAME(CX)   TO WS-PST-CCTR-NAME
           END-SEARCH.

      ******************************************************************
       5010-TAKE-RULE.
           MOVE WS-GR-BUCKET(GX)       TO WS-PST-BUCKET
           MOVE WS-GR-DR-ACCT(GX)      TO WS-PST-DR-ACCT
           MOVE WS-GR-CR-ACCT(GX)      TO WS-PST-CR-ACCT
           MOVE WS-GR-DESC(GX)         TO WS-PST-DESC
           SET WS-BX                   TO GX.

      ******************************************************************
       6000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO GLR-H1-PAGE
           WRITE RPT-RECORD FROM GLR-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM GLR-HEAD-2 AFTER ADVANCING 1
           MOVE 2                      TO WS-LINE-COUNT.

      ******************************************************************
       6050-WRITE-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6000-PRINT-HEADINGS
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6100-WRITE-MSG.
           MOVE GLR-MSG-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE SPACES                 TO GLR-MSG-TEXT.

      ******************************************************************
       6500-PRINT-TOTALS.
           PERFORM 6100-WRITE-MSG
           MOVE '*** TOTALS BY GL BUCKET' TO GLR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           PERFORM VARYING WS-BX FROM 1 BY 1 UNTIL WS-BX > 10
              IF WS-BX = 10
                 MOVE 'SUSPNS  UNMAPPED - SUSPENSE' TO GLR-T-LABEL
              ELSE
                 MOVE SPACES           TO GLR-T-LABEL
                 STRING WS-GR-BUCKET(WS-BX) '  '
                        WS-GR-DESC(WS-BX)
                        DELIMITED BY SIZE INTO GLR-T-LABEL
              END-IF
              MOVE WS-BT-COUNT(WS-BX)  TO GLR-T-COUNT
              MOVE WS-BT-AMOUNT(WS-BX) TO GLR-T-AMOUNT
              PERFORM 6550-WRITE-TOTAL
           END-PERFORM
           MOVE '*** CONTROL TOTALS'   TO GLR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 'SUMMARY ROWS READ / TRANSACTIONS COVERED'
                                       TO GLR-T-LABEL
           MOVE WS-ROWS-READ           TO GLR-T-COUNT
           MOVE WS-TXNS-READ           TO GLR-T-AMOUNT
           PERFORM 6550-WRITE-TOTAL
           MOVE 'JOURNAL LINES WRITTEN / TOTAL DEBITS'
                                       TO GLR-T-LABEL
           MOVE WS-LINES-WRITTEN       TO GLR-T-COUNT
           MOVE WS-TOTAL-DEBITS        TO GLR-T-AMOUNT
           PERFORM 6550-WRITE-TOTAL
           MOVE 'TOTAL CREDITS'        TO GLR-T-LABEL
           MOVE 0                      TO GLR-T-COUNT
           MOVE WS-TOTAL-CREDITS       TO GLR-T-AMOUNT
           PERFORM 6550-WRITE-TOTAL
           MOVE 'ACCOUNT HASH TOTAL'   TO GLR-T-LABEL
           MOVE 0                      TO GLR-T-COUNT
           MOVE WS-ACCOUNT-HASH        TO GLR-T-AMOUNT
           PERFORM 6550-WRITE-TOTAL
           MOVE 'ROWS TO SUSPENSE / ROWS NETTING TO ZERO'
                                       TO GLR-T-LABEL
           MOVE WS-SUSPENSE-ROWS       TO GLR-T-COUNT
           MOVE WS-ZERO-ROWS           TO GLR-T-AMOUNT
           PERFORM 6550-WRITE-TOTAL
           MOVE 'RETURN CODE'          TO GLR-T-LABEL
           MOVE WS-MAX-RC              TO GLR-T-COUNT
           MOVE 0                      TO GLR-T-AMOUNT
           PERFORM 6550-WRITE-TOTAL.

      ******************************************************************
       6550-WRITE-TOTAL.
           MOVE GLR-TOT-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       6900-ABORT-RUN.
           PERFORM 6100-WRITE-MSG
           EXEC SQL ROLLBACK END-EXEC
           MOVE 16                     TO WS-MAX-RC
           PERFORM 9000-RETURN.

      ******************************************************************
       7000-COMMIT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '7000-COMMIT'       TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-UNCOMMITTED.

      ******************************************************************
       7100-WRITE-CHECKPOINT.
           MOVE WS-PROGRAM-ID          TO GLC-PROGRAM
           IF NOT GLC-RUN-COMPLETE
              MOVE 'R'                 TO GLC-RUN-STATUS
           END-IF
           MOVE WS-BUS-DATE            TO GLC-BUS-DATE
           MOVE WS-ROWS-READ           TO GLC-ROWS-READ
           MOVE WS-LINES-WRITTEN       TO GLC-LINES-WRITTEN
           MOVE FUNCTION CURRENT-DATE  TO GLC-CHECKPOINT-TS
           CLOSE CHK-FILE
           OPEN OUTPUT CHK-FILE
           WRITE GLC-CHECKPOINT-REC
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'CLMGLEXT CHECKPOINT WRITE FAILED ' WS-CHK-STATUS
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF.

      ******************************************************************
       9000-RETURN.
           CLOSE GL-FILE
           CLOSE RPT-FILE
           CLOSE CHK-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-BUS-DATE            TO ERC-ER-KEY
           STRING 'CLMGLEXT Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           EXEC SQL ROLLBACK END-EXEC
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO GLR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 12                     TO WS-MAX-RC
           PERFORM 9000-RETURN.
