       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILACHDR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILACHDR                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Direct debit (ACH / BACS) draft generation.      *
      *               Phase 1 raises a payment request for every       *
      *               billed or due invoice of an autopay account that *
      *               falls due within the draft horizon. Phase 2      *
      *               writes every created request to the bank draft   *
      *               file in one unit of work and marks it SENT.      *
      * PARM        : 1-4 commit frequency (default 0200)              *
      *               6-15 business date override yyyy-mm-dd           *
      *               17-18 horizon in business days (default 03)      *
      *               20-25 BACS service user number                   *
      * FILES       : RESTART (I, optional) CHKPT (O) DRAFTOUT (O)     *
      *               RPTOUT (O)                                       *
      * CALLS       : BILHOLD0 BILSEQGN BILDATES BILERRLG              *
      * TABLES      : BIL_ACCOUNT (R) BIL_INVOICE (R)                  *
      *               BIL_PAYMENT_INSTRUMENT (R)                       *
      *               BIL_PAYMENT_REQUEST (C R U) BIL_CONTROL (R)      *
      * RULES       : BR-BIL-ACD-001 Only active accounts with autopay *
      *                              switched on are drafted           *
      *               BR-BIL-ACD-002 Invoices BILD or DUE with money   *
      *                              due on or before the horizon of   *
      *                              N business days (default 3)       *
      *               BR-BIL-ACD-003 The autopay instrument must be    *
      *                              ACTV, the account's own, and a    *
      *                              bank mandate (sort code present)  *
      *               BR-BIL-ACD-004 An active PYMT hold stops drafting*
      *                              for the whole account             *
      *               BR-BIL-ACD-005 One live request per invoice: a   *
      *                              CRTD SENT or SETL draft blocks    *
      *                              another                           *
      *               BR-BIL-ACD-006 Unapplied cash is netted off the  *
      *                              account's invoices oldest first;  *
      *                              nothing left = no draft           *
      *               BR-BIL-ACD-007 Draft date = invoice due date,    *
      *                              never before the business date,   *
      *                              rolled to a business day          *
      *               BR-BIL-ACD-008 All CRTD requests go to one file  *
      *                              and become SENT under its file    *
      *                              reference; trailer carries count  *
      *                              and pence total                   *
      * RETURN CODE : 0 ok, 4 accounts skipped, 8 rejects, 16 abend    *
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
           SELECT DRAFT-FILE ASSIGN TO DRAFTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-DRF-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RESTART-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RESTART-REC.
           COPY BILLKBK1 REPLACING LEADING ==CKP-== BY ==RST-==.
       FD  CHECKPOINT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHECKPOINT-REC.
           COPY BILLKBK1.
       FD  DRAFT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DRAFT-REC.
           COPY BILACHD1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILACHDR'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-FILE-STATUS.
           05 WS-RST-STATUS            PIC XX.
           05 WS-CKP-STATUS            PIC XX.
           05 WS-DRF-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.
           05 WS-DD-TXN-CODE           PIC X(2)  VALUE '17'.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 200.
           05 WS-HORIZON-DAYS          PIC S9(4) COMP VALUE 3.
           05 WS-SERVICE-USER          PIC X(6)  VALUE '000000'.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-HORIZON-DATE          PIC X(10) VALUE SPACES.
           05 WS-DRAFT-DATE            PIC X(10) VALUE SPACES.
           05 WS-FILE-REF              PIC X(12) VALUE SPACES.
           05 WS-RESTART-ACCT          PIC S9(9) COMP VALUE 0.
           05 WS-RESTART-NUM           PIC 9(10).
           05 WS-PREV-ACCT             PIC S9(9) COMP VALUE 0.
           05 WS-DAY-IX                PIC S9(4) COMP VALUE 0.
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-DRAFT-CSR-FLAG        PIC X     VALUE 'N'.
              88 WS-DRAFT-CSR-OPEN      VALUE 'Y'.
           05 WS-SEND-CSR-FLAG         PIC X     VALUE 'N'.
              88 WS-SEND-CSR-OPEN       VALUE 'Y'.
           05 WS-DRAFT-FILE-FLAG       PIC X     VALUE 'N'.
              88 WS-DRAFT-FILE-OPEN     VALUE 'Y'.
           05 WS-HOLD-FLAG             PIC X     VALUE 'N'.
              88 WS-ACCT-ON-HOLD        VALUE 'Y'.
           05 WS-RETURN-CODE           PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-PAGE-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-LIVE-REQUESTS         PIC S9(9) COMP VALUE 0.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-NUM               PIC 9(10).
           05 WS-NOW                   PIC X(21).

       01  WS-AMOUNTS.
           05 WS-CREDIT-LEFT           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DRAFT-AMOUNT          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PENCE                 PIC S9(13) COMP-3 VALUE 0.

      * Instrument columns come from an outer join
       01  WS-FETCH-INSTR.
           05 WS-FI-STATUS             PIC X(4).
           05 WS-FI-SORT-CODE          PIC X(6).
           05 WS-FI-ACCOUNT-ID         PIC S9(9) COMP.

       01  WS-COUNTERS.
           05 WS-CNT-READ              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-CREATED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-NO-MANDATE        PIC S9(9) COMP VALUE 0.
           05 WS-CNT-HELD              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-LIVE              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-CREDIT            PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SENT              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-DUE-READ          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-CREATED           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-CREDIT-USED       PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-SENT              PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-PENCE-SENT            PIC S9(15) COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDINV END-EXEC.
           EXEC SQL INCLUDE BILDPIN END-EXEC.
           EXEC SQL INCLUDE BILDPRQ END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-ACD-001/002 Autopay accounts, billed or due invoices
      * inside the horizon, in account order for restart
           EXEC SQL DECLARE DRAFT-CSR CURSOR WITH HOLD FOR
              SELECT A.ACCOUNT_ID, A.ACCOUNT_NUMBER,
                     A.AUTOPAY_INSTR_ID, A.UNAPPLIED_AMT,
                     V.INVOICE_ID, V.INVOICE_NUMBER, V.DUE_DATE,
                     V.AMOUNT_DUE,
                     COALESCE(P.INSTR_STATUS, 'NONE'),
                     COALESCE(P.SORT_CODE, ' '),
                     COALESCE(P.ACCOUNT_ID, 0)
                FROM BILDB.BIL_ACCOUNT A
                JOIN BILDB.BIL_INVOICE V
                  ON V.ACCOUNT_ID = A.ACCOUNT_ID
                LEFT OUTER JOIN BILDB.BIL_PAYMENT_INSTRUMENT P
                  ON P.INSTR_ID = A.AUTOPAY_INSTR_ID
               WHERE A.AUTOPAY_FLAG    = 'Y'
                 AND A.ACCOUNT_STATUS  = 'ACTV'
                 AND A.ACCOUNT_ID      > :WS-RESTART-ACCT
                 AND V.INVOICE_STATUS IN ('BILD', 'DUE ')
                 AND V.AMOUNT_DUE      > 0
                 AND V.DUE_DATE       <= :WS-HORIZON-DATE
               ORDER BY A.ACCOUNT_ID, V.DUE_DATE, V.INVOICE_ID
               FOR FETCH ONLY
           END-EXEC.

      * BR-BIL-ACD-008 Every created request goes to this file
           EXEC SQL DECLARE SEND-CSR CURSOR FOR
              SELECT R.REQUEST_ID, R.REQUEST_AMOUNT, R.DRAFT_DATE,
                     A.ACCOUNT_NUMBER, P.SORT_CODE, P.TOKEN_REF,
                     COALESCE(P.HOLDER_NAME, A.ACCOUNT_NAME)
                FROM BILDB.BIL_PAYMENT_REQUEST R
                JOIN BILDB.BIL_ACCOUNT A
                  ON A.ACCOUNT_ID = R.ACCOUNT_ID
                JOIN BILDB.BIL_PAYMENT_INSTRUMENT P
                  ON P.INSTR_ID = R.INSTR_ID
               WHERE R.REQUEST_STATUS = 'CRTD'
               ORDER BY R.REQUEST_ID
               FOR FETCH ONLY
           END-EXEC.

       01  WS-HLD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==HLA-==.
           03 HLA-BODY.
              COPY BILCHLD REPLACING LEADING ==CA-== BY ==HLA-==.

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

           COPY BILACHDP.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-CREATE-REQUESTS
           PERFORM 2500-WRITE-DRAFT-FILE
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILACHDR OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 1100-READ-PARM
           IF WS-BUS-DATE = SPACES
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           PERFORM 5000-CALC-HORIZON
           MOVE WS-BUS-DATE            TO ADP-H1-BUS-DATE
           MOVE WS-HORIZON-DATE        TO ADP-H1-HORIZON
           PERFORM 1200-READ-RESTART.

      ******************************************************************
       1100-READ-PARM.
           IF LS-PARM-LEN >= 4 AND LS-PARM-DATA(1:4) IS NUMERIC
              MOVE LS-PARM-DATA(1:4)   TO WS-COMMIT-FREQ
              IF WS-COMMIT-FREQ = 0
                 MOVE 200              TO WS-COMMIT-FREQ
              END-IF
           END-IF
           IF LS-PARM-LEN >= 15 AND LS-PARM-DATA(6:10) NOT = SPACES
              INITIALIZE WS-DTE-CA
              MOVE 'DTVALD  '          TO DTA-REQUEST-ID
              MOVE LS-PARM-DATA(6:10)  TO DTA-DT-DATE-1
              PERFORM 8100-CALL-DATES
              IF NOT DTA-DT-IS-VALID
                 DISPLAY 'BILACHDR INVALID PARM DATE '
                         LS-PARM-DATA(6:10)
                 MOVE 16               TO RETURN-CODE
                 GOBACK
              END-IF
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           END-IF
           IF LS-PARM-LEN >= 18 AND LS-PARM-DATA(17:2) IS NUMERIC
              AND LS-PARM-DATA(17:2) NOT = '00'
              MOVE LS-PARM-DATA(17:2)  TO WS-HORIZON-DAYS
           END-IF
           IF LS-PARM-LEN >= 25 AND LS-PARM-DATA(20:6) IS NUMERIC
              MOVE LS-PARM-DATA(20:6)  TO WS-SERVICE-USER
           END-IF.

      ******************************************************************
      * Phase 1 resumes after the last account committed by the
      * failed run on the same business date.
       1200-READ-RESTART.
           OPEN INPUT RESTART-FILE
           IF WS-RST-STATUS = '00'
              READ RESTART-FILE
                 AT END CONTINUE
                 NOT AT END
                    IF RST-PROGRAM = WS-PROGRAM-ID
                       AND RST-RUN-DATE = WS-BUS-DATE
                       AND RST-RUNNING
                       AND RST-RESTART-KEY(1:10) IS NUMERIC
                       MOVE RST-RESTART-KEY(1:10) TO WS-RESTART-NUM
                       MOVE WS-RESTART-NUM TO WS-RESTART-ACCT
                       MOVE SPACES     TO ADP-M-TEXT
                       STRING 'RESTARTING AFTER ACCOUNT ID '
                              RST-RESTART-KEY(1:10)
                              DELIMITED BY SIZE INTO ADP-M-TEXT
                       PERFORM 6200-WRITE-MESSAGE
                    END-IF
              END-READ
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
      * Phase 1 - payment requests
      ******************************************************************
       2000-CREATE-REQUESTS.
           EXEC SQL OPEN DRAFT-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-CREATE-REQUESTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-DRAFT-CSR-OPEN       TO TRUE
           PERFORM 4100-FETCH-DRAFT
           PERFORM UNTIL WS-EOF
              IF ACT-ACCOUNT-ID NOT = WS-PREV-ACCT
      * Commit only on an account boundary so restart never splits
      * one account's invoices across two runs
                 IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                    PERFORM 7000-COMMIT
                 END-IF
                 PERFORM 2050-START-ACCOUNT
              END-IF
              PERFORM 2100-PROCESS-INVOICE
              ADD 1                    TO WS-SINCE-COMMIT
              PERFORM 4100-FETCH-DRAFT
           END-PERFORM
           EXEC SQL CLOSE DRAFT-CSR END-EXEC
           MOVE 'N'                    TO WS-DRAFT-CSR-FLAG
           PERFORM 7000-COMMIT.

      ******************************************************************
       2050-START-ACCOUNT.
           MOVE ACT-ACCOUNT-ID         TO WS-PREV-ACCT
      * BR-BIL-ACD-006 Unapplied cash is consumed oldest invoice first
           MOVE ACT-UNAPPLIED-AMT      TO WS-CREDIT-LEFT
           IF WS-CREDIT-LEFT < 0
              MOVE 0                   TO WS-CREDIT-LEFT
           END-IF
      * BR-BIL-ACD-004 Payment hold blocks the account's drafts
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLA-REQUEST-ID
           MOVE WS-BATCH-USER          TO HLA-USER-ID
           MOVE WS-BUS-DATE            TO HLA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HLA-CALLER-PGM
           MOVE ACT-ACCOUNT-ID         TO HLA-HL-ACCOUNT-ID
           MOVE 'PYMT'                 TO HLA-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLA-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           IF HLA-RETURN-CODE >= 12
              DISPLAY 'BILACHDR BILHOLD0 RC ' HLA-RETURN-CODE ' '
                      HLA-REASON-CODE ' ' HLA-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           MOVE 'N'                    TO WS-HOLD-FLAG
           IF HLA-HL-IS-ACTIVE
              SET WS-ACCT-ON-HOLD      TO TRUE
           END-IF.

      ******************************************************************
       2100-PROCESS-INVOICE.
           ADD 1                       TO WS-CNT-READ
           ADD INV-AMOUNT-DUE          TO WS-AMT-DUE-READ
           MOVE ACT-ACCOUNT-ID         TO WS-KEY-NUM
           MOVE SPACES                 TO WS-ERR-KEY
           STRING WS-KEY-NUM '/' INV-INVOICE-NUMBER
                  DELIMITED BY SIZE INTO WS-ERR-KEY
           INITIALIZE ADP-DETAIL
           MOVE ACT-ACCOUNT-NUMBER     TO ADP-D-ACCOUNT-NO
           MOVE INV-INVOICE-NUMBER     TO ADP-D-INVOICE-NO
           MOVE INV-DUE-DATE           TO ADP-D-DUE-DATE
           MOVE INV-AMOUNT-DUE         TO ADP-D-AMOUNT-DUE

      * BR-BIL-ACD-003 Active bank mandate owned by the account
           IF WS-FI-STATUS NOT = 'ACTV'
              OR WS-FI-SORT-CODE = SPACES
              OR WS-FI-ACCOUNT-ID NOT = ACT-ACCOUNT-ID
              ADD 1                    TO WS-CNT-NO-MANDATE
              MOVE 'SKIPPED - NO ACTIVE BANK MANDATE'
                                       TO ADP-D-RESULT
              PERFORM 5100-SET-RC-WARNING
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           IF WS-ACCT-ON-HOLD
              ADD 1                    TO WS-CNT-HELD
              MOVE 'SKIPPED - PAYMENT HOLD' TO ADP-D-RESULT
              PERFORM 5100-SET-RC-WARNING
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           PERFORM 4200-COUNT-LIVE-REQUESTS
      * BR-BIL-ACD-005 Never draft the same invoice twice
           IF WS-LIVE-REQUESTS > 0
              ADD 1                    TO WS-CNT-LIVE
              MOVE 'SKIPPED - DRAFT ALREADY RAISED' TO ADP-D-RESULT
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF

      * BR-BIL-ACD-006 Net any unapplied cash off this invoice
           IF WS-CREDIT-LEFT >= INV-AMOUNT-DUE
              SUBTRACT INV-AMOUNT-DUE  FROM WS-CREDIT-LEFT
              ADD INV-AMOUNT-DUE       TO WS-AMT-CREDIT-USED
              ADD 1                    TO WS-CNT-CREDIT
              MOVE 'SKIPPED - COVERED BY UNAPPLIED CASH'
                                       TO ADP-D-RESULT
              PERFORM 6000-WRITE-DETAIL
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-DRAFT-AMOUNT = INV-AMOUNT-DUE - WS-CREDIT-LEFT
           ADD WS-CREDIT-LEFT          TO WS-AMT-CREDIT-USED
           MOVE 0                      TO WS-CREDIT-LEFT

           PERFORM 5200-CALC-DRAFT-DATE
           PERFORM 8000-NEXT-REQUEST-ID
           PERFORM 4300-INSERT-REQUEST
           ADD 1                       TO WS-CNT-CREATED
           ADD WS-DRAFT-AMOUNT         TO WS-AMT-CREATED
           MOVE WS-DRAFT-AMOUNT        TO ADP-D-DRAFT-AMOUNT
           MOVE WS-DRAFT-DATE          TO ADP-D-DRAFT-DATE
           MOVE PRQ-REQUEST-ID         TO ADP-D-REQUEST-ID
           IF WS-DRAFT-AMOUNT < INV-AMOUNT-DUE
              MOVE 'DRAFT RAISED - NET OF UNAPPLIED' TO ADP-D-RESULT
           ELSE
              MOVE 'DRAFT RAISED'      TO ADP-D-RESULT
           END-IF
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
      * Phase 2 - draft file, one unit of work. A failure rolls every
      * request back to CRTD and the rerun rewrites the whole file.
      ******************************************************************
       2500-WRITE-DRAFT-FILE.
           MOVE FUNCTION CURRENT-DATE  TO WS-NOW
           MOVE SPACES                 TO WS-FILE-REF
           STRING 'DD' WS-NOW(3:6) WS-NOW(9:4)
                  DELIMITED BY SIZE INTO WS-FILE-REF
           OPEN OUTPUT DRAFT-FILE
           IF WS-DRF-STATUS NOT = '00'
              DISPLAY 'BILACHDR OPEN FAILED DRAFTOUT=' WS-DRF-STATUS
              PERFORM 9800-ABEND
           END-IF
           SET WS-DRAFT-FILE-OPEN      TO TRUE
           MOVE SPACES                 TO DRAFT-REC
           MOVE 'H'                    TO ACD-REC-TYPE
           MOVE WS-SERVICE-USER        TO ACD-H-SERVICE-USER
           MOVE WS-FILE-REF            TO ACD-H-FILE-REF
           STRING WS-NOW(1:4) '-' WS-NOW(5:2) '-' WS-NOW(7:2)
                  DELIMITED BY SIZE INTO ACD-H-CREATE-DATE
           MOVE WS-BUS-DATE            TO ACD-H-BUS-DATE
           PERFORM 2700-WRITE-DRAFT-REC

           EXEC SQL OPEN SEND-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-WRITE-DRAFT-FILE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-SEND-CSR-OPEN        TO TRUE
           MOVE 'N'                    TO WS-EOF-FLAG
           PERFORM 4500-FETCH-SEND
           PERFORM UNTIL WS-EOF
              PERFORM 2600-SEND-REQUEST
              PERFORM 4500-FETCH-SEND
           END-PERFORM
           EXEC SQL CLOSE SEND-CSR END-EXEC
           MOVE 'N'                    TO WS-SEND-CSR-FLAG

           MOVE SPACES                 TO DRAFT-REC
           MOVE 'T'                    TO ACD-REC-TYPE
           MOVE WS-CNT-SENT            TO ACD-T-DETAIL-COUNT
           MOVE WS-PENCE-SENT          TO ACD-T-TOTAL-PENCE
           PERFORM 2700-WRITE-DRAFT-REC
           CLOSE DRAFT-FILE
           MOVE 'N'                    TO WS-DRAFT-FILE-FLAG
           PERFORM 7000-COMMIT
           MOVE SPACES                 TO ADP-M-TEXT
           STRING 'DRAFT FILE ' WS-FILE-REF ' WRITTEN'
                  DELIMITED BY SIZE INTO ADP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE.

      ******************************************************************
       2600-SEND-REQUEST.
           MOVE PRQ-REQUEST-ID         TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           COMPUTE WS-PENCE = PRQ-REQUEST-AMOUNT * 100
           MOVE SPACES                 TO DRAFT-REC
           MOVE 'D'                    TO ACD-REC-TYPE
           MOVE PRQ-REQUEST-ID         TO ACD-D-REQUEST-ID
           MOVE PIN-SORT-CODE          TO ACD-D-SORT-CODE
           MOVE PIN-TOKEN-REF          TO ACD-D-MANDATE-TOKEN
           MOVE WS-DD-TXN-CODE         TO ACD-D-TXN-CODE
           MOVE WS-PENCE               TO ACD-D-AMOUNT-PENCE
           MOVE PRQ-DRAFT-DATE         TO ACD-D-DRAFT-DATE
           STRING ACT-ACCOUNT-NUMBER '-' WS-KEY-NUM(3:8)
                  DELIMITED BY SIZE INTO ACD-D-PAYER-REF
           IF PIN-HOLDER-NAME-LEN > 0
              MOVE PIN-HOLDER-NAME-TEXT(1:PIN-HOLDER-NAME-LEN)
                                       TO ACD-D-PAYER-NAME
           END-IF
           PERFORM 2700-WRITE-DRAFT-REC
           PERFORM 4600-MARK-SENT
           ADD 1                       TO WS-CNT-SENT
           ADD PRQ-REQUEST-AMOUNT      TO WS-AMT-SENT
           ADD WS-PENCE                TO WS-PENCE-SENT.

      ******************************************************************
       2700-WRITE-DRAFT-REC.
           WRITE DRAFT-REC
           IF WS-DRF-STATUS NOT = '00'
              DISPLAY 'BILACHDR WRITE FAILED DRAFTOUT=' WS-DRF-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * Db2 access
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

      ******************************************************************
       4100-FETCH-DRAFT.
           EXEC SQL
              FETCH DRAFT-CSR
               INTO :ACT-ACCOUNT-ID, :ACT-ACCOUNT-NUMBER,
                    :ACT-AUTOPAY-INSTR-ID, :ACT-UNAPPLIED-AMT,
                    :INV-INVOICE-ID, :INV-INVOICE-NUMBER,
                    :INV-DUE-DATE, :INV-AMOUNT-DUE,
                    :WS-FI-STATUS, :WS-FI-SORT-CODE,
                    :WS-FI-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-DRAFT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-COUNT-LIVE-REQUESTS.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-LIVE-REQUESTS
                FROM BILDB.BIL_PAYMENT_REQUEST
               WHERE INVOICE_ID     = :INV-INVOICE-ID
                 AND REQUEST_STATUS IN ('CRTD', 'SENT', 'SETL')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-COUNT-LIVE-REQUESTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-INSERT-REQUEST.
           MOVE ACT-ACCOUNT-ID         TO PRQ-ACCOUNT-ID
           MOVE ACT-AUTOPAY-INSTR-ID   TO PRQ-INSTR-ID
           MOVE INV-INVOICE-ID         TO PRQ-INVOICE-ID
           MOVE WS-DRAFT-AMOUNT        TO PRQ-REQUEST-AMOUNT
           MOVE WS-DRAFT-DATE          TO PRQ-DRAFT-DATE
           MOVE 'CRTD'                 TO PRQ-REQUEST-STATUS
           MOVE 0                      TO PRQ-MONEY-ID
           MOVE SPACES                 TO PRQ-RETURN-CODE
                                          PRQ-FILE-REF
           EXEC SQL
              INSERT INTO BILDB.BIL_PAYMENT_REQUEST
                    (REQUEST_ID, ACCOUNT_ID, INSTR_ID, INVOICE_ID,
                     REQUEST_AMOUNT, DRAFT_DATE, REQUEST_STATUS,
                     MONEY_ID, RETURN_CODE, FILE_REF, CREATE_TS,
                     UPDATE_TS)
              VALUES (:PRQ-REQUEST-ID, :PRQ-ACCOUNT-ID,
                      :PRQ-INSTR-ID, :PRQ-INVOICE-ID,
                      :PRQ-REQUEST-AMOUNT, :PRQ-DRAFT-DATE,
                      :PRQ-REQUEST-STATUS, :PRQ-MONEY-ID,
                      :PRQ-RETURN-CODE, :PRQ-FILE-REF,
                      CURRENT TIMESTAMP, CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-INSERT-REQUEST' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4500-FETCH-SEND.
           EXEC SQL
              FETCH SEND-CSR
               INTO :PRQ-REQUEST-ID, :PRQ-REQUEST-AMOUNT,
                    :PRQ-DRAFT-DATE, :ACT-ACCOUNT-NUMBER,
                    :PIN-SORT-CODE, :PIN-TOKEN-REF,
                    :PIN-HOLDER-NAME
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4500-FETCH-SEND' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4600-MARK-SENT.
           MOVE WS-FILE-REF            TO PRQ-FILE-REF
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_REQUEST
                 SET REQUEST_STATUS = 'SENT',
                     FILE_REF       = :PRQ-FILE-REF,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE REQUEST_ID     = :PRQ-REQUEST-ID
                 AND REQUEST_STATUS = 'CRTD'
           END-EXEC
           IF SQLCODE NOT = 0 OR SQLERRD(3) = 0
              MOVE '4600-MARK-SENT'    TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Calculations
      ******************************************************************
      * BR-BIL-ACD-002 Horizon = business date plus N business days
       5000-CALC-HORIZON.
           MOVE WS-BUS-DATE            TO WS-HORIZON-DATE
           PERFORM VARYING WS-DAY-IX FROM 1 BY 1
                     UNTIL WS-DAY-IX > WS-HORIZON-DAYS
              INITIALIZE WS-DTE-CA
              MOVE 'DTADDD  '          TO DTA-REQUEST-ID
              MOVE WS-HORIZON-DATE     TO DTA-DT-DATE-1
              MOVE 1                   TO DTA-DT-NUMBER
              PERFORM 8100-CALL-DATES
              MOVE DTA-DT-RESULT-DATE  TO WS-HORIZON-DATE
              INITIALIZE WS-DTE-CA
              MOVE 'DTBUSD  '          TO DTA-REQUEST-ID
              MOVE WS-HORIZON-DATE     TO DTA-DT-DATE-1
              PERFORM 8100-CALL-DATES
              MOVE DTA-DT-RESULT-DATE  TO WS-HORIZON-DATE
           END-PERFORM.

      ******************************************************************
       5100-SET-RC-WARNING.
           IF WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
      * BR-BIL-ACD-007 Due date, not before today, on a business day
       5200-CALC-DRAFT-DATE.
           IF INV-DUE-DATE > WS-BUS-DATE
              MOVE INV-DUE-DATE        TO WS-DRAFT-DATE
           ELSE
              MOVE WS-BUS-DATE         TO WS-DRAFT-DATE
           END-IF
           INITIALIZE WS-DTE-CA
           MOVE 'DTBUSD  '             TO DTA-REQUEST-ID
           MOVE WS-DRAFT-DATE          TO DTA-DT-DATE-1
           PERFORM 8100-CALL-DATES
           MOVE DTA-DT-RESULT-DATE     TO WS-DRAFT-DATE.

      ******************************************************************
      * Report
      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM ADP-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO ADP-H1-PAGE
           WRITE REPORT-REC FROM ADP-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM ADP-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM ADP-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE SPACES                 TO ADP-M-TEXT.

       6300-WRITE-TOTAL.
           WRITE REPORT-REC FROM ADP-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6900-WRITE-CONTROL-TOTALS.
           PERFORM 6100-WRITE-HEADINGS
           MOVE 'DIRECT DEBIT CONTROL TOTALS' TO ADP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'INVOICES IN HORIZON'  TO ADP-T-LABEL
           MOVE WS-CNT-READ            TO ADP-T-COUNT
           MOVE WS-AMT-DUE-READ        TO ADP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'DRAFTS RAISED'        TO ADP-T-LABEL
           MOVE WS-CNT-CREATED         TO ADP-T-COUNT
           MOVE WS-AMT-CREATED         TO ADP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'UNAPPLIED CASH NETTED / INVOICES COVERED'
                                       TO ADP-T-LABEL
           MOVE WS-CNT-CREDIT          TO ADP-T-COUNT
           MOVE WS-AMT-CREDIT-USED     TO ADP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SKIPPED - NO ACTIVE BANK MANDATE' TO ADP-T-LABEL
           MOVE WS-CNT-NO-MANDATE      TO ADP-T-COUNT
           MOVE 0                      TO ADP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SKIPPED - PAYMENT HOLD' TO ADP-T-LABEL
           MOVE WS-CNT-HELD            TO ADP-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SKIPPED - DRAFT ALREADY RAISED' TO ADP-T-LABEL
           MOVE WS-CNT-LIVE            TO ADP-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'WRITTEN TO DRAFT FILE (ALL CRTD)' TO ADP-T-LABEL
           MOVE WS-CNT-SENT            TO ADP-T-COUNT
           MOVE WS-AMT-SENT            TO ADP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO ADP-T-LABEL
           MOVE WS-CNT-COMMITS         TO ADP-T-COUNT
           MOVE 0                      TO ADP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL.

      ******************************************************************
      * Commit and checkpoint
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
           MOVE 'D'                    TO CKP-PHASE
           MOVE WS-PREV-ACCT           TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO CKP-RESTART-KEY(1:10)
           MOVE WS-FILE-REF            TO CKP-BATCH-REF
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           SET CKP-RUNNING             TO TRUE
           IF WS-FILE-REF NOT = SPACES
              MOVE 'S'                 TO CKP-PHASE
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILACHDR CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * Calls
      ******************************************************************
       8000-NEXT-REQUEST-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE WS-BATCH-USER          TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'PRQ     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 12
              DISPLAY 'BILACHDR BILSEQGN RC ' SQA-RETURN-CODE ' '
                      SQA-REASON-CODE ' ' SQA-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           MOVE SQA-SQ-VALUE           TO PRQ-REQUEST-ID.

      ******************************************************************
       8100-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              DISPLAY 'BILACHDR BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       9000-TERMINATE.
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILACHDR COMPLETE FILE=' WS-FILE-REF
                   ' READ=' WS-CNT-READ ' RAISED=' WS-CNT-CREATED
                   ' SENT=' WS-CNT-SENT ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Unrecoverable: back out the open unit of work, keep the last
      * checkpoint for restart. A part-written draft file has no
      * trailer and must not be transmitted.
       9800-ABEND.
           IF WS-DRAFT-CSR-OPEN
              EXEC SQL CLOSE DRAFT-CSR END-EXEC
              MOVE 'N'                 TO WS-DRAFT-CSR-FLAG
           END-IF
           IF WS-SEND-CSR-OPEN
              EXEC SQL CLOSE SEND-CSR END-EXEC
              MOVE 'N'                 TO WS-SEND-CSR-FLAG
           END-IF
           IF WS-DRAFT-FILE-OPEN
              CLOSE DRAFT-FILE
              MOVE 'N'                 TO WS-DRAFT-FILE-FLAG
           END-IF
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO ADP-M-TEXT
           STRING '*** BILACHDR ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK, DO NOT SEND DRAFTOUT. LAST KEY '
                  WS-ERR-KEY
                  DELIMITED BY SIZE INTO ADP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILACHDR DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA ' KEY ' WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           STRING 'BILACHDR Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9800-ABEND.
