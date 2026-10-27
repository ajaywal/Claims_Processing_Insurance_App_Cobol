       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDSBSN.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDSBSN                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Disbursement send. Phase 1 marks approved        *
      *               disbursements due by the business date SENT,     *
      *               clears DISBPAY to CASH in the ledger and tells   *
      *               ClaimsCore (DSBSENT) about claim payments.       *
      *               Phase 2 writes the bank bureau payments file     *
      *               (cheque / BACS) from the rows this run sent, so  *
      *               the file is always rebuilt whole on a restart.   *
      * PARM        : 1-4 commit frequency (default 0200)              *
      *               6-15 business date override yyyy-mm-dd           *
      * FILES       : RESTART (I, optional) CHKPT (O) PAYFILE (O)      *
      *               RPTOUT (O)                                       *
      * CALLS       : BILHOLD0 BILGLPST BILEVTQ BILSEQGN BILERRLG      *
      * TABLES      : BIL_DISBURSEMENT (R U)                           *
      *               BIL_PAYMENT_INSTRUMENT (R) BIL_CONTROL (R)       *
      * RULES       : BR-BIL-DSN-001 Only APPR disbursements due on or *
      *                              before the business date are sent *
      *               BR-BIL-DSN-002 A DISB hold on the account keeps  *
      *                              a BILL disbursement back (APPR)   *
      *               BR-BIL-DSN-003 BILL BACS needs an active         *
      *                              instrument at send time           *
      *               BR-BIL-DSN-004 Sending clears the payable:       *
      *                              DR DISBPAY / CR CASH              *
      *               BR-BIL-DSN-005 Claim payments raise DSBSENT to   *
      *                              CLMS keyed on the claim check id  *
      *               BR-BIL-DSN-006 Cheque serials come from BILSEQGN *
      *                              CHQNO in disbursement id order    *
      *               BR-BIL-DSN-007 File holds exactly the rows sent  *
      *                              by this run (run start timestamp) *
      *                              and trailer totals must agree     *
      * RETURN CODE : 0 ok, 4 held, 8 rejects, 12 control break,       *
      *               16 abend                                         *
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
           SELECT PAYMENT-FILE ASSIGN TO PAYFILE
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-PAY-STATUS.
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
       FD  PAYMENT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PAYMENT-REC.
           COPY BILDSBP1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDSBSN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-HLD-PGM                  PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'BILEVTQ '.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.

       01  WS-FILE-STATUS.
           05 WS-RST-STATUS            PIC XX.
      * Checkpoints are appended per commit: the last one counts
           05 WS-RESTART-SAVE          PIC X(80).
           05 WS-CKP-STATUS            PIC XX.
           05 WS-PAY-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 200.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-START-TS          PIC X(26) VALUE SPACES.
           05 WS-RESTARTED             PIC X     VALUE 'N'.
              88 WS-IS-RESTART          VALUE 'Y'.
           05 WS-EOF-FLAG              PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-OPEN-CURSOR           PIC X(3)  VALUE SPACES.
              88 WS-APR-CSR-OPEN        VALUE 'APR'.
              88 WS-SNT-CSR-OPEN        VALUE 'SNT'.
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
           05 WS-RESULT                PIC X(28).
           05 WS-CALLEE-RC             PIC 9(2).
           05 WS-CALLEE-REASON         PIC X(8).
           05 WS-CALLEE-MESSAGE        PIC X(80).

       01  WS-COUNTERS.
           05 WS-CNT-READ              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SENT              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-HELD              PIC S9(9) COMP VALUE 0.
           05 WS-CNT-REJECTED          PIC S9(9) COMP VALUE 0.
           05 WS-CNT-EVENTS            PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-SENT              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-FILE-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-FILE-AMOUNT           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CHQ-COUNT             PIC S9(9) COMP VALUE 0.
           05 WS-CHQ-AMOUNT            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-BACS-COUNT            PIC S9(9) COMP VALUE 0.
           05 WS-BACS-AMOUNT           PIC S9(13)V99 COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDDSB END-EXEC.
           EXEC SQL INCLUDE BILDPIN END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-DSN-001 Approved and due
           EXEC SQL DECLARE APR-CSR CURSOR WITH HOLD FOR
              SELECT DISB_ID, ACCOUNT_ID, DISB_AMOUNT, DISB_REASON,
                     PAYEE_NAME, PAY_METHOD, INSTR_ID, DUE_DATE,
                     SOURCE_SYSTEM, SOURCE_REF
                FROM BILDB.BIL_DISBURSEMENT
               WHERE DISB_STATUS = 'APPR'
                 AND DUE_DATE   <= :WS-BUS-DATE
               ORDER BY DISB_ID
               FOR FETCH ONLY
           END-EXEC.
      * BR-BIL-DSN-007 Everything this run (incl. before a restart)
      * sent today
           EXEC SQL DECLARE SNT-CSR CURSOR WITH HOLD FOR
              SELECT DISB_ID, ACCOUNT_ID, DISB_AMOUNT, PAYEE_NAME,
                     PAYEE_ADDR1, PAYEE_POSTCODE, PAY_METHOD,
                     INSTR_ID, SOURCE_SYSTEM, SOURCE_REF
                FROM BILDB.BIL_DISBURSEMENT
               WHERE DISB_STATUS = 'SENT'
                 AND SENT_DATE   = :WS-BUS-DATE
                 AND UPDATE_TS  >= :WS-RUN-START-TS
               ORDER BY DISB_ID
               FOR FETCH ONLY
           END-EXEC.

       01  WS-HLD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==HLA-==.
           03 HLA-BODY.
              COPY BILCHLD REPLACING LEADING ==CA-== BY ==HLA-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.
       01  WS-EVT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==EVA-==.
           03 EVA-BODY.
              COPY BILCEVT REPLACING LEADING ==CA-== BY ==EVA-==.
       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILEVTP1.
           COPY BILDSBR2.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SEND-APPROVED
           PERFORM 7000-COMMIT
           PERFORM 3000-WRITE-PAYMENT-FILE
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILDSBSN OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           IF LS-PARM-LEN >= 4 AND LS-PARM-DATA(1:4) IS NUMERIC
              MOVE LS-PARM-DATA(1:4)   TO WS-COMMIT-FREQ
              IF WS-COMMIT-FREQ = 0
                 MOVE 200              TO WS-COMMIT-FREQ
              END-IF
           END-IF
           IF LS-PARM-LEN >= 15 AND LS-PARM-DATA(6:10) NOT = SPACES
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           ELSE
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           MOVE WS-BUS-DATE            TO SNR-H1-BUS-DATE
           PERFORM 1100-READ-RESTART
           IF NOT WS-IS-RESTART
              EXEC SQL
                 SET :WS-RUN-START-TS = CURRENT TIMESTAMP
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '1000-INITIALISE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF.

      * The run start timestamp is the restart key: a rerun keeps it
      * so phase 2 still includes rows sent before the failure.
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
                 MOVE RST-RESTART-KEY TO WS-RUN-START-TS
                 SET WS-IS-RESTART TO TRUE
                 MOVE SPACES     TO SNR-M-TEXT
                 STRING 'RESTART - RUN STARTED ' WS-RUN-START-TS
                        DELIMITED BY SIZE INTO SNR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
              END-IF
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
      * Phase 1 - status, ledger and claims notification
       2000-SEND-APPROVED.
           EXEC SQL OPEN APR-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-SEND-APPROVED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-APR-CSR-OPEN         TO TRUE
           MOVE 'N'                    TO WS-EOF-FLAG
           PERFORM 4100-FETCH-APPROVED
           PERFORM UNTIL WS-EOF
              PERFORM 2100-SEND-ONE
              ADD 1                    TO WS-SINCE-COMMIT
              IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
              END-IF
              PERFORM 4100-FETCH-APPROVED
           END-PERFORM
           EXEC SQL CLOSE APR-CSR END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR.

       2100-SEND-ONE.
           ADD 1                       TO WS-CNT-READ
           MOVE DSB-DISB-ID            TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           INITIALIZE SNR-DETAIL
           MOVE DSB-DISB-ID            TO SNR-D-DISB-ID
           MOVE DSB-DISB-REASON        TO SNR-D-REASON
           MOVE DSB-SOURCE-SYSTEM      TO SNR-D-SOURCE
           MOVE DSB-PAY-METHOD         TO SNR-D-METHOD
           MOVE DSB-DISB-AMOUNT        TO SNR-D-AMOUNT
           MOVE DSB-DUE-DATE           TO SNR-D-DUE-DATE
           MOVE DSB-PAYEE-NAME-TEXT    TO SNR-D-PAYEE
           SET WS-ROW-IS-OK            TO TRUE
      * BR-BIL-DSN-002 Hold keeps the disbursement back
           IF DSB-SOURCE-SYSTEM = 'BILL' AND DSB-ACCOUNT-ID > 0
              PERFORM 8000-CHECK-DISB-HOLD
              IF HLA-HL-IS-ACTIVE
                 ADD 1                 TO WS-CNT-HELD
                 MOVE 'HELD - DISB HOLD ACTIVE' TO SNR-D-RESULT
                 PERFORM 6000-WRITE-DETAIL
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
                 EXIT PARAGRAPH
              END-IF
           END-IF
      * BR-BIL-DSN-003 BILL BACS instrument must still be active
           IF DSB-PAY-METHOD = 'BACS' AND DSB-SOURCE-SYSTEM = 'BILL'
              PERFORM 4300-READ-INSTRUMENT
              IF SQLCODE = 100 OR PIN-INSTR-STATUS NOT = 'ACTV'
                 MOVE 'REJECT - INSTRUMENT INACTIVE' TO WS-RESULT
                 PERFORM 2900-REJECT-ROW
                 EXIT PARAGRAPH
              END-IF
           END-IF
           PERFORM 4400-SET-SAVEPOINT
           PERFORM 4200-MARK-SENT
           IF WS-ROW-IS-OK
              PERFORM 8100-POST-SEND-LEDGER
           END-IF
           IF WS-ROW-IS-OK AND DSB-SOURCE-SYSTEM = 'CLMS'
              PERFORM 8200-RAISE-DSBSENT
           END-IF
           IF WS-ROW-IS-OK
              ADD 1                    TO WS-CNT-SENT
              ADD DSB-DISB-AMOUNT      TO WS-AMT-SENT
              MOVE 'SENT'              TO SNR-D-RESULT
              PERFORM 6000-WRITE-DETAIL
           ELSE
              PERFORM 4500-ROLLBACK-SAVEPOINT
              PERFORM 2900-REJECT-ROW
           END-IF.

       2900-REJECT-ROW.
           ADD 1                       TO WS-CNT-REJECTED
           MOVE WS-RESULT              TO SNR-D-RESULT
           PERFORM 6000-WRITE-DETAIL
           IF WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
      * Phase 2 - payments file rebuilt from the rows this run sent
       3000-WRITE-PAYMENT-FILE.
           OPEN OUTPUT PAYMENT-FILE
           IF WS-PAY-STATUS NOT = '00'
              DISPLAY 'BILDSBSN PAYFILE OPEN FAILED ' WS-PAY-STATUS
              PERFORM 9800-ABEND
           END-IF
           INITIALIZE PAYMENT-REC
           SET PAY-HEADER              TO TRUE
           STRING 'BILDSB' WS-BUS-DATE(3:2) WS-BUS-DATE(6:2)
                  WS-BUS-DATE(9:2) DELIMITED BY SIZE
                  INTO PAY-H-FILE-ID
           MOVE WS-BUS-DATE            TO PAY-H-CREATE-DATE
           MOVE 'BILLINGCOREAJ2026'    TO PAY-H-ORIGINATOR
           MOVE 'GBP'                  TO PAY-H-CURRENCY
           PERFORM 3900-WRITE-PAYMENT
           EXEC SQL OPEN SNT-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3000-WRITE-PAYMENT-FILE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-SNT-CSR-OPEN         TO TRUE
           MOVE 'N'                    TO WS-EOF-FLAG
           PERFORM 4600-FETCH-SENT
           PERFORM UNTIL WS-EOF
              PERFORM 3100-WRITE-DETAIL
              PERFORM 4600-FETCH-SENT
           END-PERFORM
           EXEC SQL CLOSE SNT-CSR END-EXEC
           MOVE SPACES                 TO WS-OPEN-CURSOR
           INITIALIZE PAYMENT-REC
           SET PAY-TRAILER             TO TRUE
           MOVE WS-FILE-COUNT          TO PAY-T-RECORD-COUNT
           MOVE WS-FILE-AMOUNT         TO PAY-T-TOTAL-AMOUNT
           MOVE WS-CHQ-COUNT           TO PAY-T-CHEQUE-COUNT
           MOVE WS-CHQ-AMOUNT          TO PAY-T-CHEQUE-AMOUNT
           MOVE WS-BACS-COUNT          TO PAY-T-BACS-COUNT
           MOVE WS-BACS-AMOUNT         TO PAY-T-BACS-AMOUNT
           PERFORM 3900-WRITE-PAYMENT
           CLOSE PAYMENT-FILE
      * Cheque serials allocated in phase 2 are committed with it
           PERFORM 7000-COMMIT
           PERFORM 3200-CONTROL-CHECK.

       3100-WRITE-DETAIL.
           MOVE DSB-DISB-ID            TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           INITIALIZE PAYMENT-REC
           SET PAY-DETAIL              TO TRUE
           MOVE DSB-DISB-ID            TO PAY-D-DISB-ID
           MOVE DSB-PAY-METHOD         TO PAY-D-METHOD
           MOVE DSB-DISB-AMOUNT        TO PAY-D-AMOUNT
           MOVE DSB-PAYEE-NAME-TEXT    TO PAY-D-PAYEE-NAME
           IF DSB-PAYEE-ADDR1-IND >= 0
              MOVE DSB-PAYEE-ADDR1-TEXT TO PAY-D-PAYEE-ADDR1
           END-IF
           IF DSB-PAYEE-POSTCODE-IND >= 0
              MOVE DSB-PAYEE-POSTCODE  TO PAY-D-PAYEE-POSTCODE
           END-IF
           MOVE WS-BUS-DATE            TO PAY-D-VALUE-DATE
           MOVE DSB-SOURCE-SYSTEM      TO PAY-D-SOURCE-SYSTEM
           STRING DSB-SOURCE-SYSTEM ' ' DSB-SOURCE-REF
                  DELIMITED BY SIZE INTO PAY-D-REFERENCE
           IF DSB-PAY-METHOD = 'CHEQ'
      * BR-BIL-DSN-006 Cheque serial
              PERFORM 8300-NEXT-CHEQUE-NO
              MOVE SQA-SQ-VALUE        TO PAY-D-CHEQUE-NO
              ADD 1                    TO WS-CHQ-COUNT
              ADD DSB-DISB-AMOUNT      TO WS-CHQ-AMOUNT
           ELSE
              IF DSB-SOURCE-SYSTEM = 'BILL'
                 PERFORM 4300-READ-INSTRUMENT
                 IF SQLCODE = 0
                    MOVE PIN-SORT-CODE TO PAY-D-SORT-CODE
                    MOVE PIN-TOKEN-REF TO PAY-D-ACCOUNT-TOKEN
                    MOVE PIN-LAST-FOUR TO PAY-D-LAST-FOUR
                 END-IF
              END-IF
              ADD 1                    TO WS-BACS-COUNT
              ADD DSB-DISB-AMOUNT      TO WS-BACS-AMOUNT
           END-IF
           ADD 1                       TO WS-FILE-COUNT
           ADD DSB-DISB-AMOUNT         TO WS-FILE-AMOUNT
           PERFORM 3900-WRITE-PAYMENT.

      * BR-BIL-DSN-007 File must agree with what phase 1 sent
       3200-CONTROL-CHECK.
           IF WS-IS-RESTART
              EXIT PARAGRAPH
           END-IF
           IF WS-FILE-COUNT NOT = WS-CNT-SENT
              OR WS-FILE-AMOUNT NOT = WS-AMT-SENT
              MOVE SPACES              TO SNR-M-TEXT
              STRING '*** CONTROL BREAK - PAYMENT FILE DOES NOT AGREE'
                     ' WITH ROWS SENT - HOLD FILE FOR INVESTIGATION'
                     DELIMITED BY SIZE INTO SNR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              IF WS-RETURN-CODE < 12
                 MOVE 12               TO WS-RETURN-CODE
              END-IF
           END-IF.

       3900-WRITE-PAYMENT.
           WRITE PAYMENT-REC
           IF WS-PAY-STATUS NOT = '00'
              DISPLAY 'BILDSBSN PAYFILE WRITE FAILED ' WS-PAY-STATUS
              PERFORM 9800-ABEND
           END-IF.

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

       4100-FETCH-APPROVED.
           EXEC SQL
              FETCH APR-CSR
               INTO :DSB-DISB-ID, :DSB-ACCOUNT-ID, :DSB-DISB-AMOUNT,
                    :DSB-DISB-REASON, :DSB-PAYEE-NAME,
                    :DSB-PAY-METHOD, :DSB-INSTR-ID, :DSB-DUE-DATE,
                    :DSB-SOURCE-SYSTEM, :DSB-SOURCE-REF
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-APPROVED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * Guarded by the expected status: a row voided since the fetch
      * is left alone.
       4200-MARK-SENT.
           EXEC SQL
              UPDATE BILDB.BIL_DISBURSEMENT
                 SET DISB_STATUS = 'SENT',
                     SENT_DATE   = :WS-BUS-DATE,
                     UPDATE_TS   = CURRENT TIMESTAMP
               WHERE DISB_ID     = :DSB-DISB-ID
                 AND DISB_STATUS = 'APPR'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    MOVE 'N'           TO WS-ROW-OK
                    MOVE 'REJECT - NO LONGER APPR' TO WS-RESULT
                 END-IF
              WHEN -911
              WHEN -913
                 MOVE 'N'              TO WS-ROW-OK
                 MOVE 'REJECT - ROW LOCKED' TO WS-RESULT
              WHEN OTHER
                 MOVE '4200-MARK-SENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4300-READ-INSTRUMENT.
           MOVE DSB-INSTR-ID           TO PIN-INSTR-ID
           MOVE DSB-ACCOUNT-ID         TO PIN-ACCOUNT-ID
           MOVE SPACES                 TO PIN-INSTR-STATUS
           EXEC SQL
              SELECT SORT_CODE, TOKEN_REF, LAST_FOUR, INSTR_STATUS
                INTO :PIN-SORT-CODE, :PIN-TOKEN-REF, :PIN-LAST-FOUR,
                     :PIN-INSTR-STATUS
                FROM BILDB.BIL_PAYMENT_INSTRUMENT
               WHERE INSTR_ID   = :PIN-INSTR-ID
                 AND ACCOUNT_ID = :PIN-ACCOUNT-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4300-READ-INSTRUMENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4400-SET-SAVEPOINT.
           EXEC SQL
              SAVEPOINT DSBSN_ROW ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-SET-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4500-ROLLBACK-SAVEPOINT.
           EXEC SQL
              ROLLBACK TO SAVEPOINT DSBSN_ROW
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-ROLLBACK-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4600-FETCH-SENT.
           EXEC SQL
              FETCH SNT-CSR
               INTO :DSB-DISB-ID, :DSB-ACCOUNT-ID, :DSB-DISB-AMOUNT,
                    :DSB-PAYEE-NAME,
                    :DSB-PAYEE-ADDR1 :DSB-PAYEE-ADDR1-IND,
                    :DSB-PAYEE-POSTCODE :DSB-PAYEE-POSTCODE-IND,
                    :DSB-PAY-METHOD, :DSB-INSTR-ID,
                    :DSB-SOURCE-SYSTEM, :DSB-SOURCE-REF
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4600-FETCH-SENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM SNR-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO SNR-H1-PAGE
           WRITE REPORT-REC FROM SNR-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM SNR-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM SNR-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6300-WRITE-TOTAL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM SNR-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE 0                      TO SNR-T-AMOUNT.

       6900-WRITE-CONTROL-TOTALS.
           MOVE '*** CONTROL TOTALS ***' TO SNR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'APPROVED DISBURSEMENTS DUE' TO SNR-T-LABEL
           MOVE WS-CNT-READ            TO SNR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'SENT THIS RUN'        TO SNR-T-LABEL
           MOVE WS-CNT-SENT            TO SNR-T-COUNT
           MOVE WS-AMT-SENT            TO SNR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'HELD - DISBURSEMENT HOLD' TO SNR-T-LABEL
           MOVE WS-CNT-HELD            TO SNR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'REJECTED'             TO SNR-T-LABEL
           MOVE WS-CNT-REJECTED        TO SNR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'DSBSENT EVENTS TO CLMS' TO SNR-T-LABEL
           MOVE WS-CNT-EVENTS          TO SNR-T-COUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'PAYMENT FILE - CHEQUES' TO SNR-T-LABEL
           MOVE WS-CHQ-COUNT           TO SNR-T-COUNT
           MOVE WS-CHQ-AMOUNT          TO SNR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'PAYMENT FILE - BACS'  TO SNR-T-LABEL
           MOVE WS-BACS-COUNT          TO SNR-T-COUNT
           MOVE WS-BACS-AMOUNT         TO SNR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'PAYMENT FILE - TOTAL' TO SNR-T-LABEL
           MOVE WS-FILE-COUNT          TO SNR-T-COUNT
           MOVE WS-FILE-AMOUNT         TO SNR-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO SNR-T-LABEL
           MOVE WS-CNT-COMMITS         TO SNR-T-COUNT
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
           MOVE WS-RUN-START-TS        TO CKP-RESTART-KEY
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           MOVE WS-CNT-SENT            TO CKP-DONE-COUNT
           SET CKP-RUNNING             TO TRUE
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILDSBSN CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       8000-CHECK-DISB-HOLD.
           INITIALIZE WS-HLD-CA
           MOVE 'HLDCHK  '             TO HLA-REQUEST-ID
           MOVE WS-BATCH-USER          TO HLA-USER-ID
           MOVE WS-BUS-DATE            TO HLA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HLA-CALLER-PGM
           MOVE DSB-ACCOUNT-ID         TO HLA-HL-ACCOUNT-ID
           MOVE 'DISB'                 TO HLA-HL-HOLD-TYPE
           MOVE WS-BUS-DATE            TO HLA-HL-AS-OF-DATE
           CALL WS-HLD-PGM USING WS-HLD-CA
           MOVE HLA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE HLA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE HLA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      * BR-BIL-DSN-004 Payable cleared to cash
       8100-POST-SEND-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE WS-BATCH-USER          TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE 'DISBSENT'             TO GLA-GL-TXN-TYPE
           MOVE DSB-ACCOUNT-ID         TO GLA-GL-ACCOUNT-ID
           MOVE 'DISB'                 TO GLA-GL-REF-TYPE
           MOVE DSB-DISB-ID            TO GLA-GL-REF-ID
           MOVE 2                      TO GLA-GL-LINE-COUNT
           MOVE 'DISBPAY '             TO GLA-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLA-GL-DR-CR(1)
           MOVE 'CASH    '             TO GLA-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLA-GL-DR-CR(2)
           MOVE DSB-DISB-AMOUNT        TO GLA-GL-AMOUNT(1)
                                          GLA-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLA-RETURN-CODE >= 90
              MOVE GLA-RETURN-CODE     TO WS-CALLEE-RC
              MOVE GLA-REASON-CODE     TO WS-CALLEE-REASON
              MOVE GLA-MESSAGE         TO WS-CALLEE-MESSAGE
              PERFORM 8900-CHECK-CALLEE
           END-IF
           IF GLA-RETURN-CODE >= 8
              MOVE 'N'                 TO WS-ROW-OK
              STRING 'REJECT - GL ' GLA-REASON-CODE
                     DELIMITED BY SIZE INTO WS-RESULT
           END-IF.

      * BR-BIL-DSN-005 DSBSENT to ClaimsCore, keyed on the check id
       8200-RAISE-DSBSENT.
           IF DSB-SOURCE-REF(1:10) NOT NUMERIC
              MOVE 'N'                 TO WS-ROW-OK
              MOVE 'REJECT - CLMS REF NOT A CHECK' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           INITIALIZE EVP-DSBSENT
           MOVE DSB-SOURCE-REF(1:10)   TO EVP-DS-CHECK-ID
           MOVE DSB-DISB-ID            TO EVP-DS-DISB-ID
           MOVE WS-BUS-DATE            TO EVP-DS-SENT-DATE
           INITIALIZE WS-EVT-CA
           MOVE 'EVPUT   '             TO EVA-REQUEST-ID
           MOVE WS-BATCH-USER          TO EVA-USER-ID
           MOVE WS-BUS-DATE            TO EVA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO EVA-CALLER-PGM
           MOVE 'DSBSENT '             TO EVA-EV-TYPE
           MOVE 'O'                    TO EVA-EV-DIRECTION
           MOVE 'BILL'                 TO EVA-EV-SOURCE
           MOVE 'CLMS'                 TO EVA-EV-TARGET
           MOVE DSB-SOURCE-REF(1:10)   TO EVA-EV-KEY
           MOVE EVP-DSBSENT            TO EVA-EV-PAYLOAD
           CALL WS-EVT-PGM USING WS-EVT-CA
           IF EVA-RETURN-CODE >= 90
              MOVE EVA-RETURN-CODE     TO WS-CALLEE-RC
              MOVE EVA-REASON-CODE     TO WS-CALLEE-REASON
              MOVE EVA-MESSAGE         TO WS-CALLEE-MESSAGE
              PERFORM 8900-CHECK-CALLEE
           END-IF
           IF EVA-RETURN-CODE >= 8
              MOVE 'N'                 TO WS-ROW-OK
              STRING 'REJECT - EVENT ' EVA-REASON-CODE
                     DELIMITED BY SIZE INTO WS-RESULT
           ELSE
              ADD 1                    TO WS-CNT-EVENTS
           END-IF.

       8300-NEXT-CHEQUE-NO.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE WS-BATCH-USER          TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'CHQNO   '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-RETURN-CODE        TO WS-CALLEE-RC
           MOVE SQA-REASON-CODE        TO WS-CALLEE-REASON
           MOVE SQA-MESSAGE            TO WS-CALLEE-MESSAGE
           PERFORM 8900-CHECK-CALLEE.

      * Any service failure that is not row-specific stops the run
       8900-CHECK-CALLEE.
           IF WS-CALLEE-RC >= 12
              MOVE SPACES              TO SNR-M-TEXT
              STRING 'SERVICE FAILURE RC ' WS-CALLEE-RC ' '
                     WS-CALLEE-REASON ' ' WS-CALLEE-MESSAGE
                     DELIMITED BY SIZE INTO SNR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       9000-TERMINATE.
           IF WS-PAGE-COUNT = 0
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           INITIALIZE CHECKPOINT-REC
           MOVE WS-PROGRAM-ID          TO CKP-PROGRAM
           MOVE WS-BUS-DATE            TO CKP-RUN-DATE
           MOVE WS-RUN-START-TS        TO CKP-RESTART-KEY
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-READ            TO CKP-READ-COUNT
           MOVE WS-FILE-COUNT          TO CKP-DONE-COUNT
           SET CKP-COMPLETE            TO TRUE
           WRITE CHECKPOINT-REC
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILDSBSN COMPLETE SENT=' WS-CNT-SENT
                   ' FILE=' WS-FILE-COUNT ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

       9800-ABEND.
           EVALUATE TRUE
              WHEN WS-APR-CSR-OPEN
                 EXEC SQL CLOSE APR-CSR END-EXEC
              WHEN WS-SNT-CSR-OPEN
                 EXEC SQL CLOSE SNT-CSR END-EXEC
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           MOVE SPACES                 TO WS-OPEN-CURSOR
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO SNR-M-TEXT
           STRING '*** BILDSBSN ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK. PAYMENT FILE INCOMPLETE. LAST KEY '
                  WS-ERR-KEY DELIMITED BY SIZE INTO SNR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
                 PAYMENT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILDSBSN DB2 ERROR ' WS-SQLCODE-DISP ' IN '
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
