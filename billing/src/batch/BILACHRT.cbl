       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILACHRT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILACHRT                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Direct debit returns and settlement. Phase R     *
      *               works the bank's returns file: each returned     *
      *               draft becomes RTND, a settled draft's receipt is *
      *               reversed NSF and the mandate failure recorded.   *
      *               Phase S settles every SENT draft that has passed *
      *               the return window without a return: the money is *
      *               receipted through BILPAYRC (source ACH) and the  *
      *               request becomes SETL.                            *
      * PARM        : 1-4 commit frequency (default 0200)              *
      *               6-15 business date override yyyy-mm-dd           *
      *               17-18 return window in business days (default 2) *
      * FILES       : RTNIN (I) RESTART (I, optional) CHKPT (O)        *
      *               RPTOUT (O)                                       *
      * CALLS       : BILPAYRC BILINSTR BILDATES BILERRLG              *
      * TABLES      : BIL_PAYMENT_REQUEST (R U) BIL_ACCOUNT (R)        *
      *               BIL_INVOICE (R) BIL_CONTROL (R)                  *
      * RULES       : BR-BIL-ACR-001 Returns file must prove: header,  *
      *                              trailer count and pence total;    *
      *                              otherwise nothing is returned or  *
      *                              settled (RC 12)                   *
      *               BR-BIL-ACR-002 Only R01 R02 R03 R04 are accepted *
      *               BR-BIL-ACR-003 Only SENT or SETL drafts can be   *
      *                              returned, and only once           *
      *               BR-BIL-ACR-004 Returned amount must equal the    *
      *                              drafted amount                    *
      *               BR-BIL-ACR-005 A settled draft that comes back   *
      *                              has its receipt reversed NSF      *
      *                              (BILPAYRC books the fee and the   *
      *                              instrument failure)               *
      *               BR-BIL-ACR-006 A draft returned before settling  *
      *                              records the instrument failure    *
      *                              directly                          *
      *               BR-BIL-ACR-007 R02/R03: the bank account is gone *
      *                              - the mandate is withdrawn now    *
      *               BR-BIL-ACR-008 A SENT draft with no return after *
      *                              the window (2 business days)      *
      *                              settles as an ACH receipt dated   *
      *                              the draft date                    *
      *               BR-BIL-ACR-009 Each record is a savepoint; a     *
      *                              failure backs out that record     *
      *                              only and is reported              *
      * RETURN CODE : 0 ok, 4 warnings, 8 rejects, 12 file rejected,   *
      *               16 abend                                         *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RETURNS-FILE ASSIGN TO RTNIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RTN-STATUS.
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
       FD  RETURNS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RETURNS-REC.
           COPY BILACHR1.
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
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILACHRT'.
       01  WS-PAY-PGM                  PIC X(8)  VALUE 'BILPAYRC'.
       01  WS-PIN-PGM                  PIC X(8)  VALUE 'BILINSTR'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-FILE-STATUS.
           05 WS-RTN-STATUS            PIC XX.
              88 WS-RTN-OK              VALUE '00'.
              88 WS-RTN-EOF             VALUE '10'.
           05 WS-RST-STATUS            PIC XX.
           05 WS-CKP-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.
           05 WS-MAX-LOOKBACK          PIC S9(4) COMP VALUE 15.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 200.
           05 WS-WINDOW-DAYS           PIC S9(4) COMP VALUE 2.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-CUTOFF-DATE           PIC X(10) VALUE SPACES.
           05 WS-PROBE-DATE            PIC X(10) VALUE SPACES.
           05 WS-FILE-REF              PIC X(12) VALUE SPACES.
           05 WS-PHASE                 PIC X     VALUE 'R'.
              88 WS-PHASE-RETURNS       VALUE 'R'.
              88 WS-PHASE-SETTLE        VALUE 'S'.
           05 WS-RESTART-PHASE         PIC X     VALUE SPACE.
           05 WS-RESTART-KEY           PIC S9(9) COMP VALUE 0.
           05 WS-RESTART-NUM           PIC 9(9).
           05 WS-LAST-KEY              PIC S9(9) COMP VALUE 0.
           05 WS-BUS-DAYS              PIC S9(4) COMP VALUE 0.
           05 WS-PROBES                PIC S9(4) COMP VALUE 0.
           05 WS-REC-NO                PIC S9(9) COMP VALUE 0.
           05 WS-FILE-FLAG             PIC X     VALUE 'Y'.
              88 WS-FILE-GOOD           VALUE 'Y'.
              88 WS-FILE-BAD            VALUE 'N'.
           05 WS-HEADER-FLAG           PIC X     VALUE 'N'.
              88 WS-HEADER-SEEN         VALUE 'Y'.
           05 WS-TRAILER-FLAG          PIC X     VALUE 'N'.
              88 WS-TRAILER-SEEN        VALUE 'Y'.
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
           05 WS-KEY-NUM               PIC 9(9).
           05 WS-COUNT-DISP            PIC Z(8)9.
           05 WS-PRIOR-STATUS          PIC X(4).
           05 WS-PRIOR-MONEY-ID        PIC S9(9) COMP.
           05 WS-PIN-REQUEST           PIC X(8).
           05 WS-PIN-FLAG              PIC X     VALUE 'N'.
              88 WS-PIN-FAILED          VALUE 'Y'.
              88 WS-PIN-DONE            VALUE 'N'.

       01  WS-AMOUNTS.
           05 WS-RETURN-AMOUNT         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PF-DETAILS            PIC S9(9) COMP VALUE 0.
           05 WS-PF-PENCE              PIC S9(15) COMP-3 VALUE 0.

       01  WS-COUNTERS.
           05 WS-CNT-RETURNS           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-RTN-SKIPPED       PIC S9(9) COMP VALUE 0.
           05 WS-CNT-RTN-REVERSED      PIC S9(9) COMP VALUE 0.
           05 WS-CNT-RTN-UNSETTLED     PIC S9(9) COMP VALUE 0.
           05 WS-CNT-RTN-REJECTED      PIC S9(9) COMP VALUE 0.
           05 WS-CNT-MANDATE-OFF       PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SETTLE-READ       PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SETTLED           PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SETTLE-SUSP       PIC S9(9) COMP VALUE 0.
           05 WS-CNT-SETTLE-REJECTED   PIC S9(9) COMP VALUE 0.
           05 WS-CNT-COMMITS           PIC S9(9) COMP VALUE 0.
           05 WS-AMT-RETURNS           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-RTN-REVERSED      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-RTN-UNSETTLED     PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-RTN-REJECTED      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-SETTLED           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-SETTLE-SUSP       PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-AMT-SETTLE-REJECTED   PIC S9(13)V99 COMP-3 VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPRQ END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDINV END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * BR-BIL-ACR-008 Drafts past the return window, key order
           EXEC SQL DECLARE SETL-CSR CURSOR WITH HOLD FOR
              SELECT R.REQUEST_ID, R.ACCOUNT_ID, R.INSTR_ID,
                     R.REQUEST_AMOUNT, R.DRAFT_DATE, R.FILE_REF,
                     A.ACCOUNT_NUMBER,
                     COALESCE(V.INVOICE_NUMBER, ' ')
                FROM BILDB.BIL_PAYMENT_REQUEST R
                JOIN BILDB.BIL_ACCOUNT A
                  ON A.ACCOUNT_ID = R.ACCOUNT_ID
                LEFT OUTER JOIN BILDB.BIL_INVOICE V
                  ON V.INVOICE_ID = R.INVOICE_ID
               WHERE R.REQUEST_STATUS = 'SENT'
                 AND R.DRAFT_DATE    <= :WS-CUTOFF-DATE
                 AND R.REQUEST_ID     > :WS-RESTART-KEY
               ORDER BY R.REQUEST_ID
               FOR FETCH ONLY
           END-EXEC.

       01  WS-PAY-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PYA-==.
           03 PYA-BODY.
              COPY BILCPAY REPLACING LEADING ==CA-== BY ==PYA-==.

       01  WS-PIN-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PIA-==.
           03 PIA-BODY.
              COPY BILCPIN REPLACING LEADING ==CA-== BY ==PIA-==.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILACHRP.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROVE-RETURNS
           IF WS-FILE-GOOD
              IF WS-RESTART-PHASE NOT = 'S'
                 PERFORM 2100-PROCESS-RETURNS
              END-IF
              PERFORM 2500-SETTLE-DRAFTS
           ELSE
              MOVE 12                  TO WS-RETURN-CODE
           END-IF
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILACHRT OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           PERFORM 1100-READ-PARM
           IF WS-BUS-DATE = SPACES
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           PERFORM 5000-CALC-CUTOFF
           MOVE WS-BUS-DATE            TO ARP-H1-BUS-DATE
           MOVE WS-CUTOFF-DATE         TO ARP-H1-CUTOFF
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
              PERFORM 8200-CALL-DATES
              IF NOT DTA-DT-IS-VALID
                 DISPLAY 'BILACHRT INVALID PARM DATE '
                         LS-PARM-DATA(6:10)
                 MOVE 16               TO RETURN-CODE
                 GOBACK
              END-IF
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           END-IF
           IF LS-PARM-LEN >= 18 AND LS-PARM-DATA(17:2) IS NUMERIC
              AND LS-PARM-DATA(17:2) NOT = '00'
              MOVE LS-PARM-DATA(17:2)  TO WS-WINDOW-DAYS
           END-IF.

      ******************************************************************
      * Phase R restarts after a returns-file record number; phase S
      * restarts after a request id and skips the returns entirely.
       1200-READ-RESTART.
           OPEN INPUT RESTART-FILE
           IF WS-RST-STATUS = '00'
              READ RESTART-FILE
                 AT END CONTINUE
                 NOT AT END
                    IF RST-PROGRAM = WS-PROGRAM-ID
                       AND RST-RUN-DATE = WS-BUS-DATE
                       AND RST-RUNNING
                       AND RST-RESTART-KEY(1:9) IS NUMERIC
                       MOVE RST-PHASE  TO WS-RESTART-PHASE
                       MOVE RST-RESTART-KEY(1:9) TO WS-RESTART-NUM
                       MOVE WS-RESTART-NUM TO WS-RESTART-KEY
                       MOVE SPACES     TO ARP-M-TEXT
                       STRING 'RESTARTING PHASE ' RST-PHASE
                              ' AFTER KEY ' RST-RESTART-KEY(1:9)
                              DELIMITED BY SIZE INTO ARP-M-TEXT
                       PERFORM 6200-WRITE-MESSAGE
                    END-IF
              END-READ
              CLOSE RESTART-FILE
           END-IF.

      ******************************************************************
      * Pass 1 - prove the returns file before acting on any of it
      ******************************************************************
       2000-PROVE-RETURNS.
           OPEN INPUT RETURNS-FILE
           IF NOT WS-RTN-OK
              DISPLAY 'BILACHRT OPEN FAILED RTNIN=' WS-RTN-STATUS
              PERFORM 9800-ABEND
           END-IF
           MOVE 0                      TO WS-REC-NO
           PERFORM 2050-READ-RETURN
           PERFORM UNTIL WS-RTN-EOF OR WS-FILE-BAD
              PERFORM 3000-PROVE-RECORD
              PERFORM 2050-READ-RETURN
           END-PERFORM
           CLOSE RETURNS-FILE
           IF WS-FILE-GOOD
              PERFORM 3100-CHECK-TRAILER
           END-IF
           IF WS-FILE-BAD
              MOVE 'RETURNS FILE REJECTED - NO RETURNS OR SETTLEMENTS'
                                       TO ARP-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
           END-IF.

      ******************************************************************
       2050-READ-RETURN.
           READ RETURNS-FILE
           EVALUATE TRUE
              WHEN WS-RTN-OK
                 ADD 1                 TO WS-REC-NO
              WHEN WS-RTN-EOF
                 CONTINUE
              WHEN OTHER
                 DISPLAY 'BILACHRT READ ERROR RTNIN=' WS-RTN-STATUS
                 PERFORM 9800-ABEND
           END-EVALUATE.

      ******************************************************************
      * Phase R - returned drafts
      ******************************************************************
       2100-PROCESS-RETURNS.
           SET WS-PHASE-RETURNS        TO TRUE
           OPEN INPUT RETURNS-FILE
           IF NOT WS-RTN-OK
              DISPLAY 'BILACHRT REOPEN FAILED RTNIN=' WS-RTN-STATUS
              PERFORM 9800-ABEND
           END-IF
           MOVE 0                      TO WS-REC-NO
           PERFORM 2050-READ-RETURN
           PERFORM UNTIL WS-RTN-EOF
              IF ACR-IS-DETAIL
                 IF WS-RESTART-PHASE = 'R'
                    AND WS-REC-NO <= WS-RESTART-KEY
                    ADD 1              TO WS-CNT-RTN-SKIPPED
                 ELSE
                    PERFORM 2200-PROCESS-RETURN
                 END-IF
                 MOVE WS-REC-NO        TO WS-LAST-KEY
                 ADD 1                 TO WS-SINCE-COMMIT
                 IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                    PERFORM 7000-COMMIT
                 END-IF
              END-IF
              PERFORM 2050-READ-RETURN
           END-PERFORM
           CLOSE RETURNS-FILE
           PERFORM 7000-COMMIT.

      ******************************************************************
       2200-PROCESS-RETURN.
           ADD 1                       TO WS-CNT-RETURNS
           COMPUTE WS-RETURN-AMOUNT = ACR-D-AMOUNT-PENCE / 100
           ADD WS-RETURN-AMOUNT        TO WS-AMT-RETURNS
           MOVE SPACES                 TO WS-ERR-KEY
           MOVE ACR-D-REQUEST-ID-X     TO WS-ERR-KEY
           INITIALIZE ARP-DETAIL
           MOVE 'RETN'                 TO ARP-D-TYPE
           MOVE ACR-D-REQUEST-ID       TO ARP-D-REQUEST-ID
           MOVE ACR-D-RETURN-CODE      TO ARP-D-RETURN-CODE
           MOVE WS-RETURN-AMOUNT       TO ARP-D-AMOUNT

           PERFORM 3200-VALIDATE-RETURN
           IF ARP-D-RESULT NOT = SPACES
              PERFORM 2290-REJECT-RETURN
              EXIT PARAGRAPH
           END-IF

      * BR-BIL-ACR-009 One savepoint per returned draft
           PERFORM 4300-SET-SAVEPOINT
           MOVE PRQ-REQUEST-STATUS     TO WS-PRIOR-STATUS
           MOVE PRQ-MONEY-ID           TO WS-PRIOR-MONEY-ID
           PERFORM 4200-MARK-RETURNED

           IF WS-PRIOR-STATUS = 'SETL' AND WS-PRIOR-MONEY-ID > 0
      * BR-BIL-ACR-005 Settled money comes back off the account
              PERFORM 8000-REVERSE-RECEIPT
              EVALUATE TRUE
                 WHEN PYA-RETURN-CODE >= 90
                    DISPLAY 'BILACHRT BILPAYRC RC ' PYA-RETURN-CODE
                            ' ' PYA-REASON-CODE ' ' PYA-MESSAGE
                    PERFORM 9800-ABEND
                 WHEN PYA-RETURN-CODE >= 12
                    PERFORM 4400-ROLLBACK-SAVEPOINT
                    STRING 'NOT REVERSED ' PYA-REASON-CODE ' '
                           PYA-MESSAGE DELIMITED BY SIZE
                           INTO ARP-D-RESULT
                    PERFORM 2290-REJECT-RETURN
                    EXIT PARAGRAPH
                 WHEN OTHER
                    MOVE WS-PRIOR-MONEY-ID TO ARP-D-MONEY-ID
                    STRING 'RECEIPT REVERSED NSF ' PYA-REASON-CODE
                           DELIMITED BY SIZE INTO ARP-D-RESULT
              END-EVALUATE
           ELSE
      * BR-BIL-ACR-006 Unsettled draft: record the mandate failure
              MOVE 'INSFAIL '          TO WS-PIN-REQUEST
              PERFORM 8100-CALL-INSTRUMENT
              IF WS-PIN-FAILED
                 PERFORM 2290-REJECT-RETURN
                 EXIT PARAGRAPH
              END-IF
              STRING 'RETURNED BEFORE SETTLEMENT ' PIA-REASON-CODE
                     DELIMITED BY SIZE INTO ARP-D-RESULT
           END-IF

      * BR-BIL-ACR-007 Closed or unknown bank account ends the mandate
           IF ACR-D-CLOSED OR ACR-D-NO-ACCOUNT
              MOVE 'INSDEA  '          TO WS-PIN-REQUEST
              PERFORM 8100-CALL-INSTRUMENT
              IF WS-PIN-FAILED
                 PERFORM 2290-REJECT-RETURN
                 EXIT PARAGRAPH
              END-IF
              IF PIA-RC-OK
                 ADD 1                 TO WS-CNT-MANDATE-OFF
              END-IF
           END-IF
           IF WS-PRIOR-STATUS = 'SETL' AND WS-PRIOR-MONEY-ID > 0
              ADD 1                    TO WS-CNT-RTN-REVERSED
              ADD WS-RETURN-AMOUNT     TO WS-AMT-RTN-REVERSED
           ELSE
              ADD 1                    TO WS-CNT-RTN-UNSETTLED
              ADD WS-RETURN-AMOUNT     TO WS-AMT-RTN-UNSETTLED
           END-IF
           PERFORM 5100-SET-RC-WARNING
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
       2290-REJECT-RETURN.
           ADD 1                       TO WS-CNT-RTN-REJECTED
           ADD WS-RETURN-AMOUNT        TO WS-AMT-RTN-REJECTED
           PERFORM 5200-SET-RC-REJECT
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
      * Phase S - settle drafts that survived the return window
      ******************************************************************
       2500-SETTLE-DRAFTS.
           SET WS-PHASE-SETTLE         TO TRUE
           IF WS-RESTART-PHASE NOT = 'S'
              MOVE 0                   TO WS-RESTART-KEY
           END-IF
           MOVE 0                      TO WS-LAST-KEY
           EXEC SQL OPEN SETL-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-SETTLE-DRAFTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           PERFORM 4500-FETCH-SETTLE
           PERFORM UNTIL WS-EOF
              PERFORM 2600-SETTLE-DRAFT
              MOVE PRQ-REQUEST-ID      TO WS-LAST-KEY
              ADD 1                    TO WS-SINCE-COMMIT
              IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
              END-IF
              PERFORM 4500-FETCH-SETTLE
           END-PERFORM
           EXEC SQL CLOSE SETL-CSR END-EXEC
           MOVE 'N'                    TO WS-CURSOR-FLAG.

      ******************************************************************
       2600-SETTLE-DRAFT.
           ADD 1                       TO WS-CNT-SETTLE-READ
           MOVE PRQ-REQUEST-ID         TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           INITIALIZE ARP-DETAIL
           MOVE 'SETL'                 TO ARP-D-TYPE
           MOVE PRQ-REQUEST-ID         TO ARP-D-REQUEST-ID
           MOVE ACT-ACCOUNT-NUMBER     TO ARP-D-ACCOUNT-NO
           MOVE PRQ-REQUEST-AMOUNT     TO ARP-D-AMOUNT
           MOVE PRQ-DRAFT-DATE         TO ARP-D-DRAFT-DATE

           PERFORM 4300-SET-SAVEPOINT
           PERFORM 8300-RECEIVE-DRAFT
           EVALUATE TRUE
              WHEN PYA-RETURN-CODE >= 90
                 DISPLAY 'BILACHRT BILPAYRC RC ' PYA-RETURN-CODE
                         ' ' PYA-REASON-CODE ' ' PYA-MESSAGE
                 PERFORM 9800-ABEND
      * BR-BIL-ACR-009 Left SENT; the next run tries again
              WHEN PYA-RETURN-CODE >= 12
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 ADD 1                 TO WS-CNT-SETTLE-REJECTED
                 ADD PRQ-REQUEST-AMOUNT TO WS-AMT-SETTLE-REJECTED
                 STRING 'NOT SETTLED ' PYA-REASON-CODE ' '
                        PYA-MESSAGE DELIMITED BY SIZE
                        INTO ARP-D-RESULT
                 PERFORM 5200-SET-RC-REJECT
              WHEN OTHER
                 MOVE PYA-PY-MONEY-ID  TO PRQ-MONEY-ID
                                          ARP-D-MONEY-ID
                 PERFORM 4600-MARK-SETTLED
                 IF PYA-PY-STATUS = 'SUSP'
                    ADD 1              TO WS-CNT-SETTLE-SUSP
                    ADD PRQ-REQUEST-AMOUNT TO WS-AMT-SETTLE-SUSP
                    STRING 'SETTLED TO SUSPENSE ' PYA-REASON-CODE
                           DELIMITED BY SIZE INTO ARP-D-RESULT
                    PERFORM 5100-SET-RC-WARNING
                 ELSE
                    ADD 1              TO WS-CNT-SETTLED
                    ADD PRQ-REQUEST-AMOUNT TO WS-AMT-SETTLED
                    STRING 'SETTLED ' PYA-PY-STATUS ' '
                           PYA-REASON-CODE
                           DELIMITED BY SIZE INTO ARP-D-RESULT
                 END-IF
           END-EVALUATE
           PERFORM 6000-WRITE-DETAIL.

      ******************************************************************
      * Validation
      ******************************************************************
      * BR-BIL-ACR-001 Header first, trailer last
       3000-PROVE-RECORD.
           EVALUATE TRUE
              WHEN WS-REC-NO = 1 AND NOT ACR-IS-HEADER
                 MOVE 'FIRST RECORD IS NOT A RETURNS HEADER'
                                       TO ARP-M-TEXT
                 PERFORM 3900-FILE-ERROR
              WHEN WS-TRAILER-SEEN
                 MOVE 'RECORDS FOUND AFTER THE TRAILER' TO ARP-M-TEXT
                 PERFORM 3900-FILE-ERROR
              WHEN ACR-IS-HEADER
                 IF WS-HEADER-SEEN
                    MOVE 'SECOND HEADER RECORD IN FILE' TO ARP-M-TEXT
                    PERFORM 3900-FILE-ERROR
                 ELSE
                    SET WS-HEADER-SEEN TO TRUE
                    MOVE ACR-H-FILE-REF TO WS-FILE-REF
                 END-IF
              WHEN ACR-IS-DETAIL
                 IF ACR-D-REQUEST-ID-X IS NOT NUMERIC
                    OR ACR-D-AMOUNT-X IS NOT NUMERIC
                    MOVE 'DETAIL WITH NON-NUMERIC REQUEST OR AMOUNT'
                                       TO ARP-M-TEXT
                    PERFORM 3900-FILE-ERROR
                 ELSE
                    ADD 1              TO WS-PF-DETAILS
                    ADD ACR-D-AMOUNT-PENCE TO WS-PF-PENCE
                 END-IF
              WHEN ACR-IS-TRAILER
                 SET WS-TRAILER-SEEN   TO TRUE
                 IF ACR-T-DETAIL-COUNT IS NOT NUMERIC
                    OR ACR-T-TOTAL-PENCE IS NOT NUMERIC
                    MOVE 'TRAILER CONTROL FIELDS ARE NOT NUMERIC'
                                       TO ARP-M-TEXT
                    PERFORM 3900-FILE-ERROR
                 ELSE
                    PERFORM 3050-COMPARE-TRAILER
                 END-IF
              WHEN OTHER
                 MOVE SPACES           TO ARP-M-TEXT
                 STRING 'UNKNOWN RECORD TYPE ' ACR-REC-TYPE
                        DELIMITED BY SIZE INTO ARP-M-TEXT
                 PERFORM 3900-FILE-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ACR-001 Count and pence total must prove
       3050-COMPARE-TRAILER.
           IF ACR-T-DETAIL-COUNT NOT = WS-PF-DETAILS
              MOVE WS-PF-DETAILS       TO WS-COUNT-DISP
              MOVE SPACES              TO ARP-M-TEXT
              STRING 'DETAIL COUNT ' WS-COUNT-DISP
                     ' DOES NOT MATCH TRAILER ' ACR-T-DETAIL-COUNT
                     DELIMITED BY SIZE INTO ARP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF
           IF ACR-T-TOTAL-PENCE NOT = WS-PF-PENCE
              MOVE 'DETAIL PENCE TOTAL DOES NOT MATCH TRAILER'
                                       TO ARP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF.

      ******************************************************************
       3100-CHECK-TRAILER.
           IF NOT WS-HEADER-SEEN OR NOT WS-TRAILER-SEEN
              MOVE 'RETURNS FILE IS MISSING ITS HEADER OR TRAILER'
                                       TO ARP-M-TEXT
              PERFORM 3900-FILE-ERROR
           END-IF.

      ******************************************************************
      * A non-blank ARP-D-RESULT on exit means the return is rejected
       3200-VALIDATE-RETURN.
      * BR-BIL-ACR-002 Known bank return codes only
           IF NOT ACR-D-KNOWN-CODE
              MOVE 'REJECTED - UNKNOWN RETURN CODE' TO ARP-D-RESULT
              EXIT PARAGRAPH
           END-IF
           MOVE ACR-D-REQUEST-ID       TO PRQ-REQUEST-ID
           PERFORM 4100-READ-REQUEST
           IF SQLCODE = 100
              MOVE 'REJECTED - NO SUCH DRAFT REQUEST' TO ARP-D-RESULT
              EXIT PARAGRAPH
           END-IF
           MOVE ACT-ACCOUNT-NUMBER     TO ARP-D-ACCOUNT-NO
           MOVE PRQ-DRAFT-DATE         TO ARP-D-DRAFT-DATE
      * BR-BIL-ACR-003 Only drafts that went to the bank, only once
           EVALUATE PRQ-REQUEST-STATUS
              WHEN 'SENT'
              WHEN 'SETL'
                 CONTINUE
              WHEN 'RTND'
                 MOVE 'REJECTED - DRAFT ALREADY RETURNED'
                                       TO ARP-D-RESULT
                 EXIT PARAGRAPH
              WHEN OTHER
                 STRING 'REJECTED - DRAFT STATUS '
                        PRQ-REQUEST-STATUS
                        DELIMITED BY SIZE INTO ARP-D-RESULT
                 EXIT PARAGRAPH
           END-EVALUATE
      * BR-BIL-ACR-004 Bank must return what was drafted
           IF WS-RETURN-AMOUNT NOT = PRQ-REQUEST-AMOUNT
              MOVE 'REJECTED - AMOUNT DIFFERS FROM DRAFT'
                                       TO ARP-D-RESULT
           END-IF.

      ******************************************************************
       3900-FILE-ERROR.
           SET WS-FILE-BAD             TO TRUE
           PERFORM 6200-WRITE-MESSAGE.

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
       4100-READ-REQUEST.
           EXEC SQL
              SELECT R.ACCOUNT_ID, R.INSTR_ID, R.REQUEST_AMOUNT,
                     R.DRAFT_DATE, R.REQUEST_STATUS, R.MONEY_ID,
                     A.ACCOUNT_NUMBER
                INTO :PRQ-ACCOUNT-ID, :PRQ-INSTR-ID,
                     :PRQ-REQUEST-AMOUNT, :PRQ-DRAFT-DATE,
                     :PRQ-REQUEST-STATUS, :PRQ-MONEY-ID,
                     :ACT-ACCOUNT-NUMBER
                FROM BILDB.BIL_PAYMENT_REQUEST R
                JOIN BILDB.BIL_ACCOUNT A
                  ON A.ACCOUNT_ID = R.ACCOUNT_ID
               WHERE R.REQUEST_ID = :PRQ-REQUEST-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4100-READ-REQUEST' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4200-MARK-RETURNED.
           MOVE ACR-D-RETURN-CODE      TO PRQ-RETURN-CODE
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_REQUEST
                 SET REQUEST_STATUS = 'RTND',
                     RETURN_CODE    = :PRQ-RETURN-CODE,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE REQUEST_ID     = :PRQ-REQUEST-ID
                 AND REQUEST_STATUS = :PRQ-REQUEST-STATUS
           END-EXEC
           IF SQLCODE NOT = 0 OR SQLERRD(3) = 0
              MOVE '4200-MARK-RETURNED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4300-SET-SAVEPOINT.
           EXEC SQL
              SAVEPOINT ACHRTREC ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-SET-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-ROLLBACK-SAVEPOINT.
           EXEC SQL
              ROLLBACK TO SAVEPOINT ACHRTREC
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-ROLLBACK-SAVEPOINT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4500-FETCH-SETTLE.
           EXEC SQL
              FETCH SETL-CSR
               INTO :PRQ-REQUEST-ID, :PRQ-ACCOUNT-ID, :PRQ-INSTR-ID,
                    :PRQ-REQUEST-AMOUNT, :PRQ-DRAFT-DATE,
                    :PRQ-FILE-REF, :ACT-ACCOUNT-NUMBER,
                    :INV-INVOICE-NUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4500-FETCH-SETTLE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4600-MARK-SETTLED.
           EXEC SQL
              UPDATE BILDB.BIL_PAYMENT_REQUEST
                 SET REQUEST_STATUS = 'SETL',
                     MONEY_ID       = :PRQ-MONEY-ID,
                     UPDATE_TS      = CURRENT TIMESTAMP
               WHERE REQUEST_ID     = :PRQ-REQUEST-ID
                 AND REQUEST_STATUS = 'SENT'
           END-EXEC
           IF SQLCODE NOT = 0 OR SQLERRD(3) = 0
              MOVE '4600-MARK-SETTLED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Calculations
      ******************************************************************
      * BR-BIL-ACR-008 Cut-off = the business day WINDOW-DAYS business
      * days before today: walk back a day at a time and count the
      * days that BILDATES DTBUSD leaves where they are.
       5000-CALC-CUTOFF.
           MOVE WS-BUS-DATE            TO WS-PROBE-DATE
           MOVE 0                      TO WS-BUS-DAYS
                                          WS-PROBES
           PERFORM UNTIL WS-BUS-DAYS >= WS-WINDOW-DAYS
                      OR WS-PROBES >= WS-MAX-LOOKBACK
              ADD 1                    TO WS-PROBES
              INITIALIZE WS-DTE-CA
              MOVE 'DTADDD  '          TO DTA-REQUEST-ID
              MOVE WS-PROBE-DATE       TO DTA-DT-DATE-1
              MOVE -1                  TO DTA-DT-NUMBER
              PERFORM 8200-CALL-DATES
              MOVE DTA-DT-RESULT-DATE  TO WS-PROBE-DATE
              INITIALIZE WS-DTE-CA
              MOVE 'DTBUSD  '          TO DTA-REQUEST-ID
              MOVE WS-PROBE-DATE       TO DTA-DT-DATE-1
              PERFORM 8200-CALL-DATES
              IF DTA-DT-RESULT-DATE = WS-PROBE-DATE
                 ADD 1                 TO WS-BUS-DAYS
              END-IF
           END-PERFORM
           IF WS-BUS-DAYS < WS-WINDOW-DAYS
              DISPLAY 'BILACHRT NO BUSINESS DAYS IN LOOKBACK FROM '
                      WS-BUS-DATE
              PERFORM 9800-ABEND
           END-IF
           MOVE WS-PROBE-DATE          TO WS-CUTOFF-DATE.

      ******************************************************************
       5100-SET-RC-WARNING.
           IF WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF.

       5200-SET-RC-REJECT.
           IF WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
      * Report
      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM ARP-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO ARP-H1-PAGE
           WRITE REPORT-REC FROM ARP-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM ARP-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM ARP-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE SPACES                 TO ARP-M-TEXT.

       6300-WRITE-TOTAL.
           WRITE REPORT-REC FROM ARP-TOTAL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6900-WRITE-CONTROL-TOTALS.
           PERFORM 6100-WRITE-HEADINGS
           MOVE 'RETURNS AND SETTLEMENT CONTROL TOTALS' TO ARP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'RETURNS IN FILE (PROVED)' TO ARP-T-LABEL
           MOVE WS-PF-DETAILS          TO ARP-T-COUNT
           COMPUTE ARP-T-AMOUNT = WS-PF-PENCE / 100
           PERFORM 6300-WRITE-TOTAL
           MOVE 'RETURNS PROCESSED THIS RUN' TO ARP-T-LABEL
           MOVE WS-CNT-RETURNS         TO ARP-T-COUNT
           MOVE WS-AMT-RETURNS         TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  SKIPPED - DONE BEFORE RESTART' TO ARP-T-LABEL
           MOVE WS-CNT-RTN-SKIPPED     TO ARP-T-COUNT
           MOVE 0                      TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  SETTLED RECEIPT REVERSED NSF' TO ARP-T-LABEL
           MOVE WS-CNT-RTN-REVERSED    TO ARP-T-COUNT
           MOVE WS-AMT-RTN-REVERSED    TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  RETURNED BEFORE SETTLEMENT' TO ARP-T-LABEL
           MOVE WS-CNT-RTN-UNSETTLED   TO ARP-T-COUNT
           MOVE WS-AMT-RTN-UNSETTLED   TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  REJECTED' TO ARP-T-LABEL
           MOVE WS-CNT-RTN-REJECTED    TO ARP-T-COUNT
           MOVE WS-AMT-RTN-REJECTED    TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  MANDATES WITHDRAWN (R02/R03)' TO ARP-T-LABEL
           MOVE WS-CNT-MANDATE-OFF     TO ARP-T-COUNT
           MOVE 0                      TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'DRAFTS DUE FOR SETTLEMENT' TO ARP-T-LABEL
           MOVE WS-CNT-SETTLE-READ     TO ARP-T-COUNT
           COMPUTE ARP-T-AMOUNT = WS-AMT-SETTLED + WS-AMT-SETTLE-SUSP
                                + WS-AMT-SETTLE-REJECTED
           PERFORM 6300-WRITE-TOTAL
           MOVE '  SETTLED TO ACCOUNT' TO ARP-T-LABEL
           MOVE WS-CNT-SETTLED         TO ARP-T-COUNT
           MOVE WS-AMT-SETTLED         TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  SETTLED TO SUSPENSE' TO ARP-T-LABEL
           MOVE WS-CNT-SETTLE-SUSP     TO ARP-T-COUNT
           MOVE WS-AMT-SETTLE-SUSP     TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE '  NOT SETTLED - RETRY NEXT RUN' TO ARP-T-LABEL
           MOVE WS-CNT-SETTLE-REJECTED TO ARP-T-COUNT
           MOVE WS-AMT-SETTLE-REJECTED TO ARP-T-AMOUNT
           PERFORM 6300-WRITE-TOTAL
           MOVE 'COMMITS'              TO ARP-T-LABEL
           MOVE WS-CNT-COMMITS         TO ARP-T-COUNT
           MOVE 0                      TO ARP-T-AMOUNT
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
           MOVE WS-PHASE               TO CKP-PHASE
           MOVE WS-LAST-KEY            TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO CKP-RESTART-KEY(1:9)
           MOVE WS-FILE-REF            TO CKP-BATCH-REF
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           COMPUTE CKP-READ-COUNT = WS-CNT-RETURNS + WS-CNT-SETTLE-READ
           SET CKP-RUNNING             TO TRUE
           IF WS-PHASE-SETTLE AND WS-EOF
              SET CKP-COMPLETE         TO TRUE
           END-IF
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILACHRT CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * Calls
      ******************************************************************
       8000-REVERSE-RECEIPT.
           INITIALIZE WS-PAY-CA
           MOVE 'PAYREV  '             TO PYA-REQUEST-ID
           PERFORM 8900-FILL-PAY-HEADER
           MOVE WS-PRIOR-MONEY-ID      TO PYA-PY-MONEY-ID
           MOVE 'NSF     '             TO PYA-PY-REVERSAL-REASON
           CALL WS-PAY-PGM USING WS-PAY-CA.

      ******************************************************************
      * INSFAIL / INSDEA on the draft's instrument. RC 24 means it is
      * already withdrawn, which is the state wanted; RC 08 means the
      * instrument has gone. Neither stops the return being booked.
      * Any other failure backs the whole return out (WS-PIN-FAILED).
       8100-CALL-INSTRUMENT.
           SET WS-PIN-DONE             TO TRUE
           INITIALIZE WS-PIN-CA
           MOVE WS-PIN-REQUEST         TO PIA-REQUEST-ID
           MOVE WS-BATCH-USER          TO PIA-USER-ID
           MOVE WS-BUS-DATE            TO PIA-BUS-DATE
           MOVE WS-ERR-KEY             TO PIA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO PIA-CALLER-PGM
           MOVE PRQ-INSTR-ID           TO PIA-PI-INSTR-ID
           MOVE PRQ-ACCOUNT-ID         TO PIA-PI-ACCOUNT-ID
           CALL WS-PIN-PGM USING WS-PIN-CA
           EVALUATE TRUE
              WHEN PIA-RETURN-CODE >= 90
                 DISPLAY 'BILACHRT BILINSTR RC ' PIA-RETURN-CODE
                         ' ' PIA-REASON-CODE ' ' PIA-MESSAGE
                 PERFORM 9800-ABEND
              WHEN PIA-RC-OK OR PIA-RC-WARNING
                 CONTINUE
              WHEN PIA-RC-STATE OR PIA-RC-NOT-FOUND
                 MOVE SPACES           TO ARP-M-TEXT
                 STRING 'REQUEST ' WS-ERR-KEY(1:9) ' '
                        PIA-REQUEST-ID ' ' PIA-REASON-CODE ' '
                        PIA-MESSAGE DELIMITED BY SIZE
                        INTO ARP-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
              WHEN OTHER
                 PERFORM 4400-ROLLBACK-SAVEPOINT
                 SET WS-PIN-FAILED     TO TRUE
                 MOVE SPACES           TO ARP-D-RESULT
                 STRING 'NOT RETURNED ' PIA-REASON-CODE ' '
                        PIA-MESSAGE DELIMITED BY SIZE
                        INTO ARP-D-RESULT
           END-EVALUATE.

      ******************************************************************
       8200-CALL-DATES.
           MOVE WS-BATCH-USER          TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              DISPLAY 'BILACHRT BILDATES ' DTA-REQUEST-ID ' RC '
                      DTA-RETURN-CODE ' ' DTA-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-BIL-ACR-008 Settled money is an ACH receipt on the draft
      * date, referenced by its request, tied to its instrument
       8300-RECEIVE-DRAFT.
           INITIALIZE WS-PAY-CA
           MOVE 'PAYRCV  '             TO PYA-REQUEST-ID
           PERFORM 8900-FILL-PAY-HEADER
           MOVE PRQ-ACCOUNT-ID         TO PYA-PY-ACCOUNT-ID
           MOVE ACT-ACCOUNT-NUMBER     TO PYA-PY-ACCOUNT-NUMBER
           MOVE INV-INVOICE-NUMBER     TO PYA-PY-INVOICE-NUMBER
           MOVE PRQ-REQUEST-AMOUNT     TO PYA-PY-AMOUNT
           MOVE 'DDBT'                 TO PYA-PY-METHOD
           MOVE 'ACH '                 TO PYA-PY-SOURCE
           STRING 'PRQ' WS-KEY-NUM DELIMITED BY SIZE
                  INTO PYA-PY-REF-NUMBER
           MOVE PRQ-DRAFT-DATE         TO PYA-PY-RECEIVED-DATE
           MOVE PRQ-INSTR-ID           TO PYA-PY-INSTR-ID
           MOVE PRQ-FILE-REF           TO PYA-PY-BATCH-REF
           CALL WS-PAY-PGM USING WS-PAY-CA.

      ******************************************************************
       8900-FILL-PAY-HEADER.
           MOVE WS-BATCH-USER          TO PYA-USER-ID
           MOVE WS-BUS-DATE            TO PYA-BUS-DATE
           MOVE WS-ERR-KEY             TO PYA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO PYA-CALLER-PGM.

      ******************************************************************
       9000-TERMINATE.
           IF WS-FILE-GOOD
              PERFORM 7000-COMMIT
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           DISPLAY 'BILACHRT COMPLETE RETURNS=' WS-CNT-RETURNS
                   ' SETTLED=' WS-CNT-SETTLED
                   ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Unrecoverable: back out the open unit of work, keep the last
      * checkpoint for restart.
       9800-ABEND.
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE SETL-CSR END-EXEC
              MOVE 'N'                 TO WS-CURSOR-FLAG
           END-IF
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO ARP-M-TEXT
           STRING '*** BILACHRT ABENDED - WORK SINCE LAST CHECKPOINT '
                  'ROLLED BACK. LAST KEY ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO ARP-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILACHRT DB2 ERROR ' WS-SQLCODE-DISP ' IN '
                   WS-ERR-PARA ' KEY ' WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           STRING 'BILACHRT Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9800-ABEND.
