       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILEVTDR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILEVTDR                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Billing event drainer (docs/EVENTS.md).          *
      *               Phase 1 delivers outbound events for ClaimsCore  *
      *               by inserting an inbound row into                 *
      *               CLMDB.CLM_EVENT_QUEUE. Phase 2 writes CANCREQ    *
      *               events for GenApp to the operations work file.   *
      *               Phase 3 processes inbound events: CLMPAYMT       *
      *               (claim payment disbursement), DEDBILL (one-time  *
      *               deductible charge) and the POLISSUE / POLCHNG /  *
      *               POLCANC events BILGAPIN queued for retry.        *
      *               Events are claimed with BILEVTQ EVGET and closed *
      *               with EVDONE or EVFAIL.                           *
      * PARM        : 1-4 commit frequency (default 0100)              *
      *               6-15 business date override yyyy-mm-dd           *
      * FILES       : CHKPT (O) OPSOUT (O) RPTOUT (O). The queue status*
      *               is the restart point; failures held in PROC by an*
      *               abended run are reclaimed by BILEVTQ after 30    *
      *               minutes.                                         *
      * CALLS       : BILEVTQ BILDISB0 BILPPER0 BILACCT0 BILISCHD      *
      *               BILGLPST BILSEQGN CLMSEQGN BILERRLG              *
      * TABLES      : BIL_POLICY_PERIOD (R) BIL_CHARGE (C R)           *
      *               BIL_CONTROL (R) CLMDB.CLM_EVENT_QUEUE (C) - the  *
      *               one cross-application write allowed              *
      * RULES       : BR-BIL-EVD-001 Outbound CLMS events are delivered*
      *                              as NEW inbound rows in the claims *
      *                              queue, keyed by CLMSEQGN EVQ,     *
      *                              then marked DONE                  *
      *               BR-BIL-EVD-002 CANCREQ to GENA goes to the       *
      *                              operations file (no GenApp API)   *
      *               BR-BIL-EVD-003 CLMPAYMT raises a CLMPAY          *
      *                              disbursement, source CLMS, ref =  *
      *                              check id, on the policy's billing *
      *                              account when one exists, else 0   *
      *               BR-BIL-EVD-004 A CLMPAYMT already disbursed      *
      *                              (DSBDUP) is complete, not failed  *
      *               BR-BIL-EVD-005 DEDBILL bills a one-time DEDUCT   *
      *                              charge on the in-force period,    *
      *                              once per claim, DR UNBILLED / CR  *
      *                              CLMPAY, then rebalances           *
      *               BR-BIL-EVD-006 POLISSUE retry needs an existing  *
      *                              billing account for the customer  *
      *               BR-BIL-EVD-007 POLCHNG passes the full-term delta*
      *                              to PPCHG; zero delta is complete  *
      *               BR-BIL-EVD-008 Failed events stay claimed until  *
      *                              the phase ends, then EVFAIL, so   *
      *                              one bad event never blocks the    *
      *                              queue for the rest of the run     *
      *               BR-BIL-EVD-009 Each event is a savepoint; a      *
      *                              failure undoes only that event    *
      * RETURN CODE : 0 ok, 4 events failed (retried), 8 events parked *
      *               in ERRR, 16 abend                                *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECKPOINT-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CKP-STATUS.
           SELECT OPS-FILE ASSIGN TO OPSOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-OPS-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CHECKPOINT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CHECKPOINT-REC.
           COPY BILDSBK1.
       FD  OPS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OPS-REC.
           COPY BILEVTO1.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILEVTDR'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-EVT-PGM                  PIC X(8)  VALUE 'BILEVTQ '.
       01  WS-DSB-PGM                  PIC X(8)  VALUE 'BILDISB0'.
       01  WS-PPR-PGM                  PIC X(8)  VALUE 'BILPPER0'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-SCH-PGM                  PIC X(8)  VALUE 'BILISCHD'.
       01  WS-GLP-PGM                  PIC X(8)  VALUE 'BILGLPST'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-CSQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.

       01  WS-FILE-STATUS.
           05 WS-CKP-STATUS            PIC XX.
           05 WS-OPS-STATUS            PIC XX.
           05 WS-RPT-STATUS            PIC XX.

       01  WS-CONSTANTS.
           05 WS-BATCH-USER            PIC X(8)  VALUE 'BILBATCH'.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 55.
           05 WS-HOLD-MAX              PIC S9(4) COMP VALUE 200.

       01  WS-CONTROL.
           05 WS-COMMIT-FREQ           PIC S9(9) COMP VALUE 100.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-PHASE-TARGET          PIC X(4).
           05 WS-PHASE-DIRECTION       PIC X.
           05 WS-PHASE-END-FLAG        PIC X     VALUE 'N'.
              88 WS-PHASE-END           VALUE 'Y'.
           05 WS-OUTCOME               PIC X.
              88 WS-EVENT-OK            VALUE 'Y'.
              88 WS-EVENT-FAILED        VALUE 'N'.
           05 WS-RESULT                PIC X(76).
           05 WS-FAIL-TEXT             PIC X(80).
           05 WS-RETURN-CODE           PIC S9(4) COMP VALUE 0.
           05 WS-SINCE-COMMIT          PIC S9(9) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-PAGE-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-NUM               PIC 9(10).
           05 WS-TX                    PIC S9(4) COMP.
           05 WS-HX                    PIC S9(4) COMP.
           05 WS-AMOUNT                PIC S9(11)V99 COMP-3.
           05 WS-AMOUNT-DISP           PIC -(10)9.99.
           05 WS-ACCOUNT-ID            PIC S9(9) COMP.
           05 WS-DUP-COUNT             PIC S9(9) COMP.

      * Failed events held (still PROC) until the phase ends
       01  WS-HOLD-TABLE.
           05 WS-HOLD-COUNT            PIC S9(4) COMP VALUE 0.
      * Entry 201 is the overflow slot for an immediate EVFAIL
           05 WS-HOLD-ENTRY OCCURS 201 TIMES.
              10 WS-HOLD-EVENT-ID      PIC S9(9) COMP.
              10 WS-HOLD-TYPE-IX       PIC S9(4) COMP.
              10 WS-HOLD-ERROR         PIC X(80).

      * Per event type counts for the report
       01  WS-TYPE-NAMES.
           05 FILLER                   PIC X(8)  VALUE 'CLMPAYMT'.
           05 FILLER                   PIC X(8)  VALUE 'DEDBILL '.
           05 FILLER                   PIC X(8)  VALUE 'POLISSUE'.
           05 FILLER                   PIC X(8)  VALUE 'POLCHNG '.
           05 FILLER                   PIC X(8)  VALUE 'POLCANC '.
           05 FILLER                   PIC X(8)  VALUE 'BILSTAT '.
           05 FILLER                   PIC X(8)  VALUE 'DSBSENT '.
           05 FILLER                   PIC X(8)  VALUE 'CANCREQ '.
           05 FILLER                   PIC X(8)  VALUE 'OTHER   '.
       01  WS-TYPE-TABLE REDEFINES WS-TYPE-NAMES.
           05 WS-TYPE-NAME             PIC X(8) OCCURS 9 TIMES.
       01  WS-TYPE-COUNTS.
           05 WS-TYPE-ENTRY OCCURS 9 TIMES.
              10 WS-TC-DONE            PIC S9(7) COMP.
              10 WS-TC-FAILED          PIC S9(7) COMP.
              10 WS-TC-PARKED          PIC S9(7) COMP.
       01  WS-TYPE-IX                  PIC S9(4) COMP VALUE 0.
       01  WS-CNT-COMMITS              PIC S9(9) COMP VALUE 0.
       01  WS-CNT-EVENTS               PIC S9(9) COMP VALUE 0.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPPR END-EXEC.
           EXEC SQL INCLUDE BILDCHG END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

      * ClaimsCore inbound queue (owned by claims; insert only)
           EXEC SQL DECLARE CLMDB.CLM_EVENT_QUEUE TABLE
           ( EVENT_ID             INTEGER        NOT NULL
           , EVENT_TYPE           CHAR(8)        NOT NULL
           , DIRECTION            CHAR(1)        NOT NULL
           , SOURCE_SYSTEM        CHAR(4)        NOT NULL
           , TARGET_SYSTEM        CHAR(4)        NOT NULL
           , EVENT_KEY            CHAR(20)       NOT NULL
           , PAYLOAD              VARCHAR(1000)  NOT NULL
           , EVENT_STATUS         CHAR(4)        NOT NULL
           , RETRY_COUNT          SMALLINT       NOT NULL
           , ERROR_TEXT           VARCHAR(80)
           , CREATE_TS            TIMESTAMP      NOT NULL
           , PROCESSED_TS         TIMESTAMP
           ) END-EXEC.
       01  CEV-ROW.
           05 CEV-EVENT-ID             PIC S9(9) COMP.
           05 CEV-EVENT-TYPE           PIC X(8).
           05 CEV-DIRECTION            PIC X.
           05 CEV-SOURCE-SYSTEM        PIC X(4).
           05 CEV-TARGET-SYSTEM        PIC X(4).
           05 CEV-EVENT-KEY            PIC X(20).
           05 CEV-PAYLOAD.
              49 CEV-PAYLOAD-LEN       PIC S9(4) COMP.
              49 CEV-PAYLOAD-TEXT      PIC X(1000).

       01  WS-EVT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==EVA-==.
           03 EVA-BODY.
              COPY BILCEVT REPLACING LEADING ==CA-== BY ==EVA-==.
       01  WS-DSB-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DBA-==.
           03 DBA-BODY.
              COPY BILCDSB REPLACING LEADING ==CA-== BY ==DBA-==.
       01  WS-PPR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==PPA-==.
           03 PPA-BODY.
              COPY BILCPPR REPLACING LEADING ==CA-== BY ==PPA-==.
       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.
       01  WS-SCH-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SCA-==.
           03 SCA-BODY.
              COPY BILCSCH REPLACING LEADING ==CA-== BY ==SCA-==.
       01  WS-GLP-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==GLA-==.
           03 GLA-BODY.
              COPY BILCGLP REPLACING LEADING ==CA-== BY ==GLA-==.
       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
      * CLMSEQGN takes CLMCAHDR + CLMCSEQ, byte-for-byte identical to
      * BILCAHDR + BILCSEQ, so the billing copybooks describe it.
       01  WS-CSQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==CSA-==.
           03 CSA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==CSA-==.
       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILEVTP1.
           COPY BILEVTR1.

       LINKAGE SECTION.
       01  LS-PARM.
           05 LS-PARM-LEN              PIC S9(4) COMP.
           05 LS-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LS-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           MOVE 'CLMS'                 TO WS-PHASE-TARGET
           MOVE 'O'                    TO WS-PHASE-DIRECTION
           PERFORM 2000-DRAIN-PHASE
           MOVE 'GENA'                 TO WS-PHASE-TARGET
           MOVE 'O'                    TO WS-PHASE-DIRECTION
           PERFORM 2000-DRAIN-PHASE
           MOVE 'BILL'                 TO WS-PHASE-TARGET
           MOVE 'I'                    TO WS-PHASE-DIRECTION
           PERFORM 2000-DRAIN-PHASE
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT CHECKPOINT-FILE
                       OPS-FILE
                       REPORT-FILE
           IF WS-CKP-STATUS NOT = '00' OR WS-OPS-STATUS NOT = '00'
              OR WS-RPT-STATUS NOT = '00'
              DISPLAY 'BILEVTDR OPEN FAILED CHKPT=' WS-CKP-STATUS
                      ' OPSOUT=' WS-OPS-STATUS ' RPTOUT=' WS-RPT-STATUS
              MOVE 16                  TO RETURN-CODE
              GOBACK
           END-IF
           INITIALIZE WS-TYPE-COUNTS
           IF LS-PARM-LEN >= 4 AND LS-PARM-DATA(1:4) IS NUMERIC
              MOVE LS-PARM-DATA(1:4)   TO WS-COMMIT-FREQ
              IF WS-COMMIT-FREQ = 0
                 MOVE 100              TO WS-COMMIT-FREQ
              END-IF
           END-IF
           IF LS-PARM-LEN >= 15 AND LS-PARM-DATA(6:10) NOT = SPACES
              MOVE LS-PARM-DATA(6:10)  TO WS-BUS-DATE
           ELSE
              PERFORM 4000-READ-BUSINESS-DATE
           END-IF
           MOVE WS-BUS-DATE            TO EDR-H1-BUS-DATE.

      ******************************************************************
      * One queue phase: claim, process, close every event for the
      * target/direction, then release the held failures.
       2000-DRAIN-PHASE.
           MOVE SPACES                 TO EDR-M-TEXT
           STRING '--- PHASE: TARGET ' WS-PHASE-TARGET
                  ' DIRECTION ' WS-PHASE-DIRECTION ' ---'
                  DELIMITED BY SIZE INTO EDR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE 'N'                    TO WS-PHASE-END-FLAG
           MOVE 0                      TO WS-HOLD-COUNT
           PERFORM UNTIL WS-PHASE-END
              PERFORM 8000-GET-EVENT
              IF NOT WS-PHASE-END
                 PERFORM 2100-HANDLE-EVENT
                 ADD 1                 TO WS-SINCE-COMMIT
                 IF WS-SINCE-COMMIT >= WS-COMMIT-FREQ
                    PERFORM 7000-COMMIT
                 END-IF
              END-IF
           END-PERFORM
           PERFORM 2900-RELEASE-FAILURES
           PERFORM 7000-COMMIT.

       2100-HANDLE-EVENT.
           ADD 1                       TO WS-CNT-EVENTS
           MOVE EVA-EV-EVENT-ID        TO WS-KEY-NUM
           MOVE WS-KEY-NUM             TO WS-ERR-KEY
           PERFORM 5000-FIND-TYPE-INDEX
           SET WS-EVENT-OK             TO TRUE
           MOVE SPACES                 TO WS-RESULT WS-FAIL-TEXT
      * BR-BIL-EVD-009 Savepoint after the claim: a rollback keeps the
      * event claimed (PROC) and undoes only its processing
           EXEC SQL
              SAVEPOINT EVTDR_EVT ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-HANDLE-EVENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EVALUATE TRUE
              WHEN WS-PHASE-TARGET = 'CLMS'
                 PERFORM 3000-DELIVER-TO-CLAIMS
              WHEN WS-PHASE-TARGET = 'GENA'
                 PERFORM 3100-WRITE-OPERATIONS
              WHEN EVA-EV-TYPE = 'CLMPAYMT'
                 PERFORM 3200-CLAIM-PAYMENT
              WHEN EVA-EV-TYPE = 'DEDBILL '
                 PERFORM 3300-DEDUCTIBLE-BILL
              WHEN EVA-EV-TYPE = 'POLISSUE'
                 PERFORM 3400-POLICY-ISSUE
              WHEN EVA-EV-TYPE = 'POLCHNG '
                 PERFORM 3500-POLICY-CHANGE
              WHEN EVA-EV-TYPE = 'POLCANC '
                 PERFORM 3600-POLICY-CANCEL
              WHEN OTHER
                 SET WS-EVENT-FAILED   TO TRUE
                 STRING 'No inbound handler for ' EVA-EV-TYPE
                        DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-EVALUATE
           INITIALIZE EDR-DETAIL
           MOVE EVA-EV-EVENT-ID        TO EDR-D-EVENT-ID
           MOVE EVA-EV-TYPE            TO EDR-D-TYPE
           MOVE EVA-EV-SOURCE          TO EDR-D-SOURCE
           MOVE WS-PHASE-TARGET        TO EDR-D-TARGET
           MOVE EVA-EV-KEY             TO EDR-D-KEY
           IF WS-EVENT-OK
              PERFORM 8100-EVENT-DONE
              ADD 1                    TO WS-TC-DONE(WS-TYPE-IX)
           ELSE
              EXEC SQL
                 ROLLBACK TO SAVEPOINT EVTDR_EVT
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '2100-HANDLE-EVENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              PERFORM 2200-HOLD-FAILURE
              MOVE WS-FAIL-TEXT        TO WS-RESULT
           END-IF
           MOVE WS-RESULT              TO EDR-D-RESULT
           PERFORM 6000-WRITE-DETAIL.

      * BR-BIL-EVD-008 Hold the failure; a full table fails at once
      * and ends the phase so the event is not claimed again
       2200-HOLD-FAILURE.
           ADD 1                       TO WS-TC-FAILED(WS-TYPE-IX)
           IF WS-HOLD-COUNT < WS-HOLD-MAX
              ADD 1                    TO WS-HOLD-COUNT
              MOVE EVA-EV-EVENT-ID TO WS-HOLD-EVENT-ID(WS-HOLD-COUNT)
              MOVE WS-TYPE-IX      TO WS-HOLD-TYPE-IX(WS-HOLD-COUNT)
              MOVE WS-FAIL-TEXT    TO WS-HOLD-ERROR(WS-HOLD-COUNT)
           ELSE
              COMPUTE WS-HX = WS-HOLD-MAX + 1
              MOVE EVA-EV-EVENT-ID     TO WS-HOLD-EVENT-ID(WS-HX)
              MOVE WS-TYPE-IX          TO WS-HOLD-TYPE-IX(WS-HX)
              MOVE WS-FAIL-TEXT        TO WS-HOLD-ERROR(WS-HX)
              PERFORM 8200-EVENT-FAIL
              MOVE 'FAILURE TABLE FULL - PHASE ENDED EARLY'
                                       TO EDR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              SET WS-PHASE-END         TO TRUE
           END-IF
           IF WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF.

       2900-RELEASE-FAILURES.
           PERFORM VARYING WS-HX FROM 1 BY 1
                   UNTIL WS-HX > WS-HOLD-COUNT
              PERFORM 8200-EVENT-FAIL
           END-PERFORM
           MOVE 0                      TO WS-HOLD-COUNT.

      ******************************************************************
      * BR-BIL-EVD-001 Deliver to ClaimsCore as a NEW inbound row
       3000-DELIVER-TO-CLAIMS.
           PERFORM 8600-NEXT-CLAIMS-EVENT-ID
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           MOVE EVA-EV-TYPE            TO CEV-EVENT-TYPE
           MOVE 'I'                    TO CEV-DIRECTION
           MOVE 'BILL'                 TO CEV-SOURCE-SYSTEM
           MOVE 'CLMS'                 TO CEV-TARGET-SYSTEM
           MOVE EVA-EV-KEY             TO CEV-EVENT-KEY
           MOVE EVA-EV-PAYLOAD         TO CEV-PAYLOAD-TEXT
           MOVE 0                      TO WS-TX
           INSPECT FUNCTION REVERSE(EVA-EV-PAYLOAD)
                   TALLYING WS-TX FOR LEADING SPACE
           COMPUTE CEV-PAYLOAD-LEN = 1000 - WS-TX
           EXEC SQL
              INSERT INTO CLMDB.CLM_EVENT_QUEUE
                   ( EVENT_ID, EVENT_TYPE, DIRECTION, SOURCE_SYSTEM,
                     TARGET_SYSTEM, EVENT_KEY, PAYLOAD, EVENT_STATUS,
                     RETRY_COUNT, ERROR_TEXT, CREATE_TS, PROCESSED_TS )
              VALUES
                   ( :CEV-EVENT-ID, :CEV-EVENT-TYPE, :CEV-DIRECTION,
                     :CEV-SOURCE-SYSTEM, :CEV-TARGET-SYSTEM,
                     :CEV-EVENT-KEY, :CEV-PAYLOAD, 'NEW ', 0, NULL,
                     CURRENT TIMESTAMP, NULL )
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE CEV-EVENT-ID     TO WS-KEY-NUM
                 STRING 'DELIVERED TO CLM_EVENT_QUEUE ID ' WS-KEY-NUM
                        DELIMITED BY SIZE INTO WS-RESULT
              WHEN -803
              WHEN -911
              WHEN -913
                 MOVE SQLCODE          TO WS-SQLCODE-DISP
                 SET WS-EVENT-FAILED   TO TRUE
                 STRING 'Claims queue insert SQLCODE ' WS-SQLCODE-DISP
                        DELIMITED BY SIZE INTO WS-FAIL-TEXT
              WHEN OTHER
                 MOVE '3000-DELIVER-TO-CLAIMS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      * BR-BIL-EVD-002 GenApp has no inbound API: operations work it
       3100-WRITE-OPERATIONS.
           IF EVA-EV-TYPE NOT = 'CANCREQ '
              SET WS-EVENT-FAILED      TO TRUE
              STRING 'No GenApp handler for ' EVA-EV-TYPE
                     DELIMITED BY SIZE INTO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE EVA-EV-PAYLOAD         TO EVP-CANCREQ
           IF EVP-CR-POLICY-NO NOT NUMERIC
              OR EVP-CR-PASTDUE NOT NUMERIC
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'CANCREQ payload not numeric' TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           INITIALIZE OPS-REC
           MOVE EVA-EV-EVENT-ID        TO OPS-EVENT-ID
           MOVE EVA-EV-TYPE            TO OPS-EVENT-TYPE
           MOVE EVP-CR-POLICY-NO       TO OPS-POLICY-NO
           MOVE EVP-CR-CANCEL-DATE     TO OPS-CANCEL-DATE
           MOVE EVP-CR-REASON          TO OPS-REASON
           MOVE EVP-CR-PASTDUE         TO OPS-PASTDUE
           MOVE WS-BUS-DATE            TO OPS-BUS-DATE
           MOVE 'CANCEL POLICY IN GENAPP FOR NON-PAYMENT'
                                       TO OPS-ACTION
           WRITE OPS-REC
           IF WS-OPS-STATUS NOT = '00'
              DISPLAY 'BILEVTDR OPSOUT WRITE FAILED ' WS-OPS-STATUS
              PERFORM 9800-ABEND
           END-IF
           MOVE 'WRITTEN TO OPERATIONS FILE' TO WS-RESULT.

      ******************************************************************
      * BR-BIL-EVD-003 Claim payment -> CLMPAY disbursement
       3200-CLAIM-PAYMENT.
           MOVE EVA-EV-PAYLOAD         TO EVP-CLMPAYMT
           IF EVP-CP-CHECK-ID NOT NUMERIC
              OR EVP-CP-POLICY-NO NOT NUMERIC
              OR EVP-CP-AMOUNT NOT NUMERIC
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'CLMPAYMT payload not numeric' TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE EVP-CP-POLICY-NO       TO PPR-GENAPP-POL-NUM
           PERFORM 4100-READ-LATEST-PERIOD
           IF SQLCODE = 0
              MOVE PPR-ACCOUNT-ID      TO WS-ACCOUNT-ID
           ELSE
              MOVE 0                   TO WS-ACCOUNT-ID
           END-IF
           INITIALIZE WS-DSB-CA
           MOVE 'DSBCRT  '             TO DBA-REQUEST-ID
           MOVE WS-BATCH-USER          TO DBA-USER-ID
           MOVE WS-BUS-DATE            TO DBA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DBA-CALLER-PGM
           MOVE WS-ACCOUNT-ID          TO DBA-DB-ACCOUNT-ID
           MOVE EVP-CP-AMOUNT          TO DBA-DB-AMOUNT
           MOVE 'CLMPAY  '             TO DBA-DB-REASON
           MOVE EVP-CP-PAYEE-NAME      TO DBA-DB-PAYEE-NAME
           MOVE EVP-CP-PAYEE-ADDR1     TO DBA-DB-PAYEE-ADDR1
           MOVE EVP-CP-PAYEE-POSTCODE  TO DBA-DB-PAYEE-POSTCODE
           IF EVP-CP-PAY-METHOD = 'CHEQ' OR EVP-CP-PAY-METHOD = 'CHK '
              MOVE 'CHEQ'              TO DBA-DB-PAY-METHOD
           ELSE
              MOVE 'BACS'              TO DBA-DB-PAY-METHOD
           END-IF
           IF EVP-CP-SCHED-DATE > WS-BUS-DATE
              MOVE EVP-CP-SCHED-DATE   TO DBA-DB-DUE-DATE
           ELSE
              MOVE WS-BUS-DATE         TO DBA-DB-DUE-DATE
           END-IF
           MOVE 'CLMS'                 TO DBA-DB-SOURCE-SYSTEM
           MOVE EVP-CP-CHECK-ID        TO DBA-DB-SOURCE-REF
           CALL WS-DSB-PGM USING WS-DSB-CA
           EVALUATE TRUE
              WHEN DBA-RC-OK OR DBA-RC-WARNING
                 MOVE DBA-DB-DISB-ID   TO WS-KEY-NUM
                 STRING 'DISBURSEMENT ' WS-KEY-NUM ' '
                        DBA-DB-STATUS ' CLAIM ' EVP-CP-CLAIM-NO
                        DELIMITED BY SIZE INTO WS-RESULT
      * BR-BIL-EVD-004 Already disbursed is a completed event
              WHEN DBA-RC-CONFLICT AND DBA-REASON-CODE = 'DSBDUP  '
                 MOVE 'ALREADY DISBURSED - NO ACTION' TO WS-RESULT
              WHEN DBA-RETURN-CODE >= 90
                 MOVE DBA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
              WHEN OTHER
                 SET WS-EVENT-FAILED   TO TRUE
                 STRING DBA-REASON-CODE ' ' DBA-MESSAGE
                        DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-EVD-005 Deductible owed by the insured, billed once
       3300-DEDUCTIBLE-BILL.
           MOVE EVA-EV-PAYLOAD         TO EVP-DEDBILL
           IF EVP-DB-POLICY-NO NOT NUMERIC
              OR EVP-DB-AMOUNT NOT NUMERIC
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'DEDBILL payload not numeric' TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           IF EVP-DB-AMOUNT NOT > 0
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'Deductible amount must be positive'
                                       TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE EVP-DB-POLICY-NO       TO PPR-GENAPP-POL-NUM
           PERFORM 4200-READ-INFORCE-PERIOD
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           MOVE EVP-DB-CLAIM-NO        TO CHG-SOURCE-REF
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-DUP-COUNT
                FROM BILDB.BIL_CHARGE
               WHERE PP_ID          = :PPR-PP-ID
                 AND CHARGE_PATTERN = 'DEDUCT  '
                 AND SOURCE_REF     = :CHG-SOURCE-REF
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3300-DEDUCTIBLE-BILL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DUP-COUNT > 0
              MOVE 'DEDUCTIBLE ALREADY BILLED - NO ACTION' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           MOVE EVP-DB-AMOUNT          TO WS-AMOUNT
           PERFORM 4300-INSERT-DEDUCT-CHARGE
           PERFORM 8300-SCHEDULE-CHARGE
           IF WS-EVENT-OK
              PERFORM 8400-POST-DEDUCT-LEDGER
           END-IF
           IF WS-EVENT-OK
              PERFORM 8500-REBALANCE-ACCOUNT
           END-IF
           IF WS-EVENT-OK
              MOVE WS-AMOUNT           TO WS-AMOUNT-DISP
              MOVE CHG-CHARGE-ID       TO WS-KEY-NUM
              STRING 'DEDUCT CHARGE ' WS-KEY-NUM ' ' WS-AMOUNT-DISP
                     ' FIRST DUE ' SCA-SC-FIRST-DUE-DATE
                     DELIMITED BY SIZE INTO WS-RESULT
           END-IF.

      ******************************************************************
      * BR-BIL-EVD-006 Retry of a GenApp policy BILGAPIN rejected
       3400-POLICY-ISSUE.
           MOVE EVA-EV-PAYLOAD         TO EVP-POLISSUE
           IF EVP-PI-POLICY-NO NOT NUMERIC
              OR EVP-PI-CUSTOMER-NO NOT NUMERIC
              OR EVP-PI-PREMIUM NOT NUMERIC
              OR EVP-PI-BROKER-ID NOT NUMERIC
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'POLISSUE payload not numeric' TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTFND '             TO ACA-REQUEST-ID
           MOVE WS-BATCH-USER          TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE EVP-PI-CUSTOMER-NO     TO ACA-AC-GENAPP-CUST-NUM
           CALL WS-ACT-PGM USING WS-ACT-CA
           EVALUATE TRUE
              WHEN ACA-RC-OK OR ACA-RC-WARNING
                 CONTINUE
              WHEN ACA-RETURN-CODE >= 90
                 MOVE ACA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
              WHEN OTHER
                 SET WS-EVENT-FAILED   TO TRUE
                 STRING 'No billing account for GenApp customer '
                        EVP-PI-CUSTOMER-NO DELIMITED BY SIZE
                        INTO WS-FAIL-TEXT
                 EXIT PARAGRAPH
           END-EVALUATE
           INITIALIZE WS-PPR-CA
           MOVE 'PPNEW   '             TO PPA-REQUEST-ID
           PERFORM 8700-PRIME-PERIOD-CALL
           MOVE EVP-PI-POLICY-NO       TO PPA-PP-GENAPP-POL-NUM
           MOVE ACA-AC-ACCOUNT-ID      TO PPA-PP-ACCOUNT-ID
           MOVE 1                      TO PPA-PP-TERM-NUMBER
           MOVE EVP-PI-PRODUCT         TO PPA-PP-PRODUCT-CODE
           MOVE EVP-PI-EFF-DATE        TO PPA-PP-EFF-DATE
           MOVE EVP-PI-EXP-DATE        TO PPA-PP-EXP-DATE
           MOVE EVP-PI-PLAN-CODE       TO PPA-PP-PLAN-CODE
           MOVE EVP-PI-BILL-METHOD     TO PPA-PP-BILLING-METHOD
           MOVE EVP-PI-BROKER-ID       TO PPA-PP-PRODUCER-CODE
           MOVE EVP-PI-PREMIUM         TO PPA-PP-TOTAL-PREMIUM
           PERFORM 8800-CALL-PERIOD.

      * BR-BIL-EVD-007 Full-term delta against the in-force period
       3500-POLICY-CHANGE.
           MOVE EVA-EV-PAYLOAD         TO EVP-POLCHNG
           IF EVP-PC-POLICY-NO NOT NUMERIC
              OR EVP-PC-PREMIUM NOT NUMERIC
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'POLCHNG payload not numeric' TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE EVP-PC-POLICY-NO       TO PPR-GENAPP-POL-NUM
           PERFORM 4200-READ-INFORCE-PERIOD
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AMOUNT = EVP-PC-PREMIUM - PPR-TOTAL-PREMIUM
           IF WS-AMOUNT = 0
              MOVE 'PREMIUM ALREADY BILLED - NO CHANGE' TO WS-RESULT
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-PPR-CA
           MOVE 'PPCHG   '             TO PPA-REQUEST-ID
           PERFORM 8700-PRIME-PERIOD-CALL
           MOVE PPR-PP-ID              TO PPA-PP-PP-ID
           MOVE PPR-ACCOUNT-ID         TO PPA-PP-ACCOUNT-ID
           MOVE PPR-GENAPP-POL-NUM     TO PPA-PP-GENAPP-POL-NUM
           MOVE EVP-PC-PREMIUM         TO PPA-PP-TOTAL-PREMIUM
           MOVE WS-AMOUNT              TO PPA-PP-CHANGE-AMOUNT
           MOVE EVP-PC-EFF-DATE        TO PPA-PP-CHANGE-EFF-DATE
           PERFORM 8800-CALL-PERIOD.

      * PPCAN reads the cancel date from CHANGE-EFF-DATE
       3600-POLICY-CANCEL.
           MOVE EVA-EV-PAYLOAD         TO EVP-POLCANC
           IF EVP-PX-POLICY-NO NOT NUMERIC
              SET WS-EVENT-FAILED      TO TRUE
              MOVE 'POLCANC payload not numeric' TO WS-FAIL-TEXT
              EXIT PARAGRAPH
           END-IF
           MOVE EVP-PX-POLICY-NO       TO PPR-GENAPP-POL-NUM
           PERFORM 4200-READ-INFORCE-PERIOD
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-PPR-CA
           MOVE 'PPCAN   '             TO PPA-REQUEST-ID
           PERFORM 8700-PRIME-PERIOD-CALL
           MOVE PPR-PP-ID              TO PPA-PP-PP-ID
           MOVE PPR-ACCOUNT-ID         TO PPA-PP-ACCOUNT-ID
           MOVE PPR-GENAPP-POL-NUM     TO PPA-PP-GENAPP-POL-NUM
           MOVE EVP-PX-CANCEL-DATE     TO PPA-PP-CHANGE-EFF-DATE
           MOVE EVP-PX-REASON          TO PPA-PP-CANCEL-REASON
           PERFORM 8800-CALL-PERIOD.

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

      * Any term: the billing account the policy belongs to
       4100-READ-LATEST-PERIOD.
           EXEC SQL
              SELECT PP_ID, ACCOUNT_ID
                INTO :PPR-PP-ID, :PPR-ACCOUNT-ID
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE GENAPP_POL_NUM = :PPR-GENAPP-POL-NUM
               ORDER BY TERM_NUMBER DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4100-READ-LATEST-PERIOD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

       4200-READ-INFORCE-PERIOD.
           EXEC SQL
              SELECT PP_ID, ACCOUNT_ID, TOTAL_PREMIUM
                INTO :PPR-PP-ID, :PPR-ACCOUNT-ID, :PPR-TOTAL-PREMIUM
                FROM BILDB.BIL_POLICY_PERIOD
               WHERE GENAPP_POL_NUM = :PPR-GENAPP-POL-NUM
                 AND PP_STATUS      = 'INFC'
               ORDER BY TERM_NUMBER DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EVENT-FAILED   TO TRUE
                 MOVE PPR-GENAPP-POL-NUM TO WS-KEY-NUM
                 STRING 'No in-force billing period for policy '
                        WS-KEY-NUM DELIMITED BY SIZE
                        INTO WS-FAIL-TEXT
              WHEN OTHER
                 MOVE '4200-READ-INFORCE-PERIOD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4300-INSERT-DEDUCT-CHARGE.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE WS-BATCH-USER          TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'CHG     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 12
              MOVE SQA-MESSAGE         TO EDR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           MOVE SQA-SQ-VALUE           TO CHG-CHARGE-ID
           MOVE PPR-PP-ID              TO CHG-PP-ID
           MOVE PPR-ACCOUNT-ID         TO CHG-ACCOUNT-ID
           MOVE 'DEDUCT  '             TO CHG-CHARGE-PATTERN
           MOVE 'ONETIME '             TO CHG-BILLING-INSTR
           MOVE WS-AMOUNT              TO CHG-CHARGE-AMOUNT
           MOVE WS-BUS-DATE            TO CHG-CHARGE-DATE
                                          CHG-EFF-DATE
           MOVE WS-BATCH-USER          TO CHG-CREATE-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_CHARGE
                   ( CHARGE_ID, PP_ID, ACCOUNT_ID, CHARGE_PATTERN,
                     BILLING_INSTR, CHARGE_AMOUNT, CHARGE_DATE,
                     EFF_DATE, CHARGE_STATUS, SOURCE_REF, CREATE_USER,
                     CREATE_TS )
              VALUES
                   ( :CHG-CHARGE-ID, :CHG-PP-ID, :CHG-ACCOUNT-ID,
                     :CHG-CHARGE-PATTERN, :CHG-BILLING-INSTR,
                     :CHG-CHARGE-AMOUNT, :CHG-CHARGE-DATE,
                     :CHG-EFF-DATE, 'OPEN', :CHG-SOURCE-REF,
                     :CHG-CREATE-USER, CURRENT TIMESTAMP )
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-INSERT-DEDUCT-CHARGE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       5000-FIND-TYPE-INDEX.
           MOVE 9                      TO WS-TYPE-IX
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
              IF WS-TYPE-NAME(WS-TX) = EVA-EV-TYPE
                 MOVE WS-TX            TO WS-TYPE-IX
              END-IF
           END-PERFORM.

      ******************************************************************
       6000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM EDR-DETAIL AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

       6100-WRITE-HEADINGS.
           ADD 1                       TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT          TO EDR-H1-PAGE
           WRITE REPORT-REC FROM EDR-HEAD-1 AFTER ADVANCING PAGE
           WRITE REPORT-REC FROM EDR-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO REPORT-REC
           WRITE REPORT-REC AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

       6200-WRITE-MESSAGE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM EDR-MESSAGE AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE SPACES                 TO EDR-M-TEXT.

       6900-WRITE-CONTROL-TOTALS.
           MOVE '*** EVENTS BY TYPE ***' TO EDR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
              MOVE WS-TYPE-NAME(WS-TX) TO EDR-TT-TYPE
              MOVE WS-TC-DONE(WS-TX)   TO EDR-TT-DONE
              MOVE WS-TC-FAILED(WS-TX) TO EDR-TT-FAILED
              MOVE WS-TC-PARKED(WS-TX) TO EDR-TT-PARKED
              IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
                 PERFORM 6100-WRITE-HEADINGS
              END-IF
              WRITE REPORT-REC FROM EDR-TYPE-TOTAL
                    AFTER ADVANCING 1 LINE
              ADD 1                    TO WS-LINE-COUNT
           END-PERFORM
           MOVE WS-CNT-EVENTS          TO WS-KEY-NUM
           STRING 'EVENTS CLAIMED ' WS-KEY-NUM
                  DELIMITED BY SIZE INTO EDR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           MOVE WS-CNT-COMMITS         TO WS-KEY-NUM
           STRING 'COMMITS        ' WS-KEY-NUM
                  DELIMITED BY SIZE INTO EDR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE.

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
           STRING WS-PHASE-TARGET WS-PHASE-DIRECTION ' ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO CKP-RESTART-KEY
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-EVENTS          TO CKP-READ-COUNT
           SET CKP-RUNNING             TO TRUE
           WRITE CHECKPOINT-REC
           IF WS-CKP-STATUS NOT = '00'
              DISPLAY 'BILEVTDR CHECKPOINT WRITE FAILED '
                      WS-CKP-STATUS
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * EVGET: RC 08 means the phase's queue is empty
       8000-GET-EVENT.
           INITIALIZE WS-EVT-CA
           MOVE 'EVGET   '             TO EVA-REQUEST-ID
           PERFORM 8050-PRIME-QUEUE-CALL
           MOVE WS-PHASE-TARGET        TO EVA-EV-TARGET
           MOVE WS-PHASE-DIRECTION     TO EVA-EV-DIRECTION
           CALL WS-EVT-PGM USING WS-EVT-CA
           EVALUATE TRUE
              WHEN EVA-RC-OK
                 CONTINUE
              WHEN EVA-RC-NOT-FOUND
                 SET WS-PHASE-END      TO TRUE
              WHEN EVA-RC-CONFLICT
                 STRING 'QUEUE BUSY - PHASE ENDED: ' EVA-MESSAGE
                        DELIMITED BY SIZE INTO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 SET WS-PHASE-END      TO TRUE
                 IF WS-RETURN-CODE < 4
                    MOVE 4             TO WS-RETURN-CODE
                 END-IF
              WHEN OTHER
                 MOVE EVA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
           END-EVALUATE.

       8050-PRIME-QUEUE-CALL.
           MOVE WS-BATCH-USER          TO EVA-USER-ID
           MOVE WS-BUS-DATE            TO EVA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO EVA-CALLER-PGM.

      * The claimed event's id survives the commarea reset
       8100-EVENT-DONE.
           MOVE EVA-EV-EVENT-ID        TO WS-KEY-NUM
           INITIALIZE WS-EVT-CA
           MOVE 'EVDONE  '             TO EVA-REQUEST-ID
           PERFORM 8050-PRIME-QUEUE-CALL
           MOVE WS-KEY-NUM             TO EVA-EV-EVENT-ID
           CALL WS-EVT-PGM USING WS-EVT-CA
           IF EVA-RETURN-CODE >= 8
              MOVE EVA-MESSAGE         TO EDR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF.

      * Fifth failure parks the event in ERRR for a trouble ticket
       8200-EVENT-FAIL.
           INITIALIZE WS-EVT-CA
           MOVE 'EVFAIL  '             TO EVA-REQUEST-ID
           PERFORM 8050-PRIME-QUEUE-CALL
           MOVE WS-HOLD-EVENT-ID(WS-HX) TO EVA-EV-EVENT-ID
           MOVE WS-HOLD-ERROR(WS-HX)   TO EVA-EV-ERROR
           CALL WS-EVT-PGM USING WS-EVT-CA
           EVALUATE TRUE
              WHEN EVA-RC-OK
                 CONTINUE
              WHEN EVA-RC-WARNING AND EVA-REASON-CODE = 'EVERRR  '
                 MOVE WS-HOLD-TYPE-IX(WS-HX) TO WS-TX
                 ADD 1                 TO WS-TC-PARKED(WS-TX)
                 MOVE EVA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 IF WS-RETURN-CODE < 8
                    MOVE 8             TO WS-RETURN-CODE
                 END-IF
              WHEN OTHER
                 MOVE EVA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
           END-EVALUATE.

       8300-SCHEDULE-CHARGE.
           INITIALIZE WS-SCH-CA
           MOVE 'SCHCHG  '             TO SCA-REQUEST-ID
           MOVE WS-BATCH-USER          TO SCA-USER-ID
           MOVE WS-BUS-DATE            TO SCA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SCA-CALLER-PGM
           MOVE CHG-CHARGE-ID          TO SCA-SC-CHARGE-ID
           MOVE PPR-PP-ID              TO SCA-SC-PP-ID
           SET SCA-SC-IS-ONE-TIME      TO TRUE
           CALL WS-SCH-PGM USING WS-SCH-CA
           IF SCA-RETURN-CODE >= 90
              MOVE SCA-MESSAGE         TO EDR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           IF SCA-RETURN-CODE >= 8
              SET WS-EVENT-FAILED      TO TRUE
              STRING 'BILISCHD ' SCA-REASON-CODE ' ' SCA-MESSAGE
                     DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-IF.

      * BR-BIL-EVD-005 Receivable raised against claims payable
       8400-POST-DEDUCT-LEDGER.
           INITIALIZE WS-GLP-CA
           MOVE 'GLPOST  '             TO GLA-REQUEST-ID
           MOVE WS-BATCH-USER          TO GLA-USER-ID
           MOVE WS-BUS-DATE            TO GLA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO GLA-CALLER-PGM
           MOVE 'DEDBILL '             TO GLA-GL-TXN-TYPE
           MOVE PPR-ACCOUNT-ID         TO GLA-GL-ACCOUNT-ID
           MOVE 'CHRG'                 TO GLA-GL-REF-TYPE
           MOVE CHG-CHARGE-ID          TO GLA-GL-REF-ID
           MOVE 2                      TO GLA-GL-LINE-COUNT
           MOVE 'UNBILLED'             TO GLA-GL-T-ACCOUNT(1)
           MOVE 'D'                    TO GLA-GL-DR-CR(1)
           MOVE 'CLMPAY  '             TO GLA-GL-T-ACCOUNT(2)
           MOVE 'C'                    TO GLA-GL-DR-CR(2)
           MOVE WS-AMOUNT              TO GLA-GL-AMOUNT(1)
                                          GLA-GL-AMOUNT(2)
           CALL WS-GLP-PGM USING WS-GLP-CA
           IF GLA-RETURN-CODE >= 90
              MOVE GLA-MESSAGE         TO EDR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           IF GLA-RETURN-CODE >= 8
              SET WS-EVENT-FAILED      TO TRUE
              STRING 'BILGLPST ' GLA-REASON-CODE ' ' GLA-MESSAGE
                     DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-IF.

       8500-REBALANCE-ACCOUNT.
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTBAL '             TO ACA-REQUEST-ID
           MOVE WS-BATCH-USER          TO ACA-USER-ID
           MOVE WS-BUS-DATE            TO ACA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE PPR-ACCOUNT-ID         TO ACA-AC-ACCOUNT-ID
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF ACA-RETURN-CODE >= 90
              MOVE ACA-MESSAGE         TO EDR-M-TEXT
              PERFORM 6200-WRITE-MESSAGE
              PERFORM 9800-ABEND
           END-IF
           IF ACA-RETURN-CODE >= 8
              SET WS-EVENT-FAILED      TO TRUE
              STRING 'BILACCT0 ' ACA-REASON-CODE ' ' ACA-MESSAGE
                     DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-IF.

      * BR-BIL-EVD-001 Claims queue keys come from the claims sequence
       8600-NEXT-CLAIMS-EVENT-ID.
           INITIALIZE WS-CSQ-CA
           MOVE 'SEQNEXT '             TO CSA-REQUEST-ID
           MOVE WS-BATCH-USER          TO CSA-USER-ID
           MOVE WS-BUS-DATE            TO CSA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CSA-CALLER-PGM
           MOVE 'EVQ     '             TO CSA-SQ-NAME
           MOVE 1                      TO CSA-SQ-COUNT
           CALL WS-CSQ-PGM USING WS-CSQ-CA
           EVALUATE TRUE
              WHEN CSA-RC-OK OR CSA-RC-WARNING
                 MOVE CSA-SQ-VALUE     TO CEV-EVENT-ID
              WHEN CSA-RETURN-CODE >= 90
                 MOVE CSA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
              WHEN OTHER
                 SET WS-EVENT-FAILED   TO TRUE
                 STRING 'CLMSEQGN ' CSA-REASON-CODE ' ' CSA-MESSAGE
                        DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-EVALUATE.

       8700-PRIME-PERIOD-CALL.
           MOVE WS-BATCH-USER          TO PPA-USER-ID
           MOVE WS-BUS-DATE            TO PPA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO PPA-CALLER-PGM
           STRING 'EVQ ' WS-ERR-KEY DELIMITED BY SIZE
                  INTO PPA-PP-SOURCE-REF.

       8800-CALL-PERIOD.
           CALL WS-PPR-PGM USING WS-PPR-CA
           EVALUATE TRUE
              WHEN PPA-RC-OK OR PPA-RC-WARNING
                 MOVE PPA-PP-PP-ID     TO WS-KEY-NUM
                 STRING PPA-REQUEST-ID ' DONE - PERIOD ' WS-KEY-NUM
                        DELIMITED BY SIZE INTO WS-RESULT
              WHEN PPA-RETURN-CODE >= 90
                 MOVE PPA-MESSAGE      TO EDR-M-TEXT
                 PERFORM 6200-WRITE-MESSAGE
                 PERFORM 9800-ABEND
              WHEN OTHER
                 SET WS-EVENT-FAILED   TO TRUE
                 STRING 'BILPPER0 ' PPA-REASON-CODE ' ' PPA-MESSAGE
                        DELIMITED BY SIZE INTO WS-FAIL-TEXT
           END-EVALUATE.

      ******************************************************************
       9000-TERMINATE.
           IF WS-PAGE-COUNT = 0
              PERFORM 6100-WRITE-HEADINGS
           END-IF
           PERFORM 6900-WRITE-CONTROL-TOTALS
           INITIALIZE CHECKPOINT-REC
           MOVE WS-PROGRAM-ID          TO CKP-PROGRAM
           MOVE WS-BUS-DATE            TO CKP-RUN-DATE
           MOVE WS-CNT-COMMITS         TO CKP-COMMIT-COUNT
           MOVE WS-CNT-EVENTS          TO CKP-READ-COUNT
           SET CKP-COMPLETE            TO TRUE
           WRITE CHECKPOINT-REC
           CLOSE CHECKPOINT-FILE
                 OPS-FILE
                 REPORT-FILE
           DISPLAY 'BILEVTDR COMPLETE EVENTS=' WS-CNT-EVENTS
                   ' RC=' WS-RETURN-CODE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           MOVE SPACES                 TO EDR-M-TEXT
           STRING '*** BILEVTDR ABENDED - WORK SINCE LAST COMMIT '
                  'ROLLED BACK. LAST EVENT ' WS-ERR-KEY
                  DELIMITED BY SIZE INTO EDR-M-TEXT
           PERFORM 6200-WRITE-MESSAGE
           PERFORM 6900-WRITE-CONTROL-TOTALS
           CLOSE CHECKPOINT-FILE
                 OPS-FILE
                 REPORT-FILE
           MOVE 16                     TO RETURN-CODE
           GOBACK.

       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           DISPLAY 'BILEVTDR DB2 ERROR ' WS-SQLCODE-DISP ' IN '
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
