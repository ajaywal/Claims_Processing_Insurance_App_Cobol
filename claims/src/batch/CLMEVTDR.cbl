       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMEVTDR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMEVTDR                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Integration event drainer for CLM_EVENT_QUEUE.   *
      *               Phase OUTB delivers outbound claims events for   *
      *               target BILL (CLMPAYMT, DEDBILL) by inserting an  *
      *               inbound row into BILDB.BIL_EVENT_QUEUE, then     *
      *               marks the claims row DONE through CLMEVTQ.       *
      *               Phase INBD processes inbound BillingCore events  *
      *               (BILSTAT, DSBSENT) against claims data.          *
      *               PARM: OUT | IN | ALL (default ALL), optional     *
      *               ',nnnn' commit frequency.                        *
      * CALLS       : CLMEVTQ  (EVDONE, EVFAIL)                        *
      *               CLMHIST0 (HISADD)                                *
      *               CLMACTV0 (ACTNEW)                                *
      *               CLMDATES (DTVALD)                                *
      *               BILSEQGN (SEQNEXT EVQ - billing queue key)       *
      *               CLMERRLG (ERRLOG)                                *
      * FILES       : RPTOUT  132-column run report                    *
      *               CHKPT   checkpoint / restart record (CLMEVTC1)   *
      * TABLES      : CLM_EVENT_QUEUE      (R U)                       *
      *               BIL_EVENT_QUEUE      (C R) - billing schema      *
      *               CLM_CONTROL          (R)                         *
      *               CLM_CLAIM            (R)                         *
      *               CLM_POLICY_SNAPSHOT  (U)                         *
      *               CLM_CHECK            (R U)                       *
      *               CLM_ACTIVITY_PATTERN (R)                         *
      *               CLM_ACTIVITY         (R)                         *
      * RULES       : BR-CLM-EVD-001 Only events created before the    *
      *                              run started are drained           *
      *               BR-CLM-EVD-002 Outbound type must be in the      *
      *                              catalogue and its payload must    *
      *                              pass the EVENTS.md layout check   *
      *               BR-CLM-EVD-003 Delivery is idempotent: an equal  *
      *                              inbound row already on the BIL    *
      *                              queue completes the event         *
      *               BR-CLM-EVD-004 Delivered row is direction I,     *
      *                              source CLMS, status NEW, keyed    *
      *                              from the billing EVQ sequence     *
      *               BR-CLM-EVD-005 BILSTAT refreshes the cached      *
      *                              billing status on every non-      *
      *                              closed claim of the policy        *
      *               BR-CLM-EVD-006 DLNQ / CANC is a coverage concern:*
      *                              history row on each open claim    *
      *               BR-CLM-EVD-007 BILLCHK activity only when the    *
      *                              pattern exists and the claim has  *
      *                              no open BILLCHK activity          *
      *               BR-CLM-EVD-008 DSBSENT clears PEND/ISSU checks;  *
      *                              CLRD is a duplicate, HOLD (still  *
      *                              awaiting approval) and VOID fail  *
      *               BR-CLM-EVD-009 An existing issue date is kept,   *
      *                              otherwise the billing sent date   *
      *               BR-CLM-EVD-010 A failed event is rolled back to  *
      *                              its savepoint and EVFAILed; an    *
      *                              event reaching ERRR gives RC 8    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE   ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL CHK-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).
       FD  CHK-FILE
           RECORDING MODE IS F.
           COPY CLMEVTC1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMEVTDR'.
       01  WS-EVQ-PGM                  PIC X(8)  VALUE 'CLMEVTQ '.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-BSQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-RPT-STATUS            PIC XX    VALUE '00'.
           05 WS-CHK-STATUS            PIC XX    VALUE '00'.

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-CLM-EOF-SW            PIC X     VALUE 'N'.
              88 WS-CLM-EOF             VALUE 'Y'.
           05 WS-EVENT-OK-SW           PIC X     VALUE 'Y'.
              88 WS-EVENT-OK            VALUE 'Y'.
              88 WS-EVENT-FAILED        VALUE 'N'.
           05 WS-DUP-SW                PIC X     VALUE 'N'.
              88 WS-EVENT-DUP           VALUE 'Y'.
           05 WS-BILLCHK-SW            PIC X     VALUE 'N'.
              88 WS-BILLCHK-DEFINED     VALUE 'Y'.
           05 WS-RESTART-SW            PIC X     VALUE 'N'.
              88 WS-RESTARTING          VALUE 'Y'.

       01  WS-PARM-WORK.
           05 WS-RUN-MODE              PIC X(3)  VALUE 'ALL'.
              88 WS-MODE-OUT            VALUE 'OUT' 'ALL'.
              88 WS-MODE-IN             VALUE 'IN ' 'ALL'.
           05 WS-PARM-FREQ             PIC X(5)  JUSTIFIED RIGHT
                                                 VALUE SPACES.
           05 WS-PARM-FREQ-N REDEFINES WS-PARM-FREQ
                                       PIC 9(5).

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-START-TS          PIC X(26) VALUE SPACES.
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-UNCOMMITTED           PIC S9(4) COMP VALUE 0.
           05 WS-RESUME-PHASE          PIC X(4)  VALUE SPACES.
           05 WS-RESUME-KEY            PIC S9(9) COMP VALUE 0.
           05 WS-CUR-PHASE             PIC X(4)  VALUE SPACES.
           05 WS-CUR-DIRECTION         PIC X     VALUE SPACE.
           05 WS-CUR-TARGET            PIC X(4)  VALUE SPACES.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.
           05 WS-TOTAL-DONE            PIC S9(7) COMP-3 VALUE 0.
           05 WS-TOTAL-FAILED          PIC S9(7) COMP-3 VALUE 0.

      * Per event type control totals (row 5 = unknown types)
       01  WS-TYPE-TABLE.
           05 WS-TYPE-ENTRY OCCURS 5 TIMES INDEXED BY TX.
              10 WS-T-TYPE             PIC X(8).
              10 WS-T-DIR              PIC X.
              10 WS-T-READ             PIC S9(9) COMP-3.
              10 WS-T-DONE             PIC S9(9) COMP-3.
              10 WS-T-DUP              PIC S9(9) COMP-3.
              10 WS-T-FAILED           PIC S9(9) COMP-3.
              10 WS-T-ERRORED          PIC S9(9) COMP-3.
              10 WS-T-CLAIMS           PIC S9(9) COMP-3.
              10 WS-T-ACTS             PIC S9(9) COMP-3.
       01  WS-TYPE-IX                  PIC S9(4) COMP VALUE 0.

       01  WS-EVENT-WORK.
           05 WS-FAIL-REASON           PIC X(8)  VALUE SPACES.
           05 WS-FAIL-TEXT             PIC X(80) VALUE SPACES.
           05 WS-AMOUNT                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-AMOUNT-EDIT           PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-NEW-STATUS            PIC X(4)  VALUE SPACES.
           05 WS-DESC                  PIC X(120) VALUE SPACES.
           05 WS-PL-AMOUNT-X           PIC X(13) VALUE SPACES.
           05 WS-PL-AMOUNT-N REDEFINES WS-PL-AMOUNT-X
                                       PIC 9(11)V99.
           05 WS-PL-AMOUNT-SIGN        PIC X     VALUE SPACE.

      * Payload views (fixed positions per docs/EVENTS.md)
       01  WS-PAYLOAD                  PIC X(1000) VALUE SPACES.
       01  WS-PL-CLMPAYMT REDEFINES WS-PAYLOAD.
           05 PCP-CHECK-ID             PIC X(10).
           05 PCP-CLAIM-NO             PIC X(12).
           05 PCP-POLICY-NO            PIC X(10).
           05 PCP-AMOUNT               PIC X(13).
           05 PCP-AMOUNT-SIGN          PIC X.
           05 PCP-PAYEE-NAME           PIC X(60).
           05 PCP-PAYEE-ADDR1          PIC X(60).
           05 PCP-PAYEE-POSTCODE       PIC X(8).
           05 PCP-PAY-METHOD           PIC X(4).
           05 PCP-SCHED-DATE           PIC X(10).
           05 FILLER                   PIC X(812).
       01  WS-PL-DEDBILL REDEFINES WS-PAYLOAD.
           05 PDB-CLAIM-NO             PIC X(12).
           05 PDB-POLICY-NO            PIC X(10).
           05 PDB-AMOUNT               PIC X(13).
           05 PDB-AMOUNT-SIGN          PIC X.
           05 FILLER                   PIC X(964).
       01  WS-PL-BILSTAT REDEFINES WS-PAYLOAD.
           05 PBS-POLICY-NO            PIC X(10).
           05 PBS-POLICY-NUM REDEFINES PBS-POLICY-NO
                                       PIC 9(10).
           05 PBS-STATUS               PIC X(4).
              88 PBS-STATUS-VALID       VALUE 'CURR' 'PDUE' 'DLNQ'
                                              'CANC'.
              88 PBS-COVERAGE-CONCERN   VALUE 'DLNQ' 'CANC'.
           05 PBS-PASTDUE              PIC X(13).
           05 PBS-PASTDUE-SIGN         PIC X.
           05 PBS-ASOF-DATE            PIC X(10).
           05 FILLER                   PIC X(962).
       01  WS-PL-DSBSENT REDEFINES WS-PAYLOAD.
           05 PDS-CHECK-ID             PIC X(10).
           05 PDS-CHECK-NUM REDEFINES PDS-CHECK-ID
                                       PIC 9(10).
           05 PDS-DISB-ID              PIC X(10).
           05 PDS-SENT-DATE            PIC X(10).
           05 FILLER                   PIC X(970).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDEVQ END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDCHK END-EXEC.
           EXEC SQL INCLUDE CLMDPSN END-EXEC.
           EXEC SQL INCLUDE CLMDAPT END-EXEC.

      * BR-CLM-EVD-004 The one cross-application write in the suite:
      * BillingCore's inbound queue. Declared here (no BILD DCLGEN in
      * the claims library); host variables mirror BILDDL08.
           EXEC SQL DECLARE BILDB.BIL_EVENT_QUEUE TABLE
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
       01  DCLBIL-EVENT-QUEUE.
           10 BEV-EVENT-ID                   PIC S9(9) USAGE COMP.
           10 BEV-EVENT-TYPE                 PIC X(8).
           10 BEV-DIRECTION                  PIC X(1).
           10 BEV-SOURCE-SYSTEM              PIC X(4).
           10 BEV-TARGET-SYSTEM              PIC X(4).
           10 BEV-EVENT-KEY                  PIC X(20).
           10 BEV-PAYLOAD.
              49 BEV-PAYLOAD-LEN              PIC S9(4) USAGE COMP.
              49 BEV-PAYLOAD-TEXT             PIC X(1000).
           10 BEV-EVENT-STATUS               PIC X(4).
           10 BEV-RETRY-COUNT                PIC S9(4) USAGE COMP.

           EXEC SQL DECLARE EVQCSR CURSOR WITH HOLD FOR
                SELECT EVENT_ID, EVENT_TYPE, DIRECTION,
                       SOURCE_SYSTEM, TARGET_SYSTEM, EVENT_KEY,
                       PAYLOAD, RETRY_COUNT
                  FROM CLMDB.CLM_EVENT_QUEUE
                 WHERE EVENT_STATUS  = 'NEW'
                   AND DIRECTION     = :WS-CUR-DIRECTION
                   AND TARGET_SYSTEM = :WS-CUR-TARGET
                   AND EVENT_ID      > :WS-RESUME-KEY
                   AND CREATE_TS    <= :WS-RUN-START-TS
                 ORDER BY EVENT_ID
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE POLCSR CURSOR FOR
                SELECT CLAIM_ID, CLAIM_NUMBER, ASSIGNED_USER
                  FROM CLMDB.CLM_CLAIM
                 WHERE GENAPP_POL_NUM = :CLM-GENAPP-POL-NUM
                   AND CLAIM_STATE NOT IN ('CLSD', 'DRFT')
                 ORDER BY CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-EVQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==EVA-==.
           03 EVA-BODY.
              COPY CLMCEVT REPLACING LEADING ==CA-== BY ==EVA-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
      * BILSEQGN takes BILCAHDR + BILCSEQ, which are byte-for-byte the
      * same layouts as CLMCAHDR + CLMCSEQ (billing copybooks are not
      * in the claims SYSLIB concatenation).
       01  WS-BSQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==BSQ-==.
           03 BSQ-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==BSQ-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY CLMEVTR1.

       LINKAGE SECTION.
       01  LK-PARM.
           05 LK-PARM-LEN              PIC S9(4) COMP.
           05 LK-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF WS-MODE-OUT
              PERFORM 2000-DRAIN-OUTBOUND
           END-IF
           IF WS-MODE-IN
              PERFORM 2500-DRAIN-INBOUND
           END-IF
           PERFORM 7000-COMMIT
           MOVE 'C'                    TO EDC-RUN-STATUS
           PERFORM 7100-WRITE-CHECKPOINT
           PERFORM 6500-PRINT-TOTALS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'CLMEVTDR RPTOUT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           PERFORM 1100-READ-PARM
           PERFORM 4000-READ-CONTROL
           EXEC SQL
              SELECT CURRENT TIMESTAMP
                INTO :WS-RUN-START-TS
                FROM SYSIBM.SYSDUMMY1
           END-EXEC
           MOVE WS-BUS-DATE            TO EDR-H1-BUS-DATE
           MOVE WS-RUN-START-TS(1:19)  TO EDR-H1-RUN-TS
           PERFORM 1200-INIT-TYPE-TABLE
           PERFORM 1300-READ-RESTART
           PERFORM 4050-CHECK-BILLCHK-PATTERN.

      ******************************************************************
       1100-READ-PARM.
           IF LK-PARM-LEN > 0
              UNSTRING LK-PARM-DATA(1:LK-PARM-LEN) DELIMITED BY ','
                  INTO WS-RUN-MODE WS-PARM-FREQ
              END-UNSTRING
           END-IF
           IF NOT WS-MODE-OUT AND NOT WS-MODE-IN
              MOVE 'Invalid PARM - expected OUT, IN or ALL'
                                       TO EDR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           IF WS-PARM-FREQ NOT = SPACES
              INSPECT WS-PARM-FREQ REPLACING LEADING SPACE BY ZERO
              IF WS-PARM-FREQ-N IS NUMERIC
                 MOVE WS-PARM-FREQ-N   TO WS-COMMIT-FREQ
              END-IF
              IF WS-COMMIT-FREQ < 1 OR WS-COMMIT-FREQ > 5000
                 MOVE 200              TO WS-COMMIT-FREQ
              END-IF
           END-IF.

      ******************************************************************
       1200-INIT-TYPE-TABLE.
           INITIALIZE WS-TYPE-TABLE
           MOVE 'CLMPAYMT'             TO WS-T-TYPE(1)
           MOVE 'O'                    TO WS-T-DIR(1)
           MOVE 'DEDBILL '             TO WS-T-TYPE(2)
           MOVE 'O'                    TO WS-T-DIR(2)
           MOVE 'BILSTAT '             TO WS-T-TYPE(3)
           MOVE 'I'                    TO WS-T-DIR(3)
           MOVE 'DSBSENT '             TO WS-T-TYPE(4)
           MOVE 'I'                    TO WS-T-DIR(4)
           MOVE '*OTHER* '             TO WS-T-TYPE(5)
           MOVE '*'                    TO WS-T-DIR(5).

      ******************************************************************
      * A previous run that ended without status C restarts after the
      * last committed event of the phase it was working on.
       1300-READ-RESTART.
           OPEN INPUT CHK-FILE
           IF WS-CHK-STATUS = '00'
              READ CHK-FILE
                 AT END
                    CONTINUE
                 NOT AT END
                    IF EDC-PROGRAM = WS-PROGRAM-ID
                    AND EDC-RUN-ACTIVE
                    AND EDC-BUS-DATE = WS-BUS-DATE
                       SET WS-RESTARTING TO TRUE
                       MOVE EDC-PHASE  TO WS-RESUME-PHASE
                       MOVE EDC-LAST-KEY TO WS-RESUME-KEY
                    END-IF
              END-READ
              CLOSE CHK-FILE
           END-IF
           OPEN OUTPUT CHK-FILE
           IF WS-CHK-STATUS NOT = '00'
              MOVE 'CHKPT file cannot be opened for output'
                                       TO EDR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           IF WS-RESTARTING
              MOVE WS-RESUME-KEY       TO EDR-MSG-NUM
              STRING 'RESTART: phase ' WS-RESUME-PHASE
                     ' resumes after event id'
                     DELIMITED BY SIZE INTO EDR-MSG-TEXT
              PERFORM 6100-WRITE-MSG
           END-IF.

      ******************************************************************
       2000-DRAIN-OUTBOUND.
           MOVE 'OUTB'                 TO WS-CUR-PHASE
           MOVE 'O'                    TO WS-CUR-DIRECTION
           MOVE 'BILL'                 TO WS-CUR-TARGET
           PERFORM 2050-OPEN-PHASE
           PERFORM 4100-FETCH-EVENT
           PERFORM UNTIL WS-EOF
              PERFORM 2900-CLAIM-EVENT
              IF WS-EVENT-OK
                 PERFORM 2100-DELIVER-EVENT
                 PERFORM 2800-FINISH-EVENT
              END-IF
              PERFORM 4100-FETCH-EVENT
           END-PERFORM
           PERFORM 4190-CLOSE-EVENTS.

      ******************************************************************
       2050-OPEN-PHASE.
           MOVE 'N'                    TO WS-EOF-SW
           IF WS-RESTARTING AND WS-RESUME-PHASE = WS-CUR-PHASE
              CONTINUE
           ELSE
              MOVE 0                   TO WS-RESUME-KEY
           END-IF
           MOVE WS-CUR-PHASE           TO EDC-PHASE
           MOVE WS-RESUME-KEY          TO EDC-LAST-KEY
           EXEC SQL OPEN EVQCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2050-OPEN-PHASE'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-EVD-002 Catalogue and layout validation before delivery
       2100-DELIVER-EVENT.
           EVALUATE EVQ-EVENT-TYPE
              WHEN 'CLMPAYMT'
                 IF PCP-CHECK-ID IS NOT NUMERIC
                 OR PCP-CLAIM-NO = SPACES
                 OR PCP-POLICY-NO IS NOT NUMERIC
                 OR PCP-PAYEE-NAME = SPACES
                    MOVE 'PAYLOAD '    TO WS-FAIL-REASON
                    MOVE 'CLMPAYMT payload keys or payee missing'
                                       TO WS-FAIL-TEXT
                    SET WS-EVENT-FAILED TO TRUE
                 END-IF
                 IF WS-EVENT-OK
                    IF EVQ-EVENT-KEY(1:10) NOT = PCP-CHECK-ID
                       MOVE 'KEYMISMT' TO WS-FAIL-REASON
                       MOVE 'Event key differs from payload check id'
                                       TO WS-FAIL-TEXT
                       SET WS-EVENT-FAILED TO TRUE
                    END-IF
                 END-IF
                 IF WS-EVENT-OK
                    MOVE PCP-AMOUNT    TO WS-PL-AMOUNT-X
                    MOVE PCP-AMOUNT-SIGN TO WS-PL-AMOUNT-SIGN
                    PERFORM 3100-VALIDATE-AMOUNT
                 END-IF
                 IF WS-EVENT-OK
                    MOVE PCP-SCHED-DATE TO DTA-DT-DATE-1
                    PERFORM 3200-VALIDATE-DATE
                 END-IF
              WHEN 'DEDBILL '
                 IF PDB-CLAIM-NO = SPACES
                 OR PDB-POLICY-NO IS NOT NUMERIC
                    MOVE 'PAYLOAD '    TO WS-FAIL-REASON
                    MOVE 'DEDBILL payload claim/policy invalid'
                                       TO WS-FAIL-TEXT
                    SET WS-EVENT-FAILED TO TRUE
                 ELSE
                    MOVE PDB-AMOUNT    TO WS-PL-AMOUNT-X
                    MOVE PDB-AMOUNT-SIGN TO WS-PL-AMOUNT-SIGN
                    PERFORM 3100-VALIDATE-AMOUNT
                    IF WS-EVENT-OK AND WS-AMOUNT NOT > 0
                       MOVE 'DEDAMT  ' TO WS-FAIL-REASON
                       MOVE 'Deductible owed must be positive'
                                       TO WS-FAIL-TEXT
                       SET WS-EVENT-FAILED TO TRUE
                    END-IF
                 END-IF
              WHEN OTHER
                 MOVE 'UNKEVENT'       TO WS-FAIL-REASON
                 MOVE 'Event type not in outbound catalogue'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
           END-EVALUATE
           IF WS-EVENT-OK
              PERFORM 4200-CHECK-ALREADY-DELIVERED
           END-IF
           IF WS-EVENT-OK AND NOT WS-EVENT-DUP
              PERFORM 8500-NEXT-BILLING-EVENT-ID
           END-IF
           IF WS-EVENT-OK AND NOT WS-EVENT-DUP
              PERFORM 4210-INSERT-BILLING-EVENT
           END-IF.

      ******************************************************************
       2500-DRAIN-INBOUND.
           MOVE 'INBD'                 TO WS-CUR-PHASE
           MOVE 'I'                    TO WS-CUR-DIRECTION
           MOVE 'CLMS'                 TO WS-CUR-TARGET
           PERFORM 2050-OPEN-PHASE
           PERFORM 4100-FETCH-EVENT
           PERFORM UNTIL WS-EOF
              PERFORM 2900-CLAIM-EVENT
              IF WS-EVENT-OK
                 EVALUATE EVQ-EVENT-TYPE
                    WHEN 'BILSTAT '
                       PERFORM 2600-PROCESS-BILSTAT
                    WHEN 'DSBSENT '
                       PERFORM 2700-PROCESS-DSBSENT
                    WHEN OTHER
                       MOVE 'UNKEVENT' TO WS-FAIL-REASON
                       MOVE 'Inbound event type not handled by claims'
                                       TO WS-FAIL-TEXT
                       SET WS-EVENT-FAILED TO TRUE
                 END-EVALUATE
                 PERFORM 2800-FINISH-EVENT
              END-IF
              PERFORM 4100-FETCH-EVENT
           END-PERFORM
           PERFORM 4190-CLOSE-EVENTS.

      ******************************************************************
       2600-PROCESS-BILSTAT.
           IF PBS-POLICY-NO IS NOT NUMERIC
           OR PBS-POLICY-NUM > 999999999
           OR PBS-POLICY-NUM = 0
              MOVE 'POLICYNO'          TO WS-FAIL-REASON
              MOVE 'BILSTAT policy number not numeric or out of range'
                                       TO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
           END-IF
           IF WS-EVENT-OK AND NOT PBS-STATUS-VALID
              MOVE 'BILSTATS'          TO WS-FAIL-REASON
              MOVE 'Billing status not CURR/PDUE/DLNQ/CANC'
                                       TO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
           END-IF
           IF WS-EVENT-OK
              MOVE PBS-PASTDUE         TO WS-PL-AMOUNT-X
              MOVE PBS-PASTDUE-SIGN    TO WS-PL-AMOUNT-SIGN
              PERFORM 3100-VALIDATE-AMOUNT
           END-IF
           IF WS-EVENT-OK
              MOVE PBS-ASOF-DATE       TO DTA-DT-DATE-1
              PERFORM 3200-VALIDATE-DATE
           END-IF
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           MOVE PBS-POLICY-NUM         TO CLM-GENAPP-POL-NUM
      * BR-CLM-EVD-005 Cache the billing status on the claim snapshots
           PERFORM 4300-UPDATE-SNAPSHOT-STATUS
      * BR-CLM-EVD-006 Coverage concern only for DLNQ / CANC
           IF PBS-COVERAGE-CONCERN
              PERFORM 2650-FLAG-OPEN-CLAIMS
           END-IF.

      ******************************************************************
       2650-FLAG-OPEN-CLAIMS.
           MOVE WS-AMOUNT              TO WS-AMOUNT-EDIT
           MOVE 'N'                    TO WS-CLM-EOF-SW
           EXEC SQL OPEN POLCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2650-FLAG-OPEN-CLAIMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4310-FETCH-POLICY-CLAIM
           PERFORM UNTIL WS-CLM-EOF OR WS-EVENT-FAILED
              MOVE SPACES              TO WS-DESC
              STRING 'Billing status ' PBS-STATUS
                     ' as of ' PBS-ASOF-DATE
                     ' past due GBP ' WS-AMOUNT-EDIT
                     ' - coverage in question, confirm before payment'
                     DELIMITED BY SIZE INTO WS-DESC
              PERFORM 8100-ADD-HISTORY
              IF WS-EVENT-OK
                 ADD 1                 TO WS-T-CLAIMS(TX)
                 PERFORM 2660-RAISE-BILLCHK
              END-IF
              PERFORM 4310-FETCH-POLICY-CLAIM
           END-PERFORM
           EXEC SQL CLOSE POLCSR END-EXEC.

      ******************************************************************
      * BR-CLM-EVD-007 BILLCHK activity: pattern defined, none open
       2660-RAISE-BILLCHK.
           IF NOT WS-BILLCHK-DEFINED
              EXIT PARAGRAPH
           END-IF
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_ACTIVITY
               WHERE CLAIM_ID        = :CLM-CLAIM-ID
                 AND PATTERN_CODE    = 'BILLCHK'
                 AND ACTIVITY_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2660-RAISE-BILLCHK' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0
              PERFORM 8200-ADD-ACTIVITY
              IF WS-EVENT-OK
                 ADD 1                 TO WS-T-ACTS(TX)
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-EVD-008 Only PEND / ISSU checks can be cleared
       2700-PROCESS-DSBSENT.
           IF PDS-CHECK-ID IS NOT NUMERIC
           OR PDS-CHECK-NUM > 999999999
           OR PDS-CHECK-NUM = 0
              MOVE 'CHECKID '          TO WS-FAIL-REASON
              MOVE 'DSBSENT check id not numeric or out of range'
                                       TO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
              EXIT PARAGRAPH
           END-IF
           IF PDS-DISB-ID = SPACES
              MOVE 'DISBID  '          TO WS-FAIL-REASON
              MOVE 'DSBSENT disbursement id missing'
                                       TO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
              EXIT PARAGRAPH
           END-IF
           MOVE PDS-SENT-DATE          TO DTA-DT-DATE-1
           PERFORM 3200-VALIDATE-DATE
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           MOVE PDS-CHECK-NUM          TO CHK-CHECK-ID
           PERFORM 4400-READ-CHECK
           IF WS-EVENT-FAILED
              EXIT PARAGRAPH
           END-IF
           EVALUATE CHK-CHECK-STATUS
              WHEN 'PEND'
              WHEN 'ISSU'
                 PERFORM 4410-CLEAR-CHECK
              WHEN 'CLRD'
                 SET WS-EVENT-DUP      TO TRUE
                 MOVE 'ALRDCLRD'       TO WS-FAIL-REASON
                 MOVE 'Check already cleared - duplicate DSBSENT'
                                       TO WS-FAIL-TEXT
              WHEN 'HOLD'
                 MOVE 'CHKHOLD '       TO WS-FAIL-REASON
                 MOVE 'Billing disbursed a check held for approval'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN 'VOID'
                 MOVE 'CHKVOID '       TO WS-FAIL-REASON
                 MOVE 'Billing disbursed a voided claim check'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN OTHER
                 MOVE 'CHKSTATE'       TO WS-FAIL-REASON
                 STRING 'Check status ' CHK-CHECK-STATUS
                        ' cannot be cleared'
                        DELIMITED BY SIZE INTO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
           END-EVALUATE
           IF WS-EVENT-OK AND NOT WS-EVENT-DUP
              MOVE CHK-CLAIM-ID        TO CLM-CLAIM-ID
              MOVE SPACES              TO WS-DESC
              MOVE CHK-CHECK-AMOUNT    TO WS-AMOUNT-EDIT
              STRING 'Check ' PDS-CHECK-ID ' GBP ' WS-AMOUNT-EDIT
                     ' cleared - billing disbursement ' PDS-DISB-ID
                     ' sent ' PDS-SENT-DATE
                     DELIMITED BY SIZE INTO WS-DESC
              PERFORM 8100-ADD-HISTORY
              IF WS-EVENT-OK
                 ADD 1                 TO WS-T-CLAIMS(TX)
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-EVD-010 Commit the outcome through CLMEVTQ; business
      * failures are undone to the event savepoint first.
       2800-FINISH-EVENT.
           INITIALIZE WS-EVQ-CA
           MOVE WS-PROGRAM-ID          TO EVA-USER-ID EVA-CALLER-PGM
           MOVE WS-BUS-DATE            TO EVA-BUS-DATE
           MOVE EVQ-EVENT-ID           TO EVA-EV-EVENT-ID
           IF WS-EVENT-OK
              MOVE 'EVDONE  '          TO EVA-REQUEST-ID
              IF WS-EVENT-DUP
                 ADD 1                 TO WS-T-DUP(TX)
                 MOVE 'DUPLICATE'      TO EDR-EX-OUTCOME
                 PERFORM 6200-WRITE-EXCEPTION
              ELSE
                 ADD 1                 TO WS-T-DONE(TX)
              END-IF
              ADD 1                    TO WS-TOTAL-DONE
           ELSE
              EXEC SQL ROLLBACK TO SAVEPOINT EVTSP END-EXEC
              MOVE 'EVFAIL  '          TO EVA-REQUEST-ID
              MOVE WS-FAIL-TEXT        TO EVA-EV-ERROR
              ADD 1                    TO WS-T-FAILED(TX)
              ADD 1                    TO WS-TOTAL-FAILED
              MOVE 'FAILED'            TO EDR-EX-OUTCOME
              PERFORM 6200-WRITE-EXCEPTION
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF
           CALL WS-EVQ-PGM USING WS-EVQ-CA
           IF EVA-RETURN-CODE >= 12
              MOVE EVA-MESSAGE         TO EDR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           IF WS-EVENT-FAILED
              PERFORM 4500-CHECK-ERRORED
           END-IF
           MOVE EVQ-EVENT-ID           TO EDC-LAST-KEY
           ADD 1                       TO WS-UNCOMMITTED
           IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
              PERFORM 7000-COMMIT
              PERFORM 7100-WRITE-CHECKPOINT
           END-IF.

      ******************************************************************
      * Take ownership of the row (NEW -> PROC) the same way CLMEVTQ
      * EVGET does, but for this specific row so a failing event can
      * never block the ones behind it. Losing the race to another
      * drainer is not an error.
       2900-CLAIM-EVENT.
           SET WS-EVENT-OK             TO TRUE
           MOVE 'N'                    TO WS-DUP-SW
           MOVE SPACES                 TO WS-FAIL-REASON WS-FAIL-TEXT
           PERFORM 3000-LOCATE-TYPE
           ADD 1                       TO WS-T-READ(TX)
           EXEC SQL
              UPDATE CLMDB.CLM_EVENT_QUEUE
                 SET EVENT_STATUS = 'PROC'
               WHERE EVENT_ID     = :EVQ-EVENT-ID
                 AND EVENT_STATUS = 'NEW'
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 1
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SUBTRACT 1            FROM WS-T-READ(TX)
                 SET WS-EVENT-FAILED   TO TRUE
                 EXIT PARAGRAPH
              WHEN OTHER
                 MOVE '2900-CLAIM-EVENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           EXEC SQL SAVEPOINT EVTSP ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2900-CLAIM-EVENT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE EVQ-PAYLOAD-TEXT       TO WS-PAYLOAD.

      ******************************************************************
       3000-LOCATE-TYPE.
           SET TX                      TO 1
           SEARCH WS-TYPE-ENTRY
              AT END
                 SET TX                TO 5
              WHEN WS-T-TYPE(TX) = EVQ-EVENT-TYPE
               AND WS-T-DIR(TX) = EVQ-DIRECTION
                 CONTINUE
           END-SEARCH.

      ******************************************************************
      * EVENTS.md amounts: 13 digits, implied 2 decimals, trailing sign
       3100-VALIDATE-AMOUNT.
           IF WS-PL-AMOUNT-X IS NOT NUMERIC
           OR (WS-PL-AMOUNT-SIGN NOT = '+'
               AND WS-PL-AMOUNT-SIGN NOT = '-')
              MOVE 'AMOUNT  '          TO WS-FAIL-REASON
              MOVE 'Payload amount not 13 digits plus sign'
                                       TO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
           ELSE
              MOVE WS-PL-AMOUNT-N      TO WS-AMOUNT
              IF WS-PL-AMOUNT-SIGN = '-'
                 COMPUTE WS-AMOUNT = 0 - WS-AMOUNT
              END-IF
           END-IF.

      ******************************************************************
       3200-VALIDATE-DATE.
           INITIALIZE DTA-HEADER
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK OR NOT DTA-DT-IS-VALID
              MOVE 'DATE    '          TO WS-FAIL-REASON
              STRING 'Payload date ' DTA-DT-DATE-1 ' is not valid'
                     DELIMITED BY SIZE INTO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
           END-IF.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4000-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE.

      ******************************************************************
       4050-CHECK-BILLCHK-PATTERN.
           MOVE 'BILLCHK '             TO APT-PATTERN-CODE
           EXEC SQL
              SELECT PATTERN_CODE
                INTO :APT-PATTERN-CODE
                FROM CLMDB.CLM_ACTIVITY_PATTERN
               WHERE PATTERN_CODE = :APT-PATTERN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET WS-BILLCHK-DEFINED TO TRUE
              WHEN 100
                 STRING 'Activity pattern BILLCHK not defined - '
                        'BILSTAT activities will not be raised'
                        DELIMITED BY SIZE INTO EDR-MSG-TEXT
                 PERFORM 6100-WRITE-MSG
              WHEN OTHER
                 MOVE '4050-CHECK-BILLCHK' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-EVD-001 Run boundary is the run start timestamp
       4100-FETCH-EVENT.
           EXEC SQL
              FETCH EVQCSR
               INTO :EVQ-EVENT-ID, :EVQ-EVENT-TYPE, :EVQ-DIRECTION,
                    :EVQ-SOURCE-SYSTEM, :EVQ-TARGET-SYSTEM,
                    :EVQ-EVENT-KEY, :EVQ-PAYLOAD, :EVQ-RETRY-COUNT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-EVENT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4190-CLOSE-EVENTS.
           EXEC SQL CLOSE EVQCSR END-EXEC
           PERFORM 7000-COMMIT
           PERFORM 7100-WRITE-CHECKPOINT.

      ******************************************************************
      * BR-CLM-EVD-003 Same type, key and payload already queued
       4200-CHECK-ALREADY-DELIVERED.
           MOVE EVQ-PAYLOAD            TO BEV-PAYLOAD
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_EVENT_QUEUE
               WHERE SOURCE_SYSTEM = 'CLMS'
                 AND DIRECTION     = 'I'
                 AND EVENT_TYPE    = :EVQ-EVENT-TYPE
                 AND EVENT_KEY     = :EVQ-EVENT-KEY
                 AND PAYLOAD       = :BEV-PAYLOAD
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-CHECK-DELIVERED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET WS-EVENT-DUP         TO TRUE
              MOVE 'ALRDDLVR'          TO WS-FAIL-REASON
              MOVE 'Identical event already on billing queue'
                                       TO WS-FAIL-TEXT
           END-IF.

      ******************************************************************
      * BR-CLM-EVD-004 Inbound NEW row on the billing queue
       4210-INSERT-BILLING-EVENT.
           MOVE BSQ-SQ-VALUE           TO BEV-EVENT-ID
           MOVE EVQ-EVENT-TYPE         TO BEV-EVENT-TYPE
           MOVE 'I'                    TO BEV-DIRECTION
           MOVE 'CLMS'                 TO BEV-SOURCE-SYSTEM
           MOVE 'BILL'                 TO BEV-TARGET-SYSTEM
           MOVE EVQ-EVENT-KEY          TO BEV-EVENT-KEY
           MOVE 'NEW '                 TO BEV-EVENT-STATUS
           MOVE 0                      TO BEV-RETRY-COUNT
           EXEC SQL
              INSERT INTO BILDB.BIL_EVENT_QUEUE
                     (EVENT_ID, EVENT_TYPE, DIRECTION, SOURCE_SYSTEM,
                      TARGET_SYSTEM, EVENT_KEY, PAYLOAD, EVENT_STATUS,
                      RETRY_COUNT, ERROR_TEXT, CREATE_TS, PROCESSED_TS)
              VALUES (:BEV-EVENT-ID, :BEV-EVENT-TYPE, :BEV-DIRECTION,
                      :BEV-SOURCE-SYSTEM, :BEV-TARGET-SYSTEM,
                      :BEV-EVENT-KEY, :BEV-PAYLOAD, :BEV-EVENT-STATUS,
                      :BEV-RETRY-COUNT, NULL, CURRENT TIMESTAMP, NULL)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 MOVE 'DUPKEY  '       TO WS-FAIL-REASON
                 MOVE 'Billing queue key collision on EVQ sequence'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN -911
              WHEN -913
                 MOVE 'DEADLOCK'       TO WS-FAIL-REASON
                 MOVE 'Billing queue locked - retry next run'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN OTHER
                 MOVE '4210-INSERT-BILLING' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-UPDATE-SNAPSHOT-STATUS.
           MOVE PBS-STATUS             TO PSN-BILLING-STATUS
           EXEC SQL
              UPDATE CLMDB.CLM_POLICY_SNAPSHOT S
                 SET BILLING_STATUS = :PSN-BILLING-STATUS
               WHERE S.POLICY_NUMBER = :CLM-GENAPP-POL-NUM
                 AND EXISTS
                     (SELECT 1 FROM CLMDB.CLM_CLAIM C
                       WHERE C.CLAIM_ID = S.CLAIM_ID
                         AND C.CLAIM_STATE <> 'CLSD')
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4300-UPDATE-SNAPSHOT' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4310-FETCH-POLICY-CLAIM.
           EXEC SQL
              FETCH POLCSR
               INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                    :CLM-ASSIGNED-USER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-CLM-EOF        TO TRUE
              WHEN OTHER
                 EXEC SQL CLOSE POLCSR END-EXEC
                 MOVE '4310-FETCH-POLICY-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-READ-CHECK.
           EXEC SQL
              SELECT CLAIM_ID, CHECK_STATUS, CHECK_AMOUNT,
                     SCHEDULED_DATE, ISSUE_DATE
                INTO :CHK-CLAIM-ID, :CHK-CHECK-STATUS,
                     :CHK-CHECK-AMOUNT, :CHK-SCHEDULED-DATE,
                     :CHK-ISSUE-DATE :CHK-ISSUE-DATE-IND
                FROM CLMDB.CLM_CHECK
               WHERE CHECK_ID = :CHK-CHECK-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 'CHKNOTFD'       TO WS-FAIL-REASON
                 MOVE 'Claim check for DSBSENT not found'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN OTHER
                 MOVE '4400-READ-CHECK' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-EVD-009 Keep an issue date already recorded by CLMCHKIS
       4410-CLEAR-CHECK.
           IF CHK-ISSUE-DATE-IND < 0
              MOVE PDS-SENT-DATE       TO CHK-ISSUE-DATE
           END-IF
           MOVE PDS-DISB-ID            TO CHK-BILLING-DISB-REF
           EXEC SQL
              UPDATE CLMDB.CLM_CHECK
                 SET CHECK_STATUS     = 'CLRD',
                     ISSUE_DATE       = :CHK-ISSUE-DATE,
                     BILLING_DISB_REF = :CHK-BILLING-DISB-REF,
                     UPDATE_TS        = CURRENT TIMESTAMP
               WHERE CHECK_ID     = :CHK-CHECK-ID
                 AND CHECK_STATUS = :CHK-CHECK-STATUS
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 1
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 MOVE 'STALECHK'       TO WS-FAIL-REASON
                 MOVE 'Check status changed while clearing'
                                       TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN OTHER
                 MOVE '4410-CLEAR-CHECK' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * EVFAIL moves the row to ERRR on its fifth failure
       4500-CHECK-ERRORED.
           EXEC SQL
              SELECT EVENT_STATUS
                INTO :WS-NEW-STATUS
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_ID = :EVQ-EVENT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-CHECK-ERRORED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-NEW-STATUS = 'ERRR'
              ADD 1                    TO WS-T-ERRORED(TX)
              MOVE 'ERRORED'           TO EDR-EX-OUTCOME
              MOVE 'Retry limit reached - event parked in ERRR'
                                       TO WS-FAIL-TEXT
              PERFORM 6200-WRITE-EXCEPTION
              IF WS-MAX-RC < 8
                 MOVE 8                TO WS-MAX-RC
              END-IF
           END-IF.

      ******************************************************************
       6000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO EDR-H1-PAGE
           WRITE RPT-RECORD FROM EDR-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM EDR-HEAD-2 AFTER ADVANCING 1
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
           MOVE EDR-MSG-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE SPACES                 TO EDR-MSG-TEXT
           MOVE 0                      TO EDR-MSG-NUM.

      ******************************************************************
       6200-WRITE-EXCEPTION.
           MOVE EVQ-EVENT-ID           TO EDR-EX-EVENT-ID
           MOVE EVQ-EVENT-TYPE         TO EDR-EX-TYPE
           MOVE EVQ-DIRECTION          TO EDR-EX-DIR
           MOVE EVQ-EVENT-KEY          TO EDR-EX-KEY
           MOVE WS-FAIL-REASON         TO EDR-EX-REASON
           MOVE WS-FAIL-TEXT           TO EDR-EX-DETAIL
           MOVE EDR-EXC-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE 'CONTROL TOTALS BY EVENT TYPE' TO EDR-SECTION-TEXT
           MOVE EDR-SECTION-LINE       TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE EDR-TOT-HEAD           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
              MOVE WS-T-TYPE(TX)       TO EDR-TL-TYPE
              MOVE WS-T-DIR(TX)        TO EDR-TL-DIR
              MOVE WS-T-READ(TX)       TO EDR-TL-READ
              MOVE WS-T-DONE(TX)       TO EDR-TL-DONE
              MOVE WS-T-DUP(TX)        TO EDR-TL-DUP
              MOVE WS-T-FAILED(TX)     TO EDR-TL-FAILED
              MOVE WS-T-ERRORED(TX)    TO EDR-TL-ERRORED
              MOVE WS-T-CLAIMS(TX)     TO EDR-TL-CLAIMS
              MOVE WS-T-ACTS(TX)       TO EDR-TL-ACTS
              MOVE EDR-TOT-LINE        TO RPT-RECORD
              PERFORM 6050-WRITE-LINE
           END-PERFORM
           MOVE WS-TOTAL-DONE          TO EDR-MSG-NUM
           MOVE 'TOTAL EVENTS COMPLETED (DONE + DUPLICATE)'
                                       TO EDR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE WS-TOTAL-FAILED        TO EDR-MSG-NUM
           MOVE 'TOTAL EVENTS FAILED (RETURNED FOR RETRY OR ERRR)'
                                       TO EDR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE WS-MAX-RC              TO EDR-MSG-NUM
           MOVE 'RETURN CODE'          TO EDR-MSG-TEXT
           PERFORM 6100-WRITE-MSG.

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
           MOVE WS-PROGRAM-ID          TO EDC-PROGRAM
           IF NOT EDC-RUN-COMPLETE
              MOVE 'R'                 TO EDC-RUN-STATUS
           END-IF
           MOVE WS-BUS-DATE            TO EDC-BUS-DATE
           MOVE WS-TOTAL-DONE          TO EDC-EVENTS-DONE
           MOVE WS-TOTAL-FAILED        TO EDC-EVENTS-FAILED
           MOVE FUNCTION CURRENT-DATE  TO EDC-CHECKPOINT-TS
           CLOSE CHK-FILE
           OPEN OUTPUT CHK-FILE
           WRITE EDC-CHECKPOINT-REC
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'CLMEVTDR CHECKPOINT WRITE FAILED ' WS-CHK-STATUS
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF.

      ******************************************************************
       8100-ADD-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO HIA-USER-ID HIA-CALLER-PGM
                                          HIA-HI-PROGRAM
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE CLM-CLAIM-ID           TO HIA-HI-CLAIM-ID
           IF EVQ-EVENT-TYPE = 'BILSTAT '
              MOVE 'BILSTAT '          TO HIA-HI-EVENT-TYPE
           ELSE
              MOVE 'CHKCLRD '          TO HIA-HI-EVENT-TYPE
           END-IF
           MOVE WS-DESC                TO HIA-HI-DESCRIPTION
           CALL WS-HIS-PGM USING WS-HIS-CA
           EVALUATE TRUE
              WHEN HIA-RC-OK
              WHEN HIA-RC-WARNING
                 CONTINUE
              WHEN HIA-RETURN-CODE < 90
                 MOVE HIA-REASON-CODE  TO WS-FAIL-REASON
                 MOVE HIA-MESSAGE      TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN OTHER
                 MOVE HIA-MESSAGE      TO EDR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
           END-EVALUATE.

      ******************************************************************
       8200-ADD-ACTIVITY.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTNEW  '             TO AVA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO AVA-USER-ID AVA-CALLER-PGM
           MOVE WS-BUS-DATE            TO AVA-BUS-DATE
           MOVE CLM-CLAIM-ID           TO AVA-AV-CLAIM-ID
           MOVE 0                      TO AVA-AV-EXPOSURE-ID
           MOVE 'BILLCHK '             TO AVA-AV-PATTERN-CODE
           MOVE CLM-ASSIGNED-USER      TO AVA-AV-ASSIGNED-USER
           STRING 'Policy billing ' PBS-STATUS
                  ' - confirm cover before any payment'
                  DELIMITED BY SIZE INTO AVA-AV-SUBJECT
           CALL WS-ACT-PGM USING WS-ACT-CA
           EVALUATE TRUE
              WHEN AVA-RC-OK
              WHEN AVA-RC-WARNING
                 CONTINUE
              WHEN AVA-RETURN-CODE < 90
                 MOVE AVA-REASON-CODE  TO WS-FAIL-REASON
                 MOVE AVA-MESSAGE      TO WS-FAIL-TEXT
                 SET WS-EVENT-FAILED   TO TRUE
              WHEN OTHER
                 MOVE AVA-MESSAGE      TO EDR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
           END-EVALUATE.

      ******************************************************************
       8500-NEXT-BILLING-EVENT-ID.
           INITIALIZE WS-BSQ-CA
           MOVE 'SEQNEXT '             TO BSQ-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO BSQ-USER-ID BSQ-CALLER-PGM
           MOVE WS-BUS-DATE            TO BSQ-BUS-DATE
           MOVE 'EVQ     '             TO BSQ-SQ-NAME
           MOVE 1                      TO BSQ-SQ-COUNT
           CALL WS-BSQ-PGM USING WS-BSQ-CA
           IF NOT BSQ-RC-OK AND NOT BSQ-RC-WARNING
              IF BSQ-RETURN-CODE >= 90
                 MOVE BSQ-MESSAGE      TO EDR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
              END-IF
              MOVE BSQ-REASON-CODE     TO WS-FAIL-REASON
              MOVE BSQ-MESSAGE         TO WS-FAIL-TEXT
              SET WS-EVENT-FAILED      TO TRUE
           END-IF.

      ******************************************************************
       9000-RETURN.
           CLOSE RPT-FILE
           CLOSE CHK-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           MOVE EVQ-EVENT-ID           TO WS-KEY-DISP
           MOVE WS-KEY-DISP            TO WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'CLMEVTDR Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           EXEC SQL ROLLBACK END-EXEC
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO EDR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 12                     TO WS-MAX-RC
           PERFORM 9000-RETURN.
