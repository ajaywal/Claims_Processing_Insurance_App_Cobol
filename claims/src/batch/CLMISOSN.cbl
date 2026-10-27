       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMISOSN.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMISOSN                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : ISO ClaimSearch-style industry fraud bureau      *
      *               interface.                                       *
      *               PARM SEND: queue CLM_ISO_REQUEST rows for new    *
      *               claims (INIT) and claims whose parties changed   *
      *               since the last submission (UPDT), then write     *
      *               every PEND request with its parties to ISOOUT    *
      *               and mark it SENT.                                *
      *               PARM RECV: balance the ISOIN response file, post *
      *               match counts (RCVD) and refer claims with two or *
      *               more bureau matches to SIU scoring.              *
      *               Optional ',nnnn' after the mode = commit freq.   *
      * CALLS       : CLMSEQGN (SEQNEXT ISO)                           *
      *               CLMSIUSC (SIUSCR)                                *
      *               CLMHIST0 (HISADD)                                *
      *               CLMERRLG (ERRLOG)                                *
      * FILES       : ISOOUT  request file (CLMISOR1)  SEND            *
      *               ISOIN   response file (CLMISOS1) RECV            *
      *               RPTOUT  132-column run report                    *
      *               CHKPT   checkpoint / restart record (CLMISOC1)   *
      * TABLES      : CLM_ISO_REQUEST     (C R U)                      *
      *               CLM_CLAIM           (R)                          *
      *               CLM_POLICY_SNAPSHOT (R)                          *
      *               CLM_CLAIM_CONTACT   (R)                          *
      *               CLM_CONTACT         (R)                          *
      *               CLM_RISK_UNIT       (R)                          *
      *               CLM_CONTROL         (R)                          *
      *               GENAPP.CUSTOMER     (R)                          *
      * RULES       : BR-CLM-ISO-001 Every non-draft claim without an  *
      *                              ISO request is queued once (INIT) *
      *               BR-CLM-ISO-002 Parties added or changed after    *
      *                              the last SENT request re-queue    *
      *                              the claim (UPDT) unless a request *
      *                              is already pending                *
      *               BR-CLM-ISO-003 A request needs claim number,     *
      *                              loss date and insured name; motor *
      *                              also a vehicle registration - an  *
      *                              incomplete request stays PEND     *
      *               BR-CLM-ISO-004 At most 20 parties per request    *
      *               BR-CLM-ISO-005 Responses post only if the file   *
      *                              has a header and the trailer      *
      *                              count balances the detail records *
      *               BR-CLM-ISO-006 Only SENT requests accept a       *
      *                              response; RCVD is a duplicate and *
      *                              a claim number mismatch rejects   *
      *               BR-CLM-ISO-007 A bureau-rejected request (ER) is *
      *                              reset to PEND for resubmission    *
      *               BR-CLM-ISO-008 Two or more matches trigger SIU   *
      *                              scoring and a claim history row   *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ISO-OUT-FILE ASSIGN TO ISOOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-OUT-STATUS.
           SELECT ISO-IN-FILE  ASSIGN TO ISOIN
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-IN-STATUS.
           SELECT RPT-FILE     ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL CHK-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ISO-OUT-FILE
           RECORDING MODE IS F.
           COPY CLMISOR1.
       FD  ISO-IN-FILE
           RECORDING MODE IS F.
           COPY CLMISOS1.
       FD  RPT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).
       FD  CHK-FILE
           RECORDING MODE IS F.
           COPY CLMISOC1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMISOSN'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-SIU-PGM                  PIC X(8)  VALUE 'CLMSIUSC'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-ISO-SENDER-ID            PIC X(8)  VALUE 'PNCCLM01'.
       01  WS-MAX-PARTIES              PIC S9(4) COMP VALUE 20.
       01  WS-SIU-MATCH-THRESHOLD      PIC S9(4) COMP VALUE 2.

       01  WS-FILE-STATUS.
           05 WS-OUT-STATUS            PIC XX    VALUE '00'.
           05 WS-IN-STATUS             PIC XX    VALUE '00'.
           05 WS-RPT-STATUS            PIC XX    VALUE '00'.
           05 WS-CHK-STATUS            PIC XX    VALUE '00'.

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-PTY-EOF-SW            PIC X     VALUE 'N'.
              88 WS-PTY-EOF             VALUE 'Y'.
           05 WS-REQ-OK-SW             PIC X     VALUE 'Y'.
              88 WS-REQ-OK              VALUE 'Y'.
              88 WS-REQ-REJECTED        VALUE 'N'.
           05 WS-RESTART-SW            PIC X     VALUE 'N'.
              88 WS-RESTARTING          VALUE 'Y'.

       01  WS-PARM-WORK.
           05 WS-RUN-MODE              PIC X(4)  VALUE SPACES.
              88 WS-MODE-SEND           VALUE 'SEND'.
              88 WS-MODE-RECV           VALUE 'RECV'.
           05 WS-PARM-FREQ             PIC X(5)  JUSTIFIED RIGHT
                                                 VALUE SPACES.
           05 WS-PARM-FREQ-N REDEFINES WS-PARM-FREQ
                                       PIC 9(5).

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-RUN-TS                PIC X(26) VALUE SPACES.
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-UNCOMMITTED           PIC S9(4) COMP VALUE 0.
           05 WS-RESUME-KEY            PIC S9(9) COMP VALUE 0.
           05 WS-RESUME-RECNO          PIC S9(9) COMP VALUE 0.
           05 WS-RECNO                 PIC S9(9) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.

       01  WS-TOTALS.
           05 WS-QUEUED-INIT           PIC S9(9) COMP-3 VALUE 0.
           05 WS-QUEUED-UPDT           PIC S9(9) COMP-3 VALUE 0.
           05 WS-SENT                  PIC S9(9) COMP-3 VALUE 0.
           05 WS-HELD                  PIC S9(9) COMP-3 VALUE 0.
           05 WS-PARTIES-SENT          PIC S9(9) COMP-3 VALUE 0.
           05 WS-CLAIM-HASH            PIC S9(15) COMP-3 VALUE 0.
           05 WS-RESP-READ             PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESP-POSTED           PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESP-NOMATCH          PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESP-REFERRED         PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESP-RESUBMIT         PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESP-DUP              PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESP-REJECTED         PIC S9(9) COMP-3 VALUE 0.
           05 WS-BAL-DETAILS           PIC S9(9) COMP-3 VALUE 0.
           05 WS-BAL-TRAILER           PIC S9(9) COMP-3 VALUE 0.

       01  WS-WORK.
           05 WS-REASON                PIC X(8)  VALUE SPACES.
           05 WS-DETAIL                PIC X(75) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-ERR-KEY               PIC X(20) VALUE SPACES.
           05 WS-REQUEST-TYPE          PIC X(4)  VALUE SPACES.
           05 WS-MATCHES-DISP          PIC ZZ9.
           05 WS-SCORE-DISP            PIC ZZZ9.
           05 WS-PTY-COUNT             PIC S9(4) COMP VALUE 0.
           05 WS-PX                    PIC S9(4) COMP VALUE 0.

      * Parties staged so the request record can carry their count
       01  WS-PARTY-TABLE.
           05 WS-PARTY OCCURS 20 TIMES.
              10 WS-P-ROLE             PIC X(8).
              10 WS-P-TYPE             PIC X(4).
              10 WS-P-FIRST            PIC X(30).
              10 WS-P-LAST             PIC X(40).
              10 WS-P-COMPANY          PIC X(60).
              10 WS-P-DOB              PIC X(10).
              10 WS-P-POSTCODE         PIC X(8).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDISO END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDPSN END-EXEC.
           EXEC SQL INCLUDE CLMDCCR END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDRSK END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.
           EXEC SQL INCLUDE GENDCUS END-EXEC.

      * BR-CLM-ISO-001 Non-draft claims never submitted
           EXEC SQL DECLARE NEWCSR CURSOR WITH HOLD FOR
                SELECT C.CLAIM_ID
                  FROM CLMDB.CLM_CLAIM C
                 WHERE C.CLAIM_STATE <> 'DRFT'
                   AND NOT EXISTS
                       (SELECT 1 FROM CLMDB.CLM_ISO_REQUEST I
                         WHERE I.CLAIM_ID = C.CLAIM_ID)
                 ORDER BY C.CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

      * BR-CLM-ISO-002 Parties added / changed since the last send
           EXEC SQL DECLARE CHGCSR CURSOR WITH HOLD FOR
                SELECT C.CLAIM_ID
                  FROM CLMDB.CLM_CLAIM C
                  JOIN (SELECT CLAIM_ID, MAX(SENT_TS) AS LAST_SENT
                          FROM CLMDB.CLM_ISO_REQUEST
                         WHERE SENT_TS IS NOT NULL
                         GROUP BY CLAIM_ID) S
                    ON S.CLAIM_ID = C.CLAIM_ID
                 WHERE C.CLAIM_STATE NOT IN ('DRFT', 'CLSD')
                   AND NOT EXISTS
                       (SELECT 1 FROM CLMDB.CLM_ISO_REQUEST P
                         WHERE P.CLAIM_ID   = C.CLAIM_ID
                           AND P.ISO_STATUS = 'PEND')
                   AND EXISTS
                       (SELECT 1
                          FROM CLMDB.CLM_CLAIM_CONTACT CC
                          JOIN CLMDB.CLM_CONTACT K
                            ON K.CONTACT_ID = CC.CONTACT_ID
                         WHERE CC.CLAIM_ID = C.CLAIM_ID
                           AND (CC.ADDED_TS > S.LAST_SENT
                                OR K.UPDATE_TS > S.LAST_SENT))
                 ORDER BY C.CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE PNDCSR CURSOR WITH HOLD FOR
                SELECT I.ISO_REQ_ID, I.CLAIM_ID, I.REQUEST_TYPE,
                       C.CLAIM_NUMBER, C.GENAPP_POL_NUM,
                       C.GENAPP_CUST_NUM, C.POLICY_TYPE,
                       C.LOSS_DATE, C.LOSS_TYPE, C.LOSS_CAUSE,
                       C.LOSS_POSTCODE
                  FROM CLMDB.CLM_ISO_REQUEST I
                  JOIN CLMDB.CLM_CLAIM C
                    ON C.CLAIM_ID = I.CLAIM_ID
                 WHERE I.ISO_STATUS = 'PEND'
                   AND I.ISO_REQ_ID > :WS-RESUME-KEY
                 ORDER BY I.ISO_REQ_ID
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE PTYCSR CURSOR FOR
                SELECT CC.CONTACT_ROLE, K.CONTACT_TYPE,
                       K.FIRST_NAME, K.LAST_NAME, K.COMPANY_NAME,
                       K.DATE_OF_BIRTH, K.POSTCODE
                  FROM CLMDB.CLM_CLAIM_CONTACT CC
                  JOIN CLMDB.CLM_CONTACT K
                    ON K.CONTACT_ID = CC.CONTACT_ID
                 WHERE CC.CLAIM_ID = :CLM-CLAIM-ID
                 ORDER BY CC.CONTACT_ROLE, CC.CONTACT_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-SIU-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SIA-==.
           03 SIA-BODY.
              COPY CLMCSIU REPLACING LEADING ==CA-== BY ==SIA-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY CLMISOP1.

       LINKAGE SECTION.
       01  LK-PARM.
           05 LK-PARM-LEN              PIC S9(4) COMP.
           05 LK-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF WS-MODE-SEND
              PERFORM 2000-SEND-MODE
           ELSE
              PERFORM 2500-RECV-MODE
           END-IF
           PERFORM 7000-COMMIT
           MOVE 'C'                    TO ISC-RUN-STATUS
           PERFORM 7100-WRITE-CHECKPOINT
           PERFORM 6500-PRINT-TOTALS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'CLMISOSN RPTOUT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           IF LK-PARM-LEN > 0
              UNSTRING LK-PARM-DATA(1:LK-PARM-LEN) DELIMITED BY ','
                  INTO WS-RUN-MODE WS-PARM-FREQ
              END-UNSTRING
           END-IF
           MOVE WS-RUN-MODE            TO ISP-H1-MODE
           IF NOT WS-MODE-SEND AND NOT WS-MODE-RECV
              MOVE 'Invalid PARM - expected SEND or RECV'
                                       TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           IF WS-PARM-FREQ NOT = SPACES
              INSPECT WS-PARM-FREQ REPLACING LEADING SPACE BY ZERO
              IF WS-PARM-FREQ-N IS NUMERIC
              AND WS-PARM-FREQ-N > 0 AND WS-PARM-FREQ-N <= 5000
                 MOVE WS-PARM-FREQ-N   TO WS-COMMIT-FREQ
              END-IF
           END-IF
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE, CURRENT TIMESTAMP
                INTO :CTL-BUSINESS-DATE, :WS-RUN-TS
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '1000-INITIALISE'   TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE ISP-H1-BUS-DATE
           MOVE WS-RUN-TS(1:19)        TO ISP-H1-RUN-TS
           PERFORM 1100-READ-RESTART.

      ******************************************************************
      * Restart resumes in the same mode for the same business date,
      * restoring the control totals the trailer must carry.
       1100-READ-RESTART.
           OPEN INPUT CHK-FILE
           IF WS-CHK-STATUS = '00'
              READ CHK-FILE
                 AT END
                    CONTINUE
                 NOT AT END
                    IF ISC-PROGRAM = WS-PROGRAM-ID
                    AND ISC-MODE = WS-RUN-MODE
                    AND ISC-RUN-ACTIVE
                    AND ISC-BUS-DATE = WS-BUS-DATE
                       SET WS-RESTARTING TO TRUE
                       MOVE ISC-LAST-KEY   TO WS-RESUME-KEY
                       MOVE ISC-LAST-RECNO TO WS-RESUME-RECNO
                       MOVE ISC-REQUESTS   TO WS-SENT
                       MOVE ISC-PARTIES    TO WS-PARTIES-SENT
                       MOVE ISC-CLAIM-HASH TO WS-CLAIM-HASH
                    END-IF
              END-READ
              CLOSE CHK-FILE
           END-IF
           OPEN OUTPUT CHK-FILE
           IF WS-CHK-STATUS NOT = '00'
              MOVE 'CHKPT file cannot be opened for output'
                                       TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           INITIALIZE ISC-CHECKPOINT-REC
           IF WS-RESTARTING
              MOVE WS-RESUME-KEY       TO ISC-LAST-KEY WS-KEY-DISP
              MOVE WS-RESUME-RECNO     TO ISC-LAST-RECNO
              STRING 'RESTART: resuming after ISO request id '
                     WS-KEY-DISP ' / response record '
                     ISC-LAST-RECNO
                     DELIMITED BY SIZE INTO ISP-MSG-TEXT
              PERFORM 6100-WRITE-MSG
           END-IF.

      ******************************************************************
       2000-SEND-MODE.
           PERFORM 2100-QUEUE-NEW-CLAIMS
           PERFORM 2200-QUEUE-CHANGED-PARTIES
           PERFORM 7000-COMMIT
           IF WS-RESTARTING
              OPEN EXTEND ISO-OUT-FILE
           ELSE
              OPEN OUTPUT ISO-OUT-FILE
           END-IF
           IF WS-OUT-STATUS NOT = '00'
              MOVE 'ISOOUT cannot be opened' TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
      * A restarted run extends the file it began; header once only
           IF NOT WS-RESTARTING
              MOVE SPACES              TO ISQ-HEADER-REC
              MOVE 'H'                 TO ISQ-REC-TYPE
              MOVE WS-ISO-SENDER-ID    TO ISQ-H-SENDER-ID
              MOVE WS-BUS-DATE         TO ISQ-H-FILE-DATE
              MOVE WS-RUN-TS           TO ISQ-H-CREATE-TS
              WRITE ISQ-HEADER-REC
           END-IF
           MOVE ISP-DET-HEAD           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           PERFORM 2300-SEND-PENDING
           MOVE SPACES                 TO ISQ-TRAILER-REC
           MOVE 'T'                    TO ISQ-REC-TYPE
           MOVE WS-SENT                TO ISQ-T-REQUEST-COUNT
           MOVE WS-PARTIES-SENT        TO ISQ-T-PARTY-COUNT
           MOVE WS-CLAIM-HASH          TO ISQ-T-CLAIM-HASH
           WRITE ISQ-TRAILER-REC
           CLOSE ISO-OUT-FILE.

      ******************************************************************
       2100-QUEUE-NEW-CLAIMS.
           MOVE 'INIT'                 TO WS-REQUEST-TYPE
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN NEWCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-QUEUE-NEW-CLAIMS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-EOF
              EXEC SQL FETCH NEWCSR INTO :CLM-CLAIM-ID END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 4100-QUEUE-REQUEST
                    ADD 1              TO WS-QUEUED-INIT
                 WHEN 100
                    SET WS-EOF         TO TRUE
                 WHEN OTHER
                    MOVE '2100-QUEUE-NEW-CLAIMS' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE NEWCSR END-EXEC.

      ******************************************************************
       2200-QUEUE-CHANGED-PARTIES.
           MOVE 'UPDT'                 TO WS-REQUEST-TYPE
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN CHGCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-QUEUE-CHANGED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-EOF
              EXEC SQL FETCH CHGCSR INTO :CLM-CLAIM-ID END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 4100-QUEUE-REQUEST
                    ADD 1              TO WS-QUEUED-UPDT
                 WHEN 100
                    SET WS-EOF         TO TRUE
                 WHEN OTHER
                    MOVE '2200-QUEUE-CHANGED' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE CHGCSR END-EXEC.

      ******************************************************************
       2300-SEND-PENDING.
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN PNDCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-SEND-PENDING' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4200-FETCH-PENDING
           PERFORM UNTIL WS-EOF
              PERFORM 2310-BUILD-REQUEST
              IF WS-REQ-OK
                 PERFORM 2320-WRITE-REQUEST
                 PERFORM 4230-MARK-SENT
              ELSE
                 ADD 1                 TO WS-HELD
                 MOVE 'HELD'           TO ISP-D-OUTCOME
                 PERFORM 6200-WRITE-DETAIL
                 IF WS-MAX-RC < 4
                    MOVE 4             TO WS-MAX-RC
                 END-IF
              END-IF
              MOVE ISO-ISO-REQ-ID      TO ISC-LAST-KEY
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
                 PERFORM 7100-WRITE-CHECKPOINT
              END-IF
              PERFORM 4200-FETCH-PENDING
           END-PERFORM
           EXEC SQL CLOSE PNDCSR END-EXEC.

      ******************************************************************
      * BR-CLM-ISO-003 Mandatory identification data for the bureau
       2310-BUILD-REQUEST.
           SET WS-REQ-OK               TO TRUE
           MOVE SPACES                 TO WS-REASON WS-DETAIL
           PERFORM 4300-READ-INSURED
           PERFORM 4320-READ-VEHICLE-REG
           EVALUATE TRUE
              WHEN CLM-CLAIM-NUMBER = SPACES
                 MOVE 'NOCLMNO '       TO WS-REASON
                 MOVE 'Claim number not yet assigned' TO WS-DETAIL
              WHEN CLM-LOSS-DATE = SPACES
                 MOVE 'NOLOSSDT'       TO WS-REASON
                 MOVE 'Loss date missing' TO WS-DETAIL
              WHEN PSN-INSURED-NAME-LEN = 0
                 MOVE 'NOINSRD '       TO WS-REASON
                 MOVE 'Insured name missing - policy snapshot absent'
                                       TO WS-DETAIL
              WHEN CLM-POLICY-TYPE = 'M' AND RSK-VEHICLE-REG = SPACES
                 MOVE 'NOVEHREG'       TO WS-REASON
                 MOVE 'Motor claim without vehicle registration'
                                       TO WS-DETAIL
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           IF WS-REASON NOT = SPACES
              SET WS-REQ-REJECTED      TO TRUE
           ELSE
              PERFORM 4400-LOAD-PARTIES
           END-IF.

      ******************************************************************
       2320-WRITE-REQUEST.
           MOVE SPACES                 TO ISQ-REQUEST-REC
           MOVE 'R'                    TO ISQ-REC-TYPE
           MOVE ISO-ISO-REQ-ID         TO ISQ-R-ISO-REQ-ID
           MOVE ISO-REQUEST-TYPE       TO ISQ-R-REQUEST-TYPE
           MOVE CLM-CLAIM-NUMBER       TO ISQ-R-CLAIM-NUMBER
           MOVE CLM-GENAPP-POL-NUM     TO ISQ-R-POLICY-NUMBER
           MOVE CLM-POLICY-TYPE        TO ISQ-R-POLICY-TYPE
           MOVE PSN-INSURED-NAME-TEXT(1:PSN-INSURED-NAME-LEN)
                                       TO ISQ-R-INSURED-NAME
           MOVE GCU-DATEOFBIRTH        TO ISQ-R-INSURED-DOB
           MOVE GCU-POSTCODE           TO ISQ-R-INSURED-POSTCODE
           MOVE RSK-VEHICLE-REG        TO ISQ-R-VEHICLE-REG
           MOVE CLM-LOSS-DATE          TO ISQ-R-LOSS-DATE
           MOVE CLM-LOSS-TYPE          TO ISQ-R-LOSS-TYPE
           MOVE CLM-LOSS-CAUSE         TO ISQ-R-LOSS-CAUSE
           MOVE CLM-LOSS-POSTCODE      TO ISQ-R-LOSS-POSTCODE
           MOVE WS-PTY-COUNT           TO ISQ-R-PARTY-COUNT
           WRITE ISQ-REQUEST-REC
           PERFORM VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-PTY-COUNT
              MOVE SPACES              TO ISQ-PARTY-REC
              MOVE 'P'                 TO ISQ-REC-TYPE
              MOVE ISO-ISO-REQ-ID      TO ISQ-P-ISO-REQ-ID
              MOVE WS-P-ROLE(WS-PX)    TO ISQ-P-ROLE
              MOVE WS-P-TYPE(WS-PX)    TO ISQ-P-CONTACT-TYPE
              MOVE WS-P-FIRST(WS-PX)   TO ISQ-P-FIRST-NAME
              MOVE WS-P-LAST(WS-PX)    TO ISQ-P-LAST-NAME
              MOVE WS-P-COMPANY(WS-PX) TO ISQ-P-COMPANY-NAME
              MOVE WS-P-DOB(WS-PX)     TO ISQ-P-DOB
              MOVE WS-P-POSTCODE(WS-PX) TO ISQ-P-POSTCODE
              WRITE ISQ-PARTY-REC
              ADD 1                    TO WS-PARTIES-SENT
           END-PERFORM
           IF WS-OUT-STATUS NOT = '00'
              MOVE 'ISOOUT write failed' TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           ADD 1                       TO WS-SENT
           ADD ISO-CLAIM-ID            TO WS-CLAIM-HASH.

      ******************************************************************
       2500-RECV-MODE.
           PERFORM 2510-BALANCE-FILE
           OPEN INPUT ISO-IN-FILE
           IF WS-IN-STATUS NOT = '00'
              MOVE 'ISOIN cannot be opened' TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE ISP-DET-HEAD           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE 0                      TO WS-RECNO
           MOVE 'N'                    TO WS-EOF-SW
           PERFORM 5100-READ-RESPONSE
           PERFORM UNTIL WS-EOF
              IF ISR-DETAIL AND WS-RECNO > WS-RESUME-RECNO
                 ADD 1                 TO WS-RESP-READ
                 PERFORM 2600-POST-RESPONSE
                 MOVE WS-RECNO         TO ISC-LAST-RECNO
                 ADD 1                 TO WS-UNCOMMITTED
                 IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                    PERFORM 7000-COMMIT
                    PERFORM 7100-WRITE-CHECKPOINT
                 END-IF
              END-IF
              PERFORM 5100-READ-RESPONSE
           END-PERFORM
           CLOSE ISO-IN-FILE.

      ******************************************************************
      * BR-CLM-ISO-005 Balance the whole file before posting anything
       2510-BALANCE-FILE.
           OPEN INPUT ISO-IN-FILE
           IF WS-IN-STATUS NOT = '00'
              MOVE 'ISOIN cannot be opened' TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE 'N'                    TO WS-EOF-SW
           MOVE 0                      TO WS-RECNO
           PERFORM 5100-READ-RESPONSE
           IF WS-EOF OR NOT ISR-HEADER
              MOVE 'ISOIN has no header record - nothing posted'
                                       TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE -1                     TO WS-BAL-TRAILER
           PERFORM 5100-READ-RESPONSE
           PERFORM UNTIL WS-EOF
              EVALUATE TRUE
                 WHEN ISR-DETAIL
                    ADD 1              TO WS-BAL-DETAILS
                 WHEN ISR-TRAILER AND ISR-T-DETAIL-COUNT IS NUMERIC
                    MOVE ISR-T-DETAIL-NUM TO WS-BAL-TRAILER
                 WHEN OTHER
                    MOVE 'ISOIN unknown record type - nothing posted'
                                       TO ISP-MSG-TEXT
                    PERFORM 6900-ABORT-RUN
              END-EVALUATE
              PERFORM 5100-READ-RESPONSE
           END-PERFORM
           CLOSE ISO-IN-FILE
           IF WS-BAL-TRAILER NOT = WS-BAL-DETAILS
              MOVE WS-BAL-DETAILS      TO WS-KEY-DISP
              STRING 'ISOIN out of balance - trailer missing or count '
                     'differs from ' WS-KEY-DISP ' detail records'
                     DELIMITED BY SIZE INTO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF.

      ******************************************************************
       2600-POST-RESPONSE.
           SET WS-REQ-OK               TO TRUE
           MOVE SPACES                 TO WS-REASON WS-DETAIL
           MOVE 0                      TO ISP-D-MATCHES ISP-D-REQ-ID
           MOVE SPACES                 TO ISP-D-TYPE
           MOVE ISR-D-CLAIM-NUMBER     TO ISP-D-CLAIM-NUMBER
           EVALUATE TRUE
              WHEN ISR-D-ISO-REQ-ID IS NOT NUMERIC
                 MOVE 'REQIDBAD'       TO WS-REASON
                 MOVE 'Response request id not numeric' TO WS-DETAIL
              WHEN NOT ISR-D-ACCEPTED AND NOT ISR-D-REJECTED
                 MOVE 'RESPCODE'       TO WS-REASON
                 MOVE 'Response code must be OK or ER' TO WS-DETAIL
              WHEN ISR-D-ACCEPTED AND ISR-D-MATCH-COUNT IS NOT NUMERIC
                 MOVE 'MATCHCNT'       TO WS-REASON
                 MOVE 'Match count not numeric' TO WS-DETAIL
              WHEN OTHER
                 MOVE ISR-D-ISO-REQ-NUM TO ISO-ISO-REQ-ID
                 PERFORM 4500-READ-ISO-REQUEST
           END-EVALUATE
           IF WS-REASON = SPACES
              PERFORM 3500-CHECK-RESPONSE-STATE
           END-IF
           IF WS-REASON NOT = SPACES AND WS-REASON NOT = 'ALRDRCVD'
              SET WS-REQ-REJECTED      TO TRUE
           END-IF
           EVALUATE TRUE
              WHEN WS-REQ-REJECTED
                 ADD 1                 TO WS-RESP-REJECTED
                 MOVE 'REJECTED'       TO ISP-D-OUTCOME
                 PERFORM 6200-WRITE-DETAIL
                 IF WS-MAX-RC < 8
                    MOVE 8             TO WS-MAX-RC
                 END-IF
              WHEN WS-REASON = 'ALRDRCVD'
                 ADD 1                 TO WS-RESP-DUP
                 MOVE 'DUPLICAT'       TO ISP-D-OUTCOME
                 PERFORM 6200-WRITE-DETAIL
              WHEN ISR-D-REJECTED
      * BR-CLM-ISO-007 Bureau rejected the request: resubmit
                 PERFORM 4520-RESET-FOR-RESEND
                 ADD 1                 TO WS-RESP-RESUBMIT
                 MOVE 'RESUBMIT'       TO ISP-D-OUTCOME
                 MOVE ISR-D-ERROR-CODE TO WS-REASON
                 MOVE 'Bureau rejected request - reset to PEND'
                                       TO WS-DETAIL
                 PERFORM 6200-WRITE-DETAIL
              WHEN OTHER
                 PERFORM 4510-POST-MATCHES
                 ADD 1                 TO WS-RESP-POSTED
                 PERFORM 2650-ACT-ON-MATCHES
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-ISO-008 Two or more bureau matches -> SIU scoring
       2650-ACT-ON-MATCHES.
           IF ISO-MATCH-COUNT < WS-SIU-MATCH-THRESHOLD
              ADD 1                    TO WS-RESP-NOMATCH
              EXIT PARAGRAPH
           END-IF
           PERFORM 8400-SCORE-SIU
           MOVE ISO-MATCH-COUNT        TO WS-MATCHES-DISP
           MOVE SIA-SI-TOTAL-SCORE     TO WS-SCORE-DISP
           MOVE SPACES                 TO WS-DETAIL
           STRING 'ISO ClaimSearch ' ISO-REQUEST-TYPE ' ref '
                  ISR-D-ISO-REF ' returned ' WS-MATCHES-DISP
                  ' prior matches - SIU score ' WS-SCORE-DISP
                  ' referred ' SIA-SI-REFERRED
                  DELIMITED BY SIZE INTO WS-DETAIL
           PERFORM 8100-ADD-HISTORY
           ADD 1                       TO WS-RESP-REFERRED
           MOVE 'SIUSCORE'             TO ISP-D-OUTCOME
           MOVE 'MATCHES '             TO WS-REASON
           MOVE ISO-MATCH-COUNT        TO ISP-D-MATCHES
           PERFORM 6200-WRITE-DETAIL.

      ******************************************************************
      * BR-CLM-ISO-006 State check on the request being answered
       3500-CHECK-RESPONSE-STATE.
           EVALUATE TRUE
              WHEN CLM-CLAIM-NUMBER NOT = ISR-D-CLAIM-NUMBER
                 MOVE 'CLMMISMT'       TO WS-REASON
                 MOVE 'Claim number differs from the request sent'
                                       TO WS-DETAIL
              WHEN ISO-ISO-STATUS = 'RCVD'
                 MOVE 'ALRDRCVD'       TO WS-REASON
                 MOVE 'Response already posted for this request'
                                       TO WS-DETAIL
              WHEN ISO-ISO-STATUS NOT = 'SENT'
                 MOVE 'NOTSENT '       TO WS-REASON
                 STRING 'Request is ' ISO-ISO-STATUS
                        ' - only SENT requests accept a response'
                        DELIMITED BY SIZE INTO WS-DETAIL
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.

      ******************************************************************
       4100-QUEUE-REQUEST.
           PERFORM 8300-NEXT-ISO-ID
           MOVE SQA-SQ-VALUE           TO ISO-ISO-REQ-ID
           MOVE CLM-CLAIM-ID           TO ISO-CLAIM-ID
           MOVE WS-REQUEST-TYPE        TO ISO-REQUEST-TYPE
           EXEC SQL
              INSERT INTO CLMDB.CLM_ISO_REQUEST
                     (ISO_REQ_ID, CLAIM_ID, REQUEST_TYPE, ISO_STATUS,
                      MATCH_COUNT, ISO_REF, SENT_TS, RESPONSE_TS)
              VALUES (:ISO-ISO-REQ-ID, :ISO-CLAIM-ID,
                      :ISO-REQUEST-TYPE, 'PEND', 0, ' ', NULL, NULL)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-QUEUE-REQUEST' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-UNCOMMITTED
           IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
              PERFORM 7000-COMMIT
           END-IF.

      ******************************************************************
       4200-FETCH-PENDING.
           EXEC SQL
              FETCH PNDCSR
               INTO :ISO-ISO-REQ-ID, :ISO-CLAIM-ID, :ISO-REQUEST-TYPE,
                    :CLM-CLAIM-NUMBER, :CLM-GENAPP-POL-NUM,
                    :CLM-GENAPP-CUST-NUM, :CLM-POLICY-TYPE,
                    :CLM-LOSS-DATE, :CLM-LOSS-TYPE, :CLM-LOSS-CAUSE,
                    :CLM-LOSS-POSTCODE :CLM-LOSS-POSTCODE-IND
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE ISO-CLAIM-ID     TO CLM-CLAIM-ID
                 IF CLM-LOSS-POSTCODE-IND < 0
                    MOVE SPACES        TO CLM-LOSS-POSTCODE
                 END-IF
                 MOVE ISO-ISO-REQ-ID   TO ISP-D-REQ-ID
                 MOVE CLM-CLAIM-NUMBER TO ISP-D-CLAIM-NUMBER
                 MOVE ISO-REQUEST-TYPE TO ISP-D-TYPE
                 MOVE 0                TO ISP-D-MATCHES
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4200-FETCH-PENDING' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4230-MARK-SENT.
           EXEC SQL
              UPDATE CLMDB.CLM_ISO_REQUEST
                 SET ISO_STATUS = 'SENT',
                     SENT_TS    = CURRENT TIMESTAMP
               WHERE ISO_REQ_ID = :ISO-ISO-REQ-ID
                 AND ISO_STATUS = 'PEND'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4230-MARK-SENT'    TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Insured identity: name from the policy snapshot, DOB and
      * postcode from the GenApp customer record.
       4300-READ-INSURED.
           MOVE 0                      TO PSN-INSURED-NAME-LEN
           EXEC SQL
              SELECT INSURED_NAME
                INTO :PSN-INSURED-NAME
                FROM CLMDB.CLM_POLICY_SNAPSHOT
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4300-READ-INSURED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE SPACES                 TO GCU-DATEOFBIRTH GCU-POSTCODE
           MOVE CLM-GENAPP-CUST-NUM    TO GCU-CUSTOMERNUMBER
           EXEC SQL
              SELECT DATEOFBIRTH, POSTCODE
                INTO :GCU-DATEOFBIRTH :GCU-DATEOFBIRTH-IND,
                     :GCU-POSTCODE :GCU-POSTCODE-IND
                FROM GENAPP.CUSTOMER
               WHERE CUSTOMERNUMBER = :GCU-CUSTOMERNUMBER
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF GCU-DATEOFBIRTH-IND < 0
                    MOVE SPACES        TO GCU-DATEOFBIRTH
                 END-IF
                 IF GCU-POSTCODE-IND < 0
                    MOVE SPACES        TO GCU-POSTCODE
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4300-READ-INSURED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4320-READ-VEHICLE-REG.
           MOVE SPACES                 TO RSK-VEHICLE-REG
           EXEC SQL
              SELECT VEHICLE_REG
                INTO :RSK-VEHICLE-REG
                FROM CLMDB.CLM_RISK_UNIT
               WHERE CLAIM_ID    = :CLM-CLAIM-ID
                 AND VEHICLE_REG <> ' '
               ORDER BY RISK_UNIT_SEQ
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4320-READ-VEHICLE-REG' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-ISO-004 Party list capped at WS-MAX-PARTIES
       4400-LOAD-PARTIES.
           MOVE 0                      TO WS-PTY-COUNT
           MOVE 'N'                    TO WS-PTY-EOF-SW
           EXEC SQL OPEN PTYCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-LOAD-PARTIES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM UNTIL WS-PTY-EOF OR WS-PTY-COUNT >= WS-MAX-PARTIES
              EXEC SQL
                 FETCH PTYCSR
                  INTO :CCR-CONTACT-ROLE, :CON-CONTACT-TYPE,
                       :CON-FIRST-NAME :CON-FIRST-NAME-IND,
                       :CON-LAST-NAME :CON-LAST-NAME-IND,
                       :CON-COMPANY-NAME :CON-COMPANY-NAME-IND,
                       :CON-DATE-OF-BIRTH :CON-DATE-OF-BIRTH-IND,
                       :CON-POSTCODE :CON-POSTCODE-IND
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    PERFORM 4410-STAGE-PARTY
                 WHEN 100
                    SET WS-PTY-EOF     TO TRUE
                 WHEN OTHER
                    EXEC SQL CLOSE PTYCSR END-EXEC
                    MOVE '4400-LOAD-PARTIES' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE PTYCSR END-EXEC.

      ******************************************************************
       4410-STAGE-PARTY.
           ADD 1                       TO WS-PTY-COUNT
           MOVE WS-PTY-COUNT           TO WS-PX
           INITIALIZE WS-PARTY(WS-PX)
           MOVE CCR-CONTACT-ROLE       TO WS-P-ROLE(WS-PX)
           MOVE CON-CONTACT-TYPE       TO WS-P-TYPE(WS-PX)
           IF CON-FIRST-NAME-IND >= 0 AND CON-FIRST-NAME-LEN > 0
              MOVE CON-FIRST-NAME-TEXT(1:CON-FIRST-NAME-LEN)
                                       TO WS-P-FIRST(WS-PX)
           END-IF
           IF CON-LAST-NAME-IND >= 0 AND CON-LAST-NAME-LEN > 0
              MOVE CON-LAST-NAME-TEXT(1:CON-LAST-NAME-LEN)
                                       TO WS-P-LAST(WS-PX)
           END-IF
           IF CON-COMPANY-NAME-IND >= 0 AND CON-COMPANY-NAME-LEN > 0
              MOVE CON-COMPANY-NAME-TEXT(1:CON-COMPANY-NAME-LEN)
                                       TO WS-P-COMPANY(WS-PX)
           END-IF
           IF CON-DATE-OF-BIRTH-IND >= 0
              MOVE CON-DATE-OF-BIRTH   TO WS-P-DOB(WS-PX)
           END-IF
           IF CON-POSTCODE-IND >= 0
              MOVE CON-POSTCODE        TO WS-P-POSTCODE(WS-PX)
           END-IF.

      ******************************************************************
       4500-READ-ISO-REQUEST.
           EXEC SQL
              SELECT I.CLAIM_ID, I.REQUEST_TYPE, I.ISO_STATUS,
                     C.CLAIM_NUMBER
                INTO :ISO-CLAIM-ID, :ISO-REQUEST-TYPE,
                     :ISO-ISO-STATUS, :CLM-CLAIM-NUMBER
                FROM CLMDB.CLM_ISO_REQUEST I
                JOIN CLMDB.CLM_CLAIM C
                  ON C.CLAIM_ID = I.CLAIM_ID
               WHERE I.ISO_REQ_ID = :ISO-ISO-REQ-ID
           END-EXEC
           MOVE ISO-ISO-REQ-ID         TO ISP-D-REQ-ID
           EVALUATE SQLCODE
              WHEN 0
                 MOVE ISO-CLAIM-ID     TO CLM-CLAIM-ID
                 MOVE ISO-REQUEST-TYPE TO ISP-D-TYPE
              WHEN 100
                 MOVE 'ISONOTFD'       TO WS-REASON
                 MOVE 'No ISO request with this id' TO WS-DETAIL
              WHEN OTHER
                 MOVE '4500-READ-ISO-REQUEST' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4510-POST-MATCHES.
           MOVE ISR-D-MATCH-NUM        TO ISO-MATCH-COUNT
           MOVE ISR-D-ISO-REF          TO ISO-ISO-REF
           EXEC SQL
              UPDATE CLMDB.CLM_ISO_REQUEST
                 SET ISO_STATUS  = 'RCVD',
                     MATCH_COUNT = :ISO-MATCH-COUNT,
                     ISO_REF     = :ISO-ISO-REF,
                     RESPONSE_TS = CURRENT TIMESTAMP
               WHERE ISO_REQ_ID  = :ISO-ISO-REQ-ID
                 AND ISO_STATUS  = 'SENT'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4510-POST-MATCHES' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4520-RESET-FOR-RESEND.
           EXEC SQL
              UPDATE CLMDB.CLM_ISO_REQUEST
                 SET ISO_STATUS  = 'PEND',
                     SENT_TS     = NULL,
                     RESPONSE_TS = CURRENT TIMESTAMP
               WHERE ISO_REQ_ID  = :ISO-ISO-REQ-ID
                 AND ISO_STATUS  = 'SENT'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4520-RESET-FOR-RESEND' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       5100-READ-RESPONSE.
           READ ISO-IN-FILE
              AT END
                 SET WS-EOF            TO TRUE
              NOT AT END
                 ADD 1                 TO WS-RECNO
           END-READ
           IF WS-IN-STATUS NOT = '00' AND WS-IN-STATUS NOT = '10'
              MOVE 'ISOIN read error'  TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF.

      ******************************************************************
       6000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO ISP-H1-PAGE
           WRITE RPT-RECORD FROM ISP-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM ISP-HEAD-2 AFTER ADVANCING 1
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
           MOVE ISP-MSG-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE SPACES                 TO ISP-MSG-TEXT.

      ******************************************************************
       6200-WRITE-DETAIL.
           MOVE WS-REASON              TO ISP-D-REASON
           MOVE WS-DETAIL              TO ISP-D-DETAIL
           MOVE ISP-DET-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO ISP-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE '*** CONTROL TOTALS'   TO ISP-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           IF WS-MODE-SEND
              MOVE 'NEW CLAIMS QUEUED (INIT)'      TO ISP-T-LABEL
              MOVE WS-QUEUED-INIT      TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'CLAIMS RE-QUEUED FOR CHANGED PARTIES (UPDT)'
                                       TO ISP-T-LABEL
              MOVE WS-QUEUED-UPDT      TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'REQUESTS SENT (R RECORDS / TRAILER COUNT)'
                                       TO ISP-T-LABEL
              MOVE WS-SENT             TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'PARTY RECORDS SENT' TO ISP-T-LABEL
              MOVE WS-PARTIES-SENT     TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'REQUESTS HELD INCOMPLETE (STAY PEND)'
                                       TO ISP-T-LABEL
              MOVE WS-HELD             TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'CLAIM-ID HASH TOTAL' TO ISP-T-LABEL
              MOVE WS-CLAIM-HASH       TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
           ELSE
              MOVE 'RESPONSE DETAIL RECORDS (BALANCED TO TRAILER)'
                                       TO ISP-T-LABEL
              MOVE WS-BAL-DETAILS      TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'RESPONSES PROCESSED THIS RUN' TO ISP-T-LABEL
              MOVE WS-RESP-READ        TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'POSTED RCVD'       TO ISP-T-LABEL
              MOVE WS-RESP-POSTED      TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE '  OF WHICH BELOW SIU MATCH THRESHOLD'
                                       TO ISP-T-LABEL
              MOVE WS-RESP-NOMATCH     TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE '  OF WHICH REFERRED TO SIU SCORING'
                                       TO ISP-T-LABEL
              MOVE WS-RESP-REFERRED    TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'BUREAU REJECTS RESET FOR RESUBMISSION'
                                       TO ISP-T-LABEL
              MOVE WS-RESP-RESUBMIT    TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'DUPLICATE RESPONSES IGNORED' TO ISP-T-LABEL
              MOVE WS-RESP-DUP         TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
              MOVE 'RESPONSES REJECTED' TO ISP-T-LABEL
              MOVE WS-RESP-REJECTED    TO ISP-T-COUNT
              PERFORM 6550-WRITE-TOTAL
           END-IF
           MOVE 'RETURN CODE'          TO ISP-T-LABEL
           MOVE WS-MAX-RC              TO ISP-T-COUNT
           PERFORM 6550-WRITE-TOTAL.

      ******************************************************************
       6550-WRITE-TOTAL.
           MOVE ISP-TOT-LINE           TO RPT-RECORD
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
           MOVE WS-PROGRAM-ID          TO ISC-PROGRAM
           MOVE WS-RUN-MODE            TO ISC-MODE
           IF NOT ISC-RUN-COMPLETE
              MOVE 'R'                 TO ISC-RUN-STATUS
           END-IF
           MOVE WS-BUS-DATE            TO ISC-BUS-DATE
           MOVE WS-SENT                TO ISC-REQUESTS
           MOVE WS-PARTIES-SENT        TO ISC-PARTIES
           MOVE WS-CLAIM-HASH          TO ISC-CLAIM-HASH
           MOVE FUNCTION CURRENT-DATE  TO ISC-CHECKPOINT-TS
           CLOSE CHK-FILE
           OPEN OUTPUT CHK-FILE
           WRITE ISC-CHECKPOINT-REC
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'CLMISOSN CHECKPOINT WRITE FAILED ' WS-CHK-STATUS
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
           MOVE 'ISOMATCH'             TO HIA-HI-EVENT-TYPE
           MOVE WS-DETAIL              TO HIA-HI-DESCRIPTION
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF HIA-RETURN-CODE > 4
              MOVE HIA-MESSAGE         TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF.

      ******************************************************************
       8300-NEXT-ISO-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO SQA-USER-ID SQA-CALLER-PGM
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE 'ISO     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE > 4
              MOVE SQA-MESSAGE         TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF.

      ******************************************************************
       8400-SCORE-SIU.
           INITIALIZE WS-SIU-CA
           MOVE 'SIUSCR  '             TO SIA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO SIA-USER-ID SIA-CALLER-PGM
           MOVE WS-BUS-DATE            TO SIA-BUS-DATE
           MOVE CLM-CLAIM-ID           TO SIA-SI-CLAIM-ID
           CALL WS-SIU-PGM USING WS-SIU-CA
           IF SIA-RETURN-CODE > 4
              MOVE SIA-MESSAGE         TO ISP-MSG-TEXT
              PERFORM 6900-ABORT-RUN
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
           MOVE ISO-ISO-REQ-ID         TO WS-KEY-DISP
           MOVE WS-KEY-DISP            TO WS-ERR-KEY
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'CLMISOSN Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           EXEC SQL ROLLBACK END-EXEC
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO ISP-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 12                     TO WS-MAX-RC
           PERFORM 9000-RETURN.
