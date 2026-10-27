       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMRIRE0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMRIRE0                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Reinsurance cession. For every claim whose       *
      *               incurred moved today (a financial transaction    *
      *               on the business date) the treaties in force for  *
      *               its policy type at the loss date are applied:    *
      *               quota share cedes a percentage, excess of loss   *
      *               cedes the layer above retention up to the limit. *
      *               CLM_RI_CESSION is upserted, first large-loss     *
      *               crossings are flagged for notice and every       *
      *               movement goes to the bordereau file.             *
      * FILES       : BORDX   - cession bordereau (CLMRIRBD, 200)      *
      *               CHKPT   - checkpoint / restart (CLMRIRCK, 80)    *
      *               SYSIN   - control card (CLMRIRCC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : after RK-LAST-CLAIM-ID; bordereau lines are      *
      *               buffered and written after each commit           *
      * CALLS       : CLMMONEY MNRATE  CLMHIST0 HISADD                 *
      *               CLMERRLG ERRLOG                                  *
      * TABLES      : CLM_CLAIM (R)  CLM_TRANSACTION (R)               *
      *               CLM_RI_TREATY (R)  CLM_RI_CESSION (C R U)        *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-RIC-001 A claim is re-ceded when it has a *
      *                              financial transaction dated the   *
      *                              business date                     *
      *               BR-CLM-RIC-002 Treaties attach by policy type    *
      *                              with the loss date inside the     *
      *                              treaty period                     *
      *               BR-CLM-RIC-003 QS ceded = incurred x CEDED_PCT,  *
      *                              nil when incurred is not positive,*
      *                              capped at the treaty limit        *
      *               BR-CLM-RIC-004 XOL ceded = incurred - retention, *
      *                              floor nil, capped at the limit    *
      *               BR-CLM-RIC-005 No movement (gross and ceded      *
      *                              unchanged) writes no bordereau    *
      *               BR-CLM-RIC-006 First time ceded reaches 50,000   *
      *                              the cession is flagged notified   *
      *                              and a large-loss notice raised    *
      *               BR-CLM-RIC-007 An XOL cession row is created     *
      *                              only once the layer is pierced    *
      *               BR-CLM-RIC-008 Unknown treaty type or XOL with   *
      *                              no limit is skipped and reported  *
      * RETURN CODE : 0 ok, 4 treaties skipped, 16 Db2 or service      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BORDX-OUT     ASSIGN TO BORDX
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-BDX.
           SELECT CHKPT-FILE    ASSIGN TO CHKPT
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-CHK.
           SELECT CTL-CARD      ASSIGN TO SYSIN
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-CTL.
           SELECT RPT-OUT       ASSIGN TO RPTOUT
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-RPT.
       DATA DIVISION.
       FILE SECTION.
       FD  BORDX-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
           COPY CLMRIRBD.
       FD  CHKPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMRIRCK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMRIRCC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMRIRE0'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-MNY-PGM                  PIC X(8)  VALUE 'CLMMONEY'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-LARGE-LOSS-LIMIT         PIC S9(11)V99 COMP-3
                                                 VALUE 50000.00.

       01  WS-FILE-STATUS.
           05 WS-FS-BDX                PIC X(2).
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-CLM-EOF-SW            PIC X     VALUE 'N'.
              88 WS-CLM-EOF             VALUE 'Y'.
           05 WS-TRT-EOF-SW            PIC X     VALUE 'N'.
              88 WS-TRT-EOF             VALUE 'Y'.
           05 WS-CESSION-SW            PIC X     VALUE 'N'.
              88 WS-CESSION-EXISTS      VALUE 'Y'.
           05 WS-TREATY-SW             PIC X     VALUE 'Y'.
              88 WS-TREATY-VALID        VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-COMMIT-FREQ           PIC S9(5) COMP-3 VALUE 100.
           05 WS-RESTART-ID            PIC S9(9) COMP   VALUE 0.
           05 WS-LAST-CLAIM-ID         PIC S9(9) COMP   VALUE 0.
           05 WS-UNCOMMITTED           PIC S9(5) COMP-3 VALUE 0.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-CLAIMS-READ           PIC S9(9) COMP-3 VALUE 0.
           05 WS-CLAIMS-NO-TREATY      PIC S9(9) COMP-3 VALUE 0.
           05 WS-CESSIONS-CALC         PIC S9(9) COMP-3 VALUE 0.
           05 WS-CESSIONS-INSERTED     PIC S9(9) COMP-3 VALUE 0.
           05 WS-CESSIONS-UPDATED      PIC S9(9) COMP-3 VALUE 0.
           05 WS-CESSIONS-UNCHANGED    PIC S9(9) COMP-3 VALUE 0.
           05 WS-TREATIES-SKIPPED      PIC S9(9) COMP-3 VALUE 0.
           05 WS-NOTICES               PIC S9(9) COMP-3 VALUE 0.
           05 WS-BDX-COUNT             PIC S9(9) COMP-3 VALUE 0.
           05 WS-BDX-CEDED             PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-BDX-MOVEMENT          PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-CALC-WORK.
           05 WS-TREATIES-FOR-CLAIM    PIC S9(4) COMP.
           05 WS-INCURRED              PIC S9(11)V99 COMP-3.
           05 WS-CEDED                 PIC S9(11)V99 COMP-3.
           05 WS-RETAINED              PIC S9(11)V99 COMP-3.
           05 WS-PRIOR-CEDED           PIC S9(11)V99 COMP-3.
           05 WS-PRIOR-GROSS           PIC S9(11)V99 COMP-3.
           05 WS-PRIOR-NOTIFIED        PIC X.
           05 WS-MOVEMENT              PIC S9(11)V99 COMP-3.
           05 WS-NOTICE-SW             PIC X.
           05 WS-NOTE                  PIC X(29).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).
           05 WS-DISP-AMOUNT           PIC Z,ZZZ,ZZZ,ZZ9.99.

       01  WS-BDX-BUFFER.
           05 WS-BDX-BUF-COUNT         PIC S9(5) COMP   VALUE 0.
           05 WS-BDX-BUF-MAX           PIC S9(5) COMP   VALUE 990.
           05 WS-BDX-BUF-IX            PIC S9(5) COMP   VALUE 0.
           05 WS-BDX-BUF-REC           PIC X(200) OCCURS 1000 TIMES.

           COPY CLMRIRBD REPLACING LEADING ==BD-== BY ==WB-==.

           COPY CLMRIRCK REPLACING LEADING ==RK-== BY ==WK-==.

           COPY CLMRIRCC REPLACING LEADING ==RC-== BY ==WC-==.

           COPY CLMRIRRP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDRIT END-EXEC.
           EXEC SQL INCLUDE CLMDRIC END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CLM-CSR CURSOR WITH HOLD FOR
              SELECT C.CLAIM_ID, C.CLAIM_NUMBER, C.POLICY_TYPE,
                     C.LOSS_DATE, C.TOTAL_INCURRED, C.CAT_CODE
                FROM CLMDB.CLM_CLAIM C
               WHERE C.CLAIM_ID > :WS-RESTART-ID
                 AND EXISTS
                     (SELECT 1
                        FROM CLMDB.CLM_TRANSACTION T
                       WHERE T.CLAIM_ID      = C.CLAIM_ID
                         AND T.BUSINESS_DATE = :WS-BUS-DATE)
               ORDER BY C.CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE TRT-CSR CURSOR FOR
              SELECT TREATY_CODE, TREATY_TYPE, RETENTION_AMOUNT,
                     LIMIT_AMOUNT, CEDED_PCT
                FROM CLMDB.CLM_RI_TREATY
               WHERE POLICY_TYPE = :CLM-POLICY-TYPE
                 AND EFF_DATE   <= :CLM-LOSS-DATE
                 AND EXP_DATE   >= :CLM-LOSS-DATE
               ORDER BY TREATY_CODE
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.
       01  WS-MNY-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==MNA-==.
           03 MNA-BODY.
              COPY CLMCMNY REPLACING LEADING ==CA-== BY ==MNA-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PROCESS-CLAIMS
           PERFORM 2900-FINISH
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMRIRE0 RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO RR-M-TEXT WS-ABEND-TEXT
           OPEN INPUT CTL-CARD
           MOVE SPACES                 TO WC-CARD
           IF WS-FS-CTL = '00'
              READ CTL-CARD INTO WC-CARD
                 AT END MOVE SPACES    TO WC-CARD
              END-READ
              CLOSE CTL-CARD
           END-IF
           IF WC-COMMIT-FREQ-X IS NUMERIC AND WC-COMMIT-FREQ > 0
              MOVE WC-COMMIT-FREQ      TO WS-COMMIT-FREQ
           END-IF
           PERFORM 4000-GET-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO RR-H1-BUS-DATE
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
              OPEN OUTPUT BORDX-OUT
           ELSE
              OPEN INPUT CHKPT-FILE
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE RK-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              IF NOT WK-IN-FLIGHT
                 MOVE 'Restart requested but no in-flight checkpoint'
                                       TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
              MOVE WK-LAST-CLAIM-ID    TO WS-RESTART-ID
              MOVE WK-BORDEREAU-COUNT  TO WS-BDX-COUNT
              MOVE WK-CEDED-TOTAL      TO WS-BDX-CEDED
              OPEN EXTEND CHKPT-FILE
              OPEN EXTEND BORDX-OUT
              MOVE WK-LAST-CLAIM-ID    TO WS-DISP-ID
              STRING 'Restart after claim id ' WS-DISP-ID
                     DELIMITED BY SIZE INTO RR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF
           IF NOT WS-FS-CHK-OK OR WS-FS-BDX NOT = '00'
              MOVE 'CHKPT or BORDX open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-RIC-001 Claims with a transaction on the business date
       2000-PROCESS-CLAIMS.
           EXEC SQL OPEN CLM-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-CLAIMS' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-CLAIM
           PERFORM UNTIL WS-CLM-EOF
              ADD 1                    TO WS-CLAIMS-READ
              PERFORM 2100-CEDE-CLAIM
              MOVE CLM-CLAIM-ID        TO WS-LAST-CLAIM-ID
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 OR WS-BDX-BUF-COUNT >= WS-BDX-BUF-MAX
                 SET WK-IN-FLIGHT      TO TRUE
                 PERFORM 4900-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4100-FETCH-CLAIM
           END-PERFORM
           EXEC SQL CLOSE CLM-CSR END-EXEC.

      ******************************************************************
      * BR-CLM-RIC-002 Treaties for the policy type at the loss date
       2100-CEDE-CLAIM.
           MOVE 0                      TO WS-TREATIES-FOR-CLAIM
           MOVE CLM-TOTAL-INCURRED     TO WS-INCURRED
           EXEC SQL OPEN TRT-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-CEDE-CLAIM'   TO WS-ERR-PARAGRAPH
              MOVE CLM-CLAIM-NUMBER    TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-TRT-EOF-SW
           PERFORM 4200-FETCH-TREATY
           PERFORM UNTIL WS-TRT-EOF
              ADD 1                    TO WS-TREATIES-FOR-CLAIM
              PERFORM 2200-APPLY-TREATY
              PERFORM 4200-FETCH-TREATY
           END-PERFORM
           EXEC SQL CLOSE TRT-CSR END-EXEC
           IF WS-TREATIES-FOR-CLAIM = 0
              ADD 1                    TO WS-CLAIMS-NO-TREATY
           END-IF.

      ******************************************************************
       2200-APPLY-TREATY.
           MOVE SPACES                 TO WS-NOTE
           MOVE 'N'                    TO WS-NOTICE-SW
           PERFORM 5000-CALC-CEDED
           IF NOT WS-TREATY-VALID
              ADD 1                    TO WS-TREATIES-SKIPPED
              MOVE 0                   TO WS-CEDED WS-PRIOR-CEDED
                                          WS-MOVEMENT
              PERFORM 6400-PRINT-DETAIL
           ELSE
              ADD 1                    TO WS-CESSIONS-CALC
              COMPUTE WS-RETAINED = WS-INCURRED - WS-CEDED
              PERFORM 4300-READ-CESSION
              COMPUTE WS-MOVEMENT = WS-CEDED - WS-PRIOR-CEDED
      * BR-CLM-RIC-005 No movement, no bordereau line
              EVALUATE TRUE
                 WHEN WS-CESSION-EXISTS
                      AND WS-PRIOR-GROSS = WS-INCURRED
                      AND WS-MOVEMENT = 0
                    ADD 1              TO WS-CESSIONS-UNCHANGED
      * BR-CLM-RIC-007 XOL row only once the layer is pierced
                 WHEN NOT WS-CESSION-EXISTS AND WS-CEDED = 0
                    ADD 1              TO WS-CESSIONS-UNCHANGED
                 WHEN OTHER
                    PERFORM 2300-RECORD-CESSION
              END-EVALUATE
           END-IF.

      ******************************************************************
      * BR-CLM-RIC-006 First crossing of the large-loss threshold
       2300-RECORD-CESSION.
           IF WS-CEDED >= WS-LARGE-LOSS-LIMIT
              AND WS-PRIOR-NOTIFIED NOT = 'Y'
              MOVE 'Y'                 TO WS-NOTICE-SW
              MOVE 'Y'                 TO RIC-NOTIFIED-FLAG
              ADD 1                    TO WS-NOTICES
              MOVE 'LARGE LOSS NOTICE' TO WS-NOTE
           ELSE
              MOVE WS-PRIOR-NOTIFIED   TO RIC-NOTIFIED-FLAG
           END-IF
           MOVE CLM-CLAIM-ID           TO RIC-CLAIM-ID
           MOVE RIT-TREATY-CODE        TO RIC-TREATY-CODE
           MOVE WS-INCURRED            TO RIC-GROSS-INCURRED
           MOVE WS-RETAINED            TO RIC-RETAINED-AMOUNT
           MOVE WS-CEDED               TO RIC-CEDED-AMOUNT
           MOVE WS-BUS-DATE            TO RIC-CALC-DATE
           IF WS-CESSION-EXISTS
              PERFORM 4500-UPDATE-CESSION
           ELSE
              PERFORM 4400-INSERT-CESSION
           END-IF
           IF WS-NOTICE-SW = 'Y'
              PERFORM 8200-WRITE-HISTORY
           END-IF
           PERFORM 6300-BUFFER-BORDEREAU
           PERFORM 6400-PRINT-DETAIL.

      ******************************************************************
       2900-FINISH.
           SET WK-ENDED                TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           MOVE SPACES                 TO WB-RECORD
           SET WB-IS-TRAILER           TO TRUE
           MOVE WS-BUS-DATE            TO WB-BUS-DATE
           MOVE WS-BDX-COUNT           TO WB-T-RECORD-COUNT
           MOVE WS-BDX-CEDED           TO WB-T-CEDED-TOTAL
           MOVE WS-BDX-MOVEMENT        TO WB-T-MOVEMENT-TOTAL
           IF WS-BDX-MOVEMENT < 0
              MOVE '-'                 TO WB-T-MOVEMENT-SIGN
           ELSE
              MOVE '+'                 TO WB-T-MOVEMENT-SIGN
           END-IF
           WRITE BD-RECORD FROM WB-RECORD
           IF WS-FS-BDX NOT = '00'
              MOVE 'BORDX trailer write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           IF WS-TREATIES-SKIPPED > 0 AND WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF
           PERFORM 6500-PRINT-TOTALS.

      ******************************************************************
       4000-GET-BUSINESS-DATE.
           IF WC-BUS-DATE NOT = SPACES
              MOVE WC-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4000-GET-BUSINESS-DATE' TO WS-ERR-PARAGRAPH
                 MOVE CTL-CONTROL-KEY  TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
       4100-FETCH-CLAIM.
           EXEC SQL
              FETCH CLM-CSR
               INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                    :CLM-POLICY-TYPE, :CLM-LOSS-DATE,
                    :CLM-TOTAL-INCURRED, :CLM-CAT-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-CLM-EOF        TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-CLAIM' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-FETCH-TREATY.
           EXEC SQL
              FETCH TRT-CSR
               INTO :RIT-TREATY-CODE, :RIT-TREATY-TYPE,
                    :RIT-RETENTION-AMOUNT, :RIT-LIMIT-AMOUNT,
                    :RIT-CEDED-PCT
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-TRT-EOF        TO TRUE
              WHEN OTHER
                 MOVE '4200-FETCH-TREATY' TO WS-ERR-PARAGRAPH
                 MOVE CLM-CLAIM-NUMBER TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-READ-CESSION.
           MOVE CLM-CLAIM-ID           TO RIC-CLAIM-ID
           MOVE RIT-TREATY-CODE        TO RIC-TREATY-CODE
           EXEC SQL
              SELECT GROSS_INCURRED, CEDED_AMOUNT, NOTIFIED_FLAG
                INTO :RIC-GROSS-INCURRED, :RIC-CEDED-AMOUNT,
                     :RIC-NOTIFIED-FLAG
                FROM CLMDB.CLM_RI_CESSION
               WHERE CLAIM_ID    = :RIC-CLAIM-ID
                 AND TREATY_CODE = :RIC-TREATY-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE 'Y'              TO WS-CESSION-SW
                 MOVE RIC-GROSS-INCURRED TO WS-PRIOR-GROSS
                 MOVE RIC-CEDED-AMOUNT TO WS-PRIOR-CEDED
                 MOVE RIC-NOTIFIED-FLAG TO WS-PRIOR-NOTIFIED
              WHEN 100
                 MOVE 'N'              TO WS-CESSION-SW
                 MOVE 0                TO WS-PRIOR-GROSS
                                          WS-PRIOR-CEDED
                 MOVE 'N'              TO WS-PRIOR-NOTIFIED
              WHEN OTHER
                 MOVE '4300-READ-CESSION' TO WS-ERR-PARAGRAPH
                 MOVE CLM-CLAIM-NUMBER TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-INSERT-CESSION.
           EXEC SQL
              INSERT INTO CLMDB.CLM_RI_CESSION
                    (CLAIM_ID, TREATY_CODE, GROSS_INCURRED,
                     RETAINED_AMOUNT, CEDED_AMOUNT, NOTIFIED_FLAG,
                     CALC_DATE)
              VALUES (:RIC-CLAIM-ID, :RIC-TREATY-CODE,
                      :RIC-GROSS-INCURRED, :RIC-RETAINED-AMOUNT,
                      :RIC-CEDED-AMOUNT, :RIC-NOTIFIED-FLAG,
                      :RIC-CALC-DATE)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 ADD 1                 TO WS-CESSIONS-INSERTED
              WHEN -803
                 PERFORM 4500-UPDATE-CESSION
              WHEN OTHER
                 MOVE '4400-INSERT-CESSION' TO WS-ERR-PARAGRAPH
                 MOVE CLM-CLAIM-NUMBER TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-UPDATE-CESSION.
           EXEC SQL
              UPDATE CLMDB.CLM_RI_CESSION
                 SET GROSS_INCURRED  = :RIC-GROSS-INCURRED,
                     RETAINED_AMOUNT = :RIC-RETAINED-AMOUNT,
                     CEDED_AMOUNT    = :RIC-CEDED-AMOUNT,
                     NOTIFIED_FLAG   = :RIC-NOTIFIED-FLAG,
                     CALC_DATE       = :RIC-CALC-DATE
               WHERE CLAIM_ID    = :RIC-CLAIM-ID
                 AND TREATY_CODE = :RIC-TREATY-CODE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-UPDATE-CESSION' TO WS-ERR-PARAGRAPH
              MOVE CLM-CLAIM-NUMBER    TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-CESSIONS-UPDATED.

      ******************************************************************
      * Commit, write the buffered bordereau lines, then checkpoint.
       4900-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-COMMIT-CHECKPOINT' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM VARYING WS-BDX-BUF-IX FROM 1 BY 1
                   UNTIL WS-BDX-BUF-IX > WS-BDX-BUF-COUNT
              WRITE BD-RECORD FROM WS-BDX-BUF-REC(WS-BDX-BUF-IX)
              IF WS-FS-BDX NOT = '00'
                 MOVE 'BORDX write failed' TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
           END-PERFORM
           MOVE 0                      TO WS-BDX-BUF-COUNT
                                          WS-UNCOMMITTED
           MOVE WS-PROGRAM-ID          TO WK-PROGRAM
           MOVE WS-BUS-DATE            TO WK-BUS-DATE
           MOVE WS-LAST-CLAIM-ID       TO WK-LAST-CLAIM-ID
           MOVE WS-BDX-COUNT           TO WK-BORDEREAU-COUNT
           MOVE WS-BDX-CEDED           TO WK-CEDED-TOTAL
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE RK-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       5000-CALC-CEDED.
           MOVE 'Y'                    TO WS-TREATY-SW
           MOVE 0                      TO WS-CEDED
           EVALUATE RIT-TREATY-TYPE
      * BR-CLM-RIC-003 Quota share
              WHEN 'QS  '
                 IF WS-INCURRED > 0
                    PERFORM 8100-CALL-MONEY-RATE
                 END-IF
                 IF RIT-LIMIT-AMOUNT > 0
                    AND WS-CEDED > RIT-LIMIT-AMOUNT
                    MOVE RIT-LIMIT-AMOUNT TO WS-CEDED
                    MOVE 'QS CESSION CAPPED AT LIMIT' TO WS-NOTE
                 END-IF
      * BR-CLM-RIC-004 Excess of loss layer
              WHEN 'XOL '
                 IF RIT-LIMIT-AMOUNT NOT > 0
                    MOVE 'N'           TO WS-TREATY-SW
                    MOVE 'XOL TREATY HAS NO LIMIT' TO WS-NOTE
                 ELSE
                    IF WS-INCURRED > RIT-RETENTION-AMOUNT
                       COMPUTE WS-CEDED =
                               WS-INCURRED - RIT-RETENTION-AMOUNT
                    END-IF
                    IF WS-CEDED > RIT-LIMIT-AMOUNT
                       MOVE RIT-LIMIT-AMOUNT TO WS-CEDED
                    END-IF
                 END-IF
      * BR-CLM-RIC-008 Unknown treaty type
              WHEN OTHER
                 MOVE 'N'              TO WS-TREATY-SW
                 MOVE 'UNKNOWN TREATY TYPE - SKIPPED' TO WS-NOTE
           END-EVALUATE.

      ******************************************************************
       8100-CALL-MONEY-RATE.
           INITIALIZE MNA-BODY
           MOVE STH-HEADER             TO MNA-HEADER
           MOVE 'MNRATE  '             TO MNA-REQUEST-ID
           MOVE WS-INCURRED            TO MNA-MN-TOTAL
           COMPUTE MNA-MN-RATE-BPS = RIT-CEDED-PCT * 100
           CALL WS-MNY-PGM USING WS-MNY-CA
           IF NOT MNA-RC-OK
              STRING 'CLMMONEY MNRATE failed ' MNA-REASON-CODE ' '
                     MNA-MESSAGE DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE MNA-MN-RESULT          TO WS-CEDED.

      ******************************************************************
       8200-WRITE-HISTORY.
           INITIALIZE HIA-BODY
           MOVE STH-HEADER             TO HIA-HEADER
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CLM-CLAIM-ID           TO HIA-HI-CLAIM-ID
           MOVE 'RINOTIFY'             TO HIA-HI-EVENT-TYPE
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           MOVE WS-CEDED               TO WS-DISP-AMOUNT
           STRING 'Large loss notice to reinsurer: treaty '
                  RIT-TREATY-CODE ' ceded GBP ' WS-DISP-AMOUNT
                  DELIMITED BY SIZE INTO HIA-HI-DESCRIPTION
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF NOT HIA-RC-OK
              STRING 'CLMHIST0 HISADD failed ' HIA-REASON-CODE ' '
                     HIA-MESSAGE DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       6100-PRINT-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              MOVE RPT-RECORD          TO WS-HOLD-LINE
              ADD 1                    TO WS-PAGE-NO
              MOVE WS-PAGE-NO          TO RR-H1-PAGE
              WRITE RPT-RECORD FROM RR-HEAD-1 AFTER ADVANCING PAGE
              WRITE RPT-RECORD FROM RR-HEAD-2 AFTER ADVANCING 2 LINES
              MOVE SPACES              TO RPT-RECORD
              WRITE RPT-RECORD AFTER ADVANCING 1 LINE
              MOVE 4                   TO WS-LINE-COUNT
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6200-PRINT-MESSAGE.
           MOVE RR-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                 TO RR-M-TEXT.

      ******************************************************************
       6300-BUFFER-BORDEREAU.
           MOVE SPACES                 TO WB-RECORD
           SET WB-IS-DETAIL            TO TRUE
           MOVE WS-BUS-DATE            TO WB-BUS-DATE
           MOVE RIT-TREATY-CODE        TO WB-TREATY-CODE
           MOVE RIT-TREATY-TYPE        TO WB-TREATY-TYPE
           MOVE CLM-CLAIM-NUMBER       TO WB-CLAIM-NUMBER
           MOVE CLM-POLICY-TYPE        TO WB-POLICY-TYPE
           MOVE CLM-LOSS-DATE          TO WB-LOSS-DATE
           MOVE CLM-CAT-CODE           TO WB-CAT-CODE
           MOVE WS-INCURRED            TO WB-GROSS-INCURRED
           MOVE WS-RETAINED            TO WB-RETAINED
           MOVE WS-CEDED               TO WB-CEDED
           MOVE WS-PRIOR-CEDED         TO WB-PRIOR-CEDED
           MOVE WS-MOVEMENT            TO WB-MOVEMENT
           MOVE '+'                    TO WB-GROSS-SIGN
                                          WB-RETAINED-SIGN
                                          WB-MOVEMENT-SIGN
           IF WS-INCURRED < 0
              MOVE '-'                 TO WB-GROSS-SIGN
           END-IF
           IF WS-RETAINED < 0
              MOVE '-'                 TO WB-RETAINED-SIGN
           END-IF
           IF WS-MOVEMENT < 0
              MOVE '-'                 TO WB-MOVEMENT-SIGN
           END-IF
           MOVE WS-NOTICE-SW           TO WB-LARGE-LOSS-NOTICE
           ADD 1                       TO WS-BDX-BUF-COUNT
           MOVE WB-RECORD          TO WS-BDX-BUF-REC(WS-BDX-BUF-COUNT)
           ADD 1                       TO WS-BDX-COUNT
           ADD WS-CEDED                TO WS-BDX-CEDED
           ADD WS-MOVEMENT             TO WS-BDX-MOVEMENT.

      ******************************************************************
       6400-PRINT-DETAIL.
           MOVE CLM-CLAIM-NUMBER       TO RR-D-CLAIM-NUMBER
           MOVE RIT-TREATY-CODE        TO RR-D-TREATY
           MOVE RIT-TREATY-TYPE        TO RR-D-TYPE
           MOVE CLM-POLICY-TYPE        TO RR-D-POLICY-TYPE
           MOVE CLM-LOSS-DATE          TO RR-D-LOSS-DATE
           MOVE WS-INCURRED            TO RR-D-GROSS
           MOVE WS-CEDED               TO RR-D-CEDED
           MOVE WS-PRIOR-CEDED         TO RR-D-PRIOR
           MOVE WS-MOVEMENT            TO RR-D-MOVEMENT
           MOVE WS-NOTE                TO RR-D-NOTE
           MOVE RR-DETAIL              TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO RR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           MOVE 0                      TO RR-T-AMOUNT
           MOVE 'Claims with incurred movement today' TO RR-T-LABEL
           MOVE WS-CLAIMS-READ         TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Claims with no treaty in force' TO RR-T-LABEL
           MOVE WS-CLAIMS-NO-TREATY    TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Cessions calculated'  TO RR-T-LABEL
           MOVE WS-CESSIONS-CALC       TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Cession rows inserted' TO RR-T-LABEL
           MOVE WS-CESSIONS-INSERTED   TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Cession rows updated' TO RR-T-LABEL
           MOVE WS-CESSIONS-UPDATED    TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'No movement / layer not pierced' TO RR-T-LABEL
           MOVE WS-CESSIONS-UNCHANGED  TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Treaties skipped (invalid definition)' TO RR-T-LABEL
           MOVE WS-TREATIES-SKIPPED    TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Large-loss notices raised' TO RR-T-LABEL
           MOVE WS-NOTICES             TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Bordereau lines (cumulative) / ceded' TO RR-T-LABEL
           MOVE WS-BDX-COUNT           TO RR-T-COUNT
           MOVE WS-BDX-CEDED           TO RR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Net ceded movement this run' TO RR-T-LABEL
           MOVE 0                      TO RR-T-COUNT
           MOVE WS-BDX-MOVEMENT        TO RR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 0                      TO RR-T-AMOUNT
           MOVE 'Return code'          TO RR-T-LABEL
           MOVE WS-RETURN-CODE         TO RR-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE RR-TOTAL-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       9000-TERMINATE.
           CLOSE BORDX-OUT CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
      * Abend: roll back; buffered bordereau lines are discarded.
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           MOVE 0                      TO WS-BDX-BUF-COUNT
           IF WS-RETURN-CODE < 16
              MOVE 16                  TO WS-RETURN-CODE
           END-IF
           STRING '*** RUN ABANDONED: ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO RR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           DISPLAY 'CLMRIRE0 ' WS-ABEND-TEXT
           PERFORM 6500-PRINT-TOTALS
           CLOSE BORDX-OUT CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           STOP RUN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           EXEC SQL ROLLBACK END-EXEC
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-BATCH-USER          TO ERC-USER-ID
           MOVE WS-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARAGRAPH       TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           STRING 'CLMRIRE0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
