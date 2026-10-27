       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMSEALR.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMSEALR                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Nightly audit chain seal. Calls CLMAUDCH AUCVFY  *
      *               from the last sealed row (FROM-SEQ 0) in slices  *
      *               until the chain head captured at start is        *
      *               proven; CLMAUDCH seals each proven row and this  *
      *               program commits per slice, confirms every row up *
      *               to the verified head is sealed, cross-checks the *
      *               head hash against the table and records the head *
      *               in the seal register. A broken link is printed   *
      *               with its sequence and ends the run RC 12.        *
      * FILES       : SEALREG - seal register (CLMSEARG, 120)          *
      *               CHKPT   - checkpoint trail (CLMSEACK, 80)        *
      *               SYSIN   - control card (CLMSEACC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : inherent - the last sealed row is the restart    *
      *               key; CHKPT is reconciled against it on restart   *
      * CALLS       : CLMAUDCH AUCVFY  CLMERRLG ERRLOG                 *
      * TABLES      : CLM_AUDIT_CHAIN (R U)  CLM_CONTROL (R)           *
      * RULES       : BR-CLM-SEA-001 Verification always resumes after *
      *                              the last sealed row; sealed rows  *
      *                              are never re-walked               *
      *               BR-CLM-SEA-002 The run seals up to the head      *
      *                              captured at start; rows appended  *
      *                              during the run wait for tomorrow  *
      *               BR-CLM-SEA-003 Each verified slice is committed  *
      *                              before the next is walked         *
      *               BR-CLM-SEA-004 Every row up to the verified head *
      *                              must end the run sealed           *
      *               BR-CLM-SEA-005 The verified head hash must equal *
      *                              the stored CHAIN_HASH of that row *
      *               BR-CLM-SEA-006 A broken link prints its sequence *
      *                              and fails the run (RC 12); rows   *
      *                              before it stay sealed             *
      * RETURN CODE : 0 sealed or nothing new, 4 slice limit reached,  *
      *               checkpoint/table mismatch or SEALREG DD missing, *
      *               12 chain broken or                               *
      *               head hash mismatch, 16 Db2 or service failure    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEALREG-OUT   ASSIGN TO SEALREG
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-REG.
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
       FD  SEALREG-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS.
           COPY CLMSEARG.
       FD  CHKPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMSEACK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMSEACC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMSEALR'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-AUC-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-FS-REG                PIC X(2).
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-WALK-SW               PIC X     VALUE 'C'.
              88 WS-WALK-CONTINUE       VALUE 'C'.
              88 WS-WALK-DONE           VALUE 'D'.
              88 WS-WALK-BROKEN         VALUE 'B'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-COMMIT-FREQ           PIC S9(9) COMP   VALUE 5000.
           05 WS-MAX-SLICES            PIC S9(4) COMP   VALUE 20.
           05 WS-SLICE                 PIC S9(4) COMP   VALUE 0.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CHAIN-POSITION.
           05 WS-START-SEALED          PIC S9(9) COMP   VALUE 0.
           05 WS-HEAD-AT-START         PIC S9(9) COMP   VALUE 0.
           05 WS-UNSEALED-AT-START     PIC S9(9) COMP   VALUE 0.
           05 WS-VERIFIED-SEQ          PIC S9(9) COMP   VALUE 0.
           05 WS-VERIFIED-HASH         PIC X(16) VALUE SPACES.
           05 WS-BROKEN-SEQ            PIC S9(9) COMP   VALUE 0.
           05 WS-ROWS-VERIFIED         PIC S9(9) COMP   VALUE 0.
           05 WS-LEFT-UNSEALED         PIC S9(9) COMP   VALUE 0.
           05 WS-COMPLETED-SEALS       PIC S9(9) COMP   VALUE 0.
           05 WS-SEAL-FROM             PIC S9(9) COMP   VALUE 0.
           05 WS-SEAL-TO               PIC S9(9) COMP   VALUE 0.
           05 WS-STORED-HASH           PIC X(16).
           05 WS-FINAL-REASON          PIC X(8)  VALUE SPACES.
           05 WS-CKPT-SEALED           PIC S9(9) COMP   VALUE 0.
           05 WS-REG-SW                PIC X     VALUE 'N'.
              88 WS-REG-OPEN            VALUE 'Y'.
           05 WS-CKPT-SW               PIC X     VALUE 'N'.
              88 WS-CKPT-IN-FLIGHT      VALUE 'Y'.

       01  WS-MISC.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).
           05 WS-DISP-ID2              PIC 9(9).

           COPY CLMSEARG REPLACING LEADING ==SG-== BY ==WG-==.

           COPY CLMSEACK REPLACING LEADING ==SK-== BY ==WK-==.

           COPY CLMSEACC REPLACING LEADING ==SC-== BY ==WC-==.

           COPY CLMSEARP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDAUC END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.
       01  WS-AUC-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==ACA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 4100-READ-CHAIN-POSITION
           MOVE WS-START-SEALED        TO WS-VERIFIED-SEQ
           IF WS-START-SEALED > 0
              PERFORM 4300-READ-STORED-HASH
              MOVE WS-STORED-HASH      TO WS-VERIFIED-HASH
           END-IF
           IF WS-HEAD-AT-START = 0
              SET WG-EMPTY             TO TRUE
              MOVE 'Audit chain is empty - nothing to seal'
                                       TO SR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           ELSE
              IF WS-UNSEALED-AT-START = 0
                 SET WG-NOTHING-NEW    TO TRUE
                 MOVE 'No rows appended since the last seal'
                                       TO SR-M-TEXT
                 PERFORM 6200-PRINT-MESSAGE
              ELSE
                 PERFORM 2000-VERIFY-SLICES
                 IF NOT WS-WALK-BROKEN
                    PERFORM 2500-CONFIRM-SEALED
                    PERFORM 3000-CHECK-HEAD-HASH
                 END-IF
              END-IF
           END-IF
           PERFORM 2900-FINISH
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMSEALR RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO SR-M-TEXT WS-ABEND-TEXT
                                          WG-RECORD WK-RECORD
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
           IF WC-MAX-SLICES-X IS NUMERIC AND WC-MAX-SLICES > 0
              MOVE WC-MAX-SLICES       TO WS-MAX-SLICES
           END-IF
           PERFORM 4000-GET-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO SR-H1-BUS-DATE
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM
      * The register is supplementary evidence: without it the seal
      * still runs, with a warning.
           OPEN EXTEND SEALREG-OUT
           IF WS-FS-REG = '00'
              MOVE 'Y'                 TO WS-REG-SW
           ELSE
              MOVE 4                   TO WS-RETURN-CODE
              STRING 'SEALREG not available (status ' WS-FS-REG
                     ') - seal register not written'
                     DELIMITED BY SIZE INTO SR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
           ELSE
              OPEN INPUT CHKPT-FILE
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE SK-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              IF WK-IN-FLIGHT
                 MOVE 'Y'              TO WS-CKPT-SW
                 MOVE WK-LAST-SEALED-SEQ TO WS-CKPT-SEALED
              END-IF
              OPEN EXTEND CHKPT-FILE
           END-IF
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           SET WK-IN-FLIGHT            TO TRUE.

      ******************************************************************
      * BR-CLM-SEA-001 / 002 / 003 Walk in slices from the last sealed
      * row until the head captured at start is proven.
       2000-VERIFY-SLICES.
           PERFORM UNTIL NOT WS-WALK-CONTINUE
              ADD 1                    TO WS-SLICE
              PERFORM 8100-CALL-VERIFY
              PERFORM 4900-COMMIT-CHECKPOINT
              PERFORM 6300-PRINT-SLICE
              EVALUATE TRUE
                 WHEN WS-WALK-BROKEN
                    CONTINUE
                 WHEN WS-VERIFIED-SEQ >= WS-HEAD-AT-START
                    SET WS-WALK-DONE   TO TRUE
                 WHEN ACA-REASON-CODE NOT = 'AUCPARTL'
                    SET WS-WALK-DONE   TO TRUE
                 WHEN WS-SLICE >= WS-MAX-SLICES
                    SET WS-WALK-DONE   TO TRUE
                    IF WS-RETURN-CODE < 4
                       MOVE 4          TO WS-RETURN-CODE
                    END-IF
                    MOVE 'Slice limit reached - remainder sealed on'
                         & ' the next run'
                                       TO SR-M-TEXT
                    PERFORM 6200-PRINT-MESSAGE
              END-EVALUATE
           END-PERFORM
           IF WS-WALK-BROKEN
              SET WG-BROKEN            TO TRUE
           ELSE
              SET WG-SEALED            TO TRUE
           END-IF.

      ******************************************************************
      * BR-CLM-SEA-004 Rows up to the verified head must be sealed;
      * any CLMAUDCH left (e.g. a slice cut short) are sealed here in
      * ranges of WS-COMMIT-FREQ rows, each range committed.
       2500-CONFIRM-SEALED.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-LEFT-UNSEALED
                FROM CLMDB.CLM_AUDIT_CHAIN
               WHERE CHAIN_SEQ  <= :WS-VERIFIED-SEQ
                 AND SEALED_FLAG = 'N'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-CONFIRM-SEALED' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-LEFT-UNSEALED > 0
              MOVE WS-START-SEALED     TO WS-SEAL-FROM
              PERFORM UNTIL WS-SEAL-FROM >= WS-VERIFIED-SEQ
                 COMPUTE WS-SEAL-TO = WS-SEAL-FROM + WS-COMMIT-FREQ
                 IF WS-SEAL-TO > WS-VERIFIED-SEQ
                    MOVE WS-VERIFIED-SEQ TO WS-SEAL-TO
                 END-IF
                 PERFORM 4400-SEAL-RANGE
                 MOVE WS-SEAL-TO       TO WS-SEAL-FROM
              END-PERFORM
              MOVE WS-LEFT-UNSEALED    TO WS-DISP-ID
              STRING WS-DISP-ID ' verified rows found unsealed and'
                     ' sealed by CLMSEALR'
                     DELIMITED BY SIZE INTO SR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF.

      ******************************************************************
       2900-FINISH.
           MOVE WS-BUS-DATE            TO WG-BUS-DATE
           MOVE FUNCTION CURRENT-DATE  TO WG-RUN-TIMESTAMP
           MOVE WS-START-SEALED        TO WG-FROM-SEQ
           MOVE WS-VERIFIED-SEQ        TO WG-TO-SEQ
           MOVE WS-ROWS-VERIFIED       TO WG-ROWS-VERIFIED
           MOVE WS-VERIFIED-HASH       TO WG-HEAD-HASH
           MOVE WS-BROKEN-SEQ          TO WG-BROKEN-SEQ
           MOVE WS-FINAL-REASON        TO WG-REASON-CODE
           IF WS-REG-OPEN
              WRITE SG-RECORD FROM WG-RECORD
              IF WS-FS-REG NOT = '00'
                 MOVE 'SEALREG write failed' TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
           END-IF
           SET WK-ENDED                TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           PERFORM 6500-PRINT-SUMMARY.

      ******************************************************************
      * BR-CLM-SEA-005 Head hash reported by AUCVFY must match the row
       3000-CHECK-HEAD-HASH.
           IF WS-VERIFIED-SEQ > 0
              PERFORM 4300-READ-STORED-HASH
              IF WS-STORED-HASH NOT = WS-VERIFIED-HASH
                 MOVE 12               TO WS-RETURN-CODE
                 SET WG-BROKEN         TO TRUE
                 MOVE 'HEADMISM'       TO WS-FINAL-REASON
                 MOVE WS-VERIFIED-SEQ  TO WS-BROKEN-SEQ
                                          WS-DISP-ID
                 STRING '*** HEAD HASH MISMATCH at seq ' WS-DISP-ID
                        ' verified ' WS-VERIFIED-HASH
                        ' stored ' WS-STORED-HASH
                        DELIMITED BY SIZE INTO SR-M-TEXT
                 PERFORM 6200-PRINT-MESSAGE
              END-IF
           END-IF.

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
      * Chain position at start: last sealed row, head, unsealed count.
      * On restart the checkpoint trail is reconciled with the table.
       4100-READ-CHAIN-POSITION.
           EXEC SQL
              SELECT COALESCE(MAX(CASE WHEN SEALED_FLAG = 'Y'
                                       THEN CHAIN_SEQ END), 0),
                     COALESCE(MAX(CHAIN_SEQ), 0),
                     COALESCE(SUM(CASE WHEN SEALED_FLAG = 'N'
                                       THEN 1 ELSE 0 END), 0)
                INTO :WS-START-SEALED, :WS-HEAD-AT-START,
                     :WS-UNSEALED-AT-START
                FROM CLMDB.CLM_AUDIT_CHAIN
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-READ-CHAIN-POSITION' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-START-SEALED        TO WS-DISP-ID
           MOVE WS-HEAD-AT-START       TO WS-DISP-ID2
           STRING 'Chain at start: last sealed seq ' WS-DISP-ID
                  ', head seq ' WS-DISP-ID2
                  DELIMITED BY SIZE INTO SR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           IF WS-CKPT-IN-FLIGHT
              AND WS-CKPT-SEALED NOT = WS-START-SEALED
              IF WS-RETURN-CODE < 4
                 MOVE 4                TO WS-RETURN-CODE
              END-IF
              MOVE WS-CKPT-SEALED      TO WS-DISP-ID
              STRING 'Checkpoint shows seq ' WS-DISP-ID
                     ' sealed; table position used for restart'
                     DELIMITED BY SIZE INTO SR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF.

      ******************************************************************
       4300-READ-STORED-HASH.
           MOVE WS-VERIFIED-SEQ        TO AUC-CHAIN-SEQ
           EXEC SQL
              SELECT CHAIN_HASH
                INTO :AUC-CHAIN-HASH
                FROM CLMDB.CLM_AUDIT_CHAIN
               WHERE CHAIN_SEQ = :AUC-CHAIN-SEQ
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE AUC-CHAIN-HASH   TO WS-STORED-HASH
              WHEN 100
                 MOVE SPACES           TO WS-STORED-HASH
              WHEN OTHER
                 MOVE '4300-READ-STORED-HASH' TO WS-ERR-PARAGRAPH
                 MOVE WS-VERIFIED-SEQ  TO WS-DISP-ID
                 MOVE WS-DISP-ID       TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4400-SEAL-RANGE.
           EXEC SQL
              UPDATE CLMDB.CLM_AUDIT_CHAIN
                 SET SEALED_FLAG = 'Y'
               WHERE CHAIN_SEQ   > :WS-SEAL-FROM
                 AND CHAIN_SEQ  <= :WS-SEAL-TO
                 AND SEALED_FLAG = 'N'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 ADD SQLERRD(3)        TO WS-COMPLETED-SEALS
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4400-SEAL-RANGE' TO WS-ERR-PARAGRAPH
                 MOVE WS-SEAL-TO       TO WS-DISP-ID
                 MOVE WS-DISP-ID       TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           PERFORM 4900-COMMIT-CHECKPOINT.

      ******************************************************************
       4900-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-COMMIT-CHECKPOINT' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO WK-PROGRAM
           MOVE WS-BUS-DATE            TO WK-BUS-DATE
           MOVE WS-VERIFIED-SEQ        TO WK-LAST-SEALED-SEQ
           MOVE WS-VERIFIED-HASH       TO WK-HEAD-HASH
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE SK-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-SEA-006 A break (RC 16) is reported, not abended: the
      * rows proven before it are sealed and committed.
       8100-CALL-VERIFY.
           INITIALIZE ACA-BODY
           MOVE STH-HEADER             TO ACA-HEADER
           MOVE 'AUCVFY  '             TO ACA-REQUEST-ID
           MOVE 0                      TO ACA-AC-FROM-SEQ
           MOVE WS-PROGRAM-ID          TO ACA-AC-PROGRAM
           CALL WS-AUC-PGM USING WS-AUC-CA
           MOVE ACA-REASON-CODE        TO WS-FINAL-REASON
           ADD ACA-AC-ROWS-CHECKED     TO WS-ROWS-VERIFIED
           IF ACA-AC-CHAIN-SEQ > 0
              MOVE ACA-AC-CHAIN-SEQ    TO WS-VERIFIED-SEQ
              MOVE ACA-AC-CHAIN-HASH   TO WS-VERIFIED-HASH
           END-IF
           EVALUATE TRUE
              WHEN ACA-RC-OK
              WHEN ACA-RC-WARNING
                 CONTINUE
              WHEN ACA-RC-CONFLICT
                 SET WS-WALK-BROKEN    TO TRUE
                 MOVE ACA-AC-BROKEN-SEQ TO WS-BROKEN-SEQ
                 MOVE 12               TO WS-RETURN-CODE
                 MOVE ACA-AC-BROKEN-SEQ TO WS-DISP-ID
                 STRING '*** AUDIT CHAIN BROKEN AT SEQ ' WS-DISP-ID
                        ' (' ACA-REASON-CODE ') ' ACA-MESSAGE
                        DELIMITED BY SIZE INTO SR-M-TEXT
                 PERFORM 6200-PRINT-MESSAGE
              WHEN ACA-RC-NOT-FOUND
                 SET WS-WALK-DONE      TO TRUE
              WHEN OTHER
                 STRING 'CLMAUDCH AUCVFY failed ' ACA-REASON-CODE ' '
                        ACA-MESSAGE DELIMITED BY SIZE
                        INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
           END-EVALUATE.

      ******************************************************************
       6100-PRINT-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              MOVE RPT-RECORD          TO WS-HOLD-LINE
              ADD 1                    TO WS-PAGE-NO
              MOVE WS-PAGE-NO          TO SR-H1-PAGE
              WRITE RPT-RECORD FROM SR-HEAD-1 AFTER ADVANCING PAGE
              MOVE SPACES              TO RPT-RECORD
              WRITE RPT-RECORD AFTER ADVANCING 1 LINE
              MOVE 3                   TO WS-LINE-COUNT
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6200-PRINT-MESSAGE.
           MOVE SR-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                 TO SR-M-TEXT.

      ******************************************************************
       6300-PRINT-SLICE.
           MOVE WS-SLICE               TO SR-S-SLICE
           MOVE ACA-RETURN-CODE        TO SR-S-RC
           MOVE ACA-REASON-CODE        TO SR-S-REASON
           MOVE ACA-AC-ROWS-CHECKED    TO SR-S-ROWS
           MOVE WS-VERIFIED-SEQ        TO SR-S-TO-SEQ
           MOVE WS-VERIFIED-HASH       TO SR-S-HASH
           MOVE SR-SLICE-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6500-PRINT-SUMMARY.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO SR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           MOVE SPACES                 TO SR-T-TEXT
           MOVE 'Outcome'              TO SR-T-LABEL
           MOVE 0                      TO SR-T-COUNT
           MOVE WG-OUTCOME             TO SR-T-TEXT
           PERFORM 6510-PRINT-TOTAL
           MOVE SPACES                 TO SR-T-TEXT
           MOVE 'Last sealed seq at start' TO SR-T-LABEL
           MOVE WS-START-SEALED        TO SR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Chain head seq at start' TO SR-T-LABEL
           MOVE WS-HEAD-AT-START       TO SR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Rows verified this run' TO SR-T-LABEL
           MOVE WS-ROWS-VERIFIED       TO SR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Verification slices'  TO SR-T-LABEL
           MOVE WS-SLICE               TO SR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Rows sealed by CLMSEALR completion' TO SR-T-LABEL
           MOVE WS-COMPLETED-SEALS     TO SR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Sealed head seq / head hash' TO SR-T-LABEL
           MOVE WS-VERIFIED-SEQ        TO SR-T-COUNT
           MOVE WS-VERIFIED-HASH       TO SR-T-TEXT
           PERFORM 6510-PRINT-TOTAL
           MOVE SPACES                 TO SR-T-TEXT
           MOVE 'Broken at seq (0 = intact)' TO SR-T-LABEL
           MOVE WS-BROKEN-SEQ          TO SR-T-COUNT
           MOVE WS-FINAL-REASON        TO SR-T-TEXT
           PERFORM 6510-PRINT-TOTAL
           MOVE SPACES                 TO SR-T-TEXT
           MOVE 'Return code'          TO SR-T-LABEL
           MOVE WS-RETURN-CODE         TO SR-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE SR-TOTAL-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       9000-TERMINATE.
           IF WS-REG-OPEN
              CLOSE SEALREG-OUT
           END-IF
           CLOSE CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           IF WS-RETURN-CODE < 16
              MOVE 16                  TO WS-RETURN-CODE
           END-IF
           STRING '*** RUN ABANDONED: ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO SR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           DISPLAY 'CLMSEALR ' WS-ABEND-TEXT
           PERFORM 6500-PRINT-SUMMARY
           IF WS-REG-OPEN
              CLOSE SEALREG-OUT
           END-IF
           CLOSE CHKPT-FILE RPT-OUT
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
           STRING 'CLMSEALR Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
