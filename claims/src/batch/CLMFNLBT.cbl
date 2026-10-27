       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMFNLBT.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMFNLBT                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Bulk first notice of loss intake from the web    *
      *               and partner portal gateway. Balances the feed,   *
      *               stages every FN record into CLM_FNOL_INTAKE,     *
      *               then works the NEW intake rows: validate, open   *
      *               the claim (CLMNEW), first incident (INCNEW),     *
      *               first exposure (EXPNEW) and the initial reserve  *
      *               (RSVSET) as one unit of work per claim.          *
      * FILES       : FNOLIN  - intake feed (CLMFNLIN, 600 F)          *
      *               FNOLREJ - rejected intake images (CLMFNLRJ, 700) *
      *               CHKPT   - checkpoint / restart (CLMFNLCK, 80)    *
      *               SYSIN   - control card (CLMFNLCC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : phase S restarts staging after CK-INPUT-COUNT    *
      *               feed records; phase P restarts after             *
      *               CK-LAST-INTAKE-ID. Commit per claim.             *
      * CALLS       : CLMSEQGN SEQNEXT  CLMCLAIM CLMNEW                *
      *               CLMINCDT INCNEW   CLMEXPOS EXPNEW                *
      *               CLMFINTX RSVSET   CLMDATES DTVALD DTDIFF         *
      *               CLMERRLG ERRLOG                                  *
      * TABLES      : CLM_FNOL_INTAKE (C R U)  CLM_CONTROL (R)         *
      *               CLM_RISK_UNIT (R)  CLM_POLICY_COVERAGE (R)       *
      *               CLM_CLAIM_CONTACT (R)  CLM_CODE_TABLE (R)        *
      * RULES       : BR-CLM-FNB-001 Feed must balance (header first,  *
      *                              trailer last, count and damage    *
      *                              hash) or nothing is staged        *
      *               BR-CLM-FNB-002 A source + external reference     *
      *                              already DONE is a replay: reject  *
      *               BR-CLM-FNB-003 Loss date valid, not after the    *
      *                              business date, not over 3 years   *
      *                              old                               *
      *               BR-CLM-FNB-004 Motor losses need a vehicle reg   *
      *                              on the policy schedule; injury    *
      *                              flag must be Y or N; cause must   *
      *                              belong to the loss type (LOSSCAUS)*
      *               BR-CLM-FNB-005 First exposure from loss type,    *
      *                              cause and injury flag: AUTO       *
      *                              VEHDAMAG on COLL (impact causes)  *
      *                              or COMP; PROP theft CONTENTS,     *
      *                              other PROPDAMA on DWELLING/BLDG;  *
      *                              LIAB PROPDAMA on LIAB_PD; injury  *
      *                              BODINJUR on LIAB_BI for the       *
      *                              reporter as injured person        *
      *               BR-CLM-FNB-006 Initial reserve from the table by *
      *                              exposure type and severity; a     *
      *                              higher reporter estimate is used  *
      *                              up to 3x table and coverage limit *
      *               BR-CLM-FNB-007 Intake is atomic: any failure     *
      *                              after CLMNEW rolls back to the    *
      *                              claim savepoint, intake ERRR      *
      *               BR-CLM-FNB-008 Reserve above batch authority is  *
      *                              recorded pending approval (PAPR); *
      *                              intake DONE with RSVAPRV warning  *
      *               BR-CLM-FNB-009 Reject rate above the control     *
      *                              card tolerance fails the run      *
      *               BR-CLM-FNB-010 Intake source must be a bulk      *
      *                              HOWRPTD channel: WEB APP BRKR AGGR*
      * RETURN CODE : 0 ok, 4 warnings, 8 rejects, 12 feed out of      *
      *               balance / reject tolerance, 16 Db2 or service    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FNOL-IN       ASSIGN TO FNOLIN
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-IN.
           SELECT REJECT-OUT    ASSIGN TO FNOLREJ
                                ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FS-REJ.
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
       FD  FNOL-IN
           RECORDING MODE IS F
           RECORD CONTAINS 600 CHARACTERS.
           COPY CLMFNLIN.
       FD  REJECT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 700 CHARACTERS.
           COPY CLMFNLRJ.
       FD  CHKPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMFNLCK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMFNLCC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMFNLBT'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-SERVICE-PGMS.
           05 WS-SEQ-PGM               PIC X(8)  VALUE 'CLMSEQGN'.
           05 WS-CLM-PGM               PIC X(8)  VALUE 'CLMCLAIM'.
           05 WS-INC-PGM               PIC X(8)  VALUE 'CLMINCDT'.
           05 WS-EXP-PGM               PIC X(8)  VALUE 'CLMEXPOS'.
           05 WS-FIN-PGM               PIC X(8)  VALUE 'CLMFINTX'.
           05 WS-DTE-PGM               PIC X(8)  VALUE 'CLMDATES'.
           05 WS-ERR-PGM               PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-FS-IN                 PIC X(2).
              88 WS-FS-IN-OK            VALUE '00'.
              88 WS-FS-IN-EOF           VALUE '10'.
           05 WS-FS-REJ                PIC X(2).
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
              88 WS-FS-CHK-EOF          VALUE '10'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-CURSOR-SW             PIC X     VALUE 'N'.
              88 WS-CURSOR-OPEN         VALUE 'Y'.
           05 WS-STAGING-SW            PIC X     VALUE 'Y'.
              88 WS-STAGING-NEEDED      VALUE 'Y'.
           05 WS-BALANCE-SW            PIC X     VALUE 'N'.
              88 WS-FEED-BALANCED       VALUE 'Y'.
           05 WS-RECORD-SW             PIC X     VALUE 'V'.
              88 WS-REC-VALID           VALUE 'V'.
              88 WS-REC-REJECTED        VALUE 'R'.
              88 WS-REC-FAILED          VALUE 'F'.
           05 WS-WARN-SW               PIC X     VALUE 'N'.
              88 WS-REC-WARNING         VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-COMMIT-FREQ           PIC S9(5) COMP-3 VALUE 200.
           05 WS-MAX-REJECT-PCT        PIC S9(3) COMP-3 VALUE 10.
           05 WS-RUN-MODE-TEXT         PIC X(8)  VALUE 'NEW'.
           05 WS-SKIP-COUNT            PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESTART-ID            PIC S9(9) COMP   VALUE 0.
           05 WS-UNCOMMITTED           PIC S9(9) COMP-3 VALUE 0.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-FEED-RECORDS          PIC S9(9) COMP-3 VALUE 0.
           05 WS-FEED-DETAILS          PIC S9(9) COMP-3 VALUE 0.
           05 WS-FEED-DAMAGE           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TRL-COUNT             PIC S9(9) COMP-3 VALUE 0.
           05 WS-TRL-DAMAGE            PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-STAGED                PIC S9(9) COMP-3 VALUE 0.
           05 WS-SKIPPED               PIC S9(9) COMP-3 VALUE 0.
           05 WS-PROCESSED             PIC S9(9) COMP-3 VALUE 0.
           05 WS-CLAIMS-CREATED        PIC S9(9) COMP-3 VALUE 0.
           05 WS-REJECTS               PIC S9(9) COMP-3 VALUE 0.
           05 WS-FAILURES              PIC S9(9) COMP-3 VALUE 0.
           05 WS-WARNINGS              PIC S9(9) COMP-3 VALUE 0.
           05 WS-TOTAL-RESERVE         PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-EST-DAMAGE      PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-REJECT-PCT            PIC S9(5)V99 COMP-3 VALUE 0.

       01  WS-RECORD-WORK.
           05 WS-ERR-CODE              PIC X(8).
           05 WS-ERR-TEXT              PIC X(80).
           05 WS-SEVERITY              PIC S9(4) COMP.
           05 WS-EXP-TYPE              PIC X(8).
           05 WS-INC-TYPE              PIC X(4).
           05 WS-PREF-COVERAGE         PIC X(8).
           05 WS-ALT-COVERAGE-1        PIC X(8).
           05 WS-ALT-COVERAGE-2        PIC X(8).
           05 WS-COV-RU-SEQ            PIC S9(4) COMP.
           05 WS-CLAIMANT-ID           PIC S9(9) COMP.
           05 WS-CONTACT-ROLE          PIC X(8).
           05 WS-INJURED-ID            PIC S9(9) COMP.
           05 WS-CAUSE-PARENT          PIC X(10).
           05 WS-COV-SEQ               PIC S9(4) COMP.
           05 WS-COV-TYPE              PIC X(8).
           05 WS-COV-LIMIT             PIC S9(11)V99 COMP-3.
           05 WS-RU-SEQ                PIC S9(4) COMP.
           05 WS-RU-IND                PIC S9(4) COMP.
           05 WS-INCIDENT-ID           PIC S9(9) COMP.
           05 WS-EXPOSURE-ID           PIC S9(9) COMP.
           05 WS-TABLE-RESERVE         PIC S9(11)V99 COMP-3.
           05 WS-RESERVE-CAP           PIC S9(11)V99 COMP-3.
           05 WS-INIT-RESERVE          PIC S9(11)V99 COMP-3.
           05 WS-INTAKE-STATUS         PIC X(4).
           05 WS-DUP-SOURCE            PIC X(4).
           05 WS-DUP-REF               PIC X(20).
           05 WS-DUP-COUNT             PIC S9(9) COMP.
           05 WS-DAYS-OLD              PIC S9(7) COMP-3.
           05 WS-HH                    PIC 99.
           05 WS-MI                    PIC 99.
           05 WS-SS                    PIC 99.
           05 WS-SVC-RC                PIC 9(2).
           05 WS-SVC-REASON            PIC X(8).
           05 WS-SVC-MESSAGE           PIC X(80).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).

           COPY CLMFNLIN REPLACING LEADING ==FI-== BY ==WI-==.

           COPY CLMFNLCK REPLACING LEADING ==FK-== BY ==WK-==.

           COPY CLMFNLCC REPLACING LEADING ==FC-== BY ==WC-==.

           COPY CLMFNLRT.

           COPY CLMFNLRP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDFNI END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.
           EXEC SQL INCLUDE CLMDCOD END-EXEC.

           EXEC SQL DECLARE FNI-CSR CURSOR WITH HOLD FOR
              SELECT INTAKE_ID, INTAKE_SOURCE, RAW_RECORD
                FROM CLMDB.CLM_FNOL_INTAKE
               WHERE INTAKE_STATUS = 'NEW'
                 AND INTAKE_ID     > :WS-RESTART-ID
               ORDER BY INTAKE_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-CLM-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CLA-==.
           03 CLA-BODY.
              COPY CLMCCLM REPLACING LEADING ==CA-== BY ==CLA-==.
       01  WS-INC-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==INA-==.
           03 INA-BODY.
              COPY CLMCINC REPLACING LEADING ==CA-== BY ==INA-==.
       01  WS-EXP-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==EXA-==.
           03 EXA-BODY.
              COPY CLMCEXP REPLACING LEADING ==CA-== BY ==EXA-==.
       01  WS-FIN-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==FTA-==.
           03 FTA-BODY.
              COPY CLMCFIN REPLACING LEADING ==CA-== BY ==FTA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           IF WS-STAGING-NEEDED
              PERFORM 2000-BALANCE-FEED
              IF NOT WS-FEED-BALANCED
                 PERFORM 9000-TERMINATE
              END-IF
              PERFORM 2100-STAGE-FEED
           END-IF
           PERFORM 2200-PROCESS-INTAKE
           PERFORM 2900-FINAL-CONTROLS
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMFNLBT RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO FP-M-TEXT WS-ABEND-TEXT
           PERFORM 1100-READ-CONTROL-CARD
           PERFORM 4000-GET-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO FP-H1-BUS-DATE
           MOVE WS-RUN-MODE-TEXT       TO FP-H1-MODE
           PERFORM 1200-RESTART-POSITION
           OPEN OUTPUT REJECT-OUT
           IF WS-FS-REJ NOT = '00'
              MOVE 'FNOLREJ open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM.

      ******************************************************************
       1100-READ-CONTROL-CARD.
           OPEN INPUT CTL-CARD
           IF WS-FS-CTL = '00'
              READ CTL-CARD INTO WC-CARD
                 AT END MOVE SPACES    TO WC-CARD
              END-READ
              CLOSE CTL-CARD
           ELSE
              MOVE SPACES              TO WC-CARD
           END-IF
           IF WC-MODE-RESTART
              MOVE 'RESTART'           TO WS-RUN-MODE-TEXT
           END-IF
           IF WC-COMMIT-FREQ-X IS NUMERIC
              AND WC-COMMIT-FREQ > 0
              MOVE WC-COMMIT-FREQ      TO WS-COMMIT-FREQ
           END-IF
           IF WC-MAX-REJECT-PCT-X IS NUMERIC
              MOVE WC-MAX-REJECT-PCT   TO WS-MAX-REJECT-PCT
           END-IF.

      ******************************************************************
      * Restart: read the checkpoint trail to its last record, then
      * reopen it for EXTEND so this run appends to the same trail.
       1200-RESTART-POSITION.
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
              IF NOT WS-FS-CHK-OK
                 MOVE 'CHKPT open output failed' TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
           ELSE
              OPEN INPUT CHKPT-FILE
              IF NOT WS-FS-CHK-OK
                 MOVE 'Restart requested but CHKPT unreadable'
                                       TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE FK-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              EVALUATE TRUE
                 WHEN WK-PHASE-STAGING
                    MOVE WK-INPUT-COUNT    TO WS-SKIP-COUNT
                 WHEN WK-PHASE-PROCESS
                    MOVE 'N'               TO WS-STAGING-SW
                    MOVE WK-LAST-INTAKE-ID TO WS-RESTART-ID
                    MOVE WK-CLAIMS-CREATED TO WS-CLAIMS-CREATED
                    MOVE WK-REJECTS        TO WS-REJECTS
                 WHEN WK-PHASE-END
                    MOVE 'Previous run ended normally - rerun as N'
                                           TO WS-ABEND-TEXT
                    MOVE 12                TO WS-RETURN-CODE
                    PERFORM 9800-ABEND
                 WHEN OTHER
                    MOVE 'CHKPT trail empty - nothing to restart'
                                           TO WS-ABEND-TEXT
                    PERFORM 9800-ABEND
              END-EVALUATE
              OPEN EXTEND CHKPT-FILE
              MOVE WK-LAST-INTAKE-ID   TO WS-DISP-ID
              STRING 'Restart: phase ' WK-PHASE ' skip '
                     WK-INPUT-COUNT ' feed records, after intake '
                     WS-DISP-ID DELIMITED BY SIZE INTO FP-M-TEXT
              MOVE FP-MESSAGE-LINE     TO RPT-RECORD
              PERFORM 6100-PRINT-LINE
           END-IF.

      ******************************************************************
      * BR-CLM-FNB-001 Balance pass: header first, trailer last, the
      * trailer count and damage hash must equal the detail records.
       2000-BALANCE-FEED.
           OPEN INPUT FNOL-IN
           IF NOT WS-FS-IN-OK
              MOVE 'FNOLIN open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE 'N'                    TO WS-EOF-SW
           MOVE 'Y'                    TO WS-BALANCE-SW
           PERFORM 6000-READ-FEED
           IF WS-EOF OR NOT FI-IS-HEADER
              MOVE 'N'                 TO WS-BALANCE-SW
              MOVE 'Feed rejected: first record is not a header'
                                       TO FP-M-TEXT
           END-IF
           PERFORM UNTIL WS-EOF OR NOT WS-FEED-BALANCED
              PERFORM 6000-READ-FEED
              IF NOT WS-EOF
                 EVALUATE TRUE
                    WHEN FI-IS-DETAIL
                       IF WS-TRL-COUNT NOT = 0
                          MOVE 'N'     TO WS-BALANCE-SW
                          MOVE 'Feed rejected: detail after trailer'
                                       TO FP-M-TEXT
                       END-IF
                       ADD 1           TO WS-FEED-DETAILS
                       IF FI-EST-DAMAGE-X IS NUMERIC
                          ADD FI-EST-DAMAGE TO WS-FEED-DAMAGE
                       END-IF
                    WHEN FI-IS-TRAILER
                       IF FI-TR-RECORD-COUNT-X IS NUMERIC
                          AND FI-TR-DAMAGE-TOTAL-X IS NUMERIC
                          MOVE FI-TR-RECORD-COUNT TO WS-TRL-COUNT
                          MOVE FI-TR-DAMAGE-TOTAL TO WS-TRL-DAMAGE
                       ELSE
                          MOVE 'N'     TO WS-BALANCE-SW
                          MOVE 'Feed rejected: trailer not numeric'
                                       TO FP-M-TEXT
                       END-IF
                    WHEN OTHER
                       MOVE 'N'        TO WS-BALANCE-SW
                       MOVE 'Feed rejected: unknown record type'
                                       TO FP-M-TEXT
                 END-EVALUATE
              END-IF
           END-PERFORM
           CLOSE FNOL-IN
           IF WS-FEED-BALANCED
              AND (WS-TRL-COUNT NOT = WS-FEED-DETAILS
                   OR WS-TRL-DAMAGE NOT = WS-FEED-DAMAGE
                   OR WS-FEED-DETAILS = 0)
              MOVE 'N'                 TO WS-BALANCE-SW
              MOVE 'Feed rejected: trailer count/hash out of balance'
                                       TO FP-M-TEXT
           END-IF
           IF NOT WS-FEED-BALANCED
              MOVE FP-MESSAGE-LINE     TO RPT-RECORD
              PERFORM 6100-PRINT-LINE
              MOVE 12                  TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
       2100-STAGE-FEED.
           OPEN INPUT FNOL-IN
           MOVE 'N'                    TO WS-EOF-SW
           MOVE 0                      TO WS-FEED-DETAILS
           PERFORM UNTIL WS-EOF
              PERFORM 6000-READ-FEED
              IF NOT WS-EOF AND FI-IS-DETAIL
                 ADD 1                 TO WS-FEED-DETAILS
                 IF WS-FEED-DETAILS <= WS-SKIP-COUNT
                    ADD 1              TO WS-SKIPPED
                 ELSE
                    PERFORM 4100-STAGE-RECORD
                    ADD 1              TO WS-UNCOMMITTED
                    IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                       SET WK-PHASE-STAGING TO TRUE
                       PERFORM 4900-COMMIT-CHECKPOINT
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           CLOSE FNOL-IN
           MOVE 0                      TO WS-RESTART-ID
           SET WK-PHASE-PROCESS        TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT.

      ******************************************************************
       2200-PROCESS-INTAKE.
           PERFORM 4400-OPEN-CURSOR
           PERFORM 4410-FETCH-INTAKE
           PERFORM UNTIL WS-EOF
              PERFORM 2300-PROCESS-ONE-INTAKE
              PERFORM 4410-FETCH-INTAKE
           END-PERFORM
           PERFORM 4420-CLOSE-CURSOR.

      ******************************************************************
      * One claim per unit of work; the commit also writes the
      * checkpoint so a restart resumes after this INTAKE_ID.
       2300-PROCESS-ONE-INTAKE.
           ADD 1                       TO WS-PROCESSED
           MOVE FNI-RAW-RECORD-TEXT    TO WI-RECORD
           SET WS-REC-VALID            TO TRUE
           MOVE 'N'                    TO WS-WARN-SW
           MOVE SPACES                 TO WS-ERR-CODE WS-ERR-TEXT
                                          WS-EXP-TYPE
           MOVE 0                      TO WS-INIT-RESERVE WS-SEVERITY
           MOVE SPACES                 TO CLA-CL-CLAIM-NUMBER
           MOVE 0                      TO CLA-CL-CLAIM-ID
           PERFORM 3000-VALIDATE-INTAKE
           IF WS-REC-VALID
              PERFORM 2400-CREATE-CLAIM
           END-IF
           EVALUATE TRUE
              WHEN WS-REC-VALID
                 MOVE 'DONE'           TO WS-INTAKE-STATUS
                 ADD 1                 TO WS-CLAIMS-CREATED
                 ADD WS-INIT-RESERVE   TO WS-TOTAL-RESERVE
                 IF WS-REC-WARNING
                    ADD 1              TO WS-WARNINGS
                 END-IF
              WHEN WS-REC-REJECTED
                 MOVE 'ERRR'           TO WS-INTAKE-STATUS
                 ADD 1                 TO WS-REJECTS
                 PERFORM 6200-WRITE-REJECT
              WHEN WS-REC-FAILED
      * BR-CLM-FNB-007 Undo claim, incident, exposure and reserve
                 EXEC SQL
                    ROLLBACK TO SAVEPOINT FNOLCLM
                 END-EXEC
                 IF SQLCODE NOT = 0
                    MOVE '2300-PROCESS-ONE-INTAKE' TO WS-ERR-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
                 END-IF
                 MOVE 'ERRR'           TO WS-INTAKE-STATUS
                 MOVE 0                TO CLA-CL-CLAIM-ID
                 MOVE 0                TO WS-INIT-RESERVE
                 ADD 1                 TO WS-FAILURES
                 PERFORM 6200-WRITE-REJECT
           END-EVALUATE
           PERFORM 4300-MARK-INTAKE
           PERFORM 6400-PRINT-DETAIL
           MOVE FNI-INTAKE-ID          TO WS-RESTART-ID
           SET WK-PHASE-PROCESS        TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT.

      ******************************************************************
       2400-CREATE-CLAIM.
           EXEC SQL
              SAVEPOINT FNOLCLM ON ROLLBACK RETAIN CURSORS
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2400-CREATE-CLAIM' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 8100-CALL-CLAIM-NEW
           IF WS-REC-VALID
              PERFORM 5100-DERIVE-EXPOSURE
              MOVE 0                   TO WS-RU-SEQ
              IF FT-LT-MOTOR(FT-LT-IX) AND NOT WI-INJURY-YES
                 PERFORM 4200-FIND-RISK-UNIT
              END-IF
           END-IF
           IF WS-REC-VALID
              PERFORM 4250-PICK-COVERAGE
           END-IF
           IF WS-REC-VALID
              PERFORM 4270-FIND-CLAIMANT
           END-IF
           IF WS-REC-VALID
              PERFORM 8200-CALL-INCIDENT-NEW
           END-IF
           IF WS-REC-VALID
              PERFORM 8300-CALL-EXPOSURE-NEW
           END-IF
           IF WS-REC-VALID
              PERFORM 5200-CALC-INITIAL-RESERVE
              IF WS-INIT-RESERVE > 0
                 PERFORM 8400-CALL-RESERVE-SET
              END-IF
           END-IF.

      ******************************************************************
       2900-FINAL-CONTROLS.
      * BR-CLM-FNB-009 Reject tolerance on the whole run
           IF WS-PROCESSED > 0
              COMPUTE WS-REJECT-PCT ROUNDED =
                      (WS-REJECTS + WS-FAILURES) * 100 / WS-PROCESSED
           END-IF
           IF WS-REJECTS + WS-FAILURES > 0
              PERFORM 9100-SET-RC-8
           END-IF
           IF WS-WARNINGS > 0
              PERFORM 9110-SET-RC-4
           END-IF
           IF WS-REJECT-PCT > WS-MAX-REJECT-PCT
              MOVE 12                  TO WS-RETURN-CODE
              MOVE 'Reject rate exceeds control card tolerance'
                                       TO FP-M-TEXT
              MOVE FP-MESSAGE-LINE     TO RPT-RECORD
              PERFORM 6100-PRINT-LINE
           END-IF
           SET WK-PHASE-END            TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           PERFORM 6500-PRINT-TOTALS.

      ******************************************************************
       3000-VALIDATE-INTAKE.
           PERFORM 3010-VALIDATE-SOURCE-POLICY
           IF WS-REC-VALID
              PERFORM 3020-VALIDATE-LOSS-DATE
           END-IF
           IF WS-REC-VALID
              PERFORM 3030-VALIDATE-LOSS-TIME
           END-IF
           IF WS-REC-VALID
              PERFORM 3040-VALIDATE-LOSS-DETAIL
           END-IF
           IF WS-REC-VALID
              PERFORM 3050-VALIDATE-REPORTER
           END-IF
           IF WS-REC-VALID
              PERFORM 3060-VALIDATE-MOTOR-INJURY
           END-IF
           IF WS-REC-VALID
              PERFORM 4150-CHECK-DUPLICATE-REF
           END-IF.

      ******************************************************************
       3010-VALIDATE-SOURCE-POLICY.
           IF NOT WI-IS-DETAIL
              MOVE 'RECTYPE '          TO WS-ERR-CODE
              MOVE 'Staged record is not an FN detail'
                                       TO WS-ERR-TEXT
              SET WS-REC-REJECTED      TO TRUE
           ELSE
      * BR-CLM-FNB-010 Bulk channels only; each is a HOWRPTD code
              IF WI-SOURCE NOT = 'WEB ' AND NOT = 'APP '
                 AND NOT = 'BRKR' AND NOT = 'AGGR'
                 MOVE 'SOURCE  '       TO WS-ERR-CODE
                 MOVE 'Intake source must be WEB, APP, BRKR or AGGR'
                                       TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              ELSE
                 IF WI-POLICY-NO IS NOT NUMERIC
                    OR WI-POLICY-NO-N = 0
                    MOVE 'POLICYNO'    TO WS-ERR-CODE
                    MOVE 'Policy number missing or not numeric'
                                       TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-FNB-003 Loss date valid, not future, not stale
       3020-VALIDATE-LOSS-DATE.
           MOVE STH-HEADER             TO DTA-HEADER
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE WI-LOSS-DATE           TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-DT-IS-VALID
              MOVE 'LOSSDATE'          TO WS-ERR-CODE
              MOVE 'Loss date is not a valid yyyy-mm-dd date'
                                       TO WS-ERR-TEXT
              SET WS-REC-REJECTED      TO TRUE
           ELSE
              IF WI-LOSS-DATE > WS-BUS-DATE
                 MOVE 'LOSSFUTR'       TO WS-ERR-CODE
                 MOVE 'Loss date is after the business date'
                                       TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              ELSE
                 MOVE STH-HEADER       TO DTA-HEADER
                 MOVE 'DTDIFF  '       TO DTA-REQUEST-ID
                 MOVE WI-LOSS-DATE     TO DTA-DT-DATE-1
                 MOVE WS-BUS-DATE      TO DTA-DT-DATE-2
                 CALL WS-DTE-PGM USING WS-DTE-CA
                 MOVE DTA-DT-RESULT-NUM TO WS-DAYS-OLD
                 IF WS-DAYS-OLD > 1095
                    MOVE 'LOSSSTAL'    TO WS-ERR-CODE
                    MOVE 'Loss more than 3 years old - refer to desk'
                                       TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
      * Loss time optional; hh.mm.ss or hh:mm:ss normalised to hh.mm.ss
       3030-VALIDATE-LOSS-TIME.
           IF WI-LOSS-TIME NOT = SPACES
              IF WI-LOSS-HH IS NUMERIC AND WI-LOSS-MI IS NUMERIC
                 AND WI-LOSS-SS IS NUMERIC
                 AND (WI-LOSS-SEP1 = '.' OR ':')
                 AND (WI-LOSS-SEP2 = '.' OR ':')
                 MOVE WI-LOSS-HH       TO WS-HH
                 MOVE WI-LOSS-MI       TO WS-MI
                 MOVE WI-LOSS-SS       TO WS-SS
                 IF WS-HH > 23 OR WS-MI > 59 OR WS-SS > 59
                    MOVE 'LOSSTIME'    TO WS-ERR-CODE
                    MOVE 'Loss time out of range'
                                       TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 ELSE
                    MOVE '.'           TO WI-LOSS-SEP1 WI-LOSS-SEP2
                 END-IF
              ELSE
                 MOVE 'LOSSTIME'       TO WS-ERR-CODE
                 MOVE 'Loss time must be hh.mm.ss'
                                       TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              END-IF
           END-IF.

      ******************************************************************
       3040-VALIDATE-LOSS-DETAIL.
           SET FT-LT-IX                TO 1
           SEARCH FT-LT
              AT END
                 MOVE 'LOSSTYPE'       TO WS-ERR-CODE
                 STRING 'Loss type ' WI-LOSS-TYPE ' is not accepted'
                        DELIMITED BY SIZE INTO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              WHEN FT-LT-CODE(FT-LT-IX) = WI-LOSS-TYPE
                 CONTINUE
           END-SEARCH
           IF WS-REC-VALID
              EVALUATE TRUE
                 WHEN WI-LOSS-CAUSE = SPACES
                    MOVE 'CAUSE   '    TO WS-ERR-CODE
                    MOVE 'Loss cause is required' TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 WHEN WI-LOSS-DESC = SPACES
                    MOVE 'LOSSDESC'    TO WS-ERR-CODE
                    MOVE 'Loss description is required'
                                       TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 WHEN WI-LOSS-POSTCODE NOT = SPACES
                    AND (WI-LOSS-POSTCODE(1:1) IS NOT ALPHABETIC
                         OR (WI-LOSS-POSTCODE(2:1) IS NOT NUMERIC
                         AND WI-LOSS-POSTCODE(3:1) IS NOT NUMERIC))
                    MOVE 'POSTCODE'    TO WS-ERR-CODE
                    MOVE 'Loss postcode is not a UK postcode'
                                       TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 WHEN WI-EST-DAMAGE-X IS NOT NUMERIC
                    MOVE 'ESTDAMG '    TO WS-ERR-CODE
                    MOVE 'Estimated damage must be 13 numeric digits'
                                       TO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 WHEN OTHER
                    ADD WI-EST-DAMAGE  TO WS-TOTAL-EST-DAMAGE
              END-EVALUATE
           END-IF
           IF WS-REC-VALID
              PERFORM 4160-CHECK-LOSS-CAUSE
           END-IF.

      ******************************************************************
       3050-VALIDATE-REPORTER.
           EVALUATE TRUE
              WHEN WI-RPT-LAST-NAME = SPACES
                 MOVE 'RPTNAME '       TO WS-ERR-CODE
                 MOVE 'Reporter surname is required' TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              WHEN WI-RPT-PHONE = SPACES
                 MOVE 'RPTPHONE'       TO WS-ERR-CODE
                 MOVE 'Reporter phone number is required'
                                       TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              WHEN WI-RPT-PHONE(1:1) NOT = '0' AND NOT = '+'
                 MOVE 'RPTPHONE'       TO WS-ERR-CODE
                 MOVE 'Reporter phone must start 0 or +'
                                       TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-FNB-004 Motor losses need a registration; Y/N injury
       3060-VALIDATE-MOTOR-INJURY.
           IF NOT WI-INJURY-VALID
              MOVE 'INJFLAG '          TO WS-ERR-CODE
              MOVE 'Injury flag must be Y or N' TO WS-ERR-TEXT
              SET WS-REC-REJECTED      TO TRUE
           ELSE
              IF FT-LT-MOTOR(FT-LT-IX) AND WI-VEHICLE-REG = SPACES
                 MOVE 'VEHREG  '       TO WS-ERR-CODE
                 MOVE 'Vehicle registration required for motor loss'
                                       TO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              ELSE
                 INSPECT WI-VEHICLE-REG CONVERTING
                         'abcdefghijklmnopqrstuvwxyz'
                      TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
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
       4100-STAGE-RECORD.
           MOVE STH-HEADER             TO SQA-HEADER
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE 'FNI     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF NOT SQA-RC-OK AND NOT SQA-RC-WARNING
              MOVE SQA-RETURN-CODE     TO WS-SVC-RC
              MOVE SQA-REASON-CODE     TO WS-SVC-REASON
              MOVE SQA-MESSAGE         TO WS-SVC-MESSAGE
              MOVE 'Sequence FNI unavailable' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE SQA-SQ-VALUE           TO FNI-INTAKE-ID
           MOVE FI-SOURCE              TO FNI-INTAKE-SOURCE
           MOVE FI-RECORD              TO FNI-RAW-RECORD-TEXT
           MOVE 600                    TO FNI-RAW-RECORD-LEN
           EXEC SQL
              INSERT INTO CLMDB.CLM_FNOL_INTAKE
                    (INTAKE_ID, INTAKE_SOURCE, RAW_RECORD,
                     INTAKE_STATUS, CLAIM_ID, ERROR_CODE,
                     RECEIVED_TS)
              VALUES (:FNI-INTAKE-ID, :FNI-INTAKE-SOURCE,
                      :FNI-RAW-RECORD, 'NEW', 0, ' ',
                      CURRENT TIMESTAMP)
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-STAGE-RECORD' TO WS-ERR-PARAGRAPH
              MOVE FNI-INTAKE-ID       TO WS-DISP-ID
              MOVE WS-DISP-ID          TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           ADD 1                       TO WS-STAGED.

      ******************************************************************
      * BR-CLM-FNB-002 Same source + external reference already DONE
       4150-CHECK-DUPLICATE-REF.
           MOVE WI-SOURCE              TO WS-DUP-SOURCE
           MOVE WI-EXT-REF             TO WS-DUP-REF
           IF WS-DUP-REF NOT = SPACES
              EXEC SQL
                 SELECT COUNT(*)
                   INTO :WS-DUP-COUNT
                   FROM CLMDB.CLM_FNOL_INTAKE
                  WHERE INTAKE_SOURCE = :WS-DUP-SOURCE
                    AND SUBSTR(RAW_RECORD, 7, 20) = :WS-DUP-REF
                    AND INTAKE_STATUS = 'DONE'
                    AND INTAKE_ID    <> :FNI-INTAKE-ID
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4150-CHECK-DUPLICATE-REF' TO WS-ERR-PARAGRAPH
                 MOVE WS-DUP-REF       TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
              END-IF
              IF WS-DUP-COUNT > 0
                 MOVE 'DUPREF  '       TO WS-ERR-CODE
                 STRING 'Reference ' WS-DUP-REF
                        ' already created a claim'
                        DELIMITED BY SIZE INTO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-FNB-004 The cause must be on the LOSSCAUS typelist under
      * this loss type (or the any-type parent '*').
       4160-CHECK-LOSS-CAUSE.
           MOVE 'LOSSCAUS'             TO COD-TABLE-NAME
           MOVE WI-LOSS-CAUSE          TO COD-CODE-VALUE
           EXEC SQL
              SELECT PARENT_CODE
                INTO :COD-PARENT-CODE
                FROM CLMDB.CLM_CODE_TABLE
               WHERE TABLE_NAME  = :COD-TABLE-NAME
                 AND CODE_VALUE  = :COD-CODE-VALUE
                 AND ACTIVE_FLAG = 'Y'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE WI-LOSS-TYPE     TO WS-CAUSE-PARENT
                 IF COD-PARENT-CODE NOT = WS-CAUSE-PARENT
                    AND COD-PARENT-CODE NOT = '*'
                    MOVE 'CAUSETYP'    TO WS-ERR-CODE
                    STRING 'Cause ' WI-LOSS-CAUSE
                           ' does not apply to loss type '
                           WI-LOSS-TYPE DELIMITED BY SIZE
                           INTO WS-ERR-TEXT
                    SET WS-REC-REJECTED TO TRUE
                 END-IF
              WHEN 100
                 MOVE 'CAUSE   '       TO WS-ERR-CODE
                 STRING 'Loss cause ' WI-LOSS-CAUSE
                        ' is not on the LOSSCAUS typelist'
                        DELIMITED BY SIZE INTO WS-ERR-TEXT
                 SET WS-REC-REJECTED   TO TRUE
              WHEN OTHER
                 MOVE '4160-CHECK-LOSS-CAUSE' TO WS-ERR-PARAGRAPH
                 MOVE WI-LOSS-CAUSE    TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-FNB-004 Motor: the reported vehicle must be a risk unit
      * on the verified policy, otherwise the first-party exposure
      * cannot be raised and the intake is referred back.
       4200-FIND-RISK-UNIT.
           MOVE 0                      TO WS-RU-SEQ
           EXEC SQL
              SELECT MIN(RISK_UNIT_SEQ)
                INTO :WS-RU-SEQ :WS-RU-IND
                FROM CLMDB.CLM_RISK_UNIT
               WHERE CLAIM_ID    = :CLA-CL-CLAIM-ID
                 AND VEHICLE_REG = :WI-VEHICLE-REG
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4200-FIND-RISK-UNIT' TO WS-ERR-PARAGRAPH
              MOVE CLA-CL-CLAIM-NUMBER TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-RU-IND < 0
              MOVE 0                   TO WS-RU-SEQ
              MOVE 'VREGNOTP'          TO WS-ERR-CODE
              STRING 'Vehicle ' WI-VEHICLE-REG
                     ' is not on the policy schedule'
                     DELIMITED BY SIZE INTO WS-ERR-TEXT
              SET WS-REC-FAILED        TO TRUE
           END-IF.

      ******************************************************************
      * Coverage for the first exposure: one of the coverage types that
      * can carry the exposure; for motor the cover on the damaged
      * vehicle first, then the preferred type, then policy order.
       4250-PICK-COVERAGE.
           EXEC SQL
              SELECT COV_SEQ, COVERAGE_TYPE, LIMIT_AMOUNT,
                     RISK_UNIT_SEQ
                INTO :WS-COV-SEQ, :WS-COV-TYPE, :WS-COV-LIMIT,
                     :WS-COV-RU-SEQ
                FROM CLMDB.CLM_POLICY_COVERAGE
               WHERE CLAIM_ID = :CLA-CL-CLAIM-ID
                 AND COVERAGE_TYPE IN (:WS-PREF-COVERAGE,
                                       :WS-ALT-COVERAGE-1,
                                       :WS-ALT-COVERAGE-2)
               ORDER BY CASE WHEN RISK_UNIT_SEQ = :WS-RU-SEQ
                             THEN 0 ELSE 1 END,
                        CASE WHEN COVERAGE_TYPE = :WS-PREF-COVERAGE
                             THEN 0 ELSE 1 END,
                        COV_SEQ
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF NOT FT-LT-MOTOR(FT-LT-IX)
                    MOVE WS-COV-RU-SEQ TO WS-RU-SEQ
                 END-IF
              WHEN 100
                 MOVE 'NOCOVER '       TO WS-ERR-CODE
                 STRING 'Policy has no ' WS-PREF-COVERAGE
                        ' cover for a ' WS-EXP-TYPE ' exposure'
                        DELIMITED BY SIZE INTO WS-ERR-TEXT
                 SET WS-REC-FAILED     TO TRUE
              WHEN OTHER
                 MOVE '4250-PICK-COVERAGE' TO WS-ERR-PARAGRAPH
                 MOVE CLA-CL-CLAIM-NUMBER TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Claimant: the insured for first-party damage; for injury the
      * reporter (the injured person) when CLMNEW linked one.
       4270-FIND-CLAIMANT.
           MOVE 0                      TO WS-CLAIMANT-ID
           IF WI-INJURY-YES
              MOVE 'REPORTER'          TO WS-CONTACT-ROLE
              PERFORM 4280-READ-CLAIM-CONTACT
           END-IF
           IF WS-CLAIMANT-ID = 0
              MOVE 'INSURED '          TO WS-CONTACT-ROLE
              PERFORM 4280-READ-CLAIM-CONTACT
           END-IF
           IF WS-CLAIMANT-ID = 0
              MOVE 'NOCLMNT '          TO WS-ERR-CODE
              MOVE 'No insured or reporter contact on the new claim'
                                       TO WS-ERR-TEXT
              SET WS-REC-FAILED        TO TRUE
           END-IF
           MOVE 0                      TO WS-INJURED-ID
           IF WI-INJURY-YES
              MOVE WS-CLAIMANT-ID      TO WS-INJURED-ID
           END-IF.

      ******************************************************************
       4280-READ-CLAIM-CONTACT.
           EXEC SQL
              SELECT CONTACT_ID
                INTO :WS-CLAIMANT-ID
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID     = :CLA-CL-CLAIM-ID
                 AND CONTACT_ROLE = :WS-CONTACT-ROLE
               ORDER BY CONTACT_ID
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 0                TO WS-CLAIMANT-ID
              WHEN OTHER
                 MOVE '4280-READ-CLAIM-CONTACT' TO WS-ERR-PARAGRAPH
                 MOVE CLA-CL-CLAIM-NUMBER TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-MARK-INTAKE.
           MOVE WS-INTAKE-STATUS       TO FNI-INTAKE-STATUS
           MOVE CLA-CL-CLAIM-ID        TO FNI-CLAIM-ID
           MOVE WS-ERR-CODE            TO FNI-ERROR-CODE
           IF WS-ERR-TEXT = SPACES
              MOVE -1                  TO FNI-ERROR-TEXT-IND
              MOVE 0                   TO FNI-ERROR-TEXT-LEN
           ELSE
              MOVE 0                   TO FNI-ERROR-TEXT-IND
              MOVE WS-ERR-TEXT         TO FNI-ERROR-TEXT-TEXT
              MOVE 80                  TO FNI-ERROR-TEXT-LEN
           END-IF
           EXEC SQL
              UPDATE CLMDB.CLM_FNOL_INTAKE
                 SET INTAKE_STATUS = :FNI-INTAKE-STATUS,
                     CLAIM_ID      = :FNI-CLAIM-ID,
                     ERROR_CODE    = :FNI-ERROR-CODE,
                     ERROR_TEXT    = :FNI-ERROR-TEXT
                                     :FNI-ERROR-TEXT-IND,
                     PROCESSED_TS  = CURRENT TIMESTAMP
               WHERE INTAKE_ID     = :FNI-INTAKE-ID
                 AND INTAKE_STATUS = 'NEW'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-MARK-INTAKE'  TO WS-ERR-PARAGRAPH
              MOVE FNI-INTAKE-ID       TO WS-DISP-ID
              MOVE WS-DISP-ID          TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-OPEN-CURSOR.
           EXEC SQL OPEN FNI-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-OPEN-CURSOR'  TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           SET WS-CURSOR-OPEN          TO TRUE
           MOVE 'N'                    TO WS-EOF-SW.

      ******************************************************************
       4410-FETCH-INTAKE.
           EXEC SQL
              FETCH FNI-CSR
               INTO :FNI-INTAKE-ID, :FNI-INTAKE-SOURCE,
                    :FNI-RAW-RECORD
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF FNI-RAW-RECORD-LEN < 600
                    MOVE SPACES TO FNI-RAW-RECORD-TEXT
                                   (FNI-RAW-RECORD-LEN + 1:)
                 END-IF
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4410-FETCH-INTAKE' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4420-CLOSE-CURSOR.
           IF WS-CURSOR-OPEN
              EXEC SQL CLOSE FNI-CSR END-EXEC
              MOVE 'N'                 TO WS-CURSOR-SW
           END-IF.

      ******************************************************************
       4900-COMMIT-CHECKPOINT.
           EXEC SQL COMMIT END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4900-COMMIT-CHECKPOINT' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 0                      TO WS-UNCOMMITTED
           MOVE WS-PROGRAM-ID          TO WK-PROGRAM
           MOVE WS-BUS-DATE            TO WK-BUS-DATE
           MOVE WS-FEED-DETAILS        TO WK-INPUT-COUNT
           MOVE WS-RESTART-ID          TO WK-LAST-INTAKE-ID
           MOVE WS-CLAIMS-CREATED      TO WK-CLAIMS-CREATED
           COMPUTE WK-REJECTS = WS-REJECTS + WS-FAILURES
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE FK-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-FNB-005 First exposure, incident type and the coverage
      * types that can carry it (pairs as validated by CLMEXPOS)
       5100-DERIVE-EXPOSURE.
           MOVE SPACES                 TO WS-ALT-COVERAGE-1
                                          WS-ALT-COVERAGE-2
           EVALUATE TRUE
              WHEN WI-INJURY-YES
                 MOVE 'INJ '           TO WS-INC-TYPE
                 MOVE 'BODINJUR'       TO WS-EXP-TYPE
                 MOVE 'LIAB_BI '       TO WS-PREF-COVERAGE
              WHEN FT-LT-MOTOR(FT-LT-IX)
                 MOVE 'VEH '           TO WS-INC-TYPE
                 MOVE 'VEHDAMAG'       TO WS-EXP-TYPE
                 IF WI-LOSS-CAUSE = 'COLLISION' OR 'SNGLVEH'
                    OR 'ANIMAL' OR 'KERB' OR 'PARKED'
                    MOVE 'COLL    '    TO WS-PREF-COVERAGE
                    MOVE 'COMP    '    TO WS-ALT-COVERAGE-1
                 ELSE
                    MOVE 'COMP    '    TO WS-PREF-COVERAGE
                    MOVE 'COLL    '    TO WS-ALT-COVERAGE-1
                 END-IF
              WHEN FT-LT-PROPERTY(FT-LT-IX)
                 MOVE 'PROP'           TO WS-INC-TYPE
                 IF WI-LOSS-CAUSE = 'THEFT'
                    MOVE 'CONTENTS'    TO WS-EXP-TYPE
                    MOVE 'CONTENTS'    TO WS-PREF-COVERAGE
                 ELSE
                    MOVE 'PROPDAMA'    TO WS-EXP-TYPE
                    IF CLA-CL-POLICY-TYPE = 'C'
                       MOVE 'BLDG    ' TO WS-PREF-COVERAGE
                       MOVE 'DWELLING' TO WS-ALT-COVERAGE-1
                    ELSE
                       MOVE 'DWELLING' TO WS-PREF-COVERAGE
                       MOVE 'BLDG    ' TO WS-ALT-COVERAGE-1
                    END-IF
                 END-IF
              WHEN OTHER
                 MOVE 'PROP'           TO WS-INC-TYPE
                 MOVE 'PROPDAMA'       TO WS-EXP-TYPE
                 MOVE 'LIAB_PD '       TO WS-PREF-COVERAGE
           END-EVALUATE
           IF WS-ALT-COVERAGE-1 = SPACES
              MOVE WS-PREF-COVERAGE    TO WS-ALT-COVERAGE-1
           END-IF
           MOVE WS-ALT-COVERAGE-1      TO WS-ALT-COVERAGE-2
           MOVE CLA-CL-SEVERITY        TO WS-SEVERITY
           IF WS-SEVERITY < 1
              MOVE 1                   TO WS-SEVERITY
           END-IF
           IF WS-SEVERITY > 5
              MOVE 5                   TO WS-SEVERITY
           END-IF.

      ******************************************************************
      * BR-CLM-FNB-006 Table reserve by exposure type and severity; a
      * higher reporter estimate replaces it, capped at three times the
      * table figure and at the coverage limit.
       5200-CALC-INITIAL-RESERVE.
           MOVE 0                      TO WS-TABLE-RESERVE
           SET FT-RV-IX                TO 1
           SEARCH FT-RV
              AT END
                 MOVE 0                TO WS-TABLE-RESERVE
              WHEN FT-RV-EXP-TYPE(FT-RV-IX) = WS-EXP-TYPE
                 MOVE FT-RV-AMOUNT(FT-RV-IX, WS-SEVERITY)
                                       TO WS-TABLE-RESERVE
           END-SEARCH
           MOVE WS-TABLE-RESERVE       TO WS-INIT-RESERVE
           IF WI-EST-DAMAGE > WS-TABLE-RESERVE
              COMPUTE WS-RESERVE-CAP = WS-TABLE-RESERVE * 3
              IF WI-EST-DAMAGE > WS-RESERVE-CAP
                 MOVE WS-RESERVE-CAP   TO WS-INIT-RESERVE
                 MOVE 'Y'              TO WS-WARN-SW
                 MOVE 'RSVCAPPD'       TO WS-ERR-CODE
                 MOVE 'Estimate above auto-reserve cap - review'
                                       TO WS-ERR-TEXT
              ELSE
                 MOVE WI-EST-DAMAGE    TO WS-INIT-RESERVE
              END-IF
           END-IF
           IF WS-COV-LIMIT > 0 AND WS-INIT-RESERVE > WS-COV-LIMIT
              MOVE WS-COV-LIMIT        TO WS-INIT-RESERVE
           END-IF.

      ******************************************************************
       8100-CALL-CLAIM-NEW.
           INITIALIZE CLA-BODY
           MOVE STH-HEADER             TO CLA-HEADER
           MOVE 'CLMNEW  '             TO CLA-REQUEST-ID
           MOVE WI-POLICY-NO-N         TO CLA-CL-GENAPP-POL-NUM
           MOVE WI-LOSS-TYPE           TO CLA-CL-LOSS-TYPE
           MOVE WI-LOSS-DATE           TO CLA-CL-LOSS-DATE
           MOVE WI-LOSS-TIME           TO CLA-CL-LOSS-TIME
           MOVE WS-BUS-DATE            TO CLA-CL-REPORTED-DATE
           MOVE WI-SOURCE              TO CLA-CL-HOW-REPORTED
           MOVE WI-LOSS-CAUSE          TO CLA-CL-LOSS-CAUSE
           MOVE WI-LOSS-DESC           TO CLA-CL-LOSS-DESC
           MOVE WI-LOSS-ADDR1          TO CLA-CL-LOSS-ADDR1
           MOVE WI-LOSS-POSTCODE       TO CLA-CL-LOSS-POSTCODE
           MOVE WI-RPT-FIRST-NAME      TO CLA-CL-RPT-FIRST-NAME
           MOVE WI-RPT-LAST-NAME       TO CLA-CL-RPT-LAST-NAME
           MOVE WI-RPT-PHONE           TO CLA-CL-RPT-PHONE
           MOVE WI-RPT-RELATION        TO CLA-CL-RPT-RELATION
           CALL WS-CLM-PGM USING WS-CLM-CA
           IF NOT CLA-RC-OK AND NOT CLA-RC-WARNING
              MOVE CLA-RETURN-CODE     TO WS-SVC-RC
              MOVE CLA-REASON-CODE     TO WS-SVC-REASON
              MOVE CLA-MESSAGE         TO WS-SVC-MESSAGE
              PERFORM 8900-SERVICE-FAILED
              MOVE 0                   TO CLA-CL-CLAIM-ID
           END-IF.

      ******************************************************************
       8200-CALL-INCIDENT-NEW.
           INITIALIZE INA-BODY
           MOVE STH-HEADER             TO INA-HEADER
           MOVE 'INCNEW  '             TO INA-REQUEST-ID
           MOVE CLA-CL-CLAIM-ID        TO INA-IN-CLAIM-ID
           MOVE WS-INC-TYPE            TO INA-IN-TYPE
           MOVE WS-RU-SEQ              TO INA-IN-RISK-UNIT-SEQ
           MOVE WI-LOSS-DESC(1:200)    TO INA-IN-DESCRIPTION
           MOVE WS-SEVERITY            TO INA-IN-SEVERITY
           MOVE WI-VEHICLE-REG         TO INA-IN-VEHICLE-REG
           MOVE 'Y'                    TO INA-IN-DRIVEABLE
           MOVE WI-EST-DAMAGE          TO INA-IN-EST-DAMAGE
           MOVE 'N'                    TO INA-IN-TOTAL-LOSS
           IF WI-INJURY-YES
              MOVE 'UNSPEC  '          TO INA-IN-INJURY-TYPE
              MOVE WS-INJURED-ID       TO INA-IN-INJURED-ID
           END-IF
           CALL WS-INC-PGM USING WS-INC-CA
           IF NOT INA-RC-OK AND NOT INA-RC-WARNING
              MOVE INA-RETURN-CODE     TO WS-SVC-RC
              MOVE INA-REASON-CODE     TO WS-SVC-REASON
              MOVE INA-MESSAGE         TO WS-SVC-MESSAGE
              PERFORM 8900-SERVICE-FAILED
           ELSE
              MOVE INA-IN-INCIDENT-ID  TO WS-INCIDENT-ID
           END-IF.

      ******************************************************************
       8300-CALL-EXPOSURE-NEW.
           INITIALIZE EXA-BODY
           MOVE STH-HEADER             TO EXA-HEADER
           MOVE 'EXPNEW  '             TO EXA-REQUEST-ID
           MOVE CLA-CL-CLAIM-ID        TO EXA-EX-CLAIM-ID
           MOVE WS-EXP-TYPE            TO EXA-EX-TYPE
           MOVE WS-COV-TYPE            TO EXA-EX-COVERAGE-TYPE
           MOVE WS-COV-SEQ             TO EXA-EX-COV-SEQ
           MOVE WS-CLAIMANT-ID         TO EXA-EX-CLAIMANT-ID
           MOVE WS-INCIDENT-ID         TO EXA-EX-INCIDENT-ID
           CALL WS-EXP-PGM USING WS-EXP-CA
           IF NOT EXA-RC-OK AND NOT EXA-RC-WARNING
              MOVE EXA-RETURN-CODE     TO WS-SVC-RC
              MOVE EXA-REASON-CODE     TO WS-SVC-REASON
              MOVE EXA-MESSAGE         TO WS-SVC-MESSAGE
              PERFORM 8900-SERVICE-FAILED
           ELSE
              MOVE EXA-EX-EXPOSURE-ID  TO WS-EXPOSURE-ID
              IF EXA-EX-COV-LIMIT > 0
                 MOVE EXA-EX-COV-LIMIT TO WS-COV-LIMIT
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-FNB-008 Over-authority reserve: CLMFINTX records the txn
      * as PAPR and returns RC 04 APRVREQD - the claim stands, the
      * reserve waits for a supervisor.
       8400-CALL-RESERVE-SET.
           INITIALIZE FTA-BODY
           MOVE STH-HEADER             TO FTA-HEADER
           MOVE 'RSVSET  '             TO FTA-REQUEST-ID
           MOVE CLA-CL-CLAIM-ID        TO FTA-FT-CLAIM-ID
           MOVE WS-EXPOSURE-ID         TO FTA-FT-EXPOSURE-ID
           MOVE 'CLMC'                 TO FTA-FT-COST-TYPE
           MOVE 'DEFAULT '             TO FTA-FT-COST-CATEGORY
           MOVE WS-INIT-RESERVE        TO FTA-FT-AMOUNT
           MOVE 'FNOLINIT'             TO FTA-FT-REASON
           CALL WS-FIN-PGM USING WS-FIN-CA
           EVALUATE TRUE
              WHEN FTA-RC-OK
                 CONTINUE
              WHEN FTA-RC-WARNING
                 MOVE 'Y'              TO WS-WARN-SW
                 IF FTA-REASON-CODE = 'APRVREQD'
                    MOVE 'RSVAPRV '    TO WS-ERR-CODE
                    MOVE 'Initial reserve pending supervisor approval'
                                       TO WS-ERR-TEXT
                 ELSE
                    MOVE FTA-REASON-CODE TO WS-ERR-CODE
                    MOVE FTA-MESSAGE   TO WS-ERR-TEXT
                 END-IF
              WHEN OTHER
                 MOVE FTA-RETURN-CODE  TO WS-SVC-RC
                 MOVE FTA-REASON-CODE  TO WS-SVC-REASON
                 MOVE FTA-MESSAGE      TO WS-SVC-MESSAGE
                 PERFORM 8900-SERVICE-FAILED
           END-EVALUATE.

      ******************************************************************
      * A callee RC >= 90 ends the run; lower RCs fail this intake only
       8900-SERVICE-FAILED.
           IF WS-SVC-RC >= 90
              STRING 'Service failure ' WS-SVC-REASON ' '
                     WS-SVC-MESSAGE DELIMITED BY SIZE
                     INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE WS-SVC-REASON          TO WS-ERR-CODE
           MOVE WS-SVC-MESSAGE         TO WS-ERR-TEXT
           SET WS-REC-FAILED           TO TRUE.

      ******************************************************************
       6000-READ-FEED.
           READ FNOL-IN
              AT END
                 SET WS-EOF            TO TRUE
              NOT AT END
                 ADD 1                 TO WS-FEED-RECORDS
           END-READ
           IF NOT WS-FS-IN-OK AND NOT WS-FS-IN-EOF
              MOVE 'FNOLIN read error' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       6050-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO FP-H1-PAGE
           WRITE RPT-RECORD FROM FP-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM FP-HEAD-2 AFTER ADVANCING 2 LINES
           MOVE SPACES                 TO RPT-RECORD
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           MOVE 4                      TO WS-LINE-COUNT.

      ******************************************************************
       6100-PRINT-LINE.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              MOVE RPT-RECORD          TO WS-HOLD-LINE
              PERFORM 6050-PRINT-HEADINGS
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT
           MOVE SPACES                 TO FP-M-TEXT.

      ******************************************************************
       6200-WRITE-REJECT.
           MOVE FNI-INTAKE-ID          TO FR-INTAKE-ID
           MOVE WS-BUS-DATE            TO FR-BUS-DATE
           MOVE WS-ERR-CODE            TO FR-ERROR-CODE
           MOVE WS-ERR-TEXT            TO FR-ERROR-TEXT
           MOVE WI-RECORD              TO FR-RAW-RECORD
           WRITE FR-RECORD
           IF WS-FS-REJ NOT = '00'
              MOVE 'FNOLREJ write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       6400-PRINT-DETAIL.
           MOVE FNI-INTAKE-ID          TO FP-D-INTAKE-ID
           MOVE WI-SOURCE              TO FP-D-SOURCE
           MOVE WI-POLICY-NO           TO FP-D-POLICY
           MOVE WI-LOSS-DATE           TO FP-D-LOSS-DATE
           MOVE WI-LOSS-TYPE           TO FP-D-LOSS-TYPE
           MOVE CLA-CL-CLAIM-NUMBER    TO FP-D-CLAIM-NUMBER
           MOVE WS-EXP-TYPE            TO FP-D-EXP-TYPE
           MOVE WS-SEVERITY            TO FP-D-SEVERITY
           MOVE WS-INIT-RESERVE        TO FP-D-RESERVE
           MOVE WS-INTAKE-STATUS       TO FP-D-STATUS
           MOVE WS-ERR-CODE            TO FP-D-ERROR-CODE
           MOVE WS-ERR-TEXT            TO FP-D-MESSAGE
           MOVE FP-DETAIL              TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO FP-M-TEXT
           MOVE FP-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'Feed records read (incl header/trailer)'
                                       TO FP-T-LABEL
           MOVE WS-FEED-RECORDS        TO FP-T-COUNT
           MOVE WS-FEED-DAMAGE         TO FP-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Detail records skipped on restart' TO FP-T-LABEL
           MOVE WS-SKIPPED             TO FP-T-COUNT
           MOVE 0                      TO FP-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Intake rows staged this run' TO FP-T-LABEL
           MOVE WS-STAGED              TO FP-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Intake rows processed / est damage'
                                       TO FP-T-LABEL
           MOVE WS-PROCESSED           TO FP-T-COUNT
           MOVE WS-TOTAL-EST-DAMAGE    TO FP-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Claims created / initial reserves' TO FP-T-LABEL
           MOVE WS-CLAIMS-CREATED      TO FP-T-COUNT
           MOVE WS-TOTAL-RESERVE       TO FP-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Created with warnings' TO FP-T-LABEL
           MOVE WS-WARNINGS            TO FP-T-COUNT
           MOVE 0                      TO FP-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Rejected on validation' TO FP-T-LABEL
           MOVE WS-REJECTS             TO FP-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Failed in service / rolled back' TO FP-T-LABEL
           MOVE WS-FAILURES            TO FP-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Return code'          TO FP-T-LABEL
           MOVE WS-RETURN-CODE         TO FP-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE FP-TOTAL-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       9000-TERMINATE.
           PERFORM 4420-CLOSE-CURSOR
           IF WS-RETURN-CODE >= 12 AND WS-PROCESSED = 0
              PERFORM 6500-PRINT-TOTALS
           END-IF
           CLOSE REJECT-OUT CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9100-SET-RC-8.
           IF WS-RETURN-CODE < 8
              MOVE 8                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
       9110-SET-RC-4.
           IF WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF.

      ******************************************************************
      * Abend path: back out the open unit of work, report and stop.
      * The checkpoint trail still holds the last committed position.
       9800-ABEND.
           EXEC SQL ROLLBACK END-EXEC
           MOVE 'N'                    TO WS-CURSOR-SW
           IF WS-RETURN-CODE < 16
              MOVE 16                  TO WS-RETURN-CODE
           END-IF
           STRING '*** RUN ABANDONED: ' WS-ABEND-TEXT
                  DELIMITED BY SIZE INTO FP-M-TEXT
           MOVE FP-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           DISPLAY 'CLMFNLBT ' WS-ABEND-TEXT
           PERFORM 6500-PRINT-TOTALS
           CLOSE REJECT-OUT CHKPT-FILE RPT-OUT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           STOP RUN.

      ******************************************************************
      * Roll back first, then log, then commit so the log row survives
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
           STRING 'CLMFNLBT Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
