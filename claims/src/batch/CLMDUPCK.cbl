       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMDUPCK.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMDUPCK                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Duplicate claim detection across the book. Pairs *
      *               an OPEN, not yet suspected claim with any earlier*
      *               claim on the same policy, the same insured or    *
      *               the same vehicle registration, of the same loss  *
      *               type, with loss dates within 7 days and a        *
      *               similar cause. The later claim is flagged        *
      *               DUP_SUSPECT_FLAG, written to history and re-     *
      *               scored by SIU; every pair is reported.           *
      * FILES       : CHKPT   - checkpoint / restart (CLMDUPCK, 80)    *
      *               SYSIN   - control card (CLMDUPCC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : later-claim ids above DK-LAST-CLAIM-ID; commits  *
      *               only fall between later claims                   *
      * CALLS       : CLMSIUSC SIUSCR  CLMHIST0 HISADD                 *
      *               CLMDATES DTADDD  CLMERRLG ERRLOG                 *
      * TABLES      : CLM_CLAIM (R U)  CLM_INCIDENT (R)                *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-DUP-001 Match basis: same policy, else    *
      *                              same insured (customer) on a      *
      *                              different policy, else same       *
      *                              vehicle registration on an        *
      *                              incident                          *
      *               BR-CLM-DUP-002 Same loss type and loss dates no  *
      *                              more than 7 days apart            *
      *               BR-CLM-DUP-003 Similar cause: identical cause or *
      *                              the same cause family (CLMDUPCF)  *
      *               BR-CLM-DUP-004 Later = reported later (claim id  *
      *                              breaks a tie); only the later     *
      *                              claim is flagged, and only while  *
      *                              OPEN and not already suspected    *
      *               BR-CLM-DUP-005 Flagging bumps VERSION_NO and is  *
      *                              guarded on the flag still N       *
      *               BR-CLM-DUP-006 A flagged claim is re-scored by   *
      *                              SIU so the duplicate weighs in    *
      *                              the referral decision             *
      * RETURN CODE : 0 ok, 4 suspects flagged or SIU warning,         *
      *               16 Db2 or service failure                        *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       FD  CHKPT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMDUPCK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMDUPCC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMDUPCK'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-SIU-PGM                  PIC X(8)  VALUE 'CLMSIUSC'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-MAX-DAYS-APART           PIC S9(4) COMP VALUE 7.

       01  WS-FILE-STATUS.
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-LATER-SW              PIC X     VALUE 'N'.
              88 WS-LATER-FLAGGED       VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-FROM-DATE             PIC X(10) VALUE '0001-01-01'.
           05 WS-LOOKBACK-DAYS         PIC S9(5) COMP-3 VALUE 0.
           05 WS-COMMIT-FREQ           PIC S9(5) COMP-3 VALUE 100.
           05 WS-RESTART-ID            PIC S9(9) COMP   VALUE 0.
           05 WS-CURRENT-LATER-ID      PIC S9(9) COMP   VALUE 0.
           05 WS-UNCOMMITTED           PIC S9(5) COMP-3 VALUE 0.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-PAIRS-READ            PIC S9(9) COMP-3 VALUE 0.
           05 WS-PAIRS-SIMILAR         PIC S9(9) COMP-3 VALUE 0.
           05 WS-PAIRS-DISSIMILAR      PIC S9(9) COMP-3 VALUE 0.
           05 WS-LATER-CLAIMS          PIC S9(9) COMP-3 VALUE 0.
           05 WS-FLAGGED               PIC S9(9) COMP-3 VALUE 0.
           05 WS-STALE                 PIC S9(9) COMP-3 VALUE 0.
           05 WS-SIU-REFERRED          PIC S9(9) COMP-3 VALUE 0.
           05 WS-SIU-WARNINGS          PIC S9(9) COMP-3 VALUE 0.
           05 WS-BASIS-POL             PIC S9(9) COMP-3 VALUE 0.
           05 WS-BASIS-INSD            PIC S9(9) COMP-3 VALUE 0.
           05 WS-BASIS-VREG            PIC S9(9) COMP-3 VALUE 0.

      * One candidate pair as fetched from PAIR-CSR
       01  WS-PAIR.
           05 WS-P-LATER-ID            PIC S9(9) COMP.
           05 WS-P-LATER-NUMBER        PIC X(12).
           05 WS-P-LATER-LOSS-DATE     PIC X(10).
           05 WS-P-LATER-CAUSE         PIC X(10).
           05 WS-P-LATER-VERSION       PIC S9(9) COMP.
           05 WS-P-EARLIER-ID          PIC S9(9) COMP.
           05 WS-P-EARLIER-NUMBER      PIC X(12).
           05 WS-P-EARLIER-LOSS-DATE   PIC X(10).
           05 WS-P-EARLIER-CAUSE       PIC X(10).
           05 WS-P-BASIS               PIC X(4).
           05 WS-P-DAYS-APART          PIC S9(9) COMP.

       01  WS-ROW-WORK.
           05 WS-LATER-FAMILY          PIC X(4).
           05 WS-EARLIER-FAMILY        PIC X(4).
           05 WS-FAMILY                PIC X(4).
           05 WS-CAUSE                 PIC X(10).
           05 WS-ACTION                PIC X(10).
           05 WS-NOTE                  PIC X(32).
           05 WS-SIU-SCORE             PIC S9(4) COMP.
           05 WS-SIU-REF-TEXT          PIC X(3).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).
           05 WS-DISP-DAYS             PIC Z9.

           COPY CLMDUPCK REPLACING LEADING ==DK-== BY ==WK-==.

           COPY CLMDUPCC REPLACING LEADING ==DC-== BY ==WC-==.

           COPY CLMDUPCF.

           COPY CLMDUPRP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

      * BR-CLM-DUP-001 / 002 / 004 candidate pairs, later claim first
           EXEC SQL DECLARE PAIR-CSR CURSOR WITH HOLD FOR
              SELECT B.CLAIM_ID, B.CLAIM_NUMBER, B.LOSS_DATE,
                     B.LOSS_CAUSE, B.VERSION_NO,
                     A.CLAIM_ID, A.CLAIM_NUMBER, A.LOSS_DATE,
                     A.LOSS_CAUSE, 'POL ',
                     ABS(DAYS(B.LOSS_DATE) - DAYS(A.LOSS_DATE))
                FROM CLMDB.CLM_CLAIM B
                     INNER JOIN CLMDB.CLM_CLAIM A
                        ON A.GENAPP_POL_NUM = B.GENAPP_POL_NUM
                       AND A.LOSS_TYPE      = B.LOSS_TYPE
                       AND A.CLAIM_ID      <> B.CLAIM_ID
               WHERE B.CLAIM_STATE      = 'OPEN'
                 AND B.DUP_SUSPECT_FLAG = 'N'
                 AND B.REPORTED_DATE   >= :WS-FROM-DATE
                 AND B.CLAIM_ID         > :WS-RESTART-ID
                 AND (A.REPORTED_DATE < B.REPORTED_DATE
                      OR (A.REPORTED_DATE = B.REPORTED_DATE
                          AND A.CLAIM_ID < B.CLAIM_ID))
                 AND ABS(DAYS(B.LOSS_DATE) - DAYS(A.LOSS_DATE))
                     <= :WS-MAX-DAYS-APART
              UNION ALL
              SELECT B.CLAIM_ID, B.CLAIM_NUMBER, B.LOSS_DATE,
                     B.LOSS_CAUSE, B.VERSION_NO,
                     A.CLAIM_ID, A.CLAIM_NUMBER, A.LOSS_DATE,
                     A.LOSS_CAUSE, 'INSD',
                     ABS(DAYS(B.LOSS_DATE) - DAYS(A.LOSS_DATE))
                FROM CLMDB.CLM_CLAIM B
                     INNER JOIN CLMDB.CLM_CLAIM A
                        ON A.GENAPP_CUST_NUM = B.GENAPP_CUST_NUM
                       AND A.GENAPP_POL_NUM <> B.GENAPP_POL_NUM
                       AND A.LOSS_TYPE       = B.LOSS_TYPE
               WHERE B.CLAIM_STATE      = 'OPEN'
                 AND B.DUP_SUSPECT_FLAG = 'N'
                 AND B.REPORTED_DATE   >= :WS-FROM-DATE
                 AND B.CLAIM_ID         > :WS-RESTART-ID
                 AND (A.REPORTED_DATE < B.REPORTED_DATE
                      OR (A.REPORTED_DATE = B.REPORTED_DATE
                          AND A.CLAIM_ID < B.CLAIM_ID))
                 AND ABS(DAYS(B.LOSS_DATE) - DAYS(A.LOSS_DATE))
                     <= :WS-MAX-DAYS-APART
              UNION ALL
              SELECT DISTINCT B.CLAIM_ID, B.CLAIM_NUMBER, B.LOSS_DATE,
                     B.LOSS_CAUSE, B.VERSION_NO,
                     A.CLAIM_ID, A.CLAIM_NUMBER, A.LOSS_DATE,
                     A.LOSS_CAUSE, 'VREG',
                     ABS(DAYS(B.LOSS_DATE) - DAYS(A.LOSS_DATE))
                FROM CLMDB.CLM_CLAIM B
                     INNER JOIN CLMDB.CLM_INCIDENT IB
                        ON IB.CLAIM_ID = B.CLAIM_ID
                     INNER JOIN CLMDB.CLM_INCIDENT IA
                        ON IA.VEHICLE_REG = IB.VEHICLE_REG
                       AND IA.CLAIM_ID   <> IB.CLAIM_ID
                     INNER JOIN CLMDB.CLM_CLAIM A
                        ON A.CLAIM_ID         = IA.CLAIM_ID
                       AND A.GENAPP_POL_NUM  <> B.GENAPP_POL_NUM
                       AND A.GENAPP_CUST_NUM <> B.GENAPP_CUST_NUM
                       AND A.LOSS_TYPE        = B.LOSS_TYPE
               WHERE B.CLAIM_STATE      = 'OPEN'
                 AND B.DUP_SUSPECT_FLAG = 'N'
                 AND B.REPORTED_DATE   >= :WS-FROM-DATE
                 AND B.CLAIM_ID         > :WS-RESTART-ID
                 AND IB.VEHICLE_REG    <> ' '
                 AND (A.REPORTED_DATE < B.REPORTED_DATE
                      OR (A.REPORTED_DATE = B.REPORTED_DATE
                          AND A.CLAIM_ID < B.CLAIM_ID))
                 AND ABS(DAYS(B.LOSS_DATE) - DAYS(A.LOSS_DATE))
                     <= :WS-MAX-DAYS-APART
               ORDER BY 1, 6
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.
       01  WS-SIU-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SIA-==.
           03 SIA-BODY.
              COPY CLMCSIU REPLACING LEADING ==CA-== BY ==SIA-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
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
           PERFORM 2000-SCAN-PAIRS
           PERFORM 2900-FINISH
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMDUPCK RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO DR-M-TEXT WS-ABEND-TEXT
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
           IF WC-LOOKBACK-DAYS-X IS NUMERIC
              MOVE WC-LOOKBACK-DAYS    TO WS-LOOKBACK-DAYS
           END-IF
           PERFORM 4000-GET-BUSINESS-DATE
           MOVE WS-BUS-DATE            TO DR-H1-BUS-DATE
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM
           IF WS-LOOKBACK-DAYS > 0
              MOVE STH-HEADER          TO DTA-HEADER
              MOVE 'DTADDD  '          TO DTA-REQUEST-ID
              MOVE WS-BUS-DATE         TO DTA-DT-DATE-1
              COMPUTE DTA-DT-NUMBER = 0 - WS-LOOKBACK-DAYS
              CALL WS-DTE-PGM USING WS-DTE-CA
              IF NOT DTA-RC-OK
                 STRING 'CLMDATES DTADDD failed ' DTA-REASON-CODE
                        DELIMITED BY SIZE INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
              MOVE DTA-DT-RESULT-DATE  TO WS-FROM-DATE
           END-IF
           STRING 'Later claims reported on or after ' WS-FROM-DATE
                  ', loss dates within 7 days, same loss type'
                  DELIMITED BY SIZE INTO DR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
           ELSE
              OPEN INPUT CHKPT-FILE
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE DK-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              IF NOT WK-IN-FLIGHT
                 MOVE 'Restart requested but no in-flight checkpoint'
                                       TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
              MOVE WK-LAST-CLAIM-ID    TO WS-RESTART-ID
              MOVE WK-FLAGGED          TO WS-FLAGGED
              MOVE WK-PAIRS            TO WS-PAIRS-SIMILAR
              OPEN EXTEND CHKPT-FILE
              MOVE WK-LAST-CLAIM-ID    TO WS-DISP-ID
              STRING 'Restart after later claim id ' WS-DISP-ID
                     DELIMITED BY SIZE INTO DR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
       2000-SCAN-PAIRS.
           EXEC SQL OPEN PAIR-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-SCAN-PAIRS'   TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-PAIR
           PERFORM UNTIL WS-EOF
              IF WS-P-LATER-ID NOT = WS-CURRENT-LATER-ID
                 PERFORM 2050-NEW-LATER-CLAIM
              END-IF
              ADD 1                    TO WS-PAIRS-READ
              PERFORM 3000-CHECK-SIMILAR-CAUSE
              IF WS-FAMILY NOT = SPACES
                 PERFORM 2100-PROCESS-PAIR
              ELSE
                 ADD 1                 TO WS-PAIRS-DISSIMILAR
              END-IF
              PERFORM 4100-FETCH-PAIR
           END-PERFORM
           EXEC SQL CLOSE PAIR-CSR END-EXEC.

      ******************************************************************
      * Commit only between later claims so a restart never splits the
      * pairs of one claim across two runs.
       2050-NEW-LATER-CLAIM.
           IF WS-CURRENT-LATER-ID > 0
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 SET WK-IN-FLIGHT      TO TRUE
                 PERFORM 4900-COMMIT-CHECKPOINT
              END-IF
           END-IF
           MOVE WS-P-LATER-ID          TO WS-CURRENT-LATER-ID
           MOVE 'N'                    TO WS-LATER-SW
           ADD 1                       TO WS-LATER-CLAIMS.

      ******************************************************************
       2100-PROCESS-PAIR.
           ADD 1                       TO WS-PAIRS-SIMILAR
           EVALUATE WS-P-BASIS
              WHEN 'POL '
                 ADD 1                 TO WS-BASIS-POL
              WHEN 'INSD'
                 ADD 1                 TO WS-BASIS-INSD
              WHEN OTHER
                 ADD 1                 TO WS-BASIS-VREG
           END-EVALUATE
           MOVE SPACES                 TO WS-NOTE WS-SIU-REF-TEXT
           MOVE 0                      TO WS-SIU-SCORE
           IF WS-LATER-FLAGGED
              MOVE 'ALSO MATCH'        TO WS-ACTION
              MOVE 'Further earlier claim matched' TO WS-NOTE
           ELSE
              PERFORM 4200-FLAG-LATER-CLAIM
              IF SQLERRD(3) = 0
                 ADD 1                 TO WS-STALE
                 MOVE 'NOT FLAGD'      TO WS-ACTION
                 MOVE 'Changed since read - next run'
                                       TO WS-NOTE
              ELSE
                 ADD 1                 TO WS-FLAGGED
                 MOVE 'Y'              TO WS-LATER-SW
                 MOVE 'FLAGGED'        TO WS-ACTION
                 PERFORM 8200-WRITE-HISTORY
                 PERFORM 8100-SIU-RESCORE
              END-IF
           END-IF
           PERFORM 6400-PRINT-PAIR.

      ******************************************************************
       2900-FINISH.
           SET WK-ENDED                TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           IF (WS-FLAGGED > 0 OR WS-SIU-WARNINGS > 0)
              AND WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF
           PERFORM 6500-PRINT-TOTALS.

      ******************************************************************
      * BR-CLM-DUP-003 Identical cause, or both causes in one family
       3000-CHECK-SIMILAR-CAUSE.
           MOVE SPACES                 TO WS-FAMILY
           IF WS-P-LATER-CAUSE = WS-P-EARLIER-CAUSE
              MOVE 'SAME'              TO WS-FAMILY
           ELSE
              MOVE WS-P-LATER-CAUSE    TO WS-CAUSE
              PERFORM 3100-FIND-FAMILY
              MOVE WS-FAMILY           TO WS-LATER-FAMILY
              MOVE WS-P-EARLIER-CAUSE  TO WS-CAUSE
              PERFORM 3100-FIND-FAMILY
              MOVE WS-FAMILY           TO WS-EARLIER-FAMILY
              IF WS-LATER-FAMILY = SPACES
                 OR WS-LATER-FAMILY NOT = WS-EARLIER-FAMILY
                 MOVE SPACES           TO WS-FAMILY
              END-IF
           END-IF.

      ******************************************************************
       3100-FIND-FAMILY.
           MOVE SPACES                 TO WS-FAMILY
           SET DF-CF-IX                TO 1
           SEARCH DF-CF
              AT END
                 CONTINUE
              WHEN DF-CF-CAUSE(DF-CF-IX) = WS-CAUSE
                 MOVE DF-CF-FAMILY(DF-CF-IX) TO WS-FAMILY
           END-SEARCH.

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
       4100-FETCH-PAIR.
           EXEC SQL
              FETCH PAIR-CSR
               INTO :WS-P-LATER-ID, :WS-P-LATER-NUMBER,
                    :WS-P-LATER-LOSS-DATE, :WS-P-LATER-CAUSE,
                    :WS-P-LATER-VERSION,
                    :WS-P-EARLIER-ID, :WS-P-EARLIER-NUMBER,
                    :WS-P-EARLIER-LOSS-DATE, :WS-P-EARLIER-CAUSE,
                    :WS-P-BASIS, :WS-P-DAYS-APART
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-PAIR' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-DUP-005 Optimistic lock, still OPEN and unflagged
       4200-FLAG-LATER-CLAIM.
           MOVE WS-P-LATER-ID          TO CLM-CLAIM-ID
           MOVE WS-P-LATER-VERSION     TO CLM-VERSION-NO
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET DUP_SUSPECT_FLAG = 'Y',
                     UPDATE_TS        = CURRENT TIMESTAMP,
                     VERSION_NO       = VERSION_NO + 1
               WHERE CLAIM_ID         = :CLM-CLAIM-ID
                 AND VERSION_NO       = :CLM-VERSION-NO
                 AND CLAIM_STATE      = 'OPEN'
                 AND DUP_SUSPECT_FLAG = 'N'
           END-EXEC
           IF SQLCODE NOT = 0 AND NOT = 100
              MOVE '4200-FLAG-LATER-CLAIM' TO WS-ERR-PARAGRAPH
              MOVE WS-P-LATER-NUMBER   TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
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
           MOVE WS-CURRENT-LATER-ID    TO WK-LAST-CLAIM-ID
           MOVE WS-FLAGGED             TO WK-FLAGGED
           MOVE WS-PAIRS-SIMILAR       TO WK-PAIRS
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE DK-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-DUP-006 SIU re-score of the flagged claim
       8100-SIU-RESCORE.
           INITIALIZE SIA-BODY
           MOVE STH-HEADER             TO SIA-HEADER
           MOVE 'SIUSCR  '             TO SIA-REQUEST-ID
           MOVE WS-P-LATER-ID          TO SIA-SI-CLAIM-ID
           CALL WS-SIU-PGM USING WS-SIU-CA
           EVALUATE TRUE
              WHEN SIA-RC-OK
              WHEN SIA-RC-WARNING
                 MOVE SIA-SI-TOTAL-SCORE TO WS-SIU-SCORE
                 IF SIA-SI-REFERRED = 'Y'
                    ADD 1              TO WS-SIU-REFERRED
                    MOVE 'YES'         TO WS-SIU-REF-TEXT
                    MOVE 'Referred to SIU' TO WS-NOTE
                 ELSE
                    MOVE 'NO '         TO WS-SIU-REF-TEXT
                 END-IF
              WHEN SIA-RETURN-CODE >= 90
                 STRING 'CLMSIUSC SIUSCR failed ' SIA-REASON-CODE ' '
                        SIA-MESSAGE DELIMITED BY SIZE
                        INTO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              WHEN OTHER
                 ADD 1                 TO WS-SIU-WARNINGS
                 STRING 'SIU not scored ' SIA-REASON-CODE
                        DELIMITED BY SIZE INTO WS-NOTE
           END-EVALUATE.

      ******************************************************************
       8200-WRITE-HISTORY.
           INITIALIZE HIA-BODY
           MOVE STH-HEADER             TO HIA-HEADER
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE WS-P-LATER-ID          TO HIA-HI-CLAIM-ID
           MOVE 'DUPSUSP '             TO HIA-HI-EVENT-TYPE
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           MOVE WS-P-DAYS-APART        TO WS-DISP-DAYS
           STRING 'Possible duplicate of ' WS-P-EARLIER-NUMBER
                  ' (match ' WS-P-BASIS ', loss dates '
                  WS-DISP-DAYS ' days apart, causes '
                  WS-P-LATER-CAUSE '/' WS-P-EARLIER-CAUSE ')'
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
              MOVE WS-PAGE-NO          TO DR-H1-PAGE
              WRITE RPT-RECORD FROM DR-HEAD-1 AFTER ADVANCING PAGE
              WRITE RPT-RECORD FROM DR-HEAD-2 AFTER ADVANCING 2 LINES
              MOVE SPACES              TO RPT-RECORD
              WRITE RPT-RECORD AFTER ADVANCING 1 LINE
              MOVE 4                   TO WS-LINE-COUNT
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6200-PRINT-MESSAGE.
           MOVE DR-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                 TO DR-M-TEXT.

      ******************************************************************
       6400-PRINT-PAIR.
           MOVE WS-P-LATER-NUMBER      TO DR-D-LATER-CLAIM
           MOVE WS-P-LATER-LOSS-DATE   TO DR-D-LATER-DATE
           MOVE WS-P-LATER-CAUSE       TO DR-D-LATER-CAUSE
           MOVE WS-P-EARLIER-NUMBER    TO DR-D-EARLIER-CLAIM
           MOVE WS-P-EARLIER-LOSS-DATE TO DR-D-EARLIER-DATE
           MOVE WS-P-EARLIER-CAUSE     TO DR-D-EARLIER-CAUSE
           MOVE WS-P-BASIS             TO DR-D-BASIS
           MOVE WS-P-DAYS-APART        TO DR-D-DAYS
           MOVE WS-ACTION              TO DR-D-ACTION
           MOVE WS-SIU-SCORE           TO DR-D-SIU-SCORE
           MOVE WS-SIU-REF-TEXT        TO DR-D-REFERRED
           MOVE WS-NOTE                TO DR-D-NOTE
           MOVE DR-DETAIL              TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO DR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           MOVE 'Candidate pairs read' TO DR-T-LABEL
           MOVE WS-PAIRS-READ          TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Pairs dropped - cause not similar' TO DR-T-LABEL
           MOVE WS-PAIRS-DISSIMILAR    TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Duplicate pairs reported (cumulative)' TO DR-T-LABEL
           MOVE WS-PAIRS-SIMILAR       TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE '  matched on policy'  TO DR-T-LABEL
           MOVE WS-BASIS-POL           TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE '  matched on insured' TO DR-T-LABEL
           MOVE WS-BASIS-INSD          TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE '  matched on vehicle registration' TO DR-T-LABEL
           MOVE WS-BASIS-VREG          TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Later claims examined' TO DR-T-LABEL
           MOVE WS-LATER-CLAIMS        TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Claims flagged duplicate suspect (cumulative)'
                                       TO DR-T-LABEL
           MOVE WS-FLAGGED             TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Not flagged - changed since read' TO DR-T-LABEL
           MOVE WS-STALE               TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Referred to SIU after re-score' TO DR-T-LABEL
           MOVE WS-SIU-REFERRED        TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'SIU re-score not performed' TO DR-T-LABEL
           MOVE WS-SIU-WARNINGS        TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Return code'          TO DR-T-LABEL
           MOVE WS-RETURN-CODE         TO DR-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE DR-TOTAL-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       9000-TERMINATE.
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
                  DELIMITED BY SIZE INTO DR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           DISPLAY 'CLMDUPCK ' WS-ABEND-TEXT
           PERFORM 6500-PRINT-TOTALS
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
           STRING 'CLMDUPCK Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
