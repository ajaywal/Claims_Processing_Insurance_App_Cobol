       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCATAS.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMCATAS                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Catastrophe tagging. For every active declared   *
      *               catastrophe, OPEN claims with no CAT code whose  *
      *               loss date falls in the event window and whose    *
      *               loss postcode is in the event postcode area are  *
      *               tagged, written to history and re-segmented      *
      *               (CLMSEGMT). Counts and incurred are reported per *
      *               catastrophe.                                     *
      * FILES       : CHKPT   - checkpoint / restart (CLMCATCK, 80)    *
      *               SYSIN   - control card (CLMCATCC, 80)            *
      *               RPTOUT  - run report (132)                       *
      * RESTART     : at CT-CAT-CODE after CT-LAST-CLAIM-ID            *
      * CALLS       : CLMSEGMT SEGCLM  CLMHIST0 HISADD                 *
      *               CLMERRLG ERRLOG                                  *
      * TABLES      : CLM_CATASTROPHE (R)  CLM_CLAIM (R U)             *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-CAT-001 Only ACTIVE catastrophes already  *
      *                              started on the business date with *
      *                              a valid window are applied        *
      *               BR-CLM-CAT-002 Loss date inside the window,      *
      *                              both ends inclusive               *
      *               BR-CLM-CAT-003 Postcode area match is the area   *
      *                              letters followed by a digit, so   *
      *                              area B never takes BS postcodes   *
      *               BR-CLM-CAT-004 Only OPEN claims with a blank     *
      *                              CAT code; an existing tag is      *
      *                              never overwritten                 *
      *               BR-CLM-CAT-005 Tag under optimistic lock; a      *
      *                              stale claim is left for next run  *
      *               BR-CLM-CAT-006 Tagged claims are re-segmented    *
      *                              (CLMSEGMT stores the result) and  *
      *                              segment / severity moves reported *
      * RETURN CODE : 0 ok, 4 stale claims or invalid catastrophe      *
      *               window, 16 Db2 or service failure                *
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
           COPY CLMCATCK.
       FD  CTL-CARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
           COPY CLMCATCC.
       FD  RPT-OUT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  RPT-RECORD                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMCATAS'.
       01  WS-BATCH-USER               PIC X(8)  VALUE 'CLMBATCH'.
       01  WS-SEG-PGM                  PIC X(8)  VALUE 'CLMSEGMT'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-FS-CHK                PIC X(2).
              88 WS-FS-CHK-OK           VALUE '00'.
           05 WS-FS-CTL                PIC X(2).
           05 WS-FS-RPT                PIC X(2).

       01  WS-SWITCHES.
           05 WS-CAT-EOF-SW            PIC X     VALUE 'N'.
              88 WS-CAT-EOF             VALUE 'Y'.
           05 WS-CLM-EOF-SW            PIC X     VALUE 'N'.
              88 WS-CLM-EOF             VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-COMMIT-FREQ           PIC S9(5) COMP-3 VALUE 200.
           05 WS-RESTART-CAT           PIC X(8)  VALUE SPACES.
           05 WS-RESTART-CLAIM-ID      PIC S9(9) COMP   VALUE 0.
           05 WS-LAST-CLAIM-ID         PIC S9(9) COMP   VALUE 0.
           05 WS-UNCOMMITTED           PIC S9(5) COMP-3 VALUE 0.
           05 WS-RETURN-CODE           PIC S9(4) COMP   VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP   VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP   VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP   VALUE 55.
           05 WS-TIMESTAMP             PIC X(21).
           05 WS-HOLD-LINE             PIC X(132).

       01  WS-CONTROL-TOTALS.
           05 WS-CATS-READ             PIC S9(9) COMP-3 VALUE 0.
           05 WS-CATS-INVALID          PIC S9(9) COMP-3 VALUE 0.
           05 WS-CANDIDATES            PIC S9(9) COMP-3 VALUE 0.
           05 WS-AREA-MISMATCH         PIC S9(9) COMP-3 VALUE 0.
           05 WS-TAGGED                PIC S9(9) COMP-3 VALUE 0.
           05 WS-TAGGED-INCURRED       PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-STALE                 PIC S9(9) COMP-3 VALUE 0.
           05 WS-RESEGMENTED           PIC S9(9) COMP-3 VALUE 0.
           05 WS-CAT-TAGGED            PIC S9(9) COMP-3 VALUE 0.
           05 WS-CAT-TAGGED-INC        PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CAT-ALL-COUNT         PIC S9(9) COMP   VALUE 0.
           05 WS-CAT-ALL-INC           PIC S9(13)V99 COMP-3 VALUE 0.

       01  WS-ROW-WORK.
           05 WS-AREA-LEN              PIC S9(4) COMP.
           05 WS-AREA-PATTERN          PIC X(8).
           05 WS-NEXT-CHAR             PIC X.
           05 WS-OLD-SEGMENT           PIC X(8).
           05 WS-OLD-SEVERITY          PIC S9(4) COMP.
           05 WS-NOTE                  PIC X(46).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARAGRAPH         PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-ABEND-TEXT            PIC X(120).
           05 WS-DISP-ID               PIC 9(9).

           COPY CLMCATCK REPLACING LEADING ==CT-== BY ==WK-==.

           COPY CLMCATCC REPLACING LEADING ==TC-== BY ==WC-==.

           COPY CLMCATRP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCAT END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE CAT-CSR CURSOR WITH HOLD FOR
              SELECT CAT_CODE, CAT_NAME, CAT_TYPE, START_DATE,
                     END_DATE, POSTCODE_AREA
                FROM CLMDB.CLM_CATASTROPHE
               WHERE ACTIVE_FLAG = 'Y'
                 AND START_DATE <= :WS-BUS-DATE
                 AND CAT_CODE   >= :WS-RESTART-CAT
               ORDER BY CAT_CODE
                 FOR FETCH ONLY
           END-EXEC.

           EXEC SQL DECLARE CLM-CSR CURSOR WITH HOLD FOR
              SELECT CLAIM_ID, CLAIM_NUMBER, LOSS_DATE, LOSS_POSTCODE,
                     TOTAL_INCURRED, SEGMENT_CODE, SEVERITY,
                     VERSION_NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_STATE   = 'OPEN'
                 AND CAT_CODE      = ' '
                 AND LOSS_DATE BETWEEN :CAT-START-DATE
                                   AND :CAT-END-DATE
                 AND LOSS_POSTCODE LIKE :WS-AREA-PATTERN
                 AND CLAIM_ID      > :WS-RESTART-CLAIM-ID
               ORDER BY CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-STD-HDR.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==STH-==.
       01  WS-SEG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SGA-==.
           03 SGA-BODY.
              COPY CLMCSEG REPLACING LEADING ==CA-== BY ==SGA-==.
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
           PERFORM 2000-PROCESS-CATASTROPHES
           PERFORM 2900-FINISH
           PERFORM 9000-TERMINATE.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-OUT
           IF WS-FS-RPT NOT = '00'
              DISPLAY 'CLMCATAS RPTOUT OPEN FAILED ' WS-FS-RPT
              MOVE 16                  TO RETURN-CODE
              STOP RUN
           END-IF
           MOVE SPACES                 TO TR-M-TEXT WS-ABEND-TEXT
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
           MOVE WS-BUS-DATE            TO TR-H1-BUS-DATE
           INITIALIZE WS-STD-HDR
           MOVE WS-BATCH-USER          TO STH-USER-ID
           MOVE WS-BUS-DATE            TO STH-BUS-DATE
           MOVE WS-PROGRAM-ID          TO STH-CALLER-PGM
           IF NOT WC-MODE-RESTART
              OPEN OUTPUT CHKPT-FILE
           ELSE
              OPEN INPUT CHKPT-FILE
              MOVE SPACES              TO WK-RECORD
              PERFORM UNTIL NOT WS-FS-CHK-OK
                 READ CHKPT-FILE
                 IF WS-FS-CHK-OK
                    MOVE CT-RECORD     TO WK-RECORD
                 END-IF
              END-PERFORM
              CLOSE CHKPT-FILE
              IF NOT WK-IN-FLIGHT
                 MOVE 'Restart requested but no in-flight checkpoint'
                                       TO WS-ABEND-TEXT
                 PERFORM 9800-ABEND
              END-IF
              MOVE WK-CAT-CODE         TO WS-RESTART-CAT
              MOVE WK-LAST-CLAIM-ID    TO WS-RESTART-CLAIM-ID
              MOVE WK-TAGGED           TO WS-TAGGED
              OPEN EXTEND CHKPT-FILE
              MOVE WK-LAST-CLAIM-ID    TO WS-DISP-ID
              STRING 'Restart at catastrophe ' WK-CAT-CODE
                     ' after claim id ' WS-DISP-ID
                     DELIMITED BY SIZE INTO TR-M-TEXT
              PERFORM 6200-PRINT-MESSAGE
           END-IF
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT open failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-CAT-001 Active and started catastrophes (CAT-CSR)
       2000-PROCESS-CATASTROPHES.
           EXEC SQL OPEN CAT-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-PROCESS-CATASTROPHES' TO WS-ERR-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-CATASTROPHE
           PERFORM UNTIL WS-CAT-EOF
              ADD 1                    TO WS-CATS-READ
              PERFORM 6300-PRINT-CAT-HEADER
              IF CAT-END-DATE < CAT-START-DATE
                 OR CAT-POSTCODE-AREA = SPACES
                 OR CAT-POSTCODE-AREA(1:1) IS NOT ALPHABETIC
                 ADD 1                 TO WS-CATS-INVALID
                 MOVE 'Catastrophe window or postcode area invalid'
                      & ' - not applied'
                                       TO TR-M-TEXT
                 PERFORM 6200-PRINT-MESSAGE
              ELSE
                 PERFORM 2100-TAG-CLAIMS-FOR-CAT
              END-IF
              MOVE 0                   TO WS-RESTART-CLAIM-ID
              PERFORM 4100-FETCH-CATASTROPHE
           END-PERFORM
           EXEC SQL CLOSE CAT-CSR END-EXEC.

      ******************************************************************
       2100-TAG-CLAIMS-FOR-CAT.
           MOVE 0                      TO WS-CAT-TAGGED
                                          WS-CAT-TAGGED-INC
                                          WS-AREA-LEN
           INSPECT CAT-POSTCODE-AREA TALLYING WS-AREA-LEN
                   FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE SPACES                 TO WS-AREA-PATTERN
           STRING CAT-POSTCODE-AREA(1:WS-AREA-LEN) '%'
                  DELIMITED BY SIZE INTO WS-AREA-PATTERN
           EXEC SQL OPEN CLM-CSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2100-TAG-CLAIMS-FOR-CAT' TO WS-ERR-PARAGRAPH
              MOVE CAT-CAT-CODE        TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'N'                    TO WS-CLM-EOF-SW
           PERFORM 4200-FETCH-CLAIM
           PERFORM UNTIL WS-CLM-EOF
              ADD 1                    TO WS-CANDIDATES
              PERFORM 3000-CHECK-AREA-MATCH
              IF WS-NEXT-CHAR IS NUMERIC
                 PERFORM 2200-TAG-CLAIM
              ELSE
                 ADD 1                 TO WS-AREA-MISMATCH
              END-IF
              MOVE CLM-CLAIM-ID        TO WS-LAST-CLAIM-ID
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 SET WK-IN-FLIGHT      TO TRUE
                 PERFORM 4900-COMMIT-CHECKPOINT
              END-IF
              PERFORM 4200-FETCH-CLAIM
           END-PERFORM
           EXEC SQL CLOSE CLM-CSR END-EXEC
           MOVE 0                      TO WS-LAST-CLAIM-ID
           SET WK-IN-FLIGHT            TO TRUE
           PERFORM 4900-COMMIT-CHECKPOINT
           PERFORM 4400-SUM-CATASTROPHE
           PERFORM 6400-PRINT-CAT-SUMMARY.

      ******************************************************************
       2200-TAG-CLAIM.
           MOVE SPACES                 TO WS-NOTE
           MOVE CLM-SEGMENT-CODE       TO WS-OLD-SEGMENT
           MOVE CLM-SEVERITY           TO WS-OLD-SEVERITY
           PERFORM 4300-SET-CAT-CODE
           IF SQLERRD(3) = 0
              ADD 1                    TO WS-STALE
              MOVE 'Changed since read - left for next run'
                                       TO WS-NOTE
           ELSE
              ADD 1                    TO WS-TAGGED WS-CAT-TAGGED
              ADD CLM-TOTAL-INCURRED   TO WS-TAGGED-INCURRED
                                          WS-CAT-TAGGED-INC
              PERFORM 8200-WRITE-HISTORY
              PERFORM 8100-RESEGMENT
           END-IF
           MOVE CLM-CLAIM-NUMBER       TO TR-L-CLAIM-NUMBER
           MOVE CLM-LOSS-DATE          TO TR-L-LOSS-DATE
           MOVE CLM-LOSS-POSTCODE      TO TR-L-POSTCODE
           MOVE CLM-TOTAL-INCURRED     TO TR-L-INCURRED
           MOVE WS-OLD-SEGMENT         TO TR-L-OLD-SEGMENT
           MOVE CLM-SEGMENT-CODE       TO TR-L-NEW-SEGMENT
           MOVE WS-OLD-SEVERITY        TO TR-L-OLD-SEVERITY
           MOVE CLM-SEVERITY           TO TR-L-NEW-SEVERITY
           MOVE WS-NOTE                TO TR-L-NOTE
           MOVE TR-CLAIM-LINE          TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       2900-FINISH.
           SET WK-ENDED                TO TRUE
           MOVE SPACES                 TO CAT-CAT-CODE
           PERFORM 4900-COMMIT-CHECKPOINT
           IF (WS-STALE > 0 OR WS-CATS-INVALID > 0)
              AND WS-RETURN-CODE < 4
              MOVE 4                   TO WS-RETURN-CODE
           END-IF
           PERFORM 6500-PRINT-TOTALS.

      ******************************************************************
      * BR-CLM-CAT-003 The LIKE prefix is refined here: the character
      * after the area letters must be the district digit.
       3000-CHECK-AREA-MATCH.
           MOVE SPACE                  TO WS-NEXT-CHAR
           IF CLM-LOSS-POSTCODE-IND >= 0 AND WS-AREA-LEN < 8
              MOVE CLM-LOSS-POSTCODE(WS-AREA-LEN + 1:1)
                                       TO WS-NEXT-CHAR
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
       4100-FETCH-CATASTROPHE.
           EXEC SQL
              FETCH CAT-CSR
               INTO :CAT-CAT-CODE, :CAT-CAT-NAME, :CAT-CAT-TYPE,
                    :CAT-START-DATE, :CAT-END-DATE,
                    :CAT-POSTCODE-AREA
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CAT-CAT-NAME-LEN < 40
                    MOVE SPACES TO CAT-CAT-NAME-TEXT
                                   (CAT-CAT-NAME-LEN + 1:)
                 END-IF
              WHEN 100
                 SET WS-CAT-EOF        TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-CATASTROPHE' TO WS-ERR-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-CAT-002 / BR-CLM-CAT-004 window, OPEN, blank CAT code
       4200-FETCH-CLAIM.
           EXEC SQL
              FETCH CLM-CSR
               INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER, :CLM-LOSS-DATE,
                    :CLM-LOSS-POSTCODE :CLM-LOSS-POSTCODE-IND,
                    :CLM-TOTAL-INCURRED, :CLM-SEGMENT-CODE,
                    :CLM-SEVERITY, :CLM-VERSION-NO
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-CLM-EOF        TO TRUE
              WHEN OTHER
                 MOVE '4200-FETCH-CLAIM' TO WS-ERR-PARAGRAPH
                 MOVE CAT-CAT-CODE     TO WS-ERR-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-CAT-005 Optimistic lock; CAT_CODE re-checked blank
       4300-SET-CAT-CODE.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET CAT_CODE   = :CAT-CAT-CODE,
                     UPDATE_TS  = CURRENT TIMESTAMP,
                     VERSION_NO = VERSION_NO + 1
               WHERE CLAIM_ID   = :CLM-CLAIM-ID
                 AND VERSION_NO = :CLM-VERSION-NO
                 AND CAT_CODE   = ' '
           END-EXEC
           IF SQLCODE NOT = 0 AND NOT = 100
              MOVE '4300-SET-CAT-CODE' TO WS-ERR-PARAGRAPH
              MOVE CLM-CLAIM-NUMBER    TO WS-ERR-KEY
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Whole-of-event position: every claim carrying the code, any
      * state, including those tagged on earlier runs or by handlers.
       4400-SUM-CATASTROPHE.
           EXEC SQL
              SELECT COUNT(*), COALESCE(SUM(TOTAL_INCURRED), 0)
                INTO :WS-CAT-ALL-COUNT, :WS-CAT-ALL-INC
                FROM CLMDB.CLM_CLAIM
               WHERE CAT_CODE = :CAT-CAT-CODE
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4400-SUM-CATASTROPHE' TO WS-ERR-PARAGRAPH
              MOVE CAT-CAT-CODE        TO WS-ERR-KEY
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
           MOVE CAT-CAT-CODE           TO WK-CAT-CODE
           MOVE WS-LAST-CLAIM-ID       TO WK-LAST-CLAIM-ID
           MOVE WS-TAGGED              TO WK-TAGGED
           MOVE FUNCTION CURRENT-DATE  TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP           TO WK-TIMESTAMP
           WRITE CT-RECORD FROM WK-RECORD
           IF NOT WS-FS-CHK-OK
              MOVE 'CHKPT write failed' TO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF.

      ******************************************************************
      * BR-CLM-CAT-006 Re-segment: CLMSEGMT re-derives segment and
      * severity with the catastrophe now on the claim and stores them.
       8100-RESEGMENT.
           INITIALIZE SGA-BODY
           MOVE STH-HEADER             TO SGA-HEADER
           MOVE 'SEGCLM  '             TO SGA-REQUEST-ID
           MOVE CLM-CLAIM-ID           TO SGA-SG-CLAIM-ID
           CALL WS-SEG-PGM USING WS-SEG-CA
           IF NOT SGA-RC-OK AND NOT SGA-RC-WARNING
              STRING 'CLMSEGMT SEGCLM failed ' SGA-REASON-CODE ' '
                     SGA-MESSAGE DELIMITED BY SIZE INTO WS-ABEND-TEXT
              PERFORM 9800-ABEND
           END-IF
           MOVE SGA-SG-SEGMENT         TO CLM-SEGMENT-CODE
           MOVE SGA-SG-SEVERITY        TO CLM-SEVERITY
           IF CLM-SEGMENT-CODE NOT = WS-OLD-SEGMENT
              OR CLM-SEVERITY NOT = WS-OLD-SEVERITY
              ADD 1                    TO WS-RESEGMENTED
              MOVE 'Tagged and re-segmented' TO WS-NOTE
           ELSE
              MOVE 'Tagged, segment unchanged' TO WS-NOTE
           END-IF.

      ******************************************************************
       8200-WRITE-HISTORY.
           INITIALIZE HIA-BODY
           MOVE STH-HEADER             TO HIA-HEADER
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CLM-CLAIM-ID           TO HIA-HI-CLAIM-ID
           MOVE 'CATTAG  '             TO HIA-HI-EVENT-TYPE
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           STRING 'Tagged to catastrophe ' CAT-CAT-CODE ' '
                  CAT-CAT-NAME-TEXT ' (area ' CAT-POSTCODE-AREA ')'
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
              MOVE WS-PAGE-NO          TO TR-H1-PAGE
              WRITE RPT-RECORD FROM TR-HEAD-1 AFTER ADVANCING PAGE
              MOVE SPACES              TO RPT-RECORD
              WRITE RPT-RECORD AFTER ADVANCING 1 LINE
              MOVE 3                   TO WS-LINE-COUNT
              MOVE WS-HOLD-LINE        TO RPT-RECORD
           END-IF
           WRITE RPT-RECORD AFTER ADVANCING 1 LINE
           ADD 1                       TO WS-LINE-COUNT.

      ******************************************************************
       6200-PRINT-MESSAGE.
           MOVE TR-MESSAGE-LINE        TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE SPACES                 TO TR-M-TEXT.

      ******************************************************************
       6300-PRINT-CAT-HEADER.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE CAT-CAT-CODE           TO TR-C-CAT-CODE
           MOVE CAT-CAT-NAME-TEXT      TO TR-C-CAT-NAME
           MOVE CAT-CAT-TYPE           TO TR-C-CAT-TYPE
           MOVE CAT-POSTCODE-AREA      TO TR-C-AREA
           MOVE CAT-START-DATE         TO TR-C-START
           MOVE CAT-END-DATE           TO TR-C-END
           MOVE TR-CAT-LINE            TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6400-PRINT-CAT-SUMMARY.
           MOVE 'Claims tagged this run' TO TR-S-LABEL
           MOVE WS-CAT-TAGGED          TO TR-S-COUNT
           MOVE WS-CAT-TAGGED-INC      TO TR-S-AMOUNT
           MOVE TR-SUM-LINE            TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'All claims under this event' TO TR-S-LABEL
           MOVE WS-CAT-ALL-COUNT       TO TR-S-COUNT
           MOVE WS-CAT-ALL-INC         TO TR-S-AMOUNT
           MOVE TR-SUM-LINE            TO RPT-RECORD
           PERFORM 6100-PRINT-LINE.

      ******************************************************************
       6500-PRINT-TOTALS.
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6100-PRINT-LINE
           MOVE 'CONTROL TOTALS'       TO TR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           MOVE 0                      TO TR-T-AMOUNT
           MOVE 'Active catastrophes processed' TO TR-T-LABEL
           MOVE WS-CATS-READ           TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Catastrophes skipped - invalid definition'
                                       TO TR-T-LABEL
           MOVE WS-CATS-INVALID        TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Candidate claims (window + area prefix)'
                                       TO TR-T-LABEL
           MOVE WS-CANDIDATES          TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Rejected - postcode outside area' TO TR-T-LABEL
           MOVE WS-AREA-MISMATCH       TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Claims tagged (cumulative) / incurred this run'
                                       TO TR-T-LABEL
           MOVE WS-TAGGED              TO TR-T-COUNT
           MOVE WS-TAGGED-INCURRED     TO TR-T-AMOUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 0                      TO TR-T-AMOUNT
           MOVE 'Claims re-segmented'  TO TR-T-LABEL
           MOVE WS-RESEGMENTED         TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Skipped - stale version' TO TR-T-LABEL
           MOVE WS-STALE               TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL
           MOVE 'Return code'          TO TR-T-LABEL
           MOVE WS-RETURN-CODE         TO TR-T-COUNT
           PERFORM 6510-PRINT-TOTAL.

      ******************************************************************
       6510-PRINT-TOTAL.
           MOVE TR-TOTAL-LINE          TO RPT-RECORD
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
                  DELIMITED BY SIZE INTO TR-M-TEXT
           PERFORM 6200-PRINT-MESSAGE
           DISPLAY 'CLMCATAS ' WS-ABEND-TEXT
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
           STRING 'CLMCATAS Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARAGRAPH
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT            TO WS-ABEND-TEXT
           PERFORM 9800-ABEND.
