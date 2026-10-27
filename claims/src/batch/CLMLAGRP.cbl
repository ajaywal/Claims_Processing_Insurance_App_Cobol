       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMLAGRP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMLAGRP                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : C - batch driver (IKJEFT01 / DSN RUN)            *
      * FUNCTION    : Loss run and aging report.                       *
      *               Section 1 lists every open claim by segment and  *
      *               adjuster with its age bucket (days since         *
      *               reported), reserves, paid and incurred, with     *
      *               adjuster, segment and grand totals by bucket.    *
      *               Section 2 lists claims closed this month with    *
      *               days open and final financials.                  *
      *               Every open claim is also written to a CSV        *
      *               extract for the claims MI team.                  *
      *               PARM: optional commit frequency (nnnn).          *
      * CALLS       : CLMDATES (DTDIFF)                                *
      *               CLMERRLG (ERRLOG)                                *
      * FILES       : CSVOUT  open-claim CSV extract (CLMLAGC1)        *
      *               RPTOUT  132-column report                        *
      *               CHKPT   checkpoint record (CLMLAGK1)             *
      * TABLES      : CLM_CLAIM   (R)                                  *
      *               CLM_USER    (R)                                  *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-LAG-001 Open means not draft and not      *
      *                              closed                            *
      *               BR-CLM-LAG-002 Age is days from the reported     *
      *                              date to the business date         *
      *               BR-CLM-LAG-003 Buckets 0-30, 31-90, 91-180,      *
      *                              181-365, over 365 days            *
      *               BR-CLM-LAG-004 Incurred = reserves + paid -      *
      *                              recovered; a stored total that    *
      *                              disagrees is flagged (RC 4)       *
      *               BR-CLM-LAG-005 Closed this month = close date    *
      *                              from the first of the business    *
      *                              month to the business date        *
      *               BR-CLM-LAG-006 Days open run from the open date  *
      *                              (reported date if never opened)   *
      *                              to the close date                 *
      *               BR-CLM-LAG-007 DUPLICAT closures are counted by  *
      *                              reason but kept out of the        *
      *                              average days-to-close             *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE     ASSIGN TO CSVOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CSV-STATUS.
           SELECT RPT-FILE     ASSIGN TO RPTOUT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL CHK-FILE ASSIGN TO CHKPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE
           RECORDING MODE IS F.
           COPY CLMLAGC1.
       FD  RPT-FILE
           RECORDING MODE IS F.
       01  RPT-RECORD                  PIC X(132).
       FD  CHK-FILE
           RECORDING MODE IS F.
           COPY CLMLAGK1.

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMLAGRP'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-FILE-STATUS.
           05 WS-CSV-STATUS            PIC XX    VALUE '00'.
           05 WS-RPT-STATUS            PIC XX    VALUE '00'.
           05 WS-CHK-STATUS            PIC XX    VALUE '00'.

       01  WS-SWITCHES.
           05 WS-EOF-SW                PIC X     VALUE 'N'.
              88 WS-EOF                 VALUE 'Y'.
           05 WS-FIRST-SW              PIC X     VALUE 'Y'.
              88 WS-FIRST-CLAIM         VALUE 'Y'.

       01  WS-RUN-CONTROL.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-MONTH-START           PIC X(10) VALUE SPACES.
           05 WS-RUN-TS                PIC X(26) VALUE SPACES.
           05 WS-PARM-FREQ             PIC X(5)  JUSTIFIED RIGHT
                                                 VALUE SPACES.
           05 WS-PARM-FREQ-N REDEFINES WS-PARM-FREQ
                                       PIC 9(5).
           05 WS-COMMIT-FREQ           PIC S9(4) COMP VALUE 200.
           05 WS-UNCOMMITTED           PIC S9(4) COMP VALUE 0.
           05 WS-MAX-RC                PIC S9(4) COMP VALUE 0.
           05 WS-PAGE-NO               PIC S9(4) COMP VALUE 0.
           05 WS-LINE-COUNT            PIC S9(4) COMP VALUE 99.
           05 WS-LINES-PER-PAGE        PIC S9(4) COMP VALUE 58.

       01  WS-BREAK-KEYS.
           05 WS-PREV-SEGMENT          PIC X(8)  VALUE SPACES.
           05 WS-PREV-ADJUSTER         PIC X(8)  VALUE SPACES.

      * BR-CLM-LAG-003 Age bucket upper bounds (days) and labels
       01  WS-BUCKET-VALUES.
           05 FILLER PIC X(12) VALUE '000300-30   '.
           05 FILLER PIC X(12) VALUE '0009031-90  '.
           05 FILLER PIC X(12) VALUE '0018091-180 '.
           05 FILLER PIC X(12) VALUE '00365181-365'.
           05 FILLER PIC X(12) VALUE '99999365+   '.
       01  WS-BUCKET-TABLE REDEFINES WS-BUCKET-VALUES.
           05 WS-BKT OCCURS 5 TIMES INDEXED BY BX.
              10 WS-BKT-MAX-DAYS       PIC 9(5).
              10 WS-BKT-LABEL          PIC X(7).

      * Accumulators: level 1 adjuster, 2 segment, 3 grand total
       01  WS-LEVEL-TOTALS.
           05 WS-LVL OCCURS 3 TIMES.
              10 WS-LV-CLAIMS          PIC S9(7) COMP-3.
              10 WS-LV-BKT-COUNT OCCURS 5 TIMES
                                       PIC S9(7) COMP-3.
              10 WS-LV-RESERVES        PIC S9(13)V99 COMP-3.
              10 WS-LV-PAID            PIC S9(13)V99 COMP-3.
              10 WS-LV-INCURRED        PIC S9(13)V99 COMP-3.
       01  WS-LX                       PIC S9(4) COMP VALUE 0.
       01  WS-KX                       PIC S9(4) COMP VALUE 0.

      * Close reasons written by CLMCLAIM CLMCLS (row 6 = other)
       01  WS-REASON-VALUES.
           05 FILLER PIC X(8) VALUE 'COMPLETE'.
           05 FILLER PIC X(8) VALUE 'DUPLICAT'.
           05 FILLER PIC X(8) VALUE 'DENIED  '.
           05 FILLER PIC X(8) VALUE 'WITHDRAW'.
           05 FILLER PIC X(8) VALUE 'NOPAY   '.
           05 FILLER PIC X(8) VALUE '*OTHER* '.
       01  WS-REASON-TABLE REDEFINES WS-REASON-VALUES.
           05 WS-RSN-CODE OCCURS 6 TIMES INDEXED BY RX
                                       PIC X(8).
       01  WS-REASON-COUNTS.
           05 WS-RSN-COUNT OCCURS 6 TIMES
                                       PIC S9(7) COMP-3.

       01  WS-CLOSED-TOTALS.
           05 WS-CL-CLAIMS             PIC S9(7) COMP-3 VALUE 0.
           05 WS-CL-AVG-CLAIMS         PIC S9(7) COMP-3 VALUE 0.
           05 WS-CL-DAYS               PIC S9(9) COMP-3 VALUE 0.
           05 WS-CL-PAID               PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CL-RECOVERED          PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CL-INCURRED           PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-CL-AVG-DAYS           PIC S9(5)V99 COMP-3 VALUE 0.

       01  WS-CLAIM-WORK.
           05 WS-AGE-DAYS              PIC S9(7) COMP-3 VALUE 0.
           05 WS-CALC-INCURRED         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-INCURRED-VARIANCES    PIC S9(7) COMP-3 VALUE 0.
           05 WS-CSV-RECORDS           PIC S9(7) COMP-3 VALUE 0.
           05 WS-FLAGS                 PIC X(20) VALUE SPACES.
           05 WS-FLAG-PTR              PIC S9(4) COMP VALUE 1.
           05 WS-ADJUSTER-NAME         PIC X(40) VALUE SPACES.
           05 WS-START-DATE            PIC X(10) VALUE SPACES.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-KEY-DISP              PIC 9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.

       01  WS-CSV-FIELDS.
           05 WS-CSV-AGE               PIC -(6)9.
           05 WS-CSV-RESERVES          PIC -(11)9.99.
           05 WS-CSV-PAID              PIC -(11)9.99.
           05 WS-CSV-RECOVERED         PIC -(11)9.99.
           05 WS-CSV-INCURRED          PIC -(11)9.99.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDUSR END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

      * BR-CLM-LAG-001 Open claims in segment / adjuster sequence
           EXEC SQL DECLARE OPNCSR CURSOR WITH HOLD FOR
                SELECT C.CLAIM_ID, C.CLAIM_NUMBER, C.POLICY_TYPE,
                       C.LOSS_TYPE, C.LOSS_DATE, C.REPORTED_DATE,
                       C.SEGMENT_CODE, C.ASSIGNED_USER,
                       C.TOTAL_RESERVES, C.TOTAL_PAID,
                       C.TOTAL_RECOVERED, C.TOTAL_INCURRED,
                       C.LITIGATION_FLAG, C.SIU_STATUS, C.CAT_CODE,
                       C.COVERAGE_IN_QSTN,
                       COALESCE(U.USER_NAME, 'UNASSIGNED')
                  FROM CLMDB.CLM_CLAIM C
                  LEFT JOIN CLMDB.CLM_USER U
                    ON U.USER_ID = C.ASSIGNED_USER
                 WHERE C.CLAIM_STATE NOT IN ('DRFT', 'CLSD')
                 ORDER BY C.SEGMENT_CODE, C.ASSIGNED_USER,
                          C.REPORTED_DATE, C.CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

      * BR-CLM-LAG-005 Closed from the first of the month to date
           EXEC SQL DECLARE CLSCSR CURSOR WITH HOLD FOR
                SELECT CLAIM_ID, CLAIM_NUMBER, SEGMENT_CODE,
                       ASSIGNED_USER, CLOSE_DATE, CLOSE_REASON,
                       REPORTED_DATE, OPEN_DATE, TOTAL_PAID,
                       TOTAL_RECOVERED, TOTAL_INCURRED
                  FROM CLMDB.CLM_CLAIM
                 WHERE CLAIM_STATE = 'CLSD'
                   AND CLOSE_DATE BETWEEN :WS-MONTH-START
                                      AND :WS-BUS-DATE
                 ORDER BY SEGMENT_CODE, CLOSE_DATE, CLAIM_ID
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY CLMLAGR1.

       LINKAGE SECTION.
       01  LK-PARM.
           05 LK-PARM-LEN              PIC S9(4) COMP.
           05 LK-PARM-DATA             PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           PERFORM 2000-OPEN-CLAIMS-SECTION
           PERFORM 2500-CLOSED-SECTION
           PERFORM 7000-COMMIT
           MOVE 'C'                    TO LAK-RUN-STATUS
           PERFORM 7100-WRITE-CHECKPOINT
           PERFORM 6800-PRINT-CONTROL-TOTALS
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'CLMLAGRP RPTOUT OPEN FAILED ' WS-RPT-STATUS
              MOVE 16                  TO WS-MAX-RC
              PERFORM 9000-RETURN
           END-IF
           IF LK-PARM-LEN > 0
              MOVE LK-PARM-DATA(1:LK-PARM-LEN) TO WS-PARM-FREQ
              INSPECT WS-PARM-FREQ REPLACING LEADING SPACE BY ZERO
              IF WS-PARM-FREQ-N IS NUMERIC
              AND WS-PARM-FREQ-N > 0 AND WS-PARM-FREQ-N <= 5000
                 MOVE WS-PARM-FREQ-N   TO WS-COMMIT-FREQ
              ELSE
                 MOVE 'Invalid PARM - commit frequency 1-5000'
                                       TO LAR-MSG-TEXT
                 PERFORM 6900-ABORT-RUN
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
           MOVE CTL-BUSINESS-DATE      TO WS-BUS-DATE LAR-H1-BUS-DATE
           MOVE WS-RUN-TS(1:19)        TO LAR-H1-RUN-TS
           STRING WS-BUS-DATE(1:8) '01' DELIMITED BY SIZE
                  INTO WS-MONTH-START
           INITIALIZE WS-LEVEL-TOTALS WS-REASON-COUNTS
           OPEN INPUT CHK-FILE
           IF WS-CHK-STATUS = '00'
              READ CHK-FILE
                 AT END
                    CONTINUE
                 NOT AT END
                    IF LAK-PROGRAM = WS-PROGRAM-ID
                    AND LAK-RUN-ACTIVE
                       STRING 'RESTART: previous run incomplete - '
                              'report and extract regenerated in full'
                              DELIMITED BY SIZE INTO LAR-MSG-TEXT
                       PERFORM 6100-WRITE-MSG
                    END-IF
              END-READ
              CLOSE CHK-FILE
           END-IF
           OPEN OUTPUT CHK-FILE
           INITIALIZE LAK-CHECKPOINT-REC
           OPEN OUTPUT CSV-FILE
           IF WS-CSV-STATUS NOT = '00' OR WS-CHK-STATUS NOT = '00'
              MOVE 'CSVOUT or CHKPT cannot be opened' TO LAR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           MOVE SPACES                 TO LAC-CSV-RECORD
           STRING 'CLAIM_NUMBER,SEGMENT,ADJUSTER,POLICY_TYPE,'
                  'LOSS_TYPE,LOSS_DATE,REPORTED_DATE,AGE_DAYS,'
                  'AGE_BUCKET,RESERVES,PAID,RECOVERED,INCURRED,'
                  'LITIGATION,SIU_STATUS,CAT_CODE'
                  DELIMITED BY SIZE INTO LAC-CSV-RECORD
           WRITE LAC-CSV-RECORD.

      ******************************************************************
       2000-OPEN-CLAIMS-SECTION.
           MOVE 'OPEN'                 TO LAK-SECTION
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN OPNCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2000-OPEN-CLAIMS'  TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-OPEN
           PERFORM UNTIL WS-EOF
              PERFORM 2100-CHECK-BREAKS
              PERFORM 2200-PROCESS-OPEN-CLAIM
              MOVE CLM-CLAIM-ID        TO LAK-LAST-CLAIM-ID
              ADD 1                    TO LAK-CLAIMS-READ
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
                 PERFORM 7100-WRITE-CHECKPOINT
              END-IF
              PERFORM 4100-FETCH-OPEN
           END-PERFORM
           EXEC SQL CLOSE OPNCSR END-EXEC
           IF NOT WS-FIRST-CLAIM
              PERFORM 2300-ADJUSTER-BREAK
              PERFORM 2400-SEGMENT-BREAK
           END-IF
           MOVE 'ALL OPEN CLAIMS'      TO LAR-S-LABEL
           MOVE 3                      TO WS-LX
           PERFORM 6300-PRINT-SUBTOTAL.

      ******************************************************************
       2100-CHECK-BREAKS.
           IF WS-FIRST-CLAIM
              MOVE 'N'                 TO WS-FIRST-SW
              PERFORM 2150-START-SEGMENT
              PERFORM 2160-START-ADJUSTER
           ELSE
              IF CLM-SEGMENT-CODE NOT = WS-PREV-SEGMENT
                 PERFORM 2300-ADJUSTER-BREAK
                 PERFORM 2400-SEGMENT-BREAK
                 PERFORM 2150-START-SEGMENT
                 PERFORM 2160-START-ADJUSTER
              ELSE
                 IF CLM-ASSIGNED-USER NOT = WS-PREV-ADJUSTER
                    PERFORM 2300-ADJUSTER-BREAK
                    PERFORM 2160-START-ADJUSTER
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
       2150-START-SEGMENT.
           MOVE CLM-SEGMENT-CODE       TO WS-PREV-SEGMENT
           MOVE 99                     TO WS-LINE-COUNT
           MOVE 'SEGMENT'              TO LAR-BRK-LABEL
           MOVE CLM-SEGMENT-CODE       TO LAR-BRK-CODE
           IF CLM-SEGMENT-CODE = SPACES
              MOVE 'NOT YET SEGMENTED' TO LAR-BRK-NAME
           ELSE
              MOVE SPACES              TO LAR-BRK-NAME
           END-IF
           MOVE LAR-BREAK-LINE         TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       2160-START-ADJUSTER.
           MOVE CLM-ASSIGNED-USER      TO WS-PREV-ADJUSTER
           MOVE SPACES                 TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE ' ADJUSTER'            TO LAR-BRK-LABEL
           MOVE CLM-ASSIGNED-USER      TO LAR-BRK-CODE
           MOVE WS-ADJUSTER-NAME       TO LAR-BRK-NAME
           MOVE LAR-BREAK-LINE         TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE LAR-OPEN-HEAD          TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       2200-PROCESS-OPEN-CLAIM.
      * BR-CLM-LAG-002 Age from reported date to business date
           MOVE CLM-REPORTED-DATE      TO WS-START-DATE
           MOVE WS-BUS-DATE            TO DTA-DT-DATE-2
           PERFORM 8100-DAYS-BETWEEN
           MOVE DTA-DT-RESULT-NUM      TO WS-AGE-DAYS
           IF WS-AGE-DAYS < 0
              MOVE 0                   TO WS-AGE-DAYS
           END-IF
           SET BX                      TO 1
           SEARCH WS-BKT
              AT END
                 SET BX                TO 5
              WHEN WS-AGE-DAYS <= WS-BKT-MAX-DAYS(BX)
                 CONTINUE
           END-SEARCH
           SET WS-KX                   TO BX
      * BR-CLM-LAG-004 Incurred recomputed and reconciled
           COMPUTE WS-CALC-INCURRED = CLM-TOTAL-RESERVES
                                    + CLM-TOTAL-PAID
                                    - CLM-TOTAL-RECOVERED
           MOVE SPACES                 TO WS-FLAGS
           MOVE 1                      TO WS-FLAG-PTR
           IF CLM-LITIGATION-FLAG = 'Y'
              STRING 'LIT ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
           END-IF
           IF CLM-SIU-STATUS NOT = 'NONE'
           AND CLM-SIU-STATUS NOT = SPACES
              STRING 'SIU ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
           END-IF
           IF CLM-CAT-CODE NOT = SPACES
              STRING 'CAT ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
           END-IF
           IF CLM-COVERAGE-IN-QSTN = 'Y'
              STRING 'COV ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
           END-IF
           IF WS-CALC-INCURRED NOT = CLM-TOTAL-INCURRED
              STRING 'INC? ' DELIMITED BY SIZE INTO WS-FLAGS
                     WITH POINTER WS-FLAG-PTR
              ADD 1                    TO WS-INCURRED-VARIANCES
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > 3
              ADD 1                    TO WS-LV-CLAIMS(WS-LX)
              ADD 1                    TO WS-LV-BKT-COUNT(WS-LX WS-KX)
              ADD CLM-TOTAL-RESERVES   TO WS-LV-RESERVES(WS-LX)
              ADD CLM-TOTAL-PAID       TO WS-LV-PAID(WS-LX)
              ADD WS-CALC-INCURRED     TO WS-LV-INCURRED(WS-LX)
           END-PERFORM
           MOVE CLM-CLAIM-NUMBER       TO LAR-O-CLAIM-NUMBER
           MOVE CLM-POLICY-TYPE        TO LAR-O-POL-TYPE
           MOVE CLM-LOSS-TYPE          TO LAR-O-LOSS-TYPE
           MOVE CLM-LOSS-DATE          TO LAR-O-LOSS-DATE
           MOVE CLM-REPORTED-DATE      TO LAR-O-REPORTED
           MOVE WS-AGE-DAYS            TO LAR-O-AGE
           MOVE WS-BKT-LABEL(WS-KX)    TO LAR-O-BUCKET
           MOVE CLM-TOTAL-RESERVES     TO LAR-O-RESERVES
           MOVE CLM-TOTAL-PAID         TO LAR-O-PAID
           MOVE WS-CALC-INCURRED       TO LAR-O-INCURRED
           MOVE WS-FLAGS               TO LAR-O-FLAGS
           MOVE LAR-OPEN-LINE          TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           PERFORM 6400-WRITE-CSV.

      ******************************************************************
       2300-ADJUSTER-BREAK.
           MOVE SPACES                 TO LAR-S-LABEL
           STRING 'ADJUSTER ' WS-PREV-ADJUSTER
                  DELIMITED BY SIZE INTO LAR-S-LABEL
           MOVE 1                      TO WS-LX
           PERFORM 6300-PRINT-SUBTOTAL
           INITIALIZE WS-LVL(1).

      ******************************************************************
       2400-SEGMENT-BREAK.
           MOVE SPACES                 TO LAR-S-LABEL
           STRING 'SEGMENT ' WS-PREV-SEGMENT
                  DELIMITED BY SIZE INTO LAR-S-LABEL
           MOVE 2                      TO WS-LX
           PERFORM 6300-PRINT-SUBTOTAL
           INITIALIZE WS-LVL(2).

      ******************************************************************
       2500-CLOSED-SECTION.
           MOVE 'CLSD'                 TO LAK-SECTION
           MOVE 99                     TO WS-LINE-COUNT
           MOVE SPACES                 TO LAR-MSG-TEXT
           STRING '*** CLAIMS CLOSED THIS MONTH ' WS-MONTH-START
                  ' TO ' WS-BUS-DATE
                  DELIMITED BY SIZE INTO LAR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE LAR-CLOSED-HEAD        TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE 'N'                    TO WS-EOF-SW
           EXEC SQL OPEN CLSCSR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2500-CLOSED-SECTION' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4200-FETCH-CLOSED
           PERFORM UNTIL WS-EOF
              PERFORM 2600-PROCESS-CLOSED-CLAIM
              MOVE CLM-CLAIM-ID        TO LAK-LAST-CLAIM-ID
              ADD 1                    TO WS-UNCOMMITTED
              IF WS-UNCOMMITTED >= WS-COMMIT-FREQ
                 PERFORM 7000-COMMIT
                 PERFORM 7100-WRITE-CHECKPOINT
              END-IF
              PERFORM 4200-FETCH-CLOSED
           END-PERFORM
           EXEC SQL CLOSE CLSCSR END-EXEC
           IF WS-CL-AVG-CLAIMS > 0
              COMPUTE WS-CL-AVG-DAYS ROUNDED =
                      WS-CL-DAYS / WS-CL-AVG-CLAIMS
           END-IF
           MOVE 'CLAIMS CLOSED THIS MONTH' TO LAR-MSG-TEXT
           MOVE WS-CL-CLAIMS           TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           PERFORM VARYING RX FROM 1 BY 1 UNTIL RX > 6
              MOVE SPACES              TO LAR-MSG-TEXT
              STRING '  CLOSE REASON ' WS-RSN-CODE(RX)
                     DELIMITED BY SIZE INTO LAR-MSG-TEXT
              MOVE WS-RSN-COUNT(RX)    TO LAR-MSG-NUM
              PERFORM 6100-WRITE-MSG
           END-PERFORM
           MOVE 'AVERAGE DAYS OPEN AT CLOSE (EXCLUDING DUPLICAT)'
                                       TO LAR-MSG-TEXT
           MOVE WS-CL-AVG-DAYS         TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           MOVE 'PAID ON CLAIMS CLOSED THIS MONTH' TO LAR-MSG-TEXT
           MOVE WS-CL-PAID             TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           MOVE 'RECOVERED ON CLAIMS CLOSED THIS MONTH'
                                       TO LAR-MSG-TEXT
           MOVE WS-CL-RECOVERED        TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           MOVE 'INCURRED ON CLAIMS CLOSED THIS MONTH'
                                       TO LAR-MSG-TEXT
           MOVE WS-CL-INCURRED         TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG.

      ******************************************************************
      * BR-CLM-LAG-006 Days open from open date, else reported date
       2600-PROCESS-CLOSED-CLAIM.
           IF CLM-OPEN-DATE-IND < 0
              MOVE CLM-REPORTED-DATE   TO WS-START-DATE
           ELSE
              MOVE CLM-OPEN-DATE       TO WS-START-DATE
           END-IF
           MOVE CLM-CLOSE-DATE         TO DTA-DT-DATE-2
           PERFORM 8100-DAYS-BETWEEN
           MOVE DTA-DT-RESULT-NUM      TO WS-AGE-DAYS
           IF WS-AGE-DAYS < 0
              MOVE 0                   TO WS-AGE-DAYS
           END-IF
           ADD 1                       TO WS-CL-CLAIMS
           SET RX                      TO 1
           SEARCH WS-RSN-CODE
              AT END
                 SET RX                TO 6
              WHEN WS-RSN-CODE(RX) = CLM-CLOSE-REASON
                 CONTINUE
           END-SEARCH
           ADD 1                       TO WS-RSN-COUNT(RX)
      * BR-CLM-LAG-007 Duplicates are not real closures for cycle time
           IF CLM-CLOSE-REASON NOT = 'DUPLICAT'
              ADD 1                    TO WS-CL-AVG-CLAIMS
              ADD WS-AGE-DAYS          TO WS-CL-DAYS
           END-IF
           ADD CLM-TOTAL-PAID          TO WS-CL-PAID
           ADD CLM-TOTAL-RECOVERED     TO WS-CL-RECOVERED
           ADD CLM-TOTAL-INCURRED      TO WS-CL-INCURRED
           MOVE CLM-CLAIM-NUMBER       TO LAR-C-CLAIM-NUMBER
           MOVE CLM-SEGMENT-CODE       TO LAR-C-SEGMENT
           MOVE CLM-ASSIGNED-USER      TO LAR-C-ADJUSTER
           MOVE CLM-CLOSE-DATE         TO LAR-C-CLOSE-DATE
           MOVE CLM-CLOSE-REASON       TO LAR-C-REASON
           MOVE WS-AGE-DAYS            TO LAR-C-DAYS-OPEN
           MOVE CLM-TOTAL-PAID         TO LAR-C-PAID
           MOVE CLM-TOTAL-RECOVERED    TO LAR-C-RECOVERED
           MOVE CLM-TOTAL-INCURRED     TO LAR-C-INCURRED
           MOVE LAR-CLOSED-LINE        TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       4100-FETCH-OPEN.
           EXEC SQL
              FETCH OPNCSR
               INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                    :CLM-POLICY-TYPE, :CLM-LOSS-TYPE,
                    :CLM-LOSS-DATE, :CLM-REPORTED-DATE,
                    :CLM-SEGMENT-CODE, :CLM-ASSIGNED-USER,
                    :CLM-TOTAL-RESERVES, :CLM-TOTAL-PAID,
                    :CLM-TOTAL-RECOVERED, :CLM-TOTAL-INCURRED,
                    :CLM-LITIGATION-FLAG, :CLM-SIU-STATUS,
                    :CLM-CAT-CODE, :CLM-COVERAGE-IN-QSTN,
                    :USR-USER-NAME
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE SPACES           TO WS-ADJUSTER-NAME
                 IF USR-USER-NAME-LEN > 0
                    MOVE USR-USER-NAME-TEXT(1:USR-USER-NAME-LEN)
                                       TO WS-ADJUSTER-NAME
                 END-IF
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4100-FETCH-OPEN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-FETCH-CLOSED.
           EXEC SQL
              FETCH CLSCSR
               INTO :CLM-CLAIM-ID, :CLM-CLAIM-NUMBER,
                    :CLM-SEGMENT-CODE, :CLM-ASSIGNED-USER,
                    :CLM-CLOSE-DATE :CLM-CLOSE-DATE-IND,
                    :CLM-CLOSE-REASON, :CLM-REPORTED-DATE,
                    :CLM-OPEN-DATE :CLM-OPEN-DATE-IND,
                    :CLM-TOTAL-PAID, :CLM-TOTAL-RECOVERED,
                    :CLM-TOTAL-INCURRED
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET WS-EOF            TO TRUE
              WHEN OTHER
                 MOVE '4200-FETCH-CLOSED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       6000-PRINT-HEADINGS.
           ADD 1                       TO WS-PAGE-NO
           MOVE WS-PAGE-NO             TO LAR-H1-PAGE
           WRITE RPT-RECORD FROM LAR-HEAD-1 AFTER ADVANCING PAGE
           WRITE RPT-RECORD FROM LAR-HEAD-2 AFTER ADVANCING 1
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
           MOVE LAR-MSG-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE SPACES                 TO LAR-MSG-TEXT
           MOVE 0                      TO LAR-MSG-NUM.

      ******************************************************************
       6300-PRINT-SUBTOTAL.
           MOVE LAR-SUB-HEAD           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE
           MOVE WS-LV-CLAIMS(WS-LX)    TO LAR-S-CLAIMS
           PERFORM VARYING WS-KX FROM 1 BY 1 UNTIL WS-KX > 5
              MOVE WS-LV-BKT-COUNT(WS-LX WS-KX)
                                       TO LAR-S-BKT-COUNT(WS-KX)
           END-PERFORM
           MOVE WS-LV-RESERVES(WS-LX)  TO LAR-S-RESERVES
           MOVE WS-LV-PAID(WS-LX)      TO LAR-S-PAID
           MOVE WS-LV-INCURRED(WS-LX)  TO LAR-S-INCURRED
           MOVE LAR-SUB-LINE           TO RPT-RECORD
           PERFORM 6050-WRITE-LINE.

      ******************************************************************
       6400-WRITE-CSV.
           MOVE WS-AGE-DAYS            TO WS-CSV-AGE
           MOVE CLM-TOTAL-RESERVES     TO WS-CSV-RESERVES
           MOVE CLM-TOTAL-PAID         TO WS-CSV-PAID
           MOVE CLM-TOTAL-RECOVERED    TO WS-CSV-RECOVERED
           MOVE WS-CALC-INCURRED       TO WS-CSV-INCURRED
           MOVE SPACES                 TO LAC-CSV-RECORD
           STRING FUNCTION TRIM(CLM-CLAIM-NUMBER) ','
                  FUNCTION TRIM(CLM-SEGMENT-CODE) ','
                  FUNCTION TRIM(CLM-ASSIGNED-USER) ','
                  CLM-POLICY-TYPE ','
                  FUNCTION TRIM(CLM-LOSS-TYPE) ','
                  CLM-LOSS-DATE ','
                  CLM-REPORTED-DATE ','
                  FUNCTION TRIM(WS-CSV-AGE) ','
                  FUNCTION TRIM(WS-BKT-LABEL(WS-KX)) ','
                  FUNCTION TRIM(WS-CSV-RESERVES) ','
                  FUNCTION TRIM(WS-CSV-PAID) ','
                  FUNCTION TRIM(WS-CSV-RECOVERED) ','
                  FUNCTION TRIM(WS-CSV-INCURRED) ','
                  CLM-LITIGATION-FLAG ','
                  FUNCTION TRIM(CLM-SIU-STATUS) ','
                  FUNCTION TRIM(CLM-CAT-CODE)
                  DELIMITED BY SIZE INTO LAC-CSV-RECORD
           WRITE LAC-CSV-RECORD
           IF WS-CSV-STATUS NOT = '00'
              MOVE 'CSVOUT write failed' TO LAR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF
           ADD 1                       TO WS-CSV-RECORDS.

      ******************************************************************
       6800-PRINT-CONTROL-TOTALS.
           PERFORM 6100-WRITE-MSG
           MOVE '*** CONTROL TOTALS'   TO LAR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 'OPEN CLAIMS REPORTED' TO LAR-MSG-TEXT
           MOVE WS-LV-CLAIMS(3)        TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           MOVE 'CSV DATA RECORDS WRITTEN (EXCLUDING HEADER)'
                                       TO LAR-MSG-TEXT
           MOVE WS-CSV-RECORDS         TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           MOVE 'OPEN RESERVES TOTAL'  TO LAR-MSG-TEXT
           MOVE WS-LV-RESERVES(3)      TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           STRING 'CLAIMS WHOSE STORED INCURRED DIFFERS FROM '
                  'RESERVES + PAID - RECOVERED'
                  DELIMITED BY SIZE INTO LAR-MSG-TEXT
           MOVE WS-INCURRED-VARIANCES  TO LAR-MSG-NUM
           PERFORM 6100-WRITE-MSG
           MOVE 'RETURN CODE'          TO LAR-MSG-TEXT
           MOVE WS-MAX-RC              TO LAR-MSG-NUM
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
           MOVE WS-PROGRAM-ID          TO LAK-PROGRAM
           IF NOT LAK-RUN-COMPLETE
              MOVE 'R'                 TO LAK-RUN-STATUS
           END-IF
           MOVE WS-BUS-DATE            TO LAK-BUS-DATE
           MOVE FUNCTION CURRENT-DATE  TO LAK-CHECKPOINT-TS
           CLOSE CHK-FILE
           OPEN OUTPUT CHK-FILE
           WRITE LAK-CHECKPOINT-REC
           IF WS-CHK-STATUS NOT = '00'
              DISPLAY 'CLMLAGRP CHECKPOINT WRITE FAILED ' WS-CHK-STATUS
              IF WS-MAX-RC < 4
                 MOVE 4                TO WS-MAX-RC
              END-IF
           END-IF.

      ******************************************************************
       8100-DAYS-BETWEEN.
           INITIALIZE DTA-HEADER
           MOVE 'DTDIFF  '             TO DTA-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO DTA-USER-ID DTA-CALLER-PGM
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-START-DATE          TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK
              MOVE CLM-CLAIM-ID        TO WS-KEY-DISP
              STRING 'CLMDATES DTDIFF failed for claim id ' WS-KEY-DISP
                     ' - ' DTA-REASON-CODE
                     DELIMITED BY SIZE INTO LAR-MSG-TEXT
              PERFORM 6900-ABORT-RUN
           END-IF.

      ******************************************************************
       9000-RETURN.
           CLOSE CSV-FILE
           CLOSE RPT-FILE
           CLOSE CHK-FILE
           MOVE WS-MAX-RC              TO RETURN-CODE
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           MOVE CLM-CLAIM-ID           TO WS-KEY-DISP
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE WS-PROGRAM-ID          TO ERC-USER-ID ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE WS-KEY-DISP            TO ERC-ER-KEY
           STRING 'CLMLAGRP Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           EXEC SQL ROLLBACK END-EXEC
           CALL WS-ERR-PGM USING WS-ERR-CA
           EXEC SQL COMMIT END-EXEC
           MOVE ERC-ER-TEXT(1:80)      TO LAR-MSG-TEXT
           PERFORM 6100-WRITE-MSG
           MOVE 12                     TO WS-MAX-RC
           PERFORM 9000-RETURN.
