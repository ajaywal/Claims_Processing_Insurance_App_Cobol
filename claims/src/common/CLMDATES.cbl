       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMDATES.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMDATES                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Date arithmetic on ISO yyyy-mm-dd dates. All     *
      *               claims date maths (activity due dates, reserve   *
      *               review cycles, late-reporting checks, payment    *
      *               scheduling) is routed through this program so    *
      *               weekend / UK bank holiday handling and month-end *
      *               rules live in exactly one place.                 *
      * REQUESTS    : DTADDD  DATE-1 + NUMBER days                     *
      *               DTADDM  DATE-1 + NUMBER months (EOM clamped)     *
      *               DTDIFF  DATE-2 - DATE-1 in days -> RESULT-NUM    *
      *               DTDOW   day of week of DATE-1 (1=Mon..7=Sun)     *
      *               DTBUSD  roll forward to a business day           *
      *               DTEOM   last day of DATE-1's month               *
      *               DTVALD  validate DATE-1 -> CA-DT-VALID           *
      * CALLED BY   : CLMACTV0, CLMSIUSC, CLMFINTX, CLMCLAIM, batch    *
      * CALLS       : CLMERRLG                                         *
      * TABLES      : CLM_CONTROL (R)  CLM_CODE_TABLE 'HOLIDAY' (R)    *
      * RULES       : BR-CLM-DTE-001 Dates are ISO yyyy-mm-dd with     *
      *                              hyphens in positions 5 and 8      *
      *               BR-CLM-DTE-002 Supported years 1601..9999        *
      *               BR-CLM-DTE-003 Gregorian leap year: divisible by *
      *                              4, not by 100 unless by 400       *
      *               BR-CLM-DTE-004 A blank input date defaults to    *
      *                              the business date (CA-BUS-DATE,   *
      *                              else CLM_CONTROL 'SYSTEM')        *
      *               BR-CLM-DTE-005 Adding months clamps the day to   *
      *                              the target month end; month-end   *
      *                              is not sticky                     *
      *               BR-CLM-DTE-006 Month offsets limited to +/-1200  *
      *               BR-CLM-DTE-007 DTDIFF is signed DATE-2 - DATE-1  *
      *               BR-CLM-DTE-008 Day of week is ISO 1=Mon..7=Sun   *
      *               BR-CLM-DTE-009 Saturdays, Sundays and active     *
      *                              HOLIDAY code rows are not         *
      *                              business days                     *
      *               BR-CLM-DTE-010 DTBUSD rolls DATE-1 forward to    *
      *                              the first business day on or      *
      *                              after it (NUMBER is ignored)      *
      *               BR-CLM-DTE-011 More than 31 consecutive non-     *
      *                              business days means the holiday   *
      *                              table is corrupt - request fails  *
      *               BR-CLM-DTE-012 Holidays are cached per year for  *
      *                              the run unit; malformed HOLIDAY   *
      *                              rows are ignored with a warning   *
      *               BR-CLM-DTE-013 A result outside 1601..9999 is a  *
      *                              validation error                  *
      *               BR-CLM-DTE-014 DTVALD reports an invalid date as *
      *                              RC 04 with CA-DT-VALID = 'N'      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-CONSTANTS.
           05 WS-MIN-YEAR              PIC 9(4)  VALUE 1601.
           05 WS-MAX-YEAR              PIC 9(4)  VALUE 9999.
           05 WS-MAX-MONTHS            PIC S9(5) COMP-3 VALUE 1200.
           05 WS-MAX-NONBUS-RUN        PIC S9(4) COMP VALUE 31.
           05 WS-HOL-MAX               PIC S9(4) COMP VALUE 40.

       01  WS-MONTH-DAYS-VALUES.
           05 FILLER                   PIC X(24)
                                VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS REDEFINES WS-MONTH-DAYS-VALUES.
           05 WS-MD-DAYS               PIC 99 OCCURS 12 TIMES.

      * Date being validated / converted
       01  WS-CHK-ISO                  PIC X(10).
       01  WS-CHK-ISO-R REDEFINES WS-CHK-ISO.
           05 WS-CHK-YYYY              PIC X(4).
           05 WS-CHK-SEP1              PIC X.
           05 WS-CHK-MM                PIC XX.
           05 WS-CHK-SEP2              PIC X.
           05 WS-CHK-DD                PIC XX.

       01  WS-YMD                      PIC 9(8).
       01  WS-YMD-R REDEFINES WS-YMD.
           05 WS-YMD-YEAR              PIC 9(4).
           05 WS-YMD-MONTH             PIC 99.
           05 WS-YMD-DAY               PIC 99.

       01  WS-WORK.
           05 WS-CHK-OK                PIC X     VALUE 'N'.
              88 WS-DATE-OK                      VALUE 'Y'.
           05 WS-CHK-REASON            PIC X(8).
           05 WS-CHK-TEXT              PIC X(40).
           05 WS-CHK-INT               PIC S9(9) COMP.
           05 WS-INT-1                 PIC S9(9) COMP.
           05 WS-INT-2                 PIC S9(9) COMP.
           05 WS-RES-INT               PIC S9(9) COMP.
           05 WS-MIN-INT               PIC S9(9) COMP.
           05 WS-MAX-INT               PIC S9(9) COMP.
           05 WS-YEAR                  PIC 9(4).
           05 WS-MONTH                 PIC 99.
           05 WS-DAY                   PIC 99.
           05 WS-DIM                   PIC 99.
           05 WS-LEAP                  PIC X.
              88 WS-IS-LEAP                      VALUE 'Y'.
           05 WS-REM                   PIC S9(4) COMP.
           05 WS-QUOT                  PIC S9(9) COMP.
           05 WS-MONTH-INDEX           PIC S9(9) COMP.
           05 WS-DOW                   PIC 9.
           05 WS-BUSDAY                PIC X.
              88 WS-IS-BUSDAY                    VALUE 'Y'.
           05 WS-NONBUS-RUN            PIC S9(4) COMP.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-KEY               PIC X(20).

      * Holiday cache - one calendar year at a time
       01  WS-HOLIDAY-CACHE.
           05 WS-HOL-YEAR              PIC 9(4)  VALUE 0.
           05 WS-HOL-COUNT             PIC S9(4) COMP VALUE 0.
           05 WS-HOL-ENTRY             PIC 9(8)  OCCURS 40 TIMES
                                       INDEXED BY WS-HOL-IX.
       01  WS-HOL-FROM                 PIC X(10).
       01  WS-HOL-TO                   PIC X(10).
       01  WS-HOL-CODE                 PIC X(10).
       01  WS-HOL-CODE-R REDEFINES WS-HOL-CODE.
           05 WS-HOL-CODE-YMD          PIC X(8).
           05 WS-HOL-CODE-REST         PIC X(2).
       01  WS-HOL-EOF                  PIC X     VALUE 'N'.
           88 WS-HOL-END                         VALUE 'Y'.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

           EXEC SQL DECLARE HOLCUR CURSOR FOR
              SELECT CODE_VALUE
                FROM CLMDB.CLM_CODE_TABLE
               WHERE TABLE_NAME  = 'HOLIDAY'
                 AND ACTIVE_FLAG = 'Y'
                 AND CODE_VALUE BETWEEN :WS-HOL-FROM AND :WS-HOL-TO
               ORDER BY CODE_VALUE
               FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCDTE.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'DTADDD  '
                 PERFORM 2100-ADD-DAYS
              WHEN 'DTADDM  '
                 PERFORM 2200-ADD-MONTHS
              WHEN 'DTDIFF  '
                 PERFORM 2300-DIFFERENCE
              WHEN 'DTDOW   '
                 PERFORM 2400-DAY-OF-WEEK
              WHEN 'DTBUSD  '
                 PERFORM 2500-BUSINESS-DAY
              WHEN 'DTEOM   '
                 PERFORM 2600-END-OF-MONTH
              WHEN 'DTVALD  '
                 PERFORM 2700-VALIDATE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMDATES: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          CA-DT-RESULT-DATE
           MOVE 0                      TO CA-DT-RESULT-NUM
           MOVE 'N'                    TO CA-DT-VALID
           COMPUTE WS-MIN-INT = FUNCTION INTEGER-OF-DATE(16010101)
           COMPUTE WS-MAX-INT = FUNCTION INTEGER-OF-DATE(99991231).

      ******************************************************************
      * BR-CLM-DTE-004 Blank DATE-1 defaults to the business date
       1100-DEFAULT-DATE-1.
           IF CA-DT-DATE-1 = SPACES
              PERFORM 1200-RESOLVE-BUS-DATE
              MOVE CA-BUS-DATE         TO CA-DT-DATE-1
           END-IF.

      ******************************************************************
      * Business date: the caller's CA-BUS-DATE, else the control row
       1200-RESOLVE-BUS-DATE.
           IF CA-BUS-DATE = SPACES
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = 'SYSTEM  '
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE CTL-BUSINESS-DATE TO CA-BUS-DATE
                 WHEN 100
                    SET CA-RC-NOT-FOUND TO TRUE
                    MOVE 'CTLNOTFD'    TO CA-REASON-CODE
                    MOVE 'CLM_CONTROL SYSTEM row missing'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 WHEN OTHER
                    MOVE 'SYSTEM  '    TO WS-ERR-KEY
                    MOVE '1200-RESOLVE-BUS-DATE' TO ERC-ER-PARAGRAPH
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
       2100-ADD-DAYS.
           PERFORM 1100-DEFAULT-DATE-1
           PERFORM 3100-VALIDATE-DATE-1
           COMPUTE WS-RES-INT = WS-INT-1 + CA-DT-NUMBER
           PERFORM 5000-INT-TO-RESULT.

      ******************************************************************
      * BR-CLM-DTE-005 Month arithmetic is done on a month index
      * (year * 12 + month - 1); the day is then clamped to the length
      * of the target month: 2026-01-31 + 1 month = 2026-02-28 and
      * 2026-02-28 + 1 month = 2026-03-28 (month-end is not sticky).
       2200-ADD-MONTHS.
           PERFORM 1100-DEFAULT-DATE-1
           PERFORM 3100-VALIDATE-DATE-1
      * BR-CLM-DTE-006 Month offsets limited to +/-1200 (100 years)
           IF CA-DT-NUMBER > WS-MAX-MONTHS
           OR CA-DT-NUMBER < 0 - WS-MAX-MONTHS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DTMONTHS'          TO CA-REASON-CODE
              MOVE 'Month offset must be between -1200 and +1200'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-MONTH-INDEX = WS-YMD-YEAR * 12
                                  + WS-YMD-MONTH - 1
                                  + CA-DT-NUMBER
           DIVIDE WS-MONTH-INDEX BY 12 GIVING WS-QUOT
                                       REMAINDER WS-REM
           IF WS-QUOT < WS-MIN-YEAR OR WS-QUOT > WS-MAX-YEAR
              PERFORM 3900-RESULT-OUT-OF-RANGE
           END-IF
           MOVE WS-QUOT                TO WS-YEAR
           COMPUTE WS-MONTH = WS-REM + 1
           MOVE WS-YMD-DAY             TO WS-DAY
           PERFORM 5200-DAYS-IN-MONTH
           IF WS-DAY > WS-DIM
              MOVE WS-DIM              TO WS-DAY
           END-IF
           MOVE WS-YEAR                TO WS-YMD-YEAR
           MOVE WS-MONTH               TO WS-YMD-MONTH
           MOVE WS-DAY                 TO WS-YMD-DAY
           COMPUTE WS-RES-INT = FUNCTION INTEGER-OF-DATE(WS-YMD)
           PERFORM 5000-INT-TO-RESULT.

      ******************************************************************
      * BR-CLM-DTE-007 Signed difference DATE-2 minus DATE-1; a blank
      * DATE-2 means "as at the business date" (e.g. claim age).
       2300-DIFFERENCE.
           PERFORM 1100-DEFAULT-DATE-1
           PERFORM 3100-VALIDATE-DATE-1
           IF CA-DT-DATE-2 = SPACES
              PERFORM 1200-RESOLVE-BUS-DATE
              MOVE CA-BUS-DATE         TO CA-DT-DATE-2
           END-IF
           MOVE CA-DT-DATE-2           TO WS-CHK-ISO
           PERFORM 3000-CHECK-DATE
           IF NOT WS-DATE-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE WS-CHK-REASON       TO CA-REASON-CODE
              STRING 'Date-2 ' CA-DT-DATE-2 ': ' WS-CHK-TEXT
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-CHK-INT             TO WS-INT-2
           COMPUTE CA-DT-RESULT-NUM = WS-INT-2 - WS-INT-1.

      ******************************************************************
       2400-DAY-OF-WEEK.
           PERFORM 1100-DEFAULT-DATE-1
           PERFORM 3100-VALIDATE-DATE-1
           MOVE WS-INT-1               TO WS-RES-INT
           PERFORM 5300-DAY-OF-WEEK
           MOVE WS-DOW                 TO CA-DT-RESULT-NUM
           MOVE CA-DT-DATE-1           TO CA-DT-RESULT-DATE.

      ******************************************************************
      * BR-CLM-DTE-010 DATE-1 if it is a business day, otherwise the
      * next one. CA-DT-NUMBER is ignored (callers count business days
      * by stepping DTADDD 1 + DTBUSD). RESULT-NUM returns the
      * calendar days rolled forward.
       2500-BUSINESS-DAY.
           PERFORM 1100-DEFAULT-DATE-1
           PERFORM 3100-VALIDATE-DATE-1
           MOVE WS-INT-1               TO WS-RES-INT
           MOVE 0                      TO WS-NONBUS-RUN
           PERFORM 5400-CHECK-BUSINESS-DAY
           PERFORM UNTIL WS-IS-BUSDAY
              PERFORM 5500-STEP-ONE-DAY
           END-PERFORM
           COMPUTE CA-DT-RESULT-NUM = WS-RES-INT - WS-INT-1
           PERFORM 5000-INT-TO-RESULT.

      ******************************************************************
       2600-END-OF-MONTH.
           PERFORM 1100-DEFAULT-DATE-1
           PERFORM 3100-VALIDATE-DATE-1
           MOVE WS-YMD-YEAR            TO WS-YEAR
           MOVE WS-YMD-MONTH           TO WS-MONTH
           PERFORM 5200-DAYS-IN-MONTH
           MOVE WS-DIM                 TO WS-YMD-DAY
           COMPUTE WS-RES-INT = FUNCTION INTEGER-OF-DATE(WS-YMD)
           PERFORM 5000-INT-TO-RESULT.

      ******************************************************************
      * BR-CLM-DTE-014 Validation is an answer, not a failure: an
      * invalid date returns RC 04 with the specific reason. A blank
      * date is not defaulted here.
       2700-VALIDATE.
           MOVE CA-DT-DATE-1           TO WS-CHK-ISO
           PERFORM 3000-CHECK-DATE
           IF WS-DATE-OK
              MOVE 'Y'                 TO CA-DT-VALID
              MOVE CA-DT-DATE-1        TO CA-DT-RESULT-DATE
           ELSE
              MOVE 'N'                 TO CA-DT-VALID
              SET CA-RC-WARNING        TO TRUE
              MOVE WS-CHK-REASON       TO CA-REASON-CODE
              STRING 'Date ' CA-DT-DATE-1 ': ' WS-CHK-TEXT
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * Validate WS-CHK-ISO. Sets WS-CHK-OK, reason/text and, when
      * valid, WS-YMD and WS-CHK-INT (Gregorian day number).
      * BR-CLM-DTE-001 ISO yyyy-mm-dd with hyphens
       3000-CHECK-DATE.
           MOVE 'N'                    TO WS-CHK-OK
           MOVE SPACES                 TO WS-CHK-TEXT
           MOVE 'DTFORMAT'             TO WS-CHK-REASON
           EVALUATE TRUE
              WHEN WS-CHK-ISO = SPACES
                 MOVE 'DTBLANK '       TO WS-CHK-REASON
                 MOVE 'date is blank'  TO WS-CHK-TEXT
              WHEN WS-CHK-SEP1 NOT = '-' OR WS-CHK-SEP2 NOT = '-'
                 MOVE 'not in yyyy-mm-dd format' TO WS-CHK-TEXT
              WHEN WS-CHK-YYYY IS NOT NUMERIC
                OR WS-CHK-MM   IS NOT NUMERIC
                OR WS-CHK-DD   IS NOT NUMERIC
                 MOVE 'year, month or day not numeric'
                                       TO WS-CHK-TEXT
              WHEN OTHER
                 MOVE WS-CHK-YYYY      TO WS-YEAR
                 MOVE WS-CHK-MM        TO WS-MONTH
                 MOVE WS-CHK-DD        TO WS-DAY
                 PERFORM 3050-CHECK-RANGES
           END-EVALUATE.

      ******************************************************************
       3050-CHECK-RANGES.
      * BR-CLM-DTE-002 Supported years 1601..9999
           IF WS-YEAR < WS-MIN-YEAR
              MOVE 'DTYEAR  '          TO WS-CHK-REASON
              MOVE 'year before 1601'  TO WS-CHK-TEXT
           ELSE
              IF WS-MONTH < 1 OR WS-MONTH > 12
                 MOVE 'DTMONTH '       TO WS-CHK-REASON
                 MOVE 'month not 01-12' TO WS-CHK-TEXT
              ELSE
                 PERFORM 5200-DAYS-IN-MONTH
                 IF WS-DAY < 1 OR WS-DAY > WS-DIM
                    MOVE 'DTDAY   '    TO WS-CHK-REASON
                    MOVE 'day not valid for the month'
                                       TO WS-CHK-TEXT
                 ELSE
                    MOVE 'Y'           TO WS-CHK-OK
                    MOVE SPACES        TO WS-CHK-REASON
                    MOVE WS-YEAR       TO WS-YMD-YEAR
                    MOVE WS-MONTH      TO WS-YMD-MONTH
                    MOVE WS-DAY        TO WS-YMD-DAY
                    COMPUTE WS-CHK-INT =
                            FUNCTION INTEGER-OF-DATE(WS-YMD)
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
      * DATE-1 must be valid for every arithmetic request
       3100-VALIDATE-DATE-1.
           MOVE CA-DT-DATE-1           TO WS-CHK-ISO
           PERFORM 3000-CHECK-DATE
           IF NOT WS-DATE-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE WS-CHK-REASON       TO CA-REASON-CODE
              STRING 'Date-1 ' CA-DT-DATE-1 ': ' WS-CHK-TEXT
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'Y'                    TO CA-DT-VALID
           MOVE WS-CHK-INT             TO WS-INT-1.

      ******************************************************************
      * BR-CLM-DTE-013 Results must stay inside 1601-01-01..9999-12-31
       3900-RESULT-OUT-OF-RANGE.
           SET CA-RC-VALIDATION        TO TRUE
           MOVE 'DTRANGE '             TO CA-REASON-CODE
           MOVE 'Resulting date outside 1601-01-01 to 9999-12-31'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
      * Day number WS-RES-INT -> CA-DT-RESULT-DATE (ISO)
       5000-INT-TO-RESULT.
           IF WS-RES-INT < WS-MIN-INT OR WS-RES-INT > WS-MAX-INT
              PERFORM 3900-RESULT-OUT-OF-RANGE
           END-IF
           COMPUTE WS-YMD = FUNCTION DATE-OF-INTEGER(WS-RES-INT)
           STRING WS-YMD-YEAR '-' WS-YMD-MONTH '-' WS-YMD-DAY
                  DELIMITED BY SIZE INTO CA-DT-RESULT-DATE.

      ******************************************************************
      * BR-CLM-DTE-003 Gregorian leap year
       5100-LEAP-YEAR.
           MOVE 'N'                    TO WS-LEAP
           DIVIDE WS-YEAR BY 4 GIVING WS-QUOT REMAINDER WS-REM
           IF WS-REM = 0
              MOVE 'Y'                 TO WS-LEAP
              DIVIDE WS-YEAR BY 100 GIVING WS-QUOT REMAINDER WS-REM
              IF WS-REM = 0
                 MOVE 'N'              TO WS-LEAP
                 DIVIDE WS-YEAR BY 400 GIVING WS-QUOT
                                       REMAINDER WS-REM
                 IF WS-REM = 0
                    MOVE 'Y'           TO WS-LEAP
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
      * Length of WS-MONTH in WS-YEAR -> WS-DIM
       5200-DAYS-IN-MONTH.
           MOVE WS-MD-DAYS(WS-MONTH)   TO WS-DIM
           IF WS-MONTH = 2
              PERFORM 5100-LEAP-YEAR
              IF WS-IS-LEAP
                 MOVE 29               TO WS-DIM
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-DTE-008 Day number 1 (1601-01-01) was a Monday, so
      * ISO weekday = MOD(day number - 1, 7) + 1  (1=Mon .. 7=Sun)
       5300-DAY-OF-WEEK.
           COMPUTE WS-QUOT = WS-RES-INT - 1
           DIVIDE WS-QUOT BY 7 GIVING WS-QUOT REMAINDER WS-REM
           COMPUTE WS-DOW = WS-REM + 1.

      ******************************************************************
      * BR-CLM-DTE-009 Weekends and active HOLIDAY rows are not
      * business days. Sets WS-BUSDAY for day number WS-RES-INT.
       5400-CHECK-BUSINESS-DAY.
           MOVE 'Y'                    TO WS-BUSDAY
           PERFORM 5300-DAY-OF-WEEK
           IF WS-DOW > 5
              MOVE 'N'                 TO WS-BUSDAY
           ELSE
              COMPUTE WS-YMD = FUNCTION DATE-OF-INTEGER(WS-RES-INT)
              IF WS-YMD-YEAR NOT = WS-HOL-YEAR
                 PERFORM 4000-LOAD-HOLIDAYS
                 COMPUTE WS-YMD = FUNCTION DATE-OF-INTEGER(WS-RES-INT)
              END-IF
              SET WS-HOL-IX            TO 1
              SEARCH WS-HOL-ENTRY
                 AT END
                    CONTINUE
                 WHEN WS-HOL-IX > WS-HOL-COUNT
                    CONTINUE
                 WHEN WS-HOL-ENTRY(WS-HOL-IX) = WS-YMD
                    MOVE 'N'           TO WS-BUSDAY
              END-SEARCH
           END-IF.

      ******************************************************************
      * Move WS-RES-INT one calendar day forward and classify it.
      * BR-CLM-DTE-011 Guard against a corrupt holiday table: no real
      * calendar has a month of consecutive non-business days.
       5500-STEP-ONE-DAY.
           ADD 1                       TO WS-RES-INT
           IF WS-RES-INT < WS-MIN-INT OR WS-RES-INT > WS-MAX-INT
              PERFORM 3900-RESULT-OUT-OF-RANGE
           END-IF
           PERFORM 5400-CHECK-BUSINESS-DAY
           IF WS-IS-BUSDAY
              MOVE 0                   TO WS-NONBUS-RUN
           ELSE
              ADD 1                    TO WS-NONBUS-RUN
              IF WS-NONBUS-RUN > WS-MAX-NONBUS-RUN
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'DTHOLCFG'       TO CA-REASON-CODE
                 MOVE '31+ non-business days in a row: check HOLIDAY'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-DTE-012 Load the active HOLIDAY rows of one year into
      * the cache. CODE_VALUE holds the date as yyyymmdd; rows that are
      * not a valid date in that form are skipped with a warning line.
       4000-LOAD-HOLIDAYS.
           MOVE WS-YMD-YEAR            TO WS-HOL-YEAR
           MOVE 0                      TO WS-HOL-COUNT
           MOVE SPACES                 TO WS-HOL-FROM WS-HOL-TO
           STRING WS-HOL-YEAR '0101' DELIMITED BY SIZE
                  INTO WS-HOL-FROM
           STRING WS-HOL-YEAR '1231' DELIMITED BY SIZE
                  INTO WS-HOL-TO
           MOVE 'N'                    TO WS-HOL-EOF
           EXEC SQL OPEN HOLCUR END-EXEC
           IF SQLCODE NOT = 0
              MOVE WS-HOL-FROM         TO WS-ERR-KEY
              MOVE '4000-LOAD-HOLIDAYS' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           PERFORM 4100-FETCH-HOLIDAY UNTIL WS-HOL-END
           EXEC SQL CLOSE HOLCUR END-EXEC.

      ******************************************************************
       4100-FETCH-HOLIDAY.
           EXEC SQL
              FETCH HOLCUR INTO :WS-HOL-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 PERFORM 4200-STORE-HOLIDAY
              WHEN 100
                 MOVE 'Y'              TO WS-HOL-EOF
              WHEN OTHER
                 MOVE 'Y'              TO WS-HOL-EOF
                 EXEC SQL CLOSE HOLCUR END-EXEC
                 MOVE 0                TO WS-HOL-YEAR
                 MOVE WS-HOL-FROM      TO WS-ERR-KEY
                 MOVE '4100-FETCH-HOLIDAY' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-STORE-HOLIDAY.
           IF WS-HOL-CODE-YMD IS NUMERIC
           AND WS-HOL-CODE-REST = SPACES
              MOVE SPACES              TO WS-CHK-ISO
              STRING WS-HOL-CODE-YMD(1:4) '-' WS-HOL-CODE-YMD(5:2)
                     '-' WS-HOL-CODE-YMD(7:2)
                     DELIMITED BY SIZE INTO WS-CHK-ISO
              PERFORM 3000-CHECK-DATE
           ELSE
              MOVE 'N'                 TO WS-CHK-OK
           END-IF
           IF NOT WS-DATE-OK
              DISPLAY 'CLMDATES W HOLIDAY row ignored, bad date: '
                      WS-HOL-CODE
           ELSE
              IF WS-HOL-COUNT < WS-HOL-MAX
                 ADD 1                 TO WS-HOL-COUNT
                 MOVE WS-YMD           TO WS-HOL-ENTRY(WS-HOL-COUNT)
              ELSE
                 DISPLAY 'CLMDATES W more than 40 holidays in '
                         WS-HOL-YEAR ', ignored: ' WS-HOL-CODE
              END-IF
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
           END-IF
           STRING 'CLMDATES Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
