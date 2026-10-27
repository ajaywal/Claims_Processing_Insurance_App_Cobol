       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILDATES.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILDATES                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Date arithmetic for billing schedules, due       *
      *               dates, aging and delinquency timelines. All      *
      *               dates are ISO yyyy-mm-dd; arithmetic is done on  *
      *               integer day numbers (FUNCTION INTEGER-OF-DATE /  *
      *               DATE-OF-INTEGER) so month and leap-year edges    *
      *               are exact.                                       *
      * REQUESTS    : DTADDD  date-1 + NUMBER days                     *
      *               DTADDM  date-1 + NUMBER months (EOM clamped)     *
      *               DTDIFF  date-2 minus date-1 in days              *
      *               DTDOW   day of week of date-1 (1=Mon..7=Sun)     *
      *               DTBUSD  roll date-1 forward to a business day;   *
      *                       NUMBER > 0 then advances that many       *
      *                       further business days                    *
      *               DTEOM   last day of date-1's month               *
      *               DTVALD  validate date-1                          *
      * CALLED BY   : schedule, invoicing, payment, delinquency and    *
      *               disbursement services and batch drivers          *
      * CALLS       : BILERRLG                                         *
      * TABLES      : BIL_CONTROL (R), BIL_CALENDAR (R)                *
      * RULES       : BR-BIL-DTE-001 Dates are yyyy-mm-dd with numeric *
      *                              parts and '-' separators          *
      *               BR-BIL-DTE-002 Supported years 1900-2199; any    *
      *                              result outside is rejected        *
      *               BR-BIL-DTE-003 Gregorian leap years: divisible   *
      *                              by 4, except centuries not        *
      *                              divisible by 400                  *
      *               BR-BIL-DTE-004 Blank date-1 / date-2 default to  *
      *                              the business date (CA-BUS-DATE,   *
      *                              else BIL_CONTROL 'SYSTEM  ')      *
      *                              except for DTVALD                 *
      *               BR-BIL-DTE-005 Adding months keeps the day of    *
      *                              month, clamped to the last day of *
      *                              a shorter target month            *
      *               BR-BIL-DTE-006 Business days exclude Saturday,   *
      *                              Sunday and BIL_CALENDAR rows with *
      *                              HOLIDAY_FLAG = 'Y'                *
      *               BR-BIL-DTE-007 A business-day advance is limited *
      *                              to 0..250 days                    *
      *               BR-BIL-DTE-008 Holidays are cached per run unit  *
      *                              for the requested date window     *
      *               BR-BIL-DTE-009 Day of week is derived from day   *
      *                              number 1 (1601-01-01) = Monday    *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-LIMITS.
           05 WS-MIN-YEAR              PIC 9(4)  VALUE 1900.
           05 WS-MAX-YEAR              PIC 9(4)  VALUE 2199.
           05 WS-MAX-BUS-DAYS          PIC S9(4) COMP VALUE 250.
           05 WS-HOL-MAX               PIC S9(4) COMP VALUE 120.

       01  WS-MONTH-DAYS-DATA.
           05 FILLER                   PIC X(24)
                                       VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TABLE REDEFINES WS-MONTH-DAYS-DATA.
           05 WS-MDAYS                 PIC 99 OCCURS 12 TIMES.

       01  WS-VAL-DATE.
           05 WS-VD-YYYY               PIC X(4).
           05 WS-VD-SEP1               PIC X.
           05 WS-VD-MM                 PIC X(2).
           05 WS-VD-SEP2               PIC X.
           05 WS-VD-DD                 PIC X(2).

       01  WS-VAL-RESULT.
           05 WS-VAL-OK                PIC X     VALUE 'N'.
              88 WS-VAL-IS-OK                    VALUE 'Y'.
           05 WS-VAL-INT               PIC S9(9) COMP VALUE 0.
           05 WS-VAL-REASON            PIC X(8).
           05 WS-VAL-MSG               PIC X(60).

       01  WS-CALC.
           05 WS-CALC-YYYY             PIC 9(4).
           05 WS-CALC-MM               PIC 9(2).
           05 WS-CALC-DD               PIC 9(2).
           05 WS-DIM                   PIC 9(2).
           05 WS-QUOT                  PIC S9(9) COMP.
           05 WS-REM                   PIC S9(9) COMP.
           05 WS-REM-4                 PIC S9(4) COMP.
           05 WS-REM-100               PIC S9(4) COMP.
           05 WS-REM-400               PIC S9(4) COMP.
           05 WS-INT-1                 PIC S9(9) COMP.
           05 WS-INT-2                 PIC S9(9) COMP.
           05 WS-CALC-INT              PIC S9(9) COMP.
           05 WS-TOTAL-MONTHS          PIC S9(9) COMP.
           05 WS-DOW                   PIC S9(4) COMP.
           05 WS-BUS-COUNT             PIC S9(4) COMP.
           05 WS-BUSDAY-FLAG           PIC X.
              88 WS-IS-BUSINESS-DAY              VALUE 'Y'.
           05 WS-CONV-INT              PIC S9(9) COMP.
           05 WS-CONV-DATE             PIC X(10).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-SAVE-SQLCODE          PIC S9(9) COMP.

       01  WS-YMD                      PIC 9(8).
       01  WS-YMD-PARTS REDEFINES WS-YMD.
           05 WS-YMD-YYYY              PIC 9(4).
           05 WS-YMD-MM                PIC 9(2).
           05 WS-YMD-DD                PIC 9(2).

      * BR-BIL-DTE-008 Holiday window cache (kept across CALLs)
       01  WS-HOL-CACHE.
           05 WS-HOL-LOADED            PIC X     VALUE 'N'.
              88 WS-HOL-IS-LOADED                VALUE 'Y'.
           05 WS-HOL-FROM-INT          PIC S9(9) COMP VALUE 0.
           05 WS-HOL-TO-INT            PIC S9(9) COMP VALUE 0.
           05 WS-HOL-COUNT             PIC S9(4) COMP VALUE 0.
           05 WS-HOL-IX                PIC S9(4) COMP VALUE 0.
           05 WS-HOL-INT               PIC S9(9) COMP
                                       OCCURS 120 TIMES.
       01  WS-HOL-REQ-FROM             PIC S9(9) COMP.
       01  WS-HOL-REQ-TO               PIC S9(9) COMP.
       01  WS-HOL-FROM-DATE            PIC X(10).
       01  WS-HOL-TO-DATE              PIC X(10).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.
           EXEC SQL INCLUDE BILDCAL END-EXEC.

           EXEC SQL DECLARE CSR-HOLIDAY CURSOR FOR
              SELECT CAL_DATE
                FROM BILDB.BIL_CALENDAR
               WHERE CAL_DATE BETWEEN :WS-HOL-FROM-DATE
                                  AND :WS-HOL-TO-DATE
                 AND HOLIDAY_FLAG = 'Y'
               ORDER BY CAL_DATE
                 FOR FETCH ONLY
           END-EXEC.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCDTE.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'DTADDD  '
                 PERFORM 2000-ADD-DAYS
              WHEN 'DTADDM  '
                 PERFORM 2100-ADD-MONTHS
              WHEN 'DTDIFF  '
                 PERFORM 2200-DIFFERENCE
              WHEN 'DTDOW   '
                 PERFORM 2300-DAY-OF-WEEK
              WHEN 'DTBUSD  '
                 PERFORM 2400-BUSINESS-DAY
              WHEN 'DTEOM   '
                 PERFORM 2500-END-OF-MONTH
              WHEN 'DTVALD  '
                 PERFORM 2600-VALIDATE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILDATES: unknown request '
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
           MOVE 'N'                    TO CA-DT-VALID.

      ******************************************************************
       2000-ADD-DAYS.
           PERFORM 3100-LOAD-DATE-1
           COMPUTE WS-CALC-INT = WS-INT-1 + CA-DT-NUMBER
           PERFORM 5300-RETURN-CALC-INT
           MOVE CA-DT-NUMBER           TO CA-DT-RESULT-NUM.

      ******************************************************************
      * BR-BIL-DTE-005 Same day of month, clamped to month end
       2100-ADD-MONTHS.
           PERFORM 3100-LOAD-DATE-1
           MOVE CA-DT-DATE-1(1:4)      TO WS-CALC-YYYY
           MOVE CA-DT-DATE-1(6:2)      TO WS-CALC-MM
           MOVE CA-DT-DATE-1(9:2)      TO WS-CALC-DD
           COMPUTE WS-TOTAL-MONTHS = WS-CALC-YYYY * 12
                                   + WS-CALC-MM - 1
                                   + CA-DT-NUMBER
           DIVIDE WS-TOTAL-MONTHS BY 12
                  GIVING WS-QUOT REMAINDER WS-REM
      * BR-BIL-DTE-002 Result year must stay in range
           IF WS-TOTAL-MONTHS < 0
           OR WS-QUOT < WS-MIN-YEAR OR WS-QUOT > WS-MAX-YEAR
              PERFORM 3800-REJECT-RANGE
           END-IF
           MOVE WS-QUOT                TO WS-CALC-YYYY
           COMPUTE WS-CALC-MM = WS-REM + 1
           PERFORM 5000-DAYS-IN-MONTH
           IF WS-CALC-DD > WS-DIM
              MOVE WS-DIM              TO WS-CALC-DD
              MOVE 'DTCLAMP '          TO CA-REASON-CODE
              MOVE 'Day of month clamped to end of target month'
                                       TO CA-MESSAGE
           END-IF
           COMPUTE WS-YMD = WS-CALC-YYYY * 10000
                          + WS-CALC-MM * 100 + WS-CALC-DD
           COMPUTE WS-CALC-INT = FUNCTION INTEGER-OF-DATE(WS-YMD)
           PERFORM 5300-RETURN-CALC-INT
           COMPUTE CA-DT-RESULT-NUM = WS-CALC-INT - WS-INT-1.

      ******************************************************************
       2200-DIFFERENCE.
           PERFORM 3100-LOAD-DATE-1
           PERFORM 3150-LOAD-DATE-2
           COMPUTE CA-DT-RESULT-NUM = WS-INT-2 - WS-INT-1.

      ******************************************************************
       2300-DAY-OF-WEEK.
           PERFORM 3100-LOAD-DATE-1
           MOVE WS-INT-1               TO WS-CALC-INT
           PERFORM 5200-DAY-OF-WEEK
           MOVE WS-DOW                 TO CA-DT-RESULT-NUM
           MOVE CA-DT-DATE-1           TO CA-DT-RESULT-DATE.

      ******************************************************************
      * BR-BIL-DTE-006 Roll forward over weekends and holidays, then
      * advance NUMBER further business days
       2400-BUSINESS-DAY.
           PERFORM 3100-LOAD-DATE-1
      * BR-BIL-DTE-007 Business-day advance 0..250
           IF CA-DT-NUMBER < 0 OR CA-DT-NUMBER > WS-MAX-BUS-DAYS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DTBUSRNG'          TO CA-REASON-CODE
              MOVE 'Business days to add must be 0 to 250'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-INT-1               TO WS-HOL-REQ-FROM
           COMPUTE WS-HOL-REQ-TO = WS-INT-1 + (CA-DT-NUMBER * 2) + 31
           PERFORM 4100-LOAD-HOLIDAYS
           MOVE WS-INT-1               TO WS-CALC-INT
           PERFORM 5400-CHECK-BUSINESS-DAY
           PERFORM UNTIL WS-IS-BUSINESS-DAY
              ADD 1                    TO WS-CALC-INT
              PERFORM 5400-CHECK-BUSINESS-DAY
           END-PERFORM
           PERFORM VARYING WS-BUS-COUNT FROM 1 BY 1
                   UNTIL WS-BUS-COUNT > CA-DT-NUMBER
              ADD 1                    TO WS-CALC-INT
              PERFORM 5400-CHECK-BUSINESS-DAY
              PERFORM UNTIL WS-IS-BUSINESS-DAY
                 ADD 1                 TO WS-CALC-INT
                 PERFORM 5400-CHECK-BUSINESS-DAY
              END-PERFORM
           END-PERFORM
           PERFORM 5300-RETURN-CALC-INT
           COMPUTE CA-DT-RESULT-NUM = WS-CALC-INT - WS-INT-1.

      ******************************************************************
       2500-END-OF-MONTH.
           PERFORM 3100-LOAD-DATE-1
           MOVE CA-DT-DATE-1(1:4)      TO WS-CALC-YYYY
           MOVE CA-DT-DATE-1(6:2)      TO WS-CALC-MM
           PERFORM 5000-DAYS-IN-MONTH
           MOVE CA-DT-DATE-1(1:8)      TO CA-DT-RESULT-DATE(1:8)
           MOVE WS-DIM                 TO CA-DT-RESULT-DATE(9:2)
           MOVE WS-DIM                 TO CA-DT-RESULT-NUM.

      ******************************************************************
      * DTVALD: no business-date default; RESULT-NUM = day of year
       2600-VALIDATE.
           IF CA-DT-DATE-1 = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'DTBLANK '          TO CA-REASON-CODE
              MOVE 'Date is required'  TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3100-LOAD-DATE-1
           MOVE CA-DT-DATE-1           TO CA-DT-RESULT-DATE
           COMPUTE WS-YMD = WS-CALC-YYYY * 10000 + 0101
           COMPUTE CA-DT-RESULT-NUM = WS-INT-1
                   - FUNCTION INTEGER-OF-DATE(WS-YMD) + 1.

      ******************************************************************
      * Validate an ISO date held in WS-VAL-DATE.
      * Out: WS-VAL-OK, WS-VAL-INT, WS-VAL-REASON, WS-VAL-MSG and
      *      WS-CALC-YYYY/MM/DD.
       3000-VALIDATE-DATE.
           MOVE 'N'                    TO WS-VAL-OK
           MOVE 0                      TO WS-VAL-INT
           MOVE SPACES                 TO WS-VAL-REASON WS-VAL-MSG
           EVALUATE TRUE
      * BR-BIL-DTE-001 Format yyyy-mm-dd
              WHEN WS-VD-SEP1 NOT = '-' OR WS-VD-SEP2 NOT = '-'
                 MOVE 'DTFORMAT'       TO WS-VAL-REASON
                 MOVE 'must be in yyyy-mm-dd format'
                                       TO WS-VAL-MSG
              WHEN WS-VD-YYYY NOT NUMERIC
                OR WS-VD-MM   NOT NUMERIC
                OR WS-VD-DD   NOT NUMERIC
                 MOVE 'DTNONNUM'       TO WS-VAL-REASON
                 MOVE 'has non-numeric year, month or day'
                                       TO WS-VAL-MSG
              WHEN OTHER
                 MOVE WS-VD-YYYY       TO WS-CALC-YYYY
                 MOVE WS-VD-MM         TO WS-CALC-MM
                 MOVE WS-VD-DD         TO WS-CALC-DD
                 PERFORM 3010-CHECK-PARTS
           END-EVALUATE.

      ******************************************************************
       3010-CHECK-PARTS.
           EVALUATE TRUE
      * BR-BIL-DTE-002 Year range 1900-2199
              WHEN WS-CALC-YYYY < WS-MIN-YEAR
                OR WS-CALC-YYYY > WS-MAX-YEAR
                 MOVE 'DTYEAR  '       TO WS-VAL-REASON
                 MOVE 'year must be 1900 to 2199'
                                       TO WS-VAL-MSG
              WHEN WS-CALC-MM < 1 OR WS-CALC-MM > 12
                 MOVE 'DTMONTH '       TO WS-VAL-REASON
                 MOVE 'month must be 01 to 12'
                                       TO WS-VAL-MSG
              WHEN OTHER
                 PERFORM 5000-DAYS-IN-MONTH
                 IF WS-CALC-DD < 1 OR WS-CALC-DD > WS-DIM
                    MOVE 'DTDAY   '    TO WS-VAL-REASON
                    MOVE 'day is not valid for the month'
                                       TO WS-VAL-MSG
                 ELSE
                    COMPUTE WS-YMD = WS-CALC-YYYY * 10000
                                   + WS-CALC-MM * 100 + WS-CALC-DD
                    COMPUTE WS-VAL-INT =
                            FUNCTION INTEGER-OF-DATE(WS-YMD)
                    MOVE 'Y'           TO WS-VAL-OK
                 END-IF
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DTE-004 Blank date-1 defaults to the business date
       3100-LOAD-DATE-1.
           IF CA-DT-DATE-1 = SPACES
              PERFORM 3200-GET-BUSINESS-DATE
              MOVE WS-CONV-DATE        TO CA-DT-DATE-1
           END-IF
           MOVE CA-DT-DATE-1           TO WS-VAL-DATE
           PERFORM 3000-VALIDATE-DATE
           MOVE WS-VAL-OK              TO CA-DT-VALID
           IF NOT WS-VAL-IS-OK
              SET CA-RC-VALIDATION     TO TRUE
              MOVE WS-VAL-REASON       TO CA-REASON-CODE
              STRING 'Date-1 ' CA-DT-DATE-1 ' ' WS-VAL-MSG
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-VAL-INT             TO WS-INT-1.

      ******************************************************************
       3150-LOAD-DATE-2.
           IF CA-DT-DATE-2 = SPACES
              PERFORM 3200-GET-BUSINESS-DATE
              MOVE WS-CONV-DATE        TO CA-DT-DATE-2
           END-IF
           MOVE CA-DT-DATE-2           TO WS-VAL-DATE
           PERFORM 3000-VALIDATE-DATE
           IF NOT WS-VAL-IS-OK
              MOVE 'N'                 TO CA-DT-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE WS-VAL-REASON       TO CA-REASON-CODE
              STRING 'Date-2 ' CA-DT-DATE-2 ' ' WS-VAL-MSG
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-VAL-INT             TO WS-INT-2.

      ******************************************************************
       3200-GET-BUSINESS-DATE.
           IF CA-BUS-DATE NOT = SPACES
              MOVE CA-BUS-DATE         TO WS-CONV-DATE
           ELSE
              PERFORM 4000-READ-CONTROL
              MOVE CTL-BUSINESS-DATE   TO WS-CONV-DATE
                                          CA-BUS-DATE
           END-IF.

      ******************************************************************
       3800-REJECT-RANGE.
           SET CA-RC-VALIDATION        TO TRUE
           MOVE 'DTRANGE '             TO CA-REASON-CODE
           MOVE 'Resulting date is outside years 1900 to 2199'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       4000-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM BILDB.BIL_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CTLNOTFD'       TO CA-REASON-CODE
                 MOVE 'BIL_CONTROL SYSTEM row missing - no bus date'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-CONTROL'  TO ERC-ER-PARAGRAPH
                 MOVE CTL-CONTROL-KEY      TO ERC-ER-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-DTE-008 Reload holidays only when the requested window
      * is not inside the cached one
       4100-LOAD-HOLIDAYS.
           IF WS-HOL-IS-LOADED
           AND WS-HOL-REQ-FROM >= WS-HOL-FROM-INT
           AND WS-HOL-REQ-TO   <= WS-HOL-TO-INT
              CONTINUE
           ELSE
              MOVE 'N'                 TO WS-HOL-LOADED
              MOVE 0                   TO WS-HOL-COUNT
              MOVE WS-HOL-REQ-FROM     TO WS-CONV-INT
              PERFORM 5100-INT-TO-ISO
              MOVE WS-CONV-DATE        TO WS-HOL-FROM-DATE
              MOVE WS-HOL-REQ-TO       TO WS-CONV-INT
              PERFORM 5100-INT-TO-ISO
              MOVE WS-CONV-DATE        TO WS-HOL-TO-DATE
              PERFORM 4110-OPEN-HOLIDAYS
              PERFORM 4120-FETCH-HOLIDAY
                 UNTIL SQLCODE = 100
                    OR WS-HOL-COUNT >= WS-HOL-MAX
              PERFORM 4130-CLOSE-HOLIDAYS
              MOVE WS-HOL-REQ-FROM     TO WS-HOL-FROM-INT
              MOVE WS-HOL-REQ-TO       TO WS-HOL-TO-INT
      *       a full table narrows the window to what was loaded
              IF WS-HOL-COUNT >= WS-HOL-MAX
                 MOVE WS-HOL-INT(WS-HOL-COUNT) TO WS-HOL-TO-INT
              END-IF
              MOVE 'Y'                 TO WS-HOL-LOADED
           END-IF.

       4110-OPEN-HOLIDAYS.
           EXEC SQL OPEN CSR-HOLIDAY END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4110-OPEN-HOLIDAYS'    TO ERC-ER-PARAGRAPH
              MOVE WS-HOL-FROM-DATE        TO ERC-ER-KEY
              PERFORM 9900-DB2-ERROR
           END-IF.

       4120-FETCH-HOLIDAY.
           EXEC SQL
              FETCH CSR-HOLIDAY INTO :CAL-CAL-DATE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE CAL-CAL-DATE(1:4) TO WS-YMD-YYYY
                 MOVE CAL-CAL-DATE(6:2) TO WS-YMD-MM
                 MOVE CAL-CAL-DATE(9:2) TO WS-YMD-DD
                 ADD 1                  TO WS-HOL-COUNT
                 COMPUTE WS-HOL-INT(WS-HOL-COUNT) =
                         FUNCTION INTEGER-OF-DATE(WS-YMD)
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE SQLCODE           TO WS-SAVE-SQLCODE
                 PERFORM 4130-CLOSE-HOLIDAYS
                 MOVE WS-SAVE-SQLCODE   TO SQLCODE
                 MOVE '4120-FETCH-HOLIDAY' TO ERC-ER-PARAGRAPH
                 MOVE WS-HOL-FROM-DATE     TO ERC-ER-KEY
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

       4130-CLOSE-HOLIDAYS.
           EXEC SQL CLOSE CSR-HOLIDAY END-EXEC.

      ******************************************************************
      * BR-BIL-DTE-003 Gregorian leap-year rule
       5000-DAYS-IN-MONTH.
           MOVE WS-MDAYS(WS-CALC-MM)   TO WS-DIM
           IF WS-CALC-MM = 2
              DIVIDE WS-CALC-YYYY BY 4   GIVING WS-QUOT
                     REMAINDER WS-REM-4
              DIVIDE WS-CALC-YYYY BY 100 GIVING WS-QUOT
                     REMAINDER WS-REM-100
              DIVIDE WS-CALC-YYYY BY 400 GIVING WS-QUOT
                     REMAINDER WS-REM-400
              IF (WS-REM-4 = 0 AND WS-REM-100 NOT = 0)
              OR WS-REM-400 = 0
                 MOVE 29               TO WS-DIM
              END-IF
           END-IF.

      ******************************************************************
       5100-INT-TO-ISO.
           COMPUTE WS-YMD = FUNCTION DATE-OF-INTEGER(WS-CONV-INT)
           STRING WS-YMD-YYYY '-' WS-YMD-MM '-' WS-YMD-DD
                  DELIMITED BY SIZE INTO WS-CONV-DATE.

      ******************************************************************
      * BR-BIL-DTE-009 Day 1 (1601-01-01) was a Monday
       5200-DAY-OF-WEEK.
           DIVIDE WS-CALC-INT BY 7 GIVING WS-QUOT REMAINDER WS-REM
           IF WS-REM = 0
              MOVE 7                   TO WS-DOW
           ELSE
              MOVE WS-REM              TO WS-DOW
           END-IF.

      ******************************************************************
      * BR-BIL-DTE-002 Range-check a computed day number, return it
       5300-RETURN-CALC-INT.
           MOVE WS-MIN-YEAR            TO WS-YMD-YYYY
           MOVE 01                     TO WS-YMD-MM WS-YMD-DD
           IF WS-CALC-INT < FUNCTION INTEGER-OF-DATE(WS-YMD)
              PERFORM 3800-REJECT-RANGE
           END-IF
           MOVE WS-MAX-YEAR            TO WS-YMD-YYYY
           MOVE 12                     TO WS-YMD-MM
           MOVE 31                     TO WS-YMD-DD
           IF WS-CALC-INT > FUNCTION INTEGER-OF-DATE(WS-YMD)
              PERFORM 3800-REJECT-RANGE
           END-IF
           MOVE WS-CALC-INT            TO WS-CONV-INT
           PERFORM 5100-INT-TO-ISO
           MOVE WS-CONV-DATE           TO CA-DT-RESULT-DATE.

      ******************************************************************
      * BR-BIL-DTE-006 Weekend or calendar holiday is not a business day
       5400-CHECK-BUSINESS-DAY.
           IF WS-CALC-INT > WS-HOL-TO-INT
              MOVE WS-CALC-INT         TO WS-HOL-REQ-FROM
              COMPUTE WS-HOL-REQ-TO = WS-CALC-INT + 62
              PERFORM 4100-LOAD-HOLIDAYS
           END-IF
           MOVE 'Y'                    TO WS-BUSDAY-FLAG
           PERFORM 5200-DAY-OF-WEEK
           IF WS-DOW > 5
              MOVE 'N'                 TO WS-BUSDAY-FLAG
           ELSE
              PERFORM VARYING WS-HOL-IX FROM 1 BY 1
                      UNTIL WS-HOL-IX > WS-HOL-COUNT
                         OR WS-HOL-INT(WS-HOL-IX) >= WS-CALC-INT
                 CONTINUE
              END-PERFORM
              IF WS-HOL-IX <= WS-HOL-COUNT
                 IF WS-HOL-INT(WS-HOL-IX) = WS-CALC-INT
                    MOVE 'N'           TO WS-BUSDAY-FLAG
                 END-IF
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
           STRING 'BILDATES Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
