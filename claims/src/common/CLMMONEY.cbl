       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMMONEY.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMMONEY                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Money arithmetic in GBP pence precision. One     *
      *               home for splits, percentage (basis point) rates  *
      *               and pro-rata apportionment so every caller       *
      *               rounds the same way: structured settlement       *
      *               instalments, reinsurance cessions, liability     *
      *               apportionment, deductible and VAT calculations.  *
      * REQUESTS    : MNSPLIT  TOTAL into COUNT parts, DOWNPAY-PCT on  *
      *                        part 1, remainder carried on part 1;    *
      *                        RESULT = the regular part amount        *
      *               MNRATE   RESULT = TOTAL * RATE-BPS / 10000;      *
      *                        PART(1) = RESULT, PART(2) = TOTAL -     *
      *                        RESULT (e.g. ceded / retained)          *
      *               MNPRORA  RESULT = TOTAL * DAYS-PART / DAYS-WHOLE *
      *                        PART(1) = RESULT, PART(2) = complement  *
      * CALLED BY   : CLMFINTX, CLMRECOV, CLMLITIG, batch RI cession   *
      * CALLS       : none                                             *
      * TABLES      : none                                             *
      * RULES       : BR-CLM-MNY-001 Split count 1..24                 *
      *               BR-CLM-MNY-002 Down-payment percent 0..100       *
      *               BR-CLM-MNY-003 Regular parts are truncated to    *
      *                              the penny; the rounding remainder *
      *                              is carried on the first part      *
      *               BR-CLM-MNY-004 Parts always sum exactly to TOTAL *
      *               BR-CLM-MNY-005 Rounding is half away from zero,  *
      *                              so a charge and its reversal      *
      *                              mirror exactly                    *
      *               BR-CLM-MNY-006 Rates are basis points 0..9999    *
      *               BR-CLM-MNY-007 Pro-rata needs DAYS-WHOLE > 0 and *
      *                              0 <= DAYS-PART <= DAYS-WHOLE      *
      *               BR-CLM-MNY-008 Result and complement always add  *
      *                              back to TOTAL                     *
      *               BR-CLM-MNY-009 Arithmetic overflow is rejected,  *
      *                              never truncated silently          *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMMONEY'.

       01  WS-CONSTANTS.
           05 WS-MAX-PARTS             PIC S9(4) COMP VALUE 24.
           05 WS-MAX-BPS               PIC S9(4) COMP VALUE 9999.

       01  WS-WORK.
           05 WS-ABS-TOTAL             PIC S9(11)V99 COMP-3.
           05 WS-DOWNPAY               PIC S9(11)V99 COMP-3.
           05 WS-REST                  PIC S9(11)V99 COMP-3.
           05 WS-REGULAR               PIC S9(11)V99 COMP-3.
           05 WS-REMAINDER             PIC S9(11)V99 COMP-3.
           05 WS-ALLOCATED             PIC S9(11)V99 COMP-3.
           05 WS-CHECK-SUM             PIC S9(13)V99 COMP-3.
           05 WS-REG-COUNT             PIC S9(4) COMP.
           05 WS-SIGN                  PIC S9    VALUE +1.
           05 WS-IX                    PIC S9(4) COMP.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCMNY.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'MNSPLIT '
                 PERFORM 2100-SPLIT
              WHEN 'MNRATE  '
                 PERFORM 2200-RATE
              WHEN 'MNPRORA '
                 PERFORM 2300-PRO-RATA
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMMONEY: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-MN-RESULT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-MAX-PARTS
              MOVE 0                   TO CA-MN-PART(WS-IX)
           END-PERFORM.

      ******************************************************************
      * The split is computed on the absolute total and the sign is
      * re-applied, so a negative total (refund / reversal) splits into
      * exactly the mirror image of the positive one (BR-CLM-MNY-005).
       2100-SPLIT.
           PERFORM 3100-VALIDATE-SPLIT
           IF CA-MN-TOTAL < 0
              MOVE -1                  TO WS-SIGN
              COMPUTE WS-ABS-TOTAL = 0 - CA-MN-TOTAL
           ELSE
              MOVE +1                  TO WS-SIGN
              MOVE CA-MN-TOTAL         TO WS-ABS-TOTAL
           END-IF
           MOVE 0                      TO WS-DOWNPAY
                                          WS-REGULAR
                                          WS-REMAINDER
           EVALUATE TRUE
              WHEN CA-MN-COUNT = 1
                 MOVE WS-ABS-TOTAL     TO CA-MN-PART(1)
                 MOVE WS-ABS-TOTAL     TO WS-REGULAR
              WHEN CA-MN-DOWNPAY-PCT > 0
                 PERFORM 5100-SPLIT-WITH-DOWNPAY
              WHEN OTHER
                 PERFORM 5200-SPLIT-EQUAL
           END-EVALUATE
           PERFORM 5300-APPLY-SIGN
           PERFORM 5400-CHECK-SPLIT-TOTAL.

      ******************************************************************
      * BR-CLM-MNY-006 RESULT = TOTAL * RATE-BPS / 10000 (rounded)
       2200-RATE.
           IF CA-MN-RATE-BPS < 0 OR CA-MN-RATE-BPS > WS-MAX-BPS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNRATE  '          TO CA-REASON-CODE
              MOVE 'Rate must be 0 to 9999 basis points'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-MNY-005 Half away from zero (COBOL ROUNDED default)
           COMPUTE CA-MN-RESULT ROUNDED =
                   CA-MN-TOTAL * CA-MN-RATE-BPS / 10000
              ON SIZE ERROR
                 PERFORM 3900-OVERFLOW
           END-COMPUTE
           PERFORM 5500-COMPLEMENT.

      ******************************************************************
      * BR-CLM-MNY-007 Pro-rata of TOTAL for DAYS-PART of DAYS-WHOLE
       2300-PRO-RATA.
           IF CA-MN-DAYS-WHOLE NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNDAYSWH'          TO CA-REASON-CODE
              MOVE 'Pro-rata whole period must be at least 1 day'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-MN-DAYS-PART < 0
           OR CA-MN-DAYS-PART > CA-MN-DAYS-WHOLE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNDAYSPT'          TO CA-REASON-CODE
              MOVE 'Pro-rata part days must be 0 to the whole period'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE CA-MN-RESULT ROUNDED =
                   CA-MN-TOTAL * CA-MN-DAYS-PART / CA-MN-DAYS-WHOLE
              ON SIZE ERROR
                 PERFORM 3900-OVERFLOW
           END-COMPUTE
           PERFORM 5500-COMPLEMENT.

      ******************************************************************
       3100-VALIDATE-SPLIT.
      * BR-CLM-MNY-001 Split count 1..24
           IF CA-MN-COUNT < 1 OR CA-MN-COUNT > WS-MAX-PARTS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNCOUNT '          TO CA-REASON-CODE
              MOVE 'Split count must be 1 to 24'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-MNY-002 Down-payment percent 0..100
           IF CA-MN-DOWNPAY-PCT < 0 OR CA-MN-DOWNPAY-PCT > 100
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNDOWNPC'          TO CA-REASON-CODE
              MOVE 'Down-payment percent must be 0 to 100'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-MNY-009 Never truncate an amount silently
       3900-OVERFLOW.
           SET CA-RC-VALIDATION        TO TRUE
           MOVE 'MNOVRFLW'             TO CA-REASON-CODE
           MOVE 'Amount overflow - result exceeds 11 integer digits'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
      * Part 1 = DOWNPAY-PCT of the total (rounded); the rest is shared
      * by parts 2..COUNT, truncated to the penny.
      * BR-CLM-MNY-003 Remainder pennies go on the first part
       5100-SPLIT-WITH-DOWNPAY.
           COMPUTE WS-DOWNPAY ROUNDED =
                   WS-ABS-TOTAL * CA-MN-DOWNPAY-PCT / 100
           COMPUTE WS-REST = WS-ABS-TOTAL - WS-DOWNPAY
           COMPUTE WS-REG-COUNT = CA-MN-COUNT - 1
           COMPUTE WS-REGULAR = WS-REST / WS-REG-COUNT
           COMPUTE WS-ALLOCATED = WS-REGULAR * WS-REG-COUNT
           COMPUTE WS-REMAINDER = WS-REST - WS-ALLOCATED
           COMPUTE CA-MN-PART(1) = WS-DOWNPAY + WS-REMAINDER
           PERFORM VARYING WS-IX FROM 2 BY 1
                   UNTIL WS-IX > CA-MN-COUNT
              MOVE WS-REGULAR          TO CA-MN-PART(WS-IX)
           END-PERFORM.

      ******************************************************************
      * Equal parts truncated to the penny; part 1 absorbs the pennies
      * left over (BR-CLM-MNY-003).
       5200-SPLIT-EQUAL.
           COMPUTE WS-REGULAR = WS-ABS-TOTAL / CA-MN-COUNT
           COMPUTE WS-ALLOCATED = WS-REGULAR * CA-MN-COUNT
           COMPUTE WS-REMAINDER = WS-ABS-TOTAL - WS-ALLOCATED
           COMPUTE CA-MN-PART(1) = WS-REGULAR + WS-REMAINDER
           PERFORM VARYING WS-IX FROM 2 BY 1
                   UNTIL WS-IX > CA-MN-COUNT
              MOVE WS-REGULAR          TO CA-MN-PART(WS-IX)
           END-PERFORM.

      ******************************************************************
       5300-APPLY-SIGN.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > CA-MN-COUNT
              COMPUTE CA-MN-PART(WS-IX) = CA-MN-PART(WS-IX) * WS-SIGN
           END-PERFORM
           COMPUTE CA-MN-RESULT = WS-REGULAR * WS-SIGN.

      ******************************************************************
      * BR-CLM-MNY-004 Control total: the parts must add back to TOTAL
       5400-CHECK-SPLIT-TOTAL.
           MOVE 0                      TO WS-CHECK-SUM
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > CA-MN-COUNT
              ADD CA-MN-PART(WS-IX)    TO WS-CHECK-SUM
           END-PERFORM
           IF WS-CHECK-SUM NOT = CA-MN-TOTAL
              SET CA-RC-STATE          TO TRUE
              MOVE 'MNBALERR'          TO CA-REASON-CODE
              MOVE 'Split parts do not add back to the total'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * BR-CLM-MNY-008 PART(1) = RESULT, PART(2) = TOTAL - RESULT, so a
      * caller booking both sides (ceded/retained, earned/unearned)
      * never loses a penny to rounding.
       5500-COMPLEMENT.
           MOVE CA-MN-RESULT           TO CA-MN-PART(1)
           COMPUTE CA-MN-PART(2) = CA-MN-TOTAL - CA-MN-RESULT.

      ******************************************************************
       9000-RETURN.
           GOBACK.
