       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILMONEY.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILMONEY                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Money arithmetic shared by billing: instalment   *
      *               splits, basis-point rates and day-based pro-rata *
      *               so every program rounds the same way and splits  *
      *               always add back to the original amount.         *
      * REQUESTS    : MNSPLIT  TOTAL into COUNT parts, DOWNPAY-PCT on  *
      *                        part 1; RESULT = regular instalment     *
      *               MNRATE   RESULT = TOTAL * RATE-BPS / 10000       *
      *               MNPRORA  RESULT = TOTAL * DAYS-PART / DAYS-WHOLE *
      *                        PART(1) = RESULT, PART(2) = TOTAL -     *
      *                        RESULT (earned / unearned pair)         *
      * CALLED BY   : schedule, invoicing, commission, cancellation,   *
      *               late-fee and write-off services                  *
      * CALLS       : none                                             *
      * TABLES      : none                                             *
      * RULES       : BR-BIL-MNY-001 A split has 1 to 24 parts         *
      *               BR-BIL-MNY-002 Down payment is 0 to 100 percent; *
      *                              100 percent needs a single part   *
      *               BR-BIL-MNY-003 With a down payment, part 1 is    *
      *                              the down payment and the balance  *
      *                              is spread over parts 2..COUNT;    *
      *                              without one the total is spread   *
      *                              over all parts                    *
      *               BR-BIL-MNY-004 Instalments are truncated to the  *
      *                              penny; the rounding remainder is  *
      *                              carried on part 1 so the parts    *
      *                              always sum to the total           *
      *               BR-BIL-MNY-005 A non-zero total too small to     *
      *                              give every instalment a penny is  *
      *                              rejected                          *
      *               BR-BIL-MNY-006 Negative totals (credits) split   *
      *                              and rate symmetrically            *
      *               BR-BIL-MNY-007 Rates are basis points >= 0,      *
      *                              rounded half away from zero       *
      *               BR-BIL-MNY-008 Pro-rata needs 0 <= DAYS-PART <=  *
      *                              DAYS-WHOLE and DAYS-WHOLE > 0;    *
      *                              a full period returns the total   *
      *                              exactly                           *
      *               BR-BIL-MNY-009 Arithmetic overflow is a          *
      *                              validation error, never truncated *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILMONEY'.

       01  WS-LIMITS.
           05 WS-MAX-PARTS             PIC S9(4) COMP VALUE 24.

       01  WS-WORK.
           05 WS-IX                    PIC S9(4) COMP VALUE 0.
           05 WS-INSTAL-COUNT          PIC S9(4) COMP VALUE 0.
           05 WS-OTHER-PARTS           PIC S9(4) COMP VALUE 0.
           05 WS-DOWNPAY               PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SPREAD                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-EACH                  PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SPREAD-TOTAL          PIC S9(13)V99 COMP-3 VALUE 0.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCMNY.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'MNSPLIT '
                 PERFORM 2000-SPLIT
              WHEN 'MNRATE  '
                 PERFORM 2100-RATE
              WHEN 'MNPRORA '
                 PERFORM 2200-PRO-RATA
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILMONEY: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 0                      TO CA-MN-RESULT.

      ******************************************************************
       2000-SPLIT.
           PERFORM 3000-VALIDATE-SPLIT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-MAX-PARTS
              MOVE 0                   TO CA-MN-PART(WS-IX)
           END-PERFORM
           IF CA-MN-COUNT = 1
              MOVE CA-MN-TOTAL         TO CA-MN-PART(1)
                                          CA-MN-RESULT
           ELSE
              PERFORM 5000-COMPUTE-SPLIT
           END-IF.

      ******************************************************************
       2100-RATE.
      * BR-BIL-MNY-007 Basis points, never negative
           IF CA-MN-RATE-BPS < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNRATE  '          TO CA-REASON-CODE
              MOVE 'Rate in basis points cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-MNY-006 BR-BIL-MNY-007 Symmetric half-away rounding
           COMPUTE CA-MN-RESULT ROUNDED =
                   CA-MN-TOTAL * CA-MN-RATE-BPS / 10000
              ON SIZE ERROR
                 PERFORM 3900-REJECT-OVERFLOW
           END-COMPUTE.

      ******************************************************************
       2200-PRO-RATA.
           PERFORM 3100-VALIDATE-PRO-RATA
      * BR-BIL-MNY-008 Full period and zero period are exact
           EVALUATE TRUE
              WHEN CA-MN-DAYS-PART = CA-MN-DAYS-WHOLE
                 MOVE CA-MN-TOTAL      TO CA-MN-RESULT
              WHEN CA-MN-DAYS-PART = 0
                 MOVE 0                TO CA-MN-RESULT
              WHEN OTHER
                 COMPUTE CA-MN-RESULT ROUNDED =
                         CA-MN-TOTAL * CA-MN-DAYS-PART
                                     / CA-MN-DAYS-WHOLE
                    ON SIZE ERROR
                       PERFORM 3900-REJECT-OVERFLOW
                 END-COMPUTE
           END-EVALUATE
           MOVE CA-MN-RESULT           TO CA-MN-PART(1)
           COMPUTE CA-MN-PART(2) = CA-MN-TOTAL - CA-MN-RESULT.

      ******************************************************************
       3000-VALIDATE-SPLIT.
      * BR-BIL-MNY-001 1..24 parts
           IF CA-MN-COUNT < 1 OR CA-MN-COUNT > WS-MAX-PARTS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNCOUNT '          TO CA-REASON-CODE
              MOVE 'Number of parts must be 1 to 24'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-MNY-002 Down payment percentage
           IF CA-MN-DOWNPAY-PCT < 0 OR CA-MN-DOWNPAY-PCT > 100
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNDPPCT '          TO CA-REASON-CODE
              MOVE 'Down payment percent must be 0 to 100'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-MN-DOWNPAY-PCT = 100 AND CA-MN-COUNT > 1
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNDPFULL'          TO CA-REASON-CODE
              MOVE 'A 100 percent down payment allows one part only'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3100-VALIDATE-PRO-RATA.
      * BR-BIL-MNY-008 Day counts
           EVALUATE TRUE
              WHEN CA-MN-DAYS-WHOLE <= 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'MNDAYSWH'       TO CA-REASON-CODE
                 MOVE 'Days in whole period must be greater than 0'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN CA-MN-DAYS-PART < 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'MNDAYSPT'       TO CA-REASON-CODE
                 MOVE 'Days in partial period cannot be negative'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN CA-MN-DAYS-PART > CA-MN-DAYS-WHOLE
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'MNDAYSGT'       TO CA-REASON-CODE
                 MOVE 'Partial period is longer than whole period'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-MNY-009 Overflow is reported, never truncated
       3900-REJECT-OVERFLOW.
           SET CA-RC-VALIDATION        TO TRUE
           MOVE 'MNOVFLW '             TO CA-REASON-CODE
           MOVE 'Amount overflow in money calculation'
                                       TO CA-MESSAGE
           MOVE 0                      TO CA-MN-RESULT
           PERFORM 9000-RETURN.

      ******************************************************************
      * BR-BIL-MNY-003 Down payment on part 1, balance spread evenly
       5000-COMPUTE-SPLIT.
           IF CA-MN-DOWNPAY-PCT > 0
              COMPUTE WS-DOWNPAY ROUNDED =
                      CA-MN-TOTAL * CA-MN-DOWNPAY-PCT / 100
              COMPUTE WS-SPREAD = CA-MN-TOTAL - WS-DOWNPAY
              COMPUTE WS-INSTAL-COUNT = CA-MN-COUNT - 1
           ELSE
              MOVE 0                   TO WS-DOWNPAY
              MOVE CA-MN-TOTAL         TO WS-SPREAD
              MOVE CA-MN-COUNT         TO WS-INSTAL-COUNT
           END-IF
      * BR-BIL-MNY-004 BR-BIL-MNY-006 Truncate toward zero; the
      * remainder (same sign as the total) goes to part 1
           DIVIDE WS-SPREAD BY WS-INSTAL-COUNT GIVING WS-EACH
      * BR-BIL-MNY-005 Every instalment must carry at least a penny
           IF WS-EACH = 0 AND WS-SPREAD NOT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MNTOOSML'          TO CA-REASON-CODE
              MOVE 'Amount too small to split into that many parts'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM VARYING WS-IX FROM 2 BY 1
                   UNTIL WS-IX > CA-MN-COUNT
              MOVE WS-EACH             TO CA-MN-PART(WS-IX)
           END-PERFORM
           COMPUTE WS-OTHER-PARTS = CA-MN-COUNT - 1
           MULTIPLY WS-EACH BY WS-OTHER-PARTS GIVING WS-SPREAD-TOTAL
           COMPUTE CA-MN-PART(1) = CA-MN-TOTAL - WS-SPREAD-TOTAL
           MOVE WS-EACH                TO CA-MN-RESULT.

      ******************************************************************
       9000-RETURN.
           GOBACK.
