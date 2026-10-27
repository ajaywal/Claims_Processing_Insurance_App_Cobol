      * BILCMNY - body for BILMONEY (money arithmetic)
      * MNSPLIT split TOTAL into COUNT parts with DOWNPAY-PCT on part 1;
      *         rounding remainder is carried on the first part.
      * MNRATE  RESULT = TOTAL * RATE-BPS / 10000, rounded half-up.
      * MNPRORA RESULT = TOTAL * DAYS-PART / DAYS-WHOLE (pro-rata).
              05 CA-MN-TOTAL               PIC S9(11)V99 COMP-3.
              05 CA-MN-DOWNPAY-PCT         PIC S9(3)V99 COMP-3.
              05 CA-MN-COUNT               PIC S9(4) COMP.
              05 CA-MN-RATE-BPS            PIC S9(4) COMP.
              05 CA-MN-DAYS-PART           PIC S9(5) COMP-3.
              05 CA-MN-DAYS-WHOLE          PIC S9(5) COMP-3.
              05 CA-MN-RESULT              PIC S9(11)V99 COMP-3.
              05 CA-MN-PART OCCURS 24 TIMES
                                           PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3645).
