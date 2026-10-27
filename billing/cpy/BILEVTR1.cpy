      ******************************************************************
      * BILEVTR1 - BILEVTDR event drainer report lines (132 cols)      *
      ******************************************************************
       01  EDR-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILEVTDR'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(60) VALUE
              'BillingCoreAJ2026 - Integration event drainer'.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 EDR-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(26) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 EDR-H1-PAGE              PIC ZZZZ9.
       01  EDR-HEAD-2.
           05 FILLER                   PIC X(12) VALUE '  EVENT ID'.
           05 FILLER                   PIC X(10) VALUE 'TYPE'.
           05 FILLER                   PIC X(6)  VALUE 'SRC'.
           05 FILLER                   PIC X(6)  VALUE 'TGT'.
           05 FILLER                   PIC X(22) VALUE 'KEY'.
           05 FILLER                   PIC X(76) VALUE 'RESULT'.
       01  EDR-DETAIL.
           05 EDR-D-EVENT-ID           PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EDR-D-TYPE               PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EDR-D-SOURCE             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EDR-D-TARGET             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EDR-D-KEY                PIC X(20).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EDR-D-RESULT             PIC X(76).
       01  EDR-TYPE-TOTAL.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 EDR-TT-TYPE              PIC X(8).
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(6)  VALUE 'DONE '.
           05 EDR-TT-DONE              PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(8)  VALUE 'FAILED '.
           05 EDR-TT-FAILED            PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(8)  VALUE 'PARKED '.
           05 EDR-TT-PARKED            PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(65) VALUE SPACES.
       01  EDR-MESSAGE.
           05 EDR-M-TEXT               PIC X(132) VALUE SPACES.
