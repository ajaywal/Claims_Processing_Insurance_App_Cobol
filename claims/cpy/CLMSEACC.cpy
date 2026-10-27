      ******************************************************************
      * CLMSEACC - CLMSEALR control card (SYSIN, 80 bytes)             *
      * SC-MAX-SLICES caps AUCVFY calls (each walks up to 50000 rows). *
      ******************************************************************
       01  SC-CARD.
           05 SC-RUN-MODE                  PIC X.
              88 SC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 SC-COMMIT-FREQ-X             PIC X(5).
           05 SC-COMMIT-FREQ REDEFINES SC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 SC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X.
           05 SC-MAX-SLICES-X              PIC X(3).
           05 SC-MAX-SLICES REDEFINES SC-MAX-SLICES-X
                                           PIC 9(3).
           05 FILLER                       PIC X(58).
