      ******************************************************************
      * CLMCATCC - CLMCATAS control card (SYSIN, 80 bytes)             *
      ******************************************************************
       01  TC-CARD.
           05 TC-RUN-MODE                  PIC X.
              88 TC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 TC-COMMIT-FREQ-X             PIC X(5).
           05 TC-COMMIT-FREQ REDEFINES TC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 TC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X(62).
