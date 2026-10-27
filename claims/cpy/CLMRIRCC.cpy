      ******************************************************************
      * CLMRIRCC - CLMRIRE0 control card (SYSIN, 80 bytes)             *
      ******************************************************************
       01  RC-CARD.
           05 RC-RUN-MODE                  PIC X.
              88 RC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 RC-COMMIT-FREQ-X             PIC X(5).
           05 RC-COMMIT-FREQ REDEFINES RC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 RC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X(62).
