      ******************************************************************
      * CLMDUPCC - CLMDUPCK control card (SYSIN, 80 bytes)             *
      * DC-LOOKBACK-DAYS limits the later claims examined to those     *
      * reported in the last n days; 000 = the whole book.             *
      ******************************************************************
       01  DC-CARD.
           05 DC-RUN-MODE                  PIC X.
              88 DC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 DC-COMMIT-FREQ-X             PIC X(5).
           05 DC-COMMIT-FREQ REDEFINES DC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 DC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X.
           05 DC-LOOKBACK-DAYS-X           PIC X(3).
           05 DC-LOOKBACK-DAYS REDEFINES DC-LOOKBACK-DAYS-X
                                           PIC 9(3).
           05 FILLER                       PIC X(58).
