      ******************************************************************
      * CLMFNLCC - CLMFNLBT control card (SYSIN, 80 bytes)             *
      ******************************************************************
       01  FC-CARD.
           05 FC-RUN-MODE                  PIC X.
              88 FC-MODE-NEW                VALUE 'N'.
              88 FC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 FC-COMMIT-FREQ-X             PIC X(5).
           05 FC-COMMIT-FREQ REDEFINES FC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 FC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X.
           05 FC-MAX-REJECT-PCT-X          PIC X(3).
           05 FC-MAX-REJECT-PCT REDEFINES FC-MAX-REJECT-PCT-X
                                           PIC 9(3).
           05 FILLER                       PIC X(58).
