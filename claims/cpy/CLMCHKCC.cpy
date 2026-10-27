      ******************************************************************
      * CLMCHKCC - CLMCHKIS control card (SYSIN, 80 bytes)             *
      ******************************************************************
       01  CC-CARD.
           05 CC-RUN-MODE                  PIC X.
              88 CC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 CC-COMMIT-FREQ-X             PIC X(5).
           05 CC-COMMIT-FREQ REDEFINES CC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 CC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X.
           05 CC-SORT-CODE                 PIC X(6).
           05 FILLER                       PIC X.
           05 CC-ACCOUNT-NO                PIC X(8).
           05 FILLER                       PIC X(46).
