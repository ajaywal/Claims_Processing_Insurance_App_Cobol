      ******************************************************************
      * CLMESCCC - CLMESCAL control card (SYSIN, 80 bytes)             *
      ******************************************************************
       01  EC-CARD.
           05 EC-RUN-MODE                  PIC X.
              88 EC-MODE-RESTART            VALUE 'R'.
           05 FILLER                       PIC X.
           05 EC-COMMIT-FREQ-X             PIC X(5).
           05 EC-COMMIT-FREQ REDEFINES EC-COMMIT-FREQ-X
                                           PIC 9(5).
           05 FILLER                       PIC X.
           05 EC-BUS-DATE                  PIC X(10).
           05 FILLER                       PIC X.
           05 EC-AGED-DAYS-X               PIC X(3).
           05 EC-AGED-DAYS REDEFINES EC-AGED-DAYS-X
                                           PIC 9(3).
           05 FILLER                       PIC X.
           05 EC-OVERDUE-ALERT-X           PIC X(3).
           05 EC-OVERDUE-ALERT REDEFINES EC-OVERDUE-ALERT-X
                                           PIC 9(3).
           05 FILLER                       PIC X(54).
