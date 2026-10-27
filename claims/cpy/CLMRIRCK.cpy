      ******************************************************************
      * CLMRIRCK - CLMRIRE0 checkpoint record (80 bytes)               *
      ******************************************************************
       01  RK-RECORD.
           05 RK-PROGRAM                   PIC X(8).
           05 RK-STATUS                    PIC X.
              88 RK-IN-FLIGHT               VALUE 'P'.
              88 RK-ENDED                   VALUE 'E'.
           05 RK-BUS-DATE                  PIC X(10).
           05 RK-LAST-CLAIM-ID             PIC 9(9).
           05 RK-BORDEREAU-COUNT           PIC 9(9).
           05 RK-CEDED-TOTAL               PIC 9(13)V99.
           05 RK-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(7).
