      ******************************************************************
      * CLMCHKCK - CLMCHKIS checkpoint record (80 bytes)               *
      ******************************************************************
       01  CK-RECORD.
           05 CK-PROGRAM                   PIC X(8).
           05 CK-STATUS                    PIC X.
              88 CK-IN-FLIGHT               VALUE 'P'.
              88 CK-ENDED                   VALUE 'E'.
           05 CK-BUS-DATE                  PIC X(10).
           05 CK-LAST-CHECK-ID             PIC 9(9).
           05 CK-ISSUED                    PIC 9(9).
           05 CK-ISSUED-AMOUNT             PIC 9(11)V99.
           05 CK-PP-BATCH-NO               PIC 9(4).
           05 CK-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(5).
