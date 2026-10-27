      ******************************************************************
      * CLMFNLCK - CLMFNLBT checkpoint record (80 bytes)               *
      * Phase S = staging input (restart skips CK-INPUT-COUNT records) *
      * Phase P = processing intake rows (restart after CK-LAST-ID)    *
      * Phase E = run ended normally                                   *
      ******************************************************************
       01  FK-RECORD.
           05 FK-PROGRAM                   PIC X(8).
           05 FK-PHASE                     PIC X.
              88 FK-PHASE-STAGING           VALUE 'S'.
              88 FK-PHASE-PROCESS           VALUE 'P'.
              88 FK-PHASE-END               VALUE 'E'.
           05 FK-BUS-DATE                  PIC X(10).
           05 FK-INPUT-COUNT               PIC 9(9).
           05 FK-LAST-INTAKE-ID            PIC 9(9).
           05 FK-CLAIMS-CREATED            PIC 9(9).
           05 FK-REJECTS                   PIC 9(9).
           05 FK-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(4).
