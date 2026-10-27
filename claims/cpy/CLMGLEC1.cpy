      ******************************************************************
      * CLMGLEC1 - CLMGLEXT checkpoint record (80 bytes)               *
      * The extract posts nothing until it balances, so an incomplete  *
      * run (status R) is re-run from the start for the same date.     *
      ******************************************************************
       01  GLC-CHECKPOINT-REC.
           05 GLC-PROGRAM                  PIC X(8).
           05 GLC-RUN-STATUS               PIC X.
              88 GLC-RUN-ACTIVE             VALUE 'R'.
              88 GLC-RUN-COMPLETE           VALUE 'C'.
           05 GLC-BUS-DATE                 PIC X(10).
           05 GLC-ROWS-READ                PIC 9(7).
           05 GLC-LINES-WRITTEN            PIC 9(7).
           05 GLC-CHECKPOINT-TS            PIC X(26).
           05 FILLER                       PIC X(21).
