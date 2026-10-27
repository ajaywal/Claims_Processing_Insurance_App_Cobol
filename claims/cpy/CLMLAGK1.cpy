      ******************************************************************
      * CLMLAGK1 - CLMLAGRP checkpoint record (80 bytes)               *
      * Read-only report: progress is recorded at each commit point;   *
      * an incomplete run (status R) is re-run in full.               *
      ******************************************************************
       01  LAK-CHECKPOINT-REC.
           05 LAK-PROGRAM                  PIC X(8).
           05 LAK-RUN-STATUS               PIC X.
              88 LAK-RUN-ACTIVE             VALUE 'R'.
              88 LAK-RUN-COMPLETE           VALUE 'C'.
           05 LAK-BUS-DATE                 PIC X(10).
           05 LAK-SECTION                  PIC X(4).
           05 LAK-LAST-CLAIM-ID            PIC 9(9).
           05 LAK-CLAIMS-READ              PIC 9(7).
           05 LAK-CHECKPOINT-TS            PIC X(26).
           05 FILLER                       PIC X(15).
