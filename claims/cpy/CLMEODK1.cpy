      ******************************************************************
      * CLMEODK1 - CLMEODDR checkpoint record (80 bytes)               *
      * STATUS R = EOD started, H = held by a readiness check,         *
      * C = business date rolled from ROLLED-FROM to ROLLED-TO.        *
      ******************************************************************
       01  EOK-CHECKPOINT-REC.
           05 EOK-PROGRAM                  PIC X(8).
           05 EOK-RUN-STATUS               PIC X.
              88 EOK-RUN-ACTIVE             VALUE 'R'.
              88 EOK-RUN-HELD               VALUE 'H'.
              88 EOK-RUN-COMPLETE           VALUE 'C'.
           05 EOK-ROLLED-FROM              PIC X(10).
           05 EOK-ROLLED-TO                PIC X(10).
           05 EOK-CHECKPOINT-TS            PIC X(26).
           05 FILLER                       PIC X(25).
