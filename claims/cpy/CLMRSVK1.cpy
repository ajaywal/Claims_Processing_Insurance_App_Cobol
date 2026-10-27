      ******************************************************************
      * CLMRSVK1 - CLMRSVRP checkpoint / restart record (80 bytes)     *
      * A restart for the same business date resumes after             *
      * LAST-EXPOSURE-ID; activities already raised are not repeated.  *
      ******************************************************************
       01  RVK-CHECKPOINT-REC.
           05 RVK-PROGRAM                  PIC X(8).
           05 RVK-RUN-STATUS               PIC X.
              88 RVK-RUN-ACTIVE             VALUE 'R'.
              88 RVK-RUN-COMPLETE           VALUE 'C'.
           05 RVK-BUS-DATE                 PIC X(10).
           05 RVK-LAST-EXPOSURE-ID         PIC 9(9).
           05 RVK-REVIEWED                 PIC 9(7).
           05 RVK-ACTIVITIES               PIC 9(7).
           05 RVK-CHECKPOINT-TS            PIC X(26).
           05 FILLER                       PIC X(12).
