      ******************************************************************
      * CLMEVTC1 - CLMEVTDR checkpoint / restart record (80 bytes)     *
      * Rewritten at every commit point. RUN-STATUS R = running (a     *
      * restart resumes after LAST-KEY of PHASE), C = completed.       *
      ******************************************************************
       01  EDC-CHECKPOINT-REC.
           05 EDC-PROGRAM                  PIC X(8).
           05 EDC-RUN-STATUS               PIC X.
              88 EDC-RUN-ACTIVE             VALUE 'R'.
              88 EDC-RUN-COMPLETE           VALUE 'C'.
           05 EDC-BUS-DATE                 PIC X(10).
           05 EDC-PHASE                    PIC X(4).
              88 EDC-PHASE-OUTBOUND         VALUE 'OUTB'.
              88 EDC-PHASE-INBOUND          VALUE 'INBD'.
           05 EDC-LAST-KEY                 PIC 9(9).
           05 EDC-EVENTS-DONE              PIC 9(7).
           05 EDC-EVENTS-FAILED            PIC 9(7).
           05 EDC-CHECKPOINT-TS            PIC X(26).
           05 FILLER                       PIC X(8).
