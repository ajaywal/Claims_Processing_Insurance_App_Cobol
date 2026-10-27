      ******************************************************************
      * CLMISOC1 - CLMISOSN checkpoint / restart record (100 bytes)    *
      * SEND restarts after LAST-KEY (ISO_REQ_ID) and extends ISOOUT;  *
      * RECV restarts after LAST-RECNO response records.               *
      ******************************************************************
       01  ISC-CHECKPOINT-REC.
           05 ISC-PROGRAM                  PIC X(8).
           05 ISC-MODE                     PIC X(4).
           05 ISC-RUN-STATUS               PIC X.
              88 ISC-RUN-ACTIVE             VALUE 'R'.
              88 ISC-RUN-COMPLETE           VALUE 'C'.
           05 ISC-BUS-DATE                 PIC X(10).
           05 ISC-LAST-KEY                 PIC 9(9).
           05 ISC-LAST-RECNO               PIC 9(9).
           05 ISC-REQUESTS                 PIC 9(7).
           05 ISC-PARTIES                  PIC 9(7).
           05 ISC-CLAIM-HASH               PIC 9(15).
           05 ISC-CHECKPOINT-TS            PIC X(26).
           05 FILLER                       PIC X(4).
