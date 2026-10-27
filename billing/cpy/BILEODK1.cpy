      ******************************************************************
      * BILEODK1 - End-of-day batch checkpoint record (80 bytes)        *
      * One record per program, rewritten at every commit point.        *
      * Used by BILAGERP BILRECON BILGLEXT BILEODDR.                    *
      ******************************************************************
       01  CHK-RECORD.
           05 CHK-PROGRAM-ID           PIC X(8).
           05 CHK-BUS-DATE             PIC X(10).
           05 CHK-RUN-STATUS           PIC X.
              88 CHK-RUNNING            VALUE 'R'.
              88 CHK-COMPLETE           VALUE 'C'.
              88 CHK-FAILED             VALUE 'F'.
           05 CHK-RESTART-KEY          PIC X(20).
           05 CHK-ROWS-DONE            PIC 9(9).
           05 CHK-TIMESTAMP            PIC X(26).
           05 FILLER                   PIC X(6).
