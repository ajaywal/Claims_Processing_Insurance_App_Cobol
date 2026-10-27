      ******************************************************************
      * BILINVK1 - Billing cycle batch checkpoint record (106 bytes).  *
      * One record per program, rewritten at every commit point.       *
      * Used by BILINVB0 (key 1 = account id), BILINVDU (key 1 =       *
      * account id), BILDLQEV (key 1 = DP id, key 2 = step) and        *
      * BILDOCGN (key 1 = document id). RUN-START-TS is the start of   *
      * the run; BILDOCGN keeps the original run's value across       *
      * restarts to rebuild its vendor file completely.               *
      ******************************************************************
       01  CK-RECORD.
           05 CK-PROGRAM-ID            PIC X(8).
           05 CK-BUS-DATE              PIC X(10).
           05 CK-RUN-STATUS            PIC X.
              88 CK-RUNNING             VALUE 'R'.
              88 CK-COMPLETE            VALUE 'C'.
              88 CK-FAILED              VALUE 'F'.
           05 CK-LAST-KEY-1            PIC 9(9).
           05 CK-LAST-KEY-2            PIC 9(4).
           05 CK-UNITS-DONE            PIC 9(9).
           05 CK-COMMITS               PIC 9(7).
           05 CK-TIMESTAMP             PIC X(26).
           05 CK-RUN-START-TS          PIC X(26).
           05 FILLER                   PIC X(6).
