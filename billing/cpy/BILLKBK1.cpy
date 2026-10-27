      ******************************************************************
      * BILLKBK1 - B5 cash batch checkpoint / restart record (80 bytes)*
      * Written to DD CHKPT at every commit; on a rerun the last record*
      * is supplied on DD RESTART and the driver resumes after         *
      * CKP-RESTART-KEY when program, run date (and batch reference    *
      * where one applies) match and the run did not complete.         *
      * Used by BILLKBX0 BILACHDR BILACHRT.                            *
      * COPY ... REPLACING LEADING ==CKP-== BY ==RST-== for the input. *
      ******************************************************************
           05 CKP-PROGRAM              PIC X(8).
           05 CKP-RUN-DATE             PIC X(10).
           05 CKP-PHASE                PIC X.
           05 CKP-RESTART-KEY          PIC X(20).
           05 CKP-BATCH-REF            PIC X(12).
           05 CKP-COMMIT-COUNT         PIC 9(7).
           05 CKP-READ-COUNT           PIC 9(9).
           05 CKP-RUN-STATUS           PIC X.
              88 CKP-RUNNING            VALUE 'R'.
              88 CKP-COMPLETE           VALUE 'C'.
           05 FILLER                   PIC X(12).
