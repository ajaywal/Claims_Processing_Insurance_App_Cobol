      ******************************************************************
      * CLMSEACK - CLMSEALR checkpoint record (80 bytes)               *
      * The chain itself is the restart key (AUCVFY FROM-SEQ 0 resumes *
      * after the last sealed row); this trail records each committed  *
      * slice so a restart can be reconciled against the table.        *
      ******************************************************************
       01  SK-RECORD.
           05 SK-PROGRAM                   PIC X(8).
           05 SK-STATUS                    PIC X.
              88 SK-IN-FLIGHT               VALUE 'P'.
              88 SK-ENDED                   VALUE 'E'.
           05 SK-BUS-DATE                  PIC X(10).
           05 SK-LAST-SEALED-SEQ           PIC 9(9).
           05 SK-HEAD-HASH                 PIC X(16).
           05 SK-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(15).
