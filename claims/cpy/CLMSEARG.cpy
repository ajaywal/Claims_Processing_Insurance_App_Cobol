      ******************************************************************
      * CLMSEARG - Audit chain seal register record (120 bytes)        *
      * One record per CLMSEALR run, kept off-platform as independent  *
      * evidence of the chain head on each business date.              *
      ******************************************************************
       01  SG-RECORD.
           05 SG-BUS-DATE                  PIC X(10).
           05 SG-RUN-TIMESTAMP             PIC X(21).
           05 SG-OUTCOME                   PIC X(8).
              88 SG-SEALED                  VALUE 'SEALED  '.
              88 SG-NOTHING-NEW             VALUE 'NONEW   '.
              88 SG-EMPTY                   VALUE 'EMPTY   '.
              88 SG-BROKEN                  VALUE 'BROKEN  '.
           05 SG-FROM-SEQ                  PIC 9(9).
           05 SG-TO-SEQ                    PIC 9(9).
           05 SG-ROWS-VERIFIED             PIC 9(9).
           05 SG-HEAD-HASH                 PIC X(16).
           05 SG-BROKEN-SEQ                PIC 9(9).
           05 SG-REASON-CODE               PIC X(8).
           05 FILLER                       PIC X(21).
