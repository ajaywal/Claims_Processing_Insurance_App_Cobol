      ******************************************************************
      * CLMCATCK - CLMCATAS checkpoint record (80 bytes)               *
      * Restart resumes at catastrophe CT-CAT-CODE after claim         *
      * CT-LAST-CLAIM-ID; earlier catastrophes are complete.           *
      ******************************************************************
       01  CT-RECORD.
           05 CT-PROGRAM                   PIC X(8).
           05 CT-STATUS                    PIC X.
              88 CT-IN-FLIGHT               VALUE 'P'.
              88 CT-ENDED                   VALUE 'E'.
           05 CT-BUS-DATE                  PIC X(10).
           05 CT-CAT-CODE                  PIC X(8).
           05 CT-LAST-CLAIM-ID             PIC 9(9).
           05 CT-TAGGED                    PIC 9(9).
           05 CT-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(14).
