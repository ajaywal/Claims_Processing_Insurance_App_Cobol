      ******************************************************************
      * CLMDUPCK - CLMDUPCK checkpoint record (80 bytes)               *
      * Restart resumes with later-claim ids above DK-LAST-CLAIM-ID.   *
      ******************************************************************
       01  DK-RECORD.
           05 DK-PROGRAM                   PIC X(8).
           05 DK-STATUS                    PIC X.
              88 DK-IN-FLIGHT               VALUE 'P'.
              88 DK-ENDED                   VALUE 'E'.
           05 DK-BUS-DATE                  PIC X(10).
           05 DK-LAST-CLAIM-ID             PIC 9(9).
           05 DK-FLAGGED                   PIC 9(9).
           05 DK-PAIRS                     PIC 9(9).
           05 DK-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(13).
