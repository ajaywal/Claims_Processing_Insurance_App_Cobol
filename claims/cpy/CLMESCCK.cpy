      ******************************************************************
      * CLMESCCK - CLMESCAL checkpoint record (80 bytes)               *
      * Phase E escalation (key = activity id), A aged-claim review    *
      * (key = claim id), Z run ended.                                 *
      ******************************************************************
       01  EK-RECORD.
           05 EK-PROGRAM                   PIC X(8).
           05 EK-PHASE                     PIC X.
              88 EK-PHASE-ESCALATE          VALUE 'E'.
              88 EK-PHASE-AGED              VALUE 'A'.
              88 EK-PHASE-END               VALUE 'Z'.
           05 EK-BUS-DATE                  PIC X(10).
           05 EK-LAST-KEY                  PIC 9(9).
           05 EK-ESCALATED                 PIC 9(9).
           05 EK-REVIEWS-RAISED            PIC 9(9).
           05 EK-TIMESTAMP                 PIC X(21).
           05 FILLER                       PIC X(13).
