      * CLMCSIU - body for CLMSIUSC (special investigation scoring)
      * SIUSCR evaluate active CLM_SIU_RULE rules for CLAIM-ID, store
      *        scores, set claim SIU-SCORE; >= 50 refers to SIU group
              05 CA-SI-CLAIM-ID            PIC S9(9) COMP.
              05 CA-SI-TOTAL-SCORE         PIC S9(4) COMP.
              05 CA-SI-RULES-HIT           PIC S9(4) COMP.
              05 CA-SI-REFERRED            PIC X.
              05 CA-SI-HIT OCCURS 12 TIMES.
                 07 CA-SI-H-RULE           PIC X(8).
                 07 CA-SI-H-POINTS         PIC S9(4) COMP.
              05 FILLER                    PIC X(3711).
