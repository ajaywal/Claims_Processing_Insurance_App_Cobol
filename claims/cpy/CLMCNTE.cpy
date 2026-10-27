      * CLMCNTE - body for CLMNOTE0 (notes)
      * NTEADD, NTELST newest 10 for CLAIM-ID (confidential notes only
      * for the assigned user and supervisors)
              05 CA-NT-NOTE-ID             PIC S9(9) COMP.
              05 CA-NT-CLAIM-ID            PIC S9(9) COMP.
              05 CA-NT-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-NT-TOPIC               PIC X(8).
              05 CA-NT-SUBJECT             PIC X(60).
              05 CA-NT-BODY                PIC X(2000).
              05 CA-NT-CONFIDENTIAL        PIC X.
              05 CA-NT-ROW-COUNT           PIC S9(4) COMP.
              05 CA-NT-ROW OCCURS 10 TIMES.
                 07 CA-NT-R-ID             PIC S9(9) COMP.
                 07 CA-NT-R-TS             PIC X(26).
                 07 CA-NT-R-AUTHOR         PIC X(8).
                 07 CA-NT-R-SUBJECT        PIC X(60).
              05 FILLER                    PIC X(777).
