      * CLMCHIS - body for CLMHIST0 (claim history writer)
      * HISADD append a history row; HISLST newest 20 rows for CLAIM-ID
              05 CA-HI-CLAIM-ID            PIC S9(9) COMP.
              05 CA-HI-EVENT-TYPE          PIC X(8).
              05 CA-HI-DESCRIPTION         PIC X(120).
              05 CA-HI-PROGRAM             PIC X(8).
              05 CA-HI-ROW-COUNT           PIC S9(4) COMP.
              05 CA-HI-ROW OCCURS 20 TIMES.
                 07 CA-HI-R-TS             PIC X(26).
                 07 CA-HI-R-EVENT          PIC X(8).
                 07 CA-HI-R-USER           PIC X(8).
                 07 CA-HI-R-DESC           PIC X(120).
              05 FILLER                    PIC X(458).
