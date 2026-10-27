      * CLMCDOC - body for CLMDOCRQ (document requests)
      * DOCREQ request from RECIPIENT ; DOCRCV mark received ; DOCLST
              05 CA-DC-DOC-ID              PIC S9(9) COMP.
              05 CA-DC-CLAIM-ID            PIC S9(9) COMP.
              05 CA-DC-DOC-TYPE            PIC X(8).
              05 CA-DC-DOC-NAME            PIC X(60).
              05 CA-DC-RECIPIENT-ID        PIC S9(9) COMP.
              05 CA-DC-DMS-REF             PIC X(32).
              05 CA-DC-STATUS              PIC X(4).
              05 CA-DC-ROW-COUNT           PIC S9(4) COMP.
              05 CA-DC-ROW OCCURS 10 TIMES.
                 07 CA-DC-R-ID             PIC S9(9) COMP.
                 07 CA-DC-R-TYPE           PIC X(8).
                 07 CA-DC-R-NAME           PIC X(60).
                 07 CA-DC-R-STATUS         PIC X(4).
                 07 CA-DC-R-REQUESTED      PIC X(10).
              05 FILLER                    PIC X(2862).
