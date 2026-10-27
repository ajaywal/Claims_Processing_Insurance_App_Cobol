      * BILCPRD - body for BILPROD0 (producer service)
      * PRDADD, PRDINQ (by PRODUCER-ID), PRDFND (by GenApp BROKERID
      * held in PRODUCER-CODE), PRDUPD
              05 CA-PR-PRODUCER-ID         PIC S9(9) COMP.
              05 CA-PR-PRODUCER-CODE       PIC S9(9) COMP.
              05 CA-PR-NAME                PIC X(60).
              05 CA-PR-TIER                PIC X(4).
              05 CA-PR-COMM-PLAN           PIC X(8).
              05 CA-PR-PAY-CYCLE           PIC X.
              05 CA-PR-AGENCY-BILL         PIC X.
              05 CA-PR-PAYABLE-BALANCE     PIC S9(11)V99 COMP-3.
              05 CA-PR-STATUS              PIC X(4).
              05 FILLER                    PIC X(3747).
