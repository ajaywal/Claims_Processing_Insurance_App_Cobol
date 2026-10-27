      * BILCEVT - body for BILEVTQ (integration event queue)
      * EVPUT add event; EVGET next NEW event for TARGET (sets PROC);
      * EVDONE mark done; EVFAIL increment retry, error text, back NEW
      * until 5 retries then ERRR.
              05 CA-EV-EVENT-ID            PIC S9(9) COMP.
              05 CA-EV-TYPE                PIC X(8).
              05 CA-EV-DIRECTION           PIC X.
              05 CA-EV-SOURCE              PIC X(4).
              05 CA-EV-TARGET              PIC X(4).
              05 CA-EV-KEY                 PIC X(20).
              05 CA-EV-PAYLOAD             PIC X(1000).
              05 CA-EV-ERROR               PIC X(80).
              05 FILLER                    PIC X(2719).
