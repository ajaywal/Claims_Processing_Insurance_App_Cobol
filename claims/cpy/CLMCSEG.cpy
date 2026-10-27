      * CLMCSEG - body for CLMSEGMT (segmentation & severity)
      * SEGCLM  derive SEGMENT and SEVERITY (1..5) for CLAIM-ID from
      *         loss type, cause, injury, estimated damage, total loss,
      *         litigation, catastrophe
              05 CA-SG-CLAIM-ID            PIC S9(9) COMP.
              05 CA-SG-SEGMENT             PIC X(8).
              05 CA-SG-SEVERITY            PIC S9(4) COMP.
              05 CA-SG-REASON              PIC X(60).
              05 FILLER                    PIC X(3766).
