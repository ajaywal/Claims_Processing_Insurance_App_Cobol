      * CLMCSEQ - body for CLMSEQGN (sequence numbering)
      * Request SEQNEXT: reserve CA-SQ-COUNT values, return first.
              05 CA-SQ-NAME                PIC X(8).
              05 CA-SQ-COUNT               PIC S9(4) COMP.
              05 CA-SQ-VALUE               PIC S9(9) COMP.
              05 CA-SQ-FORMATTED           PIC X(12).
              05 FILLER                    PIC X(3814).
