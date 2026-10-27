      * CLMCAUC - body for CLMAUDCH (tamper-evident audit hash chain)
      * AUCADD  append event: CHAIN-HASH = H(PREV-HASH || DIGEST(payload))
      * AUCVFY  re-walk chain from FROM-SEQ, report first broken link
              05 CA-AC-CLAIM-ID            PIC S9(9) COMP.
              05 CA-AC-EVENT-CODE          PIC X(8).
              05 CA-AC-PAYLOAD             PIC X(400).
              05 CA-AC-PROGRAM             PIC X(8).
              05 CA-AC-CHAIN-SEQ           PIC S9(9) COMP.
              05 CA-AC-CHAIN-HASH          PIC X(16).
              05 CA-AC-FROM-SEQ            PIC S9(9) COMP.
              05 CA-AC-ROWS-CHECKED        PIC S9(9) COMP.
              05 CA-AC-BROKEN-SEQ          PIC S9(9) COMP.
              05 FILLER                    PIC X(3388).
