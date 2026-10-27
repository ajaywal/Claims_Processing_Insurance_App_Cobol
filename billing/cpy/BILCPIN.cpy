      * BILCPIN - body for BILINSTR (payment instruments, tokenised)
      * INSADD, INSINQ, INSDEA deactivate, INSFAIL record failure
      * (3 failures deactivates and turns off account autopay)
              05 CA-PI-INSTR-ID            PIC S9(9) COMP.
              05 CA-PI-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-PI-TYPE                PIC X(4).
              05 CA-PI-TOKEN-REF           PIC X(32).
              05 CA-PI-LAST-FOUR           PIC X(4).
              05 CA-PI-EXPIRY-MMYY         PIC X(4).
              05 CA-PI-SORT-CODE           PIC X(6).
              05 CA-PI-HOLDER-NAME         PIC X(60).
              05 CA-PI-STATUS              PIC X(4).
              05 CA-PI-FAIL-COUNT          PIC S9(4) COMP.
              05 FILLER                    PIC X(3716).
