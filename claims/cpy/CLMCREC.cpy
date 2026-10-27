      * CLMCREC - body for CLMRECOV (subrogation, salvage, deductible)
      * SUBOPN open subrogation ; SUBUPD ; SUBCLS with outcome
      * SALASG assign salvage for INCIDENT-ID ; SALSLD record sale
      *        (creates SALVAGE recovery) ; DEDRCV deductible recovery
              05 CA-RC-CLAIM-ID            PIC S9(9) COMP.
              05 CA-RC-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-RC-INCIDENT-ID         PIC S9(9) COMP.
              05 CA-RC-ADVERSE-PARTY-ID    PIC S9(9) COMP.
              05 CA-RC-ADVERSE-CARRIER     PIC X(60).
              05 CA-RC-LIABILITY-PCT       PIC S9(3)V99 COMP-3.
              05 CA-RC-EST-RECOVERY        PIC S9(11)V99 COMP-3.
              05 CA-RC-VENDOR-ID           PIC S9(9) COMP.
              05 CA-RC-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-RC-SALE-DATE           PIC X(10).
              05 CA-RC-STATUS              PIC X(4).
              05 CA-RC-OUTCOME             PIC X(8).
              05 CA-RC-TXN-ID              PIC S9(9) COMP.
              05 FILLER                    PIC X(3717).
