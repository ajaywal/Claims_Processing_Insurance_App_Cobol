      * BILCWOF - body for BILWOFF0 (write-off service)
      * WOFCRT write off AMOUNT of ITEM-ID (or all small balances of
      *        ACCOUNT-ID below plan threshold when ITEM-ID = 0)
      * WOFREV reverse WO-ID
              05 CA-WO-WO-ID               PIC S9(9) COMP.
              05 CA-WO-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-WO-ITEM-ID             PIC S9(9) COMP.
              05 CA-WO-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-WO-REASON              PIC X(8).
              05 CA-WO-STATUS              PIC X(4).
              05 CA-WO-ITEMS-WRITTEN       PIC S9(4) COMP.
              05 CA-WO-TOTAL-WRITTEN       PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3800).
