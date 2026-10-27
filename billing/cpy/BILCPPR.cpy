      * BILCPPR - body for BILPPER0 (policy period & billing instruct.)
      * PPNEW   new-business/renewal period: creates period, premium
      *         charge (+tax, +fees), schedules items, reserves commission
      * PPCHG   mid-term change: delta charge effective CHANGE-EFF-DATE
      * PPCAN   cancellation: pro-rata return-premium credit charge
      * PPRIN   reinstatement: re-bill credit + reinstatement fee
      * PPINQ   inquire period with invoice item schedule
      * PPPLN   payment-plan change: reschedule unbilled items
              05 CA-PP-PP-ID               PIC S9(9) COMP.
              05 CA-PP-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-PP-GENAPP-POL-NUM      PIC S9(9) COMP.
              05 CA-PP-TERM-NUMBER         PIC S9(4) COMP.
              05 CA-PP-PRODUCT-CODE        PIC X.
              05 CA-PP-EFF-DATE            PIC X(10).
              05 CA-PP-EXP-DATE            PIC X(10).
              05 CA-PP-PLAN-CODE           PIC X(8).
              05 CA-PP-BILLING-METHOD      PIC X(2).
              05 CA-PP-PRODUCER-CODE       PIC S9(9) COMP.
              05 CA-PP-TOTAL-PREMIUM       PIC S9(11)V99 COMP-3.
              05 CA-PP-TAX-AMOUNT          PIC S9(11)V99 COMP-3.
              05 CA-PP-CHANGE-AMOUNT       PIC S9(11)V99 COMP-3.
              05 CA-PP-CHANGE-EFF-DATE     PIC X(10).
              05 CA-PP-CANCEL-REASON       PIC X(8).
              05 CA-PP-STATUS              PIC X(4).
              05 CA-PP-SOURCE-REF          PIC X(20).
              05 CA-PP-CHARGE-ID           PIC S9(9) COMP.
              05 CA-PP-ITEM-COUNT          PIC S9(4) COMP.
              05 CA-PP-ITEM OCCURS 24 TIMES.
                 07 CA-PP-IT-ITEM-ID       PIC S9(9) COMP.
                 07 CA-PP-IT-TYPE          PIC X(4).
                 07 CA-PP-IT-INSTALL-NO    PIC S9(4) COMP.
                 07 CA-PP-IT-EVENT-DATE    PIC X(10).
                 07 CA-PP-IT-DUE-DATE      PIC X(10).
                 07 CA-PP-IT-AMOUNT        PIC S9(11)V99 COMP-3.
                 07 CA-PP-IT-PAID          PIC S9(11)V99 COMP-3.
                 07 CA-PP-IT-STATUS        PIC X(4).
              05 FILLER                    PIC X(2570).
