      * BILCDST - body for BILPAYDS (distribution engine)
      * DSTAPLY distribute AMOUNT of MONEY-ID across ACCOUNT-ID items:
      *   1 past-due items oldest due date first, then
      *   2 billed-not-due items oldest first, then
      *   3 (MODE 'A' only) planned items earliest event date first;
      *   within a date: premium before tax before fees (PRIORITY-RANK)
      *   TARGET-INVOICE-ID restricts pass 1-2 to that invoice first.
      *   Remainder stays unapplied on the account.
      * DSTUNDO reverse every distribution of MONEY-ID
              05 CA-DS-MONEY-ID            PIC S9(9) COMP.
              05 CA-DS-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-DS-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-DS-MODE                PIC X.
                 88 CA-DS-DUE-ONLY          VALUE 'D'.
                 88 CA-DS-ALL-ITEMS         VALUE 'A'.
              05 CA-DS-TARGET-INVOICE-ID   PIC S9(9) COMP.
              05 CA-DS-DISTRIBUTED         PIC S9(11)V99 COMP-3.
              05 CA-DS-REMAINING           PIC S9(11)V99 COMP-3.
              05 CA-DS-ITEMS-TOUCHED       PIC S9(4) COMP.
              05 CA-DS-ITEMS-CLOSED        PIC S9(4) COMP.
              05 FILLER                    PIC X(3802).
