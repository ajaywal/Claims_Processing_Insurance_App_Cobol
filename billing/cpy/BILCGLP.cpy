      * BILCGLP - body for BILGLPST (double-entry ledger posting)
      * Request GLPOST: lines must balance (sum DR = sum CR) else RC 12.
      * T-accounts: CASH UNAPPLD UNBILLED BILLED DUE PREMREV FEEREV
      *   SUSPENSE WRITEOFF COMMRSV COMMPAY DISBPAY CLMPAY
              05 CA-GL-TXN-TYPE            PIC X(8).
              05 CA-GL-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-GL-PRODUCER-ID         PIC S9(9) COMP.
              05 CA-GL-REF-TYPE            PIC X(4).
              05 CA-GL-REF-ID              PIC S9(9) COMP.
              05 CA-GL-REVERSAL-OF         PIC S9(9) COMP.
              05 CA-GL-TXN-ID              PIC S9(9) COMP.
              05 CA-GL-LINE-COUNT          PIC S9(4) COMP.
              05 CA-GL-LINE OCCURS 8 TIMES.
                 07 CA-GL-T-ACCOUNT        PIC X(8).
                 07 CA-GL-DR-CR            PIC X.
                 07 CA-GL-AMOUNT           PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3678).
