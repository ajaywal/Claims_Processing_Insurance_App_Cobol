      * CLMCFIN - body for CLMFINTX (claim financial transactions)
      * RSVSET  set open reserve of (EXPOSURE, COST-TYPE, CATEGORY) to
      *         AMOUNT (delta txn); authority-checked
      * PAYCRT  payment AMOUNT to PAYEE-CONTACT-ID; erodes reserve;
      *         over-authority -> PEND approval + activity
      * RCVRSV  recovery reserve ; RCVCRT recovery receipt
      * TXNAPR  approve TXN-ID ; TXNREJ reject ; TXNVOD void (paid checks
      *         only if not cleared) ; TXNINQ
              05 CA-FT-TXN-ID              PIC S9(9) COMP.
              05 CA-FT-CLAIM-ID            PIC S9(9) COMP.
              05 CA-FT-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-FT-COST-TYPE           PIC X(4).
              05 CA-FT-COST-CATEGORY       PIC X(8).
              05 CA-FT-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-FT-PAYMENT-TYPE        PIC X(4).
              05 CA-FT-PAYEE-CONTACT-ID    PIC S9(9) COMP.
              05 CA-FT-PAY-METHOD          PIC X(4).
              05 CA-FT-SCHEDULED-DATE      PIC X(10).
              05 CA-FT-RECOVERY-CATEGORY   PIC X(8).
              05 CA-FT-REASON              PIC X(8).
              05 CA-FT-TXN-STATUS          PIC X(4).
              05 CA-FT-TXN-TYPE            PIC X(4).
              05 CA-FT-APPROVER            PIC X(8).
              05 CA-FT-CHECK-ID            PIC S9(9) COMP.
              05 CA-FT-ACTIVITY-ID         PIC S9(9) COMP.
              05 CA-FT-RL-ID               PIC S9(9) COMP.
              05 CA-FT-RL-OPEN-RESERVE     PIC S9(11)V99 COMP-3.
              05 CA-FT-RL-TOTAL-PAID       PIC S9(11)V99 COMP-3.
              05 CA-FT-RL-RECOVERY-RSV     PIC S9(11)V99 COMP-3.
              05 CA-FT-RL-TOTAL-RECOVERED  PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3715).
