      * CLMCSRQ - body for CLMSRVRQ (vendor service requests)
      * SRQNEW pick preferred vendor by type + postcode area when
      *        VENDOR-ID = 0 ; SRQQTE quote ; SRQCMP complete with
      *        invoice -> payment via CLMFINTX ; SRQCAN
              05 CA-SR-SR-ID               PIC S9(9) COMP.
              05 CA-SR-CLAIM-ID            PIC S9(9) COMP.
              05 CA-SR-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-SR-VENDOR-ID           PIC S9(9) COMP.
              05 CA-SR-SERVICE-TYPE        PIC X(8).
              05 CA-SR-STATUS              PIC X(4).
              05 CA-SR-QUOTE-AMOUNT        PIC S9(11)V99 COMP-3.
              05 CA-SR-INVOICE-AMOUNT      PIC S9(11)V99 COMP-3.
              05 CA-SR-PAYMENT-TXN-ID      PIC S9(9) COMP.
              05 FILLER                    PIC X(3794).
