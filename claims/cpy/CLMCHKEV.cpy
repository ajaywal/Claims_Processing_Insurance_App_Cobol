      ******************************************************************
      * CLMCHKEV - CLMPAYMT event payload (docs/EVENTS.md), 188 bytes  *
      ******************************************************************
       01  PE-PAYLOAD.
           05 PE-CHECK-ID                  PIC 9(10).
           05 PE-CLAIM-NUMBER              PIC X(12).
           05 PE-POLICY-NO                 PIC 9(10).
           05 PE-AMOUNT                    PIC 9(11)V99.
           05 PE-AMOUNT-SIGN               PIC X.
           05 PE-PAYEE-NAME                PIC X(60).
           05 PE-PAYEE-ADDR1               PIC X(60).
           05 PE-PAYEE-POSTCODE            PIC X(8).
           05 PE-PAY-METHOD                PIC X(4).
           05 PE-SCHEDULED-DATE            PIC X(10).
