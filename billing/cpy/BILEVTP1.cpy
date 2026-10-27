      ******************************************************************
      * BILEVTP1 - Integration event PAYLOAD layouts (docs/EVENTS.md)  *
      * Fixed-position text: numbers unsigned zero-padded display,     *
      * amounts 13 digits with 2 implied decimals + trailing sign.     *
      * Used by BILEVTDR BILGAPIN BILDSBSN.                            *
      ******************************************************************
       01  EVP-POLISSUE.
           05 EVP-PI-POLICY-NO         PIC 9(10).
           05 EVP-PI-CUSTOMER-NO       PIC 9(10).
           05 EVP-PI-PRODUCT           PIC X.
           05 EVP-PI-EFF-DATE          PIC X(10).
           05 EVP-PI-EXP-DATE          PIC X(10).
           05 EVP-PI-PREMIUM           PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
           05 EVP-PI-BROKER-ID         PIC 9(10).
           05 EVP-PI-PLAN-CODE         PIC X(8).
           05 EVP-PI-BILL-METHOD       PIC X(2).
       01  EVP-POLCHNG.
           05 EVP-PC-POLICY-NO         PIC 9(10).
           05 EVP-PC-PREMIUM           PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
           05 EVP-PC-EFF-DATE          PIC X(10).
       01  EVP-POLCANC.
           05 EVP-PX-POLICY-NO         PIC 9(10).
           05 EVP-PX-CANCEL-DATE       PIC X(10).
           05 EVP-PX-REASON            PIC X(8).
       01  EVP-CANCREQ.
           05 EVP-CR-POLICY-NO         PIC 9(10).
           05 EVP-CR-CANCEL-DATE       PIC X(10).
           05 EVP-CR-REASON            PIC X(8).
           05 EVP-CR-PASTDUE           PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
       01  EVP-CLMPAYMT.
           05 EVP-CP-CHECK-ID          PIC 9(10).
           05 EVP-CP-CLAIM-NO          PIC X(12).
           05 EVP-CP-POLICY-NO         PIC 9(10).
           05 EVP-CP-AMOUNT            PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
           05 EVP-CP-PAYEE-NAME        PIC X(60).
           05 EVP-CP-PAYEE-ADDR1       PIC X(60).
           05 EVP-CP-PAYEE-POSTCODE    PIC X(8).
           05 EVP-CP-PAY-METHOD        PIC X(4).
           05 EVP-CP-SCHED-DATE        PIC X(10).
       01  EVP-DEDBILL.
           05 EVP-DB-CLAIM-NO          PIC X(12).
           05 EVP-DB-POLICY-NO         PIC 9(10).
           05 EVP-DB-AMOUNT            PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
       01  EVP-DSBSENT.
           05 EVP-DS-CHECK-ID          PIC 9(10).
           05 EVP-DS-DISB-ID           PIC 9(10).
           05 EVP-DS-SENT-DATE         PIC X(10).
