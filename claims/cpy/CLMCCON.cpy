      * CLMCCON - body for CLMCONTC (contact service)
      * CONNEW  create (dedupe on name+postcode+DOB returns existing)
      * CONINQ, CONUPD, CONLNK link CONTACT-ID to CLAIM-ID with ROLE
      * CONSAN  sanctions screen (OFAC/HMT) -> SANCTION-STATUS
              05 CA-CO-CONTACT-ID          PIC S9(9) COMP.
              05 CA-CO-TYPE                PIC X(4).
              05 CA-CO-FIRST-NAME          PIC X(30).
              05 CA-CO-LAST-NAME           PIC X(40).
              05 CA-CO-COMPANY-NAME        PIC X(60).
              05 CA-CO-DATE-OF-BIRTH       PIC X(10).
              05 CA-CO-ADDR-LINE1          PIC X(60).
              05 CA-CO-CITY                PIC X(40).
              05 CA-CO-POSTCODE            PIC X(8).
              05 CA-CO-PHONE               PIC X(20).
              05 CA-CO-EMAIL               PIC X(100).
              05 CA-CO-GENAPP-CUST-NUM     PIC S9(9) COMP.
              05 CA-CO-VENDOR-TYPE         PIC X(8).
              05 CA-CO-PREFERRED           PIC X.
              05 CA-CO-SANCTION-STATUS     PIC X(4).
              05 CA-CO-CLAIM-ID            PIC S9(9) COMP.
              05 CA-CO-ROLE                PIC X(8).
              05 CA-CO-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-CO-EXISTING            PIC X.
              05 FILLER                    PIC X(3430).
