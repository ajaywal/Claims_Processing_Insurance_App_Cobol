      * BILCACT - body for BILACCT0 (billing account service)
      * ACCTADD create account + primary payer contact + invoice stream
      * ACCTINQ inquire by ACCOUNT-ID or ACCOUNT-NUMBER
      * ACCTFND find by GENAPP-CUST-NUM
      * ACCTUPD update mutable fields (VERSION-NO must match)
      * ACCTCLS close (no open balance, no in-force periods)
      * ACCTBAL recompute balances from invoice items
              05 CA-AC-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-AC-ACCOUNT-NUMBER      PIC X(10).
              05 CA-AC-GENAPP-CUST-NUM     PIC S9(9) COMP.
              05 CA-AC-ACCOUNT-NAME        PIC X(60).
              05 CA-AC-ACCOUNT-TYPE        PIC X(4).
              05 CA-AC-BILLING-LEVEL       PIC X(4).
              05 CA-AC-INVOICE-DAY         PIC S9(4) COMP.
              05 CA-AC-DELIVERY-METHOD     PIC X(4).
              05 CA-AC-DELINQ-PLAN         PIC X(8).
              05 CA-AC-AUTOPAY-FLAG        PIC X.
              05 CA-AC-AUTOPAY-INSTR-ID    PIC S9(9) COMP.
              05 CA-AC-STATUS              PIC X(4).
              05 CA-AC-COLLECTION-STATUS   PIC X(4).
              05 CA-AC-UNAPPLIED-AMT       PIC S9(11)V99 COMP-3.
              05 CA-AC-BILLED-AMT          PIC S9(11)V99 COMP-3.
              05 CA-AC-DUE-AMT             PIC S9(11)V99 COMP-3.
              05 CA-AC-UNBILLED-AMT        PIC S9(11)V99 COMP-3.
              05 CA-AC-OPEN-DATE           PIC X(10).
              05 CA-AC-VERSION-NO          PIC S9(9) COMP.
              05 CA-AC-STREAM-ID           PIC S9(9) COMP.
              05 CA-AC-CONTACT.
                 07 CA-AC-CONTACT-TYPE     PIC X.
                 07 CA-AC-FIRST-NAME       PIC X(30).
                 07 CA-AC-LAST-NAME        PIC X(40).
                 07 CA-AC-COMPANY-NAME     PIC X(60).
                 07 CA-AC-ADDR-LINE1       PIC X(60).
                 07 CA-AC-ADDR-LINE2       PIC X(60).
                 07 CA-AC-CITY             PIC X(40).
                 07 CA-AC-POSTCODE         PIC X(8).
                 07 CA-AC-EMAIL            PIC X(100).
                 07 CA-AC-PHONE            PIC X(20).
              05 CA-AC-PERIOD-COUNT        PIC S9(4) COMP.
              05 CA-AC-PERIOD OCCURS 10 TIMES.
                 07 CA-AC-PP-ID            PIC S9(9) COMP.
                 07 CA-AC-PP-POL-NUM       PIC S9(9) COMP.
                 07 CA-AC-PP-PRODUCT       PIC X.
                 07 CA-AC-PP-STATUS        PIC X(4).
                 07 CA-AC-PP-EXP-DATE      PIC X(10).
              05 FILLER                    PIC X(3030).
