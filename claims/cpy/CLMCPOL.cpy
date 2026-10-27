      * CLMCPOL - body for CLMPOLVR (policy verification vs GenApp)
      * POLVRF  read GENAPP.POLICY + line table for POLICY-NUMBER;
      *         in force on LOSS-DATE -> VERIFIED 'Y'; build coverages
      *         and risk units; ask BillingCore status via event cache
              05 CA-PV-POLICY-NUMBER       PIC S9(9) COMP.
              05 CA-PV-LOSS-DATE           PIC X(10).
              05 CA-PV-VERIFIED            PIC X.
                 88 CA-PV-IS-VERIFIED       VALUE 'Y'.
              05 CA-PV-POLICY-TYPE         PIC X.
              05 CA-PV-CUSTOMER-NUM        PIC S9(9) COMP.
              05 CA-PV-EFF-DATE            PIC X(10).
              05 CA-PV-EXP-DATE            PIC X(10).
              05 CA-PV-INSURED-NAME        PIC X(60).
              05 CA-PV-PRODUCER-CODE       PIC S9(9) COMP.
              05 CA-PV-STATUS-AT-LOSS      PIC X(4).
              05 CA-PV-BILLING-STATUS      PIC X(4).
              05 CA-PV-COV-COUNT           PIC S9(4) COMP.
              05 CA-PV-COV OCCURS 10 TIMES.
                 07 CA-PV-C-TYPE           PIC X(8).
                 07 CA-PV-C-RU-SEQ         PIC S9(4) COMP.
                 07 CA-PV-C-LIMIT          PIC S9(11)V99 COMP-3.
                 07 CA-PV-C-DEDUCTIBLE     PIC S9(11)V99 COMP-3.
              05 CA-PV-RU-COUNT            PIC S9(4) COMP.
              05 CA-PV-RU OCCURS 5 TIMES.
                 07 CA-PV-R-TYPE           PIC X(4).
                 07 CA-PV-R-DESC           PIC X(60).
                 07 CA-PV-R-REG            PIC X(8).
                 07 CA-PV-R-MAKE           PIC X(15).
                 07 CA-PV-R-MODEL          PIC X(15).
                 07 CA-PV-R-POSTCODE       PIC X(8).
                 07 CA-PV-R-VALUE          PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(2899).
