      ******************************************************************
      * BILEVTO1 - Operations work file (FB 120): CANCREQ events for   *
      * GenApp, which has no inbound API. Operations cancel the policy *
      * in GenApp by hand; the change then flows back via BILGAPIN.    *
      ******************************************************************
           05 OPS-EVENT-ID             PIC 9(10).
           05 OPS-EVENT-TYPE           PIC X(8).
           05 OPS-POLICY-NO            PIC 9(10).
           05 OPS-CANCEL-DATE          PIC X(10).
           05 OPS-REASON               PIC X(8).
           05 OPS-PASTDUE              PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
           05 OPS-BUS-DATE             PIC X(10).
           05 OPS-ACTION               PIC X(50).
