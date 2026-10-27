      * BILCINV - body for BILINVSV (invoice service)
      * INVINQ  by INVOICE-NUMBER or INVOICE-ID, with items
      * INVLST  latest invoices for ACCOUNT-ID (newest first)
      * INVADH  ad-hoc invoice now for all PLANned items of ACCOUNT-ID
      *         with event date <= CUTOFF-DATE
      * INVRSN  re-send: new document request for the invoice
              05 CA-IV-INVOICE-ID          PIC S9(9) COMP.
              05 CA-IV-INVOICE-NUMBER      PIC X(12).
              05 CA-IV-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-IV-CUTOFF-DATE         PIC X(10).
              05 CA-IV-BILL-DATE           PIC X(10).
              05 CA-IV-DUE-DATE            PIC X(10).
              05 CA-IV-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-IV-AMOUNT-DUE          PIC S9(11)V99 COMP-3.
              05 CA-IV-STATUS              PIC X(4).
              05 CA-IV-ROW-COUNT           PIC S9(4) COMP.
              05 CA-IV-ROW OCCURS 20 TIMES.
                 07 CA-IV-R-ID             PIC S9(9) COMP.
                 07 CA-IV-R-NUMBER         PIC X(12).
                 07 CA-IV-R-DATE           PIC X(10).
                 07 CA-IV-R-DUE            PIC X(10).
                 07 CA-IV-R-AMOUNT         PIC S9(11)V99 COMP-3.
                 07 CA-IV-R-PAID           PIC S9(11)V99 COMP-3.
                 07 CA-IV-R-STATUS         PIC X(4).
                 07 CA-IV-R-DESC           PIC X(20).
              05 FILLER                    PIC X(2290).
