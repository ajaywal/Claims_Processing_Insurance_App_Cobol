      * BILCPAY - body for BILPAYRC (payment receipt service)
      * PAYRCV  record receipt; resolve account by ACCOUNT-NUMBER or
      *         INVOICE-NUMBER; distribute; unresolved -> suspense
      * PAYREV  reverse MONEY-ID (NSF, wrong account, refund)
      * PAYINQ  receipt with its distributions
      * PAYMAT  match suspense SUSP-ID to ACCOUNT-NUMBER and apply
              05 CA-PY-MONEY-ID            PIC S9(9) COMP.
              05 CA-PY-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-PY-ACCOUNT-NUMBER      PIC X(10).
              05 CA-PY-INVOICE-NUMBER      PIC X(12).
              05 CA-PY-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-PY-METHOD              PIC X(4).
              05 CA-PY-REF-NUMBER          PIC X(20).
              05 CA-PY-SOURCE              PIC X(4).
              05 CA-PY-RECEIVED-DATE       PIC X(10).
              05 CA-PY-INSTR-ID            PIC S9(9) COMP.
              05 CA-PY-REVERSAL-REASON     PIC X(8).
              05 CA-PY-SUSP-ID             PIC S9(9) COMP.
              05 CA-PY-BATCH-REF           PIC X(12).
              05 CA-PY-DISTRIBUTED         PIC S9(11)V99 COMP-3.
              05 CA-PY-UNAPPLIED           PIC S9(11)V99 COMP-3.
              05 CA-PY-STATUS              PIC X(4).
              05 CA-PY-DIST-COUNT          PIC S9(4) COMP.
              05 CA-PY-DIST OCCURS 20 TIMES.
                 07 CA-PY-D-ITEM-ID        PIC S9(9) COMP.
                 07 CA-PY-D-INVOICE-NO     PIC X(12).
                 07 CA-PY-D-AMOUNT         PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3257).
