      * BILCDSB - body for BILDISB0 (disbursement service)
      * DSBCRT create (status PEND, or APPR when within user authority)
      * DSBAPR approve, DSBVOID void with reason, DSBINQ inquire
      * SOURCE-SYSTEM BILL | CLMS | COMM ; SOURCE-REF caller's key
              05 CA-DB-DISB-ID             PIC S9(9) COMP.
              05 CA-DB-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-DB-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-DB-REASON              PIC X(8).
              05 CA-DB-PAYEE-NAME          PIC X(60).
              05 CA-DB-PAYEE-ADDR1         PIC X(60).
              05 CA-DB-PAYEE-POSTCODE      PIC X(8).
              05 CA-DB-PAY-METHOD          PIC X(4).
              05 CA-DB-INSTR-ID            PIC S9(9) COMP.
              05 CA-DB-DUE-DATE            PIC X(10).
              05 CA-DB-STATUS              PIC X(4).
              05 CA-DB-SOURCE-SYSTEM       PIC X(4).
              05 CA-DB-SOURCE-REF          PIC X(20).
              05 CA-DB-VOID-REASON         PIC X(8).
              05 CA-DB-APPROVER            PIC X(8).
              05 FILLER                    PIC X(3627).
