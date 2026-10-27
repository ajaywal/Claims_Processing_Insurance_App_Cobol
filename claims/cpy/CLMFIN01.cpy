      ******************************************************************
      * CLMFIN01 - CLMFINP (CFIN) conversation state.                  *
      * TF-ENTRY is the screen entry as validated by ENTER; TF-REQUEST *
      * the resolved request PF5 posts. PF5 refuses if the screen no   *
      * longer matches TF-ENTRY.                                       *
      * 2026-10-27 AJ  Initial version                                 *
      ******************************************************************
              05 TF-STEP                   PIC X.
                 88 TF-STEP-ENTRY           VALUE ' '.
                 88 TF-STEP-CONFIRM         VALUE 'C'.
                 88 TF-STEP-POSTED          VALUE 'D'.
              05 TF-ENTRY.
                 07 TF-E-CLAIM-NUMBER      PIC X(12).
                 07 TF-E-EXPOSURE-SEQ      PIC X(3).
                 07 TF-E-ACTION            PIC X.
                 07 TF-E-COST-TYPE         PIC X(4).
                 07 TF-E-COST-CATEGORY     PIC X(8).
                 07 TF-E-AMOUNT            PIC X(15).
                 07 TF-E-PAYEE             PIC X(9).
                 07 TF-E-PAYMENT-TYPE      PIC X(4).
                 07 TF-E-PAY-METHOD        PIC X(4).
                 07 TF-E-SCHED-DATE        PIC X(10).
                 07 TF-E-TXN-ID            PIC X(9).
                 07 TF-E-REJECT-REASON     PIC X(8).
              05 TF-REQUEST.
                 07 TF-R-REQUEST-ID        PIC X(8).
                 07 TF-R-CLAIM-ID          PIC S9(9) COMP.
                 07 TF-R-EXPOSURE-ID       PIC S9(9) COMP.
                 07 TF-R-AMOUNT            PIC S9(11)V99 COMP-3.
                 07 TF-R-PAYEE-ID          PIC S9(9) COMP.
                 07 TF-R-SCHED-DATE        PIC X(10).
                 07 TF-R-TXN-ID            PIC S9(9) COMP.
