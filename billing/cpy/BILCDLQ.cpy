      * BILCDLQ - body for BILDLQSV (delinquency service)
      * DLQSTRT open process for ACCOUNT-ID/PP-ID if past due > threshold
      * DLQEXIT close open processes of ACCOUNT-ID when past due cleared
      * DLQINQ  open process with its event steps
              05 CA-DQ-DP-ID               PIC S9(9) COMP.
              05 CA-DQ-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-DQ-PP-ID               PIC S9(9) COMP.
              05 CA-DQ-PLAN-CODE           PIC X(8).
              05 CA-DQ-PASTDUE-AMOUNT      PIC S9(11)V99 COMP-3.
              05 CA-DQ-START-DATE          PIC X(10).
              05 CA-DQ-STATUS              PIC X(4).
              05 CA-DQ-EXIT-REASON         PIC X(8).
              05 CA-DQ-STEP-COUNT          PIC S9(4) COMP.
              05 CA-DQ-STEP OCCURS 10 TIMES.
                 07 CA-DQ-S-SEQ            PIC S9(4) COMP.
                 07 CA-DQ-S-EVENT          PIC X(8).
                 07 CA-DQ-S-TARGET         PIC X(10).
                 07 CA-DQ-S-DONE           PIC X(10).
                 07 CA-DQ-S-STATUS         PIC X(4).
              05 FILLER                    PIC X(3449).
