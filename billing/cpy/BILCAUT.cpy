      * BILCAUT - body for BILAUTHZ (authority limit check)
      * Limit types: WRITEOFF DISBURSE REVERSAL PLANCHG HOLDREL
              05 CA-AT-USER                PIC X(8).
              05 CA-AT-LIMIT-TYPE          PIC X(8).
              05 CA-AT-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-AT-ALLOWED             PIC X.
                 88 CA-AT-IS-ALLOWED        VALUE 'Y'.
              05 CA-AT-LIMIT               PIC S9(11)V99 COMP-3.
              05 CA-AT-APPROVER            PIC X(8).
              05 FILLER                    PIC X(3801).
