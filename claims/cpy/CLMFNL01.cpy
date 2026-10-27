      ******************************************************************
      * CLMFNL01 - CLMFNLP (CFNL) conversation state.                  *
      * FN-ENTRY holds the screen entry exactly as validated and       *
      * verified; PF5 compares the screen with it so a claim is never  *
      * created from data that was not verified.                       *
      * 2026-10-27 AJ  Initial version                                 *
      ******************************************************************
              05 FN-STEP                   PIC X.
                 88 FN-STEP-ENTRY           VALUE ' '.
                 88 FN-STEP-VERIFIED        VALUE 'V'.
                 88 FN-STEP-CREATED         VALUE 'C'.
              05 FN-ENTRY.
                 07 FN-E-POLICY-NO         PIC X(10).
                 07 FN-E-LOSS-DATE         PIC X(10).
                 07 FN-E-LOSS-TIME         PIC X(5).
                 07 FN-E-LOSS-TYPE         PIC X(4).
                 07 FN-E-LOSS-CAUSE        PIC X(10).
                 07 FN-E-COV-OVERRIDE      PIC X.
                 07 FN-E-DESC-LINE         PIC X(64) OCCURS 3 TIMES.
                 07 FN-E-LOSS-ADDR         PIC X(60).
                 07 FN-E-POSTCODE          PIC X(8).
                 07 FN-E-HOW-REPORTED      PIC X(4).
                 07 FN-E-RPT-FIRST         PIC X(20).
                 07 FN-E-RPT-LAST          PIC X(30).
                 07 FN-E-RPT-PHONE         PIC X(20).
                 07 FN-E-RPT-RELATION      PIC X(8).
              05 FN-VERIFIED.
                 07 FN-V-POLICY-NUM        PIC S9(9) COMP.
                 07 FN-V-CUSTOMER-NUM      PIC S9(9) COMP.
                 07 FN-V-POLICY-TYPE       PIC X.
                 07 FN-V-STATUS-AT-LOSS    PIC X(4).
                 07 FN-V-BILLING-STATUS    PIC X(4).
                 07 FN-V-COV-IN-QSTN       PIC X.
                 07 FN-V-LATE-DAYS         PIC S9(7) COMP-3.
              05 FN-CREATED.
                 07 FN-C-CLAIM-NUMBER      PIC X(12).
                 07 FN-C-CLAIM-ID          PIC S9(9) COMP.
