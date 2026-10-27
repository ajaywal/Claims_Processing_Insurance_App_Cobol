      * CLMCLIT - body for CLMLITIG (litigation matters)
      * LITNEW (sets claim LITIGATION-FLAG, reassigns to LIT group),
      * LITUPD, LITCLS with resolution/settlement, LITINQ
              05 CA-LT-MATTER-ID           PIC S9(9) COMP.
              05 CA-LT-CLAIM-ID            PIC S9(9) COMP.
              05 CA-LT-MATTER-TYPE         PIC X(8).
              05 CA-LT-COURT               PIC X(60).
              05 CA-LT-CASE-NUMBER         PIC X(20).
              05 CA-LT-PLAINTIFF-ATTY      PIC S9(9) COMP.
              05 CA-LT-DEFENSE-ATTY        PIC S9(9) COMP.
              05 CA-LT-FILE-DATE           PIC X(10).
              05 CA-LT-TRIAL-DATE          PIC X(10).
              05 CA-LT-STATUS              PIC X(4).
              05 CA-LT-RESOLUTION          PIC X(8).
              05 CA-LT-SETTLEMENT          PIC S9(11)V99 COMP-3.
              05 CA-LT-LEGAL-SPEND         PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3690).
