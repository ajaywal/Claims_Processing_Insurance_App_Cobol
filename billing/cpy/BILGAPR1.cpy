      ******************************************************************
      * BILGAPR1 - BILGAPIN GenApp policy intake report lines (132)    *
      ******************************************************************
       01  GPR-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILGAPIN'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(60) VALUE
              'BillingCoreAJ2026 - GenApp policy intake'.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 GPR-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(26) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 GPR-H1-PAGE              PIC ZZZZ9.
       01  GPR-HEAD-2.
           05 FILLER                   PIC X(12) VALUE '    POLICY'.
           05 FILLER                   PIC X(12) VALUE '  CUSTOMER'.
           05 FILLER                   PIC X(3)  VALUE 'P'.
           05 FILLER                   PIC X(10) VALUE 'EVENT'.
           05 FILLER                   PIC X(20) VALUE
              '   BILLED PREMIUM'.
           05 FILLER                   PIC X(20) VALUE
              '   GENAPP PREMIUM'.
           05 FILLER                   PIC X(12) VALUE 'ACCOUNT'.
           05 FILLER                   PIC X(12) VALUE '     PP ID'.
           05 FILLER                   PIC X(31) VALUE 'RESULT'.
       01  GPR-DETAIL.
           05 GPR-D-POLICY             PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-CUSTOMER           PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-PRODUCT            PIC X.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-EVENT              PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-OLD-PREMIUM        PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-NEW-PREMIUM        PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-ACCOUNT            PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-PP-ID              PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 GPR-D-RESULT             PIC X(31).
       01  GPR-TOTAL.
           05 GPR-T-LABEL              PIC X(40).
           05 GPR-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 GPR-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  GPR-MESSAGE.
           05 GPR-M-TEXT               PIC X(132).
