      ******************************************************************
      * BILACHDP - BILACHDR direct debit draft report lines (132 cols) *
      ******************************************************************
       01  ADP-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILACHDR'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(50) VALUE
              'BillingCoreAJ2026 - Direct debit draft generation'.
           05 FILLER                   PIC X(10) VALUE 'HORIZON '.
           05 ADP-H1-HORIZON           PIC X(10).
           05 FILLER                   PIC X(6)  VALUE SPACES.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 ADP-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 ADP-H1-PAGE              PIC ZZZZ9.
       01  ADP-HEAD-2.
           05 FILLER                   PIC X(12) VALUE 'ACCOUNT NO'.
           05 FILLER                   PIC X(14) VALUE 'INVOICE NO'.
           05 FILLER                   PIC X(12) VALUE 'DUE DATE'.
           05 FILLER                   PIC X(19) VALUE
              '         AMOUNT DUE'.
           05 FILLER                   PIC X(19) VALUE
              '       DRAFT AMOUNT'.
           05 FILLER                   PIC X(12) VALUE '  DRAFT ON'.
           05 FILLER                   PIC X(11) VALUE ' REQUEST ID'.
           05 FILLER                   PIC X(33) VALUE '  RESULT'.
       01  ADP-DETAIL.
           05 ADP-D-ACCOUNT-NO         PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-INVOICE-NO         PIC X(12).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-DUE-DATE           PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-AMOUNT-DUE         PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-DRAFT-AMOUNT       PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-DRAFT-DATE         PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-REQUEST-ID         PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ADP-D-RESULT             PIC X(33).
       01  ADP-TOTAL.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 ADP-T-LABEL              PIC X(40).
           05 ADP-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 ADP-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  ADP-MESSAGE.
           05 FILLER                   PIC X(4)  VALUE '*** '.
           05 ADP-M-TEXT               PIC X(128).
