      ******************************************************************
      * BILLKBP1 - BILLKBX0 lockbox posting report lines (132 cols)    *
      ******************************************************************
       01  LKP-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILLKBX0'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(50) VALUE
              'BillingCoreAJ2026 - Bank lockbox receipt posting'.
           05 FILLER                   PIC X(8)  VALUE 'BATCH '.
           05 LKP-H1-BATCH-REF         PIC X(12).
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 LKP-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(12) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 LKP-H1-PAGE              PIC ZZZZ9.
       01  LKP-HEAD-2.
           05 FILLER                   PIC X(8)  VALUE 'SEQ'.
           05 FILLER                   PIC X(12) VALUE 'ACCOUNT NO'.
           05 FILLER                   PIC X(14) VALUE 'INVOICE NO'.
           05 FILLER                   PIC X(12) VALUE 'CHEQUE NO'.
           05 FILLER                   PIC X(19) VALUE
              '      CHEQUE AMOUNT'.
           05 FILLER                   PIC X(11) VALUE '   MONEY ID'.
           05 FILLER                   PIC X(6)  VALUE '  STAT'.
           05 FILLER                   PIC X(19) VALUE
              '        DISTRIBUTED'.
           05 FILLER                   PIC X(31) VALUE '  RESULT'.
       01  LKP-DETAIL.
           05 LKP-D-SEQ                PIC Z(5)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-ACCOUNT-NO         PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-INVOICE-NO         PIC X(12).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-CHEQUE-NO          PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-AMOUNT             PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-MONEY-ID           PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-STATUS             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-DISTRIBUTED        PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 LKP-D-RESULT             PIC X(31).
       01  LKP-TOTAL.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 LKP-T-LABEL              PIC X(40).
           05 LKP-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 LKP-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  LKP-MESSAGE.
           05 FILLER                   PIC X(4)  VALUE '*** '.
           05 LKP-M-TEXT               PIC X(128).
