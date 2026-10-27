      ******************************************************************
      * BILACHRP - BILACHRT returns and settlement report (132 cols)   *
      ******************************************************************
       01  ARP-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILACHRT'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(56) VALUE
              'BillingCoreAJ2026 - Direct debit returns and settlement'.
           05 FILLER                   PIC X(8)  VALUE 'CUTOFF '.
           05 ARP-H1-CUTOFF            PIC X(10).
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 ARP-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(8)  VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 ARP-H1-PAGE              PIC ZZZZ9.
       01  ARP-HEAD-2.
           05 FILLER                   PIC X(6)  VALUE 'TYPE'.
           05 FILLER                   PIC X(11) VALUE 'REQUEST ID'.
           05 FILLER                   PIC X(12) VALUE 'ACCOUNT NO'.
           05 FILLER                   PIC X(5)  VALUE 'RTN'.
           05 FILLER                   PIC X(19) VALUE
              '             AMOUNT'.
           05 FILLER                   PIC X(12) VALUE '  DRAFT ON'.
           05 FILLER                   PIC X(11) VALUE '   MONEY ID'.
           05 FILLER                   PIC X(56) VALUE '  RESULT'.
       01  ARP-DETAIL.
           05 ARP-D-TYPE               PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-REQUEST-ID         PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-ACCOUNT-NO         PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-RETURN-CODE        PIC X(3).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-AMOUNT             PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-DRAFT-DATE         PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-MONEY-ID           PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 ARP-D-RESULT             PIC X(56).
       01  ARP-TOTAL.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 ARP-T-LABEL              PIC X(40).
           05 ARP-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 ARP-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  ARP-MESSAGE.
           05 FILLER                   PIC X(4)  VALUE '*** '.
           05 ARP-M-TEXT               PIC X(128).
