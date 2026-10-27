      ******************************************************************
      * BILCOMR1 - BILCOMPY producer payment run report lines (132)    *
      ******************************************************************
       01  CPR-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILCOMPY'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(60) VALUE
              'BillingCoreAJ2026 - Producer commission payment run'.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 CPR-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 CPR-H1-CYCLE             PIC X(24).
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 CPR-H1-PAGE              PIC ZZZZ9.
       01  CPR-HEAD-2.
           05 FILLER                   PIC X(12) VALUE '  PRODUCER'.
           05 FILLER                   PIC X(32) VALUE 'NAME'.
           05 FILLER                   PIC X(4)  VALUE 'CYC'.
           05 FILLER                   PIC X(8)  VALUE '   ITEMS'.
           05 FILLER                   PIC X(20) VALUE
              '            EARNED'.
           05 FILLER                   PIC X(20) VALUE
              '         THRESHOLD'.
           05 FILLER                   PIC X(12) VALUE '   PAY ID'.
           05 FILLER                   PIC X(24) VALUE 'RESULT'.
       01  CPR-DETAIL.
           05 CPR-D-PRODUCER-CODE      PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 CPR-D-NAME               PIC X(30).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 CPR-D-CYCLE              PIC X.
           05 FILLER                   PIC X(3)  VALUE SPACES.
           05 CPR-D-ITEMS              PIC ZZZZZZ9.
           05 FILLER                   PIC X(1)  VALUE SPACES.
           05 CPR-D-EARNED             PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 CPR-D-THRESHOLD          PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 CPR-D-PAY-ID             PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 CPR-D-RESULT             PIC X(24).
       01  CPR-TOTAL.
           05 CPR-T-LABEL              PIC X(40).
           05 CPR-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 CPR-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  CPR-MESSAGE.
           05 CPR-M-TEXT               PIC X(132).
