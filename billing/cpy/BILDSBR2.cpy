      ******************************************************************
      * BILDSBR2 - BILDSBSN disbursement send report lines (132 cols)  *
      ******************************************************************
       01  SNR-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILDSBSN'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(60) VALUE
              'BillingCoreAJ2026 - Disbursement payment file run'.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 SNR-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(26) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 SNR-H1-PAGE              PIC ZZZZ9.
       01  SNR-HEAD-2.
           05 FILLER                   PIC X(12) VALUE '   DISB ID'.
           05 FILLER                   PIC X(10) VALUE 'REASON'.
           05 FILLER                   PIC X(6)  VALUE 'SRC'.
           05 FILLER                   PIC X(6)  VALUE 'MTHD'.
           05 FILLER                   PIC X(20) VALUE
              '            AMOUNT'.
           05 FILLER                   PIC X(12) VALUE 'DUE DATE'.
           05 FILLER                   PIC X(38) VALUE 'PAYEE'.
           05 FILLER                   PIC X(28) VALUE 'RESULT'.
       01  SNR-DETAIL.
           05 SNR-D-DISB-ID            PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-REASON             PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-SOURCE             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-METHOD             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-DUE-DATE           PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-PAYEE              PIC X(36).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 SNR-D-RESULT             PIC X(28).
       01  SNR-TOTAL.
           05 SNR-T-LABEL              PIC X(40).
           05 SNR-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 SNR-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  SNR-MESSAGE.
           05 SNR-M-TEXT               PIC X(132).
