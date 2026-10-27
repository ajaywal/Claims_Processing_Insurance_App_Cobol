      ******************************************************************
      * BILDSBR1 - BILDSBOV overpayment sweep report lines (132 cols)  *
      ******************************************************************
       01  OVR-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILDSBOV'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(60) VALUE
              'BillingCoreAJ2026 - Account overpayment refund sweep'.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 OVR-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(26) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 OVR-H1-PAGE              PIC ZZZZ9.
       01  OVR-HEAD-2.
           05 FILLER                   PIC X(12) VALUE 'ACCOUNT NO'.
           05 FILLER                   PIC X(20) VALUE
              '         UNAPPLIED'.
           05 FILLER                   PIC X(20) VALUE
              '        REFUND AMT'.
           05 FILLER                   PIC X(6)  VALUE 'MTHD'.
           05 FILLER                   PIC X(38) VALUE 'PAYEE'.
           05 FILLER                   PIC X(12) VALUE 'DISB ID'.
           05 FILLER                   PIC X(6)  VALUE 'STAT'.
           05 FILLER                   PIC X(18) VALUE 'RESULT'.
       01  OVR-DETAIL.
           05 OVR-D-ACCOUNT-NO         PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-UNAPPLIED          PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-REFUND             PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-METHOD             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-PAYEE              PIC X(36).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-DISB-ID            PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-STATUS             PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 OVR-D-RESULT             PIC X(18).
       01  OVR-TOTAL.
           05 OVR-T-LABEL              PIC X(40).
           05 OVR-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 OVR-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  OVR-MESSAGE.
           05 OVR-M-TEXT               PIC X(132).
