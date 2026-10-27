      ******************************************************************
      * BILAGSR1 - BILAGSTM agency statement run report lines (132)    *
      ******************************************************************
       01  AGR-HEAD-1.
           05 FILLER                   PIC X(8)  VALUE 'BILAGSTM'.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(60) VALUE
              'BillingCoreAJ2026 - Agency-bill producer statements'.
           05 FILLER                   PIC X(14) VALUE
              'BUSINESS DATE '.
           05 AGR-H1-BUS-DATE          PIC X(10).
           05 FILLER                   PIC X(26) VALUE SPACES.
           05 FILLER                   PIC X(5)  VALUE 'PAGE '.
           05 AGR-H1-PAGE              PIC ZZZZ9.
       01  AGR-HEAD-2.
           05 FILLER                   PIC X(12) VALUE '  PRODUCER'.
           05 FILLER                   PIC X(30) VALUE 'NAME'.
           05 FILLER                   PIC X(8)  VALUE '  ITEMS'.
           05 FILLER                   PIC X(20) VALUE
              '             GROSS'.
           05 FILLER                   PIC X(20) VALUE
              '        COMMISSION'.
           05 FILLER                   PIC X(20) VALUE
              '           NET DUE'.
           05 FILLER                   PIC X(12) VALUE '   STMT ID'.
           05 FILLER                   PIC X(10) VALUE 'DUE DATE'.
       01  AGR-DETAIL.
           05 AGR-D-PRODUCER-CODE      PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 AGR-D-NAME               PIC X(28).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 AGR-D-ITEMS              PIC ZZZZZZ9.
           05 FILLER                   PIC X(1)  VALUE SPACES.
           05 AGR-D-GROSS              PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 AGR-D-COMMISSION         PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 AGR-D-NET                PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 AGR-D-STMT-ID            PIC Z(9)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 AGR-D-DUE-DATE           PIC X(10).
       01  AGR-TOTAL.
           05 AGR-T-LABEL              PIC X(40).
           05 AGR-T-COUNT              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 AGR-T-AMOUNT             PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                   PIC X(55) VALUE SPACES.
       01  AGR-MESSAGE.
           05 AGR-M-TEXT               PIC X(132).
