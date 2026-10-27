      ******************************************************************
      * CLMCHKRP - CLMCHKIS run report lines (132 columns)             *
      ******************************************************************
       01  CR-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMCHKIS'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - CLAIM CHECK ISSUANCE'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 CR-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(43) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 CR-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  CR-HEAD-2.
           05 FILLER                       PIC X(60) VALUE
              '  CHECK-ID CLAIM-NUMBER CHECK-NO   SCHEDULED  METH'.
           05 FILLER                       PIC X(40) VALUE
              '        AMOUNT PAYEE'.
           05 FILLER                       PIC X(32) VALUE
              '                    STAT REASON'.
       01  CR-DETAIL.
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-CHECK-ID                PIC Z(8)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-CLAIM-NUMBER            PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-CHECK-NUMBER            PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-SCHED-DATE              PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-METHOD                  PIC X(4).
           05 FILLER                       PIC X(5)  VALUE SPACES.
           05 CR-D-AMOUNT                  PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-PAYEE                   PIC X(30).
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-STATUS                  PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-REASON                  PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 CR-D-TEXT                    PIC X(17).
       01  CR-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 CR-T-LABEL                   PIC X(40).
           05 CR-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 CR-T-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(54) VALUE SPACES.
       01  CR-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 CR-M-TEXT                    PIC X(129).
