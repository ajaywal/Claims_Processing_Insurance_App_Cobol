      ******************************************************************
      * CLMSEARP - CLMSEALR run report lines (132 columns)             *
      ******************************************************************
       01  SR-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMSEALR'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - AUDIT CHAIN SEAL'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 SR-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(43) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 SR-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  SR-SLICE-LINE.
           05 FILLER                       PIC X(8)  VALUE ' SLICE '.
           05 SR-S-SLICE                   PIC ZZ9.
           05 FILLER                       PIC X(7)  VALUE '  RC '.
           05 SR-S-RC                      PIC 99.
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 SR-S-REASON                  PIC X(8).
           05 FILLER                       PIC X(16) VALUE
              '  ROWS CHECKED '.
           05 SR-S-ROWS                    PIC ZZZ,ZZ9.
           05 FILLER                       PIC X(17) VALUE
              '  VERIFIED TO '.
           05 SR-S-TO-SEQ                  PIC Z(8)9.
           05 FILLER                       PIC X(8)  VALUE '  HASH '.
           05 SR-S-HASH                    PIC X(16).
           05 FILLER                       PIC X(29) VALUE SPACES.
       01  SR-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 SR-T-LABEL                   PIC X(45).
           05 SR-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 SR-T-TEXT                    PIC X(70).
       01  SR-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 SR-M-TEXT                    PIC X(129).
