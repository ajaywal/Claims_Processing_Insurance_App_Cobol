      ******************************************************************
      * CLMFNLRP - CLMFNLBT run report lines (132 columns)             *
      ******************************************************************
       01  FP-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMFNLBT'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - BULK FNOL INTAKE RUN'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 FP-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(12) VALUE
              '   RUN MODE '.
           05 FP-H1-MODE                   PIC X(8).
           05 FILLER                       PIC X(23) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 FP-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  FP-HEAD-2.
           05 FILLER                       PIC X(56) VALUE
              ' INTAKE-ID SRC  POLICY-NO  LOSS-DATE  TYPE CLAIM-NUMBER'.
           05 FILLER                       PIC X(76) VALUE
              ' EXP-TYPE SV   INIT-RESERVE STAT ERROR-CD MESSAGE'.
       01  FP-DETAIL.
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-INTAKE-ID               PIC Z(8)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-SOURCE                  PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-POLICY                  PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-LOSS-DATE               PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-LOSS-TYPE               PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-CLAIM-NUMBER            PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-EXP-TYPE                PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-SEVERITY                PIC Z9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-RESERVE                 PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-STATUS                  PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-ERROR-CODE              PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 FP-D-MESSAGE                 PIC X(35).
       01  FP-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 FP-T-LABEL                   PIC X(40).
           05 FP-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 FP-T-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(54) VALUE SPACES.
       01  FP-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 FP-M-TEXT                    PIC X(129).
