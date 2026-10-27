      ******************************************************************
      * CLMDUPRP - CLMDUPCK run report lines (132 columns)             *
      ******************************************************************
       01  DR-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMDUPCK'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - DUPLICATE CLAIM DETECTION'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 DR-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(43) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 DR-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  DR-HEAD-2.
           05 FILLER                       PIC X(56) VALUE
              ' LATER-CLAIM  LOSS-DATE  CAUSE      EARLIER-CLM  LOSS-D'.
           05 FILLER                       PIC X(76) VALUE
              'ATE  CAUSE      BASIS DAYS ACTION     SIU REF NOTE'.
       01  DR-DETAIL.
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-LATER-CLAIM             PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-LATER-DATE              PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-LATER-CAUSE             PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-EARLIER-CLAIM           PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-EARLIER-DATE            PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-EARLIER-CAUSE           PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-BASIS                   PIC X(4).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 DR-D-DAYS                    PIC Z9.
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 DR-D-ACTION                  PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-SIU-SCORE               PIC ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-REFERRED                PIC X(3).
           05 FILLER                       PIC X     VALUE SPACE.
           05 DR-D-NOTE                    PIC X(32).
       01  DR-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 DR-T-LABEL                   PIC X(45).
           05 DR-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(73) VALUE SPACES.
       01  DR-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 DR-M-TEXT                    PIC X(129).
