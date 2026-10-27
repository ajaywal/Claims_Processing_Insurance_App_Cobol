      ******************************************************************
      * CLMESCRP - CLMESCAL run report lines (132 columns)             *
      ******************************************************************
       01  ER-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMESCAL'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - ACTIVITY ESCALATION SWEEP'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 ER-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(43) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 ER-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  ER-ESC-DETAIL.
           05 FILLER                       PIC X(3)  VALUE ' E '.
           05 ER-E-ACTIVITY-ID             PIC Z(8)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-E-CLAIM-NUMBER            PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-E-SUBJECT                 PIC X(40).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-E-DUE-DATE                PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-E-ESC-DATE                PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-E-FROM-USER               PIC X(8).
           05 FILLER                       PIC X(4)  VALUE ' -> '.
           05 ER-E-TO-USER                 PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-E-NOTE                    PIC X(22).
       01  ER-OVD-DETAIL.
           05 FILLER                       PIC X(3)  VALUE ' O '.
           05 ER-O-USER                    PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-O-USER-NAME               PIC X(40).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-O-COUNT                   PIC ZZ,ZZ9.
           05 FILLER                       PIC X(14) VALUE
              '  OLDEST DUE '.
           05 ER-O-OLDEST                  PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-O-ALERT                   PIC X(20).
           05 FILLER                       PIC X(28) VALUE SPACES.
       01  ER-AGED-DETAIL.
           05 FILLER                       PIC X(3)  VALUE ' A '.
           05 ER-A-CLAIM-NUMBER            PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-A-OPEN-DATE               PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-A-USER                    PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-A-RESERVES                PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-A-PATTERN                 PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-A-ACTIVITY-ID             PIC Z(8)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ER-A-NOTE                    PIC X(61).
       01  ER-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 ER-T-LABEL                   PIC X(45).
           05 ER-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(73) VALUE SPACES.
       01  ER-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 ER-M-TEXT                    PIC X(129).
