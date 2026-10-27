      ******************************************************************
      * CLMLAGRP - 132-column loss run and aging report lines          *
      ******************************************************************
       01  LAR-HEAD-1.
           05 FILLER                       PIC X(10) VALUE 'CLMLAGRP'.
           05 FILLER                       PIC X(50)
              VALUE 'CLAIMSCORE AJ2026 - LOSS RUN AND AGING REPORT    '.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 LAR-H1-BUS-DATE              PIC X(10).
           05 FILLER                       PIC X(11) VALUE '  RUN AT  '.
           05 LAR-H1-RUN-TS                PIC X(19).
           05 FILLER                       PIC X(7)  VALUE '  PAGE '.
           05 LAR-H1-PAGE                  PIC ZZZ9.
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  LAR-HEAD-2.
           05 FILLER                       PIC X(132) VALUE ALL '-'.
       01  LAR-BREAK-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-BRK-LABEL                PIC X(10).
           05 LAR-BRK-CODE                 PIC X(9).
           05 LAR-BRK-NAME                 PIC X(40).
           05 FILLER                       PIC X(72) VALUE SPACES.
       01  LAR-OPEN-HEAD.
           05 FILLER                       PIC X(50) VALUE
              '   CLAIM NUMBER P LOSS LOSS DATE  REPORTED    AGE '.
           05 FILLER                       PIC X(40) VALUE
              'BKT           RESERVES            PAID   '.
           05 FILLER                       PIC X(42) VALUE
              '      INCURRED FLAGS'.
       01  LAR-OPEN-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 LAR-O-CLAIM-NUMBER           PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-POL-TYPE               PIC X.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-LOSS-TYPE              PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-LOSS-DATE              PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-REPORTED               PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-AGE                    PIC ZZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-BUCKET                 PIC X(7).
           05 LAR-O-RESERVES               PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-PAID                   PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-INCURRED               PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-O-FLAGS                  PIC X(20).
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  LAR-SUB-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-S-LABEL                  PIC X(24).
           05 LAR-S-CLAIMS                 PIC ZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-S-BKT OCCURS 5 TIMES.
              10 LAR-S-BKT-COUNT           PIC ZZ,ZZ9.
              10 FILLER                    PIC X.
           05 LAR-S-RESERVES               PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-S-PAID                   PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-S-INCURRED               PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(18) VALUE SPACES.
       01  LAR-SUB-HEAD.
           05 FILLER                       PIC X(32) VALUE
              ' TOTAL                   CLAIMS '.
           05 FILLER                       PIC X(35) VALUE
              '  0-30  31-90 91-180 181-365  365+ '.
           05 FILLER                       PIC X(65) VALUE
              '       RESERVES            PAID        INCURRED'.
       01  LAR-CLOSED-HEAD.
           05 FILLER                       PIC X(50) VALUE
              '   CLAIM NUMBER SEGMENT  ADJUSTER CLOSE DT   REASON'.
           05 FILLER                       PIC X(40) VALUE
              '   DAYS OPEN            PAID       RECOV'.
           05 FILLER                       PIC X(42) VALUE
              'ERED        INCURRED'.
       01  LAR-CLOSED-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 LAR-C-CLAIM-NUMBER           PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-SEGMENT                PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-ADJUSTER               PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-CLOSE-DATE             PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-REASON                 PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-DAYS-OPEN              PIC ZZZZ9.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 LAR-C-PAID                   PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-RECOVERED              PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-C-INCURRED               PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(23) VALUE SPACES.
       01  LAR-MSG-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 LAR-MSG-TEXT                 PIC X(80).
           05 LAR-MSG-NUM                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(32) VALUE SPACES.
