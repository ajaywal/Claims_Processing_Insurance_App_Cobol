      ******************************************************************
      * CLMRSVR1 - CLMRSVRP 132-column reserve adequacy report lines   *
      ******************************************************************
       01  RVR-HEAD-1.
           05 FILLER                       PIC X(10) VALUE 'CLMRSVRP'.
           05 FILLER                       PIC X(50)
              VALUE 'CLAIMSCORE AJ2026 - RESERVE ADEQUACY REVIEW      '.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 RVR-H1-BUS-DATE              PIC X(10).
           05 FILLER                       PIC X(11) VALUE '  RUN AT  '.
           05 RVR-H1-RUN-TS                PIC X(19).
           05 FILLER                       PIC X(7)  VALUE '  PAGE '.
           05 RVR-H1-PAGE                  PIC ZZZ9.
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  RVR-HEAD-2.
           05 FILLER                       PIC X(132) VALUE ALL '-'.
       01  RVR-COL-HEAD.
           05 FILLER                       PIC X(44) VALUE
              ' CLAIM NUMBER EXPOSURE  TYPE     S  OPEN RSV '.
           05 FILLER                       PIC X(44) VALUE
              '        PAID   BENCHMARK  PCT LAST RSV   DAYS'.
           05 FILLER                       PIC X(44) VALUE
              '  PAID/30D FLAGS               ACTIVITY     '.
       01  RVR-DET-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-CLAIM-NUMBER           PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-EXPOSURE-ID            PIC Z(8)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-EXP-TYPE               PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-SEVERITY               PIC 9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-OPEN-RESERVE           PIC ZZ,ZZZ,ZZ9-.
           05 RVR-D-PAID                   PIC ZZZ,ZZZ,ZZ9-.
           05 RVR-D-BENCHMARK              PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-PCT                    PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-LAST-CHANGE            PIC X(10).
           05 RVR-D-DAYS                   PIC ZZZZZ9.
           05 RVR-D-VELOCITY               PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-FLAGS                  PIC X(20).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-D-ACTIVITY               PIC X(8).
       01  RVR-TOT-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-T-LABEL                  PIC X(60).
           05 RVR-T-VALUE                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(52) VALUE SPACES.
       01  RVR-MSG-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RVR-MSG-TEXT                 PIC X(131).
