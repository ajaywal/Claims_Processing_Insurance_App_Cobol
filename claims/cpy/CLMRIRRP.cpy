      ******************************************************************
      * CLMRIRRP - CLMRIRE0 run report lines (132 columns)             *
      ******************************************************************
       01  RR-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMRIRE0'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - REINSURANCE CESSION'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 RR-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(43) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 RR-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  RR-HEAD-2.
           05 FILLER                       PIC X(41) VALUE
              ' CLAIM-NUMBER TREATY   TYPE P LOSS-DATE '.
           05 FILLER                       PIC X(25) VALUE
              '        GROSS-INCURRED'.
           05 FILLER                       PIC X(66) VALUE
              '         CEDED   PRIOR-CEDED      MOVEMENT NOTE'.
       01  RR-DETAIL.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-CLAIM-NUMBER            PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-TREATY                  PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-TYPE                    PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-POLICY-TYPE             PIC X.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-LOSS-DATE               PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-GROSS                   PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-CEDED                   PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-PRIOR                   PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-MOVEMENT                PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 RR-D-NOTE                    PIC X(29).
       01  RR-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 RR-T-LABEL                   PIC X(45).
           05 RR-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 RR-T-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(49) VALUE SPACES.
       01  RR-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 RR-M-TEXT                    PIC X(129).
