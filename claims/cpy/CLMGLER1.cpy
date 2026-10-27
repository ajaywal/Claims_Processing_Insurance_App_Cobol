      ******************************************************************
      * CLMGLER1 - CLMGLEXT 132-column run report lines                *
      ******************************************************************
       01  GLR-HEAD-1.
           05 FILLER                       PIC X(10) VALUE 'CLMGLEXT'.
           05 FILLER                       PIC X(50)
              VALUE 'CLAIMSCORE AJ2026 - CLAIMS GENERAL LEDGER EXTRACT'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 GLR-H1-BUS-DATE              PIC X(10).
           05 FILLER                       PIC X(11) VALUE '  RUN AT  '.
           05 GLR-H1-RUN-TS                PIC X(19).
           05 FILLER                       PIC X(7)  VALUE '  PAGE '.
           05 GLR-H1-PAGE                  PIC ZZZ9.
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  GLR-HEAD-2.
           05 FILLER                       PIC X(132) VALUE ALL '-'.
       01  GLR-COL-HEAD.
           05 FILLER                       PIC X(48) VALUE
              ' POL CCTR COST TXN   BUCKET  DEBIT  CREDIT     '.
           05 FILLER                       PIC X(84) VALUE
              '  TXNS                AMOUNT  NOTE'.
       01  GLR-DET-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-D-POL-TYPE               PIC X(3).
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-D-CCTR                   PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-D-COST-TYPE              PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-D-TXN-TYPE               PIC X(4).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 GLR-D-BUCKET                 PIC X(6).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 GLR-D-DR-ACCT                PIC X(6).
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-D-CR-ACCT                PIC X(6).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 GLR-D-TXN-COUNT              PIC Z,ZZZ,ZZ9.
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 GLR-D-AMOUNT                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 GLR-D-NOTE                   PIC X(56).
       01  GLR-TOT-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-T-LABEL                  PIC X(50).
           05 GLR-T-COUNT                  PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(4)  VALUE SPACES.
           05 GLR-T-AMOUNT                 PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(43) VALUE SPACES.
       01  GLR-MSG-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 GLR-MSG-TEXT                 PIC X(131).
