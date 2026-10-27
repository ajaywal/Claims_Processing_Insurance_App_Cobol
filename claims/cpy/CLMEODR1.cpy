      ******************************************************************
      * CLMEODR1 - CLMEODDR 132-column end-of-day report lines         *
      ******************************************************************
       01  EOR-HEAD-1.
           05 FILLER                       PIC X(10) VALUE 'CLMEODDR'.
           05 FILLER                       PIC X(50)
              VALUE 'CLAIMSCORE AJ2026 - END OF DAY CONTROL           '.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 EOR-H1-BUS-DATE              PIC X(10).
           05 FILLER                       PIC X(11) VALUE '  RUN AT  '.
           05 EOR-H1-RUN-TS                PIC X(19).
           05 FILLER                       PIC X(7)  VALUE '  PAGE '.
           05 EOR-H1-PAGE                  PIC ZZZ9.
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  EOR-HEAD-2.
           05 FILLER                       PIC X(132) VALUE ALL '-'.
       01  EOR-CHECK-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EOR-C-NAME                   PIC X(60).
           05 EOR-C-VALUE                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 EOR-C-RESULT                 PIC X(8).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 EOR-C-NOTE                   PIC X(40).
       01  EOR-MSG-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EOR-MSG-TEXT                 PIC X(131).
