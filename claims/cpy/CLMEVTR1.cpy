      ******************************************************************
      * CLMEVTR1 - CLMEVTDR 132-column run report lines                *
      ******************************************************************
       01  EDR-HEAD-1.
           05 FILLER                       PIC X(10) VALUE 'CLMEVTDR'.
           05 FILLER                       PIC X(42)
              VALUE 'CLAIMSCORE AJ2026 - INTEGRATION EVENT DRAI'.
           05 FILLER                       PIC X(8)  VALUE 'NER RUN '.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 EDR-H1-BUS-DATE              PIC X(10).
           05 FILLER                       PIC X(11) VALUE '  RUN AT  '.
           05 EDR-H1-RUN-TS                PIC X(19).
           05 FILLER                       PIC X(7)  VALUE '  PAGE '.
           05 EDR-H1-PAGE                  PIC ZZZ9.
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  EDR-HEAD-2.
           05 FILLER                       PIC X(132) VALUE ALL '-'.
       01  EDR-SECTION-LINE.
           05 FILLER                       PIC X(4)  VALUE '*** '.
           05 EDR-SECTION-TEXT             PIC X(60).
           05 FILLER                       PIC X(68) VALUE SPACES.
       01  EDR-EXC-HEAD.
           05 FILLER                       PIC X(44) VALUE
              ' EVENT-ID  TYPE     DIR KEY                 '.
           05 FILLER                       PIC X(88) VALUE
              ' OUTCOME  REASON   DETAIL'.
       01  EDR-EXC-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-EVENT-ID              PIC Z(8)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-TYPE                  PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-DIR                   PIC X(3).
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-KEY                   PIC X(20).
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-OUTCOME               PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-REASON                PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-EX-DETAIL                PIC X(69).
       01  EDR-TOT-HEAD.
           05 FILLER                       PIC X(48) VALUE
              ' EVENT TYPE  DIR        READ   DELIVERED/DONE '.
           05 FILLER                       PIC X(48) VALUE
              '   DUPLICATE      FAILED     ERRORED          '.
           05 FILLER                       PIC X(36) VALUE
              'CLAIMS  ACTIVITIES'.
       01  EDR-TOT-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-TL-TYPE                  PIC X(10).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 EDR-TL-DIR                   PIC X(3).
           05 EDR-TL-READ                  PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(5)  VALUE SPACES.
           05 EDR-TL-DONE                  PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-TL-DUP                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-TL-FAILED                PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-TL-ERRORED               PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(7)  VALUE SPACES.
           05 EDR-TL-CLAIMS                PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-TL-ACTS                  PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(23) VALUE SPACES.
       01  EDR-MSG-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 EDR-MSG-TEXT                 PIC X(100).
           05 EDR-MSG-NUM                  PIC Z(8)9.
           05 FILLER                       PIC X(22) VALUE SPACES.
