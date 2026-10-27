      ******************************************************************
      * CLMISOP1 - CLMISOSN 132-column run report lines                *
      ******************************************************************
       01  ISP-HEAD-1.
           05 FILLER                       PIC X(10) VALUE 'CLMISOSN'.
           05 FILLER                       PIC X(40)
              VALUE 'CLAIMSCORE AJ2026 - ISO CLAIMSEARCH RUN '.
           05 ISP-H1-MODE                  PIC X(10).
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 ISP-H1-BUS-DATE              PIC X(10).
           05 FILLER                       PIC X(11) VALUE '  RUN AT  '.
           05 ISP-H1-RUN-TS                PIC X(19).
           05 FILLER                       PIC X(7)  VALUE '  PAGE '.
           05 ISP-H1-PAGE                  PIC ZZZ9.
           05 FILLER                       PIC X(6)  VALUE SPACES.
       01  ISP-HEAD-2.
           05 FILLER                       PIC X(132) VALUE ALL '-'.
       01  ISP-DET-HEAD.
           05 FILLER                       PIC X(50) VALUE
              ' ISO-REQ-ID CLAIM NUMBER  TYPE OUTCOME  REASON   '.
           05 FILLER                       PIC X(82) VALUE
              'MATCHES DETAIL'.
       01  ISP-DET-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-D-REQ-ID                 PIC Z(9)9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-D-CLAIM-NUMBER           PIC X(12).
           05 FILLER                       PIC X(2)  VALUE SPACES.
           05 ISP-D-TYPE                   PIC X(4).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-D-OUTCOME                PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-D-REASON                 PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-D-MATCHES                PIC ZZZ9.
           05 FILLER                       PIC X(4)  VALUE SPACES.
           05 ISP-D-DETAIL                 PIC X(75).
       01  ISP-TOT-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-T-LABEL                  PIC X(60).
           05 ISP-T-COUNT                  PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(56) VALUE SPACES.
       01  ISP-MSG-LINE.
           05 FILLER                       PIC X     VALUE SPACE.
           05 ISP-MSG-TEXT                 PIC X(131).
