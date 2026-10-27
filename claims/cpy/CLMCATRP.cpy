      ******************************************************************
      * CLMCATRP - CLMCATAS run report lines (132 columns)             *
      ******************************************************************
       01  TR-HEAD-1.
           05 FILLER                       PIC X(9)  VALUE 'CLMCATAS'.
           05 FILLER                       PIC X(45) VALUE
              'CLAIMSCORE AJ2026 - CATASTROPHE TAGGING'.
           05 FILLER                       PIC X(15) VALUE
              'BUSINESS DATE '.
           05 TR-H1-BUS-DATE               PIC X(10).
           05 FILLER                       PIC X(43) VALUE SPACES.
           05 FILLER                       PIC X(5)  VALUE 'PAGE '.
           05 TR-H1-PAGE                   PIC ZZZ9.
           05 FILLER                       PIC X     VALUE SPACE.
       01  TR-CAT-LINE.
           05 FILLER                       PIC X(13) VALUE
              ' CATASTROPHE '.
           05 TR-C-CAT-CODE                PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-C-CAT-NAME                PIC X(40).
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-C-CAT-TYPE                PIC X(8).
           05 FILLER                       PIC X(6)  VALUE ' AREA '.
           05 TR-C-AREA                    PIC X(4).
           05 FILLER                       PIC X(8)  VALUE ' WINDOW '.
           05 TR-C-START                   PIC X(10).
           05 FILLER                       PIC X(4)  VALUE ' TO '.
           05 TR-C-END                     PIC X(10).
           05 FILLER                       PIC X(19) VALUE SPACES.
       01  TR-CLAIM-LINE.
           05 FILLER                       PIC X(5)  VALUE SPACES.
           05 TR-L-CLAIM-NUMBER            PIC X(12).
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-L-LOSS-DATE               PIC X(10).
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-L-POSTCODE                PIC X(8).
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-L-INCURRED                PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-L-OLD-SEGMENT             PIC X(8).
           05 FILLER                       PIC X(4)  VALUE ' -> '.
           05 TR-L-NEW-SEGMENT             PIC X(8).
           05 FILLER                       PIC X(5)  VALUE ' SEV '.
           05 TR-L-OLD-SEVERITY            PIC 9.
           05 FILLER                       PIC X(4)  VALUE ' -> '.
           05 TR-L-NEW-SEVERITY            PIC 9.
           05 FILLER                       PIC X     VALUE SPACE.
           05 TR-L-NOTE                    PIC X(46).
       01  TR-SUM-LINE.
           05 FILLER                       PIC X(5)  VALUE SPACES.
           05 TR-S-LABEL                   PIC X(30).
           05 TR-S-COUNT                   PIC ZZZ,ZZ9.
           05 FILLER                       PIC X(11) VALUE
              ' INCURRED '.
           05 TR-S-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(58) VALUE SPACES.
       01  TR-TOTAL-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 TR-T-LABEL                   PIC X(45).
           05 TR-T-COUNT                   PIC ZZZ,ZZZ,ZZ9.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 TR-T-AMOUNT                  PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                       PIC X(49) VALUE SPACES.
       01  TR-MESSAGE-LINE.
           05 FILLER                       PIC X(3)  VALUE SPACES.
           05 TR-M-TEXT                    PIC X(129).
