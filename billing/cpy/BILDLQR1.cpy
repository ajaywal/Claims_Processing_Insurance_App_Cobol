      ******************************************************************
      * BILDLQR1 - BILDLQEV delinquency event run report lines (132)   *
      ******************************************************************
       01  R3-COL-HEAD.
           05 FILLER                   PIC X(50) VALUE
              '  ACCOUNT     PROCESS ID  STEP  EVENT     TARGET    '.
           05 FILLER                   PIC X(82) VALUE
              '        PAST DUE  RESULT'.
       01  R3-DETAIL.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-ACCOUNT-NUMBER        PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-DP-ID                 PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-STEP                  PIC ZZZ9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-EVENT                 PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-TARGET                PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-PASTDUE               PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-RESULT                PIC X(62).
       01  R3-TYPE-HEAD.
           05 FILLER                   PIC X(60) VALUE
              '  EVENT TYPE      DONE   SKIPPED   WAITING  REJECTED'.
           05 FILLER                   PIC X(72) VALUE
              '            AMOUNT'.
       01  R3-TYPE-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-T-EVENT               PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-T-DONE                PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(3)  VALUE SPACES.
           05 R3-T-SKIPPED             PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(3)  VALUE SPACES.
           05 R3-T-WAITING             PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(3)  VALUE SPACES.
           05 R3-T-REJECTED            PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R3-T-AMOUNT              PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(62) VALUE SPACES.
