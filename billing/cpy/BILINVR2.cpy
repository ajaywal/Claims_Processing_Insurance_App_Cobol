      ******************************************************************
      * BILINVR2 - BILINVDU due transition report lines (132 columns)  *
      ******************************************************************
       01  R2-DUE-HEAD.
           05 FILLER                   PIC X(50) VALUE
              '  ACCOUNT     ITEMS DUE      AMOUNT FALLING DUE  '.
           05 FILLER                   PIC X(82) VALUE
              'INV DUE  INV PAID  ITEMS PAID   REMARK'.
       01  R2-DUE-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-ACCOUNT-NUMBER        PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-ITEMS-DUE             PIC ZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 R2-AMOUNT-DUE            PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 R2-INV-DUE               PIC ZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 R2-INV-PAID              PIC ZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 R2-ITEMS-PAID            PIC ZZ,ZZ9.
           05 FILLER                   PIC X(3)  VALUE SPACES.
           05 R2-REMARK                PIC X(56).
       01  R2-DLQ-HEAD.
           05 FILLER                   PIC X(50) VALUE
              '  ACCOUNT     PLAN      PERIOD ID  GRACE CUT-OFF   '.
           05 FILLER                   PIC X(82) VALUE
              '      PAST DUE  RESULT'.
       01  R2-DLQ-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-D-ACCOUNT-NUMBER      PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-D-PLAN-CODE           PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-D-PP-ID               PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-D-CUTOFF              PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-D-PASTDUE             PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R2-D-RESULT              PIC X(68).
