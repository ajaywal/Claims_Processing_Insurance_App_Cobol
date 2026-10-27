      ******************************************************************
      * BILINVR1 - BILINVB0 invoice run report lines (132 columns)     *
      ******************************************************************
       01  R1-COL-HEAD.
           05 FILLER                   PIC X(46) VALUE
              '  ACCOUNT     INVOICE NO     STREAM ID  ITEMS  '.
           05 FILLER                   PIC X(52) VALUE
              '  INVOICE AMT      PRIOR BAL     AMOUNT DUE  DUE DATE'.
           05 FILLER                   PIC X(34) VALUE
              '    REMARK'.
       01  R1-DETAIL.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-ACCOUNT-NUMBER        PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-INVOICE-NUMBER        PIC X(12).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-STREAM-ID             PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-ITEMS                 PIC ZZZ9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-INVOICE-AMT           PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-PRIOR-BAL             PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-AMOUNT-DUE            PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-DUE-DATE              PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-REMARK                PIC X(24).
       01  R1-PROD-HEAD.
           05 FILLER                   PIC X(60) VALUE
              '  PRODUCT                     INVOICES      ITEMS  '.
           05 FILLER                   PIC X(72) VALUE
              '       AMOUNT BILLED'.
       01  R1-PROD-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-PROD-CODE             PIC X.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-PROD-NAME             PIC X(20).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-PROD-INVOICES         PIC Z,ZZZ,ZZ9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R1-PROD-ITEMS            PIC Z,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 R1-PROD-AMOUNT           PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(62) VALUE SPACES.
