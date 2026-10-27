      ******************************************************************
      * BILDOCR1 - BILDOCGN document extract report lines (132 cols)   *
      ******************************************************************
       01  R4-EXC-HEAD.
           05 FILLER                   PIC X(60) VALUE
              '  DOC ID     ACCOUNT     DOC TYPE  DELIVERY  REASON'.
           05 FILLER                   PIC X(72) VALUE SPACES.
       01  R4-EXC-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-DOC-ID                PIC Z(8)9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-ACCOUNT-NUMBER        PIC X(10).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-DOC-TYPE              PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-DELIVERY              PIC X(4).
           05 FILLER                   PIC X(6)  VALUE SPACES.
           05 R4-REASON                PIC X(87).
       01  R4-TYPE-HEAD.
           05 FILLER                   PIC X(60) VALUE
              '  DOC TYPE        SENT    ERRORS'.
           05 FILLER                   PIC X(72) VALUE SPACES.
       01  R4-TYPE-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-T-DOC-TYPE            PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-T-SENT                PIC Z,ZZZ,ZZ9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 R4-T-ERRORS              PIC Z,ZZZ,ZZ9.
           05 FILLER                   PIC X(100) VALUE SPACES.
