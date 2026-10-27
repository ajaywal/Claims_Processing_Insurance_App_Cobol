      ******************************************************************
      * BILINVH1 - 132-column run report page heading and print line.  *
      * Used by BILINVB0 BILINVDU BILDLQEV BILDOCGN. The print file is *
      * RECFM=FBA LRECL=133 (carriage control from WRITE ADVANCING).   *
      ******************************************************************
       01  RH-HEAD-1.
           05 FILLER                   PIC X(18)
                                       VALUE 'BILLINGCOREAJ2026 '.
           05 RH-PROGRAM-ID            PIC X(8).
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 RH-TITLE                 PIC X(50).
           05 FILLER                   PIC X(10) VALUE 'BUS DATE '.
           05 RH-BUS-DATE              PIC X(10).
           05 FILLER                   PIC X(6)  VALUE ' RUN '.
           05 RH-RUN-DATE              PIC X(10).
           05 FILLER                   PIC X(6)  VALUE ' PAGE'.
           05 RH-PAGE                  PIC ZZZ9.
           05 FILLER                   PIC X(6)  VALUE SPACES.
       01  RH-RULE-LINE                PIC X(132) VALUE ALL '-'.
       01  RH-TEXT-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 RH-TEXT                  PIC X(130).
       01  RH-TOTAL-LINE.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 RH-TOT-LABEL             PIC X(40).
           05 RH-TOT-COUNT             PIC Z,ZZZ,ZZ9.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 RH-TOT-AMOUNT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                   PIC X(58) VALUE SPACES.
