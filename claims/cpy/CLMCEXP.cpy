      * CLMCEXP - body for CLMEXPOS (exposure service)
      * EXPNEW  create exposure on claim (coverage must exist, claimant
      *         contact linked, reserve line(s) created at zero)
      * EXPINQ, EXPUPD, EXPCLS (no open reserve/pending txns), EXPROP
              05 CA-EX-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-EX-CLAIM-ID            PIC S9(9) COMP.
              05 CA-EX-SEQ                 PIC S9(4) COMP.
              05 CA-EX-TYPE                PIC X(8).
              05 CA-EX-COVERAGE-TYPE       PIC X(8).
              05 CA-EX-COV-SEQ             PIC S9(4) COMP.
              05 CA-EX-CLAIMANT-ID         PIC S9(9) COMP.
              05 CA-EX-INCIDENT-ID         PIC S9(9) COMP.
              05 CA-EX-STATE               PIC X(4).
              05 CA-EX-VALIDATION-LEVEL    PIC X(4).
              05 CA-EX-SEGMENT             PIC X(8).
              05 CA-EX-ASSIGNED-USER       PIC X(8).
              05 CA-EX-OPEN-RESERVE        PIC S9(11)V99 COMP-3.
              05 CA-EX-TOTAL-PAID          PIC S9(11)V99 COMP-3.
              05 CA-EX-TOTAL-RECOVERED     PIC S9(11)V99 COMP-3.
              05 CA-EX-DEDUCTIBLE-APPLIED  PIC S9(11)V99 COMP-3.
              05 CA-EX-COV-LIMIT           PIC S9(11)V99 COMP-3.
              05 CA-EX-COV-DEDUCTIBLE      PIC S9(11)V99 COMP-3.
              05 CA-EX-CLOSE-REASON        PIC X(8).
              05 CA-EX-VERSION-NO          PIC S9(9) COMP.
              05 FILLER                    PIC X(3726).
