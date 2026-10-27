      * CLMCAUT - body for CLMAUTHZ (authority limits)
      * AUTCHK  USER's profile limit for LIMIT-TYPE/COVERAGE-TYPE
      *         (RSVEXP reserve per exposure, PAYEXP payment per
      *         exposure, PAYCLM payments per claim, INCCLM incurred
      *         per claim); '*' coverage is the default row.
      *         When over, APPROVER = first supervisor up the chain
      *         whose limit covers AMOUNT.
              05 CA-AT-USER                PIC X(8).
              05 CA-AT-LIMIT-TYPE          PIC X(8).
              05 CA-AT-COVERAGE-TYPE       PIC X(8).
              05 CA-AT-AMOUNT              PIC S9(11)V99 COMP-3.
              05 CA-AT-ALLOWED             PIC X.
                 88 CA-AT-IS-ALLOWED        VALUE 'Y'.
              05 CA-AT-LIMIT               PIC S9(11)V99 COMP-3.
              05 CA-AT-APPROVER            PIC X(8).
              05 FILLER                    PIC X(3793).
