      * CLMCDTE - body for CLMDATES (date arithmetic, ISO yyyy-mm-dd)
      * Requests: DTADDD add days, DTADDM add months (end-of-month
      * clamped), DTDIFF days date-2 minus date-1, DTDOW day of week
      * (1=Mon..7=Sun), DTBUSD roll forward to business day,
      * DTEOM end of month, DTVALD validate date-1.
              05 CA-DT-DATE-1              PIC X(10).
              05 CA-DT-DATE-2              PIC X(10).
              05 CA-DT-NUMBER              PIC S9(5) COMP-3.
              05 CA-DT-RESULT-DATE         PIC X(10).
              05 CA-DT-RESULT-NUM          PIC S9(7) COMP-3.
              05 CA-DT-VALID               PIC X.
                 88 CA-DT-IS-VALID          VALUE 'Y'.
              05 FILLER                    PIC X(3802).
