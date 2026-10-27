      ******************************************************************
      * CLMINQ01 - CLMINQP (CINQ) conversation state: the claim on     *
      * display and its exposure summary, kept for paging and for the  *
      * PF4 cursor-row lookup without a second LINK to CLMCLAIM.       *
      * 2026-10-27 AJ  Initial version                                 *
      ******************************************************************
              05 IQ-LOADED                 PIC X.
                 88 IQ-CLAIM-LOADED         VALUE 'Y'.
              05 IQ-CLAIM-ID               PIC S9(9) COMP.
              05 IQ-CLAIM-NUMBER           PIC X(12).
              05 IQ-CLAIM-STATE            PIC X(4).
              05 IQ-FIRST-ROW              PIC S9(4) COMP.
              05 IQ-EXP-COUNT              PIC S9(4) COMP.
              05 IQ-EXP OCCURS 10 TIMES.
                 07 IQ-X-EXPOSURE-ID       PIC S9(9) COMP.
                 07 IQ-X-SEQ               PIC S9(4) COMP.
                 07 IQ-X-TYPE              PIC X(8).
                 07 IQ-X-COVERAGE          PIC X(8).
                 07 IQ-X-STATE             PIC X(4).
                 07 IQ-X-OPEN-RESERVE      PIC S9(11)V99 COMP-3.
                 07 IQ-X-PAID              PIC S9(11)V99 COMP-3.
