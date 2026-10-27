      ******************************************************************
      * CLMACT01 - CLMACTP (CACT) conversation state: the activity     *
      * rows returned by ACTLST (max 20, most urgent first) and the    *
      * page on display (12 rows per page). Rows are kept so paging    *
      * needs no second LINK; the list is refreshed after any update.  *
      * 2026-10-27 AJ  Initial version                                 *
      ******************************************************************
              05 AQ-PAGE                   PIC S9(4) COMP.
              05 AQ-ROW-COUNT              PIC S9(4) COMP.
              05 AQ-ROW OCCURS 20 TIMES.
                 07 AQ-R-ACTIVITY-ID       PIC S9(9) COMP.
                 07 AQ-R-CLAIM-NUMBER      PIC X(12).
                 07 AQ-R-SUBJECT           PIC X(60).
                 07 AQ-R-PRIORITY          PIC X.
                 07 AQ-R-DUE               PIC X(10).
                 07 AQ-R-STATUS            PIC X(4).
                 07 AQ-R-ESCALATED         PIC X.
