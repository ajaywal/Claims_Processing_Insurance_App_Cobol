      * CLMCACT - body for CLMACTV0 (activities)
      * ACTNEW  from PATTERN-CODE (dates from pattern target/escalation)
      * ACTGEN  create all patterns whose TRIGGER-EVENT = EVENT
      * ACTCMP complete, ACTSKP skip (not if mandatory), ACTLST list
      *         open activities for USER (or CLAIM-ID) by due date
              05 CA-AV-ACTIVITY-ID         PIC S9(9) COMP.
              05 CA-AV-CLAIM-ID            PIC S9(9) COMP.
              05 CA-AV-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-AV-PATTERN-CODE        PIC X(8).
              05 CA-AV-EVENT               PIC X(8).
              05 CA-AV-SUBJECT             PIC X(60).
              05 CA-AV-ASSIGNED-USER       PIC X(8).
              05 CA-AV-DUE-DATE            PIC X(10).
              05 CA-AV-APPROVAL-TXN-ID     PIC S9(9) COMP.
              05 CA-AV-CREATED-COUNT       PIC S9(4) COMP.
              05 CA-AV-ROW-COUNT           PIC S9(4) COMP.
              05 CA-AV-ROW OCCURS 20 TIMES.
                 07 CA-AV-R-ID             PIC S9(9) COMP.
                 07 CA-AV-R-CLAIM-NUMBER   PIC X(12).
                 07 CA-AV-R-SUBJECT        PIC X(60).
                 07 CA-AV-R-PRIORITY       PIC X.
                 07 CA-AV-R-DUE            PIC X(10).
                 07 CA-AV-R-STATUS         PIC X(4).
                 07 CA-AV-R-ESCALATED      PIC X.
              05 FILLER                    PIC X(1886).
