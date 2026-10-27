      * CLMCASG - body for CLMASSGN (assignment engine)
      * ASGCLM  pick group by CLM_ASSIGN_RULE (loss type, segment,
      *         severity, region) then user by strategy RRBN (round
      *         robin on LAST_ASSIGN_TS) or LOAD (fewest open claims)
      * ASGEXP  assign exposure (inherits claim owner unless BI/LIT)
      * ASGXFR  manual transfer to TARGET-USER
              05 CA-AS-CLAIM-ID            PIC S9(9) COMP.
              05 CA-AS-EXPOSURE-ID         PIC S9(9) COMP.
              05 CA-AS-LOSS-TYPE           PIC X(4).
              05 CA-AS-SEGMENT             PIC X(8).
              05 CA-AS-SEVERITY            PIC S9(4) COMP.
              05 CA-AS-REGION              PIC X(4).
              05 CA-AS-TARGET-USER         PIC X(8).
              05 CA-AS-ASSIGNED-USER       PIC X(8).
              05 CA-AS-ASSIGNED-GROUP      PIC X(8).
              05 CA-AS-RULE-SEQ            PIC S9(4) COMP.
              05 FILLER                    PIC X(3788).
