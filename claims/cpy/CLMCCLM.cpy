      * CLMCCLM - body for CLMCLAIM (claim service)
      * CLMNEW  FNOL: verify policy, create claim+snapshot+contacts,
      *         segment, assign, generate activities, open (state OPEN)
      * CLMINQ  by CLAIM-ID or CLAIM-NUMBER with exposure summary
      * CLMUPD  update loss details / fault / flags (VERSION-NO match)
      * CLMCLS  close: all exposures closed, no open reserve, no
      *         pending txns, no open mandatory activities
      * CLMROP  reopen closed claim with reason
              05 CA-CL-CLAIM-ID            PIC S9(9) COMP.
              05 CA-CL-CLAIM-NUMBER        PIC X(12).
              05 CA-CL-GENAPP-POL-NUM      PIC S9(9) COMP.
              05 CA-CL-GENAPP-CUST-NUM     PIC S9(9) COMP.
              05 CA-CL-POLICY-TYPE         PIC X.
              05 CA-CL-LOSS-TYPE           PIC X(4).
              05 CA-CL-LOSS-DATE           PIC X(10).
              05 CA-CL-LOSS-TIME           PIC X(8).
              05 CA-CL-REPORTED-DATE       PIC X(10).
              05 CA-CL-HOW-REPORTED        PIC X(4).
              05 CA-CL-LOSS-CAUSE          PIC X(10).
              05 CA-CL-LOSS-DESC           PIC X(250).
              05 CA-CL-LOSS-ADDR1          PIC X(60).
              05 CA-CL-LOSS-POSTCODE       PIC X(8).
              05 CA-CL-STATE               PIC X(4).
              05 CA-CL-CLOSE-REASON        PIC X(8).
              05 CA-CL-SEGMENT             PIC X(8).
              05 CA-CL-SEVERITY            PIC S9(4) COMP.
              05 CA-CL-FAULT-RATING        PIC X(4).
              05 CA-CL-COVERAGE-IN-QSTN    PIC X.
              05 CA-CL-SIU-SCORE           PIC S9(4) COMP.
              05 CA-CL-SIU-STATUS          PIC X(4).
              05 CA-CL-LITIGATION-FLAG     PIC X.
              05 CA-CL-CAT-CODE            PIC X(8).
              05 CA-CL-TOTAL-LOSS-FLAG     PIC X.
              05 CA-CL-DUP-SUSPECT-FLAG    PIC X.
              05 CA-CL-ASSIGNED-USER       PIC X(8).
              05 CA-CL-ASSIGNED-GROUP      PIC X(8).
              05 CA-CL-TOTAL-RESERVES      PIC S9(11)V99 COMP-3.
              05 CA-CL-TOTAL-PAID          PIC S9(11)V99 COMP-3.
              05 CA-CL-TOTAL-RECOVERED     PIC S9(11)V99 COMP-3.
              05 CA-CL-TOTAL-INCURRED      PIC S9(11)V99 COMP-3.
              05 CA-CL-VERSION-NO          PIC S9(9) COMP.
              05 CA-CL-REOPEN-REASON       PIC X(8).
              05 CA-CL-REPORTER.
                 07 CA-CL-RPT-FIRST-NAME   PIC X(30).
                 07 CA-CL-RPT-LAST-NAME    PIC X(40).
                 07 CA-CL-RPT-PHONE        PIC X(20).
                 07 CA-CL-RPT-RELATION     PIC X(8).
              05 CA-CL-INSURED-CONTACT-ID  PIC S9(9) COMP.
              05 CA-CL-EXP-COUNT           PIC S9(4) COMP.
              05 CA-CL-EXP OCCURS 10 TIMES.
                 07 CA-CL-X-EXPOSURE-ID    PIC S9(9) COMP.
                 07 CA-CL-X-SEQ            PIC S9(4) COMP.
                 07 CA-CL-X-TYPE           PIC X(8).
                 07 CA-CL-X-COVERAGE       PIC X(8).
                 07 CA-CL-X-STATE          PIC X(4).
                 07 CA-CL-X-OPEN-RESERVE   PIC S9(11)V99 COMP-3.
                 07 CA-CL-X-PAID           PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(2847).
