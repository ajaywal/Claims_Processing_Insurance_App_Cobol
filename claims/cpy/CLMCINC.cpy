      * CLMCINC - body for CLMINCDT (incident service)
      * INCNEW, INCINQ, INCUPD ; total-loss evaluation is CLMTOTLS
              05 CA-IN-INCIDENT-ID         PIC S9(9) COMP.
              05 CA-IN-CLAIM-ID            PIC S9(9) COMP.
              05 CA-IN-TYPE                PIC X(4).
              05 CA-IN-RISK-UNIT-SEQ       PIC S9(4) COMP.
              05 CA-IN-DESCRIPTION         PIC X(200).
              05 CA-IN-SEVERITY            PIC S9(4) COMP.
              05 CA-IN-VEHICLE-REG         PIC X(8).
              05 CA-IN-DRIVEABLE           PIC X.
              05 CA-IN-EST-DAMAGE          PIC S9(11)V99 COMP-3.
              05 CA-IN-TOTAL-LOSS          PIC X.
              05 CA-IN-INJURY-TYPE         PIC X(8).
              05 CA-IN-INJURED-ID          PIC S9(9) COMP.
              05 FILLER                    PIC X(3595).
