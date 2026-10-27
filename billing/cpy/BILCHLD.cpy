      * BILCHLD - body for BILHOLD0 (holds)
      * HLDADD, HLDREL release HOLD-ID, HLDCHK is HOLD-TYPE active on
      * ACCOUNT-ID at AS-OF-DATE (types INVC DLNQ DISB PYMT)
              05 CA-HL-HOLD-ID             PIC S9(9) COMP.
              05 CA-HL-ACCOUNT-ID          PIC S9(9) COMP.
              05 CA-HL-HOLD-TYPE           PIC X(4).
              05 CA-HL-START-DATE          PIC X(10).
              05 CA-HL-END-DATE            PIC X(10).
              05 CA-HL-REASON              PIC X(80).
              05 CA-HL-AS-OF-DATE          PIC X(10).
              05 CA-HL-ACTIVE              PIC X.
                 88 CA-HL-IS-ACTIVE         VALUE 'Y'.
              05 FILLER                    PIC X(3717).
