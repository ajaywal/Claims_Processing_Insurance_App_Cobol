      * CLMCCOD - body for CLMCODVL (code table validation)
              05 CA-CD-TABLE               PIC X(8).
              05 CA-CD-CODE                PIC X(10).
              05 CA-CD-DESC                PIC X(60).
              05 CA-CD-VALID               PIC X.
                 88 CA-CD-IS-VALID          VALUE 'Y'.
              05 FILLER                    PIC X(3761).
