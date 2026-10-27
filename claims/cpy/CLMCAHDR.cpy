      ******************************************************************
      * CLMCAHDR - ClaimsCore standard COMMAREA header (160 bytes)     *
      * Every CLM service is LINKed with header + 3840-byte body.       *
      * Callers COPY this REPLACING LEADING ==CA-== BY ==xx-==.         *
      ******************************************************************
           03 CA-HEADER.
              05 CA-REQUEST-ID             PIC X(8).
              05 CA-RETURN-CODE            PIC 9(2).
                 88 CA-RC-OK                VALUE 00.
                 88 CA-RC-WARNING           VALUE 04.
                 88 CA-RC-NOT-FOUND         VALUE 08.
                 88 CA-RC-VALIDATION        VALUE 12.
                 88 CA-RC-CONFLICT          VALUE 16.
                 88 CA-RC-AUTHORITY         VALUE 20.
                 88 CA-RC-STATE             VALUE 24.
                 88 CA-RC-DB2-ERROR         VALUE 90.
                 88 CA-RC-CICS-ERROR        VALUE 91.
                 88 CA-RC-BAD-LENGTH        VALUE 98.
                 88 CA-RC-BAD-REQUEST       VALUE 99.
              05 CA-REASON-CODE            PIC X(8).
              05 CA-MESSAGE                PIC X(80).
              05 CA-USER-ID                PIC X(8).
              05 CA-BUS-DATE               PIC X(10).
              05 CA-TRACE-ID               PIC X(16).
              05 CA-CALLER-PGM             PIC X(8).
              05 FILLER                    PIC X(20).
