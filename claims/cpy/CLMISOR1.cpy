      ******************************************************************
      * CLMISOR1 - ISO ClaimSearch outbound request file (250 bytes)   *
      * H header, R request (one per CLM_ISO_REQUEST row), P party     *
      * (claim contacts), T trailer with record count and hash total   *
      * of CLAIM_ID over the R records.                                *
      ******************************************************************
       01  ISQ-RECORD.
           05 ISQ-REC-TYPE                 PIC X.
              88 ISQ-HEADER                 VALUE 'H'.
              88 ISQ-REQUEST                VALUE 'R'.
              88 ISQ-PARTY                  VALUE 'P'.
              88 ISQ-TRAILER                VALUE 'T'.
           05 ISQ-DATA                     PIC X(249).
       01  ISQ-HEADER-REC.
           05 FILLER                       PIC X.
           05 ISQ-H-SENDER-ID              PIC X(8).
           05 ISQ-H-FILE-DATE              PIC X(10).
           05 ISQ-H-CREATE-TS              PIC X(26).
           05 FILLER                       PIC X(205).
       01  ISQ-REQUEST-REC.
           05 FILLER                       PIC X.
           05 ISQ-R-ISO-REQ-ID             PIC 9(9).
           05 ISQ-R-REQUEST-TYPE           PIC X(4).
           05 ISQ-R-CLAIM-NUMBER           PIC X(12).
           05 ISQ-R-POLICY-NUMBER          PIC 9(10).
           05 ISQ-R-POLICY-TYPE            PIC X.
           05 ISQ-R-INSURED-NAME           PIC X(60).
           05 ISQ-R-INSURED-DOB            PIC X(10).
           05 ISQ-R-INSURED-POSTCODE       PIC X(8).
           05 ISQ-R-VEHICLE-REG            PIC X(8).
           05 ISQ-R-LOSS-DATE              PIC X(10).
           05 ISQ-R-LOSS-TYPE              PIC X(4).
           05 ISQ-R-LOSS-CAUSE             PIC X(10).
           05 ISQ-R-LOSS-POSTCODE          PIC X(8).
           05 ISQ-R-PARTY-COUNT            PIC 9(3).
           05 FILLER                       PIC X(92).
       01  ISQ-PARTY-REC.
           05 FILLER                       PIC X.
           05 ISQ-P-ISO-REQ-ID             PIC 9(9).
           05 ISQ-P-ROLE                   PIC X(8).
           05 ISQ-P-CONTACT-TYPE           PIC X(4).
           05 ISQ-P-FIRST-NAME             PIC X(30).
           05 ISQ-P-LAST-NAME              PIC X(40).
           05 ISQ-P-COMPANY-NAME           PIC X(60).
           05 ISQ-P-DOB                    PIC X(10).
           05 ISQ-P-POSTCODE               PIC X(8).
           05 FILLER                       PIC X(80).
       01  ISQ-TRAILER-REC.
           05 FILLER                       PIC X.
           05 ISQ-T-REQUEST-COUNT          PIC 9(7).
           05 ISQ-T-PARTY-COUNT            PIC 9(7).
           05 ISQ-T-CLAIM-HASH             PIC 9(15).
           05 FILLER                       PIC X(220).
