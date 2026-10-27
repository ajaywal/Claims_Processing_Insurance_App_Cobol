      ******************************************************************
      * BILDOCP1 - Print-vendor document extract record (FB, LRECL 500)*
      * Written by BILDOCGN. One H header, one D per document, one T   *
      * trailer. Amounts are 13 digits, implied 2 decimals, trailing   *
      * sign (+/-), as in docs/EVENTS.md payloads. The FD record is    *
      * PIC X(500); these layouts are WRITE ... FROM sources.          *
      ******************************************************************
       01  PV-HEADER-REC.
           05 PV-H-REC-TYPE            PIC X     VALUE 'H'.
           05 PV-H-FILE-ID             PIC X(8)  VALUE 'BILDOCPV'.
           05 PV-H-SENDER              PIC X(20)
                                       VALUE 'BILLINGCOREAJ2026'.
           05 PV-H-BUS-DATE            PIC X(10).
           05 PV-H-CREATED-TS          PIC X(26).
           05 PV-H-SEGMENT             PIC 9(3).
           05 FILLER                   PIC X(432) VALUE SPACES.
       01  PV-DETAIL-REC.
           05 PV-D-REC-TYPE            PIC X     VALUE 'D'.
           05 PV-D-DOC-ID              PIC 9(9).
           05 PV-D-DOC-TYPE            PIC X(8).
           05 PV-D-DELIVERY            PIC X(4).
           05 PV-D-REQUEST-DATE        PIC X(10).
           05 PV-D-ACCOUNT-NUMBER      PIC X(10).
           05 PV-D-RECIPIENT           PIC X(60).
           05 PV-D-ADDR-LINE1          PIC X(60).
           05 PV-D-ADDR-LINE2          PIC X(60).
           05 PV-D-CITY                PIC X(40).
           05 PV-D-POSTCODE            PIC X(8).
           05 PV-D-COUNTRY             PIC X(2).
           05 PV-D-EMAIL               PIC X(100).
           05 PV-D-REF-TYPE            PIC X(4).
           05 PV-D-REF-NUMBER          PIC X(12).
           05 PV-D-POLICY-NO           PIC 9(10).
           05 PV-D-AMOUNT              PIC 9(11)V99.
           05 PV-D-AMOUNT-SIGN         PIC X.
           05 PV-D-DUE-DATE            PIC X(10).
           05 FILLER                   PIC X(78) VALUE SPACES.
       01  PV-TRAILER-REC.
           05 PV-T-REC-TYPE            PIC X     VALUE 'T'.
           05 PV-T-RECORD-COUNT        PIC 9(9).
           05 PV-T-LAST-DOC-ID         PIC 9(9).
           05 PV-T-AMOUNT              PIC 9(13)V99.
           05 PV-T-AMOUNT-SIGN         PIC X.
           05 FILLER                   PIC X(465) VALUE SPACES.
