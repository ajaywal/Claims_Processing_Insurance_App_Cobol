      ******************************************************************
      * CLMISOS1 - ISO ClaimSearch inbound response file (80 bytes)    *
      * H header, D one response per request sent, T trailer count.   *
      ******************************************************************
       01  ISR-RECORD.
           05 ISR-REC-TYPE                 PIC X.
              88 ISR-HEADER                 VALUE 'H'.
              88 ISR-DETAIL                 VALUE 'D'.
              88 ISR-TRAILER                VALUE 'T'.
           05 ISR-DATA                     PIC X(79).
       01  ISR-HEADER-REC.
           05 FILLER                       PIC X.
           05 ISR-H-SENDER-ID              PIC X(8).
           05 ISR-H-FILE-DATE              PIC X(10).
           05 FILLER                       PIC X(61).
       01  ISR-DETAIL-REC.
           05 FILLER                       PIC X.
           05 ISR-D-ISO-REQ-ID             PIC X(9).
           05 ISR-D-ISO-REQ-NUM REDEFINES ISR-D-ISO-REQ-ID
                                           PIC 9(9).
           05 ISR-D-CLAIM-NUMBER           PIC X(12).
           05 ISR-D-ISO-REF                PIC X(16).
           05 ISR-D-RESPONSE-CODE          PIC X(2).
              88 ISR-D-ACCEPTED             VALUE 'OK'.
              88 ISR-D-REJECTED             VALUE 'ER'.
           05 ISR-D-MATCH-COUNT            PIC X(3).
           05 ISR-D-MATCH-NUM REDEFINES ISR-D-MATCH-COUNT
                                           PIC 9(3).
           05 ISR-D-ERROR-CODE             PIC X(8).
           05 FILLER                       PIC X(29).
       01  ISR-TRAILER-REC.
           05 FILLER                       PIC X.
           05 ISR-T-DETAIL-COUNT           PIC X(7).
           05 ISR-T-DETAIL-NUM REDEFINES ISR-T-DETAIL-COUNT
                                           PIC 9(7).
           05 FILLER                       PIC X(72).
