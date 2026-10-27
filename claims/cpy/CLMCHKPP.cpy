      ******************************************************************
      * CLMCHKPP - Positive-pay issue file to the clearing bank        *
      * (150 bytes). One H / n D / one T per batch; a restarted run    *
      * appends a further batch with the next batch number.            *
      ******************************************************************
       01  PP-RECORD.
           05 PP-REC-TYPE                  PIC X.
              88 PP-IS-HEADER               VALUE 'H'.
              88 PP-IS-DETAIL               VALUE 'D'.
              88 PP-IS-TRAILER              VALUE 'T'.
           05 PP-SORT-CODE                 PIC X(6).
           05 PP-ACCOUNT-NO                PIC X(8).
           05 PP-BODY                      PIC X(135).
           05 PP-HEADER REDEFINES PP-BODY.
              10 PP-H-FILE-DATE            PIC X(10).
              10 PP-H-BATCH-NO             PIC 9(4).
              10 PP-H-ORIGINATOR           PIC X(20).
              10 FILLER                    PIC X(101).
           05 PP-DETAIL REDEFINES PP-BODY.
              10 PP-D-CHECK-NUMBER         PIC X(10).
              10 PP-D-ISSUE-DATE           PIC X(10).
              10 PP-D-AMOUNT               PIC 9(11)V99.
              10 PP-D-ACTION               PIC X.
                 88 PP-D-ISSUE              VALUE 'I'.
              10 PP-D-PAYEE-NAME           PIC X(60).
              10 PP-D-CLAIM-NUMBER         PIC X(12).
              10 FILLER                    PIC X(29).
           05 PP-TRAILER REDEFINES PP-BODY.
              10 PP-T-DETAIL-COUNT         PIC 9(9).
              10 PP-T-AMOUNT-TOTAL         PIC 9(13)V99.
              10 FILLER                    PIC X(111).
