      ******************************************************************
      * BILGLEG1 - General-ledger interface record (FB 120)             *
      * H header, D detail (one per GL account / product), T trailer.   *
      * Amounts are unsigned display with 2 implied decimals; the       *
      * trailer carries record count and DR / CR / account hash totals. *
      ******************************************************************
       01  GLI-RECORD.
           05 GLI-REC-TYPE             PIC X.
              88 GLI-HEADER             VALUE 'H'.
              88 GLI-DETAIL             VALUE 'D'.
              88 GLI-TRAILER            VALUE 'T'.
           05 GLI-BODY                 PIC X(119).
           05 GLI-HDR REDEFINES GLI-BODY.
              10 GLI-H-SOURCE          PIC X(8).
              10 GLI-H-COMPANY         PIC X(4).
              10 GLI-H-BUS-DATE        PIC X(10).
              10 GLI-H-CURRENCY        PIC X(3).
              10 GLI-H-CREATED-TS      PIC X(26).
              10 GLI-H-BATCH-REF       PIC X(12).
              10 FILLER                PIC X(56).
           05 GLI-DTL REDEFINES GLI-BODY.
              10 GLI-D-COMPANY         PIC X(4).
              10 GLI-D-GL-ACCOUNT      PIC 9(6).
              10 GLI-D-COST-CENTRE     PIC X(4).
              10 GLI-D-T-ACCOUNT       PIC X(8).
              10 GLI-D-PRODUCT         PIC X.
              10 GLI-D-DR-AMOUNT       PIC 9(13)V99.
              10 GLI-D-CR-AMOUNT       PIC 9(13)V99.
              10 GLI-D-NET-AMOUNT      PIC 9(13)V99.
              10 GLI-D-NET-SIGN        PIC X.
              10 GLI-D-LINE-COUNT      PIC 9(7).
              10 GLI-D-BUS-DATE        PIC X(10).
              10 FILLER                PIC X(33).
           05 GLI-TRL REDEFINES GLI-BODY.
              10 GLI-T-RECORD-COUNT    PIC 9(9).
              10 GLI-T-DR-TOTAL        PIC 9(15)V99.
              10 GLI-T-CR-TOTAL        PIC 9(15)V99.
              10 GLI-T-ACCOUNT-HASH    PIC 9(15).
              10 GLI-T-LINE-COUNT      PIC 9(9).
              10 FILLER                PIC X(52).
