      ******************************************************************
      * BILLKBR1 - bank lockbox file record (120 bytes, RECFM=FB)      *
      * One file per lockbox deposit: H header first, D details, T     *
      * trailer last. Amounts are unsigned pence-implied display.      *
      * Batch reference = LOCKBOX-NO + FILE-SEQ (12 characters).       *
      ******************************************************************
           05 LKB-REC-TYPE             PIC X.
              88 LKB-IS-HEADER          VALUE 'H'.
              88 LKB-IS-DETAIL          VALUE 'D'.
              88 LKB-IS-TRAILER         VALUE 'T'.
           05 LKB-REC-BODY             PIC X(119).
           05 LKB-HEADER-BODY REDEFINES LKB-REC-BODY.
              10 LKB-H-BANK-ID         PIC X(8).
              10 LKB-H-BATCH-REF.
                 15 LKB-H-LOCKBOX-NO   PIC X(6).
                 15 LKB-H-FILE-SEQ     PIC 9(6).
              10 LKB-H-DEPOSIT-DATE    PIC X(10).
              10 LKB-H-CREATE-DATE     PIC X(10).
              10 LKB-H-CREATE-TIME     PIC X(8).
              10 FILLER                PIC X(71).
           05 LKB-DETAIL-BODY REDEFINES LKB-REC-BODY.
              10 LKB-D-SEQ             PIC 9(6).
              10 LKB-D-ACCOUNT-NO      PIC X(10).
              10 LKB-D-INVOICE-NO      PIC X(12).
              10 LKB-D-AMOUNT          PIC 9(11)V99.
              10 LKB-D-AMOUNT-X REDEFINES LKB-D-AMOUNT
                                       PIC X(13).
              10 LKB-D-CHEQUE-NO       PIC 9(10).
              10 LKB-D-CHEQUE-NO-X REDEFINES LKB-D-CHEQUE-NO
                                       PIC X(10).
              10 LKB-D-CHEQUE-DATE     PIC X(10).
              10 LKB-D-REMITTER        PIC X(35).
              10 LKB-D-SORT-CODE       PIC X(6).
              10 LKB-D-BANK-ACCOUNT    PIC X(8).
              10 FILLER                PIC X(9).
           05 LKB-TRAILER-BODY REDEFINES LKB-REC-BODY.
              10 LKB-T-DETAIL-COUNT    PIC 9(6).
              10 LKB-T-AMOUNT-TOTAL    PIC 9(13)V99.
              10 LKB-T-CHEQUE-HASH     PIC 9(15).
              10 FILLER                PIC X(83).
