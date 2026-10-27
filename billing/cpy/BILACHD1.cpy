      ******************************************************************
      * BILACHD1 - direct debit draft file record (120 bytes, FB)      *
      * BACS-style collection file sent to the sponsoring bank: one    *
      * H header, a D detail per collection, one T trailer. The bank   *
      * de-tokenises MANDATE-TOKEN to the payer's account. Amounts are *
      * whole pence. TXN-CODE 17 = direct debit collection.            *
      ******************************************************************
           05 ACD-REC-TYPE             PIC X.
           05 ACD-REC-BODY             PIC X(119).
           05 ACD-HEADER-BODY REDEFINES ACD-REC-BODY.
              10 ACD-H-SERVICE-USER    PIC X(6).
              10 ACD-H-FILE-REF        PIC X(12).
              10 ACD-H-CREATE-DATE     PIC X(10).
              10 ACD-H-BUS-DATE        PIC X(10).
              10 FILLER                PIC X(81).
           05 ACD-DETAIL-BODY REDEFINES ACD-REC-BODY.
              10 ACD-D-REQUEST-ID      PIC 9(9).
              10 ACD-D-SORT-CODE       PIC X(6).
              10 ACD-D-MANDATE-TOKEN   PIC X(32).
              10 ACD-D-TXN-CODE        PIC X(2).
              10 ACD-D-AMOUNT-PENCE    PIC 9(11).
              10 ACD-D-DRAFT-DATE      PIC X(10).
              10 ACD-D-PAYER-REF       PIC X(18).
              10 ACD-D-PAYER-NAME      PIC X(18).
              10 FILLER                PIC X(13).
           05 ACD-TRAILER-BODY REDEFINES ACD-REC-BODY.
              10 ACD-T-DETAIL-COUNT    PIC 9(7).
              10 ACD-T-TOTAL-PENCE     PIC 9(15).
              10 FILLER                PIC X(97).
