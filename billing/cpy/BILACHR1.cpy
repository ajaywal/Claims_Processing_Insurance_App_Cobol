      ******************************************************************
      * BILACHR1 - direct debit returns file record (80 bytes, FB)     *
      * Unpaid collections reported back by the sponsoring bank: one H *
      * header, a D detail per returned draft, one T trailer. Return   *
      * codes: R01 insufficient funds, R02 account closed, R03 no      *
      * account / unable to locate, R04 invalid account number.        *
      ******************************************************************
           05 ACR-REC-TYPE             PIC X.
              88 ACR-IS-HEADER          VALUE 'H'.
              88 ACR-IS-DETAIL          VALUE 'D'.
              88 ACR-IS-TRAILER         VALUE 'T'.
           05 ACR-REC-BODY             PIC X(79).
           05 ACR-HEADER-BODY REDEFINES ACR-REC-BODY.
              10 ACR-H-BANK-ID         PIC X(8).
              10 ACR-H-FILE-DATE       PIC X(10).
              10 ACR-H-FILE-REF        PIC X(12).
              10 FILLER                PIC X(49).
           05 ACR-DETAIL-BODY REDEFINES ACR-REC-BODY.
              10 ACR-D-REQUEST-ID      PIC 9(9).
              10 ACR-D-REQUEST-ID-X REDEFINES ACR-D-REQUEST-ID
                                       PIC X(9).
              10 ACR-D-RETURN-CODE     PIC X(3).
                 88 ACR-D-NSF           VALUE 'R01'.
                 88 ACR-D-CLOSED        VALUE 'R02'.
                 88 ACR-D-NO-ACCOUNT    VALUE 'R03'.
                 88 ACR-D-INVALID       VALUE 'R04'.
                 88 ACR-D-KNOWN-CODE    VALUE 'R01' 'R02' 'R03'
                                              'R04'.
              10 ACR-D-RETURN-DATE     PIC X(10).
              10 ACR-D-AMOUNT-PENCE    PIC 9(11).
              10 ACR-D-AMOUNT-X REDEFINES ACR-D-AMOUNT-PENCE
                                       PIC X(11).
              10 ACR-D-BANK-REF        PIC X(18).
              10 ACR-D-SORT-CODE       PIC X(6).
              10 FILLER                PIC X(22).
           05 ACR-TRAILER-BODY REDEFINES ACR-REC-BODY.
              10 ACR-T-DETAIL-COUNT    PIC 9(7).
              10 ACR-T-TOTAL-PENCE     PIC 9(15).
              10 FILLER                PIC X(57).
