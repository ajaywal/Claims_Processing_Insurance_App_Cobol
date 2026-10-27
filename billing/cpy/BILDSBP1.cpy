      ******************************************************************
      * BILDSBP1 - Outbound payments file (bank bureau), FB 250.       *
      * H header, D one per disbursement, T trailer with hash totals.  *
      * CHEQ details carry the cheque serial and postal address; BACS  *
      * details carry the instrument sort code / token, or for CLMS    *
      * and COMM payees the source mandate reference.                  *
      * Amounts are unsigned, 2 implied decimals.                      *
      ******************************************************************
           05 PAY-REC-TYPE             PIC X.
              88 PAY-HEADER             VALUE 'H'.
              88 PAY-DETAIL             VALUE 'D'.
              88 PAY-TRAILER            VALUE 'T'.
           05 PAY-HEADER-DATA.
              10 PAY-H-FILE-ID         PIC X(12).
              10 PAY-H-CREATE-DATE     PIC X(10).
              10 PAY-H-ORIGINATOR      PIC X(20).
              10 PAY-H-CURRENCY        PIC X(3).
              10 FILLER                PIC X(204).
           05 PAY-DETAIL-DATA REDEFINES PAY-HEADER-DATA.
              10 PAY-D-DISB-ID         PIC 9(10).
              10 PAY-D-METHOD          PIC X(4).
              10 PAY-D-AMOUNT          PIC 9(11)V99.
              10 PAY-D-PAYEE-NAME      PIC X(60).
              10 PAY-D-PAYEE-ADDR1     PIC X(60).
              10 PAY-D-PAYEE-POSTCODE  PIC X(8).
              10 PAY-D-CHEQUE-NO       PIC 9(10).
              10 PAY-D-SORT-CODE       PIC X(6).
              10 PAY-D-ACCOUNT-TOKEN   PIC X(32).
              10 PAY-D-LAST-FOUR       PIC X(4).
              10 PAY-D-REFERENCE       PIC X(18).
              10 PAY-D-VALUE-DATE      PIC X(10).
              10 PAY-D-SOURCE-SYSTEM   PIC X(4).
              10 FILLER                PIC X(10).
           05 PAY-TRAILER-DATA REDEFINES PAY-HEADER-DATA.
              10 PAY-T-RECORD-COUNT    PIC 9(9).
              10 PAY-T-TOTAL-AMOUNT    PIC 9(13)V99.
              10 PAY-T-CHEQUE-COUNT    PIC 9(9).
              10 PAY-T-CHEQUE-AMOUNT   PIC 9(13)V99.
              10 PAY-T-BACS-COUNT      PIC 9(9).
              10 PAY-T-BACS-AMOUNT     PIC 9(13)V99.
              10 FILLER                PIC X(177).
