      ******************************************************************
      * BILAGSS1 - Agency-bill statement file, FB 200. Per producer:   *
      * H header, D one per billed item, T totals. Signed amounts are  *
      * 13 digits, 2 implied decimals, trailing separate sign.         *
      ******************************************************************
           05 AST-REC-TYPE             PIC X.
              88 AST-HEADER             VALUE 'H'.
              88 AST-DETAIL             VALUE 'D'.
              88 AST-TRAILER            VALUE 'T'.
           05 AST-HEADER-DATA.
              10 AST-H-STMT-ID         PIC 9(10).
              10 AST-H-PRODUCER-CODE   PIC 9(10).
              10 AST-H-PRODUCER-NAME   PIC X(60).
              10 AST-H-STMT-DATE       PIC X(10).
              10 AST-H-PERIOD-FROM     PIC X(10).
              10 AST-H-DUE-DATE        PIC X(10).
              10 FILLER                PIC X(89).
           05 AST-DETAIL-DATA REDEFINES AST-HEADER-DATA.
              10 AST-D-STMT-ID         PIC 9(10).
              10 AST-D-ACCOUNT-NO      PIC X(10).
              10 AST-D-POLICY-NO       PIC 9(10).
              10 AST-D-ITEM-ID         PIC 9(10).
              10 AST-D-ITEM-TYPE       PIC X(4).
              10 AST-D-INSTALL-NO      PIC 9(3).
              10 AST-D-EVENT-DATE      PIC X(10).
              10 AST-D-DUE-DATE        PIC X(10).
              10 AST-D-GROSS           PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
              10 AST-D-COMMISSION      PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
              10 AST-D-NET             PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
              10 FILLER                PIC X(90).
           05 AST-TRAILER-DATA REDEFINES AST-HEADER-DATA.
              10 AST-T-STMT-ID         PIC 9(10).
              10 AST-T-ITEM-COUNT      PIC 9(7).
              10 AST-T-GROSS           PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
              10 AST-T-COMMISSION      PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
              10 AST-T-NET             PIC S9(11)V99
                                       SIGN TRAILING SEPARATE.
              10 FILLER                PIC X(140).
