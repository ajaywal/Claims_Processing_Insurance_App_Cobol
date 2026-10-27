      ******************************************************************
      * CLMFNLRT - FNOL working tables                                 *
      * FT-LOSS-TYPE : loss types accepted by bulk intake (LOSSTYPE    *
      *                typelist) and the line they belong to           *
      * FT-RESERVE   : initial reserve by exposure type (EXPTYPE) and  *
      *                severity 1..5, GBP whole pounds (BR-CLM-FNB-006)*
      ******************************************************************
       01  FT-LOSS-TYPE-VALUES.
           05 FILLER PIC X(5) VALUE 'AUTOM'.
           05 FILLER PIC X(5) VALUE 'PROPH'.
           05 FILLER PIC X(5) VALUE 'LIABC'.
       01  FT-LOSS-TYPE-TABLE REDEFINES FT-LOSS-TYPE-VALUES.
           05 FT-LT OCCURS 3 TIMES INDEXED BY FT-LT-IX.
              10 FT-LT-CODE                PIC X(4).
              10 FT-LT-LINE                PIC X.
                 88 FT-LT-MOTOR             VALUE 'M'.
                 88 FT-LT-PROPERTY          VALUE 'H'.
                 88 FT-LT-LIABILITY         VALUE 'C'.

       01  FT-RESERVE-VALUES.
           05 FILLER PIC X(8)  VALUE 'VEHDAMAG'.
           05 FILLER PIC X(35) VALUE
              '00015000003000000600000120000025000'.
           05 FILLER PIC X(8)  VALUE 'PROPDAMA'.
           05 FILLER PIC X(35) VALUE
              '00025000005000001200000300000075000'.
           05 FILLER PIC X(8)  VALUE 'CONTENTS'.
           05 FILLER PIC X(35) VALUE
              '00015000003000000750000150000030000'.
           05 FILLER PIC X(8)  VALUE 'BODINJUR'.
           05 FILLER PIC X(35) VALUE
              '00050000010000002500000600000150000'.
       01  FT-RESERVE-TABLE REDEFINES FT-RESERVE-VALUES.
           05 FT-RV OCCURS 4 TIMES INDEXED BY FT-RV-IX.
              10 FT-RV-EXP-TYPE            PIC X(8).
              10 FT-RV-AMOUNT              PIC 9(7) OCCURS 5 TIMES.
