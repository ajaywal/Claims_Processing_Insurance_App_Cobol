      ******************************************************************
      * BILMNUC1 - BillingCore 3270 screen COMMAREA (256 bytes)         *
      * Carried between BMNU, BACI, BPAY and BIVL on every pseudo-      *
      * conversational RETURN and XCTL. SCR-OWNER-PGM is the program    *
      * that sent the current screen; any other value means the        *
      * receiving program is being entered for the first time.         *
      ******************************************************************
           05 SCR-EYECATCHER           PIC X(4).
              88 SCR-VALID              VALUE 'BILS'.
           05 SCR-OWNER-PGM            PIC X(8).
           05 SCR-FROM-PGM             PIC X(8).
           05 SCR-USER-ID              PIC X(8).
           05 SCR-ACCOUNT-NUMBER       PIC X(10).
           05 SCR-ACCOUNT-ID           PIC S9(9) COMP.
           05 SCR-PAGE-NO              PIC S9(4) COMP.
           05 SCR-ROW-COUNT            PIC S9(4) COMP.
           05 SCR-WORK-AREA            PIC X(210).
      * Payment entry (BILPAYP) two-step confirmation state
           05 SCR-PAY REDEFINES SCR-WORK-AREA.
              10 SCR-PY-STAGE          PIC X.
                 88 SCR-PY-ENTRY        VALUE ' '.
                 88 SCR-PY-CONFIRM      VALUE 'C'.
                 88 SCR-PY-POSTED       VALUE 'P'.
              10 SCR-PY-ACCOUNT-NUMBER PIC X(10).
              10 SCR-PY-INVOICE-NUMBER PIC X(12).
              10 SCR-PY-AMOUNT         PIC S9(11)V99 COMP-3.
              10 SCR-PY-METHOD         PIC X(4).
              10 SCR-PY-REF            PIC X(20).
              10 SCR-PY-RCVD-DATE      PIC X(10).
              10 SCR-PY-MONEY-ID       PIC S9(9) COMP.
              10 FILLER                PIC X(142).
