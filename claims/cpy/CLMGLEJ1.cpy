      ******************************************************************
      * CLMGLEJ1 - Claims GL journal interface record (150 bytes)      *
      * H header (one journal per business date), D journal line,      *
      * T trailer: line count, debit/credit totals, account hash.      *
      ******************************************************************
       01  GLJ-RECORD.
           05 GLJ-REC-TYPE                 PIC X.
              88 GLJ-HEADER                 VALUE 'H'.
              88 GLJ-DETAIL                 VALUE 'D'.
              88 GLJ-TRAILER                VALUE 'T'.
           05 GLJ-DATA                     PIC X(149).
       01  GLJ-HEADER-REC.
           05 FILLER                       PIC X.
           05 GLJ-H-SOURCE                 PIC X(8).
           05 GLJ-H-JOURNAL-ID             PIC X(16).
           05 GLJ-H-JOURNAL-DATE           PIC X(10).
           05 GLJ-H-PERIOD                 PIC X(6).
           05 GLJ-H-CURRENCY               PIC X(3).
           05 GLJ-H-RERUN-FLAG             PIC X.
           05 FILLER                       PIC X(105).
       01  GLJ-DETAIL-REC.
           05 FILLER                       PIC X.
           05 GLJ-D-JOURNAL-ID             PIC X(16).
           05 GLJ-D-LINE-NO                PIC 9(6).
           05 GLJ-D-ACCOUNT                PIC X(6).
           05 GLJ-D-COST-CENTRE            PIC X(4).
           05 GLJ-D-DR-CR                  PIC X(2).
           05 GLJ-D-AMOUNT                 PIC 9(13)V99.
           05 GLJ-D-CURRENCY               PIC X(3).
           05 GLJ-D-BUCKET                 PIC X(6).
           05 GLJ-D-DESCRIPTION            PIC X(40).
           05 GLJ-D-TXN-COUNT              PIC 9(7).
           05 FILLER                       PIC X(44).
       01  GLJ-TRAILER-REC.
           05 FILLER                       PIC X.
           05 GLJ-T-LINE-COUNT             PIC 9(7).
           05 GLJ-T-TOTAL-DEBITS           PIC 9(15)V99.
           05 GLJ-T-TOTAL-CREDITS          PIC 9(15)V99.
           05 GLJ-T-ACCOUNT-HASH           PIC 9(15).
           05 FILLER                       PIC X(93).
