      ******************************************************************
      * CLMFNLIN - Bulk FNOL intake file record (600 bytes, fixed)     *
      * Producer : web / partner portal gateway, one file per feed.    *
      * HD header, FN one first notice of loss, TR trailer.            *
      * Amounts: 9(11)V99 unsigned display, implied 2 decimals.        *
      ******************************************************************
       01  FI-RECORD.
           05 FI-REC-TYPE                  PIC X(2).
              88 FI-IS-HEADER               VALUE 'HD'.
              88 FI-IS-DETAIL               VALUE 'FN'.
              88 FI-IS-TRAILER              VALUE 'TR'.
           05 FI-DETAIL.
              10 FI-SOURCE                 PIC X(4).
              10 FI-EXT-REF                PIC X(20).
              10 FI-POLICY-NO              PIC X(10).
              10 FI-POLICY-NO-N REDEFINES FI-POLICY-NO
                                           PIC 9(10).
              10 FI-LOSS-DATE              PIC X(10).
              10 FI-LOSS-TIME              PIC X(8).
              10 FI-LOSS-TIME-R REDEFINES FI-LOSS-TIME.
                 15 FI-LOSS-HH             PIC X(2).
                 15 FI-LOSS-SEP1           PIC X.
                 15 FI-LOSS-MI             PIC X(2).
                 15 FI-LOSS-SEP2           PIC X.
                 15 FI-LOSS-SS             PIC X(2).
              10 FI-LOSS-TYPE              PIC X(4).
              10 FI-LOSS-CAUSE             PIC X(10).
              10 FI-LOSS-DESC              PIC X(250).
              10 FI-LOSS-ADDR1             PIC X(60).
              10 FI-LOSS-POSTCODE          PIC X(8).
              10 FI-RPT-FIRST-NAME         PIC X(30).
              10 FI-RPT-LAST-NAME          PIC X(40).
              10 FI-RPT-PHONE              PIC X(20).
              10 FI-RPT-RELATION           PIC X(8).
              10 FI-VEHICLE-REG            PIC X(8).
              10 FI-INJURY-FLAG            PIC X.
                 88 FI-INJURY-YES           VALUE 'Y'.
                 88 FI-INJURY-VALID         VALUES 'Y' 'N'.
              10 FI-EST-DAMAGE-X           PIC X(13).
              10 FI-EST-DAMAGE REDEFINES FI-EST-DAMAGE-X
                                           PIC 9(11)V99.
              10 FILLER                    PIC X(94).
           05 FI-HEADER REDEFINES FI-DETAIL.
              10 FI-HD-SOURCE              PIC X(4).
              10 FI-HD-FEED-ID             PIC X(20).
              10 FI-HD-CREATED-DATE        PIC X(10).
              10 FILLER                    PIC X(564).
           05 FI-TRAILER REDEFINES FI-DETAIL.
              10 FI-TR-RECORD-COUNT-X      PIC X(9).
              10 FI-TR-RECORD-COUNT REDEFINES FI-TR-RECORD-COUNT-X
                                           PIC 9(9).
              10 FI-TR-DAMAGE-TOTAL-X      PIC X(15).
              10 FI-TR-DAMAGE-TOTAL REDEFINES FI-TR-DAMAGE-TOTAL-X
                                           PIC 9(13)V99.
              10 FILLER                    PIC X(574).
