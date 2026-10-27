      ******************************************************************
      * CLMRIRBD - Reinsurance cession bordereau record (200 bytes)    *
      * D = one cession movement (claim x treaty), T = file trailer.   *
      * Amounts 9(11)V99 with a separate sign byte.                    *
      ******************************************************************
       01  BD-RECORD.
           05 BD-REC-TYPE                  PIC X.
              88 BD-IS-DETAIL               VALUE 'D'.
              88 BD-IS-TRAILER              VALUE 'T'.
           05 BD-BUS-DATE                  PIC X(10).
           05 BD-BODY                      PIC X(189).
           05 BD-DETAIL REDEFINES BD-BODY.
              10 BD-TREATY-CODE            PIC X(8).
              10 BD-TREATY-TYPE            PIC X(4).
              10 BD-CLAIM-NUMBER           PIC X(12).
              10 BD-POLICY-TYPE            PIC X.
              10 BD-LOSS-DATE              PIC X(10).
              10 BD-CAT-CODE               PIC X(8).
              10 BD-GROSS-INCURRED         PIC 9(11)V99.
              10 BD-GROSS-SIGN             PIC X.
              10 BD-RETAINED               PIC 9(11)V99.
              10 BD-RETAINED-SIGN          PIC X.
              10 BD-CEDED                  PIC 9(11)V99.
              10 BD-PRIOR-CEDED            PIC 9(11)V99.
              10 BD-MOVEMENT               PIC 9(11)V99.
              10 BD-MOVEMENT-SIGN          PIC X.
              10 BD-LARGE-LOSS-NOTICE      PIC X.
              10 FILLER                    PIC X(77).
           05 BD-TRAILER REDEFINES BD-BODY.
              10 BD-T-RECORD-COUNT         PIC 9(9).
              10 BD-T-CEDED-TOTAL          PIC 9(13)V99.
              10 BD-T-MOVEMENT-TOTAL       PIC 9(13)V99.
              10 BD-T-MOVEMENT-SIGN        PIC X.
              10 FILLER                    PIC X(149).
