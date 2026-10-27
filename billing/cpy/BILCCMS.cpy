      * BILCCMS - body for BILCOMSV (commission service)
      * CMSRSV  reserve commission on ITEM-ID for its period's producer
      * CMSERN  earn commission on ITEM-ID for PAID-AMOUNT just applied
      * CMSRVS  reverse earned commission for REVERSED-AMOUNT
      * CMSINQ  producer balance & counts for PRODUCER-ID
              05 CA-CM-ITEM-ID             PIC S9(9) COMP.
              05 CA-CM-PRODUCER-ID         PIC S9(9) COMP.
              05 CA-CM-PAID-AMOUNT         PIC S9(11)V99 COMP-3.
              05 CA-CM-REVERSED-AMOUNT     PIC S9(11)V99 COMP-3.
              05 CA-CM-RATE-BPS            PIC S9(4) COMP.
              05 CA-CM-COMM-AMOUNT         PIC S9(11)V99 COMP-3.
              05 CA-CM-EARNED-AMOUNT       PIC S9(11)V99 COMP-3.
              05 CA-CM-PAYABLE-BALANCE     PIC S9(11)V99 COMP-3.
              05 CA-CM-RESERVED-COUNT      PIC S9(9) COMP.
              05 CA-CM-EARNED-COUNT        PIC S9(9) COMP.
              05 FILLER                    PIC X(3787).
