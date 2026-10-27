      ******************************************************************
      * BILINVC1 - Billing cycle batch control card (SYSIN, 80 bytes)  *
      * Used by BILINVB0 BILINVDU BILDLQEV BILDOCGN.                   *
      *   1-10  business date override (blank = BIL_CONTROL SYSTEM)    *
      *  11-14  commit frequency (blank/zero = 200)                    *
      *  15     restart mode: blank = automatic from checkpoint,       *
      *         N = ignore checkpoint, F = force rerun of a completed  *
      *         business date                                          *
      ******************************************************************
       01  CC-RECORD.
           05 CC-BUS-DATE              PIC X(10).
           05 CC-COMMIT-FREQ           PIC X(4).
           05 CC-COMMIT-FREQ-N REDEFINES CC-COMMIT-FREQ
                                       PIC 9(4).
           05 CC-RESTART-MODE          PIC X.
              88 CC-RESTART-AUTO        VALUE ' '.
              88 CC-RESTART-NONE        VALUE 'N'.
              88 CC-RESTART-FORCE       VALUE 'F'.
           05 FILLER                   PIC X(65).
