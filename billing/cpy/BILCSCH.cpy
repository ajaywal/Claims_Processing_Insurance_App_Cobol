      * BILCSCH - body for BILISCHD (invoice item scheduler)
      * SCHCHG  split CHARGE-ID into invoice items per plan/stream
      * SCHRSC  reschedule all PLANned items of PP-ID onto NEW-PLAN
              05 CA-SC-CHARGE-ID           PIC S9(9) COMP.
              05 CA-SC-PP-ID               PIC S9(9) COMP.
              05 CA-SC-NEW-PLAN            PIC X(8).
              05 CA-SC-ONE-TIME            PIC X.
                 88 CA-SC-IS-ONE-TIME       VALUE 'Y'.
              05 CA-SC-ITEMS-CREATED       PIC S9(4) COMP.
              05 CA-SC-FIRST-DUE-DATE      PIC X(10).
              05 CA-SC-AMOUNT-SCHEDULED    PIC S9(11)V99 COMP-3.
              05 FILLER                    PIC X(3804).
