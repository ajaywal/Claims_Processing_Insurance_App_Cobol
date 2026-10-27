      ******************************************************************
      * CLMDUPCF - Loss cause families for duplicate detection         *
      * Causes in one family describe the same kind of event as it is  *
      * often re-reported (e.g. KERB vs SNGLVEH). A cause not listed   *
      * is only similar to itself (BR-CLM-DUP-003).                    *
      ******************************************************************
       01  DF-CAUSE-FAMILY-VALUES.
           05 FILLER PIC X(14) VALUE 'COLLISION IMPT'.
           05 FILLER PIC X(14) VALUE 'SNGLVEH   IMPT'.
           05 FILLER PIC X(14) VALUE 'ANIMAL    IMPT'.
           05 FILLER PIC X(14) VALUE 'KERB      IMPT'.
           05 FILLER PIC X(14) VALUE 'PARKED    IMPT'.
           05 FILLER PIC X(14) VALUE 'IMPACT    IMPT'.
           05 FILLER PIC X(14) VALUE 'GLASS     GLAS'.
           05 FILLER PIC X(14) VALUE 'WINDSCREENGLAS'.
           05 FILLER PIC X(14) VALUE 'VEHTHEFT  THFT'.
           05 FILLER PIC X(14) VALUE 'THEFTFROM THFT'.
           05 FILLER PIC X(14) VALUE 'THEFT     THFT'.
           05 FILLER PIC X(14) VALUE 'ESCWATER  WATR'.
           05 FILLER PIC X(14) VALUE 'FLOOD     WATR'.
           05 FILLER PIC X(14) VALUE 'VEHFLOOD  WATR'.
           05 FILLER PIC X(14) VALUE 'STORM     WTHR'.
           05 FILLER PIC X(14) VALUE 'HAIL      WTHR'.
           05 FILLER PIC X(14) VALUE 'FIRE      FIRE'.
           05 FILLER PIC X(14) VALUE 'SMOKE     FIRE'.
           05 FILLER PIC X(14) VALUE 'LIGHTNING FIRE'.
           05 FILLER PIC X(14) VALUE 'VEHFIRE   FIRE'.
           05 FILLER PIC X(14) VALUE 'VANDALISM MALI'.
           05 FILLER PIC X(14) VALUE 'VEHVANDAL MALI'.
           05 FILLER PIC X(14) VALUE 'SLIPFALL  PINJ'.
           05 FILLER PIC X(14) VALUE 'PREMISES  PINJ'.
       01  DF-CAUSE-FAMILY-TABLE REDEFINES DF-CAUSE-FAMILY-VALUES.
           05 DF-CF OCCURS 24 TIMES INDEXED BY DF-CF-IX.
              10 DF-CF-CAUSE               PIC X(10).
              10 DF-CF-FAMILY              PIC X(4).
