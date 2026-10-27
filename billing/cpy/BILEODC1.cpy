      ******************************************************************
      * BILEODC1 - BILEODDR control card (SYSIN, 80 bytes)              *
      *   MODE=BEGIN  open the end-of-day window (status O -> B)        *
      *   MODE=END    check step results and roll the business date     *
      ******************************************************************
       01  CTC-RECORD.
           05 CTC-KEYWORD              PIC X(5).
              88 CTC-KEYWORD-OK         VALUE 'MODE='.
           05 CTC-MODE                 PIC X(5).
              88 CTC-MODE-BEGIN         VALUE 'BEGIN'.
              88 CTC-MODE-END           VALUE 'END  '.
           05 FILLER                   PIC X(70).
