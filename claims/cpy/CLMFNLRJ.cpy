      ******************************************************************
      * CLMFNLRJ - FNOL reject file record (700 bytes)                 *
      * Original 600-byte intake image plus the reason it was refused. *
      ******************************************************************
       01  FR-RECORD.
           05 FR-INTAKE-ID                 PIC 9(9).
           05 FR-BUS-DATE                  PIC X(10).
           05 FR-ERROR-CODE                PIC X(8).
           05 FR-ERROR-TEXT                PIC X(73).
           05 FR-RAW-RECORD                PIC X(600).
