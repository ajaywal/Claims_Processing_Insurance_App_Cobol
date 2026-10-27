      ******************************************************************
      * BILAGEC1 - Receivables aging CSV extract record (VB-style text  *
      * in a FB 200 dataset). Comma separated, header row first:        *
      * BUS_DATE,PRODUCT,PRODUCER_CODE,ACCOUNT_NUMBER,COLL_STATUS,      *
      * CURRENT,D1_30,D31_60,D61_90,D90_PLUS,TOTAL                      *
      ******************************************************************
       01  CSV-RECORD                  PIC X(200).
