      ******************************************************************
      * BILEODS1 - EOD step-status record written by the BILEOD job     *
      * stream (IEBGENER, DISP=MOD) after each step: step name and a    *
      * return-code class 00 / 04 / 08 / 99 (abend). A restarted run    *
      * appends again, so the latest record for a step wins.            *
      ******************************************************************
       01  STS-RECORD.
           05 STS-STEP-NAME            PIC X(8).
           05 FILLER                   PIC X.
           05 STS-STEP-RC              PIC X(2).
           05 FILLER                   PIC X(69).
