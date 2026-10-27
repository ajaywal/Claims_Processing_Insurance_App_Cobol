      ******************************************************************
      * CLMMNU01 - ClaimsCore 3270 navigation block.                   *
      * First part of every ClaimsCore screen COMMAREA; it is the only *
      * part passed on XCTL between screens, so a screen reached by    *
      * XCTL sees CN-OWNER-PGM not equal to its own program id and     *
      * starts a new conversation with the claim / exposure handed on. *
      * Used by CLMMNUP CLMFNLP CLMINQP CLMFINP CLMACTP.               *
      * 2026-10-27 AJ  Initial version                                 *
      ******************************************************************
              05 CN-OWNER-PGM              PIC X(8).
              05 CN-FROM-PGM               PIC X(8).
              05 CN-USER-ID                PIC X(8).
              05 CN-CLAIM-NUMBER           PIC X(12).
              05 CN-EXPOSURE-SEQ           PIC 9(3).
              05 CN-NAV-MSG                PIC X(79).
