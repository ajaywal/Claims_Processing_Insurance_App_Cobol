--********************************************************************
-- BillingCoreAJ2026  BILDDL09  Error log
--********************************************************************
SET CURRENT SQLID = 'BILADM';

-- DCLGEN BILDERR PREFIX ERR
CREATE TABLE BILDB.BIL_ERROR_LOG
  (ERROR_ID           INTEGER        NOT NULL,
   PROGRAM_ID         CHAR(8)        NOT NULL,
   PARAGRAPH_NAME     CHAR(30)       NOT NULL,
   SQLCODE_VALUE      INTEGER        NOT NULL WITH DEFAULT 0,
   CICS_RESP          INTEGER        NOT NULL WITH DEFAULT 0,
   ERROR_TEXT         VARCHAR(120)   NOT NULL,
   ENTITY_KEY         CHAR(20)       NOT NULL WITH DEFAULT ' ',
   USER_ID            CHAR(8)        NOT NULL,
   TRAN_ID            CHAR(4)        NOT NULL WITH DEFAULT ' ',
   ERROR_TS           TIMESTAMP      NOT NULL WITH DEFAULT,
   PRIMARY KEY (ERROR_ID))
  IN BILDB.BILTS08;
