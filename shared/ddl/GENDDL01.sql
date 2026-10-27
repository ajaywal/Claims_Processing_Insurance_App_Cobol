--********************************************************************
-- Shared  GENDDL01  IBM GenApp tables read by BillingCore/ClaimsCore
-- Restated from cicsdev/cics-genapp base/cntl/db2cre.jcl (IBM, Apache
-- 2.0) so the suite can DCLGEN host structures for cross-schema reads.
-- The suite never writes these tables; GenApp remains their owner.
--********************************************************************

-- DCLGEN GENDCUS PREFIX GCU
CREATE TABLE GENAPP.CUSTOMER
  (CUSTOMERNUMBER     INTEGER        NOT NULL,
   FIRSTNAME          CHAR(10),
   LASTNAME           CHAR(20),
   DATEOFBIRTH        DATE,
   HOUSENAME          CHAR(20),
   HOUSENUMBER        CHAR(4),
   POSTCODE           CHAR(8),
   PHONEHOME          CHAR(20),
   PHONEMOBILE        CHAR(20),
   EMAILADDRESS       CHAR(100),
   PRIMARY KEY (CUSTOMERNUMBER))
  IN GENAPP.GENATS00;

-- DCLGEN GENDPOL PREFIX GPO
CREATE TABLE GENAPP.POLICY
  (POLICYNUMBER       INTEGER        NOT NULL,
   CUSTOMERNUMBER     INTEGER        NOT NULL,
   ISSUEDATE          DATE,
   EXPIRYDATE         DATE,
   POLICYTYPE         CHAR(1),
   LASTCHANGED        TIMESTAMP      NOT NULL WITH DEFAULT,
   BROKERID           INTEGER,
   BROKERSREFERENCE   CHAR(10),
   PAYMENT            INTEGER,
   COMMISSION         SMALLINT,
   PRIMARY KEY (POLICYNUMBER))
  IN GENAPP.GENATS02;

-- DCLGEN GENDMOT PREFIX GMO
CREATE TABLE GENAPP.MOTOR
  (POLICYNUMBER       INTEGER        NOT NULL,
   MAKE               CHAR(15),
   MODEL              CHAR(15),
   VALUE              INTEGER,
   REGNUMBER          CHAR(7),
   COLOUR             CHAR(8),
   CC                 SMALLINT,
   YEAROFMANUFACTURE  DATE,
   PREMIUM            INTEGER,
   ACCIDENTS          INTEGER,
   PRIMARY KEY (POLICYNUMBER))
  IN GENAPP.GENATS05;

-- DCLGEN GENDHOU PREFIX GHO
CREATE TABLE GENAPP.HOUSE
  (POLICYNUMBER       INTEGER        NOT NULL,
   PROPERTYTYPE       CHAR(15),
   BEDROOMS           SMALLINT,
   VALUE              INTEGER,
   HOUSENAME          CHAR(20),
   HOUSENUMBER        CHAR(4),
   POSTCODE           CHAR(8),
   PRIMARY KEY (POLICYNUMBER))
  IN GENAPP.GENATS04;

-- DCLGEN GENDCOM PREFIX GCO
CREATE TABLE GENAPP.COMMERCIAL
  (POLICYNUMBER       INTEGER        NOT NULL,
   REQUESTDATE        TIMESTAMP,
   STARTDATE          DATE,
   RENEWALDATE        DATE,
   ADDRESS            CHAR(255),
   ZIPCODE            CHAR(8),
   LATITUDEN          CHAR(11),
   LONGITUDEW         CHAR(11),
   CUSTOMER           CHAR(255),
   PROPERTYTYPE       CHAR(255),
   FIREPERIL          SMALLINT,
   FIREPREMIUM        INTEGER,
   CRIMEPERIL         SMALLINT,
   CRIMEPREMIUM       INTEGER,
   FLOODPERIL         SMALLINT,
   FLOODPREMIUM       INTEGER,
   WEATHERPERIL       SMALLINT,
   WEATHERPREMIUM     INTEGER,
   STATUS             SMALLINT,
   REJECTIONREASON    CHAR(255),
   PRIMARY KEY (POLICYNUMBER))
  IN GENAPP.GENATS06;
