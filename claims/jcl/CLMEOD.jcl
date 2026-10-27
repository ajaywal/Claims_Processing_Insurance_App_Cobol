//CLMEOD   JOB (CLMB,0001),'CLAIMS NIGHTLY EOD',CLASS=B,
//             MSGCLASS=X,NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* CLAIMSCOREAJ2026 - NIGHTLY END-OF-DAY STREAM (CLMEOD)            *
//*                                                                   *
//* STEP010 CLMEVTDR  DRAIN EVENTS (BILSTAT/DSBSENT IN, CLM OUT)     *
//* STEP020 CLMFNLBT  FNOL PORTAL INTAKE                              *
//* STEP030 CLMCATAS  CATASTROPHE TAGGING                             *
//* STEP040 CLMDUPCK  DUPLICATE CLAIM DETECTION                       *
//* STEP050 CLMESCAL  ACTIVITY ESCALATION                             *
//* STEP060 CLMCHKIS  CHECK ISSUE + POSITIVE PAY (RAISES CLMPAYMT)   *
//* STEP070 CLMRIRE0  REINSURANCE RECOVERABLES + BORDEREAU            *
//* STEP080 CLMISOSN  ISO CLAIMSEARCH REQUESTS (SEND)                 *
//* STEP090 CLMGLEXT  GENERAL LEDGER JOURNAL EXTRACT                  *
//* STEP100 CLMLAGRP  LOSS RUN AND AGING REPORT + CSV                 *
//* STEP110 CLMRSVRP  RESERVE ADEQUACY REVIEW (RSVREV ACTIVITIES)     *
//* STEP120 CLMSEALR  AUDIT HASH CHAIN SEAL                           *
//* STEP130 CLMEVTDR  SECOND DRAIN - DELIVERS TONIGHT'S CLMPAYMT      *
//* STEP140 CLMEODDR  READINESS CHECKS + ROLL CLM_CONTROL DATE        *
//* STEP150 IEBGENER  OPERATOR ALERT WHEN THE DAY IS NOT CLOSED       *
//*                                                                   *
//* RC CONVENTION: 0 OK, 4 WARNINGS, 8 REJECTS (SEE RPTOUT),          *
//*                12+ ABEND-WORTHY - STREAM STOPS.                   *
//* GATING: STEP010 > 8 STOPS EVERYTHING. STEP020-STEP070 FEED THE    *
//*         FINANCIALS AND RUN ONLY WHILE NO PRIOR STEP EXCEEDED 8.   *
//*         REPORTS (STEP100/110) NEVER BLOCK THE CLOSE. CLMEODDR    *
//*         RUNS ONLY WHEN THE GL EXTRACT AND THE SEAL ENDED <= 4     *
//*         AND NO STEP ABENDED; OTHERWISE STEP150 ALERTS OPERATIONS. *
//* RESTART: CHKPT DATASETS ARE PRE-ALLOCATED, ONE RECORD, DISP=OLD;  *
//*         EACH PROGRAM RESUMES FROM ITS OWN CHECKPOINT. RESTART THE *
//*         JOB AT THE FAILING STEP (RESTART=STEPnnn.RUN). FOR STEP080*
//*         POINT ISOOUT AT THE GENERATION BEING BUILT WITH DISP=MOD. *
//*         CLMEODDR IS SAFE TO RERUN: THE BUSDATE PARM STOPS A       *
//*         SECOND ROLL OF THE SAME DATE.                             *
//* BUSDATE IS SET BY THE SCHEDULER TO THE CLAIMS DATE BEING CLOSED.  *
//*********************************************************************
//         SET BUSDATE=2026-10-27
//         SET HLQ=CLM.PROD
//*
//CLMDB2   PROC
//RUN      EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=CLM.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSNC10.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//RPTOUT   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133)
//         PEND
//*
//*-------------------------------------------------------------------
//* STEP010 - FIRST EVENT DRAIN: BILLING STATUS / DISBURSEMENTS IN,
//*           ANYTHING QUEUED ONLINE OUT
//*-------------------------------------------------------------------
//STEP010  EXEC CLMDB2
//RUN.CHKPT    DD DSN=&HLQ..CLMEVTDR.CHKPT1,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMEVTDR) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB') -
      PARMS('ALL,0200')
  END
/*
//*
//IFDRAIN  IF (STEP010.RUN.RC <= 8 & STEP010.RUN.ABEND = FALSE) THEN
//*-------------------------------------------------------------------
//* STEP020 - FNOL PORTAL INTAKE
//*-------------------------------------------------------------------
//STEP020  EXEC CLMDB2
//RUN.FNOLIN   DD DSN=&HLQ..FNOL.PORTAL.FEED(0),DISP=SHR
//RUN.FNOLREJ  DD DSN=&HLQ..FNOL.REJECTS(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=700,BLKSIZE=0)
//RUN.CHKPT    DD DSN=&HLQ..CLMFNLBT.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200            010
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMFNLBT) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP030 - CATASTROPHE TAGGING (NEEDS TONIGHT'S NEW CLAIMS)
//*-------------------------------------------------------------------
//STEP030  EXEC CLMDB2,COND=(8,LT)
//RUN.CHKPT    DD DSN=&HLQ..CLMCATAS.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMCATAS) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP040 - DUPLICATE CLAIM DETECTION
//*-------------------------------------------------------------------
//STEP040  EXEC CLMDB2,COND=(8,LT)
//RUN.CHKPT    DD DSN=&HLQ..CLMDUPCK.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMDUPCK) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP050 - ACTIVITY ESCALATION
//*-------------------------------------------------------------------
//STEP050  EXEC CLMDB2,COND=(8,LT)
//RUN.CHKPT    DD DSN=&HLQ..CLMESCAL.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200            030 010
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMESCAL) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP060 - CHECK ISSUE AND POSITIVE PAY FILE
//*-------------------------------------------------------------------
//STEP060  EXEC CLMDB2,COND=(8,LT)
//RUN.POSPAY   DD DSN=&HLQ..POSPAY.ISSUE(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//RUN.CHKPT    DD DSN=&HLQ..CLMCHKIS.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200            601234 41237788
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMCHKIS) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP070 - REINSURANCE RECOVERABLES AND BORDEREAU
//*-------------------------------------------------------------------
//STEP070  EXEC CLMDB2,COND=(8,LT)
//RUN.BORDX    DD DSN=&HLQ..RI.BORDEREAU(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//RUN.CHKPT    DD DSN=&HLQ..CLMRIRE0.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMRIRE0) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP080 - ISO CLAIMSEARCH REQUEST FILE (RESPONSES ARE POSTED BY
//*           JOB CLMISOSN, PARM RECV, WHEN THE BUREAU FILE ARRIVES)
//*-------------------------------------------------------------------
//STEP080  EXEC CLMDB2,COND=(8,LT)
//RUN.ISOOUT   DD DSN=&HLQ..ISO.REQUEST(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(2,2),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)
//RUN.ISOIN    DD DUMMY,DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.CHKPT    DD DSN=&HLQ..CLMISOSN.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMISOSN) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB') -
      PARMS('SEND,0200')
  END
/*
//*-------------------------------------------------------------------
//* STEP090 - GENERAL LEDGER JOURNAL EXTRACT (AFTER ALL FINANCIALS)
//*-------------------------------------------------------------------
//STEP090  EXEC CLMDB2,COND=(8,LT)
//RUN.GLOUT    DD DSN=&HLQ..GL.JOURNAL(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//RUN.CHKPT    DD DSN=&HLQ..CLMGLEXT.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMGLEXT) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP100 - LOSS RUN AND AGING (REPORT ONLY - NEVER BLOCKS EOD)
//*-------------------------------------------------------------------
//STEP100  EXEC CLMDB2,COND=(12,LE)
//RUN.CSVOUT   DD DSN=&HLQ..LOSSRUN.CSV(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//RUN.CHKPT    DD DSN=&HLQ..CLMLAGRP.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMLAGRP) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB') -
      PARMS('0500')
  END
/*
//*-------------------------------------------------------------------
//* STEP110 - RESERVE ADEQUACY REVIEW
//*-------------------------------------------------------------------
//STEP110  EXEC CLMDB2,COND=(12,LE)
//RUN.CHKPT    DD DSN=&HLQ..CLMRSVRP.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMRSVRP) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB') -
      PARMS('0200')
  END
/*
//*-------------------------------------------------------------------
//* STEP120 - SEAL THE DAY'S AUDIT HASH CHAIN
//*-------------------------------------------------------------------
//STEP120  EXEC CLMDB2,COND=(12,LE)
//RUN.CHKPT    DD DSN=&HLQ..CLMSEALR.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSIN    DD *
N 00200
/*
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMSEALR) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB')
  END
/*
//*-------------------------------------------------------------------
//* STEP130 - SECOND DRAIN: DELIVER CLMPAYMT / DEDBILL RAISED TONIGHT
//*-------------------------------------------------------------------
//STEP130  EXEC CLMDB2,COND=(12,LE)
//RUN.CHKPT    DD DSN=&HLQ..CLMEVTDR.CHKPT2,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSTSIN  DD *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMEVTDR) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB') -
      PARMS('OUT,0200')
  END
/*
//IFDRAINE ENDIF
//*
//*-------------------------------------------------------------------
//* STEP140 - CLOSE THE CLAIMS BUSINESS DAY. ONLY WHEN THE DRAIN, THE
//*           FINANCIAL FEEDERS, THE GL EXTRACT AND THE SEAL ARE
//*           CLEAN AND NOTHING ABENDED.
//*-------------------------------------------------------------------
//IFCLOSE  IF (ABEND = FALSE & STEP010.RUN.RC <= 8 &
//             STEP060.RUN.RC <= 8 &
//             STEP090.RUN.RC <= 4 &
//             STEP120.RUN.RC <= 4 &
//             STEP130.RUN.RC <= 8) THEN
//STEP140  EXEC CLMDB2
//RUN.CHKPT    DD DSN=&HLQ..CLMEODDR.CHKPT,DISP=OLD,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//RUN.SYSTSIN  DD *,SYMBOLS=JCLONLY
  DSN SYSTEM(DB2P)
  RUN PROGRAM(CLMEODDR) PLAN(CLMBATCH) -
      LIB('CLM.PROD.LOADLIB') -
      PARMS('&BUSDATE')
  END
/*
//IFCLOSEE ENDIF
//*
//*-------------------------------------------------------------------
//* STEP150 - THE DAY WAS NOT CLOSED (STEP140 SKIPPED OR FAILED):
//*           ALERT OPERATIONS. CLM_CONTROL STAYS ON &BUSDATE AND
//*           ONLINE CONTINUES ON THE SAME BUSINESS DATE.
//*-------------------------------------------------------------------
//IFALERT  IF (ABEND | STEP140.RUN.RUN = FALSE |
//             STEP140.RUN.RC > 4) THEN
//STEP150  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT2   DD SYSOUT=(O,,OPER)
//SYSUT1   DD *,SYMBOLS=JCLONLY
CLMEOD   CLAIMSCORE END OF DAY FOR &BUSDATE DID NOT COMPLETE.
CLMEOD   CHECK THE STEP RETURN CODES AND RPTOUT OF THE FAILING STEP,
CLMEOD   FIX, THEN RESTART THE JOB AT THAT STEP. CLMEODDR WILL NOT
CLMEOD   ROLL THE DATE TWICE.
/*
//IFALERTE ENDIF
