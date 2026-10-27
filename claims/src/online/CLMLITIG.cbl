       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMLITIG.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMLITIG                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Litigation matters. Opening a matter flags the   *
      *               claim as litigated, transfers it to the          *
      *               litigation group supervisor, raises the LITOPEN  *
      *               workflow and re-segments the claim. Matters are  *
      *               updated through to trial and closed with a       *
      *               resolution; the claim flag clears with the last  *
      *               open matter.                                     *
      * REQUESTS    : LITNEW LITUPD LITCLS LITINQ                      *
      * CALLED BY   : litigation screens, legal bill intake batch      *
      * CALLS       : CLMSEQGN CLMDATES CLMASSGN CLMACTV0 CLMSEGMT     *
      *               CLMHIST0 CLMAUDCH CLMERRLG                       *
      * TABLES      : CLM_LITIGATION (C R U)  CLM_CLAIM (R U)          *
      *               CLM_CLAIM_CONTACT (R)  CLM_GROUP (R)             *
      *               CLM_CONTROL (R)                                  *
      * RULES       : BR-CLM-LIT-001 Matters only on an OPEN claim     *
      *               BR-CLM-LIT-002 Matter type SUIT, ARBITRAT or     *
      *                              MEDIATN; a SUIT needs court and   *
      *                              case number                       *
      *               BR-CLM-LIT-003 File date valid, not before the   *
      *                              loss date and not in the future   *
      *               BR-CLM-LIT-004 Attorneys must be ATTORNEY        *
      *                              contacts on the claim             *
      *               BR-CLM-LIT-005 Case number unique among the      *
      *                              claim's open matters              *
      *               BR-CLM-LIT-006 New matter sets LITIGATION_FLAG   *
      *               BR-CLM-LIT-007 Claim moves to the litigation     *
      *                              group supervisor (regional group  *
      *                              preferred) unless already there   *
      *               BR-CLM-LIT-008 New matter raises LITOPEN         *
      *                              activities and re-segments        *
      *               BR-CLM-LIT-009 Trial date must be after the file *
      *                              date                              *
      *               BR-CLM-LIT-010 Legal spend is cumulative and     *
      *                              never decreases                   *
      *               BR-CLM-LIT-011 Only OPEN matters can change      *
      *               BR-CLM-LIT-012 Resolution SETTLED needs a        *
      *                              settlement, DISMISSD none,        *
      *                              JUDGMNT any award >= 0            *
      *               BR-CLM-LIT-013 Flag clears only when no other    *
      *                              matter on the claim is open       *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMLITIG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ASG-PGM                  PIC X(8)  VALUE 'CLMASSGN'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-SEG-PGM                  PIC X(8)  VALUE 'CLMSEGMT'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10) VALUE SPACES.
           05 WS-COUNT                 PIC S9(9) COMP VALUE 0.
           05 WS-ATTY-ID               PIC S9(9) COMP VALUE 0.
           05 WS-MATTER-TYPE           PIC X(8)  VALUE SPACES.
              88 WS-VALID-MATTER-TYPE  VALUE 'SUIT    ' 'ARBITRAT'
                                             'MEDIATN '.
           05 WS-REGION                PIC X(4)  VALUE SPACES.
           05 WS-LIT-GROUP             PIC X(8)  VALUE SPACES.
           05 WS-LIT-SUPERVISOR        PIC X(8)  VALUE SPACES.
           05 WS-CUR-GROUP-TYPE        PIC X(4)  VALUE SPACES.
           05 WS-DATE-REQ              PIC X(8)  VALUE SPACES.
           05 WS-DATE-1                PIC X(10) VALUE SPACES.
           05 WS-DATE-2                PIC X(10) VALUE SPACES.
           05 WS-AMT-DISP              PIC Z(10)9.99.
           05 WS-ID-DISP               PIC 9(9).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30) VALUE SPACES.
           05 WS-HIS-EVENT             PIC X(8)  VALUE SPACES.
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDLIT END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDGRP END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

       01  WS-SVC-RESULT.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SVC-==.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-ASG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ASA-==.
           03 ASA-BODY.
              COPY CLMCASG REPLACING LEADING ==CA-== BY ==ASA-==.

       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.

       01  WS-SEG-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SGA-==.
           03 SGA-BODY.
              COPY CLMCSEG REPLACING LEADING ==CA-== BY ==SGA-==.

       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.

       01  WS-AUD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ADA-==.
           03 ADA-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==ADA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCLIT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'LITNEW  '
                 PERFORM 2100-NEW-MATTER
              WHEN 'LITUPD  '
                 PERFORM 2200-UPDATE-MATTER
              WHEN 'LITCLS  '
                 PERFORM 2300-CLOSE-MATTER
              WHEN 'LITINQ  '
                 PERFORM 2400-INQUIRE-MATTER
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMLITIG: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           IF CA-BUS-DATE NOT = SPACES
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM CLMDB.CLM_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '1000-INITIALISE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * LITNEW
       2100-NEW-MATTER.
           IF CA-LT-CLAIM-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CLAIMREQ'          TO CA-REASON-CODE
              MOVE 'Claim id is required for a litigation matter'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-READ-CLAIM
      * BR-CLM-LIT-001 Matters only on an OPEN claim
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMNOTOP'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ', not OPEN'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3100-VALIDATE-NEW-MATTER
           PERFORM 3300-VALIDATE-ATTORNEYS
           PERFORM 4400-FIND-LIT-GROUP
           PERFORM 8000-NEXT-MATTER-ID
           MOVE CA-LT-CLAIM-ID         TO LIT-CLAIM-ID
           MOVE CA-LT-MATTER-TYPE      TO LIT-MATTER-TYPE
           PERFORM 5100-SET-COURT
           MOVE CA-LT-CASE-NUMBER      TO LIT-CASE-NUMBER
           MOVE CA-LT-PLAINTIFF-ATTY   TO LIT-PLAINTIFF-ATTY-ID
           MOVE CA-LT-DEFENSE-ATTY     TO LIT-DEFENSE-ATTY-ID
           MOVE CA-LT-FILE-DATE        TO LIT-FILE-DATE
           PERFORM 5200-SET-TRIAL-DATE
           MOVE 'OPEN'                 TO LIT-MATTER-STATUS
           MOVE SPACES                 TO LIT-RESOLUTION
           MOVE 0                      TO LIT-SETTLEMENT-AMOUNT
           IF CA-LT-LEGAL-SPEND > 0
              MOVE CA-LT-LEGAL-SPEND   TO LIT-LEGAL-SPEND
           ELSE
              MOVE 0                   TO LIT-LEGAL-SPEND
           END-IF
           EXEC SQL
              INSERT INTO CLMDB.CLM_LITIGATION
                     (MATTER_ID, CLAIM_ID, MATTER_TYPE, COURT_NAME,
                      CASE_NUMBER, PLAINTIFF_ATTY_ID, DEFENSE_ATTY_ID,
                      FILE_DATE, TRIAL_DATE, MATTER_STATUS,
                      RESOLUTION, SETTLEMENT_AMOUNT, LEGAL_SPEND,
                      CLOSED_DATE)
              VALUES (:LIT-MATTER-ID, :LIT-CLAIM-ID,
                      :LIT-MATTER-TYPE,
                      :LIT-COURT-NAME :LIT-COURT-NAME-IND,
                      :LIT-CASE-NUMBER, :LIT-PLAINTIFF-ATTY-ID,
                      :LIT-DEFENSE-ATTY-ID, :LIT-FILE-DATE,
                      :LIT-TRIAL-DATE :LIT-TRIAL-DATE-IND,
                      :LIT-MATTER-STATUS, :LIT-RESOLUTION,
                      :LIT-SETTLEMENT-AMOUNT, :LIT-LEGAL-SPEND, NULL)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE LIT-MATTER-ID    TO WS-ID-DISP
                 STRING 'Matter id ' WS-ID-DISP ' already exists'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2100-NEW-MATTER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE LIT-MATTER-ID          TO CA-LT-MATTER-ID
           MOVE 'OPEN'                 TO CA-LT-STATUS
      * BR-CLM-LIT-006 Claim carries the litigation flag
           IF CLM-LITIGATION-FLAG NOT = 'Y'
              MOVE 'Y'                 TO CLM-LITIGATION-FLAG
              PERFORM 4500-UPDATE-CLAIM-FLAG
           END-IF
      * BR-CLM-LIT-007 Transfer to the litigation group supervisor
           IF WS-CUR-GROUP-TYPE NOT = 'LIT '
              AND CLM-ASSIGNED-USER NOT = WS-LIT-SUPERVISOR
              PERFORM 8100-TRANSFER-CLAIM
           END-IF
      * BR-CLM-LIT-008 LITOPEN workflow and re-segmentation
           PERFORM 8200-GENERATE-LITOPEN
           PERFORM 8300-RESEGMENT-CLAIM
           MOVE 'LITOPEN '             TO WS-HIS-EVENT
           MOVE LIT-MATTER-ID          TO WS-ID-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Litigation matter ' WS-ID-DISP ' opened ('
                  CA-LT-MATTER-TYPE ') case ' CA-LT-CASE-NUMBER
                  ' filed ' CA-LT-FILE-DATE
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8400-WRITE-HISTORY
           PERFORM 8500-AUDIT-MATTER
           STRING 'Matter ' WS-ID-DISP ' opened; claim with '
                  WS-LIT-SUPERVISOR DELIMITED BY SIZE
                  INTO CA-MESSAGE.

      ******************************************************************
      * LITUPD
       2200-UPDATE-MATTER.
           PERFORM 3400-LOAD-OPEN-MATTER
           IF CA-LT-COURT NOT = SPACES
              PERFORM 5100-SET-COURT
           END-IF
           IF CA-LT-CASE-NUMBER NOT = SPACES
              AND CA-LT-CASE-NUMBER NOT = LIT-CASE-NUMBER
              PERFORM 3500-CHECK-CASE-NUMBER
              MOVE CA-LT-CASE-NUMBER   TO LIT-CASE-NUMBER
           END-IF
           IF CA-LT-PLAINTIFF-ATTY > 0 OR CA-LT-DEFENSE-ATTY > 0
              PERFORM 3300-VALIDATE-ATTORNEYS
           END-IF
           IF CA-LT-PLAINTIFF-ATTY > 0
              MOVE CA-LT-PLAINTIFF-ATTY TO LIT-PLAINTIFF-ATTY-ID
           END-IF
           IF CA-LT-DEFENSE-ATTY > 0
              MOVE CA-LT-DEFENSE-ATTY  TO LIT-DEFENSE-ATTY-ID
           END-IF
           IF CA-LT-TRIAL-DATE NOT = SPACES
              PERFORM 3200-VALIDATE-TRIAL-DATE
              PERFORM 5200-SET-TRIAL-DATE
           END-IF
           PERFORM 3600-APPLY-LEGAL-SPEND
           EXEC SQL
              UPDATE CLMDB.CLM_LITIGATION
                 SET COURT_NAME        =
                        :LIT-COURT-NAME :LIT-COURT-NAME-IND,
                     CASE_NUMBER       = :LIT-CASE-NUMBER,
                     PLAINTIFF_ATTY_ID = :LIT-PLAINTIFF-ATTY-ID,
                     DEFENSE_ATTY_ID   = :LIT-DEFENSE-ATTY-ID,
                     TRIAL_DATE        =
                        :LIT-TRIAL-DATE :LIT-TRIAL-DATE-IND,
                     LEGAL_SPEND       = :LIT-LEGAL-SPEND
               WHERE MATTER_ID         = :LIT-MATTER-ID
                 AND MATTER_STATUS     = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-UPDATE-MATTER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Matter was closed by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 2900-RETURN-MATTER
           MOVE 'LITUPD  '             TO WS-HIS-EVENT
           MOVE LIT-MATTER-ID          TO WS-ID-DISP
           MOVE LIT-LEGAL-SPEND        TO WS-AMT-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Matter ' WS-ID-DISP ' updated; trial '
                  CA-LT-TRIAL-DATE ' legal spend ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8400-WRITE-HISTORY
           STRING 'Matter ' WS-ID-DISP ' updated'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * LITCLS
       2300-CLOSE-MATTER.
           PERFORM 3400-LOAD-OPEN-MATTER
      * BR-CLM-LIT-012 Resolution / settlement consistency
           EVALUATE CA-LT-RESOLUTION
              WHEN 'SETTLED '
                 IF CA-LT-SETTLEMENT NOT > 0
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'SETLAMT '    TO CA-REASON-CODE
                    MOVE 'A settled matter needs the settlement amount'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 'JUDGMNT '
                 IF CA-LT-SETTLEMENT < 0
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'SETLAMT '    TO CA-REASON-CODE
                    MOVE 'Judgment award cannot be negative'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 'DISMISSD'
                 IF CA-LT-SETTLEMENT NOT = 0
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'SETLAMT '    TO CA-REASON-CODE
                    MOVE 'A dismissed matter has no settlement'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN OTHER
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'BADRESOL'       TO CA-REASON-CODE
                 STRING 'Resolution ' CA-LT-RESOLUTION
                        ' must be SETTLED, JUDGMNT or DISMISSD'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE
           PERFORM 3600-APPLY-LEGAL-SPEND
           MOVE CA-LT-RESOLUTION       TO LIT-RESOLUTION
           MOVE CA-LT-SETTLEMENT       TO LIT-SETTLEMENT-AMOUNT
           MOVE 'CLSD'                 TO LIT-MATTER-STATUS
           MOVE WS-BUS-DATE            TO LIT-CLOSED-DATE
           EXEC SQL
              UPDATE CLMDB.CLM_LITIGATION
                 SET MATTER_STATUS     = 'CLSD',
                     RESOLUTION        = :LIT-RESOLUTION,
                     SETTLEMENT_AMOUNT = :LIT-SETTLEMENT-AMOUNT,
                     LEGAL_SPEND       = :LIT-LEGAL-SPEND,
                     CLOSED_DATE       = :LIT-CLOSED-DATE
               WHERE MATTER_ID         = :LIT-MATTER-ID
                 AND MATTER_STATUS     = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-CLOSE-MATTER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Matter was closed by another user'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-LIT-013 Clear the flag with the last open matter
           MOVE LIT-CLAIM-ID           TO CA-LT-CLAIM-ID
           PERFORM 4100-READ-CLAIM
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_LITIGATION
               WHERE CLAIM_ID      = :LIT-CLAIM-ID
                 AND MATTER_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2300-CLOSE-MATTER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0 AND CLM-LITIGATION-FLAG = 'Y'
              MOVE 'N'                 TO CLM-LITIGATION-FLAG
              PERFORM 4500-UPDATE-CLAIM-FLAG
              PERFORM 8300-RESEGMENT-CLAIM
           END-IF
           PERFORM 2900-RETURN-MATTER
           MOVE 'LITCLOSE'             TO WS-HIS-EVENT
           MOVE LIT-MATTER-ID          TO WS-ID-DISP
           MOVE LIT-SETTLEMENT-AMOUNT  TO WS-AMT-DISP
           MOVE SPACES                 TO WS-HIS-DESC
           STRING 'Matter ' WS-ID-DISP ' closed ' CA-LT-RESOLUTION
                  ' amount ' WS-AMT-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8400-WRITE-HISTORY
           PERFORM 8500-AUDIT-MATTER
           IF WS-COUNT = 0
              STRING 'Matter ' WS-ID-DISP ' closed; litigation flag '
                     'cleared' DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              STRING 'Matter ' WS-ID-DISP ' closed; other matters '
                     'remain open' DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
      * LITINQ - by matter id, else the claim's latest open matter
       2400-INQUIRE-MATTER.
           IF CA-LT-MATTER-ID > 0
              PERFORM 4200-READ-MATTER
           ELSE
              IF CA-LT-CLAIM-ID NOT > 0
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'KEYREQ  '       TO CA-REASON-CODE
                 MOVE 'Matter id or claim id is required'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              EXEC SQL
                 SELECT MATTER_ID
                   INTO :LIT-MATTER-ID
                   FROM CLMDB.CLM_LITIGATION
                  WHERE CLAIM_ID = :CA-LT-CLAIM-ID
                  ORDER BY CASE MATTER_STATUS WHEN 'OPEN' THEN 0
                           ELSE 1 END,
                           FILE_DATE DESC, MATTER_ID DESC
                  FETCH FIRST 1 ROW ONLY
              END-EXEC
              EVALUATE SQLCODE
                 WHEN 0
                    MOVE LIT-MATTER-ID TO CA-LT-MATTER-ID
                    PERFORM 4200-READ-MATTER
                 WHEN 100
                    SET CA-RC-NOT-FOUND TO TRUE
                    MOVE 'LITNOTFD'    TO CA-REASON-CODE
                    MOVE 'No litigation matters on this claim'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 WHEN OTHER
                    MOVE '2400-INQUIRE-MATTER' TO WS-ERR-PARA
                    PERFORM 9900-DB2-ERROR
              END-EVALUATE
           END-IF
           PERFORM 2900-RETURN-MATTER
           MOVE LIT-MATTER-ID          TO WS-ID-DISP
           STRING 'Matter ' WS-ID-DISP ' is ' LIT-MATTER-STATUS
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
       2900-RETURN-MATTER.
           MOVE LIT-MATTER-ID          TO CA-LT-MATTER-ID
           MOVE LIT-CLAIM-ID           TO CA-LT-CLAIM-ID
           MOVE LIT-MATTER-TYPE        TO CA-LT-MATTER-TYPE
           MOVE SPACES                 TO CA-LT-COURT
           IF LIT-COURT-NAME-IND = 0 AND LIT-COURT-NAME-LEN > 0
              MOVE LIT-COURT-NAME-TEXT(1:LIT-COURT-NAME-LEN)
                                       TO CA-LT-COURT
           END-IF
           MOVE LIT-CASE-NUMBER        TO CA-LT-CASE-NUMBER
           MOVE LIT-PLAINTIFF-ATTY-ID  TO CA-LT-PLAINTIFF-ATTY
           MOVE LIT-DEFENSE-ATTY-ID    TO CA-LT-DEFENSE-ATTY
           MOVE LIT-FILE-DATE          TO CA-LT-FILE-DATE
           IF LIT-TRIAL-DATE-IND = 0
              MOVE LIT-TRIAL-DATE      TO CA-LT-TRIAL-DATE
           ELSE
              MOVE SPACES              TO CA-LT-TRIAL-DATE
           END-IF
           MOVE LIT-MATTER-STATUS      TO CA-LT-STATUS
           MOVE LIT-RESOLUTION         TO CA-LT-RESOLUTION
           MOVE LIT-SETTLEMENT-AMOUNT  TO CA-LT-SETTLEMENT
           MOVE LIT-LEGAL-SPEND        TO CA-LT-LEGAL-SPEND.

      ******************************************************************
       3100-VALIDATE-NEW-MATTER.
      * BR-CLM-LIT-002 Matter type; a SUIT needs court and case number
           MOVE CA-LT-MATTER-TYPE      TO WS-MATTER-TYPE
           IF NOT WS-VALID-MATTER-TYPE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADMTYPE'          TO CA-REASON-CODE
              STRING 'Matter type ' CA-LT-MATTER-TYPE
                     ' must be SUIT, ARBITRAT or MEDIATN'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-LT-MATTER-TYPE = 'SUIT'
              AND (CA-LT-COURT = SPACES
                   OR CA-LT-CASE-NUMBER = SPACES)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SUITCRT '          TO CA-REASON-CODE
              MOVE 'A suit needs the court name and case number'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-LIT-003 File date: valid, >= loss date, <= today
           MOVE 'DTVALD  '             TO WS-DATE-REQ
           MOVE CA-LT-FILE-DATE        TO WS-DATE-1
           PERFORM 8600-CALL-DATES
           IF CA-LT-FILE-DATE = SPACES OR NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FILEDATE'          TO CA-REASON-CODE
              MOVE 'A valid file date is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DTDIFF  '             TO WS-DATE-REQ
           MOVE CLM-LOSS-DATE          TO WS-DATE-1
           MOVE CA-LT-FILE-DATE        TO WS-DATE-2
           PERFORM 8600-CALL-DATES
           IF DTA-DT-RESULT-NUM < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FILEDATE'          TO CA-REASON-CODE
              STRING 'File date is before the loss date '
                     CLM-LOSS-DATE DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-LT-FILE-DATE        TO WS-DATE-1
           MOVE WS-BUS-DATE            TO WS-DATE-2
           PERFORM 8600-CALL-DATES
           IF DTA-DT-RESULT-NUM < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'FILEDATE'          TO CA-REASON-CODE
              MOVE 'File date cannot be in the future' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-LT-FILE-DATE        TO LIT-FILE-DATE
           IF CA-LT-TRIAL-DATE NOT = SPACES
              PERFORM 3200-VALIDATE-TRIAL-DATE
           END-IF
           IF CA-LT-CASE-NUMBER NOT = SPACES
              PERFORM 3500-CHECK-CASE-NUMBER
           END-IF
           IF CA-LT-LEGAL-SPEND < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'BADSPEND'          TO CA-REASON-CODE
              MOVE 'Legal spend cannot be negative' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-LIT-009 Trial date valid and after the file date
       3200-VALIDATE-TRIAL-DATE.
           MOVE 'DTVALD  '             TO WS-DATE-REQ
           MOVE CA-LT-TRIAL-DATE       TO WS-DATE-1
           PERFORM 8600-CALL-DATES
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TRIALDT '          TO CA-REASON-CODE
              STRING 'Trial date ' CA-LT-TRIAL-DATE ' is not valid'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DTDIFF  '             TO WS-DATE-REQ
           MOVE LIT-FILE-DATE          TO WS-DATE-1
           MOVE CA-LT-TRIAL-DATE       TO WS-DATE-2
           PERFORM 8600-CALL-DATES
           IF DTA-DT-RESULT-NUM NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TRIALDT '          TO CA-REASON-CODE
              STRING 'Trial date must be after the file date '
                     LIT-FILE-DATE DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-LIT-004 Attorneys must be ATTORNEY contacts on the claim
       3300-VALIDATE-ATTORNEYS.
           IF CA-LT-PLAINTIFF-ATTY > 0
              MOVE CA-LT-PLAINTIFF-ATTY TO WS-ATTY-ID
              PERFORM 3310-CHECK-ATTORNEY-ROLE
           END-IF
           IF CA-LT-DEFENSE-ATTY > 0
              MOVE CA-LT-DEFENSE-ATTY  TO WS-ATTY-ID
              PERFORM 3310-CHECK-ATTORNEY-ROLE
           END-IF
           IF CA-LT-PLAINTIFF-ATTY > 0
              AND CA-LT-PLAINTIFF-ATTY = CA-LT-DEFENSE-ATTY
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ATTYSAME'          TO CA-REASON-CODE
              MOVE 'Plaintiff and defense attorney must differ'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3310-CHECK-ATTORNEY-ROLE.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_CLAIM_CONTACT
               WHERE CLAIM_ID     = :CA-LT-CLAIM-ID
                 AND CONTACT_ID   = :WS-ATTY-ID
                 AND CONTACT_ROLE = 'ATTORNEY'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3310-CHECK-ATTORNEY-ROLE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'NOTATTY '          TO CA-REASON-CODE
              MOVE WS-ATTY-ID          TO WS-ID-DISP
              STRING 'Contact ' WS-ID-DISP
                     ' is not an ATTORNEY on this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-LIT-011 Only OPEN matters can change
       3400-LOAD-OPEN-MATTER.
           IF CA-LT-MATTER-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'MATIDREQ'          TO CA-REASON-CODE
              MOVE 'Matter id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4200-READ-MATTER
           IF LIT-MATTER-STATUS NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'LITCLOSD'          TO CA-REASON-CODE
              MOVE LIT-MATTER-ID       TO WS-ID-DISP
              STRING 'Matter ' WS-ID-DISP ' is closed ('
                     LIT-RESOLUTION ')' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE LIT-CLAIM-ID           TO CA-LT-CLAIM-ID.

      ******************************************************************
      * BR-CLM-LIT-005 Case number unique among open matters
       3500-CHECK-CASE-NUMBER.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM CLMDB.CLM_LITIGATION
               WHERE CLAIM_ID      = :CA-LT-CLAIM-ID
                 AND CASE_NUMBER   = :CA-LT-CASE-NUMBER
                 AND MATTER_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3500-CHECK-CASE-NUMBER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'CASEDUP '          TO CA-REASON-CODE
              STRING 'Case ' CA-LT-CASE-NUMBER
                     ' is already an open matter on this claim'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-LIT-010 Legal spend is cumulative, never decreases
       3600-APPLY-LEGAL-SPEND.
           IF CA-LT-LEGAL-SPEND NOT = 0
              IF CA-LT-LEGAL-SPEND < LIT-LEGAL-SPEND
                 MOVE LIT-LEGAL-SPEND  TO WS-AMT-DISP
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'BADSPEND'       TO CA-REASON-CODE
                 STRING 'Legal spend cannot fall below '
                        WS-AMT-DISP DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE CA-LT-LEGAL-SPEND   TO LIT-LEGAL-SPEND
           END-IF.

      ******************************************************************
       4100-READ-CLAIM.
           EXEC SQL
              SELECT CLAIM_NUMBER, LOSS_DATE, CLAIM_STATE,
                     LITIGATION_FLAG, ASSIGNED_USER, ASSIGNED_GROUP,
                     VERSION_NO
                INTO :CLM-CLAIM-NUMBER, :CLM-LOSS-DATE,
                     :CLM-CLAIM-STATE, :CLM-LITIGATION-FLAG,
                     :CLM-ASSIGNED-USER, :CLM-ASSIGNED-GROUP,
                     :CLM-VERSION-NO
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CA-LT-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE CA-LT-CLAIM-ID   TO WS-ID-DISP
                 STRING 'Claim id ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-MATTER.
           EXEC SQL
              SELECT MATTER_ID, CLAIM_ID, MATTER_TYPE, COURT_NAME,
                     CASE_NUMBER, PLAINTIFF_ATTY_ID, DEFENSE_ATTY_ID,
                     FILE_DATE, TRIAL_DATE, MATTER_STATUS, RESOLUTION,
                     SETTLEMENT_AMOUNT, LEGAL_SPEND
                INTO :LIT-MATTER-ID, :LIT-CLAIM-ID, :LIT-MATTER-TYPE,
                     :LIT-COURT-NAME :LIT-COURT-NAME-IND,
                     :LIT-CASE-NUMBER, :LIT-PLAINTIFF-ATTY-ID,
                     :LIT-DEFENSE-ATTY-ID, :LIT-FILE-DATE,
                     :LIT-TRIAL-DATE :LIT-TRIAL-DATE-IND,
                     :LIT-MATTER-STATUS, :LIT-RESOLUTION,
                     :LIT-SETTLEMENT-AMOUNT, :LIT-LEGAL-SPEND
                FROM CLMDB.CLM_LITIGATION
               WHERE MATTER_ID = :CA-LT-MATTER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'LITNOTFD'       TO CA-REASON-CODE
                 MOVE CA-LT-MATTER-ID  TO WS-ID-DISP
                 STRING 'Matter ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4200-READ-MATTER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-LIT-007 Litigation group: the claim's region first
       4400-FIND-LIT-GROUP.
           MOVE SPACES                 TO WS-REGION
                                          WS-CUR-GROUP-TYPE
           IF CLM-ASSIGNED-GROUP NOT = SPACES
              EXEC SQL
                 SELECT REGION_CODE, GROUP_TYPE
                   INTO :WS-REGION, :WS-CUR-GROUP-TYPE
                   FROM CLMDB.CLM_GROUP
                  WHERE GROUP_CODE = :CLM-ASSIGNED-GROUP
              END-EXEC
              IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                 MOVE '4400-FIND-LIT-GROUP' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF
           EXEC SQL
              SELECT GROUP_CODE, SUPERVISOR_ID
                INTO :GRP-GROUP-CODE, :GRP-SUPERVISOR-ID
                FROM CLMDB.CLM_GROUP
               WHERE GROUP_TYPE  = 'LIT '
                 AND ACTIVE_FLAG = 'Y'
               ORDER BY CASE WHEN REGION_CODE = :WS-REGION
                             THEN 0 ELSE 1 END,
                        GROUP_CODE
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE GRP-GROUP-CODE   TO WS-LIT-GROUP
                 MOVE GRP-SUPERVISOR-ID TO WS-LIT-SUPERVISOR
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'NOLITGRP'       TO CA-REASON-CODE
                 MOVE 'No active litigation (LIT) group is defined'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4400-FIND-LIT-GROUP' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-CUR-GROUP-TYPE = 'LIT '
              MOVE CLM-ASSIGNED-USER   TO WS-LIT-SUPERVISOR
           END-IF.

      ******************************************************************
      * Optimistic lock on the claim row
       4500-UPDATE-CLAIM-FLAG.
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET LITIGATION_FLAG = :CLM-LITIGATION-FLAG,
                     UPDATE_TS       = CURRENT TIMESTAMP,
                     VERSION_NO      = VERSION_NO + 1
               WHERE CLAIM_ID        = :CA-LT-CLAIM-ID
                 AND VERSION_NO      = :CLM-VERSION-NO
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-UPDATE-CLAIM-FLAG' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Claim changed by another user - retry'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           ADD 1                       TO CLM-VERSION-NO.

      ******************************************************************
       5100-SET-COURT.
           IF CA-LT-COURT = SPACES
              MOVE -1                  TO LIT-COURT-NAME-IND
              MOVE 0                   TO LIT-COURT-NAME-LEN
           ELSE
              MOVE 0                   TO LIT-COURT-NAME-IND
              MOVE CA-LT-COURT         TO LIT-COURT-NAME-TEXT
              MOVE FUNCTION LENGTH(FUNCTION TRIM(CA-LT-COURT
                   TRAILING))          TO LIT-COURT-NAME-LEN
           END-IF.

      ******************************************************************
       5200-SET-TRIAL-DATE.
           IF CA-LT-TRIAL-DATE = SPACES
              MOVE -1                  TO LIT-TRIAL-DATE-IND
              MOVE SPACES              TO LIT-TRIAL-DATE
           ELSE
              MOVE 0                   TO LIT-TRIAL-DATE-IND
              MOVE CA-LT-TRIAL-DATE    TO LIT-TRIAL-DATE
           END-IF.

      ******************************************************************
       8000-NEXT-MATTER-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 'LIT     '             TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           MOVE SQA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE
           MOVE SQA-SQ-VALUE           TO LIT-MATTER-ID.

      ******************************************************************
       8100-TRANSFER-CLAIM.
           INITIALIZE WS-ASG-CA
           MOVE 'ASGXFR  '             TO ASA-REQUEST-ID
           MOVE CA-USER-ID             TO ASA-USER-ID
           MOVE WS-BUS-DATE            TO ASA-BUS-DATE
           MOVE CA-TRACE-ID            TO ASA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ASA-CALLER-PGM
           MOVE CA-LT-CLAIM-ID         TO ASA-AS-CLAIM-ID
           MOVE WS-LIT-SUPERVISOR      TO ASA-AS-TARGET-USER
           CALL WS-ASG-PGM USING WS-ASG-CA
           MOVE ASA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8200-GENERATE-LITOPEN.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTGEN  '             TO AVA-REQUEST-ID
           MOVE CA-USER-ID             TO AVA-USER-ID
           MOVE WS-BUS-DATE            TO AVA-BUS-DATE
           MOVE CA-TRACE-ID            TO AVA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AVA-CALLER-PGM
           MOVE CA-LT-CLAIM-ID         TO AVA-AV-CLAIM-ID
           MOVE 'LITOPEN '             TO AVA-AV-EVENT
           CALL WS-ACT-PGM USING WS-ACT-CA
           MOVE AVA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8300-RESEGMENT-CLAIM.
           INITIALIZE WS-SEG-CA
           MOVE 'SEGCLM  '             TO SGA-REQUEST-ID
           MOVE CA-USER-ID             TO SGA-USER-ID
           MOVE WS-BUS-DATE            TO SGA-BUS-DATE
           MOVE CA-TRACE-ID            TO SGA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SGA-CALLER-PGM
           MOVE CA-LT-CLAIM-ID         TO SGA-SG-CLAIM-ID
           CALL WS-SEG-PGM USING WS-SEG-CA
           MOVE SGA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
       8400-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE WS-BUS-DATE            TO HIA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE CA-LT-CLAIM-ID         TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           MOVE HIA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Matter open / close is sealed in the audit hash chain
       8500-AUDIT-MATTER.
           INITIALIZE WS-AUD-CA
           MOVE 'AUCADD  '             TO ADA-REQUEST-ID
           MOVE CA-USER-ID             TO ADA-USER-ID
           MOVE WS-BUS-DATE            TO ADA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ADA-CALLER-PGM
           MOVE CA-LT-CLAIM-ID         TO ADA-AC-CLAIM-ID
           MOVE CA-REQUEST-ID          TO ADA-AC-EVENT-CODE
           MOVE WS-PROGRAM-ID          TO ADA-AC-PROGRAM
           STRING CLM-CLAIM-NUMBER '|' WS-HIS-DESC
                  DELIMITED BY SIZE INTO ADA-AC-PAYLOAD
           CALL WS-AUD-PGM USING WS-AUD-CA
           MOVE ADA-HEADER             TO SVC-HEADER
           PERFORM 8900-CHECK-CALLEE.

      ******************************************************************
      * Date service: WS-DATE-REQ, WS-DATE-1, WS-DATE-2 set by caller;
      * DTVALD reports through DTA-DT-VALID, not the return code
       8600-CALL-DATES.
           INITIALIZE WS-DTE-CA
           MOVE WS-DATE-REQ            TO DTA-REQUEST-ID
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE WS-DATE-1              TO DTA-DT-DATE-1
           MOVE WS-DATE-2              TO DTA-DT-DATE-2
           CALL WS-DTE-PGM USING WS-DTE-CA
           MOVE DTA-HEADER             TO SVC-HEADER
           IF WS-DATE-REQ NOT = 'DTVALD  '
              PERFORM 8900-CHECK-CALLEE
           END-IF.

      ******************************************************************
      * Propagate a callee RC >= 12 unchanged with its reason code
       8900-CHECK-CALLEE.
           IF SVC-RETURN-CODE >= 12
              MOVE SVC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SVC-REASON-CODE     TO CA-REASON-CODE
              MOVE SVC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Litigation data locked by another task - retry'
                                       TO CA-MESSAGE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
              STRING 'CLMLITIG Db2 error SQLCODE ' WS-SQLCODE-DISP
                     ' in ' WS-ERR-PARA DELIMITED BY SIZE
                     INTO CA-MESSAGE
              INITIALIZE WS-ERR-CA
              MOVE 'ERRLOG  '          TO ERA-REQUEST-ID
              MOVE CA-USER-ID          TO ERA-USER-ID
              MOVE WS-PROGRAM-ID       TO ERA-CALLER-PGM
              MOVE WS-PROGRAM-ID       TO ERA-ER-PROGRAM
              MOVE WS-ERR-PARA         TO ERA-ER-PARAGRAPH
              MOVE SQLCODE             TO ERA-ER-SQLCODE
              MOVE CA-MESSAGE          TO ERA-ER-TEXT
              MOVE CA-LT-CLAIM-ID      TO WS-ID-DISP
              MOVE WS-ID-DISP          TO ERA-ER-KEY
              CALL WS-ERR-PGM USING WS-ERR-CA
           END-IF
           PERFORM 9000-RETURN.
