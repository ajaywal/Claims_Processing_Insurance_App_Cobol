       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILHOLD0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILHOLD0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Account holds. A hold suspends one kind of       *
      *               automatic processing on an account (invoicing,   *
      *               delinquency, disbursement, payment distribution) *
      *               for a date range. Batch and online processes     *
      *               ask HLDCHK before acting on an account.          *
      * REQUESTS    : HLDADD   place a hold                            *
      *               HLDREL   release HOLD-ID (authority HOLDREL)     *
      *               HLDCHK   is HOLD-TYPE active on ACCOUNT-ID at    *
      *                        AS-OF-DATE                              *
      * CALLED BY   : BIL account screens, BILINVSV, BILDLQSV,         *
      *               BILDISB0, BILPAYRC, invoice / delinquency batch  *
      * CALLS       : BILDATES, BILAUTHZ, BILSEQGN, BILAUDIT, BILERRLG *
      * TABLES      : BIL_HOLD               (C R U)                   *
      *               BIL_ACCOUNT            (R)                       *
      *               BIL_CONTROL            (R)                       *
      * RULES       : BR-BIL-HLD-001 Hold type is INVC, DLNQ, DISB or  *
      *                              PYMT                              *
      *               BR-BIL-HLD-002 Dates must be valid calendar      *
      *                              dates; start defaults to the      *
      *                              business date                     *
      *               BR-BIL-HLD-003 No back-dated holds: start on or  *
      *                              after the business date           *
      *               BR-BIL-HLD-004 End date is open (null) or on or  *
      *                              after the start date              *
      *               BR-BIL-HLD-005 A reason is mandatory             *
      *               BR-BIL-HLD-006 Holds are placed only on an open  *
      *                              account                           *
      *               BR-BIL-HLD-007 One active hold per type per      *
      *                              account (RC 16)                   *
      *               BR-BIL-HLD-008 Release needs the HOLDREL         *
      *                              authority row for the user role   *
      *                              (BILAUTHZ, amount 0) - RC 20      *
      *               BR-BIL-HLD-009 Only an active hold can be        *
      *                              released; release ends it on the  *
      *                              business date                     *
      *               BR-BIL-HLD-010 A hold applies when ACTV, started *
      *                              and not ended at the as-of date   *
      *               BR-BIL-HLD-011 Placing and releasing are audited *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILHOLD0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'BILDATES'.
       01  WS-AUT-PGM                  PIC X(8)  VALUE 'BILAUTHZ'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-AUD-ENTITY            PIC X(8)  VALUE 'HOLD    '.
           05 WS-RELEASE-LIMIT         PIC X(8)  VALUE 'HOLDREL '.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-COUNT                 PIC S9(9) COMP.
           05 WS-NUM-DISP              PIC 9(9).
           05 WS-AS-OF-DATE            PIC X(10).
           05 WS-DATE-TO-CHECK         PIC X(10).
           05 WS-DATE-LABEL            PIC X(10).
           05 WS-OLD-END-DATE          PIC X(10).
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-HOLD-TYPE             PIC X(4).
              88 WS-HOLD-TYPE-VALID    VALUE 'INVC' 'DLNQ' 'DISB'
                                             'PYMT'.

       01  WS-TRIM.
           05 WS-TL-SRC                PIC X(100).
           05 WS-TL-LEN                PIC S9(4) COMP.

       01  WS-AUDIT-REQ.
           05 WS-AU-ACTION             PIC X(4).
           05 WS-AU-FIELD              PIC X(18).
           05 WS-AU-BEFORE             PIC X(100).
           05 WS-AU-AFTER              PIC X(100).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDHLD END-EXEC.
           EXEC SQL INCLUDE BILDACT END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.

       01  WS-DTE-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY BILCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       01  WS-AUT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ATA-==.
           03 ATA-BODY.
              COPY BILCAUT REPLACING LEADING ==CA-== BY ==ATA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCHLD.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'HLDADD  '
                 PERFORM 2000-HOLD-ADD
              WHEN 'HLDREL  '
                 PERFORM 2100-HOLD-RELEASE
              WHEN 'HLDCHK  '
                 PERFORM 2200-HOLD-CHECK
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILHOLD0: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          WS-ERR-KEY
           IF CA-BUS-DATE NOT = SPACES
              MOVE CA-BUS-DATE         TO WS-BUS-DATE
           ELSE
              MOVE 'SYSTEM  '          TO CTL-CONTROL-KEY
              EXEC SQL
                 SELECT BUSINESS_DATE
                   INTO :CTL-BUSINESS-DATE
                   FROM BILDB.BIL_CONTROL
                  WHERE CONTROL_KEY = :CTL-CONTROL-KEY
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE 'SYSTEM'         TO WS-ERR-KEY
                 MOVE '1000-INITIALISE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE CTL-BUSINESS-DATE   TO WS-BUS-DATE
           END-IF.

      ******************************************************************
      * HLDADD
      ******************************************************************
       2000-HOLD-ADD.
           PERFORM 3000-VALIDATE-HOLD-TYPE
           PERFORM 3100-VALIDATE-ACCOUNT
      * BR-BIL-HLD-005 Reason is mandatory
           IF CA-HL-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HLDRSN  '          TO CA-REASON-CODE
              MOVE 'A reason is required to place a hold'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3200-VALIDATE-DATES
           PERFORM 3300-CHECK-ACTIVE-HOLD

           MOVE 'HLD     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO HLD-HOLD-ID
           PERFORM 4100-INSERT-HOLD

           MOVE 'ADD '                 TO WS-AU-ACTION
           MOVE SPACES                 TO WS-AU-FIELD WS-AU-BEFORE
           STRING CA-HL-HOLD-TYPE ' ' CA-HL-START-DATE ' TO '
                  CA-HL-END-DATE ' ' CA-HL-REASON
                  DELIMITED BY SIZE INTO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT

           MOVE HLD-HOLD-ID            TO CA-HL-HOLD-ID WS-NUM-DISP
           MOVE 'Y'                    TO CA-HL-ACTIVE
           IF CA-HL-START-DATE > WS-BUS-DATE
              MOVE 'N'                 TO CA-HL-ACTIVE
           END-IF
           STRING CA-HL-HOLD-TYPE ' hold ' WS-NUM-DISP ' placed'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * HLDREL
      ******************************************************************
       2100-HOLD-RELEASE.
           PERFORM 4000-READ-HOLD
      * BR-BIL-HLD-009 Only an active hold can be released
           IF HLD-HOLD-STATUS NOT = 'ACTV'
              SET CA-RC-STATE          TO TRUE
              MOVE 'HLDNACTV'          TO CA-REASON-CODE
              STRING 'Hold ' WS-NUM-DISP ' is ' HLD-HOLD-STATUS
                     ', not active' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 8200-CHECK-RELEASE-AUTHORITY

      * The hold stops applying from the business date: an open end
      * date, or one later than today, is brought forward to today
           MOVE SPACES                 TO WS-OLD-END-DATE
           IF HLD-END-DATE-IND NOT < 0
              MOVE HLD-END-DATE        TO WS-OLD-END-DATE
           END-IF
           IF HLD-END-DATE-IND < 0 OR HLD-END-DATE > WS-BUS-DATE
              MOVE WS-BUS-DATE         TO HLD-END-DATE
              MOVE 0                   TO HLD-END-DATE-IND
           END-IF
           IF HLD-START-DATE > HLD-END-DATE
              MOVE HLD-START-DATE      TO HLD-END-DATE
           END-IF
           PERFORM 4200-RELEASE-HOLD

           MOVE 'UPD '                 TO WS-AU-ACTION
           MOVE 'HOLD_STATUS'          TO WS-AU-FIELD
           MOVE 'ACTV'                 TO WS-AU-BEFORE
           MOVE 'RLSD'                 TO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT
           IF HLD-END-DATE NOT = WS-OLD-END-DATE
              MOVE 'END_DATE'          TO WS-AU-FIELD
              MOVE WS-OLD-END-DATE     TO WS-AU-BEFORE
              MOVE HLD-END-DATE        TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF

           MOVE 'RLSD'                 TO HLD-HOLD-STATUS
           PERFORM 5100-MOVE-HOLD-TO-CA
           MOVE 'N'                    TO CA-HL-ACTIVE
           STRING 'Hold ' WS-NUM-DISP ' released'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * HLDCHK - not finding a hold is a normal answer (ACTIVE = N)
      ******************************************************************
       2200-HOLD-CHECK.
           PERFORM 3000-VALIDATE-HOLD-TYPE
           IF CA-HL-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACTKEY  '          TO CA-REASON-CODE
              MOVE 'Account id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-HL-AS-OF-DATE = SPACES
              MOVE WS-BUS-DATE         TO CA-HL-AS-OF-DATE
           ELSE
              MOVE CA-HL-AS-OF-DATE    TO WS-DATE-TO-CHECK
              MOVE 'AS-OF'             TO WS-DATE-LABEL
              PERFORM 8300-VALIDATE-DATE
           END-IF
           MOVE CA-HL-AS-OF-DATE       TO WS-AS-OF-DATE
           MOVE CA-HL-ACCOUNT-ID       TO HLD-ACCOUNT-ID WS-NUM-DISP
           MOVE CA-HL-HOLD-TYPE        TO HLD-HOLD-TYPE
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
      * BR-BIL-HLD-010 Applies when active, started and not ended
           EXEC SQL
              SELECT HOLD_ID, START_DATE, END_DATE, HOLD_REASON,
                     HOLD_STATUS
                INTO :HLD-HOLD-ID, :HLD-START-DATE,
                     :HLD-END-DATE :HLD-END-DATE-IND,
                     :HLD-HOLD-REASON, :HLD-HOLD-STATUS
                FROM BILDB.BIL_HOLD
               WHERE ACCOUNT_ID  = :HLD-ACCOUNT-ID
                 AND HOLD_TYPE   = :HLD-HOLD-TYPE
                 AND HOLD_STATUS = 'ACTV'
                 AND START_DATE <= :WS-AS-OF-DATE
                 AND (END_DATE IS NULL OR END_DATE >= :WS-AS-OF-DATE)
               ORDER BY START_DATE
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 PERFORM 5100-MOVE-HOLD-TO-CA
                 MOVE 'Y'              TO CA-HL-ACTIVE
                 STRING CA-HL-HOLD-TYPE ' hold active on '
                        WS-AS-OF-DATE DELIMITED BY SIZE
                        INTO CA-MESSAGE
              WHEN 100
                 MOVE 'N'              TO CA-HL-ACTIVE
                 MOVE 0                TO CA-HL-HOLD-ID
                 MOVE SPACES           TO CA-HL-START-DATE
                                          CA-HL-END-DATE
                                          CA-HL-REASON
                 STRING 'No ' CA-HL-HOLD-TYPE ' hold on '
                        WS-AS-OF-DATE DELIMITED BY SIZE
                        INTO CA-MESSAGE
              WHEN OTHER
                 MOVE '2200-HOLD-CHECK' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-HLD-001 Hold types
       3000-VALIDATE-HOLD-TYPE.
           MOVE CA-HL-HOLD-TYPE        TO WS-HOLD-TYPE
           IF NOT WS-HOLD-TYPE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HLDTYPE '          TO CA-REASON-CODE
              MOVE 'Hold type must be INVC, DLNQ, DISB or PYMT'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-HLD-006 Account must exist and be open
       3100-VALIDATE-ACCOUNT.
           IF CA-HL-ACCOUNT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'ACTKEY  '          TO CA-REASON-CODE
              MOVE 'Account id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-HL-ACCOUNT-ID       TO ACT-ACCOUNT-ID WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT ACCOUNT_STATUS
                INTO :ACT-ACCOUNT-STATUS
                FROM BILDB.BIL_ACCOUNT
               WHERE ACCOUNT_ID = :ACT-ACCOUNT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'ACTNOTFD'       TO CA-REASON-CODE
                 STRING 'Billing account ' WS-NUM-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3100-VALIDATE-ACCOUNT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF ACT-ACCOUNT-STATUS = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'ACTCLSD '          TO CA-REASON-CODE
              STRING 'Billing account ' WS-NUM-DISP ' is closed'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-HLD-002 / 003 / 004 Date range
       3200-VALIDATE-DATES.
           IF CA-HL-START-DATE = SPACES
              MOVE WS-BUS-DATE         TO CA-HL-START-DATE
           ELSE
              MOVE CA-HL-START-DATE    TO WS-DATE-TO-CHECK
              MOVE 'Start'             TO WS-DATE-LABEL
              PERFORM 8300-VALIDATE-DATE
           END-IF
           IF CA-HL-START-DATE < WS-BUS-DATE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HLDBACK '          TO CA-REASON-CODE
              STRING 'Hold cannot start before business date '
                     WS-BUS-DATE DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-HL-END-DATE NOT = SPACES
              MOVE CA-HL-END-DATE      TO WS-DATE-TO-CHECK
              MOVE 'End'               TO WS-DATE-LABEL
              PERFORM 8300-VALIDATE-DATE
              IF CA-HL-END-DATE < CA-HL-START-DATE
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'HLDDATES'       TO CA-REASON-CODE
                 MOVE 'Hold end date is before its start date'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-BIL-HLD-007 One active hold per type per account: an ACTV
      * hold that has not already ended blocks a new one
       3300-CHECK-ACTIVE-HOLD.
           MOVE CA-HL-ACCOUNT-ID       TO HLD-ACCOUNT-ID
           MOVE CA-HL-HOLD-TYPE        TO HLD-HOLD-TYPE
           EXEC SQL
              SELECT HOLD_ID
                INTO :HLD-HOLD-ID
                FROM BILDB.BIL_HOLD
               WHERE ACCOUNT_ID  = :HLD-ACCOUNT-ID
                 AND HOLD_TYPE   = :HLD-HOLD-TYPE
                 AND HOLD_STATUS = 'ACTV'
                 AND (END_DATE IS NULL OR END_DATE >= :WS-BUS-DATE)
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'HLDDUP  '       TO CA-REASON-CODE
                 MOVE HLD-HOLD-ID      TO WS-NUM-DISP
                 STRING 'Account already has active ' CA-HL-HOLD-TYPE
                        ' hold ' WS-NUM-DISP DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 MOVE HLD-HOLD-ID      TO CA-HL-HOLD-ID
                 PERFORM 9000-RETURN
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '3300-CHECK-ACTIVE-HOLD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4000-READ-HOLD.
           IF CA-HL-HOLD-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HLDKEY  '          TO CA-REASON-CODE
              MOVE 'Hold id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE DCLBIL-HOLD
           MOVE CA-HL-HOLD-ID          TO HLD-HOLD-ID WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT ACCOUNT_ID, HOLD_TYPE, START_DATE, END_DATE,
                     HOLD_REASON, HOLD_STATUS
                INTO :HLD-ACCOUNT-ID, :HLD-HOLD-TYPE,
                     :HLD-START-DATE,
                     :HLD-END-DATE :HLD-END-DATE-IND,
                     :HLD-HOLD-REASON, :HLD-HOLD-STATUS
                FROM BILDB.BIL_HOLD
               WHERE HOLD_ID = :HLD-HOLD-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'HLDNOTFD'       TO CA-REASON-CODE
                 STRING 'Hold ' WS-NUM-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-HOLD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-HOLD.
           MOVE CA-HL-ACCOUNT-ID       TO HLD-ACCOUNT-ID
           MOVE CA-HL-HOLD-TYPE        TO HLD-HOLD-TYPE
           MOVE CA-HL-START-DATE       TO HLD-START-DATE
           MOVE CA-HL-END-DATE         TO HLD-END-DATE
           MOVE 0                      TO HLD-END-DATE-IND
           IF CA-HL-END-DATE = SPACES
              MOVE -1                  TO HLD-END-DATE-IND
           END-IF
           MOVE CA-HL-REASON           TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO HLD-HOLD-REASON-LEN
           MOVE CA-HL-REASON           TO HLD-HOLD-REASON-TEXT
           MOVE CA-USER-ID             TO HLD-CREATE-USER
           EXEC SQL
              INSERT INTO BILDB.BIL_HOLD
                     (HOLD_ID, ACCOUNT_ID, HOLD_TYPE, START_DATE,
                      END_DATE, HOLD_REASON, HOLD_STATUS,
                      CREATE_USER, CREATE_TS)
              VALUES (:HLD-HOLD-ID, :HLD-ACCOUNT-ID,
                      :HLD-HOLD-TYPE, :HLD-START-DATE,
                      :HLD-END-DATE :HLD-END-DATE-IND,
                      :HLD-HOLD-REASON, 'ACTV',
                      :HLD-CREATE-USER, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'DUPKEY  '       TO CA-REASON-CODE
                 MOVE 'Hold id already used - check sequence HLD'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-INSERT-HOLD' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Status guard in the WHERE: a concurrent release loses cleanly
       4200-RELEASE-HOLD.
           EXEC SQL
              UPDATE BILDB.BIL_HOLD
                 SET HOLD_STATUS = 'RLSD',
                     END_DATE    = :HLD-END-DATE
               WHERE HOLD_ID     = :HLD-HOLD-ID
                 AND HOLD_STATUS = 'ACTV'
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-RELEASE-HOLD' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              STRING 'Hold ' WS-NUM-DISP
                     ' was changed by another user - refresh'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       5100-MOVE-HOLD-TO-CA.
           MOVE HLD-HOLD-ID            TO CA-HL-HOLD-ID
           MOVE HLD-ACCOUNT-ID         TO CA-HL-ACCOUNT-ID
           MOVE HLD-HOLD-TYPE          TO CA-HL-HOLD-TYPE
           MOVE HLD-START-DATE         TO CA-HL-START-DATE
           MOVE SPACES                 TO CA-HL-END-DATE
                                          CA-HL-REASON
           IF HLD-END-DATE-IND NOT < 0
              MOVE HLD-END-DATE        TO CA-HL-END-DATE
           END-IF
           IF HLD-HOLD-REASON-LEN > 0
              MOVE HLD-HOLD-REASON-TEXT(1:HLD-HOLD-REASON-LEN)
                                       TO CA-HL-REASON
           END-IF.

      ******************************************************************
       5900-TRAILING-LENGTH.
           MOVE 100                    TO WS-TL-LEN
           PERFORM UNTIL WS-TL-LEN = 0
                      OR WS-TL-SRC(WS-TL-LEN:1) NOT = SPACE
              SUBTRACT 1               FROM WS-TL-LEN
           END-PERFORM.

      ******************************************************************
       8000-NEXT-SEQUENCE.
           INITIALIZE WS-SEQ-CA
           MOVE WS-SEQ-NAME            TO SQA-SQ-NAME
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE WS-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF NOT SQA-RC-OK AND NOT SQA-RC-WARNING
              MOVE SQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQA-REASON-CODE     TO CA-REASON-CODE
              MOVE SQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-HLD-011 Audit keyed by hold id
       8100-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE WS-AUD-ENTITY          TO AUA-AU-ENTITY
           MOVE HLD-HOLD-ID            TO WS-NUM-DISP
           MOVE WS-NUM-DISP            TO AUA-AU-KEY
           MOVE WS-AU-ACTION           TO AUA-AU-ACTION
           MOVE WS-AU-FIELD            TO AUA-AU-FIELD
           MOVE WS-AU-BEFORE           TO AUA-AU-BEFORE
           MOVE WS-AU-AFTER            TO AUA-AU-AFTER
           MOVE WS-PROGRAM-ID          TO AUA-AU-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF NOT AUA-RC-OK AND NOT AUA-RC-WARNING
              MOVE AUA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AUA-REASON-CODE     TO CA-REASON-CODE
              MOVE AUA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO WS-AU-BEFORE WS-AU-AFTER.

      ******************************************************************
      * BR-BIL-HLD-008 Release authority: the user's role must hold a
      * HOLDREL row in BIL_AUTHORITY_LIMIT (amount 0 = presence check)
       8200-CHECK-RELEASE-AUTHORITY.
           INITIALIZE WS-AUT-CA
           MOVE 'AUTCHK  '             TO ATA-REQUEST-ID
           MOVE CA-USER-ID             TO ATA-USER-ID
           MOVE WS-BUS-DATE            TO ATA-BUS-DATE
           MOVE CA-TRACE-ID            TO ATA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ATA-CALLER-PGM
           MOVE CA-USER-ID             TO ATA-AT-USER
           MOVE WS-RELEASE-LIMIT       TO ATA-AT-LIMIT-TYPE
           MOVE 0                      TO ATA-AT-AMOUNT
           CALL WS-AUT-PGM USING WS-AUT-CA
           EVALUATE TRUE
              WHEN (ATA-RC-OK OR ATA-RC-WARNING)
                   AND ATA-AT-IS-ALLOWED
                 CONTINUE
              WHEN ATA-RC-OK OR ATA-RC-WARNING
                   OR ATA-RC-NOT-FOUND OR ATA-RC-AUTHORITY
                 SET CA-RC-AUTHORITY   TO TRUE
                 MOVE 'NOAUTH  '       TO CA-REASON-CODE
                 STRING 'User ' CA-USER-ID
                        ' has no authority to release holds'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE ATA-RETURN-CODE  TO CA-RETURN-CODE
                 MOVE ATA-REASON-CODE  TO CA-REASON-CODE
                 MOVE ATA-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-HLD-002 Calendar validity through BILDATES DTVALD
       8300-VALIDATE-DATE.
           INITIALIZE WS-DTE-CA
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE WS-BUS-DATE            TO DTA-BUS-DATE
           MOVE CA-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE WS-DATE-TO-CHECK       TO DTA-DT-DATE-1
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF NOT DTA-RC-OK AND NOT DTA-RC-VALIDATION
              MOVE DTA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTA-REASON-CODE     TO CA-REASON-CODE
              MOVE DTA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF DTA-RC-VALIDATION OR NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'HLDDATE '          TO CA-REASON-CODE
              STRING WS-DATE-LABEL DELIMITED BY SPACE
                     ' date ' WS-DATE-TO-CHECK
                     ' is not a valid yyyy-mm-dd date'
                     DELIMITED BY SIZE INTO CA-MESSAGE
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
              MOVE 'Hold data locked - retry' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           MOVE SPACES                 TO CA-MESSAGE
           STRING 'BILHOLD0 Db2 error SQLCODE ' WS-SQLCODE-DISP
                  ' in ' WS-ERR-PARA
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
