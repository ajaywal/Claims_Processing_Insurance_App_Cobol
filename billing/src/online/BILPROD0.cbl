       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILPROD0.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILPROD0                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Producer (broker) maintenance. A producer is the *
      *               billing view of a GenApp broker: PRODUCER-CODE   *
      *               holds the GenApp BROKERID. Carries the           *
      *               commission plan, tier, payment cycle and agency  *
      *               bill flag used by commission processing.         *
      * REQUESTS    : PRDADD   add producer                            *
      *               PRDINQ   inquire by PRODUCER-ID                  *
      *               PRDFND   find by PRODUCER-CODE (GenApp BROKERID) *
      *               PRDUPD   update tier, plan, cycle, flags, status *
      * CALLED BY   : BIL producer screens, BILEVTDR (POLISSUE for an  *
      *               unknown broker), commission batch                *
      * CALLS       : BILSEQGN, BILAUDIT, BILERRLG                     *
      * TABLES      : BIL_PRODUCER           (C R U)                   *
      *               BIL_COMMISSION_PLAN    (R)                       *
      *               BIL_AGENCY_STATEMENT   (R)                       *
      *               BIL_CONTROL            (R)                       *
      * RULES       : BR-BIL-PRD-001 Producer code is the GenApp       *
      *                              broker id: positive and unique    *
      *                              (duplicate RC 16)                 *
      *               BR-BIL-PRD-002 Producer name is required         *
      *               BR-BIL-PRD-003 Tier is STD, GOLD or PLAT         *
      *                              (default STD)                     *
      *               BR-BIL-PRD-004 Commission plan must exist and be *
      *                              active                            *
      *               BR-BIL-PRD-005 Pay cycle M monthly or Q          *
      *                              quarterly; agency bill Y or N     *
      *               BR-BIL-PRD-006 New producers start ACTV with a   *
      *                              zero payable balance              *
      *               BR-BIL-PRD-007 Producer code cannot be changed   *
      *               BR-BIL-PRD-008 Status moves ACTV <-> SUSP and    *
      *                              ACTV/SUSP -> TERM; TERM is final  *
      *               BR-BIL-PRD-009 Suspending or terminating a       *
      *                              producer with a payable balance   *
      *                              above zero is rejected (RC 24)    *
      *               BR-BIL-PRD-010 Agency bill cannot be switched    *
      *                              while agency statements are open  *
      *               BR-BIL-PRD-011 Payable balance is maintained by  *
      *                              commission processing only        *
      *               BR-BIL-PRD-012 Commission plan change and every  *
      *                              other field change is audited     *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILPROD0'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'BILAUDIT'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-CONSTANTS.
           05 WS-AUD-ENTITY            PIC X(8)  VALUE 'PRODUCER'.

       01  WS-WORK.
           05 WS-BUS-DATE              PIC X(10).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-ERR-KEY               PIC X(20).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-COUNT                 PIC S9(9) COMP.
           05 WS-NUM-DISP              PIC 9(9).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-OLD-UPDATE-TS         PIC X(26).
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-TIER                  PIC X(4).
              88 WS-TIER-VALID         VALUE 'STD ' 'GOLD' 'PLAT'.
           05 WS-PAY-CYCLE             PIC X(1).
              88 WS-PAY-CYCLE-VALID    VALUE 'M' 'Q'.
           05 WS-AGENCY-BILL           PIC X(1).
              88 WS-AGENCY-BILL-VALID  VALUE 'Y' 'N'.
           05 WS-NEW-STATUS            PIC X(4).
              88 WS-STATUS-VALID       VALUE 'ACTV' 'SUSP' 'TERM'.
              88 WS-STATUS-WITHDRAWN   VALUE 'SUSP' 'TERM'.

      * Row as read, for transition checks and field-level audit
       01  WS-BEFORE.
           05 WS-BF-NAME               PIC X(60).
           05 WS-BF-TIER               PIC X(4).
           05 WS-BF-PLAN               PIC X(8).
           05 WS-BF-PAY-CYCLE          PIC X(1).
           05 WS-BF-AGENCY-BILL        PIC X(1).
           05 WS-BF-STATUS             PIC X(4).

       01  WS-TRIM.
           05 WS-TL-SRC                PIC X(100).
           05 WS-TL-LEN                PIC S9(4) COMP.

       01  WS-AUDIT-REQ.
           05 WS-AU-ACTION             PIC X(4).
           05 WS-AU-FIELD              PIC X(18).
           05 WS-AU-BEFORE             PIC X(100).
           05 WS-AU-AFTER              PIC X(100).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDPRD END-EXEC.
           EXEC SQL INCLUDE BILDCPL END-EXEC.
           EXEC SQL INCLUDE BILDAGS END-EXEC.
           EXEC SQL INCLUDE BILDCTL END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.

       01  WS-AUD-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY BILCAUD REPLACING LEADING ==CA-== BY ==AUA-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCPRD.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'PRDADD  '
                 PERFORM 2000-PRODUCER-ADD
              WHEN 'PRDINQ  '
                 PERFORM 2100-PRODUCER-INQUIRE
              WHEN 'PRDFND  '
                 PERFORM 2200-PRODUCER-FIND
              WHEN 'PRDUPD  '
                 PERFORM 2300-PRODUCER-UPDATE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILPROD0: unknown request '
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
      * PRDADD
      ******************************************************************
       2000-PRODUCER-ADD.
      * BR-BIL-PRD-001 Producer code = GenApp BROKERID
           IF CA-PR-PRODUCER-CODE NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDCODE '          TO CA-REASON-CODE
              MOVE 'Producer code (GenApp broker id) is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3000-VALIDATE-NAME
           IF CA-PR-TIER = SPACES
              MOVE 'STD '              TO CA-PR-TIER
           END-IF
           PERFORM 3100-VALIDATE-TIER
           MOVE CA-PR-COMM-PLAN        TO CPL-PLAN-CODE
           PERFORM 3200-VALIDATE-COMM-PLAN
           IF CA-PR-PAY-CYCLE = SPACE
              MOVE 'M'                 TO CA-PR-PAY-CYCLE
           END-IF
           IF CA-PR-AGENCY-BILL = SPACE
              MOVE 'N'                 TO CA-PR-AGENCY-BILL
           END-IF
           PERFORM 3300-VALIDATE-CYCLE-AND-FLAG
           PERFORM 3400-CHECK-DUPLICATE-CODE

           MOVE 'PRD     '             TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO PRD-PRODUCER-ID
           PERFORM 4100-INSERT-PRODUCER

           MOVE 'ADD '                 TO WS-AU-ACTION
           MOVE SPACES                 TO WS-AU-FIELD WS-AU-BEFORE
           MOVE CA-PR-PRODUCER-CODE    TO WS-NUM-DISP
           STRING 'CODE ' WS-NUM-DISP ' ' CA-PR-TIER ' '
                  CA-PR-COMM-PLAN ' ' CA-PR-NAME
                  DELIMITED BY SIZE INTO WS-AU-AFTER
           PERFORM 8100-WRITE-AUDIT

      * BR-BIL-PRD-006 New producer state
           MOVE PRD-PRODUCER-ID        TO CA-PR-PRODUCER-ID
           MOVE 0                      TO CA-PR-PAYABLE-BALANCE
           MOVE 'ACTV'                 TO CA-PR-STATUS
           STRING 'Producer ' WS-NUM-DISP ' added'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * PRDINQ
      ******************************************************************
       2100-PRODUCER-INQUIRE.
           PERFORM 4000-READ-PRODUCER
           PERFORM 5100-MOVE-PRODUCER-TO-CA.

      ******************************************************************
      * PRDFND - GenApp broker id to producer
      ******************************************************************
       2200-PRODUCER-FIND.
           IF CA-PR-PRODUCER-CODE NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDCODE '          TO CA-REASON-CODE
              MOVE 'Producer code (GenApp broker id) is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PR-PRODUCER-CODE    TO PRD-PRODUCER-CODE WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT PRODUCER_ID
                INTO :PRD-PRODUCER-ID
                FROM BILDB.BIL_PRODUCER
               WHERE PRODUCER_CODE = :PRD-PRODUCER-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PRDNOTFD'       TO CA-REASON-CODE
                 STRING 'No producer for GenApp broker ' WS-NUM-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '2200-PRODUCER-FIND' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE PRD-PRODUCER-ID        TO CA-PR-PRODUCER-ID
           PERFORM 4000-READ-PRODUCER
           PERFORM 5100-MOVE-PRODUCER-TO-CA.

      ******************************************************************
      * PRDUPD - spaces in a field mean "leave unchanged"
      ******************************************************************
       2300-PRODUCER-UPDATE.
           PERFORM 4000-READ-PRODUCER
           PERFORM 5200-SNAPSHOT-BEFORE
           IF PRD-PRODUCER-STATUS = 'TERM'
              SET CA-RC-STATE          TO TRUE
              MOVE 'PRDTERM '          TO CA-REASON-CODE
              STRING 'Producer ' WS-ERR-KEY ' is terminated'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-PRD-007 The GenApp broker link is immutable
           IF CA-PR-PRODUCER-CODE NOT = 0
              AND CA-PR-PRODUCER-CODE NOT = PRD-PRODUCER-CODE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDCODCH'          TO CA-REASON-CODE
              MOVE 'Producer code cannot be changed' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF

           IF CA-PR-NAME NOT = SPACES
              PERFORM 3000-VALIDATE-NAME
              MOVE CA-PR-NAME          TO WS-TL-SRC
              PERFORM 5900-TRAILING-LENGTH
              MOVE WS-TL-LEN           TO PRD-PRODUCER-NAME-LEN
              MOVE CA-PR-NAME          TO PRD-PRODUCER-NAME-TEXT
           END-IF
           IF CA-PR-TIER NOT = SPACES
              PERFORM 3100-VALIDATE-TIER
              MOVE CA-PR-TIER          TO PRD-PRODUCER-TIER
           END-IF
           IF CA-PR-COMM-PLAN NOT = SPACES
              AND CA-PR-COMM-PLAN NOT = PRD-COMM-PLAN-CODE
              MOVE CA-PR-COMM-PLAN     TO CPL-PLAN-CODE
              PERFORM 3200-VALIDATE-COMM-PLAN
              MOVE CA-PR-COMM-PLAN     TO PRD-COMM-PLAN-CODE
           END-IF
           IF CA-PR-PAY-CYCLE = SPACE
              MOVE PRD-PAY-CYCLE       TO CA-PR-PAY-CYCLE
           END-IF
           IF CA-PR-AGENCY-BILL = SPACE
              MOVE PRD-AGENCY-BILL-FLAG TO CA-PR-AGENCY-BILL
           END-IF
           PERFORM 3300-VALIDATE-CYCLE-AND-FLAG
           MOVE CA-PR-PAY-CYCLE        TO PRD-PAY-CYCLE
           IF CA-PR-AGENCY-BILL NOT = PRD-AGENCY-BILL-FLAG
              PERFORM 3500-CHECK-OPEN-STATEMENTS
              MOVE CA-PR-AGENCY-BILL   TO PRD-AGENCY-BILL-FLAG
           END-IF
           IF CA-PR-STATUS NOT = SPACES
              AND CA-PR-STATUS NOT = PRD-PRODUCER-STATUS
              PERFORM 3600-VALIDATE-STATUS-CHANGE
              MOVE CA-PR-STATUS        TO PRD-PRODUCER-STATUS
           END-IF

           PERFORM 4200-UPDATE-PRODUCER
           PERFORM 2390-AUDIT-CHANGES
           PERFORM 5100-MOVE-PRODUCER-TO-CA
           STRING 'Producer ' WS-ERR-KEY ' updated'
                  DELIMITED BY SIZE INTO CA-MESSAGE.

      ******************************************************************
      * BR-BIL-PRD-012 One audit row per changed field
       2390-AUDIT-CHANGES.
           MOVE 'UPD '                 TO WS-AU-ACTION
           IF PRD-COMM-PLAN-CODE NOT = WS-BF-PLAN
              MOVE 'COMM_PLAN_CODE'    TO WS-AU-FIELD
              MOVE WS-BF-PLAN          TO WS-AU-BEFORE
              MOVE PRD-COMM-PLAN-CODE  TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF PRD-PRODUCER-NAME-TEXT NOT = WS-BF-NAME
              MOVE 'PRODUCER_NAME'     TO WS-AU-FIELD
              MOVE WS-BF-NAME          TO WS-AU-BEFORE
              MOVE PRD-PRODUCER-NAME-TEXT TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF PRD-PRODUCER-TIER NOT = WS-BF-TIER
              MOVE 'PRODUCER_TIER'     TO WS-AU-FIELD
              MOVE WS-BF-TIER          TO WS-AU-BEFORE
              MOVE PRD-PRODUCER-TIER   TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF PRD-PAY-CYCLE NOT = WS-BF-PAY-CYCLE
              MOVE 'PAY_CYCLE'         TO WS-AU-FIELD
              MOVE WS-BF-PAY-CYCLE     TO WS-AU-BEFORE
              MOVE PRD-PAY-CYCLE       TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF PRD-AGENCY-BILL-FLAG NOT = WS-BF-AGENCY-BILL
              MOVE 'AGENCY_BILL_FLAG'  TO WS-AU-FIELD
              MOVE WS-BF-AGENCY-BILL   TO WS-AU-BEFORE
              MOVE PRD-AGENCY-BILL-FLAG TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF
           IF PRD-PRODUCER-STATUS NOT = WS-BF-STATUS
              MOVE 'PRODUCER_STATUS'   TO WS-AU-FIELD
              MOVE WS-BF-STATUS        TO WS-AU-BEFORE
              MOVE PRD-PRODUCER-STATUS TO WS-AU-AFTER
              PERFORM 8100-WRITE-AUDIT
           END-IF.

      ******************************************************************
      * BR-BIL-PRD-002 Name is required
       3000-VALIDATE-NAME.
           IF CA-PR-NAME = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDNAME '          TO CA-REASON-CODE
              MOVE 'Producer name is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PRD-003 Tier
       3100-VALIDATE-TIER.
           MOVE CA-PR-TIER             TO WS-TIER
           IF NOT WS-TIER-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDTIER '          TO CA-REASON-CODE
              MOVE 'Producer tier must be STD, GOLD or PLAT'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PRD-004 Commission plan must exist and be active
       3200-VALIDATE-COMM-PLAN.
           IF CPL-PLAN-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CMPLAN  '          TO CA-REASON-CODE
              MOVE 'Commission plan is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           EXEC SQL
              SELECT ACTIVE_FLAG
                INTO :CPL-ACTIVE-FLAG
                FROM BILDB.BIL_COMMISSION_PLAN
               WHERE PLAN_CODE = :CPL-PLAN-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF CPL-ACTIVE-FLAG NOT = 'Y'
                    SET CA-RC-VALIDATION TO TRUE
                    MOVE 'CMPLINA '    TO CA-REASON-CODE
                    STRING 'Commission plan ' CPL-PLAN-CODE
                           ' is not active' DELIMITED BY SIZE
                           INTO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'CMPLNF  '       TO CA-REASON-CODE
                 STRING 'Commission plan ' CPL-PLAN-CODE
                        ' does not exist' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE CPL-PLAN-CODE    TO WS-ERR-KEY
                 MOVE '3200-VALIDATE-COMM-PLAN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-PRD-005 Pay cycle and agency bill flag
       3300-VALIDATE-CYCLE-AND-FLAG.
           MOVE CA-PR-PAY-CYCLE        TO WS-PAY-CYCLE
           IF NOT WS-PAY-CYCLE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAYCYCLE'          TO CA-REASON-CODE
              MOVE 'Pay cycle must be M (monthly) or Q (quarterly)'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-PR-AGENCY-BILL      TO WS-AGENCY-BILL
           IF NOT WS-AGENCY-BILL-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AGCYBILL'          TO CA-REASON-CODE
              MOVE 'Agency bill flag must be Y or N' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PRD-001 One producer per GenApp broker (index XPRD01)
       3400-CHECK-DUPLICATE-CODE.
           MOVE CA-PR-PRODUCER-CODE    TO PRD-PRODUCER-CODE WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT PRODUCER_ID
                INTO :PRD-PRODUCER-ID
                FROM BILDB.BIL_PRODUCER
               WHERE PRODUCER_CODE = :PRD-PRODUCER-CODE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 PERFORM 3450-REJECT-DUPLICATE
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '3400-CHECK-DUPLICATE-CODE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       3450-REJECT-DUPLICATE.
           SET CA-RC-CONFLICT          TO TRUE
           MOVE 'DUPPRDCD'             TO CA-REASON-CODE
           STRING 'GenApp broker ' WS-NUM-DISP
                  ' is already set up as a producer'
                  DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
      * BR-BIL-PRD-010 Open agency statements are settled on the basis
      * they were issued; the billing basis cannot change under them
       3500-CHECK-OPEN-STATEMENTS.
           MOVE PRD-PRODUCER-ID        TO AGS-PRODUCER-ID
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-COUNT
                FROM BILDB.BIL_AGENCY_STATEMENT
               WHERE PRODUCER_ID = :AGS-PRODUCER-ID
                 AND STMT_STATUS = 'OPEN'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '3500-CHECK-OPEN-STATEMENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-COUNT > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'AGSOPEN '          TO CA-REASON-CODE
              MOVE WS-COUNT            TO WS-NUM-DISP
              STRING 'Agency bill flag locked: ' WS-NUM-DISP
                     ' open agency statement(s)'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-PRD-008 Status transitions
      * BR-BIL-PRD-009 No withdrawal while commission is owed
       3600-VALIDATE-STATUS-CHANGE.
           MOVE CA-PR-STATUS           TO WS-NEW-STATUS
           IF NOT WS-STATUS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDSTAT '          TO CA-REASON-CODE
              MOVE 'Producer status must be ACTV, SUSP or TERM'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF WS-STATUS-WITHDRAWN AND PRD-PAYABLE-BALANCE > 0
              SET CA-RC-STATE          TO TRUE
              MOVE 'PRDPAYBL'          TO CA-REASON-CODE
              MOVE PRD-PAYABLE-BALANCE TO WS-AMT-DISP
              STRING 'Producer has ' WS-AMT-DISP
                     ' commission payable - settle first'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4000-READ-PRODUCER.
           IF CA-PR-PRODUCER-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PRDKEY  '          TO CA-REASON-CODE
              MOVE 'Producer id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           INITIALIZE DCLBIL-PRODUCER
           MOVE CA-PR-PRODUCER-ID      TO PRD-PRODUCER-ID WS-NUM-DISP
           MOVE WS-NUM-DISP            TO WS-ERR-KEY
           EXEC SQL
              SELECT PRODUCER_CODE, PRODUCER_NAME, PRODUCER_TIER,
                     COMM_PLAN_CODE, PAY_CYCLE, AGENCY_BILL_FLAG,
                     PAYABLE_BALANCE, PRODUCER_STATUS, UPDATE_TS
                INTO :PRD-PRODUCER-CODE, :PRD-PRODUCER-NAME,
                     :PRD-PRODUCER-TIER, :PRD-COMM-PLAN-CODE,
                     :PRD-PAY-CYCLE, :PRD-AGENCY-BILL-FLAG,
                     :PRD-PAYABLE-BALANCE, :PRD-PRODUCER-STATUS,
                     :PRD-UPDATE-TS
                FROM BILDB.BIL_PRODUCER
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'PRDNOTFD'       TO CA-REASON-CODE
                 STRING 'Producer ' WS-NUM-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-PRODUCER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-PRODUCER.
           MOVE CA-PR-PRODUCER-CODE    TO PRD-PRODUCER-CODE
           MOVE CA-PR-NAME             TO WS-TL-SRC
           PERFORM 5900-TRAILING-LENGTH
           MOVE WS-TL-LEN              TO PRD-PRODUCER-NAME-LEN
           MOVE CA-PR-NAME             TO PRD-PRODUCER-NAME-TEXT
           MOVE CA-PR-TIER             TO PRD-PRODUCER-TIER
           MOVE CA-PR-COMM-PLAN        TO PRD-COMM-PLAN-CODE
           MOVE CA-PR-PAY-CYCLE        TO PRD-PAY-CYCLE
           MOVE CA-PR-AGENCY-BILL      TO PRD-AGENCY-BILL-FLAG
           EXEC SQL
              INSERT INTO BILDB.BIL_PRODUCER
                     (PRODUCER_ID, PRODUCER_CODE, PRODUCER_NAME,
                      PRODUCER_TIER, COMM_PLAN_CODE, PAY_CYCLE,
                      AGENCY_BILL_FLAG, PAYABLE_BALANCE,
                      PRODUCER_STATUS, UPDATE_TS)
              VALUES (:PRD-PRODUCER-ID, :PRD-PRODUCER-CODE,
                      :PRD-PRODUCER-NAME, :PRD-PRODUCER-TIER,
                      :PRD-COMM-PLAN-CODE, :PRD-PAY-CYCLE,
                      :PRD-AGENCY-BILL-FLAG, 0, 'ACTV',
                      CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 PERFORM 3450-REJECT-DUPLICATE
              WHEN OTHER
                 MOVE '4100-INSERT-PRODUCER' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-PRD-011 PAYABLE_BALANCE is not in the SET list.
      * The table has no VERSION_NO: UPDATE_TS as read is the guard.
       4200-UPDATE-PRODUCER.
           MOVE PRD-UPDATE-TS          TO WS-OLD-UPDATE-TS
           EXEC SQL
              UPDATE BILDB.BIL_PRODUCER
                 SET PRODUCER_NAME    = :PRD-PRODUCER-NAME,
                     PRODUCER_TIER    = :PRD-PRODUCER-TIER,
                     COMM_PLAN_CODE   = :PRD-COMM-PLAN-CODE,
                     PAY_CYCLE        = :PRD-PAY-CYCLE,
                     AGENCY_BILL_FLAG = :PRD-AGENCY-BILL-FLAG,
                     PRODUCER_STATUS  = :PRD-PRODUCER-STATUS,
                     UPDATE_TS        = CURRENT TIMESTAMP
               WHERE PRODUCER_ID = :PRD-PRODUCER-ID
                 AND UPDATE_TS   = :WS-OLD-UPDATE-TS
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-UPDATE-PRODUCER' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF SQLERRD(3) = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'STALEVER'          TO CA-REASON-CODE
              MOVE 'Producer changed by another process - refresh'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       5100-MOVE-PRODUCER-TO-CA.
           MOVE PRD-PRODUCER-ID        TO CA-PR-PRODUCER-ID
           MOVE PRD-PRODUCER-CODE      TO CA-PR-PRODUCER-CODE
           MOVE SPACES                 TO CA-PR-NAME
           IF PRD-PRODUCER-NAME-LEN > 0
              MOVE PRD-PRODUCER-NAME-TEXT(1:PRD-PRODUCER-NAME-LEN)
                                       TO CA-PR-NAME
           END-IF
           MOVE PRD-PRODUCER-TIER      TO CA-PR-TIER
           MOVE PRD-COMM-PLAN-CODE     TO CA-PR-COMM-PLAN
           MOVE PRD-PAY-CYCLE          TO CA-PR-PAY-CYCLE
           MOVE PRD-AGENCY-BILL-FLAG   TO CA-PR-AGENCY-BILL
           MOVE PRD-PAYABLE-BALANCE    TO CA-PR-PAYABLE-BALANCE
           MOVE PRD-PRODUCER-STATUS    TO CA-PR-STATUS.

      ******************************************************************
       5200-SNAPSHOT-BEFORE.
           MOVE PRD-PRODUCER-NAME-TEXT TO WS-BF-NAME
           MOVE PRD-PRODUCER-TIER      TO WS-BF-TIER
           MOVE PRD-COMM-PLAN-CODE     TO WS-BF-PLAN
           MOVE PRD-PAY-CYCLE          TO WS-BF-PAY-CYCLE
           MOVE PRD-AGENCY-BILL-FLAG   TO WS-BF-AGENCY-BILL
           MOVE PRD-PRODUCER-STATUS    TO WS-BF-STATUS.

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
      * BR-BIL-PRD-012 Audit keyed by producer code (GenApp broker id)
       8100-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUDADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE WS-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE WS-AUD-ENTITY          TO AUA-AU-ENTITY
           MOVE PRD-PRODUCER-CODE      TO WS-NUM-DISP
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
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           IF SQLCODE = -911 OR SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
              MOVE 'Producer data locked - retry' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           MOVE SPACES                 TO CA-MESSAGE
           STRING 'BILPROD0 Db2 error SQLCODE ' WS-SQLCODE-DISP
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
