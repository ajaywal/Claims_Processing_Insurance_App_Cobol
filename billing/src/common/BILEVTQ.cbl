       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILEVTQ.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILEVTQ                                          *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Integration event queue for billing. Producers   *
      *               add events (EVPUT); the drainer BILEVTDR claims  *
      *               the oldest NEW event for a target and direction  *
      *               (EVGET), then marks it DONE or FAILED. Failed    *
      *               events are retried until the fifth failure       *
      *               parks them in ERRR for a trouble ticket. Event   *
      *               types and payload layouts: docs/EVENTS.md.       *
      * REQUESTS    : EVPUT   add an event (status NEW)                *
      *               EVGET   claim next NEW event for TARGET +        *
      *                       DIRECTION (status PROC)                  *
      *               EVDONE  mark a claimed event DONE                *
      *               EVFAIL  record a failure: back to NEW, or ERRR   *
      *                       after 5 retries (RC 04 EVERRR)           *
      * CALLED BY   : BILGAPIN, BILEVTDR, delinquency, disbursement    *
      *               and billing-status services                      *
      * CALLS       : BILSEQGN, BILERRLG                               *
      * TABLES      : BIL_EVENT_QUEUE (C R U)                          *
      * RULES       : BR-BIL-EVQ-001 Only event types registered in    *
      *                              EVENTS.md are accepted            *
      *               BR-BIL-EVQ-002 Direction is I (inbound) or O     *
      *                              (outbound) and must match the     *
      *                              event type                        *
      *               BR-BIL-EVQ-003 Billing owns its queue: outbound  *
      *                              events come from BILL, inbound    *
      *                              events are addressed to BILL;     *
      *                              source and target differ          *
      *               BR-BIL-EVQ-004 Event key is mandatory and the    *
      *                              payload must reach the start of   *
      *                              the type's last field             *
      *               BR-BIL-EVQ-005 An identical event still NEW or   *
      *                              PROC is not queued twice (RC 04   *
      *                              EVDUPL returns the existing id)   *
      *               BR-BIL-EVQ-006 EVGET serves the oldest NEW event *
      *                              first (CREATE_TS, EVENT_ID)       *
      *               BR-BIL-EVQ-007 A claim flips NEW to PROC only if *
      *                              the row is still NEW; a lost race *
      *                              retries up to 5 times             *
      *               BR-BIL-EVQ-008 PROC rows untouched for 30        *
      *                              minutes are reclaimed as a failed *
      *                              attempt before the next claim     *
      *               BR-BIL-EVQ-009 Only PROC events can be marked    *
      *                              DONE or FAILED                    *
      *               BR-BIL-EVQ-010 Each failure adds 1 to            *
      *                              RETRY_COUNT; the fifth parks the  *
      *                              event in ERRR                     *
      *               BR-BIL-EVQ-011 Event keys come from sequence EVQ *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILEVTQ '.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'BILSEQGN'.

       01  WS-CONSTANTS.
           05 WS-OWN-SYSTEM            PIC X(4)  VALUE 'BILL'.
           05 WS-MAX-RETRY             PIC S9(4) COMP VALUE 5.
           05 WS-MAX-CLAIM-TRIES       PIC S9(4) COMP VALUE 5.

      * BR-BIL-EVQ-001 Registered event types (EVENTS.md):
      *   type, direction in the billing queue, start position of
      *   the payload's last field
       01  WS-EVTYPE-DATA.
           05 FILLER                   PIC X(12) VALUE 'POLISSUEI074'.
           05 FILLER                   PIC X(12) VALUE 'POLCHNG I025'.
           05 FILLER                   PIC X(12) VALUE 'POLCANC I021'.
           05 FILLER                   PIC X(12) VALUE 'CLMPAYMTI179'.
           05 FILLER                   PIC X(12) VALUE 'DEDBILL I023'.
           05 FILLER                   PIC X(12) VALUE 'CANCREQ O029'.
           05 FILLER                   PIC X(12) VALUE 'BILSTAT O029'.
           05 FILLER                   PIC X(12) VALUE 'DSBSENT O021'.
       01  WS-EVTYPE-TABLE REDEFINES WS-EVTYPE-DATA.
           05 WS-EVT-ENTRY OCCURS 8 TIMES.
              10 WS-EVT-TYPE           PIC X(8).
              10 WS-EVT-DIRECTION      PIC X.
              10 WS-EVT-LAST-FIELD     PIC 9(3).
       01  WS-EVTYPE-COUNT             PIC S9(4) COMP VALUE 8.

       01  WS-WORK.
           05 WS-TX                    PIC S9(4) COMP VALUE 0.
           05 WS-TYPE-IX               PIC S9(4) COMP VALUE 0.
           05 WS-TRAIL                 PIC S9(4) COMP VALUE 0.
           05 WS-PAYLOAD-LEN           PIC S9(4) COMP VALUE 0.
           05 WS-CLAIM-TRIES           PIC S9(4) COMP VALUE 0.
           05 WS-CLAIMED-FLAG          PIC X     VALUE 'N'.
              88 WS-CLAIMED                      VALUE 'Y'.
           05 WS-OLD-RETRY             PIC S9(4) COMP VALUE 0.
           05 WS-NEW-RETRY             PIC S9(4) COMP VALUE 0.
           05 WS-NEW-STATUS            PIC X(4).
           05 WS-ID-DISP               PIC 9(9).
           05 WS-LEN-DISP              PIC 9(4).
           05 WS-SQLCODE-DISP          PIC -9(9).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDEVQ END-EXEC.

       01  WS-SEQ-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY BILCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCEVT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'EVPUT   '
                 PERFORM 2000-PUT-EVENT
              WHEN 'EVGET   '
                 PERFORM 2100-GET-EVENT
              WHEN 'EVDONE  '
                 PERFORM 2200-EVENT-DONE
              WHEN 'EVFAIL  '
                 PERFORM 2300-EVENT-FAILED
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILEVTQ: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           IF CA-USER-ID = SPACES
              MOVE 'BATCH   '          TO CA-USER-ID
           END-IF.

      ******************************************************************
       2000-PUT-EVENT.
           MOVE 0                      TO CA-EV-EVENT-ID
           PERFORM 3000-VALIDATE-PUT
           PERFORM 4000-FIND-DUPLICATE
           IF CA-RC-OK
              PERFORM 8000-GET-EVENT-ID
              PERFORM 4100-INSERT-EVENT
           END-IF.

      ******************************************************************
       2100-GET-EVENT.
           PERFORM 3100-VALIDATE-GET
           PERFORM 4200-RECLAIM-STALE
           MOVE 'N'                    TO WS-CLAIMED-FLAG
      * BR-BIL-EVQ-007 Retry a lost claim race a bounded number of times
           PERFORM VARYING WS-CLAIM-TRIES FROM 1 BY 1
                   UNTIL WS-CLAIMED
                      OR WS-CLAIM-TRIES > WS-MAX-CLAIM-TRIES
              PERFORM 4300-SELECT-OLDEST
              PERFORM 4400-CLAIM-EVENT
           END-PERFORM
           IF NOT WS-CLAIMED
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'EVBUSY  '          TO CA-REASON-CODE
              MOVE 'Queue contention - no event claimed, retry later'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 5000-RETURN-EVENT.

      ******************************************************************
      * BR-BIL-EVQ-009 Only a claimed (PROC) event completes
       2200-EVENT-DONE.
           PERFORM 3200-VALIDATE-ID
           MOVE CA-EV-EVENT-ID         TO EVQ-EVENT-ID
           EXEC SQL
              UPDATE BILDB.BIL_EVENT_QUEUE
                 SET EVENT_STATUS = 'DONE',
                     ERROR_TEXT   = NULL,
                     PROCESSED_TS = CURRENT TIMESTAMP
               WHERE EVENT_ID     = :EVQ-EVENT-ID
                 AND EVENT_STATUS = 'PROC'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    PERFORM 4500-DIAGNOSE-STATE
                 END-IF
              WHEN 100
                 PERFORM 4500-DIAGNOSE-STATE
              WHEN OTHER
                 MOVE '2200-EVENT-DONE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-EVQ-010 Retry count and parking in ERRR
       2300-EVENT-FAILED.
           PERFORM 3200-VALIDATE-ID
           PERFORM 4600-READ-EVENT
           IF EVQ-EVENT-STATUS NOT = 'PROC'
              PERFORM 4500-DIAGNOSE-STATE
           END-IF
           MOVE EVQ-RETRY-COUNT        TO WS-OLD-RETRY
           COMPUTE WS-NEW-RETRY = WS-OLD-RETRY + 1
           IF WS-NEW-RETRY >= WS-MAX-RETRY
              MOVE 'ERRR'              TO WS-NEW-STATUS
           ELSE
              MOVE 'NEW '              TO WS-NEW-STATUS
           END-IF
           IF CA-EV-ERROR = SPACES
              MOVE 'No error text supplied by processor'
                                       TO CA-EV-ERROR
           END-IF
           MOVE CA-EV-ERROR            TO EVQ-ERROR-TEXT-TEXT
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-EV-ERROR)
                   TALLYING WS-TRAIL FOR LEADING SPACE
           COMPUTE EVQ-ERROR-TEXT-LEN = 80 - WS-TRAIL
           PERFORM 4700-UPDATE-FAILED
           IF WS-NEW-STATUS = 'ERRR'
              SET CA-RC-WARNING        TO TRUE
              MOVE 'EVERRR  '          TO CA-REASON-CODE
              MOVE CA-EV-EVENT-ID      TO WS-ID-DISP
              STRING 'Event ' WS-ID-DISP ' failed 5 times - parked'
                     ' in ERRR, raise trouble ticket'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       3000-VALIDATE-PUT.
           PERFORM 5100-FIND-EVENT-TYPE
      * BR-BIL-EVQ-001 Registered type
           IF WS-TYPE-IX = 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVTYPE  '          TO CA-REASON-CODE
              STRING 'Event type ' CA-EV-TYPE ' is not registered'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-EVQ-002 Direction valid and consistent with the type
           IF CA-EV-DIRECTION NOT = 'I' AND CA-EV-DIRECTION NOT = 'O'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVDIR   '          TO CA-REASON-CODE
              MOVE 'Event direction must be I or O'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-EV-DIRECTION NOT = WS-EVT-DIRECTION(WS-TYPE-IX)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVDIRMM '          TO CA-REASON-CODE
              STRING 'Event ' CA-EV-TYPE ' must have direction '
                     WS-EVT-DIRECTION(WS-TYPE-IX)
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-EVQ-003 Source / target and queue ownership
           IF CA-EV-SOURCE = SPACES OR CA-EV-TARGET = SPACES
           OR CA-EV-SOURCE = CA-EV-TARGET
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVSYSTEM'          TO CA-REASON-CODE
              MOVE 'Source and target systems are required and must'
                 & ' differ'           TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF (CA-EV-DIRECTION = 'O'
               AND CA-EV-SOURCE NOT = WS-OWN-SYSTEM)
           OR (CA-EV-DIRECTION = 'I'
               AND CA-EV-TARGET NOT = WS-OWN-SYSTEM)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVOWNER '          TO CA-REASON-CODE
              MOVE 'Outbound events must come from BILL, inbound'
                 & ' events must target BILL'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-EVQ-004 Key and payload extent
           IF CA-EV-KEY = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVKEY   '          TO CA-REASON-CODE
              MOVE 'Event key is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-EV-PAYLOAD)
                   TALLYING WS-TRAIL FOR LEADING SPACE
           COMPUTE WS-PAYLOAD-LEN = 1000 - WS-TRAIL
           IF WS-PAYLOAD-LEN < WS-EVT-LAST-FIELD(WS-TYPE-IX)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVPAYLEN'          TO CA-REASON-CODE
              MOVE WS-PAYLOAD-LEN      TO WS-LEN-DISP
              STRING 'Payload for ' CA-EV-TYPE ' is short ('
                     WS-LEN-DISP ' chars) - see EVENTS.md'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3100-VALIDATE-GET.
           IF CA-EV-TARGET = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVTARGET'          TO CA-REASON-CODE
              MOVE 'Target system is required for EVGET'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-EV-DIRECTION NOT = 'I' AND CA-EV-DIRECTION NOT = 'O'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVDIR   '          TO CA-REASON-CODE
              MOVE 'Event direction must be I or O'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-EV-TARGET           TO EVQ-TARGET-SYSTEM
           MOVE CA-EV-DIRECTION        TO EVQ-DIRECTION.

      ******************************************************************
       3200-VALIDATE-ID.
           IF CA-EV-EVENT-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVID    '          TO CA-REASON-CODE
              MOVE 'Event id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-EVQ-005 Suppress an identical pending event
       4000-FIND-DUPLICATE.
           MOVE CA-EV-TYPE             TO EVQ-EVENT-TYPE
           MOVE CA-EV-DIRECTION        TO EVQ-DIRECTION
           MOVE CA-EV-SOURCE           TO EVQ-SOURCE-SYSTEM
           MOVE CA-EV-TARGET           TO EVQ-TARGET-SYSTEM
           MOVE CA-EV-KEY              TO EVQ-EVENT-KEY
           MOVE CA-EV-PAYLOAD          TO EVQ-PAYLOAD-TEXT
           MOVE WS-PAYLOAD-LEN         TO EVQ-PAYLOAD-LEN
           EXEC SQL
              SELECT EVENT_ID
                INTO :EVQ-EVENT-ID
                FROM BILDB.BIL_EVENT_QUEUE
               WHERE EVENT_TYPE    = :EVQ-EVENT-TYPE
                 AND DIRECTION     = :EVQ-DIRECTION
                 AND TARGET_SYSTEM = :EVQ-TARGET-SYSTEM
                 AND EVENT_KEY     = :EVQ-EVENT-KEY
                 AND EVENT_STATUS IN ('NEW', 'PROC')
                 AND PAYLOAD       = :EVQ-PAYLOAD
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE EVQ-EVENT-ID     TO CA-EV-EVENT-ID
                                          WS-ID-DISP
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'EVDUPL  '       TO CA-REASON-CODE
                 STRING 'Identical event already queued as '
                        WS-ID-DISP DELIMITED BY SIZE
                        INTO CA-MESSAGE
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4000-FIND-DUPLICATE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4100-INSERT-EVENT.
           MOVE SQC-SQ-VALUE           TO EVQ-EVENT-ID
                                          CA-EV-EVENT-ID
           EXEC SQL
              INSERT INTO BILDB.BIL_EVENT_QUEUE
                 (EVENT_ID, EVENT_TYPE, DIRECTION, SOURCE_SYSTEM,
                  TARGET_SYSTEM, EVENT_KEY, PAYLOAD, EVENT_STATUS,
                  RETRY_COUNT, ERROR_TEXT, CREATE_TS, PROCESSED_TS)
              VALUES
                 (:EVQ-EVENT-ID, :EVQ-EVENT-TYPE, :EVQ-DIRECTION,
                  :EVQ-SOURCE-SYSTEM, :EVQ-TARGET-SYSTEM,
                  :EVQ-EVENT-KEY, :EVQ-PAYLOAD, 'NEW ',
                  0, NULL, CURRENT TIMESTAMP, NULL)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'EVDUPID '       TO CA-REASON-CODE
                 MOVE 'Event id already used - check sequence EVQ'
                                       TO CA-MESSAGE
              WHEN OTHER
                 MOVE '4100-INSERT-EVENT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-EVQ-008 Abandoned claims count as a failed attempt
       4200-RECLAIM-STALE.
           EXEC SQL
              UPDATE BILDB.BIL_EVENT_QUEUE
                 SET EVENT_STATUS = CASE
                        WHEN RETRY_COUNT + 1 >= :WS-MAX-RETRY
                        THEN 'ERRR' ELSE 'NEW ' END,
                     RETRY_COUNT  = RETRY_COUNT + 1,
                     ERROR_TEXT   = 'Claim abandoned - reclaimed after'
                                 || ' 30 minutes in PROC',
                     PROCESSED_TS = CURRENT TIMESTAMP
               WHERE EVENT_STATUS  = 'PROC'
                 AND TARGET_SYSTEM = :EVQ-TARGET-SYSTEM
                 AND DIRECTION     = :EVQ-DIRECTION
                 AND PROCESSED_TS  < CURRENT TIMESTAMP - 30 MINUTES
           END-EXEC
           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
              MOVE '4200-RECLAIM-STALE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-BIL-EVQ-006 Oldest NEW event first
       4300-SELECT-OLDEST.
           MOVE SPACES                 TO EVQ-PAYLOAD-TEXT
                                          EVQ-ERROR-TEXT-TEXT
           EXEC SQL
              SELECT EVENT_ID, EVENT_TYPE, SOURCE_SYSTEM,
                     EVENT_KEY, PAYLOAD, RETRY_COUNT, ERROR_TEXT
                INTO :EVQ-EVENT-ID, :EVQ-EVENT-TYPE,
                     :EVQ-SOURCE-SYSTEM, :EVQ-EVENT-KEY,
                     :EVQ-PAYLOAD, :EVQ-RETRY-COUNT,
                     :EVQ-ERROR-TEXT :EVQ-ERROR-TEXT-IND
                FROM BILDB.BIL_EVENT_QUEUE
               WHERE EVENT_STATUS  = 'NEW '
                 AND TARGET_SYSTEM = :EVQ-TARGET-SYSTEM
                 AND DIRECTION     = :EVQ-DIRECTION
               ORDER BY CREATE_TS, EVENT_ID
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EVQEMPTY'       TO CA-REASON-CODE
                 STRING 'No NEW events for ' CA-EV-TARGET
                        ' direction ' CA-EV-DIRECTION
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4300-SELECT-OLDEST' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-EVQ-007 Optimistic claim: only if still NEW
       4400-CLAIM-EVENT.
           EXEC SQL
              UPDATE BILDB.BIL_EVENT_QUEUE
                 SET EVENT_STATUS = 'PROC',
                     PROCESSED_TS = CURRENT TIMESTAMP
               WHERE EVENT_ID     = :EVQ-EVENT-ID
                 AND EVENT_STATUS = 'NEW '
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) > 0
                    MOVE 'Y'           TO WS-CLAIMED-FLAG
                 END-IF
              WHEN 100
                 CONTINUE
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Event queue locked - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4400-CLAIM-EVENT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Explain why a DONE/FAIL found no PROC row
       4500-DIAGNOSE-STATE.
           MOVE CA-EV-EVENT-ID         TO EVQ-EVENT-ID
                                          WS-ID-DISP
           EXEC SQL
              SELECT EVENT_STATUS
                INTO :EVQ-EVENT-STATUS
                FROM BILDB.BIL_EVENT_QUEUE
               WHERE EVENT_ID = :EVQ-EVENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'EVSTATE '       TO CA-REASON-CODE
                 STRING 'Event ' WS-ID-DISP ' is '
                        EVQ-EVENT-STATUS ' - expected PROC'
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EVNOTFD '       TO CA-REASON-CODE
                 STRING 'Event ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN OTHER
                 MOVE '4500-DIAGNOSE-STATE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       4600-READ-EVENT.
           MOVE CA-EV-EVENT-ID         TO EVQ-EVENT-ID
           EXEC SQL
              SELECT EVENT_STATUS, RETRY_COUNT
                INTO :EVQ-EVENT-STATUS, :EVQ-RETRY-COUNT
                FROM BILDB.BIL_EVENT_QUEUE
               WHERE EVENT_ID = :EVQ-EVENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 PERFORM 4500-DIAGNOSE-STATE
              WHEN OTHER
                 MOVE '4600-READ-EVENT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Retry count acts as the version: a concurrent change -> 16
       4700-UPDATE-FAILED.
           MOVE WS-NEW-STATUS          TO EVQ-EVENT-STATUS
           MOVE WS-NEW-RETRY           TO EVQ-RETRY-COUNT
           EXEC SQL
              UPDATE BILDB.BIL_EVENT_QUEUE
                 SET EVENT_STATUS = :EVQ-EVENT-STATUS,
                     RETRY_COUNT  = :EVQ-RETRY-COUNT,
                     ERROR_TEXT   = :EVQ-ERROR-TEXT,
                     PROCESSED_TS = CURRENT TIMESTAMP
               WHERE EVENT_ID     = :EVQ-EVENT-ID
                 AND EVENT_STATUS = 'PROC'
                 AND RETRY_COUNT  = :WS-OLD-RETRY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'STALEVER'    TO CA-REASON-CODE
                    MOVE 'Event changed by another process - retry'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'STALEVER'       TO CA-REASON-CODE
                 MOVE 'Event changed by another process - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4700-UPDATE-FAILED' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       5000-RETURN-EVENT.
           MOVE EVQ-EVENT-ID           TO CA-EV-EVENT-ID
           MOVE EVQ-EVENT-TYPE         TO CA-EV-TYPE
           MOVE EVQ-SOURCE-SYSTEM      TO CA-EV-SOURCE
           MOVE EVQ-EVENT-KEY          TO CA-EV-KEY
           MOVE EVQ-PAYLOAD-TEXT       TO CA-EV-PAYLOAD
           IF EVQ-ERROR-TEXT-IND < 0
              MOVE SPACES              TO CA-EV-ERROR
           ELSE
              MOVE EVQ-ERROR-TEXT-TEXT TO CA-EV-ERROR
           END-IF.

      ******************************************************************
       5100-FIND-EVENT-TYPE.
           MOVE 0                      TO WS-TYPE-IX
           PERFORM VARYING WS-TX FROM 1 BY 1
                   UNTIL WS-TX > WS-EVTYPE-COUNT OR WS-TYPE-IX > 0
              IF WS-EVT-TYPE(WS-TX) = CA-EV-TYPE
                 MOVE WS-TX            TO WS-TYPE-IX
              END-IF
           END-PERFORM.

      ******************************************************************
      * BR-BIL-EVQ-011 Event key from sequence EVQ
       8000-GET-EVENT-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'EVQ     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE > 4
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
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
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
           END-IF
           STRING 'BILEVTQ Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE CA-EV-EVENT-ID         TO WS-ID-DISP
           MOVE SPACES                 TO ERC-ER-KEY
           STRING CA-EV-TYPE ' ' WS-ID-DISP
                  DELIMITED BY SIZE INTO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
