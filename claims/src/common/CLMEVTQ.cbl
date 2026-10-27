       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMEVTQ.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMEVTQ                                          *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Integration event queue (CLM_EVENT_QUEUE) API    *
      *               per docs/EVENTS.md. Producers EVPUT outbound     *
      *               events for BillingCore (CLMPAYMT, DEDBILL); the  *
      *               drainer CLMEVTDR takes work with EVGET and       *
      *               finishes it with EVDONE or EVFAIL.               *
      * REQUESTS    : EVPUT   validate and add an event                *
      *               EVGET   claim the next NEW event for TARGET      *
      *                       (status PROC, 30 minute lease)           *
      *               EVDONE  mark a PROC event DONE                   *
      *               EVFAIL  count a retry, record the error; back to *
      *                       NEW, or ERRR after 5 retries             *
      * CALLED BY   : CLMFINTX, CLMRECOV, CLMEVTDR, CLMCHKWR           *
      * CALLS       : CLMSEQGN, CLMERRLG                               *
      * TABLES      : CLM_EVENT_QUEUE (C R U)                          *
      * RULES       : BR-CLM-EVQ-001 Only registered event types       *
      *               BR-CLM-EVQ-002 Direction, source and target come *
      *                              from the registry; supplied       *
      *                              values must agree with it         *
      *               BR-CLM-EVQ-003 Key is mandatory and must equal   *
      *                              the key field that leads the      *
      *                              payload                           *
      *               BR-CLM-EVQ-004 Payload at least the registered   *
      *                              layout length; amount fields are  *
      *                              13 digits plus a trailing sign    *
      *               BR-CLM-EVQ-005 Duplicates suppressed: one event  *
      *                              per key for CLMPAYMT / DSBSENT,   *
      *                              identical payload for DEDBILL     *
      *               BR-CLM-EVQ-006 BILSTAT coalesces: a newer status *
      *                              overwrites an unprocessed one     *
      *               BR-CLM-EVQ-007 FIFO per target by CREATE_TS,     *
      *                              EVENT_ID                          *
      *               BR-CLM-EVQ-008 Events of one type and key are    *
      *                              serialised: none is issued while  *
      *                              another holds a live lease        *
      *               BR-CLM-EVQ-009 Claiming is optimistic; a lost    *
      *                              race moves on to the next row     *
      *               BR-CLM-EVQ-010 PROC lease is 30 minutes; an      *
      *                              expired lease is re-issued and    *
      *                              counts as a retry                 *
      *               BR-CLM-EVQ-011 Only PROC events can be completed *
      *                              or failed                         *
      *               BR-CLM-EVQ-012 After 5 retries the event is      *
      *                              parked in ERRR and logged for an  *
      *                              operations activity               *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMEVTQ '.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.

       01  WS-CONSTANTS.
           05 WS-MAX-RETRIES           PIC S9(4) COMP VALUE 5.
           05 WS-MAX-CANDIDATES        PIC S9(4) COMP VALUE 20.

      * Event registry (docs/EVENTS.md). Per type: direction, source,
      * target, minimum payload length, key length at payload pos 1,
      * position of a 14-byte amount (0 = none), duplicate mode
      * (K = unique per key, P = unique per payload, C = coalesce).
      * System codes follow the suite: CLMS = ClaimsCore, BILL =
      * BillingCore.
       01  WS-EVENT-REGISTRY-VALUES.
           05 FILLER PIC X(26) VALUE 'CLMPAYMTOCLMSBILL18810033K'.
           05 FILLER PIC X(26) VALUE 'DEDBILL OCLMSBILL03612023P'.
           05 FILLER PIC X(26) VALUE 'BILSTAT IBILLCLMS03810015C'.
           05 FILLER PIC X(26) VALUE 'DSBSENT IBILLCLMS03010000K'.
       01  WS-EVENT-REGISTRY REDEFINES WS-EVENT-REGISTRY-VALUES.
           05 WS-REG-ENTRY OCCURS 4 TIMES INDEXED BY WS-REG-IX.
              10 WS-REG-TYPE           PIC X(8).
              10 WS-REG-DIRECTION      PIC X.
              10 WS-REG-SOURCE         PIC X(4).
              10 WS-REG-TARGET         PIC X(4).
              10 WS-REG-MIN-LEN        PIC 9(3).
              10 WS-REG-KEY-LEN        PIC 99.
              10 WS-REG-AMT-POS        PIC 9(3).
              10 WS-REG-DUP-MODE       PIC X.

       01  WS-WORK.
           05 WS-TRAIL                 PIC S9(4) COMP.
           05 WS-PAY-LEN               PIC S9(4) COMP.
           05 WS-KEY-LEN               PIC S9(4) COMP.
           05 WS-AMT-POS               PIC S9(4) COMP.
           05 WS-DUP-COUNT             PIC S9(9) COMP.
           05 WS-RETRY-INC             PIC S9(4) COMP.
           05 WS-CANDIDATES            PIC S9(4) COMP.
           05 WS-NEW-STATUS            PIC X(4).
           05 WS-OLD-STATUS            PIC X(4).
           05 WS-OLD-PROC-TS           PIC X(26).
           05 WS-DIR-FILTER            PIC X.
           05 WS-FOUND                 PIC X     VALUE 'N'.
              88 WS-EVENT-FOUND                  VALUE 'Y'.
           05 WS-CURSOR-OPEN           PIC X     VALUE 'N'.
              88 WS-CURSOR-IS-OPEN               VALUE 'Y'.
           05 WS-EOF                   PIC X     VALUE 'N'.
              88 WS-END-OF-CURSOR                VALUE 'Y'.
           05 WS-SAVE-SQLCODE          PIC S9(9) COMP.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ID-DISP               PIC 9(9).
           05 WS-ERR-KEY               PIC X(20).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDEVQ END-EXEC.

      * BR-CLM-EVQ-007 FIFO candidates for EVGET (expired leases too)
      * BR-CLM-EVQ-008 Skip a key while another event holds its lease
           EXEC SQL DECLARE EVQCUR CURSOR FOR
              SELECT E.EVENT_ID, E.EVENT_STATUS, E.PROCESSED_TS
                FROM CLMDB.CLM_EVENT_QUEUE E
               WHERE E.TARGET_SYSTEM = :EVQ-TARGET-SYSTEM
                 AND (E.DIRECTION = :WS-DIR-FILTER
                      OR :WS-DIR-FILTER = ' ')
                 AND (E.EVENT_STATUS = 'NEW'
                      OR (E.EVENT_STATUS = 'PROC'
                          AND E.PROCESSED_TS <
                              CURRENT TIMESTAMP - 30 MINUTES))
                 AND NOT EXISTS
                     (SELECT 1
                        FROM CLMDB.CLM_EVENT_QUEUE P
                       WHERE P.EVENT_TYPE   = E.EVENT_TYPE
                         AND P.EVENT_KEY    = E.EVENT_KEY
                         AND P.EVENT_ID    <> E.EVENT_ID
                         AND P.EVENT_STATUS = 'PROC'
                         AND P.PROCESSED_TS >=
                             CURRENT TIMESTAMP - 30 MINUTES)
               ORDER BY E.CREATE_TS, E.EVENT_ID
               FETCH FIRST 20 ROWS ONLY
               FOR FETCH ONLY
           END-EXEC.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQC-==.
           03 SQC-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQC-==.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCEVT.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'EVPUT   '
                 PERFORM 2100-PUT-EVENT
              WHEN 'EVGET   '
                 PERFORM 2200-GET-EVENT
              WHEN 'EVDONE  '
                 PERFORM 2300-DONE-EVENT
              WHEN 'EVFAIL  '
                 PERFORM 2400-FAIL-EVENT
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMEVTQ: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO WS-CURSOR-OPEN
           MOVE SPACES                 TO WS-ERR-KEY.

      ******************************************************************
       2100-PUT-EVENT.
           PERFORM 3100-VALIDATE-PUT
           PERFORM 4100-CHECK-DUPLICATE
           IF NOT CA-RC-WARNING
              PERFORM 4200-INSERT-EVENT
           END-IF.

      ******************************************************************
       2200-GET-EVENT.
           IF CA-EV-TARGET = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVTARGET'          TO CA-REASON-CODE
              MOVE 'Target system is required to get an event'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-EV-DIRECTION NOT = SPACE
           AND CA-EV-DIRECTION NOT = 'I'
           AND CA-EV-DIRECTION NOT = 'O'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVDIRECT'          TO CA-REASON-CODE
              MOVE 'Direction filter must be I, O or blank'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-EV-TARGET           TO EVQ-TARGET-SYSTEM
           MOVE CA-EV-DIRECTION        TO WS-DIR-FILTER
           MOVE CA-EV-TARGET           TO WS-ERR-KEY
           MOVE 'N'                    TO WS-FOUND WS-EOF
           MOVE 0                      TO WS-CANDIDATES
           PERFORM 4300-OPEN-CANDIDATES
           PERFORM 4310-FETCH-CANDIDATE
           PERFORM UNTIL WS-END-OF-CURSOR OR WS-EVENT-FOUND
              PERFORM 4400-CLAIM-EVENT
              IF NOT WS-EVENT-FOUND
                 PERFORM 4310-FETCH-CANDIDATE
              END-IF
           END-PERFORM
           PERFORM 4320-CLOSE-CANDIDATES
           IF WS-EVENT-FOUND
              PERFORM 4500-READ-EVENT
              PERFORM 5100-RETURN-EVENT
           ELSE
              IF WS-CANDIDATES = 0
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EVQEMPTY'       TO CA-REASON-CODE
                 STRING 'No events waiting for ' CA-EV-TARGET
                        DELIMITED BY SIZE INTO CA-MESSAGE
              ELSE
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'EVQBUSY '       TO CA-REASON-CODE
                 MOVE 'All waiting events taken by other drainers'
                                       TO CA-MESSAGE
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-EVQ-011 Only an event this drainer holds (PROC) is done
       2300-DONE-EVENT.
           PERFORM 3300-READ-FOR-UPDATE
           MOVE 'DONE'                 TO WS-NEW-STATUS
           EXEC SQL
              UPDATE CLMDB.CLM_EVENT_QUEUE
                 SET EVENT_STATUS = :WS-NEW-STATUS,
                     PROCESSED_TS = CURRENT TIMESTAMP
               WHERE EVENT_ID     = :EVQ-EVENT-ID
                 AND EVENT_STATUS = 'PROC'
           END-EXEC
           PERFORM 4900-CHECK-STATUS-UPDATE.

      ******************************************************************
      * BR-CLM-EVQ-012 Retry up to 5 times, then park in ERRR
       2400-FAIL-EVENT.
           PERFORM 3300-READ-FOR-UPDATE
           IF CA-EV-ERROR = SPACES
              MOVE 'Failure reason not supplied by drainer'
                                       TO CA-EV-ERROR
           END-IF
           ADD 1                       TO EVQ-RETRY-COUNT
           IF EVQ-RETRY-COUNT >= WS-MAX-RETRIES
              MOVE 'ERRR'              TO WS-NEW-STATUS
           ELSE
              MOVE 'NEW '              TO WS-NEW-STATUS
           END-IF
           MOVE CA-EV-ERROR            TO EVQ-ERROR-TEXT-TEXT
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-EV-ERROR)
                   TALLYING WS-TRAIL FOR LEADING SPACES
           COMPUTE EVQ-ERROR-TEXT-LEN = LENGTH OF CA-EV-ERROR
                                      - WS-TRAIL
           EXEC SQL
              UPDATE CLMDB.CLM_EVENT_QUEUE
                 SET EVENT_STATUS = :WS-NEW-STATUS,
                     RETRY_COUNT  = :EVQ-RETRY-COUNT,
                     ERROR_TEXT   = :EVQ-ERROR-TEXT,
                     PROCESSED_TS = CURRENT TIMESTAMP
               WHERE EVENT_ID     = :EVQ-EVENT-ID
                 AND EVENT_STATUS = 'PROC'
           END-EXEC
           PERFORM 4900-CHECK-STATUS-UPDATE
           IF WS-NEW-STATUS = 'ERRR'
              PERFORM 8100-LOG-PARKED-EVENT
              SET CA-RC-WARNING        TO TRUE
              MOVE 'EVPARKED'          TO CA-REASON-CODE
              MOVE 'Event parked in ERRR after 5 retries'
                                       TO CA-MESSAGE
           END-IF.

      ******************************************************************
       3100-VALIDATE-PUT.
      * BR-CLM-EVQ-001 Only event types registered in EVENTS.md
           SET WS-REG-IX               TO 1
           SEARCH WS-REG-ENTRY
              AT END
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'EVTYPE  '       TO CA-REASON-CODE
                 STRING 'Event type ' CA-EV-TYPE ' is not registered'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN WS-REG-TYPE(WS-REG-IX) = CA-EV-TYPE
                 CONTINUE
           END-SEARCH
      * BR-CLM-EVQ-002 Routing comes from the registry
           IF CA-EV-DIRECTION = SPACE
              MOVE WS-REG-DIRECTION(WS-REG-IX) TO CA-EV-DIRECTION
           END-IF
           IF CA-EV-SOURCE = SPACES
              MOVE WS-REG-SOURCE(WS-REG-IX) TO CA-EV-SOURCE
           END-IF
           IF CA-EV-TARGET = SPACES
              MOVE WS-REG-TARGET(WS-REG-IX) TO CA-EV-TARGET
           END-IF
           IF CA-EV-DIRECTION NOT = WS-REG-DIRECTION(WS-REG-IX)
           OR CA-EV-SOURCE    NOT = WS-REG-SOURCE(WS-REG-IX)
           OR CA-EV-TARGET    NOT = WS-REG-TARGET(WS-REG-IX)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVROUTE '          TO CA-REASON-CODE
              STRING 'Routing for ' CA-EV-TYPE ' must be '
                     WS-REG-DIRECTION(WS-REG-IX) ' '
                     WS-REG-SOURCE(WS-REG-IX) '->'
                     WS-REG-TARGET(WS-REG-IX)
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3200-VALIDATE-PAYLOAD.

      ******************************************************************
       3200-VALIDATE-PAYLOAD.
           IF CA-EV-KEY = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVKEY   '          TO CA-REASON-CODE
              MOVE 'Event key is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-EV-KEY              TO WS-ERR-KEY
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(CA-EV-PAYLOAD)
                   TALLYING WS-TRAIL FOR LEADING SPACES
           COMPUTE WS-PAY-LEN = LENGTH OF CA-EV-PAYLOAD - WS-TRAIL
      * BR-CLM-EVQ-004 Payload must cover the registered layout
           IF WS-PAY-LEN < WS-REG-MIN-LEN(WS-REG-IX)
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVPAYLEN'          TO CA-REASON-CODE
              STRING CA-EV-TYPE ' payload shorter than '
                     WS-REG-MIN-LEN(WS-REG-IX) ' bytes'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-EVQ-003 Key equals the identifier leading the payload
           MOVE WS-REG-KEY-LEN(WS-REG-IX) TO WS-KEY-LEN
           IF CA-EV-KEY(1:WS-KEY-LEN) NOT = CA-EV-PAYLOAD(1:WS-KEY-LEN)
           OR CA-EV-KEY(WS-KEY-LEN + 1:) NOT = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVKEYMIS'          TO CA-REASON-CODE
              STRING 'Event key does not match payload positions 1-'
                     WS-REG-KEY-LEN(WS-REG-IX)
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-REG-AMT-POS(WS-REG-IX) TO WS-AMT-POS
           IF WS-AMT-POS > 0
              IF CA-EV-PAYLOAD(WS-AMT-POS:13) IS NOT NUMERIC
              OR (CA-EV-PAYLOAD(WS-AMT-POS + 13:1) NOT = '+'
                  AND CA-EV-PAYLOAD(WS-AMT-POS + 13:1) NOT = '-')
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'EVAMOUNT'       TO CA-REASON-CODE
                 STRING CA-EV-TYPE ' amount must be 13 digits and a'
                        ' trailing sign' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * Read the event for EVDONE / EVFAIL and check it is in PROC
       3300-READ-FOR-UPDATE.
           IF CA-EV-EVENT-ID NOT > 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EVID    '          TO CA-REASON-CODE
              MOVE 'Event id is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-EV-EVENT-ID         TO EVQ-EVENT-ID
           MOVE CA-EV-EVENT-ID         TO WS-ID-DISP
           MOVE WS-ID-DISP             TO WS-ERR-KEY
           EXEC SQL
              SELECT EVENT_STATUS, RETRY_COUNT, EVENT_TYPE, EVENT_KEY
                INTO :EVQ-EVENT-STATUS, :EVQ-RETRY-COUNT,
                     :EVQ-EVENT-TYPE, :EVQ-EVENT-KEY
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_ID = :EVQ-EVENT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EVNOTFD '       TO CA-REASON-CODE
                 STRING 'Event ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '3300-READ-FOR-UPDATE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
      * BR-CLM-EVQ-011 Only PROC events can be completed or failed
           IF EVQ-EVENT-STATUS NOT = 'PROC'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EVSTATE '          TO CA-REASON-CODE
              STRING 'Event ' WS-ID-DISP ' is ' EVQ-EVENT-STATUS
                     ', not PROC' DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       4100-CHECK-DUPLICATE.
           MOVE CA-EV-TYPE             TO EVQ-EVENT-TYPE
           MOVE CA-EV-DIRECTION        TO EVQ-DIRECTION
           MOVE CA-EV-KEY              TO EVQ-EVENT-KEY
           MOVE WS-PAY-LEN             TO EVQ-PAYLOAD-LEN
           MOVE CA-EV-PAYLOAD          TO EVQ-PAYLOAD-TEXT
           EVALUATE WS-REG-DUP-MODE(WS-REG-IX)
      * BR-CLM-EVQ-005 One live event per key (a parked ERRR event
      * may be re-raised once operations have resolved the cause)
              WHEN 'K'
                 EXEC SQL
                    SELECT COUNT(*)
                      INTO :WS-DUP-COUNT
                      FROM CLMDB.CLM_EVENT_QUEUE
                     WHERE EVENT_TYPE   = :EVQ-EVENT-TYPE
                       AND DIRECTION    = :EVQ-DIRECTION
                       AND EVENT_KEY    = :EVQ-EVENT-KEY
                       AND EVENT_STATUS IN ('NEW', 'PROC', 'DONE')
                 END-EXEC
                 PERFORM 4110-REJECT-IF-DUPLICATE
              WHEN 'P'
                 EXEC SQL
                    SELECT COUNT(*)
                      INTO :WS-DUP-COUNT
                      FROM CLMDB.CLM_EVENT_QUEUE
                     WHERE EVENT_TYPE   = :EVQ-EVENT-TYPE
                       AND DIRECTION    = :EVQ-DIRECTION
                       AND EVENT_KEY    = :EVQ-EVENT-KEY
                       AND PAYLOAD      = :EVQ-PAYLOAD
                       AND EVENT_STATUS IN ('NEW', 'PROC', 'DONE')
                 END-EXEC
                 PERFORM 4110-REJECT-IF-DUPLICATE
              WHEN 'C'
                 PERFORM 4120-COALESCE-EVENT
           END-EVALUATE.

      ******************************************************************
       4110-REJECT-IF-DUPLICATE.
           IF SQLCODE NOT = 0
              MOVE '4100-CHECK-DUPLICATE' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DUP-COUNT > 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'EVDUPLIC'          TO CA-REASON-CODE
              STRING CA-EV-TYPE ' already queued for key ' CA-EV-KEY
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-EVQ-006 Only the latest status matters: overwrite the
      * payload of a still-NEW event for the same key, keeping its
      * place in the queue. RC 04 tells the caller nothing was added.
       4120-COALESCE-EVENT.
           EXEC SQL
              UPDATE CLMDB.CLM_EVENT_QUEUE
                 SET PAYLOAD      = :EVQ-PAYLOAD
               WHERE EVENT_TYPE   = :EVQ-EVENT-TYPE
                 AND DIRECTION    = :EVQ-DIRECTION
                 AND EVENT_KEY    = :EVQ-EVENT-KEY
                 AND EVENT_STATUS = 'NEW'
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'EVCOALSC'       TO CA-REASON-CODE
                 STRING CA-EV-TYPE ' merged into waiting event for '
                        CA-EV-KEY DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4120-COALESCE-EVENT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-INSERT-EVENT.
           PERFORM 8000-NEXT-EVENT-ID
           MOVE CA-EV-SOURCE           TO EVQ-SOURCE-SYSTEM
           MOVE CA-EV-TARGET           TO EVQ-TARGET-SYSTEM
           MOVE 'NEW '                 TO EVQ-EVENT-STATUS
           MOVE 0                      TO EVQ-RETRY-COUNT
           MOVE -1                     TO EVQ-ERROR-TEXT-IND
                                          EVQ-PROCESSED-TS-IND
           EXEC SQL
              INSERT INTO CLMDB.CLM_EVENT_QUEUE
                 (EVENT_ID, EVENT_TYPE, DIRECTION, SOURCE_SYSTEM,
                  TARGET_SYSTEM, EVENT_KEY, PAYLOAD, EVENT_STATUS,
                  RETRY_COUNT, ERROR_TEXT, CREATE_TS, PROCESSED_TS)
              VALUES
                 (:EVQ-EVENT-ID, :EVQ-EVENT-TYPE, :EVQ-DIRECTION,
                  :EVQ-SOURCE-SYSTEM, :EVQ-TARGET-SYSTEM,
                  :EVQ-EVENT-KEY, :EVQ-PAYLOAD, :EVQ-EVENT-STATUS,
                  :EVQ-RETRY-COUNT,
                  :EVQ-ERROR-TEXT :EVQ-ERROR-TEXT-IND,
                  CURRENT TIMESTAMP,
                  :EVQ-PROCESSED-TS :EVQ-PROCESSED-TS-IND)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE EVQ-EVENT-ID     TO CA-EV-EVENT-ID
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'EVDUPID '       TO CA-REASON-CODE
                 MOVE 'Event id already used - check EVQ sequence'
                                       TO CA-MESSAGE
              WHEN OTHER
                 MOVE '4200-INSERT-EVENT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-OPEN-CANDIDATES.
           EXEC SQL OPEN EVQCUR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-OPEN-CANDIDATES' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'Y'                    TO WS-CURSOR-OPEN.

      ******************************************************************
       4310-FETCH-CANDIDATE.
           EXEC SQL
              FETCH EVQCUR
               INTO :EVQ-EVENT-ID, :EVQ-EVENT-STATUS,
                    :EVQ-PROCESSED-TS :EVQ-PROCESSED-TS-IND
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 ADD 1                 TO WS-CANDIDATES
              WHEN 100
                 MOVE 'Y'              TO WS-EOF
              WHEN OTHER
                 MOVE '4310-FETCH-CANDIDATE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4320-CLOSE-CANDIDATES.
           IF WS-CURSOR-IS-OPEN
              MOVE 'N'                 TO WS-CURSOR-OPEN
              EXEC SQL CLOSE EVQCUR END-EXEC
           END-IF.

      ******************************************************************
      * BR-CLM-EVQ-009 Claim with an UPDATE that only succeeds if the
      * row is still in the state we fetched; zero rows = another
      * drainer won the race, so move on.
      * BR-CLM-EVQ-010 An expired lease is re-issued as a retry
       4400-CLAIM-EVENT.
           MOVE EVQ-EVENT-STATUS       TO WS-OLD-STATUS
           IF WS-OLD-STATUS = 'PROC'
              MOVE 1                   TO WS-RETRY-INC
              MOVE EVQ-PROCESSED-TS    TO WS-OLD-PROC-TS
              EXEC SQL
                 UPDATE CLMDB.CLM_EVENT_QUEUE
                    SET EVENT_STATUS = 'PROC',
                        RETRY_COUNT  = RETRY_COUNT + :WS-RETRY-INC,
                        PROCESSED_TS = CURRENT TIMESTAMP
                  WHERE EVENT_ID     = :EVQ-EVENT-ID
                    AND EVENT_STATUS = 'PROC'
                    AND PROCESSED_TS = :WS-OLD-PROC-TS
              END-EXEC
           ELSE
              MOVE 0                   TO WS-RETRY-INC
              EXEC SQL
                 UPDATE CLMDB.CLM_EVENT_QUEUE
                    SET EVENT_STATUS = 'PROC',
                        PROCESSED_TS = CURRENT TIMESTAMP
                  WHERE EVENT_ID     = :EVQ-EVENT-ID
                    AND EVENT_STATUS = 'NEW'
              END-EXEC
           END-IF
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 1
                 MOVE 'Y'              TO WS-FOUND
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 CONTINUE
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 PERFORM 4320-CLOSE-CANDIDATES
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Event queue locked - retry EVGET'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4400-CLAIM-EVENT' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           IF WS-EVENT-FOUND AND WS-RETRY-INC > 0
              PERFORM 4410-CHECK-LEASE-RETRIES
           END-IF.

      ******************************************************************
      * A reclaimed event that has now used up its retries is parked
      * rather than handed out again (poison-message protection).
       4410-CHECK-LEASE-RETRIES.
           EXEC SQL
              SELECT RETRY_COUNT
                INTO :EVQ-RETRY-COUNT
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_ID = :EVQ-EVENT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4410-CHECK-LEASE-RETRIES' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           IF EVQ-RETRY-COUNT >= WS-MAX-RETRIES
              MOVE 'Processing lease expired too often'
                                       TO EVQ-ERROR-TEXT-TEXT
              MOVE 34                  TO EVQ-ERROR-TEXT-LEN
              EXEC SQL
                 UPDATE CLMDB.CLM_EVENT_QUEUE
                    SET EVENT_STATUS = 'ERRR',
                        ERROR_TEXT   = :EVQ-ERROR-TEXT
                  WHERE EVENT_ID     = :EVQ-EVENT-ID
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4410-CHECK-LEASE-RETRIES' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
              MOVE EVQ-ERROR-TEXT-TEXT TO CA-EV-ERROR
              PERFORM 8100-LOG-PARKED-EVENT
              MOVE 'N'                 TO WS-FOUND
           END-IF.

      ******************************************************************
       4500-READ-EVENT.
           EXEC SQL
              SELECT EVENT_ID, EVENT_TYPE, DIRECTION, SOURCE_SYSTEM,
                     TARGET_SYSTEM, EVENT_KEY, PAYLOAD, EVENT_STATUS,
                     RETRY_COUNT, ERROR_TEXT
                INTO :EVQ-EVENT-ID, :EVQ-EVENT-TYPE, :EVQ-DIRECTION,
                     :EVQ-SOURCE-SYSTEM, :EVQ-TARGET-SYSTEM,
                     :EVQ-EVENT-KEY, :EVQ-PAYLOAD, :EVQ-EVENT-STATUS,
                     :EVQ-RETRY-COUNT,
                     :EVQ-ERROR-TEXT :EVQ-ERROR-TEXT-IND
                FROM CLMDB.CLM_EVENT_QUEUE
               WHERE EVENT_ID = :EVQ-EVENT-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4500-READ-EVENT'   TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-EVQ-011 / 012 status UPDATE must hit exactly the PROC
      * row read above; zero rows means it changed underneath us.
       4900-CHECK-STATUS-UPDATE.
           EVALUATE TRUE
              WHEN SQLCODE = 0 AND SQLERRD(3) = 1
                 CONTINUE
              WHEN SQLCODE = 0 OR SQLCODE = 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'EVCHANGD'       TO CA-REASON-CODE
                 STRING 'Event ' WS-ID-DISP
                        ' changed by another task - not updated'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN SQLCODE = -911 OR SQLCODE = -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Event row locked - retry' TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4900-CHECK-STATUS-UPDATE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       5100-RETURN-EVENT.
           MOVE EVQ-EVENT-ID           TO CA-EV-EVENT-ID
           MOVE EVQ-EVENT-TYPE         TO CA-EV-TYPE
           MOVE EVQ-DIRECTION          TO CA-EV-DIRECTION
           MOVE EVQ-SOURCE-SYSTEM      TO CA-EV-SOURCE
           MOVE EVQ-TARGET-SYSTEM      TO CA-EV-TARGET
           MOVE EVQ-EVENT-KEY          TO CA-EV-KEY
           MOVE SPACES                 TO CA-EV-PAYLOAD CA-EV-ERROR
           IF EVQ-PAYLOAD-LEN > 0
              MOVE EVQ-PAYLOAD-TEXT(1:EVQ-PAYLOAD-LEN)
                                       TO CA-EV-PAYLOAD
           END-IF
           IF EVQ-ERROR-TEXT-IND >= 0 AND EVQ-ERROR-TEXT-LEN > 0
              MOVE EVQ-ERROR-TEXT-TEXT(1:EVQ-ERROR-TEXT-LEN)
                                       TO CA-EV-ERROR
           END-IF
           IF EVQ-RETRY-COUNT > 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'EVRETRY '          TO CA-REASON-CODE
              MOVE EVQ-RETRY-COUNT     TO WS-ID-DISP
              STRING 'Event is a retry, attempt count ' WS-ID-DISP
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       8000-NEXT-EVENT-ID.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE 'EVQ     '             TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SQC-SQ-VALUE           TO EVQ-EVENT-ID.

      ******************************************************************
      * BR-CLM-EVQ-012 Parked events are written to the error log so
      * the operations dashboard / CLMEVTDR can raise an activity.
       8100-LOG-PARKED-EVENT.
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE '8100-LOG-PARKED-EVENT' TO ERC-ER-PARAGRAPH
           MOVE 0                      TO ERC-ER-SQLCODE
           MOVE EVQ-EVENT-ID           TO WS-ID-DISP
           STRING 'EVENT PARKED ERRR ' EVQ-EVENT-TYPE ' ID '
                  WS-ID-DISP ': ' CA-EV-ERROR
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE EVQ-EVENT-KEY          TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SAVE-SQLCODE
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           PERFORM 4320-CLOSE-CANDIDATES
           SET CA-RC-DB2-ERROR         TO TRUE
           IF WS-SAVE-SQLCODE = -911 OR WS-SAVE-SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
           END-IF
           STRING 'CLMEVTQ Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-SAVE-SQLCODE        TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
