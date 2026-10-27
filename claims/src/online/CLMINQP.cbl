       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMINQP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMINQP                                          *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * TRANSACTION : CINQ                                             *
      * MAP         : CLMIQM in mapset CLMMSET                         *
      * COMMAREA    : CLMMNU01 navigation + CLMINQ01 inquiry state     *
      * FUNCTION    : Claim summary by claim number (CLMCLAIM CLMINQ): *
      *               header, owner, segment/severity, flags, totals   *
      *               and up to 10 exposures, 8 per page.              *
      *               PF7/PF8 page the exposures; PF4 opens the        *
      *               financials screen for the exposure under the     *
      *               cursor; PF10 lists the latest notes (CLMNOTE0    *
      *               NTELST) in the message area; PF3 menu.           *
      * CALLED BY   : terminal user (CINQ), XCTL from CLMMNUP, CLMFNLP *
      *               and CLMACTP                                      *
      * CALLS       : LINK CLMCLAIM (CLMINQ)  LINK CLMNOTE0 (NTELST)   *
      *               LINK CLMERRLG (ERRLOG)  XCTL CLMMNUP CLMFINP     *
      * TABLES      : none (all data access through the services)      *
      * RULES       : BR-CLM-SIQ-001 Claim number required, 6-12       *
      *                              letters/digits starting with a    *
      *                              letter                            *
      *               BR-CLM-SIQ-002 Exposures shown 8 per page; paging*
      *                              stops at first and last page      *
      *               BR-CLM-SIQ-003 PF4 needs the cursor on an        *
      *                              exposure row of an open exposure  *
      *               BR-CLM-SIQ-004 SIU referral, litigation,         *
      *                              duplicate suspicion and coverage  *
      *                              in question are alerted           *
      *               BR-CLM-SIQ-005 Notes: newest 5 shown; the        *
      *                              service filters confidential ones *
      *               BR-CLM-SIQ-006 Page/notes/financials act only on *
      *                              the claim on display; a changed   *
      *                              claim number needs ENTER first    *
      *               BR-CLM-SIQ-007 Financials are refused for a      *
      *                              closed claim                      *
      *               BR-CLM-SIQ-008 Service RC >= 90 backs out the    *
      *                              unit of work                      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMINQP'.
       01  WS-TRANID                   PIC X(4)  VALUE 'CINQ'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CLMMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'CLMIQM'.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CLMMNUP'.
       01  WS-FIN-PGM                  PIC X(8)  VALUE 'CLMFINP'.
       01  WS-CLM-PGM                  PIC X(8)  VALUE 'CLMCLAIM'.
       01  WS-NTE-PGM                  PIC X(8)  VALUE 'CLMNOTE0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-CONSTANTS.
           05 WS-PAGE-SIZE             PIC S9(4) COMP VALUE 8.
           05 WS-FIRST-EXP-LINE        PIC S9(4) COMP VALUE 9.
           05 WS-NOTES-SHOWN           PIC S9(4) COMP VALUE 5.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP VALUE 0.
           05 WS-RESP2                 PIC S9(8) COMP VALUE 0.
           05 WS-RESP-DISP             PIC 9(4).
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-USERID                PIC X(8).
           05 WS-TASKN                 PIC 9(8).
           05 WS-TRACE-ID              PIC X(16).
           05 WS-SEND-MODE             PIC X.
              88 WS-SEND-ERASE          VALUE 'E'.
              88 WS-SEND-DATAONLY       VALUE 'D'.
           05 WS-ERROR-FLAG            PIC X.
              88 WS-NO-ERROR            VALUE 'N'.
              88 WS-HAS-ERROR           VALUE 'Y'.
           05 WS-LINK-FLAG             PIC X.
              88 WS-LINK-OK             VALUE 'Y'.
              88 WS-LINK-FAILED         VALUE 'N'.
           05 WS-LINK-PGM              PIC X(8).
           05 WS-MSG                   PIC X(79).
           05 WS-SCRATCH               PIC X(79).
           05 WS-CLAIM-NUMBER          PIC X(12).
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-ROW                   PIC S9(4) COMP.
           05 WS-LAST-ROW              PIC S9(4) COMP.
           05 WS-LINE                  PIC S9(4) COMP.
           05 WS-PTR                   PIC S9(4) COMP.
           05 WS-CHAR-COUNT            PIC S9(4) COMP.
           05 WS-BLANK-COUNT           PIC S9(4) COMP.
           05 WS-CLAIM-OK              PIC X.
              88 WS-CLAIM-VALID         VALUE 'Y'.
           05 WS-FIRST-FLAG            PIC X     VALUE 'N'.
              88 WS-FIRST-SEND          VALUE 'Y'.
           05 WS-LOWER                 PIC X(26)
                 VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 WS-UPPER                 PIC X(26)
                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05 WS-NUM-EDIT              PIC Z(8)9.
           05 WS-SMALL-EDIT            PIC Z9.
           05 WS-SEV-EDIT              PIC ZZ9.
           05 WS-VER-EDIT              PIC ZZZZ9.
           05 WS-AMT-IN                PIC S9(11)V99 COMP-3.
           05 WS-AMT-EDIT              PIC ZZZ,ZZZ,ZZ9.99-.
           05 WS-AMT-OUT               PIC X(15).
           05 WS-RSV-OUT               PIC X(15).
           05 WS-PAID-OUT              PIC X(15).
           05 WS-RCV-OUT               PIC X(15).

      * One exposure row as displayed (data starts in column 2)
       01  WS-EXP-LINE.
           05 FILLER                   PIC X     VALUE SPACE.
           05 EL-SEQ                   PIC ZZ9.
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EL-TYPE                  PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EL-COVERAGE              PIC X(8).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EL-STATE                 PIC X(4).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EL-RESERVE               PIC X(15).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 EL-PAID                  PIC X(15).
           05 FILLER                   PIC X(16) VALUE SPACES.

      * One note line: timestamp (to the minute), author, subject
       01  WS-NOTE-LINE.
           05 FILLER                   PIC X     VALUE SPACE.
           05 NL-DATE                  PIC X(10).
           05 FILLER                   PIC X     VALUE SPACE.
           05 NL-TIME                  PIC X(5).
           05 FILLER                   PIC X     VALUE SPACE.
           05 NL-AUTHOR                PIC X(8).
           05 FILLER                   PIC X     VALUE SPACE.
           05 NL-SUBJECT               PIC X(52).

       01  WS-COMMAREA.
           03 WS-CA-NAV.
              COPY CLMMNU01.
           03 WS-CA-STATE.
              COPY CLMINQ01.

       01  WS-CLM-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CLA-==.
           03 CLA-BODY.
              COPY CLMCCLM REPLACING LEADING ==CA-== BY ==CLA-==.
       01  WS-NTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==NTA-==.
           03 NTA-BODY.
              COPY CLMCNTE REPLACING LEADING ==CA-== BY ==NTA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

           COPY CLMMSET.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(4000).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           MOVE LOW-VALUES             TO CLMIQMO
           EVALUATE TRUE
              WHEN EIBCALEN = 0
              WHEN CN-OWNER-PGM NOT = WS-PROGRAM-ID
                 PERFORM 2000-FIRST-ENTRY
              WHEN EIBAID = DFHPF3
                 PERFORM 2800-RETURN-TO-MENU
              WHEN EIBAID = DFHCLEAR
                 MOVE IQ-CLAIM-NUMBER  TO CN-CLAIM-NUMBER
                 PERFORM 2000-FIRST-ENTRY
              WHEN EIBAID = DFHENTER
                 PERFORM 6000-RECEIVE-MAP
                 PERFORM 2100-INQUIRE
              WHEN EIBAID = DFHPF7
              WHEN EIBAID = DFHPF8
                 PERFORM 2300-PAGE
              WHEN EIBAID = DFHPF4
                 PERFORM 2400-FINANCIALS
              WHEN EIBAID = DFHPF10
                 PERFORM 2500-SHOW-NOTES
              WHEN OTHER
                 MOVE 'Key not active on this screen' TO WS-MSG
                 PERFORM 6300-SEND-ERROR
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           INITIALIZE WS-COMMAREA
           EVALUATE TRUE
              WHEN EIBCALEN = LENGTH OF WS-COMMAREA
                 MOVE DFHCOMMAREA(1:EIBCALEN) TO WS-COMMAREA
              WHEN EIBCALEN >= LENGTH OF WS-CA-NAV
                 MOVE DFHCOMMAREA(1:LENGTH OF WS-CA-NAV)
                                       TO WS-CA-NAV
                 MOVE SPACES           TO CN-OWNER-PGM
           END-EVALUATE
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
           END-EXEC
           MOVE EIBTASKN               TO WS-TASKN
           STRING EIBTRNID EIBTRMID WS-TASKN DELIMITED BY SIZE
                  INTO WS-TRACE-ID
           MOVE WS-USERID              TO CN-USER-ID
           MOVE SPACES                 TO WS-MSG
           SET WS-NO-ERROR             TO TRUE.

      ******************************************************************
      * New conversation; inquire at once when a claim was handed on
       2000-FIRST-ENTRY.
           INITIALIZE WS-CA-STATE
           MOVE 'Y'                    TO WS-FIRST-FLAG
           IF CN-CLAIM-NUMBER = SPACES
              MOVE CN-NAV-MSG          TO IMSGO
              IF CN-NAV-MSG = SPACES
                 MOVE 'Enter a claim number and press ENTER' TO IMSGO
              END-IF
              MOVE -1                  TO ICLMNOL
              SET WS-SEND-ERASE        TO TRUE
              PERFORM 6100-SEND-MAP
           ELSE
              MOVE CN-CLAIM-NUMBER     TO ICLMNOI
              PERFORM 2100-INQUIRE
           END-IF
           MOVE SPACES                 TO CN-NAV-MSG.

      ******************************************************************
       2100-INQUIRE.
           MOVE ICLMNOI                TO WS-CLAIM-NUMBER
           INSPECT WS-CLAIM-NUMBER CONVERTING WS-LOWER TO WS-UPPER
           PERFORM 3000-CHECK-CLAIM-NUMBER
           IF WS-NO-ERROR
              PERFORM 8100-LINK-CLMINQ
           END-IF
           IF WS-NO-ERROR
              PERFORM 2150-SAVE-CLAIM
              PERFORM 2200-SHOW-CLAIM
              MOVE 1                   TO IQ-FIRST-ROW
              PERFORM 5100-FILL-EXPOSURES
              SET WS-SEND-ERASE        TO TRUE
              MOVE -1                  TO ICLMNOL
              PERFORM 6100-SEND-MAP
           ELSE
              MOVE 'N'                 TO IQ-LOADED
              MOVE WS-CLAIM-NUMBER     TO ICLMNOO
              PERFORM 6300-SEND-ERROR
           END-IF.

      ******************************************************************
       2150-SAVE-CLAIM.
           MOVE 'Y'                    TO IQ-LOADED
           MOVE CLA-CL-CLAIM-ID        TO IQ-CLAIM-ID
           MOVE CLA-CL-CLAIM-NUMBER    TO IQ-CLAIM-NUMBER
           MOVE CLA-CL-STATE           TO IQ-CLAIM-STATE
           MOVE CLA-CL-EXP-COUNT       TO IQ-EXP-COUNT
           IF IQ-EXP-COUNT > 10
              MOVE 10                  TO IQ-EXP-COUNT
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > IQ-EXP-COUNT
              MOVE CLA-CL-X-EXPOSURE-ID(WS-IX)
                                       TO IQ-X-EXPOSURE-ID(WS-IX)
              MOVE CLA-CL-X-SEQ(WS-IX) TO IQ-X-SEQ(WS-IX)
              MOVE CLA-CL-X-TYPE(WS-IX) TO IQ-X-TYPE(WS-IX)
              MOVE CLA-CL-X-COVERAGE(WS-IX) TO IQ-X-COVERAGE(WS-IX)
              MOVE CLA-CL-X-STATE(WS-IX) TO IQ-X-STATE(WS-IX)
              MOVE CLA-CL-X-OPEN-RESERVE(WS-IX)
                                       TO IQ-X-OPEN-RESERVE(WS-IX)
              MOVE CLA-CL-X-PAID(WS-IX) TO IQ-X-PAID(WS-IX)
           END-PERFORM.

      ******************************************************************
       2200-SHOW-CLAIM.
           MOVE CLA-CL-CLAIM-NUMBER    TO ICLMNOO
           MOVE CLA-CL-STATE           TO ISTATEO
           MOVE CLA-CL-GENAPP-POL-NUM  TO WS-NUM-EDIT
           MOVE WS-NUM-EDIT            TO IPOLNOO
           MOVE CLA-CL-POLICY-TYPE     TO IPTYPEO
           MOVE CLA-CL-VERSION-NO      TO WS-VER-EDIT
           MOVE WS-VER-EDIT            TO IVERSO
           MOVE CLA-CL-LOSS-TYPE       TO ILTYPEO
           MOVE CLA-CL-LOSS-DATE       TO ILDATEO
           MOVE CLA-CL-LOSS-TIME       TO ILTIMEO
           MOVE CLA-CL-LOSS-CAUSE      TO ICAUSEO
           MOVE CLA-CL-LOSS-DESC       TO IDESCO
           MOVE CLA-CL-ASSIGNED-USER   TO IOWNERO
           IF CLA-CL-ASSIGNED-USER = SPACES
              MOVE '(queue)'           TO IOWNERO
           END-IF
           MOVE CLA-CL-ASSIGNED-GROUP  TO IGROUPO
           MOVE CLA-CL-SEGMENT         TO ISEGO
           MOVE CLA-CL-SEVERITY        TO WS-SEV-EDIT
           MOVE WS-SEV-EDIT            TO ISEVO
           MOVE CLA-CL-REPORTED-DATE   TO IRPTDO
           PERFORM 5300-FORMAT-FLAGS
           PERFORM 5400-FORMAT-TOTALS
           IF CLA-RC-WARNING
              MOVE CLA-MESSAGE         TO IMSGO
           END-IF.

      ******************************************************************
      * BR-CLM-SIQ-002 8 exposures per page, bounded paging
       2300-PAGE.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3100-CHECK-SAME-CLAIM
           IF WS-NO-ERROR
              IF EIBAID = DFHPF8
                 IF IQ-FIRST-ROW + WS-PAGE-SIZE > IQ-EXP-COUNT
                    MOVE 'Already on the last page of exposures'
                                       TO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
                 ELSE
                    ADD WS-PAGE-SIZE   TO IQ-FIRST-ROW
                 END-IF
              ELSE
                 IF IQ-FIRST-ROW = 1
                    MOVE 'Already on the first page of exposures'
                                       TO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
                 ELSE
                    SUBTRACT WS-PAGE-SIZE FROM IQ-FIRST-ROW
                 END-IF
              END-IF
           END-IF
           IF WS-NO-ERROR
              PERFORM 5100-FILL-EXPOSURES
              MOVE -1                  TO ICLMNOL
              SET WS-SEND-DATAONLY     TO TRUE
              PERFORM 6100-SEND-MAP
           ELSE
              PERFORM 6300-SEND-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-SIQ-003 Cursor row selects the exposure; must be open
      * BR-CLM-SIQ-007 No financials on a closed claim
       2400-FINANCIALS.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3100-CHECK-SAME-CLAIM
           IF WS-NO-ERROR
              COMPUTE WS-LINE = EIBCPOSN / 80 + 1
              COMPUTE WS-ROW = IQ-FIRST-ROW + WS-LINE
                             - WS-FIRST-EXP-LINE
              EVALUATE TRUE
                 WHEN WS-LINE < WS-FIRST-EXP-LINE
                   OR WS-LINE >= WS-FIRST-EXP-LINE + WS-PAGE-SIZE
                   OR WS-ROW > IQ-EXP-COUNT
                    MOVE 'Place the cursor on an exposure row, then pr
      -                  'ess PF4'     TO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
                 WHEN IQ-CLAIM-STATE = 'CLSD'
                    MOVE 'Claim is closed - reopen it before posting f
      -                  'inancials'   TO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
                 WHEN IQ-X-STATE(WS-ROW) = 'CLSD'
                    MOVE IQ-X-SEQ(WS-ROW) TO WS-SEV-EDIT
                    STRING 'Exposure ' WS-SEV-EDIT
                           ' is closed - reopen it before posting'
                           DELIMITED BY SIZE INTO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
              END-EVALUATE
           END-IF
           IF WS-NO-ERROR
              MOVE IQ-CLAIM-NUMBER     TO CN-CLAIM-NUMBER
              MOVE IQ-X-SEQ(WS-ROW)    TO CN-EXPOSURE-SEQ
              MOVE SPACES              TO CN-NAV-MSG
              MOVE WS-FIN-PGM          TO WS-LINK-PGM
              PERFORM 2900-XCTL
           ELSE
              PERFORM 6300-SEND-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-SIQ-005 Newest 5 notes in the message area
       2500-SHOW-NOTES.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3100-CHECK-SAME-CLAIM
           IF WS-NO-ERROR
              PERFORM 8200-LINK-NTELST
           END-IF
           IF WS-NO-ERROR
              PERFORM VARYING WS-IX FROM 1 BY 1
                      UNTIL WS-IX > WS-NOTES-SHOWN
                 IF WS-IX > NTA-NT-ROW-COUNT
                    MOVE SPACES        TO INOTEO(WS-IX)
                 ELSE
                    MOVE NTA-NT-R-TS(WS-IX)(1:10) TO NL-DATE
                    MOVE NTA-NT-R-TS(WS-IX)(12:5) TO NL-TIME
                    INSPECT NL-TIME REPLACING ALL '.' BY ':'
                    MOVE NTA-NT-R-AUTHOR(WS-IX)   TO NL-AUTHOR
                    MOVE NTA-NT-R-SUBJECT(WS-IX)  TO NL-SUBJECT
                    MOVE WS-NOTE-LINE  TO INOTEO(WS-IX)
                 END-IF
              END-PERFORM
              EVALUATE TRUE
                 WHEN NTA-NT-ROW-COUNT = 0
                    MOVE 'No notes on this claim' TO IMSGO
                 WHEN NTA-NT-ROW-COUNT > WS-NOTES-SHOWN
                    MOVE NTA-NT-ROW-COUNT TO WS-SMALL-EDIT
                    STRING 'Latest 5 notes shown (' WS-SMALL-EDIT
                           ' or more on the claim)'
                           DELIMITED BY SIZE INTO IMSGO
                 WHEN OTHER
                    MOVE 'All notes on the claim shown' TO IMSGO
              END-EVALUATE
              MOVE -1                  TO ICLMNOL
              SET WS-SEND-DATAONLY     TO TRUE
              PERFORM 6100-SEND-MAP
           ELSE
              PERFORM 6300-SEND-ERROR
           END-IF.

      ******************************************************************
       2800-RETURN-TO-MENU.
           MOVE IQ-CLAIM-NUMBER        TO CN-CLAIM-NUMBER
           MOVE 0                      TO CN-EXPOSURE-SEQ
           MOVE SPACES                 TO CN-NAV-MSG
           MOVE WS-MENU-PGM            TO WS-LINK-PGM
           PERFORM 2900-XCTL.

      ******************************************************************
       2900-XCTL.
           MOVE WS-PROGRAM-ID          TO CN-FROM-PGM
           MOVE SPACES                 TO CN-OWNER-PGM
           EXEC CICS XCTL PROGRAM(WS-LINK-PGM)
                     COMMAREA(WS-CA-NAV)
                     LENGTH(LENGTH OF WS-CA-NAV)
                     RESP(WS-RESP)
           END-EXEC
           MOVE WS-RESP                TO WS-RESP-DISP
           STRING 'Transfer to ' WS-LINK-PGM ' failed, CICS RESP '
                  WS-RESP-DISP DELIMITED BY SIZE INTO WS-MSG
           PERFORM 6300-SEND-ERROR.

      ******************************************************************
      * BR-CLM-SIQ-001 Claim number required and well formed
       3000-CHECK-CLAIM-NUMBER.
           MOVE 'Y'                    TO WS-CLAIM-OK
           MOVE 0                      TO WS-CHAR-COUNT WS-BLANK-COUNT
           INSPECT WS-CLAIM-NUMBER TALLYING WS-CHAR-COUNT
                   FOR CHARACTERS BEFORE INITIAL SPACE
           INSPECT WS-CLAIM-NUMBER TALLYING WS-BLANK-COUNT
                   FOR ALL SPACES
           IF WS-CLAIM-NUMBER(1:1) < 'A' OR WS-CLAIM-NUMBER(1:1) > 'Z'
              OR WS-CHAR-COUNT < 6
              OR WS-CHAR-COUNT + WS-BLANK-COUNT NOT = 12
              MOVE 'N'                 TO WS-CLAIM-OK
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CHAR-COUNT OR NOT WS-CLAIM-VALID
              IF  (WS-CLAIM-NUMBER(WS-IX:1) < 'A'
                   OR WS-CLAIM-NUMBER(WS-IX:1) > 'Z')
              AND (WS-CLAIM-NUMBER(WS-IX:1) < '0'
                   OR WS-CLAIM-NUMBER(WS-IX:1) > '9')
                 MOVE 'N'              TO WS-CLAIM-OK
              END-IF
           END-PERFORM
           EVALUATE TRUE
              WHEN WS-CLAIM-NUMBER = SPACES
                 MOVE 'Claim number is required' TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
              WHEN NOT WS-CLAIM-VALID
                 MOVE 'Claim number must be 6-12 letters/digits startin
      -               'g with a letter' TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-SIQ-006 Act only on the claim on display
       3100-CHECK-SAME-CLAIM.
           MOVE ICLMNOI                TO WS-CLAIM-NUMBER
           INSPECT WS-CLAIM-NUMBER CONVERTING WS-LOWER TO WS-UPPER
           EVALUATE TRUE
              WHEN NOT IQ-CLAIM-LOADED
                 MOVE 'Enter a claim number and press ENTER first'
                                       TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
              WHEN WS-CLAIM-NUMBER NOT = IQ-CLAIM-NUMBER
                 MOVE 'Claim number changed - press ENTER to display i
      -               't first'        TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
           END-EVALUATE.

      ******************************************************************
      * Exposure rows for the page starting at IQ-FIRST-ROW
       5100-FILL-EXPOSURES.
           COMPUTE WS-LAST-ROW = IQ-FIRST-ROW + WS-PAGE-SIZE - 1
           IF WS-LAST-ROW > IQ-EXP-COUNT
              MOVE IQ-EXP-COUNT        TO WS-LAST-ROW
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-PAGE-SIZE
              COMPUTE WS-ROW = IQ-FIRST-ROW + WS-IX - 1
              IF WS-ROW > IQ-EXP-COUNT
                 MOVE SPACES           TO IEXPLO(WS-IX)
              ELSE
                 MOVE IQ-X-SEQ(WS-ROW) TO EL-SEQ
                 MOVE IQ-X-TYPE(WS-ROW) TO EL-TYPE
                 MOVE IQ-X-COVERAGE(WS-ROW) TO EL-COVERAGE
                 MOVE IQ-X-STATE(WS-ROW) TO EL-STATE
                 MOVE IQ-X-OPEN-RESERVE(WS-ROW) TO WS-AMT-IN
                 PERFORM 5200-FORMAT-AMOUNT
                 MOVE WS-AMT-OUT       TO EL-RESERVE
                 MOVE IQ-X-PAID(WS-ROW) TO WS-AMT-IN
                 PERFORM 5200-FORMAT-AMOUNT
                 MOVE WS-AMT-OUT       TO EL-PAID
                 MOVE WS-EXP-LINE      TO IEXPLO(WS-IX)
                 IF IQ-X-STATE(WS-ROW) = 'CLSD'
                    MOVE DFHBMDAR      TO IEXPLA(WS-IX)
                 ELSE
                    MOVE DFHBMASK      TO IEXPLA(WS-IX)
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES                 TO IPAGEO
           IF IQ-EXP-COUNT = 0
              MOVE ' No exposures on this claim' TO IPAGEO
           ELSE
              MOVE IQ-FIRST-ROW        TO WS-SMALL-EDIT
              MOVE 1                   TO WS-PTR
              STRING ' Exposures ' WS-SMALL-EDIT
                     DELIMITED BY SIZE INTO IPAGEO WITH POINTER WS-PTR
              MOVE WS-LAST-ROW         TO WS-SMALL-EDIT
              STRING '-' WS-SMALL-EDIT
                     DELIMITED BY SIZE INTO IPAGEO WITH POINTER WS-PTR
              MOVE IQ-EXP-COUNT        TO WS-SMALL-EDIT
              STRING ' of ' WS-SMALL-EDIT
                     '    Cursor on a row + PF4 for financials'
                     DELIMITED BY SIZE INTO IPAGEO WITH POINTER WS-PTR
           END-IF.

      ******************************************************************
      * Amount to a 15-char display; beyond 999,999,999.99 overflows
       5200-FORMAT-AMOUNT.
           IF WS-AMT-IN > 999999999.99 OR WS-AMT-IN < -999999999.99
              MOVE '  ***********  '   TO WS-AMT-OUT
           ELSE
              MOVE WS-AMT-IN           TO WS-AMT-EDIT
              MOVE WS-AMT-EDIT         TO WS-AMT-OUT
           END-IF.

      ******************************************************************
      * BR-CLM-SIQ-004 Alerts for SIU, litigation, duplicates, coverage
       5300-FORMAT-FLAGS.
           MOVE CLA-CL-SIU-SCORE       TO WS-SEV-EDIT
           MOVE SPACES                 TO IFLAGSO
           STRING 'CovQ:' CLA-CL-COVERAGE-IN-QSTN
                  ' Fault:' CLA-CL-FAULT-RATING
                  ' SIU:' WS-SEV-EDIT '/' CLA-CL-SIU-STATUS
                  ' Lit:' CLA-CL-LITIGATION-FLAG
                  ' TotLoss:' CLA-CL-TOTAL-LOSS-FLAG
                  ' Dup:' CLA-CL-DUP-SUSPECT-FLAG
                  ' Cat:' CLA-CL-CAT-CODE
                  DELIMITED BY SIZE INTO IFLAGSO
           MOVE SPACES                 TO WS-SCRATCH
           EVALUATE TRUE
              WHEN CLA-CL-SIU-STATUS NOT = 'NONE'
               AND CLA-CL-SIU-STATUS NOT = SPACES
                 STRING 'SIU status ' CLA-CL-SIU-STATUS
                        ' - no payment without SIU sign-off'
                        DELIMITED BY SIZE INTO WS-SCRATCH
              WHEN CLA-CL-LITIGATION-FLAG = 'Y'
                 MOVE 'Claim in litigation - refer to the litigation ha
      -               'ndler before settling'
                                       TO WS-SCRATCH
              WHEN CLA-CL-DUP-SUSPECT-FLAG = 'Y'
                 MOVE 'Suspected duplicate claim - confirm before any p
      -               'ayment'         TO WS-SCRATCH
              WHEN CLA-CL-COVERAGE-IN-QSTN = 'Y'
                 MOVE 'Coverage in question - coverage decision outsta
      -               'nding'          TO WS-SCRATCH
           END-EVALUATE
           IF WS-SCRATCH NOT = SPACES
              MOVE DFHBMASB            TO IFLAGSA
              MOVE WS-SCRATCH          TO IMSGO
              MOVE DFHBMASB            TO IMSGA
           END-IF.

      ******************************************************************
       5400-FORMAT-TOTALS.
           MOVE CLA-CL-TOTAL-RESERVES  TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO WS-RSV-OUT
           MOVE CLA-CL-TOTAL-PAID      TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO WS-PAID-OUT
           MOVE CLA-CL-TOTAL-RECOVERED TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO WS-RCV-OUT
           MOVE CLA-CL-TOTAL-INCURRED  TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE SPACES                 TO ITOTSO
           STRING 'Rsv' WS-RSV-OUT ' Paid' WS-PAID-OUT
                  ' Rcv' WS-RCV-OUT ' Inc' WS-AMT-OUT
                  DELIMITED BY SIZE INTO ITOTSO.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(CLMIQMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
              INSPECT ICLMNOI REPLACING ALL LOW-VALUES BY SPACES
           ELSE
              MOVE SPACES              TO ICLMNOI
           END-IF
           MOVE ICLMNOI                TO WS-CLAIM-NUMBER
           MOVE LOW-VALUES             TO CLMIQMO
           MOVE WS-CLAIM-NUMBER        TO ICLMNOI.

      ******************************************************************
       6100-SEND-MAP.
           MOVE WS-TODAY               TO IDATEO
           IF WS-SEND-ERASE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMIQMO) ERASE CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           ELSE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMIQMO) DATAONLY CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           END-IF.

      ******************************************************************
       6300-SEND-ERROR.
           MOVE WS-MSG                 TO IMSGO
           MOVE DFHBMASB               TO IMSGA
           MOVE -1                     TO ICLMNOL
           MOVE DFHUNIMD               TO ICLMNOA
           IF WS-FIRST-SEND
              SET WS-SEND-ERASE        TO TRUE
           ELSE
              SET WS-SEND-DATAONLY     TO TRUE
           END-IF
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       8100-LINK-CLMINQ.
           INITIALIZE WS-CLM-CA
           MOVE 'CLMINQ  '             TO CLA-REQUEST-ID
           MOVE WS-USERID              TO CLA-USER-ID
           MOVE WS-TRACE-ID            TO CLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CLA-CALLER-PGM
           MOVE WS-CLAIM-NUMBER        TO CLA-CL-CLAIM-NUMBER
           MOVE WS-CLM-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-CLM-PGM)
                     COMMAREA(WS-CLM-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN CLA-RC-OK
                 WHEN CLA-RC-WARNING
                    CONTINUE
                 WHEN CLA-RC-NOT-FOUND
                    STRING 'Claim ' WS-CLAIM-NUMBER ' not found'
                           DELIMITED BY SIZE INTO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
                 WHEN CLA-RETURN-CODE >= 90
                    MOVE CLA-MESSAGE   TO WS-MSG
                    PERFORM 8900-BACK-OUT
                 WHEN OTHER
                    STRING CLA-REASON-CODE ' ' CLA-MESSAGE
                           DELIMITED BY SIZE INTO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
              END-EVALUATE
           END-IF.

      ******************************************************************
       8200-LINK-NTELST.
           INITIALIZE WS-NTE-CA
           MOVE 'NTELST  '             TO NTA-REQUEST-ID
           MOVE WS-USERID              TO NTA-USER-ID
           MOVE WS-TRACE-ID            TO NTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO NTA-CALLER-PGM
           MOVE IQ-CLAIM-ID            TO NTA-NT-CLAIM-ID
           MOVE WS-NTE-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-NTE-PGM)
                     COMMAREA(WS-NTE-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN NTA-RC-OK
                 WHEN NTA-RC-WARNING
                    CONTINUE
                 WHEN NTA-RC-NOT-FOUND
                    MOVE 0             TO NTA-NT-ROW-COUNT
                 WHEN NTA-RETURN-CODE >= 90
                    MOVE NTA-MESSAGE   TO WS-MSG
                    PERFORM 8900-BACK-OUT
                 WHEN OTHER
                    STRING NTA-REASON-CODE ' ' NTA-MESSAGE
                           DELIMITED BY SIZE INTO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
              END-EVALUATE
           END-IF.

      ******************************************************************
       8800-CHECK-LINK.
           IF WS-RESP = DFHRESP(NORMAL)
              SET WS-LINK-OK           TO TRUE
           ELSE
              SET WS-LINK-FAILED       TO TRUE
              MOVE WS-RESP             TO WS-RESP-DISP
              MOVE SPACES              TO WS-MSG
              STRING 'Service ' WS-LINK-PGM
                     ' unavailable (CICS RESP ' WS-RESP-DISP
                     ') - try again later'
                     DELIMITED BY SIZE INTO WS-MSG
              PERFORM 8950-LOG-CICS-ERROR
              PERFORM 8900-BACK-OUT
           END-IF.

      ******************************************************************
      * BR-CLM-SIQ-008 Back out the task's unit of work on RC >= 90
       8900-BACK-OUT.
           SET WS-HAS-ERROR            TO TRUE
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           MOVE WS-MSG                 TO WS-SCRATCH
           MOVE SPACES                 TO WS-MSG
           STRING 'Backed out: ' WS-SCRATCH DELIMITED BY SIZE
                  INTO WS-MSG.

      ******************************************************************
       8950-LOG-CICS-ERROR.
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERA-REQUEST-ID
           MOVE WS-USERID              TO ERA-USER-ID
           MOVE WS-TRACE-ID            TO ERA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ERA-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERA-ER-PROGRAM
           MOVE '8800-CHECK-LINK'      TO ERA-ER-PARAGRAPH
           MOVE WS-RESP                TO ERA-ER-RESP
           MOVE WS-MSG                 TO ERA-ER-TEXT
           MOVE WS-LINK-PGM            TO ERA-ER-KEY
           MOVE EIBTRNID               TO ERA-ER-TRAN-ID
           EXEC CICS LINK PROGRAM(WS-ERR-PGM)
                     COMMAREA(WS-ERR-CA) LENGTH(4000)
                     RESP(WS-RESP2)
           END-EXEC.

      ******************************************************************
       9000-RETURN.
           MOVE WS-PROGRAM-ID          TO CN-OWNER-PGM
           EXEC CICS RETURN TRANSID(WS-TRANID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
