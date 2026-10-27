       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMACTP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMACTP                                          *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * TRANSACTION : CACT                                             *
      * MAP         : CLMAVM in mapset CLMMSET                         *
      * COMMAREA    : CLMMNU01 navigation + CLMACT01 activity list     *
      * FUNCTION    : "My activities" for the signed-on user           *
      *               (CLMACTV0 ACTLST, user from EXEC CICS ASSIGN).   *
      *               Select C to complete (ACTCMP), S to skip         *
      *               (ACTSKP), V to view the claim; ENTER processes   *
      *               the selections and refreshes the list. PF4 views *
      *               the claim of the V row or the cursor row (XCTL   *
      *               CLMINQP); PF7/PF8 page; PF5 refresh; PF3 menu.   *
      * CALLED BY   : terminal user (CACT), XCTL from CLMMNUP          *
      * CALLS       : LINK CLMACTV0 (ACTLST ACTCMP ACTSKP)             *
      *               LINK CLMERRLG (ERRLOG)  XCTL CLMMNUP CLMINQP     *
      * TABLES      : none (all data access through the services)      *
      * RULES       : BR-CLM-SAC-001 Selection is C, S or V            *
      *               BR-CLM-SAC-002 One V at a time and not mixed     *
      *                              with C / S                        *
      *               BR-CLM-SAC-003 View opens claim inquiry for the  *
      *                              V row, else the cursor row        *
      *               BR-CLM-SAC-004 Only OPEN activities can be       *
      *                              completed or skipped              *
      *               BR-CLM-SAC-005 Service RC >= 90 backs out every  *
      *                              selection of the request          *
      *               BR-CLM-SAC-006 Escalated rows are highlighted    *
      *               BR-CLM-SAC-007 Overdue = due date before today   *
      *               BR-CLM-SAC-008 ACTLST returns the 20 most urgent;*
      *                              a full list is flagged            *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMACTP'.
       01  WS-TRANID                   PIC X(4)  VALUE 'CACT'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CLMMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'CLMAVM'.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CLMMNUP'.
       01  WS-INQ-PGM                  PIC X(8)  VALUE 'CLMINQP'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-CONSTANTS.
           05 WS-PAGE-SIZE             PIC S9(4) COMP VALUE 12.
           05 WS-MAX-ROWS              PIC S9(4) COMP VALUE 20.
           05 WS-FIRST-ROW-LINE        PIC S9(4) COMP VALUE 5.

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
           05 WS-ABORT-FLAG            PIC X.
              88 WS-ABORTED             VALUE 'Y'.
           05 WS-LINK-PGM              PIC X(8).
           05 WS-MSG                   PIC X(79).
           05 WS-FIRST-FAIL            PIC X(79).
           05 WS-SCRATCH               PIC X(79).
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-ROW                   PIC S9(4) COMP.
           05 WS-LINE                  PIC S9(4) COMP.
           05 WS-PAGES                 PIC S9(4) COMP.
           05 WS-VIEW-ROW              PIC S9(4) COMP.
           05 WS-VIEW-COUNT            PIC S9(4) COMP.
           05 WS-UPDATE-COUNT          PIC S9(4) COMP.
           05 WS-DONE-COUNT            PIC S9(4) COMP.
           05 WS-SKIP-COUNT            PIC S9(4) COMP.
           05 WS-FAIL-COUNT            PIC S9(4) COMP.
           05 WS-OVERDUE-COUNT         PIC S9(4) COMP.
           05 WS-SEL                   PIC X.
           05 WS-COUNT-EDIT            PIC ZZ9.
           05 WS-DONE-EDIT             PIC Z9.
           05 WS-SKIP-EDIT             PIC Z9.
           05 WS-FAIL-EDIT             PIC Z9.
           05 WS-PAGE-EDIT             PIC Z9.
           05 WS-PAGES-EDIT            PIC Z9.

      * One activity row as displayed (data starts in column 4)
       01  WS-ACT-LINE.
           05 AL-CLAIM-NUMBER          PIC X(12).
           05 FILLER                   PIC X     VALUE SPACE.
           05 AL-SUBJECT               PIC X(36).
           05 FILLER                   PIC X     VALUE SPACE.
           05 AL-PRIORITY              PIC X.
           05 FILLER                   PIC X     VALUE SPACE.
           05 AL-DUE                   PIC X(10).
           05 FILLER                   PIC X     VALUE SPACE.
           05 AL-STATUS                PIC X(4).
           05 FILLER                   PIC X     VALUE SPACE.
           05 AL-FLAG                  PIC X(4).
           05 FILLER                   PIC X(4)  VALUE SPACES.

       01  WS-COMMAREA.
           03 WS-CA-NAV.
              COPY CLMMNU01.
           03 WS-CA-STATE.
              COPY CLMACT01.

       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERA-==.
           03 ERA-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERA-==.

           COPY CLMMSET.
      * Row view of map CLMAVM: the symbolic map names each row's
      * select and text fields separately (ASEL01/ALIN01 ...); this
      * table lays an OCCURS over them. 58 = 12-byte TIOA prefix +
      * ADATE 13 + AUSER 11 + ACOUNT 6 + AOVRD 6 + APAGE 10.
       01  WS-AVM-ROWS REDEFINES CLMAVMI.
           05 FILLER                   PIC X(58).
           05 AV-ROW OCCURS 12 TIMES.
              10 AV-SEL-L              PIC S9(4) COMP.
              10 AV-SEL-A              PIC X.
              10 AV-SEL-I              PIC X.
              10 AV-LIN-L              PIC S9(4) COMP.
              10 AV-LIN-A              PIC X.
              10 AV-LIN-I              PIC X(76).
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(4000).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           MOVE LOW-VALUES             TO CLMAVMO
           EVALUATE TRUE
              WHEN EIBCALEN = 0
              WHEN CN-OWNER-PGM NOT = WS-PROGRAM-ID
              WHEN EIBAID = DFHCLEAR
                 MOVE 1                TO AQ-PAGE
                 PERFORM 2000-LIST-ACTIVITIES
                 SET WS-SEND-ERASE     TO TRUE
                 PERFORM 2050-SHOW-LIST
              WHEN EIBAID = DFHPF3
                 PERFORM 2800-RETURN-TO-MENU
              WHEN EIBAID = DFHPF5
                 PERFORM 2000-LIST-ACTIVITIES
                 MOVE 'List refreshed' TO WS-MSG
                 SET WS-SEND-DATAONLY  TO TRUE
                 PERFORM 2050-SHOW-LIST
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-PROCESS-SELECTIONS
              WHEN EIBAID = DFHPF4
                 PERFORM 2400-VIEW-CLAIM
              WHEN EIBAID = DFHPF7
              WHEN EIBAID = DFHPF8
                 PERFORM 2300-PAGE
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
           MOVE SPACES                 TO WS-MSG WS-FIRST-FAIL
           MOVE 'N'                    TO WS-ABORT-FLAG
           SET WS-NO-ERROR             TO TRUE.

      ******************************************************************
      * Fetch the open activities of the signed-on user
       2000-LIST-ACTIVITIES.
           PERFORM 8100-LINK-ACTLST
           IF WS-NO-ERROR
              MOVE AVA-AV-ROW-COUNT    TO AQ-ROW-COUNT
              IF AQ-ROW-COUNT > WS-MAX-ROWS
                 MOVE WS-MAX-ROWS      TO AQ-ROW-COUNT
              END-IF
              PERFORM VARYING WS-ROW FROM 1 BY 1
                      UNTIL WS-ROW > AQ-ROW-COUNT
                 MOVE AVA-AV-R-ID(WS-ROW) TO AQ-R-ACTIVITY-ID(WS-ROW)
                 MOVE AVA-AV-R-CLAIM-NUMBER(WS-ROW)
                                       TO AQ-R-CLAIM-NUMBER(WS-ROW)
                 MOVE AVA-AV-R-SUBJECT(WS-ROW) TO AQ-R-SUBJECT(WS-ROW)
                 MOVE AVA-AV-R-PRIORITY(WS-ROW)
                                       TO AQ-R-PRIORITY(WS-ROW)
                 MOVE AVA-AV-R-DUE(WS-ROW) TO AQ-R-DUE(WS-ROW)
                 MOVE AVA-AV-R-STATUS(WS-ROW) TO AQ-R-STATUS(WS-ROW)
                 MOVE AVA-AV-R-ESCALATED(WS-ROW)
                                       TO AQ-R-ESCALATED(WS-ROW)
              END-PERFORM
           ELSE
              MOVE 0                   TO AQ-ROW-COUNT
           END-IF
           COMPUTE WS-PAGES = (AQ-ROW-COUNT + WS-PAGE-SIZE - 1)
                            / WS-PAGE-SIZE
           IF WS-PAGES = 0
              MOVE 1                   TO WS-PAGES
           END-IF
           IF AQ-PAGE < 1 OR AQ-PAGE > WS-PAGES
              MOVE 1                   TO AQ-PAGE
           END-IF.

      ******************************************************************
       2050-SHOW-LIST.
           PERFORM 5100-FILL-ROWS
           IF WS-MSG = SPACES
              IF AQ-ROW-COUNT = 0
                 MOVE 'No open activities - nothing to do'
                                       TO AMSGO
              ELSE
                 MOVE 'Select C, S or V and press ENTER' TO AMSGO
              END-IF
           ELSE
              MOVE WS-MSG              TO AMSGO
              IF WS-HAS-ERROR
                 MOVE DFHBMASB         TO AMSGA
              END-IF
           END-IF
           MOVE -1                     TO AV-SEL-L(1)
           PERFORM 6100-SEND-MAP.

      ******************************************************************
      * ENTER: validate all selections, then apply C / S row by row
       2100-PROCESS-SELECTIONS.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3000-VALIDATE-SELECTIONS
           EVALUATE TRUE
              WHEN WS-HAS-ERROR
                 PERFORM 6300-SEND-ERROR
              WHEN WS-VIEW-COUNT = 1
                 PERFORM 2450-XCTL-INQUIRY
              WHEN WS-UPDATE-COUNT = 0
                 MOVE 'No selections made' TO WS-MSG
                 SET WS-SEND-DATAONLY  TO TRUE
                 PERFORM 2050-SHOW-LIST
              WHEN OTHER
                 PERFORM 2200-APPLY-SELECTIONS
                 PERFORM 2250-REPORT-RESULT
           END-EVALUATE.

      ******************************************************************
       2200-APPLY-SELECTIONS.
           MOVE 0                      TO WS-DONE-COUNT WS-SKIP-COUNT
                                          WS-FAIL-COUNT
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-PAGE-SIZE OR WS-ABORTED
              COMPUTE WS-ROW = (AQ-PAGE - 1) * WS-PAGE-SIZE + WS-LINE
              MOVE AV-SEL-I(WS-LINE)   TO WS-SEL
              IF WS-SEL = 'C' OR WS-SEL = 'S'
                 PERFORM 8200-LINK-ACTUPD
              END-IF
           END-PERFORM.

      ******************************************************************
      * BR-CLM-SAC-005 An aborted request reports that nothing stuck
       2250-REPORT-RESULT.
           IF WS-ABORTED
              MOVE 0                   TO WS-DONE-COUNT WS-SKIP-COUNT
              MOVE WS-FIRST-FAIL       TO WS-MSG
              SET WS-HAS-ERROR         TO TRUE
           ELSE
              MOVE WS-DONE-COUNT       TO WS-DONE-EDIT
              MOVE WS-SKIP-COUNT       TO WS-SKIP-EDIT
              MOVE WS-FAIL-COUNT       TO WS-FAIL-EDIT
              STRING WS-DONE-EDIT ' completed, ' WS-SKIP-EDIT
                     ' skipped, ' WS-FAIL-EDIT ' failed'
                     DELIMITED BY SIZE INTO WS-MSG
              IF WS-FAIL-COUNT > 0
                 MOVE WS-MSG           TO WS-SCRATCH
                 MOVE SPACES           TO WS-MSG
                 STRING WS-SCRATCH(1:34) ' - ' WS-FIRST-FAIL
                        DELIMITED BY SIZE INTO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
              END-IF
           END-IF
           PERFORM 2000-LIST-ACTIVITIES
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 2050-SHOW-LIST.

      ******************************************************************
       2300-PAGE.
           COMPUTE WS-PAGES = (AQ-ROW-COUNT + WS-PAGE-SIZE - 1)
                            / WS-PAGE-SIZE
           EVALUATE TRUE
              WHEN EIBAID = DFHPF8 AND AQ-PAGE >= WS-PAGES
                 MOVE 'Already on the last page' TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
              WHEN EIBAID = DFHPF7 AND AQ-PAGE <= 1
                 MOVE 'Already on the first page' TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
              WHEN EIBAID = DFHPF8
                 ADD 1                 TO AQ-PAGE
              WHEN OTHER
                 SUBTRACT 1            FROM AQ-PAGE
           END-EVALUATE
           IF WS-HAS-ERROR
              PERFORM 6300-SEND-ERROR
           ELSE
              SET WS-SEND-DATAONLY     TO TRUE
              PERFORM 2050-SHOW-LIST
           END-IF.

      ******************************************************************
      * BR-CLM-SAC-003 V row first, otherwise the row under the cursor
       2400-VIEW-CLAIM.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3000-VALIDATE-SELECTIONS
           IF WS-NO-ERROR AND WS-VIEW-COUNT = 0
              COMPUTE WS-LINE = EIBCPOSN / 80 + 1
                              - WS-FIRST-ROW-LINE + 1
              COMPUTE WS-ROW = (AQ-PAGE - 1) * WS-PAGE-SIZE + WS-LINE
              IF WS-LINE < 1 OR WS-LINE > WS-PAGE-SIZE
                 OR WS-ROW > AQ-ROW-COUNT
                 MOVE 'Mark a row with V or put the cursor on it, then
      -               ' PF4'           TO WS-MSG
                 SET WS-HAS-ERROR      TO TRUE
              ELSE
                 MOVE WS-ROW           TO WS-VIEW-ROW
              END-IF
           END-IF
           IF WS-HAS-ERROR
              PERFORM 6300-SEND-ERROR
           ELSE
              PERFORM 2450-XCTL-INQUIRY
           END-IF.

       2450-XCTL-INQUIRY.
           MOVE AQ-R-CLAIM-NUMBER(WS-VIEW-ROW) TO CN-CLAIM-NUMBER
           MOVE 0                      TO CN-EXPOSURE-SEQ
           MOVE SPACES                 TO CN-NAV-MSG
           MOVE WS-INQ-PGM             TO WS-LINK-PGM
           PERFORM 2900-XCTL.

      ******************************************************************
       2800-RETURN-TO-MENU.
           MOVE SPACES                 TO CN-CLAIM-NUMBER CN-NAV-MSG
           MOVE 0                      TO CN-EXPOSURE-SEQ
           MOVE WS-MENU-PGM            TO WS-LINK-PGM
           PERFORM 2900-XCTL.

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
      * BR-CLM-SAC-001 Selection C, S or V
      * BR-CLM-SAC-002 One V only, never mixed with C / S
      * BR-CLM-SAC-004 Only OPEN activities can be completed / skipped
       3000-VALIDATE-SELECTIONS.
           MOVE 0                      TO WS-VIEW-COUNT WS-UPDATE-COUNT
                                          WS-VIEW-ROW
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-PAGE-SIZE
              COMPUTE WS-ROW = (AQ-PAGE - 1) * WS-PAGE-SIZE + WS-LINE
              MOVE AV-SEL-I(WS-LINE)   TO WS-SEL
              EVALUATE TRUE
                 WHEN WS-SEL = SPACE
                    CONTINUE
                 WHEN WS-ROW > AQ-ROW-COUNT
                    MOVE 'Selection on an empty row' TO WS-SCRATCH
                    PERFORM 3900-FLAG-ROW
                 WHEN WS-SEL = 'V'
                    ADD 1              TO WS-VIEW-COUNT
                    MOVE WS-ROW        TO WS-VIEW-ROW
                 WHEN WS-SEL = 'C' OR WS-SEL = 'S'
                    IF AQ-R-STATUS(WS-ROW) = 'OPEN'
                       ADD 1           TO WS-UPDATE-COUNT
                    ELSE
                       STRING 'Activity is ' AQ-R-STATUS(WS-ROW)
                              ' - only OPEN activities can be changed'
                              DELIMITED BY SIZE INTO WS-SCRATCH
                       PERFORM 3900-FLAG-ROW
                    END-IF
                 WHEN OTHER
                    MOVE 'Select C (complete), S (skip) or V (view)'
                                       TO WS-SCRATCH
                    PERFORM 3900-FLAG-ROW
              END-EVALUATE
           END-PERFORM
           IF WS-NO-ERROR
              EVALUATE TRUE
                 WHEN WS-VIEW-COUNT > 1
                    MOVE 'Only one row can be viewed at a time'
                                       TO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
                 WHEN WS-VIEW-COUNT = 1 AND WS-UPDATE-COUNT > 0
                    MOVE 'View (V) cannot be combined with C or S'
                                       TO WS-MSG
                    SET WS-HAS-ERROR   TO TRUE
              END-EVALUATE
           END-IF.

       3900-FLAG-ROW.
           SET WS-HAS-ERROR            TO TRUE
           IF WS-MSG = SPACES
              MOVE WS-SCRATCH          TO WS-MSG
           END-IF
           MOVE SPACES                 TO WS-SCRATCH
           MOVE -1                     TO AV-SEL-L(WS-LINE)
           MOVE DFHUNIMD               TO AV-SEL-A(WS-LINE).

      ******************************************************************
      * BR-CLM-SAC-006 Escalated rows bright
      * BR-CLM-SAC-007 Overdue = due before today
      * BR-CLM-SAC-008 A full list (20) is flagged
       5100-FILL-ROWS.
           MOVE 0                      TO WS-OVERDUE-COUNT
           PERFORM VARYING WS-ROW FROM 1 BY 1
                   UNTIL WS-ROW > AQ-ROW-COUNT
              IF AQ-R-DUE(WS-ROW) < WS-TODAY
                 ADD 1                 TO WS-OVERDUE-COUNT
              END-IF
           END-PERFORM
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-PAGE-SIZE
              COMPUTE WS-ROW = (AQ-PAGE - 1) * WS-PAGE-SIZE + WS-LINE
              MOVE SPACE               TO AV-SEL-I(WS-LINE)
              IF WS-ROW > AQ-ROW-COUNT
                 MOVE SPACES           TO AV-LIN-I(WS-LINE)
                 MOVE DFHBMPRO         TO AV-SEL-A(WS-LINE)
              ELSE
                 MOVE AQ-R-CLAIM-NUMBER(WS-ROW) TO AL-CLAIM-NUMBER
                 MOVE AQ-R-SUBJECT(WS-ROW)      TO AL-SUBJECT
                 MOVE AQ-R-PRIORITY(WS-ROW)     TO AL-PRIORITY
                 MOVE AQ-R-DUE(WS-ROW)          TO AL-DUE
                 MOVE AQ-R-STATUS(WS-ROW)       TO AL-STATUS
                 EVALUATE TRUE
                    WHEN AQ-R-ESCALATED(WS-ROW) = 'Y'
                       MOVE 'ESC '     TO AL-FLAG
                    WHEN AQ-R-DUE(WS-ROW) < WS-TODAY
                       MOVE 'LATE'     TO AL-FLAG
                    WHEN AQ-R-DUE(WS-ROW) = WS-TODAY
                       MOVE 'DUE '     TO AL-FLAG
                    WHEN OTHER
                       MOVE SPACES     TO AL-FLAG
                 END-EVALUATE
                 MOVE WS-ACT-LINE      TO AV-LIN-I(WS-LINE)
                 MOVE DFHBMFSE         TO AV-SEL-A(WS-LINE)
                 IF AQ-R-ESCALATED(WS-ROW) = 'Y'
                    MOVE DFHBMASB      TO AV-LIN-A(WS-LINE)
                 ELSE
                    MOVE DFHBMASK      TO AV-LIN-A(WS-LINE)
                 END-IF
              END-IF
           END-PERFORM
           MOVE WS-USERID              TO AUSERO
           MOVE AQ-ROW-COUNT           TO WS-COUNT-EDIT
           MOVE WS-COUNT-EDIT          TO ACOUNTO
           MOVE WS-OVERDUE-COUNT       TO WS-COUNT-EDIT
           MOVE WS-COUNT-EDIT          TO AOVRDO
           COMPUTE WS-PAGES = (AQ-ROW-COUNT + WS-PAGE-SIZE - 1)
                            / WS-PAGE-SIZE
           IF WS-PAGES = 0
              MOVE 1                   TO WS-PAGES
           END-IF
           MOVE AQ-PAGE                TO WS-PAGE-EDIT
           MOVE WS-PAGES               TO WS-PAGES-EDIT
           MOVE SPACES                 TO APAGEO
           STRING WS-PAGE-EDIT ' of ' WS-PAGES-EDIT
                  DELIMITED BY SIZE INTO APAGEO
           MOVE SPACES                 TO AWRNO
           IF AQ-ROW-COUNT >= WS-MAX-ROWS
              MOVE 'Showing the 20 most urgent activities - work these
      -            ' to see the rest'  TO AWRNO
           END-IF.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(CLMAVMI) RESP(WS-RESP)
           END-EXEC
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-PAGE-SIZE
              IF WS-RESP NOT = DFHRESP(NORMAL)
                 OR AV-SEL-I(WS-LINE) = LOW-VALUE
                 MOVE SPACE            TO AV-SEL-I(WS-LINE)
              END-IF
              INSPECT AV-SEL-I(WS-LINE) CONVERTING 'csv' TO 'CSV'
              MOVE AV-SEL-I(WS-LINE)   TO WS-SCRATCH(WS-LINE:1)
           END-PERFORM
           MOVE LOW-VALUES             TO CLMAVMO
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-PAGE-SIZE
              MOVE WS-SCRATCH(WS-LINE:1) TO AV-SEL-I(WS-LINE)
           END-PERFORM
           MOVE SPACES                 TO WS-SCRATCH.

      ******************************************************************
       6100-SEND-MAP.
           MOVE WS-TODAY               TO ADATEO
           IF WS-SEND-ERASE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMAVMO) ERASE CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           ELSE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMAVMO) DATAONLY CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           END-IF.

      ******************************************************************
      * Errors keep the user's selections on screen
       6300-SEND-ERROR.
           MOVE WS-MSG                 TO AMSGO
           MOVE DFHBMASB               TO AMSGA
           PERFORM VARYING WS-LINE FROM 1 BY 1
                   UNTIL WS-LINE > WS-PAGE-SIZE
              IF AV-SEL-I(WS-LINE) = LOW-VALUE
                 MOVE SPACE            TO AV-SEL-I(WS-LINE)
              END-IF
           END-PERFORM
           IF AV-SEL-L(1) NOT = -1
              MOVE -1                  TO AV-SEL-L(1)
           END-IF
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       8100-LINK-ACTLST.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTLST  '             TO AVA-REQUEST-ID
           MOVE WS-USERID              TO AVA-AV-ASSIGNED-USER
           MOVE 0                      TO AVA-AV-CLAIM-ID
           PERFORM 8300-LINK-ACTV0
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN AVA-RC-OK
                 WHEN AVA-RC-WARNING
                    CONTINUE
                 WHEN AVA-RC-NOT-FOUND
                    MOVE 0             TO AVA-AV-ROW-COUNT
                 WHEN AVA-RETURN-CODE >= 90
                    MOVE AVA-MESSAGE   TO WS-MSG
                    PERFORM 8900-BACK-OUT
                 WHEN OTHER
                    SET WS-HAS-ERROR   TO TRUE
                    STRING AVA-REASON-CODE ' ' AVA-MESSAGE
                           DELIMITED BY SIZE INTO WS-MSG
              END-EVALUATE
           END-IF.

      ******************************************************************
      * Complete or skip one row (WS-ROW, selection in WS-SEL)
       8200-LINK-ACTUPD.
           INITIALIZE WS-ACT-CA
           IF WS-SEL = 'C'
              MOVE 'ACTCMP  '          TO AVA-REQUEST-ID
           ELSE
              MOVE 'ACTSKP  '          TO AVA-REQUEST-ID
           END-IF
           MOVE AQ-R-ACTIVITY-ID(WS-ROW) TO AVA-AV-ACTIVITY-ID
           MOVE WS-USERID              TO AVA-AV-ASSIGNED-USER
           PERFORM 8300-LINK-ACTV0
           EVALUATE TRUE
              WHEN WS-LINK-FAILED
                 MOVE 'Y'              TO WS-ABORT-FLAG
                 MOVE WS-MSG           TO WS-FIRST-FAIL
              WHEN AVA-RC-OK
              WHEN AVA-RC-WARNING
                 IF WS-SEL = 'C'
                    ADD 1              TO WS-DONE-COUNT
                 ELSE
                    ADD 1              TO WS-SKIP-COUNT
                 END-IF
              WHEN AVA-RETURN-CODE >= 90
                 MOVE AVA-MESSAGE      TO WS-MSG
                 PERFORM 8900-BACK-OUT
                 MOVE 'Y'              TO WS-ABORT-FLAG
                 MOVE WS-MSG           TO WS-FIRST-FAIL
              WHEN OTHER
                 ADD 1                 TO WS-FAIL-COUNT
                 IF WS-FIRST-FAIL = SPACES
                    STRING AQ-R-CLAIM-NUMBER(WS-ROW) ' '
                           AVA-MESSAGE
                           DELIMITED BY SIZE INTO WS-FIRST-FAIL
                 END-IF
           END-EVALUATE
           MOVE SPACES                 TO WS-MSG.

      ******************************************************************
       8300-LINK-ACTV0.
           MOVE WS-USERID              TO AVA-USER-ID
           MOVE WS-TRACE-ID            TO AVA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AVA-CALLER-PGM
           MOVE WS-ACT-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-ACT-PGM)
                     COMMAREA(WS-ACT-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
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
      * BR-CLM-SAC-005 Back out the task's unit of work
       8900-BACK-OUT.
           SET WS-HAS-ERROR            TO TRUE
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           MOVE WS-MSG                 TO WS-SCRATCH
           MOVE SPACES                 TO WS-MSG
           STRING 'Backed out, nothing saved: ' WS-SCRATCH
                  DELIMITED BY SIZE INTO WS-MSG
           MOVE SPACES                 TO WS-SCRATCH.

      ******************************************************************
       8950-LOG-CICS-ERROR.
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERA-REQUEST-ID
           MOVE WS-USERID              TO ERA-USER-ID
           MOVE WS-TRACE-ID            TO ERA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ERA-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERA-ER-PROGRAM
           MOVE '8300-LINK-ACTV0'      TO ERA-ER-PARAGRAPH
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
