       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMMNUP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMMNUP                                          *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * TRANSACTION : CMNU                                             *
      * MAP         : CLMMNU in mapset CLMMSET                         *
      * COMMAREA    : CLMMNU01 navigation block                        *
      * FUNCTION    : ClaimsCore main menu. Validates the option and   *
      *               the optional claim number / exposure sequence    *
      *               and transfers control (XCTL) to the chosen       *
      *               screen, handing the claim on in the navigation   *
      *               block. Screens return here with PF3 and may pass *
      *               a message that is shown on line 23.              *
      * FUNCTIONS   : 1 FNOL (CLMFNLP)      2 inquiry (CLMINQP)        *
      *               3 financials (CLMFINP) 4 activities (CLMACTP)    *
      * CALLED BY   : terminal user (CMNU), XCTL from every CLM screen *
      * CALLS       : XCTL CLMFNLP CLMINQP CLMFINP CLMACTP             *
      * TABLES      : none (no Db2 access in front ends)               *
      * RULES       : BR-CLM-SMN-001 Option must be 1-4                *
      *               BR-CLM-SMN-002 Claim number is left-justified,   *
      *                              starts with a letter, 6-12        *
      *                              letters/digits, no blanks         *
      *               BR-CLM-SMN-003 Inquiry (2) needs a claim number  *
      *               BR-CLM-SMN-004 Exposure seq 1-999 and only with  *
      *                              a claim number for option 3       *
      *               BR-CLM-SMN-005 FNOL and activities start without *
      *                              a claim; any claim entered is not *
      *                              passed so it cannot be mistaken   *
      *                              for the claim being created       *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMMNUP'.
       01  WS-TRANID                   PIC X(4)  VALUE 'CMNU'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CLMMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'CLMMNU'.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP VALUE 0.
           05 WS-RESP2                 PIC S9(8) COMP VALUE 0.
           05 WS-RESP-DISP             PIC 9(4).
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-USERID                PIC X(8).
           05 WS-SEND-MODE             PIC X.
              88 WS-SEND-ERASE          VALUE 'E'.
              88 WS-SEND-DATAONLY       VALUE 'D'.
           05 WS-ERROR-FLAG            PIC X.
              88 WS-NO-ERROR            VALUE 'N'.
              88 WS-HAS-ERROR           VALUE 'Y'.
           05 WS-OPTION                PIC X.
              88 WS-OPT-FNOL            VALUE '1'.
              88 WS-OPT-INQUIRY         VALUE '2'.
              88 WS-OPT-FINANCIALS      VALUE '3'.
              88 WS-OPT-ACTIVITIES      VALUE '4'.
              88 WS-OPT-VALID           VALUE '1' THRU '4'.
           05 WS-CLAIM-NUMBER          PIC X(12).
           05 WS-EXP-SEQ-X             PIC X(3).
           05 WS-EXP-SEQ               PIC 9(3).
           05 WS-TARGET-PGM            PIC X(8).
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-CHAR-COUNT            PIC S9(4) COMP.
           05 WS-BLANK-COUNT           PIC S9(4) COMP.
           05 WS-CLAIM-OK              PIC X.
              88 WS-CLAIM-VALID         VALUE 'Y'.
           05 WS-LOWER                 PIC X(26)
                 VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 WS-UPPER                 PIC X(26)
                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

      * Option -> program routing table (BR-CLM-SMN-001)
       01  WS-ROUTE-VALUES.
           05 FILLER                   PIC X(9) VALUE '1CLMFNLP '.
           05 FILLER                   PIC X(9) VALUE '2CLMINQP '.
           05 FILLER                   PIC X(9) VALUE '3CLMFINP '.
           05 FILLER                   PIC X(9) VALUE '4CLMACTP '.
       01  WS-ROUTE-TABLE REDEFINES WS-ROUTE-VALUES.
           05 WS-ROUTE OCCURS 4 TIMES.
              10 WS-ROUTE-OPT          PIC X.
              10 WS-ROUTE-PGM          PIC X(8).

       01  WS-COMMAREA.
           03 WS-CA-NAV.
              COPY CLMMNU01.

           COPY CLMMSET.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(4000).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE TRUE
              WHEN EIBCALEN = 0
                 MOVE SPACES           TO CN-NAV-MSG
                 PERFORM 2000-FIRST-DISPLAY
              WHEN CN-OWNER-PGM NOT = WS-PROGRAM-ID
                 PERFORM 2000-FIRST-DISPLAY
              WHEN EIBAID = DFHPF3
                 PERFORM 2900-END-SESSION
              WHEN EIBAID = DFHCLEAR
                 MOVE SPACES           TO CN-NAV-MSG
                 PERFORM 2000-FIRST-DISPLAY
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-PROCESS-OPTION
              WHEN OTHER
                 MOVE LOW-VALUES       TO CLMMNUO
                 MOVE 'Key not active - use ENTER, PF3 or CLEAR'
                                       TO MMSGO
                 MOVE -1               TO MOPTL
                 SET WS-SEND-DATAONLY  TO TRUE
                 PERFORM 6100-SEND-MAP
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
      * Restore the navigation block. A block longer than ours (a
      * screen that returned with its full COMMAREA) is cut to the
      * navigation part; anything shorter is a fresh start.
       1000-INITIALISE.
           INITIALIZE WS-COMMAREA
           IF EIBCALEN >= LENGTH OF WS-COMMAREA
              MOVE DFHCOMMAREA(1:LENGTH OF WS-COMMAREA)
                                       TO WS-COMMAREA
           END-IF
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
           END-EXEC
           MOVE WS-USERID              TO CN-USER-ID
           SET WS-NO-ERROR             TO TRUE.

      ******************************************************************
       2000-FIRST-DISPLAY.
           MOVE LOW-VALUES             TO CLMMNUO
           MOVE CN-CLAIM-NUMBER        TO MCLMNOO
           IF CN-EXPOSURE-SEQ > 0
              MOVE CN-EXPOSURE-SEQ     TO MEXPSQO
           END-IF
           IF CN-NAV-MSG = SPACES
              MOVE 'Select an option and press ENTER' TO MMSGO
           ELSE
              MOVE CN-NAV-MSG          TO MMSGO
           END-IF
           MOVE SPACES                 TO CN-NAV-MSG
           MOVE -1                     TO MOPTL
           SET WS-SEND-ERASE           TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       2100-PROCESS-OPTION.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3000-VALIDATE
           IF WS-HAS-ERROR
              SET WS-SEND-DATAONLY     TO TRUE
              PERFORM 6100-SEND-MAP
           ELSE
              PERFORM 2200-TRANSFER
           END-IF.

      ******************************************************************
      * BR-CLM-SMN-005 FNOL and activities never carry a claim on
       2200-TRANSFER.
           MOVE WS-PROGRAM-ID          TO CN-FROM-PGM
           MOVE SPACES                 TO CN-OWNER-PGM
                                          CN-NAV-MSG
           EVALUATE TRUE
              WHEN WS-OPT-FNOL
              WHEN WS-OPT-ACTIVITIES
                 MOVE SPACES           TO CN-CLAIM-NUMBER
                 MOVE 0                TO CN-EXPOSURE-SEQ
              WHEN OTHER
                 MOVE WS-CLAIM-NUMBER  TO CN-CLAIM-NUMBER
                 MOVE WS-EXP-SEQ       TO CN-EXPOSURE-SEQ
           END-EVALUATE
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
              IF WS-ROUTE-OPT(WS-IX) = WS-OPTION
                 MOVE WS-ROUTE-PGM(WS-IX) TO WS-TARGET-PGM
              END-IF
           END-PERFORM
           EXEC CICS XCTL PROGRAM(WS-TARGET-PGM)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
      * Only reached when the XCTL failed
           MOVE WS-RESP                TO WS-RESP-DISP
           MOVE LOW-VALUES             TO CLMMNUO
           IF WS-RESP = DFHRESP(PGMIDERR)
              STRING 'Function ' WS-TARGET-PGM
                     ' is not available in this region'
                     DELIMITED BY SIZE INTO MMSGO
           ELSE
              STRING 'Transfer to ' WS-TARGET-PGM
                     ' failed, CICS RESP ' WS-RESP-DISP
                     DELIMITED BY SIZE INTO MMSGO
           END-IF
           MOVE DFHBMASB               TO MMSGA
           MOVE -1                     TO MOPTL
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       2900-END-SESSION.
           EXEC CICS SEND TEXT FROM('ClaimsCore session ended')
                     LENGTH(24) ERASE FREEKB
           END-EXEC
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
       3000-VALIDATE.
           MOVE LOW-VALUES             TO CLMMNUO
           MOVE MOPTI                  TO WS-OPTION
           MOVE MCLMNOI                TO WS-CLAIM-NUMBER
           INSPECT WS-CLAIM-NUMBER CONVERTING WS-LOWER TO WS-UPPER
           MOVE MEXPSQI                TO WS-EXP-SEQ-X
           MOVE 0                      TO WS-EXP-SEQ

      * BR-CLM-SMN-001 Option must be 1-4
           IF NOT WS-OPT-VALID
              MOVE 'Enter option 1, 2, 3 or 4' TO MMSGO
              PERFORM 3900-FLAG-OPTION
           END-IF

           IF WS-HAS-ERROR
              CONTINUE
           ELSE
              IF WS-CLAIM-NUMBER NOT = SPACES
                 PERFORM 3100-CHECK-CLAIM-NUMBER
              ELSE
      * BR-CLM-SMN-003 Inquiry needs a claim number
                 IF WS-OPT-INQUIRY
                    MOVE 'Claim number is required for inquiry'
                                       TO MMSGO
                    PERFORM 3910-FLAG-CLAIM
                 END-IF
              END-IF
           END-IF

           IF WS-NO-ERROR AND WS-OPT-FINANCIALS
              AND WS-EXP-SEQ-X NOT = SPACES
              PERFORM 3200-CHECK-EXPOSURE-SEQ
           END-IF.

      ******************************************************************
      * BR-CLM-SMN-002 Claim number format
       3100-CHECK-CLAIM-NUMBER.
           MOVE 'Y'                    TO WS-CLAIM-OK
           MOVE 0                      TO WS-CHAR-COUNT
           INSPECT WS-CLAIM-NUMBER TALLYING WS-CHAR-COUNT
                   FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE 0                      TO WS-BLANK-COUNT
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
           IF NOT WS-CLAIM-VALID
              MOVE 'Claim number must be 6-12 letters/digits, starting w
      -            'ith a letter'      TO MMSGO
              PERFORM 3910-FLAG-CLAIM
           END-IF.

      ******************************************************************
      * BR-CLM-SMN-004 Exposure seq 1-999, needs a claim number
       3200-CHECK-EXPOSURE-SEQ.
           INSPECT WS-EXP-SEQ-X REPLACING LEADING SPACES BY ZEROS
           IF WS-EXP-SEQ-X IS NOT NUMERIC
              MOVE 'Exposure seq must be numeric' TO MMSGO
              PERFORM 3920-FLAG-EXPOSURE
           ELSE
              MOVE WS-EXP-SEQ-X        TO WS-EXP-SEQ
              EVALUATE TRUE
                 WHEN WS-EXP-SEQ = 0
                    MOVE 'Exposure seq must be 1-999' TO MMSGO
                    PERFORM 3920-FLAG-EXPOSURE
                 WHEN WS-CLAIM-NUMBER = SPACES
                    MOVE 'Exposure seq needs a claim number'
                                       TO MMSGO
                    PERFORM 3910-FLAG-CLAIM
              END-EVALUATE
           END-IF.

      ******************************************************************
       3900-FLAG-OPTION.
           SET WS-HAS-ERROR            TO TRUE
           MOVE -1                     TO MOPTL
           MOVE DFHUNINT               TO MOPTA
           MOVE DFHBMASB               TO MMSGA.

       3910-FLAG-CLAIM.
           SET WS-HAS-ERROR            TO TRUE
           MOVE -1                     TO MCLMNOL
           MOVE DFHUNIMD               TO MCLMNOA
           MOVE DFHBMASB               TO MMSGA.

       3920-FLAG-EXPOSURE.
           SET WS-HAS-ERROR            TO TRUE
           MOVE -1                     TO MEXPSQL
           MOVE DFHUNINT               TO MEXPSQA
           MOVE DFHBMASB               TO MMSGA.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(CLMMNUI) RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
              WHEN DFHRESP(NORMAL)
                 INSPECT CLMMNUI REPLACING ALL LOW-VALUES BY SPACES
              WHEN DFHRESP(MAPFAIL)
                 MOVE SPACES           TO MOPTI MCLMNOI MEXPSQI
              WHEN OTHER
                 MOVE SPACES           TO MOPTI MCLMNOI MEXPSQI
                 MOVE 'X'              TO MOPTI
           END-EVALUATE.

      ******************************************************************
       6100-SEND-MAP.
           MOVE WS-TODAY               TO MDATEO
           MOVE WS-USERID              TO MUSERO
           MOVE EIBTRMID               TO MTERMO
           IF WS-SEND-ERASE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMMNUO) ERASE CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           ELSE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMMNUO) DATAONLY CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           END-IF.

      ******************************************************************
       9000-RETURN.
           MOVE WS-PROGRAM-ID          TO CN-OWNER-PGM
           EXEC CICS RETURN TRANSID(WS-TRANID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
