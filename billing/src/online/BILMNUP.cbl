       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILMNUP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILMNUP                                          *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * FUNCTION    : BillingCore main menu (transaction BMNU, map     *
      *               BILMNU). Routes by option or PF key with XCTL to *
      *               the account summary (BILACIP / BACI), payment    *
      *               entry (BILPAYP / BPAY) and invoice list          *
      *               (BILIVMP / BIVL), carrying the account number.   *
      * REQUESTS    : n/a (screen); no business action                 *
      * CALLED BY   : terminal (BMNU), PF3 from BACI / BPAY / BIVL     *
      * CALLS       : XCTL BILACIP BILPAYP BILIVMP; LINK BILERRLG      *
      * TABLES      : none                                             *
      * RULES       : BR-BIL-MNU-001 Option must be 1, 2 or 3          *
      *               BR-BIL-MNU-002 PF1 / PF2 / PF4 are shortcuts for *
      *                              options 1 / 2 / 3                 *
      *               BR-BIL-MNU-003 An account number, when keyed,    *
      *                              is 10 characters with no embedded *
      *                              blanks                            *
      *               BR-BIL-MNU-004 Invoice list (3) needs an account *
      *                              number                            *
      *               BR-BIL-MNU-005 PF3 or CLEAR on the menu ends the *
      *                              BillingCore session               *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILMNUP'.
       01  WS-TRAN-ID                  PIC X(4)  VALUE 'BMNU'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'BILMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'BILMNU'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP.
           05 WS-RESP2                 PIC S9(8) COMP.
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-OPTION                PIC X.
              88 WS-OPT-ACCOUNT         VALUE '1'.
              88 WS-OPT-PAYMENT         VALUE '2'.
              88 WS-OPT-INVOICES        VALUE '3'.
           05 WS-TARGET-PGM            PIC X(8).
           05 WS-BLANKS                PIC S9(4) COMP.
           05 WS-END-TEXT              PIC X(40)
                 VALUE 'BillingCore session ended'.
           05 WS-ERR-TEXT              PIC X(79).

       01  WS-SCR-CA.
           COPY BILMNUC1.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

           COPY BILMSET.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(256).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE TRUE
              WHEN EIBCALEN = 0
              WHEN SCR-OWNER-PGM NOT = WS-PROGRAM-ID
                 PERFORM 2000-FIRST-DISPLAY
      * BR-BIL-MNU-005 PF3 / CLEAR end the session
              WHEN EIBAID = DFHPF3
              WHEN EIBAID = DFHCLEAR
                 PERFORM 2900-END-SESSION
              WHEN EIBAID = DFHENTER
              WHEN EIBAID = DFHPF1
              WHEN EIBAID = DFHPF2
              WHEN EIBAID = DFHPF4
                 PERFORM 2100-PROCESS-SELECTION
              WHEN OTHER
                 MOVE LOW-VALUES       TO BILMNUO
                 MOVE 'That key is not active on this screen'
                                       TO MNUMSGO
                 MOVE -1               TO MNUOPTL
                 PERFORM 7100-SEND-DATAONLY
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           IF EIBCALEN > 0
              MOVE DFHCOMMAREA(1:EIBCALEN) TO WS-SCR-CA
           END-IF
           IF EIBCALEN = 0 OR NOT SCR-VALID
              INITIALIZE WS-SCR-CA
              SET SCR-VALID            TO TRUE
              EXEC CICS ASSIGN USERID(SCR-USER-ID)
                        RESP(WS-RESP)
              END-EXEC
           END-IF
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
                     RESP(WS-RESP)
           END-EXEC.

      ******************************************************************
       2000-FIRST-DISPLAY.
           MOVE LOW-VALUES             TO BILMNUO
           MOVE SCR-ACCOUNT-NUMBER     TO MNUACCTO
           MOVE 'Select an option and press ENTER' TO MNUMSGO
           MOVE -1                     TO MNUOPTL
           PERFORM 7000-SEND-MAP.

      ******************************************************************
       2100-PROCESS-SELECTION.
           PERFORM 6000-RECEIVE-MAP
           MOVE LOW-VALUES             TO BILMNUO
           PERFORM 3000-VALIDATE-SELECTION
           MOVE WS-PROGRAM-ID          TO SCR-FROM-PGM
           MOVE 1                      TO SCR-PAGE-NO
           MOVE 0                      TO SCR-ACCOUNT-ID
           EVALUATE TRUE
              WHEN WS-OPT-ACCOUNT
                 MOVE 'BILACIP'        TO WS-TARGET-PGM
              WHEN WS-OPT-PAYMENT
                 MOVE 'BILPAYP'        TO WS-TARGET-PGM
              WHEN WS-OPT-INVOICES
                 MOVE 'BILIVMP'        TO WS-TARGET-PGM
           END-EVALUATE
           PERFORM 8000-XCTL-TARGET.

      ******************************************************************
       3000-VALIDATE-SELECTION.
      * BR-BIL-MNU-002 PF shortcuts override the keyed option
           EVALUATE EIBAID
              WHEN DFHPF1  MOVE '1'    TO WS-OPTION
              WHEN DFHPF2  MOVE '2'    TO WS-OPTION
              WHEN DFHPF4  MOVE '3'    TO WS-OPTION
              WHEN OTHER
                 IF MNUOPTL > 0
                    MOVE MNUOPTI       TO WS-OPTION
                 ELSE
                    MOVE SPACE         TO WS-OPTION
                 END-IF
           END-EVALUATE
           IF MNUACCTL > 0
              MOVE MNUACCTI            TO SCR-ACCOUNT-NUMBER
           ELSE
              IF MNUACCTF = DFHBMEOF
                 MOVE SPACES           TO SCR-ACCOUNT-NUMBER
              END-IF
           END-IF
      * BR-BIL-MNU-001 Only the three listed options exist
           IF NOT (WS-OPT-ACCOUNT OR WS-OPT-PAYMENT OR WS-OPT-INVOICES)
              MOVE 'Option must be 1, 2 or 3' TO WS-ERR-TEXT
              MOVE -1                  TO MNUOPTL
              PERFORM 3900-VALIDATION-ERROR
           END-IF
      * BR-BIL-MNU-003 Account number shape when keyed
           IF SCR-ACCOUNT-NUMBER NOT = SPACES
              MOVE 0                   TO WS-BLANKS
              INSPECT SCR-ACCOUNT-NUMBER TALLYING WS-BLANKS
                      FOR ALL SPACES
              IF WS-BLANKS > 0
                 MOVE 'Account number is 10 characters, no blanks'
                                       TO WS-ERR-TEXT
                 MOVE -1               TO MNUACCTL
                 PERFORM 3900-VALIDATION-ERROR
              END-IF
           END-IF
      * BR-BIL-MNU-004 Invoice list is always for one account
           IF WS-OPT-INVOICES AND SCR-ACCOUNT-NUMBER = SPACES
              MOVE 'Enter the account number for the invoice list'
                                       TO WS-ERR-TEXT
              MOVE -1                  TO MNUACCTL
              PERFORM 3900-VALIDATION-ERROR
           END-IF.

      ******************************************************************
       3900-VALIDATION-ERROR.
           MOVE WS-ERR-TEXT            TO MNUMSGO
           PERFORM 7100-SEND-DATAONLY
           PERFORM 9000-RETURN.

      ******************************************************************
       2900-END-SESSION.
           EXEC CICS SEND TEXT FROM(WS-END-TEXT)
                     LENGTH(LENGTH OF WS-END-TEXT)
                     ERASE FREEKB
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(BILMNUI)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
              WHEN DFHRESP(NORMAL)
                 CONTINUE
              WHEN DFHRESP(MAPFAIL)
                 MOVE LOW-VALUES       TO BILMNUI
              WHEN OTHER
                 MOVE '6000-RECEIVE-MAP' TO ERC-ER-PARAGRAPH
                 PERFORM 9800-CICS-ERROR
           END-EVALUATE.

      ******************************************************************
       7000-SEND-MAP.
           MOVE WS-TODAY               TO MNUDATEO
           MOVE SCR-USER-ID            TO MNUUSERO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILMNUO) ERASE CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7000-SEND-MAP'     TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-OWNER-PGM.

      ******************************************************************
       7100-SEND-DATAONLY.
           MOVE WS-TODAY               TO MNUDATEO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILMNUO) DATAONLY CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7100-SEND-DATAONLY' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-OWNER-PGM.

      ******************************************************************
       8000-XCTL-TARGET.
           EXEC CICS XCTL PROGRAM(WS-TARGET-PGM)
                     COMMAREA(WS-SCR-CA)
                     LENGTH(LENGTH OF WS-SCR-CA)
                     RESP(WS-RESP)
           END-EXEC
           MOVE SPACES                 TO WS-ERR-TEXT
           STRING 'Screen ' WS-TARGET-PGM ' is not available'
                  DELIMITED BY SIZE INTO WS-ERR-TEXT
           MOVE WS-ERR-TEXT            TO MNUMSGO
           MOVE -1                     TO MNUOPTL
           MOVE '8000-XCTL-TARGET'     TO ERC-ER-PARAGRAPH
           PERFORM 9850-LOG-CICS-ERROR
           PERFORM 7100-SEND-DATAONLY.

      ******************************************************************
       9000-RETURN.
           EXEC CICS RETURN TRANSID(WS-TRAN-ID)
                     COMMAREA(WS-SCR-CA)
                     LENGTH(LENGTH OF WS-SCR-CA)
           END-EXEC.

      ******************************************************************
       9800-CICS-ERROR.
           PERFORM 9850-LOG-CICS-ERROR
           EXEC CICS SEND TEXT FROM(WS-ERR-TEXT)
                     LENGTH(LENGTH OF WS-ERR-TEXT)
                     ERASE FREEKB
                     NOHANDLE
           END-EXEC
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
       9850-LOG-CICS-ERROR.
           MOVE WS-RESP                TO ERC-ER-RESP
           MOVE SPACES                 TO WS-ERR-TEXT
           STRING 'BILMNUP CICS error in ' ERC-ER-PARAGRAPH
                  DELIMITED BY SIZE INTO WS-ERR-TEXT
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE SCR-USER-ID            TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
                                          ERC-ER-PROGRAM
           MOVE 0                      TO ERC-ER-SQLCODE
           MOVE WS-ERR-TEXT            TO ERC-ER-TEXT
           MOVE SCR-ACCOUNT-NUMBER     TO ERC-ER-KEY
           MOVE EIBTRNID               TO ERC-ER-TRAN-ID
           EXEC CICS LINK PROGRAM(WS-ERR-PGM)
                     COMMAREA(WS-ERR-CA)
                     LENGTH(4000)
                     NOHANDLE
           END-EXEC.
