       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMFINP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMFINP                                          *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * TRANSACTION : CFIN                                             *
      * MAP         : CLMFTM in mapset CLMMSET                         *
      * COMMAREA    : CLMMNU01 navigation + CLMFIN01 financials state  *
      * FUNCTION    : Reserve and payment entry with a confirm step,   *
      *               and supervisor approval / rejection of pending   *
      *               transactions.                                    *
      *               ENTER validates and resolves the claim/exposure  *
      *                     (CLMCLAIM CLMINQ) or the pending txn       *
      *                     (CLMFINTX TXNINQ) and asks for PF5.        *
      *               PF5   posts: R -> RSVSET, P -> PAYCRT,           *
      *                     A -> TXNAPR, J -> TXNREJ, and shows the    *
      *                     txn status (SUBM, or PAPR with approver)   *
      *                     and the reserve line balances.             *
      *               PF12  back to entry; PF3 menu; CLEAR restart.    *
      * CALLED BY   : terminal user (CFIN), XCTL from CLMMNUP, CLMINQP *
      * CALLS       : LINK CLMCLAIM (CLMINQ)                           *
      *               LINK CLMFINTX (RSVSET PAYCRT TXNAPR TXNREJ       *
      *                              TXNINQ)                           *
      *               LINK CLMCODVL  LINK CLMDATES (DTVALD DTDIFF)     *
      *               LINK CLMERRLG (ERRLOG)  XCTL CLMMNUP             *
      * TABLES      : none (all data access through the services)      *
      * RULES       : BR-CLM-SFI-001 Action R, P, A or J               *
      *               BR-CLM-SFI-002 Claim number and exposure seq     *
      *                              (1-999) required for R and P      *
      *               BR-CLM-SFI-003 Cost type CLMC/EXAO/EXDC; category*
      *                              optional (DEFAULT), else COSTCAT  *
      *               BR-CLM-SFI-004 Amount: up to 999,999,999.99,     *
      *                              2 decimals; reserve >= 0, payment *
      *                              > 0                               *
      *               BR-CLM-SFI-005 Fields of other actions must be   *
      *                              blank (no stray payee on reserve) *
      *               BR-CLM-SFI-006 Claim not closed; exposure exists *
      *                              and is not closed                 *
      *               BR-CLM-SFI-007 Payment above the exposure's open *
      *                              reserve is warned before posting  *
      *               BR-CLM-SFI-008 Payment: payee required; type     *
      *                              PART/FINL/SUPP (default PART),    *
      *                              method CHEQ/BACS/FAST (default    *
      *                              CHEQ); scheduled date today to    *
      *                              today+30, default today           *
      *               BR-CLM-SFI-009 Approve/reject only a PAPR txn    *
      *               BR-CLM-SFI-010 Approver other than the named one *
      *                              is warned (service checks limit)  *
      *               BR-CLM-SFI-011 Reject needs a reason code        *
      *               BR-CLM-SFI-012 A posted entry is never posted    *
      *                              twice from the same screen        *
      *               BR-CLM-SFI-013 PF5 posts only the validated entry*
      *               BR-CLM-SFI-014 PAPR result names the approver    *
      *               BR-CLM-SFI-015 Service RC >= 90 backs out the    *
      *                              unit of work                      *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMFINP'.
       01  WS-TRANID                   PIC X(4)  VALUE 'CFIN'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'CLMMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'CLMFTM'.
       01  WS-MENU-PGM                 PIC X(8)  VALUE 'CLMMNUP'.
       01  WS-CLM-PGM                  PIC X(8)  VALUE 'CLMCLAIM'.
       01  WS-FIN-PGM                  PIC X(8)  VALUE 'CLMFINTX'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

       01  WS-LIMITS.
           05 WS-MAX-SCHED-DAYS        PIC S9(7) COMP-3 VALUE 30.

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
           05 WS-ERR-FIELD             PIC 9(2).
           05 WS-ERR-TEXT              PIC X(79).
           05 WS-WARN-TEXT             PIC X(79).
           05 WS-FLD-MSG               PIC X(79).
           05 WS-CAPTION               PIC X(79).
           05 WS-IX                    PIC S9(4) COMP.
           05 WS-PTR                   PIC S9(4) COMP.
           05 WS-INT-DIGITS            PIC S9(4) COMP.
           05 WS-DEC-DIGITS            PIC S9(4) COMP.
           05 WS-CHAR                  PIC X.
           05 WS-LOWER                 PIC X(26)
                 VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05 WS-UPPER                 PIC X(26)
                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05 WS-CODE-TABLE            PIC X(8).
           05 WS-CODE-VALUE            PIC X(10).
           05 WS-CODE-FLAG             PIC X.
              88 WS-CODE-VALID          VALUE 'Y'.
           05 WS-COST-TYPE             PIC X(4).
              88 WS-COST-TYPE-VALID     VALUE 'CLMC' 'EXAO' 'EXDC'.
           05 WS-PAY-TYPE              PIC X(4).
              88 WS-PAY-TYPE-VALID      VALUE 'PART' 'FINL' 'SUPP'.
           05 WS-PAY-METHOD            PIC X(4).
              88 WS-PAY-METHOD-VALID    VALUE 'CHEQ' 'BACS' 'FAST'.
           05 WS-DEFAULT-CATEGORY      PIC X(8) VALUE 'DEFAULT'.
           05 WS-EXP-IX                PIC S9(4) COMP.
           05 WS-EXP-SEQ               PIC S9(4) COMP.
           05 WS-NUM-EDIT              PIC Z(8)9.
           05 WS-AMT-IN                PIC S9(11)V99 COMP-3.
           05 WS-AMT-EDIT              PIC ZZZ,ZZZ,ZZ9.99-.
           05 WS-AMT-OUT               PIC X(15).

      * Number parser (5100): integers and money amounts
       01  WS-NUM-PARSE.
           05 WS-NUM-TEXT              PIC X(15).
           05 WS-NUM-VALUE             PIC 9(15).
           05 WS-NUM-AMOUNT            PIC S9(11)V99 COMP-3.
           05 WS-NUM-ALLOW-DEC         PIC X.
              88 WS-NUM-IS-MONEY        VALUE 'Y'.
           05 WS-NUM-FLAG              PIC X.
              88 WS-NUM-VALID           VALUE 'Y'.
           05 WS-NUM-STATE             PIC X.
              88 WS-NUM-LEADING         VALUE 'L'.
              88 WS-NUM-IN-INTEGER      VALUE 'I'.
              88 WS-NUM-IN-DECIMALS     VALUE 'D'.
              88 WS-NUM-TRAILING        VALUE 'T'.

       01  WS-SCREEN-STATE.
           03 FILLER.
              COPY CLMFIN01 REPLACING LEADING ==TF-== BY ==WF-==.

       01  WS-COMMAREA.
           03 WS-CA-NAV.
              COPY CLMMNU01.
           03 WS-CA-STATE.
              COPY CLMFIN01.

       01  WS-CLM-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CLA-==.
           03 CLA-BODY.
              COPY CLMCCLM REPLACING LEADING ==CA-== BY ==CLA-==.
       01  WS-FIN-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==FTA-==.
           03 FTA-BODY.
              COPY CLMCFIN REPLACING LEADING ==CA-== BY ==FTA-==.
       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.
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
           MOVE LOW-VALUES             TO CLMFTMO
           EVALUATE TRUE
              WHEN EIBCALEN = 0
              WHEN CN-OWNER-PGM NOT = WS-PROGRAM-ID
                 PERFORM 2000-FIRST-ENTRY
              WHEN EIBAID = DFHPF3
                 PERFORM 2800-RETURN-TO-MENU
              WHEN EIBAID = DFHCLEAR
                 MOVE TF-E-CLAIM-NUMBER TO CN-CLAIM-NUMBER
                 MOVE 0                TO CN-EXPOSURE-SEQ
                 PERFORM 2000-FIRST-ENTRY
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-VALIDATE-REQUEST
              WHEN EIBAID = DFHPF5
                 PERFORM 2300-POST
              WHEN EIBAID = DFHPF12
                 PERFORM 2700-BACK-TO-ENTRY
              WHEN OTHER
                 MOVE 'Key not active on this screen' TO WS-ERR-TEXT
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
           MOVE SPACES                 TO WS-ERR-TEXT WS-WARN-TEXT
           SET WS-NO-ERROR             TO TRUE.

      ******************************************************************
      * New conversation: claim / exposure handed on are pre-filled
       2000-FIRST-ENTRY.
           INITIALIZE WS-CA-STATE
           MOVE CN-CLAIM-NUMBER        TO TCLMNOO
           IF CN-EXPOSURE-SEQ > 0
              MOVE CN-EXPOSURE-SEQ     TO TEXPSQO
              MOVE -1                  TO TACTL
           ELSE
              MOVE -1                  TO TCLMNOL
           END-IF
           IF CN-NAV-MSG = SPACES
              MOVE 'Enter the transaction and press ENTER' TO TMSGO
           ELSE
              MOVE CN-NAV-MSG          TO TMSGO
           END-IF
           MOVE SPACES                 TO CN-NAV-MSG
           PERFORM 6400-SET-MODE-TEXT
           SET WS-SEND-ERASE           TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
      * ENTER: validate, resolve, and ask for confirmation
       2100-VALIDATE-REQUEST.
           PERFORM 6000-RECEIVE-MAP
           SET TF-STEP-ENTRY           TO TRUE
           INITIALIZE WF-REQUEST
           PERFORM 3000-VALIDATE
           IF WS-NO-ERROR
              IF WF-E-ACTION = 'R' OR WF-E-ACTION = 'P'
                 PERFORM 8100-LINK-CLMINQ
                 IF WS-NO-ERROR
                    PERFORM 2150-RESOLVE-EXPOSURE
                 END-IF
              ELSE
                 PERFORM 8300-LINK-TXNINQ
                 IF WS-NO-ERROR
                    PERFORM 2200-CHECK-PENDING-TXN
                 END-IF
              END-IF
           END-IF
           PERFORM 6200-ENTRY-TO-SCREEN
           IF WS-NO-ERROR
              MOVE WF-ENTRY            TO TF-ENTRY
              MOVE WF-REQUEST          TO TF-REQUEST
              SET TF-STEP-CONFIRM      TO TRUE
              MOVE WS-WARN-TEXT        TO TWRNO
              IF TMSGO = LOW-VALUES
                 MOVE 'Check the details - PF5 to post, PF12 to change'
                                       TO TMSGO
              END-IF
              MOVE -1                  TO TACTL
              PERFORM 6400-SET-MODE-TEXT
              SET WS-SEND-DATAONLY     TO TRUE
              PERFORM 6100-SEND-MAP
           ELSE
              PERFORM 6400-SET-MODE-TEXT
              PERFORM 6300-SEND-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-SFI-006 Claim open, exposure present and open
      * BR-CLM-SFI-007 Payment above open reserve is warned
       2150-RESOLVE-EXPOSURE.
           MOVE CLA-CL-CLAIM-ID        TO WF-R-CLAIM-ID
           MOVE CLA-CL-STATE           TO TCLSTO
           MOVE 0                      TO WS-EXP-IX
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > CLA-CL-EXP-COUNT OR WS-IX > 10
              IF CLA-CL-X-SEQ(WS-IX) = WS-EXP-SEQ
                 MOVE WS-IX            TO WS-EXP-IX
              END-IF
           END-PERFORM
           EVALUATE TRUE
              WHEN CLA-CL-STATE = 'CLSD'
                 MOVE 'Claim is closed - reopen it before posting finan
      -               'cials'          TO WS-FLD-MSG
                 MOVE 01               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR
              WHEN WS-EXP-IX = 0
                 MOVE 'Exposure seq not found on this claim'
                                       TO WS-FLD-MSG
                 MOVE 02               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR
              WHEN CLA-CL-X-STATE(WS-EXP-IX) = 'CLSD'
                 MOVE 'Exposure is closed - reopen it before posting'
                                       TO WS-FLD-MSG
                 MOVE 02               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR
              WHEN OTHER
                 MOVE CLA-CL-X-EXPOSURE-ID(WS-EXP-IX)
                                       TO WF-R-EXPOSURE-ID
           END-EVALUATE
           IF WS-EXP-IX > 0
              MOVE CLA-CL-X-TYPE(WS-EXP-IX)  TO TEXTYPO
              MOVE CLA-CL-X-COVERAGE(WS-EXP-IX) TO TEXCOVO
              MOVE CLA-CL-X-STATE(WS-EXP-IX) TO TEXSTO
              MOVE CLA-CL-X-OPEN-RESERVE(WS-EXP-IX) TO WS-AMT-IN
              PERFORM 5200-FORMAT-AMOUNT
              MOVE WS-AMT-OUT          TO TEXRSVO
              MOVE CLA-CL-X-PAID(WS-EXP-IX) TO WS-AMT-IN
              PERFORM 5200-FORMAT-AMOUNT
              MOVE WS-AMT-OUT          TO TEXPDO
              IF WS-NO-ERROR AND WF-E-ACTION = 'P'
                 AND WF-R-AMOUNT > CLA-CL-X-OPEN-RESERVE(WS-EXP-IX)
                 MOVE 'Payment exceeds the open reserve of this exposu
      -               're - reserve will be eroded to zero'
                                       TO WS-WARN-TEXT
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-SFI-009 Only a txn pending approval can be decided
      * BR-CLM-SFI-010 Warn when the user is not the named approver
       2200-CHECK-PENDING-TXN.
           MOVE FTA-FT-TXN-ID          TO WS-NUM-EDIT
           MOVE WS-NUM-EDIT            TO TRTXNO
           MOVE FTA-FT-TXN-TYPE        TO TRTYPEO
           MOVE FTA-FT-TXN-STATUS      TO TRSTATO
           MOVE FTA-FT-APPROVER        TO TRAPPRO
           PERFORM 2360-SHOW-BALANCES
           IF FTA-FT-TXN-STATUS NOT = 'PAPR'
              STRING 'Transaction is ' FTA-FT-TXN-STATUS
                     ', not pending approval - nothing to decide'
                     DELIMITED BY SIZE INTO WS-FLD-MSG
              MOVE 10                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           ELSE
              IF FTA-FT-APPROVER NOT = WS-USERID
                 STRING 'Routed to ' FTA-FT-APPROVER
                        ' - your own authority limit will be checked'
                        DELIMITED BY SIZE INTO WS-WARN-TEXT
              END-IF
              MOVE FTA-FT-AMOUNT       TO WS-AMT-IN
              PERFORM 5200-FORMAT-AMOUNT
              STRING FTA-FT-TXN-TYPE ' of' WS-AMT-OUT
                     ' pending - PF5 to '
                     DELIMITED BY SIZE INTO TMSGO
              IF WF-E-ACTION = 'A'
                 MOVE 'approve' TO TMSGO(41:7)
              ELSE
                 MOVE 'reject'  TO TMSGO(41:6)
              END-IF
           END-IF.

      ******************************************************************
      * PF5: post the validated request
      * BR-CLM-SFI-012 Never post the same entry twice
       2300-POST.
           EVALUATE TRUE
              WHEN TF-STEP-POSTED
                 MOVE 'Already posted - change the details and press EN
      -               'TER for a new transaction'
                                       TO WS-ERR-TEXT
                 PERFORM 6300-SEND-ERROR
              WHEN NOT TF-STEP-CONFIRM
                 MOVE 'Press ENTER to validate the transaction first'
                                       TO WS-ERR-TEXT
                 PERFORM 6300-SEND-ERROR
              WHEN OTHER
                 PERFORM 6000-RECEIVE-MAP
      * BR-CLM-SFI-013 Post only exactly what was validated
                 IF WF-ENTRY NOT = TF-ENTRY
                    SET TF-STEP-ENTRY  TO TRUE
                    MOVE 'Details changed since validation - press ENTE
      -                  'R to validate again'
                                       TO WS-ERR-TEXT
                    PERFORM 6400-SET-MODE-TEXT
                    PERFORM 6300-SEND-ERROR
                 ELSE
                    PERFORM 8200-LINK-FINTX
                    IF WS-NO-ERROR
                       PERFORM 2350-SHOW-RESULT
                    ELSE
                       PERFORM 6400-SET-MODE-TEXT
                       PERFORM 6300-SEND-ERROR
                    END-IF
                 END-IF
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-SFI-014 Pending approval names the approver
       2350-SHOW-RESULT.
           SET TF-STEP-POSTED          TO TRUE
           MOVE FTA-FT-TXN-ID          TO WS-NUM-EDIT
           MOVE WS-NUM-EDIT            TO TRTXNO
           MOVE FTA-FT-TXN-TYPE        TO TRTYPEO
           MOVE FTA-FT-TXN-STATUS      TO TRSTATO
           MOVE FTA-FT-APPROVER        TO TRAPPRO
           MOVE FTA-FT-ACTIVITY-ID     TO WS-NUM-EDIT
           MOVE WS-NUM-EDIT            TO TRACTO
           MOVE FTA-FT-CHECK-ID        TO WS-NUM-EDIT
           MOVE WS-NUM-EDIT            TO TRCHKO
           PERFORM 2360-SHOW-BALANCES
           MOVE SPACES                 TO TWRNO
           EVALUATE TRUE
              WHEN FTA-FT-TXN-STATUS = 'PAPR'
                 STRING 'Above your authority - sent to '
                        FTA-FT-APPROVER ' for approval'
                        DELIMITED BY SIZE INTO TMSGO
                 MOVE FTA-FT-ACTIVITY-ID TO WS-NUM-EDIT
                 STRING 'Approval activity ' WS-NUM-EDIT
                        ' created; payment is held until approved'
                        DELIMITED BY SIZE INTO TWRNO
              WHEN TF-E-ACTION = 'A'
                 STRING 'Transaction approved - status '
                        FTA-FT-TXN-STATUS
                        DELIMITED BY SIZE INTO TMSGO
              WHEN TF-E-ACTION = 'J'
                 STRING 'Transaction rejected - status '
                        FTA-FT-TXN-STATUS
                        DELIMITED BY SIZE INTO TMSGO
              WHEN OTHER
                 STRING 'Posted - transaction ' TRTXNO ' status '
                        FTA-FT-TXN-STATUS
                        DELIMITED BY SIZE INTO TMSGO
           END-EVALUATE
      * RC 04 APRVREQD comes with status PAPR and is shown above as a
      * warning; any other RC 04 text goes to the warning line
           IF FTA-RC-WARNING AND FTA-FT-TXN-STATUS NOT = 'PAPR'
              MOVE FTA-MESSAGE         TO TWRNO
           END-IF
           MOVE -1                     TO TACTL
           PERFORM 6400-SET-MODE-TEXT
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       2360-SHOW-BALANCES.
           MOVE FTA-FT-RL-ID           TO WS-NUM-EDIT
           MOVE WS-NUM-EDIT            TO TRRLO
           MOVE FTA-FT-RL-OPEN-RESERVE TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO TRORSVO
           MOVE FTA-FT-RL-TOTAL-PAID   TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO TRTPDO
           MOVE FTA-FT-RL-RECOVERY-RSV TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO TRRRSVO
           MOVE FTA-FT-RL-TOTAL-RECOVERED TO WS-AMT-IN
           PERFORM 5200-FORMAT-AMOUNT
           MOVE WS-AMT-OUT             TO TRRCVO.

      ******************************************************************
       2700-BACK-TO-ENTRY.
           SET TF-STEP-ENTRY           TO TRUE
           MOVE 'Change the details and press ENTER' TO TMSGO
           MOVE SPACES                 TO TWRNO
           MOVE -1                     TO TACTL
           PERFORM 6400-SET-MODE-TEXT
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       2800-RETURN-TO-MENU.
           MOVE TF-E-CLAIM-NUMBER      TO CN-CLAIM-NUMBER
           MOVE 0                      TO CN-EXPOSURE-SEQ
           MOVE SPACES                 TO CN-NAV-MSG
           IF TF-STEP-POSTED
              MOVE 'Last financial transaction was posted'
                                       TO CN-NAV-MSG
           END-IF
           MOVE WS-PROGRAM-ID          TO CN-FROM-PGM
           MOVE SPACES                 TO CN-OWNER-PGM
           EXEC CICS XCTL PROGRAM(WS-MENU-PGM)
                     COMMAREA(WS-CA-NAV)
                     LENGTH(LENGTH OF WS-CA-NAV)
                     RESP(WS-RESP)
           END-EXEC
           MOVE WS-RESP                TO WS-RESP-DISP
           STRING 'Transfer to ' WS-MENU-PGM ' failed, CICS RESP '
                  WS-RESP-DISP DELIMITED BY SIZE INTO WS-ERR-TEXT
           PERFORM 6300-SEND-ERROR.

      ******************************************************************
      * BR-CLM-SFI-001 Action R, P, A or J
       3000-VALIDATE.
           EVALUATE WF-E-ACTION
              WHEN 'R'
                 MOVE 'RSVSET  '       TO WF-R-REQUEST-ID
                 PERFORM 3100-VALIDATE-FINANCIAL
              WHEN 'P'
                 MOVE 'PAYCRT  '       TO WF-R-REQUEST-ID
                 PERFORM 3100-VALIDATE-FINANCIAL
              WHEN 'A'
                 MOVE 'TXNAPR  '       TO WF-R-REQUEST-ID
                 PERFORM 3200-VALIDATE-APPROVAL
              WHEN 'J'
                 MOVE 'TXNREJ  '       TO WF-R-REQUEST-ID
                 PERFORM 3200-VALIDATE-APPROVAL
              WHEN OTHER
                 MOVE 'Action must be R, P, A or J' TO WS-FLD-MSG
                 MOVE 03               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-SFI-002 Claim number and exposure seq required
       3100-VALIDATE-FINANCIAL.
           IF WF-E-CLAIM-NUMBER = SPACES
              MOVE 'Claim number is required' TO WS-FLD-MSG
              MOVE 01                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF
           MOVE WF-E-EXPOSURE-SEQ      TO WS-NUM-TEXT
           PERFORM 5100-PARSE-INTEGER
           IF NOT WS-NUM-VALID OR WS-NUM-VALUE = 0
                               OR WS-NUM-VALUE > 999
              MOVE 'Exposure seq 1-999 is required' TO WS-FLD-MSG
              MOVE 02                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           ELSE
              MOVE WS-NUM-VALUE        TO WS-EXP-SEQ
           END-IF

      * BR-CLM-SFI-003 Cost type CLMC/EXAO/EXDC; category optional
      * (CLMFINTX books a blank category as DEFAULT), else COSTCAT code
           MOVE WF-E-COST-TYPE         TO WS-COST-TYPE
           IF NOT WS-COST-TYPE-VALID
              MOVE 'Cost type must be CLMC, EXAO or EXDC' TO WS-FLD-MSG
              MOVE 04                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF
           IF WF-E-COST-CATEGORY NOT = SPACES
              AND WF-E-COST-CATEGORY NOT = WS-DEFAULT-CATEGORY
              MOVE 'COSTCAT '          TO WS-CODE-TABLE
              MOVE WF-E-COST-CATEGORY  TO WS-CODE-VALUE
              MOVE 05                  TO WS-ERR-FIELD
              MOVE 'Cost category'     TO WS-FLD-MSG
              PERFORM 3500-CHECK-CODE
           END-IF

      * BR-CLM-SFI-004 Amount format and sign by action
           MOVE WF-E-AMOUNT            TO WS-NUM-TEXT
           MOVE 'Y'                    TO WS-NUM-ALLOW-DEC
           PERFORM 5100-PARSE-INTEGER
           MOVE 'N'                    TO WS-NUM-ALLOW-DEC
           EVALUATE TRUE
              WHEN WF-E-AMOUNT = SPACES
                 MOVE 'Amount is required' TO WS-FLD-MSG
              WHEN NOT WS-NUM-VALID
                 MOVE 'Amount must be like 1234.56 (max 999,999,999.99)
      -               ''               TO WS-FLD-MSG
              WHEN WF-E-ACTION = 'P' AND WS-NUM-AMOUNT = 0
                 MOVE 'Payment amount must be greater than zero'
                                       TO WS-FLD-MSG
              WHEN OTHER
                 MOVE SPACES           TO WS-FLD-MSG
                 MOVE WS-NUM-AMOUNT    TO WF-R-AMOUNT
           END-EVALUATE
           IF WS-FLD-MSG NOT = SPACES
              MOVE 06                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF

           IF WF-E-ACTION = 'P'
              PERFORM 3150-VALIDATE-PAYMENT
           ELSE
      * BR-CLM-SFI-005 A reserve carries no payment details
              IF WF-E-PAYEE NOT = SPACES
                 OR WF-E-PAYMENT-TYPE NOT = SPACES
                 OR WF-E-PAY-METHOD NOT = SPACES
                 OR WF-E-SCHED-DATE NOT = SPACES
                 MOVE 'Payee, payment type, method and date are for pa
      -               'yments only'    TO WS-FLD-MSG
                 MOVE 07               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR
              END-IF
           END-IF
           IF WF-E-TXN-ID NOT = SPACES
              OR WF-E-REJECT-REASON NOT = SPACES
              MOVE 'Txn id and reject reason are for A and J only'
                                       TO WS-FLD-MSG
              MOVE 10                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-SFI-008 Payment details and scheduled date window
       3150-VALIDATE-PAYMENT.
           MOVE WF-E-PAYEE             TO WS-NUM-TEXT
           PERFORM 5100-PARSE-INTEGER
           IF NOT WS-NUM-VALID OR WS-NUM-VALUE = 0
              MOVE 'Payee contact id is required (numeric)'
                                       TO WS-FLD-MSG
              MOVE 07                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           ELSE
              MOVE WS-NUM-VALUE        TO WF-R-PAYEE-ID
           END-IF
           IF WF-E-PAYMENT-TYPE = SPACES
              MOVE 'PART'              TO WF-E-PAYMENT-TYPE
           END-IF
           MOVE WF-E-PAYMENT-TYPE      TO WS-PAY-TYPE
           IF NOT WS-PAY-TYPE-VALID
              MOVE 'Payment type must be PART, FINL or SUPP'
                                       TO WS-FLD-MSG
              MOVE 08                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF
           IF WF-E-PAY-METHOD = SPACES
              MOVE 'CHEQ'              TO WF-E-PAY-METHOD
           END-IF
           MOVE WF-E-PAY-METHOD        TO WS-PAY-METHOD
           IF NOT WS-PAY-METHOD-VALID
              MOVE 'Payment method must be CHEQ, BACS or FAST'
                                       TO WS-FLD-MSG
              MOVE 09                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF

           IF WF-E-SCHED-DATE = SPACES
              MOVE WS-TODAY            TO WF-E-SCHED-DATE
           END-IF
           MOVE WF-E-SCHED-DATE        TO DTA-DT-DATE-1
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           PERFORM 8400-LINK-DATES
           EVALUATE TRUE
              WHEN WS-HAS-ERROR AND WS-LINK-FAILED
                 CONTINUE
              WHEN NOT DTA-DT-IS-VALID
                 MOVE 'Scheduled date is not a valid date (yyyy-mm-dd)'
                                       TO WS-FLD-MSG
              WHEN WF-E-SCHED-DATE < WS-TODAY
                 MOVE 'Scheduled date cannot be in the past'
                                       TO WS-FLD-MSG
              WHEN OTHER
                 MOVE WS-TODAY         TO DTA-DT-DATE-1
                 MOVE WF-E-SCHED-DATE  TO DTA-DT-DATE-2
                 MOVE 'DTDIFF  '       TO DTA-REQUEST-ID
                 PERFORM 8400-LINK-DATES
                 IF DTA-DT-RESULT-NUM > WS-MAX-SCHED-DAYS
                    MOVE 'Scheduled date at most 30 days ahead'
                                       TO WS-FLD-MSG
                 ELSE
                    MOVE SPACES        TO WS-FLD-MSG
                    MOVE WF-E-SCHED-DATE TO WF-R-SCHED-DATE
                 END-IF
           END-EVALUATE
           IF WS-FLD-MSG NOT = SPACES
              MOVE 11                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           END-IF.

      ******************************************************************
      * BR-CLM-SFI-011 Reject needs a reason; approve must not have one
       3200-VALIDATE-APPROVAL.
           MOVE WF-E-TXN-ID            TO WS-NUM-TEXT
           PERFORM 5100-PARSE-INTEGER
           IF NOT WS-NUM-VALID OR WS-NUM-VALUE = 0
              OR WS-NUM-VALUE > 999999999
              MOVE 'Txn id of the pending transaction is required'
                                       TO WS-FLD-MSG
              MOVE 10                  TO WS-ERR-FIELD
              PERFORM 3890-ERROR
           ELSE
              MOVE WS-NUM-VALUE        TO WF-R-TXN-ID
           END-IF
           IF WF-E-ACTION = 'J'
              MOVE 'TXNRJRSN'          TO WS-CODE-TABLE
              MOVE WF-E-REJECT-REASON  TO WS-CODE-VALUE
              MOVE 12                  TO WS-ERR-FIELD
              MOVE 'Reject reason'     TO WS-FLD-MSG
              PERFORM 3500-CHECK-CODE
           ELSE
              IF WF-E-REJECT-REASON NOT = SPACES
                 MOVE 'Reject reason is only used with action J'
                                       TO WS-FLD-MSG
                 MOVE 12               TO WS-ERR-FIELD
                 PERFORM 3890-ERROR
              END-IF
           END-IF.

      ******************************************************************
      * Required code: WS-FLD-MSG holds the field caption on entry
       3500-CHECK-CODE.
           MOVE WS-FLD-MSG             TO WS-CAPTION
           MOVE SPACES                 TO WS-FLD-MSG
           IF WS-CODE-VALUE = SPACES
              STRING WS-CAPTION DELIMITED BY '  '
                     ' is required' DELIMITED BY SIZE
                     INTO WS-FLD-MSG
              PERFORM 3890-ERROR
           ELSE
              PERFORM 8300-LINK-CODVL
              IF NOT WS-CODE-VALID
                 STRING WS-CAPTION DELIMITED BY '  '
                        ' ' WS-CODE-VALUE DELIMITED BY SPACE
                        ' is not a valid code' DELIMITED BY SIZE
                        INTO WS-FLD-MSG
                 PERFORM 3890-ERROR
              END-IF
           END-IF.

      * First message wins; every field in error is flagged
       3890-ERROR.
           IF WS-ERR-TEXT = SPACES
              MOVE WS-FLD-MSG          TO WS-ERR-TEXT
           END-IF
           MOVE SPACES                 TO WS-FLD-MSG
           PERFORM 3900-FLAG-FIELD.

       3900-FLAG-FIELD.
           SET WS-HAS-ERROR            TO TRUE
           EVALUATE WS-ERR-FIELD
              WHEN 01
                 MOVE -1 TO TCLMNOL
                 MOVE DFHUNIMD TO TCLMNOA
              WHEN 02
                 MOVE -1 TO TEXPSQL
                 MOVE DFHUNINT TO TEXPSQA
              WHEN 03
                 MOVE -1 TO TACTL
                 MOVE DFHUNIMD TO TACTA
              WHEN 04
                 MOVE -1 TO TCTYPEL
                 MOVE DFHUNIMD TO TCTYPEA
              WHEN 05
                 MOVE -1 TO TCCATL
                 MOVE DFHUNIMD TO TCCATA
              WHEN 06
                 MOVE -1 TO TAMTL
                 MOVE DFHUNIMD TO TAMTA
              WHEN 07
                 MOVE -1 TO TPAYEEL
                 MOVE DFHUNINT TO TPAYEEA
              WHEN 08
                 MOVE -1 TO TPTYPEL
                 MOVE DFHUNIMD TO TPTYPEA
              WHEN 09
                 MOVE -1 TO TPMTHL
                 MOVE DFHUNIMD TO TPMTHA
              WHEN 10
                 MOVE -1 TO TTXNIDL
                 MOVE DFHUNINT TO TTXNIDA
              WHEN 11
                 MOVE -1 TO TSCHEDL
                 MOVE DFHUNIMD TO TSCHEDA
              WHEN 12
                 MOVE -1 TO TRSNL
                 MOVE DFHUNIMD TO TRSNA
           END-EVALUATE.

      ******************************************************************
      * Parse WS-NUM-TEXT. Integer: digits only. Money (WS-NUM-IS-
      * MONEY): commas in the integer part, one point, 2 decimals,
      * at most 9 integer digits. Spaces only around the number.
       5100-PARSE-INTEGER.
           MOVE 0                      TO WS-NUM-VALUE WS-NUM-AMOUNT
                                          WS-INT-DIGITS WS-DEC-DIGITS
           MOVE 'Y'                    TO WS-NUM-FLAG
           SET WS-NUM-LEADING          TO TRUE
           PERFORM VARYING WS-PTR FROM 1 BY 1 UNTIL WS-PTR > 15
              MOVE WS-NUM-TEXT(WS-PTR:1) TO WS-CHAR
              EVALUATE TRUE
                 WHEN WS-CHAR IS NUMERIC AND WS-NUM-IN-DECIMALS
                    ADD 1              TO WS-DEC-DIGITS
                    IF WS-DEC-DIGITS > 2
                       MOVE 'N'        TO WS-NUM-FLAG
                    ELSE
                       COMPUTE WS-NUM-AMOUNT = WS-NUM-AMOUNT
                          + FUNCTION NUMVAL(WS-CHAR)
                          / (10 ** WS-DEC-DIGITS)
                    END-IF
                 WHEN WS-CHAR IS NUMERIC AND NOT WS-NUM-TRAILING
                    SET WS-NUM-IN-INTEGER TO TRUE
                    ADD 1              TO WS-INT-DIGITS
                    IF WS-INT-DIGITS > 14
                       MOVE 'N'        TO WS-NUM-FLAG
                    ELSE
                       COMPUTE WS-NUM-VALUE = WS-NUM-VALUE * 10
                                            + FUNCTION NUMVAL(WS-CHAR)
                    END-IF
                 WHEN WS-CHAR = ',' AND WS-NUM-IS-MONEY
                                    AND WS-NUM-IN-INTEGER
                    CONTINUE
                 WHEN WS-CHAR = '.' AND WS-NUM-IS-MONEY
                    AND (WS-NUM-IN-INTEGER OR WS-NUM-LEADING)
                    SET WS-NUM-IN-DECIMALS TO TRUE
                 WHEN WS-CHAR = SPACE OR LOW-VALUE
                    IF NOT WS-NUM-LEADING
                       SET WS-NUM-TRAILING TO TRUE
                    END-IF
                 WHEN OTHER
                    MOVE 'N'           TO WS-NUM-FLAG
              END-EVALUATE
           END-PERFORM
           IF WS-NUM-LEADING
              MOVE 'N'                 TO WS-NUM-FLAG
           END-IF
           IF WS-NUM-IS-MONEY AND WS-NUM-VALID
              IF WS-INT-DIGITS > 9
                 MOVE 'N'              TO WS-NUM-FLAG
              ELSE
                 ADD WS-NUM-VALUE      TO WS-NUM-AMOUNT
              END-IF
           END-IF.

      ******************************************************************
       5200-FORMAT-AMOUNT.
           IF WS-AMT-IN > 999999999.99 OR WS-AMT-IN < -999999999.99
              MOVE '  ***********  '   TO WS-AMT-OUT
           ELSE
              MOVE WS-AMT-IN           TO WS-AMT-EDIT
              MOVE WS-AMT-EDIT         TO WS-AMT-OUT
           END-IF.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(CLMFTMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
              INSPECT CLMFTMI REPLACING ALL LOW-VALUES BY SPACES
           ELSE
              MOVE SPACES              TO CLMFTMI
           END-IF
           INITIALIZE WS-SCREEN-STATE
           MOVE TCLMNOI                TO WF-E-CLAIM-NUMBER
           MOVE TEXPSQI                TO WF-E-EXPOSURE-SEQ
           MOVE TACTI                  TO WF-E-ACTION
           MOVE TCTYPEI                TO WF-E-COST-TYPE
           MOVE TCCATI                 TO WF-E-COST-CATEGORY
           MOVE TAMTI                  TO WF-E-AMOUNT
           MOVE TPAYEEI                TO WF-E-PAYEE
           MOVE TPTYPEI                TO WF-E-PAYMENT-TYPE
           MOVE TPMTHI                 TO WF-E-PAY-METHOD
           MOVE TSCHEDI                TO WF-E-SCHED-DATE
           MOVE TTXNIDI                TO WF-E-TXN-ID
           MOVE TRSNI                  TO WF-E-REJECT-REASON
           INSPECT WF-E-CLAIM-NUMBER  CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WF-E-ACTION        CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WF-E-COST-TYPE     CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WF-E-COST-CATEGORY CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WF-E-PAYMENT-TYPE  CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WF-E-PAY-METHOD    CONVERTING WS-LOWER TO WS-UPPER
           INSPECT WF-E-REJECT-REASON CONVERTING WS-LOWER TO WS-UPPER
           MOVE LOW-VALUES             TO CLMFTMO.

      ******************************************************************
      * Normalised entry back to the screen (defaulted date included)
       6200-ENTRY-TO-SCREEN.
           MOVE WF-E-CLAIM-NUMBER      TO TCLMNOO
           MOVE WF-E-EXPOSURE-SEQ      TO TEXPSQO
           MOVE WF-E-ACTION            TO TACTO
           MOVE WF-E-COST-TYPE         TO TCTYPEO
           MOVE WF-E-COST-CATEGORY     TO TCCATO
           MOVE WF-E-AMOUNT            TO TAMTO
           MOVE WF-E-PAYEE             TO TPAYEEO
           MOVE WF-E-PAYMENT-TYPE      TO TPTYPEO
           MOVE WF-E-PAY-METHOD        TO TPMTHO
           MOVE WF-E-SCHED-DATE        TO TSCHEDO
           MOVE WF-E-TXN-ID            TO TTXNIDO
           MOVE WF-E-REJECT-REASON     TO TRSNO.

      ******************************************************************
       6100-SEND-MAP.
           MOVE WS-TODAY               TO TDATEO
           IF WS-SEND-ERASE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMFTMO) ERASE CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           ELSE
              EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                        FROM(CLMFTMO) DATAONLY CURSOR FREEKB
                        RESP(WS-RESP)
              END-EXEC
           END-IF.

      ******************************************************************
       6300-SEND-ERROR.
           MOVE WS-ERR-TEXT            TO TMSGO
           MOVE DFHBMASB               TO TMSGA
           IF WS-WARN-TEXT NOT = SPACES
              MOVE WS-WARN-TEXT        TO TWRNO
           END-IF
           IF TCLMNOL NOT = -1 AND TEXPSQL NOT = -1 AND TACTL NOT = -1
              AND TCTYPEL NOT = -1 AND TCCATL NOT = -1
              AND TAMTL NOT = -1 AND TPAYEEL NOT = -1
              AND TPTYPEL NOT = -1 AND TPMTHL NOT = -1
              AND TSCHEDL NOT = -1 AND TTXNIDL NOT = -1
              AND TRSNL NOT = -1
              MOVE -1                  TO TACTL
           END-IF
           SET WS-SEND-DATAONLY        TO TRUE
           PERFORM 6100-SEND-MAP.

      ******************************************************************
       6400-SET-MODE-TEXT.
           EVALUATE TRUE
              WHEN TF-STEP-POSTED
                 MOVE 'Posted - result below' TO TMODEO
                 MOVE 'ENTER=New transaction  PF3=Menu  CLEAR=Restart'
                                       TO TPFKO
              WHEN TF-STEP-CONFIRM
                 MOVE 'Confirm: PF5 posts the transaction shown'
                                       TO TMODEO
                 MOVE 'PF5=Post  PF12=Change  PF3=Menu' TO TPFKO
              WHEN OTHER
                 MOVE 'Entry: R/P need claim + exposure, A/J a txn id'
                                       TO TMODEO
                 MOVE 'ENTER=Validate  PF3=Menu  CLEAR=Restart'
                                       TO TPFKO
           END-EVALUATE.

      ******************************************************************
       8100-LINK-CLMINQ.
           INITIALIZE WS-CLM-CA
           MOVE 'CLMINQ  '             TO CLA-REQUEST-ID
           MOVE WS-USERID              TO CLA-USER-ID
           MOVE WS-TRACE-ID            TO CLA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CLA-CALLER-PGM
           MOVE WF-E-CLAIM-NUMBER      TO CLA-CL-CLAIM-NUMBER
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
                 WHEN CLA-RETURN-CODE >= 90
                    MOVE CLA-MESSAGE   TO WS-ERR-TEXT
                    PERFORM 8900-BACK-OUT
                 WHEN CLA-RC-NOT-FOUND
                    STRING 'Claim ' WF-E-CLAIM-NUMBER ' not found'
                           DELIMITED BY SIZE INTO WS-FLD-MSG
                    MOVE 01            TO WS-ERR-FIELD
                    PERFORM 3890-ERROR
                 WHEN OTHER
                    STRING CLA-REASON-CODE ' ' CLA-MESSAGE
                           DELIMITED BY SIZE INTO WS-FLD-MSG
                    MOVE 01            TO WS-ERR-FIELD
                    PERFORM 3890-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
      * Post the confirmed request (RSVSET PAYCRT TXNAPR TXNREJ)
       8200-LINK-FINTX.
           INITIALIZE WS-FIN-CA
           MOVE TF-R-REQUEST-ID        TO FTA-REQUEST-ID
           MOVE TF-R-CLAIM-ID          TO FTA-FT-CLAIM-ID
           MOVE TF-R-EXPOSURE-ID       TO FTA-FT-EXPOSURE-ID
           MOVE TF-R-TXN-ID            TO FTA-FT-TXN-ID
           EVALUATE TF-E-ACTION
              WHEN 'R'
              WHEN 'P'
                 MOVE TF-E-COST-TYPE   TO FTA-FT-COST-TYPE
                 MOVE TF-E-COST-CATEGORY TO FTA-FT-COST-CATEGORY
                 MOVE TF-R-AMOUNT      TO FTA-FT-AMOUNT
                 IF TF-E-ACTION = 'P'
                    MOVE TF-E-PAYMENT-TYPE TO FTA-FT-PAYMENT-TYPE
                    MOVE TF-R-PAYEE-ID TO FTA-FT-PAYEE-CONTACT-ID
                    MOVE TF-E-PAY-METHOD TO FTA-FT-PAY-METHOD
                    MOVE TF-R-SCHED-DATE TO FTA-FT-SCHEDULED-DATE
                 END-IF
              WHEN 'J'
                 MOVE TF-E-REJECT-REASON TO FTA-FT-REASON
           END-EVALUATE
           PERFORM 8250-LINK-FINTX-CALL
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN FTA-RC-OK
                 WHEN FTA-RC-WARNING
                    CONTINUE
      * BR-CLM-SFI-015 RC >= 90 backs out the unit of work
                 WHEN FTA-RETURN-CODE >= 90
                    MOVE FTA-MESSAGE   TO WS-ERR-TEXT
                    PERFORM 8900-BACK-OUT
      * RC 20: NOAPPRVR (nobody above can approve), SELFAPRV, NOAUTH
                 WHEN FTA-RC-AUTHORITY
                    SET TF-STEP-ENTRY  TO TRUE
                    SET WS-HAS-ERROR   TO TRUE
                    STRING 'Not authorised (' FTA-REASON-CODE '): '
                           FTA-MESSAGE
                           DELIMITED BY SIZE INTO WS-ERR-TEXT
                 WHEN OTHER
                    SET TF-STEP-ENTRY  TO TRUE
                    SET WS-HAS-ERROR   TO TRUE
                    STRING FTA-REASON-CODE ' ' FTA-MESSAGE
                           DELIMITED BY SIZE INTO WS-ERR-TEXT
              END-EVALUATE
           END-IF.

       8250-LINK-FINTX-CALL.
           MOVE WS-USERID              TO FTA-USER-ID
           MOVE WS-TRACE-ID            TO FTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO FTA-CALLER-PGM
           MOVE WS-FIN-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-FIN-PGM)
                     COMMAREA(WS-FIN-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK.

      ******************************************************************
      * Pending transaction for approve / reject (read only)
       8300-LINK-TXNINQ.
           INITIALIZE WS-FIN-CA
           MOVE 'TXNINQ  '             TO FTA-REQUEST-ID
           MOVE WF-R-TXN-ID            TO FTA-FT-TXN-ID
           PERFORM 8250-LINK-FINTX-CALL
           IF WS-LINK-OK
              EVALUATE TRUE
                 WHEN FTA-RC-OK
                 WHEN FTA-RC-WARNING
                    MOVE FTA-FT-CLAIM-ID TO WF-R-CLAIM-ID
                    MOVE FTA-FT-EXPOSURE-ID TO WF-R-EXPOSURE-ID
                 WHEN FTA-RETURN-CODE >= 90
                    MOVE FTA-MESSAGE   TO WS-ERR-TEXT
                    PERFORM 8900-BACK-OUT
                 WHEN FTA-RC-NOT-FOUND
                    MOVE 'Transaction not found' TO WS-FLD-MSG
                    MOVE 10            TO WS-ERR-FIELD
                    PERFORM 3890-ERROR
                 WHEN OTHER
                    STRING FTA-REASON-CODE ' ' FTA-MESSAGE
                           DELIMITED BY SIZE INTO WS-FLD-MSG
                    MOVE 10            TO WS-ERR-FIELD
                    PERFORM 3890-ERROR
              END-EVALUATE
           END-IF.

      ******************************************************************
      * Code check; a missing code row is simply "not valid"
       8300-LINK-CODVL.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE WS-USERID              TO CDA-USER-ID
           MOVE WS-TRACE-ID            TO CDA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE WS-CODE-TABLE          TO CDA-CD-TABLE
           MOVE WS-CODE-VALUE          TO CDA-CD-CODE
           MOVE 'N'                    TO WS-CODE-FLAG
           MOVE WS-COD-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-COD-PGM)
                     COMMAREA(WS-COD-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK AND CDA-RC-OK AND CDA-CD-IS-VALID
              MOVE 'Y'                 TO WS-CODE-FLAG
           END-IF.

      ******************************************************************
       8400-LINK-DATES.
           MOVE WS-USERID              TO DTA-USER-ID
           MOVE WS-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE WS-DTE-PGM             TO WS-LINK-PGM
           EXEC CICS LINK PROGRAM(WS-DTE-PGM)
                     COMMAREA(WS-DTE-CA) LENGTH(4000)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC
           PERFORM 8800-CHECK-LINK
           IF WS-LINK-OK AND NOT DTA-RC-OK
              MOVE 'N'                 TO DTA-DT-VALID
           END-IF.

      ******************************************************************
       8800-CHECK-LINK.
           IF WS-RESP = DFHRESP(NORMAL)
              SET WS-LINK-OK           TO TRUE
           ELSE
              SET WS-LINK-FAILED       TO TRUE
              MOVE WS-RESP             TO WS-RESP-DISP
              MOVE SPACES              TO WS-ERR-TEXT
              STRING 'Service ' WS-LINK-PGM
                     ' unavailable (CICS RESP ' WS-RESP-DISP
                     ') - try again later'
                     DELIMITED BY SIZE INTO WS-ERR-TEXT
              PERFORM 8950-LOG-CICS-ERROR
              PERFORM 8900-BACK-OUT
           END-IF.

      ******************************************************************
      * BR-CLM-SFI-015 Back out everything done in this task
       8900-BACK-OUT.
           SET WS-HAS-ERROR            TO TRUE
           SET TF-STEP-ENTRY           TO TRUE
           EXEC CICS SYNCPOINT ROLLBACK RESP(WS-RESP) END-EXEC
           MOVE WS-ERR-TEXT            TO WS-FLD-MSG
           MOVE SPACES                 TO WS-ERR-TEXT
           STRING 'Backed out: ' WS-FLD-MSG DELIMITED BY SIZE
                  INTO WS-ERR-TEXT.

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
           MOVE WS-ERR-TEXT            TO ERA-ER-TEXT
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
