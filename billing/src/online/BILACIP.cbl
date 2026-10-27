       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILACIP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILACIP                                          *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * FUNCTION    : Account summary inquiry (transaction BACI, map   *
      *               BILACM). Looks the account up by number with     *
      *               BILACCT0 ACCTINQ, refreshes its balance buckets  *
      *               with ACCTBAL and shows status, collection        *
      *               status, balances, payer contact and the policy   *
      *               periods (4 per page, PF7/PF8). PF4 and PF5 carry *
      *               the account to the invoice list and payment      *
      *               entry screens.                                   *
      * REQUESTS    : n/a (screen) - LINKs BILACCT0 ACCTINQ, ACCTBAL   *
      * CALLED BY   : terminal (BACI), XCTL from BILMNUP               *
      * CALLS       : LINK BILACCT0 BILERRLG;                          *
      *               XCTL BILMNUP BILIVMP BILPAYP                     *
      * TABLES      : none (all data through BILACCT0)                 *
      * RULES       : BR-BIL-ACI-001 Account number is required, 10    *
      *                              characters without blanks         *
      *               BR-BIL-ACI-002 Balances are recomputed from open *
      *                              items (ACCTBAL) on every inquiry; *
      *                              if that fails the stored buckets  *
      *                              are shown with a warning          *
      *               BR-BIL-ACI-003 Net owed = unbilled + billed +    *
      *                              due - unapplied                   *
      *               BR-BIL-ACI-004 Collection status other than NORM *
      *                              and a closed account are flagged  *
      *               BR-BIL-ACI-005 Payer name: person = first + last *
      *                              name, company = company name      *
      *               BR-BIL-ACI-006 Policy periods page 4 at a time   *
      *               BR-BIL-ACI-007 PF4 / PF5 need an account on the  *
      *                              screen                            *
      *               BR-BIL-ACI-008 Service RC >= 90 rolls back the   *
      *                              task (SYNCPOINT ROLLBACK)         *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILACIP'.
       01  WS-TRAN-ID                  PIC X(4)  VALUE 'BACI'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'BILMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'BILACM'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-ROWS-PER-PAGE            PIC S9(4) COMP VALUE 4.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP.
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-TARGET-PGM            PIC X(8).
           05 WS-BLANKS                PIC S9(4) COMP.
           05 WS-ERR-TEXT              PIC X(79).
           05 WS-MSG                   PIC X(79).
           05 WS-INQ-OK                PIC X     VALUE 'N'.
              88 INQ-OK                 VALUE 'Y'.
           05 WS-PAGES                 PIC S9(4) COMP.
           05 WS-FIRST-ROW             PIC S9(4) COMP.
           05 WS-PX                    PIC S9(4) COMP.
           05 WS-ROW                   PIC S9(4) COMP.
           05 WS-NET                   PIC S9(11)V99 COMP-3.
           05 WS-AMT-EDIT              PIC -ZZZ,ZZZ,ZZ9.99.
           05 WS-NUM-EDIT              PIC Z(8)9.
           05 WS-PAGE-EDIT             PIC Z9.
           05 WS-PAGES-EDIT            PIC Z9.
           05 WS-COUNT-EDIT            PIC Z9.

       01  WS-PERIOD-LINE.
           05 WPL-PP-ID                PIC Z(8)9.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 WPL-POL-NUM              PIC Z(8)9.
           05 FILLER                   PIC XX    VALUE SPACES.
           05 WPL-PRODUCT              PIC X.
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 WPL-STATUS               PIC X(4).
           05 FILLER                   PIC XXX   VALUE SPACES.
           05 WPL-EXP-DATE             PIC X(10).
           05 FILLER                   PIC XX    VALUE SPACES.
           05 WPL-NOTE                 PIC X(24).

       01  WS-SCR-CA.
           COPY BILMNUC1.

       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.

       01  WS-BAL-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ABA-==.
           03 ABA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ABA-==.

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
              WHEN EIBAID = DFHPF3
                 MOVE 'BILMNUP'        TO WS-TARGET-PGM
                 PERFORM 8900-XCTL
              WHEN EIBAID = DFHCLEAR
                 MOVE SPACES           TO SCR-ACCOUNT-NUMBER
                 MOVE 0                TO SCR-ACCOUNT-ID
                 PERFORM 2000-FIRST-DISPLAY
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-PROCESS-ENTER
              WHEN EIBAID = DFHPF7
              WHEN EIBAID = DFHPF8
                 PERFORM 2200-PAGE-PERIODS
              WHEN EIBAID = DFHPF4
              WHEN EIBAID = DFHPF5
                 PERFORM 2300-NAVIGATE
              WHEN OTHER
                 PERFORM 6000-RECEIVE-MAP
                 MOVE 'That key is not active on this screen'
                                       TO WS-MSG
                 PERFORM 7200-SEND-MESSAGE
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
           MOVE SPACES                 TO WS-MSG
           MOVE LOW-VALUES             TO BILACMO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
                     RESP(WS-RESP)
           END-EXEC.

      ******************************************************************
      * Entered from the menu (account may be carried) or fresh
       2000-FIRST-DISPLAY.
           MOVE 1                      TO SCR-PAGE-NO
           IF SCR-ACCOUNT-NUMBER NOT = SPACES
              PERFORM 2500-INQUIRE
           ELSE
              MOVE 'Enter an account number and press ENTER'
                                       TO WS-MSG
              MOVE -1                  TO ACMACCTL
              PERFORM 7000-SEND-MAP
           END-IF.

      ******************************************************************
       2100-PROCESS-ENTER.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3000-VALIDATE-ACCOUNT
           MOVE 1                      TO SCR-PAGE-NO
           PERFORM 2500-INQUIRE.

      ******************************************************************
      * BR-BIL-ACI-006 Four periods per page
       2200-PAGE-PERIODS.
           PERFORM 6000-RECEIVE-MAP
           IF SCR-ACCOUNT-NUMBER = SPACES OR SCR-ROW-COUNT = 0
              MOVE 'No policy periods to page through' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-PAGES = (SCR-ROW-COUNT + WS-ROWS-PER-PAGE - 1)
                            / WS-ROWS-PER-PAGE
           IF EIBAID = DFHPF7
              IF SCR-PAGE-NO <= 1
                 MOVE 'Already at the first page of periods' TO WS-MSG
                 PERFORM 7200-SEND-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              SUBTRACT 1               FROM SCR-PAGE-NO
           ELSE
              IF SCR-PAGE-NO >= WS-PAGES
                 MOVE 'Already at the last page of periods' TO WS-MSG
                 PERFORM 7200-SEND-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              ADD 1                    TO SCR-PAGE-NO
           END-IF
           PERFORM 2500-INQUIRE.

      ******************************************************************
      * BR-BIL-ACI-007 Navigation carries an inquired account only
       2300-NAVIGATE.
           PERFORM 6000-RECEIVE-MAP
           IF SCR-ACCOUNT-ID = 0
              MOVE 'Inquire on an account before PF4 / PF5' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-FROM-PGM
           MOVE 1                      TO SCR-PAGE-NO
           IF EIBAID = DFHPF4
              MOVE 'BILIVMP'           TO WS-TARGET-PGM
           ELSE
              MOVE 'BILPAYP'           TO WS-TARGET-PGM
           END-IF
           PERFORM 8900-XCTL.

      ******************************************************************
       2500-INQUIRE.
           MOVE LOW-VALUES             TO BILACMO
           PERFORM 8000-LINK-ACCTINQ
           IF NOT INQ-OK
              MOVE 0                   TO SCR-ACCOUNT-ID SCR-ROW-COUNT
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE ACA-AC-ACCOUNT-ID      TO SCR-ACCOUNT-ID
           MOVE ACA-AC-PERIOD-COUNT    TO SCR-ROW-COUNT
           PERFORM 8100-LINK-ACCTBAL
           PERFORM 5000-FORMAT-ACCOUNT
           PERFORM 5100-FORMAT-BALANCES
           PERFORM 5200-FORMAT-CONTACT
           PERFORM 5300-FORMAT-PERIODS
           IF WS-MSG = SPACES
              MOVE 'Account displayed - PF4 invoices, PF5 payment'
                                       TO WS-MSG
           END-IF
           MOVE -1                     TO ACMACCTL
           PERFORM 7000-SEND-MAP.

      ******************************************************************
      * BR-BIL-ACI-001 Account number required, no embedded blanks
       3000-VALIDATE-ACCOUNT.
           IF ACMACCTL > 0
              MOVE ACMACCTI            TO SCR-ACCOUNT-NUMBER
           ELSE
              IF ACMACCTF = DFHBMEOF
                 MOVE SPACES           TO SCR-ACCOUNT-NUMBER
              END-IF
           END-IF
           MOVE 0                      TO SCR-ACCOUNT-ID
           IF SCR-ACCOUNT-NUMBER = SPACES
              MOVE 'Account number is required' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-BLANKS
           INSPECT SCR-ACCOUNT-NUMBER TALLYING WS-BLANKS
                   FOR ALL SPACES
           IF WS-BLANKS > 0
              MOVE 'Account number is 10 characters, no blanks'
                                       TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-BIL-ACI-004 Flag closed accounts and collections
       5000-FORMAT-ACCOUNT.
           MOVE ACA-AC-ACCOUNT-NUMBER  TO ACMACCTO
           MOVE ACA-AC-ACCOUNT-NAME    TO ACMNAMEO
           MOVE ACA-AC-ACCOUNT-TYPE    TO ACMTYPEO
           MOVE ACA-AC-STATUS          TO ACMSTATO
           MOVE ACA-AC-COLLECTION-STATUS TO ACMCOLLO
           MOVE ACA-AC-DELINQ-PLAN     TO ACMPLANO
           MOVE ACA-AC-BILLING-LEVEL   TO ACMLEVLO
           MOVE ACA-AC-INVOICE-DAY     TO WS-PAGE-EDIT
           MOVE WS-PAGE-EDIT           TO ACMIDAYO
           MOVE ACA-AC-AUTOPAY-FLAG    TO ACMAUTOO
           MOVE ACA-AC-DELIVERY-METHOD TO ACMDLVYO
           MOVE ACA-AC-OPEN-DATE       TO ACMOPENO
           IF ACA-AC-COLLECTION-STATUS NOT = 'NORM'
              MOVE DFHBMBRY            TO ACMCOLLA
              IF WS-MSG = SPACES
                 STRING 'Account is in collection status '
                        ACA-AC-COLLECTION-STATUS
                        DELIMITED BY SIZE INTO WS-MSG
              END-IF
           END-IF
           IF ACA-AC-STATUS NOT = 'ACTV'
              MOVE DFHBMBRY            TO ACMSTATA
              MOVE SPACES              TO WS-MSG
              STRING 'Account status is ' ACA-AC-STATUS
                     ' - no new billing activity'
                     DELIMITED BY SIZE INTO WS-MSG
           END-IF.

      ******************************************************************
      * BR-BIL-ACI-002 Use recomputed buckets when ACCTBAL succeeded
      * BR-BIL-ACI-003 Net owed
       5100-FORMAT-BALANCES.
           IF ABA-RC-OK OR ABA-RC-WARNING
              MOVE ABA-AC-UNBILLED-AMT TO ACA-AC-UNBILLED-AMT
              MOVE ABA-AC-BILLED-AMT   TO ACA-AC-BILLED-AMT
              MOVE ABA-AC-DUE-AMT      TO ACA-AC-DUE-AMT
              MOVE ABA-AC-UNAPPLIED-AMT TO ACA-AC-UNAPPLIED-AMT
           END-IF
           MOVE ACA-AC-UNBILLED-AMT    TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO ACMUNBLO
           MOVE ACA-AC-BILLED-AMT      TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO ACMBILDO
           MOVE ACA-AC-DUE-AMT         TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO ACMDUEO
           MOVE ACA-AC-UNAPPLIED-AMT   TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO ACMUNAPO
           COMPUTE WS-NET = ACA-AC-UNBILLED-AMT + ACA-AC-BILLED-AMT
                          + ACA-AC-DUE-AMT - ACA-AC-UNAPPLIED-AMT
           MOVE WS-NET                 TO WS-AMT-EDIT
           MOVE WS-AMT-EDIT            TO ACMNETO
           IF ACA-AC-DUE-AMT > 0
              MOVE DFHBMBRY            TO ACMDUEA
           END-IF.

      ******************************************************************
      * BR-BIL-ACI-005 Payer display name by contact type
       5200-FORMAT-CONTACT.
           MOVE SPACES                 TO ACMCNAMO
           IF ACA-AC-CONTACT-TYPE = 'C'
              MOVE ACA-AC-COMPANY-NAME TO ACMCNAMO
           ELSE
              STRING ACA-AC-FIRST-NAME DELIMITED BY '  '
                     ' '               DELIMITED BY SIZE
                     ACA-AC-LAST-NAME  DELIMITED BY '  '
                     INTO ACMCNAMO
           END-IF
           MOVE ACA-AC-ADDR-LINE1      TO ACMADR1O
           MOVE ACA-AC-ADDR-LINE2      TO ACMADR2O
           MOVE ACA-AC-CITY            TO ACMCITYO
           MOVE ACA-AC-POSTCODE        TO ACMPOSTO
           MOVE ACA-AC-EMAIL           TO ACMMAILO
           MOVE ACA-AC-PHONE           TO ACMPHONO.

      ******************************************************************
       5300-FORMAT-PERIODS.
           MOVE SPACES                 TO ACMPAGEO
           IF ACA-AC-PERIOD-COUNT = 0
              MOVE 'No policy periods'  TO ACMPLNO(1)
           ELSE
              COMPUTE WS-PAGES = (ACA-AC-PERIOD-COUNT
                                 + WS-ROWS-PER-PAGE - 1)
                               / WS-ROWS-PER-PAGE
              IF SCR-PAGE-NO > WS-PAGES OR SCR-PAGE-NO < 1
                 MOVE 1                TO SCR-PAGE-NO
              END-IF
              COMPUTE WS-FIRST-ROW = (SCR-PAGE-NO - 1)
                                   * WS-ROWS-PER-PAGE + 1
              PERFORM VARYING WS-ROW FROM 1 BY 1
                      UNTIL WS-ROW > WS-ROWS-PER-PAGE
                 COMPUTE WS-PX = WS-FIRST-ROW + WS-ROW - 1
                 IF WS-PX <= ACA-AC-PERIOD-COUNT
                    PERFORM 5350-FORMAT-PERIOD-ROW
                 END-IF
              END-PERFORM
              MOVE SCR-PAGE-NO         TO WS-PAGE-EDIT
              MOVE WS-PAGES            TO WS-PAGES-EDIT
              MOVE ACA-AC-PERIOD-COUNT TO WS-COUNT-EDIT
              STRING 'Page ' WS-PAGE-EDIT ' of ' WS-PAGES-EDIT
                     ' (' WS-COUNT-EDIT ')'
                     DELIMITED BY SIZE INTO ACMPAGEO
           END-IF.

      ******************************************************************
       5350-FORMAT-PERIOD-ROW.
           MOVE ACA-AC-PP-ID(WS-PX)    TO WPL-PP-ID
           MOVE ACA-AC-PP-POL-NUM(WS-PX) TO WPL-POL-NUM
           MOVE ACA-AC-PP-PRODUCT(WS-PX) TO WPL-PRODUCT
           MOVE ACA-AC-PP-STATUS(WS-PX) TO WPL-STATUS
           MOVE ACA-AC-PP-EXP-DATE(WS-PX) TO WPL-EXP-DATE
           MOVE SPACES                 TO WPL-NOTE
           IF ACA-AC-PP-STATUS(WS-PX) = 'INFC'
              AND ACA-AC-PP-EXP-DATE(WS-PX) < WS-TODAY
              MOVE 'EXPIRED - NOT RENEWED' TO WPL-NOTE
           END-IF
           MOVE WS-PERIOD-LINE         TO ACMPLNO(WS-ROW).

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(BILACMI)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
              WHEN DFHRESP(NORMAL)
                 CONTINUE
              WHEN DFHRESP(MAPFAIL)
                 MOVE LOW-VALUES       TO BILACMI
              WHEN OTHER
                 MOVE '6000-RECEIVE-MAP' TO ERC-ER-PARAGRAPH
                 PERFORM 9800-CICS-ERROR
           END-EVALUATE.

      ******************************************************************
       7000-SEND-MAP.
           MOVE WS-TODAY               TO ACMDATEO
           MOVE WS-MSG                 TO ACMMSGO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILACMO) ERASE CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7000-SEND-MAP'     TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-OWNER-PGM.

      ******************************************************************
      * Message only: keep what is on the screen, cursor to account
       7200-SEND-MESSAGE.
           MOVE LOW-VALUES             TO BILACMO
           MOVE -1                     TO ACMACCTL
           IF SCR-OWNER-PGM NOT = WS-PROGRAM-ID
              MOVE SCR-ACCOUNT-NUMBER  TO ACMACCTO
              PERFORM 7000-SEND-MAP
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-MSG                 TO ACMMSGO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILACMO) DATAONLY CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7200-SEND-MESSAGE' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-OWNER-PGM.

      ******************************************************************
       8000-LINK-ACCTINQ.
           MOVE 'N'                    TO WS-INQ-OK
           INITIALIZE WS-ACT-CA
           MOVE 'ACCTINQ '             TO ACA-REQUEST-ID
           MOVE SCR-USER-ID            TO ACA-USER-ID
           MOVE WS-PROGRAM-ID          TO ACA-CALLER-PGM
           MOVE EIBTRNID               TO ACA-TRACE-ID
           MOVE SCR-ACCOUNT-NUMBER     TO ACA-AC-ACCOUNT-NUMBER
           EXEC CICS LINK PROGRAM(WS-ACT-PGM)
                     COMMAREA(WS-ACT-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8000-LINK-ACCTINQ' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           EVALUATE TRUE
              WHEN ACA-RC-OK
              WHEN ACA-RC-WARNING
                 SET INQ-OK            TO TRUE
              WHEN ACA-RC-NOT-FOUND
                 STRING 'Account ' SCR-ACCOUNT-NUMBER ' not found'
                        DELIMITED BY SIZE INTO WS-MSG
              WHEN ACA-RETURN-CODE >= 90
                 PERFORM 8800-ROLLBACK
                 MOVE ACA-MESSAGE      TO WS-MSG
              WHEN OTHER
                 MOVE ACA-MESSAGE      TO WS-MSG
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ACI-002 Recompute balances; failure is only a warning
       8100-LINK-ACCTBAL.
           INITIALIZE WS-BAL-CA
           MOVE 'ACCTBAL '             TO ABA-REQUEST-ID
           MOVE SCR-USER-ID            TO ABA-USER-ID
           MOVE WS-PROGRAM-ID          TO ABA-CALLER-PGM
           MOVE EIBTRNID               TO ABA-TRACE-ID
           MOVE SCR-ACCOUNT-ID         TO ABA-AC-ACCOUNT-ID
           EXEC CICS LINK PROGRAM(WS-ACT-PGM)
                     COMMAREA(WS-BAL-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8100-LINK-ACCTBAL' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           EVALUATE TRUE
              WHEN ABA-RC-OK
                 CONTINUE
              WHEN ABA-RC-WARNING
                 MOVE ABA-MESSAGE      TO WS-MSG
              WHEN ABA-RETURN-CODE >= 90
                 PERFORM 8800-ROLLBACK
                 MOVE 'Balances not refreshed - stored values shown'
                                       TO WS-MSG
              WHEN OTHER
                 MOVE SPACES           TO WS-MSG
                 STRING 'Stored balances shown: ' ABA-MESSAGE
                        DELIMITED BY SIZE INTO WS-MSG
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-ACI-008 Db2 / CICS failures in a service roll back
       8800-ROLLBACK.
           EXEC CICS SYNCPOINT ROLLBACK
                     RESP(WS-RESP)
           END-EXEC.

      ******************************************************************
       8900-XCTL.
           EXEC CICS XCTL PROGRAM(WS-TARGET-PGM)
                     COMMAREA(WS-SCR-CA)
                     LENGTH(LENGTH OF WS-SCR-CA)
                     RESP(WS-RESP)
           END-EXEC
           MOVE '8900-XCTL'            TO ERC-ER-PARAGRAPH
           PERFORM 9850-LOG-CICS-ERROR
           MOVE SPACES                 TO WS-MSG
           STRING 'Screen ' WS-TARGET-PGM ' is not available'
                  DELIMITED BY SIZE INTO WS-MSG
           PERFORM 7200-SEND-MESSAGE.

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
           STRING 'BILACIP CICS error in ' ERC-ER-PARAGRAPH
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
