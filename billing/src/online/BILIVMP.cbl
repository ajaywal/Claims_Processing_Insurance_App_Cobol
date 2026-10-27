       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILIVMP.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILIVMP                                          *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : B - CICS front end, pseudo-conversational        *
      * FUNCTION    : Invoice list (transaction BIVL, map BILIVM).     *
      *               Resolves the account number with BILACCT0        *
      *               ACCTINQ, lists the account's newest invoices     *
      *               with BILINVSV INVLST, 12 rows a page with        *
      *               PF7/PF8, flags overdue invoices and shows the    *
      *               outstanding balance. PF5 carries the account to  *
      *               payment entry.                                   *
      * REQUESTS    : n/a (screen) - LINKs BILACCT0 ACCTINQ,           *
      *               BILINVSV INVLST                                  *
      * CALLED BY   : terminal (BIVL), XCTL from BILMNUP / BILACIP     *
      * CALLS       : LINK BILACCT0 BILINVSV BILERRLG;                 *
      *               XCTL BILMNUP BILPAYP                             *
      * TABLES      : none (all data through services)                 *
      * RULES       : BR-BIL-IVL-001 Account number is required, 10    *
      *                              characters without blanks         *
      *               BR-BIL-IVL-002 12 invoices per page, newest      *
      *                              first; PF7 / PF8 stop at the ends *
      *               BR-BIL-IVL-003 Only the newest 20 invoices are   *
      *                              returned by INVLST; the last page *
      *                              says so when the limit is hit     *
      *               BR-BIL-IVL-004 An invoice past its due date with *
      *                              an unpaid amount, not planned or  *
      *                              cancelled, is shown as OVERDUE    *
      *               BR-BIL-IVL-005 Outstanding = account balance     *
      *                              returned by INVLST                *
      *               BR-BIL-IVL-006 Service RC >= 90 rolls back the   *
      *                              task (SYNCPOINT ROLLBACK)         *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILIVMP'.
       01  WS-TRAN-ID                  PIC X(4)  VALUE 'BIVL'.
       01  WS-MAPSET                   PIC X(8)  VALUE 'BILMSET'.
       01  WS-MAP                      PIC X(8)  VALUE 'BILIVM'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'BILACCT0'.
       01  WS-INV-PGM                  PIC X(8)  VALUE 'BILINVSV'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.
       01  WS-ROWS-PER-PAGE            PIC S9(4) COMP VALUE 12.
       01  WS-INVLST-LIMIT             PIC S9(4) COMP VALUE 20.

       01  WS-WORK.
           05 WS-RESP                  PIC S9(8) COMP.
           05 WS-ABSTIME               PIC S9(15) COMP-3.
           05 WS-TODAY                 PIC X(10).
           05 WS-TARGET-PGM            PIC X(8).
           05 WS-BLANKS                PIC S9(4) COMP.
           05 WS-ERR-TEXT              PIC X(79).
           05 WS-MSG                   PIC X(79).
           05 WS-PAGES                 PIC S9(4) COMP.
           05 WS-FIRST-ROW             PIC S9(4) COMP.
           05 WS-RX                    PIC S9(4) COMP.
           05 WS-ROW                   PIC S9(4) COMP.
           05 WS-OVERDUE               PIC S9(4) COMP.
           05 WS-UNPAID                PIC S9(11)V99 COMP-3.
           05 WS-AMT-EDIT              PIC -ZZZ,ZZZ,ZZ9.99.
           05 WS-PAGE-EDIT             PIC Z9.
           05 WS-PAGES-EDIT            PIC Z9.
           05 WS-COUNT-EDIT            PIC Z9.
           05 WS-LIST-OK               PIC X.
              88 LIST-OK                VALUE 'Y'.

       01  WS-INVOICE-LINE.
           05 WIL-NUMBER               PIC X(12).
           05 FILLER                   PIC XX    VALUE SPACES.
           05 WIL-BILL-DATE            PIC X(10).
           05 FILLER                   PIC X     VALUE SPACE.
           05 WIL-DUE-DATE             PIC X(10).
           05 FILLER                   PIC X     VALUE SPACE.
           05 WIL-AMOUNT               PIC ZZZZ,ZZ9.99-.
           05 FILLER                   PIC X     VALUE SPACE.
           05 WIL-PAID                 PIC ZZZZ,ZZ9.99-.
           05 FILLER                   PIC X     VALUE SPACE.
           05 WIL-STATUS               PIC X(4).
           05 FILLER                   PIC X     VALUE SPACE.
           05 WIL-DESC                 PIC X(12).

       01  WS-SCR-CA.
           COPY BILMNUC1.

       01  WS-ACT-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ACA-==.
           03 ACA-BODY.
              COPY BILCACT REPLACING LEADING ==CA-== BY ==ACA-==.

       01  WS-INV-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==IVA-==.
           03 IVA-BODY.
              COPY BILCINV REPLACING LEADING ==CA-== BY ==IVA-==.

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
                 PERFORM 2000-FIRST-DISPLAY
              WHEN EIBAID = DFHENTER
                 PERFORM 2100-PROCESS-ENTER
              WHEN EIBAID = DFHPF7
              WHEN EIBAID = DFHPF8
                 PERFORM 2200-PAGE
              WHEN EIBAID = DFHPF5
                 PERFORM 2300-TO-PAYMENT
              WHEN OTHER
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
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY) DATESEP('-')
                     RESP(WS-RESP)
           END-EXEC.

      ******************************************************************
       2000-FIRST-DISPLAY.
           MOVE 1                      TO SCR-PAGE-NO
           MOVE 0                      TO SCR-ROW-COUNT
           IF SCR-ACCOUNT-NUMBER NOT = SPACES
              PERFORM 2500-LIST-INVOICES
           ELSE
              MOVE LOW-VALUES          TO BILIVMO
              MOVE 'Enter an account number and press ENTER'
                                       TO WS-MSG
              MOVE -1                  TO IVMACCTL
              PERFORM 7000-SEND-MAP
           END-IF.

      ******************************************************************
       2100-PROCESS-ENTER.
           PERFORM 6000-RECEIVE-MAP
           PERFORM 3000-VALIDATE-ACCOUNT
           MOVE 1                      TO SCR-PAGE-NO
           PERFORM 2500-LIST-INVOICES.

      ******************************************************************
      * BR-BIL-IVL-002 Paging stops at the first and last page
       2200-PAGE.
           IF SCR-ACCOUNT-NUMBER = SPACES OR SCR-ROW-COUNT = 0
              MOVE 'No invoices to page through' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-PAGES = (SCR-ROW-COUNT + WS-ROWS-PER-PAGE - 1)
                            / WS-ROWS-PER-PAGE
           IF EIBAID = DFHPF7
              IF SCR-PAGE-NO <= 1
                 MOVE 'Already at the first page' TO WS-MSG
                 PERFORM 7200-SEND-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              SUBTRACT 1               FROM SCR-PAGE-NO
           ELSE
              IF SCR-PAGE-NO >= WS-PAGES
                 MOVE 'Already at the last page' TO WS-MSG
                 PERFORM 7200-SEND-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              ADD 1                    TO SCR-PAGE-NO
           END-IF
           PERFORM 2500-LIST-INVOICES.

      ******************************************************************
       2300-TO-PAYMENT.
           IF SCR-ACCOUNT-ID = 0
              MOVE 'List an account''s invoices before PF5' TO WS-MSG
              PERFORM 7200-SEND-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-FROM-PGM
           MOVE 'BILPAYP'              TO WS-TARGET-PGM
           PERFORM 8900-XCTL.

      ******************************************************************
       2500-LIST-INVOICES.
           MOVE LOW-VALUES             TO BILIVMO
           PERFORM 8000-LINK-ACCTINQ
           IF LIST-OK
              PERFORM 8100-LINK-INVLST
           END-IF
           IF NOT LIST-OK
              MOVE 0                   TO SCR-ROW-COUNT
              MOVE SCR-ACCOUNT-NUMBER  TO IVMACCTO
              MOVE -1                  TO IVMACCTL
              PERFORM 7000-SEND-MAP
              PERFORM 9000-RETURN
           END-IF
           MOVE IVA-IV-ROW-COUNT       TO SCR-ROW-COUNT
           MOVE ACA-AC-ACCOUNT-NUMBER  TO IVMACCTO
           MOVE ACA-AC-ACCOUNT-NAME    TO IVMANAMO
           PERFORM 5000-FORMAT-PAGE
           PERFORM 5200-BUILD-MESSAGE
           MOVE -1                     TO IVMACCTL
           PERFORM 7000-SEND-MAP.

      ******************************************************************
      * BR-BIL-IVL-001 Account number required, no embedded blanks
       3000-VALIDATE-ACCOUNT.
           IF IVMACCTL > 0
              MOVE IVMACCTI            TO SCR-ACCOUNT-NUMBER
           ELSE
              IF IVMACCTF = DFHBMEOF
                 MOVE SPACES           TO SCR-ACCOUNT-NUMBER
              END-IF
           END-IF
           MOVE 0                      TO SCR-ACCOUNT-ID
                                          SCR-ROW-COUNT
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
       5000-FORMAT-PAGE.
           MOVE 0                      TO WS-OVERDUE
           COMPUTE WS-PAGES = (SCR-ROW-COUNT + WS-ROWS-PER-PAGE - 1)
                            / WS-ROWS-PER-PAGE
           IF SCR-PAGE-NO > WS-PAGES OR SCR-PAGE-NO < 1
              MOVE 1                   TO SCR-PAGE-NO
           END-IF
           COMPUTE WS-FIRST-ROW = (SCR-PAGE-NO - 1) * WS-ROWS-PER-PAGE
                                + 1
           PERFORM VARYING WS-ROW FROM 1 BY 1
                   UNTIL WS-ROW > WS-ROWS-PER-PAGE
              COMPUTE WS-RX = WS-FIRST-ROW + WS-ROW - 1
              IF WS-RX <= SCR-ROW-COUNT
                 PERFORM 5100-FORMAT-ROW
              END-IF
           END-PERFORM
           MOVE SCR-PAGE-NO            TO WS-PAGE-EDIT
           MOVE WS-PAGES               TO WS-PAGES-EDIT
           MOVE SCR-ROW-COUNT          TO WS-COUNT-EDIT
           STRING 'Page ' WS-PAGE-EDIT ' of ' WS-PAGES-EDIT
                  ' (' WS-COUNT-EDIT ')'
                  DELIMITED BY SIZE INTO IVMPAGEO.

      ******************************************************************
      * BR-BIL-IVL-004 Overdue = past due date with money still owed
       5100-FORMAT-ROW.
           MOVE IVA-IV-R-NUMBER(WS-RX) TO WIL-NUMBER
           MOVE IVA-IV-R-DATE(WS-RX)   TO WIL-BILL-DATE
           MOVE IVA-IV-R-DUE(WS-RX)    TO WIL-DUE-DATE
           MOVE IVA-IV-R-AMOUNT(WS-RX) TO WIL-AMOUNT
           MOVE IVA-IV-R-PAID(WS-RX)   TO WIL-PAID
           MOVE IVA-IV-R-STATUS(WS-RX) TO WIL-STATUS
           MOVE IVA-IV-R-DESC(WS-RX)   TO WIL-DESC
           COMPUTE WS-UNPAID = IVA-IV-R-AMOUNT(WS-RX)
                             - IVA-IV-R-PAID(WS-RX)
           IF IVA-IV-R-DUE(WS-RX) < WS-TODAY
              AND WS-UNPAID > 0
              AND IVA-IV-R-STATUS(WS-RX) NOT = 'PLAN'
              AND IVA-IV-R-STATUS(WS-RX) NOT = 'CANC'
              MOVE 'OVERDUE'           TO WIL-DESC
              MOVE DFHBMBRY            TO IVMLNA(WS-ROW)
              ADD 1                    TO WS-OVERDUE
           END-IF
           MOVE WS-INVOICE-LINE        TO IVMLNO(WS-ROW).

      ******************************************************************
      * BR-BIL-IVL-003 Contract limit made visible on the last page
      * BR-BIL-IVL-005 Outstanding balance from INVLST
       5200-BUILD-MESSAGE.
           MOVE IVA-IV-AMOUNT-DUE      TO WS-AMT-EDIT
           MOVE SPACES                 TO WS-MSG
           EVALUATE TRUE
              WHEN SCR-ROW-COUNT >= WS-INVLST-LIMIT
                   AND SCR-PAGE-NO = WS-PAGES
                 STRING 'Newest 20 invoices only. Outstanding '
                        WS-AMT-EDIT DELIMITED BY SIZE INTO WS-MSG
              WHEN WS-OVERDUE > 0
                 MOVE WS-OVERDUE       TO WS-COUNT-EDIT
                 STRING WS-COUNT-EDIT ' overdue on this page.'
                        ' Outstanding ' WS-AMT-EDIT
                        DELIMITED BY SIZE INTO WS-MSG
              WHEN OTHER
                 STRING 'Outstanding balance ' WS-AMT-EDIT
                        ' - PF5 to take a payment'
                        DELIMITED BY SIZE INTO WS-MSG
           END-EVALUATE.

      ******************************************************************
       6000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(WS-MAP) MAPSET(WS-MAPSET)
                     INTO(BILIVMI)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
              WHEN DFHRESP(NORMAL)
                 CONTINUE
              WHEN DFHRESP(MAPFAIL)
                 MOVE LOW-VALUES       TO BILIVMI
              WHEN OTHER
                 MOVE '6000-RECEIVE-MAP' TO ERC-ER-PARAGRAPH
                 PERFORM 9800-CICS-ERROR
           END-EVALUATE.

      ******************************************************************
       7000-SEND-MAP.
           MOVE WS-TODAY               TO IVMDATEO
           MOVE WS-MSG                 TO IVMMSGO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILIVMO) ERASE CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7000-SEND-MAP'     TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           MOVE WS-PROGRAM-ID          TO SCR-OWNER-PGM.

      ******************************************************************
       7200-SEND-MESSAGE.
           MOVE LOW-VALUES             TO BILIVMO
           MOVE -1                     TO IVMACCTL
           IF SCR-OWNER-PGM NOT = WS-PROGRAM-ID
              MOVE SCR-ACCOUNT-NUMBER  TO IVMACCTO
              PERFORM 7000-SEND-MAP
              PERFORM 9000-RETURN
           END-IF
           MOVE WS-MSG                 TO IVMMSGO
           EXEC CICS SEND MAP(WS-MAP) MAPSET(WS-MAPSET)
                     FROM(BILIVMO) DATAONLY CURSOR
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '7200-SEND-MESSAGE' TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF.

      ******************************************************************
       8000-LINK-ACCTINQ.
           MOVE 'N'                    TO WS-LIST-OK
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
                 MOVE 'Y'              TO WS-LIST-OK
                 MOVE ACA-AC-ACCOUNT-ID TO SCR-ACCOUNT-ID
              WHEN ACA-RC-NOT-FOUND
                 MOVE 0                TO SCR-ACCOUNT-ID
                 STRING 'Account ' SCR-ACCOUNT-NUMBER ' not found'
                        DELIMITED BY SIZE INTO WS-MSG
      * BR-BIL-IVL-006 Roll back on a failed service
              WHEN ACA-RETURN-CODE >= 90
                 PERFORM 8800-ROLLBACK
                 MOVE ACA-MESSAGE      TO WS-MSG
              WHEN OTHER
                 MOVE ACA-MESSAGE      TO WS-MSG
           END-EVALUATE.

      ******************************************************************
       8100-LINK-INVLST.
           MOVE 'N'                    TO WS-LIST-OK
           INITIALIZE WS-INV-CA
           MOVE 'INVLST  '             TO IVA-REQUEST-ID
           MOVE SCR-USER-ID            TO IVA-USER-ID
           MOVE WS-PROGRAM-ID          TO IVA-CALLER-PGM
           MOVE EIBTRNID               TO IVA-TRACE-ID
           MOVE SCR-ACCOUNT-ID         TO IVA-IV-ACCOUNT-ID
           EXEC CICS LINK PROGRAM(WS-INV-PGM)
                     COMMAREA(WS-INV-CA)
                     LENGTH(4000)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '8100-LINK-INVLST'  TO ERC-ER-PARAGRAPH
              PERFORM 9800-CICS-ERROR
           END-IF
           EVALUATE TRUE
              WHEN IVA-RC-OK
              WHEN IVA-RC-WARNING
                 MOVE 'Y'              TO WS-LIST-OK
              WHEN IVA-RC-NOT-FOUND
                 MOVE ACA-AC-ACCOUNT-NAME TO IVMANAMO
                 MOVE 'Account has no invoices yet' TO WS-MSG
              WHEN IVA-RETURN-CODE >= 90
                 PERFORM 8800-ROLLBACK
                 MOVE IVA-MESSAGE      TO WS-MSG
              WHEN OTHER
                 MOVE IVA-MESSAGE      TO WS-MSG
           END-EVALUATE.

      ******************************************************************
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
           STRING 'BILIVMP CICS error in ' ERC-ER-PARAGRAPH
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
