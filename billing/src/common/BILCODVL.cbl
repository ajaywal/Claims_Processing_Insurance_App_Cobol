       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILCODVL.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : BILCODVL                                         *
      * APPLICATION : BillingCoreAJ2026 (BillingCenter parity)         *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Validates a code against BIL_CODE_TABLE (the     *
      *               typelist store) and returns its description.     *
      *               Batch callers benefit from a small run-unit      *
      *               cache of codes already found.                    *
      * REQUESTS    : CODVAL                                           *
      * CALLED BY   : account, policy period, payment, hold, write-off *
      *               and disbursement services, online screens        *
      * CALLS       : BILERRLG                                         *
      * TABLES      : BIL_CODE_TABLE (R)                               *
      * RULES       : BR-BIL-COD-001 Code table and code are mandatory *
      *               BR-BIL-COD-002 Codes are matched in upper case   *
      *               BR-BIL-COD-003 Only ACTIVE_FLAG = 'Y' codes are  *
      *                              valid; retired codes return their *
      *                              description with RC 12            *
      *               BR-BIL-COD-004 An unknown code table is reported *
      *                              separately from an unknown code   *
      *               BR-BIL-COD-005 Found codes are cached for the    *
      *                              run unit (50 entries, round       *
      *                              robin); misses are never cached   *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'BILCODVL'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'BILERRLG'.

       01  WS-WORK.
           05 WS-DOMAIN-COUNT          PIC S9(9) COMP VALUE 0.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-FOUND-FLAG            PIC X     VALUE 'N'.
              88 WS-FOUND                        VALUE 'Y'.

      * BR-BIL-COD-005 Run-unit cache
       01  WS-CACHE.
           05 WS-CACHE-MAX             PIC S9(4) COMP VALUE 50.
           05 WS-CACHE-COUNT           PIC S9(4) COMP VALUE 0.
           05 WS-CACHE-NEXT            PIC S9(4) COMP VALUE 0.
           05 WS-CX                    PIC S9(4) COMP VALUE 0.
           05 WS-CACHE-ENTRY OCCURS 50 TIMES.
              10 WS-CE-TABLE           PIC X(8).
              10 WS-CE-CODE            PIC X(10).
              10 WS-CE-DESC            PIC X(60).
              10 WS-CE-ACTIVE          PIC X.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE BILDCOD END-EXEC.

       01  WS-ERR-CA.
           COPY BILCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY BILCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY BILCAHDR.
           03 CA-BODY.
              COPY BILCCOD.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'CODVAL  '
                 PERFORM 2000-VALIDATE-CODE
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'BILCODVL: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          CA-CD-DESC
           MOVE 'N'                    TO CA-CD-VALID
                                          WS-FOUND-FLAG.

      ******************************************************************
       2000-VALIDATE-CODE.
      * BR-BIL-COD-001 Mandatory inputs
           IF CA-CD-TABLE = SPACES OR CA-CD-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CODREQD '          TO CA-REASON-CODE
              MOVE 'Code table and code value are required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-BIL-COD-002 Upper-case match
           MOVE FUNCTION UPPER-CASE(CA-CD-TABLE) TO CA-CD-TABLE
           MOVE FUNCTION UPPER-CASE(CA-CD-CODE)  TO CA-CD-CODE
           PERFORM 5000-SEARCH-CACHE
           IF NOT WS-FOUND
              PERFORM 4000-READ-CODE
              PERFORM 5100-ADD-TO-CACHE
           END-IF
      * BR-BIL-COD-003 Only active codes are valid
           IF COD-ACTIVE-FLAG = 'Y'
              MOVE 'Y'                 TO CA-CD-VALID
           ELSE
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CODINACT'          TO CA-REASON-CODE
              STRING 'Code ' CA-CD-CODE ' in ' CA-CD-TABLE
                     ' is no longer active'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       4000-READ-CODE.
           MOVE CA-CD-TABLE            TO COD-TABLE-NAME
           MOVE CA-CD-CODE             TO COD-CODE-VALUE
           MOVE SPACES                 TO COD-DESCRIPTION-TEXT
           EXEC SQL
              SELECT DESCRIPTION, ACTIVE_FLAG
                INTO :COD-DESCRIPTION, :COD-ACTIVE-FLAG
                FROM BILDB.BIL_CODE_TABLE
               WHERE TABLE_NAME = :COD-TABLE-NAME
                 AND CODE_VALUE = :COD-CODE-VALUE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE COD-DESCRIPTION-TEXT TO CA-CD-DESC
              WHEN 100
                 PERFORM 4100-CHECK-DOMAIN
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Code table busy - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4000-READ-CODE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-BIL-COD-004 Unknown table versus unknown code
       4100-CHECK-DOMAIN.
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-DOMAIN-COUNT
                FROM BILDB.BIL_CODE_TABLE
               WHERE TABLE_NAME = :COD-TABLE-NAME
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4100-CHECK-DOMAIN' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           SET CA-RC-NOT-FOUND         TO TRUE
           IF WS-DOMAIN-COUNT = 0
              MOVE 'CODTBLNF'          TO CA-REASON-CODE
              STRING 'Code table ' CA-CD-TABLE ' is not defined'
                     DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              MOVE 'CODNOTFD'          TO CA-REASON-CODE
              STRING 'Code ' CA-CD-CODE ' is not valid for '
                     CA-CD-TABLE DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           PERFORM 9000-RETURN.

      ******************************************************************
       5000-SEARCH-CACHE.
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CACHE-COUNT OR WS-FOUND
              IF WS-CE-TABLE(WS-CX) = CA-CD-TABLE
              AND WS-CE-CODE(WS-CX) = CA-CD-CODE
                 MOVE 'Y'                 TO WS-FOUND-FLAG
                 MOVE WS-CE-DESC(WS-CX)   TO CA-CD-DESC
                 MOVE WS-CE-ACTIVE(WS-CX) TO COD-ACTIVE-FLAG
              END-IF
           END-PERFORM.

      ******************************************************************
       5100-ADD-TO-CACHE.
           IF WS-CACHE-COUNT < WS-CACHE-MAX
              ADD 1                    TO WS-CACHE-COUNT
              MOVE WS-CACHE-COUNT      TO WS-CX
           ELSE
              ADD 1                    TO WS-CACHE-NEXT
              IF WS-CACHE-NEXT > WS-CACHE-MAX
                 MOVE 1                TO WS-CACHE-NEXT
              END-IF
              MOVE WS-CACHE-NEXT       TO WS-CX
           END-IF
           MOVE CA-CD-TABLE            TO WS-CE-TABLE(WS-CX)
           MOVE CA-CD-CODE             TO WS-CE-CODE(WS-CX)
           MOVE CA-CD-DESC             TO WS-CE-DESC(WS-CX)
           MOVE COD-ACTIVE-FLAG        TO WS-CE-ACTIVE(WS-CX).

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'BILCODVL Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           STRING CA-CD-TABLE '/' CA-CD-CODE
                  DELIMITED BY SIZE INTO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
