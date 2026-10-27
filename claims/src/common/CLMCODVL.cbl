       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCODVL.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMCODVL                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Code table validation against CLM_CODE_TABLE     *
      *               (ClaimCenter typelists). Returns the description *
      *               of an active code, and for dependent typelists   *
      *               (loss cause within loss type) checks that the    *
      *               code belongs to the given parent code.           *
      * REQUESTS    : CODVAL  TABLE + CODE is an active code; DESC out *
      *               CODCHD  as CODVAL, and CODE is a child of the    *
      *                       parent code passed in CA-CD-DESC(1:10)   *
      * CALLED BY   : CLMCLAIM, CLMEXPOS, CLMFINTX, CLMCONTC, CLMNOTE0 *
      *               CLMDOCRQ, CLMSRVRQ, screens                      *
      * CALLS       : CLMERRLG                                         *
      * TABLES      : CLM_CODE_TABLE (R)                               *
      * RULES       : BR-CLM-COD-001 Table and code are mandatory      *
      *               BR-CLM-COD-002 An unknown code is not found      *
      *               BR-CLM-COD-003 An inactive code is invalid for   *
      *                              new data                          *
      *               BR-CLM-COD-004 A child code must carry the given *
      *                              parent code, or '*' (valid under  *
      *                              every parent)                     *
      *               BR-CLM-COD-005 The parent code must itself be an *
      *                              active code of the parent table   *
      *                              (LOSSCAUS -> LOSSTYPE,            *
      *                               COSTCAT  -> COSTTYPE)            *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.

      * Dependent typelists: child table -> parent table
       01  WS-PARENT-MAP-VALUES.
           05 FILLER          PIC X(16) VALUE 'LOSSCAUSLOSSTYPE'.
           05 FILLER          PIC X(16) VALUE 'COSTCAT COSTTYPE'.
       01  WS-PARENT-MAP REDEFINES WS-PARENT-MAP-VALUES.
           05 WS-PM-ENTRY OCCURS 2 TIMES INDEXED BY WS-PM-IX.
              10 WS-PM-CHILD-TABLE     PIC X(8).
              10 WS-PM-PARENT-TABLE    PIC X(8).

       01  WS-WORK.
           05 WS-PARENT-CODE           PIC X(10).
           05 WS-CHILD-PARENT          PIC X(10).
           05 WS-CHILD-DESC            PIC X(60).
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-KEY               PIC X(20).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDCOD END-EXEC.

       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCCOD.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'CODVAL  '
                 PERFORM 2100-VALIDATE-CODE
              WHEN 'CODCHD  '
                 PERFORM 2200-VALIDATE-CHILD
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMCODVL: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
           MOVE 'N'                    TO CA-CD-VALID
           STRING CA-CD-TABLE '/' CA-CD-CODE DELIMITED BY SIZE
                  INTO WS-ERR-KEY.

      ******************************************************************
       2100-VALIDATE-CODE.
           PERFORM 3100-REQUIRE-KEY
           MOVE CA-CD-TABLE            TO COD-TABLE-NAME
           MOVE CA-CD-CODE             TO COD-CODE-VALUE
           PERFORM 4100-READ-CODE
           PERFORM 3200-CHECK-ACTIVE
           MOVE 'Y'                    TO CA-CD-VALID
           MOVE SPACES                 TO CA-CD-DESC
           IF COD-DESCRIPTION-LEN > 0
              MOVE COD-DESCRIPTION-TEXT(1:COD-DESCRIPTION-LEN)
                                       TO CA-CD-DESC
           END-IF.

      ******************************************************************
      * The parent code arrives in CA-CD-DESC(1:10); on success
      * CA-CD-DESC is overwritten with the child's description.
       2200-VALIDATE-CHILD.
           PERFORM 3100-REQUIRE-KEY
           MOVE CA-CD-DESC(1:10)       TO WS-PARENT-CODE
           IF WS-PARENT-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CODPARRQ'          TO CA-REASON-CODE
              MOVE 'Parent code is required in CA-CD-DESC for CODCHD'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-CD-TABLE            TO COD-TABLE-NAME
           MOVE CA-CD-CODE             TO COD-CODE-VALUE
           PERFORM 4100-READ-CODE
           PERFORM 3200-CHECK-ACTIVE
           MOVE COD-PARENT-CODE        TO WS-CHILD-PARENT
           MOVE SPACES                 TO WS-CHILD-DESC
           IF COD-DESCRIPTION-LEN > 0
              MOVE COD-DESCRIPTION-TEXT(1:COD-DESCRIPTION-LEN)
                                       TO WS-CHILD-DESC
           END-IF
      * BR-CLM-COD-004 Child must belong to the parent (or to all)
           IF WS-CHILD-PARENT NOT = WS-PARENT-CODE
           AND WS-CHILD-PARENT NOT = '*'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CODPARNT'          TO CA-REASON-CODE
              STRING CA-CD-TABLE ' ' CA-CD-CODE ' not valid under '
                     WS-PARENT-CODE DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3300-CHECK-PARENT-ACTIVE
           MOVE 'Y'                    TO CA-CD-VALID
           MOVE WS-CHILD-DESC          TO CA-CD-DESC.

      ******************************************************************
      * BR-CLM-COD-001 Table and code are mandatory
       3100-REQUIRE-KEY.
           IF CA-CD-TABLE = SPACES OR CA-CD-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CODKEYRQ'          TO CA-REASON-CODE
              MOVE 'Code table name and code value are required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-COD-003 Retired codes stay on file for history but may
      * not be used on new or changed data.
       3200-CHECK-ACTIVE.
           IF COD-ACTIVE-FLAG NOT = 'Y'
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'CODINACT'          TO CA-REASON-CODE
              STRING COD-TABLE-NAME ' ' COD-CODE-VALUE
                     ' is no longer active' DELIMITED BY SIZE
                     INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-COD-005 For a mapped dependent typelist, the parent code
      * must be a live code of the parent table (stops a '*' child
      * being accepted under a nonsense parent).
       3300-CHECK-PARENT-ACTIVE.
           SET WS-PM-IX                TO 1
           SEARCH WS-PM-ENTRY
              AT END
                 CONTINUE
              WHEN WS-PM-CHILD-TABLE(WS-PM-IX) = CA-CD-TABLE
                 MOVE WS-PM-PARENT-TABLE(WS-PM-IX) TO COD-TABLE-NAME
                 MOVE WS-PARENT-CODE   TO COD-CODE-VALUE
                 PERFORM 4100-READ-CODE
                 PERFORM 3200-CHECK-ACTIVE
           END-SEARCH.

      ******************************************************************
       4100-READ-CODE.
           EXEC SQL
              SELECT DESCRIPTION, PARENT_CODE, ACTIVE_FLAG
                INTO :COD-DESCRIPTION, :COD-PARENT-CODE,
                     :COD-ACTIVE-FLAG
                FROM CLMDB.CLM_CODE_TABLE
               WHERE TABLE_NAME = :COD-TABLE-NAME
                 AND CODE_VALUE = :COD-CODE-VALUE
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
      * BR-CLM-COD-002 Unknown code
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CODNOTFD'       TO CA-REASON-CODE
                 STRING COD-TABLE-NAME ' ' COD-CODE-VALUE
                        ' is not a valid code' DELIMITED BY SIZE
                        INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-READ-CODE' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

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
           STRING 'CLMCODVL Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE ERC-HEADER
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE WS-ERR-KEY             TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
