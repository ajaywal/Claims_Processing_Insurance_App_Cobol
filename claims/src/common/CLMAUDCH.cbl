       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMAUDCH.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMAUDCH                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Tamper-evident audit hash chain for regulated    *
      *               claim events (reserve and payment authority,     *
      *               SIU referral, litigation, close/reopen). Each    *
      *               row stores a digest of the caller's 400-byte     *
      *               payload and a chain hash over the previous row's *
      *               hash and that digest, so altering, removing or   *
      *               re-ordering any committed row breaks every link  *
      *               after it.                                        *
      * REQUESTS    : AUCADD  append an event; returns CHAIN-SEQ and   *
      *                       CHAIN-HASH                               *
      *               AUCVFY  re-walk the chain from FROM-SEQ (0 =     *
      *                       after the last sealed row), recompute    *
      *                       every link, report the first broken seq  *
      * CALLED BY   : CLMFINTX, CLMCLAIM, CLMSIUSC, CLMLITIG, batch    *
      *               chain verification job                           *
      * CALLS       : CLMSEQGN, CLMERRLG                               *
      * TABLES      : CLM_AUDIT_CHAIN (C R U)  CLM_SEQUENCE (U)        *
      * RULES       : BR-CLM-AUC-001 Event code and payload required;  *
      *                              claim id 0 means a system event   *
      *               BR-CLM-AUC-002 Appends are serialised by locking *
      *                              CLM_SEQUENCE 'AUC' before the     *
      *                              chain tail is read                *
      *               BR-CLM-AUC-003 Genesis PREV_HASH is 16 zeros     *
      *               BR-CLM-AUC-004 DIGEST = FNV-1a-64 of the full    *
      *                              400-byte payload as passed        *
      *               BR-CLM-AUC-005 CHAIN_HASH = FNV-1a-64 of the 32  *
      *                              characters PREV_HASH || DIGEST    *
      *               BR-CLM-AUC-006 Never extend a chain whose tail   *
      *                              fails its own hash check          *
      *               BR-CLM-AUC-007 Chain sequence is contiguous: the *
      *                              new row is tail + 1               *
      *               BR-CLM-AUC-008 AUCVFY FROM-SEQ 0 resumes after   *
      *                              the last sealed row; FROM-SEQ n   *
      *                              anchors on row n-1's hash         *
      *               BR-CLM-AUC-009 A link breaks on a sequence gap,  *
      *                              PREV_HASH mismatch, CHAIN_HASH    *
      *                              mismatch or malformed hex; the    *
      *                              first break is reported           *
      *               BR-CLM-AUC-010 At most 50000 rows per AUCVFY     *
      *                              call; a partial walk is RC 04     *
      *               BR-CLM-AUC-011 Rows proven intact are sealed;    *
      *                              nothing is sealed past a break    *
      *               BR-CLM-AUC-012 A broken chain is logged to       *
      *                              CLM_ERROR_LOG as a security event *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
      *                                                                *
      * HASH FUNCTION - FNV-1a 64-bit, computed in decimal arithmetic  *
      * ------------------------------------------------------------- *
      * FNV-1a (Fowler/Noll/Vo, public domain) over bytes b1..bn:      *
      *     h = 14695981039346656037          (offset basis)           *
      *     for each byte: h = h XOR b ;  h = (h * P) MOD 2**64        *
      *     P = 1099511628211 = 2**40 + 435   (FNV 64-bit prime)       *
      * COBOL has no 64-bit unsigned type or bitwise operators, so h   *
      * is held as two 32-bit halves HI and LO in PIC 9(18) packed     *
      * fields (h = HI * 2**32 + LO) and every step is exact integer   *
      * arithmetic:                                                    *
      *  XOR    only the low 8 bits of LO change. LO = Q * 256 + R;    *
      *         R XOR b is built bit by bit (8 DIVIDE BY 2 steps), and *
      *         LO = Q * 256 + (R XOR b).                              *
      *  MULTIPLY by P = PHI * 2**32 + PLO with PHI = 256, PLO = 435:  *
      *         T      = LO * PLO              (< 2**41)               *
      *         newLO  = T MOD 2**32                                   *
      *         newHI  = (HI * PLO + LO * PHI + T / 2**32) MOD 2**32   *
      *         HI * PHI * 2**64 vanishes MOD 2**64, so this is exact. *
      *         The largest intermediate is below 2**43, far inside    *
      *         the 18 decimal digits of PIC 9(18).                    *
      * The 64-bit result is shown as 16 upper-case hex characters,    *
      * HI then LO. Byte values come from FUNCTION ORD - 1, i.e. the   *
      * native code page (EBCDIC on z/OS). Results are deterministic   *
      * on the platform that wrote them; a verifier on another         *
      * platform must hash the same EBCDIC bytes. FNV-1a gives tamper  *
      * EVIDENCE for an internal control, not cryptographic strength.  *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.

       01  WS-CONSTANTS.
           05 WS-ZERO-HASH             PIC X(16) VALUE ALL '0'.
           05 WS-AUC-SEQ-NAME          PIC X(8)  VALUE 'AUC     '.
           05 WS-MAX-VERIFY            PIC S9(9) COMP VALUE 50000.
           05 WS-HEX-DIGITS            PIC X(16)
                                       VALUE '0123456789ABCDEF'.
           05 WS-FNV-OFFSET-HI         PIC 9(18) COMP-3
                                       VALUE 3421674724.
           05 WS-FNV-OFFSET-LO         PIC 9(18) COMP-3
                                       VALUE 2216829733.
           05 WS-FNV-PRIME-HI          PIC 9(18) COMP-3 VALUE 256.
           05 WS-FNV-PRIME-LO          PIC 9(18) COMP-3 VALUE 435.
           05 WS-TWO-32                PIC 9(18) COMP-3
                                       VALUE 4294967296.

      * Hash engine work areas
       01  WS-HASH-INPUT               PIC X(400).
       01  WS-HASH-LEN                 PIC S9(4) COMP.
       01  WS-HASH-HEX                 PIC X(16).
       01  WS-HASH-ENGINE.
           05 WS-H-HI                  PIC 9(18) COMP-3.
           05 WS-H-LO                  PIC 9(18) COMP-3.
           05 WS-H-T                   PIC 9(18) COMP-3.
           05 WS-H-CARRY               PIC 9(18) COMP-3.
           05 WS-H-Q                   PIC 9(18) COMP-3.
           05 WS-H-LOW8                PIC 9(4)  COMP.
           05 WS-H-BYTE                PIC 9(4)  COMP.
           05 WS-H-XOR                 PIC 9(4)  COMP.
           05 WS-H-A                   PIC 9(4)  COMP.
           05 WS-H-B                   PIC 9(4)  COMP.
           05 WS-H-ABIT                PIC 9     COMP.
           05 WS-H-BBIT                PIC 9     COMP.
           05 WS-H-BITVAL              PIC 9(4)  COMP.
           05 WS-H-POS                 PIC S9(4) COMP.
           05 WS-H-BIT                 PIC S9(4) COMP.
           05 WS-H-HALF                PIC 9(18) COMP-3.
           05 WS-H-NIBBLE              PIC 9(4)  COMP.
           05 WS-H-HEX8                PIC X(8).
           05 WS-H-HEX-IX              PIC S9(4) COMP.

      * Link being built / checked
       01  WS-LINK.
           05 WS-LINK-PREV             PIC X(16).
           05 WS-LINK-DIGEST           PIC X(16).
       01  WS-LINK-HASH                PIC X(16).

       01  WS-WORK.
           05 WS-GENESIS               PIC X     VALUE 'N'.
              88 WS-IS-GENESIS                   VALUE 'Y'.
           05 WS-TAIL-SEQ              PIC S9(9) COMP.
           05 WS-NEW-SEQ               PIC S9(9) COMP.
           05 WS-START-AFTER           PIC S9(9) COMP.
           05 WS-EXPECT-SEQ            PIC S9(9) COMP.
           05 WS-EXPECT-PREV           PIC X(16).
           05 WS-LAST-GOOD-SEQ         PIC S9(9) COMP.
           05 WS-LAST-GOOD-HASH        PIC X(16).
           05 WS-ANCHOR-FOUND          PIC X     VALUE 'N'.
              88 WS-HAS-ANCHOR                   VALUE 'Y'.
           05 WS-BREAK                 PIC X     VALUE 'N'.
              88 WS-CHAIN-BROKEN                 VALUE 'Y'.
           05 WS-BREAK-REASON          PIC X(8).
           05 WS-BREAK-TEXT            PIC X(40).
           05 WS-EOF                   PIC X     VALUE 'N'.
              88 WS-END-OF-CURSOR                VALUE 'Y'.
           05 WS-CURSOR-OPEN           PIC X     VALUE 'N'.
              88 WS-CURSOR-IS-OPEN               VALUE 'Y'.
           05 WS-HEX-CHECK             PIC X(16).
           05 WS-SAVE-SQLCODE          PIC S9(9) COMP.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-SEQ-DISP              PIC 9(9).
           05 WS-SEQ-DISP-2            PIC 9(9).
           05 WS-ERR-KEY               PIC X(20).
       01  WS-MAX-SEALED               PIC S9(9) COMP.
       01  WS-MAX-SEALED-IND           PIC S9(4) COMP.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDAUC END-EXEC.

      * BR-CLM-AUC-008 walk the chain in sequence order after anchor
           EXEC SQL DECLARE AUCCUR CURSOR FOR
              SELECT CHAIN_SEQ, PAYLOAD_DIGEST, PREV_HASH, CHAIN_HASH
                FROM CLMDB.CLM_AUDIT_CHAIN
               WHERE CHAIN_SEQ > :WS-START-AFTER
               ORDER BY CHAIN_SEQ
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
              COPY CLMCAUC.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'AUCADD  '
                 PERFORM 2100-APPEND-EVENT
              WHEN 'AUCVFY  '
                 PERFORM 2200-VERIFY-CHAIN
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMAUDCH: unknown request '
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
                                          WS-BREAK
                                          WS-GENESIS
                                          WS-ANCHOR-FOUND
           MOVE 'AUC'                  TO WS-ERR-KEY.

      ******************************************************************
      * AUCADD: lock, read tail, prove tail, allocate seq, hash, insert
       2100-APPEND-EVENT.
           PERFORM 3100-VALIDATE-ADD
           PERFORM 4100-LOCK-CHAIN
           PERFORM 4200-READ-TAIL
           IF NOT WS-IS-GENESIS
              PERFORM 3300-PROVE-TAIL
           END-IF
           PERFORM 8000-NEXT-CHAIN-SEQ
      * BR-CLM-AUC-007 New row must directly follow the tail
           IF NOT WS-IS-GENESIS
           AND WS-NEW-SEQ NOT = WS-TAIL-SEQ + 1
              MOVE WS-NEW-SEQ          TO WS-SEQ-DISP
              MOVE WS-TAIL-SEQ         TO WS-SEQ-DISP-2
              SET CA-RC-STATE          TO TRUE
              MOVE 'AUCSEQGP'          TO CA-REASON-CODE
              STRING 'AUC sequence ' WS-SEQ-DISP
                     ' does not follow chain tail ' WS-SEQ-DISP-2
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-AUC-004 Digest of the full 400-byte payload
           MOVE CA-AC-PAYLOAD          TO WS-HASH-INPUT
           MOVE LENGTH OF CA-AC-PAYLOAD TO WS-HASH-LEN
           PERFORM 5000-FNV1A-64
           MOVE WS-HASH-HEX            TO WS-LINK-DIGEST
      * BR-CLM-AUC-003 Genesis links to sixteen zeros
           IF WS-IS-GENESIS
              MOVE WS-ZERO-HASH        TO WS-LINK-PREV
           ELSE
              MOVE AUC-CHAIN-HASH      TO WS-LINK-PREV
           END-IF
           PERFORM 5500-LINK-HASH
           PERFORM 4300-INSERT-LINK
           MOVE WS-NEW-SEQ             TO CA-AC-CHAIN-SEQ
           MOVE WS-LINK-HASH           TO CA-AC-CHAIN-HASH.

      ******************************************************************
      * AUCVFY: establish the anchor, walk forward, seal what is proven
       2200-VERIFY-CHAIN.
           MOVE 0                      TO CA-AC-ROWS-CHECKED
                                          CA-AC-BROKEN-SEQ
                                          CA-AC-CHAIN-SEQ
           MOVE SPACES                 TO CA-AC-CHAIN-HASH
           IF CA-AC-FROM-SEQ < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUCFROM '          TO CA-REASON-CODE
              MOVE 'From-sequence must be 0 (resume) or positive'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4400-FIND-ANCHOR
           MOVE WS-EXPECT-SEQ          TO WS-LAST-GOOD-SEQ
           SUBTRACT 1                FROM WS-LAST-GOOD-SEQ
           MOVE WS-EXPECT-PREV         TO WS-LAST-GOOD-HASH
           MOVE 'N'                    TO WS-EOF
           EXEC SQL OPEN AUCCUR END-EXEC
           IF SQLCODE NOT = 0
              MOVE '2200-VERIFY-CHAIN' TO ERC-ER-PARAGRAPH
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE 'Y'                    TO WS-CURSOR-OPEN
           PERFORM 4500-FETCH-LINK
           PERFORM UNTIL WS-END-OF-CURSOR
                      OR WS-CHAIN-BROKEN
                      OR CA-AC-ROWS-CHECKED >= WS-MAX-VERIFY
              ADD 1                    TO CA-AC-ROWS-CHECKED
              PERFORM 3400-CHECK-LINK
              IF NOT WS-CHAIN-BROKEN
                 MOVE AUC-CHAIN-SEQ    TO WS-LAST-GOOD-SEQ
                 MOVE AUC-CHAIN-HASH   TO WS-LAST-GOOD-HASH
                 COMPUTE WS-EXPECT-SEQ = AUC-CHAIN-SEQ + 1
                 MOVE AUC-CHAIN-HASH   TO WS-EXPECT-PREV
                 PERFORM 4500-FETCH-LINK
              END-IF
           END-PERFORM
           PERFORM 4600-CLOSE-CHAIN
           PERFORM 2300-REPORT-VERIFY.

      ******************************************************************
       2300-REPORT-VERIFY.
           IF WS-LAST-GOOD-SEQ > 0
              MOVE WS-LAST-GOOD-SEQ    TO CA-AC-CHAIN-SEQ
              MOVE WS-LAST-GOOD-HASH   TO CA-AC-CHAIN-HASH
           END-IF
           EVALUATE TRUE
              WHEN WS-CHAIN-BROKEN
                 MOVE AUC-CHAIN-SEQ    TO CA-AC-BROKEN-SEQ
                 PERFORM 4700-SEAL-VERIFIED
                 PERFORM 8100-LOG-BREAK
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE WS-BREAK-REASON  TO CA-REASON-CODE
                 MOVE AUC-CHAIN-SEQ    TO WS-SEQ-DISP
                 STRING 'Audit chain broken at seq ' WS-SEQ-DISP
                        ': ' WS-BREAK-TEXT
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN CA-AC-ROWS-CHECKED = 0 AND WS-HAS-ANCHOR
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'AUCNONEW'       TO CA-REASON-CODE
                 MOVE 'No audit rows after the verification anchor'
                                       TO CA-MESSAGE
              WHEN CA-AC-ROWS-CHECKED = 0
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'AUCEMPTY'       TO CA-REASON-CODE
                 MOVE 'Audit chain has no rows to verify'
                                       TO CA-MESSAGE
      * BR-CLM-AUC-010 Partial walk - caller resumes with FROM-SEQ 0
              WHEN NOT WS-END-OF-CURSOR
                 PERFORM 4700-SEAL-VERIFIED
                 SET CA-RC-WARNING     TO TRUE
                 MOVE 'AUCPARTL'       TO CA-REASON-CODE
                 MOVE WS-LAST-GOOD-SEQ TO WS-SEQ-DISP
                 STRING 'Verified to seq ' WS-SEQ-DISP
                        ' - row limit reached, resume with 0'
                        DELIMITED BY SIZE INTO CA-MESSAGE
              WHEN OTHER
                 PERFORM 4700-SEAL-VERIFIED
                 MOVE WS-LAST-GOOD-SEQ TO WS-SEQ-DISP
                 STRING 'Audit chain intact to seq ' WS-SEQ-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-AUC-001 Event code and payload required; claim id >= 0
       3100-VALIDATE-ADD.
           IF CA-AC-CLAIM-ID < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUCCLMID'          TO CA-REASON-CODE
              MOVE 'Claim id must be 0 (system event) or positive'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-AC-EVENT-CODE = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUCEVENT'          TO CA-REASON-CODE
              MOVE 'Audit event code is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-AC-PAYLOAD = SPACES OR CA-AC-PAYLOAD = LOW-VALUES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'AUCPAYLD'          TO CA-REASON-CODE
              MOVE 'Audit payload is required' TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-USER-ID = SPACES
              MOVE 'SYSTEM  '          TO CA-USER-ID
           END-IF
           IF CA-AC-PROGRAM = SPACES
              IF CA-CALLER-PGM = SPACES
                 MOVE 'UNKNOWN '       TO CA-AC-PROGRAM
              ELSE
                 MOVE CA-CALLER-PGM    TO CA-AC-PROGRAM
              END-IF
           END-IF
           MOVE CA-AC-CLAIM-ID         TO WS-SEQ-DISP
           MOVE WS-SEQ-DISP            TO WS-ERR-KEY.

      ******************************************************************
      * BR-CLM-AUC-006 The tail must still hash correctly before we
      * build on it; otherwise new rows would launder a tampered row.
       3300-PROVE-TAIL.
           MOVE AUC-PREV-HASH          TO WS-LINK-PREV
           MOVE AUC-PAYLOAD-DIGEST     TO WS-LINK-DIGEST
           PERFORM 5500-LINK-HASH
           IF WS-LINK-HASH NOT = AUC-CHAIN-HASH
              MOVE WS-TAIL-SEQ         TO WS-SEQ-DISP
              MOVE 'AUCHASH '          TO WS-BREAK-REASON
              MOVE 'tail hash mismatch on append'
                                       TO WS-BREAK-TEXT
              PERFORM 8100-LOG-BREAK
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'AUCTAMPR'          TO CA-REASON-CODE
              STRING 'Audit chain tail ' WS-SEQ-DISP
                     ' fails hash check - append refused'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-AUC-009 Check one fetched row against the running state;
      * the first failing test sets WS-BREAK with its reason.
       3400-CHECK-LINK.
           EVALUATE TRUE
              WHEN WS-EXPECT-SEQ > 0
               AND AUC-CHAIN-SEQ NOT = WS-EXPECT-SEQ
                 MOVE 'AUCGAP  '       TO WS-BREAK-REASON
                 MOVE WS-EXPECT-SEQ    TO WS-SEQ-DISP-2
                 STRING 'row ' WS-SEQ-DISP-2 ' missing'
                        DELIMITED BY SIZE INTO WS-BREAK-TEXT
                 MOVE 'Y'              TO WS-BREAK
              WHEN OTHER
                 PERFORM 3410-CHECK-HEX
           END-EVALUATE
           IF NOT WS-CHAIN-BROKEN
              IF AUC-PREV-HASH NOT = WS-EXPECT-PREV
                 MOVE 'AUCLINK '       TO WS-BREAK-REASON
                 MOVE 'PREV_HASH differs from prior CHAIN_HASH'
                                       TO WS-BREAK-TEXT
                 MOVE 'Y'              TO WS-BREAK
              ELSE
                 MOVE AUC-PREV-HASH    TO WS-LINK-PREV
                 MOVE AUC-PAYLOAD-DIGEST TO WS-LINK-DIGEST
                 PERFORM 5500-LINK-HASH
                 IF WS-LINK-HASH NOT = AUC-CHAIN-HASH
                    MOVE 'AUCHASH '    TO WS-BREAK-REASON
                    MOVE 'CHAIN_HASH does not recompute'
                                       TO WS-BREAK-TEXT
                    MOVE 'Y'           TO WS-BREAK
                 END-IF
              END-IF
           END-IF.

      ******************************************************************
      * All three stored hashes must be 16 upper-case hex characters
       3410-CHECK-HEX.
           MOVE AUC-PAYLOAD-DIGEST     TO WS-HEX-CHECK
           PERFORM 3420-HEX-ONLY
           IF NOT WS-CHAIN-BROKEN
              MOVE AUC-PREV-HASH       TO WS-HEX-CHECK
              PERFORM 3420-HEX-ONLY
           END-IF
           IF NOT WS-CHAIN-BROKEN
              MOVE AUC-CHAIN-HASH      TO WS-HEX-CHECK
              PERFORM 3420-HEX-ONLY
           END-IF.

      ******************************************************************
       3420-HEX-ONLY.
           IF WS-HEX-CHECK(16:1) = SPACE
              MOVE 'Y'                 TO WS-BREAK
           ELSE
              INSPECT WS-HEX-CHECK CONVERTING WS-HEX-DIGITS
                   TO '                '
              IF WS-HEX-CHECK NOT = SPACES
                 MOVE 'Y'              TO WS-BREAK
              END-IF
           END-IF
           IF WS-CHAIN-BROKEN
              MOVE 'AUCFMT  '          TO WS-BREAK-REASON
              MOVE 'stored hash is not 16 hex characters'
                                       TO WS-BREAK-TEXT
           END-IF.

      ******************************************************************
      * BR-CLM-AUC-002 Serialise appenders: the X lock on the AUC
      * sequence row is held to the caller's commit, so two tasks can
      * never read the same tail and fork the chain.
       4100-LOCK-CHAIN.
           EXEC SQL
              UPDATE CLMDB.CLM_SEQUENCE
                 SET UPDATE_TS = CURRENT TIMESTAMP
               WHERE SEQ_NAME  = :WS-AUC-SEQ-NAME
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'AUCNOSEQ'       TO CA-REASON-CODE
                 MOVE 'CLM_SEQUENCE row AUC is not defined'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 SET CA-RC-DB2-ERROR   TO TRUE
                 MOVE 'DEADLOCK'       TO CA-REASON-CODE
                 MOVE 'Audit chain locked by another task - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4100-LOCK-CHAIN' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4200-READ-TAIL.
           EXEC SQL
              SELECT CHAIN_SEQ, PAYLOAD_DIGEST, PREV_HASH, CHAIN_HASH
                INTO :AUC-CHAIN-SEQ, :AUC-PAYLOAD-DIGEST,
                     :AUC-PREV-HASH, :AUC-CHAIN-HASH
                FROM CLMDB.CLM_AUDIT_CHAIN
               ORDER BY CHAIN_SEQ DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE AUC-CHAIN-SEQ    TO WS-TAIL-SEQ
              WHEN 100
                 MOVE 'Y'              TO WS-GENESIS
                 MOVE 0                TO WS-TAIL-SEQ
              WHEN OTHER
                 MOVE '4200-READ-TAIL' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4300-INSERT-LINK.
           MOVE WS-NEW-SEQ             TO AUC-CHAIN-SEQ
           MOVE CA-AC-CLAIM-ID         TO AUC-CLAIM-ID
           MOVE CA-AC-EVENT-CODE       TO AUC-EVENT-CODE
           MOVE WS-LINK-DIGEST         TO AUC-PAYLOAD-DIGEST
           MOVE WS-LINK-PREV           TO AUC-PREV-HASH
           MOVE WS-LINK-HASH           TO AUC-CHAIN-HASH
           MOVE CA-USER-ID             TO AUC-USER-ID
           MOVE CA-AC-PROGRAM          TO AUC-PROGRAM-ID
           MOVE 'N'                    TO AUC-SEALED-FLAG
           EXEC SQL
              INSERT INTO CLMDB.CLM_AUDIT_CHAIN
                 (CHAIN_SEQ, CLAIM_ID, EVENT_CODE, PAYLOAD_DIGEST,
                  PREV_HASH, CHAIN_HASH, USER_ID, PROGRAM_ID,
                  EVENT_TS, SEALED_FLAG)
              VALUES
                 (:AUC-CHAIN-SEQ, :AUC-CLAIM-ID, :AUC-EVENT-CODE,
                  :AUC-PAYLOAD-DIGEST, :AUC-PREV-HASH,
                  :AUC-CHAIN-HASH, :AUC-USER-ID, :AUC-PROGRAM-ID,
                  CURRENT TIMESTAMP, :AUC-SEALED-FLAG)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'AUCDUPSQ'       TO CA-REASON-CODE
                 MOVE 'Audit chain sequence already used'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4300-INSERT-LINK' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * BR-CLM-AUC-008 Anchor for the walk:
      *  FROM-SEQ 0 - after the highest sealed row (already proven);
      *               no sealed rows means start at genesis.
      *  FROM-SEQ n - after the nearest row below n, using its hash;
      *               no row below n means n starts the chain.
       4400-FIND-ANCHOR.
           MOVE 0                      TO WS-START-AFTER
                                          WS-EXPECT-SEQ
           MOVE WS-ZERO-HASH           TO WS-EXPECT-PREV
           IF CA-AC-FROM-SEQ = 0
              EXEC SQL
                 SELECT MAX(CHAIN_SEQ)
                   INTO :WS-MAX-SEALED :WS-MAX-SEALED-IND
                   FROM CLMDB.CLM_AUDIT_CHAIN
                  WHERE SEALED_FLAG = 'Y'
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4400-FIND-ANCHOR' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
              IF WS-MAX-SEALED-IND >= 0
                 COMPUTE WS-START-AFTER = WS-MAX-SEALED + 1
                 PERFORM 4410-READ-ANCHOR
              END-IF
           ELSE
              MOVE CA-AC-FROM-SEQ      TO WS-START-AFTER
              PERFORM 4410-READ-ANCHOR
           END-IF.

      ******************************************************************
      * Nearest row below WS-START-AFTER becomes the trusted anchor;
      * none means the walk starts at the genesis row.
       4410-READ-ANCHOR.
           EXEC SQL
              SELECT CHAIN_SEQ, CHAIN_HASH
                INTO :AUC-CHAIN-SEQ, :AUC-CHAIN-HASH
                FROM CLMDB.CLM_AUDIT_CHAIN
               WHERE CHAIN_SEQ < :WS-START-AFTER
               ORDER BY CHAIN_SEQ DESC
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE 'Y'              TO WS-ANCHOR-FOUND
                 MOVE AUC-CHAIN-SEQ    TO WS-START-AFTER
                 COMPUTE WS-EXPECT-SEQ = AUC-CHAIN-SEQ + 1
                 MOVE AUC-CHAIN-HASH   TO WS-EXPECT-PREV
              WHEN 100
                 MOVE 0                TO WS-START-AFTER
                                          WS-EXPECT-SEQ
              WHEN OTHER
                 MOVE '4410-READ-ANCHOR' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4500-FETCH-LINK.
           EXEC SQL
              FETCH AUCCUR
               INTO :AUC-CHAIN-SEQ, :AUC-PAYLOAD-DIGEST,
                    :AUC-PREV-HASH, :AUC-CHAIN-HASH
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 MOVE 'Y'              TO WS-EOF
              WHEN OTHER
                 MOVE '4500-FETCH-LINK' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4600-CLOSE-CHAIN.
           IF WS-CURSOR-IS-OPEN
              MOVE 'N'                 TO WS-CURSOR-OPEN
              EXEC SQL CLOSE AUCCUR END-EXEC
           END-IF.

      ******************************************************************
      * BR-CLM-AUC-011 Seal only the rows this walk proved intact
       4700-SEAL-VERIFIED.
           IF WS-LAST-GOOD-SEQ > WS-START-AFTER
              EXEC SQL
                 UPDATE CLMDB.CLM_AUDIT_CHAIN
                    SET SEALED_FLAG = 'Y'
                  WHERE CHAIN_SEQ  > :WS-START-AFTER
                    AND CHAIN_SEQ <= :WS-LAST-GOOD-SEQ
                    AND SEALED_FLAG = 'N'
              END-EXEC
              IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
                 MOVE '4700-SEAL-VERIFIED' TO ERC-ER-PARAGRAPH
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF.

      ******************************************************************
      * FNV-1a 64-bit of WS-HASH-INPUT(1:WS-HASH-LEN) -> WS-HASH-HEX.
      * See the HASH FUNCTION notes in the program header.
       5000-FNV1A-64.
           MOVE WS-FNV-OFFSET-HI       TO WS-H-HI
           MOVE WS-FNV-OFFSET-LO       TO WS-H-LO
           PERFORM VARYING WS-H-POS FROM 1 BY 1
                   UNTIL WS-H-POS > WS-HASH-LEN
              COMPUTE WS-H-BYTE =
                      FUNCTION ORD(WS-HASH-INPUT(WS-H-POS:1)) - 1
              PERFORM 5100-XOR-LOW-BYTE
              PERFORM 5200-MULTIPLY-PRIME
           END-PERFORM
           MOVE WS-H-HI                TO WS-H-HALF
           PERFORM 5300-HALF-TO-HEX
           MOVE WS-H-HEX8              TO WS-HASH-HEX(1:8)
           MOVE WS-H-LO                TO WS-H-HALF
           PERFORM 5300-HALF-TO-HEX
           MOVE WS-H-HEX8              TO WS-HASH-HEX(9:8).

      ******************************************************************
      * h = h XOR byte: split LO into Q * 256 + LOW8, XOR the byte
      * into LOW8 one bit at a time, recombine.
       5100-XOR-LOW-BYTE.
           DIVIDE WS-H-LO BY 256 GIVING WS-H-Q REMAINDER WS-H-LOW8
           MOVE WS-H-LOW8              TO WS-H-A
           MOVE WS-H-BYTE              TO WS-H-B
           MOVE 0                      TO WS-H-XOR
           MOVE 1                      TO WS-H-BITVAL
           PERFORM VARYING WS-H-BIT FROM 1 BY 1 UNTIL WS-H-BIT > 8
              DIVIDE WS-H-A BY 2 GIVING WS-H-A REMAINDER WS-H-ABIT
              DIVIDE WS-H-B BY 2 GIVING WS-H-B REMAINDER WS-H-BBIT
              IF WS-H-ABIT NOT = WS-H-BBIT
                 ADD WS-H-BITVAL       TO WS-H-XOR
              END-IF
              COMPUTE WS-H-BITVAL = WS-H-BITVAL * 2
           END-PERFORM
           COMPUTE WS-H-LO = WS-H-Q * 256 + WS-H-XOR.

      ******************************************************************
      * h = h * P MOD 2**64 on the two 32-bit halves
       5200-MULTIPLY-PRIME.
           COMPUTE WS-H-T = WS-H-LO * WS-FNV-PRIME-LO
           DIVIDE WS-H-T BY WS-TWO-32 GIVING WS-H-CARRY
                                      REMAINDER WS-H-T
           COMPUTE WS-H-Q = WS-H-HI * WS-FNV-PRIME-LO
                          + WS-H-LO * WS-FNV-PRIME-HI
                          + WS-H-CARRY
           DIVIDE WS-H-Q BY WS-TWO-32 GIVING WS-H-CARRY
                                      REMAINDER WS-H-HI
           MOVE WS-H-T                 TO WS-H-LO.

      ******************************************************************
      * 32-bit WS-H-HALF -> 8 hex characters in WS-H-HEX8
       5300-HALF-TO-HEX.
           PERFORM VARYING WS-H-HEX-IX FROM 8 BY -1
                   UNTIL WS-H-HEX-IX < 1
              DIVIDE WS-H-HALF BY 16 GIVING WS-H-HALF
                                     REMAINDER WS-H-NIBBLE
              MOVE WS-HEX-DIGITS(WS-H-NIBBLE + 1:1)
                                       TO WS-H-HEX8(WS-H-HEX-IX:1)
           END-PERFORM.

      ******************************************************************
      * BR-CLM-AUC-005 CHAIN_HASH = H(PREV_HASH || DIGEST)
       5500-LINK-HASH.
           MOVE SPACES                 TO WS-HASH-INPUT
           MOVE WS-LINK                TO WS-HASH-INPUT(1:32)
           MOVE 32                     TO WS-HASH-LEN
           PERFORM 5000-FNV1A-64
           MOVE WS-HASH-HEX            TO WS-LINK-HASH.

      ******************************************************************
       8000-NEXT-CHAIN-SEQ.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQC-REQUEST-ID
           MOVE CA-USER-ID             TO SQC-USER-ID
           MOVE CA-BUS-DATE            TO SQC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO SQC-CALLER-PGM
           MOVE WS-AUC-SEQ-NAME        TO SQC-SQ-NAME
           MOVE 1                      TO SQC-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQC-RETURN-CODE >= 12
              MOVE SQC-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQC-REASON-CODE     TO CA-REASON-CODE
              MOVE SQC-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SQC-SQ-VALUE           TO WS-NEW-SEQ.

      ******************************************************************
      * BR-CLM-AUC-012 A broken chain is a security incident: record it
      * in the error log (SQLCODE 0) for the security team's feed.
       8100-LOG-BREAK.
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE 'AUDIT-CHAIN-BREAK'    TO ERC-ER-PARAGRAPH
           MOVE 0                      TO ERC-ER-SQLCODE
           MOVE AUC-CHAIN-SEQ          TO WS-SEQ-DISP
           STRING 'AUDIT CHAIN BROKEN ' WS-BREAK-REASON ' AT SEQ '
                  WS-SEQ-DISP ': ' WS-BREAK-TEXT
                  DELIMITED BY SIZE INTO ERC-ER-TEXT
           MOVE WS-SEQ-DISP            TO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA.

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SAVE-SQLCODE
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           PERFORM 4600-CLOSE-CHAIN
           SET CA-RC-DB2-ERROR         TO TRUE
           IF WS-SAVE-SQLCODE = -911 OR WS-SAVE-SQLCODE = -913
              MOVE 'DEADLOCK'          TO CA-REASON-CODE
           ELSE
              MOVE 'DB2ERR  '          TO CA-REASON-CODE
           END-IF
           STRING 'CLMAUDCH Db2 error SQLCODE ' WS-SQLCODE-DISP
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
