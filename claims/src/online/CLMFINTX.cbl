       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMFINTX.
       AUTHOR. AJAY WALGUDE.
      ******************************************************************
      * PROGRAM     : CLMFINTX                                         *
      * APPLICATION : ClaimsCoreAJ2026 (ClaimCenter parity)            *
      * STYLE       : A - dual-mode service (CICS LINK / batch CALL)   *
      * FUNCTION    : Claim financial transactions. Every movement of  *
      *               money on a claim is a CLM_TRANSACTION row against*
      *               a reserve line (exposure x cost type x category):*
      *               reserve changes (RSV), payments (PAY) with their *
      *               check, deductible withheld (DED), recovery       *
      *               reserves (RRSV) and recovery receipts (RCV).     *
      *               Amounts over the user's authority are held       *
      *               pending approval (PAPR) with an approval activity*
      *               for the first supervisor able to approve them.   *
      *               After every posting the exposure and claim totals*
      *               are recomputed from the reserve lines.           *
      * REQUESTS    : RSVSET PAYCRT RCVRSV RCVCRT TXNAPR TXNREJ TXNVOD *
      *               TXNINQ                                           *
      * CALLED BY   : financial screens, CLMEVTDR, vendor invoice batch*
      * CALLS       : CLMSEQGN CLMERRLG CLMHIST0 CLMAUDCH CLMACTV0     *
      *               CLMCONTC CLMAUTHZ CLMCODVL CLMDATES              *
      * TABLES      : CLM_TRANSACTION (C R U) CLM_RESERVE_LINE (C R U) *
      *               CLM_CHECK (C R U) CLM_EXPOSURE (R U)             *
      *               CLM_CLAIM (R U) CLM_POLICY_COVERAGE (R)          *
      *               CLM_CONTACT (R) CLM_ACTIVITY (R) CLM_CONTROL (R) *
      * STATUSES    : TXN SUBM posted, PAPR pending approval, REJT     *
      *               rejected, VOID voided. CHECK HOLD (awaiting      *
      *               approval), PEND (ready to issue), VOID.          *
      * RULES       : BR-CLM-FIN-001 Reserves and payments need an OPEN*
      *                              claim and an OPEN exposure        *
      *               BR-CLM-FIN-002 Cost types CLMC EXAO EXDC; blank  *
      *                              category is DEFAULT, others must  *
      *                              be in code table COSTCAT          *
      *               BR-CLM-FIN-003 A reserve cannot be set negative  *
      *               BR-CLM-FIN-004 One pending reserve change per    *
      *                              reserve line at a time            *
      *               BR-CLM-FIN-005 A claim-cost reserve cannot exceed*
      *                              the coverage limit not yet paid   *
      *               BR-CLM-FIN-006 Reserve decreases need no         *
      *                              authority; increases are checked  *
      *                              against RSVEXP (exposure reserve) *
      *                              and INCCLM (claim incurred)       *
      *               BR-CLM-FIN-007 Over authority the transaction is *
      *                              PAPR with an APPRPAY activity for *
      *                              the first approver who covers     *
      *                              every failed limit; with no such  *
      *                              approver it is refused (RC 20)    *
      *               BR-CLM-FIN-008 Payment types PART FINL SUPP,     *
      *                              methods CHEQ BACS FAST; amount > 0*
      *               BR-CLM-FIN-009 Scheduled date not before the     *
      *                              business date, at most 90 days    *
      *                              ahead, rolled to a business day   *
      *               BR-CLM-FIN-010 Payee must exist; an unchecked    *
      *                              payee is sanctions screened and a *
      *                              HIT blocks the payment            *
      *               BR-CLM-FIN-011 Deductible is applied once per    *
      *                              exposure on claim-cost payments:  *
      *                              the payment is reduced by the     *
      *                              deductible not yet applied and a  *
      *                              payment within it is rejected     *
      *               BR-CLM-FIN-012 Claim-cost payments cannot exceed *
      *                              the coverage limit less paid and  *
      *                              pending payments on that coverage *
      *               BR-CLM-FIN-013 SUPP payments follow a posted FINL*
      *                              payment and do not erode reserve  *
      *               BR-CLM-FIN-014 An eroding payment larger than the*
      *                              open reserve first raises the     *
      *                              reserve with a linked RSV txn     *
      *               BR-CLM-FIN-015 A FINL payment zeroes the line's  *
      *                              remaining reserve (linked RSV txn)*
      *               BR-CLM-FIN-016 Payment authority: PAYEXP on the  *
      *                              payment, PAYCLM on claim paid to  *
      *                              date including the payment        *
      *               BR-CLM-FIN-017 Recovery categories SUBRO SALVAGE *
      *                              DEDUCT OVERPAY; allowed on OPEN or*
      *                              CLSD exposures of an OPEN claim   *
      *               BR-CLM-FIN-018 A receipt reduces the recovery    *
      *                              reserve, never below zero         *
      *               BR-CLM-FIN-019 OVERPAY recovery is limited to net*
      *                              paid on the line; DEDUCT recovery *
      *                              to the deductible not withheld    *
      *               BR-CLM-FIN-020 The approver cannot be the creator*
      *                              and must hold the authority at the*
      *                              moment of approval                *
      *               BR-CLM-FIN-021 Only posted PAY and RCV can be    *
      *                              voided; a payment only while its  *
      *                              check is not ISSU or CLRD         *
      *               BR-CLM-FIN-022 A payment void restores paid      *
      *                              totals, eroded and final-zeroed   *
      *                              reserve and the deductible        *
      *               BR-CLM-FIN-023 Totals are SUMs over the reserve  *
      *                              lines; incurred = paid + open     *
      *                              reserve - recovered               *
      * CHANGE LOG  : 2026-10-27 AJ  Initial version                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(8)  VALUE 'CLMFINTX'.
       01  WS-SEQ-PGM                  PIC X(8)  VALUE 'CLMSEQGN'.
       01  WS-ERR-PGM                  PIC X(8)  VALUE 'CLMERRLG'.
       01  WS-HIS-PGM                  PIC X(8)  VALUE 'CLMHIST0'.
       01  WS-AUD-PGM                  PIC X(8)  VALUE 'CLMAUDCH'.
       01  WS-ACT-PGM                  PIC X(8)  VALUE 'CLMACTV0'.
       01  WS-CON-PGM                  PIC X(8)  VALUE 'CLMCONTC'.
       01  WS-AUT-PGM                  PIC X(8)  VALUE 'CLMAUTHZ'.
       01  WS-COD-PGM                  PIC X(8)  VALUE 'CLMCODVL'.
       01  WS-DTE-PGM                  PIC X(8)  VALUE 'CLMDATES'.

       01  WS-CONSTANTS.
           05 WS-DEFAULT-CATEGORY      PIC X(8)  VALUE 'DEFAULT '.
           05 WS-APPROVAL-PATTERN      PIC X(8)  VALUE 'APPRPAY '.
           05 WS-MAX-SCHED-DAYS        PIC S9(5) COMP-3 VALUE 90.
           05 WS-MAX-APPROVER-ROUNDS   PIC S9(4) COMP VALUE 5.

       01  WS-CODES.
           05 WS-COST-TYPE             PIC X(4).
              88 WS-COST-CLAIM          VALUE 'CLMC'.
              88 WS-COST-VALID          VALUE 'CLMC' 'EXAO' 'EXDC'.
           05 WS-PAY-TYPE              PIC X(4).
              88 WS-PAY-PARTIAL         VALUE 'PART'.
              88 WS-PAY-FINAL           VALUE 'FINL'.
              88 WS-PAY-SUPPLEMENT      VALUE 'SUPP'.
              88 WS-PAY-TYPE-VALID      VALUE 'PART' 'FINL' 'SUPP'.
           05 WS-PAY-METHOD            PIC X(4).
              88 WS-PAY-METHOD-VALID    VALUE 'CHEQ' 'BACS' 'FAST'.
           05 WS-RCV-CATEGORY          PIC X(8).
              88 WS-RCV-OVERPAY         VALUE 'OVERPAY '.
              88 WS-RCV-DEDUCTIBLE      VALUE 'DEDUCT  '.
              88 WS-RCV-CATEGORY-VALID  VALUE 'SUBRO   ' 'SALVAGE '
                                              'DEDUCT  ' 'OVERPAY '.

       01  WS-WORK.
           05 WS-NEED-APPROVAL         PIC X     VALUE 'N'.
              88 WS-APPROVAL-NEEDED     VALUE 'Y'.
           05 WS-CONFIRMED             PIC X     VALUE 'N'.
              88 WS-APPROVER-CONFIRMED  VALUE 'Y'.
           05 WS-APPROVER              PIC X(8).
           05 WS-AUTH-USER             PIC X(8).
           05 WS-ROUNDS                PIC S9(4) COMP VALUE 0.
           05 WS-AX                    PIC S9(4) COMP VALUE 0.
           05 WS-AUTH-COUNT            PIC S9(4) COMP VALUE 0.
           05 WS-AUTH-ENTRY OCCURS 2 TIMES.
              10 WS-AUTH-TYPE          PIC X(8).
              10 WS-AUTH-AMOUNT        PIC S9(11)V99 COMP-3.
              10 WS-AUTH-OK            PIC X.
              10 WS-AUTH-LIMIT         PIC S9(11)V99 COMP-3.
              10 WS-AUTH-APPROVER      PIC X(8).
           05 WS-AMOUNT                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DELTA                 PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-PAY-NET               PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DED-REMAINING         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DED-TAKEN             PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DED-AMOUNT            PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-COV-PAID              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-COV-PENDING           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-COV-REMAINING         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-EXCLUDE-PENDING       PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-EXP-RESERVE           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-FINAL-ZEROED          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-DED-RECOVERED         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-RR-REDUCTION          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ADJ-OPEN              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ADJ-PENDING           PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ADJ-PAID              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ADJ-RR                PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-ADJ-RECOVERED         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SUM-OPEN              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SUM-PAID              PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SUM-RECOVERED         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-SUM-INCURRED          PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-FINAL-COUNT           PIC S9(9) COMP VALUE 0.
           05 WS-PAY-TXN-ID            PIC S9(9) COMP VALUE 0.
           05 WS-OLD-STATUS            PIC X(4).
           05 WS-DECISION-STATUS       PIC X(4).
           05 WS-DECISION-REASON       PIC X(8).
           05 WS-CHECK-STATUS          PIC X(4).
           05 WS-SCHED-DATE            PIC X(10).
           05 WS-TRAIL                 PIC S9(4) COMP VALUE 0.
           05 WS-NAME-WORK             PIC X(60).
           05 WS-ACTIVITY-USER         PIC X(8).
           05 WS-ID-DISP               PIC 9(9).
           05 WS-ID2-DISP              PIC 9(9).
           05 WS-CHK-DISP              PIC 9(9).
           05 WS-AMT-DISP              PIC -(11)9.99.
           05 WS-AMT2-DISP             PIC -(11)9.99.
           05 WS-SQLCODE-DISP          PIC -9(9).
           05 WS-ERR-PARA              PIC X(30).
           05 WS-SEQ-NAME              PIC X(8).
           05 WS-HIS-EVENT             PIC X(8).
           05 WS-HIS-DESC              PIC X(120) VALUE SPACES.
           05 WS-AUD-EVENT             PIC X(8).
           05 WS-AUD-PAYLOAD           PIC X(400).
           05 WS-COD-TABLE             PIC X(8).
           05 WS-COD-VALUE             PIC X(10).
           05 WS-COD-REASON            PIC X(8).

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE CLMDTXN END-EXEC.
           EXEC SQL INCLUDE CLMDRSL END-EXEC.
           EXEC SQL INCLUDE CLMDCHK END-EXEC.
           EXEC SQL INCLUDE CLMDEXP END-EXEC.
           EXEC SQL INCLUDE CLMDCLM END-EXEC.
           EXEC SQL INCLUDE CLMDPCV END-EXEC.
           EXEC SQL INCLUDE CLMDCON END-EXEC.
           EXEC SQL INCLUDE CLMDACT END-EXEC.
           EXEC SQL INCLUDE CLMDCTL END-EXEC.

       01  WS-SEQ-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==SQA-==.
           03 SQA-BODY.
              COPY CLMCSEQ REPLACING LEADING ==CA-== BY ==SQA-==.
       01  WS-ERR-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ERC-==.
           03 ERC-BODY.
              COPY CLMCERR REPLACING LEADING ==CA-== BY ==ERC-==.
       01  WS-HIS-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==HIA-==.
           03 HIA-BODY.
              COPY CLMCHIS REPLACING LEADING ==CA-== BY ==HIA-==.
       01  WS-AUD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AUA-==.
           03 AUA-BODY.
              COPY CLMCAUC REPLACING LEADING ==CA-== BY ==AUA-==.
       01  WS-ACT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==AVA-==.
           03 AVA-BODY.
              COPY CLMCACT REPLACING LEADING ==CA-== BY ==AVA-==.
       01  WS-CON-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==COA-==.
           03 COA-BODY.
              COPY CLMCCON REPLACING LEADING ==CA-== BY ==COA-==.
       01  WS-AUT-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==ATA-==.
           03 ATA-BODY.
              COPY CLMCAUT REPLACING LEADING ==CA-== BY ==ATA-==.
       01  WS-COD-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==CDA-==.
           03 CDA-BODY.
              COPY CLMCCOD REPLACING LEADING ==CA-== BY ==CDA-==.
       01  WS-DTE-CA.
           COPY CLMCAHDR REPLACING LEADING ==CA-== BY ==DTA-==.
           03 DTA-BODY.
              COPY CLMCDTE REPLACING LEADING ==CA-== BY ==DTA-==.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY CLMCAHDR.
           03 CA-BODY.
              COPY CLMCFIN.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      ******************************************************************
       0000-MAINLINE.
           PERFORM 1000-INITIALISE
           EVALUATE CA-REQUEST-ID
              WHEN 'RSVSET  '
                 PERFORM 2000-SET-RESERVE
              WHEN 'PAYCRT  '
                 PERFORM 2100-CREATE-PAYMENT
              WHEN 'RCVRSV  '
                 PERFORM 2200-SET-RECOVERY-RESERVE
              WHEN 'RCVCRT  '
                 PERFORM 2300-CREATE-RECOVERY
              WHEN 'TXNAPR  '
                 PERFORM 2400-APPROVE-TXN
              WHEN 'TXNREJ  '
                 PERFORM 2500-REJECT-TXN
              WHEN 'TXNVOD  '
                 PERFORM 2600-VOID-TXN
              WHEN 'TXNINQ  '
                 PERFORM 2700-INQUIRE-TXN
              WHEN OTHER
                 SET CA-RC-BAD-REQUEST TO TRUE
                 MOVE 'BADREQ  '       TO CA-REASON-CODE
                 STRING 'CLMFINTX: unknown request '
                        CA-REQUEST-ID DELIMITED BY SIZE
                        INTO CA-MESSAGE
           END-EVALUATE
           PERFORM 9000-RETURN.

      ******************************************************************
       1000-INITIALISE.
           SET CA-RC-OK                TO TRUE
           MOVE SPACES                 TO CA-REASON-CODE
                                          CA-MESSAGE
                                          WS-APPROVER
           MOVE 'N'                    TO WS-NEED-APPROVAL
           MOVE 0                      TO WS-AUTH-COUNT
                                          WS-DED-TAKEN
           IF CA-USER-ID = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'USERREQD'          TO CA-REASON-CODE
              MOVE 'User id is required for financial transactions'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-BUS-DATE = SPACES
              PERFORM 4030-READ-CONTROL
           END-IF.

      ******************************************************************
      * RSVSET - set the open reserve of one reserve line
       2000-SET-RESERVE.
           PERFORM 3000-LOAD-EXPOSURE
           PERFORM 3010-REQUIRE-OPEN
           PERFORM 3100-VALIDATE-COST
      * BR-CLM-FIN-003 A reserve cannot be set negative
           IF CA-FT-AMOUNT < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RSVNEG  '          TO CA-REASON-CODE
              MOVE 'Reserve amount cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-GET-RESERVE-LINE
      * BR-CLM-FIN-004 One pending reserve change per line
           IF RSL-PENDING-APPROVAL NOT = 0
              SET CA-RC-CONFLICT       TO TRUE
              MOVE 'RSVPEND '          TO CA-REASON-CODE
              MOVE RSL-PENDING-APPROVAL TO WS-AMT-DISP
              STRING 'Reserve change of ' WS-AMT-DISP
                     ' already awaits approval on this line'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           COMPUTE WS-DELTA = CA-FT-AMOUNT - RSL-OPEN-RESERVE
           IF WS-DELTA = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOCHANGE'          TO CA-REASON-CODE
              MOVE 'Reserve already at the requested amount'
                                       TO CA-MESSAGE
              PERFORM 5810-FILL-RL-RESPONSE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-FIN-005 Claim-cost reserve within unpaid coverage limit
           IF WS-COST-CLAIM
              MOVE 0                   TO WS-EXCLUDE-PENDING
              PERFORM 4300-SUM-COVERAGE-PAID
              IF CA-FT-AMOUNT > WS-COV-REMAINING
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RSVLIMIT'       TO CA-REASON-CODE
                 MOVE WS-COV-REMAINING TO WS-AMT-DISP
                 STRING 'Reserve exceeds coverage limit remaining '
                        WS-AMT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
      * BR-CLM-FIN-006 Only increases are authority checked
           IF WS-DELTA > 0
              PERFORM 4130-SUM-EXPOSURE-RESERVE
              MOVE 'RSVEXP  '          TO WS-AUTH-TYPE(1)
              COMPUTE WS-AUTH-AMOUNT(1) = WS-EXP-RESERVE + WS-DELTA
              MOVE 'INCCLM  '          TO WS-AUTH-TYPE(2)
              COMPUTE WS-AUTH-AMOUNT(2) =
                      CLM-TOTAL-INCURRED + WS-DELTA
              MOVE 2                   TO WS-AUTH-COUNT
              PERFORM 5500-EVALUATE-AUTHORITY
           END-IF

           PERFORM 5000-PREPARE-TXN
           MOVE 'RSV '                 TO TXN-TXN-TYPE
           MOVE WS-DELTA               TO TXN-TXN-AMOUNT
           MOVE 'N'                    TO TXN-ERODES-RESERVE
           PERFORM 4400-INSERT-TXN
           IF WS-APPROVAL-NEEDED
              MOVE 0                   TO WS-ADJ-OPEN
              MOVE WS-DELTA            TO WS-ADJ-PENDING
           ELSE
              MOVE WS-DELTA            TO WS-ADJ-OPEN
              MOVE 0                   TO WS-ADJ-PENDING
           END-IF
           MOVE 0                      TO WS-ADJ-PAID WS-ADJ-RR
                                          WS-ADJ-RECOVERED
           PERFORM 4200-ADJUST-RESERVE-LINE

           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE CA-FT-AMOUNT           TO WS-AMT-DISP
           MOVE WS-DELTA               TO WS-AMT2-DISP
           IF WS-APPROVAL-NEEDED
              PERFORM 8600-RAISE-APPROVAL-ACTIVITY
              MOVE 'RSVPAPR '          TO WS-HIS-EVENT
              STRING 'Reserve ' CA-FT-COST-TYPE ' to ' WS-AMT-DISP
                     ' (' WS-AMT2-DISP ') pending approval by '
                     WS-APPROVER DELIMITED BY SIZE INTO WS-HIS-DESC
           ELSE
              MOVE 'RSVSET  '          TO WS-HIS-EVENT
              STRING 'Reserve ' CA-FT-COST-TYPE ' set to '
                     WS-AMT-DISP ' (' WS-AMT2-DISP ') txn '
                     WS-ID-DISP DELIMITED BY SIZE INTO WS-HIS-DESC
           END-IF
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * PAYCRT - payment to a payee with its check
       2100-CREATE-PAYMENT.
           PERFORM 3000-LOAD-EXPOSURE
           PERFORM 3010-REQUIRE-OPEN
           PERFORM 3100-VALIDATE-COST
           PERFORM 3200-VALIDATE-PAYMENT-FIELDS
           PERFORM 3210-VALIDATE-SCHEDULED-DATE
           PERFORM 3300-VALIDATE-PAYEE
           PERFORM 4100-GET-RESERVE-LINE
           MOVE CA-FT-AMOUNT           TO WS-PAY-NET
           IF WS-COST-CLAIM
              PERFORM 3400-APPLY-DEDUCTIBLE
      * BR-CLM-FIN-012 Payment within the unpaid coverage limit
              MOVE 0                   TO WS-EXCLUDE-PENDING
              PERFORM 4300-SUM-COVERAGE-PAID
              IF WS-PAY-NET > WS-COV-REMAINING
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'OVERLIM '       TO CA-REASON-CODE
                 MOVE WS-COV-REMAINING TO WS-AMT-DISP
                 STRING 'Payment exceeds coverage limit remaining '
                        WS-AMT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
      * BR-CLM-FIN-016 Single payment and claim cumulative authority
           MOVE 'PAYEXP  '             TO WS-AUTH-TYPE(1)
           MOVE WS-PAY-NET             TO WS-AUTH-AMOUNT(1)
           MOVE 'PAYCLM  '             TO WS-AUTH-TYPE(2)
           COMPUTE WS-AUTH-AMOUNT(2) = CLM-TOTAL-PAID + WS-PAY-NET
           MOVE 2                      TO WS-AUTH-COUNT
           PERFORM 5500-EVALUATE-AUTHORITY

           PERFORM 5000-PREPARE-TXN
           MOVE 'PAY '                 TO TXN-TXN-TYPE
           MOVE WS-PAY-NET             TO TXN-TXN-AMOUNT
           MOVE WS-PAY-TYPE            TO TXN-PAYMENT-TYPE
           IF WS-PAY-SUPPLEMENT
              MOVE 'N'                 TO TXN-ERODES-RESERVE
           ELSE
              MOVE 'Y'                 TO TXN-ERODES-RESERVE
           END-IF
           PERFORM 2110-CREATE-CHECK
           MOVE CHK-CHECK-ID           TO TXN-CHECK-ID
           PERFORM 4400-INSERT-TXN
           MOVE TXN-TXN-ID             TO WS-PAY-TXN-ID

           IF WS-DED-TAKEN > 0
              PERFORM 5000-PREPARE-TXN
              MOVE 'DED '              TO TXN-TXN-TYPE
              MOVE WS-DED-TAKEN        TO TXN-TXN-AMOUNT
              MOVE 'N'                 TO TXN-ERODES-RESERVE
              MOVE WS-PAY-TXN-ID       TO TXN-RELATED-TXN-ID
              PERFORM 4400-INSERT-TXN
              MOVE WS-DED-TAKEN        TO WS-DED-AMOUNT
              PERFORM 4600-ADJUST-DEDUCTIBLE-APPLIED
           END-IF

           PERFORM 4415-REREAD-PRIMARY-TXN
           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE WS-PAY-NET             TO WS-AMT-DISP
           MOVE WS-DED-TAKEN           TO WS-AMT2-DISP
           IF WS-APPROVAL-NEEDED
              PERFORM 8600-RAISE-APPROVAL-ACTIVITY
              MOVE 'PAYPAPR '          TO WS-HIS-EVENT
              STRING 'Payment ' WS-ID-DISP ' ' WS-PAY-TYPE ' '
                     WS-AMT-DISP ' pending approval by ' WS-APPROVER
                     DELIMITED BY SIZE INTO WS-HIS-DESC
           ELSE
              PERFORM 5300-POST-PAYMENT
              PERFORM 4415-REREAD-PRIMARY-TXN
              MOVE 'PAYCRT  '          TO WS-HIS-EVENT
              STRING 'Payment ' WS-ID-DISP ' ' WS-PAY-TYPE ' '
                     WS-AMT-DISP ' deductible ' WS-AMT2-DISP
                     DELIMITED BY SIZE INTO WS-HIS-DESC
           END-IF
           MOVE WS-PAY-NET             TO CA-FT-AMOUNT
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * BR-CLM-FIN-017 A pending payment's check is held until approval
       2110-CREATE-CHECK.
           MOVE 'CHK'                  TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO CHK-CHECK-ID
           MOVE 'CHKNO'                TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-FORMATTED       TO CHK-CHECK-NUMBER
           MOVE CLM-CLAIM-ID           TO CHK-CLAIM-ID
           MOVE CON-CONTACT-ID         TO CHK-PAYEE-CONTACT-ID
           MOVE WS-NAME-WORK           TO CHK-PAYEE-NAME-TEXT
           MOVE 0                      TO WS-TRAIL
           INSPECT FUNCTION REVERSE(WS-NAME-WORK)
                   TALLYING WS-TRAIL FOR LEADING SPACES
           COMPUTE CHK-PAYEE-NAME-LEN =
                   LENGTH OF WS-NAME-WORK - WS-TRAIL
           MOVE CON-ADDRESS-LINE1-LEN  TO CHK-PAYEE-ADDR1-LEN
           MOVE CON-ADDRESS-LINE1-TEXT TO CHK-PAYEE-ADDR1-TEXT
           MOVE CON-ADDRESS-LINE1-IND  TO CHK-PAYEE-ADDR1-IND
           MOVE CON-POSTCODE           TO CHK-PAYEE-POSTCODE
           MOVE CON-POSTCODE-IND       TO CHK-PAYEE-POSTCODE-IND
           MOVE WS-PAY-NET             TO CHK-CHECK-AMOUNT
           MOVE WS-PAY-METHOD          TO CHK-PAY-METHOD
           MOVE WS-SCHED-DATE          TO CHK-SCHEDULED-DATE
           IF WS-APPROVAL-NEEDED
              MOVE 'HOLD'              TO CHK-CHECK-STATUS
           ELSE
              MOVE 'PEND'              TO CHK-CHECK-STATUS
           END-IF
           PERFORM 4510-INSERT-CHECK
           MOVE CHK-CHECK-ID           TO CA-FT-CHECK-ID
           MOVE WS-SCHED-DATE          TO CA-FT-SCHEDULED-DATE.

      ******************************************************************
      * RCVRSV - set the recovery reserve of one reserve line
       2200-SET-RECOVERY-RESERVE.
           PERFORM 3000-LOAD-EXPOSURE
           PERFORM 3600-VALIDATE-RECOVERY
           IF CA-FT-AMOUNT < 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RRSVNEG '          TO CA-REASON-CODE
              MOVE 'Recovery reserve cannot be negative'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-GET-RESERVE-LINE
           COMPUTE WS-DELTA = CA-FT-AMOUNT - RSL-RECOVERY-RESERVE
           IF WS-DELTA = 0
              SET CA-RC-WARNING        TO TRUE
              MOVE 'NOCHANGE'          TO CA-REASON-CODE
              MOVE 'Recovery reserve already at the requested amount'
                                       TO CA-MESSAGE
              PERFORM 5810-FILL-RL-RESPONSE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 5000-PREPARE-TXN
           MOVE 'RRSV'                 TO TXN-TXN-TYPE
           MOVE WS-DELTA               TO TXN-TXN-AMOUNT
           MOVE 'N'                    TO TXN-ERODES-RESERVE
           MOVE WS-RCV-CATEGORY        TO TXN-RECOVERY-CATEGORY
           PERFORM 4400-INSERT-TXN
           MOVE 0                      TO WS-ADJ-OPEN WS-ADJ-PENDING
                                          WS-ADJ-PAID WS-ADJ-RECOVERED
           MOVE WS-DELTA               TO WS-ADJ-RR
           PERFORM 4200-ADJUST-RESERVE-LINE
           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE CA-FT-AMOUNT           TO WS-AMT-DISP
           MOVE 'RCVRSV  '             TO WS-HIS-EVENT
           STRING 'Recovery reserve ' WS-RCV-CATEGORY ' set to '
                  WS-AMT-DISP ' txn ' WS-ID-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * RCVCRT - recovery receipt
       2300-CREATE-RECOVERY.
           PERFORM 3000-LOAD-EXPOSURE
           PERFORM 3600-VALIDATE-RECOVERY
           IF CA-FT-AMOUNT <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RCVAMT  '          TO CA-REASON-CODE
              MOVE 'Recovery amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4100-GET-RESERVE-LINE
      * BR-CLM-FIN-019 Category ceilings
           IF WS-RCV-OVERPAY
              IF CA-FT-AMOUNT >
                 RSL-TOTAL-PAID - RSL-TOTAL-RECOVERED
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RCVOVPAY'       TO CA-REASON-CODE
                 MOVE 'Overpayment recovery exceeds net paid on line'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           IF WS-RCV-DEDUCTIBLE
              PERFORM 4320-SUM-DEDUCT-RECOVERED
              IF CA-FT-AMOUNT > PCV-DEDUCTIBLE-AMOUNT
                                - EXP-DEDUCTIBLE-APPLIED
                                - WS-DED-RECOVERED
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'RCVDEDUC'       TO CA-REASON-CODE
                 MOVE 'Deductible recovery exceeds deductible owed'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
      * BR-CLM-FIN-018 Receipt erodes recovery reserve, floor zero
           IF CA-FT-AMOUNT > RSL-RECOVERY-RESERVE
              MOVE RSL-RECOVERY-RESERVE TO WS-RR-REDUCTION
           ELSE
              MOVE CA-FT-AMOUNT        TO WS-RR-REDUCTION
           END-IF
           PERFORM 5000-PREPARE-TXN
           MOVE 'RCV '                 TO TXN-TXN-TYPE
           MOVE CA-FT-AMOUNT           TO TXN-TXN-AMOUNT
           MOVE 'Y'                    TO TXN-ERODES-RESERVE
           MOVE WS-RCV-CATEGORY        TO TXN-RECOVERY-CATEGORY
           PERFORM 4400-INSERT-TXN
           MOVE 0                      TO WS-ADJ-OPEN WS-ADJ-PENDING
                                          WS-ADJ-PAID
           COMPUTE WS-ADJ-RR = 0 - WS-RR-REDUCTION
           MOVE CA-FT-AMOUNT           TO WS-ADJ-RECOVERED
           PERFORM 4200-ADJUST-RESERVE-LINE
           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE CA-FT-AMOUNT           TO WS-AMT-DISP
           MOVE 'RCVCRT  '             TO WS-HIS-EVENT
           STRING 'Recovery ' WS-RCV-CATEGORY ' received '
                  WS-AMT-DISP ' txn ' WS-ID-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * TXNAPR - approve a pending reserve change or payment
       2400-APPROVE-TXN.
           PERFORM 3700-LOAD-PENDING-TXN
      * BR-CLM-FIN-020 No self approval; authority checked now
           IF TXN-CREATE-USER = CA-USER-ID
              SET CA-RC-AUTHORITY      TO TRUE
              MOVE 'SELFAPRV'          TO CA-REASON-CODE
              MOVE 'A transaction cannot be approved by its creator'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE TXN-EXPOSURE-ID        TO CA-FT-EXPOSURE-ID
           MOVE TXN-CLAIM-ID           TO CA-FT-CLAIM-ID
           PERFORM 3000-LOAD-EXPOSURE
           PERFORM 3010-REQUIRE-OPEN
           MOVE TXN-RL-ID              TO RSL-RL-ID
           PERFORM 4120-READ-RL-BY-ID
           MOVE RSL-COST-TYPE          TO WS-COST-TYPE
           MOVE TXN-TXN-AMOUNT         TO WS-AMOUNT
           EVALUATE TXN-TXN-TYPE
              WHEN 'RSV '
                 PERFORM 4130-SUM-EXPOSURE-RESERVE
                 MOVE 'RSVEXP  '       TO WS-AUTH-TYPE(1)
                 COMPUTE WS-AUTH-AMOUNT(1) = WS-EXP-RESERVE + WS-AMOUNT
                 MOVE 'INCCLM  '       TO WS-AUTH-TYPE(2)
                 COMPUTE WS-AUTH-AMOUNT(2) =
                         CLM-TOTAL-INCURRED + WS-AMOUNT
              WHEN 'PAY '
                 MOVE 'PAYEXP  '       TO WS-AUTH-TYPE(1)
                 MOVE WS-AMOUNT        TO WS-AUTH-AMOUNT(1)
                 MOVE 'PAYCLM  '       TO WS-AUTH-TYPE(2)
                 COMPUTE WS-AUTH-AMOUNT(2) =
                         CLM-TOTAL-PAID + WS-AMOUNT
           END-EVALUATE
           MOVE 2                      TO WS-AUTH-COUNT
           PERFORM 5520-CHECK-APPROVER-AUTHORITY

           MOVE TXN-TXN-ID             TO WS-PAY-TXN-ID
           IF TXN-TXN-TYPE = 'PAY '
              PERFORM 2410-REVALIDATE-PAYMENT
           END-IF
           MOVE 'SUBM'                 TO WS-DECISION-STATUS
           MOVE SPACES                 TO WS-DECISION-REASON
           PERFORM 4420-UPDATE-TXN-DECISION
           PERFORM 4430-DECIDE-LINKED-DED
           PERFORM 4415-REREAD-PRIMARY-TXN

           IF TXN-TXN-TYPE = 'RSV '
              MOVE WS-AMOUNT           TO WS-ADJ-OPEN
              COMPUTE WS-ADJ-PENDING = 0 - WS-AMOUNT
              MOVE 0                   TO WS-ADJ-PAID WS-ADJ-RR
                                          WS-ADJ-RECOVERED
              PERFORM 4200-ADJUST-RESERVE-LINE
           ELSE
              MOVE TXN-PAYMENT-TYPE    TO WS-PAY-TYPE
              MOVE WS-AMOUNT           TO WS-PAY-NET
              PERFORM 5300-POST-PAYMENT
              PERFORM 4415-REREAD-PRIMARY-TXN
              MOVE TXN-CHECK-ID        TO CHK-CHECK-ID
              MOVE 'PEND'              TO WS-CHECK-STATUS
              MOVE SPACES              TO CHK-VOID-REASON
              PERFORM 4530-UPDATE-CHECK-STATUS
           END-IF
           PERFORM 8610-COMPLETE-APPROVAL-ACTIVITY

           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE WS-AMOUNT              TO WS-AMT-DISP
           MOVE 'TXNAPR  '             TO WS-HIS-EVENT
           STRING TXN-TXN-TYPE ' txn ' WS-ID-DISP ' for '
                  WS-AMT-DISP ' approved by ' CA-USER-ID
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * A payment waiting for approval is re-tested against the limit
      * (excluding itself from the pending total) and the payee's
      * sanctions status before money moves.
       2410-REVALIDATE-PAYMENT.
           IF WS-COST-CLAIM
              MOVE WS-AMOUNT           TO WS-EXCLUDE-PENDING
              PERFORM 4300-SUM-COVERAGE-PAID
              IF WS-AMOUNT > WS-COV-REMAINING
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'OVERLIM '       TO CA-REASON-CODE
                 MOVE WS-COV-REMAINING TO WS-AMT-DISP
                 STRING 'Coverage limit remaining is now '
                        WS-AMT-DISP ' - reject the payment'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF
           MOVE TXN-CHECK-ID           TO CHK-CHECK-ID
           PERFORM 4520-READ-CHECK
           MOVE CHK-PAYEE-CONTACT-ID   TO CA-FT-PAYEE-CONTACT-ID
           PERFORM 3300-VALIDATE-PAYEE.

      ******************************************************************
      * TXNREJ - reject a pending transaction
       2500-REJECT-TXN.
           PERFORM 3700-LOAD-PENDING-TXN
           MOVE 'TXNRJRSN'             TO WS-COD-TABLE
           MOVE 'REJRSN  '             TO WS-COD-REASON
           PERFORM 3800-REQUIRE-REASON
           MOVE TXN-EXPOSURE-ID        TO CA-FT-EXPOSURE-ID
           MOVE TXN-CLAIM-ID           TO CA-FT-CLAIM-ID
           PERFORM 3000-LOAD-EXPOSURE
           MOVE TXN-TXN-ID             TO WS-PAY-TXN-ID
           MOVE TXN-TXN-AMOUNT         TO WS-AMOUNT
           MOVE 'REJT'                 TO WS-DECISION-STATUS
           MOVE CA-FT-REASON           TO WS-DECISION-REASON
           PERFORM 4420-UPDATE-TXN-DECISION
           IF TXN-TXN-TYPE = 'RSV '
              MOVE TXN-RL-ID           TO RSL-RL-ID
              PERFORM 4120-READ-RL-BY-ID
              MOVE 0                   TO WS-ADJ-OPEN WS-ADJ-PAID
                                          WS-ADJ-RR WS-ADJ-RECOVERED
              COMPUTE WS-ADJ-PENDING = 0 - WS-AMOUNT
              PERFORM 4200-ADJUST-RESERVE-LINE
           ELSE
              MOVE TXN-CHECK-ID        TO CHK-CHECK-ID
              MOVE 'VOID'              TO WS-CHECK-STATUS
              MOVE CA-FT-REASON        TO CHK-VOID-REASON
              PERFORM 4530-UPDATE-CHECK-STATUS
              PERFORM 4430-DECIDE-LINKED-DED
              IF WS-DED-AMOUNT > 0
                 COMPUTE WS-DED-AMOUNT = 0 - WS-DED-AMOUNT
                 PERFORM 4600-ADJUST-DEDUCTIBLE-APPLIED
              END-IF
              MOVE TXN-RL-ID           TO RSL-RL-ID
              PERFORM 4120-READ-RL-BY-ID
           END-IF
           PERFORM 4415-REREAD-PRIMARY-TXN
           PERFORM 8610-COMPLETE-APPROVAL-ACTIVITY
           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE WS-AMOUNT              TO WS-AMT-DISP
           MOVE 'TXNREJ  '             TO WS-HIS-EVENT
           STRING TXN-TXN-TYPE ' txn ' WS-ID-DISP ' for '
                  WS-AMT-DISP ' rejected ' CA-FT-REASON ' by '
                  CA-USER-ID DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * TXNVOD - void a posted payment or recovery receipt
      * BR-CLM-FIN-021 Only posted PAY / RCV can be voided
       2600-VOID-TXN.
           PERFORM 3710-LOAD-TXN
           IF TXN-TXN-STATUS NOT = 'SUBM'
              SET CA-RC-STATE          TO TRUE
              MOVE 'TXNSTATE'          TO CA-REASON-CODE
              STRING 'Only posted transactions can be voided (status '
                     TXN-TXN-STATUS ')'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF TXN-TXN-TYPE NOT = 'PAY ' AND TXN-TXN-TYPE NOT = 'RCV '
              SET CA-RC-STATE          TO TRUE
              MOVE 'NOVOID  '          TO CA-REASON-CODE
              STRING TXN-TXN-TYPE ' transactions cannot be voided -'
                     ' post a correcting reserve instead'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'VOIDRSN '             TO WS-COD-TABLE
           MOVE 'VOIDRSN '             TO WS-COD-REASON
           PERFORM 3800-REQUIRE-REASON
           MOVE TXN-EXPOSURE-ID        TO CA-FT-EXPOSURE-ID
           MOVE TXN-CLAIM-ID           TO CA-FT-CLAIM-ID
           PERFORM 3000-LOAD-EXPOSURE
           MOVE TXN-TXN-ID             TO WS-PAY-TXN-ID
           MOVE TXN-TXN-AMOUNT         TO WS-AMOUNT
           MOVE TXN-RL-ID              TO RSL-RL-ID
           PERFORM 4120-READ-RL-BY-ID
           IF TXN-TXN-TYPE = 'PAY '
              PERFORM 2610-VOID-PAYMENT
           ELSE
              PERFORM 2620-VOID-RECOVERY
           END-IF
           MOVE 'VOID'                 TO WS-DECISION-STATUS
           MOVE CA-FT-REASON           TO WS-DECISION-REASON
           MOVE 'SUBM'                 TO WS-OLD-STATUS
           PERFORM 4425-UPDATE-TXN-STATUS
           PERFORM 4415-REREAD-PRIMARY-TXN
           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE WS-AMOUNT              TO WS-AMT-DISP
           MOVE 'TXNVOD  '             TO WS-HIS-EVENT
           STRING TXN-TXN-TYPE ' txn ' WS-ID-DISP ' for '
                  WS-AMT-DISP ' voided ' CA-FT-REASON ' by '
                  CA-USER-ID DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 2900-FINISH-POSTING.

      ******************************************************************
      * BR-CLM-FIN-022 Undo everything the payment posting did except
      * an automatic reserve increase, which still reflects the cost
       2610-VOID-PAYMENT.
           PERFORM 3010-REQUIRE-OPEN
           MOVE TXN-CHECK-ID           TO CHK-CHECK-ID
           PERFORM 4520-READ-CHECK
           IF CHK-CHECK-STATUS = 'ISSU' OR 'CLRD' OR 'VOID'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CHKSTATE'          TO CA-REASON-CODE
              STRING 'Check ' CHK-CHECK-NUMBER ' is '
                     CHK-CHECK-STATUS
                     ' - stop it with the bank, then record a recovery'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 0                      TO WS-ADJ-PENDING WS-ADJ-RR
                                          WS-ADJ-RECOVERED
                                          WS-ADJ-OPEN
           COMPUTE WS-ADJ-PAID = 0 - WS-AMOUNT
           IF TXN-ERODES-RESERVE = 'Y'
              MOVE WS-AMOUNT           TO WS-ADJ-OPEN
           END-IF
           PERFORM 4200-ADJUST-RESERVE-LINE
           IF TXN-PAYMENT-TYPE = 'FINL'
              PERFORM 4440-SUM-FINAL-ZEROING
              IF WS-FINAL-ZEROED > 0
                 PERFORM 5000-PREPARE-TXN
                 MOVE 'SUBM'           TO TXN-TXN-STATUS
                 MOVE 'RSV '           TO TXN-TXN-TYPE
                 MOVE WS-FINAL-ZEROED  TO TXN-TXN-AMOUNT
                 MOVE 'N'              TO TXN-ERODES-RESERVE
                 MOVE WS-PAY-TXN-ID    TO TXN-RELATED-TXN-ID
                 PERFORM 4400-INSERT-TXN
                 MOVE WS-FINAL-ZEROED  TO WS-ADJ-OPEN
                 MOVE 0                TO WS-ADJ-PAID
                 PERFORM 4200-ADJUST-RESERVE-LINE
              END-IF
           END-IF
           MOVE WS-PAY-TXN-ID          TO TXN-TXN-ID
           MOVE 'VOID'                 TO WS-DECISION-STATUS
           MOVE CA-FT-REASON           TO WS-DECISION-REASON
           PERFORM 4430-DECIDE-LINKED-DED
           IF WS-DED-AMOUNT > 0
              COMPUTE WS-DED-AMOUNT = 0 - WS-DED-AMOUNT
              PERFORM 4600-ADJUST-DEDUCTIBLE-APPLIED
           END-IF
           MOVE 'VOID'                 TO WS-CHECK-STATUS
           MOVE CA-FT-REASON           TO CHK-VOID-REASON
           PERFORM 4530-UPDATE-CHECK-STATUS.

      ******************************************************************
       2620-VOID-RECOVERY.
           MOVE 0                      TO WS-ADJ-OPEN WS-ADJ-PENDING
                                          WS-ADJ-PAID WS-ADJ-RR
           COMPUTE WS-ADJ-RECOVERED = 0 - WS-AMOUNT
           PERFORM 4200-ADJUST-RESERVE-LINE.

      ******************************************************************
       2700-INQUIRE-TXN.
           PERFORM 3710-LOAD-TXN
           MOVE TXN-RL-ID              TO RSL-RL-ID
           PERFORM 4120-READ-RL-BY-ID
           PERFORM 5800-FILL-RESPONSE
           PERFORM 4700-FIND-APPROVAL-ACTIVITY
           IF SQLCODE = 0
              MOVE ACT-ACTIVITY-ID     TO CA-FT-ACTIVITY-ID
           END-IF.

      ******************************************************************
      * Common tail of every posting: totals, history, audit, response
       2900-FINISH-POSTING.
           PERFORM 5900-RECOMPUTE-TOTALS
           PERFORM 8100-WRITE-HISTORY
           PERFORM 5850-BUILD-AUDIT-PAYLOAD
           MOVE WS-HIS-EVENT           TO WS-AUD-EVENT
           PERFORM 8200-WRITE-AUDIT
           MOVE TXN-RL-ID              TO RSL-RL-ID
           PERFORM 4120-READ-RL-BY-ID
           PERFORM 5800-FILL-RESPONSE
           IF WS-APPROVAL-NEEDED
              SET CA-RC-WARNING        TO TRUE
              MOVE 'APRVREQD'          TO CA-REASON-CODE
              MOVE TXN-TXN-ID          TO WS-ID-DISP
              STRING 'Txn ' WS-ID-DISP ' over authority - pending '
                     'approval by ' WS-APPROVER
                     DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
              MOVE TXN-TXN-ID          TO WS-ID-DISP
              STRING CA-REQUEST-ID ' complete, txn ' WS-ID-DISP
                     ' status ' TXN-TXN-STATUS
                     DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.

      ******************************************************************
       3000-LOAD-EXPOSURE.
           IF CA-FT-EXPOSURE-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPIDREQ'          TO CA-REASON-CODE
              MOVE 'Exposure id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4010-READ-EXPOSURE
           IF CA-FT-CLAIM-ID NOT = 0
              AND CA-FT-CLAIM-ID NOT = EXP-CLAIM-ID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'EXPOTHCL'          TO CA-REASON-CODE
              MOVE 'Exposure does not belong to the claim given'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE EXP-CLAIM-ID           TO CA-FT-CLAIM-ID
           PERFORM 4000-READ-CLAIM
           PERFORM 4020-READ-COVERAGE.

      ******************************************************************
      * BR-CLM-FIN-001 Money moves only on open claims and exposures
       3010-REQUIRE-OPEN.
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMSTATE'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ' - financials need OPEN'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF EXP-EXPOSURE-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPSTATE'          TO CA-REASON-CODE
              STRING 'Exposure is ' EXP-EXPOSURE-STATE
                     ' - reopen it before posting financials'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
      * BR-CLM-FIN-002 Cost type and category
       3100-VALIDATE-COST.
           MOVE CA-FT-COST-TYPE        TO WS-COST-TYPE
           IF NOT WS-COST-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'COSTTYPE'          TO CA-REASON-CODE
              STRING 'Cost type ' CA-FT-COST-TYPE
                     ' must be CLMC EXAO or EXDC'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-FT-COST-CATEGORY = SPACES
              MOVE WS-DEFAULT-CATEGORY TO CA-FT-COST-CATEGORY
           END-IF
           IF CA-FT-COST-CATEGORY NOT = WS-DEFAULT-CATEGORY
              MOVE 'COSTCAT '          TO WS-COD-TABLE
              MOVE CA-FT-COST-CATEGORY TO WS-COD-VALUE
              MOVE 'COSTCAT '          TO WS-COD-REASON
              PERFORM 8300-VALIDATE-CODE
           END-IF.

      ******************************************************************
      * BR-CLM-FIN-008 Payment type, method and amount
       3200-VALIDATE-PAYMENT-FIELDS.
           IF CA-FT-AMOUNT <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAYAMT  '          TO CA-REASON-CODE
              MOVE 'Payment amount must be greater than zero'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-FT-PAYMENT-TYPE = SPACES
              MOVE 'PART'              TO CA-FT-PAYMENT-TYPE
           END-IF
           MOVE CA-FT-PAYMENT-TYPE     TO WS-PAY-TYPE
           IF NOT WS-PAY-TYPE-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAYTYPE '          TO CA-REASON-CODE
              STRING 'Payment type ' CA-FT-PAYMENT-TYPE
                     ' must be PART FINL or SUPP'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CA-FT-PAY-METHOD = SPACES
              MOVE 'CHEQ'              TO CA-FT-PAY-METHOD
           END-IF
           MOVE CA-FT-PAY-METHOD       TO WS-PAY-METHOD
           IF NOT WS-PAY-METHOD-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAYMETH '          TO CA-REASON-CODE
              STRING 'Payment method ' CA-FT-PAY-METHOD
                     ' must be CHEQ BACS or FAST'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
      * BR-CLM-FIN-013 Supplements only after a posted final payment
           IF WS-PAY-SUPPLEMENT
              PERFORM 4310-COUNT-FINAL-PAYMENTS
              IF WS-FINAL-COUNT = 0
                 SET CA-RC-STATE       TO TRUE
                 MOVE 'NOFINAL '       TO CA-REASON-CODE
                 MOVE 'Supplement needs a posted final payment first'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-IF.

      ******************************************************************
      * BR-CLM-FIN-009 Not back-dated, <= 90 days out, business day
       3210-VALIDATE-SCHEDULED-DATE.
           IF CA-FT-SCHEDULED-DATE = SPACES
              MOVE CA-BUS-DATE         TO CA-FT-SCHEDULED-DATE
           END-IF
           MOVE 'DTVALD  '             TO DTA-REQUEST-ID
           MOVE CA-FT-SCHEDULED-DATE   TO DTA-DT-DATE-1
           PERFORM 8500-CALL-DATES
           IF NOT DTA-DT-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SCHDATE '          TO CA-REASON-CODE
              STRING 'Scheduled date ' CA-FT-SCHEDULED-DATE
                     ' is not a valid date'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DTDIFF  '             TO DTA-REQUEST-ID
           MOVE CA-BUS-DATE            TO DTA-DT-DATE-1
           MOVE CA-FT-SCHEDULED-DATE   TO DTA-DT-DATE-2
           PERFORM 8500-CALL-DATES
           IF DTA-DT-RESULT-NUM < 0
              OR DTA-DT-RESULT-NUM > WS-MAX-SCHED-DAYS
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SCHRANGE'          TO CA-REASON-CODE
              STRING 'Scheduled date must be from ' CA-BUS-DATE
                     ' up to 90 days ahead'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'DTBUSD  '             TO DTA-REQUEST-ID
           MOVE CA-FT-SCHEDULED-DATE   TO DTA-DT-DATE-1
           PERFORM 8500-CALL-DATES
           MOVE DTA-DT-RESULT-DATE     TO WS-SCHED-DATE.

      ******************************************************************
      * BR-CLM-FIN-010 Payee exists and is not a sanctions hit
       3300-VALIDATE-PAYEE.
           IF CA-FT-PAYEE-CONTACT-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAYEEREQ'          TO CA-REASON-CODE
              MOVE 'Payee contact id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 4500-READ-CONTACT
           IF CON-SANCTION-STATUS = 'UNCK'
              PERFORM 8400-SCREEN-PAYEE
           END-IF
           IF CON-SANCTION-STATUS = 'HIT '
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'SANCHIT '          TO CA-REASON-CODE
              MOVE CON-CONTACT-ID      TO WS-ID-DISP
              STRING 'Payee ' WS-ID-DISP
                     ' matches a sanctions list - payment blocked'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO WS-NAME-WORK
           IF CON-COMPANY-NAME-IND >= 0 AND CON-COMPANY-NAME-LEN > 0
              MOVE CON-COMPANY-NAME-TEXT(1:CON-COMPANY-NAME-LEN)
                                       TO WS-NAME-WORK
           ELSE
              IF CON-FIRST-NAME-IND < 0
                 MOVE 0                TO CON-FIRST-NAME-LEN
              END-IF
              IF CON-LAST-NAME-IND < 0
                 MOVE 0                TO CON-LAST-NAME-LEN
              END-IF
              IF CON-FIRST-NAME-LEN > 0
                 STRING CON-FIRST-NAME-TEXT(1:CON-FIRST-NAME-LEN) ' '
                        DELIMITED BY SIZE INTO WS-NAME-WORK
              END-IF
              IF CON-LAST-NAME-LEN > 0
                 MOVE 0                TO WS-TRAIL
                 INSPECT FUNCTION REVERSE(WS-NAME-WORK)
                         TALLYING WS-TRAIL FOR LEADING SPACES
                 COMPUTE WS-TRAIL = LENGTH OF WS-NAME-WORK - WS-TRAIL
                 IF WS-TRAIL > 0
                    ADD 2              TO WS-TRAIL
                 ELSE
                    MOVE 1             TO WS-TRAIL
                 END-IF
                 STRING CON-LAST-NAME-TEXT(1:CON-LAST-NAME-LEN)
                        DELIMITED BY SIZE INTO WS-NAME-WORK
                        WITH POINTER WS-TRAIL
              END-IF
           END-IF
           IF WS-NAME-WORK = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'PAYEENAM'          TO CA-REASON-CODE
              MOVE 'Payee has no name to print on the payment'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF CON-ADDRESS-LINE1-IND < 0
              MOVE 0                   TO CON-ADDRESS-LINE1-LEN
              MOVE SPACES              TO CON-ADDRESS-LINE1-TEXT
           END-IF.

      ******************************************************************
      * BR-CLM-FIN-011 Deductible once per exposure on claim cost
       3400-APPLY-DEDUCTIBLE.
           COMPUTE WS-DED-REMAINING =
                   PCV-DEDUCTIBLE-AMOUNT - EXP-DEDUCTIBLE-APPLIED
           IF WS-DED-REMAINING < 0
              MOVE 0                   TO WS-DED-REMAINING
           END-IF
           IF WS-DED-REMAINING > 0
              IF CA-FT-AMOUNT <= WS-DED-REMAINING
                 SET CA-RC-VALIDATION  TO TRUE
                 MOVE 'DEDABSRB'       TO CA-REASON-CODE
                 MOVE WS-DED-REMAINING TO WS-AMT-DISP
                 STRING 'Payment is absorbed by the deductible of '
                        WS-AMT-DISP DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE WS-DED-REMAINING    TO WS-DED-TAKEN
              COMPUTE WS-PAY-NET = CA-FT-AMOUNT - WS-DED-REMAINING
           END-IF.

      ******************************************************************
      * BR-CLM-FIN-017 Recovery category and exposure state
       3600-VALIDATE-RECOVERY.
           IF CLM-CLAIM-STATE NOT = 'OPEN'
              SET CA-RC-STATE          TO TRUE
              MOVE 'CLMSTATE'          TO CA-REASON-CODE
              STRING 'Claim ' CLM-CLAIM-NUMBER ' is '
                     CLM-CLAIM-STATE ' - reopen it to record recovery'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF EXP-EXPOSURE-STATE NOT = 'OPEN'
              AND EXP-EXPOSURE-STATE NOT = 'CLSD'
              SET CA-RC-STATE          TO TRUE
              MOVE 'EXPSTATE'          TO CA-REASON-CODE
              STRING 'Exposure is ' EXP-EXPOSURE-STATE
                     ' - recoveries need OPEN or CLSD'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           PERFORM 3100-VALIDATE-COST
           MOVE CA-FT-RECOVERY-CATEGORY TO WS-RCV-CATEGORY
           IF NOT WS-RCV-CATEGORY-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RCVCAT  '          TO CA-REASON-CODE
              STRING 'Recovery category ' CA-FT-RECOVERY-CATEGORY
                     ' must be SUBRO SALVAGE DEDUCT or OVERPAY'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       3700-LOAD-PENDING-TXN.
           PERFORM 3710-LOAD-TXN
           IF TXN-TXN-STATUS NOT = 'PAPR'
              SET CA-RC-STATE          TO TRUE
              MOVE 'TXNSTATE'          TO CA-REASON-CODE
              STRING 'Transaction is ' TXN-TXN-STATUS
                     ', not pending approval'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF TXN-TXN-TYPE NOT = 'RSV ' AND TXN-TXN-TYPE NOT = 'PAY '
              SET CA-RC-STATE          TO TRUE
              MOVE 'TXNTYPE '          TO CA-REASON-CODE
              STRING TXN-TXN-TYPE ' transactions follow their payment'
                     ' and are not approved on their own'
                     DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE 'PAPR'                 TO WS-OLD-STATUS.

      ******************************************************************
       3710-LOAD-TXN.
           IF CA-FT-TXN-ID <= 0
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'TXNIDREQ'          TO CA-REASON-CODE
              MOVE 'Transaction id is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-FT-TXN-ID           TO TXN-TXN-ID
           PERFORM 4410-READ-TXN.

      ******************************************************************
       3800-REQUIRE-REASON.
           IF CA-FT-REASON = SPACES
              SET CA-RC-VALIDATION     TO TRUE
              MOVE 'RSNREQD '          TO CA-REASON-CODE
              MOVE 'A reason code is required'
                                       TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE CA-FT-REASON           TO WS-COD-VALUE
           PERFORM 8300-VALIDATE-CODE.

      ******************************************************************
       4000-READ-CLAIM.
           MOVE CA-FT-CLAIM-ID         TO CLM-CLAIM-ID
           EXEC SQL
              SELECT CLAIM_NUMBER, CLAIM_STATE, TOTAL_RESERVES,
                     TOTAL_PAID, TOTAL_RECOVERED, TOTAL_INCURRED
                INTO :CLM-CLAIM-NUMBER, :CLM-CLAIM-STATE,
                     :CLM-TOTAL-RESERVES, :CLM-TOTAL-PAID,
                     :CLM-TOTAL-RECOVERED, :CLM-TOTAL-INCURRED
                FROM CLMDB.CLM_CLAIM
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CLMNOTFD'       TO CA-REASON-CODE
                 MOVE 'Claim not found for the exposure'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4000-READ-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4010-READ-EXPOSURE.
           MOVE CA-FT-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           EXEC SQL
              SELECT CLAIM_ID, COVERAGE_TYPE, COV_SEQ,
                     EXPOSURE_STATE, TOTAL_PAID, DEDUCTIBLE_APPLIED
                INTO :EXP-CLAIM-ID, :EXP-COVERAGE-TYPE,
                     :EXP-COV-SEQ, :EXP-EXPOSURE-STATE,
                     :EXP-TOTAL-PAID, :EXP-DEDUCTIBLE-APPLIED
                FROM CLMDB.CLM_EXPOSURE
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'EXPNOTFD'       TO CA-REASON-CODE
                 MOVE CA-FT-EXPOSURE-ID TO WS-ID-DISP
                 STRING 'Exposure ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4010-READ-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4020-READ-COVERAGE.
           MOVE EXP-CLAIM-ID           TO PCV-CLAIM-ID
           MOVE EXP-COV-SEQ            TO PCV-COV-SEQ
           EXEC SQL
              SELECT COVERAGE_TYPE, LIMIT_AMOUNT, DEDUCTIBLE_AMOUNT
                INTO :PCV-COVERAGE-TYPE, :PCV-LIMIT-AMOUNT,
                     :PCV-DEDUCTIBLE-AMOUNT
                FROM CLMDB.CLM_POLICY_COVERAGE
               WHERE CLAIM_ID = :PCV-CLAIM-ID
                 AND COV_SEQ  = :PCV-COV-SEQ
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'COVNOTFD'       TO CA-REASON-CODE
                 MOVE 'Exposure coverage missing from policy snapshot'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4020-READ-COVERAGE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4030-READ-CONTROL.
           MOVE 'SYSTEM  '             TO CTL-CONTROL-KEY
           EXEC SQL
              SELECT BUSINESS_DATE
                INTO :CTL-BUSINESS-DATE
                FROM CLMDB.CLM_CONTROL
               WHERE CONTROL_KEY = :CTL-CONTROL-KEY
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4030-READ-CONTROL' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           MOVE CTL-BUSINESS-DATE      TO CA-BUS-DATE.

      ******************************************************************
      * Reserve line for (exposure, cost type, category); created at
      * zero when the category is used for the first time. The read
      * keeps update locks so the line is serialised for this UOW.
       4100-GET-RESERVE-LINE.
           MOVE CA-FT-EXPOSURE-ID      TO RSL-EXPOSURE-ID
           MOVE WS-COST-TYPE           TO RSL-COST-TYPE
           MOVE CA-FT-COST-CATEGORY    TO RSL-COST-CATEGORY
           EXEC SQL
              SELECT RL_ID, CLAIM_ID, OPEN_RESERVE, PENDING_APPROVAL,
                     TOTAL_PAID, RECOVERY_RESERVE, TOTAL_RECOVERED
                INTO :RSL-RL-ID, :RSL-CLAIM-ID, :RSL-OPEN-RESERVE,
                     :RSL-PENDING-APPROVAL, :RSL-TOTAL-PAID,
                     :RSL-RECOVERY-RESERVE, :RSL-TOTAL-RECOVERED
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE EXPOSURE_ID   = :RSL-EXPOSURE-ID
                 AND COST_TYPE     = :RSL-COST-TYPE
                 AND COST_CATEGORY = :RSL-COST-CATEGORY
                WITH RS USE AND KEEP UPDATE LOCKS
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 PERFORM 4110-CREATE-RESERVE-LINE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4100-GET-RESERVE-LINE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           MOVE RSL-RL-ID              TO CA-FT-RL-ID.

      ******************************************************************
       4110-CREATE-RESERVE-LINE.
           MOVE 'RSL'                  TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO RSL-RL-ID
           MOVE EXP-CLAIM-ID           TO RSL-CLAIM-ID
           MOVE 0                      TO RSL-OPEN-RESERVE
                                          RSL-PENDING-APPROVAL
                                          RSL-TOTAL-PAID
                                          RSL-RECOVERY-RESERVE
                                          RSL-TOTAL-RECOVERED
           EXEC SQL
              INSERT INTO CLMDB.CLM_RESERVE_LINE
                    (RL_ID, CLAIM_ID, EXPOSURE_ID, COST_TYPE,
                     COST_CATEGORY, OPEN_RESERVE, PENDING_APPROVAL,
                     TOTAL_PAID, RECOVERY_RESERVE, TOTAL_RECOVERED,
                     UPDATE_TS)
              VALUES (:RSL-RL-ID, :RSL-CLAIM-ID, :RSL-EXPOSURE-ID,
                     :RSL-COST-TYPE, :RSL-COST-CATEGORY,
                     0, 0, 0, 0, 0, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'RSLDUP  '       TO CA-REASON-CODE
                 MOVE 'Reserve line created concurrently - retry'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4110-CREATE-RESERVE-LINE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4120-READ-RL-BY-ID.
           EXEC SQL
              SELECT CLAIM_ID, EXPOSURE_ID, COST_TYPE, COST_CATEGORY,
                     OPEN_RESERVE, PENDING_APPROVAL, TOTAL_PAID,
                     RECOVERY_RESERVE, TOTAL_RECOVERED
                INTO :RSL-CLAIM-ID, :RSL-EXPOSURE-ID, :RSL-COST-TYPE,
                     :RSL-COST-CATEGORY, :RSL-OPEN-RESERVE,
                     :RSL-PENDING-APPROVAL, :RSL-TOTAL-PAID,
                     :RSL-RECOVERY-RESERVE, :RSL-TOTAL-RECOVERED
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE RL_ID = :RSL-RL-ID
                WITH RS USE AND KEEP UPDATE LOCKS
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'RSLNOTFD'       TO CA-REASON-CODE
                 MOVE 'Reserve line of the transaction not found'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4120-READ-RL-BY-ID' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4130-SUM-EXPOSURE-RESERVE.
           MOVE CA-FT-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           EXEC SQL
              SELECT COALESCE(SUM(OPEN_RESERVE), 0)
                INTO :WS-EXP-RESERVE
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4130-SUM-EXPOSURE-RESERVE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
      * Relative update so concurrent postings never lose an amount;
      * the guard keeps every bucket of the line non-negative.
       4200-ADJUST-RESERVE-LINE.
           EXEC SQL
              UPDATE CLMDB.CLM_RESERVE_LINE
                 SET OPEN_RESERVE     = OPEN_RESERVE + :WS-ADJ-OPEN,
                     PENDING_APPROVAL = PENDING_APPROVAL
                                      + :WS-ADJ-PENDING,
                     TOTAL_PAID       = TOTAL_PAID + :WS-ADJ-PAID,
                     RECOVERY_RESERVE = RECOVERY_RESERVE + :WS-ADJ-RR,
                     TOTAL_RECOVERED  = TOTAL_RECOVERED
                                      + :WS-ADJ-RECOVERED,
                     UPDATE_TS        = CURRENT TIMESTAMP
               WHERE RL_ID = :RSL-RL-ID
                 AND OPEN_RESERVE     + :WS-ADJ-OPEN      >= 0
                 AND PENDING_APPROVAL + :WS-ADJ-PENDING   >= 0
                 AND TOTAL_PAID       + :WS-ADJ-PAID      >= 0
                 AND RECOVERY_RESERVE + :WS-ADJ-RR        >= 0
                 AND TOTAL_RECOVERED  + :WS-ADJ-RECOVERED >= 0
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'RSLNEG  '    TO CA-REASON-CODE
                    MOVE 'Reserve line would go negative - refresh'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN 100
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'RSLNEG  '       TO CA-REASON-CODE
                 MOVE 'Reserve line would go negative - refresh'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4200-ADJUST-RESERVE-LINE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE
           ADD WS-ADJ-OPEN             TO RSL-OPEN-RESERVE
           ADD WS-ADJ-PENDING          TO RSL-PENDING-APPROVAL
           ADD WS-ADJ-PAID             TO RSL-TOTAL-PAID
           ADD WS-ADJ-RR               TO RSL-RECOVERY-RESERVE
           ADD WS-ADJ-RECOVERED        TO RSL-TOTAL-RECOVERED.

      ******************************************************************
      * Paid and pending claim-cost payments across every exposure on
      * the same coverage of the claim; WS-EXCLUDE-PENDING removes the
      * payment being approved from its own pending total.
       4300-SUM-COVERAGE-PAID.
           MOVE EXP-CLAIM-ID           TO PCV-CLAIM-ID
           MOVE EXP-COV-SEQ            TO PCV-COV-SEQ
           EXEC SQL
              SELECT COALESCE(SUM(R.TOTAL_PAID), 0)
                INTO :WS-COV-PAID
                FROM CLMDB.CLM_RESERVE_LINE R,
                     CLMDB.CLM_EXPOSURE E
               WHERE E.CLAIM_ID    = :PCV-CLAIM-ID
                 AND E.COV_SEQ     = :PCV-COV-SEQ
                 AND R.EXPOSURE_ID = E.EXPOSURE_ID
                 AND R.COST_TYPE   = 'CLMC'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-SUM-COVERAGE-PAID' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              SELECT COALESCE(SUM(T.TXN_AMOUNT), 0)
                INTO :WS-COV-PENDING
                FROM CLMDB.CLM_TRANSACTION T,
                     CLMDB.CLM_EXPOSURE E,
                     CLMDB.CLM_RESERVE_LINE R
               WHERE E.CLAIM_ID    = :PCV-CLAIM-ID
                 AND E.COV_SEQ     = :PCV-COV-SEQ
                 AND T.EXPOSURE_ID = E.EXPOSURE_ID
                 AND T.RL_ID       = R.RL_ID
                 AND R.COST_TYPE   = 'CLMC'
                 AND T.TXN_TYPE    = 'PAY '
                 AND T.TXN_STATUS  = 'PAPR'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4300-SUM-COVERAGE-PAID' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           COMPUTE WS-COV-REMAINING = PCV-LIMIT-AMOUNT - WS-COV-PAID
                   - (WS-COV-PENDING - WS-EXCLUDE-PENDING)
           IF WS-COV-REMAINING < 0
              MOVE 0                   TO WS-COV-REMAINING
           END-IF.

      ******************************************************************
       4310-COUNT-FINAL-PAYMENTS.
           MOVE CA-FT-EXPOSURE-ID      TO TXN-EXPOSURE-ID
           EXEC SQL
              SELECT COUNT(*)
                INTO :WS-FINAL-COUNT
                FROM CLMDB.CLM_TRANSACTION
               WHERE EXPOSURE_ID  = :TXN-EXPOSURE-ID
                 AND TXN_TYPE     = 'PAY '
                 AND PAYMENT_TYPE = 'FINL'
                 AND TXN_STATUS   = 'SUBM'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4310-COUNT-FINAL-PAYMENTS' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4320-SUM-DEDUCT-RECOVERED.
           MOVE CA-FT-EXPOSURE-ID      TO TXN-EXPOSURE-ID
           EXEC SQL
              SELECT COALESCE(SUM(TXN_AMOUNT), 0)
                INTO :WS-DED-RECOVERED
                FROM CLMDB.CLM_TRANSACTION
               WHERE EXPOSURE_ID       = :TXN-EXPOSURE-ID
                 AND TXN_TYPE          = 'RCV '
                 AND RECOVERY_CATEGORY = 'DEDUCT'
                 AND TXN_STATUS        = 'SUBM'
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4320-SUM-DEDUCT-RECOVERED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF.

      ******************************************************************
       4400-INSERT-TXN.
           MOVE 'TXN'                  TO WS-SEQ-NAME
           PERFORM 8000-NEXT-SEQUENCE
           MOVE SQA-SQ-VALUE           TO TXN-TXN-ID
           EXEC SQL
              INSERT INTO CLMDB.CLM_TRANSACTION
                    (TXN_ID, CLAIM_ID, EXPOSURE_ID, RL_ID, TXN_TYPE,
                     TXN_STATUS, TXN_AMOUNT, ERODES_RESERVE,
                     PAYMENT_TYPE, RECOVERY_CATEGORY, CHECK_ID,
                     CREATE_USER, APPROVER_USER, REJECT_REASON,
                     RELATED_TXN_ID, BUSINESS_DATE, CREATE_TS,
                     APPROVED_TS)
              VALUES (:TXN-TXN-ID, :TXN-CLAIM-ID, :TXN-EXPOSURE-ID,
                     :TXN-RL-ID, :TXN-TXN-TYPE, :TXN-TXN-STATUS,
                     :TXN-TXN-AMOUNT, :TXN-ERODES-RESERVE,
                     :TXN-PAYMENT-TYPE, :TXN-RECOVERY-CATEGORY,
                     :TXN-CHECK-ID, :TXN-CREATE-USER,
                     :TXN-APPROVER-USER, ' ', :TXN-RELATED-TXN-ID,
                     :TXN-BUSINESS-DATE, CURRENT TIMESTAMP, NULL)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE TXN-TXN-ID       TO CA-FT-TXN-ID
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'TXNDUPKY'       TO CA-REASON-CODE
                 MOVE 'Transaction id already used - sequence error'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4400-INSERT-TXN' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4410-READ-TXN.
           EXEC SQL
              SELECT TXN_ID, CLAIM_ID, EXPOSURE_ID, RL_ID, TXN_TYPE,
                     TXN_STATUS, TXN_AMOUNT, ERODES_RESERVE,
                     PAYMENT_TYPE, RECOVERY_CATEGORY, CHECK_ID,
                     CREATE_USER, APPROVER_USER, REJECT_REASON,
                     RELATED_TXN_ID, BUSINESS_DATE
                INTO :TXN-TXN-ID, :TXN-CLAIM-ID, :TXN-EXPOSURE-ID,
                     :TXN-RL-ID, :TXN-TXN-TYPE, :TXN-TXN-STATUS,
                     :TXN-TXN-AMOUNT, :TXN-ERODES-RESERVE,
                     :TXN-PAYMENT-TYPE, :TXN-RECOVERY-CATEGORY,
                     :TXN-CHECK-ID, :TXN-CREATE-USER,
                     :TXN-APPROVER-USER, :TXN-REJECT-REASON,
                     :TXN-RELATED-TXN-ID, :TXN-BUSINESS-DATE
                FROM CLMDB.CLM_TRANSACTION
               WHERE TXN_ID = :TXN-TXN-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'TXNNOTFD'       TO CA-REASON-CODE
                 MOVE TXN-TXN-ID       TO WS-ID-DISP
                 STRING 'Transaction ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4410-READ-TXN'  TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Re-read the primary transaction of this request (the payment,
      * or the reserve change) after linked rows were written.
       4415-REREAD-PRIMARY-TXN.
           MOVE WS-PAY-TXN-ID          TO TXN-TXN-ID
           IF WS-PAY-TXN-ID = 0
              MOVE CA-FT-TXN-ID        TO TXN-TXN-ID
           END-IF
           PERFORM 4410-READ-TXN.

      ******************************************************************
       4420-UPDATE-TXN-DECISION.
           MOVE 'PAPR'                 TO WS-OLD-STATUS
           PERFORM 4425-UPDATE-TXN-STATUS.

      ******************************************************************
      * Status change guarded by the expected old status so two people
      * deciding the same transaction cannot both succeed.
       4425-UPDATE-TXN-STATUS.
           MOVE CA-USER-ID             TO TXN-APPROVER-USER
           EXEC SQL
              UPDATE CLMDB.CLM_TRANSACTION
                 SET TXN_STATUS    = :WS-DECISION-STATUS,
                     APPROVER_USER = :TXN-APPROVER-USER,
                     REJECT_REASON = :WS-DECISION-REASON,
                     APPROVED_TS   = CURRENT TIMESTAMP
               WHERE TXN_ID     = :TXN-TXN-ID
                 AND TXN_STATUS = :WS-OLD-STATUS
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    PERFORM 4429-TXN-ALREADY-DECIDED
                 END-IF
              WHEN 100
                 PERFORM 4429-TXN-ALREADY-DECIDED
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4425-UPDATE-TXN-STATUS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4429-TXN-ALREADY-DECIDED.
           SET CA-RC-CONFLICT          TO TRUE
           MOVE 'STALEVER'             TO CA-REASON-CODE
           MOVE 'Transaction was decided by another user - refresh'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
      * The deductible row withheld from a payment follows the payment:
      * approved, rejected or voided with it. Returns its amount.
       4430-DECIDE-LINKED-DED.
           MOVE 0                      TO WS-DED-AMOUNT
           MOVE WS-PAY-TXN-ID          TO TXN-RELATED-TXN-ID
           EXEC SQL
              SELECT COALESCE(SUM(TXN_AMOUNT), 0)
                INTO :WS-DED-AMOUNT
                FROM CLMDB.CLM_TRANSACTION
               WHERE RELATED_TXN_ID = :TXN-RELATED-TXN-ID
                 AND TXN_TYPE       = 'DED '
                 AND TXN_STATUS     IN ('SUBM', 'PAPR')
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4430-DECIDE-LINKED-DED' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           IF WS-DED-AMOUNT > 0
              MOVE CA-USER-ID          TO TXN-APPROVER-USER
              EXEC SQL
                 UPDATE CLMDB.CLM_TRANSACTION
                    SET TXN_STATUS    = :WS-DECISION-STATUS,
                        APPROVER_USER = :TXN-APPROVER-USER,
                        REJECT_REASON = :WS-DECISION-REASON,
                        APPROVED_TS   = CURRENT TIMESTAMP
                  WHERE RELATED_TXN_ID = :TXN-RELATED-TXN-ID
                    AND TXN_TYPE       = 'DED '
                    AND TXN_STATUS     IN ('SUBM', 'PAPR')
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE '4430-DECIDE-LINKED-DED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
              END-IF
           END-IF.

      ******************************************************************
      * Reserve released by a final payment = the negative RSV rows
      * posted against it (returned as a positive amount)
       4440-SUM-FINAL-ZEROING.
           MOVE WS-PAY-TXN-ID          TO TXN-RELATED-TXN-ID
           EXEC SQL
              SELECT COALESCE(SUM(TXN_AMOUNT), 0)
                INTO :WS-FINAL-ZEROED
                FROM CLMDB.CLM_TRANSACTION
               WHERE RELATED_TXN_ID = :TXN-RELATED-TXN-ID
                 AND TXN_TYPE       = 'RSV '
                 AND TXN_STATUS     = 'SUBM'
                 AND TXN_AMOUNT     < 0
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '4440-SUM-FINAL-ZEROING' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           COMPUTE WS-FINAL-ZEROED = 0 - WS-FINAL-ZEROED.

      ******************************************************************
       4500-READ-CONTACT.
           MOVE CA-FT-PAYEE-CONTACT-ID TO CON-CONTACT-ID
           EXEC SQL
              SELECT CONTACT_ID, FIRST_NAME, LAST_NAME, COMPANY_NAME,
                     ADDRESS_LINE1, POSTCODE, SANCTION_STATUS
                INTO :CON-CONTACT-ID,
                     :CON-FIRST-NAME :CON-FIRST-NAME-IND,
                     :CON-LAST-NAME :CON-LAST-NAME-IND,
                     :CON-COMPANY-NAME :CON-COMPANY-NAME-IND,
                     :CON-ADDRESS-LINE1 :CON-ADDRESS-LINE1-IND,
                     :CON-POSTCODE :CON-POSTCODE-IND,
                     :CON-SANCTION-STATUS
                FROM CLMDB.CLM_CONTACT
               WHERE CONTACT_ID = :CON-CONTACT-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CONNOTFD'       TO CA-REASON-CODE
                 MOVE CA-FT-PAYEE-CONTACT-ID TO WS-ID-DISP
                 STRING 'Payee contact ' WS-ID-DISP ' not found'
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4500-READ-CONTACT' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4510-INSERT-CHECK.
           EXEC SQL
              INSERT INTO CLMDB.CLM_CHECK
                    (CHECK_ID, CLAIM_ID, CHECK_NUMBER,
                     PAYEE_CONTACT_ID, PAYEE_NAME, PAYEE_ADDR1,
                     PAYEE_POSTCODE, CHECK_AMOUNT, PAY_METHOD,
                     SCHEDULED_DATE, ISSUE_DATE, CHECK_STATUS,
                     BILLING_DISB_REF, VOID_REASON, CREATE_TS,
                     UPDATE_TS)
              VALUES (:CHK-CHECK-ID, :CHK-CLAIM-ID,
                     :CHK-CHECK-NUMBER, :CHK-PAYEE-CONTACT-ID,
                     :CHK-PAYEE-NAME,
                     :CHK-PAYEE-ADDR1 :CHK-PAYEE-ADDR1-IND,
                     :CHK-PAYEE-POSTCODE :CHK-PAYEE-POSTCODE-IND,
                     :CHK-CHECK-AMOUNT, :CHK-PAY-METHOD,
                     :CHK-SCHEDULED-DATE, NULL, :CHK-CHECK-STATUS,
                     ' ', ' ', CURRENT TIMESTAMP, CURRENT TIMESTAMP)
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -803
                 SET CA-RC-CONFLICT    TO TRUE
                 MOVE 'CHKDUPKY'       TO CA-REASON-CODE
                 MOVE 'Check id already used - sequence error'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4510-INSERT-CHECK' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4520-READ-CHECK.
           EXEC SQL
              SELECT CHECK_NUMBER, PAYEE_CONTACT_ID, CHECK_AMOUNT,
                     CHECK_STATUS, SCHEDULED_DATE
                INTO :CHK-CHECK-NUMBER, :CHK-PAYEE-CONTACT-ID,
                     :CHK-CHECK-AMOUNT, :CHK-CHECK-STATUS,
                     :CHK-SCHEDULED-DATE
                FROM CLMDB.CLM_CHECK
               WHERE CHECK_ID = :CHK-CHECK-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN 100
                 SET CA-RC-NOT-FOUND   TO TRUE
                 MOVE 'CHKNOTFD'       TO CA-REASON-CODE
                 MOVE 'Check for the payment not found'
                                       TO CA-MESSAGE
                 PERFORM 9000-RETURN
              WHEN OTHER
                 MOVE '4520-READ-CHECK' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
      * Checks never leave ISSU / CLRD from here: those are owned by
      * the issue run and the bank reconciliation.
       4530-UPDATE-CHECK-STATUS.
           EXEC SQL
              UPDATE CLMDB.CLM_CHECK
                 SET CHECK_STATUS = :WS-CHECK-STATUS,
                     VOID_REASON  = :CHK-VOID-REASON,
                     UPDATE_TS    = CURRENT TIMESTAMP
               WHERE CHECK_ID     = :CHK-CHECK-ID
                 AND CHECK_STATUS IN ('HOLD', 'PEND')
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    PERFORM 4539-CHECK-MOVED-ON
                 END-IF
              WHEN 100
                 PERFORM 4539-CHECK-MOVED-ON
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4530-UPDATE-CHECK-STATUS' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4539-CHECK-MOVED-ON.
           SET CA-RC-CONFLICT          TO TRUE
           MOVE 'CHKSTATE'             TO CA-REASON-CODE
           MOVE 'Check was issued or voided meanwhile - refresh'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       4600-ADJUST-DEDUCTIBLE-APPLIED.
           MOVE CA-FT-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET DEDUCTIBLE_APPLIED = DEDUCTIBLE_APPLIED
                                        + :WS-DED-AMOUNT,
                     UPDATE_TS          = CURRENT TIMESTAMP
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
                 AND DEDUCTIBLE_APPLIED + :WS-DED-AMOUNT >= 0
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 IF SQLERRD(3) = 0
                    SET CA-RC-CONFLICT TO TRUE
                    MOVE 'DEDNEG  '    TO CA-REASON-CODE
                    MOVE 'Deductible applied would go negative'
                                       TO CA-MESSAGE
                    PERFORM 9000-RETURN
                 END-IF
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '4600-ADJUST-DEDUCTIBLE-APPLIED' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       4700-FIND-APPROVAL-ACTIVITY.
           MOVE TXN-TXN-ID             TO ACT-APPROVAL-TXN-ID
           EXEC SQL
              SELECT ACTIVITY_ID, ASSIGNED_USER
                INTO :ACT-ACTIVITY-ID, :ACT-ASSIGNED-USER
                FROM CLMDB.CLM_ACTIVITY
               WHERE APPROVAL_TXN_ID = :ACT-APPROVAL-TXN-ID
                 AND ACTIVITY_STATUS = 'OPEN'
               FETCH FIRST 1 ROW ONLY
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 MOVE '4700-FIND-APPROVAL-ACTIVITY' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       5000-PREPARE-TXN.
           MOVE 0                      TO TXN-TXN-ID
                                          TXN-CHECK-ID
                                          TXN-RELATED-TXN-ID
           MOVE EXP-CLAIM-ID           TO TXN-CLAIM-ID
           MOVE CA-FT-EXPOSURE-ID      TO TXN-EXPOSURE-ID
           MOVE RSL-RL-ID              TO TXN-RL-ID
           MOVE SPACES                 TO TXN-PAYMENT-TYPE
                                          TXN-RECOVERY-CATEGORY
                                          TXN-REJECT-REASON
           MOVE CA-USER-ID             TO TXN-CREATE-USER
           MOVE CA-BUS-DATE            TO TXN-BUSINESS-DATE
           IF WS-APPROVAL-NEEDED
              MOVE 'PAPR'              TO TXN-TXN-STATUS
              MOVE WS-APPROVER         TO TXN-APPROVER-USER
           ELSE
              MOVE 'SUBM'              TO TXN-TXN-STATUS
              MOVE SPACES              TO TXN-APPROVER-USER
           END-IF.

      ******************************************************************
      * Post a payment (immediately or at approval) against its line:
      * BR-CLM-FIN-014 raise reserve first when the payment is larger
      * BR-CLM-FIN-015 a final payment releases what remains
       5300-POST-PAYMENT.
           MOVE 0                      TO WS-ADJ-PENDING WS-ADJ-RR
                                          WS-ADJ-RECOVERED
           IF TXN-ERODES-RESERVE = 'Y'
              IF WS-PAY-NET > RSL-OPEN-RESERVE
                 PERFORM 5310-AUTO-INCREASE-RESERVE
              END-IF
              COMPUTE WS-ADJ-OPEN = 0 - WS-PAY-NET
           ELSE
              MOVE 0                   TO WS-ADJ-OPEN
           END-IF
           MOVE WS-PAY-NET             TO WS-ADJ-PAID
           PERFORM 4200-ADJUST-RESERVE-LINE
           IF WS-PAY-FINAL AND RSL-OPEN-RESERVE > 0
              PERFORM 5320-ZERO-FINAL-RESERVE
           END-IF.

      ******************************************************************
       5310-AUTO-INCREASE-RESERVE.
           COMPUTE WS-DELTA = WS-PAY-NET - RSL-OPEN-RESERVE
           PERFORM 5330-INSERT-LINKED-RSV
           MOVE WS-DELTA               TO WS-ADJ-OPEN
           MOVE 0                      TO WS-ADJ-PAID
           PERFORM 4200-ADJUST-RESERVE-LINE
           MOVE WS-DELTA               TO WS-AMT-DISP
           MOVE WS-PAY-TXN-ID          TO WS-ID-DISP
           MOVE 'RSVAUTO '             TO WS-HIS-EVENT
           STRING 'Reserve raised ' WS-AMT-DISP
                  ' to cover payment ' WS-ID-DISP
                  DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY.

      ******************************************************************
       5320-ZERO-FINAL-RESERVE.
           COMPUTE WS-DELTA = 0 - RSL-OPEN-RESERVE
           PERFORM 5330-INSERT-LINKED-RSV
           MOVE WS-DELTA               TO WS-ADJ-OPEN
           MOVE 0                      TO WS-ADJ-PAID
           PERFORM 4200-ADJUST-RESERVE-LINE
           MOVE WS-DELTA               TO WS-AMT-DISP
           MOVE WS-PAY-TXN-ID          TO WS-ID-DISP
           MOVE 'RSVFINL '             TO WS-HIS-EVENT
           STRING 'Final payment ' WS-ID-DISP ' released reserve '
                  WS-AMT-DISP DELIMITED BY SIZE INTO WS-HIS-DESC
           PERFORM 8100-WRITE-HISTORY.

      ******************************************************************
       5330-INSERT-LINKED-RSV.
           MOVE 0                      TO TXN-TXN-ID TXN-CHECK-ID
           MOVE WS-PAY-TXN-ID          TO TXN-RELATED-TXN-ID
           MOVE 'RSV '                 TO TXN-TXN-TYPE
           MOVE 'SUBM'                 TO TXN-TXN-STATUS
           MOVE WS-DELTA               TO TXN-TXN-AMOUNT
           MOVE 'N'                    TO TXN-ERODES-RESERVE
           MOVE SPACES                 TO TXN-PAYMENT-TYPE
                                          TXN-RECOVERY-CATEGORY
           MOVE CA-USER-ID             TO TXN-CREATE-USER
           MOVE SPACES                 TO TXN-APPROVER-USER
           MOVE CA-BUS-DATE            TO TXN-BUSINESS-DATE
           PERFORM 4400-INSERT-TXN.

      ******************************************************************
      * BR-CLM-FIN-007 Route to the first approver who covers every
      * failed limit; nobody in reach means the request is refused.
       5500-EVALUATE-AUTHORITY.
           MOVE CA-USER-ID             TO WS-AUTH-USER
           PERFORM VARYING WS-AX FROM 1 BY 1
                   UNTIL WS-AX > WS-AUTH-COUNT
              PERFORM 8700-CALL-AUTHZ
              IF WS-AUTH-OK(WS-AX) NOT = 'Y'
                 SET WS-APPROVAL-NEEDED TO TRUE
                 IF WS-APPROVER = SPACES
                    MOVE WS-AUTH-APPROVER(WS-AX) TO WS-APPROVER
                 END-IF
              END-IF
           END-PERFORM
           IF WS-APPROVAL-NEEDED
              IF WS-APPROVER = SPACES
                 PERFORM 5590-NO-APPROVER
              END-IF
              PERFORM 5510-CONFIRM-APPROVER
           END-IF.

      ******************************************************************
       5510-CONFIRM-APPROVER.
           MOVE 0                      TO WS-ROUNDS
           MOVE 'N'                    TO WS-CONFIRMED
           PERFORM UNTIL WS-APPROVER-CONFIRMED
                      OR WS-ROUNDS >= WS-MAX-APPROVER-ROUNDS
              ADD 1                    TO WS-ROUNDS
              SET WS-APPROVER-CONFIRMED TO TRUE
              MOVE WS-APPROVER         TO WS-AUTH-USER
              PERFORM VARYING WS-AX FROM 1 BY 1
                      UNTIL WS-AX > WS-AUTH-COUNT
                         OR NOT WS-APPROVER-CONFIRMED
                 PERFORM 8700-CALL-AUTHZ
                 IF WS-AUTH-OK(WS-AX) NOT = 'Y'
                    MOVE 'N'           TO WS-CONFIRMED
                    MOVE WS-AUTH-APPROVER(WS-AX) TO WS-APPROVER
                    IF WS-APPROVER = SPACES
                       PERFORM 5590-NO-APPROVER
                    END-IF
                 END-IF
              END-PERFORM
           END-PERFORM
           IF NOT WS-APPROVER-CONFIRMED
              PERFORM 5590-NO-APPROVER
           END-IF.

      ******************************************************************
      * BR-CLM-FIN-020 The approver needs the authority at approval
       5520-CHECK-APPROVER-AUTHORITY.
           MOVE CA-USER-ID             TO WS-AUTH-USER
           PERFORM VARYING WS-AX FROM 1 BY 1
                   UNTIL WS-AX > WS-AUTH-COUNT
              PERFORM 8700-CALL-AUTHZ
              IF WS-AUTH-OK(WS-AX) NOT = 'Y'
                 SET CA-RC-AUTHORITY   TO TRUE
                 MOVE 'NOAUTH  '       TO CA-REASON-CODE
                 MOVE WS-AUTH-LIMIT(WS-AX) TO WS-AMT-DISP
                 MOVE WS-AUTH-AMOUNT(WS-AX) TO WS-AMT2-DISP
                 STRING WS-AUTH-TYPE(WS-AX) ' ' WS-AMT2-DISP
                        ' exceeds your limit ' WS-AMT-DISP
                        DELIMITED BY SIZE INTO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
           END-PERFORM.

      ******************************************************************
       5590-NO-APPROVER.
           SET CA-RC-AUTHORITY         TO TRUE
           MOVE 'NOAPPRVR'             TO CA-REASON-CODE
           MOVE 'Over authority and no supervisor can approve it'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       5800-FILL-RESPONSE.
           MOVE TXN-TXN-ID             TO CA-FT-TXN-ID
           MOVE TXN-CLAIM-ID           TO CA-FT-CLAIM-ID
           MOVE TXN-EXPOSURE-ID        TO CA-FT-EXPOSURE-ID
           MOVE TXN-TXN-TYPE           TO CA-FT-TXN-TYPE
           MOVE TXN-TXN-STATUS         TO CA-FT-TXN-STATUS
           MOVE TXN-TXN-AMOUNT         TO CA-FT-AMOUNT
           MOVE TXN-PAYMENT-TYPE       TO CA-FT-PAYMENT-TYPE
           MOVE TXN-RECOVERY-CATEGORY  TO CA-FT-RECOVERY-CATEGORY
           MOVE TXN-APPROVER-USER      TO CA-FT-APPROVER
           MOVE TXN-CHECK-ID           TO CA-FT-CHECK-ID
           MOVE TXN-REJECT-REASON      TO CA-FT-REASON
           MOVE RSL-COST-TYPE          TO CA-FT-COST-TYPE
           MOVE RSL-COST-CATEGORY      TO CA-FT-COST-CATEGORY
           PERFORM 5810-FILL-RL-RESPONSE.

      ******************************************************************
       5810-FILL-RL-RESPONSE.
           MOVE RSL-RL-ID              TO CA-FT-RL-ID
           MOVE RSL-OPEN-RESERVE       TO CA-FT-RL-OPEN-RESERVE
           MOVE RSL-TOTAL-PAID         TO CA-FT-RL-TOTAL-PAID
           MOVE RSL-RECOVERY-RESERVE   TO CA-FT-RL-RECOVERY-RSV
           MOVE RSL-TOTAL-RECOVERED    TO CA-FT-RL-TOTAL-RECOVERED.

      ******************************************************************
       5850-BUILD-AUDIT-PAYLOAD.
           MOVE TXN-TXN-ID             TO WS-ID-DISP
           MOVE TXN-EXPOSURE-ID        TO WS-ID2-DISP
           MOVE TXN-TXN-AMOUNT         TO WS-AMT-DISP
           MOVE WS-SUM-INCURRED        TO WS-AMT2-DISP
           MOVE TXN-CHECK-ID           TO WS-CHK-DISP
           MOVE SPACES                 TO WS-AUD-PAYLOAD
           STRING 'TXN=' WS-ID-DISP ' EXP=' WS-ID2-DISP
                  ' TYPE=' TXN-TXN-TYPE
                  ' STATUS=' TXN-TXN-STATUS
                  ' AMT=' WS-AMT-DISP
                  ' PTYPE=' TXN-PAYMENT-TYPE
                  ' RCAT=' TXN-RECOVERY-CATEGORY
                  ' CHK=' WS-CHK-DISP
                  ' BY=' CA-USER-ID
                  ' APPR=' TXN-APPROVER-USER
                  ' RSN=' TXN-REJECT-REASON
                  ' CLMINC=' WS-AMT2-DISP
                  DELIMITED BY SIZE INTO WS-AUD-PAYLOAD.

      ******************************************************************
      * BR-CLM-FIN-023 Totals always re-derived from the lines
       5900-RECOMPUTE-TOTALS.
           PERFORM 5910-RECOMPUTE-EXPOSURE
           PERFORM 5920-RECOMPUTE-CLAIM.

      ******************************************************************
       5910-RECOMPUTE-EXPOSURE.
           MOVE CA-FT-EXPOSURE-ID      TO EXP-EXPOSURE-ID
           EXEC SQL
              SELECT COALESCE(SUM(OPEN_RESERVE), 0),
                     COALESCE(SUM(TOTAL_PAID), 0),
                     COALESCE(SUM(TOTAL_RECOVERED), 0)
                INTO :WS-SUM-OPEN, :WS-SUM-PAID, :WS-SUM-RECOVERED
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5910-RECOMPUTE-EXPOSURE' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           EXEC SQL
              UPDATE CLMDB.CLM_EXPOSURE
                 SET OPEN_RESERVE    = :WS-SUM-OPEN,
                     TOTAL_PAID      = :WS-SUM-PAID,
                     TOTAL_RECOVERED = :WS-SUM-RECOVERED,
                     UPDATE_TS       = CURRENT TIMESTAMP,
                     VERSION_NO      = VERSION_NO + 1
               WHERE EXPOSURE_ID = :EXP-EXPOSURE-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '5910-RECOMPUTE-EXPOSURE' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       5920-RECOMPUTE-CLAIM.
           MOVE EXP-CLAIM-ID           TO CLM-CLAIM-ID
           EXEC SQL
              SELECT COALESCE(SUM(OPEN_RESERVE), 0),
                     COALESCE(SUM(TOTAL_PAID), 0),
                     COALESCE(SUM(TOTAL_RECOVERED), 0)
                INTO :WS-SUM-OPEN, :WS-SUM-PAID, :WS-SUM-RECOVERED
                FROM CLMDB.CLM_RESERVE_LINE
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE '5920-RECOMPUTE-CLAIM' TO WS-ERR-PARA
              PERFORM 9900-DB2-ERROR
           END-IF
           COMPUTE WS-SUM-INCURRED =
                   WS-SUM-PAID + WS-SUM-OPEN - WS-SUM-RECOVERED
           EXEC SQL
              UPDATE CLMDB.CLM_CLAIM
                 SET TOTAL_RESERVES  = :WS-SUM-OPEN,
                     TOTAL_PAID      = :WS-SUM-PAID,
                     TOTAL_RECOVERED = :WS-SUM-RECOVERED,
                     TOTAL_INCURRED  = :WS-SUM-INCURRED,
                     UPDATE_TS       = CURRENT TIMESTAMP,
                     VERSION_NO      = VERSION_NO + 1
               WHERE CLAIM_ID = :CLM-CLAIM-ID
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 CONTINUE
              WHEN -911
              WHEN -913
                 PERFORM 9800-DEADLOCK
              WHEN OTHER
                 MOVE '5920-RECOMPUTE-CLAIM' TO WS-ERR-PARA
                 PERFORM 9900-DB2-ERROR
           END-EVALUATE.

      ******************************************************************
       8000-NEXT-SEQUENCE.
           INITIALIZE WS-SEQ-CA
           MOVE 'SEQNEXT '             TO SQA-REQUEST-ID
           MOVE CA-USER-ID             TO SQA-USER-ID
           MOVE CA-BUS-DATE            TO SQA-BUS-DATE
           MOVE CA-TRACE-ID            TO SQA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO SQA-CALLER-PGM
           MOVE WS-SEQ-NAME            TO SQA-SQ-NAME
           MOVE 1                      TO SQA-SQ-COUNT
           CALL WS-SEQ-PGM USING WS-SEQ-CA
           IF SQA-RETURN-CODE >= 8
              MOVE SQA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE SQA-REASON-CODE     TO CA-REASON-CODE
              MOVE SQA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8100-WRITE-HISTORY.
           INITIALIZE WS-HIS-CA
           MOVE 'HISADD  '             TO HIA-REQUEST-ID
           MOVE CA-USER-ID             TO HIA-USER-ID
           MOVE CA-BUS-DATE            TO HIA-BUS-DATE
           MOVE CA-TRACE-ID            TO HIA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO HIA-CALLER-PGM
           MOVE EXP-CLAIM-ID           TO HIA-HI-CLAIM-ID
           MOVE WS-HIS-EVENT           TO HIA-HI-EVENT-TYPE
           MOVE WS-HIS-DESC            TO HIA-HI-DESCRIPTION
           MOVE WS-PROGRAM-ID          TO HIA-HI-PROGRAM
           CALL WS-HIS-PGM USING WS-HIS-CA
           IF HIA-RETURN-CODE >= 12
              MOVE HIA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE HIA-REASON-CODE     TO CA-REASON-CODE
              MOVE HIA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE SPACES                 TO WS-HIS-DESC.

      ******************************************************************
       8200-WRITE-AUDIT.
           INITIALIZE WS-AUD-CA
           MOVE 'AUCADD  '             TO AUA-REQUEST-ID
           MOVE CA-USER-ID             TO AUA-USER-ID
           MOVE CA-BUS-DATE            TO AUA-BUS-DATE
           MOVE CA-TRACE-ID            TO AUA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AUA-CALLER-PGM
           MOVE EXP-CLAIM-ID           TO AUA-AC-CLAIM-ID
           MOVE WS-AUD-EVENT           TO AUA-AC-EVENT-CODE
           MOVE WS-AUD-PAYLOAD         TO AUA-AC-PAYLOAD
           MOVE WS-PROGRAM-ID          TO AUA-AC-PROGRAM
           CALL WS-AUD-PGM USING WS-AUD-CA
           IF AUA-RETURN-CODE >= 12
              MOVE AUA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AUA-REASON-CODE     TO CA-REASON-CODE
              MOVE AUA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8300-VALIDATE-CODE.
           INITIALIZE WS-COD-CA
           MOVE 'CODVAL  '             TO CDA-REQUEST-ID
           MOVE CA-USER-ID             TO CDA-USER-ID
           MOVE CA-BUS-DATE            TO CDA-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CDA-CALLER-PGM
           MOVE WS-COD-TABLE           TO CDA-CD-TABLE
           MOVE WS-COD-VALUE           TO CDA-CD-CODE
           CALL WS-COD-PGM USING WS-COD-CA
           IF CDA-RETURN-CODE >= 12
              MOVE CDA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE CDA-REASON-CODE     TO CA-REASON-CODE
              MOVE CDA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           IF NOT CDA-CD-IS-VALID
              SET CA-RC-VALIDATION     TO TRUE
              MOVE WS-COD-REASON       TO CA-REASON-CODE
              STRING WS-COD-VALUE ' is not a valid ' WS-COD-TABLE
                     ' code' DELIMITED BY SIZE INTO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8400-SCREEN-PAYEE.
           INITIALIZE WS-CON-CA
           MOVE 'CONSAN  '             TO COA-REQUEST-ID
           MOVE CA-USER-ID             TO COA-USER-ID
           MOVE CA-BUS-DATE            TO COA-BUS-DATE
           MOVE CA-TRACE-ID            TO COA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO COA-CALLER-PGM
           MOVE CON-CONTACT-ID         TO COA-CO-CONTACT-ID
           MOVE EXP-CLAIM-ID           TO COA-CO-CLAIM-ID
           CALL WS-CON-PGM USING WS-CON-CA
           IF COA-RETURN-CODE >= 12
              MOVE COA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE COA-REASON-CODE     TO CA-REASON-CODE
              MOVE COA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE COA-CO-SANCTION-STATUS TO CON-SANCTION-STATUS.

      ******************************************************************
       8500-CALL-DATES.
           MOVE CA-USER-ID             TO DTA-USER-ID
           MOVE CA-BUS-DATE            TO DTA-BUS-DATE
           MOVE CA-TRACE-ID            TO DTA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO DTA-CALLER-PGM
           MOVE 0                      TO DTA-RETURN-CODE
           CALL WS-DTE-PGM USING WS-DTE-CA
           IF DTA-RETURN-CODE >= 12
              AND DTA-REQUEST-ID NOT = 'DTVALD  '
              MOVE DTA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE DTA-REASON-CODE     TO CA-REASON-CODE
              MOVE DTA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF.

      ******************************************************************
       8600-RAISE-APPROVAL-ACTIVITY.
           INITIALIZE WS-ACT-CA
           MOVE 'ACTNEW  '             TO AVA-REQUEST-ID
           MOVE CA-USER-ID             TO AVA-USER-ID
           MOVE CA-BUS-DATE            TO AVA-BUS-DATE
           MOVE CA-TRACE-ID            TO AVA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO AVA-CALLER-PGM
           MOVE EXP-CLAIM-ID           TO AVA-AV-CLAIM-ID
           MOVE CA-FT-EXPOSURE-ID      TO AVA-AV-EXPOSURE-ID
           MOVE WS-APPROVAL-PATTERN    TO AVA-AV-PATTERN-CODE
           MOVE WS-APPROVER            TO AVA-AV-ASSIGNED-USER
           MOVE TXN-TXN-ID             TO AVA-AV-APPROVAL-TXN-ID
           MOVE TXN-TXN-AMOUNT         TO WS-AMT-DISP
           STRING 'Approve ' TXN-TXN-TYPE WS-AMT-DISP ' on claim '
                  CLM-CLAIM-NUMBER DELIMITED BY SIZE
                  INTO AVA-AV-SUBJECT
           CALL WS-ACT-PGM USING WS-ACT-CA
           IF AVA-RETURN-CODE >= 12
              MOVE AVA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE AVA-REASON-CODE     TO CA-REASON-CODE
              MOVE AVA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE AVA-AV-ACTIVITY-ID     TO CA-FT-ACTIVITY-ID.

      ******************************************************************
      * The activity is closed on behalf of its assignee: the decision
      * itself was authorised by the authority check above.
       8610-COMPLETE-APPROVAL-ACTIVITY.
           PERFORM 4700-FIND-APPROVAL-ACTIVITY
           IF SQLCODE = 0
              MOVE ACT-ASSIGNED-USER   TO WS-ACTIVITY-USER
              INITIALIZE WS-ACT-CA
              MOVE 'ACTCMP  '          TO AVA-REQUEST-ID
              MOVE WS-ACTIVITY-USER    TO AVA-USER-ID
              MOVE CA-BUS-DATE         TO AVA-BUS-DATE
              MOVE CA-TRACE-ID         TO AVA-TRACE-ID
              MOVE WS-PROGRAM-ID       TO AVA-CALLER-PGM
              MOVE ACT-ACTIVITY-ID     TO AVA-AV-ACTIVITY-ID
              CALL WS-ACT-PGM USING WS-ACT-CA
              IF AVA-RETURN-CODE >= 12
                 MOVE AVA-RETURN-CODE  TO CA-RETURN-CODE
                 MOVE AVA-REASON-CODE  TO CA-REASON-CODE
                 MOVE AVA-MESSAGE      TO CA-MESSAGE
                 PERFORM 9000-RETURN
              END-IF
              MOVE ACT-ACTIVITY-ID     TO CA-FT-ACTIVITY-ID
           END-IF.

      ******************************************************************
       8700-CALL-AUTHZ.
           INITIALIZE WS-AUT-CA
           MOVE 'AUTCHK  '             TO ATA-REQUEST-ID
           MOVE CA-USER-ID             TO ATA-USER-ID
           MOVE CA-BUS-DATE            TO ATA-BUS-DATE
           MOVE CA-TRACE-ID            TO ATA-TRACE-ID
           MOVE WS-PROGRAM-ID          TO ATA-CALLER-PGM
           MOVE WS-AUTH-USER           TO ATA-AT-USER
           MOVE WS-AUTH-TYPE(WS-AX)    TO ATA-AT-LIMIT-TYPE
           MOVE EXP-COVERAGE-TYPE      TO ATA-AT-COVERAGE-TYPE
           MOVE WS-AUTH-AMOUNT(WS-AX)  TO ATA-AT-AMOUNT
           CALL WS-AUT-PGM USING WS-AUT-CA
           IF ATA-RETURN-CODE >= 8
              MOVE ATA-RETURN-CODE     TO CA-RETURN-CODE
              MOVE ATA-REASON-CODE     TO CA-REASON-CODE
              MOVE ATA-MESSAGE         TO CA-MESSAGE
              PERFORM 9000-RETURN
           END-IF
           MOVE ATA-AT-ALLOWED         TO WS-AUTH-OK(WS-AX)
           MOVE ATA-AT-LIMIT           TO WS-AUTH-LIMIT(WS-AX)
           MOVE ATA-AT-APPROVER        TO WS-AUTH-APPROVER(WS-AX).

      ******************************************************************
       9000-RETURN.
           GOBACK.

      ******************************************************************
       9800-DEADLOCK.
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DEADLOCK'             TO CA-REASON-CODE
           MOVE 'Financial rows locked by another task - retry'
                                       TO CA-MESSAGE
           PERFORM 9000-RETURN.

      ******************************************************************
       9900-DB2-ERROR.
           MOVE SQLCODE                TO WS-SQLCODE-DISP
           SET CA-RC-DB2-ERROR         TO TRUE
           MOVE 'DB2ERR  '             TO CA-REASON-CODE
           STRING 'CLMFINTX Db2 error SQLCODE ' WS-SQLCODE-DISP
                  DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-ERR-CA
           MOVE 'ERRLOG  '             TO ERC-REQUEST-ID
           MOVE CA-USER-ID             TO ERC-USER-ID
           MOVE CA-BUS-DATE            TO ERC-BUS-DATE
           MOVE WS-PROGRAM-ID          TO ERC-CALLER-PGM
           MOVE WS-PROGRAM-ID          TO ERC-ER-PROGRAM
           MOVE WS-ERR-PARA            TO ERC-ER-PARAGRAPH
           MOVE SQLCODE                TO ERC-ER-SQLCODE
           MOVE CA-MESSAGE             TO ERC-ER-TEXT
           MOVE CA-FT-TXN-ID           TO WS-ID-DISP
           MOVE CA-FT-EXPOSURE-ID      TO WS-ID2-DISP
           STRING 'T' WS-ID-DISP ' E' WS-ID2-DISP
                  DELIMITED BY SIZE INTO ERC-ER-KEY
           CALL WS-ERR-PGM USING WS-ERR-CA
           PERFORM 9000-RETURN.
