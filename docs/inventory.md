# Program inventory

Style A = dual-mode service (CICS LINK or batch CALL), B = CICS 3270 front end, C = batch.
Generated from `docs/xref.json` by the build.

### BillingCoreAJ2026 — BillingCenter parity

| Program | Style | Code lines | Rules | Purpose |
|---|---|---|---|---|
| `BILAUDIT` | A | 231 | 8 | Field-level audit trail. |
| `BILAUTHZ` | A | 263 | 10 | Authority limit check. |
| `BILCODVL` | A | 173 | 5 | Validates a code against BIL_CODE_TABLE (the typelist store) and returns its description. |
| `BILDATES` | A | 471 | 9 | Date arithmetic for billing schedules, due dates, aging and delinquency timelines. |
| `BILERRLG` | A | 166 | 5 | Central technical error logger. |
| `BILEVTQ` | A | 523 | 11 | Integration event queue for billing. |
| `BILGLPST` | A | 501 | 13 | Double-entry ledger posting. |
| `BILMONEY` | A | 168 | 9 | Money arithmetic shared by billing: instalment splits, basis-point rates and day-based pro-rata so every progr |
| `BILSEQGN` | A | 171 | 4 | Surrogate key and business number generator. |
| `BILACCT0` | A | 1,597 | 20 | Billing account service. |
| `BILCOMSV` | A | 742 | 14 | Producer commission service. |
| `BILDISB0` | A | 1,124 | 17 | Outgoing money. |
| `BILDLQSV` | A | 909 | 15 | Delinquency process service. |
| `BILHOLD0` | A | 558 | 11 | Account holds. |
| `BILINSTR` | A | 668 | 13 | Payment instrument service. |
| `BILINVSV` | A | 841 | 15 | Invoice service. |
| `BILISCHD` | A | 1,027 | 17 | Invoice item scheduler. |
| `BILPAYDS` | A | 914 | 16 | Payment distribution engine. |
| `BILPAYRC` | A | 1,431 | 19 | Payment receipt service. |
| `BILPPER0` | A | 1,675 | 20 | Policy period and billing instruction service. |
| `BILPROD0` | A | 593 | 12 | Producer (broker) maintenance. |
| `BILWOFF0` | A | 903 | 14 | Write-off service. |
| `BILACIP` | B | 467 | 8 | Account summary inquiry (transaction BACI, map BILACM). |
| `BILIVMP` | B | 416 | 6 | Invoice list (transaction BIVL, map BILIVM). |
| `BILMNUP` | B | 232 | 5 | BillingCore main menu (transaction BMNU, map BILMNU). |
| `BILPAYP` | B | 637 | 10 | Payment entry (transaction BPAY, map BILPYM). |
| `BILACHDR` | C | 718 | 8 | Direct debit (ACH / BACS) draft generation. |
| `BILACHRT` | C | 883 | 9 | Direct debit returns and settlement. |
| `BILAGERP` | C | 755 | 8 | Receivables aging report. |
| `BILAGSTM` | C | 570 | 6 | Agency-bill statements. |
| `BILCOMPY` | C | 604 | 8 | Producer payment run. |
| `BILDLQEV` | C | 946 | 8 | Delinquency event executor. |
| `BILDOCGN` | C | 699 | 7 | Document extract. |
| `BILDSBOV` | C | 578 | 7 | Overpayment sweep. |
| `BILDSBSN` | C | 728 | 7 | Disbursement send. |
| `BILEODDR` | C | 528 | 7 | End-of-day driver, run twice by job BILEOD: MODE=BEGIN  first step: BATCH_STATUS must be O (online day), set B |
| `BILEVTDR` | C | 970 | 9 | Billing event drainer (docs/EVENTS.md). |
| `BILGAPIN` | C | 965 | 9 | GenApp policy intake. |
| `BILGLEXT` | C | 501 | 7 | General-ledger extract for finance. |
| `BILINVB0` | C | 896 | 8 | Invoice run for the business date. |
| `BILINVDU` | C | 871 | 8 | Due transition for the business date. |
| `BILLKBX0` | C | 661 | 8 | Bank lockbox posting. |
| `BILRECON` | C | 791 | 8 | Ledger reconciliation. |

### ClaimsCoreAJ2026 — ClaimCenter parity

| Program | Style | Code lines | Rules | Purpose |
|---|---|---|---|---|
| `CLMAUDCH` | A | 608 | 12 | Tamper-evident audit hash chain for regulated claim events (reserve and payment authority, SIU referral, litig |
| `CLMCODVL` | A | 172 | 5 | Code table validation against CLM_CODE_TABLE (ClaimCenter typelists). |
| `CLMDATES` | A | 463 | 14 | Date arithmetic on ISO yyyy-mm-dd dates. |
| `CLMERRLG` | A | 130 | 5 | Central technical error logger. |
| `CLMEVTQ` | A | 639 | 12 | Integration event queue (CLM_EVENT_QUEUE) API per docs/EVENTS.md. |
| `CLMHIST0` | A | 295 | 7 | Claim history writer and reader. |
| `CLMMONEY` | A | 177 | 9 | Money arithmetic in GBP pence precision. |
| `CLMSEQGN` | A | 207 | 6 | Surrogate key and business number generator for the claims application. |
| `CLMACTV0` | A | 838 | 13 | Claim activity (diary) engine. |
| `CLMASSGN` | A | 700 | 13 | Assignment engine. |
| `CLMAUTHZ` | A | 275 | 10 | Financial authority limits. |
| `CLMCLAIM` | A | 2,083 | 22 | Claim service - first notice of loss and the claim life cycle. |
| `CLMCONTC` | A | 989 | 15 | Contact (address book) service. |
| `CLMDOCRQ` | A | 556 | 8 | Claim document requests. |
| `CLMEXPOS` | A | 1,162 | 15 | Exposure service. |
| `CLMFINTX` | A | 2,084 | 23 | Claim financial transactions. |
| `CLMINCDT` | A | 812 | 11 | Incident service. |
| `CLMLITIG` | A | 860 | 13 | Litigation matters. |
| `CLMNOTE0` | A | 455 | 8 | Claim notes. |
| `CLMPOLVR` | A | 720 | 15 | Policy verification against IBM GenApp. |
| `CLMRECOV` | A | 1,069 | 14 | Recoveries. |
| `CLMSEGMT` | A | 411 | 12 | Claim segmentation and severity. |
| `CLMSIUSC` | A | 532 | 10 | Special Investigation Unit (fraud) scoring. |
| `CLMSRVRQ` | A | 641 | 9 | Vendor service requests (appraisal, legal, repair, rental, tow). |
| `CLMTOTLS` | A | 525 | 10 | Vehicle total-loss evaluation. |
| `CLMACTP` | B | 582 | 8 | "My activities" for the signed-on user (CLMACTV0 ACTLST, user from EXEC CICS ASSIGN). |
| `CLMFINP` | B | 979 | 15 | Reserve and payment entry with a confirm step, and supervisor approval / rejection of pending transactions. |
| `CLMFNLP` | B | 1,078 | 17 | First notice of loss in two steps. |
| `CLMINQP` | B | 647 | 8 | Claim summary by claim number (CLMCLAIM CLMINQ): header, owner, segment/severity, flags, totals and up to 10 e |
| `CLMMNUP` | B | 285 | 5 | ClaimsCore main menu. |
| `CLMCATAS` | C | 546 | 6 | Catastrophe tagging. |
| `CLMCHKIS` | C | 620 | 8 | Claim check issuance. |
| `CLMDUPCK` | C | 606 | 6 | Duplicate claim detection across the book. |
| `CLMEODDR` | C | 478 | 6 | End-of-day driver for CLM_CONTROL. |
| `CLMESCAL` | C | 684 | 6 | Nightly activity escalation sweep. |
| `CLMEVTDR` | C | 1,071 | 10 | Integration event drainer for CLM_EVENT_QUEUE. |
| `CLMFNLBT` | C | 1,290 | 10 | Bulk first notice of loss intake from the web and partner portal gateway. |
| `CLMGLEXT` | C | 594 | 6 | Claims financial extract for the general ledger. |
| `CLMISOSN` | C | 1,019 | 8 | ISO ClaimSearch-style industry fraud bureau interface. |
| `CLMLAGRP` | C | 679 | 7 | Loss run and aging report. |
| `CLMRIRE0` | C | 678 | 8 | Reinsurance cession. |
| `CLMRSVRP` | C | 568 | 6 | Reserve adequacy review. |
| `CLMSEALR` | C | 540 | 6 | Nightly audit chain seal. |
