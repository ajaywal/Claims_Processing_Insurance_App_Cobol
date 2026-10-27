# PnC Core Suite — BillingCoreAJ2026 + ClaimsCoreAJ2026

Two **synthetic** enterprise-scale mainframe applications in **COBOL / CICS TS / Db2 for z/OS**, built to sit beside IBM's public [GenApp](https://github.com/cicsdev/cics-genapp) policy system and give a complete **policy → billing → claims** estate for Guidewire modernization demonstrations:

| Application | Prefix | Guidewire equivalent | Programs | Code lines* | Business rules | Db2 tables |
|---|---|---|---|---|---|---|
| **BillingCoreAJ2026** | `BIL` | BillingCenter | 43 (22 services · 4 screens · 17 batch) | 30,065 | 438 | 40 |
| **ClaimsCoreAJ2026** | `CLM` | ClaimCenter | 43 (25 services · 5 screens · 13 batch) | 30,347 | 437 | 36 |

\*Non-blank, non-comment lines. Each application is about 42,000 physical lines including copybooks, DDL, seed data, JCL and BMS. All 86 programs pass GnuCOBOL syntax validation; the integration checker finds 0 breaks.

> **Synthetic.** All code, schema and seed data are original and invented for demonstration and analysis tooling. Not affiliated with or derived from any carrier, Guidewire Software or IBM. GenApp table layouts are restated (IBM, Apache 2.0) only so the suite can read them. See [`NOTICE`](NOTICE).

## What it does

**BillingCore** — billing accounts and payers, tokenised payment instruments, holds, policy periods and billing instructions (new business, renewal, mid-term change, pro-rata cancellation, reinstatement), 12% Insurance Premium Tax, instalment scheduling across payment plans and invoice streams, invoicing, due/past-due transitions, a payment distribution engine (oldest past-due first, priority by charge pattern), lockbox and direct-debit (draft, return, settlement) processing, suspense, delinquency plans with timed steps (reminder → late fee → notice of intent → cancellation request to GenApp → write-off), write-offs and disbursements with authority limits, producer commissions (reserve → earn on payment → payable → producer pay run), agency-bill statements, a double-entry ledger with nightly trial-balance reconciliation and GL extract.

**ClaimsCore** — FNOL with policy verification directly against GenApp's Db2 tables, coverages and risk units by product, contacts with de-duplication and sanctions screening, segmentation and severity, rule-based assignment (round-robin / least-loaded), incidents, exposures, reserve lines, payments with deductible handling and coverage-limit checks, authority limits with supervisor-chain approval, checks and positive-pay, recoveries (subrogation, salvage, deductible), litigation, SIU fraud scoring, total-loss evaluation, catastrophe tagging, reinsurance cession (quota share and excess of loss), activities with escalation, notes, documents, vendor service requests, ISO ClaimSearch-style interface, a tamper-evident audit hash chain (FNV-1a 64) with nightly sealing, loss-run and reserve-adequacy reporting.

**Integration** — GenApp policy issue/change/cancel flows into billing (`BILGAPIN`); claim payments become billing disbursements and billing reports policy billing status back to claims, all through event queues with retry and parking (`docs/EVENTS.md`).

## Layout

```
billing/  claims/            one directory per application
  ddl/      Db2 DDL (BILDDLnn / CLMDDLnn) + seed data (BILSEED / CLMSEED)
  cpy/      DCLGENs (generated), COMMAREA contracts (xxxCnnn), record/report layouts, symbolic maps
  src/common  src/online  src/batch     COBOL source
  bms/      3270 mapsets            jcl/   batch jobs, nightly stream, CSD definitions
shared/     GenApp tables read by the suite (DDL + DCLGENs)
tools/      dclgen.py  validate.py  bmsgen.py  calen.py  xref.py  seedcheck.py  seedfit.py
docs/       CONVENTIONS.md  EVENTS.md  BUILDER_BRIEF.md  inventory.md  xref.json
```

Program styles: **A** dual-mode services (no EXEC CICS; LINKed online, CALLed from batch), **B** pseudo-conversational CICS 3270 front ends, **C** batch with checkpoint/restart, control totals and 132-column reports. Every service takes a 4,000-byte COMMAREA (160-byte header + typed body); all 48 contracts are length-verified. Business rules are tagged `BR-<APP>-<MOD>-<nnn>` in source for rule extraction. See [`docs/CONVENTIONS.md`](docs/CONVENTIONS.md) and the full [`docs/inventory.md`](docs/inventory.md).

## Verify it yourself (GnuCOBOL 3.x, Python 3)

```bash
python3 tools/validate.py billing          # 43/43 programs pass
python3 tools/validate.py claims           # 43/43 programs pass
python3 tools/xref.py                      # call graph, request ids, CRUD; 0 integration breaks
python3 tools/calen.py billing BILC        # every COMMAREA contract is exactly 160 / 3840 bytes
python3 tools/seedfit.py                   # every seed row fits the schema
```

`validate.py` stands in for the CICS and Db2 precompilers (EXEC blocks → CONTINUE, EXEC SQL INCLUDE → COPY) and runs `cobc -fsyntax-only -std=ibm`. It proves syntax and data-name resolution, not runtime behaviour: nothing has been executed against a Db2 subsystem or CICS region. `docs/xref.json` is the machine-readable program/table/rule graph for lineage and knowledge-graph tooling.

Author: Ajay Walgude · 2026

## Analysis and Guidewire lineage

`docs/analysis/` holds a Cap Infinity-style static analysis of the suite and a field-level lineage to Guidewire, regenerated from source:

```bash
python3 tools/infinity_analyze.py docs/analysis/infinity_analysis.json   # programs, CRUD, data flow, rules, JCL, SHA-256 evidence
```

* `pnc-infinity-analysis.html` — inventory, call graph, CRUD matrix, rules, complexity, batch, integration, Guidewire roadmap, evidence
* `guidewire-lineage.html` — screen → interface → programs → Db2 → Guidewire for GenApp, BillingCore and ClaimsCore
* `guidewire_column_map.csv` — every BillingCore / ClaimsCore column with its BillingCenter / ClaimCenter target, class and confidence (`tools/gw_map.py`)
