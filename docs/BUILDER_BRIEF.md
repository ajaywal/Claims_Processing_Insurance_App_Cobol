# Builder brief (read fully before writing code)

You are building one module of the PnC Core Suite in `/home/claude/pnc-core-suite`.
The suite is two synthetic mainframe applications in COBOL / CICS TS / Db2 for
z/OS that will be used to demonstrate legacy-to-Guidewire modernization:

* `billing/` — **BillingCoreAJ2026**, BillingCenter-parity billing (prefix BIL)
* `claims/`  — **ClaimsCoreAJ2026**, ClaimCenter-parity claims (prefix CLM)

Both sit beside IBM's GenApp policy system (tables in `shared/ddl/GENDDL01.sql`).
The code must read like a real carrier's estate: enterprise structure, real
business rules, careful Db2 handling. It is synthetic and must not copy any
third-party code.

## Read first (in this order)
1. `docs/CONVENTIONS.md` — binding rules (program styles A/B/C, COMMAREA,
   Db2, rule tags, paragraph numbering, definition of done).
2. `docs/EVENTS.md` — cross-application events and payload layouts.
3. `billing/src/common/BILSEQGN.cbl` — the exemplar program. Match its
   header block, structure, error handling and level of care.
4. Your app's `ddl/*.sql` (tables) and the DCLGEN members in `cpy/`
   (`BILDxxx` / `CLMDxxx`, generated — never edit them).
5. The contract copybooks in `cpy/` (`BILCxxx` / `CLMCxxx`) for every
   service you implement or call. They are fixed: do not edit them. If a
   contract truly cannot express what you need, finish what you can and
   report the gap in your final message.

## What you may create
* Only the programs listed in your assignment, at the paths given.
* New copybooks you need (file record layouts, report lines, working tables)
  named `<your-program-id-first-6-chars>xx.cpy` in the app's `cpy/`
  directory (e.g. `BILLKBR1.cpy` for the lockbox record). Never reuse a name.
* JCL for your batch programs in the app's `jcl/` directory
  (`<program>.jcl`, IKJEFT01 / DSN RUN, realistic DD statements).
* Nothing else. Do not touch other builders' files, `tools/`, `docs/`,
  `ddl/` (except a seed-data file if your assignment says so).

## Quality bar
* Target sizes in your assignment are for *real* code. Earn the lines with
  behaviour: every request id in the contract implemented, full validation
  with specific reason codes and messages, state-machine checks, cursors,
  calculations, audit/ledger/event side effects, error paths. No padding,
  no dead or commented-out code, no repeated boilerplate paragraphs.
* Tag each real business decision `* BR-<APP>-<MOD>-<nnn> <rule>` where it is
  implemented and list the rules in the header. Aim for 6–20 rules per
  service program, 3–10 per batch program.
* Batch programs (style C): input/output files with FD + record copybook,
  control totals, a printed run report (132-column), commit frequency,
  checkpoint/restart key, and a non-zero RETURN-CODE on failure
  (4 warnings, 8 rejects, 12+ abend-worthy).
* Online screens (style B): pseudo-conversational with a COMMAREA, PF3 exit,
  PF7/PF8 paging where lists exist, ENTER validation with cursor
  positioning (`<fld>L = -1`), messages on line 23/24, and every business
  action delegated to a style-A service via EXEC CICS LINK.

## Definition of done (check every program)
```
python3 tools/validate.py <billing|claims> <path/to/PROGRAM.cbl>
```
must print PASS. Fix every error and re-run. Then run it once more across all
your programs together. For screens, generate the symbolic map first:
`python3 tools/bmsgen.py <app>/bms/<MAPSET>.bms <app>/cpy`.

## Final message (your report back)
Keep it short and factual:
* table: program · style · lines · request ids / function · rules count · PASS
* any contract gaps or assumptions other builders must know
* anything you could not finish
