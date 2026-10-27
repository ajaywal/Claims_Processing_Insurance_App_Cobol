# Integration events (binding contract)

Events live in `BIL_EVENT_QUEUE` / `CLM_EVENT_QUEUE` (one row per event,
`DIRECTION` I = inbound to this app, O = outbound). The producing app writes
an **outbound** row in its own queue; the drainer of the producing app
(`BILEVTDR` / `CLMEVTDR`) delivers it by writing an **inbound** row into the
target app's queue (cross-schema INSERT is the only cross-app write allowed),
then marks its own row `DONE`. The target's drainer processes inbound rows by
calling the owning style-A service. GenApp has no queue: `BILGAPIN` (batch)
detects GenApp policy changes by comparing `GENAPP.POLICY.LASTCHANGED` with
the last processed timestamp in `BIL_CONTROL` row `'GENAPPHW'` and raises
inbound `POL*` events itself.

`PAYLOAD` is fixed-position text. Numbers are unsigned zero-padded display;
amounts are 13 digits with implied 2 decimals and a trailing sign (`+`/`-`).

| Event | From → To | KEY | PAYLOAD layout (1-based positions) |
|---|---|---|---|
| `POLISSUE` | GenApp → BIL | policy no | 1-10 policy no · 11-20 customer no · 21 product (M/H/C/E) · 22-31 eff date · 32-41 exp date · 42-55 premium · 56-65 broker id · 66-73 plan code · 74-75 billing method (DB/AB) |
| `POLCHNG` | GenApp → BIL | policy no | 1-10 policy no · 11-24 new annual premium · 25-34 change eff date |
| `POLCANC` | GenApp → BIL | policy no | 1-10 policy no · 11-20 cancel eff date · 21-28 reason code |
| `CANCREQ` | BIL → GenApp ops | policy no | 1-10 policy no · 11-20 requested cancel date · 21-28 reason `NONPAY` · 29-42 past-due amount (written to BIL queue, target `GENA`, worked by operations; GenApp has no inbound API) |
| `CLMPAYMT` | CLM → BIL | check id | 1-10 claim check id · 11-22 claim number · 23-32 policy no · 33-46 amount · 47-106 payee name · 107-166 payee addr1 · 167-174 payee postcode · 175-178 pay method · 179-188 scheduled date |
| `DEDBILL` | CLM → BIL | claim no | 1-12 claim number · 13-22 policy no · 23-36 deductible amount owed by insured |
| `BILSTAT` | BIL → CLM | policy no | 1-10 policy no · 11-14 billing status (CURR / PDUE / DLNQ / CANC) · 15-28 past-due amount · 29-38 as-of date |
| `DSBSENT` | BIL → CLM | check id | 1-10 claim check id · 11-20 disbursement id · 21-30 sent date |

Retries: a failed delivery or processing increments `RETRY_COUNT`; after 5
the row goes to status `ERRR` and a trouble ticket / activity is raised.
