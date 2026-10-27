#!/usr/bin/env python3
"""Build the BillingCore/ClaimsCore lineage payload for the lineage page from
the Infinity analysis JSON + the curated Guidewire map."""
import json, sys, collections, re
sys.path.insert(0, '/home/claude/pnc-core-suite/tools')
import gw_map

A = json.load(open(sys.argv[1]))
OUT = sys.argv[2]
UTIL_PGMS = set()

SERVICES = {
 # billing
 'BILACCT0': ('BC', 'Account API / Account screens', 'POST /billing/v1/accounts · GET/PATCH account', 'Account creation normally arrives from PolicyCenter (account sync); balances are computed by BC.'),
 'BILPPER0': ('BC', 'Billing instructions from PolicyCenter', 'IBillingSystemPlugin: issue/change/cancel/reinstate/renew', 'PPNEW/PPCHG/PPCAN/PPRIN become NewPlcyPeriodBI, PolicyChange, Cancellation, Reinstatement BIs sent by PolicyCenter.'),
 'BILISCHD': ('BC', 'Invoice item scheduling (PaymentPlan engine)', 'Built-in on charge creation', 'Retired: BillingCenter schedules invoice items from the payment plan itself.'),
 'BILINVSV': ('BC', 'Invoices / InvoiceStream', 'GET /billing/v1/accounts/{id}/invoices', 'Ad-hoc invoices → "Make invoice billed" / invoice resequencing.'),
 'BILPAYRC': ('BC', 'DirectBillMoneyRcvd / payment entry', 'POST /billing/v1/accounts/{id}/payments', 'Lockbox and CSR entry become BC payment APIs; reversal → payment reversal.'),
 'BILPAYDS': ('BC', 'Payment allocation plan (distribution)', 'Built-in DirectBillPayment distribution', 'Distribution order (oldest past due, premium before tax before fees) becomes a payment allocation plan configuration.'),
 'BILDLQSV': ('BC', 'DelinquencyProcess', 'Built-in delinquency workflow', 'Start/exit criteria become delinquency plan thresholds.'),
 'BILWOFF0': ('BC', 'Writeoff with approval', 'Built-in writeoff + authority limits', ''),
 'BILDISB0': ('BC', 'Disbursement', 'Built-in disbursement + approval', 'CLMPAY disbursements retire: ClaimCenter pays claims.'),
 'BILCOMSV': ('BC', 'Commission (ItemCommission / ProducerCode)', 'Built-in commission engine', ''),
 'BILPROD0': ('BC', 'Producer / ProducerCode', 'Producer admin (synced from PolicyCenter)', ''),
 'BILINSTR': ('BC', 'PaymentInstrument', 'Built-in, tokenised via payment gateway plugin', ''),
 'BILHOLD0': ('BC', 'Hold', 'Built-in holds', ''),
 'BILGLPST': ('BC', 'T-accounts / LineItems', 'Built-in double entry', 'Retired: every BC financial transaction already posts balanced line items.'),
 'BILEVTQ':  ('BC', 'Messaging', 'Message destinations / App Events', ''),
 'BILAUTHZ': ('BC', 'AuthorityLimitProfile', 'Built-in authority limits', ''),
 'BILAUDIT': ('BC', 'History', 'Built-in history events', ''),
 'BILSEQGN': ('BC', 'Numbering plugins', 'Platform', 'Retired'),
 'BILDATES': ('BC', 'Business calendar utilities', 'Platform', 'Retired'),
 'BILMONEY': ('BC', 'MonetaryAmount / proration utilities', 'Platform', 'Retired'),
 'BILCODVL': ('BC', 'Typelists', 'Platform', 'Retired'),
 'BILERRLG': ('BC', 'Logging', 'Platform', 'Retired'),
 # claims
 'CLMCLAIM': ('CC', 'Claim (FNOL wizard / claim lifecycle)', 'POST /claim/v1/claims · /submit · /close · /reopen', 'FNOL validation, coverage-in-question and duplicate checks become claim rules.'),
 'CLMPOLVR': ('CC', 'Policy system plugin (IPolicySearchAdapter)', 'Retrieve policy from PolicyCenter as of loss date', 'Direct reads of GenApp tables retire.'),
 'CLMCONTC': ('CC/CM', 'Contact + ContactManager', 'POST /claim/v1/claims/{id}/contacts', 'De-duplication moves to ContactManager; sanctions screening becomes an integration.'),
 'CLMSEGMT': ('CC', 'Segmentation rules (Gosu)', 'Claim/exposure segmentation rule set', ''),
 'CLMASSGN': ('CC', 'Assignment rules (Gosu)', 'Global/default assignment rule sets', ''),
 'CLMEXPOS': ('CC', 'Exposure', 'POST /claim/v1/claims/{id}/exposures', ''),
 'CLMINCDT': ('CC', 'Incident', 'POST /claim/v1/claims/{id}/vehicle-incidents …', ''),
 'CLMFINTX': ('CC', 'Financials (Reserve/Payment/Recovery)', 'POST /claim/v1/claims/{id}/payments · reserves · recoveries', 'Authority-gated approvals use CC approval activities.'),
 'CLMAUTHZ': ('CC', 'AuthorityLimitProfile', 'Built-in transaction approval rules', ''),
 'CLMTOTLS': ('CC', 'Total loss (VehicleIncident.TotalLoss) rules', 'Rule / total-loss calculator integration', ''),
 'CLMACTV0': ('CC', 'Activity', 'POST /claim/v1/claims/{id}/activities', 'Trigger-event generation becomes workplan / event rules.'),
 'CLMNOTE0': ('CC', 'Note', 'POST /claim/v1/claims/{id}/notes', ''),
 'CLMDOCRQ': ('CC', 'Document', 'Document management integration', ''),
 'CLMRECOV': ('CC', 'Subrogation / Salvage / Recovery', 'SubrogationSummary · recoveries', ''),
 'CLMLITIG': ('CC', 'Matter', 'POST /claim/v1/claims/{id}/matters', ''),
 'CLMSIUSC': ('CC', 'SIU rules', 'SIU rule set + SIUStatus', ''),
 'CLMSRVRQ': ('CC', 'ServiceRequest (vendor services)', 'POST /claim/v1/claims/{id}/service-requests', ''),
 'CLMHIST0': ('CC', 'History', 'Built-in claim history', ''),
 'CLMAUDCH': ('—', 'Evidence ledger (outside Guidewire)', 'Keep as independent ledger', 'No Guidewire equivalent for a tamper-evident chain.'),
 'CLMEVTQ':  ('CC', 'Messaging', 'Message destinations / App Events', ''),
 'CLMSEQGN': ('CC', 'Numbering plugins', 'Platform', 'Retired'),
 'CLMDATES': ('CC', 'Business calendar utilities', 'Platform', 'Retired'),
 'CLMMONEY': ('CC', 'MonetaryAmount utilities', 'Platform', 'Retired'),
 'CLMCODVL': ('CC', 'Typelists', 'Platform', 'Retired'),
 'CLMERRLG': ('CC', 'Logging', 'Platform', 'Retired'),
}
BATCH = {
 'BILINVB0': 'BC Invoice batch process', 'BILINVDU': 'BC InvoiceDue batch process', 'BILDLQEV': 'BC Delinquency workflow (batch-driven)',
 'BILDOCGN': 'BC document production / print integration', 'BILLKBX0': 'Lockbox integration → payment API', 'BILACHDR': 'BC PaymentRequest batch',
 'BILACHRT': 'Payment gateway returns integration', 'BILDSBOV': 'BC automatic disbursement (excess payments)', 'BILDSBSN': 'BC disbursement → payments integration',
 'BILCOMPY': 'BC producer payment batch', 'BILAGSTM': 'BC agency bill statement batch', 'BILGAPIN': 'Retires — PolicyCenter sends billing instructions',
 'BILEVTDR': 'Retires — GW messaging', 'BILAGERP': 'Reporting (Guidewire Data Platform / Explore)', 'BILRECON': 'BC trial balance / reconciliation reports',
 'BILGLEXT': 'General ledger integration', 'BILEODDR': 'Retires — platform batch scheduling',
 'CLMFNLBT': 'FNOL API bulk load', 'CLMCHKIS': 'CC check batch → payments integration', 'CLMESCAL': 'CC ActivityEsc batch process',
 'CLMCATAS': 'CC catastrophe assignment (rules)', 'CLMRIRE0': 'Reinsurance system integration', 'CLMDUPCK': 'CC duplicate-claim rules / ISO',
 'CLMSEALR': 'Evidence ledger job (outside Guidewire)', 'CLMEVTDR': 'Retires — GW messaging', 'CLMISOSN': 'CC ISO ClaimSearch integration',
 'CLMGLEXT': 'General ledger integration', 'CLMLAGRP': 'Reporting (Data Platform / Explore)', 'CLMRSVRP': 'Reserve review rules + reporting',
 'CLMEODDR': 'Retires — platform batch scheduling',
}

def build(app, gwname):
    tabs, cols = [], []
    progs = {p['program']: p for p in A['programs'] if p['app'] == app}
    rules_by_prog = {p['program']: {r['id']: r['text'] for r in p['rules']} for p in progs.values()}
    contracts = A['contracts'].get(app, {})
    screens = A['screens'].get(app, {})
    for name, t in sorted(A['tables'].items()):
        if t['app'] != app: continue
        m = gw_map.T[t['short']]
        cc = collections.Counter()
        for c in t['cols']:
            r = gw_map.classify(t['short'], c['name'], t['pk'], t['fk'])
            L = A['lineage'][f"{name}.{c['name']}"]
            typ = c['type'] + (f"({c['len']}" + (f",{c['scale']}" if c['scale'] else '') + ")" if c['len'] else '')
            cols.append({
                't': t['short'], 'c': c['name'], 'ty': typ, 'nn': c['notnull'], 'pk': c['name'] in t['pk'],
                'ddl': f"{t['ddl']}:{c['line']}",
                'scr': [{'f': s, 'm': screens[s]['map'], 'l': screens[s]['label'], 'n': screens[s]['len']} for s in L['screen'] if s in screens],
                'ca': [{'f': f, 'm': contracts.get(f, {}).get('member', '')} for f in L['ca']],
                'nr': L['near'], 'w': L['writers'], 'r': sorted(set(L['readers']) - set(L['writers'])), 'k': L['keys'],
                'gw': r['gw'], 'cls': r['cls'], 'conf': r['conf'], 'rule': r['rule'], 'by': r['by'],
            })
            cc[r['cls']] += 1
        users = sorted({p for p in progs if name in progs[p]['tables']})
        tabs.append({'t': t['short'], 'e': m['entity'], 'note': m['note'], 'n': len(t['cols']), 'cls': dict(cc),
                     'pk': t['pk'], 'fk': [f['ref'].split('.')[-1] for f in t['fk']], 'ddl': t['ddl'], 'progs': users,
                     'crud': {p: progs[p]['tables'][name] for p in users}})
    svc = []
    for p in sorted(progs.values(), key=lambda x: (x['style'], x['program'])):
        s = SERVICES.get(p['program'])
        svc.append({'p': p['program'], 'st': p['style'], 'code': p['code'], 'cc': p['cyclomatic'], 'rules': len(p['rules']),
                    'serves': p['serves'], 'purpose': p['purpose'][:160],
                    'gwapp': s[0] if s else ('CC' if app == 'claims' else 'BC'),
                    'gw': s[1] if s else (BATCH.get(p['program']) or ('3270 screen → Guidewire UI (PCF / Jutro)' if p['style'] == 'B' else '')),
                    'api': s[2] if s else '', 'note': s[3] if s else '',
                    'tables': p['tables']})
    return {'gw': gwname, 'tables': tabs, 'cols': cols, 'services': svc}

doc = {'billing': build('billing', 'BillingCenter'), 'claims': build('claims', 'ClaimCenter')}
# GenApp tables read across systems
cross = []
for name, t in A['tables'].items():
    if t['app'] != 'genapp': continue
    for c in t['cols']:
        L = A['lineage'][f"{name}.{c['name']}"]
        if L['readers']:
            cross.append({'t': t['short'], 'c': c['name'], 'r': L['readers'], 'ca': L['ca']})
doc['cross'] = cross
doc['evidence_root'] = A['evidence_root']
json.dump(doc, open(OUT, 'w'), separators=(',', ':'))
for k in ('billing', 'claims'):
    print(k, len(doc[k]['tables']), 'tables', len(doc[k]['cols']), 'cols', len(doc[k]['services']), 'programs',
          sum(1 for c in doc[k]['cols'] if c['scr']), 'screen-linked', sum(1 for c in doc[k]['cols'] if c['ca']), 'interface-linked')
print('genapp columns read cross-system', len(cross))
