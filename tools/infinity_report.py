#!/usr/bin/env python3
"""Derive the Cap Infinity analysis payload from analysis.json + source."""
import json, re, os, sys, collections, hashlib
sys.path.insert(0, '/home/claude/pnc-core-suite/tools')
import gw_map
ROOT = '/home/claude/pnc-core-suite'
A = json.load(open(sys.argv[1]))
S = json.load(open(sys.argv[2]))      # suite.json from the lineage build (GW services map)
out = sys.argv[3]

P = A['programs']
by = {p['program']: p for p in P}
val = {}
for l in open(os.path.join(os.path.dirname(sys.argv[1]), 'validate.txt')):
    m = re.match(r'(PASS|FAIL)\s+(\S+)', l)
    if m: val[os.path.basename(m.group(2))[:-4].upper()] = m.group(1)

# fan-in / fan-out
fan_in = collections.defaultdict(set)
for p in P:
    for c in p['calls']:
        if c['target'] in by: fan_in[c['target']].add(p['program'])

# events produced / consumed (literals)
EVT = ['POLISSUE', 'POLCHNG', 'POLCANC', 'CANCREQ', 'CLMPAYMT', 'DEDBILL', 'BILSTAT', 'DSBSENT']
ev = collections.defaultdict(lambda: {'produce': set(), 'consume': set()})
for p in P:
    src = open(os.path.join(ROOT, p['path']), encoding='latin-1').read().upper()
    code = '\n'.join(l[7:72] for l in src.splitlines() if len(l) > 6 and l[6] not in '*/')
    for e in EVT:
        lit = r"'" + e + r"\s*'"
        consts = set(re.findall(r'([A-Z0-9-]+)\s+PIC\s+X\(8\)\s+VALUE\s+' + lit, code))
        names = '|'.join([lit] + [re.escape(c) for c in consts])
        if re.search(r"MOVE\s+(?:" + names + r")\s+TO\s+[A-Z0-9-]*(?:EV-TYPE|EVENT-TYPE)", code): ev[e]['produce'].add(p['program'])
        if re.search(r"WHEN\s+(?:" + names + r")", code) or re.search(r"=\s*(?:" + names + r")", code):
            if p['program'] not in ev[e]['produce']: ev[e]['consume'].add(p['program'])
events = [{'e': e, 'produce': sorted(v['produce']), 'consume': sorted(v['consume'])} for e, v in ev.items()]

# cross-application data access
cross = []
for p in P:
    own = 'BILDB' if p['app'] == 'billing' else 'CLMDB'
    for t, ops in p['tables'].items():
        if not t.startswith(own):
            cross.append({'p': p['program'], 'app': p['app'], 't': t, 'ops': ops})

# copybook reuse
cpy_use = collections.Counter()
for p in P:
    for c in set(p['copies']): cpy_use[c] += 1
def ckind(c):
    if re.match(r'^(BIL|CLM)D', c) or c.startswith('GEND'): return 'DCLGEN'
    if re.match(r'^(BIL|CLM)C', c): return 'Contract'
    if c in ('SQLCA', 'DFHAID', 'DFHBMSCA', 'DFHEIBLK'): return 'System'
    if c.endswith('MSET'): return 'Symbolic map'
    return 'Record / work'
copybooks = [{'c': c, 'n': n, 'k': ckind(c)} for c, n in cpy_use.most_common()]

# table load order (topological by FK) per app
def topo(app):
    ts = {n: t for n, t in A['tables'].items() if t['app'] == app}
    deps = {n: {f['ref'] for f in t['fk'] if f['ref'] in ts and f['ref'] != n} for n, t in ts.items()}
    level, done = {}, set()
    while len(done) < len(ts):
        ready = [n for n in ts if n not in done and deps[n] <= done]
        if not ready: ready = [n for n in ts if n not in done]
        lv = max([level.get(d, -1) for n in ready for d in deps[n]] + [-1]) + 1
        for n in sorted(ready):
            level[n] = max([level.get(d, -1) for d in deps[n]] + [-1]) + 1; done.add(n)
    return [{'t': ts[n]['short'], 'lvl': level[n], 'deps': sorted(ts[d]['short'] for d in deps[n]), 'e': gw_map.T[ts[n]['short']]['entity']} for n in sorted(ts, key=lambda n: (level[n], n))]

# unreferenced paragraphs (indicative)
unref = {}
for p in P:
    targets = set()
    for v in p['perform'].values(): targets |= set(v)
    src = open(os.path.join(ROOT, p['path']), encoding='latin-1').read().upper()
    thru = set(re.findall(r'THRU\s+([A-Z0-9-]+)', src)) | set(re.findall(r'GO\s+TO\s+([A-Z0-9-]+)', src))
    names = [x['name'] for x in p['paragraphs']]
    u = [n for i, n in enumerate(names) if i > 0 and n not in targets and n not in thru and not n.endswith('-EXIT')]
    unref[p['program']] = u

# complexity bands per program (decision density)
def band(cc):
    return 'Low' if cc <= 20 else 'Moderate' if cc <= 50 else 'High' if cc <= 100 else 'Very high'

progs = []
svc = {s['p']: s for a in ('billing', 'claims') for s in S[a]['services']}
for p in P:
    sv = svc.get(p['program'], {})
    gwr = sv.get('gw', '')
    disp = 'Retire' if re.search(r'retire', (gwr + ' ' + sv.get('note', '')), re.I) else ('Replace by configuration' if re.search(r'Built-in|Platform|rule', sv.get('api', '') + gwr, re.I) else 'Re-implement / integrate')
    if p['style'] == 'B': disp = 'Replace by Guidewire UI'
    progs.append({
        'p': p['program'], 'app': p['app'], 'st': p['style'], 'path': p['path'], 'purpose': p['purpose'],
        'lines': p['lines'], 'code': p['code'], 'cmt': p['comments'], 'para': len(p['paragraphs']), 'cc': p['cyclomatic'],
        'dec': p['decisions'], 'dens': round(p['cyclomatic'] * 100 / max(p['code'], 1), 1), 'band': band(p['cyclomatic']),
        'sql': p['sql'], 'tables': p['tables'], 'calls': p['calls'], 'fanin': sorted(fan_in.get(p['program'], [])),
        'serves': p['serves'], 'cics': p['cics'], 'files': p['files'], 'copies': p['copies'], 'transid': p['transid'],
        'rules': p['rules'], 'unref': unref[p['program']], 'paras': [[x['name'], x['size']] for x in p['paragraphs']],
        'val': val.get(p['program'], '?'), 'gw': gwr, 'gwapp': sv.get('gwapp', ''), 'api': sv.get('api', ''), 'disp': disp,
    })

jobs = [j for j in A['jobs']]
tables = []
for n, t in A['tables'].items():
    users = [p['p'] for p in progs if n in p['tables']]
    writers = [p['p'] for p in progs if n in p['tables'] and set(p['tables'][n]) & set('CUD')]
    tables.append({'t': t['short'], 'full': n, 'app': t['app'], 'cols': len(t['cols']), 'pk': t['pk'],
                   'fk': [f['ref'].split('.')[-1] for f in t['fk']], 'idx': len(t['idx']), 'users': users, 'writers': writers,
                   'e': gw_map.T[t['short']]['entity'] if t['short'] in gw_map.T else 'PolicyCenter (via GenApp lineage)'})

lin = {}
for app in ('billing', 'claims'):
    cols = S[app]['cols']
    lin[app] = {'cols': len(cols), 'scr': sum(1 for c in cols if c['scr']), 'ca': sum(1 for c in cols if c['ca']),
                'cls': dict(collections.Counter(c['cls'] for c in cols)), 'conf': dict(collections.Counter(c['conf'] for c in cols)),
                'by': dict(collections.Counter(c['by'] for c in cols))}

files_by_kind = collections.Counter()
for e in A['evidence']:
    ext = e['path'].rsplit('.', 1)[-1].lower() if '.' in e['path'] else 'other'
    files_by_kind[ext] += 1
cpy_lines = {}
for app in ('billing', 'claims'):
    n = 0; f = 0
    for fn in os.listdir(os.path.join(ROOT, app, 'cpy')):
        f += 1; n += sum(1 for _ in open(os.path.join(ROOT, app, 'cpy', fn), encoding='latin-1'))
    cpy_lines[app] = [f, n]

# CICS transactions from the CSD definitions
trans = []
for app in ('billing', 'claims'):
    for f in os.listdir(os.path.join(ROOT, app, 'jcl')):
        if not f.upper().endswith('CSD.JCL'): continue
        txt = open(os.path.join(ROOT, app, 'jcl', f), encoding='latin-1').read().upper()
        for m in re.finditer(r'DEFINE\s+TRANSACTION\(\s*([A-Z0-9]{4})\s*\)(.*?)(?=DEFINE|\Z)', txt, re.S):
            pg = re.search(r'PROGRAM\(\s*([A-Z0-9]+)\s*\)', m.group(2))
            trans.append({'t': m.group(1), 'p': pg.group(1) if pg else '', 'app': app})
for p in progs:
    p['transid'] = sorted({t['t'] for t in trans if t['p'] == p['p']})
doc = {'trans': trans, 'progs': progs, 'tables': tables, 'events': events, 'cross': cross, 'copybooks': copybooks,
       'load': {'billing': topo('billing'), 'claims': topo('claims')}, 'jobs': jobs, 'lin': lin,
       'evidence': A['evidence'], 'root': A['evidence_root'], 'kinds': dict(files_by_kind), 'cpy': cpy_lines,
       'commit': '98b04a81ff7a983ac0c560e4b6866801ab97ea42', 'compiler': 'GnuCOBOL 3.1.2.0',
       'screens': {a: len(v) for a, v in A['screens'].items()}}
json.dump(doc, open(out, 'w'), separators=(',', ':'))
print('programs', len(progs), 'PASS', sum(1 for p in progs if p['val'] == 'PASS'))
print('events', [(e['e'], e['produce'], e['consume']) for e in events])
print('cross-app', [(c['p'], c['t'], c['ops']) for c in cross][:20])
print('unreferenced paragraphs', sum(len(v) for v in unref.values()), sorted(((len(v), k) for k, v in unref.items()), reverse=True)[:5])
print('bands', collections.Counter(p['band'] for p in progs))
print('disp', collections.Counter((p['app'], p['disp']) for p in progs))
print('size', os.path.getsize(out))
