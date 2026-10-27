#!/usr/bin/env python3
"""infinity_analyze.py - deterministic static analysis of the PnC Core Suite
(Cap Infinity style). Everything here is mined from source; nothing is guessed.

Produces one JSON document with:
  tables     DDL: columns, types, keys, indexes, owning app, DCLGEN prefix
  contracts  COMMAREA contract fields (CA-xx-...) with their copybook
  screens    BMS fields with position, length and the on-screen label
  programs   metrics (lines, paragraphs, decisions, cyclomatic), SQL by verb,
             column-level CRUD, calls with request ids, CICS commands, files,
             copybooks, business rules (id, text, line numbers)
  lineage    per column: writers/readers and the field-level chain
             screen field -> COMMAREA field -> host variable -> column,
             derived from MOVE / COMPUTE / SQL host-variable data flow
  jobs       JCL job streams (steps, programs, DD names)
  evidence   SHA-256 of every source file (ledger seed)
Usage: python3 tools/infinity_analyze.py <out.json>
"""
import os, re, sys, json, glob, hashlib, collections

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
APPS = {'billing': 'BIL', 'claims': 'CLM'}

def rd(p):
    return open(p, encoding='latin-1').read().splitlines()

def is_comment(l):
    return len(l) > 6 and l[6] in '*/'

# ---------------------------------------------------------------- DDL
def parse_ddl():
    tables, dcl = {}, {}
    files = []
    for app in APPS:
        files += [(app, f) for f in sorted(glob.glob(os.path.join(ROOT, app, 'ddl', '*DDL*.sql')))]
    files += [('genapp', f) for f in sorted(glob.glob(os.path.join(ROOT, 'shared', 'ddl', 'GENDDL*.sql')))]
    for app, f in files:
        cur, member, prefix = None, None, None
        for i, l in enumerate(rd(f), 1):
            m = re.match(r'--\s*DCLGEN\s+(\S+)\s+PREFIX\s+(\S+)', l)
            if m: member, prefix = m.groups(); continue
            m = re.match(r'\s*CREATE TABLE\s+(\S+)', l)
            if m:
                name = m.group(1).upper()
                cur = {'name': name, 'short': name.split('.')[-1], 'app': app, 'ddl': os.path.relpath(f, ROOT),
                       'line': i, 'member': member, 'prefix': prefix, 'cols': [], 'pk': [], 'fk': [], 'idx': []}
                tables[name] = cur
                if member: dcl[member] = name
                continue
            m = re.match(r'\s*CREATE (UNIQUE )?INDEX\s+\S+\s+ON\s+(\S+)\s*\(([^)]*)\)', l)
            if m and m.group(2).upper() in tables:
                tables[m.group(2).upper()]['idx'].append({'unique': bool(m.group(1)), 'cols': [c.strip() for c in m.group(3).split(',')]})
            if not cur: continue
            m = re.match(r'\s*PRIMARY KEY\s*\(([^)]*)\)', l)
            if m: cur['pk'] = [c.strip() for c in m.group(1).split(',')]; continue
            m = re.match(r'\s*FOREIGN KEY\s*\(([^)]*)\)\s*REFERENCES\s+(\S+)', l)
            if m: cur['fk'].append({'cols': [c.strip() for c in m.group(1).split(',')], 'ref': m.group(2).upper()}); continue
            m = re.match(r'\s*\(?\s*([A-Z0-9_]+)\s+(CHAR|VARCHAR|INTEGER|SMALLINT|BIGINT|DECIMAL|DATE|TIMESTAMP|TIME)(\((\d+)(,(\d+))?\))?(.*)', l)
            if m:
                rest = m.group(7)
                cur['cols'].append({'name': m.group(1), 'type': m.group(2), 'len': int(m.group(4) or 0),
                                    'scale': int(m.group(6) or 0), 'notnull': 'NOT NULL' in rest,
                                    'default': 'DEFAULT' in rest, 'line': i})
            if l.strip().startswith('IN ') or l.rstrip().endswith(';'):
                cur = None
    return tables, dcl

# ---------------------------------------------------------------- copybooks
PIC_LEN = re.compile(r'PIC\s+(S?)([X9AVZ\-\+\.\,\(\)0-9]+)')
def pic_bytes(pic, usage):
    s = re.sub(r'([X9AZ])\((\d+)\)', lambda m: m.group(1) * int(m.group(2)), pic)
    digits = len(re.findall(r'[9Z]', s)); chars = len(re.findall(r'[XA]', s))
    if 'COMP-3' in usage: return digits // 2 + 1
    if 'COMP' in usage: return 2 if digits <= 4 else (4 if digits <= 9 else 8)
    return digits + chars + len(re.findall(r'[\-\+\.\,]', s))

def parse_contracts():
    out = {}
    for app, pfx in APPS.items():
        for f in sorted(glob.glob(os.path.join(ROOT, app, 'cpy', pfx + 'C*.cpy'))):
            mem = os.path.basename(f)[:-4]
            for i, l in enumerate(rd(f), 1):
                if is_comment(l): continue
                m = re.match(r'\s*(\d\d)\s+(CA-[A-Z0-9-]+)(\s+.*)?', l[7:72] if len(l) > 7 else '')
                if m:
                    pic = re.search(r'PIC\s+(\S+)', l)
                    out.setdefault(app, {})[m.group(2)] = {'member': mem, 'level': m.group(1), 'line': i,
                        'pic': pic.group(1).rstrip('.') if pic else '', 'usage': 'COMP-3' if 'COMP-3' in l else ('COMP' if ' COMP' in l else '')}
    return out

def parse_bms():
    out = {}
    for app in APPS:
        for f in glob.glob(os.path.join(ROOT, app, 'bms', '*.bms')):
            stmts, cur = [], ''
            for l in rd(f):
                if l.startswith('*') or not l.strip(): continue
                body = l[:71].rstrip(); cont = len(l) >= 72 and l[71] != ' '
                cur = cur + body.strip() if cur else body
                if not cont and not body.endswith(','):
                    stmts.append(cur); cur = ''
            mapname, lastlabel = None, {}
            for s in stmts:
                m = re.match(r'^(\S*)\s+(DFHMSD|DFHMDI|DFHMDF)\s*(.*)$', s)
                if not m: continue
                label, macro, ops = m.groups()
                if macro == 'DFHMDI': mapname = label; continue
                if macro != 'DFHMDF': continue
                pos = re.search(r'POS=\((\d+),(\d+)\)', ops); ln = re.search(r'LENGTH=(\d+)', ops)
                init = re.search(r"INITIAL='([^']*)'", ops)
                row = int(pos.group(1)) if pos else 0
                if not label:
                    if init and init.group(1).strip() and not re.match(r'^[_\s\.:]+$', init.group(1)):
                        lastlabel[(mapname, row)] = init.group(1).strip()
                    continue
                out.setdefault(app, {})[label] = {'map': mapname, 'row': row, 'col': int(pos.group(2)) if pos else 0,
                    'len': int(ln.group(1)) if ln else 0, 'label': lastlabel.get((mapname, row), ''),
                    'mapset': os.path.basename(f)[:-4]}
    return out

# ---------------------------------------------------------------- programs
def statements(code):
    """split procedure code into rough statements on verbs"""
    return code

VERBS = r'(MOVE|COMPUTE|IF|ELSE|END-IF|EVALUATE|WHEN|END-EVALUATE|PERFORM|CALL|EXEC|INITIALIZE|SET|ADD|SUBTRACT|MULTIPLY|DIVIDE|STRING|UNSTRING|INSPECT|DISPLAY|GOBACK|STOP|OPEN|CLOSE|READ|WRITE|REWRITE|CONTINUE|GO|EXIT|SEARCH|ACCEPT)'

def parse_program(path, app, tables, dcl, contracts, bms):
    raw = rd(path)
    src = open(path, encoding='latin-1').read()
    lines = [(i, l[7:72] if len(l) > 7 else '') for i, l in enumerate(raw, 1) if not is_comment(l)]
    code_lines = [x for x in lines if x[1].strip()]
    up = '\n'.join(t for _, t in lines).upper()
    pid = re.search(r'PROGRAM-ID\.\s*([A-Z0-9]+)', up)
    pid = pid.group(1) if pid else os.path.basename(path)[:-4].upper()
    style = 'B' if 'EXEC CICS' in up else ('C' if os.sep + 'batch' + os.sep in path else 'A')
    p = {'program': pid, 'app': app, 'style': style, 'path': os.path.relpath(path, ROOT),
         'lines': len(raw), 'code': len(code_lines), 'comments': sum(1 for l in raw if is_comment(l)),
         'blank': sum(1 for l in raw if not l.strip())}
    # procedure division split
    pd_idx = next((k for k, (i, t) in enumerate(lines) if re.search(r'\bPROCEDURE\s+DIVISION\b', t.upper())), len(lines))
    proc = lines[pd_idx:]
    proc_text = '\n'.join(t for _, t in proc).upper()
    # paragraphs and PERFORM graph
    paras, cur, perf = [], None, collections.defaultdict(set)
    for i, t in proc[1:]:
        m = re.match(r'^ ([A-Z0-9][A-Z0-9-]*)\s*\.\s*$', t[:1].replace('', '') + t) if False else re.match(r'^\s?([A-Z0-9][A-Z0-9-]*)\s*\.\s*$', t)
        if m and not t.startswith('    ') and m.group(1) not in ('EXIT', 'GOBACK', 'CONTINUE'):
            cur = m.group(1); paras.append({'name': cur, 'line': i}); continue
        for pm in re.finditer(r'\bPERFORM\s+([A-Z0-9][A-Z0-9-]*[A-Z0-9])', t.upper()):
            tgt = pm.group(1)
            if tgt not in ('UNTIL', 'VARYING', 'WITH', 'TEST') and cur:
                perf[cur].add(tgt)
    for k in range(len(paras)):
        end = paras[k + 1]['line'] - 1 if k + 1 < len(paras) else (proc[-1][0] if proc else paras[k]['line'])
        paras[k]['size'] = sum(1 for i, t in code_lines if paras[k]['line'] <= i <= end)
    p['paragraphs'] = paras
    p['perform'] = {k: sorted(v) for k, v in perf.items()}
    # decisions / cyclomatic
    nif = len(re.findall(r'\bIF\b', proc_text)) - len(re.findall(r'\bEND-IF\b', proc_text))
    nif = len(re.findall(r'(?<!END-)\bIF\b', proc_text))
    nwhen = len(re.findall(r'\bWHEN\b(?!\s+OTHER)', proc_text))
    nuntil = len(re.findall(r'\bUNTIL\b', proc_text))
    nbool = len(re.findall(r'\b(AND|OR)\b', proc_text))
    p['decisions'] = {'if': nif, 'when': nwhen, 'until': nuntil, 'bool': nbool}
    p['cyclomatic'] = 1 + nif + nwhen + nuntil + nbool
    # SQL
    sql, crud = [], collections.defaultdict(lambda: collections.defaultdict(set))
    hv_col = []   # (hostvar, table, column, op)
    for m in re.finditer(r'EXEC\s+SQL(.*?)END-EXEC', up, re.S):
        s = ' '.join(m.group(1).split())
        verb = s.split(' ')[0] if s else ''
        if verb in ('INCLUDE', 'BEGIN', 'END'): continue
        if verb == 'DECLARE' and re.match(r'DECLARE\s+\S+\s+TABLE', s): continue
        tabs = [t for t in re.findall(r'\b((?:BILDB|CLMDB|GENAPP)\.[A-Z_]+)\b', s) if t in tables]
        aliases = dict((a, t) for t, a in re.findall(r'\b((?:BILDB|CLMDB|GENAPP)\.[A-Z_]+)\s+(?:AS\s+)?([A-Z][A-Z0-9]?)\b(?=\s*(?:,|JOIN|WHERE|ON|LEFT|INNER|ORDER|GROUP|FOR|WITH|\)|$))', s))
        op = {'INSERT': 'C', 'UPDATE': 'U', 'DELETE': 'D'}.get(verb, 'R')
        if verb == 'DECLARE' and 'CURSOR' in s:
            inner = re.search(r'\bFOR\s+(SELECT|UPDATE|INSERT|DELETE)\b', s)
            op = 'R'
        sql.append({'verb': 'CURSOR' if (verb == 'DECLARE' and 'CURSOR' in s) else verb, 'tables': sorted(set(tabs))})
        colsets = {t: set(c['name'] for c in tables[t]['cols']) for t in tabs}
        ids = set(re.findall(r'(?<![:\w-])([A-Z][A-Z0-9_]+)\b', s))
        for t in tabs:
            touched = ids & colsets[t]
            for c in touched: crud[t][c].add(op)
            if not touched: crud[t]['*'].add(op)
        # host-variable pairing
        if verb == 'INSERT':
            mm = re.search(r'INSERT\s+INTO\s+(\S+)\s*\(([^)]*)\)\s*VALUES\s*\((.*)\)', s)
            if mm and mm.group(1) in tables:
                cols = [c.strip() for c in mm.group(2).split(',')]
                vals = [v.strip() for v in re.split(r',(?![^(]*\))', mm.group(3))]
                for c, v in zip(cols, vals):
                    for hv in re.findall(r':([A-Z0-9-]+)', v): hv_col.append((hv, mm.group(1), c, 'C'))
        if verb == 'UPDATE':
            mm = re.search(r'UPDATE\s+(\S+)\s+SET\s+(.*?)(\bWHERE\b|$)', s)
            if mm and mm.group(1) in tables:
                for c, v in re.findall(r'([A-Z0-9_]+)\s*=\s*([^,]+?)(?=,\s*[A-Z0-9_]+\s*=|$)', mm.group(2)):
                    for hv in re.findall(r':([A-Z0-9-]+)', v): hv_col.append((hv, mm.group(1), c, 'U'))
        if verb in ('SELECT', 'DECLARE', 'FETCH'):
            mm = re.search(r'SELECT\s+(.*?)\s+INTO\s+(.*?)\s+FROM\s', s)
            if mm and tabs:
                sel = [x.strip() for x in re.split(r',(?![^(]*\))', mm.group(1))]
                into = [x.strip() for x in mm.group(2).split(',')]
                for c, v in zip(sel, into):
                    cn = c.split('.')[-1].strip()
                    tt = aliases.get(c.split('.')[0]) if '.' in c else None
                    for t in ([tt] if tt else tabs):
                        if t in colsets and cn in colsets[t]:
                            for hv in re.findall(r':([A-Z0-9-]+)', v): hv_col.append((hv, t, cn, 'R')); break
        # WHERE predicates with host vars -> key reads
        for c, hv in re.findall(r'\b([A-Z0-9_]+)\s*(?:=|<|>|<=|>=)\s*:([A-Z0-9-]+)', s):
            for t in tabs:
                if c in colsets.get(t, ()): hv_col.append((hv, t, c, 'K'))
    p['sql'] = dict(collections.Counter(x['verb'] for x in sql))
    p['crud'] = {t: {c: ''.join(sorted(o)) for c, o in cols.items()} for t, cols in crud.items()}
    tcrud = {}
    for t, cols in crud.items():
        tcrud[t] = ''.join(sorted(set(''.join(''.join(o) for o in cols.values()))))
    p['tables'] = tcrud
    p['hostvars'] = hv_col
    # data flow: MOVE / COMPUTE edges
    # statements: a new statement starts when a line begins with a verb
    stmts, curst = [], []
    verb_re = re.compile(r'^\s*' + VERBS + r'\b')
    for _, t in proc[1:]:
        tu = t.upper().strip()
        if not tu: continue
        if verb_re.match(tu) or re.match(r'^[A-Z0-9][A-Z0-9-]*\s*\.$', tu):
            if curst: stmts.append(' '.join(curst))
            curst = [tu]
        else:
            curst.append(tu)
    if curst: stmts.append(' '.join(curst))
    flow = []
    skip = {'SPACES', 'SPACE', 'ZERO', 'ZEROS', 'ZEROES', 'LOW-VALUES', 'HIGH-VALUES', 'LOW-VALUE', 'HIGH-VALUE', 'TO', 'OF', 'IN'}
    for st in stmts:
        st = re.sub(r"'[^']*'", "'LIT'", st).rstrip('.').strip()
        m = re.match(r'^MOVE\s+(?:CORRESPONDING\s+|CORR\s+)?(.+?)\s+TO\s+(.+)$', st)
        if m:
            s0 = m.group(1)
            if s0.startswith("'") or re.match(r'^[\-+]?\d', s0) or s0 in skip: continue
            sname = re.sub(r'\s*\(.*', '', s0).split(' ')[0]
            for d in re.findall(r'([A-Z][A-Z0-9-]*)(?:\s*\([^)]*\))?', re.sub(r'\b(OF|IN)\s+[A-Z0-9-]+', '', m.group(2))):
                if d not in skip: flow.append((sname, d))
            continue
        m = re.match(r'^UNSTRING\s+([A-Z0-9-]+).*?\bINTO\s+(.*?)(?:\s+(?:WITH|TALLYING|ON|NOT|END-UNSTRING)\b|$)', st)
        if m:
            for d in re.findall(r'([A-Z][A-Z0-9-]*)', m.group(2)):
                if d not in skip and d not in ('DELIMITER', 'COUNT', 'IN'): flow.append((m.group(1), d))
            continue
        m = re.match(r'^STRING\s+(.*?)\s+INTO\s+([A-Z0-9-]+)', st)
        if m:
            srcs = m.group(1)
            while re.search(r'\([^()]*\)', srcs): srcs = re.sub(r'\([^()]*\)', '', srcs)
            for s2 in re.findall(r'\b([A-Z][A-Z0-9-]*[A-Z0-9])\b', srcs):
                if s2 not in ('DELIMITED', 'BY', 'SIZE', 'SPACE', 'SPACES', 'LIT'): flow.append((s2, m.group(2)))
            continue
        m = re.match(r'^COMPUTE\s+([A-Z0-9-]+)(?:\s*\([^)]*\))?(?:\s+ROUNDED)?\s*=\s*(.*)$', st)
        if m:
            for s2 in set(re.findall(r'\b([A-Z][A-Z0-9-]*[A-Z0-9])\b', m.group(2))):
                if s2 not in ('ROUNDED', 'FUNCTION', 'END-COMPUTE') and not s2.startswith('ON-'):
                    flow.append((s2, m.group(1)))
    # the same copybook copied under several prefixes (screen state saved in
    # the COMMAREA and restored into working storage) -> alias edges
    tags = collections.defaultdict(set)
    for mm in re.finditer(r'COPY\s+([A-Z0-9]+)\s+REPLACING\s+LEADING\s+==([A-Z0-9]+)-==\s+BY\s+==([A-Z0-9]+)-==', up):
        tags[mm.group(1)] |= {mm.group(2), mm.group(3)}
    for mm in re.finditer(r'COPY\s+([A-Z0-9]+)\s*\.', up):
        cp = os.path.join(ROOT, app, 'cpy', mm.group(1) + '.cpy')
        if os.path.exists(cp) and not mm.group(1).startswith(('BILD', 'CLMD', 'GEND')):
            first = re.search(r'^\s+\d\d\s+([A-Z0-9]+)-', '\n'.join(l[7:72] for l in rd(cp) if not is_comment(l)), re.M)
            if first: tags[mm.group(1)].add(first.group(1))
    names = {n for e in flow for n in e}
    alias = []
    for mem, ts in tags.items():
        if len(ts) < 2 or mem.endswith('CAHDR') or re.match(r'^(BIL|CLM)C', mem): continue
        for n in names:
            pre, _, rest = n.partition('-')
            if pre in ts and rest:
                for o in ts - {pre}:
                    alias.append((n, o + '-' + rest))
    p['flow'] = sorted(set(flow))
    p['alias'] = sorted(set(alias))
    # calls with request ids (reuse xref approach)
    values = dict(re.findall(r'\b([A-Z0-9-]+)\s+PIC\s+X\(8\)\s+VALUE\s+\'([A-Z0-9]{3,8})\s*\'', up))
    calls = collections.defaultdict(set)
    tag_target = {}
    for m in re.finditer(r'COPY\s+[A-Z]{3}CAHDR\s+REPLACING\s+LEADING\s+==CA-==\s+BY\s+==([A-Z0-9]+)-==', up):
        grp = re.findall(r'01\s+([A-Z0-9-]+)\s*\.', up[:m.start()])
        if not grp: continue
        g = grp[-1]
        c = re.search(r'CALL\s+([A-Z0-9-]+|\'[A-Z0-9]+\')\s+USING\s+' + re.escape(g) + r'\b', up)
        l2 = re.search(r'PROGRAM\s*\(\s*([A-Z0-9-]+|\'[A-Z0-9]+\')\s*\)\s*COMMAREA\s*\(\s*' + re.escape(g) + r'\s*\)', up)
        h = c or l2
        if h:
            t = h.group(1).strip("'"); tag_target[m.group(1)] = values.get(t, t)
    sends = collections.defaultdict(set)
    for m in re.finditer(r"MOVE\s+'([A-Z0-9]{5,8})\s*'\s+TO\s+([A-Z0-9]+)-REQUEST-ID", up):
        sends[m.group(2)].add(m.group(1))
    for m in re.finditer(r'\bCALL\s+(\'([A-Z0-9]+)\'|([A-Z0-9-]+))', proc_text):
        tgt = m.group(2) or values.get(m.group(3))
        if tgt: calls[tgt].add('CALL')
    for m in re.finditer(r'EXEC\s+CICS\s+(LINK|XCTL)\s+PROGRAM\s*\(\s*(\'([A-Z0-9]+)\'|([A-Z0-9-]+))', up):
        tgt = m.group(3) or values.get(m.group(4))
        if tgt: calls[tgt].add(m.group(1))
    reqs = collections.defaultdict(set)
    for tag, rs in sends.items():
        if tag in tag_target: reqs[tag_target[tag]] |= rs
    p['calls'] = [{'target': t, 'how': sorted(h), 'requests': sorted(reqs.get(t, []))} for t, h in sorted(calls.items())]
    p['tags'] = tag_target
    ev = re.search(r'EVALUATE\s+CA-REQUEST-ID(.*?)END-EVALUATE', up, re.S)
    p['serves'] = sorted(set(re.findall(r"WHEN\s+'([A-Z0-9]{5,8})\s*'", ev.group(1)))) if ev else []
    p['cics'] = dict(collections.Counter(re.findall(r'EXEC\s+CICS\s+([A-Z]+(?:\s+MAP)?)', up)))
    p['copies'] = sorted(set(re.findall(r'\bCOPY\s+([A-Z0-9]+)', up)) | set(re.findall(r'INCLUDE\s+([A-Z0-9]+)', up)))
    p['files'] = sorted(set(re.findall(r'\bSELECT\s+([A-Z0-9-]+)\s+ASSIGN\s+TO\s+([A-Z0-9-]+)', up)))
    p['files'] = [{'file': a, 'dd': b} for a, b in p['files']]
    p['maps'] = sorted(set(re.findall(r"MAP\s*\(\s*'([A-Z0-9]+)'", up)))
    p['transid'] = sorted(set(re.findall(r"TRANSID\s*\(\s*'([A-Z0-9]{4})'", up)))
    # business rules: header text + implementation lines
    rules = {}
    hdr = re.findall(r'\*\s+(?:RULES\s*:)?\s*(BR-[A-Z]{3}-[A-Z0-9]{3}-\d{3})\s+(.*?)\*?\s*$((?:\n\s{6}\*\s{20,}.*?$)*)', src, re.M)
    for rid, first, more in hdr:
        cont = ' '.join(x.strip().rstrip('*').strip() for x in re.findall(r'\*\s{20,}(.*)', more))
        text = (first.strip().rstrip('*').strip() + ' ' + cont).strip()
        if rid not in rules or len(text) > len(rules[rid]['text']):
            rules[rid] = {'id': rid, 'text': re.sub(r'\s+', ' ', text)[:240], 'lines': []}
    for i, l in enumerate(raw, 1):
        for rid in re.findall(r'BR-[A-Z]{3}-[A-Z0-9]{3}-\d{3}', l):
            rules.setdefault(rid, {'id': rid, 'text': '', 'lines': []})['lines'].append(i)
    for r in rules.values():
        if not r['text']:
            r['text'] = ''
    p['rules'] = sorted(rules.values(), key=lambda r: r['id'])
    hb = re.search(r'\* FUNCTION\s*:\s*(.*?)\n\s*\* (?:REQUESTS|CALLED|CALLS|TABLES|INPUT|RULES|STYLE)', src, re.S)
    p['purpose'] = re.sub(r'\s+', ' ', re.sub(r'\n\s*\*', ' ', hb.group(1)).replace('*', '')).strip() if hb else ''
    return p

# ---------------------------------------------------------------- JCL
def parse_jcl():
    jobs = []
    for app in APPS:
        for f in sorted(glob.glob(os.path.join(ROOT, app, 'jcl', '*.jcl'))):
            steps, cur = [], None
            for l in rd(f):
                if l.startswith('//*'): continue
                m = re.match(r'^//([A-Z0-9#@$]+)\s+EXEC\s+(PGM=)?([A-Z0-9#@$]+)', l)
                if m:
                    cur = {'step': m.group(1), 'exec': m.group(3), 'program': m.group(3) if m.group(2) else None, 'dds': [],
                           'cond': (re.search(r'COND=(\S+)', l) or [None, None])[1] if re.search(r'COND=', l) else None}
                    steps.append(cur); continue
                m = re.search(r'RUN\s+PROGRAM\s*\(\s*([A-Z0-9]+)\s*\)', l)
                if m and cur: cur['program'] = m.group(1)
                m = re.match(r'^//(?:[A-Z0-9]+\.)?([A-Z0-9#@$]+)\s+DD\s', l)
                if m and cur: cur['dds'].append(m.group(1))
            jobs.append({'app': app, 'job': os.path.basename(f)[:-4], 'path': os.path.relpath(f, ROOT), 'steps': steps})
    return jobs

# ---------------------------------------------------------------- lineage
def body_prefix_index(contracts):
    idx = {}
    for app, fields in contracts.items():
        bp = {}
        for name, f in fields.items():
            m = re.match(r'CA-([A-Z0-9]{2})-', name)
            if m: bp.setdefault(m.group(1), f['member'])
        idx[app] = bp
    return idx

def normalise(name, app, contracts, bpi):
    """caller-side copy of a contract field -> canonical CA- name"""
    if name.startswith('CA-') and name in contracts.get(app, {}): return name
    m = re.match(r'^[A-Z0-9]+-([A-Z0-9]{2}-.*)$', name)
    if m and ('CA-' + m.group(1)) in contracts.get(app, {}): return 'CA-' + m.group(1)
    return None

def build_lineage(tables, programs, contracts, bms):
    bpi = body_prefix_index(contracts)
    # DCLGEN field -> column
    fld2col = {}
    for t in tables.values():
        if not t['prefix']: continue
        for c in t['cols']:
            base = f"{t['prefix']}-{c['name'].replace('_', '-')}"[:30]
            fld2col[base] = (t['name'], c['name'])
            if c['type'] == 'VARCHAR':
                fld2col[base[:25] + '-TEXT'] = (t['name'], c['name'])
    col = {}
    for t in tables.values():
        for c in t['cols']:
            col[(t['name'], c['name'])] = {'writers': set(), 'readers': set(), 'keys': set(), 'ca': set(), 'near': set(), 'screen': set(), 'files': set()}
    ca_screen = collections.defaultdict(set)     # canonical CA field -> screen fields (via front-end programs)
    ca_path = {}
    ca_ca = collections.defaultdict(set)         # CA field <-> CA field links (service-to-service moves)
    for p in programs:
        app = p['app']
        adj = collections.defaultdict(set)
        for s, d in p['flow'] + p.get('alias', []):
            adj[s].add(d); adj[d].add(s)
        # screen fields in this program
        scr = bms.get(app, {})
        def is_scr(z):
            mm = re.match(r'^([A-Z0-9]+?)[IO]$', z)
            return bool(mm and mm.group(1) in scr)
        fwd, rev = collections.defaultdict(set), collections.defaultdict(set)
        for a, b in p['flow']:
            if a != b: fwd[a].add(b); rev[b].add(a)
        fin, rin = {k: set(v) for k, v in rev.items()}, {k: set(v) for k, v in fwd.items()}   # real in-degree per direction
        for a, b in p.get('alias', []):
            fwd[a].add(b); fwd[b].add(a); rev[a].add(b); rev[b].add(a)
        # directed walk: input fields forward along MOVE direction, output
        # fields backward; stop at merge points fed by more than one source
        # (shared scratch) and never pass through another screen field
        for v in set(fwd) | set(rev):
            m = re.match(r'^([A-Z0-9]+?)([IO])$', v)
            if not (m and m.group(1) in scr): continue
            g, other = (fwd, fin) if m.group(2) == 'I' else (rev, rin)
            seen, frontier = {v}, {v}
            for depth in range(9):
                nxt = set()
                for x in frontier:
                    for y in g.get(x, ()):
                        if y in seen: continue
                        seen.add(y)
                        n = normalise(y, app, contracts, bpi)
                        if n:
                            ca_screen[n].add(m.group(1)); ca_path.setdefault((n, m.group(1)), (p['program'], x)); continue
                        if is_scr(y): continue
                        if len(other.get(y, set()) - {y}) > 1: continue
                        nxt.add(y)
                frontier = nxt
        # CA to CA within a program (service passing its input to a callee)
        for s, d in p['flow']:
            a, b = normalise(s, app, contracts, bpi), normalise(d, app, contracts, bpi)
            if a and b and a != b: ca_ca[a].add(b); ca_ca[b].add(a)
        # host vars -> columns; then neighbourhood to CA fields
        for hv, t, c, op in p['hostvars']:
            key = (t, c)
            if key not in col: continue
            {'C': col[key]['writers'], 'U': col[key]['writers'], 'R': col[key]['readers'], 'K': col[key]['keys']}[op].add(p['program'])
            # hop 0/1 = direct (host variable itself or one MOVE away);
            # hop 2 = near (two MOVEs away, shown as derived evidence)
            n0 = normalise(hv, app, contracts, bpi)
            if n0: col[key]['ca'].add(n0)
            hop1 = adj.get(hv, set())
            for x in hop1:
                n = normalise(x, app, contracts, bpi)
                if n: col[key]['ca'].add(n)
            for x in set().union(*[adj.get(y, set()) for y in hop1]) if hop1 else set():
                n = normalise(x, app, contracts, bpi)
                if n: col[key]['near'].add(n)
        # DCLGEN fields moved directly
        for s, d in p['flow']:
            for a, b in ((s, d), (d, s)):
                if a in fld2col:
                    key = fld2col[a]
                    n = normalise(b, app, contracts, bpi)
                    if n: col[key]['ca'].add(n)
        for t, cols in p['crud'].items():
            for c, ops in cols.items():
                if (t, c) in col:
                    if set(ops) & set('CUD'): col[(t, c)]['writers'].add(p['program'])
                    if 'R' in ops: col[(t, c)]['readers'].add(p['program'])
    UTIL = {'billing': {'BILCSEQ','BILCDTE','BILCERR','BILCAUD','BILCGLP','BILCMNY','BILCEVT','BILCCOD','BILCAUT','BILCAHDR'},
            'claims': {'CLMCSEQ','CLMCDTE','CLMCERR','CLMCEVT','CLMCCOD','CLMCMNY','CLMCHIS','CLMCAUC','CLMCAUT','CLMCAHDR'}}
    def is_util(n, app):
        f = contracts.get(app, {}).get(n)
        return (not f) or f['member'] in UTIL[app]
    for (t, c), v in col.items():
        app = 'billing' if t.startswith('BILDB') else ('claims' if t.startswith('CLMDB') else None)
        apps = [app] if app else ['billing', 'claims']
        util = {n for n in v['ca'] if all(is_util(n, a) for a in apps)}
        v['util'] = util; v['ca'] = v['ca'] - util
        v['near'] = {n for n in v['near'] - v['ca'] if not all(is_util(n, a) for a in apps)}
    out = {}
    for (t, c), v in col.items():
        scr = set()
        for n in v['ca']:
            scr |= ca_screen.get(n, set())
        if not v['ca']:
            for n in v['near']: scr |= ca_screen.get(n, set())
        out[f'{t}.{c}'] = {k: sorted(x) for k, x in v.items() if k != 'screen'}
        out[f'{t}.{c}']['screen'] = sorted(scr)
    global CA_PATH
    CA_PATH = ca_path
    return out, {k: sorted(v) for k, v in ca_screen.items()}

# ---------------------------------------------------------------- main
def main(out):
    tables, dcl = parse_ddl()
    contracts = parse_contracts()
    bms = parse_bms()
    programs = []
    for app in APPS:
        for f in sorted(glob.glob(os.path.join(ROOT, app, 'src', '*', '*.cbl'))):
            programs.append(parse_program(f, app, tables, dcl, contracts, bms))
    lineage, ca_screen = build_lineage(tables, programs, contracts, bms)
    jobs = parse_jcl()
    evidence = []
    for dp, _, fs in os.walk(ROOT):
        if '.git' in dp or '__pycache__' in dp: continue
        for f in sorted(fs):
            pth = os.path.join(dp, f)
            evidence.append({'path': os.path.relpath(pth, ROOT), 'sha256': hashlib.sha256(open(pth, 'rb').read()).hexdigest()})
    evidence.sort(key=lambda e: e['path'])
    root = hashlib.sha256(''.join(e['sha256'] for e in evidence).encode()).hexdigest()
    for p in programs:
        p.pop('flow', None); p.pop('hostvars', None); p.pop('alias', None)
    doc = {'generated': '2026-10-27', 'tables': tables, 'contracts': contracts, 'screens': bms,
           'programs': programs, 'lineage': lineage, 'ca_screen': ca_screen, 'jobs': jobs,
           'evidence': evidence, 'evidence_root': root}
    json.dump(doc, open(out, 'w'), separators=(',', ':'), default=list)
    by = collections.Counter(p['app'] for p in programs)
    linked = sum(1 for v in lineage.values() if v['ca'])
    scr = sum(1 for v in lineage.values() if v['screen'])
    print(f"programs {dict(by)}; tables {len(tables)}; columns {len(lineage)}; "
          f"columns with COMMAREA lineage {linked}; with screen lineage {scr}; "
          f"rules {sum(len(p['rules']) for p in programs)}; jobs {len(jobs)}; evidence files {len(evidence)}; root {root[:16]}")

if __name__ == '__main__':
    main(sys.argv[1])
