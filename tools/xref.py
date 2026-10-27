#!/usr/bin/env python3
"""xref.py - static cross-reference and integration check for the suite.

For every program it finds:
  * program id, style (A dual-mode / B CICS / C batch), code lines
  * services it calls: CALL <ws-field> / CALL 'LIT' / EXEC CICS LINK|XCTL
    PROGRAM(...) where the ws-field's VALUE gives the target
  * request ids it sends: MOVE 'XXXXXXX' TO <tag>-REQUEST-ID
  * request ids it serves: WHEN 'XXXXXXX' inside EVALUATE CA-REQUEST-ID
  * tables touched by SQL verb (C R U D), business-rule tags
Then reports integration breaks: calls to programs that do not exist and
request ids sent to a service that does not handle them.
Writes docs/xref.json for downstream tooling (lineage, knowledge graph).
Usage: python3 tools/xref.py
"""
import os, re, json, collections
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def code_lines(path):
    for l in open(path, encoding='latin-1'):
        if len(l) > 6 and l[6] in '*/':
            continue
        yield l[7:72] if len(l) > 7 else ''

def scan(path):
    lines = list(code_lines(path))
    text = '\n'.join(lines)
    up = text.upper()
    pid = re.search(r'PROGRAM-ID\.\s*([A-Z0-9]+)', up)
    pid = pid.group(1) if pid else os.path.basename(path)[:-4].upper()
    style = 'B' if 'EXEC CICS' in up else ('C' if '/BATCH/' in path.replace('\\', '/').upper() else 'A')
    values = dict(re.findall(r'\b([A-Z0-9-]+)\s+PIC\s+X\(8\)\s+VALUE\s+\'([A-Z0-9]{3,8})\s*\'', up))
    calls = set()
    for m in re.finditer(r'\bCALL\s+(\'([A-Z0-9]+)\'|([A-Z0-9-]+))', up):
        tgt = m.group(2) or values.get(m.group(3))
        if tgt: calls.add(tgt)
    for m in re.finditer(r'EXEC\s+CICS\s+(LINK|XCTL)\s+PROGRAM\s*\(\s*(\'([A-Z0-9]+)\'|([A-Z0-9-]+))', up):
        tgt = m.group(3) or values.get(m.group(4))
        if tgt: calls.add(tgt)
    sends = collections.defaultdict(set)
    for m in re.finditer(r"MOVE\s+'([A-Z0-9]{5,8})\s*'\s+TO\s+([A-Z0-9]+)-REQUEST-ID", up):
        sends[m.group(2)].add(m.group(1))
    # map caller tag -> target program by looking for CALL/LINK with COMMAREA/USING of WS-...-CA whose header tag matches
    tag_target = {}
    for m in re.finditer(r'COPY\s+[A-Z]{3}CAHDR\s+REPLACING\s+LEADING\s+==CA-==\s+BY\s+==([A-Z0-9]+)-==', up):
        tag = m.group(1)
        # find the 01 group just before this COPY
        before = up[:m.start()]
        grp = re.findall(r'01\s+([A-Z0-9-]+)\s*\.', before)
        if not grp: continue
        g = grp[-1]
        c = re.search(r'CALL\s+([A-Z0-9-]+|\'[A-Z0-9]+\')\s+USING\s+' + re.escape(g) + r'\b', up)
        l = re.search(r'PROGRAM\s*\(\s*([A-Z0-9-]+|\'[A-Z0-9]+\')\s*\)\s*COMMAREA\s*\(\s*' + re.escape(g) + r'\s*\)', up)
        hit = c or l
        if hit:
            t = hit.group(1).strip("'")
            tag_target[tag] = values.get(t, t)
    serves = set()
    ev = re.search(r'EVALUATE\s+CA-REQUEST-ID(.*?)END-EVALUATE', up, re.S)
    if ev:
        serves = set(re.findall(r"WHEN\s+'([A-Z0-9]{5,8})\s*'", ev.group(1)))
    tables = collections.defaultdict(set)
    for m in re.finditer(r'EXEC\s+SQL(.*?)END-EXEC', up, re.S):
        s = ' '.join(m.group(1).split())
        verb = s.split(' ')[0] if s else ''
        for t in re.findall(r'\b((?:BILDB|CLMDB|GENAPP)\.[A-Z_]+)', s):
            if verb in ('SELECT', 'DECLARE', 'FETCH', 'OPEN'): tables[t].add('R')
            elif verb == 'INSERT': tables[t].add('C')
            elif verb == 'UPDATE': tables[t].add('U')
            elif verb == 'DELETE': tables[t].add('D')
        if verb == 'DECLARE' and ' TABLE' in s and 'CURSOR' not in s:
            pass
    rules = sorted(set(re.findall(r'BR-[A-Z]{3}-[A-Z0-9]{3}-\d{3}', open(path, encoding='latin-1').read())))
    return {'program': pid, 'path': os.path.relpath(path, ROOT), 'style': style,
            'code_lines': sum(1 for l in lines if l.strip()), 'calls': sorted(calls),
            'sends': {k: sorted(v) for k, v in sends.items()}, 'tag_target': tag_target,
            'serves': sorted(serves), 'tables': {k: ''.join(sorted(v)) for k, v in tables.items()},
            'rules': rules}

def main():
    progs = []
    for app in ('billing', 'claims'):
        for dp, _, fs in os.walk(os.path.join(ROOT, app, 'src')):
            for f in fs:
                if f.lower().endswith('.cbl'):
                    p = scan(os.path.join(dp, f)); p['app'] = app; progs.append(p)
    by = {p['program']: p for p in progs}
    breaks = []
    for p in progs:
        for c in p['calls']:
            if c not in by and not c.startswith('DFH'):
                breaks.append(f"{p['program']}: calls missing program {c}")
        for tag, reqs in p['sends'].items():
            tgt = p['tag_target'].get(tag)
            if not tgt or tgt not in by: continue
            missing = [r for r in reqs if r not in by[tgt]['serves']]
            if missing and by[tgt]['serves']:
                breaks.append(f"{p['program']}: sends {','.join(missing)} to {tgt}, which handles {','.join(by[tgt]['serves'])}")
    json.dump({'programs': progs, 'breaks': breaks}, open(os.path.join(ROOT, 'docs', 'xref.json'), 'w'), indent=1)
    for app in ('billing', 'claims'):
        ps = [p for p in progs if p['app'] == app]
        print(f"{app}: {len(ps)} programs, {sum(p['code_lines'] for p in ps)} code lines, "
              f"{sum(len(p['rules']) for p in ps)} business rules, "
              f"styles {dict(collections.Counter(p['style'] for p in ps))}")
    print(f"\n{len(breaks)} integration breaks")
    for b in breaks: print('  ' + b)

if __name__ == '__main__':
    main()
