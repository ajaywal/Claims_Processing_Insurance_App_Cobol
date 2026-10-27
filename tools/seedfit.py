#!/usr/bin/env python3
"""seedfit.py - check every seed INSERT against the DDL: known table and
columns, CHAR/VARCHAR widths, DECIMAL precision, NOT NULL columns supplied."""
import re, os, sys, glob
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
bad = 0
for app, pfx in (('billing','BIL'),('claims','CLM')):
    cols = {}
    for f in glob.glob(os.path.join(ROOT, app, 'ddl', pfx+'DDL*.sql')):
        cur = None
        for l in open(f):
            m = re.match(r'\s*CREATE TABLE\s+(\S+)', l)
            if m: cur = m.group(1).upper(); cols[cur] = {}; continue
            if cur and re.match(r'\s*(PRIMARY|FOREIGN)', l): cur = None; continue
            m = re.match(r'\s*\(?\s*([A-Z0-9_]+)\s+([A-Z]+)(\((\d+)(,(\d+))?\))?(.*)', l)
            if cur and m and m.group(2) in ('CHAR','VARCHAR','INTEGER','SMALLINT','DECIMAL','DATE','TIMESTAMP','TIME'):
                cols[cur][m.group(1)] = (m.group(2), int(m.group(4) or 0), int(m.group(6) or 0),
                                         'NOT NULL' in m.group(7) and 'DEFAULT' not in m.group(7))
    seed = open(os.path.join(ROOT, app, 'ddl', pfx+'SEED.sql')).read()
    n = 0
    for m in re.finditer(r'INSERT INTO\s+(\S+)\s*\(([^)]*)\)\s*VALUES\s*\((.*?)\);', seed, re.S):
        n += 1
        t = m.group(1).upper(); cl = [c.strip().upper() for c in m.group(2).split(',')]
        vals = [v.strip() for v in re.findall(r"\s*('(?:[^']|'')*'|[^,]+)", m.group(3))]
        if t not in cols: print(app, 'unknown table', t); bad += 1; continue
        if len(cl) != len(vals): print(app, t, 'column/value count mismatch', len(cl), len(vals)); bad += 1; continue
        for c, v in zip(cl, vals):
            if c not in cols[t]: print(app, t, 'unknown column', c); bad += 1; continue
            typ, w, s, _ = cols[t][c]
            if v.startswith("'") and typ in ('CHAR','VARCHAR'):
                txt = v[1:-1].replace("''", "'")
                if len(txt.rstrip()) > w: print(app, t, c, f'{typ}({w}) too short for', repr(txt)); bad += 1
            if typ == 'DECIMAL' and re.match(r'-?\d+(\.\d+)?$', v):
                ip = v.lstrip('-').split('.')[0]
                if len(ip) > w - s: print(app, t, c, 'decimal overflow', v); bad += 1
        for c, (typ, w, s, req) in cols[t].items():
            if req and c not in cl: print(app, t, 'missing NOT NULL', c); bad += 1
    print(f'{app}: {n} inserts checked')
print(f'{bad} problems'); sys.exit(bad)
