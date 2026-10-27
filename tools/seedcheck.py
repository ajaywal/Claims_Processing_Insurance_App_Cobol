#!/usr/bin/env python3
"""seedcheck.py - cross-check literals used by programs against seed data:
sequence names (SEQNEXT), code tables (CODVAL/CODCHD), plan codes, users."""
import re, os, sys, glob
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
def code(p):
    return '\n'.join(l[7:72] for l in open(p, encoding='latin-1') if len(l) > 6 and l[6] not in '*/')
for app, pfx in (('billing', 'BIL'), ('claims', 'CLM')):
    seed = open(os.path.join(ROOT, app, 'ddl', pfx + 'SEED.sql')).read().upper()
    seqs = set(m.strip() for m in re.findall(pfx + r"_SEQUENCE.*?VALUES\s*\(\s*'([A-Z0-9 ]+)'", seed, re.S))
    tabs = set(m.strip() for m in re.findall(pfx + r"_CODE_TABLE.*?VALUES\s*\(\s*'([A-Z0-9 ]+)'", seed, re.S))
    used_seq, used_tab = {}, {}
    for p in glob.glob(os.path.join(ROOT, app, 'src', '*', '*.cbl')):
        t = code(p).upper(); n = os.path.basename(p)[:-4]
        for m in re.findall(r"MOVE\s+'([A-Z0-9]{2,8})\s*'\s+TO\s+[A-Z0-9]+-SQ-NAME", t): used_seq.setdefault(m, set()).add(n)
        for m in re.findall(r"MOVE\s+'([A-Z0-9]{3,8})\s*'\s+TO\s+[A-Z0-9]+-CD-TABLE", t): used_tab.setdefault(m, set()).add(n)
        for m in re.findall(r"'([A-Z0-9]{3,8})\s*'\s+TO\s+(?:SEQ-SEQ-NAME|WS-SEQ-NAME)", t): used_seq.setdefault(m, set()).add(n)
    print(f'== {app}: {len(seqs)} seeded sequences, {len(tabs)} seeded code tables')
    for s in sorted(set(used_seq) - seqs): print(f'  MISSING sequence {s:<8} used by {",".join(sorted(used_seq[s]))}')
    for s in sorted(set(used_tab) - tabs): print(f'  MISSING code table {s:<8} used by {",".join(sorted(used_tab[s]))}')
    long = re.findall(r"'([A-Z0-9_]{9,})'", seed)
    print('  9+ char literals in seed (check column widths):', sorted(set(long))[:40])
