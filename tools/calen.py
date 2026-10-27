#!/usr/bin/env python3
"""calen.py - compile and run a GnuCOBOL probe that prints the byte length of
every COMMAREA copybook (<APP>CAHDR must be 160, every body 3840).
Usage: python3 tools/calen.py <app-dir> <prefix>   e.g. billing BILC
"""
import os, sys, subprocess, tempfile
app, pfx = os.path.abspath(sys.argv[1]), sys.argv[2]
cpy = os.path.join(app, 'cpy')
mems = sorted(f[:-4] for f in os.listdir(cpy) if f.startswith(pfx) and f.endswith('.cpy'))
ws, pd = [], []
for i, m in enumerate(mems):
    ws.append(f"       01  T{i:03d}.")
    ws.append(f"           COPY {m} REPLACING LEADING ==CA-== BY ==T{i:03d}-==.")
    pd.append(f"           MOVE LENGTH OF T{i:03d} TO WS-LEN")
    pd.append(f"           DISPLAY '{m:<9}' WS-LEN")
src = ["       IDENTIFICATION DIVISION.", "       PROGRAM-ID. CALEN.", "       DATA DIVISION.",
       "       WORKING-STORAGE SECTION.", "       01  WS-LEN PIC Z(5)9."] + ws + \
      ["       PROCEDURE DIVISION."] + pd + ["           GOBACK."]
d = tempfile.mkdtemp()
p = os.path.join(d, 'CALEN.cbl'); open(p, 'w').write('\n'.join(src) + '\n')
r = subprocess.run(['cobc', '-x', '-std=ibm', '-I', cpy, '-o', os.path.join(d, 'calen'), p], capture_output=True, text=True)
if r.returncode: print(r.stdout, r.stderr); sys.exit(1)
out = subprocess.run([os.path.join(d, 'calen')], capture_output=True, text=True).stdout
bad = 0
for line in out.splitlines():
    name, n = line[:9].strip(), int(line[9:])
    want = 160 if name.endswith('CAHDR') else 3840
    flag = 'ok' if n == want else f'WRONG (want {want}, adjust FILLER by {want - n:+d})'
    bad += n != want
    print(f'{name:<9} {n:>5}  {flag}')
sys.exit(bad)
