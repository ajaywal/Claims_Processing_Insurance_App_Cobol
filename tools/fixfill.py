#!/usr/bin/env python3
"""fixfill.py - run calen.py and correct the trailing FILLER of each wrong body."""
import re, subprocess, sys, os
app, pfx = sys.argv[1], sys.argv[2]
out = subprocess.run([sys.executable, os.path.join(os.path.dirname(__file__), 'calen.py'), app, pfx], capture_output=True, text=True).stdout
for line in out.splitlines():
    m = re.match(r'(\S+)\s+\d+\s+WRONG .*by ([+-]\d+)', line)
    if not m: continue
    f = os.path.join(app, 'cpy', m.group(1) + '.cpy'); d = int(m.group(2))
    txt = open(f).read()
    fills = list(re.finditer(r'FILLER(\s+)PIC X\((\d+)\)\.', txt))
    last = fills[-1]
    txt = txt[:last.start()] + f'FILLER{last.group(1)}PIC X({int(last.group(2)) + d}).' + txt[last.end():]
    open(f, 'w').write(txt); print('fixed', m.group(1), d)
