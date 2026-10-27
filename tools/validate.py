#!/usr/bin/env python3
"""validate.py - GnuCOBOL syntax validation for CICS/Db2 COBOL source.

GnuCOBOL has no CICS or Db2 precompiler, so this script stands in for them:
  * EXEC SQL INCLUDE <m> END-EXEC  -> COPY <m>.
  * EXEC SQL / EXEC CICS blocks in the DATA DIVISION -> commented out
  * EXEC SQL / EXEC CICS blocks in the PROCEDURE DIVISION -> CONTINUE
    (keeping any terminating period, so sentence structure is preserved)
  * DFHRESP(x) / DFHVALUE(x) -> numeric literals
  * CICS programs get COPY DFHEIBLK in the LINKAGE SECTION, as the CICS
    translator would insert the EIB
Copybooks are preprocessed the same way into a scratch directory.
Line numbers are preserved, so cobc diagnostics point at the real source.

Usage: python3 tools/validate.py <app-dir> [program.cbl ...]
       (no programs = every .cbl under <app-dir>/src)
Exit code = number of failing programs.
"""
import os, re, subprocess, sys, tempfile, shutil

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RESP = {'NORMAL':0,'ERROR':1,'RDATT':2,'WRBRK':3,'EOF':4,'EODS':5,'EOC':6,'INBFMH':7,
        'ENDINPT':8,'NONVAL':9,'NOSTART':10,'TERMIDERR':11,'FILENOTFOUND':12,'NOTFND':13,
        'DUPREC':14,'DUPKEY':15,'INVREQ':16,'IOERR':17,'NOSPACE':18,'NOTOPEN':19,
        'ENDFILE':20,'ILLOGIC':21,'LENGERR':22,'QZERO':23,'SIGNAL':24,'QBUSY':25,
        'ITEMERR':26,'PGMIDERR':27,'TRANSIDERR':28,'ENDDATA':29,'EXPIRED':31,'MAPFAIL':36,
        'INVMPSZ':38,'QIDERR':44,'SYSIDERR':53,'NOTAUTH':70,'DISABLED':84,'CHANNELERR':122,
        'CONTAINERERR':110,'TOKENERR':112}
EXEC_START = re.compile(r'\bEXEC\s+(SQL|CICS|DLI)\b', re.I)

def is_comment(line):
    return len(line) > 6 and line[6] in '*/'

def comment(line):
    line = line.rstrip('\n')
    if len(line) < 7:
        line = line.ljust(7)
    return line[:6] + '*' + line[7:] + '\n'

def preprocess(lines, is_copybook=False):
    out, i, division = [], 0, 'DATA' if is_copybook else 'ID'
    has_cics = any(EXEC_START.search(l) and 'CICS' in l.upper() for l in lines if not is_comment(l))
    linkage_seen = False
    while i < len(lines):
        line = lines[i]
        if is_comment(line):
            out.append(line); i += 1; continue
        up = line.upper()
        if re.search(r'\bDATA\s+DIVISION\b', up): division = 'DATA'
        if re.search(r'\bPROCEDURE\s+DIVISION\b', up):
            if has_cics and not linkage_seen and not is_copybook:
                out.append('       LINKAGE SECTION.\n'); out.append('           COPY DFHEIBLK.\n')
                linkage_seen = True
            division = 'PROC'
        if re.search(r'\bLINKAGE\s+SECTION\b', up) and not is_copybook:
            out.append(line); i += 1
            if has_cics:
                out.append('           COPY DFHEIBLK.\n')
            linkage_seen = True
            continue
        m = EXEC_START.search(line[7:72] if len(line) > 7 else '')
        if m:
            # collect the whole EXEC ... END-EXEC block
            block = [line]
            while not re.search(r'\bEND-EXEC\b', block[-1].upper()) and i + 1 < len(lines):
                i += 1; block.append(lines[i])
            text = ' '.join(b[7:72] for b in block if not is_comment(b)).upper()
            period = bool(re.search(r'END-EXEC\s*\.', text))
            inc = re.search(r'EXEC\s+SQL\s+INCLUDE\s+([A-Z0-9#@$]+)', text)
            if inc:
                out.append(f'           COPY {inc.group(1)}.\n')
                out.extend(comment(b) for b in block[1:])
            elif division == 'PROC':
                indent = re.match(r'(\s*)', block[0][7:]).group(1)
                out.append(block[0][:7] + indent + 'CONTINUE' + ('.' if period else '') + '\n')
                out.extend(comment(b) for b in block[1:])
            else:
                out.extend(comment(b) for b in block)
            i += 1; continue
        line = re.sub(r'DFHRESP\s*\(\s*([A-Z0-9]+)\s*\)', lambda mm: str(RESP.get(mm.group(1).upper(), 0)), line, flags=re.I)
        line = re.sub(r'DFHVALUE\s*\(\s*([A-Z0-9]+)\s*\)', '0', line, flags=re.I)
        out.append(line); i += 1
    return out

def build_copylib(app_dir, tmp):
    for d in (os.path.join(ROOT, 'tools', 'stubs'), os.path.join(ROOT, 'shared', 'cpy'), os.path.join(app_dir, 'cpy')):
        if not os.path.isdir(d): continue
        for f in os.listdir(d):
            if f.lower().endswith('.cpy'):
                src = open(os.path.join(d, f), encoding='latin-1').readlines()
                open(os.path.join(tmp, f.upper()[:-4] + '.cpy'), 'w', encoding='latin-1').writelines(preprocess(src, True))

def validate(app_dir, progs):
    tmp = tempfile.mkdtemp(prefix='val')
    build_copylib(app_dir, tmp)
    fails = 0
    for p in progs:
        src = open(p, encoding='latin-1').readlines()
        pp = os.path.join(tmp, os.path.basename(p))
        open(pp, 'w', encoding='latin-1').writelines(preprocess(src))
        r = subprocess.run(['cobc', '-fsyntax-only', '-std=ibm', '-fixed', '-I', tmp, pp],
                           capture_output=True, text=True)
        msgs = (r.stdout + r.stderr).strip().replace(tmp + '/', '')
        ok = r.returncode == 0
        loc = sum(1 for l in src if l.strip() and not is_comment(l))
        print(f"{'PASS' if ok else 'FAIL'}  {os.path.relpath(p, ROOT):<48} {len(src):>5} lines {loc:>5} code")
        if not ok:
            fails += 1
            print('      ' + msgs.replace('\n', '\n      ')[:4000])
    shutil.rmtree(tmp)
    return fails

if __name__ == '__main__':
    app = os.path.abspath(sys.argv[1])
    progs = [os.path.abspath(p) for p in sys.argv[2:]] or sorted(
        os.path.join(dp, f) for dp, _, fs in os.walk(os.path.join(app, 'src')) for f in fs if f.lower().endswith('.cbl'))
    n = validate(app, progs)
    print(f"\n{len(progs) - n}/{len(progs)} programs pass")
    sys.exit(n)
