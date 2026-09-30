import re, csv, sys
M='/tmp/claude-0/-home-user-restud/14c08c23-2116-50bc-966c-8ec95367010e/scratchpad/main'
pages={i: open(f'{M}/part{i}.txt',encoding='utf-8',errors='replace').read().split('\f') for i in range(1,6)}
# editions: (part, first page (1-based PDF page), label). The table runs from its title page over the '(continued)' pages until the next 'Table' title.
EDS=[(2,152,'1983-93'),(2,279,'1984-94'),(2,404,'1985-95'),(2,480,'1986-96'),(3,248,'1989-99'),(3,474,'1990-2000'),(4,67,'1991-2001'),(4,182,'1992-2002'),(4,274,'1993-2003'),(4,375,'1994-2004'),(4,488,'1995-2005'),(5,90,'1996-2006'),(5,209,'1997-2007')]
num=re.compile(r'^(?:[\dOGlSI]{1,3}(?:[,.][\dOGlSI]{3})*|[\dOGlSI]+|[tT*†\-–—.]|\(\d+\)|\d+\(\d+\))$')
def tok(v):
    v=v.strip().replace('O','0').replace('G','6').replace('l','1').replace('S','5').replace('I','1')
    if re.fullmatch(r'\d{1,3}\.\d{3}',v): v=v.replace('.',',')
    if re.fullmatch(r'[\d,]+',v): return int(v.replace(',',''))
    if v in ('t','T','*','†'): return 2   # under 5 (shown as t / *): coded 2
    if v in ('-','–','—','.'): return 0
    return None
def parse(part,p0,label):
    y0=int(label[:4]); y1=int(label.split('-')[1]); y1 = y1 if y1>1000 else (1900+y1 if y1>50 else 2000+y1)
    years=list(range(y0,y1+1)); n=len(years); rows=[]; seen_title=False
    for p in range(p0-2, p0+8):
        if p>=len(pages[part]): break
        t=pages[part][p]
        if not seen_title and re.search(r'settlement,?\s+by\s+(country\s+of\s+)?nationality[^\n]{0,80}'+label.replace('-','\\s*-\\s*'), t, re.I): seen_title=True; tabno=re.search(r'Table\s+(\d\.\d)\s+(Acceptances|Grants)', t); tabno=tabno.group(1) if tabno else None
        elif seen_title:
            other=[m for m in re.findall(r'Table\s+(\d\.\d)', t) if m!=tabno]
            if other and 'continued' not in t: break
        if not seen_title: continue
        for line in t.split('\n'):
            parts=re.split(r'\s{2,}',line.strip())
            if len(parts)<n+1: 
                # numbers may be separated by single spaces; try splitting name from trailing numeric tokens
                toks=line.strip().split()
                vals=[];k=len(toks)
                while k>0 and num.match(toks[k-1]): vals.insert(0,toks[k-1]); k-=1
                name=' '.join(toks[:k]); parts=[name]+vals
            name=parts[0]; vals=parts[1:]
            if len(vals)<n or not name or re.match(r'^\d',name): continue
            vals=vals[-n:]  # last n tokens = the years
            conv=[tok(v) for v in vals]
            if any(c is None for c in conv): continue
            name=re.sub(r'\s*\(\d+\)\s*','',name).strip(' .:')
            rows.append((label,name,years,conv))
    return rows
out=[]; summary=[]
for part,p0,label in EDS:
    rows=parse(part,p0,label); allrow=[r for r in rows if re.match(r'^(all|ah|aii|a11)\s*national',r[1].lower())]
    summary.append((label,len(rows),allrow[0][3] if allrow else None))
    for label_,name,years,conv in rows:
        for y,v in zip(years,conv): out.append((label_,name,y,v))
for s in summary: print(s)
with open(f'{M}/ho_settlement_by_nationality_editions.csv','w',newline='') as fh:
    w=csv.writer(fh); w.writerow(['edition','nationality','year','acceptances']); w.writerows(out)
print('rows',len(out))
# quick look: a few nationalities in the 1983-93 and 1997-2007 editions
for lab in ('1983-93','1997-2007'):
    names=sorted(set(r[1] for r in out if r[0]==lab)); print(lab,'n names',len(names)); print([nm for nm in names if re.search(r'Hong|Zimb|South Af|Kenya|Uganda|India|Pakistan|Nigeria|Jamaica|USA|United States|Australia',nm)])
