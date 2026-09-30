import numpy as np, pandas as pd, warnings; warnings.filterwarnings('ignore')
from m1964 import *
from vint import NEWS
B=pd.read_csv(D+'determinants/boe_extras_1962_2019.csv').set_index('year'); TX=pd.read_csv(D+'determinants/boe_a27_tax_variants_1956_2019.csv').set_index('year')
wp=pd.read_csv(D+'determinants/work_permits.csv')
X=DF.copy()
for c in ['vacancies_k','vu_ratio']: X[c]=X.year.map(B[c])
for c in ['cg_nics_real_pw','cg_nics_employees_real_pw','cg_nics_employers_real_pw','cg_income_tax_real_pw']: X[c]=X.year.map(TX[c])
X['vac_per_wap']=1000*(1000*X.vacancies_k)/X.workpop; X['ln_vu']=np.log(X.vu_ratio); X['dln_vu']=X.ln_vu.diff(); X['ln_wap']=np.log(X.workpop)
X['permits_k']=X.year.map(wp.set_index('year').permits)/1000
for c in ['vac_per_wap','vu_ratio','dln_vu','cg_nics_real_pw','cg_nics_employees_real_pw','cg_nics_employers_real_pw','cg_income_tax_real_pw','ln_wap','permits_k']: X['L_'+c]=X[c].shift()
X=X.copy()
def r(lab,S,dv,xs):
    dd0=S[[dv]+xs].dropna(); xs=[v for v in xs if dd0[v].std()>0]; m,Xm,se,p,dd=ols(S,dv,xs)
    ex={c:(round(m.params[c],4),round(p[c],3)) for c in Xm.columns if c not in ['const','L_y','L_g','L_debt','step','L_permits_k','L_eu']}
    print(f"  {lab:34s} N={len(dd)} {ex}")
for tag,S,st in [('EXT 1965-2016',X[(X.year>=1965)&(X.year<=2016)],['step']),('CORE 1977-2016',X[(X.year>=1977)&(X.year<=2016)],[])]:
    print('==',tag); B0=['L_y','p']+st+['L_debt','L_un']
    r('NICs total',S,'y',B0+['L_cg_nics_real_pw']); r('NICs employees',S,'y',B0+['L_cg_nics_employees_real_pw']); r('NICs employers',S,'y',B0+['L_cg_nics_employers_real_pw'])
    r('employee + employer separately',S,'y',B0+['L_cg_nics_employees_real_pw','L_cg_nics_employers_real_pw'])
    r('Kronmal: tax/WAP + ln WAP',S,'y',['L_y','L_tax','p']+st+['L_debt','L_un','L_ln_wap'])
    r('EU placebo + vac/WAP (1977-2016)',X[(X.year>=1977)&(X.year<=2016)],'eu',['L_eu','L_tax','p','L_debt','L_un','L_vac_per_wap'])
# permits 1965-2008 with vacancies
P=X[(X.year>=1965)&(X.year<=2008)]
print('== permits 1965-2008'); r('permits: proj',P,'permits_k',['L_permits_k','p','step','L_tax','L_debt','L_un']); r('permits: + vac/WAP',P,'permits_k',['L_permits_k','p','step','L_tax','L_debt','L_un','L_vac_per_wap']); r('permits: vac only',P,'permits_k',['L_permits_k','step','L_tax','L_debt','L_un','L_vac_per_wap'])
P2=X[(X.year>=1948)&(X.year<=2008)]; print('permits from 1948 (no fiscal controls: tax/debt start 1962):'); r('permits 1948-2008: proj+vac',P2,'permits_k',['L_permits_k','p','L_vac_per_wap'])
# revisions predictability
N=NEWS.set_index('year'); X['revision']=X.year.map(N.revision); Sr=X[(X.year>=1977)&(X.year<=2016)&(X.revision.notna())&(X.revision!=0)]
m,Xm,se,p,dd=ols(Sr,'revision',['L_vac_per_wap']); print('revisions on lagged vac/WAP: N',len(dd),'coef',round(m.params['L_vac_per_wap'],3),'p',round(p['L_vac_per_wap'],3))
m,Xm,se,p,dd=ols(Sr,'revision',['L_vac_per_wap','L_un','L_tax']); print('  + unem, tax: vac p',round(p['L_vac_per_wap'],3),'unem p',round(p['L_un'],3))
