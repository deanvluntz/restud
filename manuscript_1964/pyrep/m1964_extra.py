import json, numpy as np, pandas as pd
from m1964 import *
from statsmodels.stats.diagnostic import breaks_cusumolsresid
O='/sessions/practical-brave-hypatia/mnt/outputs/'; T=json.load(open(O+'tables_1964.json'))
# --- permits 1965-2008 (Bauer & Salt WP+FP 1946-2008) ---
wp=pd.read_csv(D+'determinants/work_permits.csv'); print(wp.columns.tolist(), wp.year.min(), wp.year.max())
pc=[c for c in wp.columns if 'permit' in c.lower() or c.lower() in ('wp_fp','permits','value')][0]
DF['permits_k']=DF.year.map(wp.set_index('year')[pc])/1000.0; DF['L_permits_k']=DF.permits_k.shift()
d=DF[(DF.year>=1965)&(DF.year<=2008)]
m,X,se,p,dd=ols(d,'permits_k',['L_permits_k','p','step','L_tax','L_debt','L_un']); print('permits 1965-2008', len(dd), round(m.params['p'],3), round(p['p'],3))
m2,X2,se2,p2,dd2=ols(DF[(DF.year>=1979)&(DF.year<=2008)],'permits_k',['L_permits_k','p','L_tax','L_debt','L_un']); print('permits 1979-2008', len(dd2), round(m2.params['p'],3), round(p2['p'],3))
m3,X3,se3,p3,dd3=ols(d,'permits_k',['L_permits_k','dr','step','L_tax','L_debt','L_un']); print('permits DR placebo', round(m3.params['dr'],4), round(p3['dr'],3))
T['permits']=dict(n=len(dd), coef=float(m.params['p']), p=float(p['p']), n79=len(dd2), coef79=float(m2.params['p']), p79=float(p2['p']), dr_coef=float(m3.params['dr']), dr_p=float(p3['dr']))
# --- British citizens in from non-EU, 1977-2019 (3.01), same controls ---
ips=pd.read_csv(D+'determinants/ips_301_citizenship_by_residence_1975_2019.csv')
DF['brit']=np.log(1000*(1000*DF.year.map(ips.set_index('year')['Inflow_NON-EU_brit']))/DF.workpop.shift()); DF['L_brit']=DF.brit.shift()
S77=DF[(DF.year>=1977)&(DF.year<=2019)]
mb,Xb,seb,pb,ddb=ols(S77,'brit',['L_brit','p','L_tax','L_debt','L_un']); print('British 1977-2019', len(ddb), round(mb.params['p'],4), round(pb['p'],3))
mc,Xc,sec,pcc,ddc=ols(S77,'y',['L_y','p','brit','L_tax','L_debt','L_un']); print('non-EU + British control', round(mc.params['p'],4), round(pcc['p'],3))
T['british']=dict(n=len(ddb), coef=float(mb.params['p']), p=float(pb['p']), ctrl_coef=float(mc.params['p']), ctrl_p=float(pcc['p']))
# --- stability on 1965-2019: recursive from 1990, rolling 25-year, OLS-CUSUM ---
rec=[]; 
for end in range(1990,2020):
    mm,XX,ss,pp,_=ols(S[S.year<=end],'y',FISC); rec.append((end, float(mm.params['p']), float(pp['p'])))
roll=[]
for st in range(1965,1996):
    dd_=S[(S.year>=st)&(S.year<=st+24)]; xs=['L_y','L_tax','p']+(['step'] if st<1975 else [])+CTRL
    mm,XX,ss,pp,_=ols(dd_,'y',xs); roll.append((st, float(mm.params['p']), float(pp['p'])))
m0,X0,_,_,_=ols(S,'y',FISC); cus=breaks_cusumolsresid(m0.resid.values, ddof=X0.shape[1])
T['stability']=dict(recursive=rec, rolling=roll, cusum_p=float(cus[1]))
print('recursive', [(e,round(b,4),round(pp,3)) for e,b,pp in rec if e in (1990,1994,1998,2001,2005,2010,2019)])
print('rolling range', min(r[1] for r in roll), max(r[1] for r in roll), 'sig5', sum(r[2]<0.05 for r in roll), 'of', len(roll)); print('cusum p', round(cus[1],3))
# --- recursive residual bootstrap for the lagged-DV bias (2,000 reps) ---
rng=np.random.default_rng(1); m,X,se,p,dd=ols(S,'y',FISC); b=m.params.values; e=m.resid.values-m.resid.values.mean(); n=len(dd)
Xn=X.values.copy(); iy=list(X.columns).index('L_y'); rhos=[]; bp=[]
y0=dd['y'].values
for r in range(2000):
    es=rng.choice(e,n,replace=True); ys=np.empty(n); Xs=Xn.copy(); lag=Xn[0,iy]
    for t in range(n):
        Xs[t,iy]=lag; ys[t]=Xs[t]@b+es[t]; lag=ys[t]
    bb=np.linalg.lstsq(Xs,ys,rcond=None)[0]; rhos.append(bb[iy]); bp.append(bb[list(X.columns).index('p')])
bias=np.mean(rhos)-b[iy]; rho_c=b[iy]-bias; ip=list(X.columns).index('p')
lr_unc=b[ip]/(1-b[iy]); lr_c=b[ip]/(1-rho_c)
lr_boot=np.array(bp)/(1-(np.array(rhos)-bias)); ci=np.quantile(2*lr_c-lr_boot,[0.025,0.975])  # recentred percentile
T['bias']=dict(rho=float(b[iy]), bias=float(bias), rho_corrected=float(rho_c), analytic=float(-(1+3*b[iy])/n), lr_uncorrected=float(lr_unc), lr_corrected=float(lr_c), ci=[float(ci[0]),float(ci[1])])
print('bias', {k:(round(v,4) if not isinstance(v,list) else [round(x,4) for x in v]) for k,v in T['bias'].items()})
json.dump(T,open(O+'tables_1964.json','w'),indent=1)
