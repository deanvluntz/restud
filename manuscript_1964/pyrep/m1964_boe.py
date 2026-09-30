import json, numpy as np, pandas as pd
from m1964 import *
B=pd.read_csv(D+'determinants/boe_millennium_annual_1955_2017.csv').set_index('year')
w=pd.read_excel(D+'Dissertation_Dataset_Final.xlsx','1964'); w=w[w.year.notna()].set_index('year')
for c in ['admin_unem_rate','unem_spliced_boe','vacancies_k','vu_ratio','awe_gbp','wage_inflation','tbill_rate','replacement_ratio','union_density','real_short_rate','grants_personal_gbp_m','lab_prod_hour_growth']: DF[c]=DF.year.map(B[c])
for c in ['lt_inf_exp','st_inf_exp','tfp_growth']: DF[c]=DF.year.map(w[c])
DF['vac_per_wap']=1000*(1000*DF.vacancies_k)/DF.workpop; DF['ln_vu']=np.log(DF.vu_ratio); DF['real_wage_growth']=DF.wage_inflation-DF.inflation
DF['grants_gdp']=DF.year.map(B.grants_personal_gbp_m)/1000/DF.year.map(lambda y: np.nan)  # placeholder
for c in ['admin_unem_rate','unem_spliced_boe','vac_per_wap','vu_ratio','ln_vu','wage_inflation','real_wage_growth','tbill_rate','real_short_rate','replacement_ratio','union_density','lt_inf_exp','st_inf_exp','tfp_growth','lab_prod_hour_growth']: DF['L_'+c]=DF[c].shift()
S16=DF[(DF.year>=1965)&(DF.year<=2016)]; S77=DF[(DF.year>=1977)&(DF.year<=2016)]
print('coverage 1965-2016:', {c:int(S16['L_'+c].notna().sum()) for c in ['admin_unem_rate','vu_ratio','wage_inflation','tbill_rate','lt_inf_exp','st_inf_exp','tfp_growth','replacement_ratio']})
def run(lab,d,xs,term='p'):
    m,X,se,p,dd=ols(d,'y',xs); ct={c:(round(m.params[c],4),round(p[c],3)) for c in X.columns if c not in ('const','L_y','L_tax','L_debt','step')}
    print(f"{lab:46s} N={len(dd):2d} proj {m.params['p']:.4f} ({p['p']:.3f})  | {ct}")
    return dict(label=lab,n=len(dd),coef=float(m.params['p']),se=float(se['p']),p=float(p['p']),others={c:(float(m.params[c]),float(p[c])) for c in X.columns if c not in ('const','L_y','L_tax','L_debt','step','p')})
base=['L_y','L_tax','p','step','L_debt']
R=[]
R.append(run('Baseline, 1965-2016 (ILO/BoE unemployment)',S16,base+['L_un']))
R.append(run('Administrative/claimant unemployment rate',S16,base+['L_admin_unem_rate']))
R.append(run('BoE spliced unemployment rate',S16,base+['L_unem_spliced_boe']))
R.append(run('Vacancies per 1,000 WAP instead of unemployment',S16,base+['L_vac_per_wap']))
R.append(run('V/U ratio instead of unemployment',S16,base+['L_vu_ratio']))
R.append(run('ln V/U instead of unemployment',S16,base+['L_ln_vu']))
R.append(run('+ V/U ratio alongside unemployment',S16,base+['L_un','L_vu_ratio']))
R.append(run('+ wage inflation',S16,base+['L_un','L_wage_inflation']))
R.append(run('+ real wage growth',S16,base+['L_un','L_real_wage_growth']))
R.append(run('+ Treasury bill rate',S16,base+['L_un','L_tbill_rate']))
R.append(run('+ real short rate',S16,base+['L_un','L_real_short_rate']))
R.append(run('+ short-term inflation expectations',S16,base+['L_un','L_st_inf_exp']))
R.append(run('+ long-term inflation expectations',S16,base+['L_un','L_lt_inf_exp']))
R.append(run('+ TFP growth',S16,base+['L_un','L_tfp_growth']))
R.append(run('+ replacement ratio',S16,base+['L_un','L_replacement_ratio']))
R.append(run('+ union density',S16,base+['L_un','L_union_density']))
R.append(run('+ V/U, wage inflation, T-bill, expectations, TFP',S16,base+['L_un','L_vu_ratio','L_wage_inflation','L_tbill_rate','L_st_inf_exp','L_tfp_growth']))
R.append(run('Core 1977-2016 baseline',S77,['L_y','L_tax','p','L_debt','L_un']))
R.append(run('Core 1977-2016 + V/U, wage infl, T-bill, exp, TFP',S77,['L_y','L_tax','p','L_debt','L_un','L_vu_ratio','L_wage_inflation','L_tbill_rate','L_st_inf_exp','L_tfp_growth']))
json.dump(R,open('/sessions/practical-brave-hypatia/mnt/outputs/boe_tests_1964.json','w'),indent=1)
