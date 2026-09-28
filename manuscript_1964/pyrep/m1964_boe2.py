import json, numpy as np, pandas as pd, warnings; warnings.filterwarnings('ignore')
from m1964 import *
B=pd.read_csv(D+'determinants/boe_extras_1962_2019.csv').set_index('year'); TX=pd.read_csv(D+'determinants/boe_a27_tax_variants_1956_2019.csv').set_index('year')
w=pd.read_excel(D+'Dissertation_Dataset_Final.xlsx','1964'); w=w[w.year.notna()].set_index('year')
X=DF.copy()
for c in ['unem_spliced_boe','admin_unem_rate','vacancies_k','vu_ratio','wage_inflation','tbill_rate','real_short_rate','replacement_ratio','union_density']: X[c]=X.year.map(B[c])
for c in ['lt_inf_exp','st_inf_exp','tfp_growth']: X[c]=X.year.map(w[c])
for c in TX.columns: X[c]=X.year.map(TX[c])
wl=X.workpop.shift(); X['vac_per_wap']=1000*(1000*X.vacancies_k)/X.workpop; X['ln_vu']=np.log(X.vu_ratio); X['dln_vu']=X.ln_vu.diff()
X['emig_all']=1000*(1000*X.all_out)/wl; X['emig_brit']=1000*(1000*X.brit_out)/wl; X['real_wage_growth']=X.wage_inflation-X.inflation; X['tax_inc_nics_pw']=X.cg_income_tax_real_pw+X.cg_nics_real_pw
for c in ['unem_spliced_boe','admin_unem_rate','vac_per_wap','vu_ratio','ln_vu','dln_vu','emig_all','emig_brit','wage_inflation','real_wage_growth','tbill_rate','real_short_rate','st_inf_exp','lt_inf_exp','tfp_growth','replacement_ratio','union_density','cg_income_tax_real_pw','cg_nics_real_pw','tax_inc_nics_pw','cg_direct_taxes_real_pw','cg_tax_products_production_real_pw','cg_vat_real_pw','cg_corporation_tax_real_pw','cg_receipts_real_pw']: X['L_'+c]=X[c].shift()
X=X.copy()
def go(S, step, tag):
    st=['step'] if step else []
    BASE=['L_y','L_tax','p']+st+['L_debt']
    def r(lab,xs,dv='y'):
        dd0=S[[dv]+xs].dropna(); xs=[v for v in xs if dd0[v].std()>0]
        m,Xm,se,p,dd=ols(S,dv,xs); ex={c:(round(m.params[c],4),round(p[c],3)) for c in Xm.columns if c not in ['const','L_y','L_g','L_tax','L_debt','step','p']}
        print(f"  {lab:24s} N={len(dd)} proj {m.params['p']:.4f} ({p['p']:.3f}) {ex}")
    print('=== '+tag+' | 1. labour-market state')
    r('ILO',BASE+['L_un']); r('BoE spliced',BASE+['L_unem_spliced_boe']); r('admin C/E',BASE+['L_admin_unem_rate']); r('vac/WAP',BASE+['L_vac_per_wap']); r('V/U',BASE+['L_vu_ratio'])
    r('unem+vac/WAP',BASE+['L_un','L_vac_per_wap']); r('unem+V/U',BASE+['L_un','L_vu_ratio']); r('unem+dln V/U',BASE+['L_un','L_dln_vu'])
    print('=== 2. gross + emigration'); GB=['L_g','L_tax','p']+st+['L_debt','L_un']
    r('gross',GB,'g'); r('+emig all',GB+['L_emig_all'],'g'); r('+emig Brit',GB+['L_emig_brit'],'g'); r('+emig+V/U',GB+['L_emig_all','L_vu_ratio'],'g'); r('rate+emig',BASE+['L_un','L_emig_all'])
    print('=== 4. tax by type'); TB=['L_y','p']+st+['L_debt','L_un']
    for lab,v in [('NAT/WAP','L_tax'),('income tax','L_cg_income_tax_real_pw'),('NICs','L_cg_nics_real_pw'),('inc+NICs','L_tax_inc_nics_pw'),('direct','L_cg_direct_taxes_real_pw'),('indirect','L_cg_tax_products_production_real_pw'),('VAT','L_cg_vat_real_pw'),('corporation','L_cg_corporation_tax_real_pw'),('CG receipts','L_cg_receipts_real_pw')]: r(lab,TB+[v])
    print('=== 5. all'); ALL=['L_un','L_vu_ratio','L_wage_inflation','L_tbill_rate','L_st_inf_exp','L_tfp_growth']
    r('all six',BASE+ALL); r('six+emig',BASE+ALL+['L_emig_all']); r('six+dlnVU',BASE+ALL+['L_dln_vu'])
go(X[(X.year>=1965)&(X.year<=2016)], True, 'EXTENDED 1965-2016')
go(X[(X.year>=1977)&(X.year<=2016)], False, 'CORE 1977-2016')
