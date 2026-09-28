# Python replica of the manuscript pipeline on the 1964 extension (series B, 1965-2019).
import sys, os; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from hac import vcovHAC
import numpy as np, pandas as pd, statsmodels.api as sm
from statsmodels.stats.diagnostic import acorr_breusch_godfrey, het_breuschpagan, linear_reset
from statsmodels.stats.stattools import jarque_bera
from statsmodels.tsa.stattools import adfuller
from scipy import stats
D = '/sessions/practical-brave-hypatia/mnt/quiet-instruments/diss_replication/'

def build():
    raw = pd.read_excel(D + 'Dissertation_Dataset_Final.xlsx', sheet_name='ReformSPA'); raw = raw[raw.year.notna()].copy(); raw['year'] = raw.year.astype(int)
    pub = pd.read_excel(D + 'Dissertation_Dataset_Final.xlsx', sheet_name='PubShiftOADR_ZM'); pub = pub[pub.year.notna()].copy(); pub['year'] = pub.year.astype(int)
    ext = pd.read_csv(D + 'determinants/extension_1962_2019_dataset.csv')
    ex  = pd.read_csv(D + 'determinants/extension_1962_2019_extras.csv')
    o   = pd.read_csv(D + 'determinants/npp_1963_1974_oadr.csv')
    f6  = pd.read_csv(D + 'determinants/fiscal_block_1962_1974.csv')
    df = pd.DataFrame({'year': range(1962, 2020)})
    df = df.merge(ext, on='year', how='left').merge(ex.drop(columns=['dr_note']), on='year', how='left')
    r = raw.set_index('year')
    # projection level at 10 years, labour tax per WAP, tax per resident, EU, LTIM, zero-migration, publication-dated, horizons
    lev = pd.concat([o.set_index('year').oadr_10, r.oadr_10]); df['lev'] = df.year.map(lev)
    lab = pd.concat([f6.set_index('year').tax_lab_pw, r.lab_tax_workpop]); df['labtax'] = df.year.map(lab)
    tr  = pd.concat([(f6.set_index('year').tax_nat_real_gbp_bn / f6.set_index('year').totpop_m), r.tax_totpop]); df['taxres'] = df.year.map(tr)
    df['eu']   = df.year.map(np.log(r.EU_inflows_WAPlagK))
    df['ltim'] = df.year.map(np.log(r.ltim_nEU_inflows_WAPlagK))
    df['zm']   = df.year.map(r.delta_zm_10)
    df['p_pub'] = df.year.map(pub.set_index('year').delta_oadr_10_pub.astype(float))
    for h in range(1, 11):
        df[f'oadr_d{h}'] = df.year.map(r[f'oadr_{h}'] - r.oadr_0); df[f'dr_d{h}'] = df.year.map(r[f'delta_dr_{h}'])
    # dependent variables
    wap_l = df.workpop.shift()
    df['y'] = np.log(1000 * (1000 * df.dv_B_splice_raw) / wap_l)       # series B, log per 1,000 WAP(t-1)
    df['g'] = np.log(1000 * df.dv_B_splice_raw)                         # gross, persons
    df['yA'] = np.log(1000 * (1000 * df.dv_A_non_british) / wap_l)
    df['yC'] = np.log(1000 * (1000 * df.dv_C_non_british_to_2003) / wap_l)
    df['ynEU'] = np.log(1000 * (1000 * df.non_eu) / wap_l)
    df['p'] = df.delta_oadr_10; df['dr'] = df.delta_dr_10; df['un'] = df.unem; df['tax'] = df.tax_workpop; df['debt'] = df.ln_debt_gdp
    df['step'] = df.basis_pre1975
    for v in ['y', 'g', 'yA', 'yC', 'ynEU', 'eu', 'ltim', 'un', 'tax', 'labtax', 'taxres', 'debt', 'uk_growth', 'inflation', 'brit_out', 'relinc_dev', 'relinc_rich', 'youth_origin']:
        df['L_' + v] = df[v].shift()
    df['L2_y'] = df.L_y.shift()
    df['L_brit_out_rate'] = 1000 * (1000 * df.L_brit_out) / wap_l
    for nm, y0 in [('POL97', 1998), ('POL02', 2002), ('EXP04', 2004), ('ENL86', 1986), ('ENL95', 1995)]: df[nm] = (df.year >= y0).astype(float)
    df['liberal'] = ((df.year >= 1998) & (df.year <= 2007)).astype(float)
    df['trend'] = (df.year - 1965 + 1).astype(float)
    return df

DF = build()

def ols(d, y, xs):
    dd = d[[y] + xs].dropna(); X = sm.add_constant(dd[xs]); m = sm.OLS(dd[y], X).fit()
    V = vcovHAC(X.values, m.resid.values); se = np.sqrt(np.diag(V)); t = m.params.values / se; p = 2 * stats.t.sf(abs(t), m.df_resid)
    return m, X, pd.Series(se, index=X.columns), pd.Series(p, index=X.columns), dd

def nw_se(m, X, lag=1):
    # sandwich::NeweyWest(lag = 1, prewhite = FALSE, adjust = TRUE): Bartlett kernel, df adjustment n/(n-k)
    Xv = X.values; e = m.resid.values; n, k = Xv.shape; U = Xv * e[:, None]
    S = U.T @ U / n
    for l in range(1, lag + 1):
        w = 1 - l / (lag + 1); G = U[l:].T @ U[:-l] / n; S += w * (G + G.T)
    S *= n / (n - k); B = np.linalg.inv(Xv.T @ Xv / n); return np.sqrt(np.diag(B @ S @ B / n))

def diag(m, X):
    n, k = X.shape
    bg2 = acorr_breusch_godfrey(m, nlags=2)[1]; bp = het_breuschpagan(m.resid, X)[1]
    reset = linear_reset(m, power=3, use_f=True).pvalue; jb = jarque_bera(m.resid)[1]
    kk = int(np.trunc((n - 1) ** (1 / 3))); adf = adfuller(m.resid.values, maxlag=kk, regression='ct', autolag=None)[1]
    vif = max(1 / (1 - sm.OLS(X.values[:, j], np.delete(X.values, j, 1)).fit().rsquared) for j in range(1, k))
    return dict(n=int(n), adjr2=float(m.rsquared_adj), bg2=float(bg2), bp=float(bp), reset=float(reset), jb=float(jb), adf=float(adf), maxvif=float(vif))

def cell(d, y, xs, lagname=None):
    m, X, se, p, dd = ols(d, y, xs)
    out = dict(coef={c: float(m.params[c]) for c in X.columns}, se={c: float(se[c]) for c in X.columns}, p={c: float(p[c]) for c in X.columns})
    out.update(diag(m, X)); out['years'] = f"{int(dd.index.map(lambda i: d.loc[i,'year']).min())}-{int(dd.index.map(lambda i: d.loc[i,'year']).max())}"
    return out, m, X

S = DF[(DF.year >= 1965) & (DF.year <= 2019)].copy()
CTRL = ['L_debt', 'L_un']
FISC = ['L_y', 'L_tax', 'p', 'step'] + CTRL
TRND = ['L_y', 'trend', 'p', 'step'] + CTRL
if __name__ == '__main__':
    c, m, X = cell(S, 'y', FISC); print('fiscal', {k: round(v, 4) for k, v in c['coef'].items()}, 'n', c['n'], 'adjr2', round(c['adjr2'], 4), 'bg2', round(c['bg2'], 3), 'vif', round(c['maxvif'], 1))
    c, m, X = cell(S, 'y', TRND); print('trend', {k: round(v, 4) for k, v in c['coef'].items()}, 'p', {k: round(v, 3) for k, v in c['p'].items()})
