import json, numpy as np, pandas as pd
from m1964 import *
from statsmodels.stats.diagnostic import acorr_breusch_godfrey
T = {'generated': 'Python replica of the R set-up, 25 Sep 2026 (provisional until manuscript_1964.R is run)', 'sample': '1965-2019, series B (non-British to 1974, non-EU from 1975), basis step in every column'}
t2 = {}
t2['fiscal'], mF, XF = cell(S, 'y', FISC); t2['trend'], mT, XT = cell(S, 'y', TRND)
T['table2'] = t2
S77 = DF[(DF.year >= 1977) & (DF.year <= 2019)].copy()
def t4(spec):
    base = ['L_tax'] if spec == 'fiscal' else ['trend']
    out = {}
    out['dr'],   _, _ = cell(S,   'y', ['L_y'] + base + ['dr', 'step'] + CTRL)
    out['eu'],   _, _ = cell(S77, 'eu', ['L_eu'] + base + ['p'] + CTRL)
    out['gross'],_, _ = cell(S,   'g', ['L_g'] + base + ['p', 'step'] + CTRL)
    out['level'],_, _ = cell(S,   'y', ['L_y'] + base + ['lev', 'step'] + CTRL)
    out['zm'],   _, _ = cell(S77, 'y', ['L_y'] + base + ['zm'] + CTRL)
    out['lab'],  _, _ = cell(S,   'y', ['L_y', 'L_labtax', 'p', 'step'] + CTRL) if spec == 'fiscal' else (None, None, None)
    out = {k: v for k, v in out.items() if v is not None}; out['spec'] = spec; return out
T['table4'] = t4('fiscal'); T['table4_trend'] = t4('trend')
# Table B2: OLS / HAC / NW standard errors, fiscal specification
m, X, se_hac, p_hac, dd = ols(S, 'y', FISC)
T['tableB2'] = dict(terms=list(X.columns), coef=[float(v) for v in m.params], ols=[float(v) for v in m.bse], hac=[float(v) for v in se_hac], nw=[float(v) for v in nw_se(m, X, 1)])
# Figure 4 rows
def est(lab, d, y, xs, term):
    m, X, se, p, dd = ols(d, y, xs); return dict(label=lab, coef=float(m.params[term]), se=float(se[term]), p=float(p[term]), n=int(len(dd)))
R = [
 est('Baseline (fiscal specification), 1965-2019', S, 'y', FISC, 'p'),
 est('Basis step omitted', S, 'y', ['L_y', 'L_tax', 'p'] + CTRL, 'p'),
 est('Zero-net-migration projection, 1977-2019', S77, 'y', ['L_y', 'L_tax', 'zm'] + CTRL, 'zm'),
 est('Publication-dated projection, 1978-2019', S77, 'y', ['L_y', 'L_tax', 'p_pub'] + CTRL, 'p_pub'),
 est('Gross (unscaled) inflows', S, 'g', ['L_g', 'L_tax', 'p', 'step'] + CTRL, 'p'),
 est('Projected level in place of change', S, 'y', ['L_y', 'L_tax', 'lev', 'step'] + CTRL, 'lev'),
 est('Labour taxes per working-age person', S, 'y', ['L_y', 'L_labtax', 'p', 'step'] + CTRL, 'p'),
 est('All taxes per resident', S, 'y', ['L_y', 'L_taxres', 'p', 'step'] + CTRL, 'p'),
 est('+ 1998 policy step (POL97)', S, 'y', FISC + ['POL97'], 'p'),
 est('+ 1998-2007 policy window', S, 'y', FISC + ['liberal'], 'p'),
 est('+ all five MPR indicators', S, 'y', FISC + ['POL97', 'POL02', 'EXP04', 'ENL86', 'ENL95'], 'p'),
 est('+ 1968 and 1973 Act steps', S, 'y', FISC + ['post1968', 'post1973'], 'p'),
 est('+ linear trend', S, 'y', FISC + ['trend'], 'p'),
 est('+ supply-side controls (3)', S, 'y', FISC + ['L_relinc_dev', 'L_relinc_rich', 'L_youth_origin'], 'p'),
 est('+ growth, inflation, British emigration', S, 'y', FISC + ['L_uk_growth', 'L_inflation', 'L_brit_out_rate'], 'p'),
 est('Second lag of dependent variable', S, 'y', ['L_y', 'L2_y', 'L_tax', 'p', 'step'] + CTRL, 'p'),
 est('Sample from 1971', S[S.year >= 1971], 'y', FISC, 'p'),
 est('Sample from 1977', S77, 'y', ['L_y', 'L_tax', 'p'] + CTRL, 'p'),
 est('Sample from 1979', S[S.year >= 1979], 'y', ['L_y', 'L_tax', 'p'] + CTRL, 'p'),
 est('Sample to 2003 (pre-accession)', S[S.year <= 2003], 'y', FISC, 'p'),
 est('LTIM inflows, 1992-2019', S[S.year >= 1992], 'ltim', ['L_ltim', 'L_tax', 'p'] + CTRL, 'p')]
PL = [
 est('Placebo: total dependency ratio', S, 'y', ['L_y', 'L_tax', 'dr', 'step'] + CTRL, 'dr'),
 est('Placebo: total dependency ratio, alongside OADR', S, 'y', ['L_y', 'L_tax', 'p', 'dr', 'step'] + CTRL, 'dr'),
 est('Placebo: EU inflows, 1977-2019', S77, 'eu', ['L_eu', 'L_tax', 'p'] + CTRL, 'p')]
T['figure4'] = dict(rows=R, placebos=PL)
# circular-shift placebo
def t_of(x):
    d = S.copy(); d['pk'] = x; m, X, se, p, dd = ols(d, 'y', ['L_y', 'L_tax', 'pk', 'step'] + CTRL); return float(m.params['pk'] / se['pk'])
pv = S.p.values; t0 = t_of(pv); n = len(pv)
tk = [t_of(np.concatenate([pv[-k:], pv[:-k]])) for k in range(1, n)]
T['timing_placebo'] = dict(t_actual=t0, max_abs_t=float(max(abs(np.array(tk)))), pct95=float(np.quantile(abs(np.array(tk)), 0.95)), n_ge=int(sum(abs(np.array(tk)) >= abs(t0))), p=float((1 + sum(abs(np.array(tk)) >= abs(t0))) / n))
# second lag and BG orders
m1, X1, _, _, _ = ols(S, 'y', FISC); m2, X2, se2, p2, _ = ols(S, 'y', ['L_y', 'L2_y', 'L_tax', 'p', 'step'] + CTRL)
T['second_lag'] = dict(coef_p=float(m2.params['p']), p_p=float(p2['p']), L2=float(m2.params['L2_y']), L2_p=float(p2['L2_y']), aic1=float(m1.aic), aic2=float(m2.aic),
                       bg1=[float(acorr_breusch_godfrey(m1, nlags=k)[1]) for k in range(1, 5)], bg2=[float(acorr_breusch_godfrey(m2, nlags=k)[1]) for k in range(1, 5)])
# one-SD magnitude
sd_p = float(S.p.std()); b = float(m1.params['p']); rho = float(m1.params['L_y'])
T['magnitude'] = dict(sd_proj=sd_p, impact_pct=100 * (np.exp(b * sd_p) - 1), longrun_pct=100 * (np.exp(b * sd_p / (1 - rho)) - 1))
# British-citizen placebo on the extended sample (3.01 from 1975): 1977-2019 as in the thesis but with the 1977 start
# rename terms to the R names the document builder expects
RN = {'const': '(Intercept)', 'L_y': 'L_log_mig_rate', 'L_tax': 'L_tax_workpop', 'p': 'delta_oadr_10', 'step': 'basis_pre1975', 'L_debt': 'L_ln_debt_gdp', 'L_un': 'L_unem',
      'dr': 'delta_dr_10', 'lev': 'level_oadr_10', 'zm': 'delta_zm_10', 'L_eu': 'L_eu_log_mig_rate', 'L_g': 'L_alt_log_mig_rate', 'L_labtax': 'L_tax_lab_pw', 'trend': 'trend'}
def ren(col):
    for k in ('coef', 'se', 'p'): col[k] = {RN.get(t, t): v for t, v in col[k].items()}
for k in ('fiscal', 'trend'): ren(T['table2'][k])
for tb in ('table4', 'table4_trend'):
    for k, v in T[tb].items():
        if k != 'spec': ren(v)
T['tableB2']['terms'] = [RN.get(t, t) for t in T['tableB2']['terms']]
json.dump(T, open('/sessions/practical-brave-hypatia/mnt/outputs/tables_1964.json', 'w'), indent=1)
print('written')
