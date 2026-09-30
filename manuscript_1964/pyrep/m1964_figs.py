import json, numpy as np, matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
from m1964 import *
O = '/sessions/practical-brave-hypatia/mnt/outputs/'
plt.rcParams.update({'font.family': 'serif', 'font.size': 8})
F = DF[(DF.year >= 1964) & (DF.year <= 2019)]
# --- Figure 1 ---
fig, ax = plt.subplots(2, 1, figsize=(6.5, 4.6), sharex=True, gridspec_kw=dict(hspace=0.08))
for a in ax: a.axvspan(1998, 2007, color='0.9', lw=0)
g = 1000 * F.dv_B_splice_raw / 1000
ax[0].plot(F.year, F.dv_B_splice_raw, 'k-', lw=1.6)
ax[0].axvline(1974.5, color='0.45', lw=0.8, ls=':')
ax[0].set_ylabel('Gross controlled inflows\n(thousands)')
ax[0].text(2002.5, ax[0].get_ylim()[1] * 0.95, '1998-2007', color='0.35', ha='center', va='top', fontsize=7)
ax[0].text(1974.9, ax[0].get_ylim()[1] * 0.95, 'non-British | non-EU', color='0.35', ha='left', va='top', fontsize=7)
ax[1].plot(F.year, F.p, 'k-', lw=1.6, label='As published')
ax[1].plot(F.year, F.zm, '--', color='0.35', lw=1.6, label='Zero net migration (from 1977)')
ax[1].axhline(0, color='0.3', lw=0.8); ax[1].set_ylabel('Projected 10-year\nchange in OADR'); ax[1].set_xlabel('Year')
ax[1].legend(loc='lower center', frameon=False, fontsize=7)
for a in ax:
    a.tick_params(direction='out', length=2.5); a.set_xlim(1963, 2020)
fig.savefig(O + 'figure1_1964.png', dpi=400, bbox_inches='tight'); plt.close(fig)
# --- Figure 2: added-variable plots, trend specification ---
def avp(d, y, xj, others):
    dd = d[[y, xj] + others].dropna(); Z = sm.add_constant(dd[others]).values
    ry = dd[y].values - Z @ np.linalg.lstsq(Z, dd[y].values, rcond=None)[0]; rx = dd[xj].values - Z @ np.linalg.lstsq(Z, dd[xj].values, rcond=None)[0]
    return rx, ry, float((rx * ry).sum() / (rx * rx).sum())
S77 = DF[(DF.year >= 1977) & (DF.year <= 2019)]
p1 = avp(S, 'y', 'p', ['L_y', 'trend', 'step', 'L_debt', 'L_un']); p2 = avp(S77, 'eu', 'p', ['L_eu', 'trend', 'L_debt', 'L_un'])
fig, ax = plt.subplots(1, 2, figsize=(6.5, 2.9)); yl = (min(p1[1].min(), p2[1].min()) * 1.1, max(p1[1].max(), p2[1].max()) * 1.1)
for a, p, t in zip(ax, [p1, p2], ['(a) Controlled inflows, 1965-2019', '(b) EU inflows (uncontrolled), 1977-2019']):
    a.scatter(p[0], p[1], s=12, facecolors='none', edgecolors='k', lw=0.7); a.axhline(0, color='0.85', lw=0.8); a.axvline(0, color='0.85', lw=0.8)
    xx = np.array([p[0].min(), p[0].max()]); a.plot(xx, p[2] * xx, 'k-', lw=1.6); a.set_ylim(yl)
    a.set_xlabel('Projected 10-year OADR change\n(residualised)'); a.set_ylabel('ln inflow rate (residualised)'); a.set_title(t, fontsize=8)
    a.text(0.03, 0.95, f'slope = {p[2]:.4f}', transform=a.transAxes, va='top', fontsize=7.5); a.tick_params(direction='out', length=2.5)
fig.tight_layout(); fig.savefig(O + 'figure2_1964.png', dpi=400); plt.close(fig)
print('AVP slopes', round(p1[2], 4), round(p2[2], 4))
# --- Figure 4: robustness summary ---
T = json.load(open(O + 'tables_1964.json')); R = T['figure4']['rows']; PL = T['figure4']['placebos']; allr = R + PL; k = len(allr)
ypos = list(range(k, len(PL), -1)); ypos = [y + 1 for y in ypos] + list(range(len(PL), 0, -1))
plt.rcParams.update({'font.family': 'sans-serif'})
fig, a = plt.subplots(figsize=(6.5, 5.6))
for r, y in zip(allr, ypos):
    lo, hi = r['coef'] - 1.96 * r['se'], r['coef'] + 1.96 * r['se']; a.plot([lo, hi], [y, y], 'k-', lw=1.2)
    a.plot(r['coef'], y, 'o', color='k', mfc='k' if r in R else 'white', ms=5)
a.axvline(0, color='0.6', lw=0.8); a.axvline(R[0]['coef'], color='k', ls='--', lw=0.8); a.axhline(len(PL) + 1, color='0.8', lw=0.8)
a.set_yticks(ypos); a.set_yticklabels([r['label'] for r in allr], fontsize=7.5); a.set_ylim(0.5, k + 1.5)
a.set_xlabel('Coefficient on the projection term (95% HAC interval)'); a.tick_params(direction='out', length=2.5)
fig.tight_layout(); fig.savefig(O + 'figure4_1964.png', dpi=400); plt.close(fig)
print('figures written')
