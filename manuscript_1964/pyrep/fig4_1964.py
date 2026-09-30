# fig4_1964.py -- Figure 4 (robustness summary) from tables_1964.json; standalone (no replica needed).
#   /usr/bin/python3.12 fig4_1964.py [path/to/tables_1964.json] [out.png]
import json, sys, matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
src = sys.argv[1] if len(sys.argv) > 1 else '../tables_1964.json'; out = sys.argv[2] if len(sys.argv) > 2 else '../figure4_1964.png'
T = json.load(open(src)); R = T['figure4']['rows']; PL = T['figure4']['placebos']; allr = R + PL; k = len(allr)
ypos = list(range(k, len(PL), -1)); ypos = [y + 1 for y in ypos] + list(range(len(PL), 0, -1))
plt.rcParams.update({'font.family': 'sans-serif', 'font.size': 8})
fig, a = plt.subplots(figsize=(6.5, 5.6))
for r, y in zip(allr, ypos):
    lo, hi = r['coef'] - 1.96 * r['se'], r['coef'] + 1.96 * r['se']; a.plot([lo, hi], [y, y], 'k-', lw=1.2)
    a.plot(r['coef'], y, 'o', color='k', mfc='k' if r in R else 'white', ms=5)
a.axvline(0, color='0.6', lw=0.8); a.axvline(R[0]['coef'], color='k', ls='--', lw=0.8); a.axhline(len(PL) + 1, color='0.8', lw=0.8)
a.set_yticks(ypos); a.set_yticklabels([r['label'] for r in allr], fontsize=7.5); a.set_ylim(0.5, k + 1.5)
a.set_xlabel('Coefficient on the projection term (95% HAC interval)'); a.tick_params(direction='out', length=2.5)
fig.tight_layout(); fig.savefig(out, dpi=400); plt.close(fig); print('written', out, k, 'rows')
