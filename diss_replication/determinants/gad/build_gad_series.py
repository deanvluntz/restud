"""Build the GAD 'projected contribution pressure' series from the Quinquennial Reviews (27 Sep 2026).
Vintage rule as for the NPPs: vintage_for(t) = latest review whose BASE year <= t (alt: publication year).
For each year t the vintage's profile is linearly interpolated at t and t+10 (flat beyond the last tabulated year).
Rate vintages (1975 on): d10 = R(t+10)-R(t) in percentage points of the joint Class 1 rate required; pct10 = 100*(R(t+10)/R(t)-1);
gap10 = R(t+10) - actual rate in force. Balance vintages (1960, 1964, 1972: flat-rate scheme, no rate tabulated):
B = (outgo-income)/income; pct10 = 100*(B(t+10)-B(t)) = projected % increase in contribution income required (same concept).
Bases: E = earnings-linked benefits (demographic cost); P = price-linked benefits; POL = uprating policy in force
(E to 1979; P for 1982-2009 vintages; QR2010 'current announced practice'; triple lock for QR2014/2017)."""
import numpy as np, pandas as pd
V = {}
def bal(years, deficit, income): return dict(zip(years, np.array(deficit)/np.array(income)))
# 1960 (base 1959-60): level benefits, earnings +2%: deficit = outgo-income (Table 14)
V[1960] = dict(base=1959, pub=1960, kind='balance', actual=np.nan,
    E=bal([1959,1960,1961,1966,1971,1976,1981,1986,1991,1996,2001],[33,47,-37,4,6,-11,-59,-50,-59,-89,-124],[926,925,1035,1135,1240,1356,1486,1536,1592,1650,1709]))
V[1960]['P'] = V[1960]['E']; V[1960]['POL'] = V[1960]['E']
# 1964 (base 1964-65): constant earnings and benefits (= earnings basis)
V[1964] = dict(base=1964, pub=1964, kind='balance', actual=np.nan,
    E=bal([1964,1965,1966,1967,1968,1969,1970,1975,1980,1985,1990,1995,2000,2005],[-18,-65,-45,-20,7,36,-24,6,-41,-49,-60,-97,-143,-179],[1460,1549,1556,1559,1560,1560,1646,1732,1864,1938,2022,2116,2220,2329]))
V[1964]['P'] = V[1964]['E']; V[1964]['POL'] = V[1964]['E']
# 1972 Bill report (pub Nov 1972, scheme from 1975-76): earnings-constant; excess of income over outgo (Table 3)
exc = dict(zip([1975,1980,1985,1990,1995,2000,2005],[-87,28,63,103,217,355,465])); tot = dict(zip([1975,1980,1985,1990,1995,2000,2005],[3832,3981,4053,4074,4061,4056,4096]))
V[1972] = dict(base=1972, pub=1972, kind='balance', actual=np.nan, E={y: -exc[y]/(tot[y]+exc[y]) for y in exc})
V[1972]['P'] = V[1972]['E']; V[1972]['POL'] = V[1972]['E']
# 1975 Bill report (pub Feb 1975, scheme from 1978-79): required joint rate incl NHS 1.2; 4m contracted-out column
V[1975] = dict(base=1975, pub=1975, kind='rate', actual=16.5, E={1978:15.4,1979:15.5,1980:15.7,1981:15.7,1982:15.7,1995.5:16.6,2000.5:17.0,2005.5:17.7})
V[1975]['P'] = V[1975]['E']; V[1975]['POL'] = V[1975]['E']   # 1975 Act: pensions earnings-linked
# 1982 QR (base 1981-82): (a)(iii) earnings; (b)(i) prices 7%
V[1982] = dict(base=1981, pub=1982, kind='rate', actual=16.5,
    E={1981:15.3,1985:15.3,1990:15.3,1995:15.5,2000:15.7,2005:16.1,2010:16.9,2015:18.0,2020:19.1,2025:20.3},
    P={1981:15.3,1985:15.3,1990:14.9,1995:14.6,2000:14.4,2005:14.5,2010:14.9,2015:15.7,2020:16.3,2025:17.0})
V[1982]['POL'] = V[1982]['P']
# 1990 QR (base 1990-91): post-Social Security Bill 1990 (basis of the report's conclusions)
V[1990] = dict(base=1990, pub=1990, kind='rate', actual=17.5,
    E={1990:17.7,1995:18.3,2000:18.8,2010:20.0,2020:22.2,2030:25.0,2040:24.3,2050:23.0},
    P={1990:17.7,1995:17.2,2000:16.8,2010:16.3,2020:16.7,2030:17.2,2040:15.3,2050:13.0})
V[1990]['POL'] = V[1990]['P']
# 1995 QR (base 1994-95): Table 6 (existing legislation)
V[1995] = dict(base=1994, pub=1995, kind='rate', actual=18.25,
    E={1994:18.25,2000:18.8,2010:20.9,2020:24.2,2030:28.1,2040:28.7,2050:28.2},
    P={1994:18.25,2000:17.6,2010:17.6,2020:18.7,2030:20.0,2040:18.7,2050:16.8})
V[1995]['POL'] = V[1995]['P']
# 1999 QR (Cm 4406, base 1999-00; the file named 1992): Table 5.5, REG 1.5%
V[1999] = dict(base=1999, pub=1999, kind='rate', actual=20.25,
    E={1999:20.0,2000:20.2,2010:21.6,2020:23.2,2030:26.7,2040:27.6,2050:27.3,2060:27.6},
    P={1999:20.0,2000:19.9,2010:18.9,2020:18.1,2030:18.6,2040:17.1,2050:15.2,2060:14.0})
V[1999]['POL'] = V[1999]['P']
# 2003 QR (as at April 2000, base 2001-02): 1.5% real earnings
V[2003] = dict(base=2001, pub=2003, kind='rate', actual=19.85,
    E={2001:19.1,2010:20.7,2020:21.5,2030:24.4,2040:25.5,2050:26.2,2060:27.4},
    P={2001:19.1,2010:19.0,2020:17.7,2030:18.1,2040:17.3,2050:16.8,2060:17.1})
V[2003]['POL'] = V[2003]['P']
# 2010 QR (as at April 2005, base 2008-09): 1.5% real earnings variants; POL = current announced practice
V[2010] = dict(base=2008, pub=2010, kind='rate', actual=19.85,
    E={2008:21.1,2010:20.8,2020:19.9,2030:21.7,2040:23.2,2050:23.7,2060:26.6,2070:27.6},
    P={2008:21.1,2010:20.6,2020:17.3,2030:16.9,2040:16.4,2050:15.8,2060:17.1,2070:17.1},
    POL={2008:21.1,2010:20.3,2020:19.5,2030:21.1,2040:22.4,2050:22.8,2060:25.7,2070:26.7})
# 2014 QR (as at April 2010, base 2010-11): triple lock base case (POL); earnings variant (E); no price variant
V[2014] = dict(base=2010, pub=2014, kind='rate', actual=21.85,
    E={2010:21,2020:21,2030:26,2040:29,2050:30,2060:33,2070:34},
    P=None, POL={2010:21,2020:21,2030:25,2040:28,2050:29,2060:32,2070:34})
# 2017 QR (as at April 2015, base 2015-16): triple lock base case
V[2017] = dict(base=2015, pub=2017, kind='rate', actual=21.85,
    E={2015:23.5,2020:20.7,2030:23.2,2040:25.9,2050:26.0,2060:26.3,2070:26.4}, P=None,
    POL={2015:23.5,2020:20.7,2030:23.2,2040:25.9,2050:26.0,2060:26.3,2070:26.4})
def interp(prof, x):
    xs = np.array(sorted(prof)); ys = np.array([prof[k] for k in xs]); return float(np.interp(x, xs, ys))
rows = []
for rule in ['base', 'pub']:
    for t in range(1959, 2020):
        cands = [k for k, v in V.items() if v[rule] <= t]
        if not cands: continue
        k = max(cands, key=lambda k: V[k][rule]); v = V[k]; rec = dict(year=t, rule=rule, vintage=k, kind=v['kind'], actual_rate=v['actual'])
        for b in ['E', 'P', 'POL']:
            prof = v[b]
            if prof is None: rec[f'{b}_d10'] = rec[f'{b}_pct10'] = rec[f'{b}_gap10'] = rec[f'{b}_lvl10'] = np.nan; continue
            r0, r10 = interp(prof, t), interp(prof, t + 10)
            if v['kind'] == 'rate':
                rec[f'{b}_d10'] = r10 - r0; rec[f'{b}_pct10'] = 100 * (r10 / r0 - 1); rec[f'{b}_gap10'] = r10 - v['actual']; rec[f'{b}_lvl10'] = r10
            else:
                rec[f'{b}_d10'] = np.nan; rec[f'{b}_pct10'] = 100 * (r10 - r0); rec[f'{b}_gap10'] = np.nan; rec[f'{b}_lvl10'] = np.nan
        rows.append(rec)
D = pd.DataFrame(rows); D.to_csv('gad_pressure_series_1959_2019.csv', index=False)
print(D[D.rule == 'base'][['year', 'vintage', 'E_d10', 'P_d10', 'POL_d10', 'E_pct10', 'POL_pct10', 'E_gap10']].round(2).to_string(index=False))
