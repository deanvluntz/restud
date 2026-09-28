# HANDOVER — "Death, taxes, and immigration" (ReStud manuscript + Humboldt M.Sc. thesis)

Owner: **Sam Hamdane** (samham998@gmail.com). Consolidated **28 September 2026**. This file supersedes
`HANDOVER_ReStud_Project.md` (22 Sep) and `ReStud_Conversation_Summary_22Sep2026.txt`; they remain for history.
Project root on Sam's Mac: `~/quiet-instruments/` (the assistant sees it as the connected folder; in the Linux
sandbox it is `/sessions/<id>/mnt/quiet-instruments/`). The assistant's scratch folder `outputs/` holds builder
working copies, backups and the Python replica.

**Read order for a new agent:** (1) this file; (2) `quiet-instruments/CLAUDE.md` — binding rules plus a dated log of
every decision (kept current, append to it); (3) `diss_replication/README.md` — replication package (living document,
Social Science Data Editors template, with change log); (4) `manuscript_1964/README.md`.

---

## 0. Where things stand right now (28 Sep 2026, evening) — pick up here

1. **The project now lives in GitHub** (`deanvluntz/restud`, branch `claude/research-paper-synthesis-wqysr9`); the cloud
   container runs R (4.3.3, ARDL 0.2.5) and reproduces Sam's outputs to the last digit. Sam pulls with `git pull` on the Mac.
2. **Done today (all R-run, both samples):** table conventions (definitions once per document, step sentence first, widths);
   `robustness_tables.R` in Sam's panel order; `no_projection.R` (fiscal model without the projection, Sam's two-part criterion,
   inflation vs debt interest, EU placebo for each); further controls F1–F8 across `boe_tests.R`/`extra_tests.R`/`gad_obr_tests.R`;
   `primary_int_tme.R` (exploratory); permits without the basis step everywhere; `replication_1977.R` repaired; Figure 4 (1964)
   re-rendered; **combined manuscript Section 7 rewritten** (extended first, core corroboration — Sam reversed the order later on 28 Sep; new 7.1 and 7.2; CPI inflation added and selected by the rule, same verdict; appendix panels numbered B6–B32) and rebuilt
   (55 pp; `Hamdane_ReStud_Manuscript_combined.docx/.pdf` at the project root and in `manuscript_1964/`).
3. **Open for Sam:** (a) log vacancies/WAP meets both halves of his criterion on both samples (no_projection.R Table B19) —
   whether it enters the headline rule is his call; (b) confirm the series IDs of the four ONS BoP/national-accounts splices and
   the debt-interest splice (`determinants/SOURCES_2017_2019.md`); (c) read Section 7 (7.1/7.2; 57 pp; cuts are his); (d) the
   1964-only manuscript is parked; the Online Appendix is rebuilt (B6–B35) from `tables_1977.json`; `run_all_1977.R` needs
   `SKIP_VINTAGE_CHECK=1` only in the container.
4. Earlier open decisions remain in §9.

---

## 1. The research

**Question.** Why did policy-controlled (non-EU) immigration to the UK rise for four decades under governments that
all promised to cut or control it?

**Hypothesis.** The Treasury used admission of working-age migrants as a low-salience fiscal instrument to offset
projected population ageing, instead of visible tax rises or borrowing. The regressor of interest is the government's
own **projected ten-year change in the old-age dependency ratio** (`delta_oadr_10`), reconstructed from every
National Population Projection (NPP) vintage (1963-based to 2018-based; plus the 1958-based projection in the GAD's
1960 Quinquennial Review for the permits test). Vintage in force = latest **base year** ≤ t (never claim a projection
was *published* before the year it explains).

**Falsification design (the core of the argument).** The projection should predict controlled flows but not
uncontrolled ones (EU placebo null); only the *old-age* ratio should matter (total-dependency placebo null beside the
OADR); the one-year horizon carries nothing, fit improves with the horizon to ten years; it survives a zero-migration
reconstruction of the projection (so it is not the government's own migration assumption); it moves the policy lever
(work permits); realised spending, current deficits, borrowing forecasts, macro controls cannot replace it.
The two "firsts" (migration as a fiscal instrument; the DR projection as determinant) are **one claim**: the second
operationalises the first; justify them jointly by the falsification design.

**Estimating equation (fiscal specification, primary; R object `tax_spec`):**
```
mig_rate_t = a + rho*mig_rate_{t-1} + b1*delta_oadr_10_t + b2*tax_workpop_{t-1} + b3*ln_debt_gdp_{t-1} - b4*unem_{t-1} [+ basis_pre1975 on the extended sample]
```
`mig_rate` = ln inflow per 1,000 of last year's working-age population (WAP). Trend specification (`trend_spec`)
replaces tax with a trend; secondary only. Inference: `sandwich::vcovHAC` defaults everywhere; manuscript reports
HAC Wald F; thesis appendix reports classical nested F. Bounds test: PSS case 3 via R package **ARDL**
(Natsiopoulos, Tzeremes and Finnan, 2026, v0.2.5 — cite wherever the bounds test is described).

**Model section (manuscript §3):** cost function `C_t = ½φ_τ s_τ(τ_t − τ̄)² + ½φ_m s_m m_t² + φ_u u_{t−1} m_t + ½ψ(m_t − m_{t−1})²`;
linear rule `m_t = ρ m_{t−1} + β₁Δ_t + β₂τ_{t−1} + β₃B_{t−1} − β₄u_{t−1}`; Proposition 1 (P1–P7; proof Online Appendix D;
P2 needs assumption A2; P6 = EU placebo, P7 = total-ratio placebo). NB: the new "Proposition 1 / 2" empirical framing
(§6) is a different use of the word — do not confuse with the model's Proposition 1.

---

## 2. Deliverables (five documents, deliberately different)

| Document | File (in `quiet-instruments/`) | Sample | Status |
|---|---|---|---|
| Thesis (Humboldt, Economic History, Prof. Nikolaus Wolf; 2nd supervisor Prof. Michael Burda) | `Masters_Thesis_Draft_updated.docx` | **1979–2019, N = 41** (title fixes it) | Sam edits by hand; assistant makes surgical python-docx edits |
| Manuscript, core | `Hamdane_ReStud_Manuscript.docx` (+ `_45pp`, `_30pp`) | **1977–2019, N = 43** | ~50 pp (target ≤ 45; **Sam makes the cuts himself**) |
| Manuscript, 1964 | `Hamdane_ReStud_Manuscript_1964.docx` | **1965–2019, N = 55** (series B + basis step) | ~57–59 pp |
| Manuscript, combined | `Hamdane_ReStud_Manuscript_combined.docx` | core 1977–2019 **and** extended 1965–2019 side by side | ~60 pp |
| Online Appendix | `Hamdane_Online_Appendix.docx` | core | A projection reconstruction, B time series/inference, C delay quantified, D proof, E background, F digital refs |

Titles: manuscript "Death, taxes, and immigration: how Britain’s Treasury delayed the inevitable (1977–2019)"
(12 pt, one line, Oxford comma); the 1964 and combined versions say "(1964–2019)". Abstracts differ by design:
manuscript "nearly quintupled", thesis "tripled" — never "fix" without asking.

---

## 3. Architecture

### 3.1 Folders
```
quiet-instruments/
├─ CLAUDE.md                          binding rules + dated decision log (append to it)
├─ HANDOVER.md                        this file
├─ Masters_Thesis_Draft_updated.docx  thesis (Sam's hand-edited foundation)
├─ Hamdane_ReStud_Manuscript*.docx/.pdf, Hamdane_Online_Appendix.docx/.pdf
├─ Thesis_Conclusion_redraft_26Sep2026.md    full conclusion redraft — NOT installed, awaits Sam
├─ Thesis_cuts_for_approval.md        fluff-pass proposals (rows marked APPLIED approved; others not)
├─ Dell_1991_research_brief.md, Brittan_1969_research_brief.md (+ OCR text)   source briefs
├─ Guide_policy_lever_and_stability_tests.md
├─ diss_replication/                  CANONICAL R replication (data + scripts + README)
├─ manuscript_1964/                   builders, sources (ms1964/), figures, tables_*.json for 1964 + combined docs
├─ manuscript_variants/               copies of the core-manuscript builder sources
└─ archive_march2026/                 superseded March-databank run — read-only history
```
`outputs/` (assistant scratch, not visible to Sam): live builder copies for the core manuscript
(`build_docx.js`, `manuscript_part1/2.md`, assemblers), thesis backups `Thesis_before_*.docx` (one per edit), and the
Python replica `outputs/pyrep/` (`hac.py` = vcovHAC replica; `m1964.py` builds the extended data frame `DF` with
short names y, p, L_y, L_tax, L_debt, L_un, step, g…; `m1964_*.py`, `prop1_prep.py` = latest replica work).

### 3.2 R layer (`diss_replication/`, R 4.6.1 on Sam's Mac; packages lmtest, sandwich, car, tseries, readxl, ARDL, urca, jsonlite)
Workbook `Dissertation_Dataset_Final.xlsx`: sheets `ReformSPA` (core data 1975–2020), `PubShiftOADR_ZM`,
`FixedSPA`, `Sheet2`, `1964` (extended data 1962–2019, Sam's paste of the extension CSV plus his own columns
`lt_inf_exp`, `st_inf_exp`, `tfp_growth`, which END IN 2016).

| Script | Role |
|---|---|
| `replication.R` / `replication_1977.R`, `build_tables*.R` → `tables.json` / `tables_1977.json`, `verify*.R` | thesis (1979) / core manuscript (1977) numbers for the builders |
| `appendix_exhibits.R` / `_1977.R` / `_labpop_1977.R` | console panels (thesis Appendix A screenshots; manuscript exhibits) |
| `run_all.R`, `run_all_1977.R`, `run_all_thesis_extras.R` | runners (functions wrapping sink/on.exit). `run_all_1977.R` now also runs `core_1977_boe.R`, `boe_appendix.R`, `extra_tests.R`, `gad_obr_tests.R` with SAMPLE = "1977" set globally |
| `run_all_1964.R` | runs `extension_1964.R`, `robustness_tables.R`, `no_projection.R`, `extension_1964_boe.R`, `extra_tests.R`, `gad_obr_tests.R`, `boe_appendix.R`, `primary_int_tme.R`, `manuscript_1964.R` with SAMPLE = "1964" set globally and `DEFINED` reset (definitions once per run). `run_all_1977.R` now also runs `robustness_tables.R` |
| **`tables_common.R`** | shared printer + **variable dictionary `DEFS`** (every name shown in a panel is defined under it); `fit()` (drops constant columns, stores data as attr "dd"); `panel()`; `SEC_END = 2016`, `ONS_VARS` — any column using a 2017–19-extended series is re-estimated to 2016 and printed as "proj, to 2016" |
| **`data_common.R`** | builds `df` with canonical names for `SAMPLE` "1964" (extended, `mig_rate` = series B, `STEP = basis_pre1975`, Y0 1965) or "1977" (core, non-EU, Y0 1977); merges BoE extras, A27 taxes, GAD, prop1 file, ONS extension; `BASE` = fiscal spec; `BOE_END = 2019` (main); `SIX` = the six macro controls |
| `extension_1964.R` | extended sample table: `mig_rate` vs `mig_rate (no step)` + alternative series as one text line |
| **`robustness_tables.R`** | both samples, Sam's order (28 Sep): 1 tax burden (a/b), 2 unemployment measures, 3 level vs change, 4 permits (no step), 5 gross, 6 total DR, 7 emigration, 8 pre-1975 regime steps (ext), 9 horizons, 10 British, 11 sub-samples + second lag |
| `boe_tests.R` (wrappers `extension_1964_boe.R`, `core_1977_boe.R`) | further controls part 1: F1 labour demand beside unemployment (substitutions as text), F2 wages/rates/expectations/TFP/institutions, F3 all six |
| `boe_appendix.R` | Online Appendix Tables B6 (labour demand, NICs) and B7 (EU margin, permits) + revisions check |
| `extra_tests.R` | further controls part 2: F4 realised spending (A28), F5 current fiscal balance |
| `gad_obr_tests.R` | further controls part 3: F6 GAD contribution pressure; F7 OBR borrowing forecasts; F8 debt dynamics |
| **`no_projection.R`** | the fiscal model without the projection (both samples): A candidate screen on Sam's two-part criterion (fiscal candidates included), B pre-stated selection, C1/C2 models (int_tme, inflation, both), D OOS forecasts, E gross + EU placebo. **`primary_int_tme.R`**: exploratory int_tme-as-primary pipeline |
| `replication_1977.R` | core pipeline; 28 Sep: DR/pub-dated 1977–78 from `dr_corrected.csv`, pub-dated N = 42, `SKIP_VINTAGE_CHECK=1` skips the .xls reconstruction |
| `manuscript_1964.R` | writes `tables_1964.json` (Tables 2/3/4/B2, bounds with step as fixed regressor, 24 Figure 4 rows, timing placebo, second lag, bootstrap bias correction, permits, British, stability) |
| `permits_check.R`, `appendix_permits.R`, `appendix_british.R`, `stability_check.R`, `spending_check.R`, `determinants_check.R`, `tax_grid.R` | thesis Section 9 / Appendix A exhibits |

### 3.3 Document builders (Node `docx` + Python; run in the sandbox)
- Core manuscript: `outputs/build_docx.js` with `manuscript_part1.md` (§1–4) and `manuscript_part2.md` (§5–9 + refs);
  markers `[[TABLE1..4]]`, `[[FIGURE1..4]]`, `[[EQ]]…[[/EQ]]`, `[[REFS]]`, `[[TITLENOTE]]`. Env: `MS_PART1`, `MS_PART2`,
  `MS_OUT`, `MS_FIGSCALE`. 45pp: `assemble_r45v2.py`; 30pp: `assemble_r30.py` + `restructure_r30.py` from FROZEN
  `*_variantsrc.md`. `node build_online_appendix.js`.
- 1964 / combined: `cd manuscript_1964 && node build_docx_1964.js && node build_docx_combined.js` (sources
  `ms1964/manuscript_part1/2_1964.md`, `_combined.md`; needs `tables_1964.json`, `tables_1977.json`, `figure1_1964.png`,
  `figure2_1964.png`, `figure3.png`, `figure4_1964.png` in that folder). Then copy the .docx up to `quiet-instruments/`
  and `soffice --headless --convert-to pdf`; check pages with pypdf/pdfinfo.
- Figure 4 (1964) is re-rendered from `tables_1964.json` by `outputs/pyrep/m1964_figs.py`.
- Thesis: python-docx at run level (replace inside the run; deepcopy a paragraph as template); always save a
  `outputs/Thesis_before_<tag>.docx` backup first. Contents/figure-list page numbers are PAGEREF fields — never set
  updateFields; Sam refreshes with Select All + F9.

---

## 4. Binding rules (from CLAUDE.md — follow exactly)
- **RULE 0:** every uploaded thesis/manuscript copy is the new foundation; diff it first; carry pipeline work onto
  his copy; flag stale bases and adjudicate.
- **RULE 1 (non-negotiable):** never revert or "improve" Sam's manual edits (wording, punctuation, layout, spacing)
  without item-by-item consent, even apparent mistakes (flag and ask). After every merge disclose every divergence and
  which side won. Typographic normalisations allowed but disclosed.
- Oxford commas in prose, headings, titles (not in Harvard author lists or quotations). Quotations never altered.
- Thesis: first person singular; Harvard references with hanging indent (left 457200 EMU, first-line −457200) on
  every entry; access dates only on web pages/news/datasets/official publications, randomly "(Accessed: 20 June
  2026)" or "(Accessed: 21 June 2026)"; none on academic sources; remove uncited references; **no page numbers in
  in-text citations**; Acts named in text only (no reference entries).
- Never trim the full manuscript for length unasked. Don't state results in documents before Sam's R run confirms them.
- **Table conventions (27 Sep):** one short name per variable, identical in rows, column titles and text; every name
  shown defined under the panel; column titles `+name` (added), `name for X` (replaces X), `name only` (replaces the
  projection); never two names for one concept in a table; unemployment measures and vacancy controls in separate
  panels; column names must never run together; no mention of "the thesis" in manuscript scripts ("core sample,
  1977–2019").
- Tax measures: headline NAT/WAP; Sam's OADR-denominated measure is `L_tax_oadr_0` = tax £bn / lagged current OADR —
  never invent denominators; ask first.
- `ln_debt_gdp` is the ANNUAL LOG CHANGE in debt/GDP; never describe it as a log level.
- Framing constraints: Hemming & Kay (1982) — the projection *horizon* hid SERPS costs; they never allege falsification.
  Dell (1991) does not support "misrepresented/admitted". Brittan (1969) contains nothing on immigration/ageing — use
  only for Treasury institutional character/secrecy. British-placebo descent composition is "likely", cited to
  Sriskandarajah & Drew (2006) — never stated as fact. Supply-side variables (relative income, youth share, networks)
  are omitted-variable checks, never mechanism. Pemberton (2018) witness seminar: paraphrase only.

---

## 5. Data and variables (canonical names, `tables_common.R`)

| Name | Definition / source |
|---|---|
| `mig_rate`, `L_mig_rate`, `L2_mig_rate` | ln inflow per 1,000 WAP(t−1). Core: non-EU citizens (IPS 3.01, non-EU residence). Extended "series B": non-British citizens to 1974 (LTIM 2.00) spliced unscaled with non-EU from 1975 |
| `basis_pre1975` | step = 1 before 1975 (change of basis; long-run ≈ 0.25 = 1975–80 EU share of non-British inflows) |
| `mig_gross`, `L_mig_gross` | ln gross inflow (persons) |
| `delta_oadr_10` (and `_1,_3,_5,_7`) | projected h-year OADR change, vintage in force by base year; 1963–74 from pre-1975 vintages (tenth year interpolated where unpublished); 1958–62 from GAD QR1960 Table B (permits test only) |
| `delta_dr_10` | projected total-dependency change (placebo) |
| `L_tax_workpop` | national accounts taxes per WAP, £000, 2024–25 prices (OBR HPFD + HMT deflator), t−1 |
| `L_ln_debt_gdp` | annual log change in PSND/GDP, t−1 |
| `L_unem` | ILO/LFS rate (BoE A50 before 1975) — **baseline** |
| `L_unem_head`, `L_post98` | headline measure of the day: M5 administrative/claimant as published to 1997, ILO from 1998; step from 1999 |
| `L_unem_boe`, `L_unem_claim` | M5 spliced; A50 claimant-count rate 1945–2016 (ONS discontinued consistent claimant series under Universal Credit) |
| `L_vac_wap`, `L_vac_unem`, `L_dln_vac_unem` | vacancies per 1,000 WAP; vacancies/unemployed; its annual log change |
| `L_wage_infl`, `L_real_wage`, `L_tbill`, `L_real_rate`, `st_inf_exp`, `lt_inf_exp`, `inf_exp_1y`, `inf_exp_5_10y`, `L_tfp`, `L_replace`, `L_union` | BoE Millennium M1/M6/Q7/A52b and Sam's NIESR columns; expectations are start-of-year and enter **contemporaneously, never lagged** |
| `L_tax_nics`, `L_tax_nics_emp`, `L_tax_nics_er`, `L_tax_inc` … | Millennium A27 central-government receipts per WAP (FY t−1/t → t) |
| `L_int_tme`, `L_int_gdp`, `L_gilt`, `L_r_g` | debt interest share of TME / of GDP (A28), gilt yield, r−g |
| `L_grants_gdp`, `L_grants_wap`, `L_tme_gdp` | realised spending (A28) |
| `L_psnb_gdp`, `L_psnd_gdp`, `L_tax_gdp` | current fiscal balance (HPFD) |
| `fc_budget`, `fc_planned`, `fc_medium` | OBR historical official borrowing forecasts (spring Budget; from 1971) |
| `gad_earn`, `gad_policy`, `gad_gap` | GAD Quinquennial Reviews: projected 10-yr change in the required joint Class 1 NI rate (earnings basis / policy basis) |
| `L_real_eri`, `L_ca_gdp`, `L_trade_bal`, `L_tot`, `L_labour_share`, `L_real_house`, `L_growth`, `L_inflation` | exchange rate, BoP, terms of trade, labour share, real house-price growth (Millennium A1/A33/A36/A56/A32) |
| `eu_rate`, `brit_rate`, `permits`, `lever` | EU inflows rate (1977+); British inflows rate (LTIM 2.00, all origins, from 1964); work permits (Bauer & Salt 2020 Fig. 1, thousands); permits + Commonwealth vouchers 1962–72 |

Key data files in `diss_replication/determinants/`: `extension_1962_2019_dataset.csv`, `extension_1962_2019_extras.csv`,
`npp_1963_1974_oadr.csv`, `npp_1963_1973_totals_by_vintage.csv`, `fiscal_block_1962_1974.csv`,
**`pre1963_block_1956_1962.csv`** (HPFD fiscal block + 1958-based WAP/OADR), `boe_millennium_annual_1955_2017.csv`,
`boe_extras_1962_2019.csv`, `boe_a27_tax_variants_1956_2019.csv`, **`boe_prop1_1955_2019.csv`** (claimant count, ERI,
BoP, ToT, labour share, house prices, Q7 expectations, labour_gov share), **`ons_extension_2017_2019.csv`** (splice
method in its `source` column), `obr_psnb_forecasts_by_vintage_1970_2025.csv`, `gad/` (13 GAD reports: OCR text,
row-level extraction CSVs, `build_gad_series.py`, `gad_pressure_series_1959_2019.csv`), `work_permits.csv`,
**`commonwealth_vouchers_1962_1972.csv`** (Bauer & Salt Table 9), `ips_*`, `home_office_*`, `maddison_gdppc.csv`,
`unwpp_youth_share.csv`, `projection_revisions_1975_2019.csv`.
NB: `gad/…/gadqqreview1992.pdf` is actually the **July 1999** review (Cm 4406); `gadqqreview1995.pdf` is the third
review under the 1975 Act (period 1985–90).

Series that still END IN 2016 (cannot be extended consistently; N row shows it): claimant count, real short rate,
replacement ratio, union density, grants/TME, A27 detail, gilt/r−g, Sam's `st_inf_exp`/`lt_inf_exp`/`tfp_growth`
(he could add 2017–19 to sheet `1964`).

---

## 6. Established results (R-confirmed unless marked replica)

**Headline.** Core 1977–2019: projection 0.0082 (s.e. 0.0015); LR ≈ 2.0% corrected [0.9, 2.8]; bounds F 6.16, t −4.09;
HAC F(1,37) = 32.23. Thesis 1979–2019: 0.0081. Extended 1965–2019 (series B + step): 0.0081 (no step 0.0080), N 55;
bounds F 7.18 (p .003), t −4.95; LR projection 0.0155; bias-corrected ρ 0.46→0.56, LR 1.8% [1.2, 2.6].
Extended is "unchanged when extended to 1965", **not** "visible in the 1960s" (1965–78 projection collinear with time).

**Placebos/robustness.** Total-DR placebo null beside OADR (ext 0.0004 p .48; alone 0.0015 p .001 — report both); EU
placebo null (0.0030 p .41); zero-migration 0.0075; one-year horizon null, monotone to ten years (core and now
extended, replica); timing placebo p = 0.018; stability CUSUM p .94–.99; permits respond (0.61 thousand per point,
1965–2008) and the DR placebo does not; British inflows respond partially (core 0.0065–0.0070; with them as control
the projection is ≥ 0.0057 — a lower bound; extended LTIM version 0.0031 p .08, replica); supply-side controls,
trend, Act steps, growth, inflation, emigration: projection 0.0078–0.0094.

**BoE-era controls (to 2016, R-confirmed).** Vacancies are the one control that lowers the projection (to ~0.0067 ext /
0.0059 core, demand share ≈ a quarter); employee NICs dominate employer NICs (NICs fund the state pension — central to
the mechanism; manuscript says so); Kronmal null; EU margin responds to vacancies not the projection. **Primary
specification unchanged** (decided 27 Sep: no candidate is theory-prior and a > 4-AIC improvement on both samples;
dropping unemployment for vacancies rejected as p-hacking; `+dln_vac_unem` kept as robustness only).

**Can anything else do the projection's work? (R-confirmed).** Realised spending, election cycle, OBR era, current
PSNB/PSND (PSNB *instead* of the projection has the wrong sign and fails RESET), OBR borrowing forecasts (wrong sign in
its place), GAD earnings-basis contribution pressure (predicts alone, null beside the projection, 11–18 AIC worse in
its place; the price/policy basis projected the required rate to FALL 1981–2009 — the indexation point in data),
debt interest/TME (the one non-demographic pressure with a signal: +0.02, projection rises to ~0.010). Without the
projection the fiscal model fails RESET; six macro controls do not recover it.

---

## 7. Newest work (NOT yet run in R — replica numbers; verify)

**Main results to 2019** (Sam, 27 Sep; supersedes "do not chase ONS 2017–19"). Replica to 2019: `+vac_wap` proj
0.0072 ext / 0.0064 core; `vac_wap for unem` 0.0068 / 0.0059; `+dln_vac_unem` 0.0079 / 0.0078; `+wage_infl`
0.0066 / 0.0068; `+int_tme` 0.0096 (int_tme 0.018 p .05) / 0.0108 (0.021 p .02).

**Unemployment of the day.** `L_unem_head` alone: proj 0.0082–0.0083, AIC +1.8/+1.4 worse than ILO; with `L_post98`:
step +0.17 (p .04) ext / +0.16 (p .07) core, proj 0.0072/0.0070. The step coincides with the 1998 policy era, so it
cannot separate a measurement break from policy. Claimant count as baseline: indistinguishable from ILO (to 2016).
Recommendation given: keep ILO baseline — Sam to decide.

**Proposition 1 → 2** (`prop1_model.R`). Pre-stated rule: forward selection without the projection; admissible only if
observed throughout both samples, HAC p < .10 on both, same sign; lowest summed ΔAIC enters; max 5. Pool (after Sam
removed party and origin-supply variables): vacancies, V/U, wage inflation, inflation, expectations (end 2016 → not
selectable), T-bill, int_tme, real ERI, current account, trade balance, ToT, labour share, real house growth, growth.
Replica selects **L_wage_infl, L_real_house, L_real_eri**. Projection on top: **0.0059 (p .002, ΔAIC −7.2) extended;
0.0055 (p .06, ΔAIC −2.7) core**; to 2016: 0.0057 / 0.0046 (p .10). A-priori fiscal model (base + `L_int_tme`):
projection 0.0096 / 0.0108, ΔAIC −27 / −19. History: with the youth share and party allowed, the rule had picked
youth + ToT which absorbed the projection on the core (0.0022, p .18) — Sam then excluded non-fiscal variables;
disclose this path honestly if the analysis is written up. Exchange rate/BoP were null as additions to the fiscal model.
Earlier-flagged: a Labour-government dummy was +0.12 (p .02) on the core beside the projection — dropped by Sam, but it
bears on the conclusion redraft's sentence that the post-1997 rise "would have occurred under either party".

**Permits long** (`permits_long.R`): permits 1959–2008 0.58 (p < .001) using the 1958-based projection (nearly flat
29.5–29.9, adds lever observations not identifying variation); permits + Commonwealth vouchers 1964–2008 0.61.
Before 1962 permits cover aliens only; 1962–72 Commonwealth workers entered on vouchers; from 1973 permits cover both.

---

## 8. Lessons learned / pitfalls
- **R sessions leak `SAMPLE`**: shared scripts read the global; runners now set it. Always check panel titles say the
  intended sample.
- Web: ONS time-series JSON works via the in-app browser (`fetch('/…/timeseries/<cdid>/<dataset>/data')` from
  ons.gov.uk); BoE IADB works with `html.x=yes` (CSV comes back binary); BIS SDMX works via browser fetch (XML);
  Land Registry UK HPI JSON works. Never use curl/python to bypass a failed fetch.
- OCR of scanned PDFs: `pdftoppm -r 200 -gray` then `tesseract --psm 6` in batches under ~140 s per call
  (tool timeout); background processes die between calls.
- Line-number assemblers are fragile (30pp uses frozen sources); match paragraphs by opening words.
- python-docx: `paragraph.text` includes hyperlink text, runs do not; uploads are read-only.
- Collinearity traps: unemployment + vacancies levels (V/U is their quotient) — never report "both matter";
  trend columns VIF 60–75 (sign checks only).
- Python previews are for layout; never put replica numbers into documents as final.
- Keep the README change log and CLAUDE.md log updated after every change.

---

## 9. Open items awaiting Sam
1. Run `run_all_1964.R` and `run_all_1977.R`; paste output (new scripts untested in R; watch for errors in
   `data_common.R` splice block, `tables_common.R` secondary-row code, `prop1_model.R` `load_sample`).
2. Decide: unemployment baseline (ILO vs headline-of-the-day with 1998 step); whether/how to write Proposition 1/2 into
   the manuscripts; whether to add GAD/indexation observation to Section 4/5 text; permits-long and vouchers in text.
3. Supply 2017–19 values for `st_inf_exp`, `lt_inf_exp`, `tfp_growth` (sheet `1964`) if wanted in main results.
4. Thesis conclusion redraft (`Thesis_Conclusion_redraft_26Sep2026.md`) not installed; its "either party" sentence
   needs reconsidering.
5. Manuscript length cuts (Sam's); Brittan edition/year confirmation; Dell §5.2.4 wording decision; remaining
   `Thesis_cuts_for_approval.md` rows; stability.png not yet in thesis Appendix A; British-placebo write-up choices;
   IPS-by-reason composition result undecided; Sam may receive raw permit figures from Salt/Bauer (swap in if so).
6. Manuscript "to 2016" wording and Figure 4 labels to update after the R run; rebuild all three manuscripts + PDFs.
7. Earlier open items: Online Appendix own reference list?; replication package master script; literature to verify
   before citing (Dustmann & Frattini 2014; Facchini, Mayda & Mishra 2011; Aspide & DiGiuseppe 2025).

---

## 10. How to work with Sam
- He wants rigour and honesty over flattering results; report failures and weak spots plainly (he asked for the
  Prop 1/2 test precisely to stress the claim).
- He makes the substantive choices; propose, show numbers, recommend, then wait. Items explicitly rejected by him:
  cross-country comparison, age-composition test, splicing Tier 2 onto permits, splicing permits onto IPS reason data,
  fiscal-impact literature section, ONS LTIM 2017–19 chasing (reversed on 27 Sep — now use ONS for 2017–19),
  party dummy, origin-country variables in the fiscal model, dropping unemployment for vacancies.
- Concise replies; tables only where they carry numbers; always say what he needs to run and paste.
