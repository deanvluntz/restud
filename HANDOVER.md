# HANDOVER — "Death, taxes, and immigration" (ReStud manuscript + Humboldt M.Sc. thesis)

Owner: **Sam Hamdane** (samham998@gmail.com). First consolidated 28 September 2026; **updated 1 October 2026** with
everything since (GitHub cloud sessions, 28 Sep night – 1 Oct). Supersedes `HANDOVER_ReStud_Project.md` (22 Sep) and
`ReStud_Conversation_Summary_22Sep2026.txt` (history only).

**Where the project lives.** GitHub repository `deanvluntz/restud`, working branch **`claude/research-paper-synthesis-wqysr9`**
(all work since 28 Sep; `main` was merged into it on 30 Sep and holds only Sam's uploads plus a GitHub Actions workflow that
was later deleted on the branch). Sam's Mac copy is `~/quiet-instruments/`; he updates it with `git pull` (if a local edit
blocks the pull: `git stash; git pull; git stash pop`). New files since the handover was created are also collected in
**`handover_2026-10-01/`** (see its `MANIFEST.md`) for copying by hand.

**Read order for a new agent:** (1) this file; (2) `CLAUDE.md` — binding rules + dated decision log, entries (1)–(57) since
28 Sep (keep appending); (3) `diss_replication/README.md` — replication package with change log (rows up to (55)); (4)
`Glover_2001_HomeOffice_2002_research_brief.md` (contains **Sam's own notes** — pull before editing, never change his entries);
(5) `manuscript_1964/README.md`.

---

## 0. Where things stand (1 Oct 2026) — pick up here

1. **Phase: specification testing.** Since 28 Sep night Sam has been testing the model; **no manuscript, table or document
   has been updated since 28 Sep** (his rule: outputs change only when he decides). The three manuscripts and the Online
   Appendix therefore still show the 28 Sep specification (published rate DV, unemployment, B-table numbering of that day).
2. **Current core specification (decided 30 Sep, CLAUDE.md (41)):**
   ```
   mig_gross_t = a + rho*mig_gross_{t-1} + b1*delta_oadr_10_t + b2*tax_workpop_{t-1} + b3*ln_debt_gdp_{t-1} + b4*ln_vac_unem_{t-1}
   ```
   6 parameters; gross non-EU inflows in log persons (no denominator, **no basis step**, no rescaling); tax burden = national
   accounts taxes per working-age person (real, HMT GDP deflator); `ln_debt_gdp` = annual log change in debt/GDP; labour term =
   log vacancies/unemployed (V/U) in place of unemployment; projection at t (timing A). **`int_tme` is OUT** (its significance
   came only from the 2008–13 reclassification of the public-sector banks — confirmed by decomposition on 1 Oct, see §6).
   R-run: projection **0.0072 (1965–2019) / 0.0067 (1977–2019)**, p < .001; every term significant on both periods; AIC
   −101.0 / −76.9; BG(2) .043 / .019; Chow at 1977 F 1.07 (p .40). In code: `setdiff(CORE_A, "L_int_tme")` (`CORE_A` in
   `screen_engine.R` still holds the old 7-parameter list).
3. **Samples / protocol (agreed 30 Sep, (35)):** ONE main reference = **Citizen sample 1965–2019** (data span 1964–2019; 1964 is
   only the lag, so titles must read **1965–2019**); the same specification on 1977–2019 as robustness (no re-selection); Chow
   test at 1977 (classical F + HC3, not HAC); residence-based (`res_noneu_k`) and any-residence citizenship (`cit_noneu_k`)
   outcomes. `screen_engine.R::run_protocol()`; `main_spec_protocol.R`. KEY DATA FACT: from 1977 the "Citizen" and "Origin"
   outcomes are the same series (r 1.000) — "Origin sample" = the 1977–2019 subperiod, not a residence-based DV.
4. **Decisions still open (Sam):** (a) core with tax only vs the clean one-term burden `taxrint_xb` (taxes + debt interest
   excl. banks − inflation × net debt, per WAP; passes the protocol but weaker on 1977–2019 without 2002–07); (b) timing A
   (projection at t, recommended) vs B (t−1); (c) step vs no step is settled for the core (no step) but the published-rate
   variant remains in the parallel runs; (d) whether any forward-borrowing term goes in as robustness (§7.6); (e) the text
   items in §9; (f) rebuilding the manuscripts once the core is final.
5. **Text-only items decided (to write when the documents are updated):** points of failure explained in the text, not more
   tests (IPS lag-2 serial correlation, dating conventions, zero-migration projection only from 1975); headline tax rises
   sparse outside the sterling crisis and the first Thatcher government; positive tax coefficient vs the voter-side fiscal
   literature; temporary offset (agreement with Rowthorn 2008 / Pensions Commission 2004, not rebuttal); unemployment splice
   justified explicitly; wage inflation framed as the incomes-policy instrument of its era (robustness line only); British
   flow = relative-income pull vs controlled flow = projection (asymmetry); Sam's paragraph on how the specification was built.

---

## 1. The research

**Question.** Why did policy-controlled (non-EU) immigration to the UK rise for four decades under governments that all
promised to cut or control it?

**Hypothesis.** The Treasury used admission of working-age migrants as a low-salience fiscal instrument to offset projected
population ageing, instead of visible tax rises or borrowing. Regressor of interest: the government's own **projected
ten-year change in the old-age dependency ratio** (`delta_oadr_10`), reconstructed from every NPP vintage (1963-based to
2018-based; 1958-based from GAD QR1960 for the permits test). Vintage in force = latest **base year** ≤ t (never claim a
projection was *published* before the year it explains). Sam's framing: the model is a short-horizon fiscal reaction rule
(10-year projected deterioration), NOT a claim that migration permanently offsets ageing ("delayed the inevitable").

**Falsification design.** Projection predicts controlled flows, not uncontrolled ones (EU placebo null); only the old-age ratio
matters (total-dependency placebo null beside the OADR); one-year horizon carries nothing; survives a zero-migration
reconstruction; moves the policy lever (work permits; IPS work route, not study); realised spending, deficits, borrowing and
growth forecasts, macro controls cannot replace it. "First to test migration as a fiscal instrument" and "first to use the DR
projection" are ONE claim.

**How the specification was built (Sam's answer, to refine himself, (48)):** the core comes from Treasury/OBR/government
literature on how fiscal policy is set and has been consistent throughout; macro data assessed the other constraints; the
backward look from the data for missing constraints (borrowing/expenditure) was limited and justified by Treasury secrecy and
the thin literature. Sam's criterion for extending the model: a candidate must (a) improve the model WITHOUT the projection and
(b) improve it BESIDE the projection (own HAC p < .10 and lower AIC in both), on both samples.

**Model section (manuscript §3):** cost function and linear rule as before (Proposition 1, P1–P7, proof Online Appendix D).
No "proposition" language for the empirical Prop-1/Prop-2 framing in the text (Sam, 28 Sep).

---

## 2. Deliverables (unchanged since 28 Sep — all awaiting the final core)

| Document | File | Sample | Status |
|---|---|---|---|
| Thesis (Humboldt; Prof. Nikolaus Wolf; 2nd Prof. Michael Burda) | `Masters_Thesis_Draft_updated.docx` | 1979–2019, N 41 | Sam edits by hand |
| Manuscript, core | `Hamdane_ReStud_Manuscript.docx` (+ `_45pp`, `_30pp`) | 1977–2019 | ~50 pp; cuts are Sam's |
| Manuscript, 1964 | `Hamdane_ReStud_Manuscript_1964.docx` | 1965–2019 | parked ("ignore for now") |
| Manuscript, combined | `Hamdane_ReStud_Manuscript_combined.docx` | extended first, core corroboration | 57 pp, 28 Sep spec |
| Online Appendix | `Hamdane_Online_Appendix.docx` | — | B6–B35 numbering of 28 Sep |

When the documents are next rebuilt: titles "(1964–2019)" → **"(1965–2019)"**; the new core replaces the rate/unemployment
specification; int_tme comes out of every table; the replication file `replication_core_spec.R` replaces the manuscript scripts
(Sam inspects it first). The thesis keeps its own sample and abstract wording ("tripled"); never "fix" without asking.

---

## 3. Architecture

### 3.1 Folders (repository root = Sam's `quiet-instruments/`)
```
CLAUDE.md, HANDOVER.md, README.md               rules + log; this file
*_research_brief.md                              Dell 1991, Brittan 1969, Glover 2001 / Home Office 2002 (Sam's notes section)
Masters_Thesis_Draft_updated.docx, Hamdane_*.docx/.pdf   documents
HO1978.pdf … HO1988.pdf, 1970_2009 HO immigration *.pdf   Home Office Command Papers / statistical volumes (Sam's uploads)
a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx    BoE Millennium dataset v3.1 (read directly by several scripts)
diss_replication/                                CANONICAL R replication: data, scripts, README, outputs (*_output.txt)
  determinants/                                  every source series (CSV/xlsx), with subfolders gad/, ifs/, wb/, demscore/
manuscript_1964/                                 builders, ms1964/ sources, figures, tables_*.json
handover_2026-10-01/                             copy of every file added/changed since 28 Sep + session uploads (MANIFEST.md)
renv/, renv.lock, .Rprofile                      Sam's renv setup (in the cloud container run R with `Rscript --vanilla`)
```

### 3.2 Running R
- **Sam (RStudio, Mac):** working directory = `diss_replication/`, then `source("<script>.R")`. Never RStudio "Compile
  Report" (runs from the file's folder). R 4.6.1.
- **Cloud container:** R 4.3.3 with lmtest, sandwich, car, strucchange, ARDL 0.2.5, readxl, openxlsx; CRAN is blocked, so
  `renv` cannot bootstrap — always `Rscript --vanilla -e 'source("x.R")'` from `diss_replication/`, with
  `LANG=C.UTF-8 LC_ALL=C.UTF-8` when sheet names contain "£". Outputs reproduce Sam's to the last digit.
- **One dataset:** `Dissertation_Dataset_Final.xlsx`. `export_dataset.R` writes the assembled variables of each sample into
  sheets `data_1964` / `data_1977`; `data_common.R` reads them (rebuild from CSVs only inside `export_dataset.R`, via
  `options(restud.rebuild = TRUE)`). Re-run `export_dataset.R` after any change to `determinants/*.csv` or the assembly, then
  commit the workbook. New exploratory series (e.g. IFS rates, OBR forecast terms, gilt yields, HIP/IMPIC) are NOT in the
  workbook — they go in only if adopted.
- **Globals in `data_common.R` you must never reuse as script variables:** `BASE`, `LAB`, `STEP`, `Y0`, `SIX`, `WB`,
  `DATA_SHEET`, `FROM_SHEET`, `LAGV`, `LOGV`, `BOE_END`. `SAMPLE` leaks between scripts — set it explicitly.
- **Shared engines:** `tables_common.R` (printer, `DEFS` dictionary, `fit()`, `panel()`, `hacF()`), `data_common.R`,
  `screen_engine.R` (`build_data()`, `CORE_A`, `run_protocol()`, `battery()`, `coefline()`, `pf2()`), `parallel_spec.R`
  (switches GROSS, VAC, TAXVAR, LABV, INT), `fiscal_ratio.R` (clean PUSF interest series; sourced by many later scripts),
  `psnb_paths.R` (OBR forecast paths; `options(restud.paths_only = TRUE)` builds the paths without running its screen).

### 3.3 Scripts (diss_replication/) — what exists now

**Manuscript layer (28 Sep layout; not re-run since):** `robustness_tables.R` (B6–B15), `without_projection.R` (B16–B18),
`controls_tables.R` (B19–B25, categorical controls), `private_tests.R` (NOT for publication: screens, selection, sub-samples,
second lag), `manuscript_1964.R`, `replication_1977.R`, `run_all_1964.R`, `run_all_1977.R`, `export_dataset.R`.
Retired 28 Sep: `boe_tests.R`, `boe_appendix.R`, `extra_tests.R`, `gad_obr_tests.R`, `core_1977_boe.R`,
`extension_1964_boe.R`, `extension_1964_robustness.R`, `permits_long.R`, `prop1_model.R`.

**The replication file for the core (for Sam to inspect before it replaces the manuscript scripts):**
`replication_core_spec.R` (v3, 29 Sep; layout rules in CLAUDE.md (16)/(17): samples named "Citizen sample (1965–2019)" /
"Origin sample (1977–2019)"; all alternative outcomes in log persons; column titles = the variable only; Section 1 main tables
with bootstraps; 2 mechanism; 3 projection (total DR, level vs change, horizon); 4 non-economic robustness; 5 economic
regressors; 6 alternative outcomes; 7 uncontrolled flows). NB it still contains the 29 Sep core WITH `int_tme` — update to the
6-parameter core when Sam confirms. Parallel runs: `parallel_core.R`, `parallel_gross*.R`, `parallel_rate_vac.R` (Tables G2–G27).

**Exploratory scripts, 28 Sep night – 1 Oct (each has a header stating question, construction and Sam's request; each has an
`*_output.txt`; verdicts in §7):** model_search, compare_gross_* (adult, final, int, tbill, vu, vu_replace), compare_british_wage,
compare_wage_boot, composition_reason, tme_predict, dv_predict, fiscal_term, interest_terms, level_and_spread, levels_grid,
unem_variant, int_wap_variant, combined_fiscal, noproj_additions, spend_infl_screen, inflation_check, timing_bma, diag_shift,
regime_timing, break_candidates, break_citizen, relax_screen, relax_ladder, reverse_build, main_spec_protocol, serial_cycle,
tax_lag_proxy, ltim_serial, tax_rates, interest_apf, burden_motion, pop_rate, core_no_int, fiscal_ratio, density_forecast,
british_mpr, occam_british, british_relinc, influence_cluster, robust_extras, policy_index, gilt_yields, obr_forecast_terms,
psnb_paths, psnb_extend, psnb_ratios, rpi_check, inflation_rg, diagnostics_shortlist, primary_int_tme.

### 3.4 Document builders (unchanged; run in a sandbox with Node + python-docx + LibreOffice)
Core manuscript `outputs/build_docx.js` + `manuscript_part1/2.md` (markers `[[TABLE1..4]]` etc.); 45pp/30pp from FROZEN
`*_variantsrc.md`; `build_online_appendix.js`; 1964/combined `manuscript_1964/build_docx_1964.js`, `build_docx_combined.js`;
Figure 4 (1964) via `outputs/pyrep/m1964_figs.py`; thesis by python-docx at run level with a backup first; never set
updateFields. (`outputs/` was the earlier sandbox's scratch folder and is not in the repository.)

---

## 4. Binding rules (CLAUDE.md — follow exactly)
- **RULE 0:** every uploaded thesis/manuscript copy is the new foundation; diff first; carry pipeline work onto his copy.
- **RULE 1 (non-negotiable):** never revert or "improve" Sam's manual edits without item-by-item consent; disclose every
  divergence after a merge.
- **Do not update manuscript text/tables or rebuild documents on code runs** — Sam is testing; outputs change when he decides.
- Oxford commas; quotations never altered; thesis first person singular; Harvard refs with hanging indent; access dates only on
  web/news/datasets/official publications ("(Accessed: 20 June 2026)" or 21 June); no page numbers in in-text citations; Acts
  named in text only.
- Table conventions (27–28 Sep): first column "main"; `+name` adds; bare `name` replaces the baseline variable the notes name;
  no columns replacing the projection except level/horizon/total DR; every name defined under its panel; column names never run
  together; screens/selection/sub-samples/second lag/sample starts are private.
- Framing constraints: Hemming & Kay (horizon, not falsification); Dell does not support "misrepresented/admitted"; Brittan
  has nothing on immigration; British-placebo descent composition is "likely" (Sriskandarajah & Drew 2006); supply-side
  variables are omitted-variable checks, never mechanism; Pemberton (2018) paraphrase only; do not use the Glover (2001) UN
  figures to downplay GAD; the £2.6bn (Glover) is ANNUAL (1998/99), the short-term benefit.
- Sam's DV stance (record, do not revisit): the DV measures the outcome of immigration policy overall; "non-EU" is a
  convenient label; no model-based adjustment of the series.
- **Regime indicators/dummies are inconsistent with the theory — do not propose them.** A missing-data indicator (years without
  a forecast) is acceptable to Sam and preferred to zero-filling.
- Occam's window is not used for robustness checks; wage inflation = robustness only; LTIM = robustness only (IPS stays the
  outcome); int_tme stays Millennium in any historical reference but is out of the results.
- Sam's notes in the Glover brief: pull before editing; never change his entries.

---

## 5. Data and variables (additions since 28 Sep; the 28 Sep list in git history still applies)

| Name | Definition / source |
|---|---|
| `mig_gross`, `L_mig_gross` | ln gross non-EU inflow, persons (IPS 3.01 non-EU citizens from non-EU residence; non-British LTIM 2.00 before 1975), NO step |
| `res_noneu_k`, `cit_noneu_k`, `dv_noneu_k` | IPS 3.01 margins: non-EU residence (any citizenship); non-EU citizens (any residence); the DV |
| `L_ln_vac_unem` | log vacancies/unemployed, t−1 (BoE M5 + ONS splices) — core labour term |
| `L_tax_voter`, `L_tax_res`, `tax_taxpayer` | taxes per adult (WAP + pensioners), per resident, per income-tax payer (HMRC, 1979–) |
| `taxrint_xb`, `taxint_xb`, `int_xb_tme`, `int_cg_tme` | clean PUSF interest (JW2P excl. banks; NMFX+MU74 = CG net of APF) — `fiscal_ratio.R` |
| `int_tme_ons` | ANLO/EBFT (PUSF) reconstruction of Millennium int_tme (comparison only) |
| `gap_obr` | OBR PSF databank output gap (FY 1972-73 on); `gap_pca`/`gap_agg` digitised WP1 chart — not used |
| `cit_grants_k`, `cit_nat_k`, `cit_reg_k` | HO grants/naturalisations/registrations (current year; to 2018) |
| `settle_*`, `vouchers_k`, `ancestry_k` | HO settlement series 1960–2007; vouchers; ancestry route |
| `ukborn_*`, `brit_desc_noneu` | IPS 3.18: UK-born inflows by region; British from non-EU minus UK-born |
| `noneu_work_k` … `brit_*_k` | IPS 3.08 by main reason |
| `unem_dest`, `unem_diff` | OECD destination unemployment (AUS/CAN/NZL/USA) and UK minus it |
| `relinc_dev`, `relinc_rich`, `youth_origin` | Maddison GDP per head relative to developing origins / rich hosts; UN WPP youth share |
| `p_pub_ext` | publication-dated projection to 1965 |
| Exploratory, not in the workbook | IFS statutory rates (`determinants/ifs_tax_rates_1973_2023.csv`); HIP/IMPIC (`policy_uk_hip_impic.csv`, `demscore/`); gilt GRYs (`gilt_yields_bgs_1976_2025.csv`); OBR forecast terms and paths (`obr_hofd_terms_1970_2019.csv`, `obr_psnb_paths_1980_2019.csv`); WDI Gini/unemployment (`wb/`); Millennium Q7 expectations read directly in `inflation_rg.R` |

New source files (all in `determinants/` unless stated): `ons_pusf_jul2026.xlsx`, `ons_pusf_debt_interest_1946_2025.csv`,
`obr_psf_aggregates_databank_mar2026.xlsx`, `obr_psf_tme_fy_1946_2030.csv`, `obr_psf_output_gap_fy_1972_2030.csv`,
**`obr_hofd_spring2026.xlsx`** (OBR Historical Official Forecasts Database: TME/receipts forecasts from 1990, PSNB £ from 1970,
PSNB/GDP 1977, growth 1983, NGDP 1982; inflation only from 2010), **`boe_bgs_prices.xlsx`** (BoE gilt prices per £100, monthly
from Nov 1975), `ips_301_*`, `ips_308_*`, `ips_318_*`, `ho_settlement_*`, `ho_citizenship_*`, `citizenship_grants_1962_2018.csv`,
`oecd_unemployment_destinations_1955_2025.csv`, `undesa_*`, `ho_policy_timeline_jun2026.csv`, `ifs/`, `wb/`, `demscore/`.
Deflation conventions: taxes per WAP = nominal NA taxes / HMT GDP deflator (2024-25 = 100) / WAP; debt interest (PUSF accruals)
already contains the RPI uplift on index-linked gilts; "real" interest subtracts GDP-deflator inflation × net debt.

---

## 6. Established results

**Core (6-parameter, R-run 30 Sep):** projection 0.0072 / 0.0067 (p < .001), protocol passes (Chow 1977 p .40; residence-based
and citizenship outcomes hold). Robust to: leave-one-year-out (0.0059–0.0077), every episode excluded (p < .01), wild cluster
bootstrap by two-year vintage blocks (p < .001 / .007); **specification curve 288/288 positive and p < .05** (median 0.0078,
range 0.0050–0.0104; `specification_curve.png/.csv`); net non-EU migration per 1,000 population 0.0158 / 0.0165; IPS-precision
WLS 0.0064; inflows as % of total population 0.0022 / 0.0026 pp (same proportional effect, EU placebo null).

**Weak spots to explain in text (not more tests):** BG(2) rejects mildly (negative residual autocorrelation −0.16/−0.23,
consistent with IPS sampling error; it disappears with LTIM from 1991 — LTIM is robustness only); dating conventions
(fiscal block year t = FY (t−1)/t; base-year vs publication-dated projection r .95/.92); zero-migration reconstruction only
from 1975.

**int_tme (why it is out):** the Millennium series includes the public-sector banks (ANLO); split on 1 Oct, the BANK component
((ANLO−JW2P)/EBFT, zero to 2007, 4–6% of TME 2008–11) carries the whole coefficient (0.035, p .003/.006); clean interest,
CG gross, CG net of the APF are null; int_tme on years to 2007 is null. The APF is not the driver (it lowered CG net interest).

**What cannot replace or explain the projection (all tested since 28 Sep):** realised and planned spending (OBR TME forecasts),
borrowing (outturn, planned, structural), gilt yields (constructed from BoE prices, DMO conventions, validated r .986), Bank
Rate/bill/term spread, r−g, growth and inflation forecasts (absorbed by the projection), statutory tax rates (fall while real
tax per WAP doubled), measured policy (HIP/IMPIC: projection lines up with the 1998–2010 on-paper labour cycle but the codes do
not enter the inflow equation), economic cycle measures, every economic/interaction candidate for the post-2008 break under
timing B.

**British inflows (the partial placebo failure):** the response is in non-UK-born British arrivals from non-EU (not returners,
not nationality-law artefacts, not push events, not grants); MPR's relative income vs developing origins (`relinc_dev`, or the
region-matched version, r 1.00) removes it, while it is null in the main equation → asymmetry: uncontrolled flow = relative-income
pull, controlled flow = projection.

---

## 7. Exploration since 28 Sep night (chronological verdicts; nothing in any manuscript)

1. **Gross inflows / denominators (28–29 Sep):** gross inflows are Sam's measurable outcome; tax per WAP / adult / resident are
   indistinguishable; log V/U beats unemployment on every criterion; Δln V/U weak; the parallel runs G2–G27 exist for the
   alternatives. Wage inflation passes both halves but fails the recursive bootstrap on the core and the reality check → robustness.
2. **Debt-interest term (29–30 Sep):** int_tme looked like the only fiscal term holding on both samples (level ≈ log); no
   replacement (interest/revenue, /GDP, per WAP, primary deficit, programme spending, rates, spreads, stocks × prices) passed;
   Sam asked for grounding; then `interest_apf.R` found the bank reclassification → **int_tme out (30 Sep)**. `fiscal_ratio.R`:
   no clean borrowing/expenditure ratio replaces it (borrowing shares have the WRONG sign); the clean one-term burden
   `taxrint_xb` passes → Sam's pending core choice (§0.4a).
3. **Timing (30 Sep):** projection at t−1 (timing B) fits best to 2008 but the effect vanishes from 2009 and fails sup-F; timing A
   recommended; regime indicators rejected by Sam on theory.
4. **Serial correlation (30 Sep):** tax at t−2 fixes it but Sam rejected it on grounds, and the tax/GDP-change proxy too; LTIM from
   1991 removes it (robustness only). Accruals reading of the t−2 term recorded in (40).
5. **Literature-driven checks (30 Sep – 1 Oct):** MPR (2011) density forecasts (models beat AR(1); projection log-score gain
   positive, not significant; timing A calibrated); Hatton (2005)/MPR timing conventions; BMA (lagged projection dominates);
   Madigan & Raftery Occam's window (British flow only; not for robustness); MPR UK-relative determinants for British flows.
6. **Forward-looking borrowing (1 Oct):** OBR forecast paths built with the NPP vintage rule; 2,434-row screen ≈ chance;
   best terms (planned 2-year borrowing change, scaled by debt, TME or nominal growth) pass only on 1981–2019 and only beside the
   realised debt term; 59 ratios with a borrowing/spending measure: none passes on both samples or with a missing-year indicator;
   planned one-year borrowing change × OUTTURN r−g passes on both samples (product −0.0098 / −0.0081) but fails to 2007 and
   without 2008–11 on 1977–2019 (crisis-dependent; sign opposite to a pressure story); forecast r−g terms absorb the projection.
   Inflation: Millennium Q7 one-year expectations = `st_inf_exp`; 5–10-year from 1985. GDP deflator beats CPI/RPI and is the
   official/literature standard (RPI lost National Statistic status 2013).
7. **Policy data (1 Oct):** DEMIG unavailable (IMI Oxford closed 2017; Demscore Country-Year-Change version not obtained) — Sam
   dropped it. HIP/IMPIC tested (`policy_index.R`). de Haas, Natter & Vezzoli (2015) methodology read: "policies on paper", so a
   quiet administrative lever need not appear — a null is consistent with the thesis.
8. **Literature for the text (1 Oct):** Freeman (1995) client politics = rival story answered by the vacancy term; Facchini &
   Mayda (2009), Hanson–Scheve–Slaughter (2007), Razin–Sadka–Swagel (2002) predict a NEGATIVE tax coefficient, ours is positive;
   de Haas et al. (2018) selection not restriction + Storesletten (2000) fit the work-route result; HMT & HO (2007, Cm 7237) and
   HMT Long-term public finance reports (2002–08) = Treasury treated migration as a fiscal variable (check contents before
   citing); Ford, Jennings & Somerville (2015) describe the post-2008 break without a dummy; Czaika & de Haas (2013), Bonjour
   (2011) for policy effectiveness. Reading list given 1 Oct (Thain & Wright 1995, Deakin & Parry 2000, Balls & O'Donnell 2002,
   Pliatzky 1982, Lawson 1992, Burk & Cairncross 1992, Dustmann & Frattini 2014, Rowthorn 2008, Pensions Commission 2004).

---

## 8. Lessons learned / pitfalls (additions)
- **Network policy of the cloud container** blocks obr.uk, ons.gov.uk, zenodo.org, gu.se, dataverse.harvard.edu,
  legislation.gov.uk, CRAN, imi/migrationinstitute sites: ask Sam to upload files (he can upload to the branch or the session).
- Sam's local errors so far: stale workbook (missing `data_*` sheets or columns → `git pull`); a stray global `REBUILD_DATA`
  (fixed: rebuild only via option inside `export_dataset.R`); "Compile Report" instead of `source()`; typos in file names.
- Demscore downloads: the Country-Year file has no DEMIG columns; check names before analysing.
- OBR HOFD: the second "July 1996" label is July 1997 (dates forced to increase); single-year vintages need special handling.
- Report screens with their size (how many rows tried) and the share passing; a pass from a large screen is a screening result.
- Never report collinear pairs (levels of unemployment + vacancies; V/U with either) as "both matter"; watch suppression
  patterns (e.g. planned borrowing change works only beside the realised debt change, r −0.79).
- Keep README change log and CLAUDE.md log updated after every round; commit with the session footer; push with retries.

---

## 9. Open items awaiting Sam
1. Core: tax only vs `taxrint_xb`; timing A vs B; whether log V/U is final (it is in the core now); confirm before
   `data_common.R` / workbook / `CORE_A` / `replication_core_spec.R` are updated to the 6-parameter core.
2. Whether any of these go in as robustness lines: planned borrowing change / debt (1981–2019); planned one-year change × r−g;
   HIP labour-entry result; wage inflation with the BoP; British relative-income result; net migration and % of population outcomes;
   specification curve figure.
3. Then rebuild: data → `replication_core_spec.R` → manuscripts (titles 1965–2019) → Online Appendix → PDFs; thesis edits
   only on his instruction.
4. Text to write (§0.5) and Sam's own paragraph on specification search.
5. Still open from before: confirm series IDs in `determinants/SOURCES_2017_2019.md` (debt_interest_tme now moot);
   Brittan edition/year; Dell §5.2.4 wording; remaining `Thesis_cuts_for_approval.md` rows; stability.png in thesis Appendix A;
   thesis conclusion redraft (not installed; its "either party" sentence needs reconsidering); raw permit figures from Salt/Bauer
   if they arrive; literature to verify before citing (Dustmann & Frattini 2014; Facchini, Mayda & Mishra 2011; Aspide &
   DiGiuseppe 2025; the HMT/HO 2007 and LTPFR contents).

---

## 10. How to work with Sam
- Rigour and honesty over flattering results; report failures plainly, with numbers, and say how many variants were tried.
- He makes the substantive choices: propose, show numbers, recommend, then wait. Calibrate "plausible/practical" against the
  paper's aims, and raise obvious data comparisons (e.g. IPS vs LTIM) immediately.
- Rejected by Sam (do not re-propose): cross-country comparison; age-composition test; splicing Tier 2 onto permits or permits
  onto IPS reason data; fiscal-impact literature section; party dummy; origin-country variables in the fiscal model; dropping
  unemployment for vacancies (27 Sep — superseded on 29 Sep by his own choice of log V/U in the core); regime indicators; tax at
  t−2; tax/GDP-change proxy; Occam's window for robustness; Gini and unemployment beside log V/U for the British flow; DEMIG.
- Concise replies; tables only where they carry numbers; always say what he needs to run, pull or upload.
