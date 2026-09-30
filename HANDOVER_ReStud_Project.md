# Project knowledge transfer — "Death, taxes, and immigration" (ReStud manuscript + Humboldt MA thesis)

Owner: Sam Hamdane (samham998@gmail.com). Prepared 22 September 2026. Project root on Sam's machine: `~/quiet-instruments/`.
Read `quiet-instruments/CLAUDE.md` first: it holds the binding rules and is kept current.

---

## 1. Objective and current status

**Research question.** Why did policy-controlled (non-EU) immigration to the UK rise for four decades under governments that all promised to cut or control it? Hypothesis: the Treasury used migration as a low-salience fiscal instrument to offset projected population ageing, instead of visible tax rises. The regressor of interest is the government's own projected ten-year change in the old-age dependency ratio (OADR), reconstructed from every National Population Projection (NPP) vintage, 1975-based to 2018-based.

**Two deliverables, two institutions, deliberately different:**

| | ReStud manuscript | Humboldt MA thesis |
|---|---|---|
| File | `Hamdane_ReStud_Manuscript.docx` (+ `_45pp`, `_30pp`, `Hamdane_Online_Appendix.docx`) | `Masters_Thesis_Draft_updated.docx` |
| Sample | **1977–2019, N = 43** | **1979–2019, N = 41** |
| Title | "Death, taxes, and immigration: how Britain’s Treasury delayed the inevitable (1977–2019)" (12pt, one line, Oxford comma) | thesis's own title |
| Abstract | Sam's ~150 words, says "nearly quintupled" | Sam's wording, says "tripled" |
| Voice | "I" in places, journal register | first-person singular throughout |
| Built by | Node/Python pipeline from Markdown | Hand-edited .docx (Sam) + surgical python-docx edits |

**Status (end of 22 Sep 2026):**
- Manuscript fully swept to the 1977 run and restructured (see §4). Page counts: full **48** (target ≤ 45; Sam said he would make the final cuts to the full version himself), 45pp **45**, 30pp **30**, Online Appendix **25**.
- Thesis: 1979 figures current; Appendix A panels regenerated from the current run; Conclusion (§12) redrafted (original saved as `Thesis_Conclusion_original_22Sep2026.txt`); references cleaned (uncited removed, access dates per rule).
- All R numbers quoted in the manuscript were confirmed by Sam's own runs (`run_all_1977_output.txt`, `manuscript_extras_1977.R` output).

---

## 2. Architecture and technical stack

### 2.1 Folders
```
quiet-instruments/
├─ CLAUDE.md                          binding project rules (read first)
├─ Masters_Thesis_Draft_updated.docx  thesis (Sam edits by hand)
├─ Hamdane_ReStud_Manuscript*.docx    full / 45pp / 30pp manuscripts (built)
├─ Hamdane_Online_Appendix.docx       shared online appendix (built)
├─ diss_replication/                  CANONICAL R replication (data + scripts)
├─ archive_march2026/                 superseded March-databank run (do not use)
└─ manuscript_variants/               copies of the builder sources + variant md
```
The live builder working copy was in the assistant's `outputs/` folder; `manuscript_variants/` holds the latest copies of every builder file. Rebuild from there.

### 2.2 R layer (`diss_replication/`, R 4.6.1, run by Sam on macOS)
Packages: lmtest, sandwich, car, tseries, readxl, ARDL (0.2.5), urca, jsonlite.

| Script | Role |
|---|---|
| `replication.R` | Main models, bounds tests, bootstrap, placebos, publication dating, NPP vintage reconstruction CHECKs 1–4 (1979 sample) |
| `replication_1977.R` | Same on 1977–2019 (manuscript) |
| `build_tables.R` → `tables.json` | Sources replication.R, writes table numbers for the builder (thesis/1979) |
| `build_tables_1977.R` → `tables_1977.json` | Manuscript tables (copied to builder as `tables.json`) |
| `verify.R` / `verify_1977.R` | Compare json with baseline `tables_expected*.json`; `verify_accept()` re-baselines |
| `appendix_exhibits.R` / `_1977.R` | Console "panels" (thesis Appendix A screenshots) incl. nested-F rows |
| `robustness_both_specs.R`, `mpr_dummies_replication.R`, `thesis_replication.R` | Extra exhibits |
| `run_all.R` / `run_all_1977.R` | Run the set, capture to `run_all_output*.txt` (wrapped in a function so `on.exit` works) |
| `sample_start_1977.R` | Sample-start robustness (1977/78/79; no-debt 1976) |
| `manuscript_extras_1977.R` | Figures 3–4 (`figure3_1977.png`, `figure4_1977.png`), circular-shift placebo, second-lag check |
| `precision_check.R` | Stored vs full-precision projection series (0.00824 → 0.00826) |
| `delay_counterfactual.R` / `_1977.R` | Online Appendix C (wedge and zero-migration counterfactual) |

Standard run from `diss_replication/`: `source("build_tables_1977.R"); source("verify_1977.R"); verify_accept()` (only if changes intended) `; source("run_all_1977.R")`. Thesis equivalent without `_1977`.

### 2.3 Document builders (Node + Python, run in the assistant sandbox)
- `build_docx.js` (docx npm package): Markdown parts → .docx. Env vars: `MS_PART1`, `MS_PART2`, `MS_OUT`, `MS_REFS_BREAK` (default page break before References, implemented as `pageBreakBefore` on the heading), `MS_FIGSCALE` (30pp uses 0.9 for Figures 3–4). Reads `tables.json` and `figure1–4.png` from the working dir. Markers in Markdown: `[[TABLE1..4]]`, `[[FIGURE1..4]]`, `[[EQ]]…[[/EQ]]`, `[[REFS]]`, `[[TITLENOTE]]`; `*italic*` supported, no inline bold.
- `build_online_appendix.js`: evals the top of build_docx.js for helpers; sections A (projection reconstruction), B (time series & inference; Tables B1–B5), C (delay quantified), D (proof of Proposition 1), E (institutional & historical background = full original Section 2), F (digital references with URLs).
- Full manuscript sources: `manuscript_part1.md` (§1–4) and `manuscript_part2.md` (§5–9 + `[[REFS]]` list).
- **45pp:** `python3 assemble_r45v2.py` → `manuscript_r45_part1/2.md` (paragraph-prefix cuts on the full sources; drops §2.6; person-author references not cited in body or Online Appendix are dropped automatically).
- **30pp:** `python3 assemble_r30.py` (legacy line-number assembler + `r30_overrides*.py` + `period_1977.py`, reading the FROZEN `manuscript_part1/2_variantsrc.md`) then `python3 restructure_r30.py` (applies the restructure + compensating cuts). Build with `MS_FIGSCALE=0.9`.
- Build commands:
```bash
node build_docx.js                                            # full
MS_PART1=manuscript_r45_part1.md MS_PART2=manuscript_r45_part2.md MS_OUT=Hamdane_ReStud_Manuscript_45pp.docx node build_docx.js
MS_FIGSCALE=0.9 MS_PART1=manuscript_r30_part1.md MS_PART2=manuscript_r30_part2.md MS_OUT=Hamdane_ReStud_Manuscript_30pp.docx node build_docx.js
node build_online_appendix.js
soffice --headless --convert-to pdf <file>; pdfinfo <pdf> | grep Pages   # page check
```
- Thesis: edited with python-docx at run level (replace text inside the run that contains it; deepcopy an existing paragraph as a formatting template for new ones).

### 2.4 Key design decisions
- **Fiscal specification is primary**; the trend specification appears only in Table 2 col (2), thesis A.1 and Online Appendix Table B3.
- **Single-source fiscal block:** OBR Historical Public Finances Database, 2024–25 prices (HMT March 2026 deflators).
- **Inference:** `sandwich::vcovHAC` defaults everywhere (QS kernel, Andrews AR(1) bandwidth, no prewhitening). A Python replica (matches R to 4 dp) was used for previews only; final numbers always from Sam's R run.
- **Bounds test:** PSS case 3, exact, via R package ARDL; p-values reported to two decimals (stochastic response surface).
- **Manuscript sample 1977–2019** (earliest possible: `L_ln_debt_gdp` needs debt/GDP for 1975 and 1976; HPFD debt starts 1974–75). Thesis stays 1979–2019.

---

## 3. Data, variables and specifications

**Workbook:** `diss_replication/Dissertation_Dataset_Final.xlsx`. Sheets: `ReformSPA` (main), `PubShiftOADR_ZM` (publication-dated series), `FixedSPA`, `ReformSPA (2)`. Rows 1975–2020. A blank-named column appears as `...46` on read (harmless).

| Code name | Meaning |
|---|---|
| `log_mig_rate` = `loglev_nEU_WAPlagK` | ln non-EU inflows per 1,000 lagged working-age pop. (dependent variable) |
| `alt_log_mig_rate` = ln `nEU_inflows` | gross (unscaled) non-EU inflows |
| `eu_log_mig_rate` | ln EU inflows rate (placebo) |
| `delta_oadr_10` | projected 10-yr OADR change from vintage in force (`oadr_h − oadr_0`, h=1..10 available) |
| `delta_dr_10` | projected 10-yr total-dependency change (placebo) |
| `delta_zm_10` | zero-net-migration projected change |
| `*_pub` | publication-dated versions (vintage = max base ≤ year−1) |
| `tax_workpop` | national accounts taxes per working-age person, £000, 2024–25 prices |
| `ln_debt_gdp` | **ANNUAL LOG CHANGE in debt/GDP** (never call it a log level); lag `L_ln_debt_gdp` |
| `unem` | ILO unemployment rate |
| Tax alternatives | `tax_totpop, lab_tax_workpop, lab_tax_totpop, hh_tax_workpop, hh_tax_totpop, tax_gdp, hh_tax_gdp, lab_tax_comp` ("Lab. tax" = IT+NICs; "Lab. tax+VAT") |

**Models (R objects `tax_spec`, `trend_spec`):**
```r
tax_spec   <- log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 + L_ln_debt_gdp + L_unem
trend_spec <- log_mig_rate ~ L_log_mig_rate + trend        + delta_oadr_10 + L_ln_debt_gdp + L_unem
```

**Headline numbers.**

| | Manuscript (1977–2019, N=43) | Thesis (1979–2019, N=41) |
|---|---|---|
| Projection coef (HAC s.e.) | 0.0082 (0.0015) | 0.0081 (0.0014) |
| Trend spec | 0.0073 | 0.0075 |
| Tax / debt / unem | 0.052 / 0.54 / −0.020 | 0.048 / 0.57 / −0.021 |
| ρ | 0.47 (bias-corrected ≈ 0.58) | 0.49 |
| Long-run | 1.5% uncorrected, ≈ 2.0% corrected, CI [0.9, 2.8] | 1.6% uncorrected, "roughly 2%" corrected (thesis §12) |
| Bounds F / t | 6.16 (p < 0.01) / −4.09 (p < 0.05) | 4.94 (0.03) / −3.83 (0.07) |
| Total-DR placebo | 0.0020 (p 0.16) | 0.0015 (p 0.26) |
| EU placebo | 0.0030 (p 0.41) | 0.0028 (p 0.42) |
| Zero-migration | 0.0075 | 0.0075 |
| Nested F (drop projection) | F(1,37) = 17.82, ΔAIC −14.9 | F(1,35) = 18.79, ΔAIC −15.6 |
| Max VIF | 13.0 (L_y 13.0, tax 10.4, proj 1.3) | 12.7 (12.7, 9.8, 1.4) |
| Serial correlation | BG(1..4) p = 0.35, 0.06, **0.04, 0.03** | 0.43, 0.12, 0.15, 0.08 |
| Timing placebo (42 rotations) | max \|t\| 3.97 vs 5.68; p = 1/43 = 0.023 | — |
| Second lag of DV | −0.06 (p 0.65); proj 0.0080; AIC worse | — |

---

## 4. Manuscript structure (after the 22 Sep restructure)
1. Introduction: headline answer and estimates in paragraph 2, then **Figure 1**.
2. Institutional setting: condensed §2.1–2.6 (full original text in Online Appendix E).
3. Model: cost function, FOC, linear rule, **Proposition 1 (P1–P7)**; proof in Online Appendix D. P2 needs condition (A2): the salience of a tax rise must increase steeply enough with the inherited burden; this is stated openly as an assumption. P6 = EU placebo, P7 = total-ratio placebo.
4. Data. 5. Empirical strategy. 6. Results (Tables 2–3, history). 7. Robustness and falsification (Table 4, Figure 2 added-variable plots, **Figure 3** horizons 1–10, **Figure 4** coefficient plot of 15 variants + 2 placebos, Dynamics and Timing-placebo paragraphs). 8. Out-of-sample, limitations, agenda. 9. Conclusion.

---

## 5. Binding rules (from CLAUDE.md; follow exactly)
- **RULE 0:** every upload of the thesis/manuscript is the new foundation. Diff against the current version first; carry pipeline work onto Sam's copy, never the reverse; flag stale bases.
- **RULE 1 (non-negotiable):** never revert, "improve" or overwrite Sam's manual edits (wording, punctuation, layout, spacing) without asking first, item by item, even if they look like mistakes. After every merge, disclose every divergence and which side won.
- Oxford commas in prose, headings and title (not in Harvard author lists or quotations). Quotations are never altered.
- Thesis references: Harvard; hanging indent (left 457200 EMU, first-line −457200) on every entry; **access dates only on web pages, news, datasets and official/online publications**, randomly "(Accessed: 20 June 2026)" or "(Accessed: 21 June 2026)"; **no access date on academic sources**; remove uncited references.
- Thesis voice: first person singular.
- Abstracts differ by design (manuscript "nearly quintupled", thesis "tripled"); never "fix" the abstract without asking.
- Do not trim the full manuscript for page length unasked (Sam cuts it himself).
- Cite the R package ARDL (Natsiopoulos, Tzeremes and Finnan, 2026, v0.2.5) wherever the bounds test is described.
- Pemberton (2018) witness seminar: paraphrase only ("Do not quote without permission").

---

## 6. Lessons learned and dead ends
- **Line-number assemblers are fragile.** Inserting a line into `manuscript_part2.md` shifts every `L2(n)` pull in the legacy 45/30pp assemblers. The 30pp still uses the frozen `*_variantsrc.md` for this reason. New work should match paragraphs by opening words (as in `assemble_r45v2.py` / `restructure_r30.py`).
- **Page limits interact with floats.** Cuts before a figure often don't move the final page; cut after the last figure, or rescale figures (`MS_FIGSCALE`). The title note carries 1800-twip spacing (≈5 lines).
- **python-docx:** `paragraph.text` includes hyperlink text but `paragraph.runs` does not; compute offsets on run text only. Uploaded files are read-only (write to a new filename). Some outputs are hard links; write with `cat src > dst` if needed.
- **R pitfalls hit:** `on.exit` at top level (wrap `run_all` in a function); `verify_accept()` must be run in the same session after `verify.R`; `panel()` printed the wrong p-values when two blocks shared the name "  p" (fixed: iterate by position); stale `stationarity_bounds_replication*.R` expects a removed `borrow_gdp` column (dropped from `run_all_1977.R`; appendix_exhibits carries the stationarity/bounds panels); the level-vs-delta restriction label now takes its df from the fit.
- **Data pitfalls:** per-head columns once off by 10⁻⁶ (rescaled); duplicate year rows break the lag construction (asserted with `stopifnot`); `hh_tax_gdp` is built in `build_tables.R` from `fiscal_block_HPFD.csv` (not a workbook column).
- **Sample start:** 1975 and 1976 starts are impossible with the debt control. A 1976 start works only if the debt term is dropped (0.0063). Contemporaneous controls give 0.0055, but they break the information-set identification.
- **Gross vs scaled inflows:** AIC difference 1.2 (comparable likelihoods; not meaningful); an encompassing test rejects neither form; the projection coefficient is identical. The scaled rate stays the headline.
- **Second lag does not fix higher-order serial correlation**; rely on HAC plus the moving-block bootstrap (Online Appendix B).
- **Don't state results before Sam's R run confirms them.** Python previews are for layout only.

---

## 7. Unresolved tasks and open questions
1. **Full manuscript is 48 pages** against a 45-page target: Sam's cuts (the 45pp version shows one route).
2. Thesis Appendix A: the thin strip at the top (HAC note) is INTENDED (Sam, 23 Sep 2026: it saves repeating the note under every panel); the duplicated gross-inflows panel is still to be decided.
3. Thesis §5.2.4 and ¶457 wording items flagged earlier (e.g. "1976" as the earliest start in ¶457; "make any meaningful." in ¶64) await Sam's yes/no.
4. Manuscript literature to consider (verify before citing): Dustmann & Frattini (2014, *EJ*) on UK fiscal effects of immigration; Auerbach & Oreopoulos and Lee & Miller (AER P&P ~2000); Facchini, Mayda & Mishra (2011, *JIE*); Razin, Sadka & Swagel (2002) and Facchini & Mayda (2009) are in the thesis but not the manuscript. Aspide & DiGiuseppe (2025) was suggested but not added.
5. ReStud replication package: needs a README in the data-editor template and a single master script.
6. Optional archival leads not yet verified: Hansard 1986 (Meacher/Fowler) pointers.
7. `archive_march2026/` scripts are not kept in sync; treat as read-only history.
8. Manuscript reference list includes a few works cited only in Online Appendix E (Farid & Mostari, Guardian 2018, HoC Library 2025); decide whether the Online Appendix should carry its own list.

---

## 8. Reference snippets

**Projection reconstruction (per vintage v in force in year y):** `delta_oadr_h(y) = OADR_v(y+h) − OADR_v(y)`; zero-migration: `OADR(y+h)·WA(y+h)/(WA(y+h) − cumM) − OADR(y)`; base-dated vintage = max base ≤ y; publication-dated = max base ≤ y−1.

**Model (Section 3):** `C_t = ½φ_τ s_τ(τ_t − τ̄)² + ½φ_m s_m m_t² + φ_u u_{t−1} m_t + ½ψ(m_t − m_{t−1})²`; FOC `φ_m s_m m_t + φ_u u_{t−1} + ψ(m_t − m_{t−1}) = λ_t·MB(Δ_t)`; rule `m_t = ρ m_{t−1} + β₁Δ_t + β₂τ_{t−1} + β₃B_{t−1} − β₄u_{t−1}`, with `ρ = ψ/D`, `D = φ_m s_m + ψ + φ_τ s_τ MB²`.

**HAC coefficient table in R:**
```r
m <- lm(tax_spec, data = reg_df); lmtest::coeftest(m, vcov. = sandwich::vcovHAC(m))
```

**Online Appendix C (1977 start):** 2018 gap 41.1 OADR points (336.1 vs 295.1); cumulative net migration 5.08m = 12.2% of WAP; at the zero-migration pace (1.53/yr) ≈ 27 years of ageing (a simplification: one year's pace applied to the whole gap).
