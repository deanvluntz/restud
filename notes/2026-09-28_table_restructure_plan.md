# Table restructure plan (28 Sep 2026) — extended-sample robustness scripts

Source: Sam's reading of `run_all_1964_output.txt` (R run 27 Sep 23:30). This is the agreed order and
content for the next edit of `extension_1964.R`, `extension_1964_robustness.R`, `boe_tests.R`,
`extra_tests.R`, `gad_obr_tests.R`, `permits_long.R`, `prop1_model.R` and the printer in `tables_common.R`.
Nothing here has been implemented yet: the scripts and data are on Sam's Mac, not in this repository.

## A. Printer rules (`tables_common.R`)

1. **Definitions under a panel: only variables new to the document.** Keep a run-level set `DEFINED`
   (seeded with the names the body text defines: `mig_rate`, `delta_oadr_10`, `L_tax_workpop`,
   `L_ln_debt_gdp`, `L_unem`, `L_mig_rate`, `basis_pre1975`). `panel()` prints `DEFS[name]` only for
   names not yet in `DEFINED`, then adds them. Reset `DEFINED` at the start of each script that starts a
   new document. Result: the long `mig_rate = …` line disappears from every panel after the first.
2. **Each definition names the measure**, one line: e.g. `L_inflation = annual % change in the GDP
   deflator, t−1` (Sam, 28 Sep; note the tax term is deflated by the HMT GDP deflator, so real tax growth
   is nominal growth minus this same series); `L_growth = real GDP growth, %, t−1`;
   `L_emig_all = emigrants of all citizenships per 1,000 working-age persons, t−1 (LTIM 2.00)`;
   `L_emig_brit` = the same for British citizens;
   `L_int_tme = central-government debt interest as % of total managed expenditure, t−1 (A28)`;
   `trend = linear time trend`; `post1968`, `post1973` = as below.
3. **Step description first.** In the first table's note, the sentence beginning "The step absorbs the
   change of basis in 1975 …" comes immediately after the table, before anything else.
4. **Column titles never run together**: column width = max(nchar(title)) + 2 (bug visible in the
   British panel: `brit_ratebrit_rate, delta_dr_10`, and in Panel C: `fiscal, no projfiscal (baseline)`).
   Cap columns per panel at 7; split wider panels (the tax panel lost its 11th column at width 130).
5. `readxl` "New names" warnings: `.name_repair = "minimal"` (cosmetic).

## B. Order of tables, extended sample (1965–2019)

| # | Panel | Columns | Notes |
|---|---|---|---|
| 1 | The fiscal specification (existing Table 1) | `mig_rate`, `mig_rate (no step)` | step sentence first; alternative series stay as the one text line |
| 2 | Tax burden measures | `L_tax_workpop`, `all taxes per resident`, `labour taxes per WAP`, `tax_nics`, `tax_nics_emp`, `tax_nics_er`, `tax_inc` | 2b (appendix or text): `tax_direct`, `tax_indirect`, `tax_vat` (1975+), `tax_corp`, `tax_receipts`; indirect/VAT/corp fail RESET (p .04/.04/.02) — say so |
| 3 | Unemployment measures (previous version) | `L_unem (baseline)`, `unem_boe for unem`, `unem_claim for unem` | drop `unem_head` and `unem_head +post98`; ILO stays the baseline |
| 4 | Level vs change | `baseline`, `projected level for change` (OADR at t+10), `+current OADR` (change plus today's level) | only ever a Figure 4 row (0.0075, N 55); the third column asks whether the known level carries anything beyond the projected change |
| 5 | Permits (the lever), 1965–2008 | `permits`, `permits +post1973`, `lever 1964–2008` (permits + Commonwealth vouchers), `permits + total DR alongside` | **drop the 1959–2008 columns** (1958-based projection). **No `basis_pre1975`** in the permits equation (the step belongs to the inflow series, not to permits): `permits_long.R` omits it (0.613), `boe_appendix.R` B7 and `manuscript_1964.R` include it (0.622) — make them agree on 0.613 |
| 6 | Gross inflows | `mig_gross`, `mig_gross +emig_all`, `mig_rate +emig_all` | drop `+emig_brit` and `+emig_all +vac_unem` from this panel (vacancies go to §D) |
| 7 | Total dependency ratio | `OADR`, `total DR`, `both` | unchanged |
| 8 | Emigration | `mig_rate +emig_all`, `+emig_brit`, `mig_gross +emig_brit` | if 6 already carries `+emig_all`, panel 8 can be `+emig_brit` only, or fold 6 and 8 |
| 9 | Pre-1975 regime steps | `baseline`, `+Act steps`, (`+trend` as one text line, VIF 63) | see §C1 for what the steps mean; drop `+both` |
| 10 | Horizon | `delta_oadr_1 … _10` | main falsification test, keep as is |
| 11 | Other | `second lag`, `sub-samples 1965–1991 / 1992–2019 / 1965–2003 / 1975–2019`, `British placebo` (see §C2), timing placebo, bootstrap bias, CUSUM | text or Figure 4 rows |

Dropped: **origin-country determinants** panel; the `network` column (Sam, 28 Sep: a poor measure that
contains further lags of migration at t−2 and t−4); **sample-start** panel (keep the one line "Sample starts 1965–1979: 0.0081,
0.0079, 0.0079, 0.0083, 0.0082, 0.0081"); the `+all four` column.

Manuscript rule (Sam, 28 Sep): the manuscript body reports the **core-sample** (1977–2019) robustness
checks and states that the extended-sample checks corroborate them, naming any material difference.
Needs `run_all_1977_output.txt` beside this output to write those sentences.

## C. Explanations Sam asked for

### C1. Trend and policy steps — what the columns are and why they exist
Over 1965–78 alone the projection is collinear with time (r = −0.97), so on the extended sample the
question is whether the pre-1975 observations add a genuine signal or merely a trend/regime shift.
The two columns are two ways of absorbing anything in the pre-1975 level that is not the projection:
`+trend` absorbs smooth drift; `+Act steps` absorbs the discrete changes in the legal basis of control.

`post1968` = 1 from 1968 (Commonwealth Immigrants Act 1968, March 1968: UK-passport holders without a
UK-born parent or grandparent lose the right of entry). `post1973` = 1 from 1973 (Immigration Act 1971 in
force 1 January 1973: Commonwealth workers move from employment vouchers to the alien work-permit
system). Given the intercept and `basis_pre1975` (= 1 to 1974), these two dummies are identified
**only inside the pre-1975 window**: `post1968` contrasts 1965–67 with 1968–74, and `post1973` is
numerically a dummy for 1973–74 alone (post1973 = 1 − basis_pre1975 + I{1973, 1974}). So the "Act
steps" column lets the pre-1975 level float in three sub-periods (1965–67, 1968–72, 1973–74) and asks
whether the projection still carries the same coefficient: it does (0.0082). The positive 1973–74 step
(+0.07) is not a tightening effect; those two years hold the Ugandan-Asian admissions, the rush of
dependants before the 1971 Act bit, and EEC nationals, who from accession (also 1 January 1973) could
enter freely and are counted in the non-British series until the 1975 change of basis. That is also
why the basis step rises from 0.13 to 0.18 when the Act steps are in. Recommendation: keep `+Act steps`,
relabel the panel "Pre-1975 regime steps", drop `+both`, report `+trend` in one line.

### C2. British citizens panel — where the columns came from
Columns replicate the thesis Appendix design (`appendix_british.R`) on the extended LTIM series:
1. `mig_rate` — baseline.
2. `brit_rate` — the fiscal equation with **British-citizen inflows** as the dependent variable (own
   lag). British entry cannot be controlled, so the projection should be null. It is 0.0032 (p .07): a
   partial placebo failure (nationality-law and return channels), weaker than the core sample's 0.0065.
3. `brit_rate, delta_dr_10` — the same equation with the total-dependency projection in place of the
   OADR: the parallel of the main total-DR placebo, checking that the small British response is
   specific to the old-age ratio (it is: 0.0003, p .48). Defensible but confusing; **recommend dropping
   it** and mentioning the number in the note.
4. `mig_rate +brit_rate` — the controlled-flow equation with the contemporaneous British flow as a
   control, purging any common driver of return and controlled migration. The projection at 0.0076 is a
   **lower bound** (the British flow may itself carry part of the policy response, so this over-controls).
5. `mig_rate +L_brit_rate` — the lagged British flow instead, to avoid simultaneity: null (−0.03, p .74).
Recommendation: keep 1, 2, 4; give 5 as one line.

### C3. Inflation vs debt interest (`L_inflation` vs `L_int_tme`) — what the R run already shows
R's forward selection chose **`L_inflation` alone** (summed ΔAIC −17.6; wage inflation would have won on
fit but is not observed throughout the extended sample, so it was inadmissible under the pre-stated
rule; nothing was admissible at step 2). This differs from the Python replica (wage_infl, real_house,
real_eri) — R is authoritative, and the projection on top of the selected model is **0.0069 (p < .001,
ΔAIC −14.2) extended, 0.0067 (p < .001, ΔAIC −8.0) core**, stronger than the replica's 0.0059 / 0.0055.

Points against inflation as a structural control, all from the output:
- It is the only single addition that weakens the functional form (RESET p .098) and the basis step.
- It predicts the **EU flow** (−0.021, p .017): by the paper's own test it is a general determinant
  (or a 1970s-regime marker), not a policy variable.
- Out of sample it hurts: expanding-window RMSE, extended 1990–2019, fiscal baseline 0.113 vs
  "selected" 0.125; core 1997–2019, 0.123 vs 0.138. Without the projection: 0.145 / 0.153.
- Mechanically, the annual log change in debt/GDP is deficit/debt minus nominal growth, and nominal
  growth contains inflation; adding inflation re-decomposes the debt term. Sam's correlation worry is
  checked by block 2 of `diss_replication_patches/int_tme_check.R`.

`L_int_tme` on both samples (already in Panel C of `prop1_model.R`):

| | extended, no proj | extended, with proj | core, no proj | core, with proj |
|---|---|---|---|---|
| `L_int_tme` | −0.0067 (p .58) | +0.0175 (p .05) | −0.0073 (p .55) | +0.0214 (p .02) |
| projection | — | 0.0096 | — | 0.0108 |
| ΔAIC vs same model without int_tme | +1.5 | −2.0 | +1.5 | −2.5 |

Reading: debt interest is null on its own and turns positive only once the projection is held fixed.
That is a suppression pattern: the interest burden and the projected ageing are negatively correlated
over the sample (interest high in the 1980s when the projection was low, low in the 2000s when the
projection was high), so each is better identified with the other in. Conditional on projected
ageing, a heavier interest bill leaves less room to borrow and raises admissions; but int_tme cannot do
the projection's work (alone: RESET p .05, AIC 25 worse). It is a complement to the mechanism, not a
rival, and it is a-priori fiscal. The "> 4 AIC on both samples" rule of 27 Sep was the assistant's suggestion, not Sam's condition
(Sam, 28 Sep). Sam's actual criterion for extending the model: a missing fiscal variable should (a)
improve the no-projection fiscal model's ability to determine migration on its own and (b) improve
the model once the projection is added. On that criterion int_tme passes (b) modestly and fails (a);
inflation passes (a) and barely moves (b) (ΔAIC −0.8 ext, −0.6 core). Block 5 of the check script
runs every fiscal variable in the dataset through both halves of the criterion on both samples.

Still to run (the check script): inflation and int_tme **together**, with and without the projection,
on both samples; the correlation matrix; int_tme in the gross equation and in the EU placebo. If
int_tme passes the EU placebo and inflation does not, that settles which belongs in the augmented
fiscal model.

## D. "What the fiscal model predicts without the projection" (replaces the Proposition 1/2 framing)
Section content, no proposition language:
1. The fiscal model without the projection (both samples): AIC −75.6 vs −99.2 (ext), −55.2 vs −70.1
   (core); RESET p .055 / .12; the lag coefficient rises to 0.66 / 0.59, the tax term halves, the debt
   change loses significance. Persistence absorbs what the projection explains.
2. The candidate screen (Panel A) as a compact table: each control alone without the projection, then
   beside it; the a-priori fiscal augmentation with int_tme; the selected control (inflation) for
   disclosure.
3. Out-of-sample forecasts (Panel D) as the headline: 22 % lower RMSE with the projection; no control
   closes the gap.
4. EU placebo for every candidate that survives (inflation fails it).
History to disclose in one sentence: with the youth share and a party dummy admitted, the rule had
picked youth + terms of trade, which absorbed the projection on the core sample; Sam then restricted the
pool to fiscal and macro variables before this run.

## E. "Further economic and fiscal controls" subsection — categories
1. Labour demand: `+vac_wap`, `+vac_unem`, `+dln_vac_unem` (unemployment kept in every column; the
   `vac for unem` columns go to a note: "dropping unemployment for vacancies was rejected as p-hacking").
2. Wages, rates, expectations, productivity, institutions: existing Panel 3.
3. Realised spending: `+grants_gdp`, `+grants_wap`, `+tme_gdp`, `grants_gdp only`.
4. Current balance and borrowing forecasts: `+psnb_gdp`, `psnb for ln_debt_gdp`, `+psnd_gdp`,
   `+tax_gdp`, `psnb only`; `+fc_budget`, `+fc_planned`, `+fc_medium`, `fc_budget only`.
5. Debt dynamics: `+int_tme`, `+int_gdp`, `+gilt`, `+r_g`, `int_tme only`.
6. GAD contribution pressure: `+gad_earn`, `gad_earn only`, `+gad_policy`.
7. Everything together: `+six`, `+six +emig_all`, `+six +dln_vac_unem` (VIF > 30: signs only).

## F. Discrepancies found in this run
- Permits 1965–2008: 0.6126 (`permits_long.R`, no step) vs 0.6221 (`boe_appendix.R` B7, `manuscript_1964.R`,
  with step). Reconcile on the no-step version.
- Replica vs R on the forward selection (above). Update HANDOVER §7.
- `unem_boe` / `unem_claim` columns show N 53 although the claimant series ends in 2016 (N 52 expected
  for 1965–2016): check whether 2017 was filled.
- Sub-sample 1992–2019: unemployment null (−0.004, p .86) and BG p .05 — worth one sentence.

## G. Results of `int_tme_check.R` (run in the cloud container, 28 Sep; R 4.3.3 reproduces Sam's
## `run_all_1964_output.txt` to the last printed digit; ARDL 0.2.5)

Full output: `diss_replication_patches/int_tme_check_output.txt`.

1. **Inflation and debt interest together, with the projection.** Extended: int_tme +0.019 (p .045),
   inflation −0.007 (p .054), projection 0.0083, AIC −103.0 (baseline −99.2), RESET .31. Core: int_tme
   +0.027 (p .004), inflation −0.011 (p .025), projection 0.0093, AIC −75.8 (baseline −70.1), RESET .26.
   **Without the projection** the pair is worse than inflation alone (AIC −83.8 vs −85.7 ext; −61.2 vs
   −62.7 core) and RESET rejects (p .004 / .001): int_tme adds nothing to the no-projection model even
   beside inflation.
2. **Correlations.** Inflation vs lagged migration −0.61 on both samples (Sam's suspicion confirmed);
   vs the tax term −0.54 / −0.70; vs the debt-ratio change only −0.20 / −0.24, so the mechanical
   debt-dynamics channel is small. int_tme vs the projection −0.11 (ext) and **−0.47 (core)**: the
   suppression pattern is a core-sample fact. int_tme vs unemployment +0.55 on the core sample.
3. **EU placebo (core).** int_tme is null in the EU equation (−0.025, p .33; p .21 without the
   projection) and leaves the projection null (0.0002). Inflation predicts the EU flow (−0.025, p .005)
   and RESET rejects (p .036). By the paper's own test int_tme behaves as a policy-pressure variable,
   inflation as a general determinant.
4. **Gross inflows.** Same pattern: int_tme null alone, +0.020 (p .02) beside the projection, projection
   0.0102 (ext) / 0.0107 (core), RESET .79 / .86.
5. **Fiscal candidate screen, both halves of Sam's criterion, each on its own rows.** No fiscal
   variable passes both halves on either sample. Variables that improve the no-projection model
   (extended ΔAIC: TME/GDP −8.5, gilt yield −7.2, Budget borrowing forecast −6.5, GAD earnings pressure
   −5.3, T-bill −3.3, inflation −10.1) all fail RESET there and are null beside the projection (ΔAIC
   positive in every case). The one variable that improves the projection model (int_tme, −2.0 / −2.5)
   does not improve the no-projection model (+1.5 / +1.5). Reading: what predicts migration without the
   projection is whatever proxies the projection's low-frequency information, and it dies once the
   projection is in; the one genuine complement is the debt-interest burden, which sharpens the
   projection rather than replacing it.

Recommendation: keep the primary specification; make `+int_tme` the lead column of the "debt and
borrowing" panel in the further-controls subsection and report the augmented model (base + int_tme,
both samples) in the section on predicting without the projection. Promoting int_tme into the primary
specification is defensible (theory-prior, passes the EU placebo, ΔAIC −2.0 / −2.5) but would ripple
through the model section, the bounds test, the long-run estimates and every table. Sam's call.

Pre-existing failure, not from this environment: `replication_1977.R` stops at the "+ dDR10, trend
(placebo)" verification (N = 41, expected 43; two rows dropped by na.omit) in Sam's 22 Sep
`run_all_1977_output.txt` as well. To fix in the core pipeline.
