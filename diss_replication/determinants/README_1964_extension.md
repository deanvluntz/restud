# 1964 extension (manuscript): data notes and ready-made footnotes

## Series built so far

| File | Content |
|---|---|
| `npp_1963_1974_oadr.csv` | Year, vintage in force (1963/65/67/69/71/73), total population, working-age population (16–64 men, 16–59 women), pensioners, current OADR, ten-year-ahead OADR, ten-year change; `note` flags interpolated values. |
| `npp_1963_1973_totals_by_vintage.csv` | Every published projection year of each pre-1975 vintage: total, working-age and pension-age population (computed from five-year age groups for 1963–1969; published rows for 1971 and 1973) and the implied OADR. Total only where the file gives no UK age detail (1965- and 1967-based: 1971/72, 1981, 1991, 2001). |
| `inflows_non_british_1964_2019.csv` | Non-British citizen inflows 1964–2019 on one consistent basis (LTIM 2.00 all minus British for 1964–74; IPS 3.24 NON-BRITISH 1975–2015; IPS 3.01 all minus British 2016–19; the three sources agree exactly in overlap years). Also the non-EU DV (3.01), non-EU citizens from all origins (3.24) and EU citizens (3.24). |

## Footnote on the projection horizon (ready to paste)

> The 1963- to 1969-based projections publish annual figures for the first seven or eight years and selected later years only (for the 1963-based projection: 1973, 1980, 1990 and 2000). Where the year ten years ahead is not published, the projected old-age dependency ratio is interpolated linearly between the two nearest published projection years. Applying the same interpolation across seven-year gaps in the annual 1971-based projection reproduces the published values with a mean absolute error of 1.4 points and a maximum of 3.8, against a standard deviation of the ten-year change of about 9 points over the extended sample. The 1963–1969 files give population by five-year age group and sex; working age is taken as 16–64 for men and 16–59 for women, with those aged 15 estimated as one-fifth of the 15–19 group (the 1971-based file, which publishes children under 16, implies 412,000 boys aged 15 against 404,000 by this rule). Components of change are not published before the 1971-based projection, so the zero-net-migration reconstruction cannot be extended before 1971.

## The dependent variable before 1975: a decision for Sam

The `1964` sheet's `inflows` column is **non-British citizens** (all foreign nationals), not non-EU: it matches 3.24 NON-BRITISH exactly for 1975–1990 (112,000 in 1975, against 93,000 non-EU). A non-EU split does not exist before 1975. Three options:

1. **Non-British throughout 1964–2019.** One consistent series, but from 1973 it includes EU citizens under free movement, who are not controlled. Their share was 16–25% until 2003 and about half by 2015, so the series stops measuring controlled migration after 2004.
2. **Non-British 1964–1972, non-EU from 1975.** Before EEC accession every foreign national needed permission, so non-British = controlled; the 1973–74 gap can be filled by non-British less the 1975 EU share (16%). One small splice at a defensible point.
3. **Non-British 1964–2003 only**, as a separate robustness sample ending before the A8 accession.

Sam's decision (25 Sep 2026): test several, and do not rescale the pre-1975 figures by a post-1975 ratio, since that uses information outside the contemporaneous information set. The CSV now carries three candidate dependent variables: `dv_A_non_british` (all foreign nationals, 1964–2019), `dv_B_splice_raw` (non-British to 1974, non-EU from 1975, unscaled) with `basis_pre1975` as a step indicator to absorb the change of definition, and `dv_C_non_british_to_2003` (non-British, ending before the A8 accession). For B, report the projection coefficient with and without the indicator; if the two agree, the basis change is not driving the result.

## Other inputs for 1964–1978

- GDP (ABMI, chained volume, from 1948): `../series-070426.xls`, annual rows.
- Fiscal block: the OBR Historical Public Finances Database runs back to 1700, so tax, debt and GDP are available; `fiscal_block_HPFD.csv` currently starts in 1975 and would need to be rebuilt from the workbook for 1963–74.
- Unemployment: ONS LF2Q starts 1971; earlier years need the claimant-count series (Denman and McDonald, 1996, *Labour Market Trends*) or the Bank of England Millennium dataset.
- Working-age population for the tax-per-worker term: from `npp_1963_1974_oadr.csv` (vintage in force), consistent with how 1975–2018 is built.

## Extension dataset and first results (25 Sep 2026, Python replica of the R set-up)

`extension_1962_2019_dataset.csv`: year, working-age and total population (vintage in force before 1975; workbook after), projected ten-year OADR change, tax per working-age person (2024–25 prices), PSND/GDP, unemployment, the three candidate DVs, the non-EU DV, and `basis_pre1975`.

Inputs added for 1962–1974:
- `fiscal_block_1962_1974.csv`: national accounts taxes, income tax + NICs, purchase tax/VAT, PSND/GDP, PSNB/GDP and nominal GDP from the OBR Historical Public Finances Database (checked: 1974–75 values identical to `fiscal_block_HPFD.csv`).
- Deflator: the HM Treasury GDP deflator (March 2026 update, 2024–25 = 100), which runs back to 1955–56 in the same file used for 1975 onwards (`deflator_1956_1975_HMT.csv`). No splice, no approximation. The earlier chain-linked Bank of England version (`deflator_1962_1975_chainlinked.csv`) is kept only as a cross-check: it agrees with the HMT series within ±0.5% in every year but 1974 (+1.2%). The OBR PSF databank's "2024–25 prices" sheet is not usable as a deflator: its implied deflator differs across series.
- Unemployment 1962–1974: Bank of England Millennium dataset A50, break-adjusted rate (Thomas and Dimsdale, 2017), which joins the workbook's LF2Q series at 1975 within 0.1–0.2 pp (1975: 4.50 vs 4.50; 1979: 5.38 vs 5.40; 1984: 11.77 vs 11.90). The Denman and McDonald (1996) claimant-count table gives lower rates (1963: 2.6; 1970: 2.7) on the registered-unemployed basis and is the underlying source.

Fiscal specification (lagged DV, projection, lagged tax per WAP, lagged log change in debt/GDP, lagged unemployment), HAC p-values:

| Sample / DV | N | Projection (p) | ρ | adj. R² |
|---|---|---|---|---|
| A: non-British 1965–2019 | 55 | +0.0054 (<0.001) | 0.65 | 0.968 |
| B: non-British to 1974, non-EU from 1975 | 55 | +0.0080 (<0.001) | 0.57 | 0.952 |
| B + `basis_pre1975` step | 55 | +0.0081 (<0.001) | 0.46 | 0.955 |
| C: non-British 1965–2003 | 39 | +0.0081 (0.001) | 0.44 | 0.896 |
| Reference: non-EU 1979–2019 (thesis) | 41 | +0.0081 (<0.001) | 0.49 | 0.961 |
| A on 1965–1978 only | 14 | +0.0073 (0.088) | −0.55 | 0.697 |
| A on 1979–2019 only | 41 | +0.0050 (0.017) | 0.57 | 0.969 |

Reading: B and C reproduce the thesis coefficient on the longer sample; the basis step changes nothing (0.0080 vs 0.0081). A is diluted after 2004 by uncontrolled EU inflows (0.0050 on 1979–2019). The pre-1979 years alone (N = 14) give the same sign and size at p = 0.09. Unemployment, debt and tax keep their signs throughout.

Re-run with the HMT deflator throughout (25 Sep 2026): A +0.0054, B +0.0080, B + step +0.0081, C +0.0081 — unchanged to four decimals.

## R script (25 Sep 2026)

`../extension_1964.R` reproduces the panel above in R, reading Sam's `1964` sheet (the two blank `year` cells at the top, 1962 and 1963, are filled by sequence; the empty column B is dropped) and checking the 1979–2019 rows against ReformSPA before estimating. Read from the sheet, the pre-1979 column gives +0.0072 (p 0.086) rather than +0.0073 (p 0.088): rounding in the pasted cells. The script ends by printing total population by vintage and projection year from `npp_1963_1973_totals_by_vintage.csv`.

Sam's R run (25 Sep 2026) matches the table above column for column; the basis step in B is 0.133 (p 0.013) and leaves the projection at 0.0081. Note max VIF 38.0 in the A 1979–2019 column and 18.8 in the 14-observation column.

## Reading the extension (25 Sep 2026)

- The basis step in B (0.133 short run; 0.133/(1 − 0.459) = 0.25 long run) is the definitional change and nothing else: exp(−0.25) = 0.78, i.e. non-EU inflows were about 78% of non-British inflows, which is the 1975–80 EU share of 16–25%. The projection coefficient is unchanged by it.
- The pre-1979 years are consistent with the result but do not identify it on their own. Over 1965–78 the projected ten-year OADR change falls almost linearly (34.4 → −2.1; correlation with year −0.97) while inflows fall from 140k to 105k under the 1962, 1968 and 1971 Acts. On 1965–78 alone, adding a linear trend gives 0.0023 (p 0.70) and a post-1973 (Immigration Act 1971 in force) step gives 0.0043 (p 0.59). On the full 1965–2019 sample the same additions leave the projection at 0.0082–0.0084 (p < 0.001): identification comes from 1979 onwards, where the projection is not trending (correlation with year −0.09).
- So the extension is a robustness check on the sample start (N 41 → 55, coefficient 0.0081 → 0.0080), not independent evidence that the mechanism operated in the 1960s. The manuscript should say that plainly and not headline the 1965–78 column.
- A (non-British throughout) falls to 0.0050 on 1979–2019 because the series includes uncontrolled EU flows after 2004; this mirrors the EU-vs-non-EU placebo: wherever the dependent variable includes uncontrolled flows, the coefficient falls.

## Manuscript notes (Sam, 25 Sep 2026) — what the extension section should say

1. Headline: series B (non-British to 1974, non-EU from 1975) with and without `basis_pre1975`: 0.0080 / 0.0081 on 1965–2019 (N = 55) against 0.0081 on 1979–2019. Report C (non-British to 2003, 0.0081) as the pre-accession check; A (non-British throughout) only to show EU dilution after 2004 (0.0050 on 1979–2019).
2. The basis step is the definitional change and nothing else: long-run 0.133/(1 − 0.459) ≈ 0.25, exp(−0.25) = 0.78 ≈ the 1975–80 EU share of non-British inflows (16–25%). Say so.
3. Claim "the estimate is unchanged when the sample is extended back to 1965", not "the mechanism is visible in the 1960s": over 1965–78 the projection is collinear with time (r = −0.97; it fades as the WWI birth-dearth cohorts approach pension age) and inflows fall under the 1962/1968/1971 Acts, so those years add stability, not a separate test. On 1965–78 alone a trend gives 0.0023 (p 0.70). Do not headline the 1965–78 column.
4. Strength to state: fourteen years added under a different immigration regime, a different DV basis, a different unemployment source, a deflator reaching back to the 1950s and projections built from five-year age groups with interpolated tenth years — and the coefficient moves by 0.0001. The sample now spans net emigration and net immigration, so the result no longer rests on one regime of the balance of flows.
5. Diagnostics: VIF 38 in the A 1979–2019 column, 19 in the 14-observation column (¶449 caveat applies); the negative lagged DV in the short column is small-sample bias; RESET 0.076 for A is mild.
6. The interpolation footnote (above) goes wherever the pre-1975 projections are first described; components of change are unavailable before 1971, so no zero-migration variant before then.

## Robustness script (25 Sep 2026): `../extension_1964_robustness.R`, inputs `extension_1962_2019_extras.csv`

Extras: `delta_dr_10` (total DR, 1963–74 from the pre-1975 vintages with the same interpolation; 1975 from the 1975-based file; 1976 on from dr_corrected.csv), LTIM 2.00 all/British in, out, net (thousands), `uk_growth` (ABMI), `inflation` (HMT deflator), `network_B`, `post1968`, `post1973`. Replica results, series B with the basis step, HAC p-values:

| Check | Projection | Note |
|---|---|---|
| B 1965–2019 | 0.0081 (<.001) | step 0.133 (0.013) |
| + linear trend | 0.0084 (<.001) | trend −0.003 (0.50) |
| + post-1968, post-1973 steps | 0.0082 (<.001) | 1973 step +0.069 (0.017) |
| + trend and steps | 0.0085 (<.001) | |
| Total DR alone | 0.0015 (0.001) | not null on the extended sample: in the 1960s projected child and pension changes move together |
| OADR and total DR together | OADR 0.0076 (<.001); DR 0.0004 (0.48) | the horse race goes to the OADR |
| + growth (t−1) | 0.0078 (<.001) | growth 0.007 (0.31) |
| + inflation (t−1) | 0.0069 (<.001) | inflation −0.0065 (0.10) |
| + British emigration rate (t−1) | 0.0082 (<.001) | −0.009 (0.62) |
| + network (from 1968) | 0.0086 (<.001) | 0.005 (0.47) |
| + all four (from 1968) | 0.0087 (<.001) | step vanishes (−0.007, 0.94) |
| Starts 1965/68/71/74/77/79 | 0.0081, 0.0079, 0.0079, 0.0083, 0.0082, 0.0081 | |
| 1965–1991 / 1992–2019 | 0.0087 (0.001) / 0.0134 (<.001) | N 27 / 28 |
| 1965–2003 / 1975–2019 | 0.0106 (<.001) / 0.0083 (<.001) | |

Origin-country determinants (25 Sep 2026): `maddison_gdppc.csv` and `unwpp_youth_share.csv` now start in 1963 (OWID grapher fetches; 1976 values identical to the previous files; youth shares before 1976 from population counts in persons rather than rounded millions, difference ≤ 0.0012). Panel 3b of the robustness script: projection 0.0078 with relative income vs the seven origins, 0.0080 vs US/Australia, 0.0084 with the origin youth share, 0.0094 with all three; the determinants themselves are null except the youth share (0.046, p 0.056).

Sam's R run of `extension_1964_robustness.R` (25 Sep 2026) reproduces every replica number above. Additional readings from the full output: with a linear trend max VIF rises to 63–74 (trend against tax per WAP), so the trend columns are a sign check, not a specification; the DR-alone column fails BG(1) (p 0.021) where every OADR column passes; the later half-sample (1992–2019) gives 0.0134 against 0.0087 for 1965–1991.

Framing (Sam, 25 Sep 2026): relative income, origin youth share and the network term are supply-side determinants of migration, not policy variables. In the manuscript they are to be presented as *supply-side controls* — the omitted-variable check that the projection is not proxying for push/pull conditions — not as part of the mechanism. Their nullity under a controlled regime is itself consistent with policy binding.
