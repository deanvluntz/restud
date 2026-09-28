# Replication package for "Quiet Instruments: Old-Age Dependency Projections and the Fiscal Determinants of Non-EU Immigration to the United Kingdom, 1979–2019"

Author: Samy Hamdane

## Overview

The package reproduces every computed exhibit in the paper and online appendix
from one R script. A single run of `replication.R` (about two minutes)
generates Tables 2–4, the Section 5.2 bias-correction and bootstrap figures,
all Section 7 robustness estimates, Online Appendix Tables B1–B2, the Online
Appendix A reconstruction checks, and Figures 1–2. Two auxiliary scripts,
`build_tables.R` and `verify.R`, export the table values to JSON and compare
them against a stored baseline; they are conveniences for checking, not part
of the analysis chain.

## Data availability and provenance statements

### Statement about Rights

- I certify that the author of the manuscript has legitimate access to and
  permission to use the data used in this manuscript.
- I certify that the author of the manuscript has documented permission to
  redistribute/publish the data contained within this replication package.

### Licence and redistribution

All UK government sources in this package (ONS, GAD, OBR, HM Treasury) are
public sector information released under the Open Government Licence v3.0
(http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
which permits redistribution with attribution; this README provides the
attribution. The replication package itself is released under a Creative
Commons BY 4.0 licence.

### Summary

All data needed to reproduce the results are included in the package. The
analysis dataset (`Quiet_Instruments_Dataset.xlsx`) is provided in full, and
the raw source for the paper's key variable — the 22 National Population
Projection vintage workbooks — is included and independently re-processed by
the code (Online Appendix A). The upstream public sources for the remaining
columns are documented below so a replicator can re-derive them.

### Data sources

| Source | Series used | Included? | File(s) in package |
|---|---|---|---|
| ONS International Passenger Survey (contemporaneous published estimates) | Gross long-term non-EU and EU inflows by country of last residence, 1979–2019 | Yes, as columns of the analysis dataset | `Quiet_Instruments_Dataset.xlsx`, sheet ReformSPA |
| ONS Long-Term International Migration | LTIM-basis non-EU inflows, 1991–2019 (robustness) | Yes, as above | `Quiet_Instruments_Dataset.xlsx` |
| GAD/ONS National Population Projections, 1977-based to 2018-based (ONS, 2016b) | Projected OADR and total dependency ratio at horizons 1–11; working-age population; net migration assumptions | Yes, raw workbooks | 22 `.xls` files (`wuk77cc…` to `ukpppsummary18.xls`) |
| OBR public finances databank (OBR, 2026) | National accounts taxes; public sector net debt as share of GDP | Yes, as columns of the analysis dataset | `Quiet_Instruments_Dataset.xlsx` |
| ONS labour market statistics (ONS, 2026) | ILO unemployment rate (series LF2Q); inactivity rate | Yes, as above | `Quiet_Instruments_Dataset.xlsx` |
| FRED (Federal Reserve Bank of St. Louis, 2025) | 10-year and 3-month gilt yields (candidate controls, excluded; appear in Table B1 only) | Yes, as above | `Quiet_Instruments_Dataset.xlsx` |
| HM Treasury Budget reports via ESCoE historical fiscal database (ESCoE, 2026) | Budget-time official growth forecasts (candidate control, excluded) | Yes, as above | `Quiet_Instruments_Dataset.xlsx` |
| Derived: publication-dated total dependency ratio deltas | Built from the NPP workbooks above | Yes | `dr_corrected.csv` |

Access details: the NPP vintages before 1998 cannot be reached from the ONS
website by navigation; all 22 were retrieved through the spreadsheet annex to
the 2008-based NPP, which carries direct hyperlinks to every predecessor
publication (ONS, 2016b). Online Appendix A documents the retrieval route and
extraction conventions in the detail required for replication. The IPS series
is used as published contemporaneously, not as retrospectively revised
(manuscript Section 4.1). Full data citations are in the References section
below.

The mapping from each spreadsheet column to its source and definition is given
in manuscript Table 1 (variable definitions and sources) and Sections 4.1–4.3;
the column-naming convention is documented in the header of `replication.R`.

## Computational requirements

### Software

- R, version 4.5.0 or later. Results verified identical under R 4.5.0
  (aarch64-apple-darwin20) and R 4.6.1 (aarch64-apple-darwin23).
- R packages: exact versions are recorded in `renv.lock` and installed by
  `renv::restore()` into a project-local library. Principal packages:
  ARDL (0.2.5), sandwich (3.1-3), lmtest, car (3.1-5), tseries (0.10-62),
  urca (1.3-4), readxl (1.5.0), zoo (1.9-0); jsonlite for the optional
  JSON export.
- No other software is required. (`tables.json` documents that the optional
  export was generated under the R version shown in its header.)

### Hardware and runtime

Verified on an Apple-silicon MacBook Pro (aarch64, macOS). A full run of
`replication.R` takes roughly two minutes; `build_tables.R` re-runs the
replication quietly and takes the same again. Memory requirements are
trivial (well under 1 GB).

### Randomness and reproducibility

All stochastic quantities are seeded in the code itself:

- The two bounds-test p-values (Table 3) are computed by simulation;
  `set.seed(20260802)` is called immediately before each call (Section 3 of
  the script). Seeded values: F p = 0.03028, t p = 0.06910. The F and t
  statistics are deterministic; treat a differing statistic as a replication
  failure and a differing third decimal of a p-value as none.
- The residual bootstraps (iid and moving-block, Section 3b) seed themselves
  (`seed = 20260802` in the function defaults).
- The Monte Carlo calibration is seeded at its call site.
- `quiet-instruments.Rproj` sets RestoreWorkspace: No and SaveWorkspace: No,
  so opening the folder as an RStudio project guarantees the clean session
  the script assumes. If running outside the project (plain R or Rscript),
  ensure no `.RData` is restored at startup.

## Description of code

- `replication.R` — the analysis, in numbered sections: (1) data and lag
  construction with integrity assertions; (2) Table 2; (3) Table 3 bounds
  tests, with a manual Wald cross-check; (3b) Section 5.2 bias correction,
  bootstraps and Monte Carlo calibration; (4) Table 4 and Section 7
  falsification/robustness; (4b) publication-dated re-estimation; (5) Table
  B1; (6) Table B2; (7) Online Appendix A reconstruction of the projection
  series from the 22 vintage workbooks (CHECKs 1–3); (8) Figures 1–2. The
  script asserts its own preconditions (sample sizes, year continuity,
  coercion failures, reconstruction tolerances) and halts rather than return
  a wrong number.
- `build_tables.R` (optional) — sources `replication.R` with output
  suppressed and writes every value behind Tables 2, 3, 4 and B2 to
  `tables.json`.
- `verify.R` (optional) — compares `tables.json` against the stored baseline
  `tables_expected.json` (225 values, tolerance 1e-10) and stops on any
  difference.

## Instructions for the replicator

1. Unzip the package and open the folder as an RStudio project
   (`quiet-instruments.Rproj`), or start R with the package root as the
   working directory. All paths in the code are relative to this root; the
   code never changes the working directory.
2. Install the recorded package versions:
   `install.packages("renv"); renv::restore()`
   (a few minutes on first run; touches only the project-local library).
3. Run the analysis: `source("replication.R", echo = TRUE)`.
   The first output line prints the resolved path of the dataset being read.
   Console output contains every table value; `figure1.png`, `figure2.png`
   and `zero_migration_deltas_R.csv` are written to the package root.
4. Optional check: `source("build_tables.R")` then `source("verify.R")` —
   expected output: "225 values compared … no differences".

## List of exhibits and where they are produced

| Exhibit | Produced by |
|---|---|
| Table 1 (variable definitions) | Descriptive; no computation |
| Table 2, cols (1)–(2) | `replication.R` §2 (console blocks "Table 2 col (1)/(2)" and full diagnostics) |
| Table 3 (multipliers, bounds tests) | §3 (console blocks "Bounds F-test/t-test", "Long-run multipliers") |
| Section 5.2 figures (Kendall/bootstrap/MBB) | §3b |
| Table 4, cols (1)–(5) | §4 (labelled console blocks) |
| Section 7: horse race, LTIM, sample starts, horizons, DR placebos | §4 |
| Section 7: publication-dated estimates | §4b (comparison table at end of block) |
| Online Appendix Table B1 (+ ADF-GLS notes) | §5 |
| Online Appendix Table B2 | §6 |
| Online Appendix A CHECKs 1–3 | §7 |
| Figure 1, Figure 2 | §8 → `figure1.png`, `figure2.png` |

Reference console output for every exhibit is reproduced by the current code;
`tables.json` (optional export) carries the same values at full precision.

## Package contents

```
replication.R                    Analysis (run this)
Quiet_Instruments_Dataset.xlsx   Analysis dataset (sheets used: ReformSPA,
                                 PubShiftOADR_ZM)
dr_corrected.csv                 Publication-dated total dependency ratios
wuk77cc… to ukpppsummary18.xls   22 ONS NPP vintage workbooks (raw data)
renv.lock, renv/                 Package versions and project library setup
quiet-instruments.Rproj          RStudio project file (pins clean sessions)
build_tables.R, verify.R         Optional value export and regression test
tables_expected.json             Verified baseline for verify.R
README.md                        This file

Reference outputs (each regenerated by a run of the code; included so the
replicator can compare against the originals, per journal guidance):
figure1.png, figure2.png         Figures 1-2 (replication.R section 8)
zero_migration_deltas_R.csv      Appendix A reconstruction output (section 7)
tables.json                      Full-precision table values (build_tables.R)
```

## References (data citations)

- Economic Statistics Centre of Excellence (2026). *UK Historical Fiscal
  Database*. ESCoE. https://www.escoe.ac.uk/ (Budget-time growth forecasts
  digitised from HM Treasury Budget reports).
- Federal Reserve Bank of St. Louis (2025). *FRED Economic Data*: UK long-term
  government bond yields and 3-month rates. https://fred.stlouisfed.org/
- Office for Budget Responsibility (2026). *Public Finances Databank*.
  https://obr.uk/data/
- Office for National Statistics (2016b). *National Population Projections,
  2008-based: spreadsheet annex with links to all predecessor publications*.
  ONS/National Archives. (Retrieval route documented in Online Appendix A.)
- Office for National Statistics and Government Actuary's Department
  (1979–2020). *National Population Projections*, 1977-based to 2018-based
  [22 vintage workbooks, included in this package]. ONS/GAD.
- Office for National Statistics (various years). *International Passenger
  Survey: long-term international migration by country of last residence*
  (contemporaneous published estimates, 1979–2019). ONS.
- Office for National Statistics (2026). *Labour market statistics*: ILO
  unemployment rate, series LF2Q. https://www.ons.gov.uk/
