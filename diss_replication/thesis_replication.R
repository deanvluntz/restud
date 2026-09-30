# =============================================================================
# Replication code for the Master's thesis:
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# This file is the thesis-scope subset of replication.R from the companion
# manuscript's replication package ("Death, taxes and immigration"). It
# reproduces every estimate, diagnostic and robustness figure cited in the
# thesis draft, in the order the thesis presents them:
#
#   Section 1  Data (identical construction to the full package)
#   Section 2  Final log-level specifications: fiscal ("tax_spec") and
#              trend ("trend_spec") models, with OLS, HAC and Newey-West
#              standard errors and the full diagnostic block
#              (BG, BP, RESET, Jarque-Bera, VIF, residual ADF)
#   Section 4  Falsification and robustness: total-DR and EU-inflow
#              placebos, gross (unscaled) inflows, zero-net-migration
#              regressor, 1998-2007 policy-era horse race, LTIM
#              measurement check, sample-start and horizon robustness
#   Section 4b Publication-dated projections
#   Section 5  Orders of integration, sequential classification
#   Section 6  OLS / HAC / Newey-West comparison table
#
# OMITTED relative to the full package (not referenced in the thesis draft):
# the bootstrap/Monte-Carlo bias-correction block (manuscript Section 5.2), the PSS bounds tests and long-run
# multipliers,
# the independent reconstruction of the projection series (manuscript Online
# Appendix A), and the manuscript's figures. Replicators wanting those should
# run replication.R from the manuscript package; both files share the same
# dataset and produce identical overlapping output.
#
# INPUT:  Dissertation_Dataset_Final.xlsx (sheets "ReformSPA" and, for the
#         publication-dated block, "PubDated"), in the working directory.
# Estimation sample: 1979-2019 (N = 41).
#
# =============================================================================

library(lmtest)     # coeftest, bgtest, bptest, resettest
library(sandwich)   # vcovHAC, NeweyWest
library(car)        # vif, linearHypothesis, deltaMethod
library(tseries)    # jarque.bera.test, adf.test
library(readxl)     # dataset workbook
library(urca)       # ur.df, ur.ers

# -----------------------------------------------------------------------------
# 0. Helper functions (output format matches the compact and full diagnostics
#    reported in the paper: compact tables print HAC standard errors)
# -----------------------------------------------------------------------------

stars <- function(p) ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

# n_expect: assert the estimation sample size. na.action = na.omit drops rows
# silently, so a stray NA -- from a spreadsheet edit, or a coercion failure --
# shrinks the sample without any error. That is the single most dangerous silent
# failure here: it is exactly how the gross level-form models once ran on 40
# observations instead of 41 and returned 1470 rather than 1375. Any row dropped
# is reported; supplying n_expect turns the report into a hard stop.
run_model <- function(formula, data, label, n_expect = NULL) {
  mod <- lm(formula, data = data, na.action = na.omit)
  dropped <- nrow(data) - nobs(mod)
  if (!is.null(n_expect) && nobs(mod) != n_expect)
    stop(sprintf("%s: N = %d, expected %d (%d rows dropped by na.omit)",
                 label, nobs(mod), n_expect, dropped))
  ct  <- coeftest(mod, vcov. = vcovHAC(mod))
  cat(sprintf("=== %s ===\n", label))
  cat(sprintf("  N=%d%s  Adj.R2=%.4f  AIC=%.2f\n",
              nobs(mod),
              if (dropped > 0) sprintf(" (%d dropped)", dropped) else "",
              summary(mod)$adj.r.squared, AIC(mod)))
  for (i in seq_len(nrow(ct))) {
    cat(sprintf("  %-24s %8.4f  se=%7.4f  t=%6.3f  p=%.4f %s\n",
                rownames(ct)[i], ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4],
                stars(ct[i, 4])))
  }
  v <- tryCatch(max(vif(mod)), error = function(e) NA)
  cat(sprintf("  BG(1)=%.4f  BP=%.4f  RESET=%.4f  JB=%.4f  maxVIF=%.1f\n\n",
              bgtest(mod, order = 1)$p.value,
              bptest(mod)$p.value,
              resettest(mod)$p.value,
              jarque.bera.test(residuals(mod))$p.value, v))
  invisible(mod)
}

full_diag <- function(mod, label) {
  cat(sprintf("======== FULL DIAGNOSTICS: %s ========\n", label))
  print(summary(mod))
  cat("--- HAC coefficients ---\n");   print(coeftest(mod, vcov. = vcovHAC(mod)))
  cat("--- Newey-West (lag=1) comparison ---\n")
  # prewhite = FALSE, adjust = TRUE are the settings behind Online Appendix
  # Table B2. The package defaults (prewhite = TRUE, adjust = FALSE) give
  # noticeably smaller errors and would contradict the paper's statement that
  # Newey-West and HAC are essentially identical.
  print(coeftest(mod, vcov. = NeweyWest(mod, lag = 1, prewhite = FALSE, adjust = TRUE)))
  cat("--- BG(1) ---\n"); print(bgtest(mod, order = 1))
  cat("--- BG(2) ---\n"); print(bgtest(mod, order = 2))
  # Section 5.2 resamples residuals iid and cites Breusch-Godfrey as the licence.
  # BG(1) and BG(2) cannot speak to higher-order dependence, so the licence is
  # only as good as the orders actually tested. If BG(3) or BG(4) rejects, the
  # bootstrap needs block resampling and the Section 5.2 interval is unreliable.
  cat("--- BG(3) ---\n"); print(bgtest(mod, order = 3))
  cat("--- BG(4) ---\n"); print(bgtest(mod, order = 4))
  cat("--- BP ---\n");    print(bptest(mod))
  cat("--- RESET ---\n"); print(resettest(mod))
  cat("--- VIF ---\n");   print(vif(mod))
  cat("--- JB ---\n");    print(jarque.bera.test(residuals(mod)))
  # adf.test() applies standard Dickey-Fuller critical values. Residuals from an
  # ESTIMATED regression require Engle-Granger critical values, which are larger
  # in magnitude, so this printout over-rejects and is NOT a cointegration test.
  # It is reported only as a residual diagnostic; Table 3's bounds test is the
  # formal instrument, and the paper makes no cointegration claim from this line.
  cat("--- ADF on residuals (DF critical values: diagnostic only) ---\n")
  print(adf.test(residuals(mod)))
}

# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------

# The dataset is found via the working directory, so resolve it ONCE to an
# absolute path and route every read through that. A run started in the wrong
# folder fails loudly here (mustWork) or shows the wrong path in its first line
# of output; and because Sections 4b and 7 reuse DATA_XLSX rather than
# re-resolving a relative name, all three reads are guaranteed to hit the same
# file even if the working directory changes mid-session.
DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
cat("data:", DATA_XLSX, "\n")
raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]
raw <- raw[order(raw$year), ]
# lag1() shifts by POSITION, and match() below joins on year taking the first
# hit. Both are correct only if the year column is unique and gap-free. A
# duplicated or missing year would misalign every lag, or silently pick one of
# two rows, without any error. Assert the precondition rather than trust it.
stopifnot(!anyDuplicated(raw$year),
          all(diff(raw$year) == 1))

lag1 <- function(x) c(NA, head(x, -1))

reg_df <- with(raw, data.frame(
  year            = year,
  # --- dependent variables (log LEVELS, constructed from the rate columns) ---
  mig_rate        = nEU_inflows_WAPlagK,
  log_mig_rate    = loglev_nEU_WAPlagK,               # = log(nEU_inflows_WAPlagK)
  alt_log_mig_rate = log(nEU_inflows),                # gross inflows, unscaled
  eu_log_mig_rate = log(EU_inflows_WAPlagK),          # EU placebo
  ltim_log_mig_rate = log(ltim_nEU_inflows_WAPlagK),  # LTIM basis, 1991-
  # --- projection measures ---
  delta_oadr_10   = delta_oadr_10,
  delta_oadr_5wavg = delta_oadr_5wavg,   # 5-year weighted average (thesis Sec. 5.2 candidate)
  delta_oadr_1    = delta_oadr_1,
  delta_oadr_3    = delta_oadr_3,
  delta_oadr_5    = delta_oadr_5,
  delta_oadr_7    = delta_oadr_7,
  level_oadr_10   = oadr_10,
  delta_dr_10     = delta_dr_10,
  delta_dr_1      = delta_dr_1,
  delta_dr_3      = delta_dr_3,
  delta_dr_5      = delta_dr_5,
  level_dr_10     = dr_10,
  oadr_0          = oadr_0,
  delta_zm_10     = delta_zm_10,                      # zero-net-migration delta
  # --- fiscal / labour-market controls (levels; lagged below) ---
  unem            = unem,
  tax_workpop     = tax_workpop,
  ln_debt_gdp   = ln_debt_gdp,   # ANNUAL LOG CHANGE in debt/GDP (see header)
  debt_gdp        = debt_gdp,
  inactivity      = inactivity,
  ln_rgdp_pc      = ln_rgdp_pc, ln_gdp = ln_gdp, ln_workprod = ln_workprod,
  gilt_10yr       = gilt_10yr, growth_fc_yr0 = growth_fc_yr0
))

# Lags (no L_ columns exist in the workbook; they are constructed here)
reg_df$L_log_mig_rate     <- lag1(reg_df$log_mig_rate)
reg_df$L_alt_log_mig_rate <- lag1(reg_df$alt_log_mig_rate)
reg_df$L_eu_log_mig_rate  <- lag1(reg_df$eu_log_mig_rate)
reg_df$L_ltim_log_mig_rate<- lag1(reg_df$ltim_log_mig_rate)
reg_df$L_unem             <- lag1(reg_df$unem)
reg_df$L_tax_workpop      <- lag1(reg_df$tax_workpop)
reg_df$L_ln_debt_gdp      <- lag1(reg_df$ln_debt_gdp)   # lagged log CHANGE
reg_df$trend              <- seq_along(reg_df$year)

# 1998-2007 policy-era indicator (Mitchell, Pain and Riley dating)
reg_df$liberal <- as.integer(reg_df$year >= 1998 & reg_df$year <= 2007)

# Restrict to estimation sample; trend re-based so it starts at 1 in 1979
reg_df <- subset(reg_df, year >= 1979 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 41)

# -----------------------------------------------------------------------------
# 2. Final specifications (thesis Results section; Figures 2.1 and 2.2)
# -----------------------------------------------------------------------------

# Column (1): fiscal specification ("tax_spec")
tax_spec <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                        delta_oadr_10 + L_ln_debt_gdp + L_unem,
                      reg_df, "Table 2 col (1): fiscal specification", n_expect = 41)
full_diag(tax_spec, "tax_spec")

# Column (2): trend specification ("trend_spec")
trend_spec <- run_model(log_mig_rate ~ L_log_mig_rate + trend +
                        delta_oadr_10 + L_ln_debt_gdp + L_unem,
                      reg_df, "Table 2 col (2): trend specification", n_expect = 41)
full_diag(trend_spec, "trend_spec")

lag1 <- function(x) c(NA, head(x, -1))

# -----------------------------------------------------------------------------
# 4. Falsification and robustness (thesis Robustness section)
# -----------------------------------------------------------------------------

# (1) Total dependency ratio placebo
run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "Table 4 (1): total DR placebo", n_expect = 41)

# (2) EU inflows placebo  [CHECK: accession-wave indicator variants equivalent]
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "Table 4 (2): EU inflows placebo", n_expect = 41)

# (3) Gross non-EU inflows, unscaled -- the per-capita denominator is the
# analyst's choice, not the policymaker's, so Section 7 reports the result
# without it, in logs and in levels, under both specifications.
reg_df$alt_mig_rate   <- exp(reg_df$alt_log_mig_rate)          # gross inflows
# The lag must come from the PRE-SUBSET series, as every other lag in this file
# does. Applying lag1() to the trimmed reg_df would set 1979 to NA and estimate
# these two models on 40 observations rather than 41, which changes the level
# coefficients materially (1470 vs 1375 in the fiscal specification).
reg_df$L_alt_mig_rate <- exp(reg_df$L_alt_log_mig_rate)
run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "gross, log, fiscal", n_expect = 41)   # 0.0080
run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "gross, log, trend", n_expect = 41)            # 0.0073
run_model(alt_mig_rate ~ L_alt_mig_rate + delta_oadr_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "gross, level, fiscal", n_expect = 41) # 1375.4
run_model(alt_mig_rate ~ L_alt_mig_rate + delta_oadr_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "gross, level, trend", n_expect = 41)          # 1294.6

# (4) Projected 10-year OADR level in place of the change
run_model(log_mig_rate ~ L_log_mig_rate + trend + level_oadr_10 +
            L_unem + L_ln_debt_gdp, reg_df, "Table 4 (4): OADR level", n_expect = 41)

# (5) Zero-net-migration projections (Section 7, first item)
ardl_zm  <- run_model(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
                        L_tax_workpop + L_ln_debt_gdp,
                      reg_df, "Section 7 / Table 4 (5): zero-mig, fiscal spec", n_expect = 41)
full_diag(ardl_zm, "zm fiscal spec")
ardl_zm2 <- run_model(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
                        trend + L_ln_debt_gdp,
                      reg_df, "Table 4 (5): zero-mig, trend spec", n_expect = 41)
full_diag(ardl_zm2, "zm trend spec")

# 1998-2007 policy-era indicator horse race (Section 7). Named hr_base, not
# `base`: that name is shared with verify.R in the same workflow (and with the
# base package), and a partial interactive re-run of this block after sourcing
# verify.R would otherwise hand anova() a numeric vector.
# Run in both specifications; the fiscal one is what the text quotes.
for (sp in c(fiscal = "L_tax_workpop", trend = "trend")) {
  lab <- if (sp == "trend") "trend" else "fiscal"
  hr_base <- run_model(reformulate(c("L_log_mig_rate", "delta_oadr_10", "L_unem", "L_ln_debt_gdp", sp),
                                   "log_mig_rate"), reg_df, paste0("Horse race: baseline (", lab, ")"), n_expect = 41)
  withdum <- run_model(reformulate(c("L_log_mig_rate", "delta_oadr_10", "L_unem", "L_ln_debt_gdp", sp, "liberal"),
                                   "log_mig_rate"), reg_df, paste0("Horse race: + liberal dummy (", lab, ")"), n_expect = 41)
  withint <- run_model(reformulate(c("L_log_mig_rate", "delta_oadr_10", "L_unem", "L_ln_debt_gdp", sp, "liberal",
                                     "delta_oadr_10:liberal"), "log_mig_rate"),
                       reg_df, paste0("Horse race: + dummy + interaction (", lab, ")"), n_expect = 41)
  cat("-- joint F, liberal + interaction (classical) --\n"); print(anova(hr_base, withint))
  cat("-- joint F, liberal + interaction (HAC) --\n")
  print(car::linearHypothesis(withint, c("liberal = 0", "delta_oadr_10:liberal = 0"), vcov. = vcovHAC(withint)))
}
# cut() is right-closed, so an upper break of 1996 would put 1997 in the
# "liberal" bin while the `liberal` dummy above excludes it. The break below
# is 1997 so the printed means describe the same window the paper names.
reg_df$period <- cut(reg_df$year, breaks = c(1978, 1997, 2007, 2019),
                     labels = c("pre1998", "liberal", "post2007"))
stopifnot(identical(as.integer(reg_df$period == "liberal"), reg_df$liberal))
print(aggregate(cbind(level_oadr_10, delta_oadr_10) ~ period,
                data = reg_df, FUN = mean))

# Every Section 7 check below is run in BOTH specifications: fiscal first (the
# one the text quotes), then trend (Online Appendix Table B3 / thesis appendix).
SPECS <- c(fiscal = "L_tax_workpop", trend = "trend")

# LTIM measurement check (1991-2019). The subset has 29 rows; the 1991 row
# drops because the LTIM series starts in 1991, so its lag is undefined there.
for (sp in names(SPECS))
  run_model(reformulate(c("L_ltim_log_mig_rate", SPECS[sp], "delta_oadr_10", "L_unem", "L_ln_debt_gdp"),
                        "ltim_log_mig_rate"),
            subset(reg_df, year >= 1991), paste0("Section 7: LTIM basis (", sp, ")"), n_expect = 28)

# Sample-start robustness (Section 7): the year >= 1983 and year >= 1987
# subsamples (N = 37 and 33).
for (y0 in c(1983, 1987)) for (sp in names(SPECS)) {
  run_model(reformulate(c("L_log_mig_rate", SPECS[sp], "delta_oadr_10", "L_ln_debt_gdp", "L_unem"),
                        "log_mig_rate"), subset(reg_df, year >= y0),
            paste0("Section 7: sample start ", y0, " (", sp, ")"), n_expect = 2019 - y0 + 1)
}

# Horizon robustness (Section 7): the projection term at 1, 3, 5 and 7 years.
for (h in c("delta_oadr_1", "delta_oadr_3", "delta_oadr_5", "delta_oadr_7")) for (sp in names(SPECS)) {
  run_model(reformulate(c("L_log_mig_rate", SPECS[sp], h, "L_unem", "L_ln_debt_gdp"),
                        "log_mig_rate"), reg_df, paste0("Section 7 horizon: ", h, " (", sp, ")"),
            n_expect = 41)
}

# Total dependency ratio across horizons and in levels (Section 7: the
# substitution "at every horizon and in levels as in deltas" yields nothing).
for (h in c("delta_dr_1", "delta_dr_3", "delta_dr_5", "level_dr_10")) for (sp in names(SPECS)) {
  run_model(reformulate(c("L_log_mig_rate", SPECS[sp], h, "L_ln_debt_gdp", "L_unem"),
                        "log_mig_rate"), reg_df, paste0("Section 7 DR placebo: ", h, " (", sp, ")"),
            n_expect = 41)
}

# -----------------------------------------------------------------------------
# 4b. Publication-dated projections (thesis Robustness section)
# -----------------------------------------------------------------------------
# Vintages are labelled by base year but published roughly a year later. The
# baseline dates by base year, assuming the fiscal centre saw the projections
# before the public did. Publication dating uses, for calendar year y, the
# latest vintage with base <= y-1. Because vintages are biennial this bites only
# in base years (k goes 0 -> 2): 21 of the 41 sample years are reassigned, some
# by more than fifteen points. Sheet "PubShiftOADR_ZM" holds the re-dated
# series; delta_dr_10_pub falls back to dr_corrected.csv if absent from it.

pub <- as.data.frame(read_excel(DATA_XLSX, sheet = "PubShiftOADR_ZM"))
pub <- pub[!is.na(pub$year), ]

# A spreadsheet column formatted as text arrives as character; lm() then treats
# it as a factor and fits one dummy per observation, which fails inside the HAC
# bandwidth estimator rather than at the point of the mistake. Coerce on merge.
# Coercion that turns a value into NA is a data-entry problem, not a conversion:
# report which cells failed rather than letting them vanish into na.omit.
num <- function(x, nm = deparse(substitute(x))) {
  if (is.numeric(x)) return(x)
  out <- suppressWarnings(as.numeric(as.character(x)))
  lost <- which(is.na(out) & !is.na(x) & trimws(as.character(x)) != "")
  if (length(lost))
    stop(sprintf("%s: %d value(s) became NA on coercion, e.g. %s",
                 nm, length(lost), paste(head(as.character(x)[lost], 3), collapse = ", ")))
  out
}

reg_df$delta_oadr_10_pub <- num(pub$delta_oadr_10_pub[match(reg_df$year, pub$year)])
reg_df$delta_zm_10_pub   <- num(pub$delta_zm_10_pub[match(reg_df$year, pub$year)])
reg_df$delta_dr_10_pub   <- if ("delta_dr_10_pub" %in% names(pub)) {
  num(pub$delta_dr_10_pub[match(reg_df$year, pub$year)])
} else if (file.exists("dr_corrected.csv")) {
  drc <- read.csv("dr_corrected.csv")
  num(drc$delta_dr_10_pub[match(reg_df$year, drc$year)])
} else rep(NA_real_, nrow(reg_df))

# All three must be complete over 1979-2019. An incomplete column means the
# spreadsheet paste is misaligned: a series pasted one row high runs off the end
# and leaves the final year blank, which is how a one-year shift announces
# itself. N = 40 instead of 41 is the symptom.
stopifnot(!any(is.na(reg_df$delta_oadr_10_pub)),
          !any(is.na(reg_df$delta_zm_10_pub)),
          !any(is.na(reg_df$delta_dr_10_pub)))

# Cross-check the sheet against the generated file where both are present. A
# shift wholly inside the sample would pass the NA test but fail this one.
if ("delta_dr_10_pub" %in% names(pub) && file.exists("dr_corrected.csv")) {
  .ref <- read.csv("dr_corrected.csv")
  .gap <- max(abs(reg_df$delta_dr_10_pub -
                    .ref$delta_dr_10_pub[match(reg_df$year, .ref$year)]))
  if (.gap > 1e-6)
    stop(sprintf("delta_dr_10_pub disagrees with dr_corrected.csv by %.3e - check row alignment in PubShiftOADR_ZM", .gap))
  cat("delta_dr_10_pub: sheet matches dr_corrected.csv\n")
}

# Guard: any regressor that is not numeric here is a data-entry problem, and is
# far cheaper to catch now than as a singular design matrix later.
.chk <- c("delta_oadr_10_pub", "delta_zm_10_pub", "delta_dr_10_pub")
.bad <- .chk[!vapply(reg_df[.chk], is.numeric, logical(1))]
if (length(.bad)) stop("non-numeric after coercion: ", paste(.bad, collapse = ", "))
cat(sprintf("Publication-dated columns merged: %d years, all numeric\n",
            sum(!is.na(reg_df$delta_oadr_10_pub))))

pub_trend <- run_model(log_mig_rate ~ L_log_mig_rate + trend +
                         delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: trend spec", n_expect = 41)      # 0.0075
pub_fisc  <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                         delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: fiscal spec", n_expect = 41)     # 0.0086
pub_zm    <- run_model(log_mig_rate ~ L_log_mig_rate + trend +
                         delta_zm_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: zero-migration (trend)", n_expect = 41)  # 0.0067
# Fiscal-specification counterparts (the text quotes the fiscal specification).
pub_zm_f  <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                         delta_zm_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: zero-migration (fiscal)", n_expect = 41)

# Placebos under the same dating convention, so the robustness check and the
# falsification tests share a timing assumption.
if (!all(is.na(reg_df$delta_dr_10_pub)))
  pub_dr <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10_pub +
                        L_ln_debt_gdp + L_unem,
                      reg_df, "Section 7 pub-dated: total DR placebo", n_expect = 41) # -0.0001
pub_eu <- run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend +
                      delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                    reg_df, "Section 7 pub-dated: EU placebo (trend)", n_expect = 41)         # p = 0.40
if (!all(is.na(reg_df$delta_dr_10_pub)))
  pub_dr_f <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_dr_10_pub +
                          L_ln_debt_gdp + L_unem,
                        reg_df, "Section 7 pub-dated: total DR placebo (fiscal)", n_expect = 41)
pub_eu_f <- run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + L_tax_workpop +
                        delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                      reg_df, "Section 7 pub-dated: EU placebo (fiscal)", n_expect = 41)

# Base-dated vs publication-dated, as reported in Section 7.
pub_compare <- function() {
  row <- function(lab, mb, kb, mp, kp) {
    cb <- coeftest(mb, vcov. = vcovHAC(mb)); cp <- coeftest(mp, vcov. = vcovHAC(mp))
    cat(sprintf("  %-20s %9.6f (p=%.4f)  %9.6f (p=%.4f)\n",
                lab, cb[kb, 1], cb[kb, 4], cp[kp, 1], cp[kp, 4]))
  }
  b_tr <- lm(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  b_fi <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  b_zm <- lm(log_mig_rate ~ L_log_mig_rate + trend + delta_zm_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  b_dr <- lm(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  b_zm_f <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_zm_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  b_dr_f <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_dr_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  b_eu_f <- lm(eu_log_mig_rate ~ L_eu_log_mig_rate + L_tax_workpop + delta_oadr_10 +
               L_ln_debt_gdp + L_unem, reg_df)
  cat("\n--- base-dated vs publication-dated (HAC), Section 7 ---\n")
  cat(sprintf("  %-20s %-21s %s\n", "", "base-dated", "publication-dated"))
  row("OADR, trend",  b_tr, "delta_oadr_10", pub_trend, "delta_oadr_10_pub")
  row("OADR, fiscal", b_fi, "delta_oadr_10", pub_fisc,  "delta_oadr_10_pub")
  row("zero-mig, trend",  b_zm,   "delta_zm_10",   pub_zm,   "delta_zm_10_pub")
  row("zero-mig, fiscal", b_zm_f, "delta_zm_10",   pub_zm_f, "delta_zm_10_pub")
  if (exists("pub_dr")) {
    row("total DR, trend",  b_dr,   "delta_dr_10", pub_dr,   "delta_dr_10_pub")
    row("total DR, fiscal", b_dr_f, "delta_dr_10", pub_dr_f, "delta_dr_10_pub")
  }
  row("EU placebo, fiscal", b_eu_f, "delta_oadr_10", pub_eu_f, "delta_oadr_10_pub")
}
pub_compare()


# -----------------------------------------------------------------------------
# 5. Orders of integration, sequential classification (thesis appendix)
# -----------------------------------------------------------------------------
# Procedure (replaces the earlier classifier, which assigned I(1) whenever the
# level test failed to reject, without consulting the difference test):
#   Levels:      ADF with constant + trend (series are trending), lags by BIC.
#   Differences: ADF with constant only (drift, no trend), lags by BIC.
#     [tseries::adf.test includes a trend and fixes k = 3 in BOTH regressions;
#      on differenced series at T = 40 this drains power and is why the old
#      p(diff) values looked insignificant.]
#   Classification is SEQUENTIAL and can fail:
#     level rejects                 -> I(0)
#     level fails, diff rejects     -> I(1)
#     both fail                     -> "I(2)? investigate" (never forced)
#   KPSS (H0: stationarity) is reported as the confirmatory mirror image:
#   I(1) is supported when ADF(level) fails AND KPSS(level) rejects AND
#   ADF(diff) rejects AND KPSS(diff) fails.


adf_p <- function(x, type) {  # approximate p-value from ur.df tau statistic
  t <- ur.df(x, type = type, selectlags = "BIC")
  stat <- t@teststat[1]
  cv   <- t@cval[1, ]        # 1%, 5%, 10% critical values
  if (stat < cv["1pct"]) "<0.01"
  else if (stat < cv["5pct"]) "<0.05"
  else if (stat < cv["10pct"]) "<0.10"
  else ">0.10"
}

# tseries::kpss.test interpolates its p-value from a short table and warns
# whenever the statistic falls outside it, which at T = 41 is most of the time.
# The warning is not a defect: it means the true p-value lies beyond the tabled
# range. Catching it and reporting ">0.10" or "<0.01" keeps that information --
# which a bare "0.10" loses -- and leaves the console clean, so any warning that
# does appear is worth reading. suppressWarnings() would discard the same
# information silently and is the wrong instrument here.
kpss_p <- function(x, null) {
  bound <- ""
  p <- withCallingHandlers(
    kpss.test(x, null = null)$p.value,
    warning = function(w) {
      m <- conditionMessage(w)
      if (grepl("greater than", m, fixed = TRUE))      bound <<- ">"
      else if (grepl("smaller than", m, fixed = TRUE)) bound <<- "<"
      else return(NULL)                # not a truncation warning: let it through
      invokeRestart("muffleWarning")
    })
  paste0(bound, sprintf("%.2f", p))
}
order_class <- function(x, name) {
  x  <- na.omit(x)
  lvl  <- adf_p(x, "trend")          # levels: constant + trend
  dif  <- adf_p(diff(x), "drift")    # differences: constant only
  k_l  <- kpss_p(x, "Trend")
  k_d  <- kpss_p(diff(x), "Level")
  cls  <- if (lvl %in% c("<0.01", "<0.05")) "I(0)"
          else if (dif %in% c("<0.01", "<0.05")) "I(1)"
          else "I(2)? investigate"
  cat(sprintf("%-28s ADF lvl %-6s diff %-6s | KPSS lvl %6s diff %6s | %s\n",
              name, lvl, dif, k_l, k_d, cls))
}

for (v in c("mig_rate", "log_mig_rate", "alt_log_mig_rate", "eu_log_mig_rate",
            "oadr_0", "delta_oadr_10", "delta_oadr_5wavg", "level_oadr_10", "tax_workpop",
            "debt_gdp", "ln_debt_gdp", "unem", "inactivity", "ln_rgdp_pc",
            "ln_gdp", "ln_workprod", "gilt_10yr", "growth_fc_yr0")) {
  order_class(reg_df[[v]], v)
}
# --- ADF-GLS resolution of ambiguous cases (Online Appendix Table B1, dagger) --
# ADF has low power on smooth series at T = 41. Where the sequential ADF rule
# leaves a series unclassified, ADF-GLS (Elliott, Rothenberg and Stock) is the
# more powerful test and is used to settle the classification.

adfgls <- function(x, name, L = 4) {
  x   <- na.omit(x)
  lvl <- ur.ers(x,       type = "DF-GLS", model = "trend",    lag.max = L)
  dif <- ur.ers(diff(x), type = "DF-GLS", model = "constant", lag.max = L)
  band <- function(t) {
    s <- t@teststat[1]; cv <- t@cval[1, ]
    p <- if (s < cv["1pct"]) "<0.01" else
         if (s < cv["5pct"]) "<0.05" else
         if (s < cv["10pct"]) "<0.10" else ">0.10"
    sprintf("%7.3f (%s)", s, p)
  }
  cat(sprintf("%-30s level %s | diff %s\n", name, band(lvl), band(dif)))
}

adfgls(reg_df$oadr_0,        "OADR, current")
adfgls(reg_df$debt_gdp,      "Debt/GDP (level, not used)")
adfgls(reg_df$delta_oadr_10, "Projected 10-yr OADR change")   # enters the model
# Obtained (urca 1.3-x, lag.max = 4):
#   OADR, current                level -2.290 (>0.10) | diff  0.100 (>0.10)
#   Debt/GDP (level, not used)   level -1.747 (>0.10) | diff -1.829 (<0.10)
#   Projected 10-yr OADR change  level -3.544 (<0.05) | diff -1.675 (<0.10)
# At lag.max = 4 ADF-GLS does not resolve either ambiguous series, and reads the
# projection variable as trend-stationary where the ADF sequence reads it as
# I(1). The sensitivity block below shows both results to be artefacts of that
# lag order: at one lag both differences reject (I(1)), and the projection
# variable reverts to I(1) at one and two lags. Forty differenced observations
# cannot support four lag terms plus GLS detrending -- the diff statistic for
# the current OADR is +0.100 at lag.max = 4, no evidence whatsoever. The short-
# lag verdicts agree with KPSS, so the I(1) entries in Table B1 stand. The
# bounds procedure is valid under either classification in any case.
# Lag sensitivity. Table B1's dagger and the note that ADF-GLS reads the
# projection variable as trend-stationary both rest on lag.max = 4, so the
# verdicts are re-run at 1 and 2. If they hold across all three, the appendix
# note stands as written; if a verdict flips, the note must say so.
cat("\n--- ADF-GLS lag sensitivity (Online Appendix Table B1 note) ---\n")
for (L in c(1, 2, 4)) {
  cat(sprintf("lag.max = %d\n", L))
  adfgls(reg_df$oadr_0,        "  OADR, current",               L)
  adfgls(reg_df$debt_gdp,      "  Debt/GDP (level, not used)",  L)
  adfgls(reg_df$delta_oadr_10, "  Projected 10-yr OADR change", L)
}

# A genuine I(2) regressor would invalidate the bounds procedure and must be
# differenced or dropped -- do not reclassify by assumption. Neither of the two
# unresolved series is a regressor, which is why the classification does not
# bind here.

# -----------------------------------------------------------------------------
# 6. OLS / HAC / Newey-West comparison (thesis 5.4; the 0.0019/0.0014 figures)
# -----------------------------------------------------------------------------
# full_diag(tax_spec) above prints all three; this block assembles them in the
# layout of Table B2 so the appendix can be checked directly.
tableB2 <- function(m) {
  ols <- coeftest(m)
  hac <- coeftest(m, vcov. = vcovHAC(m))
  nw  <- coeftest(m, vcov. = NeweyWest(m, lag = 1, prewhite = FALSE, adjust = TRUE))
  out <- data.frame(Coefficient = ols[, 1], OLS.se = ols[, 2],
                    HAC.se = hac[, 2], NW.se = nw[, 2])
  print(round(out, 4)); invisible(out)
}
cat("--- Online Appendix Table B2 (fiscal specification) ---\n")
tableB2(tax_spec)
# Expected (Table B2): rho 0.491 (.113/.077/.083); tax 0.048 (.012/.010/.011);
# OADR 0.0082 (.0019/.0014/.0014); debt 0.573 (.193/.186/.184);
# unemployment -0.021 (.011/.007/.007); constant 0.109 (.161/.121/.123).





# (Section 17, the level-DV 'posterity' specimens, was removed on 22 Sep 2026: it
# supported no figure in either document. The specification search it recorded
# is described in words in the manuscript's Section 6.1.)
