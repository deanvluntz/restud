# =============================================================================
# APPENDIX REPLICATION
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# This file reproduces EVERYTHING printed in Appendix A of the thesis, in the
# order the appendix prints it, and nothing else:
#
#   A.1  Data results for robustness checks
#          DR vs. OADR            -> "b2: dDR10 + debt"
#          EU vs. nonEU           -> "log_eu_delta10"
#          Gross non-EU inflows   -> "alt_log_d10_lndebtgdp"
#          Level vs. Delta        -> "log_lvl10"
#   A.2  Model diagnostics        -> FULL DIAGNOSTICS: tax_spec, trend_spec
#   A.3  Stationarity tests       -> the four grouped ADF tables
#   A.4  Bounds test              -> PSS F- and t-bounds, long-run multipliers
#
# The labels below are the ones that appear in the appendix output. They are
# the labels of the original analysis script; thesis_replication.R estimates
# the same four models under its own table-oriented names ("Table 4 (1): total
# DR placebo" and so on) and returns identical coefficients.
#
# NOTE ON A.3. The appendix's stationarity tables come from the ORIGINAL
# classifier: tseries::adf.test on the level and on the first difference, with
# the conventional applied rule (reject on the level -> I(0), otherwise I(1)).
# tseries::adf.test fixes the lag order at k = trunc((n-1)^(1/3)) = 3 and always
# includes a trend, in both the level and the difference regression. That is why
# p(diff) is often insignificant: at T = 40 the differenced test has very little
# power. The p(diff) column is informational; the I(0)/I(1) split rests on the
# level test. thesis_replication.R Section 5 supersedes this with a sequential
# ur.df + KPSS procedure, which is the better test and the one to quote if a
# classification is challenged -- but it prints a different table, so it would
# NOT reproduce the appendix as it stands. Both are included in the package;
# this file is the appendix-faithful one.
#
# NOTE ON A.4. The two bounds-test STATISTICS (F = 5.0712, t = -3.8733) are
# deterministic and reproduce exactly. Their exact p-values are simulated from a
# response surface and therefore move in the third decimal from call to call
# (observed range: F 0.0281-0.0303, t 0.0682-0.0715). The appendix prints
# p = 0.03028 and p = 0.0691, which is the draw set.seed(20260802) produces. set.seed() below fixes a
# draw so the run is repeatable; it cannot make an unseeded printout match to
# five decimals, and nothing in the argument turns on the third decimal.
#
# INPUT:  Dissertation_Dataset_Final.xlsx (sheet "ReformSPA") in the working
#         directory. Estimation sample 1979-2019, N = 41.
# =============================================================================

library(lmtest)     # coeftest, bgtest, bptest, resettest
library(sandwich)   # vcovHAC, NeweyWest
library(car)        # vif
library(tseries)    # adf.test, jarque.bera.test
library(readxl)     # dataset workbook
library(ARDL)       # bounds_f_test, bounds_t_test, multipliers

# -----------------------------------------------------------------------------
# 0. Helpers (identical to thesis_replication.R, so output is byte-comparable)
# -----------------------------------------------------------------------------

stars <- function(p) ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

# n_expect asserts the estimation sample size. na.action = na.omit drops rows
# silently, so a stray NA shrinks the sample without any error -- exactly how
# the gross level-form models once ran on 40 observations instead of 41.
run_model <- function(formula, data, label, n_expect = NULL) {
  mod <- lm(formula, data = data, na.action = na.omit)
  dropped <- nrow(data) - nobs(mod)
  if (!is.null(n_expect) && nobs(mod) != n_expect)
    stop(sprintf("%s: N = %d, expected %d (%d rows dropped by na.omit)",
                 label, nobs(mod), n_expect, dropped))
  ct <- coeftest(mod, vcov. = vcovHAC(mod))
  cat(sprintf("=== %s ===\n", label))
  cat(sprintf("  N=%d%s  Adj.R2=%.4f  AIC=%.2f\n",
              nobs(mod),
              if (dropped > 0) sprintf(" (%d dropped)", dropped) else "",
              summary(mod)$adj.r.squared, AIC(mod)))
  for (i in seq_len(nrow(ct)))
    cat(sprintf("  %-24s %8.4f  se=%7.4f  t=%6.3f  p=%.4f %s\n",
                rownames(ct)[i], ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4],
                stars(ct[i, 4])))
  v <- tryCatch(max(vif(mod)), error = function(e) NA)
  cat(sprintf("  BG(1)=%.4f  BP=%.4f  RESET=%.4f  JB=%.4f  maxVIF=%.1f\n\n",
              bgtest(mod, order = 1)$p.value, bptest(mod)$p.value,
              resettest(mod)$p.value, jarque.bera.test(residuals(mod))$p.value, v))
  invisible(mod)
}

# The appendix prints BG(1), BG(2), BP, RESET, VIF, JB and the residual ADF --
# not BG(3)/BG(4), which belong to the manuscript's bootstrap licence. This
# function prints exactly the appendix's block.
full_diag <- function(mod, label) {
  cat(sprintf("======== FULL DIAGNOSTICS: %s ========\n", label))
  print(summary(mod))
  cat("--- HAC coefficients ---\n"); print(coeftest(mod, vcov. = vcovHAC(mod)))
  cat("--- Newey-West (lag=1) comparison ---\n")
  # prewhite = FALSE, adjust = TRUE: the package defaults (prewhite = TRUE,
  # adjust = FALSE) give noticeably smaller errors and would contradict the
  # thesis's statement that Newey-West and HAC are essentially identical.
  print(coeftest(mod, vcov. = NeweyWest(mod, lag = 1, prewhite = FALSE, adjust = TRUE)))
  cat("--- BG(1) ---\n"); print(bgtest(mod, order = 1))
  cat("--- BG(2) ---\n"); print(bgtest(mod, order = 2))
  cat("--- BP ---\n");    print(bptest(mod))
  cat("--- RESET ---\n"); print(resettest(mod))
  cat("--- VIF ---\n");   print(vif(mod))
  cat("--- JB ---\n");    print(jarque.bera.test(residuals(mod)))
  # adf.test() applies standard Dickey-Fuller critical values. Residuals from an
  # ESTIMATED regression require Engle-Granger critical values, which are larger
  # in magnitude, so this over-rejects and is NOT a cointegration test. It is a
  # residual diagnostic only; A.4's bounds test is the formal instrument.
  cat("--- ADF on residuals ---\n"); print(adf.test(residuals(mod)))
}

# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------

DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
cat("data:", DATA_XLSX, "\n")
raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]
raw <- raw[order(raw$year), ]
# lag1() shifts by POSITION: a duplicated or missing year would misalign every
# lag without raising an error. Assert the precondition rather than trust it.
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))

lag1 <- function(x) c(NA, head(x, -1))

reg_df <- with(raw, data.frame(
  year               = year,
  # --- dependent variables ---
  mig_rate           = nEU_inflows_WAPlagK,
  log_mig_rate       = loglev_nEU_WAPlagK,            # = log(nEU_inflows_WAPlagK)
  alt_mig_rate       = nEU_inflows,                   # gross inflows, unscaled
  alt_log_mig_rate   = log(nEU_inflows),
  eu_mig_rate        = EU_inflows_WAPlagK,
  eu_log_mig_rate    = log(EU_inflows_WAPlagK),       # EU placebo
  # --- projection measures ---
  oadr_0             = oadr_0,
  level_oadr_1 = oadr_1, level_oadr_3 = oadr_3, level_oadr_5 = oadr_5,
  level_oadr_7 = oadr_7, level_oadr_10 = oadr_10,
  delta_oadr_1 = delta_oadr_1, delta_oadr_3 = delta_oadr_3,
  delta_oadr_5 = delta_oadr_5, delta_oadr_7 = delta_oadr_7,
  delta_oadr_10 = delta_oadr_10,
  delta_oadr_5avg = delta_oadr_5avg,   delta_oadr_5wavg  = delta_oadr_5wavg,
  delta_oadr_10avg = delta_oadr_10avg, delta_oadr_10wavg = delta_oadr_10wavg,
  oadr_over5 = oadr_over5, oadr_over5avg = oadr_over5avg,
  oadr_over5wavg = oadr_over5wavg, oadr_over10 = oadr_over10,
  delta_dr_10        = delta_dr_10,
  # --- fiscal / tax controls ---
  tax_gdp = tax_gdp, ln_tax_gdp = ln_tax_gdp,
  tax_workpop = tax_workpop, tax_workpop_salary = tax_workpop_salary,
  tax_totpop = tax_totpop,
  tax_work_income_gbpB = tax_work_income_gbpB,
  ln_tax_work_income_gbpB = ln_tax_work_income_gbpB,
  avg_salary = avg_salary, ln_avg_salary = ln_avg_salary,
  borrow_gdp = borrow_gdp, debt_gdp = debt_gdp,
  # NOTE: the workbook column ln_debt_gdp is the ANNUAL LOG CHANGE in debt/GDP,
  # not the log level; its lag L_ln_debt_gdp is the regressor.
  ln_debt_gdp = ln_debt_gdp,
  # --- labour-market / output controls ---
  unem = unem, ln_unem = ln_unem, inactivity = inactivity,
  ln_rgdp_pc = ln_rgdp_pc, ln_gdp = ln_gdp, ln_workprod = ln_workprod,
  # --- financial / forecast ---
  gilt_10yr = gilt_10yr, ln_gilt_10yr = ln_gilt_10yr,
  growth_fc_yr0 = growth_fc_yr0, growth_fc_half1yr = growth_fc_half1yr
))

# d_log_mig: first difference of the log inflow rate (appendix row 3 of A.3)
reg_df$d_log_mig <- c(NA, diff(reg_df$log_mig_rate))

# Lags are built on the PRE-SUBSET series. Lagging the trimmed frame would set
# 1979 to NA and estimate on 40 observations rather than 41.
reg_df$L_log_mig_rate     <- lag1(reg_df$log_mig_rate)
reg_df$L_alt_log_mig_rate <- lag1(reg_df$alt_log_mig_rate)
reg_df$L_eu_log_mig_rate  <- lag1(reg_df$eu_log_mig_rate)
reg_df$L_unem             <- lag1(reg_df$unem)
reg_df$L_tax_workpop      <- lag1(reg_df$tax_workpop)
reg_df$L_ln_debt_gdp      <- lag1(reg_df$ln_debt_gdp)   # lagged log CHANGE
reg_df$trend              <- seq_along(reg_df$year)

reg_df <- subset(reg_df, year >= 1979 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1      # trend = 1 in 1979
stopifnot(nrow(reg_df) == 41)

# =============================================================================
# A.1  DATA RESULTS FOR ROBUSTNESS CHECKS
# =============================================================================

cat("\nData results for robustness checks\n\n")

cat("DR vs. OADR\n")
# Falsification: the TOTAL dependency ratio in place of the old-age ratio. The
# mechanism is fiscal, so the placebo should fail -- and does (p = 0.21).
run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
            L_ln_debt_gdp + L_unem,
          reg_df, "b2: dDR10 + debt", n_expect = 41)

cat("EU vs. nonEU\n")
# Falsification: EU inflows, which policy could not ration under free movement.
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10 +
            L_ln_debt_gdp + L_unem,
          reg_df, "log_eu_delta10", n_expect = 41)

cat("Gross non-EU inflows\n")
# The per-capita denominator is the analyst's choice, not the policymaker's,
# so the result is shown without it.
run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + trend + delta_oadr_10 +
            L_unem + L_ln_debt_gdp,
          reg_df, "alt_log_d10_lndebtgdp", n_expect = 41)

cat("Level vs. Delta\n")
# The projected 10-year OADR LEVEL in place of the projected change.
run_model(log_mig_rate ~ L_log_mig_rate + trend + level_oadr_10 +
            L_unem + L_ln_debt_gdp,
          reg_df, "log_lvl10 L_ln_debt_gdp", n_expect = 41)

# =============================================================================
# A.2  MODEL DIAGNOSTICS
# =============================================================================

cat("\nModel diagnostics\n\n")

# Fiscal specification ("tax_spec"), Figure 2.1
tax_spec <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                 delta_oadr_10 + L_ln_debt_gdp + L_unem,
               data = reg_df, na.action = na.omit)
stopifnot(nobs(tax_spec) == 41)
full_diag(tax_spec, "tax_spec")

# Trend specification ("trend_spec"), Figure 2.2
trend_spec <- lm(log_mig_rate ~ L_log_mig_rate + trend +
                 delta_oadr_10 + L_ln_debt_gdp + L_unem,
               data = reg_df, na.action = na.omit)
stopifnot(nobs(trend_spec) == 41)
full_diag(trend_spec, "trend_spec")

# =============================================================================
# A.3  STATIONARITY TESTS
# =============================================================================
# See the header note: this is the original tseries::adf.test classifier, which
# is what produced the appendix tables. Conventional applied rule --
#   reject on the level (p < 0.05) -> I(0); otherwise -> I(1).
# adf.test() truncates its interpolated p-values at 0.01 and 0.99 and warns when
# it does; the warning carries no information here and is suppressed per call.

adf_table <- function(vars, data, label) {
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("  %-24s %8s %9s %9s   %s\n",
              "variable", "ADFstat", "p(level)", "p(diff)", "class"))
  for (v in vars) {
    x <- na.omit(data[[v]])
    if (length(x) <= 10) { cat(sprintf("  %-24s  (too few obs)\n", v)); next }
    tl <- suppressWarnings(adf.test(x))
    pd <- tryCatch(suppressWarnings(adf.test(diff(x))$p.value),
                   error = function(e) NA_real_)
    cls <- if (tl$p.value < 0.05) "I(0)" else "I(1)"
    cat(sprintf("  %-24s %8.3f %9.4f %9s   %s\n",
                v, tl$statistic, tl$p.value,
                ifelse(is.na(pd), "n/a", sprintf("%.4f", pd)), cls))
  }
}

cat("\nStationarity tests\n")

adf_table(c("mig_rate", "log_mig_rate", "d_log_mig",
            "alt_mig_rate", "alt_log_mig_rate",
            "eu_mig_rate", "eu_log_mig_rate"),
          reg_df, "Dependent variables")

adf_table(c("oadr_0", "level_oadr_1", "level_oadr_3", "level_oadr_5",
            "level_oadr_7", "level_oadr_10",
            "delta_oadr_1", "delta_oadr_3", "delta_oadr_5", "delta_oadr_7",
            "delta_oadr_10", "delta_oadr_5avg", "delta_oadr_5wavg",
            "delta_oadr_10avg", "delta_oadr_10wavg",
            "oadr_over5", "oadr_over5avg", "oadr_over5wavg", "oadr_over10"),
          reg_df, "OADR projection measures")

adf_table(c("tax_gdp", "ln_tax_gdp", "tax_workpop", "tax_workpop_salary",
            "tax_totpop", "tax_work_income_gbpB", "ln_tax_work_income_gbpB",
            "avg_salary", "ln_avg_salary",
            "borrow_gdp", "debt_gdp", "ln_debt_gdp"),
          reg_df, "Fiscal / tax controls")

adf_table(c("unem", "ln_unem", "inactivity",
            "ln_rgdp_pc", "ln_gdp", "ln_workprod"),
          reg_df, "Labour-market / output controls")

adf_table(c("gilt_10yr", "ln_gilt_10yr", "growth_fc_yr0", "growth_fc_half1yr"),
          reg_df, "Financial / forecast")

# =============================================================================
# A.4  BOUNDS TEST
# =============================================================================
# The bounds-test p-value does NOT come from the standard F distribution:
# Pesaran, Shin and Smith (2001) give critical-value BOUNDS, and the ARDL
# package computes exact p-values by response surface for the given k, T and
# deterministic case. Case 3 = unrestricted intercept, no trend.
#
# exact = TRUE uses the small-sample surface for the actual T. Without it the
# package returns ASYMPTOTIC values (printed as T = 1000), which are more
# favourable (F: 0.0096, t: 0.0666). The exact figures are the conservative and
# correct choice at T = 40.

ts_df <- ts(reg_df[, c("log_mig_rate", "delta_oadr_10", "unem",
                       "ln_debt_gdp", "tax_workpop")],
            start = min(reg_df$year), frequency = 1)

# order = c(1,1,1,1,1) reproduces the UECM tested in the thesis:
#   d(y) ~ L(y,1) + L(x,1) + d(x)
m_ardl <- ardl(log_mig_rate ~ delta_oadr_10 + unem + ln_debt_gdp + tax_workpop,
               data = ts_df, order = c(1, 1, 1, 1, 1))

cat("\nBounds test\n")
cat("-- F-bounds test (H0: no level relationship) --\n")
set.seed(20260802)
print(bounds_f_test(m_ardl, case = 3, exact = TRUE))   # F = 5.0712
cat("-- t-bounds test --\n")
set.seed(20260802)
print(bounds_t_test(m_ardl, case = 3, exact = TRUE))   # t = -3.8733
cat("-- Long-run multipliers --\n")
print(multipliers(m_ardl))

# -----------------------------------------------------------------------------
# Session information: record the package versions the numbers were produced
# under. ARDL 0.2.4 and 0.2.5 both reproduce the statistics; the simulated
# p-values differ in the third decimal between them.
# -----------------------------------------------------------------------------
cat("\n")
print(sessionInfo())
