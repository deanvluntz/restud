# =============================================================================
# STATIONARITY AND BOUNDS TESTS  --  Appendix A, sections A.3 and A.4
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# A standalone extract of the two time-series blocks printed in Appendix A:
#   A.3  the five grouped ADF tables (48 series, level and first difference)
#   A.4  the PSS bounds tests and long-run multipliers
#
# These blocks are also sections A.3 and A.4 of appendix_replication.R, which
# reproduces the whole appendix. The code below is taken from that file
# unaltered, so the two cannot disagree; run whichever is more convenient.
# Companion files: appendix_replication.R, robustness_replication.R,
# thesis_replication.R.
#
# TWO THINGS A REPLICATOR SHOULD KNOW BEFORE READING THE OUTPUT
#
# 1. A.3 uses the ORIGINAL classifier -- tseries::adf.test on the level and on
#    the first difference, with the conventional applied rule (reject on the
#    level -> I(0), otherwise I(1)). adf.test() fixes the lag order at
#    k = trunc((n-1)^(1/3)) = 3 and always includes a trend, in BOTH
#    regressions. That is why p(diff) is so often insignificant: at T = 40 the
#    differenced test has very little power, so the p(diff) column is
#    informational and the I(0)/I(1) split rests on the level test.
#    thesis_replication.R Section 5 supersedes this with a sequential
#    ur.df + KPSS procedure, which is the better test and the one to quote if a
#    classification is challenged -- but it prints a different table and so
#    would NOT reproduce the appendix as it stands.
#
# 2. In A.4 the two STATISTICS (F = 5.0712, t = -3.8733) are deterministic and
#    reproduce exactly. Their exact p-values are simulated from a response
#    surface and move in the third decimal from call to call (observed range:
#    F 0.0281-0.0303, t 0.0682-0.0715). The appendix prints p = 0.03028 and
#    p = 0.0691, which is the draw set.seed(20260802) produces; set.seed() below fixes a draw so the
#    run is repeatable, but cannot make an unseeded printout match to five
#    decimals. Nothing in the argument turns on the third decimal.
#
# INPUT:  Dissertation_Dataset_Final.xlsx (sheet "ReformSPA") in the working
#         directory. Estimation sample 1979-2019, N = 41.
# =============================================================================

library(tseries)    # adf.test
library(readxl)     # dataset workbook
library(ARDL)       # bounds_f_test, bounds_t_test, multipliers

# -----------------------------------------------------------------------------
# Data (identical construction to appendix_replication.R)
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
  # not the log level. It is carried under both names: ln_debt_gdp for the
  # stationarity table (which is where the appendix prints it) and
  # ln_debt_gdp for the regressions, whose lag is L_ln_debt_gdp.
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
# Record the package versions the numbers were produced under. ARDL 0.2.4 and
# 0.2.5 both reproduce the statistics; the simulated p-values differ between
# them in the third decimal.
# -----------------------------------------------------------------------------
cat("\n")
print(sessionInfo())
