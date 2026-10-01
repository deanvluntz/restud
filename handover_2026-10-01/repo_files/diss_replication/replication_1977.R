# >>> 1977 VARIANT (manuscript only, Sam 22 Sep 2026) <<<
# Generated from replication.R: identical except that the estimation sample starts in
# 1977 (N = 43), the earliest year the lagged log change in debt/GDP allows.
# Outputs carry a _1977 suffix so the canonical 1979 run is never overwritten.
# The thesis stays on 1979-2019 and uses replication.R.
# =============================================================================
# Replication code for:
#   "Quiet Instruments: Old-Age Dependency Projections (OADR) and the Fiscal
#    Determinants of Immigration to the United Kingdom from beyond Europe
#    (1979-2019)"
#   Sam Hamdane
#
# Inputs: Dissertation_Dataset_Final.xlsx (sheets "ReformSPA" and
#   "PubShiftOADR_ZM") and, if the publication-dated total dependency ratio is
#   not carried in the sheet, dr_corrected.csv. Run in one clean session.
#
# Reproduces, in order: Table 2 (main estimates), Table 3 (long-run multipliers
# and bounds tests), Table 4 and Section 7 (falsification and robustness,
# including the zero-net-migration reconstruction, the publication-dated
# re-estimation of Section 7 and the 1998-2007 policy-era
# horse race), and Online Appendix Tables B1 (order-of-integration tests) and
# B2 (inference comparison), Online Appendix A (the reconstruction, Section 7),
# and Figures 1 and 2 (Section 8).
#
# INPUT:
#   Dissertation_Dataset_Final.xlsx, sheet "ReformSPA" (1975-2020).
#   The zero-net-migration regressor delta_zm_10 is already merged into that
#   sheet; Section 7 below rebuilds it from the 22 NPP vintage workbooks and
#   verifies it against the sheet, and is skipped if those workbooks are absent.
#   Publication-dated series: sheet "PubShiftOADR_ZM" (Section 4b below).
#   The workbook also carries "FixedSPA", "Ref. OA_DR" and "xtra calcs". None
#   is used by this script or by any result in the paper.
#
# NAMING CONVENTION IN THE SPREADSHEET -- important:
#   ln_X      = annual LOG CHANGE in X   (e.g. ln_debt_gdp, ln_nEU_inflows)
#   loglev_X  = LOG LEVEL of X           (e.g. loglev_nEU_WAPlagK)
#   The lag suffix is "WAPlagK" throughout (earlier versions of the workbook
#   used lower-case "lagk" in the ReformSPA sheet).
#
# Consequently the debt regressor in the estimating equation is the lagged
# annual LOG CHANGE in debt/GDP, not the log level. Verified: using the log
# change reproduces Table 2 column (1) exactly (intercept 0.1090, rho 0.4909,
# tax 0.0480, OADR 0.008159, debt 0.5727, unemployment -0.0208, R2 0.9658 --
# the UNADJUSTED R2, printed by full_diag(); run_model()'s compact line prints
# the adjusted value, 0.9609);
# using log(debt_gdp) reproduces none of it.
#
# Estimation sample: 1979-2019 (N = 41).
#
# VERIFIED ENVIRONMENTS. The full output was verified identical under both of:
#   R 4.5.0, aarch64-apple-darwin20, zoo 1.8-15 sandwich 3.1-1 car 3.1-5
#     tseries 0.10-61 readxl 1.4.5 ARDL 0.2.4 urca 1.3-4
#   R 4.6.1, aarch64-apple-darwin23, zoo 1.9-0 sandwich 3.1-3 car 3.1-5
#     tseries 0.10-62 readxl 1.5.0 ARDL 0.2.5 urca 1.3-4
# That two-environment check predates the L = 5 block bootstrap, so it covers
# every figure except: the L = 5 lines, verified under R 4.6.1 only (four
# independent runs); and the Monte Carlo calibration line, seeded at its call
# site and verified under R 4.6.1 (see the "Obtained" note below mc_check()).
# The two bounds-test p-values are stochastic but are seeded in Section 3, so
# they too are fixed: F p = 0.03028, t p = 0.06910. The script emits no warnings
# on either environment.
# =============================================================================

library(lmtest)     # coeftest, bgtest, bptest, resettest
library(sandwich)   # vcovHAC, NeweyWest
library(car)        # vif, linearHypothesis, deltaMethod
library(tseries)    # jarque.bera.test, adf.test
library(readxl)     # dataset workbook
library(ARDL)       # bounds_f_test, bounds_t_test, multipliers
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
  L_tax_oadr_0    = L_tax_oadr_0,
  gilt_10yr       = gilt_10yr, growth_fc_yr0 = growth_fc_yr0
))

# The ReformSPA sheet carries the total-dependency columns from 1979 only; 1977-78 (the 1975- and 1977-based vintages) come
# from dr_corrected.csv, the file the sheet's own 1979+ values were pasted from (28 Sep 2026: fixes the N = 41 stop on 1977-2019).
if (file.exists("dr_corrected.csv")) { .drc <- read.csv("dr_corrected.csv")
  for (.v in c("delta_dr_10", "delta_dr_1", "delta_dr_3", "delta_dr_5")) if (.v %in% names(.drc)) { .i <- is.na(reg_df[[.v]]); reg_df[[.v]][.i] <- as.numeric(.drc[[.v]][match(reg_df$year[.i], .drc$year)]) }
  if ("dr_10" %in% names(.drc)) { .i <- is.na(reg_df$level_dr_10); reg_df$level_dr_10[.i] <- as.numeric(.drc$dr_10[match(reg_df$year[.i], .drc$year)]) } }

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

# Restrict to estimation sample; trend re-based so it starts at 1 in 1977
reg_df <- subset(reg_df, year >= 1977 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 43)

# -----------------------------------------------------------------------------
# 2. Table 2: main estimates
# -----------------------------------------------------------------------------

# Column (1): fiscal specification ("tax_spec")
tax_spec <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                        delta_oadr_10 + L_ln_debt_gdp + L_unem,
                      reg_df, "Table 2 col (1): fiscal specification", n_expect = 43)
full_diag(tax_spec, "tax_spec")

# Column (2): trend specification ("trend_spec")
trend_spec <- run_model(log_mig_rate ~ L_log_mig_rate + trend +
                        delta_oadr_10 + L_ln_debt_gdp + L_unem,
                      reg_df, "Table 2 col (2): trend specification", n_expect = 43)
full_diag(trend_spec, "trend_spec")

# -----------------------------------------------------------------------------
# 2b. Section 7: the projection's marginal contribution, and the policy-era
#     horse race on Mitchell, Pain and Riley's own specification
# -----------------------------------------------------------------------------
# Two additions to the robustness battery, both cited in Section 7.
#
# (a) THE NESTED BASELINE. With a lagged dependent variable the model reaches
#     adj R2 ~ 0.94 before the projection enters, so the R2 increment is
#     uninformative and the nested F-test and AIC carry the comparison. The
#     total-DR placebo is the instructive contrast: it does not merely fail to
#     reach significance, it fails to pay for its own degree of freedom, so AIC
#     prefers the model with no projection at all.
#
# (b) POL97 IS A STEP, NOT A WINDOW. Mitchell, Pain and Riley (2011, pp. 1411-12)
#     define POL97 = 1 from 1998 ONWARDS, and enter it lagged. Their sample ends
#     in 2007, so within their data a step from 1998 and a 1998-2007 window are
#     the same variable and cannot be distinguished; over 1979-2019 they are not.
#     Both are estimated here. POL02 (2002 onwards, HSMP and the 2002 Act) and
#     the enlargement dummies for 1986, 1995 and 2004 complete their set; their
#     region-specific "POL other" dummies have no UK-aggregate counterpart.

reg_df$POL97   <- as.integer(reg_df$year >= 1998)          # step, not a window
reg_df$POL02   <- as.integer(reg_df$year >= 2002)
reg_df$EXP04   <- as.integer(reg_df$year >= 2004)
reg_df$ENL86   <- as.integer(reg_df$year >= 1986)
reg_df$ENL95   <- as.integer(reg_df$year >= 1995)
reg_df$liberal <- as.integer(reg_df$year >= 1998 & reg_df$year <= 2007)
# The lag must be taken on the step itself, not on the trimmed frame.
reg_df$L_POL97 <- c(NA, head(reg_df$POL97, -1))
# Total inflows: the EU and non-EU rates share the same lagged working-age
# denominator, verified across all 41 years, so the combined rate is their sum.
reg_df$tot_log_mig_rate <- log(exp(reg_df$log_mig_rate) + exp(reg_df$eu_log_mig_rate))
reg_df$L_tot_log_mig_rate <- c(NA, head(reg_df$tot_log_mig_rate, -1))

cat("\n--- Section 7: nested baseline (no projection term) ---\n")
pl_trend  <- run_model(log_mig_rate ~ L_log_mig_rate + trend + L_ln_debt_gdp + L_unem,
                       reg_df, "no projection, trend spec", n_expect = 43)
pl_fiscal <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + L_ln_debt_gdp + L_unem,
                       reg_df, "no projection, fiscal spec", n_expect = 43)
m_trend  <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dOADR10, trend", n_expect = 43)
m_fiscal <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dOADR10, fiscal", n_expect = 43)
m_dr     <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dDR10, trend (placebo)", n_expect = 43)
m_dr_f   <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_dr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dDR10, fiscal (placebo)", n_expect = 43)
cmp <- function(r, u, label) {
  a <- anova(r, u)
  cat(sprintf("%-30s F(%d,%d)=%7.3f  p=%.5f  dAIC=%+7.2f  adjR2 %.4f -> %.4f\n",
              label, a$Df[2], a$Res.Df[2], a$F[2], a$`Pr(>F)`[2],
              AIC(u) - AIC(r), summary(r)$adj.r.squared, summary(u)$adj.r.squared))
}
cmp(pl_trend,  m_trend,  "trend: add dOADR10")    # F=14.145 p=0.0006 dAIC=-11.92
cmp(pl_trend,  m_dr,     "trend: add dDR10")      # F= 1.32 p=0.2582 dAIC= +0.48
cmp(pl_fiscal, m_fiscal, "fiscal: add dOADR10")   # F=19.07 p=0.0001 dAIC=-15.83
cmp(pl_fiscal, m_dr_f,   "fiscal: add dDR10")     # F= 1.03 p=0.3166 dAIC= +0.81

cat("\n--- Section 7: policy-era indicator, step vs window ---\n")
for (d in c("POL97", "L_POL97", "liberal"))
  run_model(reformulate(c("L_log_mig_rate","trend",d,"delta_oadr_10","L_ln_debt_gdp","L_unem"),
                        "log_mig_rate"), reg_df, paste("policy era:", d),
            n_expect = if (d == "L_POL97") 42 else 43)   # the lagged step loses 1979
mpr_full <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 + L_ln_debt_gdp +
                        L_unem + POL97 + POL02 + EXP04 + ENL86 + ENL95,
                      reg_df, "all five MPR dummies", n_expect = 43)
DUM <- c("POL97", "POL02", "EXP04", "ENL86", "ENL95")
# The text quotes the HAC-robust joint test (Section 7: F(5,30) = 1.23, p = 0.32
# fiscal; 1.67, p = 0.17 trend); the classical anova() is printed for reference.
cat("-- joint test of the five dummies (classical) --\n"); print(anova(m_trend, mpr_full))
cat("-- joint test of the five dummies (HAC), trend spec --\n")
print(car::linearHypothesis(mpr_full, paste0(DUM, " = 0"), vcov. = vcovHAC(mpr_full)))
mpr_full_f <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 + L_ln_debt_gdp +
                          L_unem + POL97 + POL02 + EXP04 + ENL86 + ENL95,
                        reg_df, "all five MPR dummies, fiscal spec", n_expect = 43)
cat("-- joint test of the five dummies (HAC), fiscal spec --\n")
print(car::linearHypothesis(mpr_full_f, paste0(DUM, " = 0"), vcov. = vcovHAC(mpr_full_f)))
mpr_int <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 + L_ln_debt_gdp +
                       L_unem + POL97 + delta_oadr_10:POL97,
                     reg_df, "step + interaction", n_expect = 43)

# Mitchell, Pain and Riley's own dependent variable is the annual CHANGE in the
# log rate. Differencing discards the persistence that carries most of the
# variation (adj R2 falls to ~0.07) but renders both sides stationary, which
# removes the spurious-regression concern the bounds test addresses indirectly.
cat("\n--- Section 7: differenced DV, as in Mitchell, Pain and Riley ---\n")
reg_df$d_log_mig <- c(NA, diff(reg_df$log_mig_rate))
reg_df$d_tot_mig <- c(NA, diff(reg_df$tot_log_mig_rate))
for (v in c("d_log_mig", "d_tot_mig"))
  run_model(reformulate(c("delta_oadr_10","L_unem","L_ln_debt_gdp","L_POL97","POL02","EXP04"), v),
            reg_df, paste("differenced:", v), n_expect = 42)

# -----------------------------------------------------------------------------
# 3. Table 3: bounds tests and long-run multipliers (fiscal specification)
# -----------------------------------------------------------------------------
# The bounds-test p-value does NOT come from the standard F distribution:
# Pesaran, Shin and Smith (2001) give critical-value bounds, and the ARDL
# package computes exact p-values by response surface for the given k, T and
# deterministic case. Case 3 = unrestricted intercept, no trend.
#
# Two-step verification (see "Step 1" block below):
#   (a) the manual Wald test must reproduce the F STATISTIC  -> validates the
#       error-correction specification;
#   (b) the ARDL package must reproduce the P-VALUE          -> validates the
#       distribution and the case.
# Target values (HPFD vintage): F = 4.9398, t = -3.8288, k = 4, T = 40; the
# March-2026 databank vintage gave F = 5.0712, t = -3.8733. The STATISTICS are
# deterministic. The exact p-values are NOT: they are computed by simulating the
# response surface, which draws on R's RNG, so they move in the third decimal
# from call to call (observed over nine draws: F 0.0281-0.0303, t 0.0682-0.0715,
# across ARDL 0.2.4 and 0.2.5). Seeding immediately before each call therefore
# fixes them exactly, which is what the two set.seed() lines below are for.
# With set.seed(20260802) immediately before each call the values are
#   F = 4.9398, p = 0.03454      t = -3.8288, p = 0.07478
# on ARDL 0.2.5 (HPFD vintage; 5.0712/0.03028 and -3.8733/0.06910 on the databank vintage). Both round to the two decimals the manuscript reports (0.03 and
# 0.07). Note that seeding BOTH calls from the same seed changes the t p-value
# relative to an unseeded run, where the t-test drew from whatever state the
# F-test left behind; 0.06910 and the unseeded values are draws from the same
# distribution, and the conclusion is identical either way.


ts_df <- ts(reg_df[, c("log_mig_rate", "delta_oadr_10", "unem",
                       "ln_debt_gdp", "tax_workpop")],
            start = min(reg_df$year), frequency = 1)

# order = c(1,1,1,1,1) reproduces the UECM tested in the paper:
#   d(y) ~ L(y,1) + L(x,1) + d(x)
m_ardl <- ardl(log_mig_rate ~ delta_oadr_10 + unem + ln_debt_gdp + tax_workpop,
               data = ts_df, order = c(1, 1, 1, 1, 1))

# exact = TRUE uses the small-sample critical-value surface for the actual T.
# Without it the package returns ASYMPTOTIC p-values (printed as T = 1000),
# which here are more favourable (F: 0.0096, t: 0.0666) than the exact values
# reported in the paper. The exact figures are the conservative ones and are
# the correct choice at T = 40.
# The seed fixes the response-surface draw only. It cannot affect the F or t
# STATISTIC, which is computed from the data by least squares; if a replicator
# obtains a different statistic, the seed is not the explanation.
cat("--- Bounds F-test (ARDL package, case 3, exact) ---\n")
set.seed(20260802)
bf <- bounds_f_test(m_ardl, case = 3, exact = TRUE); print(bf)   # HPFD vintage: F = 4.9398, p = 0.0345
cat("--- Bounds t-test (ARDL package, case 3, exact) ---\n")
set.seed(20260802)
bt <- bounds_t_test(m_ardl, case = 3, exact = TRUE); print(bt)   # HPFD vintage: t = -3.8288, p = 0.0748
# Verified against the manuscript: F = 4.9398, t = -3.8288, k = 4, T = 40,
# case 3, and the long-run multipliers of Table 3 (0.0159, -0.0432, 1.0156,
# 0.0933). The two statistics are deterministic and reproduce exactly.
# The p-values do NOT reproduce unseeded: repeated calls returned F in
# 0.0281-0.0303 and t in 0.0682-0.0715 (ARDL 0.2.4 and 0.2.5). With the seeds
# set above they are exactly reproducible. The manuscript reports them to two
# decimals (p = 0.03 and p = 0.07), which holds across every draw observed and
# carries the same conclusion either way: rejection at 5 per cent on the F, not
# on the t. Do not treat a third-decimal difference as a failure to replicate;
# treat a different STATISTIC as one.
#
# NOTE ON SESSION HYGIENE. This script assumes a genuinely clean session. If
# RStudio is set to restore .RData at startup, the saved .Random.seed comes back
# with it and every "fresh" session begins from the same RNG state -- which
# silently makes unseeded draws look reproducible when they are not. The
# project file shipped with the package pins RestoreWorkspace: No and
# SaveWorkspace: No, so opening the folder AS AN RSTUDIO PROJECT enforces the
# requirement automatically; the hygiene travels with the package rather than
# depending on the replicator's global settings. Running outside the project
# (plain R, Rscript, or a bare setwd()), check the global option instead:
# Tools > Global Options > General > uncheck "Restore .RData into workspace at
# startup", and set "Save workspace to .RData on exit" to Never.
cat("--- Long-run multipliers (Table 3) ---\n")
print(multipliers(m_ardl, type = "lr"))
# Verified: statistics and long-run multipliers reproduce Table 3 exactly
# (0.0159, -0.0432, 1.0156, 0.0933); case 3 confirmed.

# --- Step 1: manual Wald cross-check on the F statistic -----------------------
# Same hypothesis, computed by hand. The STATISTIC should equal the package's
# F to within rounding; the p-value printed here is from the ordinary F
# distribution and is EXPECTED to differ -- do not report it.

stopifnot(!is.unsorted(reg_df$year))     # lag construction assumes year order
lag1 <- function(x) c(NA, head(x, -1))

ecm_df <- data.frame(
  d_y    = c(NA, diff(reg_df$log_mig_rate)),
  L_y    = lag1(reg_df$log_mig_rate),
  L_oadr = lag1(reg_df$delta_oadr_10),
  L_un   = lag1(reg_df$unem),
  L_debt = lag1(reg_df$ln_debt_gdp),
  L_tax  = lag1(reg_df$tax_workpop),
  d_oadr = c(NA, diff(reg_df$delta_oadr_10)),
  d_un   = c(NA, diff(reg_df$unem)),
  d_debt = c(NA, diff(reg_df$ln_debt_gdp)),
  d_tax  = c(NA, diff(reg_df$tax_workpop))
)
ecm <- lm(d_y ~ L_y + L_oadr + L_un + L_debt + L_tax +
            d_oadr + d_un + d_debt + d_tax, data = ecm_df)

cat("--- Step 1: manual Wald F (statistic only) ---\n")
wald <- linearHypothesis(ecm, c("L_y=0", "L_oadr=0", "L_un=0",
                                "L_debt=0", "L_tax=0"))
print(wald)
# Compared against the package's OWN statistics from this run, not against a
# typed constant, so the check cannot go stale when the data vintage changes.
cat(sprintf("Manual F = %.4f  vs package F = %.4f  | difference %.4f\n",
            wald$F[2], unname(bf$statistic), wald$F[2] - unname(bf$statistic)))
cat(sprintf("Manual t on L_y = %.4f  vs package t = %.4f  | difference %.4f\n",
            coeftest(ecm)["L_y", "t value"], unname(bt$statistic),
            coeftest(ecm)["L_y", "t value"] - unname(bt$statistic)))
stopifnot(abs(wald$F[2] - unname(bf$statistic)) < 1e-3)   # the ECM must reproduce the package's F
cat(sprintf("N used = %d (expect 42), k = 4\n", nobs(ecm)))
# If the statistic matches but the paper's p-value does not reproduce, the
# problem is the case; if the STATISTIC differs, the ECM specification differs
# (check lag construction, variable set, or estimation sample).

# Delta-method long-run multipliers, for comparison with multipliers() above
for (x in c("L_oadr", "L_un", "L_debt", "L_tax")) {
  print(deltaMethod(ecm, paste0("-", x, "/L_y")))
}

# -----------------------------------------------------------------------------
# 3b. Section 5.2: finite-sample bias in the autoregressive coefficient
# -----------------------------------------------------------------------------
# OLS with a lagged dependent variable is consistent but biased in finite
# samples; the attenuation in rho is of order -(1+3*rho)/T (Hurwicz 1950;
# Kendall 1954). Long-run multipliers b/(1-rho) are correspondingly understated.
# (a) analytic correction; (b) recursive residual bootstrap, which also gives
# bootstrap CIs for the long-run response -- preferred to the delta method at
# T = 41. Report the bootstrap figures in Section 5.2.

Tn   <- nobs(tax_spec)
rho0 <- coef(tax_spec)["L_log_mig_rate"]
b0   <- coef(tax_spec)["delta_oadr_10"]
rho_kendall <- rho0 + (1 + 3 * rho0) / Tn
cat(sprintf("Kendall correction: rho %.4f -> %.4f;  LR %.4f -> %.4f\n",
            rho0, rho_kendall, b0 / (1 - rho0), b0 / (1 - rho_kendall)))

# Recursive-design residual bootstrap: the dependent variable is generated
# forward through the estimated AR structure with the exogenous regressors held
# at their observed values, which is what reproduces the bias. (A fixed-design
# bootstrap that perturbs fitted values does not.) Errors are resampled iid,
# which the Breusch-Godfrey results license; use block resampling otherwise.

boot_ardl <- function(data, yname, lagname, xnames, B = 2000, seed = 20260802) {
  set.seed(seed)
  f  <- reformulate(c(lagname, xnames), response = yname)
  m0 <- lm(f, data = data)
  d  <- model.frame(m0); n <- nrow(d)
  cf <- coef(m0); res <- residuals(m0)
  a0 <- cf[["(Intercept)"]]; rho0 <- cf[[lagname]]; bx <- cf[xnames]
  X  <- as.matrix(d[, xnames, drop = FALSE])
  y0 <- d[[lagname]][1]                       # initial condition (conditional bootstrap)
  out <- matrix(NA_real_, B, length(cf), dimnames = list(NULL, names(cf)))
  for (b in seq_len(B)) {
    e <- sample(res, n, replace = TRUE)
    ys <- ylag <- numeric(n); prev <- y0
    for (t in seq_len(n)) {
      ylag[t] <- prev
      ys[t]   <- a0 + rho0 * prev + sum(bx * X[t, ]) + e[t]
      prev    <- ys[t]
    }
    db <- data.frame(ys, ylag, X); names(db) <- c(yname, lagname, xnames)
    out[b, ] <- coef(lm(f, data = db))
  }
  list(fit = m0, boot = out, rho0 = rho0, coef0 = cf)
}

report_boot <- function(bo, lagname, xkey) {
  rho0 <- bo$rho0; b0 <- bo$coef0[[xkey]]
  rstar <- bo$boot[, lagname]; bstar <- bo$boot[, xkey]
  bias  <- mean(rstar) - rho0
  rho_c <- rho0 - bias
  # Percentile interval from RECENTRED draws: the raw percentile interval is
  # generated under the biased rho and inherits the same downward bias.
  lr_c  <- bstar / (1 - (rstar - bias))
  cat(sprintf("rho: OLS %.4f | boot mean %.4f | bias %.4f | corrected %.4f\n",
              rho0, mean(rstar), bias, rho_c))
  cat(sprintf("LR:  point %.4f | bias-corrected %.4f | 95%% CI (recentred) [%.4f, %.4f]\n",
              b0/(1-rho0), b0/(1-rho_c),
              quantile(lr_c, .025), quantile(lr_c, .975)))
  invisible(list(rho_corrected = rho_c, lr_ci = quantile(lr_c, c(.025, .975))))
}

bo <- boot_ardl(reg_df, "log_mig_rate", "L_log_mig_rate",
                c("delta_oadr_10", "L_unem", "L_tax_workpop", "L_ln_debt_gdp"))
report_boot(bo, "L_log_mig_rate", "delta_oadr_10")

# --- Robustness: moving-block bootstrap --------------------------------------
# The iid resampling above is licensed by Breusch-Godfrey. BG(1) and BG(2) are
# comfortable, but BG(4) on this same fiscal specification returns p = 0.098 --
# no rejection at 5 per cent, yet the weakest of the sixteen BG tests in this
# script, and on precisely the model being bootstrapped. A moving-block
# bootstrap keeps consecutive residuals together, so if the intervals agree the
# iid assumption is not doing any work and the Section 5.2 figures stand; if
# they diverge, the block version is the one to report.
#
# Block length. A block of length L preserves dependence only up to lag L - 1:
# a run of L consecutive residuals contains pairs at separations 1 ... L-1, not
# L. To speak to BG(4) the block must therefore reach L = 5. Lengths 2 and 4
# are retained as a gradient -- they show the interval is not sensitive to the
# choice -- but L = 5 is the one commensurate with the diagnostic that prompted
# the check, and is the length to cite.
#
# Known limitation. Block starts are drawn from 1 ... n-L+1 with no wraparound,
# so residuals near either end of the sample enter fewer blocks than interior
# ones: at L = 5 the first and last residual can appear in one block each,
# against five for a residual in the interior. This is the standard edge effect
# of the non-circular moving-block bootstrap. A circular version would equalise
# coverage; it is not used here because the check is corroborative rather than
# the reported interval, and because the residuals show no trend or level shift
# at the sample boundaries that would make the under-weighted observations
# unrepresentative.
boot_ardl_block <- function(data, yname, lagname, xnames, B = 2000, L = 4,
                            seed = 20260802) {
  set.seed(seed)
  f  <- reformulate(c(lagname, xnames), response = yname)
  m0 <- lm(f, data = data)
  d  <- model.frame(m0); n <- nrow(d)
  cf <- coef(m0); res <- residuals(m0)
  a0 <- cf[["(Intercept)"]]; rho0 <- cf[[lagname]]; bx <- cf[xnames]
  X  <- as.matrix(d[, xnames, drop = FALSE])
  y0 <- d[[lagname]][1]
  nblk <- ceiling(n / L); nstart <- n - L + 1   # overlapping (moving) blocks
  out <- matrix(NA_real_, B, length(cf), dimnames = list(NULL, names(cf)))
  for (b in seq_len(B)) {
    st <- sample.int(nstart, nblk, replace = TRUE)
    e  <- unlist(lapply(st, function(s) res[s:(s + L - 1)]))[seq_len(n)]
    ys <- ylag <- numeric(n); prev <- y0
    for (t in seq_len(n)) {
      ylag[t] <- prev
      ys[t]   <- a0 + rho0 * prev + sum(bx * X[t, ]) + e[t]
      prev    <- ys[t]
    }
    db <- data.frame(ys, ylag, X); names(db) <- c(yname, lagname, xnames)
    out[b, ] <- coef(lm(f, data = db))
  }
  list(fit = m0, boot = out, rho0 = rho0, coef0 = cf)
}
for (L in c(2, 4, 5)) {
  cat(sprintf("\nmoving-block bootstrap, block length %d:\n", L))
  bb <- boot_ardl_block(reg_df, "log_mig_rate", "L_log_mig_rate",
                        c("delta_oadr_10", "L_unem", "L_tax_workpop",
                          "L_ln_debt_gdp"), L = L)
  report_boot(bb, "L_log_mig_rate", "delta_oadr_10")
}
cat("\nL = 5 is the length matched to BG(4). Compare its interval with the iid",
    "\ninterval [0.0092, 0.0294]: agreement -- overlap, with a lower bound no",
    "\nsmaller -- means the Section 5.2 figures are not an artefact of the iid",
    "\nassumption.\n")
# Obtained (Section 5.2, iid): rho OLS 0.4909, boot mean 0.3868, bias -0.1041,
# corrected 0.5950; LR point 0.0160, bias-corrected 0.0201, recentred 95%
# percentile interval [0.0092, 0.0294].
# Obtained (blocks): L = 2: 0.0208 [0.0093, 0.0301]; L = 4: 0.0214
# [0.0099, 0.0295]; L = 5: 0.0211 [0.0095, 0.0282]. The L = 5 interval lies
# inside the iid interval with a higher lower bound, so the iid figures the
# paper reports are the conservative ones (Online Appendix B).
# The bootstrap bias exceeds the analytic -(1+3rho)/T = -0.060 (at the
# estimated rho of 0.4909; -0.061 is the value at rho = 0.5) because the
# analytic result assumes a pure autoregression, whereas this equation includes
# four persistent, mutually correlated regressors.

# Monte Carlo calibration of the procedure (Section 5.2 footnote): on simulated
# data with known persistence at T = 41, OLS bias should match the analytic
# value and the bootstrap correction should be nearly unbiased.
mc_check <- function(rho_true = 0.5, T = 43, R = 500, B = 400) {
  # Seeded INSIDE the function so an isolated interactive call reproduces the
  # documented values, not only a scripted run. (The seed previously sat at
  # the call site; the RNG stream, and therefore every obtained value, is
  # identical under either placement.)
  set.seed(20260802)
  raw <- corr <- numeric(R)
  for (r in seq_len(R)) {
    x <- rnorm(T); y <- numeric(T); y[1] <- rnorm(1)
    for (t in 2:T) y[t] <- 0.2 + rho_true * y[t-1] + 0.3 * x[t] + rnorm(1)
    d  <- data.frame(y = y[-1], Ly = y[-T], x = x[-1])
    bb <- boot_ardl(d, "y", "Ly", "x", B = B, seed = r)
    raw[r]  <- bb$rho0
    corr[r] <- bb$rho0 - (mean(bb$boot[, "Ly"]) - bb$rho0)
  }
  cat(sprintf("true rho %.4f | mean OLS %.4f (bias %.4f; analytic %.4f) | mean corrected %.4f (bias %.4f)\n",
              rho_true, mean(raw), mean(raw) - rho_true, -(1 + 3*rho_true)/T,
              mean(corr), mean(corr) - rho_true))
  cat(sprintf("RMSE: raw %.4f -> corrected %.4f\n",
              sqrt(mean((raw-rho_true)^2)), sqrt(mean((corr-rho_true)^2))))
}
# Seeded (inside the function) so the printout depends neither on how much of
# the ambient RNG stream earlier sections consumed nor on whether it is run
# from the script or interactively. (Unseeded it was deterministic for the
# script as written, but shifted in the third decimal whenever upstream RNG
# use changed; the values recorded below are the seeded ones and are the only
# ones current.) Judge the line by its criterion -- OLS bias close to the
# analytic -0.0610, corrected bias near zero -- not digit for digit.
mc_check()
# Obtained (seeded, R 4.6.1): mean OLS 0.4384 (bias -0.0616; analytic -0.0610),
# mean corrected 0.4897 (bias -0.0103); RMSE raw 0.1581 -> corrected 0.1565.
# These are now fixed by the seed and no longer move with upstream edits.

# -----------------------------------------------------------------------------
# 4. Table 4 and Section 7: falsification and robustness (fiscal first, trend alongside)
# -----------------------------------------------------------------------------

# (1) Total dependency ratio placebo
# Table 4 is typeset in the FISCAL specification (build_tables.R); the trend
# versions go to Online Appendix Table B3. Both are printed here.
run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_dr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "Table 4 (1): total DR placebo (fiscal)", n_expect = 43)
run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "Table B3 (1): total DR placebo (trend)", n_expect = 43)

# (2) EU inflows placebo  [CHECK: accession-wave indicator variants equivalent]
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + L_tax_workpop + delta_oadr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "Table 4 (2): EU inflows placebo (fiscal)", n_expect = 43)
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "Table B3 (2): EU inflows placebo (trend)", n_expect = 43)

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
            L_tax_workpop + L_ln_debt_gdp, reg_df, "gross, log, fiscal", n_expect = 43)   # 0.0080
run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "gross, log, trend", n_expect = 43)            # 0.0073
run_model(alt_mig_rate ~ L_alt_mig_rate + delta_oadr_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "gross, level, fiscal", n_expect = 43) # 1375.4
run_model(alt_mig_rate ~ L_alt_mig_rate + delta_oadr_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "gross, level, trend", n_expect = 43)          # 1294.6
eu_gross <- run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "eu_gross")
eu_gross2 <- run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
                        L_tax_oadr_0 + L_ln_debt_gdp, reg_df, "eu_gross2")

# (4) Projected 10-year OADR level in place of the change
run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + level_oadr_10 +
            L_unem + L_ln_debt_gdp, reg_df, "Table 4 (4): OADR level (fiscal)", n_expect = 43)
run_model(log_mig_rate ~ L_log_mig_rate + trend + level_oadr_10 +
            L_unem + L_ln_debt_gdp, reg_df, "Table B3 (4): OADR level (trend)", n_expect = 43)

# (5) Zero-net-migration projections (Section 7, first item)
ardl_zm  <- run_model(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
                        L_tax_workpop + L_ln_debt_gdp,
                      reg_df, "Section 7 / Table 4 (5): zero-mig, fiscal spec", n_expect = 43)
full_diag(ardl_zm, "zm fiscal spec")
ardl_zm2 <- run_model(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
                        trend + L_ln_debt_gdp,
                      reg_df, "Table 4 (5): zero-mig, trend spec", n_expect = 43)
full_diag(ardl_zm2, "zm trend spec")

# 1998-2007 policy-era indicator horse race (Section 7). Named hr_base, not
# `base`: that name is shared with verify.R in the same workflow (and with the
# base package), and a partial interactive re-run of this block after sourcing
# verify.R would otherwise hand anova() a numeric vector.
# Run in both specifications; the fiscal one is what the text quotes.
for (sp in c(fiscal = "L_tax_workpop", trend = "trend")) {
  lab <- if (sp == "trend") "trend" else "fiscal"
  hr_base <- run_model(reformulate(c("L_log_mig_rate", "delta_oadr_10", "L_unem", "L_ln_debt_gdp", sp),
                                   "log_mig_rate"), reg_df, paste0("Horse race: baseline (", lab, ")"), n_expect = 43)
  withdum <- run_model(reformulate(c("L_log_mig_rate", "delta_oadr_10", "L_unem", "L_ln_debt_gdp", sp, "liberal"),
                                   "log_mig_rate"), reg_df, paste0("Horse race: + liberal dummy (", lab, ")"), n_expect = 43)
  withint <- run_model(reformulate(c("L_log_mig_rate", "delta_oadr_10", "L_unem", "L_ln_debt_gdp", sp, "liberal",
                                     "delta_oadr_10:liberal"), "log_mig_rate"),
                       reg_df, paste0("Horse race: + dummy + interaction (", lab, ")"), n_expect = 43)
  cat("-- joint F, liberal + interaction (classical) --\n"); print(anova(hr_base, withint))
  cat("-- joint F, liberal + interaction (HAC) --\n")
  print(car::linearHypothesis(withint, c("liberal = 0", "delta_oadr_10:liberal = 0"), vcov. = vcovHAC(withint)))
}
# cut() is right-closed, so an upper break of 1996 would put 1997 in the
# "liberal" bin while the `liberal` dummy above excludes it. The break below
# is 1997 so the printed means describe the same window the paper names.
reg_df$period <- cut(reg_df$year, breaks = c(1976, 1997, 2007, 2019),
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
            n_expect = 43)
}

# Total dependency ratio across horizons and in levels (Section 7: the
# substitution "at every horizon and in levels as in deltas" yields nothing).
for (h in c("delta_dr_1", "delta_dr_3", "delta_dr_5", "level_dr_10")) for (sp in names(SPECS)) {
  run_model(reformulate(c("L_log_mig_rate", SPECS[sp], h, "L_ln_debt_gdp", "L_unem"),
                        "log_mig_rate"), reg_df, paste0("Section 7 DR placebo: ", h, " (", sp, ")"),
            n_expect = 43)
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
if (file.exists("dr_corrected.csv")) {   # the sheet carries the column from 1979; 1978 comes from the generated file (28 Sep 2026)
  .drc <- read.csv("dr_corrected.csv"); .i <- is.na(reg_df$delta_dr_10_pub) & !is.na(reg_df$delta_oadr_10_pub); reg_df$delta_dr_10_pub[.i] <- num(.drc$delta_dr_10_pub[match(reg_df$year[.i], .drc$year)]) }

# All three must be complete over 1979-2019. An incomplete column means the
# spreadsheet paste is misaligned: a series pasted one row high runs off the end
# and leaves the final year blank, which is how a one-year shift announces
# itself. N = 40 instead of 41 is the symptom.
# On the 1977 start the publication-dated columns legitimately begin in 1978 (the first vintage PUBLISHED after the
# sample starts), so completeness is asserted from 1978 on (28 Sep 2026).
.pubrows <- reg_df$year >= 1978
stopifnot(!any(is.na(reg_df$delta_oadr_10_pub[.pubrows])),
          !any(is.na(reg_df$delta_zm_10_pub[.pubrows])),
          !any(is.na(reg_df$delta_dr_10_pub[.pubrows])))

# Cross-check the sheet against the generated file where both are present. A
# shift wholly inside the sample would pass the NA test but fail this one.
if ("delta_dr_10_pub" %in% names(pub) && file.exists("dr_corrected.csv")) {
  .ref <- read.csv("dr_corrected.csv")
  .gap <- max(abs(reg_df$delta_dr_10_pub -
                    .ref$delta_dr_10_pub[match(reg_df$year, .ref$year)]), na.rm = TRUE)   # 1977 is blank on the 1977 start
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
                       reg_df, "Section 7 pub-dated: trend spec", n_expect = 42)      # 0.0075
pub_fisc  <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                         delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: fiscal spec", n_expect = 42)     # 0.0086
pub_zm    <- run_model(log_mig_rate ~ L_log_mig_rate + trend +
                         delta_zm_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: zero-migration (trend)", n_expect = 42)  # 0.0067
# Fiscal-specification counterparts (the text quotes the fiscal specification).
pub_zm_f  <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                         delta_zm_10_pub + L_ln_debt_gdp + L_unem,
                       reg_df, "Section 7 pub-dated: zero-migration (fiscal)", n_expect = 42)

# Placebos under the same dating convention, so the robustness check and the
# falsification tests share a timing assumption.
if (!all(is.na(reg_df$delta_dr_10_pub)))
  pub_dr <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10_pub +
                        L_ln_debt_gdp + L_unem,
                      reg_df, "Section 7 pub-dated: total DR placebo", n_expect = 42) # -0.0001
pub_eu <- run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend +
                      delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                    reg_df, "Section 7 pub-dated: EU placebo (trend)", n_expect = 42)         # p = 0.40
if (!all(is.na(reg_df$delta_dr_10_pub)))
  pub_dr_f <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_dr_10_pub +
                          L_ln_debt_gdp + L_unem,
                        reg_df, "Section 7 pub-dated: total DR placebo (fiscal)", n_expect = 42)
pub_eu_f <- run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + L_tax_workpop +
                        delta_oadr_10_pub + L_ln_debt_gdp + L_unem,
                      reg_df, "Section 7 pub-dated: EU placebo (fiscal)", n_expect = 42)

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
# 5. Online Appendix Table B1: orders of integration, sequential classification
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
            "oadr_0", "delta_oadr_10", "level_oadr_10", "tax_workpop",
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
# 6. Online Appendix Table B2: OLS / HAC / Newey-West comparison
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



# -----------------------------------------------------------------------------
# 7. Online Appendix A: independent reconstruction of the projection series
# -----------------------------------------------------------------------------
# Rebuilds the zero-net-migration OADR series directly from the 22 National
# Population Projection vintage workbooks and checks it against the delta_zm_10
# column merged into Dissertation_Dataset_Final.xlsx. The original construction
# was done in Python; this exists so the series is verified in the same language
# as the rest of the package. Folded in from the former build_zero_migration.R.
#
#   OADR_zm(y+h)  = OADR(y+h) * WA(y+h) / (WA(y+h) - cumM(y, y+h))
#   delta_zm(y,h) = OADR_zm(y+h) - OADR(y)
#   vintage in force for year y = most recent vintage with base <= y
#   all net migrants treated as working-age (denominator only)
#
# CHECK 1 published delta vs delta_oadr_10 in the sheet   (expect ~0.263; tolerance 0.30)
# CHECK 2 zero-migration delta vs delta_zm_10 in the sheet (expect ~ 1e-4)
# CHECK 3 re-estimation on the R-built series              (expect 0.0075, HAC 0.0013)
#
# The block is skipped, with a message, if the vintage workbooks are not present,
# so the rest of the replication still runs for a user who has only the workbook.
# -----------------------------------------------------------------------------

# The vintage workbooks may sit in the working directory, in a subfolder, or
# anywhere below it. locate() searches rather than assuming a fixed path.
npp_dir <- "."                    # root of the search
xlsx    <- DATA_XLSX              # same resolved file as Sections 1 and 4b
HMAX    <- 11

locate <- function(fname) {
  direct <- file.path(npp_dir, fname)
  if (file.exists(direct)) return(direct)
  hits <- list.files(npp_dir, pattern = paste0("^", gsub("([.|()\\^{}+$*?\\[\\]])",
                     "\\\\\\1", fname), "$"), recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

FILES <- c(
  "1975" = "wuk75cc_tcm77-395111.xls", "1977" = "wuk77cc_tcm77-395021.xls", "1979" = "wuk79cc_tcm77-394992.xls",
  "1981" = "wuk81cc_tcm77-384614.xls", "1983" = "wuk83cc_tcm77-384103.xls",
  "1985" = "wuk85cc_tcm77-384028.xls", "1987" = "wuk87cc_tcm77-383757.xls",
  "1989" = "wuk89cc_tcm77-383486.xls", "1991" = "wuk91cc_tcm77-383156.xls",
  "1992" = "wuk92cc_tcm77-382886.xls", "1994" = "wuk94cc_tcm77-382733.xls",
  "1996" = "wuk96cc_tcm77-382563.xls", "1998" = "wuk98cc_tcm77-382317.xls",
  "2000" = "wuk00cc_tcm77-379749.xls", "2002" = "wuk02cc_tcm77-379889.xls",
  "2004" = "wuk04cc_tcm77-380371.xls", "2006" = "wuk06cc_tcm77-303310.xls",
  "2008" = "wUK08cc_tcm77-160543.xls", "2010" = "wuk10c_tcm77-235404.xls",
  "2012" = "wuk12cc_tcm77-329789.xls", "2014" = "2014 npp.xls",
  "2016" = "2016 NPP.xls",             "2018" = "ukpppsummary18.xls"
)

WA_LABELS <- c("working age", "16-64/59")
PA_LABELS <- c("pension age", "65/60+", "pensionable age")

# --- helpers -----------------------------------------------------------------
# Distinct name: Section 4b defines its own num() with different behaviour on
# factors, and a second definition here would silently shadow it if the sections
# were ever reordered.
num_cell <- function(x) suppressWarnings(as.numeric(x))

# contiguous run of numbers along a row, starting at column `from`
run_of <- function(row, from) {
  out <- numeric(0)
  for (j in from:length(row)) {
    v <- num_cell(row[j])
    if (is.na(v)) break
    out <- c(out, v)
  }
  out
}

parse_vintage <- function(path, expect_base) {
  m   <- suppressMessages(read_excel(path, sheet = "PERSONS",
                                     col_names = FALSE, col_types = "text"))
  m   <- as.data.frame(m, stringsAsFactors = FALSE)
  lab <- tolower(trimws(ifelse(is.na(m[[1]]), "", m[[1]])))

  if (!("ages" %in% lab)) stop("no 'Ages' row found in the PERSONS sheet")
  ages_row <- which(lab == "ages")[1]
  years    <- run_of(unlist(m[ages_row, ]), 2)
  if (length(years) < 12 || !all(diff(years) == 1))
    stop(sprintf("year header not contiguous (%d values read)", length(years)))

  allages_row <- which(lab == "all ages" & seq_along(lab) > ages_row)[1]
  allages     <- run_of(unlist(m[allages_row, ]), 2)

  # Label varies across vintages: "Migration", "Net migration",
  # "Migration & other changes+". Take the first such row above the ages table.
  mig_row <- which((startsWith(lab, "migration") | lab == "net migration") &
                     seq_along(lab) < ages_row)[1]
  if (is.na(mig_row)) stop("no migration row found above the ages table")
  mig     <- run_of(unlist(m[mig_row, ]), 3)      # components block starts col 3
  if (length(mig) < 12) stop(sprintf("migration row too short (%d values)", length(mig)))

  ps <- num_cell(m[which(startsWith(lab, "population at start"))[1], 3])
  pe <- num_cell(m[which(startsWith(lab, "population at end"))[1],   3])
  if (is.na(ps) || is.na(pe)) stop("population at start/end not readable in column 3")

  # Some vintages tabulate ages from the base year, others from base + 1.
  comp_base <- if (abs(allages[1] - ps) < 1.5) years[1] else
               if (abs(allages[1] - pe) < 1.5) years[1] - 1 else
               stop("cannot align components block: ", basename(path))
  stopifnot(comp_base == expect_base)

  dep_row  <- which(startsWith(lab, "dependants per"))[1]
  if (is.na(dep_row)) stop("no 'Dependants per ...' block found")
  oadr_row <- which(lab %in% PA_LABELS &
                      seq_along(lab) > dep_row & seq_along(lab) <= dep_row + 4)[1]
  if (is.na(oadr_row)) stop("no pension-age row inside the dependants block")
  oadr     <- run_of(unlist(m[oadr_row, ]), 2)

  wa_row <- which(lab %in% WA_LABELS &
                    seq_along(lab) > allages_row & seq_along(lab) < dep_row)[1]
  pa_row <- which(lab %in% PA_LABELS &
                    seq_along(lab) > allages_row & seq_along(lab) < dep_row)[1]
  if (is.na(wa_row) || is.na(pa_row))
    stop("working-age / pension-age population rows not found between the ",
         "ages table and the dependants block")
  wa <- run_of(unlist(m[wa_row, ]), 2)
  pa <- run_of(unlist(m[pa_row, ]), 2)

  n <- min(length(years), length(oadr), length(wa), length(pa))
  # Internal check: published OADR should equal 1000 * PA / WA except in
  # state-pension-age transition years, where ONS smooths the boundary.
  dev <- max(abs(oadr[1:n] - 1000 * pa[1:n] / wa[1:n]))
  if (dev > 0.6) message(sprintf("  note: %s published OADR departs from 1000*PA/WA (max %.1f)",
                                 basename(path), dev))

  list(base = comp_base, years = years[1:n],
       oadr = oadr[1:n], wa = wa[1:n], mig = mig)
}

# The reconstruction re-reads the original NPP .xls vintages (Mac Roman text); on a machine whose iconv lacks that encoding
# readxl aborts R, so the check can be skipped with SKIP_VINTAGE_CHECK=1 (28 Sep 2026). It never affects the tables above.
if (Sys.getenv("SKIP_VINTAGE_CHECK") == "1") { cat("\n[Online Appendix A vintage reconstruction skipped: SKIP_VINTAGE_CHECK=1]\n") } else {
# --- read all vintages --------------------------------------------------------
# Each vintage is parsed inside tryCatch so that a single awkward workbook
# reports itself instead of aborting the run.
V <- list(); failed <- character(0)
for (b in names(FILES)) {
  path <- locate(FILES[[b]])
  if (is.na(path)) {
    failed <- c(failed, sprintf("%s: file not found (%s)", b, FILES[[b]]))
    next
  }
  res <- tryCatch(parse_vintage(path, as.integer(b)),
                  error = function(e) {
                    failed <<- c(failed, sprintf("%s: %s", b, conditionMessage(e)))
                    NULL
                  })
  if (!is.null(res)) V[[b]] <- res
}

if (length(failed) == length(FILES)) {
  cat("\n[7] None of the 22 NPP vintage workbooks were found below the working\n",
      "    directory, so the independent reconstruction is skipped. The estimation\n",
      "    above is unaffected: it uses delta_zm_10 as merged into the workbook.\n",
      "    To run this section, place the vintage .xls files anywhere under the\n",
      "    working directory.\n", sep = "")
} else {
if (length(failed)) {
  cat("\nVINTAGES THAT DID NOT PARSE:\n"); cat(paste0("  ", failed, collapse = "\n"), "\n")
  cat("\nDiagnostic for the first failure -- the first column of that sheet,\n",
      "which is what the label matching reads:\n", sep = "")
  bad <- sub(":.*", "", failed[1])
  p   <- locate(FILES[[bad]])
  if (!is.na(p)) {
    mm <- suppressMessages(read_excel(p, sheet = "PERSONS", col_names = FALSE,
                                      col_types = "text"))
    print(utils::head(trimws(ifelse(is.na(mm[[1]]), "", mm[[1]])), 70))
  }
  stop("Fix the vintages listed above before relying on the reconstruction.")
}

bases <- as.integer(names(V))
cat(sprintf("parsed %d vintages: %d-%d\n", length(V), min(bases), max(bases)))

# --- construction -------------------------------------------------------------
at <- function(v, year, what) {
  i <- year - v$years[1] + 1
  if (i < 1 || i > length(v$years)) return(NA_real_)
  v[[what]][i]
}
cumM <- function(v, y0, y1) {                    # mid-year y0 to mid-year y1
  idx <- (y0 - v$base + 1):(y1 - v$base)
  if (any(idx < 1) || any(idx > length(v$mig))) return(NA_real_)
  sum(v$mig[idx])
}
vintage_for <- function(y) max(bases[bases <= y])

delta_zm <- function(y, h) {
  v <- V[[as.character(vintage_for(y))]]
  o_end <- at(v, y + h, "oadr"); w_end <- at(v, y + h, "wa")
  o_now <- at(v, y, "oadr");     cm    <- cumM(v, y, y + h)
  if (anyNA(c(o_end, w_end, o_now, cm))) return(NA_real_)
  o_end * w_end / (w_end - cm) - o_now
}
delta_pub <- function(y, h) {
  v <- V[[as.character(vintage_for(y))]]
  at(v, y + h, "oadr") - at(v, y, "oadr")
}

YEARS <- 1975:2019   # 1975-based is the earliest vintage held (added 22 Sep 2026)
out <- data.frame(year = YEARS)
for (h in 1:HMAX) {
  out[[paste0("delta_zm_", h)]]  <- vapply(YEARS, delta_zm,  numeric(1), h = h)
  out[[paste0("delta_pub_", h)]] <- vapply(YEARS, delta_pub, numeric(1), h = h)
}
write.csv(out, "zero_migration_deltas_R_1977.csv", row.names = FALSE)

# --- verification 1: published deltas must match the paper's own variable -----
ds <- as.data.frame(read_excel(xlsx, sheet = "ReformSPA"))
ds <- ds[!is.na(ds$year), ]
cmp <- merge(out[, c("year", "delta_zm_10", "delta_pub_10")],
             ds[, c("year", "delta_oadr_10", "delta_zm_10")],
             by = "year", suffixes = c("_R", "_sheet"))
cmp <- subset(cmp, year >= 1977 & year <= 2019)

d_pub <- max(abs(cmp$delta_pub_10 - cmp$delta_oadr_10), na.rm = TRUE)
d_zm  <- max(abs(cmp$delta_zm_10_R - cmp$delta_zm_10_sheet), na.rm = TRUE)

cat(sprintf("\nCHECK 1  published 10-yr delta, R rebuild vs delta_oadr_10 in sheet : max |diff| = %.6f\n", d_pub))
cat(sprintf("CHECK 2  zero-migration 10-yr delta, R rebuild vs delta_zm_10 in sheet: max |diff| = %.6f\n", d_zm))
cat(sprintf("         (1997 published = %.2f, expected 6.14; 1997 zero-mig = %.2f, expected 11.57)\n",
            cmp$delta_pub_10[cmp$year == 1997], cmp$delta_zm_10_R[cmp$year == 1997]))

# The two series have different expected tolerances and need different
# thresholds. The PUBLISHED series is compared against a column transcribed to
# one decimal place from printed tables in the early vintages, so a discrepancy
# of a few tenths is expected and is documented in Online Appendix A (max 0.263;
# re-estimating on the full-precision rebuild moves the coefficient from 0.00816
# to 0.00818). The ZERO-MIGRATION series is compared against a column built by
# the same arithmetic, so it should agree to floating-point noise.
TOL_PUB <- 0.30   # transcription precision in the early vintages
TOL_ZM  <- 0.01   # arithmetic agreement; observed ~9e-05
if (d_zm > TOL_ZM) {
  warning("The zero-migration rebuild does not match delta_zm_10 in the sheet ",
          sprintf("(max |diff| = %.6f). Investigate before relying on Section 7.", d_zm))
} else if (d_pub > TOL_PUB) {
  warning("The published rebuild departs from delta_oadr_10 by more than ",
          sprintf("transcription precision explains (max |diff| = %.6f).", d_pub))
} else {
  cat("\nBoth series reproduce within their expected tolerances:\n",
      sprintf("  published    max |diff| = %.6f  (tolerance %.2f, transcription)\n", d_pub, TOL_PUB),
      sprintf("  zero-migration max |diff| = %.6f  (tolerance %.2f, arithmetic)\n", d_zm, TOL_ZM),
      "The zero-migration regressor is verified in R.\n", sep = "")
}

# --- verification 3: does the R series reproduce the reported regression? -----
# Expected (Section 7, fiscal specification): delta_zm_10 coefficient 0.0075,
# HAC s.e. 0.0013. Requires reg_df from replication.R to be in the workspace.
if (exists("reg_df")) {
  chk <- merge(reg_df[, setdiff(names(reg_df), "delta_zm_10")],
               out[, c("year", "delta_zm_10")], by = "year")
  m <- lm(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, data = chk)
  cat("\nCHECK 3  re-estimated with the R-built series:\n")
  print(lmtest::coeftest(m, vcov. = sandwich::vcovHAC(m))["delta_zm_10", ])
}

# --- verification 4: news test -- inherited vs. revised projection --------------
# The projection in force in year y comes from vintage v = vintage_for(y). In
# a year when a NEW vintage comes into force (y == base of v), the number the
# Treasury sees can be split into what the PREVIOUS vintage p already said for
# the same years (inherited) and the change the new vintage introduces for those
# same years (revision = new - old): the part that is news on publication. In
# years with no new vintage the revision is zero and the whole term is inherited.
#   full(y)      = OADR_v(y+10) - OADR_v(y)
#   inherited(y) = OADR_p(y+10) - OADR_p(y)   [p = vintage before v]  if new vintage in y
#                = full(y)                                          otherwise
#   revision(y)  = full(y) - inherited(y)
# Entering the two parts separately asks whether inflows respond to the news
# component as they do to the level the Treasury already knew: a slow common
# trend cannot produce a response to a one-off revision. HAC inference; the last
# line tests equality of the two coefficients.
if (exists("reg_df")) {
  rev_df <- data.frame(year = YEARS, full = NA_real_, inherited = NA_real_, revision = NA_real_)
  for (k in seq_along(YEARS)) {
    y <- YEARS[k]; b <- vintage_for(y); v <- V[[as.character(b)]]
    f <- at(v, y + 10, "oadr") - at(v, y, "oadr")
    prev <- bases[bases < b]
    if (y == b && length(prev) > 0) {                 # new vintage in force this year
      p <- V[[as.character(max(prev))]]
      inh <- at(p, y + 10, "oadr") - at(p, y, "oadr")
    } else inh <- f
    rev_df[k, c("full", "inherited", "revision")] <- c(f, inh, f - inh)
  }
  news <- merge(reg_df, rev_df, by = "year")
  stopifnot(nrow(news) == 43, !anyNA(news$inherited))
  cat("\nCHECK 4  news test: projection split into inherited and revised parts\n")
  cat(sprintf("  revision years: %d of 43; revision mean %.2f, sd %.2f, range [%.2f, %.2f] points\n",
              sum(news$revision != 0), mean(news$revision[news$revision != 0]),
              sd(news$revision[news$revision != 0]), min(news$revision), max(news$revision)))
  cat(sprintf("  full delta vs sheet delta_oadr_10: max |diff| = %.3f (transcription)\n",
              max(abs(news$full - news$delta_oadr_10))))
  for (sp in c(fiscal = "L_tax_workpop", trend = "trend")) {
    m_split <- lm(reformulate(c("L_log_mig_rate", sp, "inherited", "revision", "L_ln_debt_gdp", "L_unem"),
                              "log_mig_rate"), data = news)
    ct <- lmtest::coeftest(m_split, vcov. = sandwich::vcovHAC(m_split))
    lh <- car::linearHypothesis(m_split, "inherited = revision", vcov. = sandwich::vcovHAC(m_split))
    cat(sprintf("  %-7s inherited %.4f (p %s)   revision %.4f (p %s)   equal-coef F(1,%d) = %.2f, p = %s\n",
                if (sp == "trend") "trend" else "fiscal",
                ct["inherited", 1], format.pval(ct["inherited", 4], digits = 2),
                ct["revision", 1],  format.pval(ct["revision", 4],  digits = 2),
                lh$Res.Df[2], lh$F[2], format.pval(lh$`Pr(>F)`[2], digits = 2)))
  }
  cat("  Reading: a revision coefficient of the same sign and size as the inherited one\n",
      "  means inflows respond to what the projections newly report, not only to their level.\n", sep = "")
}
}   # end of the guarded reconstruction block

}   # end of the vintage reconstruction (SKIP_VINTAGE_CHECK guard)
# -----------------------------------------------------------------------------
# 8. Figures 1 and 2
# -----------------------------------------------------------------------------
# Figure 1: non-EU inflows above the projection variable (published and
#   zero-net-migration), with the 1998-2007 policy era shaded.
# Figure 2: added-variable plots of the projection term in the trend
#   specification, for non-EU (controlled) and EU (uncontrolled) inflows. The
#   fitted slope of an added-variable plot equals the regression coefficient,
#   so the panels are exact visualisations of the reported estimates: 0.0075
#   (Table 2, column (2)) and 0.0022 (Table 4, column (2), the EU placebo).

png("figure1_1977.png", width = 6.5, height = 4.6, units = "in", res = 400,
    family = "serif")
par(mfrow = c(2, 1), mar = c(0.4, 4.6, 0.6, 1), oma = c(3.4, 0, 0, 0),
    cex = 0.75, tcl = -0.25, mgp = c(2.6, 0.5, 0), las = 1)

shade <- function(ylim) rect(1998, ylim[1] - 1e6, 2007, ylim[2] + 1e6,
                             col = "grey90", border = NA)

gross <- exp(reg_df$alt_log_mig_rate) / 1000        # thousands
plot(reg_df$year, gross, type = "n", xaxt = "n", xlab = "",
     ylab = "Gross non-EU inflows\n(thousands)")
shade(par("usr")[3:4]); box()
lines(reg_df$year, gross, lwd = 2)
text(2002.5, par("usr")[4] * 0.97, "1998-2007", col = "grey35", cex = 0.85)

par(mar = c(0.6, 4.6, 0.4, 1))
plot(reg_df$year, reg_df$delta_oadr_10, type = "n", xlab = "",
     ylab = "Projected 10-year\nchange in OADR",
     ylim = range(c(reg_df$delta_oadr_10, reg_df$delta_zm_10), na.rm = TRUE))
shade(par("usr")[3:4]); box(); abline(h = 0, col = "grey40")
lines(reg_df$year, reg_df$delta_oadr_10, lwd = 2)
lines(reg_df$year, reg_df$delta_zm_10, lwd = 2, lty = 2, col = "grey40")
legend("topleft", c("As published", "Zero net migration"),
       lwd = 2, lty = c(1, 2), col = c("black", "grey40"), bty = "n", cex = 0.9)
mtext("Year", side = 1, outer = TRUE, line = 2)
dev.off()

# --- Figure 2: added-variable plots ------------------------------------------
avp <- function(y, xj, others) {
  Z  <- cbind(1, do.call(cbind, others))
  ry <- resid(lm.fit(Z, y)); rx <- resid(lm.fit(Z, xj))
  list(rx = rx, ry = ry, b = sum(rx * ry) / sum(rx * rx))
}
d  <- na.omit(reg_df[, c("log_mig_rate", "eu_log_mig_rate", "delta_oadr_10",
                         "L_log_mig_rate", "L_eu_log_mig_rate", "trend",
                         "L_ln_debt_gdp", "L_unem")])
p1 <- avp(d$log_mig_rate,    d$delta_oadr_10,
          list(d$L_log_mig_rate,    d$trend, d$L_ln_debt_gdp, d$L_unem))
p2 <- avp(d$eu_log_mig_rate, d$delta_oadr_10,
          list(d$L_eu_log_mig_rate, d$trend, d$L_ln_debt_gdp, d$L_unem))

png("figure2_1977.png", width = 6.5, height = 2.9, units = "in", res = 400,
    family = "serif")
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.2, 0.8), cex = 0.75, tcl = -0.25,
    mgp = c(2.5, 0.5, 0), las = 1)
yl <- range(c(p1$ry, p2$ry))
for (p in list(list(p = p1, t = "(a) Non-EU inflows (policy-controlled)"),
               list(p = p2, t = "(b) EU inflows (uncontrolled)"))) {
  plot(p$p$rx, p$p$ry, pch = 1, cex = 0.8, ylim = yl,
       xlab = "Projected 10-year OADR change\n(residualised)",
       ylab = "ln inflow rate (residualised)", main = p$t, font.main = 1,
       cex.main = 0.95)
  abline(h = 0, v = 0, col = "grey85"); abline(a = 0, b = p$p$b, lwd = 2)
  legend("topleft", sprintf("slope = %.4f", p$p$b), bty = "n", cex = 0.9)
}
dev.off()

cat(sprintf("AVP slopes: non-EU %.4f (Table 2 col (2) coefficient 0.0075) | EU %.4f (Table 4 col (2), 0.0022)\n",
            p1$b, p2$b))

sessionInfo()
