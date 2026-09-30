# >>> 1977 VARIANT (manuscript only, Sam 22 Sep 2026) <<<
# Generated from mpr_dummies_replication.R: identical except that the estimation sample starts in
# 1977 (N = 43), the earliest year the lagged log change in debt/GDP allows.
# Outputs carry a _1977 suffix so the canonical 1979 run is never overwritten.
# The thesis stays on 1979-2019 and uses mpr_dummies_replication.R.
# =============================================================================
# MITCHELL, PAIN AND RILEY (2011) POLICY DUMMIES
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# Tests the policy and enlargement dummies of Mitchell, Pain and Riley (2011)
# against three dependent variables:
#
#   (a) the scaled non-EU inflow rate  -- the thesis's own DV
#   (b) GROSS non-EU inflows           -- unscaled, log level
#   (c) TOTAL inflows (EU + non-EU)    -- the margin policy could not fully ration
#
# WHAT THE ORIGINAL PAPER ACTUALLY SPECIFIES (pp. 1411-1412)
#   POL97  = 1 from 1998 onwards, 0 otherwise. A STEP, not a window. Follows
#            Hatton (2005) on the easing of the policy stance under Labour.
#            Entered LAGGED in their models (POL97_{t-1}).
#   POL02  = 1 from 2002 onwards. Highly Skilled Migrant Programme and the
#            Nationality, Immigration and Asylum Act 2002.
#   EXP04  = the 2004 eastward enlargement (A8).
#   ENL86  = 1 from 1986 onwards (Portugal and Spain).
#   ENL95  = 1 from 1995 onwards (EU11: Austria, Finland, Sweden).
#   "POL other" = region-specific dummies (Soviet collapse in Other Europe; the
#            1997 Hong Kong handover) which have no counterpart in a UK
#            aggregate series and are therefore not reproducible here.
#
# TWO DIFFERENCES THE READER MUST KEEP IN VIEW
#
# 1. POL97 IS A STEP, NOT A WINDOW. The thesis's `liberal` indicator is 1 only
#    for 1998-2007. Within MPR's own sample, which ends in 2007, a step from
#    1998 and a 1998-2007 window are the SAME VARIABLE, so the paper cannot
#    distinguish them. Over a sample running to 2019 they are quite different
#    objects, and the step is the one MPR actually specify. Both are estimated
#    below so the choice is visible rather than assumed.
#
# 2. THE DESIGNS ARE NOT THE SAME. MPR estimate a PANEL over 14 source regions,
#    1980-2007, with region fixed effects, in first differences of the log
#    migration rate relative to SOURCE-region population: Dln(m_it / P_it).
#    This file is a single UK aggregate time series in log levels relative to
#    the UK working-age population. An exact replication is therefore not
#    possible; what is ported across is the DUMMY DEFINITIONS, tested inside
#    the thesis's own specification. A differenced variant is reported at the
#    end as the closest available analogue to their dependent variable.
#
# INPUT:  Dissertation_Dataset_Final.xlsx (sheet "ReformSPA"). Sample 1979-2019.
# =============================================================================

library(lmtest); library(sandwich); library(car); library(tseries); library(readxl)

stars <- function(p) ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

run_model <- function(formula, data, label, n_expect = NULL) {
  mod <- lm(formula, data = data, na.action = na.omit)
  if (!is.null(n_expect) && nobs(mod) != n_expect)
    stop(sprintf("%s: N = %d, expected %d", label, nobs(mod), n_expect))
  ct <- coeftest(mod, vcov. = vcovHAC(mod))
  cat(sprintf("=== %s ===\n", label))
  cat(sprintf("  N=%d  Adj.R2=%.4f  AIC=%.2f\n",
              nobs(mod), summary(mod)$adj.r.squared, AIC(mod)))
  for (i in seq_len(nrow(ct)))
    cat(sprintf("  %-26s %9.5f  se=%8.5f  t=%6.3f  p=%.4f %s\n",
                rownames(ct)[i], ct[i,1], ct[i,2], ct[i,3], ct[i,4], stars(ct[i,4])))
  v <- tryCatch(max(vif(mod)), error = function(e) NA)
  cat(sprintf("  BG(1)=%.4f  BP=%.4f  RESET=%.4f  JB=%.4f  maxVIF=%.1f\n\n",
              bgtest(mod, order=1)$p.value, bptest(mod)$p.value,
              resettest(mod)$p.value, jarque.bera.test(residuals(mod))$p.value, v))
  invisible(mod)
}

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
cat("data:", DATA_XLSX, "\n\n")
raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
lag1 <- function(x) c(NA, head(x, -1))

reg_df <- with(raw, data.frame(
  year             = year,
  log_mig_rate     = loglev_nEU_WAPlagK,          # (a) scaled non-EU, log level
  alt_log_mig_rate = log(nEU_inflows),            # (b) gross non-EU, log level
  # (c) EU and non-EU rates share the same lagged working-age denominator --
  # verified exactly across all 41 years -- so the combined rate is their sum
  # and needs no separate reconstruction of the WAP series.
  tot_log_mig_rate = log(nEU_inflows_WAPlagK + EU_inflows_WAPlagK),
  eu_log_mig_rate  = log(EU_inflows_WAPlagK),
  delta_oadr_10    = delta_oadr_10,
  # Total-dependency projection: not used by any model in this file, but carried
  # so that the DR placebo from replication.R / robustness_replication.R can be
  # run against a reg_df built here without rebuilding the frame.
  delta_dr_10      = delta_dr_10,
  unem = unem, tax_workpop = tax_workpop, ln_debt_gdp = ln_debt_gdp
))
reg_df$L_log_mig_rate     <- lag1(reg_df$log_mig_rate)
reg_df$L_alt_log_mig_rate <- lag1(reg_df$alt_log_mig_rate)
reg_df$L_tot_log_mig_rate <- lag1(reg_df$tot_log_mig_rate)
reg_df$L_eu_log_mig_rate  <- lag1(reg_df$eu_log_mig_rate)
reg_df$L_unem             <- lag1(reg_df$unem)
reg_df$L_tax_workpop      <- lag1(reg_df$tax_workpop)
reg_df$L_ln_debt_gdp      <- lag1(reg_df$ln_debt_gdp)
reg_df$trend              <- seq_along(reg_df$year)

# ---- MPR dummies, defined exactly as the paper states ----
reg_df$POL97   <- as.integer(reg_df$year >= 1998)   # step, not a window
reg_df$POL02   <- as.integer(reg_df$year >= 2002)
reg_df$EXP04   <- as.integer(reg_df$year >= 2004)
reg_df$ENL86   <- as.integer(reg_df$year >= 1986)
reg_df$ENL95   <- as.integer(reg_df$year >= 1995)
# Not in MPR (their sample ends in 2007) but the natural continuation: the 2007
# A2 accession, for which transitional labour-market restrictions ran to 2014.
reg_df$EXP07   <- as.integer(reg_df$year >= 2007)
reg_df$EXP14   <- as.integer(reg_df$year >= 2014)
# The thesis's own window indicator, for contrast with POL97.
reg_df$liberal <- as.integer(reg_df$year >= 1998 & reg_df$year <= 2007)
# MPR enter POL97 lagged; build the lag before trimming the sample.
reg_df$L_POL97 <- lag1(reg_df$POL97)
reg_df$L_POL02 <- lag1(reg_df$POL02)

reg_df <- subset(reg_df, year >= 1977 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 43)

# Baseline right-hand side, in the trend specification used for robustness work.
RHS <- c("trend", "delta_oadr_10", "L_unem", "L_ln_debt_gdp")

# A dummy that is collinear with the trend, or that has almost no variation in
# sample, will produce a rank-deficient fit or an uninterpretable coefficient.
# Report the count rather than discovering it in the standard errors.
cat("dummy variation in sample (number of years = 1, out of 43):\n")
for (v in c("POL97","L_POL97","POL02","L_POL02","EXP04","ENL86","ENL95","EXP07","EXP14","liberal"))
  cat(sprintf("  %-9s %2d\n", v, sum(reg_df[[v]])))
cat("\n")

# =============================================================================
# 1. EACH DUMMY IN TURN, ON EACH DEPENDENT VARIABLE
# =============================================================================
DVS <- list(
  "(a) scaled non-EU rate" = c(y = "log_mig_rate",     L = "L_log_mig_rate"),
  "(b) gross non-EU"       = c(y = "alt_log_mig_rate", L = "L_alt_log_mig_rate"),
  "(c) total (EU+non-EU)"  = c(y = "tot_log_mig_rate", L = "L_tot_log_mig_rate")
)
DUMS <- c("POL97","L_POL97","POL02","EXP04","ENL86","ENL95","EXP07","EXP14","liberal")

for (dv in names(DVS)) {
  cat(sprintf("\n########## %s ##########\n\n", dv))
  yv <- DVS[[dv]]["y"]; lv <- DVS[[dv]]["L"]
  base <- run_model(reformulate(c(lv, RHS), yv), reg_df,
                    paste(dv, "- baseline, no dummy"), 43)
  for (d in DUMS)
    run_model(reformulate(c(lv, RHS, d), yv), reg_df, paste(dv, "-", d), 43)

  # All MPR dummies together. POL02/EXP04 and EXP07/EXP14 are adjacent steps and
  # will be collinear; maxVIF in the printout is the thing to read here.
  full <- run_model(reformulate(c(lv, RHS, "POL97","POL02","EXP04","ENL86","ENL95"), yv),
                    reg_df, paste(dv, "- all MPR dummies jointly"), 43)
  cat("-- joint F-test: the five MPR dummies against the baseline --\n")
  print(anova(base, full))
  cat("\n-- does the projection term survive the full dummy set? --\n")
  ct <- coeftest(full, vcov. = vcovHAC(full))
  print(ct["delta_oadr_10", , drop = FALSE])
  cat("\n")
}

# =============================================================================
# 2. THE HORSE RACE, WITH POL97 AS A STEP RATHER THAN A WINDOW
# =============================================================================
# The thesis reports this with `liberal`. Re-running it with MPR's own step
# dummy is the faithful version of the same test.
cat("\n########## horse race: step (POL97) vs window (liberal) ##########\n\n")
hr <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 +
                  L_unem + L_ln_debt_gdp, reg_df, "baseline", 43)
for (d in c("POL97", "liberal")) {
  m1 <- run_model(reformulate(c("L_log_mig_rate", RHS, d), "log_mig_rate"),
                  reg_df, paste("+", d), 43)
  m2 <- run_model(reformulate(c("L_log_mig_rate", RHS, d,
                                paste0("delta_oadr_10:", d)), "log_mig_rate"),
                  reg_df, paste("+", d, "+ interaction"), 43)
  cat(sprintf("-- joint test of %s and its interaction --\n", d))
  print(anova(hr, m2))
  # With an interaction present, the coefficient on delta_oadr_10 is the effect
  # where the dummy is ZERO, not the average effect. The effect where the dummy
  # is one is the SUM of the two, and that is the quantity to test.
  cc <- rep(0, length(coef(m2))); names(cc) <- names(coef(m2))
  cc["delta_oadr_10"] <- 1
  cc[paste0("delta_oadr_10:", d)] <- 1
  cat(sprintf("-- effect of delta_oadr_10 where %s = 1 (sum of the two terms) --\n", d))
  print(linearHypothesis(m2, matrix(cc, nrow = 1), rhs = 0, vcov. = vcovHAC(m2)))
  cat("\n")
}

# =============================================================================
# 3. DIFFERENCED VARIANT -- closest analogue to MPR's dependent variable
# =============================================================================
# MPR model Dln(m_it / P_it). Differencing the log rate here loses the level
# information the thesis's argument rests on, so this is reported as a
# comparison with their design, not as a specification of the thesis.
cat("\n########## differenced DV: closest analogue to MPR ##########\n\n")
reg_df$d_log_mig <- c(NA, diff(reg_df$log_mig_rate))
reg_df$d_tot_mig <- c(NA, diff(reg_df$tot_log_mig_rate))
for (v in c("d_log_mig", "d_tot_mig")) {
  run_model(reformulate(c("delta_oadr_10","L_unem","L_ln_debt_gdp","L_POL97","POL02","EXP04"), v),
            reg_df, paste("D", v, "with MPR dummies"), 42)
}

# =============================================================================
# 4. COMPACT TABLE FOR APPENDIX A -- the "Policy dummy" slot
# =============================================================================
# Two columns, four coefficient rows, five summary rows. What section 1 prints
# at length and this omits: the nine single-dummy fits per column (carried in
# one sentence of Section 9), and the gross non-EU column, which tracks the
# scaled series to the third decimal and earns a clause rather than a column.
# ENL86 and ENL95 are estimated but not displayed: EU accession indicators on a
# non-EU dependent variable are not separately interpretable.

cat("\n\n########## APPENDIX TABLE: MPR dummies, joint model ##########\n\n")

APX  <- list("(1) non-EU" = c(y = "log_mig_rate",     L = "L_log_mig_rate"),
             "(2) total"  = c(y = "tot_log_mig_rate", L = "L_tot_log_mig_rate"))
ROWS <- c("POL97" = "POL97", "POL02" = "POL02", "EXP04" = "EXP04",
          "D10 OADR" = "delta_oadr_10")

pf <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)

res <- lapply(APX, function(s) {
  b <- lm(reformulate(c(s["L"], RHS), s["y"]), reg_df)
  f <- lm(reformulate(c(s["L"], RHS, "POL97", "POL02", "EXP04", "ENL86", "ENL95"),
                      s["y"]), reg_df)
  stopifnot(nobs(b) == 43, nobs(f) == 43)
  # The joint test uses the SAME HAC covariance as the coefficient tests below.
  # anova(b, f) would return the classical F, mixing two variance estimators in
  # one table -- which is how a dummy can read as individually significant while
  # the set reads as jointly irrelevant. Here both come from vcovHAC.
  lh <- linearHypothesis(f, paste0(c("POL97","POL02","EXP04","ENL86","ENL95"), " = 0"),
                         vcov. = vcovHAC(f))
  list(ct = coeftest(f, vcov. = vcovHAC(f)),
       F = lh$F[2], Fp = lh$`Pr(>F)`[2], df1 = lh$Df[2], df2 = lh$Res.Df[2],
       r2 = summary(f)$adj.r.squared, reset = resettest(f)$p.value,
       vif = max(vif(f)))
})

col <- function(f) paste(sapply(res, f), collapse = "")
hdr <- sprintf("%-12s%s", "", paste(sprintf("%16s", names(APX)), collapse = ""))
rule <- strrep("-", nchar(hdr))

cat(hdr, "\n", rule, "\n", sep = "")
for (lab in names(ROWS)) {
  v <- ROWS[[lab]]
  cat(sprintf("%-12s%s\n", lab,
      col(function(r) sprintf("%13.4f%-3s", r$ct[v, 1], stars(r$ct[v, 4])))))
  cat(sprintf("%-12s%s\n", "",
      col(function(r) sprintf("%16s", paste0("(", pf(r$ct[v, 4]), ")")))))
}
cat(rule, "\n", sep = "")
cat(sprintf("%-12s%s\n", sprintf("joint F(%d,%d)", res[[1]]$df1, res[[1]]$df2),
    col(function(r) sprintf("%16.2f", r$F))))
cat(sprintf("%-12s%s\n", "  p",       col(function(r) sprintf("%16s",   pf(r$Fp)))))
cat(sprintf("%-12s%s\n", "adj. R2",   col(function(r) sprintf("%16.3f", r$r2))))
cat(sprintf("%-12s%s\n", "RESET p",   col(function(r) sprintf("%16s",   pf(r$reset)))))
cat(sprintf("%-12s%s\n", "max VIF",   col(function(r) sprintf("%16.1f", r$vif))))
cat(rule, "\n", sep = "")
cat("Trend specification, N = 43. HAC (vcovHAC) p-values in parentheses;\n",
    "the joint test is HAC-robust, not the classical F. Coefficients are from\n",
    "the model carrying all five indicators, so they differ from the same\n",
    "indicator entered singly. *** p<0.01, ** p<0.05, * p<0.10.\n",
    "ENL86 and ENL95 estimated but not displayed.\n", sep = "")

cat("\n"); print(sessionInfo())
