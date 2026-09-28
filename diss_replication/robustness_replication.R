# =============================================================================
# ROBUSTNESS REPLICATION
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# Every robustness check reported in Section 9, in the order the section
# presents them, one block per subsection heading:
#
#   9.0  Baseline                   no projection term at all (nested reference)
#   9.1  DR vs. OADR                total-dependency placebo, all horizons
#   9.2  IPS vs. LTIM               adjusted LTIM basis, 1991-2019
#   9.3  EU vs. non-EU migration    uncontrolled-margin placebo (+ enlargement)
#   9.4  Timing of Study            sample-start sensitivity
#   9.5  Levels vs. Delta           projected level in place of the change
#   9.6  Gross non-EU inflows       unscaled, in logs and in levels
#   9.7  Zero-net-migration proj.   purged regressor
#   9.8  Publication dating         vintages re-dated to publication year
#   9.9  The 1998-2007 'policy era' testing the literature's dummy
#
# Companion files in this package:
#   appendix_replication.R  -- reproduces Appendix A exactly as printed
#   thesis_replication.R    -- the full thesis-scope run (Sections 2-7)
# All three share the same dataset and data-construction block, so overlapping
# models return identical coefficients.
#
# INPUT:  Dissertation_Dataset_Final.xlsx, sheets "ReformSPA" and (for 9.8)
#         "PubShiftOADR_ZM", in the working directory. Sample 1979-2019, N = 41.
# =============================================================================

library(lmtest); library(sandwich); library(car); library(tseries); library(readxl)

stars <- function(p) ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

# n_expect asserts the sample size: na.omit drops rows silently, and a single
# stray NA shrinks N without raising an error. Every call below states the N it
# expects, so a misaligned column fails loudly at the model that first sees it.
run_model <- function(formula, data, label, n_expect = NULL) {
  mod <- lm(formula, data = data, na.action = na.omit)
  dropped <- nrow(data) - nobs(mod)
  if (!is.null(n_expect) && nobs(mod) != n_expect)
    stop(sprintf("%s: N = %d, expected %d (%d rows dropped by na.omit)",
                 label, nobs(mod), n_expect, dropped))
  ct <- coeftest(mod, vcov. = vcovHAC(mod))
  cat(sprintf("=== %s ===\n", label))
  cat(sprintf("  N=%d  Adj.R2=%.4f  AIC=%.2f\n",
              nobs(mod), summary(mod)$adj.r.squared, AIC(mod)))
  for (i in seq_len(nrow(ct)))
    cat(sprintf("  %-24s %8.4f  se=%7.4f  t=%6.3f  p=%.4f %s\n",
                rownames(ct)[i], ct[i,1], ct[i,2], ct[i,3], ct[i,4], stars(ct[i,4])))
  v <- tryCatch(max(vif(mod)), error = function(e) NA)
  cat(sprintf("  BG(1)=%.4f  BP=%.4f  RESET=%.4f  JB=%.4f  maxVIF=%.1f\n\n",
              bgtest(mod, order=1)$p.value, bptest(mod)$p.value,
              resettest(mod)$p.value, jarque.bera.test(residuals(mod))$p.value, v))
  invisible(mod)
}

# -----------------------------------------------------------------------------
# Data (identical construction to appendix_replication.R)
# -----------------------------------------------------------------------------
DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
cat("data:", DATA_XLSX, "\n\n")
raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
lag1 <- function(x) c(NA, head(x, -1))

# Column accessor tolerant of workbook vintages. thesis_replication.R and this
# file were written against Dissertation_Dataset_Final.xlsx; earlier exports of
# the same data omit a few of the total-DR horizon columns. A missing column
# becomes all-NA here and its model is skipped below with a message, rather than
# aborting a run whose other 20-odd checks are fine.
col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA_real_, nrow(raw))

reg_df <- with(raw, data.frame(
  year = year,
  mig_rate          = nEU_inflows_WAPlagK,
  log_mig_rate      = loglev_nEU_WAPlagK,
  alt_log_mig_rate  = log(nEU_inflows),              # gross, unscaled
  eu_log_mig_rate   = log(EU_inflows_WAPlagK),       # uncontrolled margin
  ltim_log_mig_rate = log(ltim_nEU_inflows_WAPlagK), # LTIM basis, 1991-
  delta_oadr_1 = delta_oadr_1, delta_oadr_3 = delta_oadr_3,
  delta_oadr_5 = delta_oadr_5, delta_oadr_7 = delta_oadr_7,
  delta_oadr_10 = delta_oadr_10, delta_oadr_5wavg = delta_oadr_5wavg,
  level_oadr_1 = oadr_1, level_oadr_3 = oadr_3, level_oadr_5 = oadr_5,
  level_oadr_7 = oadr_7, level_oadr_10 = oadr_10,
  delta_dr_1 = col("delta_dr_1"), delta_dr_3 = col("delta_dr_3"), delta_dr_5 = col("delta_dr_5"),
  delta_dr_10 = delta_dr_10, level_dr_10 = col("dr_10"),
  delta_zm_10 = delta_zm_10,
  unem = unem, tax_workpop = tax_workpop,
  ln_debt_gdp = ln_debt_gdp                        # ANNUAL LOG CHANGE
))
# Lags are built on the PRE-SUBSET series. Lagging the trimmed frame would set
# 1979 to NA and estimate on 40 observations, which changes the level-form
# coefficients materially (1470 against 1375 in the fiscal specification).
reg_df$L_log_mig_rate      <- lag1(reg_df$log_mig_rate)
reg_df$L_alt_log_mig_rate  <- lag1(reg_df$alt_log_mig_rate)
reg_df$L_eu_log_mig_rate   <- lag1(reg_df$eu_log_mig_rate)
reg_df$L_ltim_log_mig_rate <- lag1(reg_df$ltim_log_mig_rate)
reg_df$L_unem              <- lag1(reg_df$unem)
reg_df$L_tax_workpop       <- lag1(reg_df$tax_workpop)
reg_df$L_ln_debt_gdp       <- lag1(reg_df$ln_debt_gdp)
reg_df$trend               <- seq_along(reg_df$year)
reg_df$liberal <- as.integer(reg_df$year >= 1998 & reg_df$year <= 2007)
# EU enlargement waves and the year after (Section 9.3): migration persists, so
# an accession shock is still in the data the following year.
enl <- c(1981, 1986, 1995, 2004, 2007, 2013)
reg_df$enlargement <- as.integer(reg_df$year %in% c(enl, enl + 1))

reg_df <- subset(reg_df, year >= 1979 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 41)
# Gross inflows in levels, from the pre-subset lag (see note above).
reg_df$alt_mig_rate   <- exp(reg_df$alt_log_mig_rate)
reg_df$L_alt_mig_rate <- exp(reg_df$L_alt_log_mig_rate)

# Derived indicators, (re)created idempotently. They live in the data block
# above, but a section pasted on its own -- or run against a reg_df built by
# appendix_replication.R or stationarity_bounds_replication.R, neither of which
# needs them -- would otherwise fail with "object 'liberal' not found". Calling
# this at the head of the two sections that use them makes each block runnable
# in isolation.
ensure_indicators <- function(df) {
  if (!"liberal" %in% names(df))
    df$liberal <- as.integer(df$year >= 1998 & df$year <= 2007)
  if (!"enlargement" %in% names(df)) {
    enl <- c(1981, 1986, 1995, 2004, 2007, 2013)   # accession waves
    df$enlargement <- as.integer(df$year %in% c(enl, enl + 1))
  }
  df
}

# Skip a check whose regressor is absent from this workbook vintage, saying so.
have <- function(v) {
  ok <- v %in% names(reg_df) && !all(is.na(reg_df[[v]]))
  if (!ok) cat(sprintf("  [skipped: %s not present in this dataset vintage]\n\n", v))
  ok
}

# =============================================================================
# 9.0  Baseline without any projection term
# =============================================================================
# The nested baseline that makes 9.1 interpretable. With a lagged dependent
# variable in the model, adjusted R2 is already ~0.94 before any projection is
# added, so a reviewer will reasonably ask whether the projection term is doing
# real work or decorating an autoregressive process. The nested F-test and the
# AIC answer that; the R2 increment, by construction, cannot.
cat("\n---------- 9.0  No projection term (baseline) ----------\n\n")
placebo1 <- run_model(log_mig_rate ~ L_log_mig_rate + trend +
                        L_ln_debt_gdp + L_unem, reg_df, "no projection, trend", 41)
placebo2 <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop +
                        L_ln_debt_gdp + L_unem, reg_df, "no projection, fiscal", 41)
# The full specifications, re-estimated here so the comparison is self-contained.
placebo3  <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dOADR10, trend", 41)
placebo4 <- run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dOADR10, fiscal", 41)
placebo5     <- run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
                        L_ln_debt_gdp + L_unem, reg_df, "+ dDR10, trend (placebo)", 41)

# Nested comparisons. A negative dAIC favours the larger model; dAIC > 10 is
# conventionally decisive. The total-DR placebo is the informative contrast: it
# does not merely fail to reach significance, it fails to pay for its own degree
# of freedom, so AIC prefers the model with no projection at all.
cmp <- function(restricted, unrestricted, label) {
  a <- anova(restricted, unrestricted)
  dAIC <- AIC(unrestricted) - AIC(restricted)
  cat(sprintf("%-34s F(%d,%d)=%7.3f  p=%.5f  dAIC=%+7.2f  adjR2 %.4f -> %.4f\n",
              label, a$Df[2], a$Res.Df[2], a$F[2], a$`Pr(>F)`[2], dAIC,
              summary(restricted)$adj.r.squared, summary(unrestricted)$adj.r.squared))
}
cat("\n-- incremental contribution of the projection term --\n")
cmp(placebo1, placebo3,  "trend spec: add dOADR10")      # F=14.145 p=0.0006 dAIC=-11.92
cmp(placebo1, placebo5,     "trend spec: add dDR10")        # F= 1.32 p=0.2582 dAIC= +0.48
cmp(placebo2, placebo4, "fiscal spec: add dOADR10")     # F=19.07 p=0.0001 dAIC=-15.83
cat("\n")

# =============================================================================
# 9.1  DR vs. OADR -- the total dependency ratio placebo
# =============================================================================
# The mechanism is fiscal: pensioners are expensive to the state in a way
# children are not, so only the pensioner-specific ratio should carry a signal.
cat("\n---------- 9.1  DR vs. OADR ----------\n\n")
run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "DR placebo: delta_dr_10", 41)
# "at every horizon and in levels as in deltas": none of the variants signs in.
for (h in c("delta_dr_1", "delta_dr_3", "delta_dr_5", "level_dr_10"))
  if (have(h))
    run_model(reformulate(c("L_log_mig_rate","trend",h,"L_ln_debt_gdp","L_unem"),
                          "log_mig_rate"), reg_df, paste("DR placebo:", h), 41)

# =============================================================================
# 9.2  IPS vs. LTIM -- measurement basis
# =============================================================================
# The subset has 29 rows; the 1991 row drops because the LTIM series begins in
# 1991, so its lag is undefined there. N = 28 is correct, not a data fault.
cat("\n---------- 9.2  IPS vs. LTIM ----------\n\n")
run_model(ltim_log_mig_rate ~ L_ltim_log_mig_rate + trend + delta_oadr_10 +
            L_unem + L_ln_debt_gdp, subset(reg_df, year >= 1991),
          "LTIM basis, 1991-2019", 28)

# =============================================================================
# 9.3  EU vs. non-EU migration -- the uncontrolled margin
# =============================================================================
# Free movement meant policy could not ration EU inflows. A confounder common to
# all migration would show up here; a policy instrument should not.
cat("\n---------- 9.3  EU vs. non-EU migration ----------\n\n")
reg_df <- ensure_indicators(reg_df)
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10 +
            L_ln_debt_gdp + L_unem, reg_df, "EU placebo", 41)
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10 +
            L_ln_debt_gdp + L_unem + enlargement,
          reg_df, "EU placebo + enlargement dummy", 41)

# =============================================================================
# 9.4  Timing of Study -- sample start
# =============================================================================
cat("\n---------- 9.4  Timing of Study ----------\n\n")
for (y0 in c(1976, 1983, 1987)) {
  sub <- subset(reg_df, year >= y0)          # 1976 is outside the trimmed frame
  if (nrow(sub) == 0) { cat(sprintf("  start %d: outside the 1979-2019 frame; re-run with the untrimmed data to test it\n\n", y0)); next }
  run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 +
              L_ln_debt_gdp + L_unem, sub, paste("sample start", y0), nrow(sub))
}

# =============================================================================
# 9.5  Levels vs. Delta -- projected level in place of the projected change
# =============================================================================
cat("\n---------- 9.5  Levels vs. Delta ----------\n\n")
run_model(log_mig_rate ~ L_log_mig_rate + trend + level_oadr_10 +
            L_unem + L_ln_debt_gdp, reg_df, "OADR level, 10-year", 41)
# Horizons: significant from 3 years out, no signal at 1 year (p = 0.56).
for (h in c("delta_oadr_1","delta_oadr_3","delta_oadr_5","delta_oadr_7","delta_oadr_5wavg"))
  run_model(reformulate(c("L_log_mig_rate","trend",h,"L_unem","L_ln_debt_gdp"),
                        "log_mig_rate"), reg_df, paste("horizon:", h), 41)
for (h in c("level_oadr_1","level_oadr_3","level_oadr_5","level_oadr_7"))
  run_model(reformulate(c("L_log_mig_rate","trend",h,"L_unem","L_ln_debt_gdp"),
                        "log_mig_rate"), reg_df, paste("level horizon:", h), 41)

# =============================================================================
# 9.6  Gross non-EU inflows -- without the working-age denominator
# =============================================================================
# The per-capita denominator is the analyst's choice, not the policymaker's.
cat("\n---------- 9.6  Gross non-EU inflows ----------\n\n")
run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "gross, log, fiscal", 41)  # 0.0080
run_model(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "gross, log, trend", 41)           # 0.0073
run_model(alt_mig_rate ~ L_alt_mig_rate + delta_oadr_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "gross, level, fiscal", 41) # 1375
run_model(alt_mig_rate ~ L_alt_mig_rate + delta_oadr_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "gross, level, trend", 41)          # 1295

# =============================================================================
# 9.7  Zero-net-migration projections -- purging the regressor
# =============================================================================
# The projections embed the government's own migration assumptions, so the
# regressor could predict inflows because inflows are built into it. delta_zm_10
# rebuilds each vintage with that assumption removed.
cat("\n---------- 9.7  Zero-net-migration projections ----------\n\n")
run_model(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
            L_tax_workpop + L_ln_debt_gdp, reg_df, "zero-migration, fiscal", 41) # 0.0075
run_model(log_mig_rate ~ L_log_mig_rate + delta_zm_10 + L_unem +
            trend + L_ln_debt_gdp, reg_df, "zero-migration, trend", 41)          # 0.0072

# =============================================================================
# 9.8  Publication dating
# =============================================================================
# Vintages are labelled by base year but published roughly a year later. The
# baseline dates by base year; this block re-dates each vintage to the year
# after its base, reassigning the projection in force in 21 of the 41 years.
cat("\n---------- 9.8  Publication dating ----------\n\n")
# The sheet is named "PubShiftOADR_ZM" in the current workbook and
# "pub shift OADR" in earlier exports; accept either.
.sheets <- excel_sheets(DATA_XLSX)
.pubsheet <- intersect(c("PubShiftOADR_ZM", "pub shift OADR"), .sheets)
if (!length(.pubsheet)) stop("no publication-dating sheet found; looked for ",
                             "PubShiftOADR_ZM / 'pub shift OADR' in: ",
                             paste(.sheets, collapse = ", "))
pub <- as.data.frame(read_excel(DATA_XLSX, sheet = .pubsheet[1]))
pub <- pub[!is.na(pub$year), ]
# A column formatted as text arrives as character; lm() would then fit one dummy
# per observation and fail inside the HAC bandwidth estimator rather than here.
num <- function(x, nm = deparse(substitute(x))) {
  if (is.numeric(x)) return(x)
  out <- suppressWarnings(as.numeric(as.character(x)))
  lost <- which(is.na(out) & !is.na(x) & trimws(as.character(x)) != "")
  if (length(lost)) stop(sprintf("%s: %d value(s) became NA on coercion", nm, length(lost)))
  out
}
reg_df$delta_oadr_10_pub <- num(pub$delta_oadr_10_pub[match(reg_df$year, pub$year)])
reg_df$delta_zm_10_pub   <- num(pub$delta_zm_10_pub[match(reg_df$year, pub$year)])
reg_df$delta_dr_10_pub   <- if ("delta_dr_10_pub" %in% names(pub))
  num(pub$delta_dr_10_pub[match(reg_df$year, pub$year)]) else rep(NA_real_, nrow(reg_df))
# An incomplete column means the paste is misaligned: a series pasted one row
# high runs off the end and leaves the final year blank. N = 40 is the symptom.
stopifnot(!any(is.na(reg_df$delta_oadr_10_pub)), !any(is.na(reg_df$delta_zm_10_pub)))

run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10_pub +
            L_ln_debt_gdp + L_unem, reg_df, "pub-dated, trend", 41)   # 0.0075
run_model(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10_pub +
            L_ln_debt_gdp + L_unem, reg_df, "pub-dated, fiscal", 41)  # 0.0086
run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_zm_10_pub +
            L_ln_debt_gdp + L_unem, reg_df, "pub-dated, zero-migration", 41)  # 0.0067
if (!any(is.na(reg_df$delta_dr_10_pub)))
  run_model(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10_pub +
              L_ln_debt_gdp + L_unem, reg_df, "pub-dated, DR placebo", 41)  # -0.0001
run_model(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10_pub +
            L_ln_debt_gdp + L_unem, reg_df, "pub-dated, EU placebo", 41)    # p = 0.40

# =============================================================================
# 9.9  The 1998-2007 'policy era' -- horse race against the literature
# =============================================================================
# Mitchell, Pain and Riley (2011) attribute the rise to a policy-era dummy. Run
# against the projections, the dummy has nothing left to explain.
cat("\n---------- 9.9  The 1998-2007 'policy era' ----------\n\n")
reg_df <- ensure_indicators(reg_df)
hr_base <- run_model(log_mig_rate ~ L_log_mig_rate + delta_oadr_10 + L_unem +
                       L_ln_debt_gdp + trend, reg_df, "horse race: baseline", 41)
withdum <- run_model(log_mig_rate ~ L_log_mig_rate + delta_oadr_10 + L_unem +
                       L_ln_debt_gdp + trend + liberal,
                     reg_df, "horse race: + liberal dummy", 41)
withint <- run_model(log_mig_rate ~ L_log_mig_rate + delta_oadr_10 + L_unem +
                       L_ln_debt_gdp + trend + liberal + delta_oadr_10:liberal,
                     reg_df, "horse race: + dummy + interaction", 41)
cat("-- joint test of the dummy and its interaction --\n")
print(anova(hr_base, withint))            # F(2, 33) = 0.11, p = 0.90

# cut() is right-closed, so an upper break of 1996 would put 1997 in the
# "liberal" bin while the dummy above excludes it. The break is 1997 so the
# printed means describe the same window the thesis names.
reg_df$period <- cut(reg_df$year, breaks = c(1978, 1997, 2007, 2019),
                     labels = c("pre1998", "liberal", "post2007"))
stopifnot(identical(as.integer(reg_df$period == "liberal"), reg_df$liberal))
cat("-- mean projection term by period (the collinearity that is the point) --\n")
print(aggregate(cbind(level_oadr_10, delta_oadr_10) ~ period, data = reg_df, FUN = mean))

cat("\n"); print(sessionInfo())
