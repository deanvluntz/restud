# >>> 1977 VARIANT (manuscript only, Sam 22 Sep 2026) <<<
# Generated from robustness_both_specs.R: identical except that the estimation sample starts in
# 1977 (N = 43), the earliest year the lagged log change in debt/GDP allows.
# Outputs carry a _1977 suffix so the canonical 1979 run is never overwritten.
# The thesis stays on 1979-2019 and uses robustness_both_specs.R.
# =============================================================================
# EVERY ROBUSTNESS CHECK IN BOTH SPECIFICATIONS (trend and fiscal), HAC inference
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# The thesis reports several checks in the trend specification only. This
# file runs each check in BOTH specifications side by side, with the same HAC
# inference as the main tables, and prints one compact panel per check:
# projection coefficient, HAC p-value, adj. R2, AIC, RESET p, max VIF, N.
# Joint tests of the MPR indicators are HAC-robust (linearHypothesis).
#
# Run from a clean session:  rm(list = ls()); source("robustness_both_specs.R")
# INPUT: Dissertation_Dataset_Final.xlsx (sheet "ReformSPA").
# =============================================================================
library(lmtest); library(sandwich); library(car); library(readxl)

DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
lag1 <- function(x) c(NA, head(x, -1))

reg_df <- with(raw, data.frame(
  year = year,
  log_mig_rate = loglev_nEU_WAPlagK, alt_log_mig_rate = log(nEU_inflows),
  tot_log_mig_rate = log(nEU_inflows_WAPlagK + EU_inflows_WAPlagK),
  eu_log_mig_rate = log(EU_inflows_WAPlagK),
  delta_oadr_10 = delta_oadr_10, delta_dr_10 = delta_dr_10, level_oadr_10 = oadr_10,
  delta_zm_10 = delta_zm_10,
  delta_oadr_1 = delta_oadr_1, delta_oadr_3 = delta_oadr_3,
  delta_oadr_5 = delta_oadr_5, delta_oadr_7 = delta_oadr_7,
  unem = unem, tax_workpop = tax_workpop, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate","alt_log_mig_rate","tot_log_mig_rate","eu_log_mig_rate",
            "unem","tax_workpop","ln_debt_gdp"))
  reg_df[[paste0("L_", v)]] <- lag1(reg_df[[v]])
reg_df$trend <- seq_along(reg_df$year)
reg_df$POL97 <- as.integer(reg_df$year >= 1998); reg_df$POL02 <- as.integer(reg_df$year >= 2002)
reg_df$EXP04 <- as.integer(reg_df$year >= 2004); reg_df$ENL86 <- as.integer(reg_df$year >= 1986)
reg_df$ENL95 <- as.integer(reg_df$year >= 1995)
reg_df$liberal <- as.integer(reg_df$year >= 1998 & reg_df$year <= 2007)
reg_df <- subset(reg_df, year >= 1977 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 43)

pf <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
DUM <- c("POL97","POL02","EXP04","ENL86","ENL95")

# One check = a dependent variable, its lag, the projection term, and extras.
# The two specifications differ only in the second regressor: trend or L_tax.
both <- function(label, y, Ly, proj, extra = character(0), data = reg_df, joint = NULL) {
  cat(sprintf("\n===== %s =====\n", label))
  hdr <- sprintf("%-9s%11s%9s%8s%9s%8s%8s%4s", "spec", "proj coef", "p(HAC)", "adjR2", "AIC", "RESET", "maxVIF", "N")
  cat(hdr, "\n", strrep("-", nchar(hdr)), "\n", sep = "")
  for (kind in c("trend", "fiscal")) {
    second <- if (kind == "trend") "trend" else "L_tax_workpop"
    f <- reformulate(c(Ly, second, proj, "L_ln_debt_gdp", "L_unem", extra), y)
    m <- lm(f, data = data, na.action = na.omit)
    ct <- coeftest(m, vcov. = vcovHAC(m))
    cat(sprintf("%-9s%11.4f%9s%8.3f%9.2f%8s%8.1f%4d\n", kind, ct[proj, 1], pf(ct[proj, 4]),
                summary(m)$adj.r.squared, AIC(m), pf(resettest(m)$p.value), max(vif(m)), nobs(m)))
    if (!is.null(joint)) {
      lh <- linearHypothesis(m, paste0(joint, " = 0"), vcov. = vcovHAC(m))
      cat(sprintf("%-9s  joint HAC F(%d,%d) on {%s} = %.2f, p = %s\n", "",
                  lh$Df[2], lh$Res.Df[2], paste(joint, collapse = ","), lh$F[2], pf(lh$`Pr(>F)`[2])))
    }
  }
}

both("Baseline: 10-yr OADR delta",       "log_mig_rate", "L_log_mig_rate", "delta_oadr_10")
both("DR placebo: 10-yr total-DR delta", "log_mig_rate", "L_log_mig_rate", "delta_dr_10")
both("Projected LEVEL at 10 yrs",        "log_mig_rate", "L_log_mig_rate", "level_oadr_10")
for (h in c(1, 3, 5, 7))
  both(sprintf("Horizon: %d-yr OADR delta", h), "log_mig_rate", "L_log_mig_rate", paste0("delta_oadr_", h))
both("Zero-net-migration projection",    "log_mig_rate", "L_log_mig_rate", "delta_zm_10")
both("Gross non-EU inflows (unscaled)",  "alt_log_mig_rate", "L_alt_log_mig_rate", "delta_oadr_10")
both("EU placebo",                        "eu_log_mig_rate", "L_eu_log_mig_rate", "delta_oadr_10")
both("+ POL97 step",                      "log_mig_rate", "L_log_mig_rate", "delta_oadr_10", "POL97")
both("+ 1998-2007 window",                "log_mig_rate", "L_log_mig_rate", "delta_oadr_10", "liberal")
both("+ all five MPR indicators",         "log_mig_rate", "L_log_mig_rate", "delta_oadr_10", DUM, joint = DUM)
both("Total inflows + five indicators",   "tot_log_mig_rate", "L_tot_log_mig_rate", "delta_oadr_10", DUM, joint = DUM)
both("Sample from 1983",                  "log_mig_rate", "L_log_mig_rate", "delta_oadr_10", data = subset(reg_df, year >= 1983))
both("Sample from 1987",                  "log_mig_rate", "L_log_mig_rate", "delta_oadr_10", data = subset(reg_df, year >= 1987))


# =============================================================================
# REBUILT FISCAL BLOCK (single source: OBR Historical Public Finances Database,
# HM Treasury GDP deflator March 2026, 2024-25 = 100). Written by the Python
# rebuild to fiscal_block_HPFD.csv; year t carries FY (t-1)/t, as the workbook
# does. Adds the labour-tax variant (income tax + NICs per working-age person)
# and re-runs the two main specifications on the rebuilt totals.
# =============================================================================
if (file.exists("fiscal_block_HPFD.csv")) {
  fb <- read.csv("fiscal_block_HPFD.csv")
  for (v in c("tax_nat_pw", "tax_lab_pw", "tax_nat_gdp", "tax_lab_gdp", "psnd_gdp"))
    reg_df[[v]] <- fb[[v]][match(reg_df$year, fb$year)]
  # lags built on the full frame, then re-trim (the CSV covers 1975-2020)
  full <- fb[order(fb$year), ]
  lagv <- function(v) full[[v]][match(reg_df$year - 1, full$year)]
  reg_df$L_tax_nat_pw  <- lagv("tax_nat_pw");  reg_df$L_tax_lab_pw  <- lagv("tax_lab_pw")
  reg_df$L_tax_nat_gdp <- lagv("tax_nat_gdp"); reg_df$L_tax_lab_gdp <- lagv("tax_lab_gdp")
  reg_df$L_dln_psnd    <- (log(full$psnd_gdp) - log(c(NA, head(full$psnd_gdp, -1))))[match(reg_df$year - 1, full$year)]
  stopifnot(!anyNA(reg_df$L_tax_lab_pw), !anyNA(reg_df$L_dln_psnd))

  cat("\n\n########## REBUILT FISCAL BLOCK: main and labour-tax specifications ##########\n")
  spec <- function(label, second) {
    m  <- lm(reformulate(c("L_log_mig_rate", second, "delta_oadr_10", "L_dln_psnd", "L_unem"), "log_mig_rate"), reg_df)
    ct <- coeftest(m, vcov. = vcovHAC(m))
    cat(sprintf("%-34s proj %.4f (p %s)   %s %s   adjR2 %.3f  AIC %.2f  RESET %s  maxVIF %.1f\n", label,
        ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]),
        if (second == "trend") "" else sprintf("%s %.4f (p %s)", second, ct[second, 1], pf(ct[second, 4])), "",
        summary(m)$adj.r.squared, AIC(m), pf(resettest(m)$p.value), max(vif(m))))
    invisible(m)
  }
  spec("trend, rebuilt debt",              "trend")
  spec("fiscal, rebuilt tax_nat_pw",       "L_tax_nat_pw")
  spec("fiscal, LABOUR tax_lab_pw",        "L_tax_lab_pw")
  spec("fiscal, tax_nat / GDP",            "L_tax_nat_gdp")
  spec("fiscal, tax_lab / GDP",            "L_tax_lab_gdp")

  # Tax and trend entered together (HAC): the collinearity check quoted in the text.
  cat("\n--- tax per working-age person and trend entered jointly (HAC) ---\n")
  for (tv in c("L_tax_nat_pw", "L_tax_lab_pw")) {
    m_both <- lm(reformulate(c("L_log_mig_rate", "trend", tv, "delta_oadr_10", "L_dln_psnd", "L_unem"), "log_mig_rate"), reg_df)
    ct <- coeftest(m_both, vcov. = vcovHAC(m_both))
    cat(sprintf("%-14s tax %.4f (p %s)  trend %.4f (p %s)  proj %.4f (p %s)  maxVIF %.1f\n", tv,
        ct[tv, 1], pf(ct[tv, 4]), ct["trend", 1], pf(ct["trend", 4]),
        ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), max(vif(m_both))))
  }
  cat("Labour tax = income tax + National Insurance contributions (OBR HPFD columns 2-3), real 2024-25 prices,\n",
      "per working-age person, FY (t-1)/t. Compare with the March-databank series above: differences are vintage.\n", sep = "")
} else cat("\n(fiscal_block_HPFD.csv not found in the working directory; rebuilt-block section skipped)\n")

cat("\nHAC = sandwich::vcovHAC (as in the main tables). RESET: Ramsey, powers 2-3 of fitted values.\n",
    "The fiscal specification replaces the linear trend with lagged tax per working-age person;\n",
    "the two are collinear (r = 0.98); the joint-entry block above shows what happens when they are.\n", sep = "")
cat("\n"); print(sessionInfo())
