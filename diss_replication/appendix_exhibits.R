# =============================================================================
# APPENDIX A EXHIBITS -- one script, one screenshot per subsection
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# WHY THIS FILE EXISTS
#   The appendix screenshots were taken from interactive sessions, which is why
#   one of them carried an R continuation prompt into the thesis and why the
#   "DR vs. OADR" and "Level vs. Delta" exhibits each showed only one side of
#   the comparison their heading promises. Every exhibit below prints as a
#   two- or three-column panel, so the heading and the picture agree, and the
#   DR placebo is no longer duplicated between two separate images.
#
#   Exhibits carry no numbers, so they can be ordered by significance in the
#   thesis. Every test statistic the thesis quotes prints
#   INSIDE its panel, above the closing rule, so a screenshot cropped at the
#   caption cannot silently drop it -- which is what happened to the nested F
#   in the baseline and DR exhibits and to the joint F in the MPR exhibit. The
#   diagnostics and stationarity exhibits replace the three
#   full-page console dumps (model diagnostics, stationarity) with panels of
#   the same shape as the rest.
#
# HOW TO RUN -- from a clean session, so nothing depends on session state:
#
#   Q                                     # only if you are at Browse[1]>
#   rm(list = ls())
#   setwd("~/quiet-instruments/diss_replication")   # only if R is not already in this folder
#   source("appendix_exhibits.R", echo = FALSE)
#
# Screenshot each "=====" block separately. Do not paste the file into the
# console line by line: that is what produces the stray "+" prompts.
#
# INPUT:  Dissertation_Dataset_Final.xlsx (sheet "ReformSPA"). Sample 1979-2019.
# =============================================================================

suppressPackageStartupMessages({library(lmtest); library(sandwich); library(car); library(readxl)})
suppressPackageStartupMessages(library(tseries)) # adf.test, jarque.bera.test -- diagnostics, stationarity

# -----------------------------------------------------------------------------
# Data -- built here in full, so the script has no prerequisites
# -----------------------------------------------------------------------------
DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
cat("data:", DATA_XLSX, "\n")

raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]
raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))

lag1 <- function(x) c(NA, head(x, -1))

reg_df <- with(raw, data.frame(
  year             = year,
  log_mig_rate     = loglev_nEU_WAPlagK,                        # scaled non-EU
  alt_log_mig_rate = log(nEU_inflows),                          # gross non-EU
  tot_log_mig_rate = log(nEU_inflows_WAPlagK + EU_inflows_WAPlagK),
  eu_log_mig_rate  = log(EU_inflows_WAPlagK),
  oadr_0          = oadr_0,
  delta_oadr_10    = delta_oadr_10,     # 10-yr change, old-age dependency
  delta_dr_10      = delta_dr_10,       # 10-yr change, TOTAL dependency
  level_oadr_10    = oadr_10,           # 10-yr projected LEVEL
  delta_zm_10      = delta_zm_10,       # zero-net-migration recomputation
  unem = unem, tax_workpop = tax_workpop, ln_debt_gdp = ln_debt_gdp,
  # effective tax rate on labour: (income tax + NICs) / compensation of
  # employees (ONS DTWM), in per cent. The workbook column carries a stray
  # space in its name; both spellings are accepted.
  lab_tax_comp = 100 * if ("lab_tax_comp" %in% names(raw)) lab_tax_comp else `lab_tax _comp`
))
for (v in c("log_mig_rate", "alt_log_mig_rate", "tot_log_mig_rate",
            "eu_log_mig_rate", "unem", "tax_workpop", "ln_debt_gdp", "lab_tax_comp"))
  reg_df[[paste0("L_", v)]] <- lag1(reg_df[[v]])
reg_df$trend <- seq_along(reg_df$year)

# MPR (2011) indicators, as the paper defines them (pp. 1411-12): steps, not
# windows. `liberal` is the thesis's own 1998-2007 window, kept for contrast.
reg_df$POL97 <- as.integer(reg_df$year >= 1998)
reg_df$POL02 <- as.integer(reg_df$year >= 2002)
reg_df$EXP04 <- as.integer(reg_df$year >= 2004)
reg_df$ENL86 <- as.integer(reg_df$year >= 1986)
reg_df$ENL95 <- as.integer(reg_df$year >= 1995)
reg_df$liberal <- as.integer(reg_df$year >= 1998 & reg_df$year <= 2007)

reg_df <- subset(reg_df, year >= 1979 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 41)
cat("sample:", min(reg_df$year), "-", max(reg_df$year), " N =", nrow(reg_df), "\n")

# -----------------------------------------------------------------------------
# Printing helpers
# -----------------------------------------------------------------------------
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                     ifelse(p < 0.10, "*", "")))
pf <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)

M <- function(formula, n_expect = 41) {
  m <- lm(formula, data = reg_df, na.action = na.omit)
  if (nobs(m) != n_expect)
    stop(sprintf("N = %d, expected %d for: %s", nobs(m), n_expect,
                 deparse(formula)))
  m
}

# A panel prints several models side by side: the focal coefficients with HAC
# standard-error p-values beneath, then a shared diagnostics block.
panel <- function(models, focal, title, w = 16, extra = NULL, lw = 14) {
  cat(sprintf("\n===== %s =====\n\n", title))
  cts <- lapply(models, function(m) coeftest(m, vcov. = vcovHAC(m)))
  hdr  <- sprintf("%-*s%s", lw, "",
                  paste(sprintf("%*s", w, names(models)), collapse = ""))
  rule <- strrep("-", nchar(hdr))
  cat(hdr, "\n", rule, "\n", sep = "")
  for (v in focal) {
    if (!any(vapply(cts, function(ct) v %in% rownames(ct), logical(1)))) next
    cat(sprintf("%-*s%s\n", lw, v, paste(vapply(cts, function(ct)
      if (v %in% rownames(ct))
        sprintf("%*s", w, sprintf("%.4f%s", ct[v, 1], stars(ct[v, 4])))
      else sprintf("%*s", w, "--"), character(1)), collapse = "")))
    cat(sprintf("%-*s%s\n", lw, "", paste(vapply(cts, function(ct)
      if (v %in% rownames(ct))
        sprintf("%*s", w, paste0("(", pf(ct[v, 4]), ")"))
      else sprintf("%*s", w, ""), character(1)), collapse = "")))
  }
  cat(rule, "\n", sep = "")
  stat <- function(lab, f) cat(sprintf("%-*s%s\n", lw, lab,
    paste(vapply(models, function(m) sprintf("%*s", w, f(m)), character(1)),
          collapse = "")))
  stat("N",        function(m) sprintf("%d", nobs(m)))
  stat("adj. R2",  function(m) sprintf("%.4f", summary(m)$adj.r.squared))
  stat("AIC",      function(m) sprintf("%.2f", AIC(m)))
  stat("RESET p",  function(m) pf(resettest(m)$p.value))
  stat("BG(1) p",  function(m) pf(bgtest(m, order = 1)$p.value))
  stat("max VIF",  function(m) sprintf("%.1f", max(vif(m))))
  # Nested F against the same model without its projection term, computed for
  # every column automatically (F(1, df) with p from the classical F; dAIC on
  # restoring the term). Skipped when the caller supplies its own "F vs no proj"
  # rows, and blank for a column with no projection term.
  if (!any(grepl("^F ", names(extra)))) {   # skipped when the caller supplies its own F rows
    PROJ <- c("delta_oadr_10", "level_oadr_10", "delta_dr_10", "level_dr_10", "delta_zm_10",
              paste0("delta_oadr_", c(1, 3, 5, 7)), paste0("delta_dr_", c(1, 3, 5)))
    F <- P <- D <- rep("--", length(models)); P[] <- ""
    for (k in seq_along(models)) {
      term <- intersect(PROJ, names(coef(models[[k]])))
      if (length(term) != 1) next
      r <- update(models[[k]], as.formula(paste(". ~ . -", term)))
      a <- anova(r, models[[k]])
      F[k] <- sprintf("%.2f", a$F[2]); P[k] <- paste0("(", pf(a$`Pr(>F)`[2]), ")")
      D[k] <- sprintf("%+.2f", AIC(models[[k]]) - AIC(r))
    }
    if (any(F != "--")) extra <- c(extra, setNames(list(F, P, D), c("F vs no proj", "  p", "  dAIC")))
  }
  # Test rows live INSIDE the block, above the closing rule, so that cropping a
  # screenshot at the caption cannot drop the statistics the text quotes.
  # by position, not name: two blocks can both carry a "  p" row, and extra[[name]]
  # would print the first block's p-values under the second block's F.
  for (i in seq_along(extra))
    cat(sprintf("%-*s%s\n", lw, names(extra)[i],
        paste(vapply(extra[[i]], function(s) sprintf("%*s", w, s), character(1)),
              collapse = "")))
  cat(rule, "\n", sep = "")
  cat("HAC (vcovHAC) p-values in parentheses. *** p<0.01, ** p<0.05, * p<0.10.\n",
      "F rows: classical nested F (iid errors); joint and restriction F use HAC.\n", sep = "")
  invisible(models)
}

# Nested F, its p-value and the change in AIC, one column per model. Pass NULL
# in `restricted` for a column that is itself the restricted model.
nested_rows <- function(models, restricted, label = "F vs no proj") {
  F <- P <- D <- character(length(models))
  for (k in seq_along(models)) {
    r <- restricted[[k]]
    if (is.null(r)) { F[k] <- "--"; P[k] <- ""; D[k] <- "--"; next }
    a <- anova(r, models[[k]])
    F[k] <- sprintf("%.2f", a$F[2])
    P[k] <- paste0("(", pf(a$`Pr(>F)`[2]), ")")
    D[k] <- sprintf("%+.2f", AIC(models[[k]]) - AIC(r))
  }
  setNames(list(F, P, D), c(label, "  p", "  dAIC"))
}

# Joint test of several terms under the same HAC covariance as the coefficients.
joint_rows <- function(models, terms) {
  F <- P <- character(length(models))
  for (k in seq_along(models)) {
    lh <- linearHypothesis(models[[k]], paste0(terms, " = 0"),
                           vcov. = vcovHAC(models[[k]]))
    F[k] <- sprintf("%.2f", lh$F[2])
    P[k] <- paste0("(", pf(lh$`Pr(>F)`[2]), ")")
  }
  setNames(list(F, P),
           c(sprintf("joint F(%d,%d)", length(terms), df.residual(models[[1]])), "  p"))
}

BASE_T <- c("L_log_mig_rate", "trend", "L_ln_debt_gdp", "L_unem")
BASE_F <- c("L_log_mig_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")
add <- function(base, x, y = "log_mig_rate") reformulate(c(base, x), y)

# =============================================================================
# EXHIBIT.  Baseline specification -- what the projection adds
# =============================================================================
# The specification carries a lagged dependent variable, so adjusted R2 is high
# before any projection enters and the increment to R2 is the wrong measure.
# The nested F and the information criterion are the informative comparisons.
b_trend  <- M(reformulate(BASE_T, "log_mig_rate"))
b_fiscal <- M(reformulate(BASE_F, "log_mig_rate"))
m_trend  <- M(add(BASE_T, "delta_oadr_10"))
m_fiscal <- M(add(BASE_F, "delta_oadr_10"))

# Tax and trend entered together. Individual coefficients are not interpretable
# (VIF ~ 60); the exhibit is that the trend loses significance once the tax
# burden is present while the projection term is unchanged.
m_joint <- M(reformulate(c("L_log_mig_rate", "L_tax_workpop", "trend",
                           "delta_oadr_10", "L_ln_debt_gdp", "L_unem"), "log_mig_rate"))
b_none <- M(reformulate(c("L_log_mig_rate", "L_ln_debt_gdp", "L_unem"), "log_mig_rate"))
# Five columns (Sam, 23 Sep 2026): (1) no OADR, no tax; (2) no OADR + tax; (3) OADR, fiscal
# specification; (4) OADR, trend specification; (5) OADR with tax and trend together.
# "F added term" tests what each column adds to its restricted counterpart:
# (2) the tax term over (1); (3) the projection over (2); (4) the projection over
# the trend model without it (b_trend, not shown); (5) the trend over (3).
a1 <- list("No OADR,no tax" = b_none, "No OADR + tax" = b_fiscal, "OADR fiscal" = m_fiscal,
           "OADR trend" = m_trend, "OADR fisc/trend" = m_joint)
panel(a1, c("delta_oadr_10", "trend", "L_tax_workpop", "L_ln_debt_gdp", "L_unem"),
      "Baseline specification", w = 16, lw = 15,
      extra = nested_rows(a1, list(NULL, b_none, b_fiscal, b_trend, m_fiscal),
                          label = "F added term"))
cat("F added term: (2) tax over (1); (3) projection over (2); (4) projection over the\n",
    "trend model without it; (5) trend over (3).\n", sep = "")

# =============================================================================
# EXHIBITS.  The tax measure: per-capita measures, then GDP shares
# =============================================================================
# Same specification, the tax term measured six ways, all from ONE source:
# the OBR Historical Public Finances Database in 2024-25 prices (HM Treasury
# March 2026 deflator), written to fiscal_block_HPFD.csv. Names follow the
# workbook convention:
#   tax_workpop      national accounts taxes per working-age person (Table 2)
#   tax_totpop       national accounts taxes per resident
#   lab_tax_workpop  income tax+NICs per working-age person
#   lab_tax_totpop   income tax + NICs per resident
#   hh_tax_workpop   income tax + NICs + VAT per working-age person
#   hh_tax_totpop    income tax + NICs + VAT per resident
#   tax_gdp          national accounts taxes as a share of GDP (per cent)
#   hh_tax_gdp       income tax + NICs + VAT as a share of GDP (per cent)
#   lab_tax_comp     income tax + NICs / compensation of employees (per cent)
# Per-head measures are in thousands of pounds; all are lagged one year.
# Per-worker measures carry the political-cost argument (P2): the burden falls
# on those who pay it. GDP shares measure the state's take of output rather
# than what any taxpayer bears; they are shown in a second panel so the two
# ideas are not read as interchangeable. The HPFD series reproduces the
# workbook's tax_workpop exactly (the workbook now carries it), so column (1) of the first
# panel is the Table 2 specification.
if (file.exists("fiscal_block_HPFD.csv")) {
  fb <- read.csv("fiscal_block_HPFD.csv"); fb <- fb[order(fb$year), ]
  fb$tax_workpop     <- fb$tax_nat_real_gbp_bn / fb$wap_m
  fb$tax_totpop      <- fb$tax_nat_real_gbp_bn / fb$totpop_m
  fb$lab_tax_workpop <- fb$tax_lab_real_gbp_bn / fb$wap_m
  fb$lab_tax_totpop  <- fb$tax_lab_real_gbp_bn / fb$totpop_m
  fb$hh_tax_workpop  <- (fb$tax_lab_real_gbp_bn + fb$tax_vat_real_gbp_bn) / fb$wap_m
  fb$hh_tax_totpop   <- (fb$tax_lab_real_gbp_bn + fb$tax_vat_real_gbp_bn) / fb$totpop_m
  fb$tax_gdp         <- fb$tax_nat_gdp
  fb$lab_tax_gdp     <- fb$tax_lab_gdp
  fb$hh_tax_gdp      <- fb$tax_lab_gdp + fb$vat_gdp
  TAX_PC  <- c("tax_workpop", "tax_totpop", "lab_tax_workpop", "lab_tax_totpop",
               "hh_tax_workpop", "hh_tax_totpop")
  TAX_GDP <- c("tax_gdp", "hh_tax_gdp")
  # A copy of the estimation frame with the HPFD series in place of the
  # workbook's, so that every column of both panels shares one source.
  tax_df <- reg_df
  for (v in c(TAX_PC, TAX_GDP))
    tax_df[[paste0("L_", v)]] <- fb[[v]][match(tax_df$year - 1, fb$year)]
  stopifnot(!anyNA(tax_df[paste0("L_", c(TAX_PC, TAX_GDP))]))
  tax_model <- function(v) {
    m <- lm(reformulate(c("L_log_mig_rate", v, "delta_oadr_10", "L_ln_debt_gdp", "L_unem"),
                        "log_mig_rate"), data = tax_df)
    stopifnot(nobs(m) == 41); m
  }
  src_note <- function() cat(
      "NAT = national accounts taxes; WAP = working-age population; Lab. tax = income\n",
      "tax and National Insurance contributions. All columns: OBR Historical Public\n",
      "Finances Database, 2024-25 prices (HMT March 2026 deflator), lagged one year.\n", sep = "")

  # Per-capita measures, two panels: per working-age person, then per person (total population).
  a_wap <- setNames(lapply(paste0("L_", c("tax_workpop", "lab_tax_workpop", "hh_tax_workpop")), tax_model),
                    c("NAT", "Lab. tax", "Lab. tax+VAT"))
  panel(a_wap, c("delta_oadr_10", "L_tax_workpop", "L_lab_tax_workpop", "L_hh_tax_workpop",
                 "L_ln_debt_gdp", "L_unem"),
        "Tax burden per working-age person", w = 18, lw = 18)
  src_note()
  cat("GBP thousand per working-age person. Lab. tax+VAT = income tax, National\n",
      "Insurance contributions and VAT, the taxes borne directly by households.\n",
      "Column (1) is the Table 2 specification.\n", sep = "")

  a_pop <- setNames(lapply(paste0("L_", c("tax_totpop", "lab_tax_totpop", "hh_tax_totpop")), tax_model),
                    c("NAT", "Lab. tax", "Lab. tax+VAT"))
  panel(a_pop, c("delta_oadr_10", "L_tax_totpop", "L_lab_tax_totpop", "L_hh_tax_totpop",
                 "L_ln_debt_gdp", "L_unem"),
        "Tax burden per total population", w = 18, lw = 18)
  src_note()
  cat("GBP thousand per person (total population).\n")

  # Relative to the current OADR (Sam's measure, the workbook's L_tax_oadr_0): tax in
  # GBP bn divided by the lagged current old-age dependency ratio (per 1,000).
  o0 <- raw$oadr_0[match(tax_df$year - 1, raw$year)]                  # lagged, per 1,000
  numr <- list(tax_oadr     = fb$tax_nat_real_gbp_bn,
               lab_tax_oadr = fb$tax_lab_real_gbp_bn,
               hh_tax_oadr  = fb$tax_lab_real_gbp_bn + fb$tax_vat_real_gbp_bn)
  for (nv in names(numr))
    tax_df[[paste0("L_", nv)]] <- numr[[nv]][match(tax_df$year - 1, fb$year)] / o0
  if ("L_tax_oadr_0" %in% names(raw))
    stopifnot(max(abs(tax_df$L_tax_oadr - raw$L_tax_oadr_0[match(tax_df$year, raw$year)])) < 1e-4)
  a_pen <- setNames(lapply(paste0("L_", names(numr)), tax_model),
                    c("NAT", "Lab. tax", "Lab. tax+VAT"))
  panel(a_pen, c("delta_oadr_10", "L_tax_oadr", "L_lab_tax_oadr", "L_hh_tax_oadr",
                 "L_ln_debt_gdp", "L_unem"),
        "Tax burden relative to the current OADR", w = 18, lw = 18)
  src_note()
  cat("Tax divided by the current old-age dependency ratio (per 1,000), lagged one\n",
      "year; column (1) is the workbook's L_tax_oadr_0.\n", sep = "")

  tax_df$L_lab_tax_wages <- tax_df$L_lab_tax_comp    # "wages" = compensation of employees
  a_gdp <- setNames(lapply(paste0("L_", c(TAX_GDP, "lab_tax_wages")), tax_model),
                    c("NAT/GDP", "Lab. tax+VAT/GDP", "Lab. tax/wages"))
  panel(a_gdp, c("delta_oadr_10", paste0("L_", TAX_GDP), "L_lab_tax_wages", "L_ln_debt_gdp", "L_unem"),
        "Tax burden: ratio measures", w = 18, lw = 16)
  src_note()
  cat("Shares in per cent. Lab. tax/wages is the average effective tax rate on\n",
      "labour (Mendoza, Razin and Tesar, 1994): income tax + NICs over wages\n",
      "(compensation of employees, ONS series DTWM), the one ONS denominator.\n", sep = "")
  # Tax per income-tax payer: HMRC Table 2.1 (SPI), 1990-91 onwards, so a
  # sub-sample check on the LTIM pattern (N = 28: the lagged regressor makes
  # 1992 the first usable year). 2008-09 is interpolated.
  if ("inc_taxpayersK" %in% names(raw)) {
    tp <- raw$inc_taxpayersK[match(reg_df$year, raw$year)]
    tax_df$L_lab_tax_payer <- fb$tax_lab_real_gbp_bn[match(tax_df$year - 1, fb$year)] /
      (tp[match(tax_df$year - 1, tax_df$year)] / 1000)   # GBP thousand per taxpayer
    tax_df$L_tax_payer     <- fb$tax_nat_real_gbp_bn[match(tax_df$year - 1, fb$year)] /
      (tp[match(tax_df$year - 1, tax_df$year)] / 1000)
    sub <- subset(tax_df, year >= 1992)   # lagged regressor: first usable year is 1992
    tp_model <- function(v) {
      m <- lm(reformulate(c("L_log_mig_rate", v, "delta_oadr_10", "L_ln_debt_gdp", "L_unem"),
                          "log_mig_rate"), data = sub)
      stopifnot(nobs(m) == 28); m
    }
    a_tp <- list("NAT/WAP, 1992-"    = tp_model("L_tax_workpop"),
                 "NAT/taxpayer"      = tp_model("L_tax_payer"),
                 "Lab. tax/taxpayer" = tp_model("L_lab_tax_payer"))
    panel(a_tp, c("delta_oadr_10", "L_tax_workpop", "L_tax_payer", "L_lab_tax_payer",
                  "L_ln_debt_gdp", "L_unem"),
          "Tax burden per income-tax payer, 1992-2019", w = 20, lw = 16)
    cat("Taxpayer counts: HMRC Table 2.1 (Survey of Personal Incomes), thousands, from\n",
        "1990-91; 2008-09 interpolated. GBP thousand per income-tax payer, 2024-25 prices,\n",
        "lagged. Column (1) re-estimates the Table 2 measure on the same sub-sample.\n", sep = "")

    # Full-sample variant. Counts for 1978-79 to 1989-90 are HMRC Table 2.1a
    # ("Number of individuals paying Income Tax", Inland Revenue Statistics
    # 1994), which overlaps Table 2.1 at 1990-91 and 1991-92 with identical
    # values, so the two tables splice without adjustment. Wives' investment
    # income is counted separately only from 1990-91 (Table 2.1a, footnote 2),
    # so a step indicator from 1991 absorbs that change of basis. The lagged
    # regressor makes 1980 the first usable year (N = 40).
    if (!is.na(raw$inc_taxpayersK[raw$year == 1980])) {
      tp_all <- raw$inc_taxpayersK[match(1979:2020, raw$year)]
      tp_all <- approx(1979:2020, tp_all, xout = 1979:2020, rule = 2)$y   # fills 2008-09 only
      L_tp   <- tp_all[match(tax_df$year - 1, 1979:2020)] / 1000           # millions, lagged
      tax_df$L_tax_payer_all     <- fb$tax_nat_real_gbp_bn[match(tax_df$year - 1, fb$year)] / L_tp
      tax_df$L_lab_tax_payer_all <- fb$tax_lab_real_gbp_bn[match(tax_df$year - 1, fb$year)] / L_tp
      tax_df$indep <- as.integer(tax_df$year >= 1991)                       # independent taxation
      all_df <- subset(tax_df, year >= 1980)
      stopifnot(!anyNA(all_df$L_lab_tax_payer_all))
      all_model <- function(v) {
        m <- lm(reformulate(c("L_log_mig_rate", v, "indep", "delta_oadr_10", "L_ln_debt_gdp", "L_unem"),
                            "log_mig_rate"), data = all_df)
        stopifnot(nobs(m) == 40); m
      }
      a_all <- list("NAT/WAP+step"    = all_model("L_tax_workpop"),
                    "NAT/taxpayer"      = all_model("L_tax_payer_all"),
                    "Lab. tax/taxpayer" = all_model("L_lab_tax_payer_all"))
      panel(a_all, c("delta_oadr_10", "L_tax_workpop", "L_tax_payer_all", "L_lab_tax_payer_all",
                     "indep", "L_ln_debt_gdp", "L_unem"),
            "Tax burden per income-tax payer, 1980-2019", w = 20, lw = 20)
      cat("Counts: HMRC Table 2.1a (Inland Revenue Statistics 1994) for 1978-79 to 1989-90\n",
          "and Table 2.1 from 1990-91, individuals paying Income Tax; 2008-09 interpolated.\n",
          "'indep' = 1 from 1991: wives' investment income counted separately under\n",
          "independent taxation. Column (1) adds the same indicator to the Table 2 measure;\n",
          "that it is significant there too, with no taxpayer count in the model, shows it is\n",
          "absorbing a 1991 level shift in inflows rather than the change of basis.\n", sep = "")
    }
  }
} else cat("\n(fiscal_block_HPFD.csv not found: tax-measure exhibits skipped)\n")

# =============================================================================
# EXHIBIT.  DR vs. OADR -- both sides, both specifications
# =============================================================================
# Standing rule (Sam, 21 Sep 2026): the fiscal specification alone is quoted
# and shown in every later exhibit; the baseline exhibit is the one place the
# trend comparison lives.
# The trend counterparts (m_*_t) are still fitted for the record.
# Shown against its old-age counterpart, the contrast is the point: the
# pensioner-specific ratio earns its degree of freedom and the total ratio does
# not, in either specification.
m_dr_f <- M(add(BASE_F, "delta_dr_10"))
m_dr_t <- M(add(BASE_T, "delta_dr_10"))
a2 <- list("+OADR" = m_fiscal, "+DR" = m_dr_f)
panel(a2, c("delta_oadr_10", "delta_dr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem"),
      "Old-age vs. total dependency projection",
      extra = nested_rows(a2, list(b_fiscal, b_fiscal)))

# =============================================================================
# EXHIBIT.  Projection level vs. delta -- both specifications
# =============================================================================
m_lev_f <- M(add(BASE_F, "level_oadr_10"))
m_lev_t <- M(add(BASE_T, "level_oadr_10"))
# The projected level is an identity: level = current OADR + projected change.
# The level model therefore imposes equal coefficients on the two. Column (3)
# enters them separately; the last rows test that restriction under HAC.
m_sep <- M(add(BASE_F, c("delta_oadr_10", "oadr_0")))
a3 <- list("delta" = m_fiscal, "level" = m_lev_f, "delta + current" = m_sep)
lh3 <- linearHypothesis(m_sep, "delta_oadr_10 = oadr_0", vcov. = vcovHAC(m_sep))
panel(a3, c("delta_oadr_10", "level_oadr_10", "oadr_0", "L_tax_workpop",
            "L_ln_debt_gdp", "L_unem"),
      "Projected level vs. projected change",
      extra = setNames(list(c("--", "--", sprintf("%.2f", lh3$F[2])),
                            c("", "", paste0("(", pf(lh3$`Pr(>F)`[2]), ")"))),
                       c(sprintf("restr. F(1,%d)", lh3$Res.Df[2]), "  p")))   # df from the fit
cat("Column (2) restricts the current ratio and the projected change to a common\n",
    "coefficient; the F row tests that restriction in column (3).\n", sep = "")

# =============================================================================
# EXHIBIT.  Gross vs. scaled non-EU inflows -- both specifications
# =============================================================================
m_gross_f <- M(reformulate(c("L_alt_log_mig_rate", "L_tax_workpop", "delta_oadr_10",
                             "L_ln_debt_gdp", "L_unem"), "alt_log_mig_rate"))
m_gross_t <- M(reformulate(c("L_alt_log_mig_rate", "trend", "delta_oadr_10",
                             "L_ln_debt_gdp", "L_unem"), "alt_log_mig_rate"))
panel(list("scaled rate" = m_fiscal, "gross inflows" = m_gross_f),
      c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem"),
      "Scaled rate vs. gross non-EU inflows")

# =============================================================================
# EXHIBIT.  EU vs. non-EU -- the uncontrolled margin, both specifications
# =============================================================================
# The EU placebo is the plain specification re-estimated on EU inflows; the
# enlargement-year variant was dropped (Sam, 18 Sep 2026).
m_eu_f <- M(reformulate(c("L_eu_log_mig_rate", "L_tax_workpop", "delta_oadr_10",
                          "L_ln_debt_gdp", "L_unem"), "eu_log_mig_rate"))
m_eu_t <- M(reformulate(c("L_eu_log_mig_rate", "trend", "delta_oadr_10",
                          "L_ln_debt_gdp", "L_unem"), "eu_log_mig_rate"))
panel(list("non-EU" = m_fiscal, "EU" = m_eu_f),
      c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem"),
      "Controlled (non-EU) vs. uncontrolled (EU) margin")

# =============================================================================
# EXHIBIT.  Zero-migration projection delta
# =============================================================================
# The regressor is recomputed with the vintages' own migration assumptions
# stripped out, so it cannot contain them.
m_zm_t <- M(reformulate(c("L_log_mig_rate", "trend", "delta_zm_10",
                          "L_ln_debt_gdp", "L_unem"), "log_mig_rate"))
m_zm_f <- M(reformulate(c("L_log_mig_rate", "L_tax_workpop", "delta_zm_10",
                          "L_ln_debt_gdp", "L_unem"), "log_mig_rate"))
panel(list("published" = m_fiscal, "zero-mig" = m_zm_f),
      c("delta_oadr_10", "delta_zm_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem"),
      "Published vs. zero-net-migration projections")

# =============================================================================
# EXHIBIT.  Policy dummies (Mitchell, Pain and Riley, 2011)
# =============================================================================
# The joint test uses the SAME HAC covariance as the coefficient tests. anova()
# would return the classical F, mixing two variance estimators in one table --
# which is how an indicator can read as individually significant while the set
# reads as jointly irrelevant. Coefficients are from the five-indicator model,
# so they differ from the same indicator entered singly.
DUM <- c("POL97", "POL02", "EXP04", "ENL86", "ENL95")
pd <- list()
for (s in list(c(y = "log_mig_rate",     L = "L_log_mig_rate",     sec = "L_tax_workpop", lab = "(1) non-EU"),
               c(y = "tot_log_mig_rate", L = "L_tot_log_mig_rate", sec = "L_tax_workpop", lab = "(2) total"))) {
  pd[[s["lab"]]] <- M(reformulate(c(s["L"], s["sec"], "delta_oadr_10",
                                    "L_unem", "L_ln_debt_gdp", DUM), s["y"]))
}
panel(pd, c("POL97", "POL02", "EXP04", "delta_oadr_10"),
      "MPR policy-era indicators",
      extra = joint_rows(pd, DUM))
cat("ENL86 and ENL95 estimated in both columns but not displayed: EU accession\n",
    "indicators on a non-EU dependent variable are not separately interpretable.\n",
    sep = "")

# =============================================================================
# EXHIBIT.  Model diagnostics
# =============================================================================
# Replaces the two full-page `summary()` dumps. Everything they carried that is
# not already in the baseline exhibit appears here: the five specification tests, the residual
# unit-root check, and the comparison of the two HAC variants, which is the
# evidence for the thesis's claim that the choice between them is immaterial.
hac_p <- function(m, v = "delta_oadr_10")
  coeftest(m, vcov. = vcovHAC(m))[v, 4]
nw_p  <- function(m, v = "delta_oadr_10")
  coeftest(m, vcov. = NeweyWest(m, lag = 1, prewhite = FALSE, adjust = TRUE))[v, 4]

diag_panel <- function(models, title, w = 16) {
  cat(sprintf("\n===== %s =====\n\n", title))
  hdr  <- sprintf("%-22s%s", "",
                  paste(sprintf("%*s", w, names(models)), collapse = ""))
  rule <- strrep("-", nchar(hdr))
  cat(hdr, "\n", rule, "\n", sep = "")
  row <- function(lab, f) cat(sprintf("%-22s%s\n", lab,
    paste(vapply(models, function(m) sprintf("%*s", w, f(m)), character(1)),
          collapse = "")))
  row("Breusch-Godfrey(1)", function(m) pf(bgtest(m, order = 1)$p.value))
  row("Breusch-Godfrey(2)", function(m) pf(bgtest(m, order = 2)$p.value))
  row("Breusch-Pagan",      function(m) pf(bptest(m)$p.value))
  row("Ramsey RESET",       function(m) pf(resettest(m)$p.value))
  row("Jarque-Bera",        function(m) pf(jarque.bera.test(residuals(m))$p.value))
  row("ADF on residuals",   function(m)
      pf(suppressWarnings(adf.test(residuals(m))$p.value)))
  row("max VIF",            function(m) sprintf("%.1f", max(vif(m))))
  cat(rule, "\n", sep = "")
  row("dOADR10 p, HAC",     function(m) pf(hac_p(m)))
  row("dOADR10 p, NW(1)",   function(m) pf(nw_p(m)))
  cat(rule, "\n", sep = "")
  cat("All entries are p-values except max VIF. The residual ADF uses standard\n",
      "Dickey-Fuller critical values and is a diagnostic only, NOT a\n",
      "cointegration test; the bounds test below is the formal instrument.\n",
      sep = "")
}
diag_panel(list("trend spec" = m_trend, "fiscal spec" = m_fiscal),
           "Model diagnostics")

# =============================================================================
# EXHIBIT.  Stationarity
# =============================================================================
# The appendix previously printed the full 48-series classification. Reproduced
# here for the series that enter the reported specifications; the complete set
# is in stationarity_bounds_replication.R and is unchanged by this reduction.
adf_panel <- function(vars, title) {
  cat(sprintf("\n===== %s =====\n\n", title))
  hdr  <- sprintf("%-20s%14s%14s%9s", "series", "ADF p (level)",
                  "ADF p (diff)", "class")
  rule <- strrep("-", nchar(hdr))
  cat(hdr, "\n", rule, "\n", sep = "")
  for (v in vars) {
    x  <- reg_df[[v]]; x <- x[!is.na(x)]
    pl <- suppressWarnings(adf.test(x)$p.value)
    pd <- suppressWarnings(adf.test(diff(x))$p.value)
    cat(sprintf("%-20s%14s%14s%9s\n", v, pf(pl), pf(pd),
                if (pl < 0.05) "I(0)" else "I(1)"))
  }
  cat(rule, "\n", sep = "")
  cat("tseries::adf.test, lag order k = trunc((n-1)^(1/3)) = 3, trend included\n",
      "in both regressions. The classification rests on the level test: at\n",
      "T = 40 the differenced test has very little power, so p(diff) is\n",
      "informational. Full 48-series table: stationarity_bounds_replication.R.\n",
      sep = "")
}
adf_panel(c("log_mig_rate", "alt_log_mig_rate", "tot_log_mig_rate",
            "eu_log_mig_rate", "delta_oadr_10", "delta_dr_10", "level_oadr_10",
            "delta_zm_10", "unem", "tax_workpop", "ln_debt_gdp"),
          "Stationarity (ADF, level and first difference)")

# =============================================================================
# EXHIBIT.  PSS bounds tests, statistic against the critical bounds
# =============================================================================
# The htest print gives a p-value but not the bounds, so a reader cannot see
# whether the statistic falls in the INCONCLUSIVE region between the I(0) and
# I(1) critical values -- which is the whole point of the procedure, and which
# matters here because the t-version is marginal.
#
# WHY exact = TRUE IS PASSED WITH alpha. Called with alpha alone the package
# returns ASYMPTOTIC bounds, reporting T = 1000 in null.value, together with an
# asymptotic p-value: 0.0096 for the F-test here, against 0.0303 from the
# finite-sample test. Printing asymptotic bounds beside a finite-sample
# statistic would repeat, in a different currency, the error of mixing two
# variance estimators in one table. T is printed below so the reader can see
# which set is in use.
#
# Requires the ARDL package. The exhibits above do not.
suppressPackageStartupMessages(library(ARDL))   # silences the package citation banner

ts_df <- ts(reg_df[, c("log_mig_rate", "delta_oadr_10", "unem",
                       "ln_debt_gdp", "tax_workpop")],
            start = min(reg_df$year), frequency = 1)
m_ardl <- ardl(log_mig_rate ~ delta_oadr_10 + unem + ln_debt_gdp + tax_workpop,
               data = ts_df, order = c(1, 1, 1, 1, 1))

bounds_panel <- function(mod, case = 3, alphas = c(0.10, 0.05, 0.01)) {
  cat("\n===== PSS bounds tests against critical bounds =====\n\n")
  for (nm in c("F", "t")) {
    FUN <- if (nm == "F") bounds_f_test else bounds_t_test
    ref <- tryCatch(FUN(mod, case = case, alpha = alphas[1], exact = TRUE),
                    error = function(e) NULL)
    exact_ok <- !is.null(ref)
    if (!exact_ok) ref <- FUN(mod, case = case, alpha = alphas[1])
    s  <- unname(ref$statistic)
    kT <- ref$null.value
    set.seed(20260802)
    pv <- tryCatch(unname(FUN(mod, case = case, exact = TRUE)$p.value),
                   error = function(e) NA_real_)
    cat(sprintf("%s-bounds test:  %s = %.4f,  p = %s   (k = %g, T = %g%s)\n",
                nm, nm, s, if (is.na(pv)) "--" else sprintf("%.5f", pv),
                kT[["k"]], kT[["T"]], if (exact_ok) "" else "  ASYMPTOTIC"))
    hdr  <- sprintf("%10s%13s%13s%17s", "alpha", "I(0) bound", "I(1) bound",
                    "verdict")
    rule <- strrep("-", nchar(hdr))
    cat(hdr, "\n", rule, "\n", sep = "")
    for (a in alphas) {
      r  <- if (exact_ok) FUN(mod, case = case, alpha = a, exact = TRUE)
            else          FUN(mod, case = case, alpha = a)
      b  <- r$parameters
      # LABELS come from the element names, not from min/max. For the F-test the
      # I(0) bound is the smaller number, but for the t-test both bounds are
      # negative and the I(1) bound is the MORE negative of the two, so sorting
      # numerically and calling the smaller one "I(0)" prints the columns the
      # wrong way round.
      i0 <- unname(b[grepl("I(0)", names(b), fixed = TRUE)])
      i1 <- unname(b[grepl("I(1)", names(b), fixed = TRUE)])
      stopifnot(length(i0) == 1, length(i1) == 1)
      # VERDICTS use min/max, which is direction-safe either way: the F
      # statistic is positive and rejects above the outer bound, the t statistic
      # is negative and rejects below it.
      lo <- min(b); hi <- max(b)
      verdict <- if (nm == "F") {
        if (s > hi) "reject H0" else if (s < lo) "no level rel." else "inconclusive"
      } else {
        if (s < lo) "reject H0" else if (s > hi) "no level rel." else "inconclusive"
      }
      cat(sprintf("%10.2f%13.2f%13.2f%17s\n", a, i0, i1, verdict))
    }
    cat(rule, "\n\n", sep = "")
  }
  cat("H0: no level relationship. Case 3: unrestricted intercept, no trend.\n",
      "'Inconclusive' is the region between the two bounds, where the test\n",
      "cannot decide without knowing the regressors' integration orders --\n",
      "which is why the stationarity exhibit classifies them.\n", sep = "")
}
bounds_panel(m_ardl)

cat("\n"); print(sessionInfo())
