# int_tme_check.R -- debt interest instead of, and in addition to, inflation (28 Sep 2026)
#
# Run from diss_replication/ (same folder as run_all_1964.R):
#     setwd("~/quiet-instruments/diss_replication"); source("../restud/diss_replication_patches/int_tme_check.R")
# or copy the file into diss_replication/ and source("int_tme_check.R").
# Output goes to int_tme_check_output.txt in the working directory. Paste it back.
#
# What it does, on BOTH samples (extended 1965-2019 and core 1977-2019):
#   1. the fiscal model with and without the projection, then +inflation, +int_tme,
#      +inflation +int_tme, each with and without the projection (8 columns);
#   2. the correlation matrix behind the inflation worry (inflation vs lagged
#      migration, the debt-ratio change, the projection, int_tme);
#   3. int_tme in the gross-inflow equation and (core only) in the EU placebo.
# It uses plain lm + sandwich::vcovHAC so it does not depend on the printer in
# tables_common.R; it only needs data_common.R to build `df` with the canonical names.
# Not run in this session (no R, no data here) -- if it errors, paste the error.

suppressPackageStartupMessages({ library(sandwich); library(lmtest) })

out <- "int_tme_check_output.txt"
con <- file(out, open = "wt"); sink(con, split = TRUE); sink(con, type = "message")
on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)
options(width = 150, scipen = 999)
cat("int_tme_check.R --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-- wd:", getwd(), "\n")

load_df <- function(sample) {
  assign("SAMPLE", sample, envir = globalenv())
  e <- new.env(); source("data_common.R", local = e)
  d <- get("df", envir = e)
  yr <- intersect(c("year", "Year", "yr"), names(d))[1]
  y0 <- if (sample == "1964") 1965 else 1977
  if (!is.na(yr)) d <- d[d[[yr]] >= y0 & d[[yr]] <= 2019, ]
  d
}

fit_one <- function(d, rhs, dv = "mig_rate") {
  rhs <- rhs[rhs %in% names(d)]
  f <- reformulate(rhs, response = dv)
  dd <- d[complete.cases(d[, c(dv, rhs)]), ]
  m <- lm(f, data = dd)
  keep <- !is.na(coef(m))
  ct <- coeftest(m, vcov. = vcovHAC(m))
  list(m = m, ct = ct, n = nobs(m), aic = AIC(m),
       reset = tryCatch(resettest(m)$p.value, error = function(e) NA),
       bg = tryCatch(bgtest(m, order = 1)$p.value, error = function(e) NA))
}

show_block <- function(fits, terms, title) {
  cat("\n=====", title, "=====\n")
  w <- 18
  cat(formatC("", width = 20), paste(formatC(names(fits), width = w), collapse = ""), "\n")
  for (tm in terms) {
    co <- pv <- character(0)
    for (f in fits) {
      if (tm %in% rownames(f$ct)) {
        b <- f$ct[tm, 1]; p <- f$ct[tm, 4]
        st <- ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))
        co <- c(co, formatC(sprintf("%.4f%s", b, st), width = w))
        pv <- c(pv, formatC(sprintf("(%s)", if (p < .001) "<.001" else sprintf("%.3f", p)), width = w))
      } else { co <- c(co, formatC("--", width = w)); pv <- c(pv, formatC("", width = w)) }
    }
    cat(formatC(tm, width = -20), paste(co, collapse = ""), "\n")
    cat(formatC("", width = 20), paste(pv, collapse = ""), "\n")
  }
  for (stat in c("n", "aic", "reset", "bg")) {
    v <- sapply(fits, function(f) f[[stat]])
    lab <- c(n = "N", aic = "AIC", reset = "RESET p", bg = "BG(1) p")[stat]
    fmt <- if (stat == "n") "%d" else if (stat == "aic") "%.2f" else "%.3f"
    cat(formatC(lab, width = -20), paste(formatC(sprintf(fmt, v), width = w), collapse = ""), "\n")
  }
  cat("HAC (vcovHAC) p-values in parentheses. *** p<0.01, ** p<0.05, * p<0.10.\n")
}

for (sample in c("1964", "1977")) {
  d <- load_df(sample)
  label <- if (sample == "1964") "extended sample, 1965-2019" else "core sample, 1977-2019"
  base <- c("L_mig_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", if (sample == "1964") "basis_pre1975")
  P <- "delta_oadr_10"
  specs <- list(
    "fiscal, no proj"           = base,
    "fiscal"                    = c(P, base),
    "+inflation, no proj"       = c(base, "L_inflation"),
    "+inflation"                = c(P, base, "L_inflation"),
    "+int_tme, no proj"         = c(base, "L_int_tme"),
    "+int_tme"                  = c(P, base, "L_int_tme"),
    "+infl +int_tme, no proj"   = c(base, "L_inflation", "L_int_tme"),
    "+infl +int_tme"            = c(P, base, "L_inflation", "L_int_tme"))
  fits <- lapply(specs, function(r) fit_one(d, r))
  show_block(fits, c(P, "L_inflation", "L_int_tme", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "basis_pre1975"),
             paste0("1. Inflation and debt interest, with and without the projection (", label, ")"))

  cat("\n===== 2. Correlations (", label, "; pairwise complete) =====\n", sep = "")
  vv <- intersect(c("L_inflation", "L_int_tme", "L_ln_debt_gdp", "L_mig_rate", "delta_oadr_10", "L_tax_workpop", "L_unem"), names(d))
  print(round(cor(d[, vv], use = "pairwise.complete.obs"), 2))
  cat("Range of L_int_tme:", paste(round(range(d$L_int_tme, na.rm = TRUE), 2), collapse = " to "),
      "| of L_inflation:", paste(round(range(d$L_inflation, na.rm = TRUE), 2), collapse = " to "), "\n")

  if (all(c("mig_gross", "L_mig_gross") %in% names(d))) {
    baseg <- sub("L_mig_rate", "L_mig_gross", base)
    fg <- list("gross, no proj" = fit_one(d, baseg, "mig_gross"),
               "gross"          = fit_one(d, c(P, baseg), "mig_gross"),
               "gross +int_tme, no proj" = fit_one(d, c(baseg, "L_int_tme"), "mig_gross"),
               "gross +int_tme" = fit_one(d, c(P, baseg, "L_int_tme"), "mig_gross"))
    show_block(fg, c(P, "L_int_tme", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_gross", "basis_pre1975"),
               paste0("3. Gross inflows with debt interest (", label, ")"))
  }
  if (sample == "1977" && all(c("eu_rate", "L_eu_rate") %in% names(d))) {
    basee <- c("L_eu_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")
    fe <- list("eu_rate" = fit_one(d, c(P, basee), "eu_rate"),
               "eu_rate +int_tme" = fit_one(d, c(P, basee, "L_int_tme"), "eu_rate"),
               "eu_rate, int_tme no proj" = fit_one(d, c(basee, "L_int_tme"), "eu_rate"),
               "eu_rate +inflation" = fit_one(d, c(P, basee, "L_inflation"), "eu_rate"))
    show_block(fe, c(P, "L_int_tme", "L_inflation", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_eu_rate"),
               "4. The EU placebo with debt interest and with inflation (core sample, 1977-2019)")
    cat("A policy-pressure variable should not move the uncontrolled EU flow; a general determinant should.\n")
  }
}
# ---- 5. Fiscal candidate screen: does any fiscal variable improve the model on its own AND with the projection?
# Sam's criterion (28 Sep): a missing fiscal variable should (a) improve the no-projection fiscal model's
# ability to determine migration on its own and (b) still improve the model once the projection is added.
# Each candidate is judged on its own complete cases: the four models are fitted on the same rows.
for (sample in c("1964", "1977")) {
  d <- load_df(sample)
  label <- if (sample == "1964") "extended sample, 1965-2019" else "core sample, 1977-2019"
  base <- c("L_mig_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", if (sample == "1964") "basis_pre1975")
  P <- "delta_oadr_10"
  cands <- c("L_int_tme", "L_int_gdp", "L_gilt", "L_r_g", "L_real_rate", "L_tbill",
             "L_psnb_gdp", "L_psnd_gdp", "L_tax_gdp", "L_grants_gdp", "L_grants_wap", "L_tme_gdp",
             "fc_budget", "fc_planned", "fc_medium", "gad_earn", "gad_policy", "L_inflation")
  cands <- cands[cands %in% names(d)]
  cat("\n===== 5. Fiscal candidate screen (", label, ") =====\n", sep = "")
  cat(sprintf("%-14s %3s | %8s %6s %7s %7s | %8s %6s %7s %9s %6s\n", "candidate", "N",
              "no proj", "p", "dAIC", "RESETp", "+proj", "p", "dAIC", "proj coef", "p"))
  cat("(no proj: candidate added to the fiscal model without the projection, dAIC vs that model on the same rows;",
      "+proj: added beside the projection, dAIC vs the baseline on the same rows)\n")
  for (cv in cands) {
    dd <- d[complete.cases(d[, c("mig_rate", base, P, cv)]), ]
    if (nrow(dd) < 20) next
    f0 <- fit_one(dd, base); f1 <- fit_one(dd, c(base, cv)); g0 <- fit_one(dd, c(P, base)); g1 <- fit_one(dd, c(P, base, cv))
    pf <- function(x) if (x < .001) "<.001" else sprintf("%.3f", x)
    cat(sprintf("%-14s %3d | %8.4f %6s %7.2f %7.3f | %8.4f %6s %7.2f %9.4f %6s\n", cv, f1$n,
                f1$ct[cv, 1], pf(f1$ct[cv, 4]), f1$aic - f0$aic, f1$reset,
                g1$ct[cv, 1], pf(g1$ct[cv, 4]), g1$aic - g0$aic, g1$ct[P, 1], pf(g1$ct[P, 4])))
  }
}
cat("\nWritten to", normalizePath(out), "\n")
