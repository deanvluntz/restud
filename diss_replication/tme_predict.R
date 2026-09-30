# =============================================================================
# tme_predict.R -- Is the spending side predictable from the model's own regressors? (Sam, 29 Sep 2026; extended at his request.)
# 1  autocorrelation of total managed expenditure (OBR databank, fiscal years) as a share of GDP and in 2024-25 prices (log level and
#    growth), and of the debt-interest share, at horizons 1-10.
# 2  BLOCK MODELS: TME one year ahead from 1-year lags of debt/GDP, unemployment and tax per working-age person, each in any form
#    (level, log, first difference, log difference); three outcomes (growth of real TME; change in TME/GDP; log TME/GDP with its lag);
#    three blocks (all three variables; unemployment and tax; tax alone). The best three form combinations by AIC.
# 3  BLOCK MODELS PLUS OTHER VARIABLES IN THE DATASET: for each outcome and block, the block variables in their best forms from step 2
#    plus one or two further terms drawn from a pool of (a) the other dataset variables, lagged one year, in level and in log where
#    positive, (b) ratios built from them (debt service / tax, benefits / tax, debt interest / GDP, real rate x debt), and (c) pairwise
#    interactions of the block variables and of each block variable with the pool. The best three models by AIC among those whose
#    added terms are each significant at 10% (HAC) and whose largest VIF is below 20. Only candidates observed for every year of the
#    block model's sample are eligible, so every AIC compares the same observations. Both samples.
# RStudio: working directory = diss_replication/, then source() this file. Needs the workbook sheets data_1964 / data_1977 from the
# latest commit (columns tme_obr_gdp, tme_obr_real, gap_obr, term_spread); pull before running.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
TOP <- 3; PMAX <- 0.10; VIFMAX <- 20
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R")
  need <- c("tme_obr_gdp", "tme_obr_real", "int_tme", "psnd_gdp", "unem", "tax_workpop", "gap_obr", "term_spread")
  miss <- setdiff(need, names(df)); if (length(miss)) stop("columns missing from sheet data_", s, ": ", paste(miss, collapse = ", "), ". Pull the latest Dissertation_Dataset_Final.xlsx (git pull) and run again; do not rebuild from CSVs.")
  S <- subset(df, year >= Y0 - 1 & year <= 2019); cat("\n\n==================== ", LABS[[s]], " ====================\n", sep = "")
  # --- 1 autocorrelation ---
  cat("\n1. Autocorrelation of spending measures (lags 1-10)\n")
  ac <- function(x, nm) { x <- x[!is.na(x)]; a <- acf(x, lag.max = 10, plot = FALSE)$acf[-1]; cat(sprintf("   %-34s %s\n", nm, paste(sprintf("%5.2f", a), collapse = " "))) }
  ac(S$tme_obr_gdp, "TME / GDP (level)"); ac(diff(S$tme_obr_gdp), "change in TME / GDP"); ac(log(S$tme_obr_real), "log real TME (level)"); ac(diff(log(S$tme_obr_real)), "growth of real TME"); ac(S$int_tme, "debt interest / TME (level)"); ac(diff(log(S$int_tme)), "change in log debt interest / TME")
  cat("   (lags 1..10 in order; TME from the OBR public sector finances databank, fiscal years mapped to their starting calendar year)\n")
  # --- outcomes and block-variable forms ---
  S$g_tme <- 100 * c(NA, diff(log(S$tme_obr_real))); S$d_tme_gdp <- c(NA, diff(S$tme_obr_gdp)); S$ln_tme_gdp <- log(S$tme_obr_gdp); S$L_ln_tme_gdp <- lag1(S$ln_tme_gdp)
  forms <- function(x, base) { out <- list(); out[[base]] <- lag1(x); out[[paste0("ln_", base)]] <- lag1(log(x)); out[[paste0("d_", base)]] <- lag1(c(NA, diff(x))); out[[paste0("dln_", base)]] <- lag1(c(NA, diff(log(x)))); out }
  F1 <- forms(S$psnd_gdp, "debt_gdp"); F2 <- forms(S$unem, "unem"); F3 <- forms(S$tax_workpop, "tax_workpop"); for (nm in names(c(F1, F2, F3))) S[[paste0("L_", nm)]] <- c(F1, F2, F3)[[nm]]
  DEFS2 <- c(debt_gdp = "public sector net debt / GDP, %", unem = "ILO unemployment rate, %", tax_workpop = "national accounts taxes per working-age person, GBP 000 at 2024-25 prices")
  blocks <- list("debt/GDP + unemployment + tax per WAP" = c("debt_gdp", "unem", "tax_workpop"), "unemployment + tax per WAP" = c("unem", "tax_workpop"), "tax per WAP only" = "tax_workpop")
  outcomes <- list(A = list(dv = "g_tme", extra = NULL, lab = "growth of real TME, % (g_tme)"), B = list(dv = "d_tme_gdp", extra = NULL, lab = "change in TME / GDP, percentage points (d_tme_gdp)"), C = list(dv = "ln_tme_gdp", extra = "L_ln_tme_gdp", lab = "log TME / GDP with its own lag (ln_tme_gdp)"))
  hacline <- function(m, xs) { ct <- coeftest(m, vcov. = vcovHAC(m)); paste(sprintf("%s %.3f (p %.3f)", sub("^L_", "", xs), ct[xs, 1], ct[xs, 4]), collapse = "; ") }
  maxvif <- function(m, keep) if (length(coef(m)) > 2) { v <- vif(m); max(v[setdiff(names(v), keep)]) } else NA   # VIF over the block and added terms only; the fixed terms are reported once
  # --- 2 block models ---
  cat("\n2. Predicting spending one year ahead from 1-year lags of the block variables (each in level, log, first difference or log difference; all combinations)\n")
  cat("   regressors: debt_gdp =", DEFS2[["debt_gdp"]], "; unem =", DEFS2[["unem"]], "; tax_workpop =", DEFS2[["tax_workpop"]], "; prefixes: ln_ log, d_ first difference, dln_ log difference; all lagged one year\n")
  best <- list()
  for (o in names(outcomes)) { dv <- outcomes[[o]]$dv; extra <- outcomes[[o]]$extra; cat(sprintf("\n   Outcome %s: %s\n", o, outcomes[[o]]$lab))
    for (bn in names(blocks)) { vars <- blocks[[bn]]; grid <- expand.grid(lapply(vars, function(v) paste0("L_", c("", "ln_", "d_", "dln_"), v)), stringsAsFactors = FALSE); res <- NULL
      for (i in seq_len(nrow(grid))) { xs <- c(extra, unlist(grid[i, ])); m <- fit(S, xs, dv); if (is.null(m)) next
        res <- rbind(res, data.frame(spec = paste(sub("^L_", "", unlist(grid[i, ])), collapse = " + "), AIC = AIC(m), adjR2 = summary(m)$adj.r.squared, N = nobs(m), RESET = resettest(m)$p.value, coefs = hacline(m, unlist(grid[i, ])), xs = paste(unlist(grid[i, ]), collapse = ","), stringsAsFactors = FALSE)) }
      res <- res[order(res$AIC), ]; best[[paste(o, bn)]] <- strsplit(res$xs[1], ",")[[1]]
      cat(sprintf("     block: %s | %d models fitted; best three:\n", bn, nrow(res)))
      for (i in seq_len(min(TOP, nrow(res)))) cat(sprintf("       %d. %-45s AIC %8.2f  adjR2 %.3f  RESET %.3f  N %d  | %s\n", i, res$spec[i], res$AIC[i], res$adjR2[i], res$RESET[i], res$N[i], res$coefs[i])) } }
  # --- 3 block models plus other dataset variables ---
  cat("\n3. Block models plus other variables in the dataset (block variables held at their best forms from step 2; one or two added terms)\n")
  pool_base <- c("growth", "growth_pc", "inflation", "cpi_infl", "wage_infl", "real_wage", "gilt", "tbill", "bank_rate", "term_spread", "real_rate", "r_minus_g", "int_tme", "int_gdp",
                 "tax_gdp", "psnb_gdp", "gap_obr", "real_house", "real_eri", "ca_gdp", "trade_bal_gdp", "openness", "particip", "m4_growth", "labour_share", "tot", "lab_prod", "tfp_growth",
                 "union_density", "days_lost_k", "vac_wap", "vac_unem", "oadr_0", "delta_oadr_10", "level_oadr_10", "grants_gdp", "replacement_ratio", "cg_social_benefits_gdp", "st_inf_exp", "lt_inf_exp",
                 "elec_year", "pre_elec", "labour_gov", "youth", "tax_ceiling", "tme_real_growth")
  pool_base <- intersect(pool_base, names(S))
  # ratios and products built from the dataset (all lagged one year)
  S$debt_service_tax <- S$int_gdp / S$tax_gdp; S$benefits_tax <- S$cg_social_benefits_gdp / S$tax_gdp; S$rdebt <- S$real_rate * S$psnd_gdp / 100; S$rg_debt <- S$r_minus_g * S$psnd_gdp / 100
  ratios <- intersect(c("debt_service_tax", "benefits_tax", "rdebt", "rg_debt"), names(S))
  DEFS3 <- c(debt_service_tax = "debt interest / GDP divided by taxes / GDP (debt service as a share of tax revenue)", benefits_tax = "central government social benefits / GDP divided by taxes / GDP", rdebt = "real interest rate x net debt / GDP (real debt-service pressure, % of GDP)", rg_debt = "(r - g) x net debt / GDP (the debt-dynamics term, % of GDP)", labour_gov = "Labour government in office, 0/1", elec_year = "general election held in the year, 0/1", pre_elec = "year before a general election, 0/1", gap_obr = "output gap, % of potential GDP (OBR public sector finances databank, fiscal years)")
  pool <- list()
  for (v in c(pool_base, ratios)) { x <- lag1(S[[v]]); pool[[paste0("L_", v)]] <- x; if (all(x[!is.na(x)] > 0) && !(v %in% c("elec_year", "pre_elec", "labour_gov"))) pool[[paste0("L_ln_", v)]] <- log(x) }
  for (nm in names(pool)) S[[nm]] <- pool[[nm]]
  shown_defs <- character(0)
  for (o in names(outcomes)) { dv <- outcomes[[o]]$dv; extra <- outcomes[[o]]$extra; cat(sprintf("\n   Outcome %s: %s\n", o, outcomes[[o]]$lab))
    for (bn in names(blocks)) { bx <- best[[paste(o, bn)]]; base_xs <- c(extra, bx); m0 <- fit(S, base_xs, dv); if (is.null(m0)) next; yrs <- attr(m0, "dd")$year
      # interactions of block variables (pairwise) and of each block variable with each pool term
      inter <- list(); if (length(bx) > 1) for (i in 1:(length(bx) - 1)) for (j in (i + 1):length(bx)) { nm <- paste0(bx[i], "_x_", bx[j]); S[[nm]] <- S[[bx[i]]] * S[[bx[j]]]; inter[[nm]] <- TRUE }
      for (b in bx) for (pn in names(pool)) { nm <- paste0(b, "_x_", pn); S[[nm]] <- S[[b]] * S[[pn]]; inter[[nm]] <- TRUE }
      cands <- c(names(pool), names(inter)); ok <- vapply(cands, function(v) all(!is.na(S[[v]][S$year %in% yrs])) && sd(S[[v]][S$year %in% yrs]) > 0, logical(1)); dropped <- cands[!ok]; cands <- cands[ok]
      one <- function(add) { m <- fit(S, c(base_xs, add), dv); if (is.null(m) || nobs(m) != nobs(m0)) return(NULL); ct <- coeftest(m, vcov. = vcovHAC(m)); if (any(ct[add, 4] > PMAX)) return(NULL); vf <- maxvif(m, extra); if (!is.na(vf) && vf > VIFMAX) return(NULL)
        data.frame(spec = paste(sub("^L_", "", gsub("_x_L_", " x ", add)), collapse = " + "), AIC = AIC(m), dAIC = AIC(m) - AIC(m0), adjR2 = summary(m)$adj.r.squared, N = nobs(m), RESET = resettest(m)$p.value, VIF = vf, coefs = hacline(m, c(bx, add)), stringsAsFactors = FALSE) }
      res1 <- do.call(rbind, lapply(cands, one)); if (is.null(res1)) { cat(sprintf("     block: %s | no eligible single addition\n", bn)); next }
      res1 <- res1[order(res1$AIC), ]
      # pairs among the twenty best single additions (by AIC), main-effect pool terms and interactions alike
      top1 <- cands[match(res1$spec, vapply(cands, function(a) paste(sub("^L_", "", gsub("_x_L_", " x ", a)), collapse = " + "), character(1)))][seq_len(min(20, nrow(res1)))]
      res2 <- NULL; if (length(top1) > 1) res2 <- do.call(rbind, lapply(combn(top1, 2, simplify = FALSE), one))
      res <- rbind(res1, res2); res <- res[order(res$AIC), ]
      cat(sprintf("     block: %s | base %s: AIC %.2f, adjR2 %.3f, N %d | %d eligible candidates (%d main terms, %d interactions; %d dropped for coverage), %d single and %d paired additions pass (each added term HAC p < %.2f, max VIF of the block and added terms < %d); best three:\n",
                  bn, paste(sub("^L_", "", bx), collapse = " + "), AIC(m0), summary(m0)$adj.r.squared, nobs(m0), length(cands), sum(cands %in% names(pool)), sum(cands %in% names(inter)), length(dropped), nrow(res1), if (is.null(res2)) 0L else nrow(res2), PMAX, VIFMAX))
      for (i in seq_len(min(TOP, nrow(res)))) { cat(sprintf("       %d. + %-52s AIC %8.2f (dAIC %+.1f)  adjR2 %.3f  RESET %.3f  VIF %.1f  N %d\n          %s\n", i, res$spec[i], res$AIC[i], res$dAIC[i], res$adjR2[i], res$RESET[i], res$VIF[i], res$N[i], res$coefs[i]))
        shown_defs <- union(shown_defs, unlist(strsplit(gsub(" x ", " + ", res$spec[i]), " \\+ "))) }
      # clean up the interaction columns so the next block starts fresh
      S[names(inter)] <- NULL } }
  shown_defs <- setdiff(shown_defs, c(names(DEFS2), paste0(c("ln_", "d_", "dln_"), rep(names(DEFS2), each = 3))))
  if (length(shown_defs)) { cat("\n   Definitions of the added terms shown (all lagged one year; ln_ = log):\n")
    for (v in sort(shown_defs)) { b <- sub("^ln_", "", v); d <- if (b %in% names(DEFS3)) DEFS3[[b]] else if (b %in% names(DEFS) && DEFS[[b]] != b) DEFS[[b]] else if (paste0("L_", b) %in% names(DEFS)) sub(", t-1$", "", DEFS[[paste0("L_", b)]]) else "see data_common.R"; cat(sprintf("     %-24s %s\n", v, d)) } } }
cat("\nDone.\n")
