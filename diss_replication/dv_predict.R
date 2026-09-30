# =============================================================================
# dv_predict.R -- Predicting the outcome (gross non-EU inflows, log persons) with the projection and the tax burden held in fixed form.
# (Sam, 30 Sep 2026.) Same engine as tme_predict.R step 3.
# Fixed terms: L_mig_gross (own lag), delta_oadr_10 (projection) and L_tax_workpop (tax per working-age person, level, t-1).
# Block 1: debt/GDP and unemployment, each in any form (level, log, first difference, log difference; all lagged one year; the log
#          difference of debt/GDP is the core specification's ln_debt_gdp), plus one or two further terms from the pool.
# Block 2: unemployment alone in any form, plus one or two further terms from the pool.
# Pool: the other dataset variables lagged one year in level and in log where positive (st_inf_exp contemporaneous, by Sam's rule),
#       four ratios (debt service / tax, benefits / tax, real rate x debt, (r-g) x debt) and pairwise interactions of the block
#       variables with each other and with every pool term. Excluded from the pool: the projection family (other horizons, levels,
#       total DR, current ratio), the other tax-per-head measures, and migration flows (British, EU, total, permits, emigration).
# Step 1 picks the block variables' best forms by AIC (top three printed); step 2 adds pool terms to the best-form block and prints
# the three best models by AIC among those whose added terms are each significant at 10% (HAC), max VIF of the block and added terms
# < 20 (the fixed terms, whose own VIF is printed once, are exempt: the lagged outcome and the tax level are collinear by construction), same observations.
# The projection's coefficient and HAC p-value are printed for every model. Reference line: Sam's core specification.
# RStudio: working directory = diss_replication/, then source() this file. Needs the latest workbook (git pull).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
TOP <- 3; PMAX <- 0.10; VIFMAX <- 20; DV <- "mig_gross"
FIX <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop")
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R")
  need <- c("mig_gross", "psnd_gdp", "unem", "tax_workpop", "gap_obr", "term_spread", "int_tme", "vac_unem")
  miss <- setdiff(need, names(df)); if (length(miss)) stop("columns missing from sheet data_", s, ": ", paste(miss, collapse = ", "), ". Pull the latest Dissertation_Dataset_Final.xlsx (git pull) and run again; do not rebuild from CSVs.")
  S <- subset(df, year >= Y0 - 1 & year <= 2019); cat("\n\n==================== ", LABS[[s]], " ====================\n", sep = "")
  forms <- function(x, base) { out <- list(); out[[base]] <- lag1(x); out[[paste0("ln_", base)]] <- lag1(log(x)); out[[paste0("d_", base)]] <- lag1(c(NA, diff(x))); out[[paste0("dln_", base)]] <- lag1(c(NA, diff(log(x)))); out }
  F1 <- forms(S$psnd_gdp, "debt_gdp"); F2 <- forms(S$unem, "unem"); for (nm in names(c(F1, F2))) S[[paste0("L_", nm)]] <- c(F1, F2)[[nm]]
  blocks <- list("debt/GDP + unemployment" = c("debt_gdp", "unem"), "unemployment only" = "unem")
  hacline <- function(m, xs) { ct <- coeftest(m, vcov. = vcovHAC(m)); paste(sprintf("%s %.4f (p %.3f)", sub("^L_", "", xs), ct[xs, 1], ct[xs, 4]), collapse = "; ") }
  projline <- function(m) { ct <- coeftest(m, vcov. = vcovHAC(m)); sprintf("proj %.4f (p %.3f)", ct["delta_oadr_10", 1], ct["delta_oadr_10", 4]) }
  maxvif <- function(m, keep) if (length(coef(m)) > 2) { v <- vif(m); max(v[setdiff(names(v), keep)]) } else NA   # VIF over the block and added terms only; the fixed terms are reported once
  m_core <- fit(S, c(FIX, "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme"), DV)
  cat(sprintf("\nReference: core specification (fixed terms + ln_debt_gdp + ln_vac_unem + ln_int_tme): AIC %.2f, adjR2 %.3f, N %d, %s\n", AIC(m_core), summary(m_core)$adj.r.squared, nobs(m_core), projline(m_core)))
  cat("Fixed in every model: L_mig_gross (own lag), delta_oadr_10 (projection), L_tax_workpop (tax per WAP, level, t-1).\n")
  # --- step 1: forms of the block variables ---
  cat("\n1. Forms of the block variables (level, log, first difference, log difference; all lagged one year; dln_debt_gdp = the core specification's ln_debt_gdp)\n")
  best <- list()
  for (bn in names(blocks)) { vars <- blocks[[bn]]; grid <- expand.grid(lapply(vars, function(v) paste0("L_", c("", "ln_", "d_", "dln_"), v)), stringsAsFactors = FALSE); res <- NULL
    for (i in seq_len(nrow(grid))) { xs <- c(FIX, unlist(grid[i, ])); m <- fit(S, xs, DV); if (is.null(m)) next
      res <- rbind(res, data.frame(spec = paste(sub("^L_", "", unlist(grid[i, ])), collapse = " + "), AIC = AIC(m), adjR2 = summary(m)$adj.r.squared, N = nobs(m), RESET = resettest(m)$p.value, proj = projline(m), coefs = hacline(m, unlist(grid[i, ])), xs = paste(unlist(grid[i, ]), collapse = ","), stringsAsFactors = FALSE)) }
    res <- res[order(res$AIC), ]; best[[bn]] <- strsplit(res$xs[1], ",")[[1]]
    cat(sprintf("   block: %s | %d models fitted; best three:\n", bn, nrow(res)))
    for (i in seq_len(min(TOP, nrow(res)))) cat(sprintf("     %d. %-28s AIC %8.2f  adjR2 %.3f  RESET %.3f  N %d  | %s | %s\n", i, res$spec[i], res$AIC[i], res$adjR2[i], res$RESET[i], res$N[i], res$proj[i], res$coefs[i])) }
  # --- step 2: additions from the pool ---
  pool_base <- c("growth", "growth_pc", "inflation", "cpi_infl", "wage_infl", "real_wage", "gilt", "tbill", "bank_rate", "term_spread", "real_rate", "r_minus_g", "int_tme", "int_gdp",
                 "tax_gdp", "psnb_gdp", "gap_obr", "real_house", "real_eri", "ca_gdp", "trade_bal_gdp", "openness", "particip", "m4_growth", "labour_share", "tot", "lab_prod", "tfp_growth",
                 "union_density", "days_lost_k", "vac_wap", "vac_unem", "grants_gdp", "replacement_ratio", "cg_social_benefits_gdp", "lt_inf_exp", "elec_year", "pre_elec", "labour_gov",
                 "youth", "relinc_rich", "tax_ceiling", "tme_real_growth", "tme_obr_gdp")
  pool_base <- intersect(pool_base, names(S))
  S$debt_service_tax <- S$int_gdp / S$tax_gdp; S$benefits_tax <- S$cg_social_benefits_gdp / S$tax_gdp; S$rdebt <- S$real_rate * S$psnd_gdp / 100; S$rg_debt <- S$r_minus_g * S$psnd_gdp / 100
  ratios <- intersect(c("debt_service_tax", "benefits_tax", "rdebt", "rg_debt"), names(S))
  DEFS3 <- c(debt_service_tax = "debt interest / GDP divided by taxes / GDP (debt service as a share of tax revenue)", benefits_tax = "central government social benefits / GDP divided by taxes / GDP", rdebt = "real interest rate x net debt / GDP (real debt-service pressure, % of GDP)", rg_debt = "(r - g) x net debt / GDP (the debt-dynamics term, % of GDP)", labour_gov = "Labour government in office, 0/1", elec_year = "general election held in the year, 0/1", pre_elec = "year before a general election, 0/1", gap_obr = "output gap, % of potential GDP (OBR public sector finances databank, fiscal years)", st_inf_exp = "short-term inflation expectations, %, start of year, contemporaneous (NIESR)", tme_obr_gdp = "total managed expenditure, % of GDP (OBR databank, fiscal year)")
  pool <- list(); nodum <- c("elec_year", "pre_elec", "labour_gov")
  for (v in c(pool_base, ratios)) { x <- lag1(S[[v]]); pool[[paste0("L_", v)]] <- x; if (all(x[!is.na(x)] > 0) && !(v %in% nodum)) pool[[paste0("L_ln_", v)]] <- log(x) }
  if ("st_inf_exp" %in% names(S)) pool[["st_inf_exp"]] <- S$st_inf_exp
  for (nm in names(pool)) S[[nm]] <- pool[[nm]]
  cat("\n2. Block variables at their best forms plus one or two further terms from the pool\n")
  shown_defs <- character(0)
  for (bn in names(blocks)) { bx <- best[[bn]]; base_xs <- c(FIX, bx); m0 <- fit(S, base_xs, DV); yrs <- attr(m0, "dd")$year; v0 <- vif(m0); cat(sprintf("   (VIF of the fixed terms in the base model: %s)\n", paste(sprintf("%s %.1f", names(v0)[names(v0) %in% FIX], v0[names(v0) %in% FIX]), collapse = ", ")))
    inter <- list(); if (length(bx) > 1) for (i in 1:(length(bx) - 1)) for (j in (i + 1):length(bx)) { nm <- paste0(bx[i], "_x_", bx[j]); S[[nm]] <- S[[bx[i]]] * S[[bx[j]]]; inter[[nm]] <- TRUE }
    for (b in bx) for (pn in names(pool)) { nm <- paste0(b, "_x_", pn); S[[nm]] <- S[[b]] * S[[pn]]; inter[[nm]] <- TRUE }
    cands <- c(names(pool), names(inter)); ok <- vapply(cands, function(v) all(!is.na(S[[v]][S$year %in% yrs])) && sd(S[[v]][S$year %in% yrs]) > 0, logical(1)); dropped <- cands[!ok]; cands <- cands[ok]
    lab <- function(a) paste(sub("^L_", "", gsub("_x_L_", " x ", a)), collapse = " + ")
    one <- function(add) { m <- fit(S, c(base_xs, add), DV); if (is.null(m) || nobs(m) != nobs(m0)) return(NULL); ct <- coeftest(m, vcov. = vcovHAC(m)); if (any(ct[add, 4] > PMAX)) return(NULL); vf <- maxvif(m, FIX); if (!is.na(vf) && vf > VIFMAX) return(NULL)
      data.frame(spec = lab(add), AIC = AIC(m), dAIC = AIC(m) - AIC(m0), adjR2 = summary(m)$adj.r.squared, N = nobs(m), RESET = resettest(m)$p.value, BG1 = bgtest(m, 1)$p.value, VIF = vf, proj = projline(m), coefs = hacline(m, c(bx, add)), stringsAsFactors = FALSE) }
    res1 <- do.call(rbind, lapply(cands, one)); if (is.null(res1)) { cat(sprintf("   block: %s | no eligible single addition\n", bn)); next }
    res1 <- res1[order(res1$AIC), ]; top1 <- cands[match(res1$spec, vapply(cands, lab, character(1)))][seq_len(min(20, nrow(res1)))]
    res2 <- NULL; if (length(top1) > 1) res2 <- do.call(rbind, lapply(combn(top1, 2, simplify = FALSE), one))
    res <- rbind(res1, res2); res <- res[order(res$AIC), ]
    cat(sprintf("   block: %s | base %s: AIC %.2f, adjR2 %.3f, N %d, %s | %d eligible candidates (%d main terms, %d interactions; %d dropped for coverage), %d single and %d paired additions pass (each added term HAC p < %.2f, max VIF of the block and added terms < %d); best three:\n",
                bn, paste(sub("^L_", "", bx), collapse = " + "), AIC(m0), summary(m0)$adj.r.squared, nobs(m0), projline(m0), length(cands), sum(cands %in% names(pool)), sum(cands %in% names(inter)), length(dropped), nrow(res1), if (is.null(res2)) 0L else nrow(res2), PMAX, VIFMAX))
    for (i in seq_len(min(TOP, nrow(res)))) { cat(sprintf("     %d. + %-48s AIC %8.2f (dAIC %+.1f)  adjR2 %.3f  RESET %.3f  BG(1) %.3f  VIF %.1f  N %d  | %s\n        %s\n", i, res$spec[i], res$AIC[i], res$dAIC[i], res$adjR2[i], res$RESET[i], res$BG1[i], res$VIF[i], res$N[i], res$proj[i], res$coefs[i]))
      shown_defs <- union(shown_defs, unlist(strsplit(gsub(" x ", " + ", res$spec[i]), " \\+ "))) }
    S[names(inter)] <- NULL }
  shown_defs <- setdiff(shown_defs, c("debt_gdp", "unem", paste0(c("ln_", "d_", "dln_"), rep(c("debt_gdp", "unem"), each = 3))))
  if (length(shown_defs)) { cat("\n   Definitions of the added terms shown (all lagged one year except st_inf_exp; ln_ = log):\n")
    for (v in sort(shown_defs)) { b <- sub("^ln_", "", v); d <- if (b %in% names(DEFS3)) DEFS3[[b]] else if (b %in% names(DEFS) && DEFS[[b]] != b) DEFS[[b]] else if (paste0("L_", b) %in% names(DEFS)) sub(", t-1$", "", DEFS[[paste0("L_", b)]]) else "see data_common.R"; cat(sprintf("     %-24s %s\n", v, d)) } }
  cat("   debt_gdp = public sector net debt / GDP, %; unem = ILO unemployment rate, %; prefixes ln_ log, d_ first difference, dln_ log difference; all lagged one year.\n") }
cat("\nDone.\n")
