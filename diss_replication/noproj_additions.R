# =============================================================================
# noproj_additions.R (Sam, 30 Sep 2026): with the one-term burden (taxes + debt interest per working-age person, taxint_wap) the
# specification has one parameter fewer. Which single addition from the dataset (a) makes the NO-projection model better and the burden
# term significant there, and (b) also improves the model WITH the projection while leaving the projection intact? Sam's two-part
# criterion, applied on both samples.
# Base without the projection: mig_gross ~ L_mig_gross + L_taxint_wap + L_ln_debt_gdp + L_ln_vac_unem; with: + delta_oadr_10.
# Pool: the other dataset variables lagged one year, in level and in log where positive (st_inf_exp contemporaneous); the projection
# family, tax-per-head variants and migration flows excluded. Pairs among the ten best single additions of the no-projection model.
# Reported for each candidate: no-projection model dAIC, burden coefficient and p, addition p; with-projection model dAIC, projection
# coefficient and p, addition p; PASS = addition p < .10 in both models, dAIC < 0 in both, burden p < .05 without the projection.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp
pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
pool_base <- c("growth", "growth_pc", "inflation", "cpi_infl", "wage_infl", "real_wage", "gilt", "tbill", "bank_rate", "term_spread", "real_rate", "r_minus_g", "int_tme", "int_gdp", "tax_gdp", "psnb_gdp", "gap_obr",
               "real_house", "real_eri", "ca_gdp", "trade_bal_gdp", "openness", "particip", "m4_growth", "labour_share", "tot", "lab_prod", "tfp_growth", "union_density", "days_lost_k", "vac_wap", "unem",
               "grants_gdp", "replacement_ratio", "cg_social_benefits_gdp", "lt_inf_exp", "elec_year", "pre_elec", "labour_gov", "youth", "relinc_rich", "tax_ceiling", "tme_real_growth", "tme_obr_gdp", "psnd_gdp")
ALL <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$L_taxint_wap <- lag1(S$tax_workpop * (1 + S$int_gdp_x / S$tax_gdp))
  pool <- list(); for (v in intersect(pool_base, names(S))) { x <- lag1(S[[v]]); pool[[paste0("L_", v)]] <- x; if (all(x[!is.na(x)] > 0) && !(v %in% c("elec_year", "pre_elec", "labour_gov"))) pool[[paste0("L_ln_", v)]] <- log(x) }
  if ("st_inf_exp" %in% names(S)) pool[["st_inf_exp"]] <- S$st_inf_exp; for (nm in names(pool)) S[[nm]] <- pool[[nm]]
  B0 <- c("L_mig_gross", "L_taxint_wap", "L_ln_debt_gdp", "L_ln_vac_unem"); B1 <- c(B0, "delta_oadr_10"); F <- function(xs) fit(S, xs, OUTC)
  m0 <- F(B0); m1 <- F(B1); yrs <- attr(m0, "dd")$year; c0 <- coeftest(m0, vcov. = vcovHAC(m0)); c1 <- coeftest(m1, vcov. = vcovHAC(m1))
  cat(sprintf("   base without the projection: AIC %.2f, taxint_wap %.4f (p %.3f), RESET %.3f | with: AIC %.2f, projection %.4f (p %.4f), taxint_wap %.4f (p %.3f)\n", AIC(m0), c0["L_taxint_wap", 1], c0["L_taxint_wap", 4], resettest(m0)$p.value, AIC(m1), c1["delta_oadr_10", 1], c1["delta_oadr_10", 4], c1["L_taxint_wap", 1], c1["L_taxint_wap", 4]))
  ok <- vapply(names(pool), function(v) all(!is.na(S[[v]][S$year %in% yrs])) && sd(S[[v]][S$year %in% yrs]) > 0, logical(1)); cands <- names(pool)[ok]
  one <- function(add) { a0 <- F(c(B0, add)); a1 <- F(c(B1, add)); if (is.null(a0) || is.null(a1) || nobs(a0) != nobs(m0) || nobs(a1) != nobs(m1)) return(NULL); t0 <- coeftest(a0, vcov. = vcovHAC(a0)); t1 <- coeftest(a1, vcov. = vcovHAC(a1))
    data.frame(add = paste(sub("^L_", "", add), collapse = " + "), dAIC0 = AIC(a0) - AIC(m0), burden0 = t0["L_taxint_wap", 1], burden0_p = t0["L_taxint_wap", 4], add0_p = max(t0[add, 4]), RESET0 = resettest(a0)$p.value,
               dAIC1 = AIC(a1) - AIC(m1), proj1 = t1["delta_oadr_10", 1], proj1_p = t1["delta_oadr_10", 4], add1_p = max(t1[add, 4]), burden1_p = t1["L_taxint_wap", 4], stringsAsFactors = FALSE) }
  R1 <- do.call(rbind, lapply(cands, one)); R1 <- R1[order(R1$dAIC0), ]; top <- cands[match(R1$add[seq_len(min(10, nrow(R1)))], sub("^L_", "", cands))]
  R2 <- do.call(rbind, lapply(combn(top, 2, simplify = FALSE), one)); R <- rbind(R1, R2); R$pass <- with(R, add0_p < .10 & add1_p < .10 & dAIC0 < 0 & dAIC1 < 0 & burden0_p < .05); R <- R[order(!R$pass, R$dAIC0), ]
  cat(sprintf("   %d single and %d paired additions tested; %d pass Sam's criterion (addition p < .10 and dAIC < 0 in BOTH models, burden p < .05 without the projection). Best twenty by no-projection dAIC (passing first):\n", nrow(R1), nrow(R2), sum(R$pass)))
  print(head(R, 20), row.names = FALSE, digits = 3); R$sample <- s; ALL[[s]] <- R }
W <- merge(ALL[["1964"]], ALL[["1977"]], by = "add", suffixes = c("_cit", "_orig")); W <- W[W$pass_cit & W$pass_orig, ]
cat("\n\n==================== Additions passing the criterion on BOTH samples ====================\n"); if (nrow(W)) print(W[order(W$dAIC0_cit + W$dAIC0_orig), c("add", "dAIC0_cit", "burden0_p_cit", "add0_p_cit", "dAIC1_cit", "proj1_cit", "proj1_p_cit", "dAIC0_orig", "burden0_p_orig", "add0_p_orig", "dAIC1_orig", "proj1_orig", "proj1_p_orig")], row.names = FALSE, digits = 3) else cat("   none\n")
cat("\nDone.\n")
