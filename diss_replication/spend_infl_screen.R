# =============================================================================
# spend_infl_screen.R (Sam, 30 Sep 2026): the spare parameter from the one-term burden, screened WITHOUT the AIC condition, with
# expenditure-and-inflation constructions added to the pool.
# Base without the projection: mig_gross ~ L_mig_gross + L_taxint_wap + L_ln_debt_gdp + L_ln_vac_unem; with: + delta_oadr_10.
# Relaxed criterion (PASS): the added term(s) significant at 10% (HAC) with AND without the projection; the burden significant at 5%
# without the projection; the projection significant at 1% with the addition. dAIC is reported, not required.
# Constructed expenditure / inflation measures (all t-1; nominal debt interest and TME spliced to 2019 as before):
#   tme_wap     real total managed expenditure per working-age person, GBP 000 at 2024-25 prices (OBR databank) - spending per worker,
#               the parallel of the burden per worker
#   prog_wap    real non-interest (programme) spending per working-age person, GBP 000
#   tme_tax     TME / taxes: spending per pound of tax revenue
#   tme_taxint  TME / (taxes + debt interest): spending per pound of the one-term burden
#   g_tme_real  real TME growth, % (nominal spending growth deflated by the GDP deflator)
#   infl_debt   inflation erosion of the debt, % of GDP: GDP-deflator inflation x net debt / GDP
#   adj_psnb    inflation-adjusted borrowing, % of GDP: PSNB less the inflation erosion of the debt (the inflation-adjusted or 'real' deficit)
#
#   real_int    inflation-adjusted debt interest, % of GDP: debt interest less the inflation erosion of the debt
#   real_int_tme inflation-adjusted debt interest, % of TME
#   tme_earn    nominal TME per working-age person / annual average earnings: spending per worker in years of earnings
# Section 1: each constructed measure alone (with and without the projection). Section 2: each expenditure measure paired with each
# inflation measure (deflator, CPI, wage). Section 3: the full pool, singles and pairs among the 15 best, under the relaxed criterion.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp
pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
CONS <- c("tme_wap", "prog_wap", "tme_tax", "tme_taxint", "g_tme_real", "infl_debt", "adj_psnb", "real_int", "real_int_tme", "tme_earn")
EXPS <- c("tme_wap", "prog_wap", "tme_tax", "tme_taxint", "g_tme_real", "tme_obr_gdp"); INFS <- c("inflation", "cpi_infl", "wage_infl")
pool_base <- c("growth", "growth_pc", "inflation", "cpi_infl", "wage_infl", "real_wage", "gilt", "tbill", "bank_rate", "term_spread", "real_rate", "r_minus_g", "int_tme", "int_gdp", "tax_gdp", "psnb_gdp", "gap_obr",
               "real_house", "real_eri", "ca_gdp", "trade_bal_gdp", "openness", "particip", "m4_growth", "labour_share", "tot", "lab_prod", "tfp_growth", "union_density", "days_lost_k", "vac_wap", "unem",
               "grants_gdp", "grants_wap", "replacement_ratio", "cg_social_benefits_gdp", "lt_inf_exp", "elec_year", "pre_elec", "labour_gov", "youth", "relinc_rich", "tax_ceiling", "tme_real_growth", "tme_obr_gdp", "psnd_gdp")
ALL <- list(); SEC1 <- list(); SEC2 <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$L_taxint_wap <- lag1(S$tax_workpop * (1 + S$int_gdp_x / S$tax_gdp))
  S$tme_wap <- S$tme_obr_real * 1e6 / S$workpop; S$prog_wap <- S$tme_wap * (1 - S$int_gdp_x / S$tme_obr_gdp); S$tme_tax <- S$tme_obr_gdp / S$tax_gdp; S$tme_taxint <- S$tme_obr_gdp / (S$tax_gdp + S$int_gdp_x)
  S$g_tme_real <- 100 * c(NA, diff(log(S$tme_obr_real))); S$infl_debt <- S$inflation * S$psnd_gdp / 100; S$adj_psnb <- S$psnb_gdp - S$infl_debt; S$real_int <- S$int_gdp_x - S$infl_debt
  S$real_int_tme <- 100 * S$real_int / S$tme_obr_gdp; S$tme_earn <- if ("awe_gbp" %in% names(S)) (S$tme_obr_bn * 1e6 / S$workpop) / (S$awe_gbp * 52 / 1000) else NA
  pool <- list(); for (v in intersect(c(pool_base, CONS), names(S))) { x <- lag1(S[[v]]); pool[[paste0("L_", v)]] <- x; if (all(x[!is.na(x)] > 0) && !(v %in% c("elec_year", "pre_elec", "labour_gov"))) pool[[paste0("L_ln_", v)]] <- log(x) }
  if ("st_inf_exp" %in% names(S)) pool[["st_inf_exp"]] <- S$st_inf_exp; for (nm in names(pool)) S[[nm]] <- pool[[nm]]
  B0 <- c("L_mig_gross", "L_taxint_wap", "L_ln_debt_gdp", "L_ln_vac_unem"); B1 <- c(B0, "delta_oadr_10"); F <- function(xs) fit(S, xs, OUTC)
  m0 <- F(B0); m1 <- F(B1); yrs <- attr(m0, "dd")$year
  ok <- vapply(names(pool), function(v) all(!is.na(S[[v]][S$year %in% yrs])) && sd(S[[v]][S$year %in% yrs]) > 0, logical(1)); cands <- names(pool)[ok]
  cat(sprintf("   coverage: %d of %d pool terms observed on every year of the base sample (dropped: %s)\n", sum(ok), length(ok), paste(sub("^L_", "", names(pool)[!ok]), collapse = ", ")))
  one <- function(add) { if (!all(add %in% cands)) return(NULL); a0 <- F(c(B0, add)); a1 <- F(c(B1, add)); if (is.null(a0) || is.null(a1) || nobs(a0) != nobs(m0) || nobs(a1) != nobs(m1)) return(NULL); t0 <- coeftest(a0, vcov. = vcovHAC(a0)); t1 <- coeftest(a1, vcov. = vcovHAC(a1))
    data.frame(add = paste(sub("^L_", "", add), collapse = " + "), coef0 = paste(sprintf("%.3g", t0[add, 1]), collapse = "/"), add0_p = max(t0[add, 4]), burden0 = t0["L_taxint_wap", 1], burden0_p = t0["L_taxint_wap", 4], dAIC0 = AIC(a0) - AIC(m0), RESET0 = resettest(a0)$p.value,
               coef1 = paste(sprintf("%.3g", t1[add, 1]), collapse = "/"), add1_p = max(t1[add, 4]), proj1 = t1["delta_oadr_10", 1], proj1_p = t1["delta_oadr_10", 4], dAIC1 = AIC(a1) - AIC(m1), stringsAsFactors = FALSE) }
  flag <- function(R) { R$pass <- with(R, add0_p < .10 & add1_p < .10 & burden0_p < .05 & proj1_p < .01); R }
  show <- function(R, n = 40) print(head(R[, c("add", "coef0", "add0_p", "burden0_p", "dAIC0", "coef1", "add1_p", "proj1", "proj1_p", "dAIC1", "pass")], n), row.names = FALSE, digits = 3)
  # ---- 1 constructed measures alone ----
  cat("\n1. Expenditure and inflation constructions, one at a time (level and log where positive)\n")
  R1 <- flag(do.call(rbind, lapply(intersect(c(paste0("L_", CONS), paste0("L_ln_", CONS)), names(pool)), one))); R1 <- R1[order(!R1$pass, R1$add0_p), ]; show(R1); SEC1[[s]] <- R1
  # ---- 2 expenditure x inflation pairs ----
  cat("\n2. Each expenditure measure paired with each inflation measure\n")
  ex <- intersect(c(paste0("L_", EXPS), paste0("L_ln_", EXPS)), cands); inf <- intersect(paste0("L_", INFS), cands)
  R2 <- flag(do.call(rbind, lapply(as.list(as.data.frame(t(expand.grid(ex, inf, stringsAsFactors = FALSE)), stringsAsFactors = FALSE)), one))); R2 <- R2[order(!R2$pass, pmax(R2$add0_p, R2$add1_p)), ]; show(R2, 25); SEC2[[s]] <- R2
  # ---- 3 the whole pool ----
  cat("\n3. The whole pool under the relaxed criterion (singles; pairs among the 15 best by no-projection fit)\n")
  R3s <- flag(do.call(rbind, lapply(cands, one))); R3s <- R3s[order(R3s$dAIC0), ]; top <- cands[match(R3s$add[seq_len(min(15, nrow(R3s)))], sub("^L_", "", cands))]
  R3p <- flag(do.call(rbind, lapply(combn(top, 2, simplify = FALSE), one))); R3 <- rbind(R3s, R3p); R3 <- R3[order(!R3$pass, R3$dAIC0), ]
  cat(sprintf("   %d singles and %d pairs; %d pass\n", nrow(R3s), nrow(R3p), sum(R3$pass))); show(R3[R3$pass, ], 40)
  nm <- R3[!R3$pass & R3$burden0_p < .05 & R3$add0_p < .10, ]; nm <- nm[order(nm$add1_p), ]
  cat("   closest near-misses (burden significant and addition significant without the projection; sorted by the addition's p beside the projection):\n"); show(nm, 10)
  cat(sprintf("   binding condition: of %d candidates significant without the projection with the burden at 5%%, %d stay significant beside the projection\n", sum(R3$burden0_p < .05 & R3$add0_p < .10), sum(R3$burden0_p < .05 & R3$add0_p < .10 & R3$add1_p < .10)))
  R3$sample <- s; ALL[[s]] <- R3 }
both <- function(A, B, title) { W <- merge(A, B, by = "add", suffixes = c("_cit", "_orig")); W <- W[W$pass_cit & W$pass_orig, ]; cat("\n", title, "\n", sep = "")
  if (nrow(W)) print(W[order(W$dAIC0_cit + W$dAIC0_orig), c("add", "coef1_cit", "add1_p_cit", "burden0_p_cit", "proj1_cit", "dAIC1_cit", "coef1_orig", "add1_p_orig", "burden0_p_orig", "proj1_orig", "dAIC1_orig")], row.names = FALSE, digits = 3) else cat("   none\n") }
cat("\n\n==================== Passing on BOTH samples ====================")
both(SEC1[["1964"]], SEC1[["1977"]], "Section 1 (constructions alone):"); both(SEC2[["1964"]], SEC2[["1977"]], "Section 2 (expenditure x inflation pairs):"); both(ALL[["1964"]], ALL[["1977"]], "Section 3 (whole pool):")
cat("\nDone.\n")
