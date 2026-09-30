# =============================================================================
# interest_terms.R -- Theoretically grounded measures of the interest burden and of spending, tested in the core specification on
# both samples (Sam, 30 Sep 2026: "something which captures some aspect of interest/expenditure that is logical and significant
# across both samples").
# Core specification without the fiscal term: mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem.
# Candidates (all t-1), each with a stated meaning:
#   int_tme   debt interest / total managed expenditure, %: the share of the spending total pre-empted by debt service (the PEWP line)
#   int_rev   debt interest / national accounts taxes, %: the interest-to-revenue ratio (the IMF / rating-agency fiscal-space measure)
#   int_gdp   debt interest / GDP, %
#   int_wap   real debt interest per working-age person, GBP 000 at 2024-25 prices: the inherited burden per worker (parallel to tax_workpop);
#             computed as tax_workpop x int_gdp / tax_gdp, so it shares the deflator and population of the tax term
#   prim_def  primary deficit / GDP, pp: PSNB less debt interest (the fiscal effort net of inherited costs; positive = deficit)
#   prog_gdp  non-interest (programme) spending / GDP, %: TME less debt interest
#   eff_rate  effective interest rate on the debt, %: debt interest / net debt
# Debt interest / GDP and TME / GDP (Millennium A28, to 2016) are growth-spliced to 2019 with ONS PUSF ANLO and EBFT over OBR fiscal-year
# nominal GDP, the same splice as int_tme. Levels, logs (where positive) and first differences are tested; AIC is compared with the
# no-fiscal-term model on the same observations. RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; CORE_X <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem")
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv")
obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp; pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn; pusf$tme_gdp_ons <- 100 * pusf$EBFT / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
DEF <- c(int_tme = "debt interest / total managed expenditure, %, t-1 (Millennium A28; ONS PUSF ANLO/EBFT growth-spliced 2017-19)",
         int_rev = "debt interest / national accounts taxes, %, t-1 (interest-to-revenue ratio; Millennium A28 and OBR HPFD, ONS-spliced 2017-19)",
         int_gdp = "debt interest / GDP, %, t-1 (Millennium A28; ONS PUSF ANLO over OBR nominal GDP growth-spliced 2017-19)",
         int_wap = "real debt interest per working-age person, GBP 000 at 2024-25 prices, t-1 (= tax_workpop x int_gdp / tax_gdp)",
         prim_def = "primary deficit, % of GDP, t-1 (PSNB less debt interest; positive = deficit)",
         prog_gdp = "non-interest (programme) spending, % of GDP, t-1 (TME less debt interest)",
         eff_rate = "effective interest rate on the debt, %, t-1 (debt interest / net debt)")
for (nm in names(DEF)) { DEFS[[paste0("L_", nm)]] <- DEF[[nm]]; DEFS[[paste0("L_ln_", nm)]] <- paste("log of", DEF[[nm]]); DEFS[[paste0("L_d_", nm)]] <- paste("annual change in", DEF[[nm]]) }
DEFS[["L_ln_int_tme"]] <- paste("log of", DEF[["int_tme"]])
res_all <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R")
  need <- c("int_tme", "int_gdp", "tme_gdp", "psnd_gdp", "psnb_gdp", "tax_gdp", "tax_workpop"); miss <- setdiff(need, names(df)); if (length(miss)) stop("columns missing from sheet data_", s, ": ", paste(miss, collapse = ", "), ". Pull the latest workbook (git pull).")
  S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$tme_gdp_x <- splice(S$tme_gdp, S$year, pusf$tme_gdp_ons)
  cat(sprintf("   splice check 2016: Millennium int/GDP %.2f vs ONS %.2f; TME/GDP %.1f vs ONS %.1f; spliced int/GDP 2017-19: %s\n", S$int_gdp[S$year == 2016], pusf$int_gdp_ons[pusf$year == 2016], S$tme_gdp[S$year == 2016], pusf$tme_gdp_ons[pusf$year == 2016], paste(sprintf("%.2f", S$int_gdp_x[S$year %in% 2017:2019]), collapse = " ")))
  V <- list(int_tme = S$int_tme, int_rev = 100 * S$int_gdp_x / S$tax_gdp, int_gdp = S$int_gdp_x, int_wap = S$tax_workpop * S$int_gdp_x / S$tax_gdp, prim_def = S$psnb_gdp - S$int_gdp_x, prog_gdp = S$tme_gdp_x - S$int_gdp_x, eff_rate = 100 * S$int_gdp_x / S$psnd_gdp)
  for (nm in names(V)) { S[[paste0("L_", nm)]] <- lag1(V[[nm]]); if (all(V[[nm]][!is.na(V[[nm]])] > 0)) S[[paste0("L_ln_", nm)]] <- lag1(log(V[[nm]])); S[[paste0("L_d_", nm)]] <- lag1(c(NA, diff(V[[nm]]))) }
  F <- function(xs) fit(S, xs, OUTC); none <- F(CORE_X)
  logs <- c("L_ln_int_tme", "L_ln_int_rev", "L_ln_int_gdp", "L_ln_int_wap", "L_prim_def", "L_ln_prog_gdp", "L_ln_eff_rate"); levs <- paste0("L_", names(V)); difs <- paste0("L_d_", names(V))
  mk <- function(vs) { out <- list(none = none); for (v in vs) out[[sub("^L_", "", v)]] <- F(c(CORE_X, v)); out }
  panel(mk(logs), sprintf("Table 1. Interest-burden and spending measures in logs (primary deficit in level), %s", L), c("delta_oadr_10", logs, "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 13)
  panel(mk(levs), sprintf("Table 2. The same measures in levels, %s", L), c("delta_oadr_10", levs, "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 13)
  panel(mk(difs), sprintf("Table 3. The same measures in annual changes, %s", L), c("delta_oadr_10", difs, "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 13)
  for (v in unique(c(logs, levs, difs))) { m <- F(c(CORE_X, v)); if (is.null(m)) next; n0 <- fit(S[S$year %in% attr(m, "dd")$year, ], CORE_X, OUTC); ct <- coeftest(m, vcov. = vcovHAC(m))
    res_all[[length(res_all) + 1]] <- data.frame(sample = s, term = v, N = nobs(m), coef = ct[v, 1], p = ct[v, 4], dAIC = AIC(m) - AIC(n0), proj = ct["delta_oadr_10", 1], proj_p = ct["delta_oadr_10", 4]) } }
R <- do.call(rbind, res_all); W <- merge(R[R$sample == "1964", c("term", "N", "coef", "p", "dAIC", "proj")], R[R$sample == "1977", c("term", "N", "coef", "p", "dAIC", "proj")], by = "term", suffixes = c("_cit", "_orig"))
W$both10 <- W$p_cit < 0.10 & W$p_orig < 0.10 & sign(W$coef_cit) == sign(W$coef_orig); W$both05 <- W$p_cit < 0.05 & W$p_orig < 0.05 & sign(W$coef_cit) == sign(W$coef_orig)
W <- W[order(-W$both05, -W$both10, W$dAIC_cit + W$dAIC_orig), ]
cat("\n\n==================== SUMMARY: both samples (dAIC against the model with no fiscal term, same observations) ====================\n"); print(W, row.names = FALSE, digits = 3)
cat("\nboth10 / both05: same sign and HAC p < .10 / .05 on BOTH samples.\n")
