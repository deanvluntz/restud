# =============================================================================
# int_wap_variant.R (Sam, 30 Sep 2026): real debt interest per working-age person (int_wap, GBP 000 at 2024-25 prices, the inherited
# burden per worker, parallel to tax_workpop) in place of the debt-interest share, with ln_debt_gdp and log V/U as in the core
# specification. int_wap = tax_workpop x (debt interest / GDP) / (taxes / GDP): it shares the deflator and population of the tax term.
# Debt interest / GDP (Millennium A28, to 2016) is growth-spliced to 2019 with ONS PUSF ANLO over OBR fiscal-year nominal GDP.
# Columns: main (ln_int_tme) | int_tme | int_wap | ln_int_wap | int_wap, no proj. Diagnostics, bounds test and magnitudes for int_wap.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); library(ARDL); suppressWarnings({ library(tseries); library(strucchange) }); EXACT <- FALSE
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; CX <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem")
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp
pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
bounds <- function(S, xs, dv = "mig_gross") { lev <- sub("^L_", "", setdiff(xs, paste0("L_", dv))); vv <- c(dv, lev); ok <- complete.cases(S[, vv]); ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  m_ardl <- ardl(as.formula(paste(dv, "~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1)); bf <- bounds_f_test(m_ardl, case = 3, exact = EXACT); bt <- bounds_t_test(m_ardl, case = 3, exact = EXACT); lr <- multipliers(m_ardl, type = "lr")
  list(F = unname(bf$statistic), Fp = unname(bf$p.value), t = unname(bt$statistic), tp = unname(bt$p.value), lr = lr[lr$Term == "delta_oadr_10", ]) }
diag_line <- function(m) { dd <- attr(m, "dd"); cat(sprintf("    RESET(2,3) %s | RESET(2) %s | BG(1) %s | BG(2) %s | Ljung-Box(4) %s | rainbow %s | Breusch-Pagan %s | Jarque-Bera %s | OLS-CUSUM %s | max VIF %.1f | AIC %.2f | N %d\n",
  pf2(resettest(m)$p.value), pf2(resettest(m, power = 2)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(Box.test(residuals(m), lag = 4, type = "Ljung-Box")$p.value), pf2(raintest(m)$p.value), pf2(bptest(m)$p.value), pf2(tseries::jarque.bera.test(residuals(m))$p.value),
  pf2(sctest(efp(formula(m), data = dd, type = "OLS-CUSUM"))$p.value), max(vif(m)), AIC(m), nobs(m))) }
DEFS[["L_int_tme"]] <- "public sector debt interest, % of total managed expenditure, t-1 (Millennium A28; ONS PUSF growth-spliced 2017-19)"; DEFS[["L_ln_int_tme"]] <- "log of L_int_tme"
DEFS[["L_int_wap"]] <- "real public sector debt interest per working-age person, GBP 000 at 2024-25 prices, t-1 (= tax_workpop x debt interest / GDP / taxes / GDP; Millennium A28 and OBR HPFD, ONS-spliced 2017-19)"; DEFS[["L_ln_int_wap"]] <- "log of L_int_wap"
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$int_wap <- S$tax_workpop * S$int_gdp_x / S$tax_gdp; S$ln_int_wap <- log(S$int_wap); S$L_int_wap <- lag1(S$int_wap); S$L_ln_int_wap <- lag1(S$ln_int_wap)
  for (v in c("vac_unem", "int_tme")) S[[paste0("ln_", v)]] <- log(S[[v]]); F <- function(xs) fit(S, xs, OUTC)
  cat(sprintf("   int_wap range %.3f-%.3f (GBP 000 per working-age person, 2024-25 prices); correlation with tax_workpop %.2f, with int_tme %.2f\n", min(S$int_wap, na.rm = TRUE), max(S$int_wap, na.rm = TRUE), cor(S$int_wap, S$tax_workpop, use = "pair"), cor(S$int_wap, S$int_tme, use = "pair")))
  mods <- list("main (ln_int_tme)" = F(c(CX, "L_ln_int_tme")), "int_tme" = F(c(CX, "L_int_tme")), "int_wap" = F(c(CX, "L_int_wap")), "ln_int_wap" = F(c(CX, "L_ln_int_wap")), "int_wap, no proj" = F(c(setdiff(CX, "delta_oadr_10"), "L_int_wap")))
  panel(mods, sprintf("Table 1. Real debt interest per working-age person in place of the debt-interest share, %s", L), c("delta_oadr_10", "L_ln_int_tme", "L_int_tme", "L_int_wap", "L_ln_int_wap", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 20, nested_F = TRUE)
  m <- mods[["int_wap"]]; cat("  int_wap in full:\n"); diag_line(m); dd <- attr(m, "dd"); ct <- coeftest(m, vcov. = vcovHAC(m)); rho <- ct["L_mig_gross", 1]
  b <- tryCatch(bounds(S, c(CX, "L_int_wap")), error = function(e) { cat("    bounds test failed:", conditionMessage(e), "\n"); NULL })
  if (!is.null(b)) cat(sprintf("    bounds (case 3, asymptotic): F = %.2f (p %s), t = %.2f (p %s); long-run projection %.4f [%.4f, %.4f]\n", b$F, pf2(b$Fp), b$t, pf2(b$tp), b$lr$Estimate, b$lr$Estimate - 1.96 * b$lr$`Std. Error`, b$lr$Estimate + 1.96 * b$lr$`Std. Error`))
  cat(sprintf("    one SD of the projection (%.1f) -> %.1f%% on impact, %.1f%% long run; one SD of int_wap (GBP %.0f per working-age person) -> %.1f%% on impact, %.1f%% long run; one SD of tax_workpop (GBP %.0f) -> %.1f%% / %.1f%%\n",
              sd(dd$delta_oadr_10), 100 * (exp(ct["delta_oadr_10", 1] * sd(dd$delta_oadr_10)) - 1), 100 * (exp(ct["delta_oadr_10", 1] * sd(dd$delta_oadr_10) / (1 - rho)) - 1), 1000 * sd(dd$L_int_wap), 100 * (exp(ct["L_int_wap", 1] * sd(dd$L_int_wap)) - 1), 100 * (exp(ct["L_int_wap", 1] * sd(dd$L_int_wap) / (1 - rho)) - 1),
              1000 * sd(dd$L_tax_workpop), 100 * (exp(ct["L_tax_workpop", 1] * sd(dd$L_tax_workpop)) - 1), 100 * (exp(ct["L_tax_workpop", 1] * sd(dd$L_tax_workpop) / (1 - rho)) - 1)))
  m0 <- F(CX); cat(sprintf("    without any fiscal-pressure term: AIC %.2f; with int_wap %.2f; with int_tme %.2f\n", AIC(m0), AIC(m), AIC(mods[["int_tme"]]))) }
cat("\nDone.\n")
