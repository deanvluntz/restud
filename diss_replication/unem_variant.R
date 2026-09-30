# =============================================================================
# unem_variant.R (Sam, 30 Sep 2026): the core specification with ln_debt_gdp (annual log change in debt / GDP) as now, the debt-interest
# share in LEVEL, and the plain ILO unemployment rate in place of log V/U. Columns: main (log forms) | int_tme level, log V/U |
# int_tme level, unem | int_tme log, unem | int_tme level, unem + log V/U (both labour terms). Both samples; diagnostics, bounds test
# and one-SD magnitudes for the unemployment variant. RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); library(ARDL); suppressWarnings({ library(tseries); library(strucchange) }); EXACT <- FALSE
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; FX <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp")
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
bounds <- function(S, xs, dv = "mig_gross") { lev <- sub("^L_", "", setdiff(xs, paste0("L_", dv))); vv <- c(dv, lev); ok <- complete.cases(S[, vv]); ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  m_ardl <- ardl(as.formula(paste(dv, "~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1)); bf <- bounds_f_test(m_ardl, case = 3, exact = EXACT); bt <- bounds_t_test(m_ardl, case = 3, exact = EXACT); lr <- multipliers(m_ardl, type = "lr")
  list(F = unname(bf$statistic), Fp = unname(bf$p.value), t = unname(bt$statistic), tp = unname(bt$p.value), lr = lr[lr$Term == "delta_oadr_10", ]) }
diag_line <- function(m) { dd <- attr(m, "dd"); cat(sprintf("    RESET(2,3) %s | RESET(2) %s | BG(1) %s | BG(2) %s | Ljung-Box(4) %s | rainbow %s | Breusch-Pagan %s | Jarque-Bera %s | OLS-CUSUM %s | max VIF %.1f | AIC %.2f | N %d\n",
  pf2(resettest(m)$p.value), pf2(resettest(m, power = 2)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(Box.test(residuals(m), lag = 4, type = "Ljung-Box")$p.value), pf2(raintest(m)$p.value), pf2(bptest(m)$p.value), pf2(tseries::jarque.bera.test(residuals(m))$p.value),
  pf2(sctest(efp(formula(m), data = dd, type = "OLS-CUSUM"))$p.value), max(vif(m)), AIC(m), nobs(m))) }
DEFS[["L_int_tme"]] <- "public sector debt interest, % of total managed expenditure, t-1 (Millennium A28; ONS PUSF growth-spliced 2017-19)"; DEFS[["L_ln_int_tme"]] <- "log of L_int_tme"
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  for (v in c("vac_unem", "int_tme")) S[[paste0("ln_", v)]] <- log(S[[v]]); F <- function(xs) fit(S, xs, OUTC)
  mods <- list("main (log forms)" = F(c(FX, "L_ln_vac_unem", "L_ln_int_tme")), "int_tme | ln_vac_unem" = F(c(FX, "L_ln_vac_unem", "L_int_tme")), "int_tme | unem" = F(c(FX, "L_unem", "L_int_tme")), "ln_int_tme | unem" = F(c(FX, "L_unem", "L_ln_int_tme")), "int_tme | unem + ln_vac_unem" = F(c(FX, "L_unem", "L_ln_vac_unem", "L_int_tme")))
  panel(mods, sprintf("Table 1. The unemployment rate in place of log V/U, with the debt-interest share in level, %s", L), c("delta_oadr_10", "L_int_tme", "L_ln_int_tme", "L_unem", "L_ln_vac_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_gross"), w = 30, nested_F = TRUE,
        notes = "Column titles: fiscal term | labour term. ln_debt_gdp (the annual log change in net debt / GDP) in every column.")
  m <- mods[["int_tme | unem"]]; cat("  int_tme | unem in full:\n"); diag_line(m); dd <- attr(m, "dd"); ct <- coeftest(m, vcov. = vcovHAC(m)); rho <- ct["L_mig_gross", 1]
  b <- tryCatch(bounds(S, c(FX, "L_unem", "L_int_tme")), error = function(e) { cat("    bounds test failed:", conditionMessage(e), "\n"); NULL })
  if (!is.null(b)) cat(sprintf("    bounds (case 3, asymptotic): F = %.2f (p %s), t = %.2f (p %s); long-run projection %.4f [%.4f, %.4f]\n", b$F, pf2(b$Fp), b$t, pf2(b$tp), b$lr$Estimate, b$lr$Estimate - 1.96 * b$lr$`Std. Error`, b$lr$Estimate + 1.96 * b$lr$`Std. Error`))
  cat(sprintf("    one SD of the projection (%.1f) -> %.1f%% on impact, %.1f%% long run; one SD of int_tme (%.2f pp) -> %.1f%% / %.1f%%; one SD of unem (%.2f pp) -> %.1f%% / %.1f%%\n", sd(dd$delta_oadr_10), 100 * (exp(ct["delta_oadr_10", 1] * sd(dd$delta_oadr_10)) - 1), 100 * (exp(ct["delta_oadr_10", 1] * sd(dd$delta_oadr_10) / (1 - rho)) - 1),
              sd(dd$L_int_tme), 100 * (exp(ct["L_int_tme", 1] * sd(dd$L_int_tme)) - 1), 100 * (exp(ct["L_int_tme", 1] * sd(dd$L_int_tme) / (1 - rho)) - 1), sd(dd$L_unem), 100 * (exp(ct["L_unem", 1] * sd(dd$L_unem)) - 1), 100 * (exp(ct["L_unem", 1] * sd(dd$L_unem) / (1 - rho)) - 1)))
  m0 <- F(c(FX, "L_unem")); cat(sprintf("    without the debt-interest share (ln_debt_gdp + unem only): AIC %.2f vs %.2f with it; projection %.4f\n", AIC(m0), AIC(m), coef(m0)["delta_oadr_10"])) }
cat("\nDone.\n")
