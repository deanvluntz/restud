# =============================================================================
# combined_fiscal.R (Sam, 30 Sep 2026): (1) the tax and debt-interest terms on a COMMON base, and combined into ONE numerator;
# (2) Sam's aggregate: the annual log change in (taxes + net debt + debt interest) / GDP.
# Core specification: mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_ln_int_tme (all t-1).
# Section 1 columns (the labour term, the debt change and the own lag as in the core specification throughout):
#   main | tax_gdp + int_gdp (both % of GDP) | tax_workpop + int_wap (both real GBP 000 per working-age person) | tax_tme + int_tme (both % of TME)
#   | taxint_wap = (taxes + debt interest) per working-age person, one term | taxint_gdp = (taxes + debt interest) / GDP, one term | logs of the two combined terms.
# Section 2 columns: main | dln_fisc_gdp for ln_debt_gdp (int_tme kept) | dln_fisc_gdp for ln_debt_gdp and int_tme | dln_fisc_gdp for all three fiscal
#   terms | ln_fisc_gdp (log level) added to main | dln_taxint_gdp (taxes + interest) for ln_debt_gdp | dln_debtint_gdp (debt + interest) for ln_debt_gdp.
# Debt interest / GDP and TME / GDP (Millennium, to 2016) growth-spliced to 2019 with ONS PUSF over OBR fiscal-year nominal GDP, as before.
# Section 3: the one-term burden (taxes + debt interest per working-age person) in full: equal-coefficient Wald test against the two-term
#   version, diagnostics, bounds test, magnitudes, and without the projection.
# AIC is compared on the same observations. RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); library(ARDL); suppressWarnings({ library(tseries); library(strucchange) }); EXACT <- FALSE
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp
pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn; pusf$tme_gdp_ons <- 100 * pusf$EBFT / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
D <- c(L_tax_gdp = "national accounts taxes, % of GDP, t-1 (OBR HPFD)", L_int_gdp = "public sector debt interest, % of GDP, t-1 (Millennium A28; ONS-spliced 2017-19)",
       L_int_wap = "real debt interest per working-age person, GBP 000 at 2024-25 prices, t-1 (= tax_workpop x int_gdp / tax_gdp)", L_tax_tme = "national accounts taxes, % of total managed expenditure, t-1 (= tax_gdp / tme_gdp)",
       L_int_tme = "public sector debt interest, % of total managed expenditure, t-1 (Millennium A28; ONS-spliced 2017-19)", L_taxint_wap = "real taxes plus debt interest per working-age person, GBP 000 at 2024-25 prices, t-1",
       L_taxint_gdp = "taxes plus debt interest, % of GDP, t-1", L_ln_taxint_wap = "log of L_taxint_wap", L_ln_taxint_gdp = "log of L_taxint_gdp",
       L_dln_fisc_gdp = "annual log change in (taxes + net debt + debt interest) / GDP, t-1", L_ln_fisc_gdp = "log of (taxes + net debt + debt interest) / GDP, t-1",
       L_dln_taxint_gdp = "annual log change in (taxes + debt interest) / GDP, t-1", L_dln_debtint_gdp = "annual log change in (net debt + debt interest) / GDP, t-1", L_ln_int_tme = "log of L_int_tme")
for (k in names(D)) DEFS[[k]] <- D[[k]]
bounds <- function(S, xs, dv = "mig_gross") { lev <- sub("^L_", "", setdiff(xs, paste0("L_", dv))); vv <- c(dv, lev); ok <- complete.cases(S[, vv]); ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  m_ardl <- ardl(as.formula(paste(dv, "~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1)); bf <- bounds_f_test(m_ardl, case = 3, exact = EXACT); bt <- bounds_t_test(m_ardl, case = 3, exact = EXACT); lr <- multipliers(m_ardl, type = "lr")
  list(F = unname(bf$statistic), Fp = unname(bf$p.value), t = unname(bt$statistic), tp = unname(bt$p.value), lr = lr[lr$Term == "delta_oadr_10", ]) }
diag_line <- function(m) { dd <- attr(m, "dd"); cat(sprintf("    RESET(2,3) %s | RESET(2) %s | BG(1) %s | BG(2) %s | Ljung-Box(4) %s | rainbow %s | Breusch-Pagan %s | Jarque-Bera %s | OLS-CUSUM %s | max VIF %.1f | AIC %.2f | N %d\n",
  pf2(resettest(m)$p.value), pf2(resettest(m, power = 2)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(Box.test(residuals(m), lag = 4, type = "Ljung-Box")$p.value), pf2(raintest(m)$p.value), pf2(bptest(m)$p.value), pf2(tseries::jarque.bera.test(residuals(m))$p.value),
  pf2(sctest(efp(formula(m), data = dd, type = "OLS-CUSUM"))$p.value), max(vif(m)), AIC(m), nobs(m))) }
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$tme_gdp_x <- splice(S$tme_gdp, S$year, pusf$tme_gdp_ons)
  V <- list(tax_gdp = S$tax_gdp, int_gdp = S$int_gdp_x, int_wap = S$tax_workpop * S$int_gdp_x / S$tax_gdp, tax_tme = 100 * S$tax_gdp / S$tme_gdp_x, int_tme = S$int_tme,
            taxint_wap = S$tax_workpop * (1 + S$int_gdp_x / S$tax_gdp), taxint_gdp = S$tax_gdp + S$int_gdp_x, fisc_gdp = S$tax_gdp + S$psnd_gdp + S$int_gdp_x, debtint_gdp = S$psnd_gdp + S$int_gdp_x)
  S$ln_vac_unem <- log(S$vac_unem); S$taxint_wap <- V$taxint_wap
  for (nm in names(V)) { S[[paste0("L_", nm)]] <- lag1(V[[nm]]); S[[paste0("L_ln_", nm)]] <- lag1(log(V[[nm]])); S[[paste0("L_dln_", nm)]] <- lag1(c(NA, diff(log(V[[nm]])))) }
  cat(sprintf("   (taxes + net debt + debt interest) / GDP: range %.0f-%.0f%%; correlation of its log change with ln_debt_gdp %.2f; of taxint_wap with tax_workpop %.3f\n", min(V$fisc_gdp, na.rm = TRUE), max(V$fisc_gdp, na.rm = TRUE), cor(S$L_dln_fisc_gdp, S$L_ln_debt_gdp, use = "pair"), cor(S$L_taxint_wap, S$L_tax_workpop, use = "pair")))
  F <- function(xs) fit(S, xs, OUTC); K <- c("L_mig_gross", "delta_oadr_10", "L_ln_vac_unem"); main <- F(c(K, "L_tax_workpop", "L_ln_debt_gdp", "L_ln_int_tme"))
  same_aic <- function(mods) { yrs <- Reduce(intersect, lapply(mods, function(m) attr(m, "dd")$year)); cat(sprintf("   AIC on the common %d observations: ", length(yrs))); for (nm in names(mods)) { xs <- setdiff(names(coef(mods[[nm]])), "(Intercept)"); m2 <- fit(S[S$year %in% yrs, ], xs, OUTC); cat(sprintf("%s %.2f; ", nm, AIC(m2))) }; cat("\n") }
  # ---- 1 common base / one numerator ----
  m1 <- list(main = main, "tax_gdp + int_gdp" = F(c(K, "L_ln_debt_gdp", "L_tax_gdp", "L_int_gdp")), "tax_workpop + int_wap" = F(c(K, "L_ln_debt_gdp", "L_tax_workpop", "L_int_wap")), "tax_tme + int_tme" = F(c(K, "L_ln_debt_gdp", "L_tax_tme", "L_int_tme")),
             "taxint_wap" = F(c(K, "L_ln_debt_gdp", "L_taxint_wap")), "taxint_gdp" = F(c(K, "L_ln_debt_gdp", "L_taxint_gdp")), "ln_taxint_wap" = F(c(K, "L_ln_debt_gdp", "L_ln_taxint_wap")), "ln_taxint_gdp" = F(c(K, "L_ln_debt_gdp", "L_ln_taxint_gdp")))
  panel(m1, sprintf("Table 1. Taxes and debt interest on a common base, and as one term, %s", L), c("delta_oadr_10", "L_tax_workpop", "L_ln_int_tme", "L_tax_gdp", "L_int_gdp", "L_int_wap", "L_tax_tme", "L_int_tme", "L_taxint_wap", "L_taxint_gdp", "L_ln_taxint_wap", "L_ln_taxint_gdp", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 22, nested_F = TRUE,
        notes = "Column titles name the fiscal term(s) that replace tax_workpop and ln_int_tme; ln_debt_gdp, log V/U and the own lag as in the core specification throughout."); same_aic(m1)
  # ---- 2 Sam's aggregate ----
  m2 <- list(main = main, "dln_fisc_gdp for ln_debt_gdp" = F(c(K, "L_tax_workpop", "L_dln_fisc_gdp", "L_ln_int_tme")), "dln_fisc_gdp for debt + int" = F(c(K, "L_tax_workpop", "L_dln_fisc_gdp")), "dln_fisc_gdp for all three" = F(c(K, "L_dln_fisc_gdp")),
             "+ln_fisc_gdp" = F(c(K, "L_tax_workpop", "L_ln_debt_gdp", "L_ln_int_tme", "L_ln_fisc_gdp")), "dln_taxint_gdp for ln_debt_gdp" = F(c(K, "L_tax_workpop", "L_dln_taxint_gdp", "L_ln_int_tme")), "dln_debtint_gdp for ln_debt_gdp" = F(c(K, "L_tax_workpop", "L_dln_debtint_gdp", "L_ln_int_tme")))
  panel(m2, sprintf("Table 2. The log change in (taxes + net debt + debt interest) / GDP, %s", L), c("delta_oadr_10", "L_dln_fisc_gdp", "L_ln_fisc_gdp", "L_dln_taxint_gdp", "L_dln_debtint_gdp", "L_ln_debt_gdp", "L_tax_workpop", "L_ln_int_tme", "L_ln_vac_unem", "L_mig_gross"), w = 30, nested_F = TRUE,
        notes = "'for X': the aggregate replaces X; '+': added to the core specification. Net debt is a stock and taxes and interest are flows, so the aggregate is dominated by the debt ratio (debt / GDP is 25-100 per cent, taxes about 30, interest 2-5)."); same_aic(m2)
  # ---- 3 the one-term burden in full ----
  cat("\n3. The one-term burden per working-age person (taxes + debt interest) in full\n")
  two <- m1[["tax_workpop + int_wap"]]; w <- linearHypothesis(two, "L_tax_workpop - L_int_wap = 0", vcov. = vcovHAC(two))
  cat(sprintf("   equal-coefficient test in the two-term model (a pound of debt interest per worker = a pound of tax per worker): HAC F = %.2f, p = %.3f; two-term AIC %.2f vs one-term %.2f\n", w$F[2], w$`Pr(>F)`[2], AIC(two), AIC(m1[["taxint_wap"]])))
  one <- m1[["taxint_wap"]]; diag_line(one); dd <- attr(one, "dd"); ct <- coeftest(one, vcov. = vcovHAC(one)); rho <- ct["L_mig_gross", 1]
  b <- tryCatch(bounds(S, c(K, "L_ln_debt_gdp", "L_taxint_wap")), error = function(e) { cat("    bounds test failed:", conditionMessage(e), "\n"); NULL })
  if (!is.null(b)) cat(sprintf("    bounds (case 3, asymptotic): F = %.2f (p %s), t = %.2f (p %s); long-run projection %.4f [%.4f, %.4f]\n", b$F, pf2(b$Fp), b$t, pf2(b$tp), b$lr$Estimate, b$lr$Estimate - 1.96 * b$lr$`Std. Error`, b$lr$Estimate + 1.96 * b$lr$`Std. Error`))
  cat(sprintf("    one SD of the projection (%.1f) -> %.1f%% on impact, %.1f%% long run; one SD of taxint_wap (GBP %.0f per working-age person) -> %.1f%% on impact, %.1f%% long run; range of taxint_wap %.1f-%.1f (GBP 000)\n", sd(dd$delta_oadr_10), 100 * (exp(ct["delta_oadr_10", 1] * sd(dd$delta_oadr_10)) - 1), 100 * (exp(ct["delta_oadr_10", 1] * sd(dd$delta_oadr_10) / (1 - rho)) - 1), 1000 * sd(dd$L_taxint_wap), 100 * (exp(ct["L_taxint_wap", 1] * sd(dd$L_taxint_wap)) - 1), 100 * (exp(ct["L_taxint_wap", 1] * sd(dd$L_taxint_wap) / (1 - rho)) - 1), min(dd$L_taxint_wap), max(dd$L_taxint_wap)))
  np <- F(c(setdiff(K, "delta_oadr_10"), "L_ln_debt_gdp", "L_taxint_wap")); ctn <- coeftest(np, vcov. = vcovHAC(np)); cat(sprintf("    without the projection: taxint_wap %.4f (p %s), AIC %.2f, RESET %s\n", ctn["L_taxint_wap", 1], pf2(ctn["L_taxint_wap", 4]), AIC(np), pf2(resettest(np)$p.value))) }
cat("\nDone.\n")
