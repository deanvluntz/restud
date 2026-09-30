# =============================================================================
# level_and_spread.R (Sam, 30 Sep 2026)
# 1  The core specification with the debt-interest share in LEVEL (int_tme, % of TME) against the log, both samples: coefficients, full
#    diagnostics, bounds test, long-run multiplier, one-SD magnitudes; and the with/without-projection comparison under each form.
# 2  Does the yield spread affect British citizen inflows? British inflows (log persons, IPS 3.01, from non-EU) on the core controls with
#    the term spread (10-year gilt less bill rate), the gilt yield and the bill rate added; with and without the projection; both samples.
#    Gilt yields end 2016 (Millennium), so these columns run to 2016.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); library(ARDL); suppressWarnings({ library(tseries); library(strucchange) }); EXACT <- FALSE
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; CX <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem")
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
bounds <- function(S, xs, dv = "mig_gross") { lev <- sub("^L_", "", setdiff(xs, paste0("L_", dv))); vv <- c(dv, lev); ok <- complete.cases(S[, vv]); ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  m_ardl <- ardl(as.formula(paste(dv, "~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1)); bf <- bounds_f_test(m_ardl, case = 3, exact = EXACT); bt <- bounds_t_test(m_ardl, case = 3, exact = EXACT); lr <- multipliers(m_ardl, type = "lr")
  list(F = unname(bf$statistic), Fp = unname(bf$p.value), t = unname(bt$statistic), tp = unname(bt$p.value), lr = lr[lr$Term == "delta_oadr_10", ]) }
diag_line <- function(m) { dd <- attr(m, "dd"); cat(sprintf("    RESET(2,3) %s | RESET(2) %s | BG(1) %s | BG(2) %s | Ljung-Box(4) %s | rainbow %s | Breusch-Pagan %s | Jarque-Bera %s | OLS-CUSUM %s | max VIF %.1f | AIC %.2f | N %d\n",
  pf2(resettest(m)$p.value), pf2(resettest(m, power = 2)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(Box.test(residuals(m), lag = 4, type = "Ljung-Box")$p.value), pf2(raintest(m)$p.value), pf2(bptest(m)$p.value), pf2(tseries::jarque.bera.test(residuals(m))$p.value),
  pf2(sctest(efp(formula(m), data = dd, type = "OLS-CUSUM"))$p.value), max(vif(m)), AIC(m), nobs(m))) }
DEFS[["L_int_tme"]] <- "public sector debt interest, % of total managed expenditure, t-1 (Millennium A28; ONS PUSF growth-spliced 2017-19)"; DEFS[["L_ln_int_tme"]] <- "log of L_int_tme"
DEFS[["L_term_spread"]] <- "10-year gilt yield minus the three-month Treasury bill rate, percentage points, t-1 (Millennium; to 2016)"; DEFS[["L_gilt"]] <- "10-year gilt yield, %, t-1 (Millennium; to 2016)"; DEFS[["L_tbill"]] <- "three-month Treasury bill rate, %, t-1"
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; for (v in c("vac_unem", "int_tme")) S[[paste0("ln_", v)]] <- log(S[[v]]); cat("\n\n==================== ", L, " ====================\n", sep = "")
  F <- function(xs, dv = OUTC) fit(S, xs, dv)
  # ---- 1 level against log ----
  m_log <- F(c(CX, "L_ln_int_tme")); m_lev <- F(c(CX, "L_int_tme")); n_log <- F(setdiff(c(CX, "L_ln_int_tme"), "delta_oadr_10")); n_lev <- F(setdiff(c(CX, "L_int_tme"), "delta_oadr_10"))
  panel(list("ln_int_tme (main)" = m_log, "int_tme" = m_lev, "ln_int_tme, no proj" = n_log, "int_tme, no proj" = n_lev), sprintf("Table 1. The debt-interest share in log and in level, with and without the projection, %s", L),
        c("delta_oadr_10", "L_ln_int_tme", "L_int_tme", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 22, nested_F = TRUE)
  for (nm in c("log", "level")) { m <- if (nm == "log") m_log else m_lev; v <- if (nm == "log") "L_ln_int_tme" else "L_int_tme"; cat(sprintf("  %s form:\n", nm)); diag_line(m); dd <- attr(m, "dd"); ct <- coeftest(m, vcov. = vcovHAC(m)); rho <- ct["L_mig_gross", 1]
    b <- tryCatch(bounds(S, c(CX, v)), error = function(e) { cat("    bounds test failed:", conditionMessage(e), "\n"); NULL })
    if (!is.null(b)) cat(sprintf("    bounds (case 3, asymptotic): F = %.2f (p %s), t = %.2f (p %s); long-run projection %.4f [%.4f, %.4f]\n", b$F, pf2(b$Fp), b$t, pf2(b$tp), b$lr$Estimate, b$lr$Estimate - 1.96 * b$lr$`Std. Error`, b$lr$Estimate + 1.96 * b$lr$`Std. Error`))
    sdp <- sd(dd$delta_oadr_10); sdx <- sd(dd[[v]])
    cat(sprintf("    one SD of the projection (%.1f) -> %.1f%% on impact, %.1f%% long run; one SD of %s (%.3f%s) -> %.1f%% on impact, %.1f%% long run\n", sdp, 100 * (exp(ct["delta_oadr_10", 1] * sdp) - 1), 100 * (exp(ct["delta_oadr_10", 1] * sdp / (1 - rho)) - 1), sub("^L_", "", v), sdx, if (nm == "level") " pp of TME" else "", 100 * (exp(ct[v, 1] * sdx) - 1), 100 * (exp(ct[v, 1] * sdx / (1 - rho)) - 1))) }
  # ---- 2 the yield spread and British inflows ----
  CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme"); B <- c("L_brit_gross", "delta_oadr_10", CT)
  mods <- list(main = F(B, "brit_gross"), "+term_spread" = F(c(B, "L_term_spread"), "brit_gross"), "+gilt" = F(c(B, "L_gilt"), "brit_gross"), "+tbill" = F(c(B, "L_tbill"), "brit_gross"), "+gilt +tbill" = F(c(B, "L_gilt", "L_tbill"), "brit_gross"),
               "no proj" = F(setdiff(B, "delta_oadr_10"), "brit_gross"), "no proj +term_spread" = F(c(setdiff(B, "delta_oadr_10"), "L_term_spread"), "brit_gross"))
  panel(mods, sprintf("Table 2. British citizen inflows (log persons) and the yield spread, %s", L), c("delta_oadr_10", "L_term_spread", "L_gilt", "L_tbill", CT, "L_brit_gross"), w = 21,
        notes = "brit_gross: log gross inflow of British citizens from non-EU countries (IPS 3.01). Gilt yields end 2016, so the spread and gilt columns run to 2016; the main column on the same years is the comparison.")
  m_same <- fit(S[S$year %in% attr(mods[["+term_spread"]], "dd")$year, ], B, "brit_gross"); cat(sprintf("    main column re-estimated on the spread column's years (N %d): projection %.4f (p %s), AIC %.2f vs %.2f with the spread\n", nobs(m_same), coef(m_same)["delta_oadr_10"], pf2(coeftest(m_same, vcov. = vcovHAC(m_same))["delta_oadr_10", 4]), AIC(m_same), AIC(mods[["+term_spread"]])))
  # the spread in the non-EU equation for comparison
  mm <- list(main = m_log, "+term_spread" = F(c(CX, "L_ln_int_tme", "L_term_spread")), "+gilt" = F(c(CX, "L_ln_int_tme", "L_gilt")), "+tbill" = F(c(CX, "L_ln_int_tme", "L_tbill")))
  panel(mm, sprintf("Table 3. The same terms in the non-EU equation, %s", L), c("delta_oadr_10", "L_term_spread", "L_gilt", "L_tbill", "L_ln_int_tme", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 16) }
cat("\nDone.\n")
