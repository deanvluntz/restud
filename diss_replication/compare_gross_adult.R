# =============================================================================
# compare_gross_adult.R -- EXPLORATORY (Sam, 28 Sep 2026): the gross-inflow specification with the tax burden per ADULT, with and
# without log vacancies per working-age person and the debt-interest share of spending, on both samples:
#     mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_voter + L_ln_debt_gdp + L_unem [+ L_ln_vac_wap] [+ L_int_tme]
#   1  each of the four models with and without the projection (Sam's two-part criterion: an addition must improve the model
#      without the projection AND the model with it), nested F and dAIC of the projection, HAC diagnostics
#   2  ARDL bounds test and long-run multipliers for the four models with the projection
#   3  recursive one-step-ahead forecast RMSE, without and with the projection
# Needs only Dissertation_Dataset_Final.xlsx. RStudio: working directory = diss_replication/, then source("compare_gross_adult.R").
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R"); suppressPackageStartupMessages(library(ARDL))
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
DEFS[["mig_gross"]] <- "log gross inflow of controlled migrants, persons, unadjusted (no denominator, no basis step)"; DEFS[["L_mig_gross"]] <- "mig_gross lagged one year"
DEFS[["L_ln_vac_wap"]] <- "log of notified vacancies per 1,000 working-age persons, t-1 (Millennium M5; ONS AP2Y for 2017-19)"
if (!exists("ADD")) ADD <- list("base" = NULL, "+ln_vac_wap" = "L_ln_vac_wap", "+int_tme" = "L_int_tme", "+both" = c("L_ln_vac_wap", "L_int_tme"))   # a wrapper may set ADD (compare_gross_tbill.R)
if (!exists("TAXVAR")) TAXVAR <- "L_tax_voter"
cat("\n##### Gross inflows with the tax burden per adult: log vacancies and/or debt interest #####\n")
for (k in names(SMP)) { d <- SMP[[k]]$df; S <- subset(d, year >= SMP[[k]]$Y0 & year <= 2019); TAG <- paste0(SMP[[k]]$LAB, "2019")
  B0 <- c("L_mig_gross", TAXVAR, "L_ln_debt_gdp", "L_unem"); rows <- unique(c("delta_oadr_10", unlist(ADD), TAXVAR, "L_ln_debt_gdp", "L_unem", "L_mig_gross"))
  # --- 1 without and with the projection ---------------------------------------------------------------------------------
  p <- list(); for (a in names(ADD)) { p[[paste0(a, ", no proj")]] <- fit(S, c(B0, ADD[[a]]), "mig_gross"); p[[a]] <- fit(S, c(B0, "delta_oadr_10", ADD[[a]]), "mig_gross") }
  panel(p, paste0("1. Gross inflows, tax per adult: additions without and with the projection (", TAG, ")"), rows, w = 20, nested_F = TRUE,
        c("Read in pairs: an addition earns its place if it lowers the AIC of the column WITHOUT the projection and of the column WITH it (Sam's criterion).",
          "int_tme for 2017-19 is spliced from ONS/OBR onto Millennium A28; vacancies from ONS AP2Y; the 2016 re-estimates are printed above."))
  a0 <- AIC(p[["base, no proj"]]); a1 <- AIC(p[["base"]])
  for (a in names(ADD)[-1]) cat(sprintf("  %-12s dAIC without projection %+6.2f | with projection %+6.2f | projection %.4f -> %.4f\n", a, AIC(p[[paste0(a, ", no proj")]]) - a0, AIC(p[[a]]) - a1, coef(p[["base"]])["delta_oadr_10"], coef(p[[a]])["delta_oadr_10"]))
  # --- 2 bounds tests ------------------------------------------------------------------------------------------------------
  S$ln_vac_wap <- log(S$vac_wap); cat("\n2. ARDL(1,...) bounds tests (PSS case 3, exact) and long-run projection multiplier:\n")
  for (a in names(ADD)) { lev <- c("delta_oadr_10", "unem", "ln_debt_gdp", sub("^L_", "", TAXVAR), sub("^L_", "", ADD[[a]])); vv <- c("mig_gross", lev); ok <- complete.cases(S[, vv])
    ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1); m_ardl <- ardl(as.formula(paste("mig_gross ~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1))
    set.seed(20260802); bf <- bounds_f_test(m_ardl, case = 3, exact = TRUE); set.seed(20260802); bt <- bounds_t_test(m_ardl, case = 3, exact = TRUE); lr <- multipliers(m_ardl, type = "lr")
    cat(sprintf("  %-12s F = %.2f (p %.3f), t = %.2f (p %.3f), N %d | long-run projection %.4f (p %s)%s\n", a, bf$statistic, bf$p.value, bt$statistic, bt$p.value, sum(ok), lr$Estimate[lr$Term == "delta_oadr_10"], pf(lr$`Pr(>|t|)`[lr$Term == "delta_oadr_10"]),
                if (length(ADD[[a]])) paste0(" | ", paste(sprintf("%s %.3f (p %s)", sub("^L_", "", ADD[[a]]), lr$Estimate[match(sub("^L_", "", ADD[[a]]), lr$Term)], vapply(lr$`Pr(>|t|)`[match(sub("^L_", "", ADD[[a]]), lr$Term)], pf, character(1))), collapse = ", ")) else "")) }
  # --- 3 out-of-sample --------------------------------------------------------------------------------------------------------
  oos <- function(xs, from) { e <- c(); for (t in from:2019) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; tr <- tr[complete.cases(tr[, c("mig_gross", xs)]), ]
      if (!nrow(te) || anyNA(te[, xs])) next; mm <- lm(reformulate(xs, "mig_gross"), data = tr); e <- c(e, te$mig_gross - predict(mm, newdata = te)) }; sqrt(mean(e^2)) }
  from <- if (k == "extended") 1990 else 1997; cat(sprintf("\n3. Recursive one-step-ahead forecast RMSE, %d-2019:\n", from))
  for (a in names(ADD)) cat(sprintf("  %-12s no proj %.4f | with proj %.4f | ratio %.2f\n", a, oos(c(B0, ADD[[a]]), from), oos(c(B0, "delta_oadr_10", ADD[[a]]), from), oos(c(B0, "delta_oadr_10", ADD[[a]]), from) / oos(c(B0, ADD[[a]]), from)))
}
# --- 4 which no-projection variants pass the diagnostics? (Sam, 28 Sep 2026) -------------------------------------------------------
# The base without the projection fails RESET; here every further control of Tables B19-B25 (and the trend) is added to the
# no-projection model, singly and on top of log vacancies, and RESET / BG(1) are reported; then the same additions with the projection.
CAND <- c("L_ln_vac_wap", "L_ln_vac_unem", "L_dln_vac_unem", "L_wage_infl", "L_real_wage", "L_union", "L_replace", "L_growth", "L_growth_pc", "L_tfp", "L_lab_prod", "L_particip", "growth_fc",
          "L_real_eri", "L_openness", "L_ca_gdp", "L_trade_bal", "L_tot", "L_inflation", "L_cpi_infl", "st_inf_exp", "lt_inf_exp", "inf_exp_1y", "inf_exp_5_10y", "L_m4_growth",
          "L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_tme_real_growth", "gad_earn", "gad_policy", "L_psnb_gdp", "L_psnd_gdp", "fc_budget", "fc_medium", "L_tax_ceiling", "golden_rule", "obr_era",
          "L_int_tme", "L_int_gdp", "L_bank_rate", "L_tbill", "L_gilt", "L_consol", "L_r_g", "trend", "L_emig_all", "brit_rate", "eu_rate")
cat("\n\n##### 4. Diagnostics of the NO-PROJECTION model (gross inflows, tax per adult) under every further control #####\n")
for (k in names(SMP)) { d <- SMP[[k]]$df; S <- subset(d, year >= SMP[[k]]$Y0 & year <= 2019); B0 <- c("L_mig_gross", TAXVAR, "L_ln_debt_gdp", "L_unem")
  cat(sprintf("\n%s sample (%s2019). Columns: RESET p | BG(1) p | AIC | N, for the no-projection model with the control [and with log vacancies too]; then the same with the projection.\n", k, SMP[[k]]$LAB))
  dg <- function(xs) { m <- fit(S, xs, "mig_gross"); if (is.null(m)) return(NULL); c(resettest(m)$p.value, bgtest(m, order = 1)$p.value, AIC(m), nobs(m)) }
  fmt <- function(v) if (is.null(v)) "          --" else sprintf("%s%.3f %s%.3f %7.2f %2d", if (v[1] >= 0.05) " " else "*", v[1], if (v[2] >= 0.05) " " else "*", v[2], v[3], v[4])
  cat(sprintf("%-18s %-30s %-30s %-30s %-30s\n", "control", "no proj +control", "no proj +ln_vac +control", "proj +control", "proj +ln_vac +control")); pass <- character(0)
  cat(sprintf("%-18s %-30s %-30s %-30s %-30s\n", "(none)", fmt(dg(B0)), fmt(dg(c(B0, "L_ln_vac_wap"))), fmt(dg(c(B0, "delta_oadr_10"))), fmt(dg(c(B0, "delta_oadr_10", "L_ln_vac_wap")))))
  for (v in CAND) { if (!(v %in% names(S))) next; a <- dg(c(B0, v)); b <- if (v != "L_ln_vac_wap") dg(c(B0, "L_ln_vac_wap", v)) else NULL; cc <- dg(c(B0, "delta_oadr_10", v)); dd <- if (v != "L_ln_vac_wap") dg(c(B0, "delta_oadr_10", "L_ln_vac_wap", v)) else NULL
    cat(sprintf("%-18s %-30s %-30s %-30s %-30s\n", sub("^L_", "", v), fmt(a), fmt(b), fmt(cc), fmt(dd)))
    if (!is.null(a) && a[1] >= 0.05 && a[2] >= 0.05) pass <- c(pass, sub("^L_", "", v)); if (!is.null(b) && b[1] >= 0.05 && b[2] >= 0.05) pass <- c(pass, paste0("ln_vac+", sub("^L_", "", v))) }
  cat("* = p < 0.05 (fails). No-projection variants passing both RESET and BG(1) at 5%:", if (length(pass)) paste(pass, collapse = ", ") else "none", "\n") }

# --- 5 a fuller diagnostic set for small samples (Sam, 28 Sep 2026: are RESET and BG enough?) ------------------------------------
# RESET with squares only and with squares + cubes; the rainbow test (fit on the central half against the whole sample); the
# Harvey-Collier test (recursive residuals, linearity); BG(1), BG(2); Ljung-Box on 4 lags; Breusch-Pagan; Jarque-Bera;
# OLS-CUSUM; and a residual-bootstrap p-value for RESET(2), which corrects the asymptotic RESET for N = 43.
suppressPackageStartupMessages({library(strucchange); library(tseries)})
boot_reset <- function(m, R = 999) { X <- model.matrix(m); y0 <- fitted(m); e <- residuals(m) - mean(residuals(m)); f0 <- resettest(m, power = 2)$statistic; set.seed(20260802)
  fs <- replicate(R, { yb <- y0 + sample(e, replace = TRUE); mb <- lm(yb ~ X - 1); resettest(mb, power = 2)$statistic }); mean(fs >= f0) }
diag_row <- function(m) c(RESET2 = resettest(m, power = 2)$p.value, RESET23 = resettest(m, power = 2:3)$p.value, RESET2_boot = boot_reset(m), rainbow = raintest(m)$p.value, harvey_collier = tryCatch(harvtest(m)$p.value, error = function(e) NA),
                          BG1 = bgtest(m, order = 1)$p.value, BG2 = bgtest(m, order = 2)$p.value, LjungBox4 = Box.test(residuals(m), lag = 4, type = "Ljung-Box")$p.value, BreuschPagan = bptest(m)$p.value,
                          JarqueBera = jarque.bera.test(residuals(m))$p.value, CUSUM = sctest(efp(formula(m), data = attr(m, "dd"), type = "OLS-CUSUM"))$p.value)
cat("\n\n##### 5. Fuller diagnostics (p-values) for the models of section 1, without and with the projection #####\n")
for (k in names(SMP)) { d <- SMP[[k]]$df; S <- subset(d, year >= SMP[[k]]$Y0 & year <= 2019); B0 <- c("L_mig_gross", TAXVAR, "L_ln_debt_gdp", "L_unem"); out <- NULL
  for (a in names(ADD)) for (pr in c(FALSE, TRUE)) { m <- fit(S, c(B0, if (pr) "delta_oadr_10", ADD[[a]]), "mig_gross"); out <- rbind(out, data.frame(model = paste0(a, if (pr) "" else ", no proj"), t(round(diag_row(m), 3)))) }
  cat(sprintf("\n%s sample (%s2019)\n", k, SMP[[k]]$LAB)); print(out, row.names = FALSE) }
cat("RESET2_boot: residual-bootstrap p-value of RESET(2), 999 draws. rainbow/harvey_collier: linearity tests (lmtest). CUSUM: OLS-based CUSUM of recursive residuals (strucchange).\n")
