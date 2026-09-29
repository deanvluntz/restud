# =============================================================================
# compare_gross_final.R -- EXPLORATORY (Sam, 29 Sep 2026). Sam's conceptual model: gross inflows; tax burden per head (revenue /
# political constraint); log V/U (labour market); log change in debt/GDP (long-term capacity); log debt-interest share of spending
# (short-term capacity); the projection (future fiscal change).
#   A  which per-head tax measure performs best diagnostically (per working-age person / adult / resident / income-tax payer / average
#      earnings): fit, bounds test, fuller diagnostics, cross-sample stability, OOS
#   B  with the chosen tax measure, every other variable in the dataset added to the model: Sam's two-part criterion (AIC without and
#      with the projection), the term's coefficient, the projection, RESET and BG with the projection -- what else could carry a new effect?
#   C  robustness: balance of payments (current account, trade balance), terms of trade, nominal and real effective exchange rates
# No bootstrap; asymptotic bounds critical values. Needs only the workbook; RStudio: working directory = diss_replication/.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R"); suppressPackageStartupMessages({library(ARDL); library(strucchange); library(tseries)})
DEFS[["L_ln_vac_unem"]] <- "log of the vacancy/unemployment ratio, t-1"; DEFS[["L_ln_int_tme"]] <- "log of public sector debt interest as a share of total managed expenditure, t-1"
DEFS[["L_nom_eri"]] <- "nominal sterling effective exchange rate index, t-1 (Millennium A33; BIS for 2017-19)"; DEFS[["L_ln_nom_eri"]] <- "log of L_nom_eri"
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); df$ln_int_tme <- log(df$int_tme)
  df$L_nom_eri <- lag1(df$nominal_eri); df$L_ln_nom_eri <- log(df$L_nom_eri); df$nom_eri <- df$nominal_eri; list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
CORE <- c("L_mig_gross", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme")
diag_row <- function(m) c(RESET2 = resettest(m, power = 2)$p.value, RESET23 = resettest(m, power = 2:3)$p.value, rainbow = raintest(m)$p.value, harvey_collier = tryCatch(unname(harvtest(m)$p.value), error = function(e) NA),
  BG1 = bgtest(m, order = 1)$p.value, BG2 = bgtest(m, order = 2)$p.value, BG3 = bgtest(m, order = 3)$p.value, BreuschPagan = unname(bptest(m)$p.value), JarqueBera = unname(jarque.bera.test(residuals(m))$p.value),
  CUSUM = unname(sctest(efp(formula(m), data = attr(m, "dd"), type = "OLS-CUSUM"))$p.value), maxVIF = max(vif(m)), AIC = AIC(m), AICc = AIC(m) + 2 * length(coef(m)) * (length(coef(m)) + 1) / (nobs(m) - length(coef(m)) - 1), N = nobs(m))
bounds <- function(S, xs, dv = "mig_gross") { lev <- sub("^L_", "", setdiff(xs, paste0("L_", dv))); vv <- c(dv, lev); ok <- complete.cases(S[, vv]); ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  m_ardl <- ardl(as.formula(paste(dv, "~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1)); bf <- bounds_f_test(m_ardl, case = 3); bt <- bounds_t_test(m_ardl, case = 3); lr <- multipliers(m_ardl, type = "lr")
  c(F = unname(bf$statistic), Fp = unname(bf$p.value), t = unname(bt$statistic), tp = unname(bt$p.value), lr_proj = lr$Estimate[lr$Term == "delta_oadr_10"], lr_p = lr$`Pr(>|t|)`[lr$Term == "delta_oadr_10"]) }
oos <- function(S, xs, dv, from) { e <- c(); for (t in from:2019) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; tr <- tr[complete.cases(tr[, c(dv, xs)]), ]; if (!nrow(te) || anyNA(te[, xs])) next; mm <- lm(reformulate(xs, dv), data = tr); e <- c(e, te[[dv]] - predict(mm, newdata = te)) }; sqrt(mean(e^2)) }
# --- A tax measures -----------------------------------------------------------------------------------------------------------------
cat("\n##### A. The tax burden per head: which measure performs best in the conceptual model? #####\n")
TAX <- c("L_tax_workpop", "L_tax_voter", "L_tax_res", "L_tax_taxpayer", "L_tax_earn")
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); from <- if (k == "extended") 1990 else 1997
  p <- setNames(lapply(TAX, function(v) fit(S, c(CORE, v, "delta_oadr_10"), "mig_gross")), sub("^L_", "", TAX))
  panel(p, paste0("A. Conceptual model by tax measure (", SMP[[k]]$LAB, "2019)"), c("delta_oadr_10", TAX, "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme", "L_mig_gross"), w = 15, nested_F = TRUE,
        "Each column: gross inflows on the lagged inflow, the projection, the tax burden named, the log change in debt/GDP, log V/U and the log debt-interest share. tax_taxpayer from 1979.")
  out <- NULL; for (v in TAX) { xs <- c(CORE, v, "delta_oadr_10"); m <- fit(S, xs, "mig_gross"); b <- bounds(S, xs); out <- rbind(out, data.frame(tax = sub("^L_", "", v), t(round(diag_row(m), 3)), boundsF = round(b["F"], 2), boundsFp = round(b["Fp"], 3), lr_proj = round(b["lr_proj"], 4), lr_p = round(b["lr_p"], 3), oos = round(oos(S, xs, "mig_gross", from), 4))) }
  cat("\nDiagnostics (p-values), bounds test, long-run projection and recursive OOS RMSE:\n"); print(out, row.names = FALSE) }
if (!exists("TAXBEST")) TAXBEST <- "L_tax_voter"; cat("\nTax measure carried forward:", TAXBEST, "(set TAXBEST before sourcing to change)\n"); BASE0 <- c(CORE, TAXBEST)
# --- B sweep ----------------------------------------------------------------------------------------------------------------------------
cat("\n\n##### B. Every other variable added to the conceptual model (", sub("^L_", "", TAXBEST), "): Sam's two-part criterion #####\n", sep = "")
CAND <- c("L_unem", "L_unem_boe", "L_ln_vac_wap", "L_dln_vac_unem", "L_wage_infl", "L_real_wage", "L_union", "L_replace", "L_growth", "L_growth_pc", "L_tfp", "L_lab_prod", "L_particip", "growth_fc",
          "L_nom_eri", "L_ln_nom_eri", "L_real_eri", "L_ln_real_eri", "L_openness", "L_ca_gdp", "L_trade_bal", "L_tot", "L_ln_tot", "L_inflation", "L_cpi_infl", "st_inf_exp", "lt_inf_exp", "inf_exp_1y", "inf_exp_5_10y", "L_m4_growth",
          "L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_tme_real_growth", "gad_earn", "gad_policy", "L_psnb_gdp", "L_psnd_gdp", "L_ln_psnd_gdp", "fc_budget", "fc_medium", "fc_planned", "L_tax_ceiling", "golden_rule", "obr_era", "boe_indep",
          "L_int_gdp", "L_bank_rate", "L_tbill", "L_gilt", "L_consol", "L_r_g", "trend", "L_emig_all", "L_emig_brit", "brit_rate", "eu_rate", "L_relinc_dev", "L_relinc_rich", "L_youth", "L_real_house", "L_labour_share", "L_days_lost", "L_ln_wap", "oadr_0", "delta_dr_10", "level_dr_10", "L_permits")
res <- NULL
for (v in CAND) { row <- data.frame(control = sub("^L_", "", v)); ok <- TRUE
  for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); if (!(v %in% names(S))) { ok <- FALSE; break }
    m0 <- fit(S, BASE0, "mig_gross"); m1 <- fit(S, c(BASE0, "delta_oadr_10"), "mig_gross"); a0 <- fit(S, c(BASE0, v), "mig_gross"); a1 <- fit(S, c(BASE0, "delta_oadr_10", v), "mig_gross"); if (is.null(a0) || is.null(a1)) { ok <- FALSE; break }
    m0 <- lm(formula(m0), data = attr(a0, "dd")); m1 <- lm(formula(m1), data = attr(a1, "dd"))   # same observations as the augmented model
    ct <- coeftest(a1, vcov. = vcovHAC(a1)); sfx <- if (k == "extended") "_ext" else "_core"
    row[[paste0("dAIC0", sfx)]] <- round(AIC(a0) - AIC(m0), 2); row[[paste0("dAIC1", sfx)]] <- round(AIC(a1) - AIC(m1), 2); row[[paste0("b", sfx)]] <- round(ct[v, 1], 4); row[[paste0("p", sfx)]] <- round(ct[v, 4], 3)
    row[[paste0("proj", sfx)]] <- round(ct["delta_oadr_10", 1], 4); row[[paste0("reset", sfx)]] <- round(resettest(a1)$p.value, 3); row[[paste0("bg1", sfx)]] <- round(bgtest(a1, order = 1)$p.value, 3); row[[paste0("N", sfx)]] <- nobs(a1) }
  if (ok) res <- rbind(res, row) }
res$pass <- with(res, dAIC0_ext < 0 & dAIC1_ext < 0 & dAIC0_core < 0 & dAIC1_core < 0); res <- res[order(res$dAIC1_ext + res$dAIC1_core), ]
cat("\ndAIC0 = AIC change from adding the control WITHOUT the projection; dAIC1 = WITH the projection (negative = better); b, p = the control's HAC coefficient and p-value beside the projection; proj = the projection beside it.\n")
cat("Controls passing both halves of the criterion on BOTH samples:", if (any(res$pass)) paste(res$control[res$pass], collapse = ", ") else "none", "\n\n"); print(res, row.names = FALSE)
# --- C balance of payments, terms of trade, exchange rates -----------------------------------------------------------------------------
cat("\n\n##### C. Robustness: the external position in the conceptual model #####\n")
EXT <- c("L_ca_gdp", "L_trade_bal", "L_tot", "L_nom_eri", "L_real_eri")
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019)
  p <- c(list("main" = fit(S, c(BASE0, "delta_oadr_10"), "mig_gross")), setNames(lapply(EXT, function(v) fit(S, c(BASE0, "delta_oadr_10", v), "mig_gross")), paste0("+", sub("^L_", "", EXT))))
  panel(p, paste0("C. The external position added to the conceptual model (", SMP[[k]]$LAB, "2019)"), c("delta_oadr_10", EXT, TAXBEST, "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme", "L_mig_gross"), w = 13, lw = 16,
        c("ca_gdp: current account balance, % of GDP; trade_bal: trade balance, % of GDP; tot: terms of trade; nom_eri / real_eri: nominal and real sterling effective exchange rate",
          "indices (Millennium A33/A1, BIS 2017-19), all t-1. The balance of payments was the Treasury's binding constraint before 1972 (Dell, 1991; Brittan, 1969).")) }

# --- D. Which controls make the NO-projection conceptual model pass its diagnostics? -------------------------------------------------
cat("\n\n##### D. Diagnostics of the NO-projection conceptual model (", sub("^L_", "", TAXBEST), ") with each control added #####\n", sep = "")
cat("Columns: the control's HAC p-value in the no-projection model; RESET(2,3) and RESET(2) p; BG(1), BG(2) p; rainbow p; 'pass' = RESET(2,3) > .05 & BG(1) > .05 & BG(2) > .05 & rainbow > .05.\n")
cat("proj_p = the projection's HAC p-value when then added on top of that model; dAICp = its AIC change (negative = improves).\n")
resD <- NULL
for (v in CAND) { row <- data.frame(control = sub("^L_", "", v)); ok <- TRUE
  for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); if (!(v %in% names(S))) { ok <- FALSE; break }
    a0 <- fit(S, c(BASE0, v), "mig_gross"); a1 <- fit(S, c(BASE0, v, "delta_oadr_10"), "mig_gross"); if (is.null(a0) || is.null(a1)) { ok <- FALSE; break }
    a0 <- lm(formula(a0), data = attr(a1, "dd"))
    ct0 <- coeftest(a0, vcov. = vcovHAC(a0)); ct1 <- coeftest(a1, vcov. = vcovHAC(a1)); sfx <- if (k == "extended") "_ext" else "_core"
    r23 <- resettest(a0)$p.value; r2 <- resettest(a0, power = 2)$p.value; b1 <- bgtest(a0, order = 1)$p.value; b2 <- bgtest(a0, order = 2)$p.value; rb <- raintest(a0)$p.value
    row[[paste0("p", sfx)]] <- round(ct0[v, 4], 3); row[[paste0("RESET23", sfx)]] <- round(r23, 3); row[[paste0("RESET2", sfx)]] <- round(r2, 3)
    row[[paste0("BG1", sfx)]] <- round(b1, 3); row[[paste0("BG2", sfx)]] <- round(b2, 3); row[[paste0("rain", sfx)]] <- round(rb, 3)
    row[[paste0("pass", sfx)]] <- r23 > .05 & b1 > .05 & b2 > .05 & rb > .05
    row[[paste0("proj_p", sfx)]] <- round(ct1["delta_oadr_10", 4], 3); row[[paste0("dAICp", sfx)]] <- round(AIC(a1) - AIC(a0), 1) }
  if (ok) resD <- rbind(resD, row) }
for (k in c("ext", "core")) { S0 <- subset(SMP[[if (k == "ext") "extended" else "core"]]$df, year >= SMP[[if (k == "ext") "extended" else "core"]]$Y0 & year <= 2019)
  m0 <- fit(S0, BASE0, "mig_gross"); cat(sprintf("\nNo-projection model without any addition (%s): RESET(2,3) %.3f, RESET(2) %.3f, BG(1) %.3f, BG(2) %.3f, rainbow %.3f\n", k,
    resettest(m0)$p.value, resettest(m0, power = 2)$p.value, bgtest(m0, order = 1)$p.value, bgtest(m0, order = 2)$p.value, raintest(m0)$p.value)) }
cat("\nControls under which the no-projection model passes on BOTH samples:", { z <- resD$control[resD$pass_ext & resD$pass_core]; if (length(z)) paste(z, collapse = ", ") else "none" }, "\n")
cat("Passes on the extended sample only:", { z <- resD$control[resD$pass_ext & !resD$pass_core]; if (length(z)) paste(z, collapse = ", ") else "none" }, "\n")
cat("Passes on the core sample only:", { z <- resD$control[!resD$pass_ext & resD$pass_core]; if (length(z)) paste(z, collapse =, ", ") else "none" }, "\n\n")
resD <- resD[order(-(resD$pass_ext + resD$pass_core), -(resD$RESET23_ext + resD$RESET23_core)), ]
print(resD, row.names = FALSE)
