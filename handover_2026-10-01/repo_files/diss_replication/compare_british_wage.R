# =============================================================================
# compare_british_wage.R -- EXPLORATORY (Sam, 29 Sep 2026). Four questions:
#   A  does wage inflation (or any other variable) make the projection insignificant in the BRITISH-inflow equation (the partial
#      placebo failure)?  Published British equation and its conceptual-model form, each control added, both samples.
#   B  in the gross-inflow equation with brit_rate as a control, does adding wage inflation (or another control) make brit_rate null?
#   C  what does wage inflation carry?  Decomposition beside deflator inflation, CPI inflation and real-wage growth (the tax burden is
#      already real: nominal taxes / GDP deflator / adults).
#   D  a less strict cross-sample rule: pass on the EXTENDED sample, with the core coefficient of the same sign and inside the extended
#      95% HAC interval (coefficient stability rather than significance on both).  Which controls pass under that rule?
# No bootstrap; needs only the workbook; RStudio: working directory = diss_replication/.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R"); suppressPackageStartupMessages({library(ARDL); library(strucchange); library(tseries)})
DEFS[["L_ln_vac_unem"]] <- "log of the vacancy/unemployment ratio, t-1"; DEFS[["L_ln_int_tme"]] <- "log of public sector debt interest as a share of total managed expenditure, t-1"
DEFS[["L_nom_eri"]] <- "nominal sterling effective exchange rate index, t-1 (Millennium A33; BIS for 2017-19)"; DEFS[["L_ln_nom_eri"]] <- "log of L_nom_eri"
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); df$ln_int_tme <- log(df$int_tme)
  df$L_nom_eri <- lag1(df$nominal_eri); df$L_ln_nom_eri <- log(df$L_nom_eri); df$nom_eri <- df$nominal_eri; list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
CONC <- c("L_mig_gross", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme", "L_tax_voter")
PUBC <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
CAND <- c("L_wage_infl", "L_unem", "L_unem_boe", "L_ln_vac_wap", "L_ln_vac_unem", "L_dln_vac_unem", "L_real_wage", "L_union", "L_replace", "L_growth", "L_growth_pc", "L_tfp", "L_lab_prod", "L_particip", "growth_fc",
          "L_nom_eri", "L_real_eri", "L_openness", "L_ca_gdp", "L_trade_bal", "L_tot", "L_inflation", "L_cpi_infl", "st_inf_exp", "lt_inf_exp", "L_m4_growth",
          "L_grants_gdp", "L_tme_gdp", "L_tme_real_growth", "gad_earn", "gad_policy", "L_psnb_gdp", "L_psnd_gdp", "fc_budget", "fc_medium", "fc_planned", "L_tax_ceiling", "golden_rule", "obr_era", "boe_indep",
          "L_int_gdp", "L_ln_int_tme", "L_bank_rate", "L_tbill", "L_gilt", "L_consol", "L_r_g", "trend", "L_emig_all", "L_emig_brit", "eu_rate", "L_relinc_dev", "L_relinc_rich", "L_youth", "L_real_house", "L_labour_share", "L_days_lost", "oadr_0", "delta_dr_10", "level_dr_10", "L_permits",
          "cit_grants_k", "cit_nat_k", "cit_reg_k")
hac <- function(m) coeftest(m, vcov. = vcovHAC(m))
# --- A British-inflow equation ---------------------------------------------------------------------------------------------------------
cat("\n##### A. The British-inflow equation with each control added: does the projection survive? #####\n")
for (form in c("published", "conceptual")) { CT <- if (form == "published") PUBC else setdiff(CONC, "L_mig_gross"); BR <- c("L_brit_rate", "delta_oadr_10", CT)
  cat("\n=== British equation, ", form, " controls: brit_rate ~ L_brit_rate + delta_oadr_10 + ", paste(CT, collapse = " + "), " ===\n", sep = "")
  res <- NULL
  for (v in c("(none)", CAND)) { if (v %in% BR) next; row <- data.frame(control = sub("^L_", "", v)); ok <- TRUE
    for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); xs <- if (v == "(none)") BR else c(BR, v); if (v != "(none)" && !(v %in% names(S))) { ok <- FALSE; break }
      m <- fit(S, xs, "brit_rate"); if (is.null(m)) { ok <- FALSE; break }; ct <- hac(m); sfx <- if (k == "extended") "_ext" else "_core"
      row[[paste0("proj", sfx)]] <- round(ct["delta_oadr_10", 1], 4); row[[paste0("proj_p", sfx)]] <- round(ct["delta_oadr_10", 4], 3)
      row[[paste0("b", sfx)]] <- if (v == "(none)") NA else round(ct[v, 1], 4); row[[paste0("p", sfx)]] <- if (v == "(none)") NA else round(ct[v, 4], 3); row[[paste0("N", sfx)]] <- nobs(m) }
    if (ok) res <- rbind(res, row) }
  res$proj_ns_ext <- res$proj_p_ext > .10; res$proj_ns_core <- res$proj_p_core > .10
  cat("Controls under which the projection is insignificant (p > .10) in the British equation -- extended:", paste(res$control[res$proj_ns_ext & res$control != "(none)"], collapse = ", "), "\n")
  cat("                                                                                       -- core:    ", paste(res$control[res$proj_ns_core & res$control != "(none)"], collapse = ", "), "\n")
  cat("Controls significant at 10% in the British equation on BOTH samples:", paste(res$control[!is.na(res$p_ext) & res$p_ext < .10 & res$p_core < .10], collapse = ", "), "\n\n")
  print(res[order(res$proj_p_ext + res$proj_p_core, decreasing = TRUE), ], row.names = FALSE) }
# --- B gross equation + brit_rate + control -----------------------------------------------------------------------------------------------
cat("\n\n##### B. Gross-inflow conceptual model + brit_rate + each control: does brit_rate stay significant? #####\n")
BASE1 <- c(CONC, "delta_oadr_10", "brit_rate"); res <- NULL
for (v in c("(none)", CAND)) { if (v %in% BASE1) next; row <- data.frame(control = sub("^L_", "", v)); ok <- TRUE
  for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); xs <- if (v == "(none)") BASE1 else c(BASE1, v); if (v != "(none)" && !(v %in% names(S))) { ok <- FALSE; break }
    m <- fit(S, xs, "mig_gross"); if (is.null(m)) { ok <- FALSE; break }; ct <- hac(m); sfx <- if (k == "extended") "_ext" else "_core"
    row[[paste0("brit", sfx)]] <- round(ct["brit_rate", 1], 4); row[[paste0("brit_p", sfx)]] <- round(ct["brit_rate", 4], 3); row[[paste0("proj", sfx)]] <- round(ct["delta_oadr_10", 1], 4); row[[paste0("proj_p", sfx)]] <- round(ct["delta_oadr_10", 4], 3)
    row[[paste0("b", sfx)]] <- if (v == "(none)") NA else round(ct[v, 1], 4); row[[paste0("p", sfx)]] <- if (v == "(none)") NA else round(ct[v, 4], 3) }
  if (ok) res <- rbind(res, row) }
cat("Controls under which brit_rate becomes insignificant (p > .10) -- extended:", paste(res$control[res$brit_p_ext > .10 & res$control != "(none)"], collapse = ", "), "\n")
cat("                                                                -- core:    ", paste(res$control[res$brit_p_core > .10 & res$control != "(none)"], collapse = ", "), "\n\n")
print(res[order(res$brit_p_ext + res$brit_p_core, decreasing = TRUE), ], row.names = FALSE)
# --- C what does wage inflation carry? --------------------------------------------------------------------------------------------------
cat("\n\n##### C. Wage inflation beside price inflation and real-wage growth (conceptual model with the projection) #####\n")
B1 <- c(CONC, "delta_oadr_10")
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019)
  p <- list("main" = fit(S, B1, "mig_gross"), "+wage_infl" = fit(S, c(B1, "L_wage_infl"), "mig_gross"), "+inflation" = fit(S, c(B1, "L_inflation"), "mig_gross"), "+cpi_infl" = fit(S, c(B1, "L_cpi_infl"), "mig_gross"),
            "+real_wage" = fit(S, c(B1, "L_real_wage"), "mig_gross"), "+wage_infl +inflation" = fit(S, c(B1, "L_wage_infl", "L_inflation"), "mig_gross"), "+inflation +real_wage" = fit(S, c(B1, "L_inflation", "L_real_wage"), "mig_gross"),
            "+wage_infl +real_wage" = fit(S, c(B1, "L_wage_infl", "L_real_wage"), "mig_gross"))
  panel(p, paste0("C. Wage inflation decomposed (", SMP[[k]]$LAB, "2019)"), c("delta_oadr_10", "L_wage_infl", "L_inflation", "L_cpi_infl", "L_real_wage", "L_tax_voter", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme", "L_mig_gross"), w = 13, lw = 16, nested_F = TRUE,
        "Nominal wage growth = price inflation + real-wage growth (approximately). If wage inflation only proxied prices, +inflation would do its work and wage_infl would be null beside it.")
  for (nm in names(p)) if (!is.null(p[[nm]])) cat(sprintf("  %-24s AIC %8.2f\n", nm, AIC(p[[nm]]))) }
# --- D coefficient-stability rule ----------------------------------------------------------------------------------------------------------
cat("\n\n##### D. A less strict rule: pass on the extended sample; core coefficient same sign and inside the extended 95% HAC interval #####\n")
cat("z_diff = |b_ext - b_core| / sqrt(se_ext^2 + se_core^2), a rough equality statistic (the samples overlap, so it is indicative only; > 1.96 = the two estimates differ).\n"); cat("ext_pass = dAIC0 < 0 & dAIC1 < 0 & p < .10 on the extended sample; stable = core b same sign and within [b_ext - 1.96 se, b_ext + 1.96 se]; core_pass = the two-part criterion on the core (the strict rule).\n\n")
res <- NULL
for (v in CAND) { if (v %in% B1) next; row <- data.frame(control = sub("^L_", "", v)); ok <- TRUE
  for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); if (!(v %in% names(S))) { ok <- FALSE; break }
    a0 <- fit(S, c(CONC, v), "mig_gross"); a1 <- fit(S, c(B1, v), "mig_gross"); if (is.null(a0) || is.null(a1)) { ok <- FALSE; break }
    m0 <- lm(reformulate(CONC, "mig_gross"), data = attr(a0, "dd")); m1 <- lm(reformulate(B1, "mig_gross"), data = attr(a1, "dd")); ct <- hac(a1); sfx <- if (k == "extended") "_ext" else "_core"
    row[[paste0("dAIC0", sfx)]] <- round(AIC(a0) - AIC(m0), 1); row[[paste0("dAIC1", sfx)]] <- round(AIC(a1) - AIC(m1), 1); row[[paste0("b", sfx)]] <- round(ct[v, 1], 4); row[[paste0("se", sfx)]] <- round(ct[v, 2], 4); row[[paste0("p", sfx)]] <- round(ct[v, 4], 3); row[[paste0("proj", sfx)]] <- round(ct["delta_oadr_10", 1], 4) }
  if (ok) res <- rbind(res, row) }
res$ext_pass <- with(res, dAIC0_ext < 0 & dAIC1_ext < 0 & p_ext < .10); res$stable <- with(res, sign(b_core) == sign(b_ext) & b_core >= b_ext - 1.96 * se_ext & b_core <= b_ext + 1.96 * se_ext)
res$z_diff <- with(res, round(abs(b_ext - b_core) / sqrt(se_ext^2 + se_core^2), 2)); res$core_pass <- with(res, dAIC0_core < 0 & dAIC1_core < 0); res$new_rule <- res$ext_pass & res$stable; res$strict <- res$ext_pass & res$core_pass
cat("Pass under the coefficient-stability rule:", paste(res$control[res$new_rule], collapse = ", "), "\n"); cat("Pass under the strict both-samples rule:", paste(res$control[res$strict], collapse = ", "), "\n")
cat("Pass on the extended sample but NOT stable on the core (a pre-1977 effect or an artefact):", paste(res$control[res$ext_pass & !res$stable], collapse = ", "), "\n\n")
print(res[order(-res$new_rule, -res$ext_pass, res$dAIC1_ext), c("control", "dAIC0_ext", "dAIC1_ext", "b_ext", "se_ext", "p_ext", "proj_ext", "b_core", "se_core", "p_core", "proj_core", "dAIC0_core", "dAIC1_core", "z_diff", "ext_pass", "stable", "core_pass", "new_rule")], row.names = FALSE)
