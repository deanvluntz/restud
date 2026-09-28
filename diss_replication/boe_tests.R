# =============================================================================
# boe_tests.R -- MANUSCRIPT: further economic controls, part 1 (labour market and macroeconomy), either sample. Set SAMPLE first:
#     SAMPLE <- "1964"; source("boe_tests.R")   # extended sample, 1965-2019
#     SAMPLE <- "1977"; source("boe_tests.R")   # core sample, 1977-2019
# (wrappers extension_1964_boe.R / core_1977_boe.R do this). Main results run to 2019 with the Millennium series extended by
# ONS/BoE figures for 2017-19 (Sam, 27 Sep 2026); every column that uses an extended series is re-estimated to 2016 under its
# panel. Series not extendable end in 2016 (the N row shows it). Names: tables_common.R; data: data_common.R.
# The tax-burden, unemployment-measure and gross-inflow panels moved to robustness_tables.R (Sam, 28 Sep 2026).
#   F1  Labour demand, unemployment retained   baseline | +vac_wap | +vac_unem | +dln_vac_unem   (the 'for unem' substitutions as text lines)
#   F2  Wages, rates, expectations, productivity, institutions   one added control per column
#   F3  All labour-market and macro controls together
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= BOE_END); TAG <- paste0(LAB, BOE_END)
cat("SAMPLE", SAMPLE, "| BoE series end", BOE_END, "| N =", nrow(S), "\n")
swap <- function(base, drop, add) c(setdiff(base, drop), add)
line <- function(k, m, vs) { ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-28s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f\n", nobs(m), AIC(m))) }
cat("\n##### Further economic and fiscal controls, part 1: the labour market and the macroeconomy (", TAG, ") #####\n", sep = "")
# --- F1 labour demand, unemployment retained --------------------------------------------------------
p1 <- list("baseline" = fit(S, BASE), "+vac_wap" = fit(S, c(BASE, "L_vac_wap")), "+vac_unem" = fit(S, c(BASE, "L_vac_unem")), "+dln_vac_unem" = fit(S, c(BASE, "L_dln_vac_unem")))
panel(p1, paste0("F1. Labour demand beside unemployment (", TAG, ")"), c("delta_oadr_10", "L_unem", "L_vac_wap", "L_vac_unem", "L_dln_vac_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 18,
      c("Unemployment is retained in every column: it is the documented Treasury input (dropping it for vacancies was rejected as data-driven). L_unem and L_vac_wap in levels",
        "are collinear (the vacancy/unemployment ratio is their quotient), so both lose significance together in column 2; the change in log(vacancies/unemployed) is the",
        "labour-demand measure that can be held beside the unemployment rate."))
cat("Substitutions, for reference only (not a candidate specification):\n")
line("vac_wap for unem", fit(S, swap(BASE, "L_unem", "L_vac_wap")), c("delta_oadr_10", "L_vac_wap")); line("vac_unem for unem", fit(S, swap(BASE, "L_unem", "L_vac_unem")), c("delta_oadr_10", "L_vac_unem"))
# --- F2 wages, rates, expectations, productivity, institutions ---------------------------------------
ctl2 <- c("L_wage_infl", "L_real_wage", "L_tbill", "L_real_rate", "st_inf_exp", "lt_inf_exp", "L_tfp", "L_replace", "L_union")
p2 <- c(list("baseline" = fit(S, BASE)), setNames(lapply(ctl2, function(v) fit(S, c(BASE, v))), paste0("+", sub("^L_", "", ctl2))))
panel(p2, paste0("F2. Wages, interest rates, expectations, productivity, institutions (", TAG, ")"), c("delta_oadr_10", ctl2, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 12, lw = 15,
      "Inflation expectations are start-of-year information and enter contemporaneously; every other control is lagged one year.")
# --- F3 all together ------------------------------------------------------------------------------------
p3 <- list("baseline" = fit(S, BASE), "+six" = fit(S, c(BASE, SIX)), "+six +emig_all" = fit(S, c(BASE, SIX, "L_emig_all")), "+six +dln_vac_unem" = fit(S, c(BASE, SIX, "L_dln_vac_unem")))
panel(p3, paste0("F3. All labour-market and macroeconomic controls together (", TAG, ")"), c("delta_oadr_10", SIX, "L_emig_all", "L_dln_vac_unem", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 19,
      "six = L_vac_unem, L_wage_infl, L_tbill, st_inf_exp, L_tfp, L_vac_wap. VIF above 30: read the individual controls as signs only.")
