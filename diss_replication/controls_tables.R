# =============================================================================
# controls_tables.R -- MANUSCRIPT: further economic and fiscal controls in categorical tables, Online Appendix Tables B18-B24,
# either sample (SAMPLE <- "1964" / "1977"). Every column adds the control named to the fiscal specification ("main"), with
# unemployment retained (Sam, 27 Sep 2026); logs are used where the log form is the one that fits (vacancies, Sam 28 Sep 2026).
# Main results run to 2019 (Millennium series spliced for 2017-19; see determinants/SOURCES_2017_2019.md); columns using a
# spliced series are re-estimated to 2016 under each table; series that cannot be extended end in 2016 (N row).
#   B18 Labour market        +ln_vac_wap | +ln_vac_unem | +dln_vac_unem | +wage_infl | +real_wage | +union | +replace
#   B19 Growth               +growth | +growth_pc | +tfp | +lab_prod | +particip | +growth_fc
#   B20 Trade                +real_eri | +openness | +ca_gdp | +trade_bal | +tot
#   B21 Inflation            +inflation | +cpi_infl | +st_inf_exp | +lt_inf_exp | +inf_exp_1y | +inf_exp_5_10y | +m4_growth
#   B22 Spending             +grants_gdp | +grants_wap | +tme_gdp | +tme_real_growth | +gad_earn | +gad_policy
#   B23 Borrowing and rules  +psnb_gdp | psnb_gdp for ln_debt_gdp | +psnd_gdp | +fc_budget | +fc_medium | +tax_ceiling | +golden_rule | +obr_era
#   B24 Debt dynamics        +int_tme | +int_gdp | +bank_rate | +tbill | +gilt | +consol | +r_g
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, "2019")
swap <- function(base, drop, add) c(setdiff(base, drop), add)
add_tab <- function(vars, title, notes = NULL, w = 13, lw = 18, extra = list()) {
  p <- c(list("main" = fit(S, BASE)), setNames(lapply(vars, function(v) fit(S, c(BASE, v))), paste0("+", sub("^L_", "", vars))), extra)
  panel(p, title, unique(c("delta_oadr_10", vars, unlist(lapply(extra, function(m) setdiff(names(coef(m)), c("(Intercept)", BASE)))), "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP)), w = w, lw = lw, notes = notes) }
line <- function(k, m, vs) { if (is.null(m)) return(invisible()); ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-26s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f\n", nobs(m), AIC(m))); defs(vs) }
cat("\n##### Further economic and fiscal controls (", TAG, "): each column adds one control to the fiscal specification (main); unemployment retained #####\n", sep = "")
add_tab(c("L_ln_vac_wap", "L_ln_vac_unem", "L_dln_vac_unem", "L_wage_infl", "L_real_wage", "L_union", "L_replace"), paste0("Table B18. Labour market controls (", TAG, ")"),
        c("Vacancies enter in logs: the log form is the one that improves the fit both without and with the projection (private tests); L_unem and log vacancies are near-collinear",
          "(the vacancy/unemployment ratio is their quotient), so unemployment loses precision beside them. Union density and the replacement ratio end in 2016."))
line("+vac_wap (level)", fit(S, c(BASE, "L_vac_wap")), c("delta_oadr_10", "L_vac_wap")); line("+vac_unem (level)", fit(S, c(BASE, "L_vac_unem")), c("delta_oadr_10", "L_vac_unem"))
add_tab(c("L_growth", "L_growth_pc", "L_tfp", "L_lab_prod", "L_particip", "growth_fc"), paste0("Table B19. Growth, productivity and participation (", TAG, ")"),
        "growth_fc is the Treasury's own start-of-year forecast for the current year and enters contemporaneously, as the projection does; every other control is lagged one year.")
add_tab(c("L_real_eri", "L_openness", "L_ca_gdp", "L_trade_bal", "L_tot"), paste0("Table B20. Trade and the external balance (", TAG, ")"),
        "Openness ends in 2016; the exchange rate, balance of payments and terms of trade are spliced to 2019 (BIS, ONS).")
add_tab(c("L_inflation", "L_cpi_infl", "st_inf_exp", "lt_inf_exp", "inf_exp_1y", "inf_exp_5_10y", "L_m4_growth"), paste0("Table B21. Inflation, expectations and money (", TAG, ")"),
        "Inflation expectations are start-of-year information and enter contemporaneously; realised inflation and money growth are lagged one year. inf_exp_5_10y begins in 1985.")
add_tab(c("L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_tme_real_growth", "gad_earn", "gad_policy"), paste0("Table B22. Current and projected spending (", TAG, ")"),
        c("Realised welfare spending and total spending (Millennium A28, to 2016) and the Government Actuary's projected contribution pressure (Quinquennial Reviews in force by base year,",
          "entered contemporaneously like the population projection; gad_policy = under the uprating policy in force, which projected the required rate to FALL from 1982 to 2010)."))
add_tab(c("L_psnb_gdp", "L_psnd_gdp", "fc_budget", "fc_medium", "L_tax_ceiling", "golden_rule", "obr_era"), paste0("Table B23. Borrowing, borrowing plans and fiscal rules (", TAG, ")"),
        c("psnb_gdp for ln_debt_gdp: the borrowing ratio in place of the change in the debt ratio. Budget-of-year-t forecasts (OBR historical official forecasts, from 1971) are start-of-year",
          "information and enter contemporaneously; golden_rule and obr_era are the fiscal-rule regimes (Bank independence from 1998 coincides with golden_rule's start and is in the private tests)."),
        extra = list("psnb_gdp for ln_debt_gdp" = fit(S, swap(BASE, "L_ln_debt_gdp", "L_psnb_gdp"))))
line("+fc_planned", fit(S, c(BASE, "fc_planned")), c("delta_oadr_10", "fc_planned")); line("+boe_indep", fit(S, c(BASE, "boe_indep")), c("delta_oadr_10", "boe_indep"))
add_tab(c("L_int_tme", "L_int_gdp", "L_bank_rate", "L_tbill", "L_gilt", "L_consol", "L_r_g"), paste0("Table B24. Debt dynamics and the cost of borrowing (", TAG, ")"),
        c("Debt interest as a share of spending and of GDP (A28); short rates: Bank Rate and the three-month Treasury bill rate; long rates: the 10-year gilt yield and the consol yield",
          "(Millennium A31, annual averages, to 2016); r_g = the 10-year yield minus nominal GDP growth. Debt interest is null without the projection and positive beside it (private tests)."))
