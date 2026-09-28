# =============================================================================
# boe_tests.R -- MANUSCRIPT: the Bank of England Millennium controls, either sample. Set SAMPLE first:
#     SAMPLE <- "1964"; source("boe_tests.R")   # extended sample, 1965-2019
#     SAMPLE <- "1977"; source("boe_tests.R")   # core sample, 1977-2019
# (wrappers extension_1964_boe.R / core_1977_boe.R do this). Main results run to 2019 with the Millennium series extended
# by ONS/BoE figures for 2017-19 (Sam, 27 Sep 2026); every column that uses an extended series is re-estimated to 2016
# under its panel. Series not extendable end in 2016 (the N row shows it). Names: tables_common.R; data: data_common.R.
#   1a  Unemployment measures       L_unem (ILO, baseline) | unem_head for unem | unem_head +post98 | unem_boe for unem | unem_claim for unem
#   1b  Vacancies                   baseline | +vac_wap | +vac_unem | +dln_vac_unem | vac_wap for unem | vac_unem for unem
#   2   Gross inflows and emigration  mig_gross | +emig_all | +emig_brit | +emig_all +vac_unem | mig_rate +emig_all
#   3   Wages, rates, expectations, productivity, institutions   one added control per column
#   4   The tax burden by type (A27)  each measure in place of L_tax_workpop
#   5   All labour-market and macro controls together
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= BOE_END); TAG <- paste0(LAB, BOE_END)
cat("SAMPLE", SAMPLE, "| BoE series end", BOE_END, "| N =", nrow(S), "\n")
swap <- function(base, drop, add) c(setdiff(base, drop), add)

# --- 1a unemployment measures -----------------------------------------------------------------
p1a <- list("L_unem (baseline)" = fit(S, BASE), "unem_head for unem" = fit(S, swap(BASE, "L_unem", "L_unem_head")),
            "unem_head +post98" = fit(S, swap(BASE, "L_unem", c("L_unem_head", "L_post98"))), "unem_boe for unem" = fit(S, swap(BASE, "L_unem", "L_unem_boe")), "unem_claim for unem" = fit(S, swap(BASE, "L_unem", "L_unem_claim")))
panel(p1a, paste0("1a. Unemployment measures (", TAG, ")"), c("delta_oadr_10", "L_unem", "L_unem_head", "L_post98", "L_unem_boe", "L_unem_claim", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 20,
      c("L_unem_head follows the information set: the measure published as the headline in each year. L_post98 absorbs the level shift when the",
        "headline changed from the claimant count to the ILO rate. The claimant count ends in 2016 (ONS discontinued the consistent series under Universal Credit)."))
# --- 1b vacancies -------------------------------------------------------------------------------
p1b <- list("baseline" = fit(S, BASE), "+vac_wap" = fit(S, c(BASE, "L_vac_wap")), "+vac_unem" = fit(S, c(BASE, "L_vac_unem")), "+dln_vac_unem" = fit(S, c(BASE, "L_dln_vac_unem")),
            "vac_wap for unem" = fit(S, swap(BASE, "L_unem", "L_vac_wap")), "vac_unem for unem" = fit(S, swap(BASE, "L_unem", "L_vac_unem")))
panel(p1b, paste0("1b. Vacancies beside and in place of unemployment (", TAG, ")"), c("delta_oadr_10", "L_unem", "L_vac_wap", "L_vac_unem", "L_dln_vac_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 18,
      c("L_unem and L_vac_wap in levels are collinear (the vacancy/unemployment ratio is their quotient), so both lose significance together in column 2;",
        "the change in log(vacancies/unemployed) is the labour-demand measure that can be held beside the unemployment rate."))
# --- 2 gross inflows and emigration ---------------------------------------------------------------
GB <- swap(BASE, "L_mig_rate", "L_mig_gross")
p2 <- list("mig_gross" = fit(S, GB, "mig_gross"), "+emig_all" = fit(S, c(GB, "L_emig_all"), "mig_gross"), "+emig_brit" = fit(S, c(GB, "L_emig_brit"), "mig_gross"),
           "+emig_all +vac_unem" = fit(S, c(GB, "L_emig_all", "L_vac_unem"), "mig_gross"), "mig_rate +emig_all" = fit(S, c(BASE, "L_emig_all")))
panel(p2, paste0("2. Gross inflows, with lagged emigration (", TAG, ")"), c("delta_oadr_10", "L_emig_all", "L_emig_brit", "L_vac_unem", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_gross", "L_mig_rate", STEP), w = 20,
      "Columns 1-4: dependent variable mig_gross; column 5: mig_rate. The tax term survives without any population term on the left-hand side.")
# --- 3 wages, rates, expectations, productivity, institutions -------------------------------------
ctl3 <- c("L_wage_infl", "L_real_wage", "L_tbill", "L_real_rate", "st_inf_exp", "lt_inf_exp", "L_tfp", "L_replace", "L_union")
p3 <- c(list("baseline" = fit(S, BASE)), setNames(lapply(ctl3, function(v) fit(S, c(BASE, v))), paste0("+", sub("^L_", "", ctl3))))
panel(p3, paste0("3. Wages, interest rates, expectations, productivity, institutions (", TAG, ")"), c("delta_oadr_10", ctl3, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 12, lw = 15,
      "Inflation expectations are start-of-year information and enter contemporaneously; every other control is lagged one year.")
# --- 4 the tax burden by type -----------------------------------------------------------------------
tv <- c("L_tax_inc", "L_tax_nics", "L_tax_nics_emp", "L_tax_nics_er", "L_tax_inc_nics", "L_tax_direct", "L_tax_indirect", "L_tax_vat", "L_tax_corp", "L_tax_receipts")
p4 <- c(list("L_tax_workpop" = fit(S, BASE)), setNames(lapply(tv, function(v) fit(S, swap(BASE, "L_tax_workpop", v))), sub("^L_", "", tv)))
panel(p4, paste0("4. The tax burden by type, each in place of L_tax_workpop (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", tv, "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14, lw = 15,
      "A27 receipts are central government only; L_tax_workpop is the national accounts total. VAT restricts the sample to 1975 on.")
# --- 5 all together --------------------------------------------------------------------------------
p5 <- list("baseline" = fit(S, BASE), "+six" = fit(S, c(BASE, SIX)), "+six +emig_all" = fit(S, c(BASE, SIX, "L_emig_all")), "+six +dln_vac_unem" = fit(S, c(BASE, SIX, "L_dln_vac_unem")))
panel(p5, paste0("5. All labour-market and macroeconomic controls together (", TAG, ")"), c("delta_oadr_10", SIX, "L_emig_all", "L_dln_vac_unem", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 19,
      "six = L_vac_unem, L_wage_infl, L_tbill, st_inf_exp, L_tfp, L_vac_wap. VIF above 30: read the individual controls as signs only.")
