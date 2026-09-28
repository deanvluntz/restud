# =============================================================================
# gad_obr_tests.R -- MANUSCRIPT: further fiscal controls, part 3 -- can a FORWARD-LOOKING, NON-DEMOGRAPHIC fiscal
# pressure do the projection's work? Either sample (SAMPLE <- "1964" / "1977"). Names and definitions: tables_common.R; data: data_common.R.
#   F6 Government Actuary's projected contribution pressure (Quinquennial Reviews 1960-2017; series built by
#      determinants/gad/build_gad_series.py, vintage by review base year as for the population projections; entered
#      contemporaneously like delta_oadr_10)          baseline | +gad_earn | gad_earn only | +gad_policy | gad_policy only [| +gad_gap | gad_gap only]
#   F7 Official borrowing forecasts (OBR database)   baseline | +fc_budget | +L_fc_budget | +fc_planned | +fc_medium | fc_budget only | fc_medium only
#   F8 Debt dynamics (BoE Millennium, to 2016)       baseline | +int_gdp | +int_tme | +gilt | +r_g | int_tme only | r_g only
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= 2019); S16 <- subset(df, year >= Y0 & year <= BOE_END); TAG <- paste0(LAB, "2019")
swap <- function(base, drop, add) c(setdiff(base, drop), add)
cat("\n##### Further economic and fiscal controls, part 3: borrowing, debt and the Government Actuary (", TAG, ") #####\n", sep = "")
p1 <- list("baseline" = fit(S, BASE), "+gad_earn" = fit(S, c(BASE, "gad_earn")), "gad_earn only" = fit(S, swap(BASE, "delta_oadr_10", "gad_earn")),
           "+gad_policy" = fit(S, c(BASE, "gad_policy")), "gad_policy only" = fit(S, swap(BASE, "delta_oadr_10", "gad_policy")))
if (SAMPLE != "1964") p1 <- c(p1, list("+gad_gap" = fit(S, c(BASE, "gad_gap")), "gad_gap only" = fit(S, swap(BASE, "delta_oadr_10", "gad_gap"))))
panel(p1, paste0("Table B30. The Government Actuary's projected contribution pressure (", TAG, ")"), c("delta_oadr_10", "gad_earn", "gad_policy", "gad_gap", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 16,
      c("Reviews in force: 1960, 1964, 1972, 1975, 1982, 1990, 1995, 1999, 2003, 2010, 2014, 2017 (base years 1959, 1964, 1972, 1975, 1981, 1990, 1994, 1999, 2001, 2008, 2010, 2015);",
        "linear interpolation within each review. Under price indexation every review from 1982 to 2010 projected the required rate to fall (gad_policy < 0)."))
p2 <- list("baseline" = fit(S, BASE), "+fc_budget" = fit(S, c(BASE, "fc_budget")), "+L_fc_budget" = fit(S, c(BASE, "L_fc_budget")), "+fc_planned" = fit(S, c(BASE, "fc_planned")),
           "+fc_medium" = fit(S, c(BASE, "fc_medium")), "fc_budget only" = fit(S, swap(BASE, "delta_oadr_10", "fc_budget")), "fc_medium only" = fit(S, swap(BASE, "delta_oadr_10", "fc_medium")))
panel(p2, paste0("Table B31. Official borrowing forecasts (", sub("1965", "1972", TAG), ")"), c("delta_oadr_10", "fc_budget", "L_fc_budget", "fc_planned", "fc_medium", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 16,
      "Higher values = more borrowing planned. Budget-of-year-t forecasts are start-of-year information and enter contemporaneously. Forecasts begin with the March 1971 Budget.")
p3 <- list("baseline" = fit(S16, BASE), "+int_gdp" = fit(S16, c(BASE, "L_int_gdp")), "+int_tme" = fit(S16, c(BASE, "L_int_tme")), "+gilt" = fit(S16, c(BASE, "L_gilt")), "+r_g" = fit(S16, c(BASE, "L_r_g")),
           "int_tme only" = fit(S16, swap(BASE, "delta_oadr_10", "L_int_tme")), "r_g only" = fit(S16, swap(BASE, "delta_oadr_10", "L_r_g")))
panel(p3, paste0("Table B32. Debt dynamics (", LAB, BOE_END, ")"), c("delta_oadr_10", "L_int_gdp", "L_int_tme", "L_gilt", "L_r_g", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 15)
