# =============================================================================
# extra_tests.R -- MANUSCRIPT: further fiscal controls, part 2 (realised spending and the current fiscal balance).
# Either sample (SAMPLE <- "1964" / "1977"; wrappers as boe_tests.R). Names and definitions: tables_common.R; data: data_common.R.
# The 'without the projection' comparison moved to no_projection.R (Sam, 28 Sep 2026).
#   B30 Realised spending (A28)              baseline | +grants_gdp | +grants_wap | +tme_gdp | grants_gdp only
#   B31 The current fiscal balance (HPFD)    baseline | +psnb_gdp | psnb_gdp for ln_debt_gdp | +psnd_gdp | +tax_gdp | psnb_gdp only
#   B32 The Treasury view (Macpherson, 2014): openness, broad money, participation, the revenue ceiling, real spending growth, institutional regimes
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= BOE_END); S19 <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, BOE_END)
swap <- function(base, drop, add) c(setdiff(base, drop), add)
cat("\n##### Further economic and fiscal controls, part 2: realised spending and the current balance (", TAG, ") #####\n", sep = "")
p1 <- list("baseline" = fit(S, BASE), "+grants_gdp" = fit(S, c(BASE, "L_grants_gdp")), "+grants_wap" = fit(S, c(BASE, "L_grants_wap")), "+tme_gdp" = fit(S, c(BASE, "L_tme_gdp")),
           "grants_gdp only" = fit(S, swap(BASE, "delta_oadr_10", "L_grants_gdp")))
panel(p1, paste0("Table B30. Realised spending (", TAG, ")"), c("delta_oadr_10", "L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 16,
      "'only' = in place of delta_oadr_10: realised spending does not carry the projection's information.")
p2 <- list("baseline" = fit(S19, BASE), "+psnb_gdp" = fit(S19, c(BASE, "L_psnb_gdp")), "psnb_gdp for ln_debt_gdp" = fit(S19, swap(BASE, "L_ln_debt_gdp", "L_psnb_gdp")),
           "+psnd_gdp" = fit(S19, c(BASE, "L_psnd_gdp")), "+tax_gdp" = fit(S19, c(BASE, "L_tax_gdp")), "psnb_gdp only" = fit(S19, swap(BASE, "delta_oadr_10", "L_psnb_gdp")))
panel(p2, paste0("Table B31. The current fiscal balance (", sub(BOE_END, "2019", TAG), ")"), c("delta_oadr_10", "L_psnb_gdp", "L_psnd_gdp", "L_tax_gdp", "L_ln_debt_gdp", "L_unem", "L_tax_workpop", "L_mig_rate", STEP), w = 16,
      "The last column replaces the projection with the borrowing ratio: the sign reverses (a larger deficit is followed by fewer inflows) and RESET rejects.")
# --- B32 the Treasury view -----------------------------------------------------------------------------------------------
TV <- c("L_openness", "L_m4_growth", "L_particip", "L_tax_ceiling", "L_tme_real_growth", "boe_indep", "golden_rule", "obr_era")
p3 <- c(list("baseline" = fit(S19, BASE)), setNames(lapply(TV, function(v) fit(S19, c(BASE, v))), paste0("+", sub("^L_", "", TV))), list("+all eight" = fit(S19, c(BASE, TV))))
panel(p3, paste0("Table B32. The Treasury view: openness, money, participation, the revenue ceiling, spending growth and institutional regimes (", sub(BOE_END, "2019", TAG), ")"),
      c("delta_oadr_10", TV, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 13, lw = 18,
      c("Candidates suggested by Macpherson's (2014) account of the Treasury view: free trade (openness), sound money (broad money growth; inflation and rates are in Table B27),",
        "the supply side (participation), doubts about raising revenue beyond a certain point (distance from the ten-year revenue ceiling), spending control (real spending growth),",
        "and the institutional regimes of Bank independence, the golden rule and the OBR. Millennium series end in 2016 (N row)."))
