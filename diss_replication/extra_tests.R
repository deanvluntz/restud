# =============================================================================
# extra_tests.R -- MANUSCRIPT: further fiscal controls, part 2 (realised spending and the current fiscal balance).
# Either sample (SAMPLE <- "1964" / "1977"; wrappers as boe_tests.R). Names and definitions: tables_common.R; data: data_common.R.
# The 'without the projection' comparison moved to no_projection.R (Sam, 28 Sep 2026).
#   F4  Realised spending (A28)              baseline | +grants_gdp | +grants_wap | +tme_gdp | grants_gdp only
#   F5  The current fiscal balance (HPFD)    baseline | +psnb_gdp | psnb_gdp for ln_debt_gdp | +psnd_gdp | +tax_gdp | psnb_gdp only
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= BOE_END); S19 <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, BOE_END)
swap <- function(base, drop, add) c(setdiff(base, drop), add)
cat("\n##### Further economic and fiscal controls, part 2: realised spending and the current balance (", TAG, ") #####\n", sep = "")
p1 <- list("baseline" = fit(S, BASE), "+grants_gdp" = fit(S, c(BASE, "L_grants_gdp")), "+grants_wap" = fit(S, c(BASE, "L_grants_wap")), "+tme_gdp" = fit(S, c(BASE, "L_tme_gdp")),
           "grants_gdp only" = fit(S, swap(BASE, "delta_oadr_10", "L_grants_gdp")))
panel(p1, paste0("F4. Realised spending (", TAG, ")"), c("delta_oadr_10", "L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 16,
      "'only' = in place of delta_oadr_10: realised spending does not carry the projection's information.")
fbk <- rbind(read.csv("determinants/fiscal_block_1962_1974.csv")[, c("year", "psnb_gdp", "psnd_gdp", "tax_nat_gdp")], read.csv("fiscal_block_HPFD.csv")[, c("year", "psnb_gdp", "psnd_gdp", "tax_nat_gdp")])
fbk <- fbk[!duplicated(fbk$year), ]
for (v in c("psnb_gdp", "psnd_gdp")) S19[[paste0("L_", v)]] <- lag1(fbk[[v]][match(S19$year, fbk$year)])
S19$L_tax_gdp <- lag1(fbk$tax_nat_gdp[match(S19$year, fbk$year)])
p2 <- list("baseline" = fit(S19, BASE), "+psnb_gdp" = fit(S19, c(BASE, "L_psnb_gdp")), "psnb_gdp for ln_debt_gdp" = fit(S19, swap(BASE, "L_ln_debt_gdp", "L_psnb_gdp")),
           "+psnd_gdp" = fit(S19, c(BASE, "L_psnd_gdp")), "+tax_gdp" = fit(S19, c(BASE, "L_tax_gdp")), "psnb_gdp only" = fit(S19, swap(BASE, "delta_oadr_10", "L_psnb_gdp")))
panel(p2, paste0("F5. The current fiscal balance (", sub(BOE_END, "2019", TAG), ")"), c("delta_oadr_10", "L_psnb_gdp", "L_psnd_gdp", "L_tax_gdp", "L_ln_debt_gdp", "L_unem", "L_tax_workpop", "L_mig_rate", STEP), w = 16,
      "The last column replaces the projection with the borrowing ratio: the sign reverses (a larger deficit is followed by fewer inflows) and RESET rejects.")
