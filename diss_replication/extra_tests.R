# =============================================================================
# extra_tests.R -- MANUSCRIPT: realised spending, the current fiscal balance, and 'can the model predict
# inflows without the projection?'. Either sample (SAMPLE <- "1964" / "1977"; wrappers as boe_tests.R).
# Names and definitions: tables_common.R; data: data_common.R.
#   1  Realised spending (A28, to 2016)      baseline | +grants_gdp | +grants_wap | +tme_gdp | grants_gdp only
#   2  Without the projection (to 2016)      no projection | no projection +six | delta_oadr_10 only | baseline | +six | +significant | +significant +L2_mig_rate
#   2b The same for gross inflows            mig_gross: baseline | +significant | +significant +L2
#   3  The current fiscal balance (to 2019)  baseline | +psnb_gdp | psnb_gdp for ln_debt_gdp | +psnd_gdp | +tax_gdp | psnb_gdp only
# 'significant' = the controls among the six with HAC p < 0.10 in the '+six' column of THIS sample (named in the notes).
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= BOE_END); S19 <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, BOE_END)
swap <- function(base, drop, add) c(setdiff(base, drop), add)
# --- 1 realised spending ------------------------------------------------------------------------
p1 <- list("baseline" = fit(S, BASE), "+grants_gdp" = fit(S, c(BASE, "L_grants_gdp")), "+grants_wap" = fit(S, c(BASE, "L_grants_wap")), "+tme_gdp" = fit(S, c(BASE, "L_tme_gdp")),
           "grants_gdp only" = fit(S, swap(BASE, "delta_oadr_10", "L_grants_gdp")))
panel(p1, paste0("1. Realised spending across the sample (", TAG, ")"), c("delta_oadr_10", "L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 16,
      "'only' = in place of delta_oadr_10: realised spending does not carry the projection's information.")
# --- 2 without the projection -------------------------------------------------------------------
m6 <- fit(S, c(BASE, SIX)); ct6 <- coeftest(m6, vcov. = vcovHAC(m6)); pv <- ct6[, 4]; SIG <- names(pv)[names(pv) %in% SIX & pv < 0.10]
NOP <- setdiff(BASE, "delta_oadr_10")
p2 <- list("no projection" = fit(S, NOP), "no projection +six" = fit(S, c(NOP, SIX)), "delta_oadr_10 only" = fit(S, c("L_mig_rate", STEP, "delta_oadr_10")), "baseline" = fit(S, BASE), "+six" = m6)
if (length(SIG)) p2 <- c(p2, list("+significant" = fit(S, c(BASE, SIG)), "+significant +L2" = fit(S, c(BASE, SIG, "L2_mig_rate"))))
panel(p2, paste0("2. Can the model predict inflows without the projection? (", TAG, ")"), c("delta_oadr_10", SIX, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", "L2_mig_rate", STEP), w = 19, lw = 15,
      c(paste0("six = ", paste(SIX, collapse = ", "), ". significant = ", if (length(SIG)) paste(SIG, collapse = ", ") else "none at 10% in this sample", "."),
        "Compare AIC and RESET across columns: without delta_oadr_10 the six controls do not recover the fit or the functional form. L2_mig_rate addresses residual",
        "autocorrelation (BG) in the augmented model."))
if (length(SIG)) {
  GB <- swap(BASE, "L_mig_rate", "L_mig_gross")
  p2b <- list("mig_gross" = fit(S, GB, "mig_gross"), "+significant" = fit(S, c(GB, SIG), "mig_gross"), "+significant +L2" = fit(S, c(GB, SIG, "L2_mig_rate"), "mig_gross"))
  panel(p2b, paste0("2b. The augmented model for gross inflows (", TAG, ")"), c("delta_oadr_10", SIG, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_gross", "L2_mig_rate", STEP), w = 19,
        "Dependent variable mig_gross (no population denominator); L2_mig_rate is the second lag of the per-WAP rate.")
}
# --- 3 the current fiscal balance ------------------------------------------------------------------
fbk <- rbind(read.csv("determinants/fiscal_block_1962_1974.csv")[, c("year", "psnb_gdp", "psnd_gdp", "tax_nat_gdp")], read.csv("fiscal_block_HPFD.csv")[, c("year", "psnb_gdp", "psnd_gdp", "tax_nat_gdp")])
fbk <- fbk[!duplicated(fbk$year), ]
for (v in c("psnb_gdp", "psnd_gdp")) S19[[paste0("L_", v)]] <- lag1(fbk[[v]][match(S19$year, fbk$year)])
S19$L_tax_gdp <- lag1(fbk$tax_nat_gdp[match(S19$year, fbk$year)])
p3 <- list("baseline" = fit(S19, BASE), "+psnb_gdp" = fit(S19, c(BASE, "L_psnb_gdp")), "psnb_gdp for ln_debt_gdp" = fit(S19, swap(BASE, "L_ln_debt_gdp", "L_psnb_gdp")),
           "+psnd_gdp" = fit(S19, c(BASE, "L_psnd_gdp")), "+tax_gdp" = fit(S19, c(BASE, "L_tax_gdp")), "psnb_gdp only" = fit(S19, swap(BASE, "delta_oadr_10", "L_psnb_gdp")))
panel(p3, paste0("3. The current fiscal balance (", sub(BOE_END, "2019", TAG), ")"), c("delta_oadr_10", "L_psnb_gdp", "L_psnd_gdp", "L_tax_gdp", "L_ln_debt_gdp", "L_unem", "L_tax_workpop", "L_mig_rate", STEP), w = 16,
      "The last column replaces the projection with the borrowing ratio: the sign reverses (a larger deficit is followed by fewer inflows) and RESET rejects.")
