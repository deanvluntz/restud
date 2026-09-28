# =============================================================================
# robustness_tables.R -- MANUSCRIPT: Online Appendix Tables B6-B15, either sample (Sam's layout, 28 Sep 2026, second revision).
#     SAMPLE <- "1964"; source("robustness_tables.R")   # extended sample, 1965-2019 (series B with the basis step)
#     SAMPLE <- "1977"; source("robustness_tables.R")   # core sample, 1977-2019 (non-EU citizens)
# Conventions: the first column of every table is the fiscal specification and is titled "main"; "+name" adds a variable;
# a bare "name" replaces the corresponding baseline variable (the notes say which); columns that replace the projection
# appear only where the check is about the projection itself (level, horizon, total DR). Every variable shown is defined
# under the first table that shows it (tables_common.R). Names: tables_common.R; data: data_common.R.
#   B6  Dependent variable classification            main | [mig_rate_s] | nb_rate | nb03_rate | [ltim_rate]   (scaled series on the extended sample; LTIM basis on the core)
#   B7  Tax burden per worker: alternative measures     main | tax_inc | tax_nics | tax_receipts   (other receipts as text lines)
#   B8  National accounts taxes: alternative shares     main | tax_res | tax_gdp | tax_earn | tax_voter | tax_taxpayer | dln_tax_workpop | dln_tax_res
#   B9  Unemployment measures                           main | unem_boe | unem_claim
#   B10 Level and change                                main | level_oadr_10 | +oadr_0
#   B11 The policy lever                                main | main, to 2008 | permits, to 2008
#   B12 Gross inflows                                   main | mig_gross [| mig_gross (no step)]
#   B13 Old-age vs total dependency                     main | delta_dr_10 | level_dr_10
#   B14 The projection horizon                          main (10) | 1 | 3 | 5 | 7
#   B15 British citizens and Commonwealth               main | +Act steps (ext) | brit_rate | +cit_grants | +L_cit_grants | +cit_nat | +L_cit_nat | +brit_rate | +emig_all | +emig_brit (cit_reg as text lines); core: region panel with naturalisations
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, "2019"); EXT <- SAMPLE == "1964"; SN <- if (EXT) "extended sample" else "core sample"
swap <- function(base, drop, add) c(setdiff(base, drop), add)
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
line <- function(k, m, vs) { if (is.null(m)) return(invisible()); ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-26s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f, RESET %s\n", nobs(m), AIC(m), pf(resettest(m)$p.value))); defs(vs) }

# --- B6 dependent variable classification ---------------------------------------------------------------------------
fdv <- function(dv, d = S, step = EXT) fit(d, c(paste0("L_", dv), "delta_oadr_10", if (step) STEP, CT), dv)
p <- list("main" = fit(S, BASE))
if (EXT) { p[["mig_rate_s"]] <- fdv("mig_rate_s", step = FALSE); p[["nb_rate"]] <- fdv("nb_rate", step = FALSE); p[["nb03_rate"]] <- fdv("nb03_rate", subset(df, year >= Y0 & year <= 2003))
} else { p[["nb_rate"]] <- fdv("nb_rate"); p[["nb03_rate"]] <- fdv("nb03_rate", subset(df, year >= Y0 & year <= 2003)); p[["ltim_rate"]] <- fdv("ltim_rate", subset(df, year >= 1992 & year <= 2019)) }
panel(p, paste0("Table B6. The dependent variable under the other sample's classification (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_mig_rate_s", "L_nb_rate", "L_nb03_rate", "L_ltim_rate", STEP), w = 14,
      if (EXT) c(sprintf("mig_rate_s: the pre-1975 non-British counts multiplied by exp(%.3f) = %.3f, the 1975-80 mean log ratio of non-EU to non-British inflows, so that the series is one", SPLICE_LOGRATIO, exp(SPLICE_LOGRATIO)),
                 "consistent measure and no step is estimated (the estimated step's long-run value, 0.13 / (1 - 0.46), is the same ratio). nb_rate: non-British citizens throughout,",
                 "which after 2004 counts EU citizens no government could ration; nb03_rate ends before the A8 accession (with the basis step, which is then a pre-1975 indicator).")
      else c("The core sample counts non-EU citizens arriving from outside the EU (IPS 3.01). nb_rate applies the extended sample's pre-1975 classification, non-British citizens of all",
             "origins (LTIM 2.00), to the core years; nb03_rate ends before the A8 accession; ltim_rate is the non-EU series on the LTIM basis, available from 1992."))
if (EXT) { cmp <- subset(df, year >= 1964 & year <= 1982, c("year", "dv_A", "dv_nonEU", "dv_B_scaled", "basis_pre1975")); names(cmp) <- c("year", "non_british_k", "non_eu_k", "series_B_scaled_k", "basis_pre1975")
  cat(sprintf("\nThe splice: 1975-80 mean log ratio non-EU / non-British = %.4f (factor %.3f); the estimated step implies exp(-0.1327 / (1 - 0.4587)) = %.3f.\n", SPLICE_LOGRATIO, exp(SPLICE_LOGRATIO), exp(-0.1327 / (1 - 0.4587))))
  print(format(cmp, digits = 4), row.names = FALSE); write.csv(cmp, "determinants/dv_series_comparison_1964_1982.csv", row.names = FALSE)
  cat("(written determinants/dv_series_comparison_1964_1982.csv)\n") }
# --- B7 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "tax_inc" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_inc")), "tax_nics" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_nics")), "tax_receipts" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_receipts")))
panel(p, paste0("Table B7. Tax burden per worker: alternative measures (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_tax_inc", "L_tax_nics", "L_tax_receipts", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      "Each column replaces L_tax_workpop (national accounts taxes per working-age person) with the central government receipt named (Millennium A27, FY t-1/t assigned to t), per working-age person, 2024-25 prices, t-1.")
cat("Other receipts per working-age person, in place of L_tax_workpop:\n")
for (v in c("L_tax_nics_emp", "L_tax_nics_er", "L_tax_direct", "L_tax_indirect", "L_tax_vat", "L_tax_corp")) line(sub("^L_", "", v), fit(S, swap(BASE, "L_tax_workpop", v)), c("delta_oadr_10", v))
line("tax_nics_emp + tax_nics_er", fit(S, swap(BASE, "L_tax_workpop", c("L_tax_nics_emp", "L_tax_nics_er"))), c("delta_oadr_10", "L_tax_nics_emp", "L_tax_nics_er"))
# --- B7 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "tax_res" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_res")), "tax_gdp" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_gdp")), "tax_earn" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_earn")),
          "tax_voter" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_voter")), "tax_taxpayer" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_taxpayer")),
          "dln_tax_workpop" = fit(S, swap(BASE, "L_tax_workpop", "L_dln_tax_workpop")), "dln_tax_res" = fit(S, swap(BASE, "L_tax_workpop", "L_dln_tax_res")))
panel(p, paste0("Table B8. National accounts taxes: alternative shares and log changes (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_tax_res", "L_tax_gdp", "L_tax_earn", "L_tax_voter", "L_tax_taxpayer", "L_dln_tax_workpop", "L_dln_tax_res", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 17,
      c("The same national accounts tax total (OBR HPFD) over alternative denominators: the working-age population (main), the resident population, GDP, average annual earnings, adults",
        "(working-age plus pensioners: the electorate, approximately) and income-tax payers (HMRC, from 1979); then the annual LOG CHANGE in taxes per working-age person and per resident",
        "in place of the level (the tax burden as a change, like the debt term)."))
# --- B8 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "unem_boe" = fit(S, swap(BASE, "L_unem", "L_unem_boe")), "unem_claim" = fit(S, swap(BASE, "L_unem", "L_unem_claim")))
panel(p, paste0("Table B9. Unemployment measures (", TAG, ")"), c("delta_oadr_10", "L_unem", "L_unem_boe", "L_unem_claim", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 14,
      "Each column replaces L_unem (the ILO rate) with the measure named. The claimant count ends in 2016 (ONS discontinued the consistent series under Universal Credit).")
# --- B9 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "level_oadr_10" = fit(S, swap(BASE, "delta_oadr_10", "level_oadr_10")), "+oadr_0" = fit(S, c(BASE, "oadr_0")))
panel(p, paste0("Table B10. The projected level and the projected change (", TAG, ")"), c("delta_oadr_10", "level_oadr_10", "oadr_0", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 16,
      c("level_oadr_10 = oadr_0 + delta_oadr_10, so the level column constrains the current ratio and the projected change to one coefficient;",
        "the third column frees them: the projected change carries the effect, the current (known) ratio adds nothing."))
if (!is.null(p[[3]])) { lh <- linearHypothesis(p[[3]], "delta_oadr_10 = oadr_0", vcov. = vcovHAC(p[[3]])); cat(sprintf("Equality of the two coefficients in column 3: HAC F = %.2f (p %s).\n", lh$F[2], pf(lh$`Pr(>F)`[2]))) }
# --- B10 --------------------------------------------------------------------------------------------------
Y0P <- if (EXT) 1965 else 1977; P <- subset(df, year >= Y0P & year <= 2008)
p <- list("main" = fit(S, BASE)); p[[sprintf("main, %d-2008", Y0P)]] <- fit(P, BASE); p[[sprintf("permits, %d-2008", Y0P)]] <- fit(P, c("L_permits", "delta_oadr_10", CT), "permits")
panel(p, paste0("Table B11. The policy lever: work permits (", SN, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_permits", STEP), w = 20,
      c("Column 3: dependent variable permits, in thousands (Bauer and Salt, 2020, Fig. 1; the series ends in 2008, Tier 2 being a different basis), with the fiscal specification's",
        "right-hand side and no basis step (the permit series has no change of basis). Column 2 re-estimates the main column on the permit years for comparison."))
# --- B11 --------------------------------------------------------------------------------------------------
GB <- swap(BASE, "L_mig_rate", "L_mig_gross")
p <- list("main" = fit(S, BASE), "mig_gross" = fit(S, GB, "mig_gross")); if (EXT) p[["mig_gross (no step)"]] <- fit(S, setdiff(GB, STEP), "mig_gross")
panel(p, paste0("Table B12. Gross inflows (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_mig_gross", STEP), w = 20,
      "mig_gross: the log of gross inflows in persons, with no population term on either side of the equation.")
cat("Emigration (added to the main column): "); e1 <- fit(S, c(BASE, "L_emig_all")); e2 <- fit(S, c(BASE, "L_emig_brit"))
for (k in list(list("+emig_all", e1, "L_emig_all"), list("+emig_brit", e2, "L_emig_brit"))) { ct <- coeftest(k[[2]], vcov. = vcovHAC(k[[2]])); cat(sprintf("%s: %s %.4f (p %s), delta_oadr_10 %.4f; ", k[[1]], k[[3]], ct[k[[3]], 1], pf(ct[k[[3]], 4]), ct["delta_oadr_10", 1])) }; cat("\n"); defs(c("L_emig_all", "L_emig_brit"))
# --- B12 --------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "delta_dr_10" = fit(S, swap(BASE, "delta_oadr_10", "delta_dr_10")), "level_dr_10" = fit(S, swap(BASE, "delta_oadr_10", "level_dr_10")))
panel(p, paste0("Table B13. Old-age vs. total dependency ratio (", TAG, ")"), c("delta_oadr_10", "delta_dr_10", "level_dr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      c("Columns 2-3 replace the projected old-age change with the projected change and the projected level of the total dependency ratio (children + pensioners), same vintages.",
        if (EXT) "In the 1960s delta_dr_10 swings from +106 to -64 with the projected size of the child population; under the fiscal mechanism only the old-age term should carry the effect." else NULL))
line("delta_oadr_10 + delta_dr_10 together", fit(S, c(BASE, "delta_dr_10")), c("delta_oadr_10", "delta_dr_10"))
# --- B13 --------------------------------------------------------------------------------------------------
H <- paste0("delta_oadr_", c(1, 3, 5, 7)); p <- c(list("main" = fit(S, BASE)), setNames(lapply(H, function(h) fit(S, swap(BASE, "delta_oadr_10", h))), H))
hh <- vapply(names(p), function(k) hacF(p[[k]], if (k == "main") "delta_oadr_10" else k), numeric(2))
panel(p, paste0("Table B14. The projection horizon (", TAG, ")"), c("delta_oadr_10", H, "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      c("Each column replaces the ten-year projected change (main) with the projected change over the horizon named, same vintages.",
        sprintf("HAC F of the horizon term: %s.", paste(sprintf("%s %.2f (p %s)", names(p), hh[1, ], vapply(hh[2, ], pf, character(1))), collapse = "; "))))
# --- B15 British citizens and Commonwealth: inflows, citizenship policy and emigration -------------------------------
BR <- c("L_brit_rate", "delta_oadr_10", CT)
p <- list("main" = fit(S, BASE)); if (EXT) p[["+Act steps"]] <- fit(S, c(BASE, "post1968", "post1973"))
p[["brit_rate"]] <- fit(S, BR, "brit_rate"); p[["brit_rate +cit_grants"]] <- fit(S, c(BR, "cit_grants_k"), "brit_rate"); p[["brit_rate +L_cit_grants"]] <- fit(S, c(BR, "L_cit_grants_k"), "brit_rate")
p[["brit_rate +cit_nat"]] <- fit(S, c(BR, "cit_nat_k"), "brit_rate"); p[["brit_rate +L_cit_nat"]] <- fit(S, c(BR, "L_cit_nat_k"), "brit_rate")
p[["+brit_rate"]] <- fit(S, c(BASE, "brit_rate")); p[["+emig_all"]] <- fit(S, c(BASE, "L_emig_all")); p[["+emig_brit"]] <- fit(S, c(BASE, "L_emig_brit"))
panel(p, paste0("Table B15. British citizens and Commonwealth: inflows, citizenship policy and emigration (", TAG, ")"),
      c("delta_oadr_10", "post1968", "post1973", "cit_grants_k", "L_cit_grants_k", "cit_nat_k", "L_cit_nat_k", "brit_rate", "L_emig_all", "L_emig_brit", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_brit_rate", STEP), w = 13, lw = 16,
      c(if (EXT) c("+Act steps: indicators for the Commonwealth Immigrants Act 1968 and the Immigration Act 1971 in force (1973). Given the intercept and basis_pre1975 they are identified inside",
                   "the pre-1975 window only (post1968 contrasts 1965-67 with 1968-74; post1973 is numerically an indicator for 1973-74), so the column lets the pre-1975 level float.") else NULL,
        "brit_rate columns: the fiscal specification for British-citizen inflows, all origins (LTIM 2.00), the uncontrolled flow, as placebo (no basis step), then with current-year grants of",
        "British citizenship (all grants; naturalisations on residence or marriage, the discretionary route; registrations, an entitlement) as the citizenship-policy channel. +brit_rate holds the British flow constant in the controlled-flow equation (a lower",
        "bound on the projection). +emig_all / +emig_brit: lagged emigrants per working-age person, all citizens / British citizens, added to the main column.",
        "Grants made outside the UK (cz_04) exist for 1962-82 only and are in the private tests."))
line("brit_rate +cit_reg", fit(S, c(BR, "cit_reg_k"), "brit_rate"), c("delta_oadr_10", "cit_reg_k")); line("brit_rate +L_cit_reg", fit(S, c(BR, "L_cit_reg_k"), "brit_rate"), c("delta_oadr_10", "L_cit_reg_k"))
if (EXT) line("+trend (VIF 63, sign only)", fit(S, c(BASE, "trend")), c("delta_oadr_10", "trend"))
line("+L_brit_rate", fit(S, c(BASE, "L_brit_rate")), c("delta_oadr_10", "L_brit_rate"))
if (!EXT) {   # by region of last residence, with the citizenship-grants channel (IPS 3.01, core sample)
  REG <- c("brit_noneu", "brit_oce", "brit_nor", "brit_oth", "brit_asi", "brit_sub", "brit_res")
  p <- setNames(lapply(REG, function(v) fit(S, c(paste0("L_", v), "delta_oadr_10", CT, "cit_nat_k"), v)), c("non-EU", "Oceania", "N America", "Oth Europe", "Asia", "SS Africa", "Rest"))
  panel(p, "Table B15 (continued). British citizens arriving from outside the EU, by region of last residence, with naturalisations (core sample, 1977-2019)",
        c("delta_oadr_10", "cit_nat_k", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", paste0("L_", REG)), w = 13, lw = 16,
        c("Each column: British citizens arriving from that region of last residence (IPS 3.01), with the fiscal specification's right-hand side and current-year naturalisations (total grants give the same reading).",
          "A response concentrated in regions of the former Empire, and to grants, points to citizens by descent or registration rather than returning emigrants.")) }
