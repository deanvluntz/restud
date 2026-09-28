# =============================================================================
# robustness_tables.R -- MANUSCRIPT: Online Appendix Tables B6-B14, either sample (Sam's layout, 28 Sep 2026, second revision).
#     SAMPLE <- "1964"; source("robustness_tables.R")   # extended sample, 1965-2019 (series B with the basis step)
#     SAMPLE <- "1977"; source("robustness_tables.R")   # core sample, 1977-2019 (non-EU citizens)
# Conventions: the first column of every table is the fiscal specification and is titled "main"; "+name" adds a variable;
# a bare "name" replaces the corresponding baseline variable (the notes say which); columns that replace the projection
# appear only where the check is about the projection itself (level, horizon, total DR). Every variable shown is defined
# under the first table that shows it (tables_common.R). Names: tables_common.R; data: data_common.R.
#   B6  Tax burden per worker: alternative measures     main | tax_inc | tax_nics | tax_receipts   (other receipts as text lines)
#   B7  National accounts taxes: alternative shares     main | tax_res | tax_gdp | tax_earn
#   B8  Unemployment measures                           main | unem_boe | unem_claim
#   B9  Level and change                                main | level_oadr_10 | +oadr_0
#   B10 The policy lever                                main | main, to 2008 | permits, to 2008
#   B11 Gross inflows                                   main | mig_gross [| mig_gross (no step)]
#   B12 Old-age vs total dependency                     main | delta_dr_10 | level_dr_10
#   B13 The projection horizon                          main (10) | 1 | 3 | 5 | 7
#   B14 British citizens and Commonwealth               main | +Act steps (extended) | brit_rate | +brit_rate   (trend, L_brit_rate, emigration as text lines)
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, "2019"); EXT <- SAMPLE == "1964"; SN <- if (EXT) "extended sample" else "core sample"
swap <- function(base, drop, add) c(setdiff(base, drop), add)
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
line <- function(k, m, vs) { if (is.null(m)) return(invisible()); ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-26s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f, RESET %s\n", nobs(m), AIC(m), pf(resettest(m)$p.value))); defs(vs) }

# --- B6 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "tax_inc" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_inc")), "tax_nics" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_nics")), "tax_receipts" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_receipts")))
panel(p, paste0("Table B6. Tax burden per worker: alternative measures (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_tax_inc", "L_tax_nics", "L_tax_receipts", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      "Each column replaces L_tax_workpop (national accounts taxes per working-age person) with the central government receipt named (Millennium A27, FY t-1/t assigned to t), per working-age person, 2024-25 prices, t-1.")
cat("Other receipts per working-age person, in place of L_tax_workpop:\n")
for (v in c("L_tax_nics_emp", "L_tax_nics_er", "L_tax_direct", "L_tax_indirect", "L_tax_vat", "L_tax_corp")) line(sub("^L_", "", v), fit(S, swap(BASE, "L_tax_workpop", v)), c("delta_oadr_10", v))
line("tax_nics_emp + tax_nics_er", fit(S, swap(BASE, "L_tax_workpop", c("L_tax_nics_emp", "L_tax_nics_er"))), c("delta_oadr_10", "L_tax_nics_emp", "L_tax_nics_er"))
# --- B7 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "tax_res" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_res")), "tax_gdp" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_gdp")), "tax_earn" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_earn")))
panel(p, paste0("Table B7. National accounts taxes: alternative shares (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_tax_res", "L_tax_gdp", "L_tax_earn", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      "The same national accounts tax total (OBR HPFD) over alternative denominators: the working-age population (main), the resident population, GDP, and average annual earnings.")
# --- B8 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "unem_boe" = fit(S, swap(BASE, "L_unem", "L_unem_boe")), "unem_claim" = fit(S, swap(BASE, "L_unem", "L_unem_claim")))
panel(p, paste0("Table B8. Unemployment measures (", TAG, ")"), c("delta_oadr_10", "L_unem", "L_unem_boe", "L_unem_claim", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 14,
      "Each column replaces L_unem (the ILO rate) with the measure named. The claimant count ends in 2016 (ONS discontinued the consistent series under Universal Credit).")
# --- B9 ---------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "level_oadr_10" = fit(S, swap(BASE, "delta_oadr_10", "level_oadr_10")), "+oadr_0" = fit(S, c(BASE, "oadr_0")))
panel(p, paste0("Table B9. The projected level and the projected change (", TAG, ")"), c("delta_oadr_10", "level_oadr_10", "oadr_0", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 16,
      c("level_oadr_10 = oadr_0 + delta_oadr_10, so the level column constrains the current ratio and the projected change to one coefficient;",
        "the third column frees them: the projected change carries the effect, the current (known) ratio adds nothing."))
if (!is.null(p[[3]])) { lh <- linearHypothesis(p[[3]], "delta_oadr_10 = oadr_0", vcov. = vcovHAC(p[[3]])); cat(sprintf("Equality of the two coefficients in column 3: HAC F = %.2f (p %s).\n", lh$F[2], pf(lh$`Pr(>F)`[2]))) }
# --- B10 --------------------------------------------------------------------------------------------------
Y0P <- if (EXT) 1965 else 1977; P <- subset(df, year >= Y0P & year <= 2008)
p <- list("main" = fit(S, BASE)); p[[sprintf("main, %d-2008", Y0P)]] <- fit(P, BASE); p[[sprintf("permits, %d-2008", Y0P)]] <- fit(P, c("L_permits", "delta_oadr_10", CT), "permits")
panel(p, paste0("Table B10. The policy lever: work permits (", SN, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_permits", STEP), w = 20,
      c("Column 3: dependent variable permits, in thousands (Bauer and Salt, 2020, Fig. 1; the series ends in 2008, Tier 2 being a different basis), with the fiscal specification's",
        "right-hand side and no basis step (the permit series has no change of basis). Column 2 re-estimates the main column on the permit years for comparison."))
# --- B11 --------------------------------------------------------------------------------------------------
GB <- swap(BASE, "L_mig_rate", "L_mig_gross")
p <- list("main" = fit(S, BASE), "mig_gross" = fit(S, GB, "mig_gross")); if (EXT) p[["mig_gross (no step)"]] <- fit(S, setdiff(GB, STEP), "mig_gross")
panel(p, paste0("Table B11. Gross inflows (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_mig_gross", STEP), w = 20,
      "mig_gross: the log of gross inflows in persons, with no population term on either side of the equation.")
cat("Emigration (added to the main column): "); e1 <- fit(S, c(BASE, "L_emig_all")); e2 <- fit(S, c(BASE, "L_emig_brit"))
for (k in list(list("+emig_all", e1, "L_emig_all"), list("+emig_brit", e2, "L_emig_brit"))) { ct <- coeftest(k[[2]], vcov. = vcovHAC(k[[2]])); cat(sprintf("%s: %s %.4f (p %s), delta_oadr_10 %.4f; ", k[[1]], k[[3]], ct[k[[3]], 1], pf(ct[k[[3]], 4]), ct["delta_oadr_10", 1])) }; cat("\n"); defs(c("L_emig_all", "L_emig_brit"))
# --- B12 --------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE), "delta_dr_10" = fit(S, swap(BASE, "delta_oadr_10", "delta_dr_10")), "level_dr_10" = fit(S, swap(BASE, "delta_oadr_10", "level_dr_10")))
panel(p, paste0("Table B12. Old-age vs. total dependency ratio (", TAG, ")"), c("delta_oadr_10", "delta_dr_10", "level_dr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      c("Columns 2-3 replace the projected old-age change with the projected change and the projected level of the total dependency ratio (children + pensioners), same vintages.",
        if (EXT) "In the 1960s delta_dr_10 swings from +106 to -64 with the projected size of the child population; under the fiscal mechanism only the old-age term should carry the effect." else NULL))
line("delta_oadr_10 + delta_dr_10 together", fit(S, c(BASE, "delta_dr_10")), c("delta_oadr_10", "delta_dr_10"))
# --- B13 --------------------------------------------------------------------------------------------------
H <- paste0("delta_oadr_", c(1, 3, 5, 7)); p <- c(list("main" = fit(S, BASE)), setNames(lapply(H, function(h) fit(S, swap(BASE, "delta_oadr_10", h))), H))
hh <- vapply(names(p), function(k) hacF(p[[k]], if (k == "main") "delta_oadr_10" else k), numeric(2))
panel(p, paste0("Table B13. The projection horizon (", TAG, ")"), c("delta_oadr_10", H, "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      c("Each column replaces the ten-year projected change (main) with the projected change over the horizon named, same vintages.",
        sprintf("HAC F of the horizon term: %s.", paste(sprintf("%s %.2f (p %s)", names(p), hh[1, ], vapply(hh[2, ], pf, character(1))), collapse = "; "))))
# --- B14 --------------------------------------------------------------------------------------------------
p <- list("main" = fit(S, BASE)); if (EXT) p[["+Act steps"]] <- fit(S, c(BASE, "post1968", "post1973"))
p[["brit_rate"]] <- fit(S, c("L_brit_rate", "delta_oadr_10", CT), "brit_rate"); p[["+brit_rate"]] <- fit(S, c(BASE, "brit_rate"))
panel(p, paste0("Table B14. British citizens and Commonwealth: inflows and policy (", TAG, ")"), c("delta_oadr_10", "post1968", "post1973", "brit_rate", "L_brit_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      c(if (EXT) c("+Act steps: indicators for the Commonwealth Immigrants Act 1968 and the Immigration Act 1971 in force (1973). Given the intercept and basis_pre1975, they are identified",
                   "inside the pre-1975 window only (post1968 contrasts 1965-67 with 1968-74; post1973 is numerically an indicator for 1973-74, the Ugandan Asian admissions, the rush",
                   "of dependants before the Act bit, and EEC nationals free to enter from accession), so the column lets the pre-1975 level float; the projection is unchanged.") else NULL,
        "brit_rate: the fiscal specification for British-citizen inflows, all origins (LTIM 2.00), the uncontrolled flow, as placebo (no basis step). +brit_rate: the British flow held",
        "constant in the controlled-flow equation, a lower bound on the projection."))
if (EXT) line("+trend (VIF 63, sign only)", fit(S, c(BASE, "trend")), c("delta_oadr_10", "trend"))
line("+L_brit_rate", fit(S, c(BASE, "L_brit_rate")), c("delta_oadr_10", "L_brit_rate"))
