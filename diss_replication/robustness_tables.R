# =============================================================================
# robustness_tables.R -- MANUSCRIPT: the robustness tables in the order agreed with Sam (28 Sep 2026), either sample.
#     SAMPLE <- "1964"; source("robustness_tables.R")   # extended sample, 1965-2019 (series B with the basis step)
#     SAMPLE <- "1977"; source("robustness_tables.R")   # core sample, 1977-2019 (non-EU citizens)
# The first table of the extended-sample document (the fiscal specification with and without the basis step) is
# extension_1964.R; the core sample's is replication_1977.R Table 2. Names and definitions: tables_common.R (each name is
# defined under the first panel that shows it and never again); data: data_common.R. Main results run to 2019; any column
# using a series spliced for 2017-19 is re-estimated to 2016 under its panel.
#   1  Tax burden measures        (a) the per-head measures and the labour taxes; (b) the other A27 receipts
#   2  Unemployment measures      L_unem (ILO, baseline) | unem_boe for unem | unem_claim for unem
#   3  Level and change           baseline | oadr_10 for delta_oadr_10 | +oadr_0
#   4  The policy lever           permits (1965-2008) | +post1973 | lever (1964-2008) | permits +delta_dr_10   [core: 1977-2008]
#   5  Gross inflows              mig_gross | mig_gross (no step) [extended] | mig_gross +emig_all
#   6  Old-age vs total DR        OADR | total DR | both
#   7  Emigration                 baseline | +emig_all | +emig_brit
#   8  Pre-1975 regime steps      baseline | +Act steps (+trend as one line)   [extended only]
#   9  The projection horizon     delta_oadr_1 | 3 | 5 | 7 | 10
#  10  British citizens           mig_rate | brit_rate | mig_rate +brit_rate (+L_brit_rate as one line)
#  11  Sub-samples and dynamics   sub-samples; second lag; sample starts as one line [extended]
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= 2019); TAG <- paste0(LAB, "2019"); EXT <- SAMPLE == "1964"; SN <- if (EXT) "extended sample" else "core sample"
swap <- function(base, drop, add) c(setdiff(base, drop), add)
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
b <- function(m, v = "delta_oadr_10") { ct <- coeftest(m, vcov. = vcovHAC(m)); sprintf("%.4f (p %s)", ct[v, 1], pf(ct[v, 4])) }
line <- function(k, m, vs = "delta_oadr_10") { ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-28s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f\n", nobs(m), AIC(m))) }

# --- 1 tax burden measures ---------------------------------------------------------------------------
tv1 <- c("L_tax_res", "L_tax_lab", "L_tax_inc", "L_tax_nics", "L_tax_nics_emp", "L_tax_nics_er")
tv2 <- c("L_tax_inc_nics", "L_tax_direct", "L_tax_indirect", "L_tax_vat", "L_tax_corp", "L_tax_receipts")
p1a <- c(list("L_tax_workpop" = fit(S, BASE)), setNames(lapply(tv1, function(v) fit(S, swap(BASE, "L_tax_workpop", v))), sub("^L_", "", tv1)))
panel(p1a, paste0("Table B6. Tax burden measures, each in place of L_tax_workpop (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", tv1, "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14, lw = 15,
      "L_tax_workpop, L_tax_res and L_tax_lab are national accounts totals (OBR HPFD); the A27 measures are central government receipts (Millennium A27), FY t-1/t assigned to t.")
p1b <- c(list("L_tax_workpop" = fit(S, BASE)), setNames(lapply(tv2, function(v) fit(S, swap(BASE, "L_tax_workpop", v))), sub("^L_", "", tv2)))
panel(p1b, paste0("Table B7. Tax burden measures, other central government receipts (", TAG, ")"), c("delta_oadr_10", "L_tax_workpop", tv2, "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14, lw = 15,
      "VAT restricts the sample to 1975 on. The indirect, VAT and corporation-tax columns fail RESET: a burden on production is not a burden on the working-age voter.")

# --- 2 unemployment measures -----------------------------------------------------------------------
p2 <- list("L_unem (baseline)" = fit(S, BASE), "unem_boe for unem" = fit(S, swap(BASE, "L_unem", "L_unem_boe")), "unem_claim for unem" = fit(S, swap(BASE, "L_unem", "L_unem_claim")))
panel(p2, paste0("Table B8. Unemployment measures (", TAG, ")"), c("delta_oadr_10", "L_unem", "L_unem_boe", "L_unem_claim", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 20,
      "The claimant count ends in 2016 (ONS discontinued the consistent series under Universal Credit), so the last two columns end in 2017 (t-1).")

# --- 3 level and change ------------------------------------------------------------------------------
p3 <- list("baseline" = fit(S, BASE), "oadr_10 for delta_oadr_10" = fit(S, swap(BASE, "delta_oadr_10", "oadr_10")), "+oadr_0" = fit(S, c(BASE, "oadr_0")))
panel(p3, paste0("Table B9. The projected level and the projected change (", TAG, ")"), c("delta_oadr_10", "oadr_10", "oadr_0", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 26,
      c("oadr_10 = oadr_0 + delta_oadr_10, so the level column constrains the current ratio and the projected change to one coefficient;",
        "the third column frees them: the projected change carries the effect, the current (known) ratio adds nothing."))
if (!is.null(p3[[3]])) { lh <- linearHypothesis(p3[[3]], "delta_oadr_10 = oadr_0", vcov. = vcovHAC(p3[[3]])); cat(sprintf("Equality of the two coefficients in column 3: HAC F = %.2f (p %s).\n", lh$F[2], pf(lh$`Pr(>F)`[2]))) }

# --- 4 the policy lever ------------------------------------------------------------------------------
Y0P <- if (EXT) 1965 else 1977; P <- subset(df, year >= Y0P & year <= 2008); PL <- subset(df, year >= 1964 & year <= 2008)
PB <- c("L_permits", "delta_oadr_10", CT)   # no basis step: the permit series has no change of basis (its 1973 break is post1973 / lever)
p4 <- list(); p4[[sprintf("permits %d-2008", Y0P)]] <- fit(P, PB, "permits"); p4[["+post1973"]] <- if (EXT) fit(P, c(PB, "post1973"), "permits") else NULL
if (EXT) p4[["lever 1964-2008"]] <- fit(PL, c("L_lever", "delta_oadr_10", CT), "lever")
p4[["permits +delta_dr_10"]] <- fit(P, c(PB, "delta_dr_10"), "permits")
panel(p4, paste0("Table B10. The policy lever: work permits, ", Y0P, "-2008 (", SN, ")"), c("delta_oadr_10", "delta_dr_10", "post1973", CT, "L_permits", "L_lever"), w = 20, lw = 15,
      c("Dependent variable in thousands of permits. Before 1973 permits cover aliens only (Commonwealth workers entered on employment vouchers, 1962-72);",
        "lever = permits + vouchers. Tier 2 (2009-) is a different basis and is not spliced. Permits cannot go before 1965: the earliest projection vintage held is the 1963-based."))

# --- 5 gross inflows -----------------------------------------------------------------------------------
GB <- swap(BASE, "L_mig_rate", "L_mig_gross")
p5 <- list("mig_gross" = fit(S, GB, "mig_gross")); if (EXT) p5[["mig_gross (no step)"]] <- fit(S, setdiff(GB, STEP), "mig_gross")
p5[["mig_gross +emig_all"]] <- fit(S, c(GB, "L_emig_all"), "mig_gross")
panel(p5, paste0("Table B11. Gross inflows (", TAG, ")"), c("delta_oadr_10", "L_emig_all", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_gross", STEP), w = 22,
      "Dependent variable mig_gross: no population term on either side of the equation, so the tax term is not a ratio artefact.")

# --- 6 old-age vs total dependency -----------------------------------------------------------------------
p6 <- list("OADR" = fit(S, BASE), "total DR" = fit(S, swap(BASE, "delta_oadr_10", "delta_dr_10")), "both" = fit(S, c(BASE, "delta_dr_10")))
panel(p6, paste0("Table B12. Old-age vs. total dependency ratio (", TAG, ")"), c("delta_oadr_10", "delta_dr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      if (EXT) c("In the 1960s delta_dr_10 swings from +106 to -64 with the projected size of the child population; under the fiscal mechanism only the",
                 "old-age term should carry the effect. The total ratio alone is not null on the extended sample; beside the old-age ratio it is.") else NULL)

# --- 7 emigration --------------------------------------------------------------------------------------------
p7 <- list("baseline" = fit(S, BASE), "+emig_all" = fit(S, c(BASE, "L_emig_all")), "+emig_brit" = fit(S, c(BASE, "L_emig_brit")))
panel(p7, paste0("Table B13. Emigration (", TAG, ")"), c("delta_oadr_10", "L_emig_all", "L_emig_brit", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      "Emigration is not governed by the policy rule; if admissions merely replaced departures, lagged emigration would enter positively.")

# --- 8 pre-1975 regime steps (extended sample only) ------------------------------------------------------------
if (EXT) {
  p8 <- list("baseline" = fit(S, BASE), "+Act steps" = fit(S, c(BASE, "post1968", "post1973")))
  panel(p8, paste0("Table B14. Pre-1975 regime steps (", TAG, ")"), c("delta_oadr_10", "post1968", "post1973", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
        c("Given the intercept and basis_pre1975 (= 1 to 1974), the two steps are identified inside the pre-1975 window only: post1968 contrasts 1965-67 with 1968-74,",
          "and post1973 is numerically an indicator for 1973-74 (post1973 = 1 - basis_pre1975 + I{1973, 1974}). The column lets the pre-1975 level float in three",
          "sub-periods; the projection is unchanged. The positive 1973-74 step is not a tightening effect: those years hold the Ugandan Asian admissions, the rush",
          "of dependants before the 1971 Act bit, and EEC nationals, who entered freely from accession and are counted in the non-British series until the 1975",
          "change of basis. Over 1965-78 alone the projection is collinear with time (r = -0.97), so these are the checks that matter."))
  line("+trend (VIF 63, sign only)", fit(S, c(BASE, "trend")), c("delta_oadr_10", "trend"))
}

# --- 9 the projection horizon -----------------------------------------------------------------------------------
H <- paste0("delta_oadr_", c(1, 3, 5, 7, 10))
p9 <- setNames(lapply(H, function(h) fit(S, swap(BASE, "delta_oadr_10", h))), H)
p9_hac <- vapply(H, function(h) hacF(p9[[h]], h), numeric(2))
panel(p9, paste0("Table B15. The projection horizon (", TAG, ")"), c(H, "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 14,
      c(sprintf("HAC F of the horizon term in each column: %s.", paste(sprintf("%s %.2f (p %s)", H, p9_hac[1, ], vapply(p9_hac[2, ], pf, character(1))), collapse = "; ")),
        "The fit improves monotonically with the horizon; the one-year change carries nothing."))

# --- 10 British citizens --------------------------------------------------------------------------------------------
p10 <- list("mig_rate" = fit(S, BASE), "brit_rate" = fit(S, c("L_brit_rate", "delta_oadr_10", CT), "brit_rate"), "mig_rate +brit_rate" = fit(S, c(BASE, "brit_rate")))
panel(p10, paste0("Table B16. British citizens: the uncontrolled flow as placebo and as control (", TAG, ")"), c("delta_oadr_10", "brit_rate", "L_brit_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 20,
      c("Column 2: the fiscal specification for British-citizen inflows, all origins (LTIM 2.00; no basis step, the British series has no change of basis). A response is a",
        "partial placebo failure (nationality-law and return channels). Column 3 holds the British flow constant in the controlled-flow equation: a lower bound on the projection.",
        if (!EXT) "The core-sample manuscript figure uses British citizens arriving from outside the EU (IPS 3.01); see manuscript_1964.R." else NULL))
line("mig_rate +L_brit_rate", fit(S, c(BASE, "L_brit_rate")), c("delta_oadr_10", "L_brit_rate"))

# --- 11 sub-samples and dynamics ----------------------------------------------------------------------------------------
sub <- if (EXT) list("1965-1991" = c(1965, 1991), "1992-2019" = c(1992, 2019), "1965-2003" = c(1965, 2003), "1975-2019" = c(1975, 2019)) else list("1977-1997" = c(1977, 1997), "1998-2019" = c(1998, 2019), "1977-2003" = c(1977, 2003))
p11 <- lapply(sub, function(r) fit(subset(df, year >= r[1] & year <= r[2]), BASE)); p11[["+L2_mig_rate"]] <- fit(S, c(BASE, "L2_mig_rate"))
panel(p11, paste0("Table B17. Sub-samples and a second lag (", SN, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L2_mig_rate", STEP), w = 14,
      if (EXT) "1965-2003 ends before the A8 accession; 1975-2019 uses non-EU citizens only (no splice). The last column adds the second lag on the full sample." else "1977-2003 ends before the A8 accession. The last column adds the second lag on the full sample.")
if (EXT) { st <- c(1965, 1968, 1971, 1974, 1977, 1979)
  cat(sprintf("Sample starts (end 2019), delta_oadr_10: %s.\n", paste(vapply(st, function(y) sprintf("%d %s", y, b(fit(subset(df, year >= y & year <= 2019), BASE))), character(1)), collapse = "; "))) }
