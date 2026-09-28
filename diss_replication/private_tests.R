# =============================================================================
# private_tests.R -- PRIVATE TESTS, NOT FOR PUBLICATION (Sam, 28 Sep 2026): the candidate screens (levels, log forms, gross inflows),
# the pre-stated forward-selection rule, the augmented models (debt interest, inflation, CPI), the out-of-sample comparison of the
# augmented models, sub-samples, the second lag and the sample starts. Both samples in one run:  source("private_tests.R")
# Criterion (Sam): a variable extends the fiscal model only if it (a) improves the model without the projection AND (b) improves
# the model beside the projection. The published tables are B6-B25 (robustness_tables.R, without_projection.R, controls_tables.R).
# Runs LAST in run_all_1964.R so that variable definitions print under the published tables first.
# =============================================================================
source("tables_common.R")
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, STEP = STEP, Y0 = Y0, LAB = LAB, END = BOE_END) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
for (k in names(SMP)) SMP[[k]]$S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= SMP[[k]]$END)
fbase <- function(k, proj = FALSE) c("L_mig_rate", "L_tax_workpop", SMP[[k]]$STEP, "L_ln_debt_gdp", "L_unem", if (proj) "delta_oadr_10")
hacp <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }

FISCAL <- c("L_int_tme", "L_int_gdp", "L_psnb_gdp", "L_psnd_gdp", "L_tax_gdp", "fc_budget", "fc_planned", "fc_medium", "L_grants_gdp", "L_grants_wap", "L_tme_gdp", "gad_earn", "gad_policy", "L_gilt", "L_r_g", "L_real_rate")
MACRO  <- c("L_vac_wap", "L_vac_unem", "L_wage_infl", "L_inflation", "L_cpi_infl", "st_inf_exp", "lt_inf_exp", "L_tbill", "L_real_eri", "L_ca_gdp", "L_trade_bal", "L_tot", "L_labour_share", "L_real_house", "L_growth")
# "Treasury view" candidates (Sam, 28 Sep 2026; Macpherson, 2014): free trade -> openness; sound money -> broad money growth (inflation and
# rates are above); supply side -> participation; revenue "beyond a certain point" -> distance from the revenue ceiling; spending control ->
# real spending growth; institutions and rules -> Bank independence (1998), the golden rule (1998-2009), the OBR era (2010)
TREASURY <- c("L_openness", "L_m4_growth", "L_particip", "L_tax_ceiling", "L_tme_real_growth", "boe_indep", "golden_rule", "obr_era")
LOGS <- paste0("L_ln_", c("vac_wap", "vac_unem", "int_tme", "int_gdp", "tbill", "gilt", "tme_gdp", "grants_gdp", "grants_wap", "psnd_gdp", "tax_gdp", "real_eri", "tot", "labour_share", "openness", "particip"))
POOL <- c(FISCAL, MACRO, TREASURY, LOGS)
EXTRA_SCREEN <- c("inf_exp_5_10y", "L_eu_rate")   # st_inf_exp / lt_inf_exp are in POOL but end 2016, so the rule below cannot select them

# --- A. screen: each candidate alone, without the projection (a) and beside it (b), on both samples, on its own rows ------------
screen <- function(cands, dv = "mig_rate", title) {
  cat("\n=====", title, "=====\n")
  cat(sprintf("%-18s %-9s %4s | %9s %7s %7s %7s | %9s %7s %7s %14s %7s\n", "candidate", "sample", "N", "(a) coef", "p", "dAIC", "RESETp", "(b) coef", "p", "dAIC", "delta_oadr_10", "p"))
  for (v in cands) for (k in names(SMP)) {
    S <- SMP[[k]]$S; if (!(v %in% names(S)) || sum(!is.na(S[[v]])) < 25) next
    fb <- fbase(k); if (dv == "mig_gross") fb <- sub("^L_mig_rate$", "L_mig_gross", fb)
    S <- S[complete.cases(S[, c(dv, fb, "delta_oadr_10", v)]), ]
    a0 <- fit(S, fb, dv); a <- fit(S, c(fb, v), dv); b0 <- fit(S, c(fb, "delta_oadr_10"), dv); b <- fit(S, c(fb, "delta_oadr_10", v), dv); if (is.null(a) || is.null(b)) next
    ha <- hacp(a, v); hb <- hacp(b, v); hp <- hacp(b, "delta_oadr_10")
    cat(sprintf("%-18s %-9s %4d | %9.4f %7s %+7.2f %7s | %9.4f %7s %+7.2f %14.4f %7s\n", v, k, nobs(a), ha[1], pf(ha[2]), AIC(a) - AIC(a0), pf(resettest(a)$p.value), hb[1], pf(hb[2]), AIC(b) - AIC(b0), hp[1], pf(hp[2])))
  }
  cat("(a): the candidate added to the fiscal model without the projection, dAIC against that model on the same rows, RESET p of the augmented model;\n",
      "(b): the candidate added beside the projection, dAIC against the baseline on the same rows. A variable that extends the fiscal model must improve both.\n", sep = "")
}
screen(c(FISCAL, MACRO, TREASURY, EXTRA_SCREEN), "mig_rate", paste0("P1. Candidate screen, levels: each variable added alone, without the projection (a) and beside it (b) (samples to ", SMP$core$END, ")"))
screen(LOGS, "mig_rate", paste0("P2. Candidate screen, log forms of the positive-valued controls (samples to ", SMP$core$END, ")"))
screen(c(FISCAL, MACRO, TREASURY, LOGS), "mig_gross", paste0("P3. Candidate screen for GROSS inflows (mig_gross), levels and log forms (samples to ", SMP$core$END, ")"))
defs(c(FISCAL, MACRO, TREASURY, LOGS, EXTRA_SCREEN))

# --- B. selection ----------------------------------------------------------------------------------
cat("\n===== Forward selection over levels, log forms and the Treasury-view candidates (rule stated in the header; no projection at any step) =====\n")
sel <- character(0)
for (step in 1:5) {
  best <- NULL
  for (v in setdiff(POOL, sel)) {
    ok <- TRUE; sc <- 0; sg <- c()
    for (k in names(SMP)) { S <- SMP[[k]]$S; if (!(v %in% names(S)) || anyNA(S[[v]])) { ok <- FALSE; break }
      m0 <- fit(S, c(fbase(k), sel)); m <- fit(S, c(fbase(k), sel, v)); h <- hacp(m, v)
      if (is.na(h[2]) || h[2] >= 0.10) ok <- FALSE; sg <- c(sg, sign(h[1])); sc <- sc + AIC(m) - AIC(m0) }
    if (ok && length(unique(sg)) == 1 && (is.null(best) || sc < best$sc)) best <- list(v = v, sc = sc)
  }
  if (is.null(best)) { cat("no admissible candidate at step", step, "\n"); break }
  sel <- c(sel, best$v); cat(sprintf("step %d: %-14s summed dAIC %+.2f\n", step, best$v, best$sc))
}
SEL <- sel; cat("selected:", paste(SEL, collapse = ", "), "\n")

# --- C. the models ----------------------------------------------------------------------------------
APR <- c("L_int_tme")
for (k in names(SMP)) {
  S <- SMP[[k]]$S; STEP <- SMP[[k]]$STEP
  mods <- list("fiscal, no proj" = fit(S, fbase(k)), "fiscal" = fit(S, fbase(k, TRUE)),
               "+int_tme, no proj" = fit(S, c(fbase(k), APR)), "+int_tme" = fit(S, c(fbase(k, TRUE), APR)),
               "selected, no proj" = fit(S, c(fbase(k), SEL)), "selected" = fit(S, c(fbase(k, TRUE), SEL)),
               "selected +int_tme, no proj" = fit(S, c(fbase(k), SEL, APR)), "selected +int_tme" = fit(S, c(fbase(k, TRUE), SEL, APR)),
               "+cpi_infl, no proj" = fit(S, c(fbase(k), "L_cpi_infl")), "+cpi_infl" = fit(S, c(fbase(k, TRUE), "L_cpi_infl")))
  panel(mods[1:4], paste0("P4. The fiscal model without and with the projection, and with debt interest (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", APR, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP),
        w = 18, lw = 16, nested_F = TRUE,
        c("no proj = delta_oadr_10 omitted. L_int_tme is added a priori (debt service as a claim on the budget), not by fit: it is null without the projection and",
          "positive beside it, because the interest burden and the projected ageing are negatively correlated over the sample."))
  panel(mods[5:10], paste0("P5. The selected control, alone and with debt interest; CPI inflation for comparison (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", SEL, "L_cpi_infl", APR, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP),
        w = 26, lw = 16, nested_F = TRUE,
        paste0("selected = the variable chosen in B (", paste(SEL, collapse = ", "), "), chosen without the projection. The projection's contribution (F vs no proj, dAIC) is the test. L_cpi_infl: the CPI measure of the same pressure (Sam, 28 Sep 2026)."))
  SMP[[k]]$mods <- mods
}

# --- D. out-of-sample ---------------------------------------------------------------------------------
cat("\n===== P6. Recursive one-step-ahead forecasts of the augmented models (expanding window; actual lagged mig_rate) =====\n")
oos <- function(S, xs, from) { e <- c()
  for (t in from:max(S$year)) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; if (!nrow(te)) next
    m <- lm(reformulate(xs, "mig_rate"), data = tr[complete.cases(tr[, c("mig_rate", xs)]), ]); e <- c(e, te$mig_rate - predict(m, newdata = te)) }
  c(sqrt(mean(e^2, na.rm = TRUE)), sum(!is.na(e))) }
for (k in names(SMP)) for (from in if (k == "extended") c(1990, 1996, 2001) else c(1997, 2001)) {
  S <- SMP[[k]]$S; xs <- list("fiscal, no proj" = fbase(k), "fiscal" = fbase(k, TRUE), "+int_tme, no proj" = c(fbase(k), APR), "+int_tme" = c(fbase(k, TRUE), APR),
                              "selected, no proj" = c(fbase(k), SEL), "selected" = c(fbase(k, TRUE), SEL))
  res <- vapply(xs, function(x) oos(S, x, from), numeric(2))
  cat(sprintf("%-9s forecasts %d-%d (n = %d): %s\n", k, from, max(S$year), res[2, 1], paste(sprintf("%s %.4f", colnames(res), res[1, ]), collapse = "; "))) }
cat("Caveat: the selected variables were chosen on the full samples, which flatters them out of sample; delta_oadr_10 was fixed in advance.\n")

# --- E. gross inflows and the EU placebo ------------------------------------------------------------------
for (k in names(SMP)) { S <- SMP[[k]]$S; GB <- c("L_mig_gross", setdiff(fbase(k), "L_mig_rate"))
  panel(list("fiscal, no proj" = fit(S, GB, "mig_gross"), "fiscal" = fit(S, c(GB, "delta_oadr_10"), "mig_gross"),
             "+int_tme, no proj" = fit(S, c(GB, APR), "mig_gross"), "+int_tme" = fit(S, c(GB, APR, "delta_oadr_10"), "mig_gross"),
             "selected, no proj" = fit(S, c(GB, SEL), "mig_gross"), "selected" = fit(S, c(GB, SEL, "delta_oadr_10"), "mig_gross")),
        paste0("P7. Gross inflows: the fiscal, debt-interest and selected models (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", APR, SEL, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_gross", SMP[[k]]$STEP), w = 18) }
S <- subset(SMP$core$S, !is.na(eu_rate)); EUB <- c("L_eu_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")
panel(list("eu_rate" = fit(S, c(EUB, "delta_oadr_10"), "eu_rate"), "eu_rate +int_tme" = fit(S, c(EUB, "delta_oadr_10", APR), "eu_rate"),
           "eu_rate, selected" = fit(S, c(EUB, SEL), "eu_rate"), "eu_rate, selected +proj" = fit(S, c(EUB, SEL, "delta_oadr_10"), "eu_rate")),
      paste0("P8. The uncontrolled EU margin with the augmented models (core sample, 1977-", SMP$core$END, ")"), c("delta_oadr_10", APR, SEL, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_eu_rate"), w = 24,
      "A general migration determinant should move the EU flow as well; a policy-pressure variable should not.")
# --- P9 sub-samples, second lag, sample starts (from robustness_tables.R, 28 Sep 2026) ----------------------------------------
for (k in names(SMP)) { d <- SMP[[k]]$df; S <- SMP[[k]]$S; STEP <- SMP[[k]]$STEP; EXT <- k == "extended"
  sub <- if (EXT) list("1965-1991" = c(1965, 1991), "1992-2019" = c(1992, 2019), "1965-2003" = c(1965, 2003), "1975-2019" = c(1975, 2019)) else list("1977-1997" = c(1977, 1997), "1998-2019" = c(1998, 2019), "1977-2003" = c(1977, 2003))
  p <- c(list("main" = fit(S, fbase(k, TRUE))), lapply(sub, function(r) fit(subset(d, year >= r[1] & year <= r[2]), fbase(k, TRUE)))); p[["+L2_mig_rate"]] <- fit(S, c(fbase(k, TRUE), "L2_mig_rate"))
  panel(p, paste0("P9. Sub-samples and a second lag (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L2_mig_rate", STEP), w = 14)
  if (EXT) { st <- c(1965, 1968, 1971, 1974, 1977, 1979); cat("Sample starts (end 2019), delta_oadr_10:", paste(vapply(st, function(y) { m <- fit(subset(d, year >= y & year <= 2019), fbase(k, TRUE)); sprintf("%d %.4f", y, coef(m)["delta_oadr_10"]) }, character(1)), collapse = "; "), "\n") } }
# --- P10 the British inflow: grants abroad (cz_04, 1962-82), sample starts, return-migration pool, nationality-law regimes ---------
# Sam, 28 Sep 2026: how much of the British-inflow response is carried by the early years (events in former colonies) and by
# citizenship policy; grants and naturalisations tried both for the current year and lagged.
for (k in names(SMP)) { S <- SMP[[k]]$S; d <- SMP[[k]]$df; EXT <- k == "extended"; CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem"); BR <- c("L_brit_rate", "delta_oadr_10", CT)
  cat("\nP10 ", k, " sample: the British inflow (brit_rate)\n", sep = "")
  m <- fit(subset(S, year <= 1983), c(BR, "cit_overseas_k"), "brit_rate")
  if (!is.null(m)) { ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("  to 1983 +cit_overseas_k: %.4f (p %s), delta_oadr_10 %.4f (p %s), N %d\n", ct["cit_overseas_k", 1], pf(ct["cit_overseas_k", 4]), ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), nobs(m))) }
  st <- if (EXT) c(1965, 1970, 1975, 1978:1990, 1995) else c(1977:1990, 1995, 2000)   # every start 1978-90 (Sam, 28 Sep 2026: when does the British response fade?)
  cat("  sample starts (end 2019), delta_oadr_10 in the British equation:\n")
  for (y0 in st) { m <- fit(subset(d, year >= y0 & year <= 2019), BR, "brit_rate"); if (is.null(m)) next; ct <- coeftest(m, vcov. = vcovHAC(m))
    m2 <- if (!EXT) fit(subset(d, year >= y0 & year <= 2019), c("L_brit_noneu", "delta_oadr_10", CT), "brit_noneu") else NULL; c2 <- if (is.null(m2)) "" else { cc <- coeftest(m2, vcov. = vcovHAC(m2)); sprintf(" | brit_noneu (IPS 3.01) %.4f (p %s)", cc["delta_oadr_10", 1], pf(cc["delta_oadr_10", 4])) }
    cat(sprintf("    %d-2019: %.4f (p %s), N %d, AIC %.2f%s\n", y0, ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), nobs(m), AIC(m), c2)) }
  # the British Nationality Act 1981 (in force 1983) as the factor: the projection before and after, and the interaction
  S$proj_x_bna <- S$delta_oadr_10 * S$bna1981; m <- fit(S, c(BR, "bna1981", "proj_x_bna"), "brit_rate"); ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("  BNA 1981 split: projection before 1983 %.4f (p %s), change from 1983 %.4f (p %s), step %.3f (p %s); implied after 1983 %.4f\n", ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), ct["proj_x_bna", 1], pf(ct["proj_x_bna", 4]), ct["bna1981", 1], pf(ct["bna1981", 4]), ct["delta_oadr_10", 1] + ct["proj_x_bna", 1]))
  for (r in list(c(SMP[[k]]$Y0, 1982), c(1983, 2019))) { m <- fit(subset(d, year >= r[1] & year <= r[2]), BR, "brit_rate"); if (is.null(m)) next; ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("    %d-%d alone: %.4f (p %s), N %d\n", r[1], r[2], ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), nobs(m))) }
  cat("  sample ends (start ", SMP[[k]]$Y0, "), delta_oadr_10 in the British equation:\n", sep = "")
  for (y1 in c(1990, 1997, 2003, 2010)) { m <- fit(subset(S, year <= y1), BR, "brit_rate"); if (is.null(m)) next; ct <- coeftest(m, vcov. = vcovHAC(m))
    cat(sprintf("    %d-%d: %.4f (p %s), N %d\n", SMP[[k]]$Y0, y1, ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), nobs(m))) }
  if (!EXT) {   # who responds: UK-born returners (IPS 3.18) or British citizens not born in the UK (3.01 minus 3.18), by origin
    fb <- function(dv, xs = NULL) fit(S, c(paste0("L_", dv), "delta_oadr_10", CT, xs), dv)
    p <- list("brit_noneu" = fb("brit_noneu"), "ukborn_noneu" = fb("ukborn_noneu"), "brit_desc_noneu" = fb("brit_desc_noneu"), "ukborn_all" = fb("ukborn_all"),
              "ukborn_oce" = fb("ukborn_oce"), "ukborn_nor" = fb("ukborn_nor"), "ukborn_asi" = fb("ukborn_asi"), "ukborn_sub" = fb("ukborn_sub"), "brit_noneu +ukborn" = fb("brit_noneu", "ukborn_noneu"))
    panel(p, "P10. British citizens from outside the EU: UK-born returners (IPS 3.18) against citizens not born in the UK (core sample, 1977-2019)",
          c("delta_oadr_10", "ukborn_noneu", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_brit_noneu", "L_ukborn_noneu", "L_brit_desc_noneu", "L_ukborn_all", "L_ukborn_oce", "L_ukborn_nor", "L_ukborn_asi", "L_ukborn_sub"), w = 16, lw = 18,
          c("ukborn_x: UK-born persons arriving from region x (returning emigrants and their UK-born children); brit_desc_noneu: British citizens from outside the EU less the UK-born from there,",
            "i.e. citizens by descent, registration or naturalisation abroad. If the projection moves brit_desc_noneu and not ukborn_noneu, the British response is not return migration."))
    cat(sprintf("  brit_desc_noneu (thousands): mean %.1f, min %.1f (%d), share of British non-EU inflow %.0f%% (1977-79) -> %.0f%% (2017-19)\n", mean(S$brit_desc_noneu_k, na.rm = TRUE), min(S$brit_desc_noneu_k, na.rm = TRUE), S$year[which.min(S$brit_desc_noneu_k)],
                100 * mean(S$brit_desc_noneu_k[S$year <= 1979] / (S$brit_desc_noneu_k[S$year <= 1979] + S$ukborn_noneu_k[S$year <= 1979])), 100 * mean(S$brit_desc_noneu_k[S$year >= 2017] / (S$brit_desc_noneu_k[S$year >= 2017] + S$ukborn_noneu_k[S$year >= 2017])))) }
  p <- list("brit_rate" = fit(S, BR, "brit_rate"), "+cit_grants +L_cit_grants" = fit(S, c(BR, "cit_grants_k", "L_cit_grants_k"), "brit_rate"), "+cit_nat +L_cit_nat" = fit(S, c(BR, "cit_nat_k", "L_cit_nat_k"), "brit_rate"),
            "+emig_brit_5" = fit(S, c(BR, "emig_brit_5"), "brit_rate"), "+L_emig_brit" = fit(S, c(BR, "L_emig_brit"), "brit_rate"), "+law steps" = fit(S, c(BR, "bna1981", "hk1990", "nia2002"), "brit_rate"),
            "+relinc_rich" = fit(S, c(BR, "L_relinc_rich"), "brit_rate"), "+real_eri" = fit(S, c(BR, "L_real_eri"), "brit_rate"), "+eu_rate" = fit(S, c(BR, "eu_rate"), "brit_rate"))
  panel(p, paste0("P10. The British inflow: citizenship policy (both timings), the return pool, nationality-law regimes, relative income, sterling, the EU flow (", SMP[[k]]$LAB, "2019)"),
        c("delta_oadr_10", "cit_grants_k", "L_cit_grants_k", "cit_nat_k", "L_cit_nat_k", "emig_brit_5", "L_emig_brit", "bna1981", "hk1990", "nia2002", "L_relinc_rich", "L_real_eri", "eu_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_brit_rate"), w = 14, lw = 16,
        c("Candidates for what else could carry the British-inflow correlation: the stock of recent British emigrants (returns), the nationality-law regimes that created citizens by descent",
          "or registration, UK income relative to the settler destinations (US, Australia; Maddison), the real exchange rate, and the EU inflow (a common mobility factor)."))
}
