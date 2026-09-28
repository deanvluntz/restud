# =============================================================================
# no_projection.R -- MANUSCRIPT (Sam, 27-28 Sep 2026): what the fiscal model predicts WITHOUT the projection, and whether
# any observed fiscal or macroeconomic variable can either replace the projection or improve the model beside it.
# Runs BOTH samples in one go (the selection rule needs both):  source("no_projection.R")
# Names and definitions: tables_common.R; data: data_common.R. Samples end 2019 (Millennium series extended by ONS/BoE/BIS/OBR
# for 2017-19); columns using extended series are re-estimated to 2016 under each panel.
# Scope (Sam, 27 Sep 2026): the question is about FISCAL policy, so origin-country supply variables (youth share, relative
# income) and the party in government are not candidates; they are not part of the Treasury framework.
# Sam's criterion for an extension of the fiscal model (28 Sep 2026): a missing variable should (a) improve the model's ability
# to determine inflows on its own, without the projection, AND (b) still improve the model once the projection is added.
#
#   A  Candidate screens (B18 levels, B19 log forms, B20 gross inflows): each candidate added alone to the fiscal model without the
#      projection (half a), then with it (half b). Log forms (Sam, 28 Sep 2026): L_ln_x for every positive-valued control.
#      Candidates and a-priori reasons (all t-1 unless start-of-year information):
#        fiscal: debt service   L_int_tme (+), L_int_gdp                          debt-service squeeze on the room for manoeuvre
#        fiscal: balance/plans  L_psnb_gdp, L_psnd_gdp, L_tax_gdp, fc_budget, fc_planned, fc_medium, L_grants_gdp, L_grants_wap, L_tme_gdp
#        fiscal: actuarial      gad_earn, gad_policy                              the Government Actuary's projected contribution pressure
#        rates                  L_tbill, L_gilt, L_r_g, L_real_rate
#        labour demand          L_vac_wap (+), L_vac_unem (+)                      Hatton (2005); Mitchell, Pain and Riley (2011)
#        wage / price pressure  L_wage_infl, L_inflation, L_cpi_infl (A47, Sam 28 Sep), st_inf_exp, lt_inf_exp  imported labour as anti-inflation supply policy
#        external balance       L_real_eri, L_ca_gdp, L_trade_bal, L_tot          1960s-70s balance-of-payments constraint (Brittan, 1969)
#        distribution / housing L_labour_share (-), L_real_house (-)             employer demand; housing as a political cost
#        activity               L_growth
#      Screened but not selectable: L_eu_rate (core sample only), inf_exp_5_10y (from 1985), st_inf_exp and lt_inf_exp (end 2016).
#   B  Selection rule, fixed in advance: forward selection from the fiscal model WITHOUT the projection; a candidate is
#      admissible only if it is observed throughout both samples, has HAC p < 0.10 in BOTH samples and the same sign
#      in both; the admissible candidate with the lowest summed change in AIC enters; stop when none is admissible
#      or after 5 additions. (R run of 27 Sep: L_inflation alone; wage inflation is not observed throughout the extended sample.)
#   C  Models, each sample: fiscal without and with the projection; the a-priori fiscal addition (L_int_tme) without and with;
#      the selected variable (inflation) without and with; both additions together without and with.
#   D  Recursive one-step-ahead forecasts (expanding window): RMSE of the models in C.
#   E  Gross inflows, and the EU placebo (core) for every addition: a policy-pressure variable should not move the EU flow.
# History (disclose if written up): with the origin youth share and a party dummy admitted, the rule had picked youth + terms
# of trade, which absorbed the projection on the core sample; Sam then restricted the pool to fiscal and macro variables.
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
screen(c(FISCAL, MACRO, TREASURY, EXTRA_SCREEN), "mig_rate", paste0("Table B18. Candidate screen, levels: each variable added alone, without the projection (a) and beside it (b) (samples to ", SMP$core$END, ")"))
screen(LOGS, "mig_rate", paste0("Table B19. Candidate screen, log forms of the positive-valued controls (samples to ", SMP$core$END, ")"))
screen(c(FISCAL, MACRO, TREASURY, LOGS), "mig_gross", paste0("Table B20. Candidate screen for GROSS inflows (mig_gross), levels and log forms (samples to ", SMP$core$END, ")"))
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
  panel(mods[1:4], paste0("Table B21. The fiscal model without and with the projection, and with debt interest (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", APR, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP),
        w = 18, lw = 16, nested_F = TRUE,
        c("no proj = delta_oadr_10 omitted. L_int_tme is added a priori (debt service as a claim on the budget), not by fit: it is null without the projection and",
          "positive beside it, because the interest burden and the projected ageing are negatively correlated over the sample."))
  panel(mods[5:10], paste0("Table B22. The selected control, alone and with debt interest; CPI inflation for comparison (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", SEL, "L_cpi_infl", APR, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP),
        w = 26, lw = 16, nested_F = TRUE,
        paste0("selected = the variable chosen in B (", paste(SEL, collapse = ", "), "), chosen without the projection. The projection's contribution (F vs no proj, dAIC) is the test. L_cpi_infl: the CPI measure of the same pressure (Sam, 28 Sep 2026)."))
  SMP[[k]]$mods <- mods
}

# --- D. out-of-sample ---------------------------------------------------------------------------------
cat("\n===== Table B23. Recursive one-step-ahead forecasts (expanding window; actual lagged mig_rate) =====\n")
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
        paste0("Table B24. Gross inflows: the fiscal, debt-interest and selected models (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", APR, SEL, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_gross", SMP[[k]]$STEP), w = 18) }
S <- subset(SMP$core$S, !is.na(eu_rate)); EUB <- c("L_eu_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")
panel(list("eu_rate" = fit(S, c(EUB, "delta_oadr_10"), "eu_rate"), "eu_rate +int_tme" = fit(S, c(EUB, "delta_oadr_10", APR), "eu_rate"),
           "eu_rate, selected" = fit(S, c(EUB, SEL), "eu_rate"), "eu_rate, selected +proj" = fit(S, c(EUB, SEL, "delta_oadr_10"), "eu_rate")),
      paste0("Table B25. The uncontrolled EU margin (core sample, 1977-", SMP$core$END, ")"), c("delta_oadr_10", APR, SEL, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_eu_rate"), w = 24,
      "A general migration determinant should move the EU flow as well; a policy-pressure variable should not.")
