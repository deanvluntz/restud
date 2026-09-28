# =============================================================================
# prop1_model.R -- MANUSCRIPT (Sam, 27 Sep 2026): build the case in two steps.
#   Proposition 1  controlled immigration can be modelled with fiscal (and macro-political) determinants
#                  WITHOUT the demographic projection;
#   Proposition 2  the projection (delta_oadr_10) adds to the best such model.
# Runs BOTH samples in one go (it needs both for the selection rule):  source("prop1_model.R")
# Names and definitions: tables_common.R; data: data_common.R. Samples end 2019 (Millennium series extended by ONS/BoE/BIS/OBR
# for 2017-19); columns using extended series are re-estimated to 2016 under each panel.
# Scope (Sam, 27 Sep 2026): Proposition 1 is about FISCAL policy, so origin-country supply variables (youth share, relative
# income) and the party in government are not candidates; they are not part of the Treasury framework.
#
#   A  Candidate screen: each candidate added alone to the fiscal model without the projection, then with it.
#      Candidates and a-priori reasons (all t-1 unless start-of-year information):
#        labour demand       L_vac_wap (+), L_vac_unem (+)                     Hatton (2005); Mitchell, Pain and Riley (2011)
#        wage / price pressure  L_wage_infl, L_inflation, st_inf_exp, lt_inf_exp  imported labour as anti-inflation supply policy
#                            (inflation expectations: start of year t, contemporaneous)
#        monetary / debt     L_tbill, L_int_tme (+)                              debt-service squeeze on the room for manoeuvre
#        external balance    L_real_eri, L_ca_gdp, L_trade_bal, L_tot            1960s-70s balance-of-payments constraint (Brittan, 1969);
#                                                                               sterling's value to migrants
#        distribution / housing  L_labour_share (-), L_real_house (-)           employer demand; housing as a political cost
#        activity            L_growth
#      Screened but not selectable: L_eu_rate (core sample only), inf_exp_5_10y (from 1985), st_inf_exp and lt_inf_exp (end 2016).
#   B  Selection rule, fixed in advance: forward selection from the fiscal model WITHOUT the projection; a candidate is
#      admissible only if it is observed throughout both samples, has HAC p < 0.10 in BOTH samples and the same sign
#      in both; the admissible candidate with the lowest summed change in AIC enters; stop when none is admissible
#      or after 5 additions (keeps >= 28 residual degrees of freedom on the core sample once the projection is added).
#   C  Models, each sample: fiscal without and with the projection; fiscal + the a-priori fiscal addition (L_int_tme,
#      the debt-service share of spending) without and with; the selected model without and with.
#   D  Recursive one-step-ahead forecasts (expanding window): RMSE of the four main models.
#   E  The selected model for gross inflows (mig_gross), and its variables on the EU placebo (core).
# =============================================================================
source("tables_common.R")
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, STEP = STEP, Y0 = Y0, LAB = LAB, END = BOE_END) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
for (k in names(SMP)) SMP[[k]]$S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= SMP[[k]]$END)
fbase <- function(k, proj = FALSE) c("L_mig_rate", "L_tax_workpop", SMP[[k]]$STEP, "L_ln_debt_gdp", "L_unem", if (proj) "delta_oadr_10")
hacp <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }

POOL <- c("L_vac_wap", "L_vac_unem", "L_wage_infl", "L_inflation", "st_inf_exp", "lt_inf_exp", "L_tbill", "L_int_tme", "L_real_eri", "L_ca_gdp",
          "L_trade_bal", "L_tot", "L_labour_share", "L_real_house", "L_growth")
EXTRA_SCREEN <- c("inf_exp_5_10y", "L_eu_rate")   # st_inf_exp / lt_inf_exp are in POOL but end 2016, so the rule below cannot select them

# --- A. screen ------------------------------------------------------------------------------------
cat("\n===== A. Candidate screen: each variable added alone (sample to", SMP$core$END, ") =====\n")
cat(sprintf("%-16s %-9s %4s | %9s %7s %7s | %9s %7s %14s %7s\n", "candidate", "sample", "N", "no proj", "p", "dAIC", "+proj", "p", "delta_oadr_10", "p"))
for (v in c(POOL, EXTRA_SCREEN)) for (k in names(SMP)) {
  S <- SMP[[k]]$S; if (!(v %in% names(S)) || sum(!is.na(S[[v]])) < 25) next; S <- S[!is.na(S[[v]]), ]
  a0 <- fit(S, fbase(k)); a <- fit(S, c(fbase(k), v)); b <- fit(S, c(fbase(k, TRUE), v)); if (is.null(a) || is.null(b)) next
  ha <- hacp(a, v); hb <- hacp(b, v); hp <- hacp(b, "delta_oadr_10")
  cat(sprintf("%-16s %-9s %4d | %9.4f %7s %+7.2f | %9.4f %7s %14.4f %7s\n", v, k, nobs(a), ha[1], pf(ha[2]), AIC(a) - AIC(a0), hb[1], pf(hb[2]), hp[1], pf(hp[2])))
}
cat("dAIC: change in AIC from adding the candidate to the fiscal model without the projection. Definitions:\n"); defs(c(POOL, EXTRA_SCREEN))

# --- B. selection ----------------------------------------------------------------------------------
cat("\n===== B. Forward selection (rule stated in the header; no projection at any step) =====\n")
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
  mods <- list("fiscal, no proj" = fit(S, fbase(k)), "fiscal (baseline)" = fit(S, fbase(k, TRUE)),
               "+int_tme, no proj" = fit(S, c(fbase(k), APR)), "+int_tme" = fit(S, c(fbase(k, TRUE), APR)),
               "selected, no proj" = fit(S, c(fbase(k), SEL)), "selected" = fit(S, c(fbase(k, TRUE), SEL)))
  panel(mods, paste0("C. Proposition 1, then Proposition 2 (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", SEL, APR, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", STEP),
        w = 16, lw = 16, nested_F = TRUE,
        c(paste0("no proj = delta_oadr_10 omitted. selected = the variables chosen in B (", paste(SEL, collapse = ", "), "), chosen without the projection."),
          "L_int_tme is added a priori (debt service as a claim on the budget), not by fit."))
}

# --- D. out-of-sample ---------------------------------------------------------------------------------
cat("\n===== D. Recursive one-step-ahead forecasts (expanding window; actual lagged mig_rate) =====\n")
oos <- function(S, xs, from) { e <- c()
  for (t in from:max(S$year)) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; if (!nrow(te)) next
    m <- lm(reformulate(xs, "mig_rate"), data = tr[complete.cases(tr[, c("mig_rate", xs)]), ]); e <- c(e, te$mig_rate - predict(m, newdata = te)) }
  c(sqrt(mean(e^2, na.rm = TRUE)), sum(!is.na(e))) }
for (k in names(SMP)) for (from in if (k == "extended") c(1990, 1996, 2001) else c(1997, 2001)) {
  S <- SMP[[k]]$S; xs <- list("fiscal, no proj" = fbase(k), "fiscal (baseline)" = fbase(k, TRUE), "selected, no proj" = c(fbase(k), SEL), "selected" = c(fbase(k, TRUE), SEL))
  res <- vapply(xs, function(x) oos(S, x, from), numeric(2))
  cat(sprintf("%-9s forecasts %d-%d (n = %d): %s\n", k, from, max(S$year), res[2, 1], paste(sprintf("%s %.4f", colnames(res), res[1, ]), collapse = "; "))) }
cat("Caveat: the selected variables were chosen on the full samples, which flatters them out of sample; delta_oadr_10 was fixed in advance.\n")

# --- E. gross inflows and the EU placebo ------------------------------------------------------------------
for (k in names(SMP)) { S <- SMP[[k]]$S; GB <- c("L_mig_gross", setdiff(fbase(k), "L_mig_rate"))
  panel(list("fiscal, no proj" = fit(S, GB, "mig_gross"), "fiscal" = fit(S, c(GB, "delta_oadr_10"), "mig_gross"),
             "selected, no proj" = fit(S, c(GB, SEL), "mig_gross"), "selected" = fit(S, c(GB, SEL, "delta_oadr_10"), "mig_gross")),
        paste0("E. Gross inflows (", SMP[[k]]$LAB, SMP[[k]]$END, ")"), c("delta_oadr_10", SEL, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_gross", SMP[[k]]$STEP), w = 18) }
S <- subset(SMP$core$S, !is.na(eu_rate)); EUB <- c("L_eu_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")
panel(list("eu_rate" = fit(S, c(EUB, "delta_oadr_10"), "eu_rate"), "eu_rate, selected" = fit(S, c(EUB, SEL), "eu_rate"), "eu_rate, selected +proj" = fit(S, c(EUB, SEL, "delta_oadr_10"), "eu_rate")),
      paste0("E. The uncontrolled EU margin (core sample, 1977-", SMP$core$END, ")"), c("delta_oadr_10", SEL, "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_eu_rate"), w = 24,
      "A general migration determinant should move the EU flow as well; a policy variable should not.")
