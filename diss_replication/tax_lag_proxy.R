# =============================================================================
# tax_lag_proxy.R -- what is the tax burden at t-2 picking up? (Sam, 30 Sep 2026)
# serial_cycle.R found that tax per WAP at t-2 fits better than at t-1 (and with both, t-1 goes null). Sam rejects a t-2 timing on
# grounds, and asks whether a variable in the data captures what the t-2 term does while the t-1 term keeps its role.
# Algebra: b1*tax_{t-1} + b2*tax_{t-2} = (b1 + b2)*tax_{t-1} - b2*(tax_{t-1} - tax_{t-2}). With b1 ~ 0 and b2 > 0, the pair says: the
# LEVEL at t-1 raises inflows and the most recent CHANGE in the burden lowers them. So the question is what drives the change in tax
# per WAP: fiscal drag (inflation and nominal earnings growth pushing receipts up with fixed thresholds), the cycle (receipts move
# with incomes and employment), or discretionary Budget measures.
# Step 1: the reparametrisation itself (change in the burden, level and log) beside tax at t-1.
# Step 2: candidate variables (t-1; inflation expectations at t) beside the core. For each: its coefficient, tax at t-1, AIC against the
# core on the same years, BG(2), out-of-sample error; then with tax at t-2 also added: does the t-2 term lose significance (absorbed)?
# A candidate "captures" the t-2 term when: its own HAC p < .10, tax at t-1 HAC p < .05, and tax at t-2 added on top has HAC p >= .10.
# Run on the Citizen sample (main, 1965-2019) and reported for 1977-2019 (same specification, nothing re-selected).
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); SAMPLE <- "1964"; source("data_common.R"); A <- subset(df, year <= 2019)
dl  <- function(x) lag1(c(NA, diff(log(x))))            # log change t-2 -> t-1
dd1 <- function(x) lag1(c(NA, diff(x)))                 # level change t-2 -> t-1
A$L2_tax_workpop <- lag1(A$L_tax_workpop); A$L_d_tax_workpop <- dd1(A$tax_workpop)
for (v in c("tax_gdp", "tax_inc", "tax_nics", "tax_nics_emp", "tax_indirect", "tax_vat", "tax_corp", "tax_direct", "tax_receipts")) if (v %in% names(A)) A[[paste0("L_dln_", v)]] <- dl(A[[v]])
A$L_d_tax_gdp <- dd1(A$tax_gdp); A$L_dln_workpop <- dl(A$workpop); A$L_d_unem <- dd1(A$unem); if ("workforce_k" %in% names(A)) A$L_dln_workforce <- dl(A$workforce_k)
if ("awe_gbp" %in% names(A)) A$L_dln_awe <- dl(A$awe_gbp)
S <- subset(A, year >= 1965); S7 <- subset(S, year >= 1977)
CANDS <- list(
  "reparametrisation"   = c("L_d_tax_workpop", "L_dln_tax_workpop"),
  "fiscal drag"         = c("L_inflation", "L_cpi_infl", "L_wage_infl", "st_inf_exp", "L_dln_awe"),
  "cycle and incomes"   = c("L_growth", "L_growth_pc", "L_gap_obr", "L_d_unem", "L_dln_workforce", "L_lab_prod", "L_real_wage"),
  "tax composition and ratio" = c("L_tax_gdp", "L_d_tax_gdp", "L_dln_tax_gdp", "L_dln_tax_inc", "L_dln_tax_nics", "L_dln_tax_nics_emp", "L_dln_tax_indirect", "L_dln_tax_vat", "L_dln_tax_corp", "L_dln_tax_direct", "L_dln_tax_receipts"),
  "Budget and fiscal stance" = c("L_fc_budget", "L_psnb_gdp", "L_tme_real_growth", "L_growth_fc"),
  "population"          = c("L_dln_workpop"))
row <- function(d, v) { m0 <- fit(d, CORE_A, "mig_gross"); m <- fit(d, c(CORE_A, v), "mig_gross"); if (is.null(m)) return(NULL); yrs <- attr(m, "dd")$year
  m0 <- fit(d[d$year %in% yrs, ], CORE_A, "mig_gross"); ct <- coeftest(m, vcov. = vcovHAC(m)); m2 <- fit(d[d$year %in% yrs, ], c(CORE_A, v, "L2_tax_workpop"), "mig_gross")
  c2 <- if (is.null(m2)) c(NA, NA, NA, NA) else coeftest(m2, vcov. = vcovHAC(m2))["L2_tax_workpop", ]
  list(n = nobs(m), zc = ct[v, 1], zp = ct[v, 4], tc = ct["L_tax_workpop", 1], tp = ct["L_tax_workpop", 4], pc = ct["delta_oadr_10", 1], pp = ct["delta_oadr_10", 4],
       dA = AIC(m) - AIC(m0), bg2 = bgtest(m, 2)$p.value, oos = oos(m, "mig_gross"), l2c = c2[1], l2p = c2[4]) }
for (nm in c("Citizen 1965-2019", "1977-2019")) { d <- if (nm == "1977-2019") S7 else S; cat(sprintf("\n==================== %s ====================\n", nm))
  m0 <- fit(d, CORE_A, "mig_gross"); m2 <- fit(d, c(CORE_A, "L2_tax_workpop"), "mig_gross"); c2 <- coeftest(m2, vcov. = vcovHAC(m2))
  cat(sprintf("   core: AIC %.2f, BG(2) %s, OOS %.3f | core + tax at t-2: tax t-1 %.4f (p %s), tax t-2 %.4f (p %s), dAIC %.2f\n", AIC(m0), pf2(bgtest(m0, 2)$p.value), oos(m0, "mig_gross"),
              c2["L_tax_workpop", 1], pf2(c2["L_tax_workpop", 4]), c2["L2_tax_workpop", 1], pf2(c2["L2_tax_workpop", 4]), AIC(m2) - AIC(m0)))
  cat(sprintf("   %-22s %3s %18s %18s %16s %7s %6s %6s %18s %s\n", "candidate", "N", "own coef (p)", "tax t-1 (p)", "projection (p)", "dAIC", "BG(2)", "OOS", "tax t-2 on top (p)", ""))
  for (g in names(CANDS)) { cat(sprintf("   [%s]\n", g)); for (v in CANDS[[g]]) { if (!v %in% names(d)) { cat(sprintf("   %-22s (not in the data)\n", v)); next }
    r <- tryCatch(row(d, v), error = function(e) NULL); if (is.null(r)) next
    cap <- r$zp < 0.10 && r$tp < 0.05 && !is.na(r$l2p) && r$l2p >= 0.10
    cat(sprintf("   %-22s %3d %9.4f (%s) %9.4f (%s) %8.4f (%s) %7.2f %6s %6.3f %9.4f (%s) %s\n", v, r$n, r$zc, pf2(r$zp), r$tc, pf2(r$tp), r$pc, pf2(r$pp), r$dA, pf2(r$bg2), r$oos,
                r$l2c, pf2(r$l2p), if (cap) "<- captures it" else "")) } } }
cat("\nDone.\n")

# The candidate that captures the t-2 term on both periods, through the protocol (same specification on 1977-2019, Chow at 1977, other outcomes)
S$L_d_tax_gdp <- A$L_d_tax_gdp[match(S$year, A$year)]
A$res_gross <- log(1000 * A$res_noneu_k); A$L_res_gross <- lag1(A$res_gross); A$cit_gross <- log(1000 * A$cit_noneu_k); A$L_cit_gross <- lag1(A$cit_gross)
for (v in c("res_gross", "L_res_gross", "cit_gross", "L_cit_gross")) S[[v]] <- A[[v]][match(S$year, A$year)]
invisible(run_protocol("current core + change in tax/GDP (t-2 -> t-1)", c(CORE_A, "L_d_tax_gdp"), S))
cat("\nDone (protocol).\n")
