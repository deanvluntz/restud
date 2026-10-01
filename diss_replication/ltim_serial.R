# =============================================================================
# ltim_serial.R -- does the lag-2 serial correlation come from IPS sampling error? (Sam, 30 Sep 2026)
# serial_cycle.R: the core's residual autocorrelation is negative (-0.16, -0.23), the pattern that sampling noise in the outcome produces
# when the outcome's own lag is a regressor. Tests:
# (A) IPS vs LTIM outcome (non-EU citizens; LTIM = IPS plus Home Office asylum data, switcher and Irish adjustments; held from 1991) on the
#     same years, 1992-2019 (N 28; a short sample for seven parameters). Note: LTIM is still built on the IPS sample, so its sampling
#     error is largely the IPS's -- the adjustments add administrative components, they do not enlarge the survey.
# (B) the full Citizen sample with LTIM from 1991 and IPS before (log-level splice at 1991, so only the post-1991 changes differ).
# (C) outcomes with no sampling error at all: Home Office settlement grants (1965-2006) and work permits (1965-2008), log persons, own lag.
# (D) LTIM all-citizenship inflows (1965-2019), for reference.
# Same right-hand side as the current core (projection at t) throughout.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); SAMPLE <- "1964"; source("data_common.R"); A <- subset(df, year <= 2019)
A$ltim_gross <- A$ltim_rate + log(lag1(A$workpop))                                          # LTIM non-EU inflows, log persons up to a constant
k91 <- which(A$year == 1991); A$splice_gross <- A$mig_gross; A$splice_gross[A$year >= 1991] <- A$ltim_gross[A$year >= 1991] - (A$ltim_gross[k91] - A$mig_gross[k91])
A$settle_gross <- log(1000 * A$settle_total_k); A$permit_gross <- log(1000 * A$permits); A$all_gross <- log(1000 * A$all_in)
for (v in c("ltim_gross", "splice_gross", "settle_gross", "permit_gross", "all_gross")) A[[paste0("L_", v)]] <- lag1(A[[v]])
X <- setdiff(CORE_A, "L_mig_gross")
diag1 <- function(m) { r <- residuals(m); ac <- acf(r, lag.max = 3, plot = FALSE)$acf[-1]; ct <- coeftest(m, vcov. = vcovHAC(m))
  sprintf("N %2d  ACF %s  BG(1) %s BG(2) %s BG(3) %s LB(4) %s  lag %.3f  projection %.4f (p %s)", nobs(m), paste(sprintf("%+.2f", ac), collapse = " "), pf2(bgtest(m, 1)$p.value),
          pf2(bgtest(m, 2)$p.value), pf2(bgtest(m, 3)$p.value), pf2(Box.test(r, 4, "Ljung-Box")$p.value), coef(m)[2], ct["delta_oadr_10", 1], pf2(ct["delta_oadr_10", 4])) }
run <- function(d, y, lab) { m <- fit(d, c(paste0("L_", y), X), y); if (is.null(m)) { cat(sprintf("   %-44s (not estimable)\n", lab)); return(invisible()) }; cat(sprintf("   %-44s %s\n", lab, diag1(m))) }
cat(sprintf("Correlation of IPS and LTIM non-EU inflows (1991-2019): levels %.3f, annual changes %.3f\n",
            cor(A$ltim_gross, A$mig_gross, use = "pair"), cor(diff(A$ltim_gross), diff(A$mig_gross), use = "pair")))
S <- subset(A, year >= 1965); Y92 <- subset(A, year >= 1992)
cat("\n(A) Same years, 1992-2019 (the LTIM series starts in 1991, so its lag starts in 1992)\n")
run(Y92, "mig_gross", "IPS (the headline outcome)"); run(Y92, "ltim_gross", "LTIM")
cat("\n(B) Citizen sample 1965-2019\n")
run(S, "mig_gross", "IPS throughout (current core)"); run(S, "splice_gross", "IPS to 1990, LTIM from 1991 (spliced)")
run(subset(S, year >= 1977), "mig_gross", "IPS, 1977-2019"); run(subset(S, year >= 1977), "splice_gross", "spliced, 1977-2019")
cat("\n(C) Administrative outcomes (no sampling error)\n")
run(S, "settle_gross", "settlement grants, 1965-2006"); run(S, "permit_gross", "work permits and first permissions, 1965-2008")
run(subset(S, year <= 2006), "mig_gross", "IPS outcome on the same years, 1965-2006")
run(subset(S, year <= 2008), "mig_gross", "IPS outcome on the same years, 1965-2008")
cat("\n(D) LTIM all-citizenship inflows, 1965-2019 (includes British and EU citizens)\n")
run(S, "all_gross", "LTIM all citizenships")
cat("\nDone.\n")
