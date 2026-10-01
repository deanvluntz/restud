# =============================================================================
# break_citizen.R -- timing B (projection t-1): the best models that remove the post-2008 break on the Citizen sample alone
# (Sam, 30 Sep 2026: "ignoring Origin, test the best models for this").
# Search: lagged outcome + projection (t-1) + a tax form (required) + a labour form, a debt form and a debt-interest form (each may be
# empty) + none or one pool term; at most 8 parameters (intercept included). Pool = Sam's rules (no wage inflation, no origin-country
# youth share or relative income); short rates (bill rate, Bank Rate, gilt) are in the pool but flagged, because they are nearly
# flat from 2009 and can act as a post-2009 indicator.
# Citizen-only criteria: projection HAC p < .05; every term other than the lagged outcome and the projection HAC p < .10; max VIF of
# those terms < 20; sup-F p >= .10; post-2009 change in the projection HAC p >= .10.
# The best five (by AIC), and the best five without a short rate, are then tested: the full battery on the Citizen sample and the
# protocol (same specification on 1977-2019, Chow at 1977, residence-based outcome) -- reported, not used to select.
# RStudio: working directory = diss_replication/, then source() this file (several minutes).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); ED <- build_data(); S <- ED[["1964"]]$S
P <- "L_delta_oadr_10"; RATES <- c("L_tbill", "L_bank_rate", "L_gilt")
POOL_S <- setdiff(POOL_E, c("L_wage_infl", "L_youth", "L_relinc_rich", "L_relinc_dev"))
opt <- function(v) c(list(character(0)), as.list(v))
G <- expand.grid(tax = seq_along(F_TAX), lab = 0:length(F_LAB), debt = 0:length(F_DEBT), int = 0:length(F_INT), pool = 0:length(POOL_S))
pick <- function(v, i) if (i == 0) character(0) else v[i]
M <- as.matrix(S[, c("year", "mig_gross", unique(c("L_mig_gross", P, F_TAX, F_LAB, F_DEBT, F_INT, POOL_S)))])
core_B <- sub("^delta_oadr_10$", P, CORE_A); aic_ref <- AIC(fit(S, core_B, "mig_gross")); res <- list()
for (i in seq_len(nrow(G))) { sp <- c(F_TAX[G$tax[i]], pick(F_LAB, G$lab[i]), pick(F_DEBT, G$debt[i]), pick(F_INT, G$int[i]), pick(POOL_S, G$pool[i]))
  xs <- c("L_mig_gross", P, sp); q <- qols(M, "mig_gross", xs); if (is.null(q) || q$p[P] > 0.10 || any(q$p[sp] > 0.20)) next
  m <- fit(S[S$year %in% q$yrs, ], xs, "mig_gross"); hp <- hacp(m); if (hp[P] >= 0.05 || any(hp[sp] >= 0.10)) next
  if (length(sp) >= 2) { v <- vif(m); if (max(v[sp]) >= 20) next }
  b <- breaks(m, "mig_gross"); if (b["supf"] < 0.10 || b["chp"] < 0.10) next
  res[[paste(sp, collapse = " + ")]] <- list(xs = xs, aic = AIC(m), n = nobs(m), proj = coef(m)[P], pp = hp[P], supf = b["supf"], chp = b["chp"], ch = b["ch"], rate = any(sp %in% RATES)) }
cat(sprintf("Timing B, Citizen sample: %d specifications; %d pass and remove the break (reference: current core with the projection at t-1, AIC %.2f, N 55)\n", nrow(G), length(res), aic_ref))
if (length(res)) { a <- vapply(res, function(r) r$aic, 0); ord <- names(res)[order(a)]; nr <- ord[!vapply(res[ord], function(r) r$rate, TRUE)]
  show <- function(ks, title) { cat(sprintf("\n-- %s\n", title)); for (k in ks) { r <- res[[k]]
    cat(sprintf("   %-62s N %d AIC %7.2f (%+.2f vs core) proj %.4f (p %s) supF %s post09 %+.4f (p %s)\n", k, r$n, r$aic, r$aic - aic_ref, r$proj, pf2(r$pp), pf2(r$supf), r$ch, pf2(r$chp))) } }
  show(head(ord, 10), "Best ten by AIC (short-rate models marked by their terms)"); show(head(nr, 10), "Best ten without a short rate")
  cat("\n\n#################### Tests of the best five, and the best five without a short rate ####################\n")
  for (k in unique(c(head(ord, 5), head(nr, 5)))) { r <- res[[k]]; out <- run_protocol(k, r$xs, S)
    cat(sprintf("    AIC relative to the current core (projection t-1) on the same years: %+.2f; OOS RMSE from 1995: this model %.3f\n", r$aic - aic_ref, out$B1[["OOS RMSE from 1995"]])) } }
cat(sprintf("\nReference OOS RMSE from 1995: current core, projection t %.3f; projection t-1 %.3f\n", oos(fit(S, CORE_A, "mig_gross"), "mig_gross"), oos(fit(S, core_B, "mig_gross"), "mig_gross")))
cat("\nDone.\n")
