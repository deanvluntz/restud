# =============================================================================
# reverse_build.R -- build the specification up in the reverse of Sam's release order (Sam, 30 Sep 2026).
# Start: lagged outcome + projection. Then add, in order, the tax burden, the labour-market term, the debt term and the debt-interest
# term, each in the form that fits best at that point (lowest AIC among its forms; the term is skipped if no form lowers AIC). Then fill
# with pool terms by forward selection (lowest AIC, each new term HAC p < .10, max VIF of the non-projection terms < 20, AIC must fall)
# until the parameter limit is reached. Limits: 7 and 8 parameters, intercept included (the current core has 7).
# A second, free-order run lets every remaining core slot and pool term compete at each step (same rules), as a comparison.
# Selection is on the Citizen sample (1965-2019), the main reference; the chosen specifications then go through the protocol
# (main_spec_protocol.R): the same specification on 1977-2019 with nothing re-selected, Chow at 1977, residence-based outcome.
# For information only, the ordered build is also run with selection on 1977-2019. Candidates must be observed in every year the current
# model uses (AIC compared on the same years). Timings: A (projection t) and B (projection t-1).
# Two rule sets: 'all forms' (the tax slot may take tax + debt interest per WAP, which absorbs the interest term; full pool) and
# 'Sam's rules' (tax alone in the tax slot, so the interest term is its own step; pool without wage inflation -- robustness line only,
# 29 Sep -- and without the origin-country supply terms youth share and relative income -- supply-side controls, not candidates, 27 Sep).
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); ED <- build_data()
RULES <- list("all forms" = list(slots = list(tax = F_TAX, labour = F_LAB, debt = F_DEBT, interest = F_INT), pool = POOL_E),
  "Sam's rules" = list(slots = list(tax = setdiff(F_TAX, "L_taxint_wap"), labour = F_LAB, debt = F_DEBT, interest = F_INT),
                       pool = setdiff(POOL_E, c("L_wage_infl", "L_youth", "L_relinc_rich", "L_relinc_dev"))))
try_add <- function(S, cur, cands, need_sig) { m0 <- fit(S, cur, "mig_gross"); n0 <- nobs(m0); best <- NULL
  for (v in cands) { m <- tryCatch(fit(S, c(cur, v), "mig_gross"), error = function(e) NULL); if (is.null(m) || nobs(m) != n0) next
    hp <- hacp(m); if (need_sig && hp[v] >= 0.10) next
    fr <- setdiff(names(coef(m))[-1], c(proj_of(cur))); if (need_sig && length(fr) >= 2) { vf <- vif(m); if (max(vf[intersect(names(vf), setdiff(fr, "L_mig_gross"))]) >= 20) next }
    a <- AIC(m); if (is.null(best) || a < best$aic) best <- list(v = v, aic = a, p = hp[v], coef = coef(m)[v], proj = coef(m)[proj_of(cur)], pp = hp[proj_of(cur)]) }
  if (!is.null(best)) best$d <- best$aic - AIC(m0); best }
build <- function(S, P, limit, free = FALSE, verbose = TRUE) { cur <- c("L_mig_gross", P); left <- SLOTS; m <- fit(S, cur, "mig_gross")
  if (verbose) cat(sprintf("   start: lagged outcome + projection, AIC %.2f, N %d\n", AIC(m), nobs(m)))
  if (!free) for (sl in names(SLOTS)) { if (length(cur) + 1 >= limit) break; b <- try_add(S, cur, SLOTS[[sl]], FALSE)
    if (!is.null(b) && b$d < 0) { cur <- c(cur, b$v); if (verbose) cat(sprintf("   + %-9s %-18s coef %8.4f (HAC p %s) dAIC %6.2f  projection %.4f (p %s)\n", sl, b$v, b$coef, pf2(b$p), b$d, b$proj, pf2(b$pp))) }
    else if (verbose) cat(sprintf("   + %-9s no form lowers AIC; skipped\n", sl)) }
  repeat { if (length(cur) + 1 >= limit) break
    cands <- if (free) c(unlist(left), POOL) else POOL; cands <- setdiff(cands, cur); b <- try_add(S, cur, cands, TRUE)
    if (is.null(b) || b$d >= 0) { if (verbose) cat("   no further addition passes (HAC p < .10, VIF < 20, lower AIC)\n"); break }
    sl <- names(left)[vapply(left, function(f) b$v %in% f, logical(1))]; if (length(sl)) left[[sl]] <- NULL
    cur <- c(cur, b$v); if (verbose) cat(sprintf("   + %-9s %-18s coef %8.4f (HAC p %s) dAIC %6.2f  projection %.4f (p %s)\n", if (length(sl)) sl else "pool", b$v, b$coef, pf2(b$p), b$d, b$proj, pf2(b$pp))) }
  m <- fit(S, cur, "mig_gross"); if (verbose) cat(sprintf("   => %d parameters, AIC %.2f: %s\n", length(coef(m)), AIC(m), paste(sub("^L_", "", cur), collapse = " + "))); cur }
S <- ED[["1964"]]$S; S7 <- subset(S, year >= 1977); CH <- list()
for (rn in names(RULES)) { SLOTS <- RULES[[rn]]$slots; POOL <- RULES[[rn]]$pool
 for (P in c("delta_oadr_10", "L_delta_oadr_10")) { tl <- if (P == "delta_oadr_10") "A (projection t)" else "B (projection t-1)"
  cat(sprintf("\n\n==================== %s; timing %s ====================\n", rn, tl))
  cat(sprintf("   reference: current core %.2f AIC (Citizen)\n", AIC(fit(S, sub("^delta_oadr_10$", P, CORE_A), "mig_gross"))))
  for (lim in c(7, 8)) {
    cat(sprintf("\n-- Ordered build (tax, labour, debt, interest, then pool), limit %d parameters, selection on the Citizen sample\n", lim)); a <- build(S, P, lim)
    CH[[sprintf("%s: ordered build, limit %d, timing %s", rn, lim, substr(tl, 1, 1))]] <- a
    cat(sprintf("-- Free-order build, limit %d, selection on the Citizen sample\n", lim)); b <- build(S, P, lim, free = TRUE)
    CH[[sprintf("%s: free-order build, limit %d, timing %s", rn, lim, substr(tl, 1, 1))]] <- b
    cat(sprintf("-- For information: ordered build with selection on 1977-2019, limit %d\n", lim)); build(S7, P, lim) } } }
cat("\n\n#################### The selected specifications through the protocol ####################\n")
CH <- CH[!duplicated(vapply(CH, function(x) paste(sort(x), collapse = "+"), ""))]
for (nm in names(CH)) run_protocol(nm, CH[[nm]], S)
cat("\nDone.\n")
