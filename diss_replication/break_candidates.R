# =============================================================================
# break_candidates.R -- can an economic indicator explain the post-2008 break?
# Sam, 30 Sep 2026. regime_timing.R: under the shifted timing (projection at t-1) the projection effect ends from 2008-10;
# the window holds the end of work permits (PBS), the Great Recession and the 2010 cap. Question: is there an economic
# indicator, or a configuration (an interaction with the projection), missing from the model whose inclusion removes the
# break -- even at some cost to the earlier diagnostics?
# Base (timing B): mig_gross ~ L_mig_gross + L_delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_int_tme (level).
# Timing A (projection at t) is run alongside for reference.
# For each candidate: its coefficient (HAC p), the projection, AIC against the base ON THE SAME YEARS, and two break tests:
#   sup-F (Andrews, 20% trimming) and the post-2009 change in the projection (post dummy + post x projection, HAC p).
# A candidate "removes the break" when sup-F p >= .10 AND the post-2009 change has p >= .10 -- on BOTH samples.
# Then pre-period costs: RESET (2,3), BG(1), BG(2), rainbow.
# Candidates enter at t-1 (inflation expectations at t, Sam's rule). Interactions are candidate x projection (plus the candidate).
# Section 3 is a SCREEN over all pairs of candidates: a screening result, not a prior; report as such.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); suppressWarnings(library(strucchange))
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
X <- c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_int_tme")
CANDS <- list(
  "cycle and output"   = c("L_growth", "L_growth_pc", "L_gap_obr", "L_lab_prod", "L_tfp"),
  "labour market"      = c("L_unem", "L_dln_vac_unem", "L_wage_infl", "L_real_wage", "L_particip", "L_replace"),
  "prices"             = c("L_inflation", "L_cpi_infl", "st_inf_exp"),
  "money and rates"    = c("L_bank_rate", "L_tbill", "L_gilt", "L_real_rate", "L_term_spread", "L_m4_growth", "L_r_g"),
  "fiscal"             = c("L_psnb_gdp", "L_psnd_gdp", "L_tme_real_growth", "L_tme_obr_gdp", "L_grants_gdp", "L_tax_gdp", "L_fc_budget"),
  "external"           = c("L_real_eri", "L_ca_gdp", "L_trade_bal", "L_tot", "L_openness"),
  "assets"             = c("L_real_house"),
  "alternatives and supply" = c("L_unem_dest", "L_unem_diff", "L_relinc_rich", "L_relinc_dev", "L_youth"),
  "EU substitution"    = c("L_eu_rate"))
INTER <- c("L_psnd_gdp", "L_psnb_gdp", "L_bank_rate", "L_real_rate", "L_ln_vac_unem", "L_unem", "L_int_tme", "L_growth", "L_gap_obr", "L_real_eri", "L_eu_rate", "L_unem_diff")
hac <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, ] else rep(NA, 4) }
supf <- function(m) suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value)
chow <- function(d, xs, P) { d$post09 <- as.numeric(d$year >= 2009); d$post_x_proj <- d$post09 * d[[P]]; m <- fit(d, c(xs, "post09", "post_x_proj"), OUTC); hac(m, "post_x_proj") }
# evaluate one right-hand side on the years where it and the base are both observed
evalx <- function(S, P, add) { xs <- c(P, X, add); yrs <- S$year[complete.cases(S[, c(OUTC, xs)])]; d <- S[S$year %in% yrs, ]
  m0 <- fit(d, c(P, X), OUTC); m <- fit(d, xs, OUTC); if (is.null(m)) return(NULL)
  ch <- chow(d, xs, P); list(m = m, n = nobs(m), dAIC = AIC(m) - AIC(m0), proj = hac(m, P), supf = supf(m), supf0 = supf(m0), ch = ch,
    reset = resettest(m)$p.value, bg1 = bgtest(m, 1)$p.value, bg2 = bgtest(m, 2)$p.value, rain = raintest(m)$p.value) }
RES <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]
  S$L_delta_oadr_10 <- lag1(S$delta_oadr_10)
  for (v in INTER) for (P in c("delta_oadr_10", "L_delta_oadr_10")) S[[paste0(v, "_x_", P)]] <- S[[v]] * S[[P]]
  cat("\n\n==================== ", L, " ====================\n", sep = "")
  for (P in c("L_delta_oadr_10", "delta_oadr_10")) { tl <- if (P == "L_delta_oadr_10") "B (projection t-1)" else "A (projection t)"
    b <- evalx(S, P, character(0)); cat(sprintf("\n-- Timing %s. Base: N %d, projection %.4f (p %s), sup-F p %s, post-2009 change %.4f (p %s); RESET %s BG(1) %s BG(2) %s rainbow %s\n",
      tl, b$n, b$proj[1], pf2(b$proj[4]), pf2(b$supf), b$ch[1], pf2(b$ch[4]), pf2(b$reset), pf2(b$bg1), pf2(b$bg2), pf2(b$rain)))
    cat(sprintf("   %-28s %4s %9s %7s %9s %7s %7s %7s %9s %7s %6s %6s %6s %6s\n", "candidate", "N", "coef", "p", "proj", "p", "dAIC", "sup-F", "post09", "p", "RESET", "BG1", "BG2", "rain"))
    rows <- list(); for (g in names(CANDS)) { cat(sprintf("   [%s]\n", g))
      for (v in c(CANDS[[g]], if (g == names(CANDS)[length(CANDS)]) NULL)) { r <- evalx(S, P, v); if (is.null(r)) next; cf <- hac(r$m, v)
        cat(sprintf("   %-28s %4d %9.4f %7s %9.4f %7s %7.2f %7s %9.4f %7s %6s %6s %6s %6s\n", v, r$n, cf[1], pf2(cf[4]), r$proj[1], pf2(r$proj[4]), r$dAIC, pf2(r$supf), r$ch[1], pf2(r$ch[4]), pf2(r$reset), pf2(r$bg1), pf2(r$bg2), pf2(r$rain)))
        rows[[v]] <- r } }
    cat("   [interactions with the projection: candidate + candidate x projection]\n")
    for (v in INTER) { iv <- paste0(v, "_x_", P); add <- unique(c(setdiff(v, X), iv)); r <- evalx(S, P, add); if (is.null(r)) next; cf <- hac(r$m, iv)
      cat(sprintf("   %-28s %4d %9.4f %7s %9.4f %7s %7.2f %7s %9.4f %7s %6s %6s %6s %6s\n", paste0(v, " x proj"), r$n, cf[1], pf2(cf[4]), r$proj[1], pf2(r$proj[4]), r$dAIC, pf2(r$supf), r$ch[1], pf2(r$ch[4]), pf2(r$reset), pf2(r$bg1), pf2(r$bg2), pf2(r$rain)))
      rows[[paste0(v, " x proj")]] <- r }
    RES[[paste(s, P)]] <- rows } }

cat("\n\n==================== Summary: which additions remove the break on BOTH samples? ====================\n")
for (P in c("L_delta_oadr_10", "delta_oadr_10")) { a <- RES[[paste("1964", P)]]; b <- RES[[paste("1977", P)]]; k <- intersect(names(a), names(b))
  ok <- k[vapply(k, function(v) a[[v]]$supf >= 0.10 && b[[v]]$supf >= 0.10 && a[[v]]$ch[4] >= 0.10 && b[[v]]$ch[4] >= 0.10, logical(1))]
  cat(sprintf("\n-- Timing %s: %d of %d remove the break on both samples\n", if (P == "L_delta_oadr_10") "B" else "A", length(ok), length(k)))
  for (v in ok) cat(sprintf("   %-28s Citizen: proj %.4f (p %s) dAIC %6.2f sup-F %s post09 p %s | Origin: proj %.4f (p %s) dAIC %6.2f sup-F %s post09 p %s\n", v,
    a[[v]]$proj[1], pf2(a[[v]]$proj[4]), a[[v]]$dAIC, pf2(a[[v]]$supf), pf2(a[[v]]$ch[4]), b[[v]]$proj[1], pf2(b[[v]]$proj[4]), b[[v]]$dAIC, pf2(b[[v]]$supf), pf2(b[[v]]$ch[4]))) }

cat("\n\n==================== Screen: pairs of candidates, timing B (screening result, not a prior) ====================\n")
POOL <- unlist(CANDS, use.names = FALSE); PR <- combn(POOL, 2, simplify = FALSE); out <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); S$L_delta_oadr_10 <- lag1(S$delta_oadr_10)
  out[[s]] <- lapply(PR, function(pr) { r <- tryCatch(evalx(S, "L_delta_oadr_10", pr), error = function(e) NULL); if (is.null(r)) return(NULL)
    c(supf = r$supf, chp = r$ch[4], ch = r$ch[1], dAIC = r$dAIC, proj = r$proj[1], pp = r$proj[4], n = r$n, p1 = hac(r$m, pr[1])[4], p2 = hac(r$m, pr[2])[4], reset = r$reset, bg2 = r$bg2) }) }
ok <- which(vapply(seq_along(PR), function(i) { a <- out[["1964"]][[i]]; b <- out[["1977"]][[i]]; !is.null(a) && !is.null(b) && all(c(a["supf"], b["supf"], a["chp"], b["chp"]) >= 0.10) }, logical(1)))
cat(sprintf("   %d pairs tried; %d remove the break on both samples. Listed by summed dAIC (best 15):\n", length(PR), length(ok)))
if (length(ok)) { sc <- vapply(ok, function(i) out[["1964"]][[i]]["dAIC"] + out[["1977"]][[i]]["dAIC"], 0); for (i in ok[order(sc)][1:min(15, length(ok))]) { a <- out[["1964"]][[i]]; b <- out[["1977"]][[i]]
  cat(sprintf("   %-34s Cit: proj %.4f (p %s) dAIC %6.2f supF %s post09 %.4f (p %s) terms p %s/%s | Ori: proj %.4f (p %s) dAIC %6.2f supF %s post09 %.4f (p %s) terms p %s/%s\n",
    paste(PR[[i]], collapse = " + "), a["proj"], pf2(a["pp"]), a["dAIC"], pf2(a["supf"]), a["ch"], pf2(a["chp"]), pf2(a["p1"]), pf2(a["p2"]), b["proj"], pf2(b["pp"]), b["dAIC"], pf2(b["supf"]), b["ch"], pf2(b["chp"]), pf2(b["p1"]), pf2(b["p2"]))) } }
cat("\nDone.\n")
