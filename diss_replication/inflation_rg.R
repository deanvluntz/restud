# =============================================================================
# inflation_rg.R -- inflation expectations from the Millennium dataset, forecast r - g, the planned-borrowing x r - g terms, and what drives
# int_tme (banks vs the APF) (Sam, 1 Oct 2026).
# A. Inflation expectations, Millennium Q7 (quarterly; the year's Q1 value = start-of-year information, Sam's convention for expectations):
#    niesr_1y   NIESR one-year-ahead forecast (consumer expenditure deflator / RPI / RPIX), 1965-2010
#    comp_1y    spliced composite one-year-ahead expectation (CPI basis, simple wedges), 1965-2016
#    comp_510   spliced composite 5-10 years ahead, 1985-2016;  comp_510c  the same adjusted for the CPI wedge
#    (Bank of England, HMT-survey and household series start 1998-2006 and are not used.) Compared with the dataset's st_inf_exp.
# B. Forecast nominal growth = HMT start-of-year real growth forecast (growth_fc, 1976-) + each expectation; forecast r - g = previous year's
#    10-year gilt yield (Millennium A31, constructed GRY from the BoE price file for 2017-19) minus forecast nominal growth.
#    Outturn r - g (r_g in the dataset) = 10-year gilt yield minus outturn nominal GDP growth, extended to 2019 the same way.
# C. The screen's best interaction rows were planned short-horizon changes in borrowing x r - g (L_chgb_1 * L_r_g, L_chgb_2 * L_r_g). Here:
#    each planned change (chgb_1, chgb_2, chg_2; Budget of t-1) x each r - g (outturn; forecast with each expectation), as interaction
#    (both main terms + product) and product alone; on the years where both exist, and on BOTH samples with the planned ONE-year change from
#    the Budget backfilled (1971-) and a missing-year indicator for the years without it. Then the checks on the best row: without 2008-11,
#    to 2007, leave-one-out.
# D. int_tme: the Millennium series is public-sector debt interest INCLUDING the public-sector banks over TME. Split into the clean part
#    (excluding banks) and the bank part ((ANLO - JW2P) / EBFT): which part carries the coefficient? And the APF: central-government
#    interest gross (NMFX) vs net of the APF transfers (NMFX + MU74).
# Sam's criterion: added terms jointly HAC-significant (p < .10) and lower AIC, with AND without the projection.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.paths_only = TRUE); invisible(capture.output(source("psnb_paths.R"))); options(restud.paths_only = FALSE)
CORE6 <- setdiff(CORE_A, "L_int_tme"); NP <- setdiff(CORE6, "delta_oadr_10")
mf <- "../a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx"
q7 <- suppressMessages(as.data.frame(readxl::read_excel(mf, "Q7. Qrtly inflation expectation", col_names = FALSE, .name_repair = "minimal")))
yq <- suppressWarnings(as.numeric(q7[[1]])); for (i in seq_along(yq)) if (is.na(yq[i]) && i > 1) yq[i] <- yq[i - 1]
q1 <- which(q7[[2]] == "Q1" & !is.na(yq)); col <- function(j) { v <- suppressWarnings(as.numeric(q7[[j]][q1])); setNames(v, yq[q1]) }
EX <- data.frame(year = yq[q1], niesr_1y = col(3), comp_1y = col(21), comp_510 = col(22), comp_510c = col(23))
yrs <- S$year; for (v in names(EX)[-1]) S[[v]] <- EX[[v]][match(yrs, EX$year)]
cat("A. Inflation expectations (Q1 of each year), coverage and correlation with st_inf_exp:\n")
for (v in names(EX)[-1]) cat(sprintf("   %-10s %d-%d  r with st_inf_exp %.2f\n", v, min(EX$year[!is.na(EX[[v]])]), max(EX$year[!is.na(EX[[v]])]), cor(S[[v]], S$st_inf_exp, use = "pair")))
gilt <- A$gilt10_yield; k <- is.na(gilt) & A$year >= 2017; gilt[k] <- gy$gry_10[match(A$year[k], gy$year)]; A$gilt_x <- gilt
A$rg_out <- A$gilt_x - A$nom_gdp_growth
EXP <- c("niesr_1y", "comp_1y", "comp_510", "comp_510c", "st_inf_exp")
for (v in EXP) { e <- if (v == "st_inf_exp") A$st_inf_exp else EX[[v]][match(A$year, EX$year)]
  A[[paste0("gn_", v)]] <- A$growth_fc + e; A[[paste0("rgf_", v)]] <- pv(A$gilt_x) - A[[paste0("gn_", v)]] }
RG <- c("rg_out", paste0("rgf_", EXP))
for (v in RG) S[[paste0("L_", v)]] <- A[[v]][match(S$year - 1, A$year)]
cat(sprintf("\nB. r - g: outturn %d-%d; forecast versions from %s\n", min(A$year[!is.na(A$rg_out) & A$year >= 1964]), max(A$year[!is.na(A$rg_out)]),
            paste(sprintf("%s %d-%d", RG[-1], sapply(RG[-1], function(v) min(A$year[!is.na(A[[v]])])), sapply(RG[-1], function(v) max(A$year[!is.na(A[[v]])]))), collapse = "; ")))
jt <- function(m, add) { V <- tryCatch(vcovHAC(m), error = function(e) NULL); b <- coef(m)[add]; if (is.null(V) || any(is.na(b))) return(NA)
  W <- tryCatch(drop(t(b) %*% solve(V[add, add, drop = FALSE], b)), error = function(e) NA); stats::pf(W / length(add), length(add), df.residual(m), lower.tail = FALSE) }
ev <- function(dd, add, focus = tail(setdiff(add, ".miss"), 1)) { dd <- dd[complete.cases(dd[, c("mig_gross", CORE6, add)]), ]; m <- lm(reformulate(c(CORE6, add), "mig_gross"), dd); m0 <- lm(reformulate(CORE6, "mig_gross"), dd)
  n <- lm(reformulate(c(NP, add), "mig_gross"), dd); n0 <- lm(reformulate(NP, "mig_gross"), dd); a <- jt(m, add); e <- jt(n, add); pr <- coeftest(m, vcov. = vcovHAC(m))["delta_oadr_10", ]
  list(N = nrow(dd), y0 = min(dd$year), y1 = max(dd$year), p = a, dA = AIC(m) - AIC(m0), pn = e, dAn = AIC(n) - AIC(n0), proj = pr[1], pp = pr[4], bx = coef(m)[focus], bxn = coef(n)[focus],
       pass = !is.na(a) && a < .10 && AIC(m) < AIC(m0) && !is.na(e) && e < .10 && AIC(n) < AIC(n0)) }
pr <- function(lab, r) cat(sprintf("   %-44s %d-%d N %2d  product/term %8.4f (no proj %8.4f)  joint p %5s dAIC %6.2f | no proj p %5s dAIC %6.2f | proj %.4f (p %s)%s\n", lab, r$y0, r$y1, r$N, r$bx, r$bxn, pf2(r$p), r$dA,
                                    pf2(r$pn), r$dAn, r$proj, pf2(r$pp), if (r$pass) "  PASS" else ""))
cat("\nC. Planned change in borrowing (Budget of t-1) x r - g (t-1)\n")
one <- sapply(1964:2019, function(t) { x <- path(t, 0); if (is.null(x)) NA else unname(x[2] - x[1]) }); names(one) <- 1964:2019
S$bf1 <- ifelse(!is.na(S$L_chgb_1), S$L_chgb_1, one[as.character(S$year - 1)])           # planned 1-year change, backfilled from the 1971 Budget
nres <- 0; npass <- 0
for (p in c("L_chgb_1", "L_chgb_2", "L_chg_2")) for (r in paste0("L_", RG)) { S$.px <- S[[p]] * S[[r]]
  for (form in c("interaction", "product")) { add <- if (form == "interaction") c(p, r, ".px") else ".px"; rr <- ev(subset(S, year >= 1981), add); nres <- nres + 1; npass <- npass + rr$pass
    pr(sprintf("%s %s %s", sub("^L_", "", p), if (form == "interaction") "*" else "x", sub("^L_", "", r)), rr) } }
cat(sprintf("   %d rows, %d pass\n", nres, npass))
cat("\n  -- both samples: planned ONE-year change backfilled to the 1971 Budget, x r - g, + indicator for the years without either --\n")
for (r in paste0("L_", RG)) { S$.b <- S$bf1 * S[[r]]; S$.miss <- as.numeric(is.na(S$.b)); S$.b0 <- ifelse(is.na(S$.b), 0, S$.b); S$.p0 <- ifelse(is.na(S$bf1), 0, S$bf1); S$.r0 <- ifelse(is.na(S[[r]]), 0, S[[r]])
  for (y0 in c(1965, 1977)) { dd <- subset(S, year >= y0); ind <- if (length(unique(dd$.miss)) > 1) ".miss" else NULL
    pr(sprintf("bf1 * %s, from %d%s", sub("^L_", "", r), y0, if (is.null(ind)) "" else " (+indicator)"), ev(dd, c(".p0", ".r0", ".b0", ind)))
    pr(sprintf("bf1 x %s, from %d%s", sub("^L_", "", r), y0, if (is.null(ind)) "" else " (+indicator)"), ev(dd, c(".b0", ind))) } }
cat("\n  -- the screen's best row, L_chgb_1 * r_g (outturn), checks --\n")
S$.px <- S$L_chgb_1 * S$L_rg_out; base <- subset(S, year >= 1981); add <- c("L_chgb_1", "L_rg_out", ".px")
pr("all years", ev(base, add)); pr("without 2008-11", ev(subset(base, !year %in% 2008:2011), add)); pr("to 2007", ev(subset(base, year <= 2007), add))
loo <- sapply(base$year, function(y) { dd <- subset(base, year != y); dd <- dd[complete.cases(dd[, c("mig_gross", CORE6, add)]), ]; coef(lm(reformulate(c(CORE6, add), "mig_gross"), dd))[".px"] })
cat(sprintf("   leave-one-out, product coefficient: %.4f to %.4f (all-year %.4f)\n", min(loo), max(loo), coef(lm(reformulate(c(CORE6, add), "mig_gross"), base[complete.cases(base[, c("mig_gross", CORE6, add)]), ]))[".px"]))

cat("\nD. int_tme: banks vs the clean part vs the APF (t-1, in place of int_tme)\n")
P <- .pusf; P$bank_tme <- 100 * (P$ANLO - P$JW2P) / P$EBFT; P$clean_tme <- 100 * P$JW2P / P$EBFT; P$cg_gross_tme <- 100 * P$NMFX / P$EBFT
mu <- A$MU74[match(P$year, A$year)]; mu[is.na(mu)] <- 0; P$cg_net_apf_tme <- 100 * (P$NMFX + mu) / P$EBFT
for (v in c("bank_tme", "clean_tme", "cg_gross_tme", "cg_net_apf_tme")) S[[paste0("L_", v)]] <- P[[v]][match(S$year - 1, P$year)]
cat(sprintf("   bank part of interest, %% of TME: 2006 %.2f, 2008 %.2f, 2009 %.2f, 2011 %.2f, 2014 %.2f, 2017 %.2f; APF transfers (MU74) reduce CG interest from 2009\n",
            P$bank_tme[P$year == 2006], P$bank_tme[P$year == 2008], P$bank_tme[P$year == 2009], P$bank_tme[P$year == 2011], P$bank_tme[P$year == 2014], P$bank_tme[P$year == 2017]))
for (y0 in c(1965, 1977)) { dd <- subset(S, year >= y0); cat(sprintf("  -- from %d\n", y0))
  for (v in c("L_int_tme", "L_clean_tme", "L_bank_tme", "L_cg_gross_tme", "L_cg_net_apf_tme")) pr(sub("^L_", "", v), ev(dd, v))
  pr("clean + bank parts together", ev(dd, c("L_clean_tme", "L_bank_tme")))
  pr("int_tme, years to 2007 only", ev(subset(dd, year <= 2007), "L_int_tme")) }
cat("\nDone.\n")

cat("\n  -- checks on the both-sample version: planned one-year change (backfilled to 1971) * outturn r - g --\n")
S$.b <- S$bf1 * S$L_rg_out; S$.miss <- as.numeric(is.na(S$.b)); S$.b0 <- ifelse(is.na(S$.b), 0, S$.b); S$.p0 <- ifelse(is.na(S$bf1), 0, S$bf1); S$.r0 <- S$L_rg_out
for (y0 in c(1965, 1977)) { dd <- subset(S, year >= y0); ind <- if (length(unique(dd$.miss)) > 1) ".miss" else NULL; add <- c(".p0", ".r0", ".b0", ind)
  pr(sprintf("from %d, all years", y0), ev(dd, add, ".b0")); pr(sprintf("from %d, without 2008-11", y0), ev(subset(dd, !year %in% 2008:2011), add, ".b0"))
  pr(sprintf("from %d, to 2007", y0), ev(subset(dd, year <= 2007), add, ".b0"))
  m <- lm(reformulate(c(CORE6, add), "mig_gross"), dd); ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("      main terms: planned change %.4f (p %s), r - g %.4f (p %s), product %.4f (p %s); effect of a 1-point planned rise in borrowing at r - g = -3 / 0 / +3: %+.3f / %+.3f / %+.3f\n",
              ct[".p0", 1], pf2(ct[".p0", 4]), ct[".r0", 1], pf2(ct[".r0", 4]), ct[".b0", 1], pf2(ct[".b0", 4]), ct[".p0", 1] - 3 * ct[".b0", 1], ct[".p0", 1], ct[".p0", 1] + 3 * ct[".b0", 1]))
  loo <- sapply(dd$year, function(y) coef(lm(reformulate(c(CORE6, add), "mig_gross"), subset(dd, year != y)))[".b0"])
  cat(sprintf("      leave-one-out product coefficient %.4f to %.4f\n", min(loo), max(loo))) }
cat("Done.\n")
