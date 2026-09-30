# =============================================================================
# british_mpr.R -- can the UK-relative determinants of Mitchell, Pain and Riley (2011, EJ, Table 6) explain British inflows? (Sam, 30 Sep 2026)
# The British flows are the partial placebo failure: British citizens' inflows respond to the projection. Earlier rounds showed the response is
# in NON-UK-born British arriving from non-EU countries (citizens by descent or registration), not in UK-born returners.
# MPR's robust determinants (share of source regions, gross immigration) mapped to what the data hold, all at t-1, with the sign expected for
# British arrivals stated before estimation:
#   relinc_rich  UK GDP per head relative to the rich alternative hosts (MPR's ln(YR), UK relative to EU4 and US)         prior +
#   relinc_dev   UK GDP per head relative to the developing non-EU origins (MPR's ln(Y_i), UK relative to region i)      prior +
#   youth        share aged 15-29 in the non-EU origins (MPR's D_i)                                                     prior +
#   unem_diff    UK minus destination unemployment (Australia, Canada, New Zealand, US) -- MPR use UK unemployment alone,
#                which the core already carries through log V/U; the difference is the relative version                prior -
#   pool10/pool5 the return pool: log of British citizens' emigration over the previous 10 / 5 years (LTIM 2.00) -- the
#                analogue, for returners, of MPR's migrant stock ln(S_i / P_i); the 10-year sum exists from 1974         prior +
#   Not available or not usable: the UK-to-origin Gini ratio (no origin series over the period); UK immigration relative to other
#   hosts (a function of the outcome itself); policy dummies (regime indicators -- excluded by Sam's rule).
# Outcomes (log persons): British citizens, all origins (LTIM 2.00; 1965-2019 and 1977-2019); British from non-EU residence (IPS 3.01);
# non-UK-born British from non-EU; UK-born British from non-EU (IPS 3.18; 1977-2019). Right-hand side: the outcome's own lag + the current
# core (projection, tax per WAP, log change in debt/GDP, log V/U). For each: the projection with and without the determinant, the
# determinant's coefficient (HAC p), dAIC; then all five together. The question: does any of them remove the projection's coefficient?
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); A <- { SAMPLE <- "1964"; source("data_common.R"); df[df$year <= 2019, ] }; A <- A[order(A$year), ]
roll <- function(x, k) vapply(seq_along(x), function(i) if (i < k) NA else sum(x[(i - k + 1):i]), 0)
A$pool10 <- log(roll(A$brit_out, 10)); A$pool5 <- log(roll(A$brit_out, 5))
A$brit_nf_gross <- log(1000 * (A$brit_desc_noneu_k + A$ukborn_noneu_k)); A$ukborn_gross <- log(1000 * A$ukborn_noneu_k)
for (v in c("pool10", "pool5", "brit_nf_gross", "ukborn_gross")) A[[paste0("L_", v)]] <- lag1(A[[v]])
S <- subset(A, year >= 1965)
X <- setdiff(CORE_A, c("L_mig_gross", "L_int_tme"))                                   # projection, tax per WAP, log change in debt/GDP, log V/U
DET <- c(relinc_rich = "+", relinc_dev = "+", youth = "+", unem_diff = "-", pool10 = "+", pool5 = "+")
OUT <- list(c("brit_gross", "British, all origins (LTIM)"), c("brit_nf_gross", "British from non-EU (IPS)"), c("brit_desc_gross", "non-UK-born British from non-EU"),
            c("ukborn_gross", "UK-born British from non-EU"))
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
for (per in list(c(1965, 2019), c(1977, 2019))) for (o in OUT) { d <- subset(S, year >= per[1] & year <= per[2]); xs <- c(paste0("L_", o[1]), X)
  if (sum(complete.cases(d[, c(o[1], xs)])) < 30) next
  cat(sprintf("\n-- %s, %d-%d\n", o[2], per[1], per[2]))
  cat(sprintf("   %-26s %3s %18s %18s %7s\n", "determinant (prior)", "N", "determinant (p)", "projection (p)", "dAIC"))
  for (v in c("(none)", names(DET), "all MPR-type (pool10)")) { add <- switch(v, "(none)" = character(0), "all MPR-type (pool10)" = paste0("L_", c("relinc_rich", "relinc_dev", "youth", "unem_diff", "pool10")), paste0("L_", v))
    dd <- d[complete.cases(d[, c(o[1], xs, add)]), ]; if (nrow(dd) < 25) next; m0 <- fit(dd, xs, o[1]); m <- fit(dd, c(xs, add), o[1]); if (is.null(m)) next
    own <- if (length(add) == 1) hc(m, add) else { w <- tryCatch(linearHypothesis(m, paste(add, "= 0"), vcov. = vcovHAC(m)), error = function(e) NULL); c(NA, if (is.null(w)) NA else w$`Pr(>F)`[2]) }
    cat(sprintf("   %-26s %3d %9s (%5s) %9.4f (%5s) %7.2f\n", if (v %in% names(DET)) sprintf("%s (%s)", v, DET[[v]]) else v, nobs(m), if (is.na(own[1])) (if (length(add) > 1) "joint" else "--") else sprintf("%.4f", own[1]),
                pf2(own[2]), hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]), AIC(m) - AIC(m0))) } }
cat("\nDone.\n")
