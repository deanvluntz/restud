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

# ============================================================================= PART 2 (Sam, 30 Sep 2026)
# (a) unemployment in LEVELS rather than as the UK-minus-destination difference: UK ILO unemployment (MPR's U) and the mean of the Australian,
#     Canadian, New Zealand and US rates (OECD, Sam's upload of 29 Sep; 1967 on), each alone and both together (the unrestricted difference).
# (b) the Gini ratio, from Sam's World Bank upload (WDI SI.POV.GINI; determinants/wb/): UK Gini relative to the mean of the US and Canada (the rich
#     destinations with near-annual series; Australia has 13 observations, New Zealand none), gaps filled by linear interpolation inside each
#     country's observed range, never extrapolated; and the UK Gini level. MPR count a Gini ratio as robust 'regardless of sign': no prior sign.
#     The UK-to-origin ratio for the seven developing origins cannot be built: India 8 observations, Bangladesh 10, Nigeria 9, South Africa 7
#     (from 1993), Pakistan 14, Philippines 14, China 23 (from 1981).
# (c) World Bank unemployment (WDI SL.UEM.TOTL.ZS, modelled ILO) starts in 1991 for every country, so it is used only to check the OECD
#     destination mean over 1991-2019, not as a regressor (a 1992-2019 sample would leave 28 observations).
# The main non-EU equation is shown first in each block, for the asymmetry.
# =============================================================================
wb <- function(f) { a <- suppressMessages(as.data.frame(readxl::read_excel(f, sheet = 1, col_names = FALSE))); h <- which(a[[1]] == "Country Name"); names(a) <- as.character(unlist(a[h, ]))
  a <- a[-(1:h), ]; yrs <- grep("^[0-9]{4}$", names(a), value = TRUE); do.call(rbind, lapply(seq_len(nrow(a)), function(i) data.frame(iso = a[["Country Code"]][i], year = as.integer(yrs),
    value = suppressWarnings(as.numeric(unlist(a[i, yrs])))))) }
GI <- wb("determinants/wb/API_SI.POV.GINI.xls"); UN <- wb("determinants/wb/API_SL.UEM.TOTL.ZS.xls")
ip <- function(iso, D) { d <- D[D$iso == iso & !is.na(D$value), ]; if (nrow(d) < 2) return(rep(NA, nrow(A))); v <- approx(d$year, d$value, xout = A$year, rule = 1)$y; v }
A$gini_uk <- ip("GBR", GI); A$gini_ratio <- A$gini_uk / rowMeans(cbind(ip("USA", GI), ip("CAN", GI)))
for (v in c("gini_uk", "gini_ratio")) A[[paste0("L_", v)]] <- lag1(A[[v]])
for (v in c("gini_uk", "gini_ratio", "L_gini_uk", "L_gini_ratio")) S[[v]] <- A[[v]][match(S$year, A$year)]
wbd <- rowMeans(sapply(c("AUS", "CAN", "NZL", "USA"), function(i) UN$value[UN$iso == i][match(A$year, UN$year[UN$iso == i])])); k <- A$year %in% 1991:2019
cat(sprintf("\n\nPART 2\n(c) OECD destination unemployment vs World Bank modelled ILO, same four countries, 1991-2019: correlation %.3f, mean gap %+.2f points\n",
            cor(A$unem_dest[k], wbd[k], use = "pair"), mean(A$unem_dest[k] - wbd[k], na.rm = TRUE)))
cat(sprintf("    Gini (WDI), interpolated: UK 1970 %.1f, 1990 %.1f, 2019 %.1f; UK / (US, Canada) 1972 %.2f, 1990 %.2f, 2019 %.2f\n", A$gini_uk[A$year == 1970], A$gini_uk[A$year == 1990], A$gini_uk[A$year == 2019],
            A$gini_ratio[A$year == 1972], A$gini_ratio[A$year == 1990], A$gini_ratio[A$year == 2019]))
ADD <- list("UK unemployment (level)" = "L_unem", "destination unemployment (level)" = "L_unem_dest", "UK + destination (levels)" = c("L_unem", "L_unem_dest"),
            "UK Gini (level)" = "L_gini_uk", "Gini ratio UK / (US, Canada)" = "L_gini_ratio")
OUT2 <- c(list(c("mig_gross", "MAIN: non-EU inflows (controlled)")), OUT)
for (per in list(c(1965, 2019), c(1977, 2019))) for (o in OUT2) { d <- subset(S, year >= per[1] & year <= per[2]); xs <- c(paste0("L_", o[1]), X)
  if (sum(complete.cases(d[, c(o[1], xs)])) < 30) next
  cat(sprintf("\n-- %s, %d-%d\n", o[2], per[1], per[2])); cat(sprintf("   %-34s %3s %26s %18s %7s\n", "added (t-1)", "N", "coefficient(s) (p)", "projection (p)", "dAIC"))
  for (nm in c("(none)", names(ADD))) { add <- if (nm == "(none)") character(0) else ADD[[nm]]; dd <- d[complete.cases(d[, c(o[1], xs, add)]), ]; if (nrow(dd) < 25) next
    m0 <- fit(dd, xs, o[1]); m <- fit(dd, c(xs, add), o[1]); if (is.null(m)) next
    cf <- if (length(add)) paste(vapply(add, function(a) { h <- hc(m, a); sprintf("%.4f (%s)", h[1], pf2(h[2])) }, ""), collapse = ", ") else "--"
    cat(sprintf("   %-34s %3d %26s %9.4f (%5s) %7.2f\n", nm, nobs(m), cf, hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]), AIC(m) - AIC(m0))) } }
cat("\nDone.\n")
