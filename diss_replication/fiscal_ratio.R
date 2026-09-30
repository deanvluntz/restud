# =============================================================================
# fiscal_ratio.R -- a clean borrowing/expenditure ratio in place of int_tme, and the clean one-term burden (Sam, 30 Sep 2026)
# Sam: the structure he liked -- LEVELS of the fiscal position (the tax burden per person = revenue extracted from capacity; a ratio of
# borrowing to expenditure) beside CHANGES in broader conditions (log change in debt/GDP; log V/U). int_tme left the core because its
# significance came from the 2008-13 reclassification of the banks (interest_apf.R). Is there a clean ratio that can take its place? If not,
# his preferred core is the one-term burden -- real taxes + debt interest per working-age person -- built on a clean interest series.
# Every series EXCLUDES the public-sector banks: ONS PUSF (calendar years; J5II and JW2T are stored as net lending / current surplus, so
# borrowing = -J5II and the current budget deficit = -JW2T) and the OBR PSF aggregates databank (fiscal years, % of GDP).
# A. Ratios in place of int_tme, dated as int_tme was (calendar year t, or the fiscal year starting in April of t) and entering at t-1.
#    Prior sign + for all (a larger share of the spending envelope pre-empted by, or financed from, borrowing = the borrowing margin in use):
#    int_xb_tme   public-sector debt interest / TME (int_tme's own concept, like for like, banks excluded; ONS JW2P / KX5Q)
#    int_cg_tme   central-government debt interest net of the APF / TME (OBR definition; (NMFX + MU74) / KX5Q)
#    rint_xb_tme  REAL public-sector debt interest / TME: interest less inflation x net debt at the start of the year (inflation
#                 compensation -- including, in spirit, the RPI uplift on index-linked gilts -- is not a real cost)
#    psnb_tme     net borrowing / TME: the share of spending financed by borrowing (the budget identity tau + b = g, read as b / g)
#    cbd_tme      current budget deficit / TME: borrowing to pay for current spending (the golden-rule measure, the 1997-2009 target)
#    pd_tme       primary deficit / TME: borrowing net of net interest paid (the discretionary part of the stance)
#    ncr_tme      public-sector net cash requirement / TME (the PSBR, the Treasury's target measure from the 1970s to the 1990s; NOTE: a cash
#                 measure, so it also carries financial transactions, including the 2008-09 bank recapitalisations and loans)
#    debt_tme     net debt / TME (the inherited stock against the annual envelope)
#    capsnb_tme   cyclically adjusted net borrowing / TME (OBR, from 1975-76: 1977-2019 only)
#    Each is added to the 6-parameter core: lagged outcome, projection, tax per WAP, log change in debt/GDP, log V/U.
# B. The one-term burden per working-age person, dated as tax per WAP (year t = fiscal year ending in March of t); calendar-year ONS shares
#    are converted to that fiscal year with weights 0.75 (t-1) and 0.25 (t):
#    taxint_xb / taxrint_xb   taxes + public-sector debt interest excluding the banks (nominal / real)
#    taxint_cg / taxrint_cg   taxes + central-government debt interest net of the APF (OBR, fiscal years; nominal / real)
#    taxrint_mil              the Millennium (bank-inclusive) version of 30 Sep (40), for reference only
#    Each in place of tax per WAP (6 parameters).
# C. The clean real one-term burden + each borrowing ratio (the interest ratios are left out: interest is already in the burden).
# Rule (Sam's criterion, BOTH periods): own HAC p < .10 with the prior sign and a lower AIC, (i) beside the projection and (ii) without it.
# D. The agreed protocol for the clean one-term core and for any ratio that passes; for a passing ratio, the fiscal-block dating as a check.
# Nine ratios are tried in A: a pass there is a screening result among nine and is reported as such.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); suppressWarnings(library(readxl))
S <- build_data()[["1964"]]$S                                                                   # estimation frame, 1965-2019
A <- { SAMPLE <- "1964"; source("data_common.R"); df[df$year <= 2020, ] }; A <- A[order(A$year), ]   # every year, so new lags start on time
pu <- suppressMessages(as.data.frame(read_excel("determinants/ons_pusf_jul2026.xlsx", col_names = FALSE, col_types = "text"))); id <- unlist(pu[2, ]); yv <- pu[[1]]
ann <- grepl("^[0-9]{4}$", yv); g <- function(v) as.numeric(pu[ann, which(id == v)[1]])
PU <- data.frame(year = as.integer(yv[ann]), J5II = g("J5II"), JW2T = g("JW2T"), JW2P = g("JW2P"), JW2L = g("JW2L"), JW2M = g("JW2M"), KX5Q = g("KX5Q"),
                 NMFX = g("NMFX"), MU74 = g("MU74"), JW38 = g("JW38")); PU$MU74[is.na(PU$MU74)] <- 0
ob <- suppressMessages(as.data.frame(read_excel("determinants/obr_psf_aggregates_databank_mar2026.xlsx", sheet = 4, col_names = FALSE, col_types = "text")))
nb <- function(j) suppressWarnings(as.numeric(ifelse(ob[[j]] == "-", NA, ob[[j]])))
OB <- data.frame(fy = suppressWarnings(as.integer(substr(ob[[1]], 1, 4))), tme = nb(3), capsnb = nb(13), int_cg = nb(21)); OB <- OB[!is.na(OB$fy) & !duplicated(OB$fy), ]
A <- merge(A, PU, by = "year", all.x = TRUE); A <- A[order(A$year), ]
fs <- function(v) OB[[v]][match(A$year, OB$fy)]                    # fiscal year STARTING in April of the year (dated like int_tme)
fe <- function(v) OB[[v]][match(A$year - 1, OB$fy)]                # fiscal year ENDING in March of the year (dated like tax per WAP)
nx <- function(x) c(x[-1], NA); pv <- function(x) c(NA, head(x, -1))
# ---- A. ratios, dated as int_tme ----
A$tme_fs <- fs("tme")
A$int_xb_tme  <- 100 * A$JW2P / A$KX5Q
A$int_cg_tme  <- 100 * (A$NMFX + A$MU74) / A$KX5Q
A$rint_xb_tme <- A$int_xb_tme - 100 * (nx(A$inflation) / 100 * A$psnd_gdp) / A$tme_fs    # inflation over the fiscal year starting in t x net debt at its start
A$psnb_tme    <- 100 * -A$J5II / A$KX5Q
A$cbd_tme     <- 100 * -A$JW2T / A$KX5Q
A$pd_tme      <- 100 * (-A$J5II - A$JW2P + A$JW2L + A$JW2M) / A$KX5Q
A$ncr_tme     <- 100 * A$JW38 / A$KX5Q
A$debt_tme    <- 100 * nx(A$psnd_gdp) / A$tme_fs                                              # net debt at the end of the fiscal year starting in t / its TME
A$capsnb_tme  <- 100 * fs("capsnb") / A$tme_fs
# ---- B. one-term burdens, dated as tax per WAP ----
w <- function(x) 0.75 * pv(x) + 0.25 * x                                                       # calendar-year ratio -> fiscal year ending in March of t
A$tme_fe <- fe("tme"); adj <- A$inflation / 100 * pv(A$psnd_gdp)                                 # inflation over that fiscal year x net debt at its start, % of GDP
A$int_xb_gdp <- w(A$JW2P / A$KX5Q) * A$tme_fe; A$int_cg_gdp <- fe("int_cg")
bw <- function(x_gdp) A$tax_workpop * (1 + x_gdp / A$tax_gdp)                                    # a % of GDP item in real GBP k per WAP, added to taxes
A$taxint_xb <- bw(A$int_xb_gdp); A$taxrint_xb <- bw(A$int_xb_gdp - adj); A$taxint_cg <- bw(A$int_cg_gdp); A$taxrint_cg <- bw(A$int_cg_gdp - adj)
A$taxrint_mil <- bw(.splice(A$int_gdp, A$year, .pusf$int_gdp_ons) - adj)
RAT <- c("int_xb_tme", "int_cg_tme", "rint_xb_tme", "psnb_tme", "cbd_tme", "pd_tme", "ncr_tme", "debt_tme", "capsnb_tme")
BUR <- c("taxint_xb", "taxrint_xb", "taxint_cg", "taxrint_cg", "taxrint_mil")
for (v in c(RAT, BUR)) A[[paste0("L_", v)]] <- lag1(A[[v]])
for (v in RAT) A[[paste0("Lfe_", v)]] <- lag1(w(A[[v]]))                                         # fiscal-block dating check (fiscal year ending in t-1)
NEW <- grep("^(L_|Lfe_)?(int_xb_tme|int_cg_tme|rint_xb_tme|psnb_tme|cbd_tme|pd_tme|ncr_tme|debt_tme|capsnb_tme|taxint_xb|taxrint_xb|taxint_cg|taxrint_cg|taxrint_mil)$", names(A), value = TRUE)
for (v in NEW) S[[v]] <- A[[v]][match(S$year, A$year)]

cat("Ratios (dated as int_tme; % of TME) and burdens (real GBP k per WAP), selected years\n")
print(round(A[A$year %in% c(1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2008, 2009, 2010, 2013, 2016, 2019), c("year", "int_tme", RAT)], 1), row.names = FALSE)
print(round(A[A$year %in% c(1965, 1975, 1985, 1995, 2005, 2009, 2013, 2019), c("year", "tax_workpop", BUR)], 2), row.names = FALSE)
cat(sprintf("Correlation with int_tme (Millennium), 1965-2019: %s\n", paste(sprintf("%s %.2f", RAT, vapply(RAT, function(v) cor(A$int_tme[A$year %in% 1965:2019], A[[v]][A$year %in% 1965:2019], use = "pair"), 0)), collapse = "; ")))
cat(sprintf("Correlation with L_ln_debt_gdp (t-1 ratios), 1965-2019: %s\n", paste(sprintf("%s %.2f", RAT, vapply(RAT, function(v) cor(S$L_ln_debt_gdp, S[[paste0("L_", v)]], use = "pair"), 0)), collapse = "; ")))

CORE6 <- setdiff(CORE_A, "L_int_tme")
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
supf <- function(m) suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value)
ev <- function(d, xs, v, ref = CORE6) { dd <- d[complete.cases(d[, c("mig_gross", unique(c(xs, ref)))]), ]
  m <- fit(dd, xs, "mig_gross"); m0 <- fit(dd, ref, "mig_gross"); n1 <- fit(dd, setdiff(xs, "delta_oadr_10"), "mig_gross"); n0 <- fit(dd, setdiff(ref, "delta_oadr_10"), "mig_gross")
  list(n = nobs(m), own = hc(m, v), tax = hc(m, "L_tax_workpop"), proj = hc(m, "delta_oadr_10"), dA = AIC(m) - AIC(m0), np = hc(n1, v), ndA = AIC(n1) - AIC(n0),
       bg2 = bgtest(m, 2)$p.value, reset = resettest(m)$p.value, supf = supf(m), vif = max(vif(m)), oos = oos(m, "mig_gross"), oos0 = oos(m0, "mig_gross")) }
ok <- function(r) !any(is.na(c(r$own, r$np))) && r$own[1] > 0 && r$np[1] > 0 && r$own[2] < .10 && r$np[2] < .10 && r$dA < 0 && r$ndA < 0
HD <- sprintf("   %-16s %3s %17s %17s %17s %6s | %17s %6s | %5s %5s %6s %5s %11s %4s\n", "", "N", "own coef (p)", "tax/WAP (p)", "projection (p)", "dAIC", "no-proj own (p)", "dAIC",
              "BG2", "RESET", "sup-F", "VIF", "OOS (core)", "rule")
pr <- function(lab, r) cat(sprintf("   %-16s %3d %9.4f (%5s) %9.4f (%5s) %9.4f (%5s) %6.2f | %9.4f (%5s) %6.2f | %5s %5s %6s %5.1f %.3f (%.3f) %4s\n", lab, r$n, r$own[1], pf2(r$own[2]),
  r$tax[1], pf2(r$tax[2]), r$proj[1], pf2(r$proj[2]), r$dA, r$np[1], pf2(r$np[2]), r$ndA, pf2(r$bg2), pf2(r$reset), pf2(r$supf), r$vif, r$oos, r$oos0, if (ok(r)) "PASS" else ""))
PER <- list("Citizen sample (1965-2019)" = function(d) d, "1977-2019" = function(d) subset(d, year >= 1977)); RES <- list()
CB <- sub("^L_tax_workpop$", "L_taxrint_xb", CORE6)
for (nm in names(PER)) { d <- PER[[nm]](S); cat(sprintf("\n==================== %s ====================\n", nm))
  m0 <- fit(d, CORE6, "mig_gross"); cat(sprintf("   6-parameter core: N %d, AIC %.2f, projection %.4f (p %s); dAIC below is against it on the same years ('no-proj' against the core without the projection)\n",
    nobs(m0), AIC(m0), hc(m0, "delta_oadr_10")[1], pf2(hc(m0, "delta_oadr_10")[2])))
  cat("\n   A. ratio added to the core, in the place of int_tme (prior +)\n"); cat(HD)
  for (v in RAT) { r <- tryCatch(ev(d, c(CORE6, paste0("L_", v)), paste0("L_", v)), error = function(e) NULL); if (is.null(r)) next; pr(v, r); RES[[paste(nm, "A", v)]] <- r }
  cat("\n   B. one-term burden in place of tax per WAP (6 parameters; 'own' = the burden)\n"); cat(HD)
  for (v in BUR) { r <- ev(d, sub("^L_tax_workpop$", paste0("L_", v), CORE6), paste0("L_", v)); pr(paste0(v, if (v == "taxrint_mil") " (ref.)" else ""), r); RES[[paste(nm, "B", v)]] <- r }
  cat("\n   C. clean real one-term burden (taxrint_xb) + a borrowing ratio; dAIC against the taxrint_xb core\n"); cat(HD)
  for (v in c("psnb_tme", "cbd_tme", "pd_tme", "ncr_tme", "debt_tme", "capsnb_tme")) { r <- tryCatch(ev(d, c(CB, paste0("L_", v)), paste0("L_", v), ref = CB), error = function(e) NULL)
    if (is.null(r)) next; pr(v, r); RES[[paste(nm, "C", v)]] <- r } }

cat("\n==================== Which pass Sam's criterion on BOTH periods? ====================\n")
for (p in c("A", "B", "C")) { k <- unique(sub(".* [ABC] ", "", grep(paste0(" ", p, " "), names(RES), value = TRUE)))
  both <- k[vapply(k, function(v) { a <- RES[[paste("Citizen sample (1965-2019)", p, v)]]; b <- RES[[paste("1977-2019", p, v)]]; !is.null(a) && !is.null(b) && ok(a) && ok(b) }, logical(1))]
  cat(sprintf("   %s: %s\n", c(A = "A (ratio for int_tme)", B = "B (one-term burden)", C = "C (burden + ratio)")[[p]], if (length(both)) paste(both, collapse = ", ") else "none")) }

cat("\n==================== D. Protocol ====================\n")
invisible(run_protocol("Clean one-term core: real taxes + public-sector debt interest (banks excluded) per WAP", CB, S))
for (v in RAT) { a <- RES[[paste("Citizen sample (1965-2019)", "A", v)]]; b <- RES[[paste("1977-2019", "A", v)]]; if (is.null(a) || is.null(b) || !(ok(a) && ok(b))) next
  invisible(run_protocol(paste0("6-parameter core + ", v), c(CORE6, paste0("L_", v)), S))
  for (nm in names(PER)) { r <- ev(PER[[nm]](S), c(CORE6, paste0("Lfe_", v)), paste0("Lfe_", v)); cat(sprintf("   fiscal-block dating (fiscal year ending in t-1), %s:\n", nm)); cat(HD); pr(paste0(v, " (fe)"), r) } }
cat("\nDone.\n")
