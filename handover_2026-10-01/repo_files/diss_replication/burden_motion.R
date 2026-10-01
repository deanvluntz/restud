# =============================================================================
# burden_motion.R -- (1) the tax burden with debt interest and/or an inflation adjustment; (2) how the headline tax rates move together.
# (Sam, 30 Sep 2026, after tax_rates.R: headline rates fell while real tax per WAP doubled.)
# (1) BURDEN VARIANTS, real GBP k per working-age person at t-1 (fiscal block convention: year t = FY (t-1)/t):
#   tax        national accounts taxes per WAP (the core term)
#   taxint     taxes + debt interest (Millennium share; the one-term burden of 30 Sep (25)) -- replaces tax AND int_tme
#   (the variants rescale the model's interest / GDP by the ONS ratio of each definition to public-sector interest including the banks, ANLO)
#   taxint_cg  taxes + central-government debt interest NET OF THE APF (OBR definition; ONS PUSF NMFX + MU74) -- replaces tax and int_tme
#   taxint_xb  taxes + public-sector debt interest EXCLUDING the public-sector banks (ONS PUSF JW2P; see interest_apf.R) -- replaces both
#   taxrint    taxes + REAL debt interest (interest less inflation x debt: the part of nominal interest that only compensates for inflation
#              is taken out -- this also removes the RPI uplift on index-linked gilts in spirit) -- Millennium interest; replaces both
#   taxrint_cg the same with central-government interest net of the APF -- replaces both
#   taxdebt    taxes + net debt per WAP (the stock, i.e. future taxes) -- replaces tax only
# For each: its coefficient, the projection, AIC against the core on the same years, BG(2), sup-F, and the same model without the projection.
# Then the rate question under each burden: the payslip rate (basic + employee NIC) added, and its correlation with the burden (1977-2019).
# Rate timing: tax_rates.R dated a rate by the fiscal year that STARTS in the year (the rate is announced at that spring's Budget), while the
# fiscal block dates receipts by the fiscal year that ENDS in the year. The key rate constructs are re-run on the fiscal-block dating.
# (2) THE MOTION OF THE HEADLINE RATES (IFS tables, 1975-2023, fiscal year starting in April of the year; NIC % rate exists from 1975-76):
#   basic income-tax rate, employee NIC main rate, standard VAT rate, main corporation-tax rate (headline four); top income-tax rate and
#   employer NIC main rate as an extended set. A change = the rate differs from the previous fiscal year (within-year changes, e.g. the
#   temporary VAT cut of December 2008, are dated by the fiscal year in which the rate applied for most of the year).
#   (a) frequency; (b) same-year coincidence; (c) after a change in one tax, how often another changes 0-5 years later, against a null in
#   which each tax's own change history is kept but shifted in time at random (circular shifts, 10,000 draws) -- a p-value below .05 in the
#   'fewer' column means changes in one tax are followed by FEWER changes in the others than chance; (d) the same for rises only and cuts only.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); suppressWarnings(library(readxl)); set.seed(20260930)
RT <- read.csv("determinants/ifs_tax_rates_1973_2023.csv")
a <- suppressMessages(as.data.frame(read_excel("determinants/ons_pusf_jul2026.xlsx", col_names = FALSE, col_types = "text"))); id <- unlist(a[2, ]); yv <- a[[1]]; ann <- grepl("^[0-9]{4}$", yv)
g <- function(v) as.numeric(a[ann, which(id == v)])
PU <- data.frame(year = as.integer(yv[ann]), NMFX = g("NMFX"), MU74 = g("MU74"), JW2P = g("JW2P"), ANLO = g("ANLO"), KX5Q = g("KX5Q")); PU$MU74[is.na(PU$MU74)] <- 0

# ---- (1) burden variants ----------------------------------------------------------------------------------------------------------------------
S <- merge(build_data()[["1964"]]$S, PU, by = "year", all.x = TRUE); S <- S[order(S$year), ]
S <- merge(S, RT[, c("year", "basic", "ni_ee", "vat")], by = "year", all.x = TRUE); S <- S[order(S$year), ]
pw <- function(x_gdp) S$tax_workpop * x_gdp / S$tax_gdp                         # a % of GDP item in real GBP k per WAP, on the tax per WAP scale
# the model's interest / GDP (Millennium, spliced to 2019) rescaled by the ONS ratio of each definition to public-sector interest incl. banks (ANLO),
# so every variant shares the Millennium series' GDP basis and dating and differs only in what counts as debt interest
int_mil <- S$int_gdp_x; int_cg <- int_mil * (S$NMFX + S$MU74) / S$ANLO; int_xb <- int_mil * S$JW2P / S$ANLO
infl_adj <- S$inflation / 100 * lag1(S$psnd_gdp)                                  # inflation x net debt at the start of the year, % of GDP
S$taxint <- S$tax_workpop + pw(int_mil); S$taxint_cg <- S$tax_workpop + pw(int_cg); S$taxint_xb <- S$tax_workpop + pw(int_xb)
S$taxrint <- S$tax_workpop + pw(int_mil - infl_adj); S$taxrint_cg <- S$tax_workpop + pw(int_cg - infl_adj); S$taxdebt <- S$tax_workpop + pw(S$psnd_gdp)
S$payslip <- S$basic + S$ni_ee
for (v in c("taxint", "taxint_cg", "taxint_xb", "taxrint", "taxrint_cg", "taxdebt", "payslip", "vat")) S[[paste0("L_", v)]] <- lag1(S[[v]])
S$L2_payslip <- lag1(S$L_payslip); S$L_d_payslip <- lag1(c(NA, diff(S$payslip))); S$L2_d_payslip <- lag1(S$L_d_payslip); S$L2_vat <- lag1(S$L_vat)
S$L_d_vat <- lag1(c(NA, diff(S$vat))); S$L2_d_vat <- lag1(S$L_d_vat)
cat("(1) Burden variants, real GBP k per working-age person, selected years (level at t, fiscal year ending in March of t)\n")
print(round(S[S$year %in% c(1965, 1975, 1980, 1985, 1990, 2000, 2008, 2010, 2013, 2019), c("year", "tax_workpop", "taxint", "taxint_cg", "taxint_xb", "taxrint", "taxrint_cg", "taxdebt", "payslip")], 2), row.names = FALSE)
S7 <- subset(S, year >= 1977)
cat("\n   Correlation with the payslip rate (basic + employee NIC), 1977-2019, burdens at t-1:\n   ")
cat(paste(sprintf("%s %.2f", c("tax", "taxint", "taxint_cg", "taxint_xb", "taxrint", "taxrint_cg", "taxdebt"),
  vapply(c("L_tax_workpop", "L_taxint", "L_taxint_cg", "L_taxint_xb", "L_taxrint", "L_taxrint_cg", "L_taxdebt"), function(v) cor(S7[[v]], S7$L_payslip, use = "pair"), 0)), collapse = "; "), "\n")
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
supf <- function(m) suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value)
VAR <- list(tax = CORE_A,
  taxint     = c(setdiff(CORE_A, c("L_tax_workpop", "L_int_tme")), "L_taxint"),
  taxint_cg  = c(setdiff(CORE_A, c("L_tax_workpop", "L_int_tme")), "L_taxint_cg"),
  taxint_xb  = c(setdiff(CORE_A, c("L_tax_workpop", "L_int_tme")), "L_taxint_xb"),
  taxrint    = c(setdiff(CORE_A, c("L_tax_workpop", "L_int_tme")), "L_taxrint"),
  taxrint_cg = c(setdiff(CORE_A, c("L_tax_workpop", "L_int_tme")), "L_taxrint_cg"),
  taxdebt    = c(setdiff(CORE_A, "L_tax_workpop"), "L_taxdebt"))
BV <- c(tax = "L_tax_workpop", taxint = "L_taxint", taxint_cg = "L_taxint_cg", taxint_xb = "L_taxint_xb", taxrint = "L_taxrint", taxrint_cg = "L_taxrint_cg", taxdebt = "L_taxdebt")
for (nm in c("Citizen sample (1965-2019)", "1977-2019")) { d <- if (nm == "1977-2019") S7 else S; cat(sprintf("\n==================== %s ====================\n", nm))
  cat(sprintf("   %-11s %3s %17s %17s %6s %5s %6s | %17s %6s %s\n", "burden", "N", "burden (p)", "projection (p)", "dAIC", "BG2", "sup-F", "no-proj burden (p)", "dAIC", if (nm == "1977-2019") "| + payslip rate (p)" else ""))
  for (k in names(VAR)) { xs <- VAR[[k]]; dd <- d[complete.cases(d[, c("mig_gross", unique(c(xs, CORE_A)))]), ]; m <- fit(dd, xs, "mig_gross"); m0 <- fit(dd, CORE_A, "mig_gross")
    n1 <- fit(dd, setdiff(xs, "delta_oadr_10"), "mig_gross"); n0 <- fit(dd, setdiff(CORE_A, "delta_oadr_10"), "mig_gross")
    ps <- if (nm == "1977-2019") hc(fit(dd, c(xs, "L_payslip"), "mig_gross"), "L_payslip") else c(NA, NA)
    cat(sprintf("   %-11s %3d %9.4f (%5s) %9.4f (%5s) %6.2f %5s %6s | %9.4f (%5s) %6.2f %s\n", k, nobs(m), hc(m, BV[[k]])[1], pf2(hc(m, BV[[k]])[2]), hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]),
                AIC(m) - AIC(m0), pf2(bgtest(m, 2)$p.value), pf2(supf(m)), hc(n1, BV[[k]])[1], pf2(hc(n1, BV[[k]])[2]), AIC(n1) - AIC(n0),
                if (nm == "1977-2019") sprintf("| %8.4f (%5s)", ps[1], pf2(ps[2])) else "")) } }
cat("\n   Rate timing on the fiscal-block dating (rate of the fiscal year ENDING in t-1), added to the core, 1977-2019 (1978 for changes):\n")
for (pr in list(c("L_payslip", "L2_payslip"), c("L_d_payslip", "L2_d_payslip"), c("L_vat", "L2_vat"), c("L_d_vat", "L2_d_vat"))) for (v in pr) {
  dd <- subset(S, year >= 1978); dd <- dd[complete.cases(dd[, c("mig_gross", CORE_A, pr)]), ]; m <- fit(dd, c(CORE_A, v), "mig_gross"); m0 <- fit(dd, CORE_A, "mig_gross")
  n1 <- fit(dd, c(setdiff(CORE_A, "delta_oadr_10"), v), "mig_gross")
  cat(sprintf("   %-13s %s  %8.4f (p %s)  dAIC %6.2f  BG(2) %s | no-proj %8.4f (p %s)\n", v, if (grepl("^L2_", v)) "fiscal-block dating" else "tax_rates.R dating ", hc(m, v)[1], pf2(hc(m, v)[2]),
              AIC(m) - AIC(m0), pf2(bgtest(m, 2)$p.value), hc(n1, v)[1], pf2(hc(n1, v)[2]))) }

# ---- (2) the motion of the headline rates -----------------------------------------------------------------------------------------------------
R <- subset(RT, year >= 1975 & year <= 2023); R <- R[order(R$year), ]
HEAD <- c(basic = "basic", ni_ee = "ni_ee", vat = "vat", corp = "corp"); EXT <- c(HEAD, top = "top_rate", ni_er = "ni_er")
chg <- function(x, type) { dx <- c(NA, diff(x)); switch(type, all = as.numeric(dx != 0), rise = as.numeric(dx > 0), cut = as.numeric(dx < 0)) }
motion <- function(taxes, type, yrs = 1976:2023, B = 10000) {
  M <- sapply(taxes, function(v) chg(R[[v]], type))[R$year %in% yrs, , drop = FALSE]; n <- nrow(M); K <- ncol(M)
  cat(sprintf("\n   -- %s, %d-%d (%d years), taxes: %s --\n", switch(type, all = "ALL changes", rise = "RISES only", cut = "CUTS only"), min(yrs), max(yrs), n, paste(names(taxes), collapse = ", ")))
  for (j in seq_len(K)) { w <- which(M[, j] == 1); cat(sprintf("   %-6s %2d %s in %d years (%.0f%%); years: %s\n", names(taxes)[j], length(w), switch(type, all = "changes", rise = "rises", cut = "cuts"), n, 100 * length(w) / n,
      paste(R$year[R$year %in% yrs][w], collapse = " "))) }
  tab <- table(factor(rowSums(M), levels = 0:K)); cat(sprintf("   years by number of taxes changing together: %s\n", paste(sprintf("%s: %d", names(tab), tab), collapse = ", ")))
  # statistic for lag s: among events (tax j changes at t), the share followed by a change in ANOTHER tax at t+s (s = 0: same year)
  stat <- function(M) vapply(0:5, function(s) { num <- 0; den <- 0; for (j in 1:K) for (t in which(M[, j] == 1)) { if (t + s > n) next; den <- den + 1; num <- num + any(M[t + s, -j] == 1) }; if (den) num / den else NA }, 0)
  cum  <- function(M) vapply(1:5, function(s) { num <- 0; den <- 0; for (j in 1:K) for (t in which(M[, j] == 1)) { if (t + s > n) next; den <- den + 1; num <- num + any(M[(t + 1):(t + s), -j] == 1) }; if (den) num / den else NA }, 0)
  o <- stat(M); oc <- cum(M); sh <- function() apply(M, 2, function(x) { k <- sample.int(n, 1) - 1; if (k == 0) x else c(tail(x, k), head(x, n - k)) })
  N <- replicate(B, { Z <- sh(); c(stat(Z), cum(Z)) })
  cat(sprintf("   %-34s %8s %8s %12s %12s\n", "after a change in one tax ...", "observed", "chance", "p (fewer)", "p (more)"))
  for (s in 0:5) cat(sprintf("   %-34s %8.2f %8.2f %12s %12s\n", if (s == 0) "another changes the SAME year" else sprintf("another changes exactly %d yr later", s), o[s + 1], mean(N[s + 1, ], na.rm = TRUE),
                             pf2(mean(N[s + 1, ] <= o[s + 1], na.rm = TRUE)), pf2(mean(N[s + 1, ] >= o[s + 1], na.rm = TRUE))))
  for (s in 1:5) cat(sprintf("   %-34s %8.2f %8.2f %12s %12s\n", sprintf("another changes within %d yr after", s), oc[s], mean(N[6 + s, ], na.rm = TRUE),
                             pf2(mean(N[6 + s, ] <= oc[s], na.rm = TRUE)), pf2(mean(N[6 + s, ] >= oc[s], na.rm = TRUE)))) }
cat("\n\n(2) The motion of the headline rates (IFS; fiscal year starting in April of the year shown)\n")
for (type in c("all", "rise", "cut")) motion(HEAD, type)
cat("\n   ---- the same on the model period only (changes 1976-2019) ----\n"); for (type in c("all", "rise")) motion(HEAD, type, 1976:2019)
cat("\n   ---- extended set (adds the top income-tax rate and the employer NIC rate) ----\n"); for (type in c("all", "rise")) motion(EXT, type)
cat("\n   Any headline rise (basic, employee NIC, VAT, corporation tax), years: ")
anyr <- R$year[apply(sapply(HEAD, function(v) chg(R[[v]], "rise")), 1, function(z) any(z == 1, na.rm = TRUE))]; cat(paste(anyr, collapse = " "), "\n")
cat(sprintf("   gaps between years with a headline rise: %s (median %.1f years)\n", paste(diff(anyr), collapse = " "), median(diff(anyr))))
cat("\nDone.\n")
