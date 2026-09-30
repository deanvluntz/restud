# =============================================================================
# tax_rates.R -- what can the statutory tax rates add to the core? (Sam, 30 Sep 2026)
# Data: IFS Fiscal Facts rate tables uploaded by Sam (determinants/ifs/: income_2023, ni_2023, vat_2023, corp_2023). Fiscal year y-(y+1) is
# year y: rates are announced at the spring Budget of y and in force from April y; like the other regressors they enter at t-1.
# Coverage: income tax, VAT and corporation tax from 1973-74; NICs as a percentage main rate from 1975-76 (flat-rate stamps before). Every
# construct that uses the NIC rate therefore starts in 1976 (level at t-1) or 1977 (change at t-1): the rate tests run on 1977-2019 (N 43), the
# Citizen outcome's years from 1977, same specification, nothing re-selected. Only the projection x tax-burden interaction runs on 1965-2019 too.
#
# Grounding (manuscript Section 3). The cost of the tax instrument is its salience, s_tau; P2 needs the salience of a further rise to increase with
# the burden already borne (condition A2); lambda_t, the shadow value of fiscal room, rises with the inherited burden, and the first-order condition
# multiplies lambda_t by the marginal benefit of relief, MB(projection). Receipts per WAP mix what voters see (rates announced on Budget Day) with
# what they do not (fiscal drag on frozen thresholds, the base, the cycle). Constructs and prior signs, stated before estimation:
#  A. SALIENT BURDEN (level, t-1; P2 through salience; prior +)
#     payslip   basic income-tax rate + employee NIC main rate: the marginal rate the typical worker reads on the payslip (pp)
#     atr_ave   income tax + employee NIC paid by a single average earner, % of earnings (rates AND thresholds: the burden actually borne)
#     paid_ave  the same payment in real pounds (2024-25 prices): the deduction the average earner sees, per individual (Sam's framing of the burden)
#     top_rate  highest marginal income-tax rate (the most symbolic rate, paid by few; weak prior)
#     wedge     full wedge on the average worker's marginal pound: basic + employee NIC + employer NIC + VAT (prior: weaker than payslip, since
#               employer NIC and VAT reach the voter only through wages and prices)
#  B. RECENT VISIBLE RATE CHANGES (Sam: governments are reluctant to keep changing rates; the policy counterpart of the t-2 tax term)
#     d_payslip change in the payslip rate, t-2 -> t-1 (prior -: a gap just closed by a visible rise needs less quiet relief)
#     d_vat     change in the standard VAT rate, t-2 -> t-1 (prior -)
#     rises3    number of the last three years in which a headline rate (basic, employee NIC or standard VAT) rose (prior -)
#  C. THE QUIET TAX INSTRUMENT (fiscal drag: thresholds held while earnings rise raise the burden without an announced rate change)
#     pa_earn   single personal allowance as % of annual average earnings (prior +: less drag in use, more need for the other quiet instrument)
#     d_pa_earn its change, t-2 -> t-1 (prior +)
#  D. THE MODEL'S INTERACTION (FOC: lambda_t x MB(projection); P1: the projection response grows with the cost of the tax alternative; prior +)
#     proj x payslip, proj x atr_ave (1977-2019) and proj x tax per WAP (both periods); the burden is centred at its sample mean, so the
#     projection coefficient remains the effect at the average burden.
# For each: (i) added to the core -- own coefficient (HAC p), tax per WAP, projection, dAIC on the same years, diagnostics; (ii) Sam's criterion
# for extending the fiscal model -- the addition also improves the model WITHOUT the projection; (iii) for A, in place of tax per WAP; (iv) for B
# and C, whether tax per WAP at t-2 still adds anything on top (the t-2 term Sam rejected on grounds).
# Secondary objective: serial correlation -- BG(1), BG(2) and residual ACF for every row; which constructs move BG(2) above .05.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); suppressWarnings(library(readxl))

# ---- 1. Parse the IFS tables -> determinants/ifs_tax_rates_1973_2023.csv -------------------------------------------------------------------------
IFS <- "determinants/ifs/"
rd <- function(f, sh) suppressMessages(as.data.frame(read_excel(paste0(IFS, f), sheet = sh, col_names = FALSE, col_types = "text")))
yr <- function(x) suppressWarnings(as.integer(substr(x, 1, 4)))
num <- function(x) vapply(x, function(s) { if (is.na(s)) return(NA_real_); s <- gsub(",", " ", gsub(",(?=[0-9]{3})", "", s, perl = TRUE)); v <- suppressWarnings(as.numeric(regmatches(s, gregexpr("[0-9]*\\.?[0-9]+", s))[[1]]))
  if (!length(v) || all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) }, 0, USE.NAMES = FALSE)          # footnote letters stripped; a range -> its maximum
pc <- function(x) ifelse(!is.na(x) & x < 1, 100 * x, x)                                               # fractions (0.3) -> percentage points
by_year <- function(d) { d <- d[!is.na(d$year), ]; aggregate(. ~ year, d, function(z) if (all(is.na(z))) NA else max(z, na.rm = TRUE), na.action = na.pass) }
a <- rd("income_2023.xlsx", "Rates and limits (non-devolved)")
inc <- by_year(data.frame(year = yr(a[[1]]), start_band = num(a[[2]]), brl = num(a[[3]]), start_rate = pc(num(a[[6]])), basic = pc(num(a[[7]])),
                          higher = pc(num(a[[8]])), addl = pc(num(a[[9]]))))
a <- rd("income_2023.xlsx", "Allowances")
alw <- by_year(data.frame(year = yr(a[[1]]), pa = dplyr::coalesce(num(a[[2]]), num(a[[10]]), num(a[[13]]))))  # single, under 65 (to 1989-90 col 2; 1990-2015 col 10; from 2016 col 13)
a <- rd("ni_2023.xlsx", "Employed")
nic <- by_year(data.frame(year = yr(a[[1]]), lel = num(a[[2]]), uel = num(a[[3]]), pt = num(a[[4]]), ni_ee = num(a[[9]]), ni_er = num(a[[13]])))  # the two 1985 and 1989 rows share main rates
a <- rd("vat_2023.xlsx", "VAT rates"); vat <- by_year(data.frame(year = yr(a[[1]]), vat = num(a[[2]])))
a <- rd("corp_2023.xlsx", "Tax rates"); crp <- by_year(data.frame(year = yr(a[[1]]), corp = pc(num(a[[2]]))))
RT <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE), list(inc, alw, nic, vat, crp)); RT <- subset(RT, year >= 1973 & year <= 2023)
RT$top_rate <- pmax(RT$higher, RT$addl, na.rm = TRUE)
write.csv(RT, "determinants/ifs_tax_rates_1973_2023.csv", row.names = FALSE)

# ---- 2. Constructs ------------------------------------------------------------------------------------------------------------------------------
S <- build_data()[["1964"]]$S; S <- merge(S, RT, by = "year", all.x = TRUE); S <- S[order(S$year), ]
for (i in which(is.na(S$awe_gbp) & S$year >= 2017)) S$awe_gbp[i] <- S$awe_gbp[i - 1] * (1 + S$wage_infl[i] / 100)   # 2017-19 by KAB9 growth, as data_common.R does for tax_earn
E <- 52 * S$awe_gbp                                                                                   # annual average earnings (GBP)
taxable <- pmax(E - S$pa, 0); sb <- ifelse(is.na(S$start_rate) | is.na(S$start_band), 0, S$start_band)   # starting / lower band where one existed (1992-2007)
it <- ifelse(is.na(S$start_rate), 0, S$start_rate / 100) * pmin(sb, taxable) + S$basic / 100 * pmax(taxable - sb, 0)
wk <- S$awe_gbp; nic_w <- ifelse(S$year <= 1988, S$ni_ee / 100 * pmin(wk, S$uel),                     # 1975-88: main rate on all earnings once above the LEL
               ifelse(S$year <= 1998, 0.02 * S$lel + S$ni_ee / 100 * (pmin(wk, S$uel) - S$lel),       # Oct 1989-1998: 2% on the LEL, main rate above it
                      S$ni_ee / 100 * pmax(pmin(wk, S$uel) - ifelse(is.na(S$pt), S$lel, S$pt), 0)))   # 1999: main rate above the LEL (no entry charge); from 2000 above the primary threshold
S$payslip <- S$basic + S$ni_ee
S$atr_ave <- 100 * (it + 52 * nic_w) / E
S$wedge   <- 100 * (1 - (1 - S$payslip / 100) / ((1 + S$ni_er / 100) * (1 + S$vat / 100)))
S$pa_earn <- 100 * S$pa / E
rf <- as.data.frame(read_excel(WB, sheet = "ReformSPA")); dfl <- as.numeric(rf$`deflator24_25=100`)[match(S$year, rf$year)]
S$paid_ave <- (it + 52 * nic_w) / dfl * 100 / 1000                                                   # income tax + employee NIC of the average earner, GBP k, 2024-25 prices
up <- function(x) as.numeric(c(NA, diff(x)) > 0)
S$rise_any <- pmax(up(S$basic), up(S$ni_ee), up(S$vat))
for (v in c("payslip", "atr_ave", "paid_ave", "top_rate", "wedge", "pa_earn", "vat", "ni_er", "corp")) { S[[paste0("L_", v)]] <- lag1(S[[v]]); S[[paste0("L_d_", v)]] <- lag1(c(NA, diff(S[[v]]))) }
S$rises3 <- lag1(S$rise_any) + lag1(lag1(S$rise_any)) + lag1(lag1(lag1(S$rise_any)))
S$L2_tax_workpop <- lag1(S$L_tax_workpop)
S7 <- subset(S, year >= 1977); if (any(is.na(S7$L_atr_ave))) cat(sprintf("(average-earner constructs missing at t-1 for: %s)\n", paste(S7$year[is.na(S7$L_atr_ave)], collapse = ", ")))
ctr <- function(d, v) d[[v]] - mean(d[[v]][complete.cases(d[, c("mig_gross", CORE_A)])], na.rm = TRUE)
mkint <- function(d) { for (v in c("L_payslip", "L_atr_ave", "L_tax_workpop")) d[[paste0("proj_x_", sub("^L_", "", v))]] <- d$delta_oadr_10 * ctr(d, v); d }
S <- mkint(S); S7 <- mkint(S7)

cat("Statutory rates used (fiscal year beginning in April of the year shown), selected years:\n")
print(RT[RT$year %in% c(1973, 1975, 1979, 1985, 1988, 1994, 1999, 2003, 2008, 2011, 2019), c("year", "basic", "top_rate", "ni_ee", "ni_er", "vat", "corp", "pa")], row.names = FALSE)
cat("\nConstructs, selected years (average earner = AWE x 52):\n")
print(round(S[S$year %in% c(1976, 1979, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2019), c("year", "payslip", "atr_ave", "paid_ave", "wedge", "pa_earn", "tax_workpop")], 2), row.names = FALSE)
cat(sprintf("\nCorrelations on 1977-2019 (t-1): tax per WAP with payslip %.2f, atr_ave %.2f, wedge %.2f, pa_earn %.2f; payslip with atr_ave %.2f\n",
            cor(S7$L_tax_workpop, S7$L_payslip), cor(S7$L_tax_workpop, S7$L_atr_ave, use = "pair"), cor(S7$L_tax_workpop, S7$L_wedge), cor(S7$L_tax_workpop, S7$L_pa_earn, use = "pair"), cor(S7$L_payslip, S7$L_atr_ave, use = "pair")))
cat(sprintf("   tax per WAP with paid_ave %.2f (both real, per person)\n", cor(S7$L_tax_workpop, S7$L_paid_ave, use = "pair")))

# ---- 3. Evaluation -------------------------------------------------------------------------------------------------------------------------------
NOPROJ <- setdiff(CORE_A, "delta_oadr_10")
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
same <- function(d, xs) d[complete.cases(d[, c("mig_gross", xs)]), ]
acf2 <- function(m) acf(residuals(m), lag.max = 2, plot = FALSE)$acf[2:3]
row_add <- function(d, v, base = CORE_A, focus = v) { dd <- same(d, c(base, v, NOPROJ)); m0 <- fit(dd, base, "mig_gross"); m <- fit(dd, c(base, v), "mig_gross")
  n0 <- fit(dd, NOPROJ, "mig_gross"); n1 <- fit(dd, c(NOPROJ, setdiff(v, "delta_oadr_10")), "mig_gross"); t2 <- fit(dd, c(base, v, "L2_tax_workpop"), "mig_gross")
  list(n = nobs(m), own = hc(m, focus), tax = hc(m, "L_tax_workpop"), proj = hc(m, "delta_oadr_10"), dA = AIC(m) - AIC(m0),
       np = if (grepl("^proj_x_", focus)) c(NA, NA) else hc(n1, focus), ndA = if (grepl("^proj_x_", focus)) NA else AIC(n1) - AIC(n0),
       t2 = hc(t2, "L2_tax_workpop"), bg1 = bgtest(m, 1)$p.value, bg2 = bgtest(m, 2)$p.value, bg2_0 = bgtest(m0, 2)$p.value, ac = acf2(m),
       reset = resettest(m)$p.value, rain = raintest(m)$p.value, supf = suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value),
       oos = oos(m, "mig_gross"), oos0 = oos(m0, "mig_gross"), vif = max(vif(m))) }
HDR <- sprintf("   %-18s %3s %17s %17s %17s %6s | %17s %6s | %14s | %5s %5s %11s %5s %5s %5s %5s %6s\n", "construct", "N", "own coef (p)", "tax/WAP (p)",
               "projection (p)", "dAIC", "no-proj: own (p)", "dAIC", "tax t-2 on top", "BG1", "BG2", "ACF 1, 2", "RESET", "rain", "sup-F", "VIF", "OOS")
prow <- function(lab, r) cat(sprintf("   %-18s %3d %9.4f (%5s) %9.4f (%5s) %9.4f (%5s) %6.2f | %9.4f (%5s) %6.2f | %7.4f (%4s) | %5s %5s %+.2f %+.2f %5s %5s %5s %5.1f %.3f\n",
  lab, r$n, r$own[1], pf2(r$own[2]), r$tax[1], pf2(r$tax[2]), r$proj[1], pf2(r$proj[2]), r$dA, r$np[1], pf2(r$np[2]), r$ndA, r$t2[1], pf2(r$t2[2]),
  pf2(r$bg1), pf2(r$bg2), r$ac[1], r$ac[2], pf2(r$reset), pf2(r$rain), pf2(r$supf), r$vif, r$oos))
GRP <- list("A. salient burden (level, t-1; prior +)" = c("L_payslip", "L_atr_ave", "L_paid_ave", "L_top_rate", "L_wedge"),
            "B. recent visible rate changes (t-2 -> t-1; prior -)" = c("L_d_payslip", "L_d_vat", "rises3", "L_d_atr_ave", "L_d_paid_ave", "L_d_top_rate"),
            "C. quiet tax instrument: fiscal drag (prior +)" = c("L_pa_earn", "L_d_pa_earn"),
            "D. projection x burden (centred; prior +)" = c("proj_x_payslip", "proj_x_atr_ave", "proj_x_tax_workpop"),
            "other statutory rates (no prior)" = c("L_ni_er", "L_d_ni_er", "L_vat", "L_corp", "L_d_corp"))
RES <- list()
for (nm in c("1977-2019", "Citizen sample (1965-2019)")) { d <- if (nm == "1977-2019") S7 else S
  m0 <- fit(d, CORE_A, "mig_gross"); cat(sprintf("\n==================== %s ====================\n", nm))
  cat(sprintf("   core: N %d, AIC %.2f, projection %.4f (p %s), tax/WAP %.4f (p %s); BG(1) %s BG(2) %s, ACF %+.2f %+.2f; OOS %.3f\n", nobs(m0), AIC(m0), hc(m0, "delta_oadr_10")[1],
              pf2(hc(m0, "delta_oadr_10")[2]), hc(m0, "L_tax_workpop")[1], pf2(hc(m0, "L_tax_workpop")[2]), pf2(bgtest(m0, 1)$p.value), pf2(bgtest(m0, 2)$p.value), acf2(m0)[1], acf2(m0)[2], oos(m0, "mig_gross")))
  if (nm != "1977-2019") { cat("   (only the projection x tax per WAP interaction exists before 1976)\n"); cat(HDR); r <- row_add(d, "proj_x_tax_workpop"); prow("proj_x_tax_workpop", r); RES[[paste(nm, "proj_x_tax_workpop")]] <- r; next }
  cat("   (i) added to the core; (ii) Sam's criterion: the same addition to the core WITHOUT the projection; 'tax t-2 on top' = tax per WAP at t-2 added as well\n"); cat(HDR)
  for (g in names(GRP)) { cat(sprintf("   [%s]\n", g)); for (v in GRP[[g]]) { if (!v %in% names(d)) next
    add <- if (grepl("^proj_x_", v)) v else v; r <- tryCatch(row_add(d, add), error = function(e) NULL); if (is.null(r)) next; prow(v, r); RES[[paste(nm, v)]] <- r } }
  cat("\n   (iii) salient measure IN PLACE of tax per WAP (dAIC against the core on the same years)\n")
  for (v in GRP[[1]]) { dd <- same(d, c(CORE_A, v)); m0 <- fit(dd, CORE_A, "mig_gross"); m <- fit(dd, c(setdiff(CORE_A, "L_tax_workpop"), v), "mig_gross")
    n0 <- fit(dd, NOPROJ, "mig_gross"); n1 <- fit(dd, c(setdiff(NOPROJ, "L_tax_workpop"), v), "mig_gross")
    cat(sprintf("   %-12s own %8.4f (p %s)  projection %.4f (p %s)  dAIC %6.2f | no-proj: own %8.4f (p %s) dAIC %6.2f | BG(2) %s RESET %s sup-F %s\n", v, hc(m, v)[1], pf2(hc(m, v)[2]),
                hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]), AIC(m) - AIC(m0), hc(n1, v)[1], pf2(hc(n1, v)[2]), AIC(n1) - AIC(n0),
                pf2(bgtest(m, 2)$p.value), pf2(resettest(m)$p.value), pf2(suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value)))) }
  cat("\n   (iv) salient level + recent change together (the level-and-change form of the t-1/t-2 pair, in rates rather than receipts)\n")
  for (pr in list(c("L_payslip", "L_d_payslip"), c("L_atr_ave", "L_d_atr_ave"), c("L_pa_earn", "L_d_pa_earn"), c("L_payslip", "L_pa_earn"), c("L_d_payslip", "L_d_pa_earn"))) {
    dd <- same(d, c(CORE_A, pr)); m0 <- fit(dd, CORE_A, "mig_gross"); m <- fit(dd, c(CORE_A, pr), "mig_gross"); n0 <- fit(dd, NOPROJ, "mig_gross"); n1 <- fit(dd, c(NOPROJ, pr), "mig_gross")
    cat(sprintf("   %-26s %s %.4f (p %s), %s %.4f (p %s); tax/WAP %.4f (p %s); projection %.4f (p %s); dAIC %6.2f | no-proj dAIC %6.2f | BG(2) %s\n", paste(pr, collapse = " + "),
                pr[1], hc(m, pr[1])[1], pf2(hc(m, pr[1])[2]), pr[2], hc(m, pr[2])[1], pf2(hc(m, pr[2])[2]), hc(m, "L_tax_workpop")[1], pf2(hc(m, "L_tax_workpop")[2]),
                hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]), AIC(m) - AIC(m0), AIC(n1) - AIC(n0), pf2(bgtest(m, 2)$p.value))) } }

# ---- 4. Which constructs meet Sam's criterion (own p < .10 beside the projection AND without it, dAIC < 0 in both)? --------------------------------
cat("\n==================== Summary (1977-2019) ====================\n")
k <- grep("^1977-2019 ", names(RES), value = TRUE); pass <- k[vapply(k, function(z) { r <- RES[[z]]; !is.na(r$np[2]) && r$own[2] < 0.10 && r$np[2] < 0.10 && r$dA < 0 && r$ndA < 0 }, logical(1))]
cat(sprintf("   meet both halves of the criterion: %s\n", if (length(pass)) paste(sub("^1977-2019 ", "", pass), collapse = ", ") else "none"))
sig <- k[vapply(k, function(z) RES[[z]]$own[2] < 0.10, logical(1))]
cat(sprintf("   significant (p < .10) beside the projection only: %s\n", if (length(sig)) paste(sub("^1977-2019 ", "", sig), collapse = ", ") else "none"))
cat("   Secondary objective -- serial correlation: BG(2) of the core on 1977-2019 and the constructs that lift it above .05:\n")
b2 <- vapply(k, function(z) RES[[z]]$bg2, 0); cat(sprintf("   core BG(2) %s; above .05: %s\n", pf2(RES[[k[1]]]$bg2_0), if (any(b2 >= 0.05)) paste(sprintf("%s (%s)", sub("^1977-2019 ", "", k[b2 >= 0.05]), vapply(b2[b2 >= 0.05], pf2, "")), collapse = ", ") else "none"))
for (z in pass) { v <- sub("^1977-2019 ", "", z); S7x <- S7
  for (o in list(c("res_gross", "L_res_gross", "residence-based"), c("cit_gross", "L_cit_gross", "citizenship, any residence"))) {
    mo <- fit(S7x, c(o[2], setdiff(c(CORE_A, v), "L_mig_gross")), o[1]); cat(sprintf("   %s, outcome %s: %s %.4f (p %s), projection %.4f (p %s), BG(2) %s\n", v, o[3], v, hc(mo, v)[1], pf2(hc(mo, v)[2]),
      hc(mo, "delta_oadr_10")[1], pf2(hc(mo, "delta_oadr_10")[2]), pf2(bgtest(mo, 2)$p.value))) } }
cat("\nDone.\n")
