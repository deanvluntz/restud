# =============================================================================
# interest_apf.R -- what does the debt-interest term measure, and do the structural deficit or monetary policy belong in the model?
# (Sam, 30 Sep 2026: the OBR defines debt interest as central government NET OF THE APF; the APF (QE) and the 2008 bank rescues may
# explain part of the post-2008 break; index-linked gilts make debt interest move with RPI; the OBR's structural (cyclically adjusted)
# deficit; the money supply.)
# Data (all already in the repo):
#   ONS PUSF (determinants/ons_pusf_jul2026.xlsx, calendar years): NMFX = central government debt interest (gross); MU74 = the APF adjustment
#     (NMFX + MU74 = "Central government debt interest, net of APF", the OBR databank definition; MU74 is zero before 2009, the APF's first
#     year); ANLO = public-sector interest paid INCLUDING the public-sector banks (Northern Rock, RBS, Lloyds from 2008); JW2P = the same
#     EXCLUDING public-sector banks; JNYY = capital uplift on index-linked gilts (from 1998 only); FZIU = APF gilt holdings (from 2009);
#     KX5Q = total managed expenditure.
#   OBR PSF aggregates databank (determinants/obr_psf_aggregates_databank_mar2026.xlsx, fiscal years, fiscal year y-(y+1) = year y):
#     cyclically adjusted net borrowing, cyclically adjusted current budget deficit, cyclically adjusted primary balance (all from 1975-76,
#     % of GDP), public-sector current receipts (% of GDP), nominal GDP.
# The model's int_tme is the Millennium series (to 2016; 2017-19 growth-spliced). Section 1 shows what it tracks.
# Tests (core, projection at t = timing A; timing B = projection at t-1 shown for the break):
#   2. interest measure in place of int_tme -- both periods; coefficient, projection, AIC, BG(2), sup-F, and under timing B the sup-F and the
#      post-2009 change in the projection (the break found in diag_shift.R)
#   3. structural deficit (1977-2019 only; the cyclically adjusted series start in 1975-76) -- added, in place of ln_debt_gdp, and Sam's
#      criterion (the same addition without the projection); actual PSNB shown for comparison (it had the wrong sign earlier: the cycle)
#   4. monetary policy: APF gilt holdings / GDP (QE stock; zero before 2009), its change, Bank Rate, M4 growth -- added; timing B break.
#      Caution: the QE stock is zero before 2009, so it absorbs any post-2009 shift mechanically; read a break "removed" by it with that in mind.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); suppressWarnings(library(readxl))
a <- suppressMessages(as.data.frame(read_excel("determinants/ons_pusf_jul2026.xlsx", col_names = FALSE, col_types = "text"))); id <- unlist(a[2, ]); yv <- a[[1]]; ann <- grepl("^[0-9]{4}$", yv)
g <- function(v) as.numeric(a[ann, which(id == v)])
PU <- data.frame(year = as.integer(yv[ann]), NMFX = g("NMFX"), MU74 = g("MU74"), ANLO = g("ANLO"), JW2P = g("JW2P"), KX5Q = g("KX5Q"), JNYY = g("JNYY"), FZIU = g("FZIU"))
PU$MU74[is.na(PU$MU74)] <- 0; PU$FZIU[is.na(PU$FZIU) | PU$year < 2009] <- 0
yr <- function(x) suppressWarnings(as.integer(substr(x, 1, 4)))
b <- suppressMessages(as.data.frame(read_excel("determinants/obr_psf_aggregates_databank_mar2026.xlsx", sheet = 4, col_names = FALSE, col_types = "text")))
nb <- function(j) suppressWarnings(as.numeric(ifelse(b[[j]] == "-", NA, b[[j]])))
OB <- data.frame(year = yr(b[[1]]), receipts_gdp = nb(2), ca_cbd_gdp = nb(10), ca_pb_gdp = nb(12), capsnb_gdp = nb(13), psnb_obr_gdp = nb(14), int_apf_gdp = nb(21), gdp_bn = nb(30))
OB <- OB[!is.na(OB$year) & !duplicated(OB$year), ]
S <- merge(build_data()[["1964"]]$S, merge(PU, OB, by = "year", all = TRUE), by = "year", all.x = TRUE); S <- S[order(S$year), ]
S$int_ps      <- 100 * S$ANLO / S$KX5Q                    # public sector incl. public-sector banks
S$int_ps_xbk  <- 100 * S$JW2P / S$KX5Q                    # public sector excl. public-sector banks
S$int_cg      <- 100 * S$NMFX / S$KX5Q                    # central government, gross
S$int_cg_apf  <- 100 * (S$NMFX + S$MU74) / S$KX5Q         # central government net of APF (OBR definition)
S$int_rec_apf <- 100 * S$int_apf_gdp / S$receipts_gdp     # the same as a share of current receipts (the OBR's 'share of revenue' framing)
S$qe_gdp      <- 100 * S$FZIU / 1000 / S$gdp_bn           # APF gilt holdings, % of GDP
S$ca_pd_gdp   <- -S$ca_pb_gdp                              # cyclically adjusted primary DEFICIT (sign turned so + = deficit, like the others)
for (v in c("int_ps", "int_ps_xbk", "int_cg", "int_cg_apf", "int_rec_apf", "qe_gdp", "capsnb_gdp", "ca_cbd_gdp", "ca_pd_gdp", "psnb_obr_gdp")) S[[paste0("L_", v)]] <- lag1(S[[v]])
S$L_d_qe_gdp <- lag1(c(NA, diff(S$qe_gdp)))

cat("1. The interest share of TME under each definition (% of TME), selected years\n")
print(round(S[S$year %in% c(1965, 1975, 1985, 1995, 2000, 2005:2019), c("year", "int_tme", "int_ps", "int_ps_xbk", "int_cg", "int_cg_apf", "int_rec_apf", "qe_gdp")], 2), row.names = FALSE)
cat(sprintf("   Correlation with the model's int_tme, 1965-2019: incl. banks %.2f, excl. banks %.2f, CG gross %.2f, CG net of APF %.2f\n",
            cor(S$int_tme, S$int_ps, use = "pair"), cor(S$int_tme, S$int_ps_xbk, use = "pair"), cor(S$int_tme, S$int_cg, use = "pair"), cor(S$int_tme, S$int_cg_apf, use = "pair")))
cat(sprintf("   Correlation 1965-2007 (before the bank reclassification): incl. banks %.2f, excl. banks %.2f, CG gross %.2f\n",
            with(subset(S, year <= 2007), cor(int_tme, int_ps, use = "pair")), with(subset(S, year <= 2007), cor(int_tme, int_ps_xbk, use = "pair")), with(subset(S, year <= 2007), cor(int_tme, int_cg, use = "pair"))))
il <- subset(S, year >= 1998 & year <= 2019); cat(sprintf("   Index-linked gilt capital uplift (JNYY, from 1998), share of CG gross debt interest: mean %.0f%%, range %.0f%% to %.0f%% (%d: %.0f%%, RPI fell)\n",
            100 * mean(il$JNYY / il$NMFX), 100 * min(il$JNYY / il$NMFX), 100 * max(il$JNYY / il$NMFX), il$year[which.min(il$JNYY)], 100 * min(il$JNYY / il$NMFX)))

hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
supf <- function(m) suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value)
post09 <- function(d, xs, P) { d$post09 <- as.numeric(d$year >= 2009); d$post_x_proj <- d$post09 * d[[P]]; m <- fit(d, c(xs, "post09", "post_x_proj"), "mig_gross"); hc(m, "post_x_proj") }
B_A <- CORE_A; B_B <- sub("^delta_oadr_10$", "L_delta_oadr_10", CORE_A)
one <- function(d, xsA, focus, ref = CORE_A) { dd <- d[complete.cases(d[, c("mig_gross", unique(c(xsA, ref, "L_delta_oadr_10")))]), ]
  mA <- fit(dd, xsA, "mig_gross"); m0 <- fit(dd, ref, "mig_gross"); xsB <- sub("^delta_oadr_10$", "L_delta_oadr_10", xsA); mB <- fit(dd, xsB, "mig_gross")
  np <- fit(dd, setdiff(xsA, "delta_oadr_10"), "mig_gross"); np0 <- fit(dd, setdiff(ref, "delta_oadr_10"), "mig_gross")
  list(n = nobs(mA), own = hc(mA, focus), proj = hc(mA, "delta_oadr_10"), dA = AIC(mA) - AIC(m0), bg2 = bgtest(mA, 2)$p.value, reset = resettest(mA)$p.value, supA = supf(mA),
       supB = supf(mB), chB = post09(dd, xsB, "L_delta_oadr_10"), projB = hc(mB, "L_delta_oadr_10"), npown = hc(np, focus), npdA = AIC(np) - AIC(np0)) }
HD <- sprintf("   %-26s %3s %17s %17s %6s %5s %5s %6s | %17s %6s | timing B: %6s %17s\n", "", "N", "own coef (p)", "projection (p)", "dAIC", "BG2", "RESET", "sup-F",
              "no-proj own (p)", "dAIC", "sup-F", "post-2009 chg (p)")
pr <- function(lab, r) cat(sprintf("   %-26s %3d %9.4f (%5s) %9.4f (%5s) %6.2f %5s %5s %6s | %9.4f (%5s) %6.2f |           %6s %9.4f (%5s)\n", lab, r$n, r$own[1], pf2(r$own[2]), r$proj[1], pf2(r$proj[2]),
  r$dA, pf2(r$bg2), pf2(r$reset), pf2(r$supA), r$npown[1], pf2(r$npown[2]), r$npdA, pf2(r$supB), r$chB[1], pf2(r$chB[2])))
for (nm in c("Citizen sample (1965-2019)", "1977-2019")) { d <- if (nm == "1977-2019") subset(S, year >= 1977) else S; cat(sprintf("\n==================== %s ====================\n", nm))
  r0 <- one(d, CORE_A, "L_int_tme"); cat("   dAIC is against the current core on the same years; 'no-proj' = the same model without the projection (Sam's criterion, part a)\n"); cat(HD)
  cat("   [2. interest measure in place of int_tme]\n"); pr("int_tme (Millennium; core)", r0)
  for (v in c("L_int_ps", "L_int_ps_xbk", "L_int_cg", "L_int_cg_apf", "L_int_rec_apf")) pr(sub("^L_", "", v), one(d, sub("^L_int_tme$", v, CORE_A), v))
  pr("no interest term", one(d, setdiff(CORE_A, "L_int_tme"), "L_mig_gross"))
  cat("   [4. monetary policy, added to the core]\n"); for (v in c("L_qe_gdp", "L_d_qe_gdp", "L_bank_rate", "L_m4_growth")) pr(paste0("+", sub("^L_", "", v)), one(d, c(CORE_A, v), v))
  pr("+qe_gdp, int_cg_apf for int_tme", one(d, c(sub("^L_int_tme$", "L_int_cg_apf", CORE_A), "L_qe_gdp"), "L_qe_gdp"))
  if (nm != "1977-2019") next
  cat("   [3. structural deficit (% of GDP, t-1), added to the core; then in place of ln_debt_gdp]\n")
  for (v in c("L_capsnb_gdp", "L_ca_cbd_gdp", "L_ca_pd_gdp", "L_psnb_obr_gdp")) pr(paste0("+", sub("^L_", "", v)), one(d, c(CORE_A, v), v))
  for (v in c("L_capsnb_gdp", "L_ca_cbd_gdp", "L_ca_pd_gdp")) pr(paste0(sub("^L_", "", v), " for ln_debt_gdp"), one(d, sub("^L_ln_debt_gdp$", v, CORE_A), v))
  pr("+capsnb_gdp, int_cg_apf", one(d, c(sub("^L_int_tme$", "L_int_cg_apf", CORE_A), "L_capsnb_gdp"), "L_capsnb_gdp")) }
cat("\nDone.\n")
