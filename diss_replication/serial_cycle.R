# =============================================================================
# serial_cycle.R -- (1) the lag-2 serial correlation in the current core; (2) cyclical components, the post-2008 break and British
# inflows (Sam, 30 Sep 2026).
# Current core (timing A): mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_int_tme.
# L_ln_debt_gdp = ln(debt/GDP)_{t-1} - ln(debt/GDP)_{t-2}, so the debt term reaches back to t-2.
# PART 1. (a) residual autocorrelations and tests; (b) which term: drop each regressor; (c) timing of the debt and interest terms;
# (d) second lags (outcome and each regressor); (e) estimators that allow AR(2) errors with a lagged outcome: maximum likelihood with
# AR(2) errors, and IV for the lagged outcome (instruments: second lags of the regressors), because with a lagged outcome serially
# correlated errors bias OLS -- HAC standard errors fix inference but not that bias.
# PART 2. Cyclical components built from the dataset: HP cycle of log real GDP (lambda 100 and 6.25; two-sided, so it uses later
# years), Hamilton (2018) cycle (h = 2, backward-looking), unemployment gap and log V/U gap (HP trend, lambda 100), OBR output gap
# (from 1972). Each is tested (i) in the main equation under timing A and B (break tests), (ii) as an interaction with the lagged
# projection under B, and (iii) in the British-inflow equations (all origins, LTIM, 1965-2019; from non-EU countries, IPS, 1977-2019).
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); ED <- build_data(); SAMPLE <- "1964"; source("data_common.R")
A <- subset(df, year <= 2019); A$L_delta_oadr_10 <- lag1(A$delta_oadr_10)
# cyclical components on the full available span, then lagged
hp <- function(y, lambda) { ok <- !is.na(y); n <- sum(ok); D <- diff(diag(n), differences = 2); tr <- rep(NA, length(y)); tr[ok] <- solve(diag(n) + lambda * crossprod(D), y[ok]); tr }
A$lgdp <- cumsum(ifelse(is.na(A$growth), 0, log(1 + A$growth / 100))); A$lgdp[is.na(A$growth)] <- NA
A$cyc_hp100 <- 100 * (A$lgdp - hp(A$lgdp, 100)); A$cyc_hp6 <- 100 * (A$lgdp - hp(A$lgdp, 6.25))
ham <- rep(NA, nrow(A)); i <- 5:nrow(A); hm <- lm(A$lgdp[i] ~ A$lgdp[i - 2] + A$lgdp[i - 3] + A$lgdp[i - 4]); ham[as.integer(names(residuals(hm))) + 4] <- 100 * residuals(hm); A$cyc_ham <- ham
A$unem_gap <- A$unem - hp(A$unem, 100); A$vu_gap <- log(A$vac_unem) - hp(log(A$vac_unem), 100)
for (v in c("cyc_hp100", "cyc_hp6", "cyc_ham", "unem_gap", "vu_gap")) A[[paste0("L_", v)]] <- lag1(A[[v]])
A$L2_mig_gross <- lag1(A$L_mig_gross); for (v in c("tax_workpop", "ln_debt_gdp", "ln_vac_unem", "int_tme")) A[[paste0("L2_", v)]] <- lag1(A[[paste0("L_", v)]])
A$ln_debt_now <- c(NA, head(A$L_ln_debt_gdp, -1)) * NA; A$ln_debt_now <- c(A$L_ln_debt_gdp[-1], NA)          # change t-1 -> t (current year)
A$int_now <- c(A$L_int_tme[-1], NA); A$d2_debt <- A$L_ln_debt_gdp + A$L2_ln_debt_gdp                          # two-year change t-3 -> t-1
A$brit_noneu_gross <- A$brit_noneu + log(lag1(A$workpop)) - log(1000); A$L_brit_noneu_gross <- lag1(A$brit_noneu_gross); A$L_brit_gross <- lag1(A$brit_gross)
S <- subset(A, year >= 1965); S7 <- subset(S, year >= 1977)
bgl <- function(m) sprintf("BG(1) %s BG(2) %s BG(3) %s LB(4) %s", pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(bgtest(m, 3)$p.value), pf2(Box.test(residuals(m), 4, "Ljung-Box")$p.value))
pj <- function(m, P = "delta_oadr_10") { ct <- coeftest(m, vcov. = vcovHAC(m)); sprintf("projection %.4f (p %s)", ct[P, 1], pf2(ct[P, 4])) }

cat("\n==================== PART 1. Lag-2 serial correlation in the current core ====================\n")
for (nm in c("Citizen 1965-2019", "1977-2019")) { d <- if (nm == "1977-2019") S7 else S; m <- fit(d, CORE_A, "mig_gross"); r <- residuals(m); n <- length(r)
  ac <- acf(r, lag.max = 4, plot = FALSE)$acf[-1]; e <- data.frame(e = r, e2 = c(NA, NA, head(r, -2)), attr(m, "dd")[, CORE_A]); m2 <- lm(e ~ ., data = e)
  cat(sprintf("\n-- %s (N %d): residual autocorrelations lag 1-4: %s (approx. 95%% band +/- %.2f)\n   %s; lag-2-only test (e_t on e_{t-2} and the regressors): coefficient %.2f (p %s)\n",
              nm, n, paste(sprintf("%.2f", ac), collapse = ", "), 1.96 / sqrt(n), bgl(m), coef(m2)["e2"], pf2(summary(m2)$coefficients["e2", 4])))
  big <- order(-abs(r))[1:4]; cat(sprintf("   largest residuals: %s\n", paste(sprintf("%d %+.3f", attr(m, "dd")$year[big], r[big]), collapse = "; "))) }
cat("\n(b) Drop one regressor at a time (Citizen 1965-2019):\n")
for (v in CORE_A[-(1:2)]) { m <- fit(S, setdiff(CORE_A, v), "mig_gross"); cat(sprintf("   without %-15s AIC %7.2f  %s  %s\n", v, AIC(m), bgl(m), pj(m))) }
m <- fit(S, CORE_A, "mig_gross"); cat(sprintf("   (core               AIC %7.2f  %s  %s)\n", AIC(m), bgl(m), pj(m)))
cat("\n(c) Timing of the debt and interest terms (Citizen 1965-2019; same years):\n")
V <- list("core: debt change t-2 -> t-1" = CORE_A,
  "debt change t-1 -> t (current)" = c(setdiff(CORE_A, "L_ln_debt_gdp"), "ln_debt_now"),
  "debt changes t-2 -> t-1 and t-3 -> t-2" = c(CORE_A, "L2_ln_debt_gdp"),
  "two-year debt change t-3 -> t-1" = c(setdiff(CORE_A, "L_ln_debt_gdp"), "d2_debt"),
  "no debt term" = setdiff(CORE_A, "L_ln_debt_gdp"),
  "interest share at t (current)" = c(setdiff(CORE_A, "L_int_tme"), "int_now"),
  "interest share t-1 and t-2" = c(CORE_A, "L2_int_tme"))
yrs <- Reduce(intersect, lapply(V, function(x) fit(S, x, "mig_gross") |> attr("dd") |> (\(d) d$year)()))
for (k in names(V)) { m <- fit(S[S$year %in% yrs, ], V[[k]], "mig_gross"); cat(sprintf("   %-40s AIC %7.2f  %s  %s\n", k, AIC(m), bgl(m), pj(m))) }
cat("\n(d) Second lags (Citizen 1965-2019):\n")
for (v in c("L2_mig_gross", "L2_tax_workpop", "L2_ln_debt_gdp", "L2_ln_vac_unem", "L2_int_tme")) { m <- fit(S, c(CORE_A, v), "mig_gross"); ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("   + %-15s coef %8.4f (p %s)  AIC %7.2f  %s  %s\n", v, ct[v, 1], pf2(ct[v, 4]), AIC(m), bgl(m), pj(m))) }
m <- fit(S, c(CORE_A, "L2_mig_gross", "L2_tax_workpop", "L2_ln_debt_gdp", "L2_ln_vac_unem", "L2_int_tme"), "mig_gross")
cat(sprintf("   + all second lags (ARDL(2,2), %d parameters): AIC %7.2f  %s  %s; long-run projection %.4f\n", length(coef(m)), AIC(m), bgl(m), pj(m),
            coef(m)["delta_oadr_10"] / (1 - coef(m)["L_mig_gross"] - coef(m)["L2_mig_gross"])))
cat("\n(e) Estimators allowing AR(2) errors with a lagged outcome:\n")
for (nm in c("Citizen 1965-2019", "1977-2019")) { d <- if (nm == "1977-2019") S7 else S; m <- fit(d, CORE_A, "mig_gross"); dd <- attr(m, "dd")
  X <- as.matrix(dd[, CORE_A]); a2 <- arima(dd$mig_gross, order = c(2, 0, 0), xreg = X, method = "ML"); se <- sqrt(diag(a2$var.coef))
  lr <- function(b) b["delta_oadr_10"] / (1 - b["L_mig_gross"])
  cat(sprintf("   %-18s OLS: projection %.4f, lagged outcome %.3f, long run %.4f, AIC %.2f\n", nm, coef(m)["delta_oadr_10"], coef(m)["L_mig_gross"], lr(coef(m)), AIC(m)))
  cat(sprintf("   %-18s ML with AR(2) errors: projection %.4f (se %.4f, p %s), lagged outcome %.3f, long run %.4f; AR coefficients %.2f, %.2f (se %.2f, %.2f); AIC %.2f; Ljung-Box(4) of innovations p %s\n", "",
              a2$coef["delta_oadr_10"], se["delta_oadr_10"], pf2(2 * pnorm(-abs(a2$coef["delta_oadr_10"] / se["delta_oadr_10"]))), a2$coef["L_mig_gross"], lr(a2$coef),
              a2$coef["ar1"], a2$coef["ar2"], se["ar1"], se["ar2"], a2$aic, pf2(Box.test(residuals(a2), 4, "Ljung-Box", fitdf = 2)$p.value)))
  ivd <- subset(A, year %in% dd$year); iv <- AER::ivreg(mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_int_tme |
    delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_int_tme + L_delta_oadr_10 + L2_tax_workpop + L2_ln_debt_gdp + L2_ln_vac_unem + L2_int_tme, data = ivd)
  civ <- coeftest(iv, vcov. = sandwich::vcovHAC(iv)); fs <- summary(iv, diagnostics = TRUE)$diagnostics
  cat(sprintf("   %-18s IV for the lagged outcome: projection %.4f (p %s), lagged outcome %.3f (p %s), long run %.4f; first-stage F %.1f, Sargan p %s\n", "",
              civ["delta_oadr_10", 1], pf2(civ["delta_oadr_10", 4]), civ["L_mig_gross", 1], pf2(civ["L_mig_gross", 4]), lr(coef(iv)), fs["Weak instruments", "statistic"], pf2(fs["Sargan", "p-value"]))) }

cat("\n(f) The tax burden at t-2 in place of t-1 (same number of parameters; found in (d), so a candidate, not a prior):\n")
X2 <- c(setdiff(CORE_A, "L_tax_workpop"), "L2_tax_workpop")
for (nm in c("Citizen 1965-2019", "1977-2019")) { d <- if (nm == "1977-2019") S7 else S; m <- fit(d, X2, "mig_gross"); m0 <- fit(d, CORE_A, "mig_gross"); b <- breaks(m, "mig_gross")
  cat(sprintf("   %-18s AIC %.2f (core %.2f); %s; RESET %s rainbow %s sup-F %s; OOS RMSE from 1995 %.3f (core %.3f)\n      %s\n", nm, AIC(m), AIC(m0), bgl(m),
              pf2(resettest(m)$p.value), pf2(raintest(m)$p.value), pf2(b["supf"]), oos(m, "mig_gross"), oos(m0, "mig_gross"), coefline(m))) }
m <- fit(S, X2, "mig_gross"); ch <- chow(m, "mig_gross", 1977); cat(sprintf("   Chow at 1977 (tax at t-2): F %.2f (p %s)\n", ch["F"], pf2(ch["Fp"])))

cat("\n\n==================== PART 2. Cyclical components ====================\n")
CYC <- c("L_cyc_hp100", "L_cyc_hp6", "L_cyc_ham", "L_unem_gap", "L_vu_gap", "L_gap_obr")
cat(sprintf("   correlations with the lagged OBR output gap (1972-2019): %s\n", paste(sprintf("%s %.2f", sub("^L_", "", CYC[-6]), vapply(CYC[-6], function(v) cor(S[[v]], S$L_gap_obr, use = "pair"), 0)), collapse = "; ")))
for (P in c("delta_oadr_10", "L_delta_oadr_10")) { base <- sub("^delta_oadr_10$", P, CORE_A)
  cat(sprintf("\n(i) Main equation, timing %s, Citizen sample: + cycle (break tests: sup-F; post-2009 change in the projection)\n", if (P == "delta_oadr_10") "A (projection t)" else "B (projection t-1)"))
  for (v in c("", CYC)) { xs <- c(base, if (nchar(v)) v); m <- fit(S, xs, "mig_gross"); b <- breaks(m, "mig_gross"); ct <- coeftest(m, vcov. = vcovHAC(m))
    m0 <- fit(S[S$year %in% attr(m, "dd")$year, ], base, "mig_gross")
    cat(sprintf("   %-14s N %d %s projection %.4f (p %s) dAIC %6.2f sup-F %s post09 %+.4f (p %s) BG(2) %s\n", if (nchar(v)) v else "(none)", nobs(m),
                if (nchar(v)) sprintf("coef %7.4f (p %s)", ct[v, 1], pf2(ct[v, 4])) else sprintf("%21s", ""), ct[P, 1], pf2(ct[P, 4]), AIC(m) - AIC(m0), pf2(b["supf"]), b["ch"], pf2(b["chp"]), pf2(bgtest(m, 2)$p.value))) } }
cat("\n(ii) Timing B: cycle x projection (does the projection effect depend on the cycle?)\n")
for (v in CYC) { d <- S; d$ix <- d[[v]] * d$L_delta_oadr_10; m <- fit(d, c(sub("^delta_oadr_10$", "L_delta_oadr_10", CORE_A), v, "ix"), "mig_gross"); b <- breaks(m, "mig_gross"); ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("   %-14s interaction %+.5f (p %s)  projection at zero cycle %.4f  sup-F %s post09 %+.4f (p %s)\n", v, ct["ix", 1], pf2(ct["ix", 4]), ct["L_delta_oadr_10", 1], pf2(b["supf"]), b["ch"], pf2(b["chp"]))) }
cat("\n(iii) British inflows (log persons, own lag, core regressors, timing A): does a cycle absorb the projection?\n")
for (o in list(c("brit_gross", "L_brit_gross", "all origins (LTIM 2.00), Citizen years 1965-2019"), c("brit_noneu_gross", "L_brit_noneu_gross", "from non-EU countries (IPS 3.01), 1977-2019"))) {
  d <- if (o[1] == "brit_noneu_gross") S7 else S; cat(sprintf("   -- British citizens %s\n", o[3]))
  for (v in c("", CYC)) { xs <- c(o[2], setdiff(CORE_A, "L_mig_gross"), if (nchar(v)) v); m <- fit(d, xs, o[1]); if (is.null(m)) next; ct <- coeftest(m, vcov. = vcovHAC(m))
    cat(sprintf("      %-14s N %d projection %.4f (p %s) %s\n", if (nchar(v)) v else "(no cycle)", nobs(m), ct["delta_oadr_10", 1], pf2(ct["delta_oadr_10", 4]), if (nchar(v)) sprintf("cycle %+.4f (p %s)", ct[v, 1], pf2(ct[v, 4])) else "")) } }
cat("\nDone.\n")
