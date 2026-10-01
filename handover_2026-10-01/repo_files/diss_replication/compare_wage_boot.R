# =============================================================================
# compare_wage_boot.R -- EXPLORATORY (Sam, 29 Sep 2026). Sam's core specification: gross inflows (no step), tax per working-age person,
# the three ratios (log change in debt/GDP, log V/U, log debt interest/TME) and the projection.  Question: is adding wage inflation
# justified at this sample size, or suspicious given that it was found by screening ~65 candidates?
#   A  degrees of freedom: parameters, N/k, AIC vs AICc, recursive OOS RMSE, with and without wage inflation
#   B  recursive residual bootstrap under H0 (no wage inflation), R = 999: finite-sample p-values for the HAC t, the classical F and
#      the AIC change (MacKinnon, 2006; Kilian, 1998 for the recursive scheme), plus a wild (Rademacher) variant for heteroskedasticity
#   C  data-snooping check (White, 2000 reality check): under H0, the bootstrap distribution of the BEST AIC improvement across all
#      full-sample candidates -- how often does screening alone produce an improvement as large as wage inflation's?
#   D  bootstrap RESET (squares) for both models
# RStudio: working directory = diss_replication/.  Runtime ~1-3 minutes.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R")
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); df$ln_int_tme <- log(df$int_tme); df$L_nom_eri <- lag1(df$nominal_eri); list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
B0 <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme"); ADDV <- "L_wage_infl"
CAND <- c("L_wage_infl", "L_unem", "L_unem_boe", "L_ln_vac_wap", "L_dln_vac_unem", "L_real_wage", "L_growth", "L_growth_pc", "L_real_eri", "L_ln_real_eri", "L_ca_gdp", "L_trade_bal", "L_tot", "L_ln_tot", "L_inflation", "L_cpi_infl",
          "L_tme_gdp", "gad_earn", "gad_policy", "L_psnb_gdp", "L_psnd_gdp", "L_ln_psnd_gdp", "golden_rule", "obr_era", "boe_indep", "trend", "L_emig_all", "L_emig_brit", "brit_rate", "L_relinc_dev", "L_relinc_rich", "L_youth", "L_real_house",
          "L_labour_share", "L_ln_wap", "oadr_0", "delta_dr_10", "L_tbill", "L_gilt", "L_consol", "L_bank_rate", "L_openness", "L_int_gdp", "L_r_g", "L_m4_growth", "L_tfp", "L_lab_prod", "L_particip", "L_days_lost", "L_grants_gdp", "L_tme_real_growth", "L_union", "L_replace", "st_inf_exp", "lt_inf_exp")
aicc <- function(m) { k <- length(coef(m)) + 1; AIC(m) + 2 * k * (k + 1) / (nobs(m) - k - 1) }
oos <- function(S, xs, from) { e <- c(); for (t in from:2019) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; tr <- tr[complete.cases(tr[, c("mig_gross", xs)]), ]; if (!nrow(te) || anyNA(te[, xs])) next; mm <- lm(reformulate(xs, "mig_gross"), data = tr); e <- c(e, te$mig_gross - predict(mm, newdata = te)) }; sqrt(mean(e^2)) }
hac_t <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); ct[v, 3] }
# recursive bootstrap: regenerate the gross-inflow series from the restricted model, with the lagged DV following its own draws
gen_y <- function(dd, m0, e) { b <- coef(m0); X <- model.matrix(m0); rho <- b["L_mig_gross"]; xb <- as.vector(X[, setdiff(colnames(X), "L_mig_gross"), drop = FALSE] %*% b[setdiff(names(b), "L_mig_gross")])
  y <- numeric(nrow(dd)); ylag <- dd$L_mig_gross[1]; for (t in seq_len(nrow(dd))) { y[t] <- xb[t] + rho * ylag + e[t]; ylag <- y[t] }; y }
boot_stats <- function(dd, m0, xs0, xs1, R, wild = FALSE, seed = 20260929) { set.seed(seed); e0 <- residuals(m0) - mean(residuals(m0)); n <- nrow(dd); out <- matrix(NA, R, 3, dimnames = list(NULL, c("t", "F", "dAIC")))
  for (r in seq_len(R)) { e <- if (wild) e0 * sample(c(-1, 1), n, replace = TRUE) else sample(e0, n, replace = TRUE); db <- dd; db$mig_gross <- gen_y(dd, m0, e)
    db$L_mig_gross <- c(dd$L_mig_gross[1], db$mig_gross[-n])   # the lag follows the regenerated series
    b0 <- lm(reformulate(xs0, "mig_gross"), data = db); b1 <- lm(reformulate(xs1, "mig_gross"), data = db)
    out[r, ] <- c(hac_t(b1, ADDV), anova(b0, b1)$F[2], AIC(b1) - AIC(b0)) }
  out }
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); from <- if (k == "extended") 1990 else 1997
  cat("\n\n######## ", SMP[[k]]$LAB, "2019 ########\n", sep = "")
  m1 <- fit(S, c(B0, ADDV), "mig_gross"); dd <- attr(m1, "dd"); m0 <- lm(reformulate(B0, "mig_gross"), data = dd); attr(m0, "dd") <- dd
  ct1 <- coeftest(m1, vcov. = vcovHAC(m1)); ct0 <- coeftest(m0, vcov. = vcovHAC(m0)); Fobs <- anova(m0, m1)$F[2]; tobs <- ct1[ADDV, 3]; dA <- AIC(m1) - AIC(m0)
  cat("\n--- A. Degrees of freedom and fit ---\n")
  cat(sprintf("%-22s %4s %3s %6s %9s %9s %9s %10s %10s\n", "model", "N", "k", "N/k", "AIC", "AICc", "adjR2", "OOS RMSE", "proj"))
  for (nm in c("core spec", "+wage_infl")) { m <- if (nm == "core spec") m0 else m1; ct <- if (nm == "core spec") ct0 else ct1
    cat(sprintf("%-22s %4d %3d %6.1f %9.2f %9.2f %9.4f %10.4f %10.4f\n", nm, nobs(m), length(coef(m)), nobs(m) / length(coef(m)), AIC(m), aicc(m), summary(m)$adj.r.squared, oos(S, if (nm == "core spec") B0 else c(B0, ADDV), from), ct["delta_oadr_10", 1])) }
  cat(sprintf("wage_infl: b = %.4f, HAC t = %.2f (asymptotic p %.3f); classical F(1,%d) = %.2f (p %.3f); dAIC = %.2f, dAICc = %.2f\n", ct1[ADDV, 1], tobs, ct1[ADDV, 4], m1$df.residual, Fobs, anova(m0, m1)$`Pr(>F)`[2], dA, aicc(m1) - aicc(m0)))
  cat("\n--- B. Recursive residual bootstrap under H0 (no wage inflation), R = 1999 ---\n")
  for (wild in c(FALSE, TRUE)) { bs <- boot_stats(dd, m0, B0, c(B0, ADDV), 1999, wild = wild)
    cat(sprintf("%s bootstrap: p(|t| >= %.2f) = %.3f; p(F >= %.2f) = %.3f; p(dAIC <= %.2f) = %.3f; bootstrap 5%% critical |t| = %.2f, F = %.2f, dAIC = %.2f\n", if (wild) "wild (Rademacher)" else "i.i.d. residual", abs(tobs), mean(abs(bs[, "t"]) >= abs(tobs)), Fobs, mean(bs[, "F"] >= Fobs), dA, mean(bs[, "dAIC"] <= dA),
      quantile(abs(bs[, "t"]), .95), quantile(bs[, "F"], .95), quantile(bs[, "dAIC"], .05))) }
  cat("\n--- C. Data-snooping check: best AIC improvement across all candidates under H0, R = 499 ---\n")
  cc <- CAND[vapply(CAND, function(v) v %in% names(S) && all(!is.na(S[[v]][S$year %in% dd$year])) && sd(S[[v]][S$year %in% dd$year]) > 0, logical(1))]; cc <- setdiff(cc, B0)
  Sd <- S[S$year %in% dd$year, ]; obs <- sapply(cc, function(v) AIC(lm(reformulate(c(B0, v), "mig_gross"), data = Sd)) - AIC(m0))
  cat(sprintf("%d full-sample candidates screened; observed best dAIC = %.2f (%s); wage_infl dAIC = %.2f (rank %d)\n", length(cc), min(obs), sub("^L_", "", names(obs)[which.min(obs)]), obs[ADDV], rank(obs)[ADDV]))
  obs_t <- sapply(cc, function(v) abs(hac_t(lm(reformulate(c(B0, v), "mig_gross"), data = Sd), v)))
  cat(sprintf("observed max |HAC t| across candidates = %.2f (%s); wage_infl |t| = %.2f (rank %d)\n", max(obs_t), sub("^L_", "", names(obs_t)[which.max(obs_t)]), obs_t[ADDV], rank(-obs_t)[ADDV]))
  set.seed(20260929); e0 <- residuals(m0) - mean(residuals(m0)); n <- nrow(dd); best <- numeric(499); maxt <- numeric(499)
  for (r in 1:499) { e <- sample(e0, n, replace = TRUE); db <- Sd; db$mig_gross <- gen_y(dd, m0, e); db$L_mig_gross <- c(dd$L_mig_gross[1], db$mig_gross[-n]); a0 <- AIC(lm(reformulate(B0, "mig_gross"), data = db))
    fits <- lapply(cc, function(v) lm(reformulate(c(B0, v), "mig_gross"), data = db)); best[r] <- min(sapply(fits, AIC) - a0); maxt[r] <- max(mapply(function(m, v) abs(hac_t(m, v)), fits, cc)) }
  cat(sprintf("Under H0 the best of %d candidates improves AIC by at least %.2f in 5%% of samples (median best %.2f); p(best dAIC <= wage_infl's %.2f) = %.3f; p(best dAIC <= observed best %.2f) = %.3f\n", length(cc), quantile(best, .05), median(best), obs[ADDV], mean(best <= obs[ADDV]), min(obs), mean(best <= min(obs))))
  cat(sprintf("Max |HAC t| under H0: 95%% quantile %.2f (median %.2f); p(max |t| >= wage_infl's %.2f) = %.3f -- the reality-check p-value for wage inflation's t statistic after screening\n", quantile(maxt, .95), median(maxt), obs_t[ADDV], mean(maxt >= obs_t[ADDV])))
  cat("\n--- D. Bootstrap RESET (squares), R = 999 ---\n")
  boot_reset <- function(m, R = 999) { X <- model.matrix(m); y0 <- fitted(m); e <- residuals(m) - mean(residuals(m)); f0 <- resettest(m, power = 2)$statistic; set.seed(20260802)
    fs <- replicate(R, { yb <- y0 + sample(e, replace = TRUE); mb <- lm(yb ~ X - 1); resettest(mb, power = 2)$statistic }); mean(fs >= f0) }
  cat(sprintf("core spec: RESET(2) asymptotic p %.3f, bootstrap p %.3f, RESET(2,3) p %.3f, BG(1) %.3f, BG(2) %.3f | +wage_infl: RESET(2) p %.3f, bootstrap p %.3f, RESET(2,3) p %.3f, BG(1) %.3f, BG(2) %.3f\n",
    resettest(m0, power = 2)$p.value, boot_reset(m0), resettest(m0)$p.value, bgtest(m0, 1)$p.value, bgtest(m0, 2)$p.value, resettest(m1, power = 2)$p.value, boot_reset(m1), resettest(m1)$p.value, bgtest(m1, 1)$p.value, bgtest(m1, 2)$p.value)) }
