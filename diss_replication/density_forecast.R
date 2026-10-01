# =============================================================================
# density_forecast.R -- one-year-ahead density forecasts in the manner of Mitchell, Pain and Riley (2011, EJ, Section 4.4) (Sam, 30 Sep 2026)
# MPR: recursive out-of-sample exercise (2003-07), estimation on data up to T to forecast T+1, Student-t predictive densities (Zellner,
# 1971), evaluated by the RMSE of the point forecast, the average log score of the density (higher = better) and the 95% interval, against
# an AR(1) benchmark; with five forecasts per region they "eschew statistical tests for equal forecast accuracy".
# Here: recursive estimation from the start of the sample; one-year-ahead forecasts for 1990-2019 (30 years) on the Citizen sample (and
# 1995-2019 when estimation starts in 1977). Predictive density: Student-t with the OLS forecast variance s^2 (1 + x0'(X'X)^-1 x0) and n-k
# degrees of freedom. Pseudo out of sample only: today's data vintages, and the projection at t (timing A) is not strictly information at
# t-1 (a base-year t vintage is published later), so timing B (projection at t-1), which is a pure 'direct' forecast in MPR's sense, is shown.
# Models: tax-only core (6 parameters); clean one-term core (real taxes + public-sector debt interest excl. banks per WAP; fiscal_ratio.R);
# each without the projection; each with the projection at t-1; the previous core with int_tme (reference); AR(1) (MPR's benchmark).
# Evaluation: RMSE; mean log score; coverage of the 90% and 95% intervals (Christoffersen, 1998, test of independent 90% misses);
# calibration of the whole density: Berkowitz (2001) LR test that the probability integral transforms, mapped to normal scores, are iid
# N(0,1) (3 d.f.); and the Amisano-Giacomini (2007) test of equal mean log score (Newey-West t, lag 2) against the same model WITHOUT the
# projection -- the out-of-sample version of "does the projection add information?". Also the mean log score over MPR's window, 2003-07.
# RStudio: working directory = diss_replication/, then source() this file (after fiscal_ratio.R has been sourced, or it is sourced here).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
if (!exists("CB") || !"L_taxrint_xb" %in% names(S)) source("fiscal_ratio.R")
CORE6 <- setdiff(CORE_A, "L_int_tme"); pB <- function(xs) sub("^delta_oadr_10$", "L_delta_oadr_10", xs)
MODELS <- list("tax core (6)" = CORE6, "  without projection" = setdiff(CORE6, "delta_oadr_10"), "  projection at t-1" = pB(CORE6),
               "one-term core (6)" = CB, "  without projection " = setdiff(CB, "delta_oadr_10"), "  projection at t-1 " = pB(CB),
               "previous core, int_tme (7)" = CORE_A, "AR(1)" = "L_mig_gross")
dens <- function(d, xs, from) { d <- d[complete.cases(d[, c("mig_gross", xs)]), ]; out <- NULL
  for (t in from:max(d$year)) { tr <- d[d$year < t, ]; te <- d[d$year == t, ]; if (!nrow(te)) next
    m <- lm(reformulate(xs, "mig_gross"), data = tr); X <- model.matrix(m); x0 <- model.matrix(delete.response(terms(m)), te)
    s2 <- sum(residuals(m)^2) / df.residual(m); sd <- sqrt(s2 * (1 + drop(x0 %*% solve(crossprod(X), t(x0))))); mu <- drop(x0 %*% coef(m)); nu <- df.residual(m); z <- (te$mig_gross - mu) / sd
    out <- rbind(out, data.frame(year = t, e = te$mig_gross - mu, z = z, nu = nu, pit = pt(z, nu), ls = dt(z, nu, log = TRUE) - log(sd), h90 = abs(z) > qt(.95, nu), h95 = abs(z) > qt(.975, nu))) }
  out }
berk <- function(pit) { x <- qnorm(pmin(pmax(pit, 1e-6), 1 - 1e-6)); y <- x[-1]; x1 <- x[-length(x)]; m <- lm(y ~ x1); s2 <- mean(residuals(m)^2); n <- length(y)
  l1 <- sum(dnorm(y, fitted(m), sqrt(s2), log = TRUE)); l0 <- sum(dnorm(y, 0, 1, log = TRUE)); lr <- 2 * (l1 - l0); c(lr = lr, p = pchisq(lr, 3, lower.tail = FALSE), mean = mean(x), sd = sd(x), rho = unname(coef(m)[2])) }
chris <- function(h) { h <- as.numeric(h); a <- h[-length(h)]; b <- h[-1]; n00 <- sum(a == 0 & b == 0); n01 <- sum(a == 0 & b == 1); n10 <- sum(a == 1 & b == 0); n11 <- sum(a == 1 & b == 1)
  if (n01 + n11 == 0 || n10 + n11 == 0) return(NA); p01 <- n01 / (n00 + n01); p11 <- n11 / (n10 + n11); p <- (n01 + n11) / length(b)
  ll <- function(q, k0, k1) ifelse(q %in% c(0, 1), 0, k0 * log(1 - q) + k1 * log(q)); lr <- 2 * (ll(p01, n00, n01) + ll(p11, n10, n11) - ll(p, n00 + n10, n01 + n11)); pchisq(lr, 1, lower.tail = FALSE) }
ag <- function(d1, d0) { dd <- d1 - d0; m <- lm(dd ~ 1); c(diff = mean(dd), p = 2 * pnorm(-abs(coef(m)[[1]] / sqrt(NeweyWest(m, lag = 2, prewhite = FALSE)[1, 1])))) }
for (run in list(list(nm = "Citizen sample: estimation from 1965, forecasts 1990-2019", from = 1990, d = S), list(nm = "Estimation from 1977, forecasts 1995-2019", from = 1995, d = subset(S, year >= 1977)))) {
  cat(sprintf("\n==================== %s ====================\n", run$nm))
  F <- lapply(MODELS, function(xs) dens(run$d, xs, run$from))
  cat(sprintf("   %-28s %3s %6s %8s %8s %6s %6s %9s %10s %22s %16s\n", "model", "n", "RMSE", "log sc.", "2003-07", "90%", "95%", "Chr. p", "Berk. p", "PIT z: mean / sd / rho", "vs no-proj (p)"))
  for (k in names(MODELS)) { f <- F[[k]]; b <- berk(f$pit)
    base <- if (grepl("^tax core|^  projection at t-1$", k)) "  without projection" else if (grepl("^one-term|^  projection at t-1 $", k)) "  without projection " else NA
    a <- if (!is.na(base)) ag(f$ls, F[[base]]$ls) else c(NA, NA)
    cat(sprintf("   %-28s %3d %6.3f %8.3f %8.3f %6.2f %6.2f %9s %10s %7.2f / %4.2f / %+5.2f %s\n", k, nrow(f), sqrt(mean(f$e^2)), mean(f$ls), mean(f$ls[f$year %in% 2003:2007]),
                1 - mean(f$h90), 1 - mean(f$h95), pf2(chris(f$h90)), pf2(b[["p"]]), b[["mean"]], b[["sd"]], b[["rho"]],
                if (is.na(a[1])) "" else sprintf("%+7.3f (%s)", a[1], pf2(a[2])))) }
  cat("   log sc. = mean log predictive density (higher is better); 90% / 95% = share of outturns inside the interval; Chr. p = Christoffersen\n")
  cat("   independence of 90% misses; Berk. p = Berkowitz LR (PIT normal scores iid N(0,1)); vs no-proj = mean log-score gain over the same model\n")
  cat("   without the projection, Amisano-Giacomini p (Newey-West, lag 2)\n") }
cat("\nDone.\n")
