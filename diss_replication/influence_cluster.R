# =============================================================================
# influence_cluster.R -- two standard checks not yet run (Sam, 1 Oct 2026): (1) influence -- leave-one-year-out (jackknife) range of the
# projection coefficient, the largest DFBETA, and leaving out documented episodes; (2) inference that allows the errors of years sharing a
# projection vintage to be correlated -- wild cluster bootstrap (restricted, Rademacher, 9,999 draws; Cameron, Gelbach and Miller, 2008) with
# clusters = two-year blocks (the NPP vintages are roughly biennial), against the HAC p-value.
# Models: the 6-parameter core (tax per WAP) and the clean one-term core (real taxes + debt interest excl. banks per WAP; pending Sam's decision).
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
invisible(capture.output(source("fiscal_ratio.R"))); set.seed(20261001)
CORE6 <- setdiff(CORE_A, "L_int_tme"); MODS <- list("6-parameter core (tax per WAP)" = CORE6, "clean one-term core (taxrint_xb)" = CB)
EPIS <- list("1972-73 Ugandan Asians" = 1972:1973, "1979-83 first Thatcher government" = 1979:1983, "1997-98 Hong Kong handover" = 1997:1998,
             "2002-07 pre-crisis boom" = 2002:2007, "2008-10 crisis, PBS, cap" = 2008:2010, "2016-19 after the referendum" = 2016:2019)
wcb <- function(d, xs, B = 9999) { d <- d[complete.cases(d[, c("mig_gross", xs)]), ]; cl <- floor((d$year - min(d$year)) / 2); G <- length(unique(cl))
  X <- cbind(1, as.matrix(d[, xs])); y <- d$mig_gross; k <- which(colnames(X) == "delta_oadr_10")
  tstat <- function(yy) { b <- solve(crossprod(X), crossprod(X, yy)); e <- drop(yy - X %*% b); XtXi <- solve(crossprod(X))
    meat <- Reduce(`+`, lapply(split(seq_along(e), cl), function(i) { s <- crossprod(X[i, , drop = FALSE], e[i]); s %*% t(s) }))
    n <- nrow(X); p <- ncol(X); V <- G / (G - 1) * (n - 1) / (n - p) * XtXi %*% meat %*% XtXi; b[k] / sqrt(V[k, k]) }
  t0 <- tstat(y); Xr <- X[, -k]; br <- solve(crossprod(Xr), crossprod(Xr, y)); fr <- drop(Xr %*% br); ur <- y - fr; idx <- match(cl, sort(unique(cl)))
  tb <- replicate(B, { w <- sample(c(-1, 1), G, replace = TRUE)[idx]; tstat(fr + ur * w) }); c(t = t0, p = mean(abs(tb) >= abs(t0)), G = G) }
for (nm in names(MODS)) for (y0 in c(1965, 1977)) { xs <- MODS[[nm]]; d <- subset(S, year >= y0); d <- d[complete.cases(d[, c("mig_gross", xs)]), ]
  m <- fit(d, xs, "mig_gross"); b0 <- coef(m)[["delta_oadr_10"]]
  jk <- vapply(d$year, function(yy) coef(fit(d[d$year != yy, ], xs, "mig_gross"))[["delta_oadr_10"]], 0)
  pj <- vapply(d$year, function(yy) { mm <- fit(d[d$year != yy, ], xs, "mig_gross"); coeftest(mm, vcov. = vcovHAC(mm))["delta_oadr_10", 4] }, 0)
  dfb <- dfbetas(m)[, "delta_oadr_10"]
  cat(sprintf("\n==================== %s, %d-2019 (N %d) ====================\n", nm, y0, nobs(m)))
  cat(sprintf("   projection %.4f (HAC p %s)\n", b0, pf2(coeftest(m, vcov. = vcovHAC(m))["delta_oadr_10", 4])))
  cat(sprintf("   leave-one-year-out: projection %.4f to %.4f (dropping %d / %d); largest HAC p %s (dropping %d)\n", min(jk), max(jk), d$year[which.min(jk)], d$year[which.max(jk)], pf2(max(pj)), d$year[which.max(pj)]))
  cat(sprintf("   largest |DFBETA| %.2f (year %d; rule of thumb 2/sqrt(N) = %.2f); years above it: %s\n", max(abs(dfb)), d$year[which.max(abs(dfb))], 2 / sqrt(nobs(m)),
              paste(d$year[abs(dfb) > 2 / sqrt(nobs(m))], collapse = " ")))
  for (e in names(EPIS)) { dd <- d[!d$year %in% EPIS[[e]], ]; if (nrow(dd) == nrow(d)) next; mm <- fit(dd, xs, "mig_gross")
    cat(sprintf("   without %-34s projection %.4f (HAC p %s), N %d\n", e, coef(mm)[["delta_oadr_10"]], pf2(coeftest(mm, vcov. = vcovHAC(mm))["delta_oadr_10", 4]), nobs(mm))) }
  w <- wcb(d, xs); cat(sprintf("   wild cluster bootstrap by two-year blocks (%d clusters): t %.2f, p %s\n", w[["G"]], w[["t"]], pf2(w[["p"]]))) }
cat("\nDone.\n")
