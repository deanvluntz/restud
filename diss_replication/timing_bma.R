# =============================================================================
# timing_bma.R (Sam, 30 Sep 2026): (1) timing of the model; (2) Bayesian model averaging in the manner of Mitchell, Pain and Riley
# (2011, Economic Journal), who follow the Bayesian Averaging of Classical Estimates of Sala-i-Martin, Doppelhofer and Miller (2004).
# Core specification (debt-interest share in level): mig_gross_t ~ mig_gross_{t-1} + delta_oadr_10_t + x_{t-1},
#   x = tax_workpop, ln_debt_gdp, ln_vac_unem, int_tme.
# 1  Timing, on the same observations:
#    A  current:                  projection at t, economic terms at t-1
#    B  shifted (Sam):            migration one year after BOTH the projection and the economic terms, i.e. projection at t-1 as well
#    C  contemporaneous economics: projection at t, economic terms at t
#    D  ARDL(1,1) in the form of Mitchell, Pain and Riley (2011, eq. 2) and Hatton (2005): levels at t-1 plus current-year changes
#       of every regressor; nests A, B and C. Wald tests of A (changes of the economic terms zero, change of the projection equal to
#       its level coefficient) and B (all current-year changes zero).
#    E  Hatton (2005) labour-market timing: the current-year change in unemployment beside the core specification.
# 2  BMA over the model space below, for each sample. Posterior model probabilities from BIC (their eq. 1); prior inclusion
#    probability kbar / K for every regressor (kbar = 7 as in the paper; kbar = K / 2, i.e. equal model probabilities, as a check);
#    the lagged outcome and the constant are in every model (the ARDL dynamics). Every model is enumerated exactly (2^K models).
#    Reported: posterior inclusion probability (PIP; 'robust' if above the prior kbar / K), unconditional and conditional posterior
#    means, conditional posterior SD, probability of a positive sign, and the ten most probable models.
#    Candidates (one proxy per concept, as Mitchell, Pain and Riley advise, footnote 28): the projection at t and at t-1 (the
#    timing question; their combined PIP is also reported); tax per WAP; log change in debt / GDP; debt interest / TME; ONE labour-
#    market level (run a: log V/U; run b: ILO unemployment); the current-year change in unemployment (Hatton); wage inflation;
#    GDP-deflator inflation; T-bill rate; PSNB / GDP; real GDP growth; terms of trade; real exchange rate; current account; UK income
#    relative to rich hosts and to developing origins, and the origin youth share (supply-side drivers). All t-1 unless stated.
# RStudio: working directory = diss_replication/, then source() this file. Runs in about a minute per sample.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); library(ARDL); EXACT <- FALSE
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
D <- c(L_int_tme = "public sector debt interest, % of total managed expenditure, t-1", int_tme = "debt interest, % of TME, current year",
       L_delta_oadr_10 = "delta_oadr_10, t-1 (the projection one year earlier)", d_delta_oadr_10 = "current-year change in delta_oadr_10",
       tax_workpop = "tax per working-age person, current year", ln_debt_gdp = "log change in net debt / GDP, current year", ln_vac_unem = "log V/U, current year",
       d_tax_workpop = "current-year change in tax_workpop", d_ln_debt_gdp = "current-year change in ln_debt_gdp", d_ln_vac_unem = "current-year change in log V/U",
       d_int_tme = "current-year change in int_tme", d_unem = "current-year change in the ILO unemployment rate (Hatton, 2005)")
for (k in names(D)) DEFS[[k]] <- D[[k]]
X4 <- c("tax_workpop", "ln_debt_gdp", "ln_vac_unem", "int_tme")
CAND0 <- c("delta_oadr_10", "L_delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_int_tme", "d_unem", "L_wage_infl", "L_inflation",
           "L_tbill", "L_psnb_gdp", "L_growth", "L_tot", "L_real_eri", "L_ca_gdp", "L_relinc_rich", "L_relinc_dev", "L_youth")
RUNS <- list("a (labour level = log V/U)" = "L_ln_vac_unem", "b (labour level = unemployment)" = "L_unem")
lab <- function(v) ifelse(v == "delta_oadr_10", "proj (t)", ifelse(v == "L_delta_oadr_10", "proj (t-1)", sub("^L_", "", v)))
bace <- function(S, cand, kbar, always = "L_mig_gross", dv = OUTC) {
  dd <- S[complete.cases(S[, c(dv, always, cand)]), ]; y <- dd[[dv]]; n <- length(y); K <- length(cand); Z0 <- cbind(1, as.matrix(dd[, always, drop = FALSE])); XX <- as.matrix(dd[, cand])
  M <- 2^K; ll <- numeric(M); kk <- integer(M); B <- matrix(0, M, K); V <- matrix(0, M, K); pr <- kbar / K
  for (j in 0:(M - 1)) { inc <- bitwAnd(j, 2^(0:(K - 1))) > 0; Z <- cbind(Z0, XX[, inc, drop = FALSE]); f <- .lm.fit(Z, y); rss <- sum(f$residuals^2); p <- ncol(Z)
    ll[j + 1] <- -n / 2 * log(rss / n); kk[j + 1] <- sum(inc)
    if (any(inc)) { R <- qr.R(qr(Z)); ci <- chol2inv(R); s2 <- rss / (n - p); idx <- (ncol(Z0) + 1):p; B[j + 1, inc] <- f$coefficients[idx]; V[j + 1, inc] <- s2 * diag(ci)[idx] } }
  bic <- ll - (kk + ncol(Z0)) / 2 * log(n); lp <- bic + kk * log(pr) + (K - kk) * log(1 - pr); w <- exp(lp - max(lp)); w <- w / sum(w)
  inc <- sapply(0:(K - 1), function(b) bitwAnd(0:(M - 1), 2^b) > 0); pip <- colSums(w * inc); um <- colSums(w * B); cm <- um / pip
  cv <- colSums(w * inc * (V + B^2)) / pip - cm^2; psign <- colSums(w * inc * (B > 0)) / pip
  top <- order(-w)[1:10]; mods <- sapply(top, function(j) paste(lab(cand[inc[j, ]]), collapse = " + "))
  pj <- which(cand %in% c("delta_oadr_10", "L_delta_oadr_10")); anyp <- sum(w[rowSums(inc[, pj, drop = FALSE]) > 0])
  list(n = n, K = K, prior = pr, anyp = anyp, pmode = mods[1], tab = data.frame(term = lab(cand), PIP = pip, robust = ifelse(pip > pr, "yes", ""), uncond_mean = um, cond_mean = cm, cond_sd = sqrt(pmax(cv, 0)), P_positive = psign),
       top = data.frame(prob = w[top], size = kk[top], model = mods)) }
BM <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$ln_vac_unem <- log(S$vac_unem); S$L_delta_oadr_10 <- lag1(S$delta_oadr_10); S$d_delta_oadr_10 <- c(NA, diff(S$delta_oadr_10)); S$d_unem <- c(NA, diff(S$unem))
  for (v in X4) S[[paste0("d_", v)]] <- c(NA, diff(S[[v]]))
  F <- function(xs, d = S) fit(d, xs, OUTC)
  A <- c("L_mig_gross", "delta_oadr_10", paste0("L_", X4)); B_ <- c("L_mig_gross", "L_delta_oadr_10", paste0("L_", X4)); C_ <- c("L_mig_gross", "delta_oadr_10", X4)
  Dx <- c("L_mig_gross", "L_delta_oadr_10", paste0("L_", X4), "d_delta_oadr_10", paste0("d_", X4)); E_ <- c(A, "d_unem")
  mD <- F(Dx); yrs <- attr(mD, "dd")$year; SS <- S[S$year %in% yrs, ]
  mods <- list("A current" = F(A, SS), "B shifted (Sam)" = F(B_, SS), "C contemporaneous" = F(C_, SS), "D ARDL(1,1)" = mD, "E +d_unem (Hatton)" = F(E_, SS))
  panel(mods, sprintf("Table 1. Timing of the regressors, same observations (%d-%d), %s", min(yrs), max(yrs), L),
        c("delta_oadr_10", "L_delta_oadr_10", "d_delta_oadr_10", paste0("L_", X4), X4, paste0("d_", X4), "d_unem", "L_mig_gross"), w = 20,
        notes = c("A: projection at t, economic terms at t-1 (the core specification). B: every regressor one year before migration (Sam's shift: migration in the year after the projection and the outturns). C: economic terms in the same year as migration.",
                  "D: ARDL(1,1) in levels at t-1 plus current-year changes (Mitchell, Pain and Riley, 2011, eq. 2; the long-run projection effect is the L_delta_oadr_10 coefficient over 1 - L_mig_gross). E: Hatton's (2005) current-year change in unemployment added to A."))
  wA <- linearHypothesis(mD, c(paste0("d_", X4, " = 0"), "d_delta_oadr_10 - L_delta_oadr_10 = 0"), vcov. = vcovHAC(mD)); wB <- linearHypothesis(mD, c(paste0("d_", X4, " = 0"), "d_delta_oadr_10 = 0"), vcov. = vcovHAC(mD))
  wC <- linearHypothesis(mD, c(paste0("L_", X4, " - d_", X4, " = 0"), "d_delta_oadr_10 - L_delta_oadr_10 = 0"), vcov. = vcovHAC(mD))
  cat(sprintf("   restrictions on the ARDL(1,1) (HAC Wald): A current timing F %.2f p %s | B shifted timing F %.2f p %s | C contemporaneous timing F %.2f p %s\n", wA$F[2], pf2(wA$`Pr(>F)`[2]), wB$F[2], pf2(wB$`Pr(>F)`[2]), wC$F[2], pf2(wC$`Pr(>F)`[2])))
  ctD <- coeftest(mD, vcov. = vcovHAC(mD)); lr <- ctD["L_delta_oadr_10", 1] / (1 - ctD["L_mig_gross", 1]); cat(sprintf("   ARDL(1,1) long-run projection effect %.4f (A: %.4f, B: %.4f)\n", lr, coef(mods[[1]])["delta_oadr_10"] / (1 - coef(mods[[1]])["L_mig_gross"]), coef(mods[[2]])["L_delta_oadr_10"] / (1 - coef(mods[[2]])["L_mig_gross"])))
  for (nm in c("A current", "B shifted (Sam)")) { m <- mods[[nm]]; cat(sprintf("   %s: RESET %s, BG(1) %s, BG(2) %s, rainbow %s\n", nm, pf2(resettest(m)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(raintest(m)$p.value))) }
  S$L_ln_vac_unem <- lag1(S$ln_vac_unem); S$ln_delta_oadr_10 <- NULL
  S2 <- S; S2$L_delta_oadr_10 <- lag1(S2$delta_oadr_10); S2$L2_mig_gross <- lag1(S2$L_mig_gross)
  S2$ln_vac_unem_b <- S2$ln_vac_unem; S2$L_delta_oadr_10b <- S2$L_delta_oadr_10
  mB <- F(B_, S2); b <- tryCatch({ sh <- S2; sh$delta_oadr_10 <- sh$L_delta_oadr_10; vv <- c(OUTC, "delta_oadr_10", X4); sh$delta_oadr_10 <- c(NA, head(S2$delta_oadr_10, -1)); ok <- complete.cases(sh[, vv])
    ts_df <- ts(sh[ok, vv], start = min(sh$year[ok])); m_ardl <- ardl(as.formula(paste(OUTC, "~", paste(vv[-1], collapse = " + "))), data = ts_df, order = rep(1, length(vv)))
    list(F = bounds_f_test(m_ardl, case = 3, exact = EXACT), t = bounds_t_test(m_ardl, case = 3, exact = EXACT), lr = multipliers(m_ardl, type = "lr")) }, error = function(e) NULL)
  if (!is.null(b)) { r <- b$lr[b$lr$Term == "delta_oadr_10", ]; cat(sprintf("   B shifted, bounds test (ARDL(1,...,1), case 3, asymptotic): F %.2f (p %s), t %.2f (p %s); long-run projection %.4f [%.4f, %.4f]\n", b$F$statistic, pf2(b$F$p.value), b$t$statistic, pf2(b$t$p.value), r$Estimate, r$Estimate - 1.96 * r$`Std. Error`, r$Estimate + 1.96 * r$`Std. Error`)) }
  # ---- 2 BMA ----
  for (rn in names(RUNS)) { cand <- c(CAND0[1:4], RUNS[[rn]], CAND0[-(1:4)]); cand <- cand[cand %in% names(S)]
    cov_ok <- vapply(cand, function(v) sum(!is.na(S[[v]][S$year >= Y0 & S$year <= 2019])) >= sum(S$year >= Y0 & S$year <= 2019) - 1, logical(1))
    if (any(!cov_ok)) cat(sprintf("\n   BMA: dropped for coverage: %s\n", paste(cand[!cov_ok], collapse = ", "))); cand <- cand[cov_ok]
    for (kb in c(7, length(cand) / 2)) { t0 <- Sys.time(); r <- bace(S, cand, kb)
      cat(sprintf("\n2%s. BMA (BACE), %s: N %d, K %d candidates (2^%d = %s models), prior mean model size %.1f (prior inclusion probability %.2f); %.0f s\n", substr(rn, 1, 1), L, r$n, r$K, r$K, format(2^r$K, big.mark = ","), kb, r$prior, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      cat(sprintf("   run %s; PIP of the projection at either timing: %.3f\n", rn, r$anyp))
      tb <- r$tab[order(-r$tab$PIP), ]; print(tb, row.names = FALSE, digits = 3); cat("   five most probable models (lagged outcome and constant in all):\n"); print(head(r$top, 5), row.names = FALSE, digits = 3)
      tb$sample <- s; tb$run <- substr(rn, 1, 1); tb$kbar <- kb; BM[[paste(s, rn, kb)]] <- tb } } }
ALLB <- do.call(rbind, BM); write.csv(ALLB, file.path(tempdir(), "bma_results.csv"), row.names = FALSE)
cat("\n\n==================== BMA summary: PIP by sample, labour proxy and prior (kbar = 7) ====================\n")
k7 <- ALLB[ALLB$kbar == 7, ]; k7$key <- paste0(ifelse(k7$sample == "1964", "Citizen", "Origin"), "_", k7$run); terms <- unique(k7$term)
W <- data.frame(term = terms); for (key in unique(k7$key)) W[[key]] <- round(k7$PIP[k7$key == key][match(terms, k7$term[k7$key == key])], 2)
W <- W[order(-rowMeans(W[, -1], na.rm = TRUE)), ]; print(W, row.names = FALSE); cat("(run a: labour level = log V/U; run b: labour level = unemployment; prior inclusion probability 7/18 = 0.39; results for kbar = K/2 printed above)\n")
cat("\nDone.\n")
