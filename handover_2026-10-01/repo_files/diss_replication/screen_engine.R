# =============================================================================
# screen_engine.R -- shared functions for main_spec_protocol.R, reverse_build.R and break_citizen.R (30 Sep 2026).
# Sourced by those scripts; does nothing on its own. Names avoid data_common.R's globals (BASE, LAB, STEP, Y0, SIX, ...).
# =============================================================================
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); suppressWarnings({ library(strucchange); library(tseries) })
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
SMP_LABEL <- c("1964" = "Citizen sample (1965-2019)", "1977" = "1977-2019 sub-period")
# forms of the core terms (all t-1) and the pool of other economic indicators
F_TAX  <- c("L_tax_workpop", "L_ln_tax_workpop", "L_tax_voter", "L_tax_res", "L_taxint_wap")
F_LAB  <- c("L_ln_vac_unem", "L_vac_unem", "L_unem", "L_ln_vac_wap", "L_vac_wap", "L_dln_vac_unem")
F_DEBT <- c("L_ln_debt_gdp", "L_psnd_gdp", "L_ln_psnd_gdp", "L_d_psnd_gdp", "L_psnb_gdp")
F_INT  <- c("L_int_tme", "L_ln_int_tme", "L_int_gdp_x", "L_ln_int_gdp_x", "L_int_wap")
POOL_E <- c("L_growth", "L_growth_pc", "L_gap_obr", "L_lab_prod", "L_tfp", "L_wage_infl", "L_real_wage", "L_particip", "L_replace",
            "L_inflation", "L_cpi_infl", "st_inf_exp", "L_real_rate", "L_term_spread", "L_m4_growth", "L_r_g", "L_tme_real_growth",
            "L_tme_obr_gdp", "L_grants_gdp", "L_tax_gdp", "L_fc_budget", "L_real_eri", "L_ca_gdp", "L_trade_bal", "L_tot", "L_openness",
            "L_real_house", "L_unem_dest", "L_unem_diff", "L_relinc_rich", "L_relinc_dev", "L_youth", "L_tbill", "L_bank_rate", "L_gilt")
CORE_A <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_int_tme")   # current core, projection t
.pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); .obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); .obr$gdp_bn <- 100 * .obr$tme_bn / .obr$tme_gdp
.pusf <- merge(.pusf, .obr[, c("year", "gdp_bn")], by = "year"); .pusf$int_gdp_ons <- 100 * .pusf$ANLO / 1000 / .pusf$gdp_bn
.splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, .pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
build_data <- function() { out <- list(); for (s in c("1964", "1977")) { SAMPLE <<- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019)
  S$L_delta_oadr_10 <- lag1(S$delta_oadr_10); S$int_gdp_x <- .splice(S$int_gdp, S$year, .pusf$int_gdp_ons)
  S$L_int_gdp_x <- lag1(S$int_gdp_x); S$L_ln_int_gdp_x <- lag1(log(S$int_gdp_x)); S$L_int_wap <- lag1(S$tax_workpop * S$int_gdp_x / S$tax_gdp)
  S$L_taxint_wap <- lag1(S$tax_workpop * (1 + S$int_gdp_x / S$tax_gdp)); S$L_ln_tax_workpop <- lag1(log(S$tax_workpop))
  S$L_d_psnd_gdp <- lag1(c(NA, diff(S$psnd_gdp))); if (!"L_ln_psnd_gdp" %in% names(S)) S$L_ln_psnd_gdp <- lag1(log(S$psnd_gdp))
  S$res_gross <- log(1000 * S$res_noneu_k); S$L_res_gross <- lag1(S$res_gross); S$cit_gross <- log(1000 * S$cit_noneu_k); S$L_cit_gross <- lag1(S$cit_gross)
  out[[s]] <- list(S = subset(S, year >= Y0), step = STEP) }; out }
qols <- function(M, dv, xs) { cols <- c(dv, xs); ok <- rowSums(is.na(M[, cols, drop = FALSE])) == 0; X <- cbind(1, M[ok, xs, drop = FALSE]); y <- M[ok, dv]
  if (ncol(X) > 1 && any(apply(X[, -1, drop = FALSE], 2, sd) == 0)) return(NULL)
  q <- qr(X); if (q$rank < ncol(X)) return(NULL); b <- qr.coef(q, y); r <- y - X %*% b; n <- length(y); k <- ncol(X); s2 <- sum(r^2) / (n - k)
  se <- sqrt(diag(chol2inv(q$qr[1:k, 1:k, drop = FALSE])) * s2); p <- 2 * pt(-abs(b / se), n - k); names(p) <- c("(Intercept)", xs)
  list(p = p, aic = n * log(sum(r^2) / n) + 2 * k, n = n, yrs = M[ok, "year"]) }
hacp <- function(m) { ct <- coeftest(m, vcov. = vcovHAC(m)); setNames(ct[, 4], rownames(ct)) }
proj_of <- function(xs) intersect(xs, c("delta_oadr_10", "L_delta_oadr_10"))[1]
breaks <- function(m, dv) { d <- attr(m, "dd"); P <- proj_of(names(coef(m))); sf <- suppressWarnings(sctest(Fstats(formula(m), data = d, from = 0.2))$p.value)
  d$post09 <- as.numeric(d$year >= 2009); d$post_x_proj <- d$post09 * d[[P]]; xs <- c(attr(terms(m), "term.labels"), "post09", "post_x_proj")
  mi <- lm(reformulate(xs, dv), data = d); ct <- coeftest(mi, vcov. = vcovHAC(mi)); c(supf = sf, ch = ct["post_x_proj", 1], chp = ct["post_x_proj", 4]) }
# Chow test at a split year: every coefficient (intercept included) allowed to differ from the split. Classical F, and an HC3
# heteroskedasticity-robust Wald (a HAC Wald on 2k parameters with N 55 over-rejects badly, so it is not used); projection change with HC3 p.
chow <- function(m, dv, yr) { d <- attr(m, "dd"); xs <- attr(terms(m), "term.labels"); d$post <- as.numeric(d$year >= yr); P <- proj_of(xs)
  for (v in xs) d[[paste0("post_x_", v)]] <- d$post * d[[v]]; ix <- c("post", paste0("post_x_", xs)); mi <- lm(reformulate(c(xs, ix), dv), data = d)
  if (any(is.na(coef(mi)))) return(c(F = NA, Fp = NA, W = NA, Wp = NA, pch = NA, pchp = NA, npre = sum(d$post == 0)))
  a <- anova(lm(reformulate(xs, dv), data = d), mi); V <- vcovHC(mi, type = "HC3"); w <- tryCatch(linearHypothesis(mi, paste(ix, "= 0"), vcov. = V), error = function(e) NULL)
  ct <- coeftest(mi, vcov. = V); c(F = a$F[2], Fp = a$`Pr(>F)`[2], W = if (is.null(w)) NA else w$F[2], Wp = if (is.null(w)) NA else w$`Pr(>F)`[2],
    pch = ct[paste0("post_x_", P), 1], pchp = ct[paste0("post_x_", P), 4], npre = sum(d$post == 0)) }
oos <- function(m, dv, from = 1995) { dd <- attr(m, "dd"); f <- formula(m); e <- c(); for (t in from:max(dd$year)) { tr <- dd[dd$year < t, ]; te <- dd[dd$year == t, ]
  if (!nrow(te)) next; e <- c(e, te[[dv]] - predict(lm(f, data = tr), te)) }; sqrt(mean(e^2)) }
battery <- function(m, dv) { dd <- attr(m, "dd"); f <- formula(m); r <- residuals(m); b <- breaks(m, dv)
  c("RESET (2,3)" = unname(resettest(m)$p.value), "RESET (2)" = unname(resettest(m, power = 2)$p.value), "BG (1)" = unname(bgtest(m, 1)$p.value), "BG (2)" = unname(bgtest(m, 2)$p.value),
    "BG (3)" = unname(bgtest(m, 3)$p.value), "Ljung-Box (4)" = Box.test(r, 4, "Ljung-Box")$p.value, "rainbow, time" = unname(raintest(m)$p.value),
    "rainbow, by fitted" = unname(raintest(m, order.by = ~ fitted(m), data = dd)$p.value), "Harvey-Collier" = tryCatch(unname(harvtest(m)$p.value), error = function(e) NA),
    "Breusch-Pagan" = unname(bptest(m)$p.value), "Jarque-Bera" = unname(tseries::jarque.bera.test(r)$p.value), "OLS-CUSUM" = unname(sctest(efp(f, data = dd, type = "OLS-CUSUM"))$p.value),
    "Rec-CUSUM" = unname(sctest(efp(f, data = dd, type = "Rec-CUSUM"))$p.value), "sup-F" = unname(b["supf"]), "post-2009 change p" = unname(b["chp"]),
    "max VIF" = if (length(coef(m)) > 2) max(vif(m)) else NA, "adj. R2" = summary(m)$adj.r.squared, "AIC" = AIC(m), "BIC" = BIC(m), "OOS RMSE from 1995" = oos(m, dv), "N" = nobs(m)) }
coefline <- function(m) { ct <- coeftest(m, vcov. = vcovHAC(m)); paste(sprintf("%s %.4f (%s)", sub("^L_", "", rownames(ct)[-1]), ct[-1, 1], vapply(ct[-1, 4], pf2, "")), collapse = "; ") }
# the agreed protocol for one specification (xs includes the lagged outcome and the projection): main estimate on the Citizen sample,
# the same specification on 1977-2019, Chow at 1977, and the residence-based and all-residence citizenship outcomes on 1977-2019
run_protocol <- function(nm, xs, S) { P <- proj_of(xs); cat("\n==================== ", nm, " ====================\n", sep = "")
  m <- fit(S, xs, "mig_gross"); m7 <- fit(subset(S, year >= 1977), xs, "mig_gross")
  cat(sprintf("(1) Main, Citizen sample 1965-2019 (N %d): %s\n", nobs(m), coefline(m)))
  cat(sprintf("(2) Same specification, 1977-2019 (N %d): %s\n", nobs(m7), coefline(m7)))
  B1 <- battery(m, "mig_gross"); B7 <- battery(m7, "mig_gross"); cat(sprintf("    %-22s %12s %12s\n", "diagnostic", "1965-2019", "1977-2019"))
  for (k in names(B1)) { fm <- function(v) if (k %in% c("AIC", "BIC")) sprintf("%.2f", v) else if (k == "N") sprintf("%d", as.integer(v)) else if (k %in% c("adj. R2", "OOS RMSE from 1995", "max VIF")) sprintf("%.3f", v) else pf2(v)
    cat(sprintf("    %-22s %12s %12s\n", k, fm(B1[[k]]), fm(B7[[k]]))) }
  ch <- chow(m, "mig_gross", 1977)
  cat(sprintf("(3) Chow at 1977 (%d years before), all coefficients: classical F %.2f (p %s), HC3 Wald F %.2f (p %s); projection change from 1977 %+.4f (HC3 p %s)\n",
              ch["npre"], ch["F"], pf2(ch["Fp"]), ch["W"], pf2(ch["Wp"]), ch["pch"], pf2(ch["pchp"])))
  for (o in list(c("res_gross", "L_res_gross", "residence-based: all citizenships from non-EU countries"), c("cit_gross", "L_cit_gross", "citizenship: non-EU citizens from any country"))) {
    mo <- fit(subset(S, year >= 1977), c(o[2], setdiff(xs, "L_mig_gross")), o[1]); if (is.null(mo)) next; ct <- coeftest(mo, vcov. = vcovHAC(mo))
    cat(sprintf("(4) %s, 1977-2019 (N %d): projection %.4f (p %s); RESET %s BG(1) %s BG(2) %s\n    %s\n", o[3], nobs(mo), ct[P, 1], pf2(ct[P, 4]),
                pf2(resettest(mo)$p.value), pf2(bgtest(mo, 1)$p.value), pf2(bgtest(mo, 2)$p.value), coefline(mo))) }
  invisible(list(m = m, m7 = m7, B1 = B1)) }
