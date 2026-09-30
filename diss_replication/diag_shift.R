# =============================================================================
# diag_shift.R (Sam, 30 Sep 2026): the current and the shifted timing compared on a full diagnostic battery, each with the base-year
# projection and with the publication-dated projection, on common observations.
#   A      migration_t ~ migration_{t-1} + delta_oadr_10_t     + x_{t-1}      (current)
#   B      migration_t ~ migration_{t-1} + delta_oadr_10_{t-1} + x_{t-1}      (Sam's shift)
#   A pub  as A with the projection dated by publication (the vintage published by year t)
#   B pub  as B with the publication-dated projection one year earlier
#   x = tax_workpop, ln_debt_gdp, ln_vac_unem, int_tme (level).
# The publication-dated series (p_pub_ext) is missing for 1976-77 (the 1975 vintage does not reach ten years ahead), so the pub
# columns lose those years; a second block repeats A and B on exactly the pub columns' years so every comparison is like with like.
# Rainbow test (Utts, 1982): does the model fit the central half of the observations significantly better than all of them? It is
# run in time order (the default: centre = mid-sample years), ordered by fitted values (the functional-form reading) and at wider
# central fractions. RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); suppressWarnings({ library(tseries); library(strucchange) })
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
X <- c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_int_tme")
battery <- function(m) { dd <- attr(m, "dd"); f <- formula(m); r <- residuals(m); n <- nobs(m)
  oos <- function(from) { e <- c(); for (t in from:max(dd$year)) { tr <- dd[dd$year < t, ]; te <- dd[dd$year == t, ]; if (!nrow(te)) next; mm <- lm(f, data = tr); e <- c(e, te[[OUTC]] - predict(mm, te)) }; sqrt(mean(e^2)) }
  c("RESET (2,3)" = resettest(m)$p.value, "RESET (2)" = resettest(m, power = 2)$p.value, "BG (1)" = bgtest(m, 1)$p.value, "BG (2)" = bgtest(m, 2)$p.value, "BG (3)" = bgtest(m, 3)$p.value,
    "Ljung-Box (4)" = Box.test(r, 4, "Ljung-Box")$p.value, "rainbow, time order, 50%" = raintest(m)$p.value, "rainbow, time order, 70%" = raintest(m, fraction = 0.7)$p.value,
    "rainbow, by fitted, 50%" = raintest(m, order.by = ~ fitted(m), data = dd)$p.value, "Harvey-Collier" = tryCatch(harvtest(m)$p.value, error = function(e) NA),
    "Breusch-Pagan" = bptest(m)$p.value, "White (squares)" = bptest(m, as.formula(paste("~", paste(sprintf("%s + I(%s^2)", attr(terms(m), "term.labels"), attr(terms(m), "term.labels")), collapse = " + "))), data = dd)$p.value,
    "Jarque-Bera" = tseries::jarque.bera.test(r)$p.value, "OLS-CUSUM" = sctest(efp(f, data = dd, type = "OLS-CUSUM"))$p.value, "Rec-CUSUM" = sctest(efp(f, data = dd, type = "Rec-CUSUM"))$p.value,
    "sup-F break" = suppressWarnings(sctest(Fstats(f, data = dd, from = 0.2))$p.value), "adj. R2" = summary(m)$adj.r.squared, "AIC" = AIC(m), "BIC" = BIC(m), "OOS RMSE from 1995" = oos(1995), "N" = n) }
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$L_delta_oadr_10 <- lag1(S$delta_oadr_10); S$L_p_pub_ext <- lag1(S$p_pub_ext)
  cat(sprintf("   correlations: projection t with t-1 %.3f; base-year t-1 with publication-dated t %.3f; publication-dated t with t-1 %.3f\n", cor(S$delta_oadr_10, S$L_delta_oadr_10, use = "pair"), cor(S$L_delta_oadr_10, S$p_pub_ext, use = "pair"), cor(S$p_pub_ext, S$L_p_pub_ext, use = "pair")))
  P <- c("A" = "delta_oadr_10", "B (shift)" = "L_delta_oadr_10", "A pub" = "p_pub_ext", "B pub (shift)" = "L_p_pub_ext")
  for (blk in 1:2) { need <- if (blk == 1) P[1:2] else P; yrs <- S$year[complete.cases(S[, c(OUTC, X, need)])]; SS <- S[S$year %in% yrs, ]
    mods <- lapply(need, function(p) fit(SS, c(p, X), OUTC)); names(mods) <- names(need)
    cat(sprintf("\n   Block %d: %s, %d observations (%s)\n", blk, if (blk == 1) "base-year projection" else "base-year and publication-dated projections on the publication-dated years", length(yrs), paste(range(yrs), collapse = "-")))
    cf <- sapply(mods, function(m) { ct <- coeftest(m, vcov. = vcovHAC(m)); sprintf("%.4f (p %s)", ct[2, 1], pf2(ct[2, 4])) }); cat(sprintf("   %-26s %s\n", "projection coefficient", paste(sprintf("%18s", cf), collapse = "")))
    lr <- sapply(mods, function(m) sprintf("%.4f", coef(m)[2] / (1 - coef(m)["L_mig_gross"]))); cat(sprintf("   %-26s %s\n", "long-run effect", paste(sprintf("%18s", lr), collapse = "")))
    B <- sapply(mods, battery); rn <- rownames(B)
    for (i in seq_along(rn)) { v <- B[i, ]; fmt <- if (rn[i] %in% c("AIC", "BIC")) sprintf("%.2f", v) else if (rn[i] == "N") sprintf("%d", as.integer(v)) else if (rn[i] %in% c("adj. R2", "OOS RMSE from 1995")) sprintf("%.4f", v) else vapply(v, pf2, "")
      cat(sprintf("   %-26s %s\n", rn[i], paste(sprintf("%18s", fmt), collapse = ""))) }
    pv <- B[!rownames(B) %in% c("adj. R2", "AIC", "BIC", "OOS RMSE from 1995", "N"), , drop = FALSE]
    cat(sprintf("   %-26s %s\n", "tests rejecting at 5%", paste(sprintf("%18s", colSums(pv < 0.05, na.rm = TRUE)), collapse = "")))
    cat(sprintf("   %-26s %s\n", "tests rejecting at 10%", paste(sprintf("%18s", colSums(pv < 0.10, na.rm = TRUE)), collapse = "")))
    if (blk == 1) { m <- mods[[2]]; dd <- attr(m, "dd"); k <- floor(0.25 * nrow(dd)); r <- residuals(m); o <- order(dd$year)
      cat(sprintf("   B (shift) residual SD by period: first quarter (%d-%d) %.3f | central half (%d-%d) %.3f | last quarter (%d-%d) %.3f; A: %.3f | %.3f | %.3f\n",
                  dd$year[o][1], dd$year[o][k], sd(r[o][1:k]), dd$year[o][k + 1], dd$year[o][nrow(dd) - k], sd(r[o][(k + 1):(nrow(dd) - k)]), dd$year[o][nrow(dd) - k + 1], max(dd$year), sd(r[o][(nrow(dd) - k + 1):nrow(dd)]),
                  sd(residuals(mods[[1]])[o][1:k]), sd(residuals(mods[[1]])[o][(k + 1):(nrow(dd) - k)]), sd(residuals(mods[[1]])[o][(nrow(dd) - k + 1):nrow(dd)]))) } } }
# Where is the break? Andrews sup-F date and the projection coefficient before/after it (post x projection interaction, HAC).
cat("\n\n==================== Break location and the projection after 2008 ====================\n")
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); S$L_delta_oadr_10 <- lag1(S$delta_oadr_10)
  for (p in c("delta_oadr_10", "L_delta_oadr_10")) { m <- fit(S, c(p, X), OUTC); dd <- attr(m, "dd"); fs <- suppressWarnings(Fstats(formula(m), data = dd, from = 0.2))
    dd$post09 <- as.numeric(dd$year >= 2009); dd$post_x_proj <- dd$post09 * dd[[p]]; mi <- lm(as.formula(paste(OUTC, "~", paste(c(p, X, "post09", "post_x_proj"), collapse = " + "))), data = dd)
    ct <- coeftest(mi, vcov. = vcovHAC(mi))
    cat(sprintf("   %-26s %-16s sup-F p %s, break after %d | projection to 2008 %.4f; change from 2009 %.4f (p %s) -> %.4f\n", LABS[[s]], p, pf2(sctest(fs)$p.value), dd$year[fs$breakpoint],
                ct[p, 1], ct["post_x_proj", 1], pf2(ct["post_x_proj", 4]), ct[p, 1] + ct["post_x_proj", 1])) } }
cat("\nDone.\n")
