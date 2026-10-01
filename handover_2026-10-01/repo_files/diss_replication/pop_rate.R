# =============================================================================
# pop_rate.R -- the outcome as the headline immigration rate: gross inflows as a percentage of the total population (Sam, 30 Sep 2026).
# Sam: "From a political economy standpoint, that is the headline rate when people discuss immigration, so testing the most salient version
# of the outcome is a necessary contribution."
# Outcome forms (same inflows as mig_gross: non-British citizens to 1974, non-EU citizens from 1975, IPS 3.01; population = mid-year total
# population, the series used in the dataset):
#   pop_pct      100 x inflows_t / population_t          (percent, level -- the headline form)
#   ln_pop_pct   log of the same                         (log form; differs from mig_gross only by log population)
#   pop_pct_l    100 x inflows_t / population_{t-1}      (denominator known at the start of the year; check)
# Each with its own lag, the core right-hand side (projection at t), both periods. Then the two placebos in the same form: the total
# dependency ratio in place of the OADR, and EU citizens' inflows as a % of population (1977-2019; EU flows are uncontrolled). Sam's
# criterion reading: the projection's contribution = the same model without it.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); S <- build_data()[["1964"]]$S
A <- { SAMPLE <- "1964"; source("data_common.R"); subset(df, year <= 2019) }                 # to recover 1964 values for the first lag
A <- A[order(A$year), ]; inflow <- exp(A$mig_gross)
A$pop_pct <- 100 * inflow / A$totpop; A$ln_pop_pct <- log(A$pop_pct); A$pop_pct_l <- 100 * inflow / lag1(A$totpop)
A$eu_pop_pct <- 100 * exp(A$eu_gross) / A$totpop
for (v in c("pop_pct", "ln_pop_pct", "pop_pct_l", "eu_pop_pct")) { A[[paste0("L_", v)]] <- lag1(A[[v]]); S[[v]] <- A[[v]][match(S$year, A$year)]; S[[paste0("L_", v)]] <- A[[paste0("L_", v)]][match(S$year, A$year)] }
cat("Inflows as a percentage of the total population, selected years\n")
print(round(S[S$year %in% c(1965, 1970, 1975, 1980, 1990, 2000, 2004, 2010, 2015, 2019), c("year", "pop_pct", "pop_pct_l", "eu_pop_pct")], 3), row.names = FALSE)
cat(sprintf("Correlation of the headline rate with the main outcome (log persons), 1965-2019: %.3f; with the published per-WAP rate: %.3f\n",
            cor(S$pop_pct, S$mig_gross, use = "pair"), cor(S$ln_pop_pct, S$mig_rate, use = "pair")))
X <- setdiff(CORE_A, "L_mig_gross")
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
for (nm in c("Citizen sample (1965-2019)", "1977-2019")) { d <- if (nm == "1977-2019") subset(S, year >= 1977) else S
  cat(sprintf("\n==================== %s ====================\n", nm))
  for (y in c("mig_gross", "pop_pct", "ln_pop_pct", "pop_pct_l")) { xs <- c(paste0("L_", y), X); m <- fit(d, xs, y); np <- fit(d, setdiff(xs, "delta_oadr_10"), y)
    md <- fit(d, sub("^delta_oadr_10$", "delta_dr_10", xs), y); b <- battery(m, y); lrun <- coef(m)[["delta_oadr_10"]] / (1 - coef(m)[[paste0("L_", y)]])
    cat(sprintf("\n-- outcome %s%s (N %d)\n   %s\n", y, if (y == "mig_gross") " (main: log persons)" else "", nobs(m), coefline(m)))
    cat(sprintf("   projection %.4f (p %s), long run %.4f; mean of outcome %.3f -> projection effect on impact = %.1f%% of the mean per OADR point\n", hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]), lrun,
                mean(attr(m, "dd")[[y]]), if (grepl("ln_|mig_gross", y)) 100 * hc(m, "delta_oadr_10")[1] else 100 * hc(m, "delta_oadr_10")[1] / mean(attr(m, "dd")[[y]])))
    cat(sprintf("   without the projection: AIC %+.2f (the projection lowers AIC by %.2f); total DR in place of the OADR: %.4f (p %s), AIC %+.2f vs OADR\n", AIC(np) - AIC(m), AIC(np) - AIC(m),
                hc(md, "delta_dr_10")[1], pf2(hc(md, "delta_dr_10")[2]), AIC(md) - AIC(m)))
    cat(sprintf("   diagnostics: RESET %s  BG(1) %s  BG(2) %s  rainbow %s  sup-F %s  Jarque-Bera %s  Breusch-Pagan %s  max VIF %.1f  OOS %.3f\n", pf2(b[["RESET (2,3)"]]), pf2(b[["BG (1)"]]), pf2(b[["BG (2)"]]),
                pf2(b[["rainbow, time"]]), pf2(b[["sup-F"]]), pf2(b[["Jarque-Bera"]]), pf2(b[["Breusch-Pagan"]]), b[["max VIF"]], b[["OOS RMSE from 1995"]])) }
  if (nm == "1977-2019") { m <- fit(d, c("L_eu_pop_pct", X), "eu_pop_pct")
    cat(sprintf("\n-- placebo: EU citizens' inflows as %% of population (uncontrolled), N %d\n   %s\n   projection %.4f (p %s); main-outcome projection in the same units above\n", nobs(m), coefline(m),
                hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]))) } }
cat("\nDone.\n")
