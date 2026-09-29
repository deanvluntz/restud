# =============================================================================
# tme_predict.R -- Is the spending side predictable from the model's own regressors? (Sam, 29 Sep 2026.)
# 1  autocorrelation of total managed expenditure (OBR databank, fiscal years) as a share of GDP, in 2024-25 prices (log level and
#    growth) and of the debt-interest share, at horizons 1-10
# 2  predicting TME one year ahead from 1-year lags of debt/GDP, unemployment and tax per working-age person, each in any form
#    (level, log, first difference, log difference); three outcomes: growth of real TME, change in TME/GDP, log TME/GDP with its lag.
#    For each outcome and each block (all three variables; unemployment and tax; tax alone) the three best models by AIC, with HAC
#    p-values, adjusted R2 and RESET. Both samples.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)")
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); cat("\n\n==================== ", LABS[[s]], " ====================\n", sep = "")
  # --- 1 autocorrelation ---
  cat("\n1. Autocorrelation of spending measures (lags 1-10)\n")
  ac <- function(x, nm) { x <- x[!is.na(x)]; a <- acf(x, lag.max = 10, plot = FALSE)$acf[-1]; cat(sprintf("   %-34s %s\n", nm, paste(sprintf("%5.2f", a), collapse = " "))) }
  ac(S$tme_obr_gdp, "TME / GDP (level)"); ac(diff(S$tme_obr_gdp), "change in TME / GDP"); ac(log(S$tme_obr_real), "log real TME (level)"); ac(diff(log(S$tme_obr_real)), "growth of real TME"); ac(S$int_tme, "debt interest / TME (level)"); ac(diff(log(S$int_tme)), "change in log debt interest / TME")
  cat("   (lags 1..10 in order; TME from the OBR public sector finances databank, fiscal years mapped to their starting calendar year)\n")
  # --- 2 prediction ---
  S$g_tme <- 100 * c(NA, diff(log(S$tme_obr_real))); S$d_tme_gdp <- c(NA, diff(S$tme_obr_gdp)); S$ln_tme_gdp <- log(S$tme_obr_gdp); S$L_ln_tme_gdp <- lag1(S$ln_tme_gdp)
  forms <- function(x, base) { out <- list(); out[[base]] <- lag1(x); out[[paste0("ln_", base)]] <- lag1(log(x)); out[[paste0("d_", base)]] <- lag1(c(NA, diff(x))); out[[paste0("dln_", base)]] <- lag1(c(NA, diff(log(x)))); out }
  F1 <- forms(S$psnd_gdp, "debt_gdp"); F2 <- forms(S$unem, "unem"); F3 <- forms(S$tax_workpop, "tax_workpop"); for (nm in names(c(F1, F2, F3))) S[[paste0("L_", nm)]] <- c(F1, F2, F3)[[nm]]
  DEFS2 <- c(debt_gdp = "public sector net debt / GDP, %", unem = "ILO unemployment rate, %", tax_workpop = "national accounts taxes per working-age person, GBP 000 at 2024-25 prices")
  run <- function(dv, extra, blocks) { for (bn in names(blocks)) { vars <- blocks[[bn]]; grid <- expand.grid(lapply(vars, function(v) paste0("L_", c("", "ln_", "d_", "dln_"), v)), stringsAsFactors = FALSE); res <- NULL
      for (i in seq_len(nrow(grid))) { xs <- c(extra, unlist(grid[i, ])); m <- fit(S, xs, dv); if (is.null(m)) next; ct <- coeftest(m, vcov. = vcovHAC(m)); res <- rbind(res, data.frame(spec = paste(sub("^L_", "", unlist(grid[i, ])), collapse = " + "), AIC = AIC(m), adjR2 = summary(m)$adj.r.squared, N = nobs(m), RESET = resettest(m)$p.value, coefs = paste(sprintf("%s %.3f (p %.3f)", sub("^L_", "", unlist(grid[i, ])), ct[unlist(grid[i, ]), 1], ct[unlist(grid[i, ]), 4]), collapse = "; "))) }
      res <- res[order(res$AIC), ]; cat(sprintf("\n   outcome %s | block: %s | %d models fitted; best three:\n", dv, bn, nrow(res)))
      for (i in seq_len(min(3, nrow(res)))) cat(sprintf("     %d. %-45s AIC %8.2f  adjR2 %.3f  RESET %.3f  N %d  | %s\n", i, res$spec[i], res$AIC[i], res$adjR2[i], res$RESET[i], res$N[i], res$coefs[i])) } }
  blocks <- list("debt/GDP + unemployment + tax per WAP" = c("debt_gdp", "unem", "tax_workpop"), "unemployment + tax per WAP" = c("unem", "tax_workpop"), "tax per WAP only" = "tax_workpop")
  cat("\n2. Predicting spending one year ahead from 1-year lags (each regressor in level, log, first difference or log difference; all combinations)\n")
  cat("   regressors: debt_gdp =", DEFS2[["debt_gdp"]], "; unem =", DEFS2[["unem"]], "; tax_workpop =", DEFS2[["tax_workpop"]], "; prefixes: ln_ log, d_ first difference, dln_ log difference; all lagged one year\n")
  cat("\n   Outcome A: growth of real TME, % (g_tme)"); run("g_tme", NULL, blocks)
  cat("\n   Outcome B: change in TME / GDP, percentage points (d_tme_gdp)"); run("d_tme_gdp", NULL, blocks)
  cat("\n   Outcome C: log TME / GDP with its own lag (ln_tme_gdp)"); run("ln_tme_gdp", "L_ln_tme_gdp", blocks) }
