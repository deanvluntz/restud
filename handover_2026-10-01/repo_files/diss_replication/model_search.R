# =============================================================================
# model_search.R -- EXPLORATORY (Sam, 28 Sep 2026): which configuration of Treasury-model variables performs best, on both samples,
# under the paper's criteria? Constraints: the dependent variable is the controlled inflow (gross inflows, or the published rate);
# every model contains the ten-year projected OADR change and one unemployment measure. Everything else is searched: the tax burden
# (per working-age person / adult / resident / NICs), borrowing and debt (log change in debt/GDP, PSNB/GDP, PSND/GDP, debt interest/
# TME), labour demand (log vacancies/WAP, change in log V/U), wages, inflation (deflator / CPI), the bill rate, the real exchange
# rate, growth, the current account -- the Heclo and Wildavsky / Macpherson list as far as the data reach 2019 without gaps.
# Exhaustive over subsets of up to MAXK additions (one member per family), for each unemployment measure, each DV, each sample.
# Criteria per model and sample: AIC with and without the projection (the two-part criterion), RESET(2) and BG(1) p, HAC p of the
# projection, max VIF. A model is ELIGIBLE if, on BOTH samples, RESET and BG pass at 5%, the projection is significant at 5%, and
# max VIF < 20. Eligible models are ranked by the summed AIC with the projection (comparable within a DV: same N on each sample).
# Recursive OOS RMSE is computed for the top 25. Output: ranked lists per DV. Runs in a few minutes.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R"); if (!exists("MAXK")) MAXK <- 4
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, Y0 = Y0, STEP = STEP, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
FAM <- list(tax = c("L_tax_workpop", "L_tax_voter", "L_tax_res", "L_tax_nics"), debt = c("L_ln_debt_gdp", "L_psnb_gdp", "L_psnd_gdp"), interest = "L_int_tme",
            labour = c("L_ln_vac_wap", "L_dln_vac_unem"), wages = "L_wage_infl", inflation = c("L_inflation", "L_cpi_infl"), rate = "L_tbill", external = c("L_real_eri", "L_ca_gdp"), growth = "L_growth")
UNEM <- c("L_unem", "L_unem_boe")
# candidates must be complete on both samples' years
S1 <- subset(SMP$extended$df, year >= 1965 & year <= 2019); S2 <- subset(SMP$core$df, year >= 1977 & year <= 2019)
ok <- function(v) v %in% names(S1) && v %in% names(S2) && !anyNA(S1[[v]]) && !anyNA(S2[[v]])
FAM <- lapply(FAM, function(v) v[vapply(v, ok, logical(1))]); FAM <- FAM[lengths(FAM) > 0]; cat("Candidate families:\n"); for (f in names(FAM)) cat(" ", f, ":", paste(FAM[[f]], collapse = ", "), "\n")
# enumerate combinations: choose up to MAXK families, one member each
fams <- names(FAM); combos <- list()
for (k in 0:MAXK) for (fs in if (k == 0) list(character(0)) else combn(fams, k, simplify = FALSE)) { grid <- expand.grid(lapply(fs, function(f) FAM[[f]]), stringsAsFactors = FALSE); if (k == 0) combos[[length(combos) + 1]] <- character(0) else for (i in seq_len(nrow(grid))) combos[[length(combos) + 1]] <- unname(unlist(grid[i, ])) }
cat(sprintf("%d variable sets x %d unemployment measures x 2 dependent variables x 2 samples\n", length(combos), length(UNEM)))
ev <- function(S, xs, dv) { m <- lm(reformulate(xs, dv), data = S); m0 <- lm(reformulate(setdiff(xs, "delta_oadr_10"), dv), data = S); ct <- coeftest(m, vcov. = vcovHAC(m))
  c(aic = AIC(m), aic0 = AIC(m0), reset = resettest(m)$p.value, bg = bgtest(m, order = 1)$p.value, p_proj = ct["delta_oadr_10", 4], b_proj = ct["delta_oadr_10", 1], vif = max(vif(m)), reset0 = resettest(m0)$p.value, bg0 = bgtest(m0, order = 1)$p.value) }
oos <- function(S, xs, dv, from) { e <- c(); for (t in from:2019) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; mm <- lm(reformulate(xs, dv), data = tr); e <- c(e, te[[dv]] - predict(mm, newdata = te)) }; sqrt(mean(e^2)) }
RES <- list()
for (dv in c("mig_gross", "mig_rate")) { rows <- list()
  for (u in UNEM) for (i in seq_along(combos)) { xs <- combos[[i]]; r1 <- ev(S1, c(paste0("L_", dv), "delta_oadr_10", u, if (dv == "mig_rate") "basis_pre1975", xs), dv); r2 <- ev(S2, c(paste0("L_", dv), "delta_oadr_10", u, xs), dv)
    rows[[length(rows) + 1]] <- data.frame(dv = dv, unem = u, vars = paste(sub("^L_", "", xs), collapse = " + "), k = length(xs), aic_sum = r1["aic"] + r2["aic"], aic_ext = r1["aic"], aic_core = r2["aic"], dAIC0_ext = r1["aic"] - r1["aic0"], dAIC0_core = r2["aic"] - r2["aic0"],
      b_ext = r1["b_proj"], b_core = r2["b_proj"], p_ext = r1["p_proj"], p_core = r2["p_proj"], reset_ext = r1["reset"], reset_core = r2["reset"], bg_ext = r1["bg"], bg_core = r2["bg"], vif_ext = r1["vif"], vif_core = r2["vif"], reset0_ext = r1["reset0"], reset0_core = r2["reset0"]) }
  R <- do.call(rbind, rows); rownames(R) <- NULL
  R$eligible <- with(R, reset_ext >= 0.05 & reset_core >= 0.05 & bg_ext >= 0.05 & bg_core >= 0.05 & p_ext < 0.05 & p_core < 0.05 & vif_ext < 20 & vif_core < 20)
  R <- R[order(R$aic_sum), ]; RES[[dv]] <- R
  cat(sprintf("\n\n===== %s: %d models, %d eligible (RESET, BG pass and projection significant on both samples, VIF < 20) =====\n", dv, nrow(R), sum(R$eligible)))
  top <- head(R[R$eligible, ], 25); top$oos_ext <- vapply(seq_len(nrow(top)), function(i) { xs <- strsplit(top$vars[i], " \\+ ")[[1]]; xs <- if (top$k[i] == 0) character(0) else paste0("L_", xs); oos(S1, c(paste0("L_", dv), "delta_oadr_10", top$unem[i], if (dv == "mig_rate") "basis_pre1975", xs), dv, 1990) }, numeric(1))
  top$oos_core <- vapply(seq_len(nrow(top)), function(i) { xs <- strsplit(top$vars[i], " \\+ ")[[1]]; xs <- if (top$k[i] == 0) character(0) else paste0("L_", xs); oos(S2, c(paste0("L_", dv), "delta_oadr_10", top$unem[i], xs), dv, 1997) }, numeric(1))
  num <- vapply(top, is.numeric, logical(1)); top[num] <- lapply(top[num], function(x) round(x, 4)); top$aic_sum <- round(top$aic_sum, 1); top$aic_ext <- round(top$aic_ext, 1); top$aic_core <- round(top$aic_core, 1); top$dAIC0_ext <- round(top$dAIC0_ext, 1); top$dAIC0_core <- round(top$dAIC0_core, 1); top$vif_ext <- round(top$vif_ext, 1); top$vif_core <- round(top$vif_core, 1)
  cat("Top 25 eligible by summed AIC with the projection (dAIC0 = AIC with minus without the projection; b = projection coefficient; oos = recursive RMSE 1990-/1997-2019):\n")
  print(top[, c("unem", "vars", "aic_sum", "aic_ext", "aic_core", "dAIC0_ext", "dAIC0_core", "b_ext", "b_core", "reset_ext", "reset_core", "bg_ext", "bg_core", "vif_ext", "vif_core", "oos_ext", "oos_core")], row.names = FALSE)
  cat("\nBest eligible model for each number of additions k:\n"); for (kk in 0:MAXK) { b <- R[R$eligible & R$k == kk, ][1, ]; if (!is.na(b$dv)) cat(sprintf("  k = %d: %s | %s | AIC ext %.1f core %.1f | proj %.4f / %.4f\n", kk, b$unem, if (b$vars == "") "(none)" else b$vars, b$aic_ext, b$aic_core, b$b_ext, b$b_core)) }
  cat("\nThe published fiscal specification for reference:\n"); ref <- R[R$unem == "L_unem" & R$vars == "tax_workpop + ln_debt_gdp", ]; print(ref[, c("aic_ext", "aic_core", "b_ext", "b_core", "reset_ext", "reset_core", "bg_ext", "bg_core", "eligible")], row.names = FALSE)
  write.csv(R, paste0("determinants/model_search_", dv, ".csv"), row.names = FALSE) }
cat("\nFull results written to determinants/model_search_mig_gross.csv and _mig_rate.csv\n")
