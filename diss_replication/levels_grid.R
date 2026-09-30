# =============================================================================
# levels_grid.R (Sam, 30 Sep 2026): the core specification with the debt-interest share in LEVEL (int_tme, % of TME), and the debt and
# labour-market terms in level form, one at a time and together.
#   debt term:   ln_debt_gdp = annual log change in net debt / GDP (main) | d_psnd_gdp = annual change in pp | psnd_gdp = the ratio's level, %
#   labour term: ln_vac_unem = log V/U (main) | vac_unem = V/U in level
# Six columns per sample; the projection, tax per WAP and the own lag as in the core specification; all regressors t-1.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; FX <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_int_tme")
DEFS[["L_int_tme"]] <- "public sector debt interest, % of total managed expenditure, t-1 (Millennium A28; ONS PUSF growth-spliced 2017-19)"
DEFS[["L_d_psnd_gdp"]] <- "annual change in public sector net debt / GDP, percentage points, t-1 (OBR HPFD)"; DEFS[["L_psnd_gdp"]] <- "public sector net debt, % of GDP, t-1 (OBR HPFD)"
DEFS[["L_vac_unem"]] <- "vacancies / unemployed ratio, t-1 (Millennium M5 over A50; ONS 2017-19)"
debt <- c("ln_debt_gdp" = "L_ln_debt_gdp", "d_psnd_gdp" = "L_d_psnd_gdp", "psnd_gdp" = "L_psnd_gdp"); lab <- c("ln_vac_unem" = "L_ln_vac_unem", "vac_unem" = "L_vac_unem")
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$L_d_psnd_gdp <- lag1(c(NA, diff(S$psnd_gdp)))
  mods <- list(); for (dn in names(debt)) for (ln in names(lab)) mods[[paste(dn, "|", ln)]] <- fit(S, c(FX, debt[[dn]], lab[[ln]]), OUTC)
  names(mods)[1] <- "main (log forms)"
  panel(mods, sprintf("Table 1. Debt and labour-market terms in level form beside the debt-interest share in level, %s", L),
        c("delta_oadr_10", "L_int_tme", unname(debt), unname(lab), "L_tax_workpop", "L_mig_gross"), w = 26, nested_F = TRUE,
        notes = "Column titles: debt term | labour term. ln_debt_gdp is the annual log change in net debt / GDP (a flow); d_psnd_gdp the same change in percentage points; psnd_gdp the level of the ratio.")
  # same-observation AIC (the change terms lose the first year on the Citizen sample)
  yrs <- Reduce(intersect, lapply(mods, function(m) attr(m, "dd")$year)); cat(sprintf("   AIC on the common %d observations (%d-%d):\n", length(yrs), min(yrs), max(yrs)))
  for (nm in names(mods)) { m <- mods[[nm]]; xs <- setdiff(names(coef(m)), "(Intercept)"); m2 <- fit(S[S$year %in% yrs, ], xs, OUTC); ct <- coeftest(m2, vcov. = vcovHAC(m2))
    cat(sprintf("     %-28s AIC %8.2f  adjR2 %.4f  RESET %s  BG(1) %s  proj %.4f (p %s)  int_tme %.4f (p %s)\n", nm, AIC(m2), summary(m2)$adj.r.squared, pf(resettest(m2)$p.value), pf(bgtest(m2, 1)$p.value), ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), ct["L_int_tme", 1], pf(ct["L_int_tme", 4]))) } }
cat("\nDone.\n")
