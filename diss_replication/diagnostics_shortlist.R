# diagnostics_shortlist.R -- EXPLORATORY (Sam, 28 Sep 2026): the fuller diagnostic set of compare_gross_adult.R section 5 applied to
# the published specification and the shortlisted models (M1 fiscal core, M2 + log vacancies, M3 + debt interest + log vacancies), on
# both samples, without and with the projection. Needs only the workbook; RStudio: working directory = diss_replication/.
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R"); suppressPackageStartupMessages({library(strucchange); library(tseries)})
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, Y0 = Y0, STEP = STEP) }; SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
boot_reset <- function(m, R = 999) { X <- model.matrix(m); y0 <- fitted(m); e <- residuals(m) - mean(residuals(m)); f0 <- resettest(m, power = 2)$statistic; set.seed(20260802)
  mean(replicate(R, { yb <- y0 + sample(e, replace = TRUE); resettest(lm(yb ~ X - 1), power = 2)$statistic }) >= f0) }
boot_reset23 <- function(m, R = 999) { X <- model.matrix(m); y0 <- fitted(m); e <- residuals(m) - mean(residuals(m)); f0 <- resettest(m, power = 2:3)$statistic; set.seed(20260802)
  mean(replicate(R, { yb <- y0 + sample(e, replace = TRUE); resettest(lm(yb ~ X - 1), power = 2:3)$statistic }) >= f0) }
diag_row <- function(m) c(RESET2 = resettest(m, power = 2)$p.value, RESET2_boot = boot_reset(m), RESET23 = resettest(m, power = 2:3)$p.value, RESET23_boot = boot_reset23(m), rainbow = raintest(m)$p.value,
  harvey_collier = tryCatch(unname(harvtest(m)$p.value), error = function(e) NA), BG1 = bgtest(m, order = 1)$p.value, BG2 = bgtest(m, order = 2)$p.value, BG3 = bgtest(m, order = 3)$p.value,
  BreuschPagan = unname(bptest(m)$p.value), JarqueBera = unname(jarque.bera.test(residuals(m))$p.value), CUSUM = unname(sctest(efp(formula(m), data = attr(m, "dd"), type = "OLS-CUSUM"))$p.value), AICc = AIC(m) + 2 * length(coef(m)) * (length(coef(m)) + 1) / (nobs(m) - length(coef(m)) - 1))
SPECS <- list("published (rate; tax_workpop, debt, unem)" = list("mig_rate", c("L_tax_workpop", "L_ln_debt_gdp", "L_unem"), TRUE),
              "M1 gross: tax_voter, debt, unem" = list("mig_gross", c("L_tax_voter", "L_ln_debt_gdp", "L_unem"), FALSE),
              "M2 gross: M1 + ln_vac_wap" = list("mig_gross", c("L_tax_voter", "L_ln_debt_gdp", "L_unem", "L_ln_vac_wap"), FALSE),
              "M2r rate: tax_workpop, debt, unem + ln_vac_wap" = list("mig_rate", c("L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_ln_vac_wap"), TRUE),
              "M3 rate: M2r + int_tme" = list("mig_rate", c("L_tax_workpop", "L_ln_debt_gdp", "L_int_tme", "L_ln_vac_wap", "L_unem"), TRUE),
              "M3g gross: tax_res + int_tme + tbill, unem" = list("mig_gross", c("L_tax_res", "L_int_tme", "L_tbill", "L_unem"), FALSE))
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2019); out <- NULL
  for (nm in names(SPECS)) { sp <- SPECS[[nm]]; dv <- sp[[1]]; for (pr in c(FALSE, TRUE)) { xs <- c(paste0("L_", dv), if (pr) "delta_oadr_10", if (sp[[3]] && k == "extended") "basis_pre1975", sp[[2]]); m <- fit(S, xs, dv)
      out <- rbind(out, data.frame(model = paste0(nm, if (pr) "" else " [no proj]"), t(round(diag_row(m), 3)))) } }
  cat(sprintf("\n===== %s sample, %d-2019: p-values (AICc last) =====\n", k, SMP[[k]]$Y0)); print(out, row.names = FALSE) }
cat("\nRESET2/RESET23: Ramsey RESET with squares / squares+cubes of the fitted values; _boot: residual-bootstrap p (999 draws). rainbow: Utts (1982); harvey_collier: Harvey and Collier (1977);\nBG: Breusch (1978)-Godfrey (1978) LM, valid with a lagged dependent variable; CUSUM: OLS-based CUSUM (Ploberger and Kramer, 1992); AICc: Hurvich and Tsai (1989).\n")
