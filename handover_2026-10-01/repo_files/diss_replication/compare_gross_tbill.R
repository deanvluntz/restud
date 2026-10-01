# compare_gross_tbill.R -- EXPLORATORY (Sam, 28 Sep 2026): gross inflows, tax per adult, with log vacancies and/or the three-month
# Treasury bill rate, on the criteria of compare_gross_adult.R (two-part AIC criterion, bounds tests, OOS RMSE, no-projection
# diagnostics, fuller diagnostics). Needs only the workbook; RStudio: working directory = diss_replication/.
ADD <- list("base" = NULL, "+ln_vac_wap" = "L_ln_vac_wap", "+tbill" = "L_tbill", "+both" = c("L_ln_vac_wap", "L_tbill")); TAXVAR <- "L_tax_voter"; source("compare_gross_adult.R")
