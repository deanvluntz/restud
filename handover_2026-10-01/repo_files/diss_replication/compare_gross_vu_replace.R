# compare_gross_vu_replace.R -- EXPLORATORY (Sam, 29 Sep 2026): gross inflows, tax per adult, log change in debt/GDP, with the vacancy/
# unemployment ratio (log level, or annual change in the log) IN PLACE of unemployment, with and without the debt-interest share of
# spending; the first column has no labour-market term at all, for reference. Criteria of compare_gross_adult.R; no bootstrap.
# Needs only the workbook; RStudio: working directory = diss_replication/.
B0X <- c("L_mig_gross", "L_tax_voter", "L_ln_debt_gdp"); TAXVAR <- "L_tax_voter"
ADD <- list("no labour term" = NULL, "unem" = "L_unem", "ln_vac_unem" = "L_ln_vac_unem", "dln_vac_unem" = "L_dln_vac_unem", "ln_vac_unem +int_tme" = c("L_ln_vac_unem", "L_int_tme"),
            "dln_vac_unem +int_tme" = c("L_dln_vac_unem", "L_int_tme"), "unem +int_tme" = c("L_unem", "L_int_tme"))
source("tables_common.R")
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977")); SKIP_LOAD <- TRUE
source("compare_gross_adult.R")
