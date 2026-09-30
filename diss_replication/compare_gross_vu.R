# compare_gross_vu.R -- EXPLORATORY (Sam, 29 Sep 2026): gross inflows, tax per adult, with the vacancy/unemployment ratio in its three
# forms (log level, annual change in the log, level) and the term spread (10-year gilt yield minus the bill rate), on the criteria of
# compare_gross_adult.R. Needs only the workbook; RStudio: working directory = diss_replication/.
ADD <- list("base" = NULL, "+ln_vac_unem" = "L_ln_vac_unem", "+dln_vac_unem" = "L_dln_vac_unem", "+vac_unem" = "L_vac_unem", "+term_spread" = "L_term_spread"); TAXVAR <- "L_tax_voter"
source("tables_common.R"); DEFS[["L_term_spread"]] <- "10-year gilt yield minus the three-month Treasury bill rate, percentage points, t-1 (Millennium A31/M1; to 2016)"
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$L_term_spread <- df$L_gilt - df$L_tbill; df$term_spread <- df$gilt - df$tbill; df$ln_vac_unem <- log(df$vac_unem); list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977")); SKIP_LOAD <- TRUE
source("compare_gross_adult.R")
