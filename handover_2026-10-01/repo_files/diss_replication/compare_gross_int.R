# compare_gross_int.R -- EXPLORATORY (Sam, 29 Sep 2026): Sam's conceptual model -- gross inflows; tax per adult (revenue / political
# constraint); log V/U in place of unemployment (labour market); log change in debt/GDP (long-term macroeconomic capacity); the debt-
# interest share of spending (short-term fiscal capacity) in four forms: level, log level, annual change in the log, first difference;
# the projection (future fiscal change). Criteria of compare_gross_adult.R; no bootstrap, asymptotic bounds critical values.
# Needs only the workbook; RStudio: working directory = diss_replication/.
B0X <- c("L_mig_gross", "L_tax_voter", "L_ln_debt_gdp", "L_ln_vac_unem"); TAXVAR <- "L_tax_voter"
ADD <- list("no int term" = NULL, "+int_tme" = "L_int_tme", "+ln_int_tme" = "L_ln_int_tme", "+dln_int_tme" = "L_dln_int_tme", "+d_int_tme" = "L_d_int_tme")
source("tables_common.R")
DEFS[["L_dln_int_tme"]] <- "annual change in the log of the debt-interest share of spending, t-1"; DEFS[["L_d_int_tme"]] <- "annual change in the debt-interest share of spending, percentage points, t-1"
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); df$ln_int_tme <- log(df$int_tme)
  df$dln_int_tme <- c(NA, diff(log(df$int_tme))); df$d_int_tme <- c(NA, diff(df$int_tme)); df$L_dln_int_tme <- lag1(df$dln_int_tme); df$L_d_int_tme <- lag1(df$d_int_tme); list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977")); SKIP_LOAD <- TRUE
source("compare_gross_adult.R")
