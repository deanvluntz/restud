# =============================================================================
# parallel_core.R -- Sam's CORE SPECIFICATION (29 Sep 2026) through the full parallel battery (parallel_spec.R engine):
#   gross inflows (no step) ~ lagged inflow + delta_oadr_10 + tax per working-age person + log change in debt/GDP + log V/U (in place
#   of unemployment) + log debt-interest share of TME.  Tables G2-G27 on both samples.  Nothing here is in any manuscript.
# RStudio: working directory = diss_replication/, then source this file.
# =============================================================================
GROSS <- TRUE; VAC <- FALSE; TAXVAR <- "L_tax_workpop"; LABV <- "L_ln_vac_unem"; INT <- TRUE
source("parallel_spec.R")
