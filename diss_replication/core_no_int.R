# =============================================================================
# core_no_int.R -- the core WITHOUT the debt-interest share (Sam, 30 Sep 2026: int_tme is significant only through the 2008-13 reclassification
# of Northern Rock, RBS and Lloyds to the public sector (interest_apf.R), so it is not part of the results).
# Core: mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem (6 parameters), through the agreed protocol
# (main = Citizen sample 1965-2019; same specification on 1977-2019; Chow at 1977; residence-based and any-residence citizenship outcomes),
# with the previous core (with int_tme) printed alongside for comparison.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); S <- build_data()[["1964"]]$S
CORE_NI <- setdiff(CORE_A, "L_int_tme")
r1 <- run_protocol("Core without the debt-interest share (6 parameters)", CORE_NI, S)
r0 <- run_protocol("Previous core, with int_tme (7 parameters), for comparison", CORE_A, S)
lr <- function(m) coef(m)[["delta_oadr_10"]] / (1 - coef(m)[["L_mig_gross"]])
cat(sprintf("\nLong-run projection effect (OLS, no bias correction): without int_tme %.4f (1965-2019) / %.4f (1977-2019); with int_tme %.4f / %.4f\n", lr(r1$m), lr(r1$m7), lr(r0$m), lr(r0$m7)))
cat("\nDone.\n")
