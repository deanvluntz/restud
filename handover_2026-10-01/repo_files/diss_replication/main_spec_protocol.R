# =============================================================================
# main_spec_protocol.R -- one main reference sample, with the legitimacy checks agreed with Sam (30 Sep 2026).
# Protocol: (1) the main specification is fixed on theory and documentary grounds and estimated on the Citizen sample (1965-2019);
# (2) the SAME specification, with nothing re-selected, is reported on the 1977-2019 sub-period (the "Origin sample" is this sub-period of
#     the same outcome series: from 1977 the two outcomes are identical); (3) a Chow test at 1977 asks whether the coefficients differ
#     before and after 1977 (every coefficient allowed to change; classical F and HC3 Wald; 12 years before 1977), and whether the projection's coefficient does;
# (4) literature comparison: the same specification with the residence-based outcome (all citizenships arriving from non-EU countries,
#     IPS 3.01, log persons, own lag) and with all non-EU citizens from any country of residence, 1977-2019.
# Specifications run through the protocol: the current core (timing A and B) and the two candidates from relax_ladder.R.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); ED <- build_data(); S <- ED[["1964"]]$S
SPECS <- list(
  "current core, projection t"   = CORE_A,
  "current core, projection t-1" = sub("^delta_oadr_10$", "L_delta_oadr_10", CORE_A),
  "published terms + debt-interest share + GDP-deflator inflation" = c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_int_tme", "L_inflation"),
  "published terms + debt-interest share + wage inflation (robustness line)" = c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_int_tme", "L_wage_infl"))
if (exists("EXTRA_SPECS")) SPECS <- c(SPECS, EXTRA_SPECS)   # optional: add specifications to run through the protocol
for (nm in names(SPECS)) run_protocol(nm, SPECS[[nm]], S)
cat("\nDone.\n")
