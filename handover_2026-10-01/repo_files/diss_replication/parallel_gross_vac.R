# parallel_gross_vac.R -- PARALLEL SPECIFICATION 1 (Sam, 28 Sep 2026): main = log UNADJUSTED gross inflows (persons; no denominator,
# no basis step, no rescaling) + log vacancies per working-age person (L_ln_vac_wap), unemployment retained. Every test of the paper on
# both samples; engine and table list in parallel_spec.R. Needs only Dissertation_Dataset_Final.xlsx. RStudio: set the working directory
# to diss_replication/ (Session > Set Working Directory > To Source File Location), then  source("parallel_gross_vac.R").
GROSS <- TRUE; VAC <- TRUE; source("parallel_spec.R")
