# parallel_rate_vac.R -- PARALLEL SPECIFICATION 3 (Sam, 28 Sep 2026): main = the PUBLISHED dependent variable (log inflows per 1,000
# working-age persons, t-1; basis step on the extended sample) + log vacancies per working-age person (L_ln_vac_wap), unemployment
# retained. Every test of the paper on both samples; engine and table list in parallel_spec.R. Needs only Dissertation_Dataset_Final.xlsx.
# RStudio: set the working directory to diss_replication/ (Session > Set Working Directory > To Source File Location), then
# source("parallel_rate_vac.R").
GROSS <- FALSE; VAC <- TRUE; source("parallel_spec.R")
