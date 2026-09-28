# parallel_gross.R -- PARALLEL SPECIFICATION 2 (Sam, 28 Sep 2026): main = log UNADJUSTED gross inflows (persons; no denominator,
# no basis step, no rescaling) with the fiscal controls exactly as published (no vacancy term). Every test of the paper on both
# samples; engine and table list in parallel_spec.R. Needs only Dissertation_Dataset_Final.xlsx. RStudio: set the working directory
# to diss_replication/ (Session > Set Working Directory > To Source File Location), then  source("parallel_gross.R").
GROSS <- TRUE; VAC <- FALSE; source("parallel_spec.R")
