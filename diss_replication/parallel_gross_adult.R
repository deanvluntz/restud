# parallel_gross_adult.R -- PARALLEL SPECIFICATION 4 (Sam, 28 Sep 2026): main = log UNADJUSTED gross inflows (persons; no denominator,
# no basis step, no rescaling) with the fiscal controls as published except that the tax burden is per ADULT (working-age plus
# pensioners: the electorate, approximately; L_tax_voter) instead of per working-age person; no vacancy term. Every test of the paper
# on both samples; engine and table list in parallel_spec.R. Needs only Dissertation_Dataset_Final.xlsx. RStudio: set the working
# directory to diss_replication/ (Session > Set Working Directory > To Source File Location), then  source("parallel_gross_adult.R").
GROSS <- TRUE; VAC <- FALSE; TAXVAR <- "L_tax_voter"; source("parallel_spec.R")
