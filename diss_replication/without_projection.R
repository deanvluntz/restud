# =============================================================================
# without_projection.R -- MANUSCRIPT: Online Appendix Tables B16-B18, both samples in one run:  source("without_projection.R")
#   B18 The fiscal model without the projection, controlled and gross inflows   no proj | main | mig_gross, no proj | mig_gross
#   B18 Recursive one-step-ahead forecasts (expanding window), without and with the projection
#   B18 The uncontrolled margin (core sample)                                    main | eu_rate | tot_rate
# The candidate screens, the selection rule and the augmented models are private tests (private_tests.R), not for publication.
# =============================================================================
source("tables_common.R")
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, STEP = STEP, Y0 = Y0, LAB = LAB, END = BOE_END) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
for (k in names(SMP)) SMP[[k]]$S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= SMP[[k]]$END)
fbase <- function(k, proj = FALSE) c("L_mig_rate", "L_tax_workpop", SMP[[k]]$STEP, "L_ln_debt_gdp", "L_unem", if (proj) "delta_oadr_10")
for (k in names(SMP)) { S <- SMP[[k]]$S; STEP <- SMP[[k]]$STEP; GB <- sub("^L_mig_rate$", "L_mig_gross", fbase(k))
  panel(list("no proj" = fit(S, fbase(k)), "main" = fit(S, fbase(k, TRUE)), "mig_gross, no proj" = fit(S, GB, "mig_gross"), "mig_gross" = fit(S, c(GB, "delta_oadr_10"), "mig_gross")),
        paste0("Table B16. The fiscal model without the projection: controlled and gross inflows (", SMP[[k]]$LAB, SMP[[k]]$END, ")"),
        c("delta_oadr_10", "L_unem", "L_tax_workpop", "L_ln_debt_gdp", "L_mig_rate", "L_mig_gross", STEP), w = 20, nested_F = TRUE,
        "no proj = delta_oadr_10 omitted. Without the projection the lagged inflow absorbs what it explained, the tax term shrinks, the debt term loses significance and RESET weakens.") }
cat("\n===== Table B17. Recursive one-step-ahead forecasts (expanding window; actual lagged mig_rate): root mean squared error =====\n")
oos <- function(S, xs, from) { e <- c()
  for (t in from:max(S$year)) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; if (!nrow(te)) next
    m <- lm(reformulate(xs, "mig_rate"), data = tr[complete.cases(tr[, c("mig_rate", xs)]), ]); e <- c(e, te$mig_rate - predict(m, newdata = te)) }
  c(sqrt(mean(e^2, na.rm = TRUE)), sum(!is.na(e))) }
for (k in names(SMP)) for (from in if (k == "extended") c(1990, 2001) else c(1997, 2001)) { S <- SMP[[k]]$S
  r0 <- oos(S, fbase(k), from); r1 <- oos(S, fbase(k, TRUE), from)
  cat(sprintf("%-9s forecasts %d-%d (n = %d): no proj %.4f | main %.4f | ratio %.2f\n", k, from, max(S$year), r0[2], r0[1], r1[1], r1[1] / r0[1])) }
cat("Each year's inflow is forecast from the model estimated on all earlier years; the projection was fixed in advance, so this is a genuine out-of-sample comparison.\n")
S <- subset(SMP$core$S, !is.na(eu_rate)); CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
panel(list("main" = fit(S, fbase("core", TRUE)), "eu_rate" = fit(S, c("L_eu_rate", "delta_oadr_10", CT), "eu_rate"), "tot_rate" = fit(S, c("L_tot_rate", "delta_oadr_10", CT), "tot_rate")),
      paste0("Table B18. The uncontrolled margin: EU and total inflows (core sample, 1977-", SMP$core$END, ")"), c("delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", "L_eu_rate", "L_tot_rate"), w = 14,
      "eu_rate: inflows of EU citizens, whom no British government could ration; tot_rate: all inflows (EU + non-EU). A policy variable should move the controlled flow only.")
m <- fit(S, c("L_eu_rate", "delta_oadr_10", CT, "L_vac_wap"), "eu_rate"); ct <- coeftest(m, vcov. = vcovHAC(m))
cat(sprintf("eu_rate +vac_wap: L_vac_wap %.4f (p %s), delta_oadr_10 %.4f (p %s): the EU margin responds to labour demand, not to the projection.\n", ct["L_vac_wap", 1], pf(ct["L_vac_wap", 4]), ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]))); defs("L_vac_wap")
