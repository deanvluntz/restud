# =============================================================================
# boe_appendix.R -- MANUSCRIPT Online Appendix Tables B6 and B7 (screenshot each '=====' block).
# Either sample (SAMPLE <- "1964" / "1977"); primary end 2016 (BoE data end).
# Names and definitions: tables_common.R; data: data_common.R.
#   B6  Labour demand and the tax base   baseline | +dln_vac_unem | vac_wap for unem | tax_nics for tax_workpop | tax_nics_emp for tax_workpop
#       text-only variants printed below: +vac_wap; tax_nics_er; tax_nics_emp + tax_nics_er; +ln_wap (Kronmal)
#   B7  Where labour demand shows up      eu_rate | eu_rate +vac_wap | permits | permits +vac_wap
#       plus: do lagged vacancies predict the projections' revisions?
# =============================================================================
source("tables_common.R"); source("data_common.R")
S <- subset(df, year >= Y0 & year <= BOE_END); TAG <- paste0(LAB, BOE_END)
swap <- function(base, drop, add) c(setdiff(base, drop), add)
b6 <- list("baseline" = fit(S, BASE), "+dln_vac_unem" = fit(S, c(BASE, "L_dln_vac_unem")), "vac_wap for unem" = fit(S, swap(BASE, "L_unem", "L_vac_wap")),
           "tax_nics for tax" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_nics")), "tax_nics_emp for tax" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_nics_emp")))
panel(b6, paste0("Table B6. Labour demand and the tax base (", TAG, ")"), c("delta_oadr_10", "L_dln_vac_unem", "L_vac_wap", "L_tax_workpop", "L_tax_nics", "L_tax_nics_emp", "L_unem", "L_ln_debt_gdp", "L_mig_rate", STEP), w = 21,
      "'for tax' = in place of L_tax_workpop; 'for unem' = in place of L_unem.")
tx <- list("+vac_wap" = fit(S, c(BASE, "L_vac_wap")), "tax_nics_er for tax" = fit(S, swap(BASE, "L_tax_workpop", "L_tax_nics_er")),
           "tax_nics_emp + tax_nics_er for tax" = fit(S, swap(BASE, "L_tax_workpop", c("L_tax_nics_emp", "L_tax_nics_er"))), "+ln_wap (Kronmal)" = fit(S, c(BASE, "L_ln_wap")))
cat("\n--- Text-only variants (delta_oadr_10; named regressors) ---\n")
for (k in names(tx)) { ct <- coeftest(tx[[k]], vcov. = vcovHAC(tx[[k]])); extra <- setdiff(rownames(ct), c("(Intercept)", BASE))
  cat(sprintf("%-36s delta_oadr_10 %.4f (p %s)", k, ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4])))
  for (e in extra) cat(sprintf(" | %s %.4f (p %s)", e, ct[e, 1], pf(ct[e, 4]))); cat("\n") }
defs(c("L_tax_nics_er", "L_ln_wap"))
# --- B7 ------------------------------------------------------------------------------------
S77 <- subset(df, year >= 1977 & year <= BOE_END); P <- subset(df, year >= 1965 & year <= 2008)
EUB <- swap(BASE, c("L_mig_rate", STEP), "L_eu_rate"); PB <- swap(BASE, "L_mig_rate", "L_permits")
b7 <- list("eu_rate" = fit(S77, EUB, "eu_rate"), "eu_rate +vac_wap" = fit(S77, c(EUB, "L_vac_wap"), "eu_rate"),
           "permits" = fit(P, PB, "permits"), "permits +vac_wap" = fit(P, c(PB, "L_vac_wap"), "permits"))
panel(b7, paste0("Table B7. Where labour demand shows up: the EU margin (1977-", BOE_END, ") and the permit lever (1965-2008)"),
      c("delta_oadr_10", "L_vac_wap", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_eu_rate", "L_permits", STEP), w = 17,
      c("The uncontrolled EU margin should respond to vacancies and not to the projection; permits (the lever) respond to both.",
        "Permits before 1965 cannot be used: the earliest projection vintage held is the 1963-based."))
rv <- read.csv("determinants/projection_revisions_1975_2019.csv"); df$revision <- rv$revision[match(df$year, rv$year)]
R <- subset(df, year >= 1977 & year <= BOE_END & !is.na(revision) & revision != 0)
m <- lm(revision ~ L_vac_wap, data = R); m2 <- lm(revision ~ L_vac_wap + L_unem + L_tax_workpop, data = R)
cat("\n--- Do lagged vacancies predict the projections' revisions? (years a new vintage came into force, 1977-", BOE_END, "; N = ", nobs(m), ") ---\n", sep = "")
print(coeftest(m, vcov. = vcovHAC(m))); print(coeftest(m2, vcov. = vcovHAC(m2)))
