# sample_start_1977.R -- the main specifications on the longest sample the data
# allow, with the falsification design re-run on each start date.
#
# Why 1977 and not 1975 (Sam, 22 Sep 2026): the workbook now runs from 1975,
# and every projection column (delta_oadr_10, delta_dr_10, delta_zm_10) is
# filled from the 1975- and 1977-based vintages. The binding constraint is the
# debt control. L_ln_debt_gdp in year t is the log change in debt/GDP between
# t-2 and t-1, and the OBR HPFD debt ratio starts in 1974-75 (workbook 1975),
# so the first year with a lagged log change is 1977. EU inflows (the placebo)
# also start in 1976, so their lag starts in 1977. A 1976 start is possible only
# by dropping the debt control; that run is printed at the end for the record.
#
# Contemporaneous controls are also shown for the record: the lagged form is the
# one the Budget-time information set implies and the one that avoids
# simultaneity.
# Run from diss_replication/:  source("sample_start_1977.R")
library(lmtest); library(sandwich); library(car); library(readxl)
raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK,
                           eu_log_mig_rate = log(EU_inflows_WAPlagK),
                           delta_oadr_10 = delta_oadr_10, delta_dr_10 = delta_dr_10,
                           delta_zm_10 = delta_zm_10, unem = unem,
                           tax_workpop = tax_workpop, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "eu_log_mig_rate", "unem", "tax_workpop", "ln_debt_gdp"))
  df[[paste0("L_", v)]] <- lag1(df[[v]])
df$trend <- seq_along(df$year)
pf <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)
show <- function(f, data, label, term = "delta_oadr_10") {
  m <- lm(f, data = data, na.action = na.omit); ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("%-40s N=%d  %-13s %.4f (p %s)  adjR2 %.3f  AIC %.2f  RESET %s  maxVIF %.1f\n", label, nobs(m),
              term, ct[term, 1], pf(ct[term, 4]), summary(m)$adj.r.squared, AIC(m),
              pf(resettest(m)$p.value), max(vif(m))))
  invisible(m)
}
CTRL <- "L_tax_workpop + L_ln_debt_gdp + L_unem"
f_  <- function(dv, ldv, proj, ctrl = CTRL) as.formula(paste(dv, "~", ldv, "+", proj, "+", ctrl))
for (y0 in c(1977, 1978, 1979)) {
  d <- subset(df, year >= y0 & year <= 2019)
  n_exp <- 2019 - y0 + 1
  cat(sprintf("\n--- sample %d-2019 (expected N = %d) ---\n", y0, n_exp))
  m <- show(f_("log_mig_rate", "L_log_mig_rate", "delta_oadr_10"), d, sprintf("fiscal, OADR, from %d", y0))
  stopifnot(nobs(m) == n_exp)
  show(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 + L_ln_debt_gdp + L_unem, d,
       sprintf("trend,  OADR, from %d", y0))
  show(f_("log_mig_rate", "L_log_mig_rate", "delta_dr_10"),  d, sprintf("fiscal, total-DR placebo, from %d", y0), "delta_dr_10")
  show(f_("log_mig_rate", "L_log_mig_rate", "delta_zm_10"),  d, sprintf("fiscal, zero-migration, from %d", y0), "delta_zm_10")
  show(f_("eu_log_mig_rate", "L_eu_log_mig_rate", "delta_oadr_10"), d, sprintf("fiscal, EU placebo, from %d", y0))
}
cat("\nContemporaneous controls (1979-2019):\n")
d <- subset(df, year >= 1979 & year <= 2019)
show(log_mig_rate ~ L_log_mig_rate + tax_workpop + delta_oadr_10 + ln_debt_gdp + unem, d, "fiscal, controls at t (not t-1)")
show(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10 + ln_debt_gdp + unem,       d, "trend,  controls at t (not t-1)")

cat("\nFor the record -- 1976 start, debt control dropped (compare like with like):\n")
for (y0 in c(1976, 1979)) {
  d <- subset(df, year >= y0 & year <= 2019)
  show(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 + L_unem, d,
       sprintf("fiscal, no debt term, from %d", y0))
}
