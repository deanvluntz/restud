# =============================================================================
# spending_check.R -- does REALISED age-related spending do the work of the
# PROJECTED OADR change? (Sam, 23 Sep 2026)
# Each lagged spending measure (Sheet2 of the dataset; OBR Historical Public
# Finances Database, IFS spending composition) is (a) added to tax_spec and
# (b) swapped in for the projection. dAIC is against tax_spec on the same sample.
#   health_gdp   health spending, per cent of GDP
#   health_pw    health spending, 2024-25 GBP thousand per working-age person
#   pension_gdp  social security to pensioners, per cent of GDP (from 1978-79)
#   nonpens_gdp  social security to non-pensioners, per cent of GDP (from 1978-79)
#   agecost_gdp  health + pensioner social security, per cent of GDP
# Sheet2 year = fiscal year ending (1979 = 1978-79), as in fiscal_block_HPFD.csv.
# Run from diss_replication/:   START <- 1979; source("spending_check.R")   (thesis)
#                               START <- 1977; source("spending_check.R")   (manuscript)
# =============================================================================
library(readxl); library(lmtest); library(sandwich); library(car)
if (!exists("START")) START <- 1979

raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
s2  <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "Sheet2"))
fb  <- read.csv("fiscal_block_HPFD.csv")
gdp <- fb$gdp_nominal_gbp_bn[match(s2$year, fb$year)]
SP <- data.frame(year = s2$year,
                 health_gdp  = s2$health_gdp,
                 health_pw   = s2$health_pw,
                 pension_gdp = 100 * s2[["pension_£m"]] / 1000 / gdp,
                 nonpens_gdp = 100 * s2[["socsec_£m"]] / 1000 / gdp,
                 agecost_gdp = 100 * (s2[["health_£m"]] + s2[["pension_£m"]]) / 1000 / gdp)

lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK,
                           delta_oadr_10 = delta_oadr_10, tax_workpop = tax_workpop,
                           unem = unem, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "tax_workpop", "unem", "ln_debt_gdp")) df[[paste0("L_", v)]] <- lag1(df[[v]])
for (v in names(SP)[-1]) df[[paste0("L_", v)]] <- SP[[v]][match(df$year - 1, SP$year)]
S <- subset(df, year >= START & year <= 2019)

BASE <- c("L_log_mig_rate", "L_tax_workpop", "delta_oadr_10", "L_ln_debt_gdp", "L_unem")
fit  <- function(xs, d) lm(reformulate(xs, "log_mig_rate"), data = d)
hp   <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); c(ct[v, 1], ct[v, 4]) }
pf   <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)

cat(sprintf("\n===== Realised spending vs the projection, %d-2019 (HAC p) =====\n\n", START))
cat(sprintf("%-12s %3s | %-16s %-16s %6s %5s | %-16s %6s\n", "measure", "N",
            "proj (added)", "spending (added)", "dAIC", "VIF", "spending (swap)", "dAIC"))
for (v in names(SP)[-1]) {
  lv <- paste0("L_", v); d <- S[!is.na(S[[lv]]), ]
  m0 <- fit(BASE, d); m1 <- fit(c(BASE, lv), d); m2 <- fit(c(setdiff(BASE, "delta_oadr_10"), lv), d)
  a <- hp(m1, "delta_oadr_10"); b <- hp(m1, lv); s <- hp(m2, lv)
  cat(sprintf("%-12s %3d | %7.4f (%5s) %+8.4f (%5s) %+6.2f %5.1f | %+8.4f (%5s) %+6.2f\n", v, nobs(m1),
              a[1], pf(a[2]), b[1], pf(b[2]), AIC(m1) - AIC(m0), max(vif(m1)),
              s[1], pf(s[2]), AIC(m2) - AIC(m0)))
}
cat("added: spending term added to tax_spec; swap: spending term replaces the projection.\n",
    "dAIC vs tax_spec on the same sample (pensioner rows start 1980: split available from 1978-79).\n", sep = "")
