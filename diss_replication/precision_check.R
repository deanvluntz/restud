# precision_check.R -- does transcription precision in the stored projection
# series matter? (Online Appendix A, "Independent reconstruction")
#
# The workbook's delta_oadr_10 was transcribed from the vintage workbooks, the
# early vintages to one decimal place. replication.R / replication_1977.R rebuild
# the same series at full precision from the vintage files and write it to
# zero_migration_deltas_R[_1977].csv (column delta_pub_10). This script
# re-estimates the fiscal specification with each version of the regressor, on
# both samples, and prints the coefficients to five decimals.
#
# Run from diss_replication/ AFTER replication_1977.R (or run_all_1977.R) has
# written zero_migration_deltas_R_1977.csv:
#     source("precision_check.R")
library(readxl); library(lmtest); library(sandwich)

rebuilt <- read.csv(if (file.exists("zero_migration_deltas_R_1977.csv"))
                      "zero_migration_deltas_R_1977.csv" else "zero_migration_deltas_R.csv")
raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))

lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK,
                           delta_sheet = delta_oadr_10, unem = unem,
                           tax_workpop = tax_workpop, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "unem", "tax_workpop", "ln_debt_gdp"))
  df[[paste0("L_", v)]] <- lag1(df[[v]])
df$delta_full <- rebuilt$delta_pub_10[match(df$year, rebuilt$year)]

pf <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)
fit <- function(term, d) {
  m  <- lm(reformulate(c("L_log_mig_rate", "L_tax_workpop", term, "L_ln_debt_gdp", "L_unem"),
                       "log_mig_rate"), data = d)
  ct <- coeftest(m, vcov. = vcovHAC(m))
  c(coef = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m))
}

cat("Fiscal specification, projection coefficient: stored (transcribed) vs full-precision rebuild\n\n")
cat(sprintf("%-11s %4s  %-26s %-26s %9s\n", "sample", "N", "stored delta_oadr_10",
            "full-precision rebuild", "change"))
for (y0 in c(1977, 1979)) {
  d <- subset(df, year >= y0 & year <= 2019)
  stopifnot(!anyNA(d$delta_full))
  a <- fit("delta_sheet", d); b <- fit("delta_full", d)
  cat(sprintf("%d-2019  %4d  %.5f (s.e. %.5f, p %s)  %.5f (s.e. %.5f, p %s)  %+.5f\n",
              y0, a["n"], a["coef"], a["se"], pf(a["p"]), b["coef"], b["se"], pf(b["p"]),
              b["coef"] - a["coef"]))
  cat(sprintf("           max |stored - rebuilt| over the sample: %.3f points\n",
              max(abs(d$delta_sheet - d$delta_full))))
}
cat("\nThe Online Appendix sentence quotes the 1977-2019 row (manuscript sample):\n",
    "\"re-estimating on the full-precision rebuild moves the coefficient from X to Y\".\n", sep = "")
