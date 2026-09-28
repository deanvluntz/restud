# =============================================================================
# permits_check.R -- the policy lever: do work permits respond to the projection?
# (Sam, 25 Sep 2026). Data: determinants/work_permits.csv, read from Figure 1 of
# Bauer and Salt (2020) "Managing foreign labour immigration to the UK" (work
# permits and first permissions, long-term, 1946-2008; Tier 2 visa issues 2009-18;
# see README for the digitisation and its check against the paper's Annex 1).
#   Part 1  log permits per 1,000 working-age persons on its lag, the projection
#           and the tax_spec controls (HAC); also zero-migration projection and the
#           total-DR placebo; also in first differences.
#   Part 2  inflows on permits, then permits + projection (does the projection work
#           through permits?).
#   Part 3  timing: inflows on lagged permits and lagged projection.
# Main window 1979-2008 (one consistent series). 1979-2018 adds a Tier 2 step.
# Run from diss_replication/:   START <- 1979; source("permits_check.R")
# =============================================================================
library(readxl); library(lmtest); library(sandwich)
if (!exists("START")) START <- 1979
raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
wp  <- read.csv("determinants/work_permits.csv")
drc <- read.csv("dr_corrected.csv")
lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK, delta_oadr_10 = delta_oadr_10,
                           delta_zm_10 = delta_zm_10, tax_workpop = tax_workpop, unem = unem,
                           ln_debt_gdp = ln_debt_gdp, workpop = workpop))
df$delta_dr_10 <- drc$delta_dr_10[match(df$year, drc$year)]
df$permits <- wp$permits[match(df$year, wp$year)]
df$lpr <- log(1000 * df$permits / lag1(df$workpop))          # permits per 1,000 of last year's WAP
for (v in c("log_mig_rate", "lpr", "tax_workpop", "unem", "ln_debt_gdp", "delta_oadr_10")) df[[paste0("L_", v)]] <- lag1(df[[v]])
df$dlpr <- df$lpr - df$L_lpr; df$t2 <- as.integer(df$year >= 2009)
hac <- function(m) coeftest(m, vcov. = vcovHAC(m))
pf  <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)
CT  <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
show <- function(m, vars) for (v in vars) cat(sprintf("   %-14s %+8.4f (p %s)\n", v, hac(m)[v, 1], pf(hac(m)[v, 4])))

for (END in c(2008, 2018)) {
  S <- subset(df, year >= START & year <= END & !is.na(permits))
  ex <- if (END > 2008) "t2" else NULL
  cat(sprintf("\n===== Work permits and the projection, %d-%d (N = %d) =====\n", START, END, nrow(S)))
  cat("Part 1: log permit rate on lagged permit rate + projection + controls\n")
  m <- lm(reformulate(c("L_lpr", "delta_oadr_10", CT, ex), "lpr"), data = S); show(m, c("L_lpr", "delta_oadr_10"))
  cat("   controls dropped:\n"); m <- lm(reformulate(c("L_lpr", "delta_oadr_10", ex), "lpr"), data = S); show(m, "delta_oadr_10")
  cat("   zero-migration projection:\n"); m <- lm(reformulate(c("L_lpr", "delta_zm_10", CT, ex), "lpr"), data = S); show(m, "delta_zm_10")
  cat("   total-DR placebo:\n"); m <- lm(reformulate(c("L_lpr", "delta_dr_10", CT, ex), "lpr"), data = S); show(m, "delta_dr_10")
  cat("   first difference of log permits:\n"); m <- lm(reformulate(c("delta_oadr_10", CT, ex), "dlpr"), data = S); show(m, "delta_oadr_10")
  cat("Part 2: inflows on permits; then permits + projection\n")
  m0 <- lm(reformulate(c("L_log_mig_rate", "delta_oadr_10", CT, ex), "log_mig_rate"), data = S)
  m2 <- lm(reformulate(c("L_log_mig_rate", "lpr", CT, ex), "log_mig_rate"), data = S); show(m2, "lpr")
  m3 <- lm(reformulate(c("L_log_mig_rate", "lpr", "delta_oadr_10", CT, ex), "log_mig_rate"), data = S); show(m3, c("lpr", "delta_oadr_10"))
  cat(sprintf("   projection without permits: %.4f\n", coef(m0)["delta_oadr_10"]))
  cat("Part 3: timing, inflows on lagged permits and lagged projection\n")
  m4 <- lm(reformulate(c("L_log_mig_rate", "L_lpr", "L_delta_oadr_10", CT, ex), "log_mig_rate"), data = S); show(m4, c("L_lpr", "L_delta_oadr_10"))
}
cat("\nReading: Part 1 significant = the instrument itself responds to the projection. Part 2: if the\n",
    "projection keeps its coefficient once permits enter, its effect on inflows runs largely through\n",
    "routes other than work permits (students, dependants) or permits measure the lever with error.\n", sep = "")
