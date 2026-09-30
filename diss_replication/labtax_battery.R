# =============================================================================
# labtax_battery.R -- the fiscal specification with the tax term measured as
# LABOUR TAXES PER TOTAL POPULATION (income tax + NICs / total population) set
# against the Table 2 measure (national accounts taxes per working-age person),
# through the whole robustness battery (Sam, 23 Sep 2026, "for posterity").
#
#   Part 1  projection coefficient (HAC p) and AIC under each measure, check by
#           check: baseline, gross inflows, EU placebo, total-DR placebo,
#           zero-migration and publication-dated projections, projected level,
#           MPR indicators, second DV lag, later sample starts, LTIM inflows.
#   Part 2  telling the two measures apart: encompassing regression, Davidson-
#           MacKinnon J-test, Cox test, recursive one-step-ahead forecasts
#           (1999-2019) with a Diebold-Mariano comparison.
#   Part 3  PSS bounds test (ARDL package, case 3, exact) with each measure.
#
# Run from diss_replication/:   START <- 1979; source("labtax_battery.R")   (thesis)
#                               START <- 1977; source("labtax_battery.R")   (manuscript)
# =============================================================================
library(readxl); library(lmtest); library(sandwich); library(ARDL)
if (!exists("START")) START <- 1979

raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
pub <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "PubShiftOADR_ZM"))
pub <- pub[!is.na(pub$year), ]
fb  <- read.csv("fiscal_block_HPFD.csv"); fb <- fb[order(fb$year), ]
drc <- read.csv("dr_corrected.csv")            # total-DR projection incl. 1976-78

lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, y = loglev_nEU_WAPlagK, g = log(nEU_inflows),
                           eu = log(EU_inflows_WAPlagK), ltim = log(ltim_nEU_inflows_WAPlagK),
                           p = delta_oadr_10, lev = oadr_10, zm = delta_zm_10,
                           un = unem, debt = ln_debt_gdp))
df$dr    <- drc$delta_dr_10[match(df$year, drc$year)]
df$p_pub <- as.numeric(pub$delta_oadr_10_pub[match(df$year, pub$year)])
df$natwap <- (fb$tax_nat_real_gbp_bn / fb$wap_m)[match(df$year, fb$year)]     # Table 2 measure
df$labpop <- (fb$tax_lab_real_gbp_bn / fb$totpop_m)[match(df$year, fb$year)]  # labour taxes / total pop.
for (v in c("y", "g", "eu", "ltim", "un", "debt", "natwap", "labpop"))
  df[[paste0("L_", v)]] <- lag1(df[[v]])
df$L2_y  <- lag1(df$L_y)
df$POL97 <- as.integer(df$year >= 1998); df$POL02 <- as.integer(df$year >= 2002)
df$EXP04 <- as.integer(df$year >= 2004); df$ENL86 <- as.integer(df$year >= 1986)
df$ENL95 <- as.integer(df$year >= 1995)
S <- subset(df, year >= START & year <= 2019)
stopifnot(nrow(S) == 2019 - START + 1)

pf <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)
CT <- c("L_debt", "L_un")
cell <- function(d, dv, ldv, tax, proj, extra = NULL) {
  m  <- lm(reformulate(c(ldv, tax, proj, extra, CT), dv), data = d)
  ct <- coeftest(m, vcov. = vcovHAC(m))
  sprintf("%.4f (%s) %6.1f N=%d", ct[proj, 1], pf(ct[proj, 4]), AIC(m), nobs(m))
}
CHECKS <- list(
  list("Baseline",                      S, "y",    "L_y",    "p"),
  list("Gross non-EU inflows (DV)",     S, "g",    "L_g",    "p"),
  list("EU inflows (placebo DV)",       S, "eu",   "L_eu",   "p"),
  list("Total DR projection (placebo)", S, "y",    "L_y",    "dr"),
  list("Zero-migration projection",     S, "y",    "L_y",    "zm"),
  list("Publication-dated projection",  S, "y",    "L_y",    "p_pub"),
  list("Projected level (not change)",  S, "y",    "L_y",    "lev"),
  list("+ five MPR indicators",         S, "y",    "L_y",    "p", c("POL97", "POL02", "EXP04", "ENL86", "ENL95")),
  list("+ second lag of DV",            S, "y",    "L_y",    "p", "L2_y"),
  list("Sample from 1983",              subset(S, year >= 1983), "y", "L_y", "p"),
  list("Sample from 1987",              subset(S, year >= 1987), "y", "L_y", "p"),
  list("LTIM inflows, 1991-2019",       subset(S, year >= 1991), "ltim", "L_ltim", "p"))

cat(sprintf("\n===== Part 1: projection coefficient (HAC p), AIC, N -- %d-2019 =====\n\n", START))
cat(sprintf("%-31s %-30s %-30s\n", "check", "NAT per WAP (Table 2)", "Lab. tax per total pop."))
for (k in CHECKS) {
  ex <- if (length(k) > 5) k[[6]] else NULL
  cat(sprintf("%-31s %-30s %-30s\n", k[[1]],
              cell(k[[2]], k[[3]], k[[4]], "L_natwap", k[[5]], ex),
              cell(k[[2]], k[[3]], k[[4]], "L_labpop", k[[5]], ex)))
}

cat(sprintf("\n===== Part 2: telling the two measures apart -- %d-2019 =====\n", START))
mA <- lm(y ~ L_y + L_natwap + p + L_debt + L_un, data = S)
mB <- lm(y ~ L_y + L_labpop + p + L_debt + L_un, data = S)
mE <- lm(y ~ L_y + L_natwap + L_labpop + p + L_debt + L_un, data = S)
cat(sprintf("correlation of the two lagged measures: %.3f\n", cor(S$L_natwap, S$L_labpop)))
cat("\nEncompassing regression (both terms, HAC):\n")
print(coeftest(mE, vcov. = vcovHAC(mE))[c("L_natwap", "L_labpop", "p"), ])
cat("\nDavidson-MacKinnon J-test (model 1 = NAT/WAP, model 2 = Lab./pop.):\n"); print(jtest(mA, mB))
cat("\nCox test:\n"); print(coxtest(mA, mB))
cat("\nEncompassing test (lmtest::encomptest):\n"); print(encomptest(mA, mB))
fc <- function(tax) sapply(1999:2019, function(yr) {
  tr <- subset(S, year < yr); te <- subset(S, year == yr)
  m  <- lm(reformulate(c("L_y", tax, "p", CT), "y"), data = tr)
  te$y - predict(m, newdata = te)
})
eA <- fc("L_natwap"); eB <- fc("L_labpop"); dl <- eA^2 - eB^2
dm <- mean(dl) / (sd(dl) / sqrt(length(dl)))
cat(sprintf("\nRecursive one-step-ahead forecasts, 1999-2019: RMSE NAT/WAP %.4f, Lab./pop. %.4f;\n",
            sqrt(mean(eA^2)), sqrt(mean(eB^2))))
cat(sprintf("Diebold-Mariano (squared loss) t = %.2f, p = %.3f (positive favours Lab./pop.)\n",
            dm, 2 * pt(-abs(dm), length(dl) - 1)))

cat(sprintf("\n===== Part 3: PSS bounds test, case 3, exact -- %d-2019 =====\n", START))
for (tax in c("natwap", "labpop")) {
  ts_df <- ts(subset(df, year >= START & year <= 2019)[, c("y", "p", "un", "debt", tax)],
              start = START, frequency = 1)
  f  <- as.formula(paste("y ~ p + un + debt +", tax))
  ma <- ardl(f, data = ts_df, order = c(1, 1, 1, 1, 1))
  set.seed(20260802); bf <- bounds_f_test(ma, case = 3, exact = TRUE)
  set.seed(20260802); bt <- bounds_t_test(ma, case = 3, exact = TRUE)
  cat(sprintf("%-7s F = %.2f (p %s)   t = %.2f (p %s)\n", tax, bf$statistic, pf(bf$p.value),
              bt$statistic, pf(bt$p.value)))
}
