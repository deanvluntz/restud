# =============================================================================
# stability_check.R -- is the projection coefficient stable over time?
# (Sam, 25 Sep 2026). Three views of tax_spec:
#   Part 1  recursive (expanding-window) estimates of the projection coefficient,
#           from the first 15 years to the full sample, with HAC 95% bands;
#   Part 2  rolling 25-year windows;
#   Part 3  structural-break tests: OLS-CUSUM (strucchange::efp), the sup-F test
#           for an unknown break date (Andrews, 1993) and Bai-Perron breakpoints.
# Writes stability.png (recursive + rolling panels) and prints the tests.
# Needs: install.packages("strucchange")
# Run from diss_replication/:   START <- 1979; source("stability_check.R")   (thesis)
#                               START <- 1977; source("stability_check.R")   (manuscript)
# =============================================================================
library(readxl); library(lmtest); library(sandwich); library(strucchange)
if (!exists("START")) START <- 1979
MINW <- 15    # first recursive window (years)
ROLL <- 25    # rolling window length

raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK,
                           delta_oadr_10 = delta_oadr_10, tax_workpop = tax_workpop,
                           unem = unem, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "tax_workpop", "unem", "ln_debt_gdp")) df[[paste0("L_", v)]] <- lag1(df[[v]])
S <- subset(df, year >= START & year <= 2019)
FML <- log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10 + L_ln_debt_gdp + L_unem

est <- function(d) { m <- lm(FML, data = d); ct <- coeftest(m, vcov. = vcovHAC(m))
  c(b = ct["delta_oadr_10", 1], se = ct["delta_oadr_10", 2]) }
full <- est(S)

# --- Part 1: recursive ---------------------------------------------------------
rec <- do.call(rbind, lapply(MINW:nrow(S), function(n) {
  e <- est(S[1:n, ]); data.frame(end = S$year[n], b = e["b"], lo = e["b"] - 1.96 * e["se"], hi = e["b"] + 1.96 * e["se"]) }))
# --- Part 2: rolling -----------------------------------------------------------
rol <- do.call(rbind, lapply(1:(nrow(S) - ROLL + 1), function(i) {
  e <- est(S[i:(i + ROLL - 1), ]); data.frame(mid = S$year[i] + (ROLL - 1) / 2, start = S$year[i],
  b = e["b"], lo = e["b"] - 1.96 * e["se"], hi = e["b"] + 1.96 * e["se"]) }))

cat(sprintf("\n===== Stability of the projection coefficient, %d-2019 =====\n", START))
cat(sprintf("Full sample: %.4f (HAC s.e. %.4f)\n", full["b"], full["se"]))
cat(sprintf("Recursive estimates: min %.4f (to %d), max %.4f (to %d); all windows significant at 5%%: %s\n",
            min(rec$b), rec$end[which.min(rec$b)], max(rec$b), rec$end[which.max(rec$b)], all(rec$lo > 0)))
cat(sprintf("Rolling %d-year windows: min %.4f (%d-%d), max %.4f (%d-%d); windows with lower bound > 0: %d of %d\n",
            ROLL, min(rol$b), rol$start[which.min(rol$b)], rol$start[which.min(rol$b)] + ROLL - 1,
            max(rol$b), rol$start[which.max(rol$b)], rol$start[which.max(rol$b)] + ROLL - 1, sum(rol$lo > 0), nrow(rol)))

# --- Part 3: break tests -------------------------------------------------------
cus <- efp(FML, data = S, type = "OLS-CUSUM"); sc <- sctest(cus)
cat(sprintf("\nOLS-CUSUM test (all coefficients): S = %.3f, p = %.3f\n", sc$statistic, sc$p.value))
fs <- Fstats(FML, data = S, from = 8)               # sup-F; 8-year trimming (>= regressors + 2 at each end)
sf <- sctest(fs, type = "supF")
cat(sprintf("sup-F test for a single unknown break: supF = %.2f, p = %.3f; candidate break at %d\n",
            sf$statistic, sf$p.value, S$year[fs$breakpoint]))
bp <- breakpoints(FML, data = S, h = 8)                # minimum segment 8 years (> 6 regressors incl. intercept)
cat("Bai-Perron (min segment 8 years): BIC-selected number of breaks =", which.min(BIC(bp)) - 1, "\n")
if (which.min(BIC(bp)) > 1) print(S$year[breakpoints(bp)$breakpoints])
cat("Interpretation: no rejection = no evidence that the relationship shifted; a recursive path that\n",
    "settles early and stays inside the full-sample band says the same thing graphically.\n", sep = "")

# --- figure --------------------------------------------------------------------
png(sprintf("stability%s.png", if (START == 1977) "_1977" else ""), width = 1800, height = 800, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 1))
plot(rec$end, rec$b, type = "n", ylim = range(c(rec$lo, rec$hi, 0)), xlab = "End of sample", ylab = "Projection coefficient",
     main = sprintf("Recursive estimates (first window %d-%d)", START, START + MINW - 1))
polygon(c(rec$end, rev(rec$end)), c(rec$lo, rev(rec$hi)), col = "grey85", border = NA)
lines(rec$end, rec$b, lwd = 2); abline(h = full["b"], lty = 2); abline(h = 0, col = "grey40")
plot(rol$mid, rol$b, type = "n", ylim = range(c(rol$lo, rol$hi, 0)), xlab = "Window midpoint", ylab = "",
     main = sprintf("Rolling %d-year windows", ROLL))
polygon(c(rol$mid, rev(rol$mid)), c(rol$lo, rev(rol$hi)), col = "grey85", border = NA)
lines(rol$mid, rol$b, lwd = 2); abline(h = full["b"], lty = 2); abline(h = 0, col = "grey40")
dev.off()
cat("Figure written: stability*.png (dashed line = full-sample estimate; band = HAC 95%).\n")
