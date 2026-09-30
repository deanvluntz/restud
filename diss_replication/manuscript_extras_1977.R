# =============================================================================
# manuscript_extras_1977.R -- the robustness figure, the horizon figure, the
# circular-shift placebo and the second-lag check for the manuscript
# (1977-2019 sample, fiscal specification). Added 22 Sep 2026.
#
#   figure4_1977.png  every robustness variant's projection coefficient with its
#                     HAC 95% interval, placebos shown separately (Figure 4)
#   figure3_1977.png  projection coefficient by horizon h = 1..10, OADR against
#                     the total-dependency placebo (Figure 3)
#   printed           circular-shift randomisation p-value; second lag of the
#                     dependent variable with Breusch-Godfrey tests to order 4
#
# HAC = sandwich::vcovHAC defaults, as in every table of the paper.
# Run from diss_replication/:  source("manuscript_extras_1977.R")
# =============================================================================
library(readxl); library(lmtest); library(sandwich)

raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
pub <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "PubShiftOADR_ZM"))
pub <- pub[!is.na(pub$year), ]

lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(
  year = year, y = loglev_nEU_WAPlagK, g = log(nEU_inflows), eu = log(EU_inflows_WAPlagK),
  ltim = log(ltim_nEU_inflows_WAPlagK), p = delta_oadr_10, lev = oadr_10, zm = delta_zm_10,
  dr = delta_dr_10, un = unem, tax = tax_workpop, labtax = lab_tax_workpop,
  taxres = tax_totpop, debt = ln_debt_gdp))
for (h in 1:10) {
  df[[paste0("oadr_d", h)]] <- raw[[paste0("oadr_", h)]] - raw$oadr_0
  df[[paste0("dr_d", h)]]   <- raw[[paste0("delta_dr_", h)]]
}
df$p_pub <- as.numeric(pub$delta_oadr_10_pub[match(df$year, pub$year)])
for (v in c("y", "g", "eu", "ltim", "un", "tax", "labtax", "taxres", "debt"))
  df[[paste0("L_", v)]] <- lag1(df[[v]])
df$L2_y <- lag1(df$L_y)
df$POL97 <- as.integer(df$year >= 1998); df$POL02 <- as.integer(df$year >= 2002)
df$EXP04 <- as.integer(df$year >= 2004); df$ENL86 <- as.integer(df$year >= 1986)
df$ENL95 <- as.integer(df$year >= 1995)
df$liberal <- as.integer(df$year >= 1998 & df$year <= 2007)
S <- subset(df, year >= 1977 & year <= 2019)
stopifnot(nrow(S) == 43)

CTRL <- c("L_debt", "L_un")
est <- function(lab, data, y, xs, term) {
  m  <- lm(reformulate(xs, y), data = data)
  ct <- coeftest(m, vcov. = vcovHAC(m))
  data.frame(label = lab, coef = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m))
}
R <- rbind(
  est("Baseline (fiscal specification)",     S, "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("Zero-net-migration projection",       S, "y", c("L_y", "L_tax", "zm", CTRL), "zm"),
  est("Publication-dated projection",        S, "y", c("L_y", "L_tax", "p_pub", CTRL), "p_pub"),
  est("Gross (unscaled) non-EU inflows",     S, "g", c("L_g", "L_tax", "p", CTRL), "p"),
  est("Projected level in place of change",  S, "y", c("L_y", "L_tax", "lev", CTRL), "lev"),
  est("Labour taxes per working-age person", S, "y", c("L_y", "L_labtax", "p", CTRL), "p"),
  est("All taxes per resident",              S, "y", c("L_y", "L_taxres", "p", CTRL), "p"),
  est("+ 1998 policy step (POL97)",          S, "y", c("L_y", "L_tax", "p", "POL97", CTRL), "p"),
  est("+ 1998-2007 policy window",           S, "y", c("L_y", "L_tax", "p", "liberal", CTRL), "p"),
  est("+ all five MPR indicators",           S, "y", c("L_y", "L_tax", "p", "POL97", "POL02", "EXP04",
                                                      "ENL86", "ENL95", CTRL), "p"),
  est("Second lag of dependent variable",    S, "y", c("L_y", "L2_y", "L_tax", "p", CTRL), "p"),
  est("Sample from 1979",                    subset(S, year >= 1979), "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("Sample from 1983",                    subset(S, year >= 1983), "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("Sample from 1987",                    subset(S, year >= 1987), "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("LTIM inflows, 1992-2019",             subset(S, year >= 1991), "ltim", c("L_ltim", "L_tax", "p", CTRL), "p"))
PL <- rbind(
  est("Placebo: total dependency ratio", S, "y",  c("L_y", "L_tax", "dr", CTRL), "dr"),
  est("Placebo: EU inflows",             S, "eu", c("L_eu", "L_tax", "p", CTRL), "p"))
cat("--- Figure 4: projection coefficient across variants (HAC) ---\n")
print(format(rbind(R, PL), digits = 3), row.names = FALSE)

H <- do.call(rbind, lapply(1:10, function(h) cbind(h = h,
  setNames(est("oadr", S, "y", c("L_y", "L_tax", paste0("oadr_d", h), CTRL), paste0("oadr_d", h))[2:4],
           c("oadr", "oadr_se", "oadr_p")),
  setNames(est("dr", S, "y", c("L_y", "L_tax", paste0("dr_d", h), CTRL), paste0("dr_d", h))[2:4],
           c("dr", "dr_se", "dr_p")))))
cat("\n--- Figure 3: coefficient by horizon (HAC) ---\n"); print(round(H, 4), row.names = FALSE)

# --- Figure 4 (robustness) -----------------------------------------------------------------
png("figure4_1977.png", width = 6.5, height = 5.2, units = "in", res = 400, family = "sans")
all <- rbind(R, PL); k <- nrow(all); ypos <- c(k:(nrow(PL) + 1) + 1, nrow(PL):1)
lo <- all$coef - 1.96 * all$se; hi <- all$coef + 1.96 * all$se
par(mar = c(4, 17, 1, 1), cex = 0.8)
plot(all$coef, ypos, xlim = range(c(lo, hi, 0)), ylim = c(0.5, k + 1.5), yaxt = "n", pch = NA,
     xlab = "Coefficient on the projection term (95% HAC interval)", ylab = "")
abline(v = 0, col = "grey60"); abline(v = R$coef[1], lty = 2)
abline(h = nrow(PL) + 1, col = "grey80")
segments(lo, ypos, hi, ypos, lwd = 1.3)
points(all$coef, ypos, pch = c(rep(19, nrow(R)), rep(1, nrow(PL))), cex = 1.1)
axis(2, at = ypos, labels = all$label, las = 1, tick = FALSE)
dev.off()

# --- Figure 3 (horizons) -----------------------------------------------------------------
png("figure3_1977.png", width = 6.5, height = 3.2, units = "in", res = 400, family = "sans")
par(mar = c(4, 4.5, 1, 1), cex = 0.85)
off <- 0.12
yl <- range(c(H$oadr - 1.96 * H$oadr_se, H$oadr + 1.96 * H$oadr_se,
              H$dr - 1.96 * H$dr_se, H$dr + 1.96 * H$dr_se))
plot(H$h - off, H$oadr, ylim = yl, xlim = c(0.5, 10.5), pch = 19, xaxt = "n",
     xlab = "Projection horizon h (years)", ylab = "Coefficient (95% HAC interval)")
axis(1, at = 1:10)
abline(h = 0, col = "grey60")
segments(H$h - off, H$oadr - 1.96 * H$oadr_se, H$h - off, H$oadr + 1.96 * H$oadr_se, lwd = 1.3)
points(H$h + off, H$dr, pch = 1)
segments(H$h + off, H$dr - 1.96 * H$dr_se, H$h + off, H$dr + 1.96 * H$dr_se, lwd = 1.3, lty = 3)
legend("topright", bty = "n", pch = c(19, 1), lty = c(1, 3),
       legend = c("Old-age dependency ratio", "Total dependency ratio (placebo)"))
dev.off()

# --- Circular-shift placebo -----------------------------------------------------
# The projection series is rotated through the 43 sample years (k = 1..42), which
# keeps its own persistence and distribution but breaks its timing relative to
# inflows; each rotation is re-estimated. The randomisation p-value is the share
# of the 43 alignments (including the actual one) with |t| at least as large.
t_of <- function(x) { d <- S; d$pk <- x
  m <- lm(y ~ L_y + L_tax + pk + L_debt + L_un, data = d)
  coeftest(m, vcov. = vcovHAC(m))["pk", 3] }
t0 <- t_of(S$p)
tk <- vapply(1:42, function(k) t_of(c(tail(S$p, k), head(S$p, -k))), numeric(1))
cat(sprintf("\n--- circular-shift placebo ---\nactual t = %.2f; largest |t| among 42 rotations = %.2f; 95th pct |t| = %.2f\n",
            t0, max(abs(tk)), quantile(abs(tk), 0.95)))
cat(sprintf("rotations with |t| >= actual: %d of 42; randomisation p = %.3f (minimum attainable 1/43 = %.3f)\n",
            sum(abs(tk) >= abs(t0)), (1 + sum(abs(tk) >= abs(t0))) / 43, 1 / 43))

# --- Second lag of the dependent variable ---------------------------------------
m1 <- lm(y ~ L_y + L_tax + p + L_debt + L_un, data = S)
m2 <- lm(y ~ L_y + L2_y + L_tax + p + L_debt + L_un, data = S)
cat("\n--- second lag of the dependent variable ---\n")
print(coeftest(m2, vcov. = vcovHAC(m2)))
cat(sprintf("AIC %.2f (one lag) vs %.2f (two lags)\n", AIC(m1), AIC(m2)))
cat("Breusch-Godfrey p, orders 1-4:  one lag ",
    paste(sprintf("%.3f", sapply(1:4, function(k) bgtest(m1, order = k)$p.value)), collapse = " "),
    " | two lags ",
    paste(sprintf("%.3f", sapply(1:4, function(k) bgtest(m2, order = k)$p.value)), collapse = " "), "\n")
