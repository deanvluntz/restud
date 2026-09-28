# =============================================================================
# extension_1964_robustness.R -- MANUSCRIPT: robustness of the 1964 extension
# (Sam, 25 Sep 2026). Companion to extension_1964.R. Dependent variable B
# throughout (non-British citizens to 1974, non-EU citizens from 1975), entered
# as the log of the inflow per 1,000 of last year's working-age population, with
# basis_pre1975 in every column so that the definitional change is absorbed.
# Manuscript convention: HAC (vcovHAC) p-values; HAC Wald F for the projection.
#
# Data: sheet `1964` of Dissertation_Dataset_Final.xlsx (or the CSV) plus
# determinants/extension_1962_2019_extras.csv:
#   delta_dr_10   projected ten-year change in the TOTAL dependency ratio
#                 (children + pensioners per 1,000 WAP), vintage in force;
#                 1963-74 from the pre-1975 vintages (tenth year interpolated
#                 as for the OADR), 1975 from the 1975-based file, 1976 on from
#                 dr_corrected.csv
#   brit_out      British citizens emigrating, thousands (LTIM 2.00; IPS-only
#                 before 1991) -- the emigration regime
#   uk_growth     real GDP growth, % (ONS ABMI)
#   inflation     GDP deflator inflation, % (HM Treasury, March 2026)
#   network     inflows in t-2..t-4 per 1,000 WAP (t-1), series B
#   post1968, post1973   Commonwealth Immigrants Act 1968; Immigration Act 1971 in force
#   relinc_dev, relinc_rich, youth_origin   the determinants of determinants_check.R
#                 (Maddison relative income vs seven origins / vs US and Australia;
#                 UN WPP share aged 15-24 in the seven origins), now from 1963
#
#   Panel 1  Trend and policy steps      B | + trend | + Act steps | + both
#   Panel 2  Total-DR placebo            OADR | total DR | both (extended sample)
#   Panel 3  Added regressors            + growth | + inflation | + British emigration | + network | all four
#   Panel 3b Origin determinants         + rel. income (dev) | + rel. income (rich) | + youth share | all three
#   Panel 4  Sample starts               1965 | 1968 | 1971 | 1974 | 1977 | 1979 (the thesis)
#   Panel 5  Sub-samples                 1965-1991 | 1992-2019 | 1965-2003 | 1975-2019 (non-EU only)
# Run from diss_replication/:   source("extension_1964_robustness.R")
# =============================================================================
suppressPackageStartupMessages({library(lmtest); library(sandwich); library(car); library(readxl)})

WB <- "Dissertation_Dataset_Final.xlsx"; CSV <- "determinants/extension_1962_2019_dataset.csv"
if ("1964" %in% excel_sheets(WB)) {
  raw <- as.data.frame(read_excel(WB, sheet = "1964"))
  raw <- raw[, !vapply(raw, function(x) all(is.na(x)), logical(1))]; raw <- raw[rowSums(!is.na(raw)) > 1, ]
  if (anyNA(raw$year)) { first <- min(which(!is.na(raw$year))); raw$year <- raw$year[first] - (first - 1) + seq_len(nrow(raw)) - 1 }
} else raw <- read.csv(CSV)
raw <- raw[order(raw$year), ]; raw$dr_note <- NULL                      # text column, if the extras were pasted into the sheet
for (v in setdiff(names(raw), "year")) raw[[v]] <- as.numeric(raw[[v]])
EXTRAS <- c("delta_dr_10", "brit_out", "brit_in", "uk_growth", "inflation", "network_B", "post1968", "post1973",
            "relinc_dev", "relinc_rich", "youth_origin")
add <- setdiff(EXTRAS, names(raw))                                       # take from the CSV only what the sheet lacks
df <- if (length(add)) merge(raw, read.csv("determinants/extension_1962_2019_extras.csv")[, c("year", add)], by = "year", all.x = TRUE) else raw
df <- df[order(df$year), ]
cat("extras from the sheet:", paste(setdiff(EXTRAS, add), collapse = ", "), if (length(add)) paste("; from the CSV:", paste(add, collapse = ", ")) else "", "\n")

lag1 <- function(x) c(NA, head(x, -1))
r <- 1000 * (1000 * df$dv_B_splice_raw) / lag1(df$workpop); r[!is.finite(r) | r <= 0] <- NA
df$mig_rate <- log(r); df$L_mig_rate <- lag1(df$mig_rate)
for (v in c("tax_workpop", "ln_debt_gdp", "unem", "inflation", "brit_out", "relinc_dev", "relinc_rich")) df[[paste0("L_", v)]] <- lag1(df[[v]])
df$L_growth <- lag1(df$uk_growth); df$L_youth <- lag1(df$youth_origin); df$network <- df$network_B
df$brit_rate <- log(1000 * (1000 * df$brit_in) / lag1(df$workpop)); df$L_brit_rate <- lag1(df$brit_rate)
df$L_emig_brit <- 1000 * (1000 * df$L_brit_out) / lag1(df$workpop)   # British emigrants (t-1) per 1,000 WAP (t-1)
df$trend <- df$year - 1965

source("tables_common.R")
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
PROJ <- c("delta_oadr_10", "delta_dr_10")
hac_F <- function(m, terms) { lh <- linearHypothesis(m, paste0(terms, " = 0"), vcov. = vcovHAC(m)); c(lh$F[2], lh$`Pr(>F)`[2]) }
fitr <- function(y0 = 1965, y1 = 2019, proj = "delta_oadr_10", extra = NULL, step = TRUE) {
  d <- subset(df, year >= y0 & year <= y1)
  m <- lm(reformulate(c("L_mig_rate", proj, if (step && y0 < 1975) "basis_pre1975", extra, CT), "mig_rate"), data = d)
  stopifnot(nobs(m) == y1 - y0 + 1); m
}
BASE <- c("delta_oadr_10", "delta_dr_10", "basis_pre1975")

# --- Panel 1: trend and policy steps ------------------------------------------
p1 <- list("mig_rate"     = fitr(),
           "+trend"      = fitr(extra = "trend"),
           "+Act steps"  = fitr(extra = c("post1968", "post1973")),
           "+both"       = fitr(extra = c("trend", "post1968", "post1973")))
panel(p1, "Trend and policy steps, 1965-2019", c(BASE, "trend", "post1968", "post1973", "L_mig_rate", CT),
      notes = c("post1968: Commonwealth Immigrants Act 1968 (from 1968). post1973: Immigration Act 1971 in force (from 1973).",
                "Over 1965-78 alone the projection is collinear with time (r = -0.97), so these are the checks that matter."))

# --- Panel 2: total-DR placebo on the extended sample ---------------------------
p2 <- list("OADR"     = fitr(),
           "total DR" = fitr(proj = "delta_dr_10"),
           "both"     = fitr(proj = c("delta_oadr_10", "delta_dr_10")))
panel(p2, "Old-age vs. total dependency ratio, 1965-2019", c(BASE, "L_mig_rate", CT),
      notes = c("delta_dr_10: projected ten-year change in children + pensioners per 1,000 WAP, same vintages and",
                "interpolation as the OADR. In the 1960s it swings from +106 to -64 with the projected size of the child",
                "population; under the fiscal mechanism only the old-age term should carry the effect."))

# --- Panel 3: added regressors --------------------------------------------------
p3 <- list("mig_rate"             = fitr(),
           "+growth"      = fitr(extra = "L_growth"),
           "+inflation"   = fitr(extra = "L_inflation"),
           "+Brit out"    = fitr(extra = "L_emig_brit"),
           "+network"     = fitr(y0 = 1968, extra = "network"),
           "+all four"    = fitr(y0 = 1968, extra = c("L_growth", "L_inflation", "L_emig_brit", "network")))
panel(p3, "Added regressors, 1965-2019 (network columns from 1968)",
      c(BASE, "L_growth", "L_inflation", "L_emig_brit", "network", "L_mig_rate", CT),
      notes = "L_emig_brit stands for the emigration regime of the 1960s-70s.")

# --- Panel 3b: origin-country determinants (as in determinants_check.R, now from 1963) ----
p3b <- list("mig_rate"        = fitr(),
            "+rel inc dev"   = fitr(extra = "L_relinc_dev"),
            "+rel inc rich"  = fitr(extra = "L_relinc_rich"),
            "+youth share"   = fitr(extra = "L_youth"),
            "+all three"     = fitr(extra = c("L_relinc_dev", "L_relinc_rich", "L_youth")))
panel(p3b, "Origin-country determinants, 1965-2019", c(BASE, "L_relinc_dev", "L_relinc_rich", "L_youth", "L_mig_rate", CT),
      notes = "Supply-side determinants of migration, entered as omitted-variable checks, not as mechanism.")

# --- Panel 4: sample start ----------------------------------------------------
starts <- c(1965, 1968, 1971, 1974, 1977, 1979)
p4 <- lapply(starts, function(y) fitr(y0 = y)); names(p4) <- as.character(starts)
panel(p4, "Sample start (end 2019)", c(BASE, "L_mig_rate", CT), w = 12,
      notes = "basis_pre1975 enters only where the sample begins before 1975. 1977 is the core-sample start.")

# --- Panel 5: sub-samples -----------------------------------------------------
p5 <- list("1965-1991" = fitr(1965, 1991), "1992-2019" = fitr(1992, 2019), "1965-2003" = fitr(1965, 2003), "1975-2019" = fitr(1975, 2019))
panel(p5, "Sub-samples", c(BASE, "L_mig_rate", CT),
      notes = "1965-2003 ends before the A8 accession; 1975-2019 uses non-EU citizens only (no splice).")

# --- one-line summary ----------------------------------------------------------
b <- function(m) coef(m)["delta_oadr_10"]
cat(sprintf("\nProjection coefficient: base %.4f; with trend %.4f; with Act steps %.4f; with total DR alongside %.4f; with all four added regressors %.4f; with the three origin determinants %.4f.\n",
            b(p1[[1]]), b(p1[[2]]), b(p1[[3]]), b(p2[[3]]), b(p3[[6]]), b(p3b[[5]])))
cat(sprintf("Sample starts 1965-1979: %s.\n", paste(sprintf("%.4f", vapply(p4, b, numeric(1))), collapse = ", ")))

# --- Panel 6: the projection horizon on the extended sample ---------------------------------------
# For 1975 on, ReformSPA carries the projected h-year change (delta_oadr_h). For 1963-74 it is built here from
# the pre-1975 vintages (npp_1963_1973_totals_by_vintage.csv): OADR of the vintage in force, linearly interpolated
# between its published projection years, at t+h minus at t -- the construction of delta_oadr_10 for those years.
tv <- read.csv("determinants/npp_1963_1973_totals_by_vintage.csv"); tv <- tv[!is.na(tv$oadr), ]
o <- read.csv("determinants/npp_1963_1974_oadr.csv"); ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA")); ref <- ref[!is.na(ref$year), ]
oadr_v <- function(v, yr) { t <- tv[tv$vintage == v, ]; approx(t$year, t$oadr, xout = yr, rule = 2)$y }
for (h in c(1, 3, 5, 7)) { nm <- paste0("delta_oadr_", h)
  pre <- vapply(seq_len(nrow(o)), function(i) oadr_v(o$vintage[i], o$year[i] + h) - oadr_v(o$vintage[i], o$year[i]), numeric(1))
  df[[nm]] <- ifelse(df$year <= 1974, pre[match(df$year, o$year)], as.numeric(ref[[nm]])[match(df$year, ref$year)]) }
p6 <- lapply(c(1, 3, 5, 7, 10), function(h) fitr(proj = paste0("delta_oadr_", h))); names(p6) <- paste0("delta_oadr_", c(1, 3, 5, 7, 10))
p6_hac <- vapply(seq_along(p6), function(i) { m <- p6[[i]]; hacF(m, names(p6)[i]) }, numeric(2))
panel(p6, "The projection horizon, 1965-2019", c(paste0("delta_oadr_", c(1, 3, 5, 7, 10)), "basis_pre1975", "L_mig_rate", CT), w = 14,
      notes = c(sprintf("HAC F of the horizon term in each column: %s.", paste(sprintf("%s %.2f (p %s)", names(p6), p6_hac[1, ], vapply(p6_hac[2, ], pf, character(1))), collapse = "; ")),
                "delta_oadr_h: projected change in the old-age dependency ratio over the next h years, same vintages. The fit improves monotonically with the horizon; the one-year change carries nothing."))

# --- Panel 7: British citizens -- the uncontrolled flow, 1965-2019 --------------------------------
# brit_in (LTIM 2.00) counts British-citizen immigrants from ALL origins, the only British series from 1964; the
# core-sample placebo uses British citizens arriving from outside the EU (IPS 3.01, from 1975).
fitb <- function(dv, xs, y0 = 1965) { d <- subset(df, year >= y0 & year <= 2019); lm(reformulate(xs, dv), data = d) }
p7 <- list("mig_rate" = fitr(), "brit_rate" = fitb("brit_rate", c("L_brit_rate", "delta_oadr_10", CT)),
           "brit_rate, delta_dr_10" = fitb("brit_rate", c("L_brit_rate", "delta_dr_10", CT)),
           "mig_rate +brit_rate" = fitr(extra = "brit_rate"), "mig_rate +L_brit_rate" = fitr(extra = "L_brit_rate"))
panel(p7, "British citizens: the uncontrolled flow as placebo and as control, 1965-2019", c(BASE, "brit_rate", "L_brit_rate", "L_mig_rate", CT), w = 20,
      notes = c("Column 2 estimates the fiscal specification for British-citizen inflows (no basis step: the British series has no change of basis);",
                "column 3 replaces the old-age projection with the total-dependency projection; columns 4-5 add the British flow to the controlled-flow equation (t, t-1).",
                "A response of the British flow is a partial placebo failure (nationality-law and return channels); the projection with the British flow held constant is a lower bound."))
