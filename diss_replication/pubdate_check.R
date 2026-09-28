# =============================================================================
# PUBLICATION-DATING CHECK  --  verifies every figure in thesis paragraph 484
#
#   "Does immigration depend on dependency ratios? Modelling fiscal
#    determinants of UK migration policy from beyond Europe (1979-2019)"
#   Sam Hamdane, Humboldt University of Berlin
#
# WHAT THE PARAGRAPH CLAIMS
#   "Each NPP is labelled by base year but published roughly a year later, and
#    my baseline assumes the fiscal centre saw the projections before the public
#    did. Re-dating every vintage from the year after its base -- the
#    conservative alternative -- reassigns the projection in force in 21 of my
#    41 sample years, some by more than fifteen points. The estimates barely
#    move: 0.0075 in the trend specification and 0.0086 in the fiscal, both
#    significant at the 0.1% level, with the zero-migration variant at 0.0067.
#    Both falsifications survive re-dating as well: the total dependency ratio
#    falls to -0.0001 (p = 0.94) and the EU placebo stays insignificant
#    (p = 0.40)."
#
# HOW THE RE-DATING IS COUNTED
#   Comparing the two series with a floating-point tolerance makes all 41 years
#   look changed, because the publication-dated series is recomputed rather than
#   copied and every year moves in the sixth decimal. That is not a
#   reassignment. The count below uses a 0.1-point threshold, and reports the
#   k_pub == 2 count beside it: k_pub records how far the year sits from its
#   vintage's base, so k_pub == 2 marks the years where re-dating genuinely
#   puts a different vintage in force. The two counts should agree closely.
#
# INPUT:  Dissertation_Dataset_Final.xlsx, sheets "ReformSPA" and
#         "PubShiftOADR_ZM" (older exports name the latter "pub shift OADR").
# Run from a clean session:  rm(list = ls()); source("pubdate_check.R")
# =============================================================================

library(lmtest); library(sandwich); library(readxl)

DATA_XLSX <- normalizePath("Dissertation_Dataset_Final.xlsx", mustWork = TRUE)
cat("data:", DATA_XLSX, "\n\n")

raw <- as.data.frame(read_excel(DATA_XLSX, sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]

.sheets   <- excel_sheets(DATA_XLSX)
.pubsheet <- intersect(c("PubShiftOADR_ZM", "pub shift OADR"), .sheets)
if (!length(.pubsheet))
  stop("no publication-dating sheet found in: ", paste(.sheets, collapse = ", "))
pub <- as.data.frame(read_excel(DATA_XLSX, sheet = .pubsheet[1]))
pub <- pub[!is.na(pub$year), ]

num <- function(x) if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
lag1 <- function(x) c(NA, head(x, -1))

reg_df <- with(raw, data.frame(
  year = year, log_mig_rate = loglev_nEU_WAPlagK,
  eu_log_mig_rate = log(EU_inflows_WAPlagK),
  delta_oadr_10 = delta_oadr_10,
  unem = unem, tax_workpop = tax_workpop, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "eu_log_mig_rate", "unem", "tax_workpop", "ln_debt_gdp"))
  reg_df[[paste0("L_", v)]] <- lag1(reg_df[[v]])
reg_df$trend <- seq_along(reg_df$year)

# Publication-dated series, matched on year rather than position: a misaligned
# paste would otherwise pass silently and shift every projection by one year.
for (v in c("delta_oadr_10_pub", "delta_zm_10_pub", "delta_dr_10_pub", "k_pub"))
  reg_df[[v]] <- if (v %in% names(pub))
    num(pub[[v]][match(reg_df$year, pub$year)]) else rep(NA_real_, nrow(reg_df))

reg_df <- subset(reg_df, year >= 1979 & year <= 2019)
reg_df$trend <- reg_df$trend - min(reg_df$trend) + 1
stopifnot(nrow(reg_df) == 41,
          !any(is.na(reg_df$delta_oadr_10_pub)), !any(is.na(reg_df$delta_zm_10_pub)))

# -----------------------------------------------------------------------------
# 1. How much re-dating actually moves
# -----------------------------------------------------------------------------
cat("===== How far re-dating moves the projection =====\n\n")
dlt <- reg_df$delta_oadr_10_pub - reg_df$delta_oadr_10
for (thr in c(0.001, 0.01, 0.1, 1, 5, 10, 15))
  cat(sprintf("  years changed by more than %6.3f points: %2d of %d\n",
              thr, sum(abs(dlt) > thr), nrow(reg_df)))
cat(sprintf("\n  years with k_pub == 2 (different vintage in force): %d\n",
            sum(reg_df$k_pub == 2, na.rm = TRUE)))
cat(sprintf("  largest single reassignment: %.2f points\n", max(abs(dlt))))
ord <- order(abs(dlt), decreasing = TRUE)[1:5]
cat("\n  five largest:\n")
cat(sprintf("    %d  baseline %8.2f  re-dated %8.2f  change %+7.2f\n",
            reg_df$year[ord], reg_df$delta_oadr_10[ord],
            reg_df$delta_oadr_10_pub[ord], dlt[ord]), sep = "")

# -----------------------------------------------------------------------------
# 2. The five re-dated specifications
# -----------------------------------------------------------------------------
cat("\n\n===== Re-dated estimates (HAC inference) =====\n\n")
hdr  <- sprintf("%-26s%12s%10s%12s", "specification", "coefficient", "p (HAC)",
                "claimed")
rule <- strrep("-", nchar(hdr))
cat(hdr, "\n", rule, "\n", sep = "")

check <- function(formula, term, label, claim, n_expect = 41) {
  m <- lm(formula, data = reg_df, na.action = na.omit)
  if (nobs(m) != n_expect)
    stop(sprintf("%s: N = %d, expected %d", label, nobs(m), n_expect))
  ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("%-26s%12.5f%10.4f%12s\n", label, ct[term, 1], ct[term, 4], claim))
  invisible(m)
}

check(log_mig_rate ~ L_log_mig_rate + trend + delta_oadr_10_pub +
        L_ln_debt_gdp + L_unem, "delta_oadr_10_pub", "trend", "0.0075")
check(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_oadr_10_pub +
        L_ln_debt_gdp + L_unem, "delta_oadr_10_pub", "fiscal", "0.0086")
check(log_mig_rate ~ L_log_mig_rate + trend + delta_zm_10_pub +
        L_ln_debt_gdp + L_unem, "delta_zm_10_pub", "zero-migration", "0.0067")
if (!any(is.na(reg_df$delta_dr_10_pub)))
  check(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10_pub +
          L_ln_debt_gdp + L_unem, "delta_dr_10_pub", "DR placebo", "-0.0001")
check(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10_pub +
        L_ln_debt_gdp + L_unem, "delta_oadr_10_pub", "EU placebo", "p = 0.40")
cat(rule, "\n", sep = "")
cat("The first three should be significant at the 0.1 per cent level and the\n",
    "last two insignificant, which is what paragraph 484 asserts.\n", sep = "")

cat("\n"); print(sessionInfo())
