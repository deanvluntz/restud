# =============================================================================
# extension_1964.R -- MANUSCRIPT: the fiscal specification on the 1964 extension
# (Sam, 25 Sep 2026). Same printing conventions as appendix_exhibits.R (HAC
# p-values; classical nested F against the model without its projection term).
#
# Data: sheet `1964` of Dissertation_Dataset_Final.xlsx (Sam's copy of
# determinants/extension_1962_2019_dataset.csv), falling back to the CSV if the
# sheet is absent. Columns:
#   workpop, totpop      working-age and total population: vintage in force
#                        (1963-based to 1973-based NPP) before 1975, workbook after
#   delta_oadr_10        projected ten-year change in the OADR, vintage in force
#                        (npp_1963_1974_oadr.csv; interpolated where the tenth year
#                        is not published -- see README_1964_extension.md)
#   tax_workpop          national accounts taxes per working-age person, 2024-25
#                        prices (OBR HPFD; HMT GDP deflator, March 2026 update)
#   psnd_gdp, ln_debt_gdp, unem   PSND/GDP; its annual log change; unemployment
#                        (BoE Millennium A50 before 1975, LF2Q after)
#   dv_A_non_british     non-British citizen inflows, thousands, 1964-2019
#   dv_B_splice_raw      non-British to 1974, non-EU from 1975 (unscaled)
#   dv_C_non_british_to_2003   non-British, ending before the A8 accession
#   non_eu               the thesis dependent variable (non-EU citizens), 1975-2019
#   basis_pre1975        1 before 1975: step indicator for the change of basis in B
# Every DV enters as the log of the inflow per 1,000 of last year's working-age
# population, as in the thesis (loglev_nEU_WAPlagK).
#   Table: mig_rate (series B with basis_pre1975) | mig_rate (no step). The alternative series
#   (A non-British throughout, C non-British to 2003, non-EU 1977-2019, A on sub-periods) are
#   printed as one text line. Names and definitions: tables_common.R.
# Run from diss_replication/:   source("extension_1964.R")
# =============================================================================
suppressPackageStartupMessages({library(lmtest); library(sandwich); library(car); library(readxl)})

WB <- "Dissertation_Dataset_Final.xlsx"; CSV <- "determinants/extension_1962_2019_dataset.csv"
if ("1964" %in% excel_sheets(WB)) {
  raw <- as.data.frame(read_excel(WB, sheet = "1964"))
  raw <- raw[, !vapply(raw, function(x) all(is.na(x)), logical(1))]      # drop empty columns
  raw <- raw[rowSums(!is.na(raw)) > 1, ]                                   # drop empty rows
  if (anyNA(raw$year)) {                                                   # year cells left blank at the top of the sheet
    first <- min(which(!is.na(raw$year))); raw$year <- raw$year[first] - (first - 1) + seq_len(nrow(raw)) - 1
    message("year column completed by sequence: ", min(raw$year), "-", max(raw$year))
  }
  if (!"totpop" %in% names(raw)) { cs <- read.csv(CSV); raw$totpop <- cs$totpop[match(raw$year, cs$year)] }
  src <- "workbook sheet 1964"
} else { raw <- read.csv(CSV); src <- CSV }
raw <- raw[order(raw$year), ]
for (v in setdiff(names(raw), "year")) raw[[v]] <- as.numeric(raw[[v]])
cat("data:", src, " years", min(raw$year), "-", max(raw$year), "\n")

lag1 <- function(x) c(NA, head(x, -1))
df <- raw
DVS <- c(A = "dv_A_non_british", B = "dv_B_splice_raw", C = "dv_C_non_british_to_2003", nonEU = "non_eu")
for (k in names(DVS)) {                                # log inflow (thousands -> persons) per 1,000 WAP (t-1)
  r <- 1000 * (1000 * df[[DVS[k]]]) / lag1(df$workpop); r[!is.finite(r) | r <= 0] <- NA
  df[[paste0("lr_", k)]] <- log(r); df[[paste0("L_lr_", k)]] <- lag1(df[[paste0("lr_", k)]])
}
for (v in c("tax_workpop", "ln_debt_gdp", "unem")) df[[paste0("L_", v)]] <- lag1(df[[v]])

# --- checks against the thesis sample -----------------------------------------
ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA")); ref <- ref[!is.na(ref$year), ]
chk <- merge(df[, c("year", "lr_nonEU", "delta_oadr_10", "tax_workpop", "unem", "ln_debt_gdp")],
             ref[, c("year", "loglev_nEU_WAPlagK", "delta_oadr_10", "tax_workpop", "unem", "ln_debt_gdp")], by = "year", suffixes = c("", "_wb"))
chk <- subset(chk, year >= 1979 & year <= 2019)
cat(sprintf("1979-2019 agreement with ReformSPA: max |diff| DV %.2e, projection %.2e, tax %.2e, unem %.2e, debt %.2e\n",
            max(abs(chk$lr_nonEU - chk$loglev_nEU_WAPlagK)), max(abs(chk$delta_oadr_10 - chk$delta_oadr_10_wb)),
            max(abs(chk$tax_workpop - chk$tax_workpop_wb)), max(abs(chk$unem - chk$unem_wb)), max(abs(chk$ln_debt_gdp - chk$ln_debt_gdp_wb))))

source("tables_common.R")
# canonical names: mig_rate = series B; the A, C and non-EU series are kept for the text lines below
df$mig_rate <- df$lr_B; df$L_mig_rate <- df$L_lr_B
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
fit1 <- function(dv, y0, y1, extra = NULL) { d <- subset(df, year >= y0 & year <= y1)
  m <- lm(reformulate(c(paste0("L_", dv), "delta_oadr_10", extra, CT), dv), data = d); stopifnot(nobs(m) == y1 - y0 + 1); m }
models <- list("mig_rate" = fit1("mig_rate", 1965, 2019, extra = "basis_pre1975"), "mig_rate (no step)" = fit1("mig_rate", 1965, 2019))
panel(models, "The fiscal specification on the extended sample, 1965-2019", c("delta_oadr_10", "basis_pre1975", CT, "L_mig_rate"), w = 20, lw = 16, nested_F = TRUE,
      c("The step absorbs the change of basis in 1975: before 1975 the inflow series counts non-British citizens (LTIM 2.00 to 1974), from 1975 non-EU citizens (IPS 3.01),",
        "without rescaling; its long-run value (0.13 / (1 - 0.46), about 0.25) is the log ratio of the two series where they overlap, i.e. the EU share of non-British inflows in 1975-80. The projection",
        "coefficient is the same with and without it.",
        "Projection before 1975: 1963- to 1973-based national population projections, vintage in force by base year; tenth year interpolated where unpublished (1964,",
        "1966-1970). Tax per WAP in 2024-25 prices (HMT deflator). Unemployment before 1975: Bank of England Millennium dataset A50 (Thomas and Dimsdale, 2017)."))
# --- reference results, text only ------------------------------------------------------------------
alt <- list("non-British throughout (A), 1965-2019" = fit1("lr_A", 1965, 2019), "non-British to 2003 (C), 1965-2003" = fit1("lr_C", 1965, 2003),
            "non-EU citizens, 1977-2019 (core sample)" = fit1("lr_nonEU", 1977, 2019), "non-British (A), 1965-1978 alone" = fit1("lr_A", 1965, 1978), "non-British (A), 1979-2019" = fit1("lr_A", 1979, 2019))
cat("\nAlternative series (delta_oadr_10, HAC p, N): ")
cat(paste(vapply(names(alt), function(k) { ct <- coeftest(alt[[k]], vcov. = vcovHAC(alt[[k]])); sprintf("%s %.4f (p %s, N %d)", k, ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]), nobs(alt[[k]])) }, character(1)), collapse = "; "), ".\n")
cat("A after 2004 includes EU citizens, whom no British government could restrict, which dilutes the coefficient; C ends before the A8 accession.\n")
cat("Population by pre-1975 vintage: determinants/npp_1963_1973_totals_by_vintage.csv (not printed).\n")
