# =============================================================================
# data_common.R -- one data frame `df` with the canonical variable names of tables_common.R,
# for either sample. Sourced by boe_tests.R, boe_appendix.R, extra_tests.R, gad_obr_tests.R.
#     SAMPLE == "1964": extended sample (sheet `1964`; mig_rate = series B), Y0 = 1965, STEP = "basis_pre1975"
#     SAMPLE == "1977": core sample (sheet ReformSPA; mig_rate = non-EU citizens), Y0 = 1977, STEP = NULL
# Inputs: Dissertation_Dataset_Final.xlsx; determinants/boe_extras_1962_2019.csv;
# determinants/boe_a27_tax_variants_1956_2019.csv; determinants/extension_1962_2019_extras.csv;
# determinants/gad/gad_pressure_series_1959_2019.csv; determinants/work_permits.csv.
# =============================================================================
suppressPackageStartupMessages(library(readxl))
if (!exists("SAMPLE")) SAMPLE <- "1964"
WB <- "Dissertation_Dataset_Final.xlsx"
num_df <- function(d) { d <- d[, !vapply(d, function(x) all(is.na(x)) || is.character(x), logical(1)), drop = FALSE]
  for (v in setdiff(names(d), "year")) d[[v]] <- suppressWarnings(as.numeric(d[[v]])); d }
ext <- num_df(as.data.frame(read_excel(WB, sheet = "1964"))); ext <- ext[!is.na(ext$year), ]
ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA")); ref <- ref[!is.na(ref$year), ]
boe <- num_df(read.csv("determinants/boe_extras_1962_2019.csv")); tax <- read.csv("determinants/boe_a27_tax_variants_1956_2019.csv")
exs <- read.csv("determinants/extension_1962_2019_extras.csv"); exs$dr_note <- NULL
gad <- read.csv("determinants/gad/gad_pressure_series_1959_2019.csv"); gad <- gad[gad$rule == "base", ]
p1 <- read.csv("determinants/boe_prop1_1955_2019.csv")   # claimant count (A50), exchange rates, BoP, labour share, house prices, Q7 expectations, party in government
wp <- if (file.exists("determinants/work_permits.csv")) read.csv("determinants/work_permits.csv") else NULL
take <- function(d, src, cols) { for (v in intersect(cols, names(src))) if (!(v %in% names(d)) || all(is.na(d[[v]]))) d[[v]] <- src[[v]][match(d$year, src$year)]; d }

if (SAMPLE == "1964") {
  raw <- ext; wl <- c(NA, head(raw$workpop, -1))
  raw$mig_rate <- log(1000 * (1000 * raw$dv_B_splice_raw) / wl); raw$mig_gross <- log(1000 * raw$dv_B_splice_raw)
  STEP <- "basis_pre1975"; Y0 <- 1965; LAB <- "extended sample, 1965-"
} else {
  raw <- data.frame(year = ref$year, mig_rate = ref$loglev_nEU_WAPlagK, mig_gross = log(ref$nEU_inflows), delta_oadr_10 = ref$delta_oadr_10,
                    tax_workpop = ref$tax_workpop, ln_debt_gdp = ref$ln_debt_gdp, unem = ref$unem, workpop = ref$workpop,
                    inflation = 100 * c(NA, diff(log(ref$`deflator24_25=100`))))
  raw <- take(raw, ext, c("lt_inf_exp", "st_inf_exp", "tfp_growth", "delta_dr_10"))
  STEP <- NULL; Y0 <- 1977; LAB <- "core sample, 1977-"
}
# 2017-19 from ONS / BoE / BIS / Land Registry / OBR, spliced onto the Millennium 2016 values (Sam, 27 Sep 2026: main results
# run to 2019; the secondary check ends in 2016 with Millennium data only). Method and sources: the 'source' column of the file.
onsx <- read.csv("determinants/ons_extension_2017_2019.csv"); onsx$source <- NULL
for (v in setdiff(names(onsx), "year")) { for (src in c("boe", "p1")) { d0 <- get(src); if (v %in% names(d0)) { i <- match(onsx$year, d0$year); ok <- !is.na(i) & is.na(d0[[v]][i]); d0[[v]][i[ok]] <- onsx[[v]][ok]; assign(src, d0) } } }
raw <- take(raw, boe, setdiff(names(boe), "year")); raw <- take(raw, tax, setdiff(names(tax), "year"))
raw <- take(raw, exs, c("all_out", "brit_out", "brit_in", "inflation", "delta_dr_10", "uk_growth", "relinc_dev", "relinc_rich", "youth_origin", "post1968", "post1973")); raw <- take(raw, p1, setdiff(names(p1), "year")); raw <- take(raw, gad, c("E_d10", "POL_d10", "E_pct10", "POL_pct10", "E_gap10"))
raw <- raw[order(raw$year), ]; y <- raw$year; wl <- lag1(raw$workpop)
raw$eu_rate <- log(ref$EU_inflows_WAPlagK[match(y, ref$year)])
if (!is.null(wp)) { wp$permits[wp$basis != "WP+FP"] <- NA; raw$permits <- wp$permits[match(y, wp$year)] / 1000 }   # Tier 2 (2009-) is a different basis: never spliced
raw$brit_rate <- log(1000 * (1000 * raw$brit_in) / wl)
# canonical names for the BoE / A27 / OBR / GAD columns
ren <- c(unem_boe = "unem_spliced_boe", unem_claim = "unem_cc", growth = "uk_growth", youth = "youth_origin", tot = "terms_of_trade", trade_bal = "trade_bal_gdp", vac_wap = "vac_per_wap", vac_unem = "vu_ratio",
         wage_infl = "wage_inflation", tbill = "tbill_rate", real_rate = "real_short_rate", tfp = "tfp_growth", replace = "replacement_ratio", union = "union_density",
         tax_inc = "cg_income_tax_real_pw", tax_nics = "cg_nics_real_pw", tax_nics_emp = "cg_nics_employees_real_pw", tax_nics_er = "cg_nics_employers_real_pw",
         tax_direct = "cg_direct_taxes_real_pw", tax_indirect = "cg_tax_products_production_real_pw", tax_vat = "cg_vat_real_pw",
         tax_corp = "cg_corporation_tax_real_pw", tax_receipts = "cg_receipts_real_pw", int_gdp = "debt_interest_gdp", int_tme = "debt_interest_tme",
         gilt = "gilt10_yield", r_g = "r_minus_g", fc_budget = "fc_psnb_gdp_h0", fc_planned = "fc_psnb_surprise", fc_medium = "fc_psnb_gdp_hmax",
         grants_gdp = "grants_gdp", tme_gdp = "tme_gdp", days_lost_k = "days_lost_k")
for (k in names(ren)) if (ren[[k]] %in% names(raw)) raw[[k]] <- raw[[ren[[k]]]]
if (!"vac_wap" %in% names(raw)) raw$vac_wap <- NA
raw$vac_wap <- ifelse(is.na(raw$vac_wap), 1000 * (1000 * raw$vacancies_k) / raw$workpop, raw$vac_wap)   # 2017-19 from ONS AP2Y
raw$dln_vac_unem <- c(NA, diff(log(raw$vac_unem))); raw$emig_all <- 1000 * (1000 * raw$all_out) / wl; raw$emig_brit <- 1000 * (1000 * raw$brit_out) / wl
raw$real_wage <- raw$wage_infl - raw$inflation; raw$tax_inc_nics <- raw$tax_inc + raw$tax_nics; raw$ln_wap <- log(raw$workpop)
if ("grants_real_gbp_bn" %in% names(raw)) raw$grants_wap <- raw$grants_real_gbp_bn / (raw$workpop / 1e6)
if ("days_lost_k" %in% names(raw)) raw$days_lost <- raw$days_lost_k / (raw$workpop / 1e6)
# the headline unemployment measure of the day (information set): administrative / claimant count as published (Millennium M5,
# Denman and Macdonald, 1996, to 1995; claimant count 1996-97) until the ONS made the LFS (ILO) rate the headline in 1998
raw$unem_head <- ifelse(raw$year <= 1997, raw$admin_unem_rate, raw$unem)
raw$real_house <- 100 * c(NA, diff(log(raw$house_price_index))) - raw$inflation   # real house-price growth, %
# levels of the ratio, alternative tax denominators and the shorter horizons: ReformSPA from 1975; the pre-1975 vintages
# (npp_1963_1974_oadr.csv, npp_1963_1973_totals_by_vintage.csv) and fiscal_block_1962_1974.csv before (extended sample only)
o63 <- read.csv("determinants/npp_1963_1974_oadr.csv"); f62 <- read.csv("determinants/fiscal_block_1962_1974.csv")
tv <- read.csv("determinants/npp_1963_1973_totals_by_vintage.csv"); tv <- tv[!is.na(tv$oadr), ]
gref <- function(col) as.numeric(ref[[col]])[match(y, ref$year)]
pre <- function(col, src) src[[col]][match(y, src$year)]
raw$oadr_10 <- ifelse(y < 1975, pre("oadr_10", o63), gref("oadr_10")); raw$oadr_0 <- ifelse(y < 1975, pre("oadr_0", o63), gref("oadr_0"))
raw$tax_lab <- ifelse(y < 1975, pre("tax_lab_pw", f62), gref("lab_tax_workpop"))
raw$tax_res <- ifelse(y < 1975, pre("tax_nat_real_gbp_bn", f62) / pre("totpop_m", f62), gref("tax_totpop"))
oadr_v <- function(v, yr) { t <- tv[tv$vintage == v, ]; approx(t$year, t$oadr, xout = yr, rule = 2)$y }
for (h in c(1, 3, 5, 7)) { nm <- paste0("delta_oadr_", h)
  pre_h <- vapply(seq_len(nrow(o63)), function(i) oadr_v(o63$vintage[i], o63$year[i] + h) - oadr_v(o63$vintage[i], o63$year[i]), numeric(1))
  raw[[nm]] <- ifelse(y <= 1974, pre_h[match(y, o63$year)], gref(nm)) }
vo <- read.csv("determinants/commonwealth_vouchers_1962_1972.csv")
if (!is.null(wp)) { raw$vouchers <- ifelse(y >= 1962 & y <= 1972, vo$vouchers[match(y, vo$year)] / 1000, 0); raw$lever <- raw$permits + raw$vouchers }
raw$trend <- y - min(y)
raw$gad_earn <- if (SAMPLE == "1964") raw$E_pct10 else raw$E_d10; raw$gad_policy <- if (SAMPLE == "1964") raw$POL_pct10 else raw$POL_d10; raw$gad_gap <- raw$E_gap10
df <- raw
LAGV <- c("mig_rate", "mig_gross", "brit_rate", "eu_rate", "permits", "lever", "tax_lab", "tax_res", "tax_workpop", "ln_debt_gdp", "unem", "unem_boe", "unem_claim", "vac_wap", "vac_unem", "dln_vac_unem",
          "emig_all", "emig_brit", "wage_infl", "real_wage", "tbill", "real_rate", "tfp", "replace", "union", "tax_inc", "tax_nics", "tax_nics_emp", "tax_nics_er",
          "tax_inc_nics", "tax_direct", "tax_indirect", "tax_vat", "tax_corp", "tax_receipts", "ln_wap", "grants_gdp", "grants_wap", "tme_gdp", "int_gdp", "int_tme",
          "gilt", "r_g", "days_lost", "fc_budget", "growth", "inflation", "real_eri", "ca_gdp", "trade_bal", "labour_share", "tot",
          "real_house", "labour_gov", "youth", "relinc_dev", "relinc_rich", "unem_head")
for (v in intersect(LAGV, names(df))) df[[paste0("L_", v)]] <- lag1(df[[v]])
df$L2_mig_rate <- lag1(df$L_mig_rate); df$L_post98 <- as.numeric(df$year >= 1999)   # L_unem_head switches to the ILO rate from 1999 (lagged 1998 value)
BASE <- c("L_mig_rate", "L_tax_workpop", "delta_oadr_10", STEP, "L_ln_debt_gdp", "L_unem")   # the fiscal specification
BOE_END <- 2019   # main results end 2019 (ONS-extended); tables_common.R prints the 2016 secondary check                                       # BoE Millennium series end
SIX <- c("L_vac_unem", "L_wage_infl", "L_tbill", "st_inf_exp", "L_tfp", "L_vac_wap")   # 'six': the labour-market and macro controls used together (boe_tests.R 5, extra_tests.R 2)
