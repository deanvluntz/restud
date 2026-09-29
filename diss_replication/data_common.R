# =============================================================================
# data_common.R -- one data frame `df` with the canonical variable names of tables_common.R,
# for either sample. Sourced by boe_tests.R, boe_appendix.R, extra_tests.R, gad_obr_tests.R.
#     SAMPLE == "1964": extended sample (sheet `1964`; mig_rate = series B), Y0 = 1965, STEP = "basis_pre1975"
#     SAMPLE == "1977": core sample (sheet ReformSPA; mig_rate = non-EU citizens), Y0 = 1977, STEP = NULL
# ONE DATASET (Sam, 28 Sep 2026): the assembled variables live in the workbook itself, sheets `data_1964` and `data_1977`
# (written by export_dataset.R), so a run needs nothing but Dissertation_Dataset_Final.xlsx. When that sheet exists it is read
# as is; the determinants/*.csv sources are used only when the sheet is absent or REBUILD_DATA <- TRUE (export_dataset.R),
# which is how the sheet is regenerated after any source changes.
# =============================================================================
suppressPackageStartupMessages(library(readxl))
if (!exists("SAMPLE")) SAMPLE <- "1964"
WB <- "Dissertation_Dataset_Final.xlsx"
num_df <- function(d) { d <- d[, !vapply(d, function(x) all(is.na(x)) || is.character(x), logical(1)), drop = FALSE]
  for (v in setdiff(names(d), "year")) d[[v]] <- suppressWarnings(as.numeric(d[[v]])); d }
ext <- num_df(as.data.frame(read_excel(WB, sheet = "1964"))); ext <- ext[!is.na(ext$year), ]
ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA")); ref <- ref[!is.na(ref$year), ]
if (SAMPLE == "1964") { STEP <- "basis_pre1975"; Y0 <- 1965; LAB <- "extended sample, 1965-" } else { STEP <- NULL; Y0 <- 1977; LAB <- "core sample, 1977-" }
DATA_SHEET <- paste0("data_", SAMPLE); FROM_SHEET <- !isTRUE(get0("REBUILD_DATA", ifnotfound = FALSE)) && DATA_SHEET %in% excel_sheets(WB)
if (FROM_SHEET) {
  raw <- num_df(as.data.frame(read_excel(WB, sheet = DATA_SHEET))); raw <- raw[!is.na(raw$year), ]; SPLICE_LOGRATIO <- raw$splice_logratio[1]
} else {
boe <- num_df(read.csv("determinants/boe_extras_1962_2019.csv")); tax <- read.csv("determinants/boe_a27_tax_variants_1956_2019.csv")
exs <- read.csv("determinants/extension_1962_2019_extras.csv"); exs$dr_note <- NULL
gad <- read.csv("determinants/gad/gad_pressure_series_1959_2019.csv"); gad <- gad[gad$rule == "base", ]
cpi <- read.csv("determinants/boe_a47_cpi_1955_2019.csv")
tvw <- read.csv("determinants/treasury_view_1955_2016.csv")
cit <- read.csv("determinants/citizenship_grants_1962_2018.csv")   # Home Office cz_03/cz_04: grants of British citizenship 1962-2018
ips <- read.csv("determinants/ips_301_citizenship_by_residence_1975_2019.csv", check.names = FALSE)   # openness, broad money growth, workforce (Millennium A36/A9/A24/A50; build_treasury_view.R)
fbk <- rbind(read.csv("determinants/fiscal_block_1962_1974.csv")[, c("year", "psnb_gdp", "psnd_gdp", "tax_nat_gdp")], read.csv("fiscal_block_HPFD.csv")[, c("year", "psnb_gdp", "psnd_gdp", "tax_nat_gdp")]); fbk <- fbk[!duplicated(fbk$year), ]; names(fbk)[4] <- "tax_gdp"   # CPI inflation, Millennium A47 col E (preferred CPI), ONS D7G7 for 2017-19
p1 <- read.csv("determinants/boe_prop1_1955_2019.csv")   # claimant count (A50), exchange rates, BoP, labour share, house prices, Q7 expectations, party in government
wp <- if (file.exists("determinants/work_permits.csv")) read.csv("determinants/work_permits.csv") else NULL
take <- function(d, src, cols) { for (v in intersect(cols, names(src))) if (!(v %in% names(d)) || all(is.na(d[[v]]))) d[[v]] <- src[[v]][match(d$year, src$year)]; d }

if (SAMPLE == "1964") {
  raw <- ext; wl <- c(NA, head(raw$workpop, -1))
  raw$mig_rate <- log(1000 * (1000 * raw$dv_B_splice_raw) / wl); raw$mig_gross <- log(1000 * raw$dv_B_splice_raw)
} else {
  raw <- data.frame(year = ref$year, mig_rate = ref$loglev_nEU_WAPlagK, mig_gross = log(ref$nEU_inflows), delta_oadr_10 = ref$delta_oadr_10,
                    tax_workpop = ref$tax_workpop, ln_debt_gdp = ref$ln_debt_gdp, unem = ref$unem, workpop = ref$workpop,
                    inflation = 100 * c(NA, diff(log(ref$`deflator24_25=100`))))
  raw <- take(raw, ext, c("lt_inf_exp", "st_inf_exp", "tfp_growth", "delta_dr_10"))
}
# 2017-19 from ONS / BoE / BIS / Land Registry / OBR, spliced onto the Millennium 2016 values (Sam, 27 Sep 2026: main results
# run to 2019; the secondary check ends in 2016 with Millennium data only). Method and sources: the 'source' column of the file.
onsx <- read.csv("determinants/ons_extension_2017_2019.csv"); onsx$source <- NULL
for (v in setdiff(names(onsx), "year")) { for (src in c("boe", "p1")) { d0 <- get(src); if (v %in% names(d0)) { i <- match(onsx$year, d0$year); ok <- !is.na(i) & is.na(d0[[v]][i]); d0[[v]][i[ok]] <- onsx[[v]][ok]; assign(src, d0) } } }
raw <- take(raw, boe, setdiff(names(boe), "year")); raw <- take(raw, tax, setdiff(names(tax), "year"))
# the `1964` sheet carries Sam's paste of the BoE extras to 2016, which take() keeps; the 2017-19 splices apply to both samples (28 Sep 2026)
for (v in setdiff(names(onsx), "year")) if (v %in% names(raw)) { i <- match(onsx$year, raw$year); ok <- !is.na(i) & is.na(raw[[v]][i]); raw[[v]][i[ok]] <- onsx[[v]][ok] }
raw <- take(raw, exs, c("all_out", "brit_out", "brit_in", "inflation", "delta_dr_10", "uk_growth", "relinc_dev", "relinc_rich", "youth_origin", "post1968", "post1973")); raw <- take(raw, p1, setdiff(names(p1), "year")); raw <- take(raw, cpi, "cpi_infl"); raw <- take(raw, tvw, c("openness", "m4_growth", "workforce_k", "bank_rate")); raw <- take(raw, fbk, c("psnb_gdp", "psnd_gdp", "tax_gdp")); raw <- take(raw, gad, c("E_d10", "POL_d10", "E_pct10", "POL_pct10", "E_gap10"))
# ONS series used in full where they cover the whole sample (Sam, 29 Sep 2026): current account AA6H and compensation of employees IHXP
# replace the Millennium-plus-splice versions (kept as *_mill for comparison); the PUSF debt-interest ratio ANLO / EBFT (public sector
# interest and dividends paid over TME, the Millennium definition on the current ONS vintage) is carried as int_tme_ons beside the
# Millennium-based int_tme; the two agree to 1996 and differ by 1-2 points from 1997 (ONS revisions to TME and interest since 2017).
ons_ca <- read.csv("determinants/ons_AA6H_current_account_gdp_1948_2025.csv"); ons_ls <- read.csv("determinants/ons_IHXP_compensation_employees_gdp_1948_2025.csv"); ons_pu <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv")
raw$ca_gdp_mill <- raw$ca_gdp; raw$ca_gdp <- ons_ca$ca_gdp_ons[match(raw$year, ons_ca$year)]
raw$labour_share_mill <- raw$labour_share; raw$labour_share <- ons_ls$labour_share_ons[match(raw$year, ons_ls$year)]
raw$int_tme_ons <- ons_pu$int_tme_ons[match(raw$year, ons_pu$year)]
# Destination unemployment for the British inflow (Sam, 29 Sep 2026): OECD annual LFS unemployment rates, 15+, for the four English-speaking
# destinations of British emigrants (Australia from 1967, Canada, New Zealand and the United States from 1955; Spain only from 1999, not used).
# unem_dest = their simple mean; unem_diff = UK ILO rate minus unem_dest (positive = UK worse than the destinations).
oecd <- read.csv("determinants/oecd_unemployment_destinations_1955_2025.csv"); j <- match(raw$year, oecd$year)
raw$unem_aus <- oecd$unem_aus[j]; raw$unem_usa <- oecd$unem_usa[j]; raw$unem_can <- oecd$unem_can[j]; raw$unem_nzl <- oecd$unem_nzl[j]
raw$unem_dest <- rowMeans(cbind(raw$unem_aus, raw$unem_usa, raw$unem_can, raw$unem_nzl)); raw$unem_diff <- raw$unem - raw$unem_dest
raw <- raw[order(raw$year), ]; y <- raw$year; wl <- lag1(raw$workpop)
raw$eu_rate <- log(ref$EU_inflows_WAPlagK[match(y, ref$year)])
if (!is.null(wp)) { wp$permits[wp$basis != "WP+FP"] <- NA; raw$permits <- wp$permits[match(y, wp$year)] / 1000 }   # Tier 2 (2009-) is a different basis: never spliced
raw$brit_rate <- log(1000 * (1000 * raw$brit_in) / wl)
# Alternative classifications of the dependent variable (Sam, 28 Sep 2026): non-British citizens throughout (LTIM 2.00, series A),
# non-British to 2003 (series C), the LTIM-basis non-EU series (1992-), and, on the extended sample, series B with the pre-1975
# non-British counts RESCALED by the 1975-80 overlap ratio non-EU / non-British instead of the estimated basis step.
raw$dv_A <- ext$dv_A_non_british[match(y, ext$year)]; raw$dv_C <- ext$dv_C_non_british_to_2003[match(y, ext$year)]; raw$dv_nonEU <- ext$non_eu[match(y, ext$year)]
raw$nb_rate <- log(1000 * (1000 * raw$dv_A) / wl); raw$nb03_rate <- log(1000 * (1000 * raw$dv_C) / wl)
raw$ltim_rate <- log(as.numeric(ref$ltim_nEU_inflows_WAPlagK)[match(y, ref$year)])
raw$cit_grants_k <- cit$cit_grants[match(y, cit$year)] / 1000; raw$cit_reg_k <- (cit$cit_reg_minor + cit$cit_reg_other)[match(y, cit$year)] / 1000
raw$cit_overseas_k <- cit$cit_overseas[match(y, cit$year)] / 1000
raw$cit_nat_k <- (cit$cit_nat_res + cit$cit_nat_marr)[match(y, cit$year)] / 1000
# UK-born inflows by country of last residence (IPS 3.18, Sam's upload 28 Sep 2026): returning emigrants as against British citizens
# by descent or registration; brit_desc_noneu = British citizens from outside the EU (3.01) minus UK-born from outside the EU (3.18)
b318 <- read.csv("determinants/ips_318_birth_by_residence_1975_2019.csv"); ukb <- subset(b318, direction == "Inflow" & birth == "UNITED KINGDOM")
gb <- function(r) ukb$est_k[ukb$residence == r][match(y, ukb$year[ukb$residence == r])]
for (r in c("NON-EU", "ALL", "OCEANIA", "NORTH AMERICA", "OTHER EUROPE", "ASIA", "SUB-SAHARAN AFRICA", "REST OF THE WORLD")) { v <- 1000 * (1000 * gb(r)) / wl; v[!is.finite(v) | v <= 0] <- NA
  raw[[paste0("ukborn_", if (r == "NON-EU") "noneu" else if (r == "ALL") "all" else tolower(gsub("[^A-Z]", "", substr(r, 1, 3))))]] <- log(v) }
bd <- 1000 * (1000 * (ips[["Inflow_NON-EU_brit"]][match(y, ips$year)] - gb("NON-EU"))) / wl; bd[!is.finite(bd) | bd <= 0] <- NA; raw$brit_desc_noneu <- log(bd)
raw$ukborn_noneu_k <- gb("NON-EU"); raw$brit_desc_noneu_k <- ips[["Inflow_NON-EU_brit"]][match(y, ips$year)] - gb("NON-EU")   # naturalisations (residence + marriage), the discretionary route (Sam, 28 Sep 2026)
for (r in c("OCEANIA", "NORTH AMERICA", "OTHER EUROPE", "ASIA", "SUB-SAHARAN AFRICA", "REST OF THE WORLD")) {   # British citizens by region of last residence (IPS 3.01)
  v <- 1000 * (1000 * ips[[paste0("Inflow_", r, "_brit")]][match(y, ips$year)]) / wl; v[!is.finite(v) | v <= 0] <- NA
  raw[[paste0("brit_", tolower(gsub("[^A-Z]", "", substr(r, 1, 3))))]] <- log(v) }
raw$brit_noneu <- log(1000 * (1000 * ips[["Inflow_NON-EU_brit"]][match(y, ips$year)]) / wl)
ov <- y >= 1975 & y <= 1980; SPLICE_LOGRATIO <- mean(log(raw$dv_nonEU[ov] / raw$dv_A[ov]))   # log ratio where the two series overlap
raw$dv_B_scaled <- ifelse(y < 1975, raw$dv_A * exp(SPLICE_LOGRATIO), raw$dv_nonEU)
raw$mig_rate_s <- log(1000 * (1000 * raw$dv_B_scaled) / wl); raw$mig_gross_s <- log(1000 * raw$dv_B_scaled)
# canonical names for the BoE / A27 / OBR / GAD columns
ren <- c(unem_boe = "unem_spliced_boe", unem_claim = "unem_cc", growth = "uk_growth", youth = "youth_origin", tot = "terms_of_trade", trade_bal = "trade_bal_gdp", vac_wap = "vac_per_wap", vac_unem = "vu_ratio",
         wage_infl = "wage_inflation", tbill = "tbill_rate", real_rate = "real_short_rate", tfp = "tfp_growth", replace = "replacement_ratio", union = "union_density",
         tax_inc = "cg_income_tax_real_pw", tax_nics = "cg_nics_real_pw", tax_nics_emp = "cg_nics_employees_real_pw", tax_nics_er = "cg_nics_employers_real_pw",
         tax_direct = "cg_direct_taxes_real_pw", tax_indirect = "cg_tax_products_production_real_pw", tax_vat = "cg_vat_real_pw",
         tax_corp = "cg_corporation_tax_real_pw", tax_receipts = "cg_receipts_real_pw", int_gdp = "debt_interest_gdp", int_tme = "debt_interest_tme",
         gilt = "gilt10_yield", consol = "consol_yield", r_g = "r_minus_g", fc_budget = "fc_psnb_gdp_h0", fc_planned = "fc_psnb_surprise", fc_medium = "fc_psnb_gdp_hmax",
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
# publication-dated projection (Sam, 29 Sep 2026): the vintage PUBLISHED by year t rather than the vintage with base year <= t. Pre-1975 vintages
# (biennial, published the year after their base year) from npp_1963_1973_totals_by_vintage.csv; 1978 onward from Sam's PubShiftOADR_ZM sheet
# (1977 vintage in force from 1978); 1976-77 would need the 1975 vintage beyond its tenth projection year and are left missing.
pubs <- as.data.frame(readxl::read_excel(WB, sheet = "PubShiftOADR_ZM", .name_repair = "minimal")); pubs <- pubs[!is.na(pubs$year), ]
vint_pub <- vapply(y, function(t) { v <- unique(tv$vintage[tv$vintage + 1 <= t]); if (length(v)) max(v) else NA_real_ }, numeric(1))
raw$p_pub_ext <- ifelse(y <= 1975, vapply(seq_along(y), function(i) if (is.na(vint_pub[i])) NA_real_ else oadr_v(vint_pub[i], y[i] + 10) - oadr_v(vint_pub[i], y[i]), numeric(1)), as.numeric(pubs$delta_oadr_10_pub)[match(y, pubs$year)])
# "Treasury view" candidates (Sam, 28 Sep 2026, after Macpherson's 2014 list): openness (free trade), broad money growth (sound money),
# participation (supply side), the revenue ceiling (doubts about raising revenue "beyond a certain point": taxes/GDP minus their
# highest value over the previous ten years), real spending growth (spending control), and three institutional regimes.
raw$particip <- 100 * (1000 * raw$workforce_k) / raw$workpop
raw$tax_ceiling <- vapply(seq_along(y), function(i) { w <- which(y >= y[i] - 10 & y < y[i]); if (length(w) < 5) NA_real_ else raw$tax_gdp[i] - max(raw$tax_gdp[w], na.rm = TRUE) }, numeric(1))
raw$tme_real_growth <- if ("tme_gbp_m" %in% names(raw)) 100 * c(NA, diff(log(raw$tme_gbp_m))) - raw$inflation else NA
raw$boe_indep <- as.numeric(y >= 1998); raw$golden_rule <- as.numeric(y >= 1998 & y <= 2009); raw$obr_era <- as.numeric(y >= 2010)
# growth per head, the Treasury's start-of-year growth forecast, labour productivity, the level of the total DR, total inflows
raw$totpop_all <- if (SAMPLE == "1964") raw$totpop else as.numeric(ref$totpop)[match(y, ref$year)]
raw$growth_pc <- raw$growth - 100 * c(NA, diff(log(raw$totpop_all)))
raw$growth_fc <- as.numeric(ref$growth_fc_yr0)[match(y, ref$year)]                          # HM Treasury forecast of growth in year t, made at the start of t (from 1976)
raw$lab_prod <- if ("lab_prod_hour_growth" %in% names(raw)) raw$lab_prod_hour_growth else NA
tvb <- tv[tv$year == tv$vintage, ]; dr0_pre <- 1000 * (tvb$totpop_k - tvb$workpop_k) / tvb$workpop_k   # total dependants per 1,000 WAP in the vintage base year
vint_pre <- o63$vintage[match(y, o63$year)]                                                          # vintage in force before 1975
drc <- read.csv("dr_corrected.csv"); dr10_post <- gref("dr_10"); i <- is.na(dr10_post) & y >= 1975; dr10_post[i] <- as.numeric(drc$dr_10[match(y[i], drc$year)])   # 1977-78 from the generated file
raw$level_dr_10 <- ifelse(y < 1975, dr0_pre[match(vint_pre, tvb$vintage)] + raw$delta_dr_10, dr10_post)
raw$level_oadr_10 <- raw$oadr_10
raw$tot_rate <- log(1000 * (1000 * (as.numeric(ref$nEU_inflows)[match(y, ref$year)] + as.numeric(ref$EU_inflows)[match(y, ref$year)])) / wl)   # all inflows (EU + non-EU), from 1975
# national accounts taxes over alternative denominators: per resident (tax_res), % of GDP (tax_gdp), and in millions of average
# annual earnings (tax_earn: real taxes / (52 x real average weekly earnings), Millennium A47 AWE to 2016, KAB9 growth 2017-19)
defl <- if (SAMPLE == "1964") ifelse(y < 1975, pre("deflator_2024_25_100", f62), as.numeric(ref$`deflator24_25=100`)[match(y, ref$year)]) else as.numeric(ref$`deflator24_25=100`)[match(y, ref$year)]
if ("awe_gbp" %in% names(raw)) { awe <- raw$awe_gbp; for (i in which(is.na(awe) & !is.na(raw$wage_inflation) & y >= 2017)) awe[i] <- awe[i - 1] * (1 + raw$wage_inflation[i] / 100)
  raw$tax_earn <- (raw$tax_workpop * 1000 * raw$workpop) / (52 * awe / defl * 100) / 1e6 }
raw$gad_earn <- if (SAMPLE == "1964") raw$E_pct10 else raw$E_d10; raw$gad_policy <- if (SAMPLE == "1964") raw$POL_pct10 else raw$POL_d10; raw$gad_gap <- raw$E_gap10
# log changes in the tax burden (Sam, 28 Sep 2026): annual change in log taxes per working-age person and per resident
raw$dln_tax_workpop <- c(NA, diff(log(raw$tax_workpop))); raw$dln_tax_res <- c(NA, diff(log(raw$tax_res)))
# tax per adult ("per voter", Sam 28 Sep 2026: adults = working-age population plus pensioners, from the current OADR) and per
# income-tax payer (HMRC counts in ReformSPA, from 1979); both in the units of tax_workpop (GBP 000, 2024-25 prices)
raw$tax_voter <- raw$tax_workpop / (1 + raw$oadr_0 / 1000)
raw$tax_taxpayer <- raw$tax_workpop * raw$workpop / (1000 * as.numeric(ref$inc_taxpayersK)[match(y, ref$year)])
# British return migration and nationality-law regimes (Sam, 28 Sep 2026, British-inflow checks): British emigration averaged over
# the previous five years (returns typically follow within a few years); steps for the British Nationality Act 1981 (in force
# 1983), the British Nationality (Hong Kong) Act 1990 (to the 1997 handover) and the Nationality, Immigration and Asylum Act 2002 (2003)
raw$emig_brit_5 <- vapply(seq_along(y), function(i) if (i < 6) NA_real_ else mean(raw$emig_brit[(i - 5):(i - 1)]), numeric(1))
raw$bna1981 <- as.numeric(y >= 1983); raw$hk1990 <- as.numeric(y >= 1990 & y <= 1997); raw$nia2002 <- as.numeric(y >= 2003)
# Home Office administrative series (Sam's Command Papers, 28 Sep 2026): acceptances for settlement 1975-88 (special voucher holders =
# UK passport holders from East Africa; ancestry route) and grants of citizenship by region 1971-83 (thousands)
hs <- read.csv("determinants/ho_settlement_1975_1988.csv"); hc <- read.csv("determinants/ho_citizenship_by_region_1971_1983.csv")
raw$settle_k <- hs$all_acceptances[match(y, hs$year)] / 1000; raw$vouchers_k <- hs$special_voucher_holders[match(y, hs$year)] / 1000; raw$ancestry_k <- hs$uk_born_grandparent[match(y, hs$year)] / 1000
raw$cit_newcw_k <- hc$new_commonwealth[match(y, hc$year)]; raw$cit_afrcw_k <- hc$african_commonwealth[match(y, hc$year)]; raw$cit_oldcw_k <- hc$old_commonwealth[match(y, hc$year)]
raw$splice_logratio <- SPLICE_LOGRATIO   # carried in the data sheet so the FROM_SHEET path recovers it
}
df <- raw
LAGV <- c("mig_rate", "mig_gross", "mig_rate_s", "mig_gross_s", "nb_rate", "nb03_rate", "ltim_rate", "brit_rate", "cit_grants_k", "cit_reg_k", "cit_overseas_k", "cit_nat_k", "settle_k", "vouchers_k", "ancestry_k", "cit_newcw_k", "cit_afrcw_k", "cit_oldcw_k", "dln_tax_workpop", "dln_tax_res", "tax_voter", "tax_taxpayer", "ukborn_noneu", "ukborn_all", "ukborn_oce", "ukborn_nor", "ukborn_oth", "ukborn_asi", "ukborn_sub", "ukborn_res", "brit_desc_noneu", "brit_oce", "brit_nor", "brit_oth", "brit_asi", "brit_sub", "brit_res", "brit_noneu", "eu_rate", "permits", "lever", "tax_lab", "tax_res", "tax_workpop", "ln_debt_gdp", "unem", "unem_boe", "unem_claim", "vac_wap", "vac_unem", "dln_vac_unem",
          "emig_all", "emig_brit", "wage_infl", "real_wage", "tbill", "real_rate", "tfp", "replace", "union", "tax_inc", "tax_nics", "tax_nics_emp", "tax_nics_er",
          "tax_inc_nics", "tax_direct", "tax_indirect", "tax_vat", "tax_corp", "tax_receipts", "ln_wap", "grants_gdp", "grants_wap", "tme_gdp", "int_gdp", "int_tme",
          "gilt", "r_g", "days_lost", "fc_budget", "growth", "inflation", "real_eri", "ca_gdp", "trade_bal", "labour_share", "tot", "ca_gdp_mill", "labour_share_mill", "int_tme_ons", "unem_dest", "unem_diff", "unem_aus", "unem_usa", "unem_can", "unem_nzl",
          "real_house", "labour_gov", "youth", "relinc_dev", "relinc_rich", "unem_head", "cpi_infl", "psnb_gdp", "psnd_gdp", "tax_gdp", "openness", "m4_growth", "particip", "tax_ceiling", "tme_real_growth", "bank_rate", "growth_pc", "lab_prod", "tax_earn", "tot_rate", "consol")
for (v in intersect(LAGV, names(df))) df[[paste0("L_", v)]] <- lag1(df[[v]])
# log forms of the positive-valued controls (Sam, 28 Sep 2026): L_ln_x = log of the lagged level
LOGV <- c("vac_wap", "vac_unem", "int_tme", "int_tme_ons", "int_gdp", "tbill", "gilt", "tme_gdp", "grants_gdp", "grants_wap", "psnd_gdp", "tax_gdp", "real_eri", "tot", "labour_share", "openness", "particip", "wage_infl_pos")
df$L_wage_infl_pos <- NULL
for (v in setdiff(LOGV, "wage_infl_pos")) if (paste0("L_", v) %in% names(df)) { x <- df[[paste0("L_", v)]]; x[!is.na(x) & x <= 0] <- NA; df[[paste0("L_ln_", v)]] <- log(x) }
df$L2_mig_rate <- lag1(df$L_mig_rate); df$L_post98 <- as.numeric(df$year >= 1999)   # L_unem_head switches to the ILO rate from 1999 (lagged 1998 value)
BASE <- c("L_mig_rate", "L_tax_workpop", "delta_oadr_10", STEP, "L_ln_debt_gdp", "L_unem")   # the fiscal specification
BOE_END <- 2019   # main results end 2019 (ONS-extended); tables_common.R prints the 2016 secondary check                                       # BoE Millennium series end
SIX <- c("L_vac_unem", "L_wage_infl", "L_tbill", "st_inf_exp", "L_tfp", "L_vac_wap")   # 'six': the labour-market and macro controls used together (boe_tests.R 5, extra_tests.R 2)
