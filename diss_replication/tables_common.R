# =============================================================================
# tables_common.R -- shared printing conventions and the VARIABLE DICTIONARY for every
# manuscript table script (extension_1964.R, extension_1964_robustness.R, boe_tests.R,
# boe_appendix.R, extra_tests.R, gad_obr_tests.R). Sourced by each of them; do not run alone.
#
# Naming rule (Sam, 27 Sep 2026): one short name per variable, used identically as a row label,
# as a column title ("+name" = added to the baseline; "name for X" = in place of X; "name only"
# = in place of the projection) and in the text. L_ prefix = lagged one year. Every name that
# appears in a panel is defined in the notes printed under that panel (from DEFS below).
# =============================================================================
suppressPackageStartupMessages({library(lmtest); library(sandwich); library(car)})
# Main results run to 2019. Variables whose 2017-19 values are spliced from ONS/BoE/BIS/Land Registry/OBR onto the Millennium dataset
# (determinants/ons_extension_2017_2019.csv); any column using one is re-estimated to SEC_END and reported under the panel.
SEC_END <- 2016
ONS_VARS <- c("L_cpi_infl", "L_ln_vac_wap", "L_ln_vac_unem", "L_ln_int_tme", "L_ln_tbill", "L_ln_real_eri", "L_ln_tot", "L_ln_labour_share", "L_vac_wap", "L_vac_unem", "L_dln_vac_unem", "L_wage_infl", "L_real_wage", "L_tbill", "L_unem_boe", "L_int_tme", "L_real_eri",
              "L_ca_gdp", "L_trade_bal", "L_tot", "L_labour_share", "L_real_house")
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
pf <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
lag1 <- function(x) c(NA, head(x, -1))

DEFS <- c(
  mig_rate      = "log inflow of controlled migrants per 1,000 working-age persons (t-1): non-EU citizens (core sample); non-British citizens to 1974 spliced with non-EU citizens from 1975 (extended sample)",
  L_mig_rate    = "mig_rate lagged one year",
  mig_rate_s    = "as mig_rate, but with the pre-1975 non-British counts rescaled by the 1975-80 overlap ratio non-EU / non-British (a fixed splice in place of the estimated basis step)",
  L_mig_rate_s  = "mig_rate_s lagged one year",
  nb_rate       = "log inflow of non-British citizens (all origins, LTIM 2.00) per 1,000 working-age persons (t-1): the extended sample's pre-1975 classification, throughout",
  L_nb_rate     = "nb_rate lagged one year",
  nb03_rate     = "nb_rate ending in 2003, before the A8 accession",
  L_nb03_rate   = "nb03_rate lagged one year",
  ltim_rate     = "log inflow of non-EU citizens per 1,000 working-age persons (t-1) on the LTIM basis (IPS adjusted with Home Office and LFS data), 1992-2019",
  L_ltim_rate   = "ltim_rate lagged one year",
  mig_gross     = "log gross inflow (persons), same citizenship series as mig_rate",
  L_mig_gross   = "mig_gross lagged one year",
  L2_mig_rate   = "mig_rate lagged two years",
  delta_oadr_10 = "projected ten-year change in the old-age dependency ratio (pensioners per 1,000 working-age persons), national population projection in force by base year",
  delta_dr_10   = "projected ten-year change in the TOTAL dependency ratio (children + pensioners per 1,000 working-age persons), same vintages",
  basis_pre1975 = "indicator = 1 before 1975: the pre-1975 observations of mig_rate are non-British rather than non-EU citizens",
  L_tax_workpop = "national accounts taxes per working-age person, GBP 000, 2024-25 prices, t-1",
  L_ln_debt_gdp = "annual log change in public sector net debt / GDP, t-1",
  L_unem        = "unemployment rate, %, t-1: ILO measure (LFS; BoE Millennium A50 before 1975) -- the baseline measure",
  L_unem_boe    = "unemployment rate, %, t-1: Bank of England spliced series (Millennium M5, col G)",
  L_unem_claim  = "claimant-count unemployment rate, %, t-1: claimants of unemployment-related benefits (Millennium A50; ONS BCJE from 1971, Ministry of Labour Gazette before, rate on the A50 workforce), 1945-2016",
  L_real_eri    = "real effective exchange rate of sterling, 1913 = 100, t-1 (Millennium A33)",
  L_ca_gdp      = "current account balance of the balance of payments, % of GDP, t-1 (Millennium A36; negative = deficit)",
  L_trade_bal   = "visible trade balance, % of GDP, t-1 (Millennium A36)",
  L_labour_share= "labour share of GDP excluding rents, %, t-1 (Millennium A56)",
  L_tot         = "terms of trade (export over import prices), 2013 = 100, t-1 (Millennium A35)",
  L_real_house  = "real house-price growth, %, t-1 (Millennium A32 house prices less GDP-deflator inflation)",
  L_unem_head   = "the headline unemployment rate of the day, %, t-1: administrative/claimant count as published (Millennium M5, Denman and Macdonald, 1996) to 1997, the LFS (ILO) rate from 1998, when the ONS made it the headline",
  L_post98      = "indicator from 1999: L_unem_head measured by the ILO rate (step for the change of headline measure)",
  inf_exp_1y    = "one-year-ahead inflation expectation, spliced composite, start of year t (Millennium Q7, Q1 value)",
  inf_exp_5_10y = "inflation expectation 5-10 years ahead, start of year t (Millennium Q7, Q1 value; from 1985)",
  L_vac_wap     = "notified vacancies per 1,000 working-age persons, t-1 (Millennium M5)",
  L_vac_unem    = "vacancies / unemployed ratio, t-1 (M5 vacancies over A50 unemployed)",
  L_dln_vac_unem= "annual change in log(vacancies / unemployed), t-1",
  L_emig_all    = "emigrants of all citizenships per 1,000 working-age persons, t-1 (LTIM 2.00)",
  L_emig_brit   = "British-citizen emigrants per 1,000 working-age persons, t-1 (LTIM 2.00)",
  cit_grants_k  = "grants of British citizenship made in the UK, thousands, year t (Home Office cz_03; Sam, 28 Sep 2026: grants enter contemporaneously, not lagged)",
  cit_nat_k     = "naturalisations as British citizens (on the basis of residence or marriage), thousands, year t (cz_03)",
  cit_reg_k     = "registrations as British citizens (minor children and other grounds), thousands, year t (cz_03)",
  cit_overseas_k= "grants of British citizenship made outside the UK, thousands, year t (cz_04; 1962-82 only)",
  L_cit_grants_k= "grants of British citizenship made in the UK, thousands, t-1 (Home Office citizenship table cz_03; 1962-2018)",
  L_cit_nat_k   = "naturalisations as British citizens (on the basis of residence or marriage), thousands, t-1 (Home Office cz_03)",
  L_cit_reg_k   = "registrations as British citizens (minor children and other grounds), thousands, t-1 (cz_03): the descent and discretionary channel",
  L_cit_overseas_k = "grants of British citizenship made outside the UK, thousands, t-1 (cz_04; 1962-82 only)",
  brit_noneu    = "log inflow of British citizens arriving from outside the EU per 1,000 working-age persons (t-1), IPS 3.01 (from 1975)",
  L_brit_noneu  = "brit_noneu lagged one year",
  brit_oce      = "log inflow of British citizens from Oceania per 1,000 working-age persons (t-1), IPS 3.01",
  brit_nor      = "log inflow of British citizens from North America per 1,000 working-age persons (t-1), IPS 3.01",
  brit_oth      = "log inflow of British citizens from other (non-EU) Europe per 1,000 working-age persons (t-1), IPS 3.01",
  brit_asi      = "log inflow of British citizens from Asia per 1,000 working-age persons (t-1), IPS 3.01",
  brit_sub      = "log inflow of British citizens from Sub-Saharan Africa per 1,000 working-age persons (t-1), IPS 3.01",
  brit_res      = "log inflow of British citizens from the rest of the world per 1,000 working-age persons (t-1), IPS 3.01",
  brit_rate     = "log inflow of British citizens per 1,000 working-age persons (t-1), all countries of origin (LTIM 2.00)",
  L_brit_rate   = "brit_rate lagged one year",
  L_wage_infl   = "growth of average weekly earnings, %, t-1 (Millennium M6)",
  L_real_wage   = "wage_infl minus GDP-deflator inflation, t-1",
  L_tbill       = "three-month Treasury bill rate, %, annual average, t-1 (Millennium M1; Bank of England IUMAAJNB/IUMABEDR for 2017-19)",
  L_real_rate   = "tbill minus GDP-deflator inflation, t-1",
  st_inf_exp    = "one-year-ahead inflation expectation at the start of year t, % (NIESR)",
  lt_inf_exp    = "long-term inflation expectation at the start of year t, % (NIESR)",
  L_tfp         = "total factor productivity growth, %, t-1",
  L_replace     = "benefit replacement ratio, t-1 (Millennium A52b)",
  L_union       = "trade union density, %, t-1 (Millennium A52b)",
  L_tax_inc     = "central government income tax receipts per working-age person, GBP 000, 2024-25 prices, t-1 (Millennium A27; FY t-1/t assigned to t)",
  L_tax_nics    = "National Insurance contributions per working-age person, GBP 000, 2024-25 prices, t-1 (A27; employee + employer)",
  L_tax_nics_emp= "employee National Insurance contributions per working-age person, GBP 000, 2024-25 prices, t-1 (A27)",
  L_tax_nics_er = "employer National Insurance contributions per working-age person, GBP 000, 2024-25 prices, t-1 (A27)",
  L_tax_inc_nics= "income tax + National Insurance contributions per working-age person, t-1 (A27)",
  L_tax_direct  = "direct taxes per working-age person, t-1 (A27)",
  L_tax_indirect= "taxes on products and production per working-age person, t-1 (A27)",
  L_tax_vat     = "VAT per working-age person, t-1 (A27; from 1975)",
  L_tax_corp    = "corporation tax per working-age person, t-1 (A27)",
  L_tax_receipts= "total central government receipts per working-age person, t-1 (A27)",
  L_tax_res     = "national accounts taxes per resident, GBP 000, 2024-25 prices, t-1",
  L_tax_lab     = "labour taxes (income tax and National Insurance contributions) per working-age person, GBP 000, 2024-25 prices, t-1",
  oadr_10       = "projected LEVEL of the old-age dependency ratio ten years ahead (pensioners per 1,000 working-age persons), same vintages: the current ratio plus delta_oadr_10",
  oadr_0        = "old-age dependency ratio in the base year of the vintage in force (the current ratio)",
  delta_oadr_1  = "projected one-year change in the old-age dependency ratio, same vintages",
  delta_oadr_3  = "projected three-year change in the old-age dependency ratio, same vintages",
  delta_oadr_5  = "projected five-year change in the old-age dependency ratio, same vintages",
  delta_oadr_7  = "projected seven-year change in the old-age dependency ratio, same vintages",
  lever         = "work permits and first permissions plus Commonwealth employment vouchers (1962-72), thousands",
  L_lever       = "lever lagged one year",
  L_ln_wap      = "log working-age population, t-1 (Kronmal, 1993: the denominator of mig_rate as its own regressor)",
  L_grants_gdp  = "current grants to the personal sector (cash social benefits), % of GDP, t-1 (Millennium A28)",
  L_grants_wap  = "current grants to the personal sector per working-age person, GBP 000, 2024-25 prices, t-1 (A28)",
  L_tme_gdp     = "total managed expenditure, % of GDP, t-1 (A28)",
  L_int_gdp     = "public sector debt interest, % of GDP, t-1 (A28)",
  L_int_tme     = "public sector debt interest, % of total managed expenditure, t-1 (A28)",
  L_gilt        = "yield on 10-year British government securities, %, annual average, t-1 (Millennium A31; to 2016)",
  L_r_g         = "10-year gilt yield minus nominal GDP growth, percentage points, t-1",
  L_days_lost   = "working days lost through industrial disputes per working-age person, t-1 (A52b)",
  L_psnb_gdp    = "public sector net borrowing, % of GDP, t-1 (OBR HPFD; negative = surplus)",
  L_psnd_gdp    = "public sector net debt, % of GDP, t-1 (OBR HPFD)",
  L_tax_gdp     = "national accounts taxes, % of GDP, t-1 (OBR HPFD)",
  fc_budget     = "spring Budget forecast (year t) of public sector net borrowing for the coming financial year, % of GDP (OBR historical official forecasts; PSBR before 1994; GBP forecasts before 1977 over the latest GDP outturn)",
  L_fc_budget   = "fc_budget lagged one year",
  fc_planned    = "fc_budget minus the latest borrowing outturn: the change in borrowing the Budget planned",
  fc_medium     = "the same Budget's furthest-horizon borrowing forecast (1-4 years ahead), % of GDP",
  gad_earn      = "Government Actuary: projected change over the next ten years in the joint Class 1 contribution rate required to balance the National Insurance Fund with earnings-linked benefits (percentage points; % of contribution income for the flat-rate reviews of 1960-72), Quinquennial Review in force by base year",
  gad_policy    = "as gad_earn, under the uprating policy in force (price indexation 1980-2010, triple lock after)",
  gad_gap       = "Government Actuary: contribution rate required in ten years minus the rate actually in force, percentage points",
  eu_rate       = "log inflow of EU citizens per 1,000 working-age persons (t-1)",
  L_eu_rate     = "eu_rate lagged one year",
  permits       = "work permits and first permissions issued, thousands (Bauer and Salt, 2020)",
  L_permits     = "permits lagged one year",
  trend         = "linear trend (1965 = 0)",
  post1968      = "indicator from 1968: Commonwealth Immigrants Act 1968",
  post1973      = "indicator from 1973: Immigration Act 1971 in force on 1 January 1973 (Commonwealth workers move from vouchers to permits; EEC accession the same day)",
  L_growth      = "real GDP growth, %, t-1 (ONS ABMI)",
  L_inflation   = "GDP-deflator inflation, %, t-1 (HM Treasury)",
  L_cpi_infl    = "CPI inflation, %, t-1 (Millennium A47, preferred CPI measure; ONS D7G7 for 2017-19)",
  L_bank_rate   = "Bank Rate (Minimum Lending Rate 1972-81, minimum band 1 dealing rate 1981-97, repo rate 1997-2006), %, annual average, t-1 (Millennium A31; to 2016)",
  L_consol      = "yield on consols (perpetual government stock; 2016+ the longest-dated gilt), %, t-1 (Millennium A31; to 2016)",
  L_growth_pc   = "real GDP growth per head of population, %, t-1",
  growth_fc     = "HM Treasury forecast of real GDP growth for year t, made at the start of t, % (workbook, from 1976)",
  L_lab_prod    = "growth of output per hour worked, %, t-1 (Millennium A52b; to 2016)",
  L_tax_earn    = "national accounts taxes in millions of average annual earnings: real taxes / (52 x real average weekly earnings), t-1 (OBR HPFD; Millennium A47 AWE, KAB9 for 2017-19)",
  level_oadr_10 = "projected LEVEL of the old-age dependency ratio ten years ahead (pensioners per 1,000 working-age persons), same vintages: the current ratio plus delta_oadr_10",
  level_dr_10   = "projected LEVEL of the TOTAL dependency ratio ten years ahead (children + pensioners per 1,000 working-age persons), same vintages",
  tot_rate      = "log inflow of all citizens (EU + non-EU) per 1,000 working-age persons (t-1), IPS, from 1975",
  L_tot_rate    = "tot_rate lagged one year",
  L_openness    = "trade openness: exports plus imports of goods and services, % of GDP, t-1 (Millennium A36/A9; to 2016)",
  L_m4_growth   = "broad money growth, %, t-1 (Millennium A24 spliced M3/M4/M4x, break-adjusted, year end; to 2016)",
  L_particip    = "workforce (employed + unemployed) per 100 working-age persons, t-1 (Millennium A50; to 2016)",
  L_tax_ceiling = "national accounts taxes, % of GDP, minus their highest value over the previous ten years, t-1 (OBR HPFD): distance from the revenue ceiling",
  L_tme_real_growth = "real growth of total managed expenditure, % (log change less GDP-deflator inflation), t-1 (A28; to 2016)",
  boe_indep     = "indicator from 1998: Bank of England operational independence (1997 announcement, 1998 Act)",
  golden_rule   = "indicator 1998-2009: the golden rule and sustainable investment rule in force",
  obr_era       = "indicator from 2010: Office for Budget Responsibility and the fiscal mandate",
  L_relinc_dev  = "log UK GDP per head minus its mean over India, Pakistan, Bangladesh, China, Nigeria, the Philippines and South Africa, t-1 (Maddison Project Database 2023)",
  L_relinc_rich = "log UK GDP per head minus its mean over the US and Australia, t-1 (Maddison)",
  L_youth       = "mean share aged 15-24 in the seven origin countries, %, t-1 (UN World Population Prospects 2024)"
)
# Definitions are printed ONCE per document run (Sam, 28 Sep 2026): the body text defines the baseline names (BODY_DEFINED); every
# other name is defined under the first panel that shows it and never again. The set of names already defined lives in the global
# environment (DEFINED) so it carries across the scripts of one run_all; a script run on its own starts from BODY_DEFINED.
BODY_DEFINED <- c("mig_rate", "L_mig_rate", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "basis_pre1975")
if (!exists("DEFINED", envir = globalenv())) assign("DEFINED", BODY_DEFINED, envir = globalenv())
for (.v in names(DEFS)) if (startsWith(.v, "L_") && !startsWith(.v, "L_ln_")) { .ln <- sub("^L_", "L_ln_", .v); if (!(.ln %in% names(DEFS))) DEFS[[.ln]] <- paste("log of", DEFS[[.v]]) }
defs <- function(vars) { v <- unique(vars); v <- v[v %in% names(DEFS) & !(v %in% get("DEFINED", envir = globalenv()))]; if (!length(v)) return(invisible())
  cat("Definitions: ", paste(sprintf("%s = %s", v, DEFS[v]), collapse = "; "), ".\n", sep = "")
  assign("DEFINED", union(get("DEFINED", envir = globalenv()), v), envir = globalenv()) }

# fit(): OLS on complete cases; constant columns dropped (e.g. basis_pre1975 when the sample starts in 1975)
fit <- function(d, xs, dv = "mig_rate", min_extra = 5) {
  if (!all(c(dv, xs) %in% names(d))) return(NULL); cc <- complete.cases(d[, c(dv, xs)]); dd <- d[cc, ]
  xs <- xs[vapply(xs, function(v) sd(dd[[v]]) > 0, logical(1))]
  m <- lm(reformulate(xs, dv), data = dd); if (nobs(m) < length(xs) + min_extra) return(NULL); attr(m, "dd") <- dd; m }
hacF <- function(m, term = "delta_oadr_10") if (term %in% names(coef(m))) { lh <- linearHypothesis(m, paste0(term, " = 0"), vcov. = vcovHAC(m)); c(lh$F[2], lh$`Pr(>F)`[2]) } else c(NA, NA)

# panel(): several models side by side -- focal coefficients with HAC p-values, a diagnostics block,
# then the definitions of every variable shown and any extra notes.
panel <- function(models, title, rows, notes = NULL, w = 14, lw = 16, reset = TRUE, nested_F = FALSE) {
  models <- Filter(Negate(is.null), models); if (!length(models)) { cat("\n", title, ": no estimable column\n"); return(invisible()) }
  w <- max(w, max(nchar(names(models))) + 2)   # column titles never run together
  cat(sprintf("\n===== %s =====\n\n", title)); cts <- lapply(models, function(m) coeftest(m, vcov. = vcovHAC(m)))
  hdr <- sprintf("%-*s%s", lw, "", paste(sprintf("%*s", w, names(models)), collapse = "")); rule <- strrep("-", nchar(hdr)); cat(hdr, "\n", rule, "\n", sep = "")
  shown <- character(0)
  for (v in rows) { if (!any(vapply(cts, function(ct) v %in% rownames(ct), logical(1)))) next; shown <- c(shown, v)
    cell <- function(ct, what) { if (!(v %in% rownames(ct))) return(if (what == "b") "--" else ""); if (what == "b") sprintf("%.4f%s", ct[v, 1], stars(ct[v, 4])) else paste0("(", pf(ct[v, 4]), ")") }
    cat(sprintf("%-*s%s\n", lw, v, paste(sprintf("%*s", w, vapply(cts, cell, character(1), what = "b")), collapse = "")))
    cat(sprintf("%-*s%s\n", lw, "", paste(sprintf("%*s", w, vapply(cts, cell, character(1), what = "p")), collapse = ""))) }
  cat(rule, "\n", sep = "")
  stat <- function(lab, f) cat(sprintf("%-*s%s\n", lw, lab, paste(vapply(models, function(m) sprintf("%*s", w, tryCatch(f(m), error = function(e) "--")), character(1)), collapse = "")))
  stat("N", function(m) sprintf("%d", nobs(m))); stat("adj. R2", function(m) sprintf("%.4f", summary(m)$adj.r.squared)); stat("AIC", function(m) sprintf("%.2f", AIC(m)))
  if (reset) stat("RESET p", function(m) pf(resettest(m)$p.value))
  stat("BG(1) p", function(m) pf(bgtest(m, order = 1)$p.value)); stat("max VIF", function(m) sprintf("%.1f", max(vif(m))))
  stat("HAC F (proj)", function(m) { a <- hacF(m); if (is.na(a[1])) "--" else sprintf("%.2f", a[1]) }); stat("  p", function(m) { a <- hacF(m); if (is.na(a[1])) "" else paste0("(", pf(a[2]), ")") })
  if (nested_F) { nf <- function(m) { if (!("delta_oadr_10" %in% names(coef(m)))) return(c(NA, NA, NA)); r <- lm(update(formula(m), . ~ . - delta_oadr_10), data = model.frame(m)); a <- anova(r, m); c(a$F[2], a$`Pr(>F)`[2], AIC(m) - AIC(r)) }
    stat("F vs no proj", function(m) sprintf("%.2f", nf(m)[1])); stat("  p", function(m) paste0("(", pf(nf(m)[2]), ")")); stat("  dAIC", function(m) sprintf("%+.2f", nf(m)[3])) }
  sec <- function(m) { dd <- attr(m, "dd"); if (is.null(dd) || !("year" %in% names(dd)) || max(dd$year) <= SEC_END || !any(ONS_VARS %in% names(coef(m)))) return(NULL)
    d2 <- subset(dd, year <= SEC_END); xs <- attr(terms(m), "term.labels"); xs <- xs[vapply(xs, function(v) sd(d2[[v]]) > 0, logical(1))]
    lm(reformulate(xs, all.vars(formula(m))[1]), data = d2) }
  secs <- lapply(models, sec)
  if (any(!vapply(secs, is.null, logical(1)))) {
    stat(paste0("proj, to ", SEC_END), function(m) { s2 <- sec(m); if (is.null(s2)) return("same"); a <- coeftest(s2, vcov. = vcovHAC(s2)); if (!("delta_oadr_10" %in% rownames(a))) return("--"); sprintf("%.4f%s", a["delta_oadr_10", 1], stars(a["delta_oadr_10", 4])) })
    stat(paste0("  N, to ", SEC_END), function(m) { s2 <- sec(m); if (is.null(s2)) "" else sprintf("%d", nobs(s2)) }) }
  cat(rule, "\nHAC (vcovHAC) p-values in parentheses. *** p<0.01, ** p<0.05, * p<0.10. HAC F: Wald test of delta_oadr_10 under the same covariance",
      if (nested_F) "; F vs no proj: classical nested F against the column without delta_oadr_10." else ".", "\n", sep = "")
  if (any(!vapply(secs, is.null, logical(1)))) cat("proj, to ", SEC_END, ": delta_oadr_10 re-estimated without the ONS-extended 2017-19 values ('same' = the column uses no extended series).\n", sep = "")
  dvs <- unique(vapply(models, function(m) all.vars(formula(m))[1], character(1))); defs(c(dvs, shown)); if (!is.null(notes)) cat(notes, sep = "\n")
  invisible(models)
}
