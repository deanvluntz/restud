# =============================================================================
# replication_core_spec.R -- FULL REPLICATION of the core specification in the agreed order (Sam, 29 Sep 2026). For inspection and
# refinement before it replaces the manuscript scripts. Nothing here writes to any document.
#
# Core specification (7 parameters):  mig_gross_t = a + rho mig_gross_{t-1} + b delta_oadr_10_t + c L_tax_workpop + d L_ln_debt_gdp
#                                                   + e L_ln_vac_unem + f L_ln_int_tme + u_t
#   mig_gross      log gross inflow of non-EU citizens from non-EU residence (IPS 3.01), persons; before 1975 non-British citizens
#                  (LTIM 2.00) on the extended sample, no basis step
#   delta_oadr_10  projected 10-year change in the old-age dependency ratio, latest projection with base year <= t
#   L_tax_workpop  national accounts taxes per working-age person, GBP 000 at 2024-25 prices, t-1 (revenue / political constraint)
#   L_ln_debt_gdp  annual log change in public sector net debt / GDP, t-1 (long-term capacity)
#   L_ln_vac_unem  log of notified vacancies / unemployment, t-1 (labour market; contains unemployment)
#   L_ln_int_tme   log of public sector debt interest / TME, t-1, Millennium vintage (short-term fiscal capacity)
# Order of the robustness checks (Sam): the broadest measure first, then narrower and more precise sources as they become available:
#   1 main results on both samples (extended 1965-2019 first, core 1977-2019 as corroboration)
#   2 the dependent variable: citizenship basis across samples -> citizenship vs country of last residence -> work permits -> IPS by main
#     reason (separate panel; British by reason beside it) -> IPS vs LTIM
#   3 the regressors: tax measure, labour-market term, debt-interest term, wage inflation and the balance of payments (each and both),
#     level vs change and horizon of the projection, total-DR placebo
#   4 uncontrolled flows: EU inflows, British inflows (core controls; + output gap; + destination unemployment; British not born in the UK)
#   5 the model without the projection: nested F, diagnostics, out-of-sample forecasts
#   6 final diagnostics (BOOT <- TRUE before sourcing: recursive bootstrap of the projection's t and bootstrap RESET; EXACT <- TRUE: exact
#     ARDL bounds critical values). Both off by default (slow).
# DATA: Dissertation_Dataset_Final.xlsx only (sheets data_1964, data_1977, ReformSPA, PubShiftOADR_ZM). RStudio: working directory =
# diss_replication/ (Session > Set Working Directory > To Source File Location), then source this file. Packages: readxl, lmtest,
# sandwich, car, ARDL, strucchange, tseries. Runtime ~2 minutes without BOOT/EXACT.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/ (RStudio: Session > Set Working Directory > To Source File Location)")
if (!exists("BOOT")) BOOT <- FALSE; if (!exists("EXACT")) EXACT <- FALSE
# --- what this run reads (printed so a failure can be diagnosed at a glance) --------------------------------------------------------------
suppressPackageStartupMessages(library(readxl)); .wb <- "Dissertation_Dataset_Final.xlsx"; if (!file.exists(.wb)) stop("Dissertation_Dataset_Final.xlsx is not in the working directory ", getwd())
.sh <- excel_sheets(.wb); cat("working directory:", getwd(), "\nworkbook:", normalizePath(.wb), "(", format(file.info(.wb)$mtime), ")\nsheets:", paste(.sh, collapse = ", "), "\n")
if (!all(c("data_1964", "data_1977") %in% .sh)) stop("The workbook lacks the assembled data sheets data_1964 / data_1977. They are written by export_dataset.R and committed with the workbook: pull the latest Dissertation_Dataset_Final.xlsx from the repository (a local copy of the workbook that was never replaced by the pulled one is the usual cause).")
options(warn = 1)
source("tables_common.R"); suppressPackageStartupMessages({library(ARDL); library(strucchange); library(tseries); library(readxl)})
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); df$ln_int_tme <- log(df$int_tme)
  for (k in c("res_noneu_k", "cit_noneu_k", "dv_noneu_k", "permits", "settle_total_k", "settle_push_k", "settle_isc_k")) if (k %in% names(df)) { x <- df[[k]]; x[!is.na(x) & x <= 0] <- NA; nm <- paste0("ln_", sub("_k$", "", k)); df[[nm]] <- log(1000 * x); df[[paste0("L_", nm)]] <- lag1(df[[nm]]) }
  for (v in c("noneu", "brit")) for (r in c("work", "study", "join", "other", "all", "nonwork")) { k <- paste0(v, "_", r, "_k"); if (k %in% names(df)) { x <- df[[k]]; x[!is.na(x) & x <= 0] <- NA; df[[paste0("ln_", v, "_", r)]] <- log(1000 * x); df[[paste0("L_ln_", v, "_", r)]] <- lag1(df[[paste0("ln_", v, "_", r)]]) } }
  df$ltim_gross <- df$ltim_rate + log(lag1(df$workpop) / 1000) - log(1000); df$L_ltim_gross <- lag1(df$ltim_gross)   # LTIM non-EU inflows per WAP back to persons (log), 1991-
  list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme"); G <- c("L_mig_gross", "delta_oadr_10", CT); NP <- setdiff(G, "delta_oadr_10")
PUBCT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
DEFS[["ln_res_noneu"]] <- "log inflow of ALL citizenships arriving from non-EU countries of last residence, persons (IPS 3.01; the residence basis)"; DEFS[["L_ln_res_noneu"]] <- "ln_res_noneu, t-1"
DEFS[["ln_cit_noneu"]] <- "log inflow of non-EU citizens arriving from ANY country of last residence, persons (IPS 3.01; the citizenship basis)"; DEFS[["L_ln_cit_noneu"]] <- "ln_cit_noneu, t-1"
DEFS[["ln_dv_noneu"]] <- "log inflow of non-EU citizens from non-EU residence, persons (IPS 3.01) -- the dependent variable from 1975"; DEFS[["L_ln_dv_noneu"]] <- "ln_dv_noneu, t-1"
DEFS[["ln_permits"]] <- "log work permits and first permissions issued, persons (Bauer and Salt, 2020, Fig. 1; 1946-2008)"; DEFS[["L_ln_permits"]] <- "ln_permits, t-1"; DEFS[["permits"]] <- "work permits and first permissions issued, thousands (Bauer and Salt, 2020, Fig. 1; 1946-2008)"; DEFS[["L_permits"]] <- "permits, t-1"
DEFS[["ltim_gross"]] <- "log inflow of non-EU citizens on the LTIM basis (IPS adjusted for asylum seekers, visitor and migrant switchers), persons, 1991-2019"; DEFS[["L_ltim_gross"]] <- "ltim_gross, t-1"
DEFS[["eu_rate"]] <- "log inflow of EU citizens per 1,000 working-age persons, t-1 denominator (IPS 3.01, from 1975)"; DEFS[["L_eu_rate"]] <- "eu_rate, t-1"
DEFS[["brit_desc_noneu"]] <- "log inflow of British citizens from non-EU residence NOT born in the UK (IPS 3.01 minus 3.18) per 1,000 working-age persons"
for (h in c(3, 5, 7)) DEFS[[paste0("delta_oadr_", h)]] <- paste0("projected ", h, "-year change in the old-age dependency ratio, same vintages"); DEFS[["level_oadr_10"]] <- "projected old-age dependency ratio ten years ahead, level"
hac <- function(m) coeftest(m, vcov. = vcovHAC(m)); pf2 <- function(p) if (p < .001) "<.001" else sprintf("%.3f", p)
bounds <- function(S, xs, dv = "mig_gross") { lev <- sub("^L_", "", setdiff(xs, paste0("L_", dv))); vv <- c(dv, lev); ok <- complete.cases(S[, vv]); ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  m_ardl <- ardl(as.formula(paste(dv, "~", paste(lev, collapse = " + "))), data = ts_df, order = rep(1, length(lev) + 1)); bf <- bounds_f_test(m_ardl, case = 3, exact = EXACT); bt <- bounds_t_test(m_ardl, case = 3, exact = EXACT); lr <- multipliers(m_ardl, type = "lr")
  list(F = unname(bf$statistic), Fp = unname(bf$p.value), t = unname(bt$statistic), tp = unname(bt$p.value), lr = lr[lr$Term == "delta_oadr_10", ]) }
oos <- function(S, xs, dv, from) { e <- c(); for (t in from:2019) { tr <- S[S$year < t, ]; te <- S[S$year == t, ]; tr <- tr[complete.cases(tr[, c(dv, xs)]), ]; if (!nrow(te) || anyNA(te[, xs])) next; mm <- lm(reformulate(xs, dv), data = tr); e <- c(e, te[[dv]] - predict(mm, newdata = te)) }; c(rmse = sqrt(mean(e^2)), n = length(e)) }
diag_line <- function(m) { dd <- attr(m, "dd"); cat(sprintf("    RESET(2,3) %s | RESET(2) %s | BG(1) %s | BG(2) %s | rainbow %s | Breusch-Pagan %s | Jarque-Bera %s | OLS-CUSUM %s | max VIF %.1f | AIC %.2f | N %d\n",
  pf2(resettest(m)$p.value), pf2(resettest(m, power = 2)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value), pf2(raintest(m)$p.value), pf2(bptest(m)$p.value), pf2(jarque.bera.test(residuals(m))$p.value),
  pf2(sctest(efp(formula(m), data = dd, type = "OLS-CUSUM"))$p.value), max(vif(m)), AIC(m), nobs(m))) }
sub_s <- function(k, to = 2019) subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= to)
# =============================================================================
cat("\n\n#################### 1. MAIN RESULTS ####################\n")
for (k in names(SMP)) { S <- sub_s(k); LAB <- SMP[[k]]$LAB
  p <- list("main" = fit(S, G, "mig_gross"), "no projection" = fit(S, NP, "mig_gross"), "no ln_int_tme" = fit(S, setdiff(G, "L_ln_int_tme"), "mig_gross"), "unem for ln_vac_unem" = fit(S, c(setdiff(G, "L_ln_vac_unem"), "L_unem"), "mig_gross"),
            "published spec (mig_rate)" = fit(S, c("L_mig_rate", "delta_oadr_10", PUBCT, if (k == "extended") "basis_pre1975"), "mig_rate"))
  panel(p, paste0("Table 1. The core specification (", LAB, "2019)"), c("delta_oadr_10", CT, "L_unem", "L_mig_gross", "L_mig_rate", "basis_pre1975"), w = 16, lw = 16, nested_F = TRUE,
        "main: the core specification; no projection: the same without delta_oadr_10 (F vs no proj tests the projection); the last column is the published specification (inflows per working-age person, unemployment, basis step on the extended sample) for comparison.")
  m <- p[["main"]]; ct <- hac(m); cat("  main diagnostics:\n"); diag_line(m); cat("  no projection diagnostics:\n"); diag_line(p[["no projection"]])
  b <- bounds(S, G); cat(sprintf("  ARDL(1,...) bounds test, PSS case 3%s: F = %.2f (p %s), t = %.2f (p %s); long-run projection %.4f (p %s)\n", if (EXACT) " (exact)" else " (asymptotic)", b$F, pf2(b$Fp), b$t, pf2(b$tp), b$lr$Estimate, pf2(b$lr$`Pr(>|t|)`)))
  sdp <- sd(S$delta_oadr_10[complete.cases(S[, c("mig_gross", G)])]); cat(sprintf("  one SD of the projection (%.2f) raises gross inflows by %.1f%% on impact and %.1f%% in the long run\n", sdp, 100 * (exp(ct["delta_oadr_10", 1] * sdp) - 1), 100 * (exp(b$lr$Estimate * sdp) - 1)))
  from <- if (k == "extended") 1990 else 1997; o1 <- oos(S, G, "mig_gross", from); o0 <- oos(S, NP, "mig_gross", from); cat(sprintf("  recursive one-step forecasts %d-2019 (n %d): RMSE main %.4f, no projection %.4f, ratio %.2f\n", from, o1["n"], o1["rmse"], o0["rmse"], o1["rmse"] / o0["rmse"])) }
# =============================================================================
cat("\n\n#################### 2. THE DEPENDENT VARIABLE: broadest measure first, narrower sources as robustness ####################\n")
cat("\n--- 2.1 Citizenship basis across the two samples (non-British citizens before 1975, non-EU citizens from 1975) ---\n")
for (k in names(SMP)) { S <- sub_s(k); p <- list("main" = fit(S, G, "mig_gross"))
  if (k == "extended") { p[["+basis_pre1975"]] <- fit(S, c(G, "basis_pre1975"), "mig_gross"); p[["mig_gross_s (rescaled)"]] <- fit(S, c("L_mig_gross_s", "delta_oadr_10", CT), "mig_gross_s"); p[["non-British to 2019"]] <- fit(S, c("L_nb_rate", "delta_oadr_10", CT), "nb_rate"); p[["non-British to 2003"]] <- fit(subset(S, year <= 2003), c("L_nb03_rate", "delta_oadr_10", CT), "nb03_rate") }
  else { p[["non-British (nb_rate)"]] <- fit(S, c("L_nb_rate", "delta_oadr_10", CT), "nb_rate"); p[["non-British to 2003"]] <- fit(subset(S, year <= 2003), c("L_nb03_rate", "delta_oadr_10", CT), "nb03_rate") }
  panel(p, paste0("Table 2.1. The dependent variable under each citizenship classification (", SMP[[k]]$LAB, "2019)"), c("delta_oadr_10", "basis_pre1975", CT), w = 16, lw = 16, nested_F = TRUE,
        "nb_rate: non-British citizens (LTIM 2.00) per 1,000 working-age persons throughout (EU citizens included, so the projection is diluted after the 2004 accession); nb03: the same to 2003; mig_gross_s: the pre-1975 series rescaled by the 1975-80 overlap ratio instead of unadjusted.") }
cat("\n--- 2.2 Citizenship versus country of last residence (core sample; IPS 3.01 from 1975) ---\n")
S <- sub_s("core"); p <- list("main (non-EU from non-EU)" = fit(S, G, "mig_gross"), "residence basis (all citizens)" = fit(S, c("L_ln_res_noneu", "delta_oadr_10", CT), "ln_res_noneu"), "citizenship basis (any residence)" = fit(S, c("L_ln_cit_noneu", "delta_oadr_10", CT), "ln_cit_noneu"))
panel(p, "Table 2.2. Citizenship basis against the country-of-last-residence basis (core sample, 1977-2019)", c("delta_oadr_10", CT), w = 18, lw = 16, nested_F = TRUE,
      "The dependent variable is the intersection of the two margins: non-EU citizens arriving from non-EU countries. Column 2 counts everyone arriving from a non-EU country (British and EU citizens included), the residence basis standard in the literature; column 3 counts non-EU citizens arriving from anywhere.")
cat("\n--- 2.3 Work permits as the outcome (Bauer and Salt, 2020; 1946-2008, WP + first permissions) ---\n")
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2008); p <- list("main (to 2008)" = fit(S, G, "mig_gross"), "permits (k)" = fit(S, c("L_permits", "delta_oadr_10", CT), "permits"), "log permits" = fit(S, c("L_ln_permits", "delta_oadr_10", CT), "ln_permits"))
  panel(p, paste0("Table 2.3. Work permits (", SMP[[k]]$LAB, "2008)"), c("delta_oadr_10", CT), w = 16, lw = 16, nested_F = TRUE, "permits: the administrative policy lever, gross permits in thousands (levels) and logs, on the same right-hand side; Tier 2 (2009-) is a different basis and is never spliced.") }
cat("\n--- 2.3b Grants of settlement as the outcome (Home Office Control of Immigration Statistics, Table 5.8 of the 2006 volume; 1960-2006) ---\n")
for (k in names(SMP)) { S <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2006); p <- list("main (to 2006)" = fit(S, G, "mig_gross"), "settlement (log)" = fit(S, c("L_ln_settle_total", "delta_oadr_10", CT), "ln_settle_total"), "settlement, no lag" = fit(S, c("delta_oadr_10", CT), "ln_settle_total"))
  panel(p, paste0("Table 2.3b. Grants of settlement, all nationalities (", SMP[[k]]$LAB, "2006)"), c("delta_oadr_10", CT, "L_ln_settle_total"), w = 16, lw = 16, nested_F = TRUE,
        "Settlement is the administrative count of permanent admissions of persons subject to control, the narrowest outcome; before the mid-1970s it is dominated by the Commonwealth Immigrants Act regime (dependants and voucher holders), which the extended-sample column shows.") }
cat("\n--- 2.4 IPS by main reason for migration (core sample, 1977-2019): a separate panel ---\n")
S <- sub_s("core"); p <- list(); for (r in c("all", "work", "study", "join", "other")) { dv <- paste0("ln_noneu_", r); p[[paste0("non-EU ", r)]] <- fit(S, c(paste0("L_", dv), "delta_oadr_10", CT), dv) }
panel(p, "Table 2.4a. Non-EU inflows by main reason for migration (core sample, 1977-2019)", c("delta_oadr_10", CT), w = 15, lw = 16, nested_F = TRUE, "Each column: the log inflow of non-EU citizens giving the reason named (IPS 3.08), on its own lag, the projection and the core controls. The work route is the one the Treasury controls.")
p <- list(); for (r in c("all", "work", "study", "nonwork")) { dv <- paste0("ln_brit_", r); p[[paste0("British ", r)]] <- fit(S, c(paste0("L_", dv), "delta_oadr_10", CT), dv) }
panel(p, "Table 2.4b. British inflows by main reason for migration (core sample, 1977-2019)", c("delta_oadr_10", CT), w = 15, lw = 16, nested_F = TRUE, "nonwork = all reasons minus work minus study (going home, accompanying or joining, other), the split that is coded consistently from 1977.")
cat("\n--- 2.5 IPS against LTIM (core sample, 1991-2019: the years LTIM adjusts the IPS) ---\n")
S <- subset(SMP$core$df, year >= 1991 & year <= 2019); p <- list("main (1991-2019)" = fit(S, G, "mig_gross"), "LTIM basis" = fit(S, c("L_ltim_gross", "delta_oadr_10", CT), "ltim_gross"))
panel(p, "Table 2.5. IPS against LTIM non-EU inflows (core sample, 1991-2019)", c("delta_oadr_10", CT), w = 16, lw = 16, nested_F = TRUE, "LTIM adds asylum seekers and visitor/migrant switchers to the IPS count; available from 1991.")
# =============================================================================
cat("\n\n#################### 3. THE REGRESSORS ####################\n")
for (k in names(SMP)) { S <- sub_s(k); LAB <- SMP[[k]]$LAB; sw <- function(drop, add) c(setdiff(G, drop), add)
  p <- list("main (tax_workpop)" = fit(S, G, "mig_gross"), "tax_voter" = fit(S, sw("L_tax_workpop", "L_tax_voter"), "mig_gross"), "tax_res" = fit(S, sw("L_tax_workpop", "L_tax_res"), "mig_gross"), "tax_nics_emp" = fit(S, sw("L_tax_workpop", "L_tax_nics_emp"), "mig_gross"))
  panel(p, paste0("Table 3.1. The tax burden per head (", LAB, "2019)"), c("delta_oadr_10", "L_tax_workpop", "L_tax_voter", "L_tax_res", "L_tax_nics_emp", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme"), w = 16, lw = 16, "Each column replaces L_tax_workpop by the measure named.")
  p <- list("main (ln_vac_unem)" = fit(S, G, "mig_gross"), "unem for ln_vac_unem" = fit(S, sw("L_ln_vac_unem", "L_unem"), "mig_gross"), "unem_boe for ln_vac_unem" = fit(S, sw("L_ln_vac_unem", "L_unem_boe"), "mig_gross"), "+unem" = fit(S, c(G, "L_unem"), "mig_gross"), "ln_vac_wap for ln_vac_unem" = fit(S, sw("L_ln_vac_unem", "L_ln_vac_wap"), "mig_gross"))
  panel(p, paste0("Table 3.2. The labour-market term (", LAB, "2019)"), c("delta_oadr_10", "L_ln_vac_unem", "L_unem", "L_unem_boe", "L_ln_vac_wap", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_int_tme"), w = 16, lw = 16,
        "Log V/U contains unemployment; the ILO rate in its place is the published specification's term; +unem adds the rate beside log V/U (the two are near-collinear). Unemployment is spliced across the 1971 and 1992 changes of measure; the splice does not change the results.")
  p <- list("main (ln_int_tme)" = fit(S, G, "mig_gross"), "no ln_int_tme" = fit(S, setdiff(G, "L_ln_int_tme"), "mig_gross"), "int_tme (level)" = fit(S, sw("L_ln_int_tme", "L_int_tme"), "mig_gross"), "ln_int_tme_ons for ln_int_tme" = fit(S, sw("L_ln_int_tme", "L_ln_int_tme_ons"), "mig_gross"), "+psnb_gdp" = fit(S, c(G, "L_psnb_gdp"), "mig_gross"))
  panel(p, paste0("Table 3.3. The debt-interest term (", LAB, "2019)"), c("delta_oadr_10", "L_ln_int_tme", "L_int_tme", "L_ln_int_tme_ons", "L_psnb_gdp", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem"), w = 16, lw = 16,
        "ln_int_tme_ons: the same ratio (ANLO / EBFT) on the current ONS PUSF vintage for every year; the main uses the Millennium vintage, the figures known at the time.")
  p <- list("main" = fit(S, G, "mig_gross"), "+wage_infl" = fit(S, c(G, "L_wage_infl"), "mig_gross"), "+ca_gdp" = fit(S, c(G, "L_ca_gdp"), "mig_gross"), "+wage_infl +ca_gdp" = fit(S, c(G, "L_wage_infl", "L_ca_gdp"), "mig_gross"), "+tot" = fit(S, c(G, "L_tot"), "mig_gross"), "+wage_infl +ca_gdp +tot" = fit(S, c(G, "L_wage_infl", "L_ca_gdp", "L_tot"), "mig_gross"))
  panel(p, paste0("Table 3.4. Wage inflation and the balance of payments (", LAB, "2019)"), c("delta_oadr_10", "L_wage_infl", "L_ca_gdp", "L_tot", CT), w = 16, lw = 16, nested_F = TRUE,
        "The instruments of their era: incomes policy (wage inflation) and the external constraint (current account, terms of trade) were Treasury objectives in the 1960s and 1970s and fade in the later sample; reported as controls of their period, not carried into the specification.")
  p <- list("main (10-year change)" = fit(S, G, "mig_gross"), "3-year change" = fit(S, sw("delta_oadr_10", "delta_oadr_3"), "mig_gross"), "5-year change" = fit(S, sw("delta_oadr_10", "delta_oadr_5"), "mig_gross"), "7-year change" = fit(S, sw("delta_oadr_10", "delta_oadr_7"), "mig_gross"), "level 10 years ahead" = fit(S, sw("delta_oadr_10", "level_oadr_10"), "mig_gross"), "current OADR (level)" = fit(S, sw("delta_oadr_10", "oadr_0"), "mig_gross"), "total DR (placebo)" = fit(S, sw("delta_oadr_10", "delta_dr_10"), "mig_gross"))
  panel(p, paste0("Table 3.5. Horizon, level and the total-DR placebo (", LAB, "2019)"), c("delta_oadr_10", "delta_oadr_3", "delta_oadr_5", "delta_oadr_7", "level_oadr_10", "oadr_0", "delta_dr_10", CT), w = 16, lw = 16, "Each column replaces the ten-year projected change by the projected change over the horizon named (same vintages), its level ten years ahead, the current ratio, or delta_dr_10: the projected 10-year change in the TOTAL dependency ratio (young + old), the placebo: the young are not a pension cost.") }
# =============================================================================
cat("\n\n#################### 4. UNCONTROLLED FLOWS ####################\n")
S <- sub_s("core"); p <- list("main" = fit(S, G, "mig_gross"), "eu_rate" = fit(S, c("L_eu_rate", "delta_oadr_10", CT), "eu_rate"), "brit_rate" = fit(S, c("L_brit_rate", "delta_oadr_10", CT), "brit_rate"), "+brit_rate" = fit(S, c(G, "brit_rate"), "mig_gross"))
panel(p, "Table 4.1. EU and British inflows on the core controls (core sample, 1977-2019)", c("delta_oadr_10", "brit_rate", CT, "L_eu_rate", "L_brit_rate", "L_mig_gross"), w = 16, lw = 16, nested_F = TRUE,
      "eu_rate: EU citizens, whom no British government could ration; brit_rate: British citizens, all origins (LTIM 2.00), per 1,000 working-age persons; +brit_rate holds the British flow constant in the main.")
for (k in names(SMP)) { S <- sub_s(k); BR <- c("L_brit_rate", "delta_oadr_10", CT); S10 <- subset(S, year <= 2010)
  p <- list("brit_rate" = fit(S, BR, "brit_rate"), "+unem_diff" = fit(S, c(BR, "L_unem_diff"), "brit_rate"), "+cit_nat" = fit(S, c(BR, "cit_nat_k"), "brit_rate"), "+L_gap_obr" = fit(S, c(BR, "L_gap_obr"), "brit_rate"), "+L_gap_pca (to 2010)" = fit(S10, c(BR, "L_gap_pca"), "brit_rate"))
  if (k == "core") { p[["brit_desc_noneu"]] <- fit(S, c("L_brit_rate", "delta_oadr_10", CT), "brit_desc_noneu"); p[["brit_desc +L_gap_obr"]] <- fit(S, c("L_brit_rate", "delta_oadr_10", CT, "L_gap_obr"), "brit_desc_noneu") }
  p[["main +L_gap_obr"]] <- fit(S, c(G, "L_gap_obr"), "mig_gross")
  panel(p, paste0("Table 4.2. The British inflow: relative conditions, citizenship policy, the cycle (", SMP[[k]]$LAB, "2019)"), c("delta_oadr_10", "L_unem_diff", "cit_nat_k", "L_gap_obr", "L_gap_pca", CT, "L_brit_rate", "L_mig_gross"), w = 16, lw = 16, nested_F = TRUE,
        "unem_diff: UK unemployment minus the mean of Australia, Canada, New Zealand and the United States (OECD); cit_nat: naturalisations in the current year (thousands); gap_obr: the OBR output gap from the public sector finances databank (fiscal years from 1972-73; on the extended sample the column starts in 1973); gap_pca: the same gap digitised from Working Paper No. 1 (to 2010); brit_desc_noneu: British citizens from non-EU residence not born in the UK; the last column adds the gap to the main equation.") }
cat("\n--- 4.3 Push events: settlement grants to nationals of the former colonies with large British-passport populations (1983-2007) ---\n")
S <- subset(SMP$core$df, year >= 1977 & year <= 2019); BR <- c("L_brit_rate", "delta_oadr_10", CT); Sp <- subset(S, !is.na(settle_push_k))
p <- list("brit_rate (1983-2007)" = fit(Sp, BR, "brit_rate"), "+ln_settle_push" = fit(S, c(BR, "ln_settle_push"), "brit_rate"), "+L_ln_settle_push" = fit(S, c(BR, "L_ln_settle_push"), "brit_rate"), "brit_desc (1983-2007)" = fit(Sp, BR, "brit_desc_noneu"), "brit_desc +ln_settle_push" = fit(S, c(BR, "ln_settle_push"), "brit_desc_noneu"))
panel(p, "Table 4.3. The British inflow and push-country settlement (core sample, 1983-2007)", c("delta_oadr_10", "ln_settle_push", "L_ln_settle_push", CT, "L_brit_rate"), w = 16, lw = 16, nested_F = TRUE,
      "settle_push: grants of settlement to nationals of Hong Kong, Zimbabwe, South Africa, Kenya and Uganda (Home Office retrospective tables), the flows a push event in those countries would raise; if British citizens by descent left with them, the British inflow would co-move.")
# =============================================================================
cat("\n\n#################### 5. WITHOUT THE PROJECTION ####################\n")
for (k in names(SMP)) { S <- sub_s(k); LAB <- SMP[[k]]$LAB; m0 <- fit(S, NP, "mig_gross"); m1 <- fit(S, G, "mig_gross"); m0 <- lm(formula(m0), data = attr(m1, "dd")); attr(m0, "dd") <- attr(m1, "dd")
  cat(sprintf("\n%s2019: no projection  AIC %.2f  |  with projection  AIC %.2f  |  classical F(1,%d) = %.2f (p %s)  |  HAC F = %.2f\n", LAB, AIC(m0), AIC(m1), m1$df.residual, anova(m0, m1)$F[2], pf2(anova(m0, m1)$`Pr(>F)`[2]), hacF(m1)[1]))
  cat("  no projection:"); diag_line(m0); cat("  with projection:"); diag_line(m1)
  cat("  RESET(2,3) rejects the no-projection model on the core sample and is marginal on the extended; rainbow rejects it on both; BG(2) is the one test the model WITH the projection fails on the extended sample (biennial projection vintages), which the long-run claims should acknowledge.\n") }
# =============================================================================
if (BOOT) { cat("\n\n#################### 6. FINAL DIAGNOSTICS (bootstrap, R = 1999) ####################\n")
  for (k in names(SMP)) { S <- sub_s(k); m1 <- fit(S, G, "mig_gross"); dd <- attr(m1, "dd"); m0 <- lm(reformulate(NP, "mig_gross"), data = dd); b <- coef(m0); X <- model.matrix(m0); rho <- b["L_mig_gross"]; xb <- as.vector(X[, setdiff(colnames(X), "L_mig_gross"), drop = FALSE] %*% b[setdiff(names(b), "L_mig_gross")])
    e0 <- residuals(m0) - mean(residuals(m0)); n <- nrow(dd); tobs <- hac(m1)["delta_oadr_10", 3]; set.seed(20260929)
    ts <- replicate(1999, { e <- sample(e0, n, TRUE); y <- numeric(n); yl <- dd$L_mig_gross[1]; for (t in 1:n) { y[t] <- xb[t] + rho * yl + e[t]; yl <- y[t] }; db <- dd; db$mig_gross <- y; db$L_mig_gross <- c(dd$L_mig_gross[1], y[-n]); mb <- lm(reformulate(G, "mig_gross"), data = db); coeftest(mb, vcov. = vcovHAC(mb))["delta_oadr_10", 3] })
    Xm <- model.matrix(m1); y0 <- fitted(m1); e1 <- residuals(m1) - mean(residuals(m1)); f0 <- resettest(m1, power = 2)$statistic; fs <- replicate(1999, { yb <- y0 + sample(e1, n, TRUE); resettest(lm(yb ~ Xm - 1), power = 2)$statistic })
    cat(sprintf("%s2019: recursive bootstrap under no projection: p(|t| >= %.2f) = %.4f (5%% critical |t| %.2f); bootstrap RESET(2) p = %.3f (asymptotic %.3f)\n", SMP[[k]]$LAB, abs(tobs), mean(abs(ts) >= abs(tobs)), quantile(abs(ts), .95), mean(fs >= f0), resettest(m1, power = 2)$p.value)) } }
cat("\nDone. Definitions of every variable shown were printed under the first table that used it.\n")
