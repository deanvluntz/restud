# =============================================================================
# parallel_spec.R -- the ENGINE of the parallel specification runs (Sam, 28 Sep 2026): every test of the paper repeated with a
# different main specification, chosen by two switches set in the wrapper that sources this file:
#     GROSS = TRUE   dependent variable = log of UNADJUSTED gross inflows in persons (mig_gross: no population denominator, no basis
#                    step, no rescaling of the pre-1975 non-British counts on the extended sample)
#     GROSS = FALSE  dependent variable = the published mig_rate (log inflows per 1,000 working-age persons, t-1; basis step on the
#                    extended sample, as in the published specification)
#     VAC   = TRUE   the log of notified vacancies per working-age person (L_ln_vac_wap, t-1) is added to the fiscal right-hand side
#     VAC   = FALSE  the fiscal right-hand side as published (unemployment is retained in every case)
#     TAXVAR         the tax-burden regressor: "L_tax_workpop" (published, per working-age person), "L_tax_voter" (per adult) or "L_tax_res"
# Wrappers:  parallel_gross_vac.R (GROSS, VAC) | parallel_gross.R (GROSS, no VAC) | parallel_rate_vac.R (mig_rate, VAC)
# Both samples, in this order (numbering mirrors the published tables so the runs can be read side by side):
#   G2  Table 2 analogue      main | main with the vacancy term removed / added | the other dependent variable with main's controls
#   G3  Table 3 analogue      ARDL(1,...) bounds test (PSS case 3) and long-run multipliers; one-SD magnitude; recursive-design
#                             bootstrap bias correction of the lagged-DV bias (2,000 replications)
#   G6-G15  the robustness tables: DV classification, tax measures, tax shares, unemployment measures, level and change, permits,
#           the other dependent variable, old-age vs total DR, horizons, British citizens and Commonwealth (grants / naturalisations /
#           registrations; + emigration)
#   G16-G18 the fiscal model without the projection (nested F), recursive out-of-sample forecasts, the uncontrolled margin (core)
#   G19-G25 further controls by category, each added to the main
#   G26 core only: zero-migration and publication-dated projections;  G27 second lag, timing placebo, recursive / rolling / CUSUM
# DATA: only Dissertation_Dataset_Final.xlsx (sheets data_1964, data_1977, ReformSPA, PubShiftOADR_ZM) -- see data_common.R.
# RStudio: set the working directory to diss_replication/ (Session > Set Working Directory > To Source File Location), then source
# the wrapper. Packages: readxl, lmtest, sandwich, car, ARDL, strucchange.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/ (RStudio: Session > Set Working Directory > To Source File Location)")
if (!exists("GROSS")) GROSS <- TRUE; if (!exists("VAC")) VAC <- TRUE; if (!exists("TAXVAR")) TAXVAR <- "L_tax_workpop"   # TAXVAR: the tax-burden regressor of the main (L_tax_workpop, L_tax_voter, L_tax_res)
source("tables_common.R"); suppressPackageStartupMessages({library(ARDL); library(strucchange); library(readxl)})
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, STEP = STEP, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
WB <- "Dissertation_Dataset_Final.xlsx"; ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA", .name_repair = "minimal")); ref <- ref[!is.na(ref$year), ]
pub <- as.data.frame(read_excel(WB, sheet = "PubShiftOADR_ZM", .name_repair = "minimal")); pub <- pub[!is.na(pub$year), ]
swap <- function(base, drop, add) c(setdiff(base, drop), add)
DV <- if (GROSS) "mig_gross" else "mig_rate"; LDV <- paste0("L_", DV); ODV <- if (GROSS) "mig_rate" else "mig_gross"; LODV <- paste0("L_", ODV)
SPEC <- paste0(if (GROSS) "unadjusted gross inflows" else "inflows per working-age person (published DV)", if (VAC) " + log vacancies" else ", fiscal controls as published", if (TAXVAR != "L_tax_workpop") paste0(", tax burden = ", sub("^L_", "", TAXVAR)))
fit0 <- fit; fit <- function(d, xs, dv = DV, min_extra = 5) fit0(d, xs, dv, min_extra)   # in this engine the default dependent variable is the main's
line <- function(k, m, vs) { if (is.null(m)) return(invisible()); ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-30s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f, RESET %s\n", nobs(m), AIC(m), pf(resettest(m)$p.value))); defs(vs) }
# gross (persons, logged) counterparts of the alternative dependent variables, built here so data_common.R is untouched
DEFS[["mig_gross"]] <- "log gross inflow of controlled migrants, persons, unadjusted: non-EU citizens (core sample); non-British citizens to 1974 and non-EU citizens from 1975 with no basis step (extended sample)"
DEFS[["L_mig_gross"]] <- "mig_gross lagged one year"; DEFS[["L_ln_vac_wap"]] <- "log of notified vacancies per 1,000 working-age persons, t-1 (Millennium M5; ONS AP2Y for 2017-19)"
DEFS[["nb_gross"]] <- "log gross inflow of non-British citizens, persons, all origins (LTIM 2.00)"; DEFS[["L_nb_gross"]] <- "nb_gross lagged one year"
DEFS[["nb03_gross"]] <- "nb_gross to 2003 (before the A8 accession)"; DEFS[["L_nb03_gross"]] <- "nb03_gross lagged one year"
DEFS[["ltim_gross"]] <- "log gross inflow of non-EU citizens on the LTIM basis, persons (from 1992)"; DEFS[["L_ltim_gross"]] <- "ltim_gross lagged one year"
DEFS[["mig_gross_s"]] <- "mig_gross with the pre-1975 non-British counts rescaled by the 1975-80 overlap ratio (the alternative to the basis step)"; DEFS[["L_mig_gross_s"]] <- "mig_gross_s lagged one year"
DEFS[["brit_gross"]] <- "log gross inflow of British citizens, persons, all origins (LTIM 2.00)"; DEFS[["L_brit_gross"]] <- "brit_gross lagged one year"
DEFS[["eu_gross"]] <- "log gross inflow of EU citizens, persons (IPS, from 1975)"; DEFS[["L_eu_gross"]] <- "eu_gross lagged one year"
DEFS[["tot_gross"]] <- "log gross inflow of all citizens (EU + non-EU), persons (IPS, from 1975)"; DEFS[["L_tot_gross"]] <- "tot_gross lagged one year"
DEFS[["delta_zm_10"]] <- "projected ten-year change in the old-age dependency ratio under the zero-migration variant of the same vintage"
DEFS[["p_pub"]] <- "the ten-year projected change dated by publication year rather than base year"
for (k in names(SMP)) { d <- SMP[[k]]$df; y <- d$year
  d$nb_gross <- log(1000 * d$dv_A); d$nb03_gross <- log(1000 * d$dv_C); d$ltim_gross <- log(as.numeric(ref$ltim_nEU_inflows)[match(y, ref$year)])
  d$brit_gross <- log(1000 * d$brit_in); d$eu_gross <- log(as.numeric(ref$EU_inflows)[match(y, ref$year)]); d$tot_gross <- log(as.numeric(ref$nEU_inflows)[match(y, ref$year)] + as.numeric(ref$EU_inflows)[match(y, ref$year)])
  d$delta_zm_10 <- as.numeric(ref$delta_zm_10)[match(y, ref$year)]; d$p_pub <- as.numeric(pub$delta_oadr_10_pub)[match(y, pub$year)]
  for (v in c("nb_gross", "nb03_gross", "ltim_gross", "brit_gross", "eu_gross", "tot_gross")) { x <- d[[v]]; x[!is.finite(x)] <- NA; d[[v]] <- x; d[[paste0("L_", v)]] <- lag1(x) }
  d$L2_mig_gross <- lag1(d$L_mig_gross); SMP[[k]]$df <- d }
CT0 <- c(TAXVAR, "L_ln_debt_gdp", "L_unem"); CT <- c(CT0, if (VAC) "L_ln_vac_wap")   # the main's controls (right-hand side minus the lagged DV, the projection and the step)
sfx <- if (GROSS) "_gross" else "_rate"                                                          # suffix of the alternative-DV names on the main's basis
cat("\n##### PARALLEL RUN: ", SPEC, " as the main specification #####\n", sep = "")
cat("Main: ", DV, " ~ ", LDV, " + delta_oadr_10 + ", paste(CT, collapse = " + "), if (!GROSS) " [+ basis_pre1975 on the extended sample]", ".",
    if (VAC) " Vacancies for 2017-19 are ONS AP2Y spliced onto Millennium M5, so every column carries the 2016 re-estimate (tables_common.R).", " Definitions print once per run.\n", sep = "")
for (k in names(SMP)) {
  d <- SMP[[k]]$df; STEP <- SMP[[k]]$STEP; Y0 <- SMP[[k]]$Y0; LAB <- SMP[[k]]$LAB; EXT <- k == "extended"; S <- subset(d, year >= Y0 & year <= 2019); TAG <- paste0(LAB, "2019"); SN <- paste0(k, " sample")
  MSTEP <- if (GROSS) NULL else STEP                                                           # the published DV keeps its basis step; gross inflows carry none
  G <- c(LDV, "delta_oadr_10", MSTEP, CT)                                                       # the main specification
  grow <- c("delta_oadr_10", if (VAC) "L_ln_vac_wap", TAXVAR, "L_ln_debt_gdp", "L_unem", LDV, MSTEP)
  OG <- c(LODV, "delta_oadr_10", if (!GROSS) NULL else STEP, CT)                                 # the other DV with the main's controls (mig_rate gets the step, mig_gross none)
  cat("\n\n########## ", toupper(TAG), " ##########\n", sep = "")
  # --- G2 Table 2 analogue -----------------------------------------------------------------------------------------
  p <- list("main" = fit(S, G)); p[[if (VAC) "no ln_vac_wap" else "+ln_vac_wap"]] <- fit(S, if (VAC) setdiff(G, "L_ln_vac_wap") else c(G, "L_ln_vac_wap"))
  p[[ODV]] <- fit(S, OG, ODV); if (GROSS && EXT) p[["+basis_pre1975"]] <- fit(S, c(G, STEP))
  panel(p, paste0("Table G2. The main specification: ", SPEC, " (", TAG, ")"), unique(c(grow, "L_ln_vac_wap", LODV, STEP)), w = 22, nested_F = TRUE,
        paste0("main: ", SPEC, "; column 2 ", if (VAC) "drops" else "adds", " the vacancy term; column 3 is the other dependent variable (", ODV, ") with the main's controls",
               if (GROSS && EXT) "; column 4 adds the basis step the published extended-sample specification carries." else "."))
  m <- p[["main"]]; b <- coef(m); sdp <- sd(attr(m, "dd")$delta_oadr_10)
  cat(sprintf("Magnitude: one-SD move in delta_oadr_10 (%.1f) = %.1f%% of inflows on impact, %.1f%% in the long run (b / (1 - rho), rho = %.3f).\n", sdp, 100 * (exp(b["delta_oadr_10"] * sdp) - 1), 100 * (exp(b["delta_oadr_10"] * sdp / (1 - b[LDV])) - 1), b[LDV]))
  # --- G3 bounds test, long-run multipliers, bias correction -----------------------------------------------------------
  S$ln_vac_wap <- log(S$vac_wap); lev <- c("delta_oadr_10", "unem", "ln_debt_gdp", sub("^L_", "", TAXVAR), if (VAC) "ln_vac_wap"); vv <- c(DV, lev, MSTEP); ok <- complete.cases(S[, vv])
  ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  fm <- as.formula(paste(DV, "~", paste(lev, collapse = " + "), if (!is.null(MSTEP)) paste("|", MSTEP)))
  m_ardl <- ardl(fm, data = ts_df, order = rep(1, length(lev) + 1)); lr <- multipliers(m_ardl, type = "lr")
  set.seed(20260802); bf <- bounds_f_test(m_ardl, case = 3, exact = TRUE); set.seed(20260802); bt <- bounds_t_test(m_ardl, case = 3, exact = TRUE)
  cat(sprintf("\nTable G3. ARDL(%s) bounds test, %d-2019 (%s): F = %.3f (p %.3f), t = %.3f (p %.3f). Long-run multipliers:\n", paste(rep(1, length(lev) + 1), collapse = ","), min(S$year[ok]), SN, bf$statistic, bf$p.value, bt$statistic, bt$p.value)); print(lr)
  X <- model.matrix(m); e <- residuals(m) - mean(residuals(m)); iy <- which(colnames(X) == LDV); ip <- which(colnames(X) == "delta_oadr_10"); n <- nobs(m)
  set.seed(20260802); rhos <- bp <- numeric(2000)
  for (r in 1:2000) { es <- sample(e, n, replace = TRUE); Xs <- X; lag <- X[1, iy]; ys <- numeric(n)
    for (t in 1:n) { Xs[t, iy] <- lag; ys[t] <- sum(Xs[t, ] * b) + es[t]; lag <- ys[t] }
    bb <- lm.fit(Xs, ys)$coefficients; rhos[r] <- bb[iy]; bp[r] <- bb[ip] }
  bias <- mean(rhos) - b[iy]; rho_c <- b[iy] - bias; lr_boot <- bp / (1 - (rhos - bias)); lr_c <- b[ip] / (1 - rho_c); ci <- quantile(2 * lr_c - lr_boot, c(0.025, 0.975))
  cat(sprintf("Bias correction (recursive-design residual bootstrap, 2,000 replications): rho %.3f, bias %.3f (analytic %.3f), corrected rho %.3f; long-run projection %.4f -> %.4f [%.4f, %.4f].\n",
              b[iy], bias, -(1 + 3 * b[iy]) / n, rho_c, b[ip] / (1 - b[iy]), lr_c, ci[1], ci[2]))
  # --- G6 dependent variable classification (on the main's basis: gross counts or rates) --------------------------------
  fdv <- function(dv, dd = S, step = FALSE) fit(dd, c(paste0("L_", dv), "delta_oadr_10", if (step) STEP, CT), dv)
  p <- list("main" = fit(S, G)); alt <- function(nm) paste0(nm, sfx)
  if (EXT) { p[[alt("mig")]] <- fdv(paste0("mig", sfx, "_s")); p[[alt("nb")]] <- fdv(alt("nb")); p[[alt("nb03")]] <- fdv(alt("nb03"), subset(d, year >= Y0 & year <= 2003), step = !GROSS) }
  else { p[[alt("nb")]] <- fdv(alt("nb")); p[[alt("nb03")]] <- fdv(alt("nb03"), subset(d, year >= Y0 & year <= 2003)); p[[alt("ltim")]] <- fdv(alt("ltim"), subset(d, year >= 1992 & year <= 2019)) }
  if (EXT) names(p)[2] <- paste0("mig", sfx, "_s")
  panel(p, paste0("Table G6. The dependent variable under the other sample's classification (", TAG, ")"), unique(c(grow, paste0("L_mig", sfx, "_s"), paste0("L_", alt(c("nb", "nb03", "ltim"))), STEP)), w = 14,
        if (EXT) "The _s series rescales the pre-1975 counts by the overlap ratio (no step); nb = non-British citizens throughout; nb03 ends before the A8 accession. Gross series carry no step." else
                 "nb applies the extended sample's pre-1975 classification (non-British citizens, LTIM 2.00) to the core years; nb03 ends in 2003; ltim is the LTIM-basis non-EU series from 1992.")
  # --- G7, G8 tax measures and shares ----------------------------------------------------------------------------------------
  p <- list("main" = fit(S, G), "tax_inc" = fit(S, swap(G, TAXVAR, "L_tax_inc")), "tax_nics" = fit(S, swap(G, TAXVAR, "L_tax_nics")), "tax_receipts" = fit(S, swap(G, TAXVAR, "L_tax_receipts")))
  panel(p, paste0("Table G7. Tax burden per worker: alternative measures (", TAG, ")"), unique(c("delta_oadr_10", TAXVAR, "L_tax_inc", "L_tax_nics", "L_tax_receipts", grow)), w = 14,
        "Each column replaces the main's tax burden with the central government receipt named (Millennium A27), per working-age person, 2024-25 prices, t-1.")
  cat("Other receipts per working-age person, in place of L_tax_workpop:\n")
  for (v in c("L_tax_nics_emp", "L_tax_nics_er", "L_tax_direct", "L_tax_indirect", "L_tax_vat", "L_tax_corp")) line(sub("^L_", "", v), fit(S, swap(G, TAXVAR, v)), c("delta_oadr_10", v))
  p <- list("main" = fit(S, G), "tax_workpop" = fit(S, swap(G, TAXVAR, "L_tax_workpop")), "tax_res" = fit(S, swap(G, TAXVAR, "L_tax_res")), "tax_gdp" = fit(S, swap(G, TAXVAR, "L_tax_gdp")), "tax_earn" = fit(S, swap(G, TAXVAR, "L_tax_earn")),
            "tax_voter" = fit(S, swap(G, TAXVAR, "L_tax_voter")), "tax_taxpayer" = fit(S, swap(G, TAXVAR, "L_tax_taxpayer")),
            "dln_tax_workpop" = fit(S, swap(G, TAXVAR, "L_dln_tax_workpop")), "dln_tax_res" = fit(S, swap(G, TAXVAR, "L_dln_tax_res")))
  panel(p, paste0("Table G8. National accounts taxes: alternative shares and log changes (", TAG, ")"), unique(c("delta_oadr_10", TAXVAR, "L_tax_workpop", "L_tax_res", "L_tax_gdp", "L_tax_earn", "L_tax_voter", "L_tax_taxpayer", "L_dln_tax_workpop", "L_dln_tax_res", grow)), w = 17,
        "The same tax total over the resident population, GDP, average annual earnings, adults (working-age plus pensioners) and income-tax payers (from 1979); then the annual log change in taxes per working-age person and per resident in place of the level.")
  # --- G9 unemployment measures ------------------------------------------------------------------------------------------------
  p <- list("main" = fit(S, G), "unem_boe" = fit(S, swap(G, "L_unem", "L_unem_boe")), "unem_claim" = fit(S, swap(G, "L_unem", "L_unem_claim")))
  panel(p, paste0("Table G9. Unemployment measures (", TAG, ")"), unique(c("delta_oadr_10", "L_unem", "L_unem_boe", "L_unem_claim", grow)), w = 14,
        paste0("Each column replaces L_unem (ILO) with the measure named", if (VAC) "; log vacancies retained throughout (the vacancy/unemployment ratio is their quotient, so the two share precision)." else "."))
  # --- G10 level and change ----------------------------------------------------------------------------------------------------
  p <- list("main" = fit(S, G), "level_oadr_10" = fit(S, swap(G, "delta_oadr_10", "level_oadr_10")), "+oadr_0" = fit(S, c(G, "oadr_0")))
  panel(p, paste0("Table G10. The projected level and the projected change (", TAG, ")"), unique(c("delta_oadr_10", "level_oadr_10", "oadr_0", grow)), w = 16,
        "level_oadr_10 = oadr_0 + delta_oadr_10; the third column frees the current ratio and the projected change.")
  if (!is.null(p[[3]])) { lh <- linearHypothesis(p[[3]], "delta_oadr_10 = oadr_0", vcov. = vcovHAC(p[[3]])); cat(sprintf("Equality of the two coefficients in column 3: HAC F = %.2f (p %s).\n", lh$F[2], pf(lh$`Pr(>F)`[2]))) }
  # --- G11 permits ----------------------------------------------------------------------------------------------------------------
  Y0P <- if (EXT) 1965 else 1977; P <- subset(d, year >= Y0P & year <= 2008)
  p <- list("main" = fit(S, G)); p[[sprintf("main, %d-2008", Y0P)]] <- fit(P, G); p[[sprintf("permits, %d-2008", Y0P)]] <- fit(P, c("L_permits", "delta_oadr_10", CT), "permits")
  panel(p, paste0("Table G11. The policy lever: work permits (", SN, ")"), unique(c(grow, "L_permits")), w = 20,
        "Column 3: work permits and first permissions, thousands (Bauer and Salt, 2020; to 2008), on the main's controls, no basis step.")
  # --- G12 the other dependent variable --------------------------------------------------------------------------------------------
  p <- list("main" = fit(S, G)); p[[ODV]] <- fit(S, OG, ODV); p[[paste0(ODV, if (VAC) ", no ln_vac_wap" else " +ln_vac_wap")]] <- fit(S, if (VAC) setdiff(OG, "L_ln_vac_wap") else c(OG, "L_ln_vac_wap"), ODV)
  panel(p, paste0("Table G12. ", if (GROSS) "Gross inflows and inflows per working-age person" else "Inflows per working-age person and gross inflows", " (", TAG, ")"), unique(c(grow, LODV, "L_ln_vac_wap", STEP)), w = 26,
        paste0(ODV, ": ", if (GROSS) "the published dependent variable (log inflows per 1,000 working-age persons, t-1, with its basis step on the extended sample)" else "log gross inflows in persons, no population term on either side and no basis step", ", with the main's controls; column 3 ", if (VAC) "drops" else "adds", " the vacancy term."))
  # --- G13 old-age vs total DR ---------------------------------------------------------------------------------------------------
  p <- list("main" = fit(S, G), "delta_dr_10" = fit(S, swap(G, "delta_oadr_10", "delta_dr_10")), "level_dr_10" = fit(S, swap(G, "delta_oadr_10", "level_dr_10")))
  panel(p, paste0("Table G13. Old-age vs. total dependency ratio (", TAG, ")"), unique(c("delta_oadr_10", "delta_dr_10", "level_dr_10", grow)), w = 14,
        "Columns 2-3 replace the projected old-age change with the projected change and level of the total dependency ratio, same vintages.")
  line("delta_oadr_10 + delta_dr_10 together", fit(S, c(G, "delta_dr_10")), c("delta_oadr_10", "delta_dr_10"))
  # --- G14 horizons ----------------------------------------------------------------------------------------------------------------
  H <- paste0("delta_oadr_", c(1, 3, 5, 7)); p <- c(list("main" = fit(S, G)), setNames(lapply(H, function(h) fit(S, swap(G, "delta_oadr_10", h))), H))
  hh <- vapply(names(p), function(kk) hacF(p[[kk]], if (kk == "main") "delta_oadr_10" else kk), numeric(2))
  panel(p, paste0("Table G14. The projection horizon (", TAG, ")"), unique(c("delta_oadr_10", H, grow)), w = 14,
        sprintf("HAC F of the horizon term: %s.", paste(sprintf("%s %.2f (p %s)", names(p), hh[1, ], vapply(hh[2, ], pf, character(1))), collapse = "; ")))
  # --- G15 British citizens and Commonwealth, citizenship policy, emigration ----------------------------------------------------------
  BV <- paste0("brit", sfx); BR <- c(paste0("L_", BV), "delta_oadr_10", CT)
  p <- list("main" = fit(S, G)); if (EXT) p[["+Act steps"]] <- fit(S, c(G, "post1968", "post1973"))
  p[[BV]] <- fit(S, BR, BV); p[[paste0(BV, " +cit_grants")]] <- fit(S, c(BR, "cit_grants_k"), BV); p[[paste0(BV, " +L_cit_grants")]] <- fit(S, c(BR, "L_cit_grants_k"), BV); p[[paste0(BV, " +cit_nat")]] <- fit(S, c(BR, "cit_nat_k"), BV); p[[paste0(BV, " +L_cit_nat")]] <- fit(S, c(BR, "L_cit_nat_k"), BV)
  p[[paste0("+", BV)]] <- fit(S, c(G, BV)); p[["+emig_all"]] <- fit(S, c(G, "L_emig_all")); p[["+emig_brit"]] <- fit(S, c(G, "L_emig_brit"))
  panel(p, paste0("Table G15. British citizens and Commonwealth: inflows, citizenship policy and emigration (", TAG, ")"),
        unique(c("delta_oadr_10", "post1968", "post1973", "cit_grants_k", "L_cit_grants_k", "cit_nat_k", "L_cit_nat_k", BV, "L_emig_all", "L_emig_brit", grow, paste0("L_", BV))), w = 13, lw = 16,
        c(if (EXT) paste0("+Act steps: the Commonwealth Immigrants Act 1968 and the Immigration Act 1971 in force (1973)", if (GROSS) "; without a basis step they are identified against the whole sample." else "; with the basis step they are identified inside the pre-1975 window only.") else NULL,
          paste0(BV, " columns: British-citizen inflows (all origins, LTIM 2.00), the uncontrolled flow, on the main's controls, then with grants of citizenship (all grants; naturalisations), current year and lagged; registrations as a text line."),
          paste0("+", BV, " holds the British flow constant in the controlled-flow equation; +emig_all / +emig_brit add lagged emigrants per working-age person.")))
  line(paste0(BV, " +cit_reg"), fit(S, c(BR, "cit_reg_k"), BV), c("delta_oadr_10", "cit_reg_k")); line(paste0(BV, " +L_cit_reg"), fit(S, c(BR, "L_cit_reg_k"), BV), c("delta_oadr_10", "L_cit_reg_k"))
  if (EXT) line("+trend (sign only)", fit(S, c(G, "trend")), c("delta_oadr_10", "trend"))
  line(paste0("+L_", BV), fit(S, c(G, paste0("L_", BV))), c("delta_oadr_10", paste0("L_", BV)))
  # --- G16-G17 without the projection; out-of-sample -----------------------------------------------------------------------------------
  NP <- setdiff(G, "delta_oadr_10"); ONP <- setdiff(OG, "delta_oadr_10")
  p <- list("no proj" = fit(S, NP), "main" = fit(S, G)); p[[paste0(ODV, ", no proj")]] <- fit(S, ONP, ODV); p[[ODV]] <- fit(S, OG, ODV)
  panel(p, paste0("Table G16. The fiscal model without and with the projection (", TAG, ")"), unique(c(grow, LODV, STEP)), w = 22, nested_F = TRUE,
        paste0("no proj = delta_oadr_10 omitted; columns 3-4 the same for the other dependent variable (", ODV, ")."))
  oos <- function(SS, xs, dv, from) { e <- c()
    for (t in from:max(SS$year)) { tr <- SS[SS$year < t, ]; te <- SS[SS$year == t, ]; if (!nrow(te)) next
      mm <- lm(reformulate(xs, dv), data = tr[complete.cases(tr[, c(dv, xs)]), ]); e <- c(e, te[[dv]] - predict(mm, newdata = te)) }
    c(sqrt(mean(e^2, na.rm = TRUE)), sum(!is.na(e))) }
  cat("\nTable G17. Recursive one-step-ahead forecasts (expanding window): root mean squared error\n")
  for (from in if (EXT) c(1990, 2001) else c(1997, 2001)) { r0 <- oos(S, NP, DV, from); r1 <- oos(S, G, DV, from)
    cat(sprintf("forecasts %d-2019 (n = %d): no proj %.4f | main %.4f | ratio %.2f\n", from, r0[2], r0[1], r1[1], r1[1] / r0[1])) }
  # --- G18 uncontrolled margin (core) --------------------------------------------------------------------------------------------------
  if (!EXT) { EV <- paste0("eu", sfx); TV <- paste0("tot", sfx); S75 <- subset(S, !is.na(S[[EV]]))
    p <- list("main" = fit(S75, G)); p[[EV]] <- fit(S75, c(paste0("L_", EV), "delta_oadr_10", CT), EV); p[[TV]] <- fit(S75, c(paste0("L_", TV), "delta_oadr_10", CT), TV)
    panel(p, paste0("Table G18. The uncontrolled margin: EU and total inflows (", TAG, ")"), unique(c(grow, paste0("L_", c(EV, TV)))), w = 14,
          paste0(EV, ": inflows of EU citizens, whom no British government could ration; ", TV, ": all inflows (EU + non-EU), on the main's basis and controls.")) }
  # --- G19-G25 further controls by category, added to the main ---------------------------------------------------------------------
  add_tab <- function(vars, title, notes = NULL, w = 13, lw = 18, extra = list()) {
    p <- c(list("main" = fit(S, G)), setNames(lapply(vars, function(v) fit(S, c(G, v))), paste0("+", sub("^L_", "", vars))), extra)
    panel(p, title, unique(c("delta_oadr_10", vars, unlist(lapply(extra, function(mm) setdiff(names(coef(mm)), c("(Intercept)", G)))), grow)), w = w, lw = lw, notes = notes) }
  cat("\n##### Further controls, each added to the main (", TAG, ") #####\n", sep = "")
  add_tab(c(if (!VAC) "L_ln_vac_wap", "L_ln_vac_unem", "L_dln_vac_unem", "L_wage_infl", "L_real_wage", "L_union", "L_replace"), paste0("Table G19. Labour market controls (", TAG, ")"),
          if (VAC) "Log vacancies per working-age person is already in the main; ln_vac_unem is near-collinear with it and L_unem." else "Vacancies enter in logs (the form that fits in the private tests); L_unem and log vacancies are near-collinear.")
  add_tab(c("L_growth", "L_growth_pc", "L_tfp", "L_lab_prod", "L_particip", "growth_fc"), paste0("Table G20. Growth, productivity and participation (", TAG, ")"))
  add_tab(c("L_real_eri", "L_openness", "L_ca_gdp", "L_trade_bal", "L_tot"), paste0("Table G21. Trade and the external balance (", TAG, ")"))
  add_tab(c("L_inflation", "L_cpi_infl", "st_inf_exp", "lt_inf_exp", "inf_exp_1y", "inf_exp_5_10y", "L_m4_growth"), paste0("Table G22. Inflation, expectations and money (", TAG, ")"))
  add_tab(c("L_grants_gdp", "L_grants_wap", "L_tme_gdp", "L_tme_real_growth", "gad_earn", "gad_policy"), paste0("Table G23. Current and projected spending (", TAG, ")"))
  add_tab(c("L_psnb_gdp", "L_psnd_gdp", "fc_budget", "fc_medium", "L_tax_ceiling", "golden_rule", "obr_era"), paste0("Table G24. Borrowing, borrowing plans and fiscal rules (", TAG, ")"),
          extra = list("psnb_gdp for ln_debt_gdp" = fit(S, swap(G, "L_ln_debt_gdp", "L_psnb_gdp"))))
  add_tab(c("L_int_tme", "L_int_gdp", "L_bank_rate", "L_tbill", "L_gilt", "L_consol", "L_r_g"), paste0("Table G25. Debt dynamics and the cost of borrowing (", TAG, ")"))
  # --- G26 core-only projections; G27 second lag, timing placebo and stability ----------------------------------------------------------
  if (!EXT) { cat("\nTable G26. Alternative projections (core sample)\n"); line("zero-migration projection", fit(S, swap(G, "delta_oadr_10", "delta_zm_10")), c("delta_zm_10"))
    line("publication-dated projection", fit(S, swap(G, "delta_oadr_10", "p_pub")), c("p_pub")) }
  L2 <- paste0("L2_", DV); line(paste0("+", L2), fit(S, c(G, L2)), c("delta_oadr_10", L2))
  m <- fit(S, G); dd <- attr(m, "dd"); t_of <- function(x) { d2 <- dd; d2$pk <- x; mm <- fit(d2, swap(G, "delta_oadr_10", "pk")); coeftest(mm, vcov. = vcovHAC(mm))["pk", 3] }
  n <- nrow(dd); t0 <- t_of(dd$delta_oadr_10); tk <- vapply(1:(n - 1), function(j) t_of(c(tail(dd$delta_oadr_10, j), head(dd$delta_oadr_10, -j))), numeric(1))
  cat(sprintf("\nTable G27. Timing placebo (%s): actual t %.2f, largest rotated |t| %.2f, randomisation p = %.3f.\n", TAG, t0, max(abs(tk)), (1 + sum(abs(tk) >= abs(t0))) / n))
  ends <- seq(1990, 2019, by = 5); rec <- vapply(ends, function(en) { mm <- fit(subset(S, year <= en), G); ct <- coeftest(mm, vcov. = vcovHAC(mm)); sprintf("%.4f (p %s)", ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4])) }, character(1))
  cat(sprintf("Recursive (sample ending %s): %s.\n", paste(ends, collapse = "/"), paste(rec, collapse = " / ")))
  starts <- Y0:(2019 - 24); roll <- t(sapply(starts, function(st) { mm <- fit(subset(S, year >= st & year <= st + 24), G); ct <- coeftest(mm, vcov. = vcovHAC(mm)); c(ct["delta_oadr_10", 1], ct["delta_oadr_10", 4]) }))
  cat(sprintf("Rolling 25-year windows (%d-%d starts): projection %.4f to %.4f, significant at 5%% in %d of %d; OLS-CUSUM p = %.2f.\n", min(starts), max(starts), min(roll[, 1]), max(roll[, 1]), sum(roll[, 2] < 0.05), nrow(roll),
              sctest(efp(formula(m), data = dd, type = "OLS-CUSUM"))$p.value))
}
