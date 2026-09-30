# =============================================================================
# inflation_check.R (Sam, 30 Sep 2026)
# 1  Is inflation already in the debt term? ln_debt_gdp = annual log change in net debt / NOMINAL GDP, so by identity
#       ln_debt_gdp = dln(nominal debt) - dln(nominal GDP) = dln(nominal debt) - ln(1 + inflation) - ln(1 + real growth).
#    Inflation therefore enters with coefficient minus that on nominal debt growth. The core specification is re-estimated with the
#    three components entered separately, and the restrictions that inflation (and real growth) enter only through the ratio are tested.
# 2  With the debt-interest share in LEVEL (the core specification: tax_workpop, ln_debt_gdp, ln_vac_unem, int_tme, own lag), does any
#    inflation measure pass the relaxed test? PASS = inflation term p < .10 with AND without the projection, tax_workpop p < .05 without
#    the projection, projection p < .01; STRICT also requires int_tme p < .05 without the projection. dAIC reported, not required.
#    Measures: GDP-deflator, CPI and wage inflation, real wage growth, short- and long-term inflation expectations (NIESR, start of
#    year, contemporaneous), 1-year and 5-10-year expectations, inflation erosion of the debt; levels, logs where positive, first
#    differences. Measures that end in 2016 are compared with the core specification on the same years.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
D <- c(L_int_tme = "public sector debt interest, % of total managed expenditure, t-1 (Millennium A28; ONS PUSF growth-spliced 2017-19)",
       L_dln_debt_nom = "annual log change in nominal public sector net debt, t-1 (= ln_debt_gdp + ln(1 + nominal GDP growth))",
       L_dln_ngdp = "annual log change in nominal GDP, t-1", L_dln_p = "ln(1 + GDP-deflator inflation / 100), t-1", L_dln_y = "ln(1 + real GDP growth / 100), t-1")
for (k in names(D)) DEFS[[k]] <- D[[k]]
INF <- c("inflation", "cpi_infl", "wage_infl", "real_wage", "lt_inf_exp", "inf_exp_1y", "inf_exp_5_10y", "infl_debt")
ALLR <- list()
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$dln_ngdp <- log(1 + S$nom_gdp_growth / 100); S$dln_p <- log(1 + S$inflation / 100); S$dln_y <- log(1 + S$growth / 100); S$dln_debt_nom <- S$ln_debt_gdp + S$dln_ngdp
  for (v in c("dln_ngdp", "dln_p", "dln_y", "dln_debt_nom")) S[[paste0("L_", v)]] <- lag1(S[[v]])
  S$infl_debt <- S$inflation * S$psnd_gdp / 100
  F <- function(xs, d = S) fit(d, xs, OUTC)
  K <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_vac_unem", "L_int_tme")
  # ---- 1 decomposition ----
  cat(sprintf("\n1. Inflation in the debt term. Correlations with ln_debt_gdp (t-1): deflator inflation %.2f, nominal GDP growth %.2f, nominal debt growth %.2f; check: dln_p + dln_y vs dln_ngdp, max gap %.4f\n",
              cor(S$L_ln_debt_gdp, S$L_inflation, use = "pair"), cor(S$L_ln_debt_gdp, S$L_dln_ngdp, use = "pair"), cor(S$L_ln_debt_gdp, S$L_dln_debt_nom, use = "pair"), max(abs(S$dln_p + S$dln_y - S$dln_ngdp), na.rm = TRUE)))
  m_main <- F(c(K, "L_ln_debt_gdp")); m_2 <- F(c(K, "L_dln_debt_nom", "L_dln_ngdp")); m_3 <- F(c(K, "L_dln_debt_nom", "L_dln_p", "L_dln_y")); m_4 <- F(c(K, "L_ln_debt_gdp", "L_dln_p"))
  panel(list(main = m_main, "debt, nominal GDP" = m_2, "debt, inflation, real growth" = m_3, "+dln_p" = m_4), sprintf("Table 1. The debt term split into nominal debt growth, inflation and real growth, %s", L),
        c("delta_oadr_10", "L_ln_debt_gdp", "L_dln_debt_nom", "L_dln_ngdp", "L_dln_p", "L_dln_y", "L_tax_workpop", "L_int_tme", "L_ln_vac_unem", "L_mig_gross"), w = 30, nested_F = TRUE,
        notes = "main: ln_debt_gdp = dln_debt_nom - dln_ngdp = dln_debt_nom - dln_p - dln_y (the ratio restricts the three coefficients to b, -b, -b). +dln_p: inflation added beside the ratio (tests whether inflation matters beyond its effect through the ratio).")
  w2 <- linearHypothesis(m_2, "L_dln_debt_nom + L_dln_ngdp = 0", vcov. = vcovHAC(m_2)); w3 <- linearHypothesis(m_3, c("L_dln_debt_nom + L_dln_p = 0", "L_dln_debt_nom + L_dln_y = 0"), vcov. = vcovHAC(m_3))
  w3p <- linearHypothesis(m_3, "L_dln_debt_nom + L_dln_p = 0", vcov. = vcovHAC(m_3))
  cat(sprintf("   ratio restriction, nominal GDP (b_debt = -b_ngdp): HAC F %.2f, p %s | inflation and real growth both through the ratio: F %.2f, p %s | inflation alone through the ratio: F %.2f, p %s\n",
              w2$F[2], pf2(w2$`Pr(>F)`[2]), w3$F[2], pf2(w3$`Pr(>F)`[2]), w3p$F[2], pf2(w3p$`Pr(>F)`[2])))
  # ---- 2 inflation screen with int_tme in level ----
  cat("\n2. Inflation measures added to the core specification with the debt-interest share in level\n")
  C1 <- c(K, "L_ln_debt_gdp"); C0 <- setdiff(C1, "delta_oadr_10")
  cand <- list(); for (v in intersect(INF, names(S))) { x <- lag1(S[[v]]); if (v %in% c("st_inf_exp")) x <- S[[v]]; S[[paste0("L_", v)]] <- x; cand[[paste0("L_", v)]] <- TRUE
    if (all(x[!is.na(x)] > 0)) { S[[paste0("L_ln_", v)]] <- log(x); cand[[paste0("L_ln_", v)]] <- TRUE }; S[[paste0("L_d_", v)]] <- lag1(c(NA, diff(S[[v]]))); cand[[paste0("L_d_", v)]] <- TRUE }
  if ("st_inf_exp" %in% names(S)) { S$st_inf_exp_c <- S$st_inf_exp; cand[["st_inf_exp_c"]] <- TRUE; S$ln_st_inf_exp_c <- ifelse(S$st_inf_exp > 0, log(S$st_inf_exp), NA); if (all(S$st_inf_exp[!is.na(S$st_inf_exp)] > 0)) cand[["ln_st_inf_exp_c"]] <- TRUE }
  res <- NULL
  for (v in names(cand)) { a1 <- F(c(C1, v)); a0 <- F(c(C0, v)); if (is.null(a1) || is.null(a0)) next; yrs <- attr(a1, "dd")$year; b1 <- F(C1, S[S$year %in% yrs, ]); b0 <- F(C0, S[S$year %in% attr(a0, "dd")$year, ])
    t0 <- coeftest(a0, vcov. = vcovHAC(a0)); t1 <- coeftest(a1, vcov. = vcovHAC(a1))
    res <- rbind(res, data.frame(add = sub("^L_", "", v), N = nobs(a1), coef0 = t0[v, 1], p0 = t0[v, 4], tax0_p = t0["L_tax_workpop", 4], int0_p = t0["L_int_tme", 4], dAIC0 = AIC(a0) - AIC(b0),
                                 coef1 = t1[v, 1], p1 = t1[v, 4], int1_p = t1["L_int_tme", 4], proj1 = t1["delta_oadr_10", 1], proj1_p = t1["delta_oadr_10", 4], dAIC1 = AIC(a1) - AIC(b1), stringsAsFactors = FALSE)) }
  res$pass <- with(res, p0 < .10 & p1 < .10 & tax0_p < .05 & proj1_p < .01); res$strict <- res$pass & res$int0_p < .05; res <- res[order(!res$pass, res$p1), ]
  c0 <- coeftest(m0 <- F(C0), vcov. = vcovHAC(m0)); cat(sprintf("   core specification without the projection: tax_workpop p %s, int_tme %.4f (p %s)\n", pf2(c0["L_tax_workpop", 4]), c0["L_int_tme", 1], pf2(c0["L_int_tme", 4])))
  print(res, row.names = FALSE, digits = 3); res$sample <- s; ALLR[[s]] <- res }
W <- merge(ALLR[["1964"]], ALLR[["1977"]], by = "add", suffixes = c("_cit", "_orig"))
cat("\n\n==================== Inflation measures passing on BOTH samples (int_tme in level) ====================\n")
P <- W[W$pass_cit & W$pass_orig, ]; if (nrow(P)) print(P[, c("add", "coef1_cit", "p1_cit", "p0_cit", "int0_p_cit", "proj1_cit", "dAIC1_cit", "coef1_orig", "p1_orig", "p0_orig", "int0_p_orig", "proj1_orig", "dAIC1_orig", "strict_cit", "strict_orig")], row.names = FALSE, digits = 3) else cat("   none\n")
cat("\nDone.\n")
