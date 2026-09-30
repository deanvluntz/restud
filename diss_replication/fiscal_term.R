# =============================================================================
# fiscal_term.R -- What does the debt-interest term do in the core specification? (Sam, 30 Sep 2026.)
# Core specification: mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_ln_int_tme.
# 1  The term: with and without it; its two components (debt interest / GDP, TME / GDP) entered separately; the ratio's
#    restriction (equal and opposite log coefficients) tested; the three-way decomposition int/TME = (effective rate x debt/GDP) / (TME/GDP).
# 2  Collinearity: correlations of L_ln_int_tme with the change and the level of debt/GDP and with the other fiscal series; its VIF and
#    auxiliary R2 in the core specification; the level of debt/GDP added to, and in place of, the log change.
# 3  Replacements: every borrowing, spending and cost-of-borrowing term in the dataset in place of the debt-interest share, and the
#    logical pairs (stock x price of debt, borrowing + price), on both samples.
# Column titles name the variable that replaces L_ln_int_tme ("+" = added beside it; "none" = dropped).
# RStudio: working directory = diss_replication/, then source() this file. Needs the latest workbook (git pull).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)")
OUTC <- "mig_gross"; CORE_X <- c("L_mig_gross", "delta_oadr_10", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem"); FT <- "L_ln_int_tme"
adddef <- function(k, v) if (!(k %in% names(DEFS)) || DEFS[[k]] == k) DEFS[[k]] <<- v
adddef("L_ln_int_gdp", "log of public sector debt interest, % of GDP, t-1 (Millennium A28; ONS 2017-19)")
adddef("L_ln_tme_gdp", "log of total managed expenditure, % of GDP, t-1 (Millennium A28; ONS 2017-19)")
adddef("L_ln_psnd_gdp", "log of public sector net debt, % of GDP, t-1 (OBR HPFD)")
adddef("L_ln_eff_rate", "log of the effective interest rate on the debt: debt interest / net debt, %, t-1")
adddef("L_psnb_gdp", "public sector net borrowing, % of GDP, t-1 (OBR HPFD; negative = surplus)")
adddef("L_ln_tbill", "log of the three-month Treasury bill rate, %, t-1")
adddef("L_ln_gilt", "log of the 10-year gilt yield, %, t-1")
adddef("L_bank_rate", "Bank Rate, %, annual average, t-1")
adddef("L_term_spread", "10-year gilt yield minus the three-month bill rate, percentage points, t-1")
adddef("L_real_rate", "real short-term interest rate, %, t-1 (bill rate minus inflation)")
adddef("L_r_g", "real interest rate minus real GDP growth, percentage points, t-1")
adddef("L_ln_debt_service_tax", "log of debt interest / GDP divided by taxes / GDP (debt service as a share of tax revenue), t-1")
adddef("L_rdebt", "real interest rate x net debt / GDP (real debt-service pressure, % of GDP), t-1")
adddef("L_rg_debt", "(r - g) x net debt / GDP (the debt-dynamics term, % of GDP), t-1")
adddef("L_ln_grants_gdp", "log of current grants to persons, % of GDP, t-1 (Millennium A28)")
adddef("L_ln_tme_obr_gdp", "log of total managed expenditure, % of GDP, fiscal year, t-1 (OBR public sector finances databank)")
adddef("L_ln_tax_gdp", "log of national accounts taxes, % of GDP, t-1")
adddef("L_d_psnd_gdp", "annual change in net debt / GDP, percentage points, t-1")
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R")
  need <- c("int_tme", "int_gdp", "tme_gdp", "psnd_gdp", "psnb_gdp", "tbill", "gilt", "bank_rate", "term_spread", "real_rate", "r_minus_g", "grants_gdp", "tax_gdp", "tme_obr_gdp")
  miss <- setdiff(need, names(df)); if (length(miss)) stop("columns missing from sheet data_", s, ": ", paste(miss, collapse = ", "), ". Pull the latest Dissertation_Dataset_Final.xlsx (git pull) and run again.")
  S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]; cat("\n\n==================== ", L, " ====================\n", sep = "")
  S$L_ln_eff_rate <- lag1(log(100 * S$int_gdp / S$psnd_gdp)); S$L_ln_debt_service_tax <- lag1(log(S$int_gdp / S$tax_gdp))
  S$L_rdebt <- lag1(S$real_rate * S$psnd_gdp / 100); S$L_rg_debt <- lag1(S$r_minus_g * S$psnd_gdp / 100); S$L_d_psnd_gdp <- lag1(c(NA, diff(S$psnd_gdp)))
  if (!"L_r_g" %in% names(S)) S$L_r_g <- lag1(S$r_minus_g)
  F <- function(xs) fit(S, xs, OUTC); core <- F(c(CORE_X, FT))
  # ---- 1 the term ----
  cat("\n1. The debt-interest term and its components\n")
  m_sep <- F(c(CORE_X, "L_ln_int_gdp", "L_ln_tme_gdp")); m_dec <- F(c(CORE_X, "L_ln_eff_rate", "L_ln_psnd_gdp", "L_ln_tme_gdp"))
  panel(list(main = core, none = F(CORE_X), "ln_int_gdp" = F(c(CORE_X, "L_ln_int_gdp")), "ln_tme_gdp" = F(c(CORE_X, "L_ln_tme_gdp")), "ln_int_gdp + ln_tme_gdp" = m_sep, "eff. rate + debt + TME" = m_dec),
        sprintf("Table 1. The debt-interest share, its components and its decomposition, %s", L),
        c("delta_oadr_10", FT, "L_ln_int_gdp", "L_ln_tme_gdp", "L_ln_eff_rate", "L_ln_psnd_gdp", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 24, nested_F = TRUE,
        notes = c("ln_int_gdp + ln_tme_gdp: the ratio's two components entered separately; the ratio restricts their log coefficients to be equal and opposite.",
                  "eff. rate + debt + TME: the identity int/TME = (effective rate x debt/GDP) / (TME/GDP) in logs; the ratio restricts the three coefficients to (+1, +1, -1) times a common factor."))
  w1 <- linearHypothesis(m_sep, "L_ln_int_gdp + L_ln_tme_gdp = 0", vcov. = vcovHAC(m_sep))
  core_s <- fit(S[S$year %in% attr(m_sep, "dd")$year, ], c(CORE_X, FT), OUTC)
  cat(sprintf("   Ratio restriction (b_int + b_tme = 0) in the separated model: HAC F = %.2f, p = %.3f; on the same %d observations AIC separated %.2f vs ratio %.2f, none %.2f.\n", w1$F[2], w1$`Pr(>F)`[2], nobs(m_sep), AIC(m_sep), AIC(core_s), AIC(fit(S[S$year %in% attr(m_sep, "dd")$year, ], CORE_X, OUTC))))
  w2 <- linearHypothesis(m_dec, c("L_ln_eff_rate - L_ln_psnd_gdp = 0", "L_ln_eff_rate + L_ln_tme_gdp = 0"), vcov. = vcovHAC(m_dec))
  cat(sprintf("   Decomposition restriction (equal rate and debt coefficients, TME equal and opposite): HAC F = %.2f, p = %.3f; AIC decomposed %.2f.\n", w2$F[2], w2$`Pr(>F)`[2], AIC(m_dec)))
  ct <- coeftest(core, vcov. = vcovHAC(core)); sdx <- sd(attr(core, "dd")[[FT]])
  cat(sprintf("   Magnitude: one SD of ln_int_tme (%.3f, i.e. %.0f%% of the share) raises log gross inflows by %.3f (%.1f%%) on impact; long run %.1f%%. Sample range of the share: %.1f-%.1f%% of TME.\n",
              sdx, 100 * (exp(sdx) - 1), ct[FT, 1] * sdx, 100 * (exp(ct[FT, 1] * sdx) - 1), 100 * (exp(ct[FT, 1] * sdx / (1 - ct["L_mig_gross", 1])) - 1), 100 * min(exp(attr(core, "dd")[[FT]])) / 100, 100 * max(exp(attr(core, "dd")[[FT]])) / 100))
  # ---- 2 collinearity ----
  cat("\n2. Collinearity of the debt-interest share with the debt regressor and the other fiscal series (estimation sample)\n")
  dd <- attr(core, "dd"); cv <- c(FT, "L_ln_debt_gdp", "L_ln_psnd_gdp", "L_d_psnd_gdp", "L_psnb_gdp", "L_ln_int_gdp", "L_ln_tme_gdp", "L_ln_eff_rate", "L_ln_tbill", "L_ln_gilt", "L_tax_workpop", "L_ln_vac_unem", "delta_oadr_10")
  cv <- cv[cv %in% names(S)]; cm <- cor(S[S$year %in% dd$year, cv], use = "pairwise"); print(round(cm[FT, , drop = FALSE], 2))
  aux <- summary(lm(reformulate(CORE_X, FT), data = dd))$r.squared
  cat(sprintf("   VIF of L_ln_int_tme in the core specification: %.1f (auxiliary R2 on the other regressors %.2f); VIF of L_ln_debt_gdp: %.1f.\n", vif(core)[FT], aux, vif(core)["L_ln_debt_gdp"]))
  cat("   L_ln_debt_gdp is the annual log CHANGE in net debt / GDP (a flow: borrowing net of nominal growth); L_ln_int_tme is a LEVEL (the share of spending pre-empted by debt service = effective rate x debt stock / spending).\n")
  panel(list(main = core, "+ln_psnd_gdp" = F(c(CORE_X, FT, "L_ln_psnd_gdp")), "ln_psnd_gdp" = F(c(setdiff(CORE_X, "L_ln_debt_gdp"), "L_ln_psnd_gdp", FT)), "+d_psnd_gdp" = F(c(CORE_X, FT, "L_d_psnd_gdp")), "+psnb_gdp" = F(c(CORE_X, FT, "L_psnb_gdp"))),
        sprintf("Table 2. The level and the change of debt / GDP beside the debt-interest share, %s", L),
        c("delta_oadr_10", FT, "L_ln_debt_gdp", "L_ln_psnd_gdp", "L_d_psnd_gdp", "L_psnb_gdp", "L_tax_workpop", "L_ln_vac_unem", "L_mig_gross"), w = 16,
        notes = "ln_psnd_gdp (bare): the log level of debt / GDP in place of the log change; +: added beside it.")
  # ---- 3 replacements ----
  cat("\n3. Borrowing, spending and cost-of-borrowing terms in place of the debt-interest share\n")
  R <- function(v) F(c(CORE_X, v)); alts <- list(main = core, none = F(CORE_X), "ln_int_gdp" = R("L_ln_int_gdp"), "ln_debt_service_tax" = R("L_ln_debt_service_tax"), "ln_eff_rate" = R("L_ln_eff_rate"), "ln_psnd_gdp" = R("L_ln_psnd_gdp"),
              "psnb_gdp" = R("L_psnb_gdp"), "ln_tme_gdp" = R("L_ln_tme_gdp"), "ln_tme_obr_gdp" = R("L_ln_tme_obr_gdp"), "ln_grants_gdp" = R("L_ln_grants_gdp"), "ln_tax_gdp" = R("L_ln_tax_gdp"))
  panel(alts[1:6], sprintf("Table 3a. Debt-service and debt-stock terms in place of the debt-interest share, %s", L),
        c("delta_oadr_10", FT, "L_ln_int_gdp", "L_ln_debt_service_tax", "L_ln_eff_rate", "L_ln_psnd_gdp", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 20)
  panel(alts[c(1, 7:11)], sprintf("Table 3b. Borrowing and spending shares in place of the debt-interest share, %s", L),
        c("delta_oadr_10", FT, "L_psnb_gdp", "L_ln_tme_gdp", "L_ln_tme_obr_gdp", "L_ln_grants_gdp", "L_ln_tax_gdp", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 16)
  costs <- list(main = core, "ln_tbill" = R("L_ln_tbill"), "ln_gilt" = R("L_ln_gilt"), "bank_rate" = R("L_bank_rate"), "term_spread" = R("L_term_spread"), "real_rate" = R("L_real_rate"), "r_g" = R("L_r_g"), "rdebt" = R("L_rdebt"), "rg_debt" = R("L_rg_debt"))
  panel(costs, sprintf("Table 3c. The cost of borrowing in place of the debt-interest share, %s", L),
        c("delta_oadr_10", FT, "L_ln_tbill", "L_ln_gilt", "L_bank_rate", "L_term_spread", "L_real_rate", "L_r_g", "L_rdebt", "L_rg_debt", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 13)
  pairs <- list(main = core, "ln_psnd_gdp + ln_tbill" = R(c("L_ln_psnd_gdp", "L_ln_tbill")), "ln_psnd_gdp + ln_gilt" = R(c("L_ln_psnd_gdp", "L_ln_gilt")), "psnb_gdp + ln_tbill" = R(c("L_psnb_gdp", "L_ln_tbill")),
                "ln_psnd_gdp + ln_tme_gdp" = R(c("L_ln_psnd_gdp", "L_ln_tme_gdp")), "ln_int_gdp + ln_tbill" = R(c("L_ln_int_gdp", "L_ln_tbill")), "+ln_tbill" = F(c(CORE_X, FT, "L_ln_tbill")))
  panel(pairs, sprintf("Table 3d. Logical pairs: the stock of debt with its price, borrowing with its price, debt with spending, %s", L),
        c("delta_oadr_10", FT, "L_ln_psnd_gdp", "L_psnb_gdp", "L_ln_int_gdp", "L_ln_tbill", "L_ln_gilt", "L_ln_tme_gdp", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_mig_gross"), w = 26,
        notes = "Debt interest = effective rate x debt stock: the stock-with-price pairs are the unrestricted version of the debt-interest term; +ln_tbill adds the current bill rate beside the share (does the market rate carry information the accrued cost does not?).")
  # summary line: AIC against the core specification re-estimated on the SAME observations as each alternative (series differ in coverage)
  allm <- c(alts[-1], costs[-1], pairs[-1]); sm <- t(sapply(names(allm), function(n) { m <- allm[[n]]; if (is.null(m)) return(c(NA, NA, NA, NA, NA)); yrs <- attr(m, "dd")$year; c0 <- fit(S[S$year %in% yrs, ], c(CORE_X, FT), OUTC); ct <- coeftest(m, vcov. = vcovHAC(m))
    xs <- setdiff(names(coef(m)), c("(Intercept)", CORE_X)); c(nobs(m), AIC(m) - AIC(c0), if (length(xs)) min(ct[xs, 4]) else NA, ct["delta_oadr_10", 1], ct["delta_oadr_10", 4]) }))
  colnames(sm) <- c("N", "dAIC vs main", "min p (term)", "proj", "proj p"); cat("\n   Summary (AIC relative to the core specification with the debt-interest share on the same observations; negative = better; min p = the smallest HAC p among the replacing terms):\n"); print(round(sm[order(sm[, 2]), ], 4)) }
cat("\nDone.\n")
