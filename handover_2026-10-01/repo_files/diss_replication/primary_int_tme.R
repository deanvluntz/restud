# =============================================================================
# primary_int_tme.R -- EXPLORATORY (Sam, 28 Sep 2026; "out of curiosity", not for the manuscripts): the fiscal specification
# with the debt-interest share of spending (L_int_tme) as a PRIMARY regressor, run through the main pipeline on both samples:
#     mig_rate = a + rho L_mig_rate + b1 delta_oadr_10 + b2 L_tax_workpop + b3 L_ln_debt_gdp - b4 L_unem + b5 L_int_tme [+ basis_pre1975]
#   1  Table 2 analogue      fiscal (baseline) | fiscal +int_tme, each sample (2016 re-estimate printed under the panel)
#   2  Table 3 analogue      ARDL(1,...) bounds test (PSS case 3, ARDL package) and long-run multipliers with int_tme as a level regressor
#   3  Falsification battery with int_tme in every column: total DR (in place / alongside), EU placebo (core), gross inflows,
#      horizons 1-10, zero-migration and publication-dated projections (core), permits, British citizens, sub-samples, second lag
#   4  Timing placebo (rotations of the projection) and the recursive/rolling stability of the projection coefficient
# Names and definitions: tables_common.R; data: data_common.R. Run:  source("primary_int_tme.R")
# =============================================================================
source("tables_common.R"); suppressPackageStartupMessages({library(ARDL); library(strucchange); library(readxl)})
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); list(df = df, STEP = STEP, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
WB <- "Dissertation_Dataset_Final.xlsx"; ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA", .name_repair = "minimal")); ref <- ref[!is.na(ref$year), ]
pub <- as.data.frame(read_excel(WB, sheet = "PubShiftOADR_ZM", .name_repair = "minimal")); pub <- pub[!is.na(pub$year), ]
swap <- function(base, drop, add) c(setdiff(base, drop), add)
line <- function(k, m, vs) { if (is.null(m)) return(invisible()); ct <- coeftest(m, vcov. = vcovHAC(m)); cat(sprintf("%-34s", k)); for (v in vs) if (v %in% rownames(ct)) cat(sprintf(" | %s %.4f (p %s)", v, ct[v, 1], pf(ct[v, 4]))); cat(sprintf(" | N %d, AIC %.2f, RESET %s\n", nobs(m), AIC(m), pf(resettest(m)$p.value))) }
cat("\n##### EXPLORATORY: debt interest as a primary regressor #####\n")
for (k in names(SMP)) {
  d <- SMP[[k]]$df; STEP <- SMP[[k]]$STEP; Y0 <- SMP[[k]]$Y0; LAB <- SMP[[k]]$LAB; S <- subset(d, year >= Y0 & year <= 2019); TAG <- paste0(LAB, "2019")
  d$delta_zm_10 <- as.numeric(ref$delta_zm_10)[match(d$year, ref$year)]; d$p_pub <- as.numeric(pub$delta_oadr_10_pub)[match(d$year, pub$year)]; S <- subset(d, year >= Y0 & year <= 2019)
  B0 <- c("L_mig_rate", "L_tax_workpop", "delta_oadr_10", STEP, "L_ln_debt_gdp", "L_unem"); B <- c(B0, "L_int_tme"); CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_int_tme")
  # --- 1 Table 2 analogue ------------------------------------------------------------------------------------------
  panel(list("fiscal" = fit(S, B0), "fiscal +int_tme" = fit(S, B)), paste0("1. The fiscal specification with debt interest (", TAG, ")"),
        c("delta_oadr_10", "L_int_tme", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_mig_rate", STEP), w = 18, nested_F = TRUE,
        "L_int_tme for 2017-19 is spliced from ONS/OBR onto the Millennium A28 series; the 2016 re-estimate is printed above.")
  m <- fit(S, B); ct <- coeftest(m, vcov. = vcovHAC(m)); bb <- coef(m)
  cat(sprintf("Long run (b / (1 - rho)): projection %.4f, int_tme %.4f; one-SD move in int_tme (%.2f pp) = %.1f%% of inflows on impact, %.1f%% in the long run.\n",
              bb["delta_oadr_10"] / (1 - bb["L_mig_rate"]), bb["L_int_tme"] / (1 - bb["L_mig_rate"]), sd(S$L_int_tme), 100 * (exp(bb["L_int_tme"] * sd(S$L_int_tme)) - 1), 100 * (exp(bb["L_int_tme"] * sd(S$L_int_tme) / (1 - bb["L_mig_rate"])) - 1)))
  # --- 2 bounds test and long-run multipliers -----------------------------------------------------------------------
  vv <- c("mig_rate", "delta_oadr_10", "unem", "ln_debt_gdp", "tax_workpop", "int_tme", STEP); ok <- complete.cases(S[, vv])
  ts_df <- ts(S[ok, vv], start = min(S$year[ok]), frequency = 1)
  fm <- if (is.null(STEP)) mig_rate ~ delta_oadr_10 + unem + ln_debt_gdp + tax_workpop + int_tme else mig_rate ~ delta_oadr_10 + unem + ln_debt_gdp + tax_workpop + int_tme | basis_pre1975
  m_ardl <- ardl(fm, data = ts_df, order = rep(1, 6)); lr <- multipliers(m_ardl, type = "lr")
  set.seed(20260802); bf <- bounds_f_test(m_ardl, case = 3, exact = TRUE); set.seed(20260802); bt <- bounds_t_test(m_ardl, case = 3, exact = TRUE)
  cat(sprintf("\n2. ARDL(1,1,1,1,1,1) bounds test with int_tme (%s): F = %.3f (p %.3f), t = %.3f (p %.3f). Long-run multipliers:\n", TAG, bf$statistic, bf$p.value, bt$statistic, bt$p.value)); print(lr)
  # --- 3 falsification battery ---------------------------------------------------------------------------------------
  cat("\n3. Falsification battery with int_tme in every column (", TAG, ")\n", sep = "")
  line("total DR in place of OADR", fit(S, swap(B, "delta_oadr_10", "delta_dr_10")), c("delta_dr_10", "L_int_tme"))
  line("total DR alongside", fit(S, c(B, "delta_dr_10")), c("delta_oadr_10", "delta_dr_10", "L_int_tme"))
  if (k == "core") { S77 <- subset(S, !is.na(eu_rate)); line("EU placebo (eu_rate)", fit(S77, c("L_eu_rate", "delta_oadr_10", CT), "eu_rate"), c("delta_oadr_10", "L_int_tme"))
    line("zero-migration projection", fit(S, swap(B, "delta_oadr_10", "delta_zm_10")), c("delta_zm_10", "L_int_tme"))
    line("publication-dated projection", fit(S, swap(B, "delta_oadr_10", "p_pub")), c("p_pub", "L_int_tme")) }
  line("gross inflows (mig_gross)", fit(S, swap(B, "L_mig_rate", "L_mig_gross"), "mig_gross"), c("delta_oadr_10", "L_int_tme"))
  for (h in c(1, 3, 5, 7)) line(sprintf("horizon %d years", h), fit(S, swap(B, "delta_oadr_10", paste0("delta_oadr_", h))), c(paste0("delta_oadr_", h), "L_int_tme"))
  P <- subset(d, year >= max(Y0, 1965) & year <= 2008); line("permits (thousands), to 2008", fit(P, c("L_permits", "delta_oadr_10", CT), "permits"), c("delta_oadr_10", "L_int_tme"))
  line("British citizens (brit_rate)", fit(S, c("L_brit_rate", "delta_oadr_10", CT), "brit_rate"), c("delta_oadr_10", "L_int_tme"))
  line("+brit_rate as control", fit(S, c(B, "brit_rate")), c("delta_oadr_10", "L_int_tme"))
  line("+L2_mig_rate", fit(S, c(B, "L2_mig_rate")), c("delta_oadr_10", "L_int_tme", "L2_mig_rate"))
  sub <- if (k == "extended") list(c(1965, 1991), c(1992, 2019), c(1965, 2003), c(1975, 2019)) else list(c(1977, 1997), c(1998, 2019), c(1977, 2003))
  for (r in sub) line(sprintf("sub-sample %d-%d", r[1], r[2]), fit(subset(d, year >= r[1] & year <= r[2]), B), c("delta_oadr_10", "L_int_tme"))
  # --- 4 timing placebo and stability ----------------------------------------------------------------------------------
  t_of <- function(x) { dd <- S; dd$pk <- x; mm <- fit(dd, swap(B, "delta_oadr_10", "pk")); coeftest(mm, vcov. = vcovHAC(mm))["pk", 3] }
  n <- nrow(S); t0 <- t_of(S$delta_oadr_10); tk <- vapply(1:(n - 1), function(j) t_of(c(tail(S$delta_oadr_10, j), head(S$delta_oadr_10, -j))), numeric(1))
  cat(sprintf("4. Timing placebo: actual t %.2f, largest rotated |t| %.2f, randomisation p = %.3f.\n", t0, max(abs(tk)), (1 + sum(abs(tk) >= abs(t0))) / n))
  rec <- vapply(seq(1990, 2019, by = 5), function(en) coef(fit(subset(S, year <= en), B))["delta_oadr_10"], numeric(1))
  cat(sprintf("   Recursive (sample ending %s): %s. OLS-CUSUM p = %.2f.\n", paste(seq(1990, 2019, by = 5), collapse = "/"), paste(sprintf("%.4f", rec), collapse = "/"),
              sctest(efp(formula(m), data = attr(m, "dd"), type = "OLS-CUSUM"))$p.value))
}
