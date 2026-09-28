# =============================================================================
# manuscript_1964.R -- the manuscript pipeline on the 1964 extension (series B,
# 1965-2019): Tables 2, 3, 4 and B2 of Hamdane_ReStud_Manuscript_1964.docx,
# the Figure 4 rows, the timing placebo, the second-lag check, the bootstrap
# bias correction, the permits and British-citizen checks and the stability
# statistics. Writes tables_1964.json in the structure build_docx_1964.js reads
# (the same as tables_1977.json, plus a basis_pre1975 term and extra blocks).
#
# The document was first built from a Python replica of this pipeline (matches
# the R set-up to four decimals on every check so far); this script is the
# authoritative run. Run from diss_replication/:
#     source("manuscript_1964.R")
# then copy tables_1964.json to the manuscript folder and rebuild with
#     node build_docx_1964.js
#
# Data: sheet `1964` of the workbook (extension + extras pasted by Sam) with
# determinants/extension_1962_2019_extras.csv for anything the sheet lacks;
# ReformSPA and PubShiftOADR_ZM for the 1975+ columns the extension does not
# carry (EU, LTIM, zero-migration, publication-dated, oadr_10 levels, labour
# and per-resident taxes); npp_1963_1974_oadr.csv and fiscal_block_1962_1974.csv
# for their pre-1975 counterparts; ips_301 for British citizens; work_permits.csv.
# Conventions as everywhere in the paper: HAC = sandwich::vcovHAC defaults;
# NW = NeweyWest(lag = 1, prewhite = FALSE, adjust = TRUE); tseries adf.test.
# =============================================================================
suppressPackageStartupMessages({library(readxl); library(lmtest); library(sandwich); library(car); library(tseries); library(jsonlite); library(ARDL); library(strucchange)})
set.seed(20260802)

# --- data ---------------------------------------------------------------------
WB <- "Dissertation_Dataset_Final.xlsx"
raw <- as.data.frame(read_excel(WB, sheet = "1964")); raw <- raw[!is.na(raw$year), ]; raw$dr_note <- NULL
raw <- raw[, !vapply(raw, function(x) all(is.na(x)), logical(1))]
for (v in setdiff(names(raw), "year")) raw[[v]] <- as.numeric(raw[[v]])
ex <- read.csv("determinants/extension_1962_2019_extras.csv"); ex$dr_note <- NULL
add <- setdiff(names(ex), names(raw)); if (length(add)) raw <- merge(raw, ex[, c("year", add)], by = "year", all.x = TRUE)
raw <- raw[order(raw$year), ]
ref <- as.data.frame(read_excel(WB, sheet = "ReformSPA")); ref <- ref[!is.na(ref$year), ]
pub <- as.data.frame(read_excel(WB, sheet = "PubShiftOADR_ZM")); pub <- pub[!is.na(pub$year), ]
o63 <- read.csv("determinants/npp_1963_1974_oadr.csv"); f62 <- read.csv("determinants/fiscal_block_1962_1974.csv")
ips <- read.csv("determinants/ips_301_citizenship_by_residence_1975_2019.csv", check.names = FALSE)
wp  <- read.csv("determinants/work_permits.csv")
g <- function(tab, col, key = "year") tab[[col]][match(raw$year, tab[[key]])]
lag1 <- function(x) c(NA, head(x, -1))

df <- data.frame(year = raw$year, workpop = raw$workpop)
wl <- lag1(df$workpop)
df$y     <- log(1000 * (1000 * raw$dv_B_splice_raw) / wl)        # series B, log per 1,000 WAP (t-1)
df$g     <- log(1000 * raw$dv_B_splice_raw)                       # gross, persons
df$p     <- raw$delta_oadr_10; df$dr <- raw$delta_dr_10; df$step <- raw$basis_pre1975
df$tax   <- raw$tax_workpop; df$debt <- raw$ln_debt_gdp; df$un <- raw$unem
df$lev   <- ifelse(df$year < 1975, g(o63, "oadr_10"), g(ref, "oadr_10"))
df$labtax <- ifelse(df$year < 1975, g(f62, "tax_lab_pw"), g(ref, "lab_tax_workpop"))
df$taxres <- ifelse(df$year < 1975, g(f62, "tax_nat_real_gbp_bn") / g(f62, "totpop_m"), g(ref, "tax_totpop"))
df$eu    <- log(g(ref, "EU_inflows_WAPlagK")); df$ltim <- log(g(ref, "ltim_nEU_inflows_WAPlagK"))
df$zm    <- g(ref, "delta_zm_10"); df$p_pub <- as.numeric(g(pub, "delta_oadr_10_pub"))
df$uk_growth <- raw$uk_growth; df$inflation <- raw$inflation; df$brit_out <- raw$brit_out
df$relinc_dev <- raw$relinc_dev; df$relinc_rich <- raw$relinc_rich; df$youth_origin <- raw$youth_origin
df$network_B <- raw$network_B; df$post1968 <- raw$post1968; df$post1973 <- raw$post1973
df$brit <- log(1000 * (1000 * g(ips, "Inflow_NON-EU_brit")) / wl)
# BoE Millennium controls for the three extra Figure 4 rows (end 2016; see boe_tests.R)
boe <- read.csv("determinants/boe_extras_1962_2019.csv"); a27 <- read.csv("determinants/boe_a27_tax_variants_1956_2019.csv")
df$vac_per_wap <- 1000 * (1000 * g(boe, "vacancies_k")) / df$workpop; df$dln_vu <- c(NA, diff(log(g(boe, "vu_ratio"))))
df$nics_pw <- g(a27, "cg_nics_real_pw"); df$nics_emp_pw <- g(a27, "cg_nics_employees_real_pw")
df$permits_k <- g(wp, "permits") / 1000
for (v in c("y", "g", "eu", "ltim", "un", "tax", "labtax", "taxres", "debt", "uk_growth", "inflation", "brit_out", "vac_per_wap", "dln_vu", "nics_pw", "nics_emp_pw",
            "relinc_dev", "relinc_rich", "youth_origin", "brit", "permits_k")) df[[paste0("L_", v)]] <- lag1(df[[v]])
df$L2_y <- lag1(df$L_y); df$L_brit_out_rate <- 1000 * (1000 * df$L_brit_out) / wl
df$POL97 <- as.integer(df$year >= 1998); df$POL02 <- as.integer(df$year >= 2002); df$EXP04 <- as.integer(df$year >= 2004)
df$ENL86 <- as.integer(df$year >= 1986); df$ENL95 <- as.integer(df$year >= 1995)
df$liberal <- as.integer(df$year >= 1998 & df$year <= 2007); df$trend <- df$year - 1965 + 1
S   <- subset(df, year >= 1965 & year <= 2019); stopifnot(nrow(S) == 55)
S77 <- subset(df, year >= 1977 & year <= 2019)

# names the document builder expects
RN <- c(y = "log_mig_rate", L_y = "L_log_mig_rate", L_tax = "L_tax_workpop", p = "delta_oadr_10", step = "basis_pre1975",
        L_debt = "L_ln_debt_gdp", L_un = "L_unem", dr = "delta_dr_10", lev = "level_oadr_10", zm = "delta_zm_10",
        L_eu = "L_eu_log_mig_rate", L_g = "L_alt_log_mig_rate", L_labtax = "L_tax_lab_pw", trend = "trend")
rn <- function(x) { n <- names(x); n2 <- ifelse(n %in% names(RN), RN[n], n); names(x) <- n2; x }
hac <- function(m) coeftest(m, vcov. = vcovHAC(m))
grab <- function(m, d) {
  ct <- hac(m); nm <- rownames(ct)
  list(coef = as.list(rn(setNames(ct[, 1], nm))), se = as.list(rn(setNames(ct[, 2], nm))), p = as.list(rn(setNames(ct[, 4], nm))),
       n = unname(nobs(m)), adjr2 = summary(m)$adj.r.squared, bg2 = bgtest(m, order = 2)$p.value, bp = bptest(m)$p.value,
       reset = resettest(m)$p.value, jb = jarque.bera.test(residuals(m))$p.value,
       adf = suppressWarnings(adf.test(residuals(m))$p.value), maxvif = tryCatch(max(vif(m)), error = function(e) NA_real_),
       years = { yr <- d$year[match(rownames(model.frame(m)), rownames(d))]; paste0(min(yr), "-", max(yr)) })
}
fit <- function(d, y, xs) lm(reformulate(xs, y), data = d)
CTRL <- c("L_debt", "L_un"); FISC <- c("L_y", "L_tax", "p", "step", CTRL); TRND <- c("L_y", "trend", "p", "step", CTRL)

# --- Table 2 -------------------------------------------------------------------
tax_spec <- fit(S, "y", FISC); trend_spec <- fit(S, "y", TRND)
out <- list(generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R = paste(R.version$major, R.version$minor, sep = "."),
            sample = "1965-2019, series B (non-British to 1974, non-EU from 1975), basis step in every column")
out$table2 <- list(fiscal = grab(tax_spec, S), trend = grab(trend_spec, S))

# --- Table 3: bounds test and long-run multipliers on 1965-2019 ----------------
ts_df <- ts(S[, c("y", "p", "un", "debt", "tax", "step")], start = 1965, frequency = 1)
colnames(ts_df) <- c("log_mig_rate", "delta_oadr_10", "unem", "ln_debt_gdp", "tax_workpop", "basis_pre1975")
m_ardl <- ardl(log_mig_rate ~ delta_oadr_10 + unem + ln_debt_gdp + tax_workpop | basis_pre1975, data = ts_df, order = c(1, 1, 1, 1, 1))
lr <- multipliers(m_ardl, type = "lr")
set.seed(20260802); bf <- bounds_f_test(m_ardl, case = 3, exact = TRUE)
set.seed(20260802); bt <- bounds_t_test(m_ardl, case = 3, exact = TRUE)
out$table3 <- list(terms = as.character(lr$Term), est = lr$Estimate, se = lr$`Std. Error`, p = lr$`Pr(>|t|)`,
                   boundsF = list(stat = unname(bf$statistic), p = unname(bf$p.value)), boundsT = list(stat = unname(bt$statistic), p = unname(bt$p.value)))
cat(sprintf("Bounds: F = %.3f (p %.3f), t = %.3f (p %.3f)\n", bf$statistic, bf$p.value, bt$statistic, bt$p.value)); print(lr)

# --- Table 4 (fiscal) and B3 (trend) -------------------------------------------
t4 <- function(spec) {
  base <- if (spec == "fiscal") "L_tax" else "trend"
  cols <- list(dr = grab(fit(S, "y", c("L_y", base, "dr", "step", CTRL)), S),
               eu = grab(fit(S77, "eu", c("L_eu", base, "p", CTRL)), S77),
               gross = grab(fit(S, "g", c("L_g", base, "p", "step", CTRL)), S),
               level = grab(fit(S, "y", c("L_y", base, "lev", "step", CTRL)), S),
               zm = grab(fit(S77, "y", c("L_y", base, "zm", CTRL)), S77))
  if (spec == "fiscal") cols$lab <- grab(fit(S, "y", c("L_y", "L_labtax", "p", "step", CTRL)), S)
  cols$spec <- spec; cols
}
out$table4 <- t4("fiscal"); out$table4_trend <- t4("trend")

# --- Table B2: three inference methods -----------------------------------------
ols <- coeftest(tax_spec); h <- hac(tax_spec)
nw <- coeftest(tax_spec, vcov. = NeweyWest(tax_spec, lag = 1, prewhite = FALSE, adjust = TRUE))
out$tableB2 <- list(terms = unname(ifelse(rownames(ols) %in% names(RN), RN[rownames(ols)], rownames(ols))),
                    coef = unname(ols[, 1]), ols = unname(ols[, 2]), hac = unname(h[, 2]), nw = unname(nw[, 2]))

# --- Figure 4 rows ----------------------------------------------------------------
est <- function(lab, d, y, xs, term) { m <- fit(d, y, xs); ct <- hac(m)
  data.frame(label = lab, coef = ct[term, 1], se = ct[term, 2], p = ct[term, 4], n = nobs(m)) }
R <- rbind(
  est("Baseline (fiscal specification), 1965-2019", S, "y", FISC, "p"),
  est("Basis step omitted", S, "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("Zero-net-migration projection, 1977-2019", S77, "y", c("L_y", "L_tax", "zm", CTRL), "zm"),
  est("Publication-dated projection, 1978-2019", S77, "y", c("L_y", "L_tax", "p_pub", CTRL), "p_pub"),
  est("Gross (unscaled) inflows", S, "g", c("L_g", "L_tax", "p", "step", CTRL), "p"),
  est("Projected level in place of change", S, "y", c("L_y", "L_tax", "lev", "step", CTRL), "lev"),
  est("Labour taxes per working-age person", S, "y", c("L_y", "L_labtax", "p", "step", CTRL), "p"),
  est("All taxes per resident", S, "y", c("L_y", "L_taxres", "p", "step", CTRL), "p"),
  est("+ 1998 policy step (POL97)", S, "y", c(FISC, "POL97"), "p"),
  est("+ 1998-2007 policy window", S, "y", c(FISC, "liberal"), "p"),
  est("+ all five MPR indicators", S, "y", c(FISC, "POL97", "POL02", "EXP04", "ENL86", "ENL95"), "p"),
  est("+ 1968 and 1973 Act steps", S, "y", c(FISC, "post1968", "post1973"), "p"),
  est("+ linear trend", S, "y", c(FISC, "trend"), "p"),
  est("+ supply-side controls (3)", S, "y", c(FISC, "L_relinc_dev", "L_relinc_rich", "L_youth_origin"), "p"),
  est("+ growth, inflation, British emigration", S, "y", c(FISC, "L_uk_growth", "L_inflation", "L_brit_out_rate"), "p"),
  est("Second lag of dependent variable", S, "y", c("L_y", "L2_y", "L_tax", "p", "step", CTRL), "p"),
  est("Sample from 1971", subset(S, year >= 1971), "y", FISC, "p"),
  est("Sample from 1977", S77, "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("Sample from 1979", subset(S, year >= 1979), "y", c("L_y", "L_tax", "p", CTRL), "p"),
  est("Sample to 2003 (pre-accession)", subset(S, year <= 2003), "y", FISC, "p"),
  est("LTIM inflows, 1992-2019", subset(S, year >= 1992), "ltim", c("L_ltim", "L_tax", "p", CTRL), "p"),
  est("+ change in vacancy/unemployment ratio, to 2016", subset(S, year <= 2016), "y", c(FISC, "L_dln_vu"), "p"),
  est("Vacancies per WAP for unemployment, to 2016", subset(S, year <= 2016), "y", c("L_y", "L_tax", "p", "step", "L_debt", "L_vac_per_wap"), "p"),
  est("Employee NICs per WAP for taxes, to 2016", subset(S, year <= 2016), "y", c("L_y", "L_nics_emp_pw", "p", "step", CTRL), "p"))
PL <- rbind(
  est("Placebo: total dependency ratio", S, "y", c("L_y", "L_tax", "dr", "step", CTRL), "dr"),
  est("Placebo: total dependency ratio, alongside OADR", S, "y", c("L_y", "L_tax", "p", "dr", "step", CTRL), "dr"),
  est("Placebo: EU inflows, 1977-2019", S77, "eu", c("L_eu", "L_tax", "p", CTRL), "p"))
out$figure4 <- list(rows = R, placebos = PL)
cat("\n--- Figure 4 rows ---\n"); print(format(rbind(R, PL), digits = 3), row.names = FALSE)
# joint HAC F of the five MPR indicators
m_mpr <- fit(S, "y", c(FISC, "POL97", "POL02", "EXP04", "ENL86", "ENL95"))
print(linearHypothesis(m_mpr, paste0(c("POL97", "POL02", "EXP04", "ENL86", "ENL95"), " = 0"), vcov. = vcovHAC(m_mpr)))

# --- timing placebo, second lag, magnitude, bias -----------------------------------
t_of <- function(x) { d <- S; d$pk <- x; m <- fit(d, "y", c("L_y", "L_tax", "pk", "step", CTRL)); hac(m)["pk", 3] }
t0 <- t_of(S$p); n <- nrow(S); tk <- vapply(1:(n - 1), function(k) t_of(c(tail(S$p, k), head(S$p, -k))), numeric(1))
out$timing_placebo <- list(t_actual = t0, max_abs_t = max(abs(tk)), pct95 = unname(quantile(abs(tk), 0.95)),
                           n_ge = sum(abs(tk) >= abs(t0)), p = (1 + sum(abs(tk) >= abs(t0))) / n)
cat(sprintf("\nTiming placebo: actual t %.2f, largest rotated |t| %.2f, p = %.3f\n", t0, max(abs(tk)), out$timing_placebo$p))
m2 <- fit(S, "y", c("L_y", "L2_y", "L_tax", "p", "step", CTRL)); c2 <- hac(m2)
out$second_lag <- list(coef_p = c2["p", 1], p_p = c2["p", 4], L2 = c2["L2_y", 1], L2_p = c2["L2_y", 4], aic1 = AIC(tax_spec), aic2 = AIC(m2),
                       bg1 = sapply(1:4, function(k) bgtest(tax_spec, order = k)$p.value), bg2 = sapply(1:4, function(k) bgtest(m2, order = k)$p.value))
b <- coef(tax_spec); sdp <- sd(S$p)
out$magnitude <- list(sd_proj = sdp, impact_pct = 100 * (exp(b["p"] * sdp) - 1), longrun_pct = 100 * (exp(b["p"] * sdp / (1 - b["L_y"])) - 1))
# recursive-design residual bootstrap of the lagged-DV bias (2,000 replications)
X <- model.matrix(tax_spec); e <- residuals(tax_spec) - mean(residuals(tax_spec)); iy <- which(colnames(X) == "L_y"); ip <- which(colnames(X) == "p")
rhos <- bp <- numeric(2000)
for (r in 1:2000) {
  es <- sample(e, n, replace = TRUE); Xs <- X; lag <- X[1, iy]; ys <- numeric(n)
  for (t in 1:n) { Xs[t, iy] <- lag; ys[t] <- sum(Xs[t, ] * b) + es[t]; lag <- ys[t] }
  bb <- lm.fit(Xs, ys)$coefficients; rhos[r] <- bb[iy]; bp[r] <- bb[ip]
}
bias <- mean(rhos) - b[iy]; rho_c <- b[iy] - bias; lr_boot <- bp / (1 - (rhos - bias)); lr_c <- b[ip] / (1 - rho_c)
out$bias <- list(rho = unname(b[iy]), bias = unname(bias), rho_corrected = unname(rho_c), analytic = unname(-(1 + 3 * b[iy]) / n),
                 lr_uncorrected = unname(b[ip] / (1 - b[iy])), lr_corrected = unname(lr_c), ci = unname(quantile(2 * lr_c - lr_boot, c(0.025, 0.975))))
cat(sprintf("Bias: rho %.3f, bootstrap bias %.3f (analytic %.3f), corrected rho %.3f; long run %.4f -> %.4f [%.4f, %.4f]\n",
            b[iy], bias, out$bias$analytic, rho_c, out$bias$lr_uncorrected, lr_c, out$bias$ci[1], out$bias$ci[2]))

# --- permits (1965-2008), British citizens (1977-2019), stability -----------------------
P <- subset(df, year >= 1965 & year <= 2008); mp <- fit(P, "permits_k", c("L_permits_k", "p", "step", "L_tax", CTRL)); cp <- hac(mp)
mp2 <- fit(P, "permits_k", c("L_permits_k", "p", "dr", "step", "L_tax", CTRL)); cp2 <- hac(mp2)
mp79 <- fit(subset(df, year >= 1979 & year <= 2008), "permits_k", c("L_permits_k", "p", "L_tax", CTRL)); cp79 <- hac(mp79)
out$permits <- list(n = nobs(mp), coef = cp["p", 1], p = cp["p", 4], both_p = cp2["p", 1], both_p_p = cp2["p", 4], both_dr = cp2["dr", 1], both_dr_p = cp2["dr", 4],
                    n79 = nobs(mp79), coef79 = cp79["p", 1], p79 = cp79["p", 4])
mb <- fit(S77, "brit", c("L_brit", "p", "L_tax", CTRL)); cb <- hac(mb); mc <- fit(S77, "y", c("L_y", "p", "brit", "L_tax", CTRL)); cc2 <- hac(mc)
out$british <- list(n = nobs(mb), coef = cb["p", 1], p = cb["p", 4], ctrl_coef = cc2["p", 1], ctrl_p = cc2["p", 4])
rec <- t(sapply(1990:2019, function(en) { m <- fit(subset(S, year <= en), "y", FISC); ct <- hac(m); c(en, ct["p", 1], ct["p", 4]) }))
roll <- t(sapply(1965:1995, function(st) { d <- subset(S, year >= st & year <= st + 24); xs <- c("L_y", "L_tax", "p", if (st < 1975) "step", CTRL)
  m <- fit(d, "y", xs); ct <- hac(m); c(st, ct["p", 1], ct["p", 4]) }))
out$stability <- list(recursive = rec, rolling = roll, cusum_p = sctest(efp(formula(tax_spec), data = S, type = "OLS-CUSUM"))$p.value)
cat(sprintf("\nPermits 1965-2008: %.3f (p %.3f); with DR alongside %.3f vs %.4f (p %.2f). British 1977-2019: %.4f (p %.3f); control -> %.4f.\nOLS-CUSUM p = %.2f; rolling 25-yr windows significant at 5%% in %d of %d.\n",
            cp["p", 1], cp["p", 4], cp2["p", 1], cp2["dr", 1], cp2["dr", 4], cb["p", 1], cb["p", 4], cc2["p", 1], out$stability$cusum_p, sum(roll[, 3] < 0.05), nrow(roll)))

write_json(out, "tables_1964.json", auto_unbox = TRUE, digits = NA, pretty = TRUE)
cat("\nwritten tables_1964.json\n")
