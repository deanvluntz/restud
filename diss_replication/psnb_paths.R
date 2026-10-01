# =============================================================================
# psnb_paths.R -- projected borrowing over multi-year horizons from the Budget forecasts, alone and with interest/yield indicators (Sam, 1 Oct 2026)
# Source: OBR Historical Official Forecasts Database, Spring 2026 (determinants/obr_hofd_spring2026.xlsx): PSNB (PSBR before November 1994) as a
# per cent of GDP from March 1977, in GBP from April 1970 (converted with outturn nominal GDP where the % sheet is missing). Multi-year paths begin
# with the Medium-Term Financial Strategy (March 1980); earlier Budgets forecast only the current year, so every path variable starts in 1980.
# Vintage rule -- the NPP method (vintage_for(y) = latest base <= y): for year t and horizon h, the latest forecast published by the end of July
# of t whose path reaches t + h (summer Budgets of 1994-97 run only to t + 1, so the preceding November forecast is used for longer horizons);
# a forecast more than 18 months old is not used. Missing interior years of a path are filled by linear interpolation, as for the NPP
# interim years. The second 'July 1996' label is July 1997 (vintage dates forced to increase).
# Path variables, h = 1..4 years (P(j) = forecast PSNB/GDP for fiscal year j in the chosen vintage; positive = borrowing):
#   chg_h    P(t+h) - P(t)                      projected change in borrowing (the analogue of delta_oadr_10)
#   chgb_h   P(t+h) - P(t-1)                    projected change from the base-year estimate in the same forecast
#   avg_h    mean of P(t..t+h)                  rolling average of the projected path
#   wavg_h   weights h+1, h, ..., 1 on P(t..t+h) (near years heavier)
#   cum_h    sum of P(t+1..t+h)                 borrowing projected to be added to the debt
#   ldebt_h  ln d(t+h) - ln d(t), d(j) = d(j-1) / (1 + g) + P(j)/100 from the outturn debt ratio of t-1, g = mean nominal GDP growth of the
#            five years to t-1 (known at the time): the projected log change in debt/GDP (the forward analogue of ln_debt_gdp)
#   Each at t (the Budget of year t, start-of-year information) and t-1. 6 forms x 4 horizons x 2 timings = 48 path variables.
# Interest / yield indicators (t-1; the widest set built so far): int_tme (Millennium, incl. banks), ln_int_tme, int_xb_tme (banks excluded),
# int_cg_tme (CG net of APF), rint_xb_tme (real), int_gdp, ln_int_gdp, tbill, ln_tbill, bank_rate, gilt (10-year), ln_gilt, consol, r_g,
# real_rate, and from gilt_yields.R: gry_10, gry_all, gry_short, gry_long, slope, real_gry_10, d_gry_10 = 22.
# Combinations with each path variable: alongside (P + I), interaction (P + I + P x I) and the product alone (P x I: projected borrowing
# priced at the current rate, a projected debt-service term). 48 x 22 x 3 = 3,168 combinations + 48 alone + 22 indicators alone.
# Common window 1981-2019 (N 39) so that every AIC is comparable; base = the 6-parameter core (gross inflows, projection, tax per WAP, log
# change in debt/GDP, log V/U). Sam's criterion: the added terms are jointly significant (HAC Wald p < .10) and lower the AIC, (i) beside the
# projection AND (ii) without it. The single-sample window means the both-samples rule cannot be applied; every count is reported, because a
# screen of ~3,200 combinations produces ~10% 'significant' rows by chance.
# Outputs: psnb_paths_screen.csv (every row), determinants/obr_psnb_paths_1980_2019.csv (the path variables).
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
invisible(capture.output(source("fiscal_ratio.R")))                                   # S (1965-2019) with the clean interest ratios; A = full frame
f <- "determinants/obr_hofd_spring2026.xlsx"; SH <- readxl::excel_sheets(f)
hofd <- function(sh) { a <- suppressMessages(as.data.frame(readxl::read_excel(f, sh, col_names = FALSE, .name_repair = "minimal", col_types = "text")))
  hr <- which(a[[1]] == "Back to contents")[1]; yr <- as.integer(substr(unlist(a[hr, -1]), 1, 4)); out <- NULL; last <- as.Date("1900-01-01")
  for (r in (hr + 1):nrow(a)) { lab <- a[[1]][r]; if (is.na(lab) || !grepl("^[A-Z][a-z]+ [0-9]{4}$", lab)) next
    d <- as.Date(paste("1", lab), "%d %B %Y"); if (is.na(d)) next; while (d <= last) d <- seq(d, by = "1 year", length.out = 2)[2]; last <- d
    v <- suppressWarnings(as.numeric(unlist(a[r, -1]))); k <- !is.na(v) & !is.na(yr)
    if (any(k)) out <- rbind(out, data.frame(vint = d, target = yr[k], value = v[k])) }
  out }
ot <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); ot$gdp_bn <- 100 * ot$tme_bn / ot$tme_gdp
pg <- hofd("PSNB"); pb <- hofd(grep("^.PSNB$", SH, value = TRUE)[1]); pb$value <- 100 * pb$value / ot$gdp_bn[match(pb$target, ot$year)]
pb <- pb[!paste(pb$vint, pb$target) %in% paste(pg$vint, pg$target), ]; P <- rbind(pg, pb)                          # % sheet first, GBP/GDP where missing
V <- split(P, P$vint); vd <- as.Date(names(V))
path <- function(t, h) {                                                                                             # P(t-1..t+h) from the NPP-rule vintage
  ok <- which(vd <= as.Date(sprintf("%d-07-31", t)) & vd >= as.Date(sprintf("%d-02-01", t - 1)))
  for (k in rev(ok)) { v <- V[[k]]; if (min(v$target) <= t && max(v$target) >= t + h) {
    x <- if (nrow(v) < 2) v$value[match((t - 1):(t + h), v$target)] else approx(v$target, v$value, xout = (t - 1):(t + h), rule = 1)$y; names(x) <- (t - 1):(t + h); return(x) } }
  NULL }
gdpg <- 100 * c(NA, diff(log(ot$gdp_bn))); names(gdpg) <- ot$year
yrs <- 1975:2019; PV <- data.frame(year = yrs)
for (h in 1:4) for (t in yrs) { x <- path(t, h); i <- which(PV$year == t); if (is.null(x)) next; p <- x[as.character(t:(t + h))]; if (any(is.na(p))) next
  PV[i, paste0("chg_", h)] <- p[h + 1] - p[1]; PV[i, paste0("chgb_", h)] <- if (is.na(x[1])) NA else p[h + 1] - x[1]
  PV[i, paste0("avg_", h)] <- mean(p); PV[i, paste0("wavg_", h)] <- sum((h + 1):1 * p) / sum(1:(h + 1)); PV[i, paste0("cum_", h)] <- sum(p[-1])
  d0 <- A$psnd_gdp[A$year == t - 1] / 100; g <- mean(gdpg[as.character((t - 5):(t - 1))]) / 100
  if (length(d0) && !is.na(d0) && !is.na(g)) { d <- d0 / (1 + g) + p[1] / 100; dt <- d; for (j in 1:h) d <- d / (1 + g) + p[j + 1] / 100; PV[i, paste0("ldebt_", h)] <- log(d) - log(dt) } }
PVAR <- setdiff(names(PV), "year"); write.csv(PV, "determinants/obr_psnb_paths_1980_2019.csv", row.names = FALSE, na = "")
cat(sprintf("Projected borrowing paths: %d variables; first year %s\n", length(PVAR), paste(range(vapply(PVAR, function(v) min(PV$year[!is.na(PV[[v]])]), 0)), collapse = "-")))
print(round(PV[PV$year %in% c(1980, 1985, 1990, 1994, 2000, 2008, 2009, 2010, 2015, 2019), c("year", "chg_2", "chg_4", "avg_4", "cum_4", "ldebt_4")], 3), row.names = FALSE)
for (v in PVAR) { S[[paste0("t_", v)]] <- PV[[v]][match(S$year, PV$year)]; S[[paste0("L_", v)]] <- PV[[v]][match(S$year - 1, PV$year)] }
gy <- read.csv("determinants/gilt_yields_bgs_1976_2025.csv"); gy$d_gry_10 <- c(NA, diff(gy$gry_10)); gy$real_gry_10 <- gy$gry_10 - S$inflation[match(gy$year, S$year)]
for (v in c("gry_10", "gry_all", "gry_short", "gry_long", "slope", "real_gry_10", "d_gry_10")) S[[paste0("L_", v)]] <- gy[[v]][match(S$year - 1, gy$year)]
S$L_ln_tbill <- if ("L_ln_tbill" %in% names(S)) S$L_ln_tbill else log(S$L_tbill)
IND <- c("L_int_tme", "L_ln_int_tme", "L_int_xb_tme", "L_int_cg_tme", "L_rint_xb_tme", "L_int_gdp", "L_ln_int_gdp", "L_tbill", "L_ln_tbill", "L_bank_rate", "L_gilt",
         "L_ln_gilt", "L_consol", "L_r_g", "L_real_rate", "L_gry_10", "L_gry_all", "L_gry_short", "L_gry_long", "L_slope", "L_real_gry_10", "L_d_gry_10")
PTS <- c(paste0("t_", PVAR), paste0("L_", PVAR))
CORE6 <- setdiff(CORE_A, "L_int_tme"); NP <- setdiff(CORE6, "delta_oadr_10")
d <- subset(S, year >= 1981 & year <= 2019); need <- c("mig_gross", CORE6, PTS, IND); miss <- need[colSums(is.na(d[, need])) > 0]
if (length(miss)) cat("NOTE: missing values on 1981-2019 in:", paste(miss, collapse = ", "), "-- rows using them run on fewer years\n")
m0 <- lm(reformulate(CORE6, "mig_gross"), d); n0 <- lm(reformulate(NP, "mig_gross"), d)
jt <- function(m, add) { V <- tryCatch(vcovHAC(m), error = function(e) NULL); b <- coef(m)[add]; if (is.null(V) || any(is.na(b))) return(c(NA, NA))
  W <- tryCatch(drop(t(b) %*% solve(V[add, add, drop = FALSE], b)), error = function(e) NA); c(W / length(add), stats::pf(W / length(add), length(add), df.residual(m), lower.tail = FALSE)) }
run <- function(xs, add, label, kind, pv, iv) { dd <- d[complete.cases(d[, unique(c("mig_gross", CORE6, xs))]), , drop = FALSE]; if (nrow(dd) < 30) return(NULL)
  b0 <- if (nrow(dd) == nrow(d)) m0 else lm(reformulate(CORE6, "mig_gross"), dd); c0 <- if (nrow(dd) == nrow(d)) n0 else lm(reformulate(NP, "mig_gross"), dd)
  m <- lm(reformulate(c(CORE6, xs), "mig_gross"), dd); n <- lm(reformulate(c(NP, xs), "mig_gross"), dd)
  if (any(is.na(coef(m))) || any(is.na(coef(n)))) return(NULL)
  a <- jt(m, add); e <- jt(n, add); pr <- coeftest(m, vcov. = vcovHAC(m))["delta_oadr_10", ]
  data.frame(label = label, kind = kind, path = pv, ind = iv, N = nrow(dd), b1 = coef(m)[add[1]], p_with = a[2], dAIC_with = AIC(m) - AIC(b0), p_without = e[2],
             dAIC_without = AIC(n) - AIC(c0), b1_without = coef(n)[add[1]], proj = pr[1], p_proj = pr[4],
             pass = !is.na(a[2]) && a[2] < .10 && AIC(m) < AIC(b0) && !is.na(e[2]) && e[2] < .10 && AIC(n) < AIC(c0), row.names = NULL) }
if (!isTRUE(getOption("restud.paths_only"))) {                                               # the screen (skipped when another script needs only the paths)
R <- list()
for (p in PTS) R[[length(R) + 1]] <- run(p, p, p, "path alone", p, "")
for (i in IND) R[[length(R) + 1]] <- run(i, i, i, "indicator alone", "", i)
for (p in PTS) for (i in IND) { d[[".px"]] <- d[[p]] * d[[i]]; nm <- paste0(p, "_x_", i); d[[nm]] <- d[[".px"]]
  R[[length(R) + 1]] <- run(c(p, i), c(p, i), paste(p, "+", i), "alongside", p, i)
  R[[length(R) + 1]] <- run(c(p, i, nm), c(p, i, nm), paste(p, "*", i), "interaction", p, i)
  R[[length(R) + 1]] <- run(nm, nm, paste(p, "x", i), "product", p, i) }
R <- do.call(rbind, R); write.csv(R, "psnb_paths_screen.csv", row.names = FALSE)
ref <- run("L_int_tme", "L_int_tme", "int_tme", "reference", "", "L_int_tme")
cat(sprintf("\nWindow 1981-2019, N %d. Reference: int_tme beside the 6-parameter core %.4f, joint p %s, dAIC %.2f; without the projection p %s, dAIC %.2f\n",
            nrow(d), ref$b1, pf2(ref$p_with), ref$dAIC_with, pf2(ref$p_without), ref$dAIC_without))
cat(sprintf("Rows: %d. Joint p < .10 beside the projection: %d (%.0f%%); without it: %d (%.0f%%); pass both halves of the criterion: %d (%.1f%%)\n", nrow(R),
            sum(R$p_with < .10, na.rm = TRUE), 100 * mean(R$p_with < .10, na.rm = TRUE), sum(R$p_without < .10, na.rm = TRUE), 100 * mean(R$p_without < .10, na.rm = TRUE),
            sum(R$pass), 100 * mean(R$pass)))
cat(sprintf("Projection coefficient across all rows: median %.4f, range %.4f to %.4f; p < .05 in %.0f%% of rows\n", median(R$proj), min(R$proj), max(R$proj), 100 * mean(R$p_proj < .05)))
cat("\n-- path variables alone (beside the projection | without it) --\n")
pa <- R[R$kind == "path alone", ]; pa <- pa[order(pa$dAIC_with), ]
for (k in seq_len(nrow(pa))) with(pa[k, ], cat(sprintf("   %-12s N %d  %8.4f (p %5s) dAIC %6.2f | %8.4f (p %5s) dAIC %6.2f  proj %.4f%s\n", label, N, b1, pf2(p_with), dAIC_with, b1_without,
                                                     pf2(p_without), dAIC_without, proj, if (pass) "  PASS" else "")))
cat("\n-- indicators alone --\n"); ia <- R[R$kind == "indicator alone", ]; ia <- ia[order(ia$dAIC_with), ]
for (k in seq_len(nrow(ia))) with(ia[k, ], cat(sprintf("   %-14s N %d  %8.4f (p %5s) dAIC %6.2f | %8.4f (p %5s) dAIC %6.2f  proj %.4f%s\n", label, N, b1, pf2(p_with), dAIC_with, b1_without,
                                                     pf2(p_without), dAIC_without, proj, if (pass) "  PASS" else "")))
cat("\n-- passes by kind and path form --\n"); R$form <- sub("^(t|L)_([a-z]+)_[0-9]$", "\\2", R$path); R$timing <- ifelse(grepl("^t_", R$path), "t", ifelse(grepl("^L_", R$path), "t-1", ""))
print(with(R[R$kind %in% c("alongside", "interaction", "product"), ], table(kind, pass)))
print(with(R[R$kind %in% c("alongside", "interaction", "product") & R$pass, ], table(form, timing)))
cat("\n-- the 25 passing combinations with the lowest AIC beside the projection --\n")
ps <- R[R$pass & R$kind %in% c("alongside", "interaction", "product"), ]; ps <- ps[order(ps$dAIC_with), ]
for (k in seq_len(min(25, nrow(ps)))) with(ps[k, ], cat(sprintf("   %-40s %-11s dAIC %6.2f (p %5s) | without: dAIC %6.2f (p %5s)  proj %.4f (p %s)\n", label, kind, dAIC_with, pf2(p_with),
                                                        dAIC_without, pf2(p_without), proj, pf2(p_proj))))
cat("\n-- indicators that pass most often (share of their combinations passing) --\n")
tb <- aggregate(pass ~ ind, R[R$kind %in% c("alongside", "interaction", "product"), ], mean); tb <- tb[order(-tb$pass), ]; print(head(tb, 10), row.names = FALSE)
cat("\nDone.\n")

cat("\n-- checks on the path variables that pass alone (1981-2019 window): episodes, sub-period, leave-one-out, diagnostics, beside int_tme --\n")
for (p in pa$label[pa$pass]) { dd <- d[complete.cases(d[, c("mig_gross", CORE6, p)]), ]
  f1 <- function(x) { m <- lm(reformulate(c(CORE6, p), "mig_gross"), x); ct <- coeftest(m, vcov. = vcovHAC(m)); c(ct[p, 1], ct[p, 4], ct["delta_oadr_10", 1]) }
  a <- f1(dd); b <- f1(subset(dd, !year %in% 2008:2011)); c2 <- f1(subset(dd, year <= 2007)); loo <- sapply(dd$year, function(y) f1(subset(dd, year != y))[1])
  m <- lm(reformulate(c(CORE6, p), "mig_gross"), dd); mi <- lm(reformulate(c(CORE6, p, "L_int_tme"), "mig_gross"), dd); ci <- coeftest(mi, vcov. = vcovHAC(mi))
  cat(sprintf("   %-9s all %.4f (p %s) | without 2008-11 %.4f (p %s) | to 2007 %.4f (p %s) | leave-one-out %.4f to %.4f | RESET %s BG(2) %s | beside int_tme: %.4f (p %s), int_tme %.4f (p %s), proj %.4f\n",
              p, a[1], pf2(a[2]), b[1], pf2(b[2]), c2[1], pf2(c2[2]), min(loo), max(loo), pf2(resettest(m)$p.value), pf2(bgtest(m, 2)$p.value),
              ci[p, 1], pf2(ci[p, 4]), ci["L_int_tme", 1], pf2(ci["L_int_tme", 4]), ci["delta_oadr_10", 1])) }
cat(sprintf("   Sign: chg = projected borrowing in t+h minus t (negative = consolidation planned); a positive coefficient = inflows rise when the last Budget\n   projected borrowing to rise (or to fall less), i.e. a deteriorating projected fiscal path, the same direction as the demographic projection.\n"))
cat(sprintf("   Correlation of L_chg_2 with the projection %.2f, with L_ln_debt_gdp %.2f, with L_int_tme %.2f (1981-2019)\n", cor(d$L_chg_2, d$delta_oadr_10, use = "pair"),
            cor(d$L_chg_2, d$L_ln_debt_gdp, use = "pair"), cor(d$L_chg_2, d$L_int_tme, use = "pair")))
cat("\n-- projected (L_chg_2) vs realised (L_ln_debt_gdp) debt dynamics, 1981-2019 --\n")
for (sp in list(list("core (realised debt change)", CORE6), list("core + L_chg_2", c(CORE6, "L_chg_2")), list("L_chg_2 in place of L_ln_debt_gdp", sub("^L_ln_debt_gdp$", "L_chg_2", CORE6)),
                list("core + L_chg_2 + L_int_tme", c(CORE6, "L_chg_2", "L_int_tme")))) { m <- lm(reformulate(sp[[2]], "mig_gross"), d); ct <- coeftest(m, vcov. = vcovHAC(m))
  cat(sprintf("   %-36s AIC %7.2f  BG(2) %s  max VIF %5.1f  proj %.4f (p %s)%s%s\n", sp[[1]], AIC(m), pf2(bgtest(m, 2)$p.value), max(car::vif(m)), ct["delta_oadr_10", 1], pf2(ct["delta_oadr_10", 4]),
              if ("L_ln_debt_gdp" %in% rownames(ct)) sprintf("  debt chg %.3f (p %s)", ct["L_ln_debt_gdp", 1], pf2(ct["L_ln_debt_gdp", 4])) else "",
              if ("L_chg_2" %in% rownames(ct)) sprintf("  chg_2 %.4f (p %s)", ct["L_chg_2", 1], pf2(ct["L_chg_2", 4])) else "")) }
cat("Done.\n")

}
