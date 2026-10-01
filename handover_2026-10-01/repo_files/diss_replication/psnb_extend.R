# =============================================================================
# psnb_extend.R -- follow-ups to psnb_paths.R (Sam, 1 Oct 2026)
# A. L_chg_2 works only beside the realised log change in debt/GDP (suppression). Transformations that KEEP debt/GDP inside one term:
#    traj_2      realised + projected debt path: L_ln_debt_gdp (t-2 -> t-1, outturn) + L_ldebt_2 (the Budget of t-1's projected log change in
#                debt/GDP over the next two years); in PLACE of L_ln_debt_gdp (same number of parameters)
#    chg2_debt   L_chg_2 / L_psnd_gdp x 100: the planned change in borrowing relative to the debt ratio (a ratio), beside L_ln_debt_gdp
#    chg2_x_debt L_chg_2 x L_ln_debt_gdp: in place of L_chg_2 (product) and with both main terms (interaction)
# B. Earlier years with empty values, so that BOTH samples can be used (Citizen 1965-2019 and 1977-2019):
#    B1 zeros before the paths exist (1981 for the t-1 term), no indicator     B2 zeros + a missing-value indicator (= a pre-1981 step:
#    reported, but it is a period indicator, which Sam excludes on theory)      B3 backfill with the planned ONE-year change in the same Budget,
#    P(t) - P(t-1), available from the April 1970 Budget (GBP over outturn GDP before 1977), zeros before
# C. Tax and debt in one price basis: tax per WAP and the one-term burden (taxes + real debt interest excl. banks per WAP) deflated with
#    the GDP deflator (current), CPI or RPI -- the same index deflating the taxes AND measuring the inflation erosion of the debt.
#    (The log change in debt/GDP is a ratio of two nominal series: no deflator enters it.) CPI and RPI from Millennium A47 (spliced
#    indices, to 2016), extended with ONS annual inflation 2017-19 (CPI 2.7, 2.5, 1.8; RPI 3.6, 3.3, 2.6); calendar years to the fiscal year
#    ending in March of t with weights 0.75 / 0.25; rebased so that 2019 equals the deflator-based value (scale only).
# D. RATIOS built on int_tme's own identity (interest / total managed expenditure), forward-looking:
#    fwd_int_h_r  projected interest share of spending: [clean debt interest/GDP + (borrowing projected over the next h years, % of GDP) x
#                 the gilt rate / 100] / TME/GDP x 100 -- int_tme as it would be once the projected borrowing is financed at today's rate.
#                 Before the paths exist the projected part is empty (zero), so the term equals the clean interest share and both
#                 samples are usable. h = 1, 2, 3; rate = constructed 10-year GRY (gry_10) or all-stock GRY (gry_all); t-1.
#    new_int_h_r  the projected part alone (new interest on projected borrowing / TME), zero before the paths exist
#    chg2_over_r  L_chg_2 divided by each positive rate (the raw ratio form Sam asked about)
#    Each in place of int_tme (i.e. added to the 6-parameter core), both samples where defined.
# E. The 232 screen rows that passed: how close each comes to int_tme's identity -- the correlation of the combined contribution of its
#    added terms (fitted) with L_int_tme on 1981-2019, and with the clean share L_int_xb_tme.
# Sam's criterion throughout: the added term(s) jointly HAC-significant (p < .10) and AIC lower, with AND without the projection.
# RStudio: working directory = diss_replication/, then source() this file (it builds the paths via psnb_paths.R without its screen).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.paths_only = TRUE); invisible(capture.output(source("psnb_paths.R"))); options(restud.paths_only = FALSE)
jt <- function(m, add) { V <- tryCatch(vcovHAC(m), error = function(e) NULL); b <- coef(m)[add]; if (is.null(V) || any(is.na(b))) return(NA)
  W <- tryCatch(drop(t(b) %*% solve(V[add, add, drop = FALSE], b)), error = function(e) NA); stats::pf(W / length(add), length(add), df.residual(m), lower.tail = FALSE) }
ev <- function(dd, xs, add, base = CORE6) { dd <- dd[complete.cases(dd[, unique(c("mig_gross", xs, base))]), ]
  m <- lm(reformulate(xs, "mig_gross"), dd); m0 <- lm(reformulate(base, "mig_gross"), dd)
  n <- lm(reformulate(setdiff(xs, "delta_oadr_10"), "mig_gross"), dd); n0 <- lm(reformulate(setdiff(base, "delta_oadr_10"), "mig_gross"), dd)
  pr <- coeftest(m, vcov. = vcovHAC(m))["delta_oadr_10", ]; a <- jt(m, add); e <- jt(n, add)
  list(N = nrow(dd), y0 = min(dd$year), b = coef(m)[add[1]], p = a, dA = AIC(m) - AIC(m0), bn = coef(n)[add[1]], pn = e, dAn = AIC(n) - AIC(n0), proj = pr[1], pp = pr[4],
       pass = !is.na(a) && a < .10 && AIC(m) < AIC(m0) && !is.na(e) && e < .10 && AIC(n) < AIC(n0)) }
pr <- function(lab, r) cat(sprintf("   %-34s %d-2019 N %2d  %9.4f (p %5s) dAIC %6.2f | no proj %9.4f (p %5s) dAIC %6.2f | proj %.4f (p %s)%s\n", lab, r$y0, r$N, r$b, pf2(r$p),
                                    r$dA, r$bn, pf2(r$pn), r$dAn, r$proj, pf2(r$pp), if (r$pass) "  PASS" else ""))
Sall <- S                                                                                         # 1965-2019 frame from fiscal_ratio.R + path variables

cat("==================== A. Keeping debt/GDP in the term (1981-2019) ====================\n")
Sall$L_traj_2 <- Sall$L_ln_debt_gdp + Sall$L_ldebt_2
Sall$L_chg2_debt <- 100 * Sall$L_chg_2 / Sall$L_psnd_gdp
Sall$L_chg2_x_debt <- Sall$L_chg_2 * Sall$L_ln_debt_gdp
d81 <- subset(Sall, year >= 1981)
pr("reference: + L_chg_2", ev(d81, c(CORE6, "L_chg_2"), "L_chg_2"))
pr("traj_2 for ln_debt_gdp", ev(d81, sub("^L_ln_debt_gdp$", "L_traj_2", CORE6), "L_traj_2"))
cat(sprintf("     (traj_2 row: dAIC is against the core with L_ln_debt_gdp; same number of parameters)\n"))
pr("+ chg2_debt (ratio)", ev(d81, c(CORE6, "L_chg2_debt"), "L_chg2_debt"))
pr("+ chg2_x_debt (product)", ev(d81, c(CORE6, "L_chg2_x_debt"), "L_chg2_x_debt"))
pr("+ chg_2 * ln_debt_gdp (interaction)", ev(d81, c(CORE6, "L_chg_2", "L_chg2_x_debt"), c("L_chg_2", "L_chg2_x_debt")))
cat(sprintf("   correlations, 1981-2019: chg2_debt with ln_debt_gdp %.2f; traj_2 with ln_debt_gdp %.2f; projected part (L_ldebt_2) with ln_debt_gdp %.2f\n",
            cor(d81$L_chg2_debt, d81$L_ln_debt_gdp, use = "pair"), cor(d81$L_traj_2, d81$L_ln_debt_gdp, use = "pair"), cor(d81$L_ldebt_2, d81$L_ln_debt_gdp, use = "pair")))

cat("\n==================== B. Earlier years with empty values (both samples) ====================\n")
one <- sapply(1966:2019, function(t) { x <- path(t, 0); if (is.null(x)) NA else unname(x[2] - x[1]) }); names(one) <- 1966:2019
lagv <- function(v) v[match(Sall$year - 1, Sall$year)]
Sall$b1 <- ifelse(is.na(Sall$L_chg_2), 0, Sall$L_chg_2); Sall$miss <- as.numeric(is.na(Sall$L_chg_2))
one_t <- one[as.character(Sall$year - 1)]; Sall$b3 <- ifelse(!is.na(Sall$L_chg_2), Sall$L_chg_2, ifelse(is.na(one_t), 0, one_t))
cat(sprintf("   planned one-year change P(t) - P(t-1) available from the Budget of %d; two-year path from %d (t-1 term from %d)\n", min(as.numeric(names(one))[!is.na(one)]),
            min(PV$year[!is.na(PV$chg_2)]), min(Sall$year[!is.na(Sall$L_chg_2)])))
for (y0 in c(1965, 1977)) { dd <- subset(Sall, year >= y0); cat(sprintf("  -- %d-2019\n", y0))
  pr("B1 zeros", ev(dd, c(CORE6, "b1"), "b1")); pr("B2 zeros + missing indicator", ev(dd, c(CORE6, "b1", "miss"), c("b1", "miss")))
  pr("B3 one-year backfill", ev(dd, c(CORE6, "b3"), "b3")) }

cat("\n==================== C. Tax and debt in one price basis: GDP deflator vs CPI vs RPI ====================\n")
mf <- "../a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx"
a47 <- suppressMessages(as.data.frame(readxl::read_excel(mf, "A47. Wages and prices", col_names = FALSE, .name_repair = "minimal"))); yy <- suppressWarnings(as.numeric(a47[[1]]))
idx <- function(col, ext) { v <- suppressWarnings(as.numeric(a47[[col]])); k <- !is.na(yy) & !is.na(v) & yy <= 2016; z <- data.frame(year = yy[k], i = v[k])
  for (j in seq_along(ext)) z <- rbind(z, data.frame(year = 2016 + j, i = tail(z$i, 1) * (1 + ext[j] / 100))); z }
CP <- idx(4, c(2.7, 2.5, 1.8)); RP <- idx(7, c(3.6, 3.3, 2.6))
fy <- function(Z) { c0 <- Z$i[match(A$year, Z$year)]; 0.75 * pv(c0) + 0.25 * c0 }
defl <- 100 * A$tax_workpop / (A$tax_workpop / 1)                                                # placeholder (overwritten below)
ref <- as.data.frame(readxl::read_excel("Dissertation_Dataset_Final.xlsx", "ReformSPA")); dfl <- as.numeric(ref[["deflator24_25=100"]])[match(A$year, ref$year)]
if (any(is.na(dfl[A$year >= 1965 & A$year <= 2019]))) { e <- read.csv("determinants/deflator_1956_1975_HMT.csv"); k <- is.na(dfl); dfl[k] <- e[[2]][match(A$year[k], e[[1]])] }
nom_pw <- A$tax_workpop * dfl / 100                                                             # nominal taxes per WAP (GBP 000)
for (nm in c("cpi", "rpi")) { I <- fy(if (nm == "cpi") CP else RP); k19 <- which(A$year == 2019)
  A[[paste0("tax_workpop_", nm)]] <- nom_pw / I * I[k19] * 100 / dfl[k19]
  infl <- 100 * c(NA, diff(log(I))); adjI <- infl / 100 * pv(A$psnd_gdp)
  A[[paste0("taxrint_xb_", nm)]] <- A[[paste0("tax_workpop_", nm)]] * (1 + (A$int_xb_gdp - adjI) / A$tax_gdp) }
for (v in c("tax_workpop_cpi", "tax_workpop_rpi", "taxrint_xb_cpi", "taxrint_xb_rpi")) Sall[[paste0("L_", v)]] <- lag1(A[[v]])[match(Sall$year, A$year)]
cat(sprintf("   check: deflator-based tax per WAP rebuilt from nominal x deflator matches tax_workpop exactly; real tax per WAP 1975 / 2019 (GBP 000): deflator %.1f / %.1f, CPI %.1f / %.1f, RPI %.1f / %.1f\n",
            A$tax_workpop[A$year == 1975], A$tax_workpop[A$year == 2019], A$tax_workpop_cpi[A$year == 1975], A$tax_workpop_cpi[A$year == 2019], A$tax_workpop_rpi[A$year == 1975], A$tax_workpop_rpi[A$year == 2019]))
for (y0 in c(1965, 1977)) { dd <- subset(Sall, year >= y0); cat(sprintf("  -- %d-2019 (each in place of tax per WAP; dAIC against tax per WAP, GDP deflator)\n", y0))
  for (v in c("L_tax_workpop_cpi", "L_tax_workpop_rpi", "L_taxrint_xb", "L_taxrint_xb_cpi", "L_taxrint_xb_rpi")) pr(sub("^L_", "", v), ev(dd, sub("^L_tax_workpop$", v, CORE6), v)) }

cat("\n==================== D. Ratios on int_tme's identity (interest / TME), forward-looking ====================\n")
gy2 <- read.csv("determinants/gilt_yields_bgs_1976_2025.csv"); tmeg <- ot$tme_gdp[match(Sall$year - 1, ot$year)]
intg <- A$int_xb_gdp[match(Sall$year - 1, A$year)]
Dv <- character(0)
for (h in 1:3) for (r in c("gry_10", "gry_all")) { cum <- Sall[[paste0("L_cum_", h)]]; rr <- gy2[[r]][match(Sall$year - 1, gy2$year)]
  newp <- ifelse(is.na(cum), 0, cum * ifelse(is.na(rr), 0, rr) / 100)
  Sall[[paste0("fwd_int_", h, "_", r)]] <- 100 * (intg + newp) / tmeg; Sall[[paste0("new_int_", h, "_", r)]] <- 100 * newp / tmeg; Dv <- c(Dv, paste0(c("fwd_int_", "new_int_"), h, "_", r)) }
for (r in c("L_gry_10", "L_gry_all", "L_gry_long", "L_tbill", "L_bank_rate", "L_int_xb_tme", "L_int_tme")) { nm <- paste0("chg2_over_", sub("^L_", "", r)); Sall[[nm]] <- Sall$L_chg_2 / Sall[[r]]; Dv <- c(Dv, nm) }
cat(sprintf("   correlation with L_int_tme / L_int_xb_tme (1981-2019): %s\n", paste(sprintf("%s %.2f/%.2f", Dv[grepl("^fwd", Dv)], sapply(Dv[grepl("^fwd", Dv)], function(v) cor(Sall[[v]], Sall$L_int_tme, use = "pair")),
            sapply(Dv[grepl("^fwd", Dv)], function(v) cor(Sall[[v]], Sall$L_int_xb_tme, use = "pair"))), collapse = "; ")))
for (y0 in c(1965, 1977, 1981)) { dd <- subset(Sall, year >= y0); cat(sprintf("  -- from %d (added to the 6-parameter core, in place of int_tme)\n", y0))
  for (v in Dv) { if (y0 < 1981 && grepl("^chg2_over", v)) next; pr(v, ev(dd, c(CORE6, v), v)) }
  if (y0 < 1981) pr("reference: int_xb_tme (clean share)", ev(dd, c(CORE6, "L_int_xb_tme"), "L_int_xb_tme"))
  pr("reference: int_tme (Millennium)", ev(dd, c(CORE6, "L_int_tme"), "L_int_tme")) }

cat("\n==================== E. The 232 passing screen rows: closeness to int_tme's identity ====================\n")
SC <- read.csv("psnb_paths_screen.csv"); SC <- SC[SC$pass & SC$kind %in% c("alongside", "interaction", "product"), ]
cl <- t(sapply(seq_len(nrow(SC)), function(i) { p <- SC$path[i]; q <- SC$ind[i]; dd <- d; dd$px <- dd[[p]] * dd[[q]]
  add <- switch(SC$kind[i], alongside = c(p, q), interaction = c(p, q, "px"), product = "px"); dd <- dd[complete.cases(dd[, c("mig_gross", CORE6, add)]), ]
  m <- lm(reformulate(c(CORE6, add), "mig_gross"), dd); contrib <- as.matrix(dd[, add, drop = FALSE]) %*% coef(m)[add]
  c(cor(contrib, dd$L_int_tme, use = "pair"), cor(contrib, dd$L_int_xb_tme, use = "pair")) }))
SC$r_int_tme <- cl[, 1]; SC$r_int_xb <- cl[, 2]; SC <- SC[order(-abs(SC$r_int_tme)), ]
cat("   contribution of the added terms (fitted) correlated with L_int_tme / L_int_xb_tme; top 15 by |r| with int_tme\n")
for (k in 1:15) with(SC[k, ], cat(sprintf("   %-42s %-11s r %+.2f / %+.2f   dAIC %6.2f, no proj %6.2f\n", label, kind, r_int_tme, r_int_xb, dAIC_with, dAIC_without)))
cat(sprintf("   median |r| with int_tme across the 232: %.2f; rows with |r| > .7: %d\n", median(abs(SC$r_int_tme)), sum(abs(SC$r_int_tme) > .7)))
write.csv(SC, "psnb_screen_closeness.csv", row.names = FALSE)
cat("\nDone.\n")
