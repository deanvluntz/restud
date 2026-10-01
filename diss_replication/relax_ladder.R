# =============================================================================
# relax_ladder.R -- additions to the model, (1) from each earlier 'main' specification and (2) along Sam's release order
# (Sam, 30 Sep 2026). Economic terms only; no regime indicators.
# (1) R1 from each starting point (lagged outcome and projection always included; the listed terms fixed; one or two additions from
#     the pool, which here also holds the alternative forms of the core terms not already in the start):
#     S1 published specification  mig_rate ~ L_mig_rate + projection + L_tax_workpop + L_ln_debt_gdp + L_unem (+ basis step, Citizen)
#     S2 gross, published terms   mig_gross ~ L_mig_gross + projection + L_tax_workpop + L_ln_debt_gdp + L_unem
#     S3 gross + vacancies        S2 + L_ln_vac_wap (unemployment retained; 28 Sep)
#     S4 29 Sep configuration     mig_gross ~ L_mig_gross + projection + L_tax_voter + L_ln_debt_gdp + L_ln_vac_unem + L_ln_int_tme
#     S5 current core             mig_gross ~ L_mig_gross + projection + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_int_tme
# (2) Release order (Sam): the debt-interest term goes first, then the debt/GDP term, then the labour-market term; the tax burden,
#     the lagged outcome and the projection stay fixed throughout. From S5:
#     L1 = S5 R1; L2 interest slot free; L3 interest and debt slots free; L4 interest, debt and labour slots free (+ none or one pool term).
# Criteria as relax_screen.R: PASS on a sample = projection HAC p < .05, every non-fixed term HAC p < .10, max VIF of the non-fixed terms
# < 20; REMOVES THE BREAK = sup-F p >= .10 and post-2009 change in the projection HAC p >= .10. dAIC against the starting specification
# (the ladder: against S5) on the same years. Printed for timing B (projection t-1) and A (projection t): (i) best three that pass and
# remove the break on both samples; (ii) best three that pass on both; (iii) best three on the Citizen sample alone (pass there),
# with the same specification's Origin results shown whatever they are. Screening result, not a prior.
# RStudio: working directory = diss_replication/, then source() this file (several minutes).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); suppressWarnings(library(strucchange))
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
SL_FISC <- c("L_int_tme", "L_ln_int_tme", "L_int_gdp_x", "L_tbill", "L_bank_rate", "L_gilt")
SL_DEBT <- c("L_ln_debt_gdp", "L_psnb_gdp", "L_psnd_gdp", "L_d_psnd_gdp")
SL_LAB  <- c("L_ln_vac_unem", "L_unem", "L_ln_vac_wap", "L_dln_vac_unem")
POOL <- c("L_growth", "L_growth_pc", "L_gap_obr", "L_lab_prod", "L_tfp", "L_wage_infl", "L_real_wage", "L_particip", "L_replace",
          "L_inflation", "L_cpi_infl", "st_inf_exp", "L_real_rate", "L_term_spread", "L_m4_growth", "L_r_g", "L_tme_real_growth",
          "L_tme_obr_gdp", "L_grants_gdp", "L_tax_gdp", "L_fc_budget", "L_real_eri", "L_ca_gdp", "L_trade_bal", "L_tot", "L_openness",
          "L_real_house", "L_unem_dest", "L_unem_diff", "L_relinc_rich", "L_relinc_dev", "L_youth")
STARTS <- list(
  S1 = list(label = "published specification (mig_rate)", dv = "mig_rate", fixed = c("L_mig_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem"), step = TRUE),
  S2 = list(label = "gross, published terms", dv = "mig_gross", fixed = c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")),
  S3 = list(label = "gross + log vacancies/WAP", dv = "mig_gross", fixed = c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_unem", "L_ln_vac_wap")),
  S4 = list(label = "29 Sep configuration", dv = "mig_gross", fixed = c("L_mig_gross", "L_tax_voter", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme")),
  S5 = list(label = "current core", dv = "mig_gross", fixed = c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_int_tme")))
S5F <- STARTS$S5$fixed
LADDER <- list(
  L2 = list(label = "interest slot free", fixed = setdiff(S5F, "L_int_tme"), slots = list(SL_FISC)),
  L3 = list(label = "interest and debt slots free", fixed = setdiff(S5F, c("L_int_tme", "L_ln_debt_gdp")), slots = list(SL_FISC, SL_DEBT)),
  L4 = list(label = "interest, debt and labour slots free", fixed = c("L_mig_gross", "L_tax_workpop"), slots = list(SL_FISC, SL_DEBT, SL_LAB)))
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp
pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
opt <- function(v) c(list(character(0)), as.list(v))
ALLX <- unique(c(SL_FISC, SL_DEBT, SL_LAB, POOL, "L_tax_workpop", "L_tax_voter", "L_mig_rate", "L_mig_gross"))
qols <- function(M, dv, xs) { cols <- c(dv, xs); ok <- rowSums(is.na(M[, cols, drop = FALSE])) == 0; X <- cbind(1, M[ok, xs, drop = FALSE]); y <- M[ok, dv]
  if (any(apply(X[, -1, drop = FALSE], 2, sd) == 0)) return(NULL)
  q <- qr(X); if (q$rank < ncol(X)) return(NULL); b <- qr.coef(q, y); r <- y - X %*% b; n <- length(y); k <- ncol(X); s2 <- sum(r^2) / (n - k)
  se <- sqrt(diag(chol2inv(q$qr[1:k, 1:k, drop = FALSE])) * s2); p <- 2 * pt(-abs(b / se), n - k); names(p) <- c("(Intercept)", xs)
  list(p = p, aic = n * log(sum(r^2) / n) + 2 * k, n = n, yrs = M[ok, "year"]) }
hacp <- function(m) { ct <- coeftest(m, vcov. = vcovHAC(m)); setNames(ct[, 4], rownames(ct)) }
breaks <- function(m, P, dv) { d <- attr(m, "dd"); sf <- suppressWarnings(sctest(Fstats(formula(m), data = d, from = 0.2))$p.value)
  d$post09 <- as.numeric(d$year >= 2009); d$post_x_proj <- d$post09 * d[[P]]; xs <- c(attr(terms(m), "term.labels"), "post09", "post_x_proj")
  mi <- lm(reformulate(xs, dv), data = d); ct <- coeftest(mi, vcov. = vcovHAC(mi)); c(supf = sf, ch = ct["post_x_proj", 1], chp = ct["post_x_proj", 4]) }
# full statistics for one specification (no filtering): used for passing models and for the Origin side of the Citizen-only list
stats1 <- function(S, M, P, dv, fixed, sp, refx) { xs <- c(fixed, sp); q <- qols(M, dv, xs); if (is.null(q)) return(NULL)
  q0 <- qols(M[M[, "year"] %in% q$yrs, , drop = FALSE], dv, refx); m <- fit(S[S$year %in% q$yrs, ], xs, dv); if (is.null(m)) return(NULL); hp <- hacp(m)
  vf <- if (length(sp) >= 2) max(vif(m)[sp]) else NA
  pass <- hp[P] < 0.05 && all(hp[sp] < 0.10) && (is.na(vf) || vf < 20)
  c(pass = pass, dAIC = q$aic - q0$aic, proj = unname(coef(m)[P]), pp = unname(hp[P]), breaks(m, P, dv), n = nobs(m),
    reset = resettest(m)$p.value, bg1 = bgtest(m, 1)$p.value, bg2 = bgtest(m, 2)$p.value, rain = raintest(m)$p.value) }
screen <- function(S, P, dv, fixed, SP, refx) { M <- as.matrix(S[, c("year", unique(c(dv, P, fixed, ALLX, refx)))]); res <- list()
  for (sp in SP) { q <- qols(M, dv, c(fixed, sp)); if (is.null(q) || q$p[P] > 0.10 || any(q$p[sp] > 0.20)) next
    r <- stats1(S, M, P, dv, fixed, sp, refx); if (is.null(r) || !r["pass"]) next
    res[[if (length(sp)) paste(sort(sp), collapse = " + ") else "(none)"]] <- r }
  res }
DATA <- list(); for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019)
  S$L_delta_oadr_10 <- lag1(S$delta_oadr_10); S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$L_int_gdp_x <- lag1(S$int_gdp_x)
  S$L_d_psnd_gdp <- lag1(c(NA, diff(S$psnd_gdp))); DATA[[s]] <- list(S = subset(S, year >= Y0), step = STEP) }
line <- function(k, a, b) { f <- function(x, tag) if (is.null(x)) sprintf("%s: (not estimable)", tag) else sprintf("%s: proj %.4f (p %s) dAIC %6.2f supF %s post09 %+.4f (p %s) RESET %s BG1 %s BG2 %s rain %s N %d%s", tag,
  x["proj"], pf2(x["pp"]), x["dAIC"], pf2(x["supf"]), x["ch"], pf2(x["chp"]), pf2(x["reset"]), pf2(x["bg1"]), pf2(x["bg2"]), pf2(x["rain"]), x["n"], if (!x["pass"]) "  [fails the pass rule]" else "")
  cat(sprintf("   %-50s %s\n   %-50s %s\n", if (k == "(none)") "(no additions)" else k, f(a, "Cit"), "", f(b, "Ori"))) }
report <- function(R, P, dv, fixed_of, refx_of) { k <- intersect(names(R[["1964"]]), names(R[["1977"]]))
  cat(sprintf("   pass on Citizen %d, Origin %d, both %d\n", length(R[["1964"]]), length(R[["1977"]]), length(k)))
  if (length(k)) { sc <- vapply(k, function(x) R[["1964"]][[x]]["dAIC"] + R[["1977"]][[x]]["dAIC"], 0)
    brk <- k[vapply(k, function(x) all(c(R[["1964"]][[x]][c("supf", "chp")], R[["1977"]][[x]][c("supf", "chp")]) >= 0.10), logical(1))]
    cat(sprintf("   (i) pass and remove the break on both samples: %d%s\n", length(brk), if (length(brk)) ". Best by summed dAIC:" else ""))
    for (x in head(brk[order(sc[brk])], 3)) line(x, R[["1964"]][[x]], R[["1977"]][[x]])
    cat("   (ii) pass on both samples, best by summed dAIC:\n"); for (x in head(k[order(sc)], 3)) line(x, R[["1964"]][[x]], R[["1977"]][[x]]) }
  kc <- names(R[["1964"]]); if (length(kc)) { sc1 <- vapply(kc, function(x) R[["1964"]][[x]]["dAIC"], 0)
    cat("   (iii) best on the Citizen sample alone (Origin shown for the same specification, whatever it gives):\n")
    for (x in head(kc[order(sc1)], 3)) { sp <- if (x == "(none)") character(0) else strsplit(x, " \\+ ")[[1]]; D <- DATA[["1977"]]
      M <- as.matrix(D$S[, c("year", unique(c(dv, P, fixed_of("1977"), ALLX, refx_of("1977"))))])
      line(x, R[["1964"]][[x]], tryCatch(stats1(D$S, M, P, dv, fixed_of("1977"), sp, refx_of("1977")), error = function(e) NULL)) } } }
for (P in c("L_delta_oadr_10", "delta_oadr_10")) { cat(sprintf("\n\n==================== Timing %s ====================\n", if (P == "L_delta_oadr_10") "B (projection t-1)" else "A (projection t)"))
  cat("\n######## (1) R1 from each earlier main specification ########\n")
  for (st in names(STARTS)) { z <- STARTS[[st]]; fx <- function(s) c(P, z$fixed, if (isTRUE(z$step)) DATA[[s]]$step)
    pool <- setdiff(unique(c(POOL, SL_FISC, SL_DEBT, SL_LAB)), z$fixed); SP <- c(as.list(pool), combn(pool, 2, simplify = FALSE))
    cat(sprintf("\n-- %s %s: %d specifications\n", st, z$label, length(SP)))
    R <- lapply(c("1964", "1977"), function(s) screen(DATA[[s]]$S, P, z$dv, fx(s), SP, fx(s))); names(R) <- c("1964", "1977"); report(R, P, z$dv, fx, fx) }
  cat("\n######## (2) Release order from the current core (tax, lagged outcome, projection fixed) ########\n")
  refS5 <- function(s) c(P, S5F)
  for (ln in names(LADDER)) { lv <- LADDER[[ln]]; fx <- function(s) c(P, lv$fixed)
    base <- expand.grid(lapply(lv$slots, function(v) seq_len(length(v) + 1)), KEEP.OUT.ATTRS = FALSE)
    SP <- list(); for (i in seq_len(nrow(base))) { sl <- unlist(mapply(function(v, j) opt(v)[[j]], lv$slots, as.integer(base[i, ]), SIMPLIFY = FALSE))
      for (a in c(list(character(0)), as.list(POOL))) SP[[length(SP) + 1]] <- c(sl, a) }
    cat(sprintf("\n-- %s %s: %d specifications (dAIC against the current core)\n", ln, lv$label, length(SP)))
    R <- lapply(c("1964", "1977"), function(s) screen(DATA[[s]]$S, P, "mig_gross", fx(s), SP, refS5(s))); names(R) <- c("1964", "1977"); report(R, P, "mig_gross", fx, refS5) } }
cat("\nDone.\n")
