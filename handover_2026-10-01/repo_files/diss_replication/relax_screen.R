# =============================================================================
# relax_screen.R -- additions to the core model with gradually less restriction on the core terms (Sam, 30 Sep 2026).
# Economic terms only (no regime indicators: Sam, they are inconsistent with the theory behind the model).
# Outcome: mig_gross (gross non-EU inflows, log persons). Two timings: B (projection t-1; the timing with the post-2008 break) and
# A (projection t; current). Economic terms at t-1 throughout (inflation expectations at t, Sam's rule).
# Restriction ladder (the lagged outcome and the projection are always fixed):
#   R1  fixed: L_mig_gross, projection, L_tax_workpop, L_ln_debt_gdp, L_ln_vac_unem, L_int_tme          + one or two pool terms
#   R2  fixed: L_mig_gross, projection, L_tax_workpop, L_int_tme; debt slot and labour slot free          + none or one pool term
#   R3  fixed: L_mig_gross, projection, L_tax_workpop; fiscal, debt and labour slots free               + none or one pool term
#   R4  fixed: L_mig_gross, projection; tax, fiscal, debt and labour slots free                          + none or one pool term
# Slots (each may also be empty): SL_TAX, SL_FISC, SL_DEBT, SL_LAB below. POOL = the other economic indicators of break_candidates.R.
# A model PASSES on a sample when: projection HAC p < .05; every non-fixed term HAC p < .10; max VIF of the non-fixed terms < 20.
# It also REMOVES THE BREAK when sup-F p >= .10 and the post-2009 change in the projection (post dummy + post x projection) has HAC
# p >= .10. Models are compared on both samples; dAIC is against the current core (same timing) fitted on the same years.
# Printed per level and timing: (i) the three best (summed dAIC) that pass AND remove the break on both samples; (ii) the three best
# that pass on both samples regardless of the break, with their break tests shown. A screen over thousands of models: a screening
# result, not a prior. RStudio: working directory = diss_replication/, then source() this file (takes a few minutes).
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R"); suppressWarnings(library(strucchange))
LABS <- c("1964" = "Citizen sample (1965-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
SL_TAX  <- c("L_tax_workpop", "L_tax_voter", "L_tax_res", "L_taxint_wap")
SL_FISC <- c("L_int_tme", "L_ln_int_tme", "L_int_gdp_x", "L_tbill", "L_bank_rate", "L_gilt")
SL_DEBT <- c("L_ln_debt_gdp", "L_psnb_gdp", "L_psnd_gdp", "L_d_psnd_gdp")
SL_LAB  <- c("L_ln_vac_unem", "L_unem", "L_ln_vac_wap", "L_dln_vac_unem")
POOL <- c("L_growth", "L_growth_pc", "L_gap_obr", "L_lab_prod", "L_tfp", "L_wage_infl", "L_real_wage", "L_particip", "L_replace",
          "L_inflation", "L_cpi_infl", "st_inf_exp", "L_real_rate", "L_term_spread", "L_m4_growth", "L_r_g", "L_tme_real_growth",
          "L_tme_obr_gdp", "L_grants_gdp", "L_tax_gdp", "L_fc_budget", "L_real_eri", "L_ca_gdp", "L_trade_bal", "L_tot", "L_openness",
          "L_real_house", "L_unem_dest", "L_unem_diff", "L_relinc_rich", "L_relinc_dev", "L_youth")
CORE <- c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_int_tme")
pusf <- read.csv("determinants/ons_pusf_debt_interest_1946_2025.csv"); obr <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv"); obr$gdp_bn <- 100 * obr$tme_bn / obr$tme_gdp
pusf <- merge(pusf, obr[, c("year", "gdp_bn")], by = "year"); pusf$int_gdp_ons <- 100 * pusf$ANLO / 1000 / pusf$gdp_bn
splice <- function(x, yr, ref) { last <- max(yr[!is.na(x)]); r <- ref[match(yr, pusf$year)]; ifelse(yr > last, x[yr == last] * r / r[yr == last], x) }
opt <- function(v) c(list(character(0)), as.list(v))                                   # a slot: empty or one of its terms
LEVELS <- list(
  R1 = list(fixed = CORE, slots = list(), pool2 = TRUE),
  R2 = list(fixed = c("L_mig_gross", "L_tax_workpop", "L_int_tme"), slots = list(SL_DEBT, SL_LAB), pool2 = FALSE),
  R3 = list(fixed = c("L_mig_gross", "L_tax_workpop"), slots = list(SL_FISC, SL_DEBT, SL_LAB), pool2 = FALSE),
  R4 = list(fixed = c("L_mig_gross"), slots = list(SL_TAX, SL_FISC, SL_DEBT, SL_LAB), pool2 = FALSE))
specs <- function(lv) { base <- expand.grid(lapply(lv$slots, function(v) seq_len(length(v) + 1)), KEEP.OUT.ATTRS = FALSE); out <- list()
  adds <- if (lv$pool2) c(as.list(POOL), combn(POOL, 2, simplify = FALSE)) else c(list(character(0)), as.list(POOL))
  rows <- if (length(lv$slots)) seq_len(nrow(base)) else 1
  for (i in rows) { sl <- if (length(lv$slots)) unlist(mapply(function(v, j) opt(v)[[j]], lv$slots, as.integer(base[i, ]), SIMPLIFY = FALSE)) else character(0)
    for (a in adds) out[[length(out) + 1]] <- c(sl, a) }
  out }
qols <- function(M, xs) { cols <- c(OUTC, xs); ok <- rowSums(is.na(M[, cols, drop = FALSE])) == 0; X <- cbind(1, M[ok, xs, drop = FALSE]); y <- M[ok, OUTC]
  q <- qr(X); if (q$rank < ncol(X)) return(NULL); b <- qr.coef(q, y); r <- y - X %*% b; n <- length(y); k <- ncol(X); s2 <- sum(r^2) / (n - k)
  se <- sqrt(diag(chol2inv(q$qr[1:k, 1:k, drop = FALSE])) * s2); p <- 2 * pt(-abs(b / se), n - k); names(p) <- c("(Intercept)", xs)
  list(p = p, aic = n * log(sum(r^2) / n) + 2 * k, n = n, yrs = M[ok, "year"]) }
hacp <- function(m) { ct <- coeftest(m, vcov. = vcovHAC(m)); setNames(ct[, 4], rownames(ct)) }
breaks <- function(m, P) { d <- attr(m, "dd"); sf <- suppressWarnings(sctest(Fstats(formula(m), data = d, from = 0.2))$p.value)
  d$post09 <- as.numeric(d$year >= 2009); d$post_x_proj <- d$post09 * d[[P]]; xs <- c(attr(terms(m), "term.labels"), "post09", "post_x_proj")
  mi <- lm(reformulate(xs, OUTC), data = d); ct <- coeftest(mi, vcov. = vcovHAC(mi)); c(supf = sf, ch = ct["post_x_proj", 1], chp = ct["post_x_proj", 4]) }
screen <- function(S, P, lv, SP) { M <- as.matrix(S[, c("year", OUTC, unique(c(P, CORE, SL_TAX, SL_FISC, SL_DEBT, SL_LAB, POOL)))]); fixed <- c(P, lv$fixed); cache <- list(); res <- list()
  for (sp in SP) { xs <- c(fixed, sp); free <- sp; q <- qols(M, xs); if (is.null(q)) next
    if (q$p[P] > 0.10 || any(q$p[free] > 0.20)) next                                   # lenient classical pre-filter
    key0 <- paste(q$yrs, collapse = ","); if (is.null(cache[[key0]])) cache[[key0]] <- qols(M[M[, "year"] %in% q$yrs, , drop = FALSE], c(P, CORE))$aic
    m <- fit(S[S$year %in% q$yrs, ], xs, OUTC); if (is.null(m)) next; hp <- hacp(m)
    if (hp[P] >= 0.05 || any(hp[free] >= 0.10)) next
    if (length(free) >= 2) { v <- vif(m); if (max(v[free]) >= 20) next }
    b <- breaks(m, P); res[[if (length(sp)) paste(sort(sp), collapse = " + ") else "(none)"]] <- c(dAIC = q$aic - cache[[key0]], proj = unname(coef(m)[P]), pp = unname(hp[P]), b, n = nobs(m),
      reset = resettest(m)$p.value, bg1 = bgtest(m, 1)$p.value, bg2 = bgtest(m, 2)$p.value, rain = raintest(m)$p.value) }
  res }
DATA <- list(); for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019)
  S$L_delta_oadr_10 <- lag1(S$delta_oadr_10); S$int_gdp_x <- splice(S$int_gdp, S$year, pusf$int_gdp_ons); S$L_int_gdp_x <- lag1(S$int_gdp_x)
  S$L_taxint_wap <- lag1(S$tax_workpop * (1 + S$int_gdp_x / S$tax_gdp)); S$L_d_psnd_gdp <- lag1(c(NA, diff(S$psnd_gdp)))
  DATA[[s]] <- subset(S, year >= Y0) }   # lags built on the year before; estimation years = the sample's own (Citizen 1965-, Origin 1977-)
line <- function(k, a, b) cat(sprintf("   %-44s Cit: proj %.4f (p %s) dAIC %6.2f supF %s post09 %+.4f (p %s) RESET %s BG1 %s BG2 %s rain %s N %d\n   %-44s Ori: proj %.4f (p %s) dAIC %6.2f supF %s post09 %+.4f (p %s) RESET %s BG1 %s BG2 %s rain %s N %d\n",
  if (k == "(none)") "(slots and pool empty)" else k, a["proj"], pf2(a["pp"]), a["dAIC"], pf2(a["supf"]), a["ch"], pf2(a["chp"]), pf2(a["reset"]), pf2(a["bg1"]), pf2(a["bg2"]), pf2(a["rain"]), a["n"],
  "", b["proj"], pf2(b["pp"]), b["dAIC"], pf2(b["supf"]), b["ch"], pf2(b["chp"]), pf2(b["reset"]), pf2(b["bg1"]), pf2(b["bg2"]), pf2(b["rain"]), b["n"]))
for (P in c("L_delta_oadr_10", "delta_oadr_10")) { cat(sprintf("\n\n==================== Timing %s ====================\n", if (P == "L_delta_oadr_10") "B (projection t-1)" else "A (projection t)"))
  for (ln in names(LEVELS)) { lv <- LEVELS[[ln]]; SP <- specs(lv); R <- lapply(DATA, function(S) screen(S, P, lv, SP)); k <- intersect(names(R[["1964"]]), names(R[["1977"]]))
    cat(sprintf("\n-- %s: fixed = lagged outcome, projection%s; %d specifications; pass on Citizen %d, Origin %d, both %d\n", ln, if (length(lv$fixed) > 1) paste0(", ", paste(sub("^L_", "", setdiff(lv$fixed, "L_mig_gross")), collapse = ", ")) else "",
      length(SP), length(R[["1964"]]), length(R[["1977"]]), length(k)))
    if (!length(k)) next
    sc <- vapply(k, function(x) R[["1964"]][[x]]["dAIC"] + R[["1977"]][[x]]["dAIC"], 0)
    brk <- k[vapply(k, function(x) all(c(R[["1964"]][[x]][c("supf", "chp")], R[["1977"]][[x]][c("supf", "chp")]) >= 0.10), logical(1))]
    cat(sprintf("   (i) pass AND remove the break on both samples: %d. Best by summed dAIC:\n", length(brk)))
    for (x in head(brk[order(sc[brk])], 3)) line(x, R[["1964"]][[x]], R[["1977"]][[x]])
    cat("   (ii) pass on both samples, break not required. Best by summed dAIC:\n")
    for (x in head(k[order(sc)], 3)) line(x, R[["1964"]][[x]], R[["1977"]][[x]]) } }
cat("\nDone.\n")
