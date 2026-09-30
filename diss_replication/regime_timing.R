# =============================================================================
# regime_timing.R -- does the timing of the projection effect change with the institutions that ran economic migration?
# Sam, 30 Sep 2026. diag_shift.R found that the shifted timing (projection at t-1) fits best but breaks after 2008 (sup-F),
# while the current timing (projection at t) does not. Sam's reading: the shift may be the finding for most of the period,
# and the break lines up with the end of the work permit system. The candidate dates below come from the documentary record,
# not from the data (though 2008 was also where sup-F put the break, so 2008 is not an independent test):
#   2001  Work Permits (UK) moves from DfEE (Overseas Labour Service) to the Home Office (IND) -- "now part of the Immigration
#         and Nationality Directorate" (Home Office, 2002, Cm 5387, para 3.10); first full year under the Home Office = 2002
#   2002  Highly Skilled Migrant Programme (points-based; Cm 5387, paras 3.14-3.20)
#   2004  EU enlargement (A8)
#   2008  Points-Based System: Tier 1 replaces HSMP (2008), Tier 2 replaces work permits (November 2008); last work-permit year
#   2010  Coalition net-migration target and interim limit; annual Tier 2 limit from April 2011
# Core specification: mig_gross ~ L_mig_gross + projection + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_int_tme (level).
# Sections: (1) break at each documented date under timing A (projection t) and B (projection t-1): projection before, change
# after (post x projection, with a post intercept), HAC p; (2) the pre-PBS period alone (to 2008) and to 2001, A vs B;
# (3) regime-specific timing: projection t-1 up to the break and t after it; (4) work permits (1965-2008) under A and B -- if the
# one-year lag is the permit pipeline, permits themselves should prefer the lagged projection.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.rebuild = FALSE, restud.secondary = FALSE); source("tables_common.R")
LABS <- c("1964" = "Citizen sample (1964-2019)", "1977" = "Origin sample (1977-2019)"); OUTC <- "mig_gross"
pf2 <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
X <- c("L_mig_gross", "L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_int_tme")
DATES <- c(2001, 2002, 2004, 2008, 2009, 2010, 2011)
hac <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, ] else rep(NA, 4) }
lr <- function(m, v) coef(m)[[v]] / (1 - coef(m)[["L_mig_gross"]])
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R"); S <- subset(df, year >= Y0 - 1 & year <= 2019); L <- LABS[[s]]
  S$L_delta_oadr_10 <- lag1(S$delta_oadr_10); S$L_permits <- lag1(S$permits)
  S <- S[complete.cases(S[, c(OUTC, X, "delta_oadr_10", "L_delta_oadr_10")]), ]
  cat("\n\n==================== ", L, sprintf(" (%d-%d, N %d) ====================\n", min(S$year), max(S$year), nrow(S)), sep = "")
  P <- c(A = "delta_oadr_10", B = "L_delta_oadr_10")

  cat("\n1. Break at each documented date: projection coefficient before, change from the date (post x projection), HAC p, net after; AIC\n")
  cat(sprintf("   %-6s %-5s %10s %10s %8s %9s %8s %4s\n", "from", "timing", "before", "change", "p", "after", "AIC", "Npost"))
  for (a in names(P)) { m0 <- fit(S, c(P[[a]], X), OUTC); cat(sprintf("   %-6s %-5s %10.4f %10s %8s %9s %8.2f\n", "none", a, coef(m0)[[P[[a]]]], "", "", "", AIC(m0))) }
  for (D in DATES) for (a in names(P)) { S$post <- as.numeric(S$year >= D); S$post_x_proj <- S$post * S[[P[[a]]]]
    m <- fit(S, c(P[[a]], X, "post", "post_x_proj"), OUTC); b <- hac(m, P[[a]]); ch <- hac(m, "post_x_proj")
    cat(sprintf("   %-6d %-5s %10.4f %10.4f %8s %9.4f %8.2f %4d\n", D, a, b[1], ch[1], pf2(ch[4]), b[1] + ch[1], AIC(m), sum(S$post))) }

  cat("\n2. Sub-periods alone: A vs B (same years). Projection (HAC p), long-run effect, AIC, BIC\n")
  for (end in c(2000, 2008)) { SS <- subset(S, year <= end)
    for (a in names(P)) { m <- fit(SS, c(P[[a]], X), OUTC); b <- hac(m, P[[a]])
      cat(sprintf("   to %d  %-2s  proj %.4f (p %s)  long run %.4f  AIC %7.2f  BIC %7.2f  N %d\n", end, a, b[1], pf2(b[4]), lr(m, P[[a]]), AIC(m), BIC(m), nobs(m))) } }
  SS <- subset(S, year >= 2009); cat(sprintf("   2009-2019 alone (N %d): too few observations for the seven-parameter model; see section 3.\n", nrow(SS)))

  cat("\n3. Regime-specific timing (post intercept in every model, same observations): A = t throughout; B = t-1 throughout;\n")
  cat("   S = t-1 before the date and t from it; S' = t before and t-1 after (the reverse, as a check)\n")
  for (D in c(2002, 2008, 2009, 2010)) { S$post <- as.numeric(S$year >= D)
    S$proj_pre_lag <- (1 - S$post) * S$L_delta_oadr_10; S$proj_post_cur <- S$post * S$delta_oadr_10
    S$proj_pre_cur <- (1 - S$post) * S$delta_oadr_10; S$proj_post_lag <- S$post * S$L_delta_oadr_10
    mA <- fit(S, c("delta_oadr_10", X, "post"), OUTC); mB <- fit(S, c("L_delta_oadr_10", X, "post"), OUTC)
    mS <- fit(S, c("proj_pre_lag", "proj_post_cur", X, "post"), OUTC); mR <- fit(S, c("proj_pre_cur", "proj_post_lag", X, "post"), OUTC)
    a1 <- hac(mS, "proj_pre_lag"); a2 <- hac(mS, "proj_post_cur"); r1 <- hac(mR, "proj_pre_cur"); r2 <- hac(mR, "proj_post_lag")
    cat(sprintf("   from %d: AIC A %.2f | B %.2f | S %.2f (t-1 before %.4f p %s; t after %.4f p %s) | S' %.2f (t before %.4f p %s; t-1 after %.4f p %s)\n", D, AIC(mA), AIC(mB), AIC(mS),
                a1[1], pf2(a1[4]), a2[1], pf2(a2[4]), AIC(mR), r1[1], pf2(r1[4]), r2[1], pf2(r2[4])))
    if (D == 2008) cat(sprintf("            S diagnostics: RESET %s  BG(1) %s  BG(2) %s  rainbow %s  sup-F %s\n", pf2(resettest(mS)$p.value), pf2(bgtest(mS, 1)$p.value), pf2(bgtest(mS, 2)$p.value),
                               pf2(raintest(mS)$p.value), pf2(suppressWarnings(strucchange::sctest(strucchange::Fstats(formula(mS), data = attr(mS, "dd"), from = 0.2))$p.value)))) }

  cat("\n4. Work permits and first permissions (thousands, levels; 1946-2008 series) on the same right-hand side, A vs B\n")
  SP <- subset(S, year <= 2008 & !is.na(permits) & !is.na(L_permits))
  for (a in names(P)) { m <- fit(SP, c("L_permits", P[[a]], setdiff(X, "L_mig_gross")), "permits"); b <- hac(m, P[[a]])
    cat(sprintf("   %-2s permits: proj %.4f (p %s)  AIC %7.2f  N %d (%d-%d)\n", a, b[1], pf2(b[4]), AIC(m), nobs(m), min(attr(m, "dd")$year), max(attr(m, "dd")$year))) }
  m2 <- fit(SP, c("L_permits", "delta_oadr_10", "L_delta_oadr_10", setdiff(X, "L_mig_gross")), "permits"); b1 <- hac(m2, "delta_oadr_10"); b2 <- hac(m2, "L_delta_oadr_10")
  cat(sprintf("   both  permits: t %.4f (p %s), t-1 %.4f (p %s)  AIC %7.2f\n", b1[1], pf2(b1[4]), b2[1], pf2(b2[4]), AIC(m2)))
  for (a in names(P)) { m <- fit(SP, c(P[[a]], X), OUTC); b <- hac(m, P[[a]])
    cat(sprintf("   %-2s inflows, same years: proj %.4f (p %s)  AIC %7.2f  N %d\n", a, b[1], pf2(b[4]), AIC(m), nobs(m))) } }
cat("\nDone.\n")
