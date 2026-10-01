# =============================================================================
# policy_index.R -- measured UK immigration policy as the mechanism (Sam, 1 Oct 2026)
# Source: Demscore v7 Thematic Dataset on Migration, Country-Year version (Sam's upload; determinants/demscore/migration_dataset.rds).
# DEMIG POLICY is NOT in this version (only in the Country-Year-Change download). Two policy series for the UK are:
#   HIP  Historical Immigration Policies (Peters, Borang, Kalm, Lindvall and Shin, 2024; Harvard Dataverse doi:10.7910/DVN/F7V8YL): annual
#        LEVEL codes 1 (closed) - 5 (open), UK to 2010. Labour-entry codes that move over 1965-2010: skillcode (skill/income restrictions)
#        and reccode (recruitment procedures); labcode (occupational limits) and qcode (quota) are 5 throughout. Others: natcode
#        (nationality exclusions), citcode, rightscode, famcode, encode (enforcement), decode (deportation), refcode, asylcode.
#   IMPIC Immigration Policies in Comparison 2024 (Helbling, Bjerre, Romer and Zobel, 2017): annual indices 0 (open) - 1 (restrictive),
#        UK 1980-2018; avgs_reg_b = regulations, labour migration; avgs_immpol = overall immigration policy (fields A-D).
#        Reversed here (1 - index) so that higher = more open in both sources.
#   IMPIC's 'use of administrative guidelines' series is NOT coded for the UK (the one direct measure of a quiet lever is unavailable).
# Both are 'policies on paper' (laws and regulations, not their administration); HIP and IMPIC code levels, not individual measures.
# Constructed (higher = more open):
#   hip_lab    mean of skillcode and reccode (labour entry)        hip_entry  mean of natcode, skillcode, reccode, labcode, famcode, qcode
#   impic_lab  1 - avgs_reg_b (labour regulations)                 impic_all  1 - avgs_immpol (all four fields)
# A. Policy as the outcome: does the projection predict liberalisation of measured policy? d(policy)_t on the projection (at t and at t-1)
#    and the core fiscal/labour terms; and the level on its own lag (partial adjustment). Plus the list of years in which hip_lab changed.
# B. Mediation: the core inflow equation (gross inflows; tax per WAP, log change in debt/GDP, log V/U) on each source's window, with
#    and without the policy level at t-1 (and at t); then the policy level in place of the projection.
# Reading (de Haas, Natter and Vezzoli, 2015): codes record legal changes and lean to salient ones, so a projection that does NOT move the
# codes is what a low-salience administrative lever predicts; a projection that does move them is corroboration. Nothing in any manuscript.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); S <- build_data()[["1964"]]$S
f_csv <- "determinants/policy_uk_hip_impic.csv"; f_rds <- "determinants/demscore/migration_dataset.rds"
HIPV <- c("natcode", "skillcode", "citcode", "rightscode", "refcode", "asylcode", "reccode", "labcode", "decode", "encode", "famcode", "qcode")
IMPV <- c("avgs_immpol", "avgs_reg_a", "avgs_reg_b", "avgs_reg_c", "avgs_reg_d", "avgs_extreg_b", "avgs_intreg_b", "avgs_elig_b", "avgs_cond_b",
          "avgs_secu_b", "avgs_righ_b", "avgs_extcont", "avgs_intcont")
if (file.exists(f_rds)) { x <- readRDS(f_rds); uk <- x[x$u_demscore_country_year_country == "United Kingdom" & x$u_demscore_country_year_year %in% 1945:2023, ]
  P <- data.frame(year = uk$u_demscore_country_year_year, uk[, c(HIPV, IMPV)]); P[, -1][P[, -1] < -100] <- NA; P <- P[order(P$year), ]
  names(P)[match(HIPV, names(P))] <- paste0("hip_", HIPV); write.csv(P, f_csv, row.names = FALSE); rm(x, uk) } else P <- read.csv(f_csv)
P$hip_lab <- (P$hip_skillcode + P$hip_reccode) / 2
P$hip_entry <- rowMeans(P[, paste0("hip_", c("natcode", "skillcode", "reccode", "labcode", "famcode", "qcode"))])
P$impic_lab <- 1 - P$avgs_reg_b; P$impic_all <- 1 - P$avgs_immpol
POL <- c("hip_lab", "hip_entry", "impic_lab", "impic_all")
P <- P[order(P$year), ]; for (v in POL) { P[[paste0("d_", v)]] <- c(NA, diff(P[[v]])); P[[paste0("L_", v)]] <- c(NA, head(P[[v]], -1)) }
for (v in c(POL, paste0("d_", POL), paste0("L_", POL), "hip_skillcode", "hip_reccode")) S[[v]] <- P[[v]][match(S$year, P$year)]
WIN <- list(hip_lab = 2010, hip_entry = 2010, impic_lab = 2018, impic_all = 2018)
CORE6 <- setdiff(CORE_A, "L_int_tme"); X <- setdiff(CORE6, "L_mig_gross")
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }

cat("UK policy series (higher = more open), selected years\n")
print(round(S[S$year %in% c(1965, 1968, 1971, 1980, 1990, 1998, 2002, 2003, 2005, 2008, 2010, 2012, 2018), c("year", POL)], 3), row.names = FALSE)
k <- which(!is.na(S$d_hip_lab) & S$d_hip_lab != 0)
cat(sprintf("\nYears in which HIP labour-entry openness changed (1965-2010): %d of %d\n", length(k), sum(!is.na(S$d_hip_lab))))
print(data.frame(year = S$year[k], d_hip_lab = S$d_hip_lab[k], skill = S$hip_skillcode[k], recruit = S$hip_reccode[k], projection_t = round(S$delta_oadr_10[k], 2),
                 projection_t1 = round(S$L_delta_oadr_10[k], 2)), row.names = FALSE)
cat(sprintf("Mean projection (t): years liberalising %.2f, tightening %.2f, unchanged %.2f\n", mean(S$delta_oadr_10[k][S$d_hip_lab[k] > 0]),
            mean(S$delta_oadr_10[k][S$d_hip_lab[k] < 0]), mean(S$delta_oadr_10[S$year <= 2010 & S$d_hip_lab == 0], na.rm = TRUE)))
cat(sprintf("Correlation with the projection (levels, own window): %s\n", paste(vapply(POL, function(v) { d <- subset(S, year <= WIN[[v]])
  sprintf("%s %.2f", v, cor(d[[v]], d$delta_oadr_10, use = "pair")) }, ""), collapse = "; ")))

cat("\n==================== A. Policy as the outcome ====================\n")
cat(sprintf("   %-10s %-10s %-24s %3s %18s %18s   %s\n", "policy", "window", "model", "N", "projection t (p)", "projection t-1 (p)", "other terms"))
for (v in POL) for (y0 in c(1965, 1977)) { d <- subset(S, year >= max(y0, if (grepl("impic", v)) 1981 else 1965) & year <= WIN[[v]])
  if (grepl("impic", v) && y0 == 1977) next
  for (mod in list(list("change, core terms", paste0("d_", v), c("delta_oadr_10", setdiff(X, "delta_oadr_10"))),
                   list("change, proj t-1", paste0("d_", v), c("L_delta_oadr_10", setdiff(X, "delta_oadr_10"))),
                   list("level on own lag", v, c(paste0("L_", v), X)))) {
    m <- fit(d, mod[[3]], mod[[2]]); if (is.null(m)) next; a <- hc(m, "delta_oadr_10"); b <- hc(m, "L_delta_oadr_10")
    oth <- setdiff(names(coef(m))[-1], c("delta_oadr_10", "L_delta_oadr_10"))
    cat(sprintf("   %-10s %d-%d %-24s %3d %18s %18s   %s\n", v, min(d$year), WIN[[v]], mod[[1]], nobs(m), if (is.na(a[1])) "--" else sprintf("%.4f (%s)", a[1], pf2(a[2])),
                if (is.na(b[1])) "--" else sprintf("%.4f (%s)", b[1], pf2(b[2])), paste(vapply(oth, function(o) { h <- hc(m, o); sprintf("%s %.3f (%s)", sub("^L_", "", o), h[1], pf2(h[2])) }, ""), collapse = "; "))) } }

cat("\n==================== B. Mediation: the inflow equation with measured policy ====================\n")
cat(sprintf("   %-10s %-10s %-26s %3s %18s %18s %8s\n", "policy", "window", "model", "N", "projection (p)", "policy (p)", "dAIC"))
for (v in POL) for (y0 in c(1965, 1977)) { d <- subset(S, year >= max(y0, if (grepl("impic", v)) 1981 else 1965) & year <= WIN[[v]])
  if (grepl("impic", v) && y0 == 1977) next; d <- d[complete.cases(d[, c("mig_gross", CORE6, v, paste0("L_", v))]), ]
  m0 <- fit(d, CORE6, "mig_gross")
  for (mod in list(list("core (same years)", CORE6, NA), list("+ policy t-1", c(CORE6, paste0("L_", v)), paste0("L_", v)), list("+ policy t", c(CORE6, v), v),
                   list("policy t-1, no projection", c(setdiff(CORE6, "delta_oadr_10"), paste0("L_", v)), paste0("L_", v)),
                   list("no projection, no policy", setdiff(CORE6, "delta_oadr_10"), NA))) {
    m <- fit(d, mod[[2]], "mig_gross"); if (is.null(m)) next; a <- hc(m, "delta_oadr_10"); b <- if (is.na(mod[[3]])) c(NA, NA) else hc(m, mod[[3]])
    cat(sprintf("   %-10s %d-%d %-26s %3d %18s %18s %8.2f\n", v, min(d$year), WIN[[v]], mod[[1]], nobs(m), if (is.na(a[1])) "--" else sprintf("%.4f (%s)", a[1], pf2(a[2])),
                if (is.na(b[1])) "--" else sprintf("%.4f (%s)", b[1], pf2(b[2])), AIC(m) - AIC(m0))) } }
cat("\ndAIC relative to the core on the same years. Policy coefficients: change in log inflows per unit of the index (HIP 1-5 scale; IMPIC 0-1).\n")
cat("\n==================== C. Episodes: HIP labour-entry openness on the projection, leaving out each episode ====================\n")
cat("   (HIP UK labour codes are flat 1972-97: the 1977-2010 variation is the 1998-2003 liberalisation and the 2005-10 tightening)\n")
for (drop in list(integer(0), 2002:2003, 1998:2003, 2005:2010, 2008:2010)) for (y0 in c(1965, 1977)) { d <- subset(S, year >= y0 & year <= 2010 & !year %in% drop)
  m1 <- fit(d, c("delta_oadr_10", setdiff(X, "delta_oadr_10")), "d_hip_lab"); m2 <- fit(d, c("L_hip_lab", X), "hip_lab")
  cat(sprintf("   %d-2010 without %-10s N %d   change: %.4f (%s)   level on own lag: %.4f (%s)\n", y0, if (length(drop)) paste(range(drop), collapse = "-") else "(none)", nobs(m1),
              hc(m1, "delta_oadr_10")[1], pf2(hc(m1, "delta_oadr_10")[2]), hc(m2, "delta_oadr_10")[1], pf2(hc(m2, "delta_oadr_10")[2]))) }
cat("Done.\n")
