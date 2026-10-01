# =============================================================================
# british_relinc.R -- relative income matched to where non-UK-born British arrivals come from (Sam, 1 Oct 2026)
# relinc_dev (determinants_check.R, 23 Sep) compares the UK with seven large non-EU origins of the MAIN (non-EU citizen) flow: India, Pakistan,
# Bangladesh, China, Nigeria, the Philippines and South Africa. It was not chosen for British arrivals. MPR's ln(Y_i) is region-specific, so
# here relative income is matched to the regions non-UK-born British citizens arrive from (IPS 3.01 British by region of last residence minus
# IPS 3.18 UK-born by region): Oceania -> Australia; North America -> United States; Asia -> India, Pakistan, Bangladesh, China, Philippines;
# Sub-Saharan Africa -> Nigeria, South Africa (Maddison 2023, the countries held). Other Europe and Rest of the World have no country in the
# data and are left out (their share is reported). relinc_w = sum over regions of FIXED weights (each region's mean share of non-UK-born British
# arrivals, 1977-2019, renormalised over the covered regions) x (log UK GDP per head - mean log GDP per head of the region's countries).
# Unemployment, MPR's way: UK ILO unemployment IN PLACE OF log V/U (in british_mpr.R it sat beside log V/U, which holds V/U fixed and is not MPR's U).
# Outcome: non-UK-born British from non-EU (log persons), 1977-2019; also British all origins (LTIM) and the main non-EU inflows for reference.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); A <- { SAMPLE <- "1964"; source("data_common.R"); df[df$year <= 2019, ] }; A <- A[order(A$year), ]
ips <- read.csv("determinants/ips_301_citizenship_by_residence_1975_2019.csv", check.names = FALSE)                        # IPS 3.01 (thousands)
b318 <- read.csv("determinants/ips_318_birth_by_residence_1975_2019.csv"); ukb <- subset(b318, direction == "Inflow" & birth == "UNITED KINGDOM")   # IPS 3.18
gb <- function(r) ukb$est_k[ukb$residence == r][match(A$year, ukb$year[ukb$residence == r])]
REG <- c(OCEANIA = "oce", "NORTH AMERICA" = "nor", ASIA = "asi", "SUB-SAHARAN AFRICA" = "sub", "OTHER EUROPE" = "oth", "REST OF THE WORLD" = "res")
nub <- sapply(names(REG), function(r) ips[[paste0("Inflow_", r, "_brit")]][match(A$year, ips$year)] - gb(r))   # non-UK-born British by region, thousands
k <- A$year >= 1977 & A$year <= 2019; sh <- colMeans(pmax(nub[k, ], 0), na.rm = TRUE); sh <- sh / sum(sh)
cat("Non-UK-born British arrivals by region of last residence, mean share 1977-2019 (IPS 3.01 minus 3.18):\n"); print(round(100 * sh, 1))
g <- read.csv("determinants/maddison_gdppc.csv"); lg <- function(cc) rowMeans(log(g[match(A$year, g$year), cc, drop = FALSE]))
RI <- cbind(OCEANIA = lg("GBR") - lg("AUS"), "NORTH AMERICA" = lg("GBR") - lg("USA"), ASIA = lg("GBR") - lg(c("IND", "PAK", "BGD", "CHN", "PHL")),
            "SUB-SAHARAN AFRICA" = lg("GBR") - lg(c("NGA", "ZAF")))
w <- sh[colnames(RI)] / sum(sh[colnames(RI)]); cat(sprintf("Covered regions: %.0f%% of non-UK-born British arrivals; weights: %s\n", 100 * sum(sh[colnames(RI)]),
  paste(sprintf("%s %.2f", colnames(RI), w), collapse = ", ")))
A$relinc_w <- drop(RI %*% w); A$L_relinc_w <- lag1(A$relinc_w)
A$brit_nf_gross <- log(1000 * (A$brit_desc_noneu_k + A$ukborn_noneu_k)); A$L_brit_nf_gross <- lag1(A$brit_nf_gross)
cat(sprintf("Correlation of the weighted measure with relinc_dev: %.2f; with the projection (1977-2019): %.2f\n", cor(A$relinc_w, A$relinc_dev, use = "pair"),
            cor(A$delta_oadr_10[k], A$L_relinc_w[k], use = "pair")))
S <- subset(A, year >= 1977)
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
LABS <- list(vu = c("L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem"), un = c("L_tax_workpop", "L_ln_debt_gdp", "L_unem"))
for (o in list(c("brit_desc_gross", "non-UK-born British from non-EU"), c("brit_gross", "British, all origins (LTIM)"), c("mig_gross", "MAIN: non-EU inflows (controlled)")))
  for (lb in names(LABS)) { xs <- c(paste0("L_", o[1]), "delta_oadr_10", LABS[[lb]])
    cat(sprintf("\n-- %s, 1977-2019, labour term: %s\n", o[2], if (lb == "vu") "log V/U (core)" else "UK unemployment in place of log V/U (MPR's U)"))
    cat(sprintf("   %-30s %3s %18s %18s %18s %7s\n", "added (t-1)", "N", "relative income (p)", "labour term (p)", "projection (p)", "dAIC"))
    for (add in c("(none)", "L_relinc_dev", "L_relinc_w")) { a <- if (add == "(none)") character(0) else add; d <- S[complete.cases(S[, c(o[1], xs, "L_relinc_dev", "L_relinc_w")]), ]
      m0 <- fit(d, xs, o[1]); m <- fit(d, c(xs, a), o[1]); lt <- tail(LABS[[lb]], 1)
      cat(sprintf("   %-30s %3d %18s %9.4f (%5s) %9.4f (%5s) %7.2f\n", c("(none)" = "(none)", L_relinc_dev = "relinc_dev (7 main-flow origins)", L_relinc_w = "relinc_w (British regions)")[[add]],
        nobs(m), if (length(a)) sprintf("%.4f (%s)", hc(m, a)[1], pf2(hc(m, a)[2])) else "--", hc(m, lt)[1], pf2(hc(m, lt)[2]), hc(m, "delta_oadr_10")[1], pf2(hc(m, "delta_oadr_10")[2]), AIC(m) - AIC(m0))) } }
cat("\nDone.\n")
