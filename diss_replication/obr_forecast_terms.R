# =============================================================================
# obr_forecast_terms.R -- Budget-time spending, borrowing and growth forecasts from the OBR Historical Official Forecasts Database
# (Spring 2026; Sam's upload, determinants/obr_hofd_spring2026.xlsx) as replacements for int_tme, and growth forecasts on their own (Sam, 1 Oct 2026).
# Coverage: TME and receipts (PSCR) forecasts from March 1990 (general government cash spending/receipts before November 1994); GBP PSNB from
# April 1970 (the PSBR before November 1994); PSNB/GDP from March 1977; real GDP growth from November 1983. Interest, welfare, pensions,
# output gap and gilt-rate forecasts start only in June 2010 and are not used.
# Vintage for year t: the latest forecast published January-July of t (the spring Budget, or the summer Budget in 1994-98). Forecast columns
# are fiscal years ("1990-91" = FY starting April 1990 = year t). The second "July 1996" label in the TME/PSCR sheets sits between November
# 1996 and November 1997 and is read as July 1997 (vintage dates are forced to increase).
# All candidates are information available at the start of year t and are tried at t (start-of-year information) and at t-1 (int_tme's
# timing). Each is defined so that a POSITIVE sign means more fiscal pressure, as with int_tme (a larger pre-empted share of spending):
#   TME (from 1990; estimation 1991-2019 at t-1, 1990-2019 at t):
#     fc_tme_gdp     planned TME, % of GDP, FY t
#     fc_tme_chg     planned change in TME/GDP: FY t forecast minus the same vintage's estimate for FY t-1 (pp)
#     fc_tme_consol  planned medium-term squeeze: TME/GDP in FY t minus the furthest year of the same forecast (pp; positive = cuts planned)
#     fc_gap_tme     planned spending not covered by receipts: (TME - PSCR) / TME, %, FY t (the int_tme concept: share of the envelope
#                    that is not tax-financed)
#     fc_tme_over    spending overrun: the real-time estimate of FY t-1 TME/GDP (spring t vintage) minus its spring t-1 forecast (pp)
#   Borrowing (GBP from 1970, % of GDP from 1977):
#     fc_psnb_tme    planned borrowing as a share of spending: GBP PSNB forecast for FY t over TME outturn in FY t-1 (OBR PSF databank), %
#     fc_psnb_chg    planned change in borrowing: PSNB/GDP forecast for FY t minus the same vintage's estimate for FY t-1 (pp; from 1977)
#     fc_psnb_over   borrowing overrun: real-time estimate of FY t-1 PSNB/GDP minus its spring t-1 forecast (pp; from 1978)
#   Growth (calendar years, from the November 1983 vintage; Sam: worth trying on their own, given the literature):
#     fc_g0          real GDP growth forecast for year t
#     fc_g_med       mean growth forecast for t+1..t+3 (the Treasury's view of medium-term growth)
#     fc_g_over      growth surprise: real-time estimate of growth in t-1 minus its forecast made in spring t-1
# A. The 7-parameter version (lagged outcome, projection, tax per WAP, log change in debt/GDP, log V/U + the fiscal term) with each
#    candidate in place of int_tme, on the years the candidate exists, beside int_tme on the same years. Sam's criterion: own HAC p < .10
#    and a lower AIC than the 6-parameter core (i) beside the projection and (ii) without it. 8 spending/borrowing candidates x 2 timings.
# B. Growth forecasts added to the 6-parameter core (and the existing HMT start-of-year forecast growth_fc for comparison).
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("screen_engine.R"); S <- build_data()[["1964"]]$S
f <- "determinants/obr_hofd_spring2026.xlsx"; SH <- readxl::excel_sheets(f)
hofd <- function(sh) { a <- suppressMessages(as.data.frame(readxl::read_excel(f, sh, col_names = FALSE, .name_repair = "minimal", col_types = "text")))
  hr <- which(a[[1]] == "Back to contents")[1]; cols <- unlist(a[hr, -1]); yr <- as.integer(substr(cols, 1, 4)); out <- NULL; last <- as.Date("1900-01-01")
  for (r in (hr + 1):nrow(a)) { lab <- a[[1]][r]; if (is.na(lab) || !grepl("^[A-Z][a-z]+ [0-9]{4}$", lab)) next
    d <- as.Date(paste("1", lab), "%d %B %Y"); if (is.na(d)) next; while (d <= last) d <- seq(d, by = "1 year", length.out = 2)[2]; last <- d
    v <- suppressWarnings(as.numeric(unlist(a[r, -1]))); k <- !is.na(v) & !is.na(yr)
    if (any(k)) out <- rbind(out, data.frame(vint = d, vyear = as.integer(format(d, "%Y")), vmonth = as.integer(format(d, "%m")), target = yr[k], value = v[k])) }
  out }
spring <- function(D, t) { d <- D[D$vyear == t & D$vmonth <= 7, ]; if (!nrow(d)) return(NULL); d[d$vint == max(d$vint), ] }
pick <- function(D, t, tg) { d <- spring(D, t); if (is.null(d)) return(NA_real_); x <- d$value[d$target == tg]; if (length(x)) x[1] else NA_real_ }
furthest <- function(D, t) { d <- spring(D, t); if (is.null(d)) return(NA_real_); d <- d[d$target >= t, ]; if (!nrow(d)) NA_real_ else d$value[which.max(d$target)] }
med <- function(D, t) { d <- spring(D, t); if (is.null(d)) return(NA_real_); x <- d$value[d$target %in% (t + 1):(t + 3)]; if (length(x)) mean(x) else NA_real_ }
TMEg <- hofd("TME"); TMEb <- hofd(grep("TME$", SH, value = TRUE)[grepl("^.TME$", grep("TME$", SH, value = TRUE))][1])
PSCRg <- hofd("PSCR"); PSNBb <- hofd(grep("^.PSNB$", SH, value = TRUE)[1]); PSNBg <- hofd("PSNB"); GDP <- hofd("UKGDP")
ot <- read.csv("determinants/obr_psf_tme_fy_1946_2030.csv")
yrs <- 1966:2019; Fc <- data.frame(year = yrs)
Fc$fc_tme_gdp    <- sapply(yrs, function(t) pick(TMEg, t, t))
Fc$fc_tme_chg    <- Fc$fc_tme_gdp - sapply(yrs, function(t) pick(TMEg, t, t - 1))
Fc$fc_tme_consol <- Fc$fc_tme_gdp - sapply(yrs, function(t) furthest(TMEg, t))
Fc$fc_gap_tme    <- 100 * (Fc$fc_tme_gdp - sapply(yrs, function(t) pick(PSCRg, t, t))) / Fc$fc_tme_gdp
Fc$fc_tme_over   <- sapply(yrs, function(t) pick(TMEg, t, t - 1) - pick(TMEg, t - 1, t - 1))
Fc$fc_psnb_tme   <- 100 * sapply(yrs, function(t) pick(PSNBb, t, t)) / ot$tme_bn[match(yrs - 1, ot$year)]
Fc$fc_psnb_chg   <- sapply(yrs, function(t) pick(PSNBg, t, t) - pick(PSNBg, t, t - 1))
Fc$fc_psnb_over  <- sapply(yrs, function(t) pick(PSNBg, t, t - 1) - pick(PSNBg, t - 1, t - 1))
Fc$fc_g0         <- sapply(yrs, function(t) pick(GDP, t, t))
Fc$fc_g_med      <- sapply(yrs, med, D = GDP)
Fc$fc_g_over     <- sapply(yrs, function(t) pick(GDP, t, t - 1) - pick(GDP, t - 1, t - 1))
SP <- c("fc_tme_gdp", "fc_tme_chg", "fc_tme_consol", "fc_gap_tme", "fc_tme_over", "fc_psnb_tme", "fc_psnb_chg", "fc_psnb_over"); GR <- c("fc_g0", "fc_g_med", "fc_g_over")
for (v in c(SP, GR)) { S[[v]] <- Fc[[v]][match(S$year, Fc$year)]; S[[paste0("L_", v)]] <- Fc[[v]][match(S$year - 1, Fc$year)] }
write.csv(Fc[Fc$year >= 1970, ], "determinants/obr_hofd_terms_1970_2019.csv", row.names = FALSE)
cat("Budget-time forecast terms (spring vintage of each year), selected years\n")
print(round(Fc[Fc$year %in% c(1971, 1977, 1985, 1990, 1995, 2000, 2005, 2008, 2009, 2010, 2015, 2019), ], 2), row.names = FALSE)
cat(sprintf("\nFirst year available: %s\n", paste(sprintf("%s %d", c(SP, GR), sapply(c(SP, GR), function(v) min(Fc$year[!is.na(Fc[[v]])]))), collapse = "; ")))

CORE6 <- setdiff(CORE_A, "L_int_tme"); NP <- setdiff(CORE6, "delta_oadr_10")
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 4)] else c(NA, NA) }
cat("\n==================== A. In place of int_tme (7-parameter version), on the years each candidate exists ====================\n")
cat("   pass = own HAC p < .10 with a positive sign and lower AIC than the 6-parameter core, both with and without the projection\n")
cat(sprintf("   %-15s %-6s %-10s %3s %16s %16s %8s %8s %16s %8s  %s\n", "term", "timing", "window", "N", "term (p)", "projection (p)", "dAIC", "vs int", "no proj: term", "dAIC", "pass"))
npass <- 0; ntry <- 0
for (v in SP) for (tim in c("t", "t-1")) { x <- if (tim == "t") v else paste0("L_", v); seen <- integer(0)
  for (y0 in c(1965, 1977)) { d <- subset(S, year >= y0); d <- d[complete.cases(d[, c("mig_gross", CORE6, x, "L_int_tme")]), ]; if (nrow(d) < 20) next
    if (min(d$year) %in% seen) next; seen <- c(seen, min(d$year))
    m0 <- fit(d, CORE6, "mig_gross"); m <- fit(d, c(CORE6, x), "mig_gross"); mi <- fit(d, c(CORE6, "L_int_tme"), "mig_gross")
    n0 <- fit(d, NP, "mig_gross"); n1 <- fit(d, c(NP, x), "mig_gross"); a <- hc(m, x); b <- hc(m, "delta_oadr_10"); c1 <- hc(n1, x)
    ps <- !is.na(a[1]) && a[1] > 0 && a[2] < .10 && AIC(m) < AIC(m0) && c1[1] > 0 && c1[2] < .10 && AIC(n1) < AIC(n0); ntry <- ntry + 1; npass <- npass + ps
    cat(sprintf("   %-15s %-6s %d-2019 %3d %8.4f (%5s) %8.4f (%5s) %8.2f %8.2f %8.4f (%5s) %8.2f  %s\n", v, tim, min(d$year), nobs(m), a[1], pf2(a[2]), b[1], pf2(b[2]), AIC(m) - AIC(m0),
                AIC(m) - AIC(mi), c1[1], pf2(c1[2]), AIC(n1) - AIC(n0), if (ps) "PASS" else "")) } }
cat(sprintf("   %d of %d candidate-timing-window rows pass. dAIC = against the 6-parameter core on the same years; 'vs int' = against int_tme on the same years.\n", npass, ntry))
cat("   For reference, int_tme itself on each window:\n")
for (w in list(c(1965, "full"), c(1977, ""), c(1991, ""))) { d <- subset(S, year >= as.numeric(w[1])); d <- d[complete.cases(d[, c("mig_gross", CORE6, "L_int_tme")]), ]
  m0 <- fit(d, CORE6, "mig_gross"); m <- fit(d, c(CORE6, "L_int_tme"), "mig_gross"); a <- hc(m, "L_int_tme")
  cat(sprintf("   int_tme %d-2019 N %d: %.4f (%s), dAIC %.2f\n", min(d$year), nobs(m), a[1], pf2(a[2]), AIC(m) - AIC(m0))) }

cat("\n==================== B. Growth forecasts added to the 6-parameter core ====================\n")
S$growth_fc_t <- S$growth_fc; S$L_growth_fc_t <- c(NA, head(S$growth_fc, -1))[match(S$year, S$year)]
cat(sprintf("   %-15s %-6s %-10s %3s %16s %16s %8s %16s %8s\n", "term", "timing", "window", "N", "term (p)", "projection (p)", "dAIC", "no proj: term", "dAIC"))
for (v in c(GR, "growth_fc_t")) for (tim in c("t", "t-1")) { x <- if (tim == "t") v else paste0("L_", v); seen <- integer(0)
  for (y0 in c(1965, 1977, 1985)) { d <- subset(S, year >= y0); d <- d[complete.cases(d[, c("mig_gross", CORE6, x)]), ]; if (nrow(d) < 20 || min(d$year) %in% seen) next; seen <- c(seen, min(d$year))
    m0 <- fit(d, CORE6, "mig_gross"); m <- fit(d, c(CORE6, x), "mig_gross"); n0 <- fit(d, NP, "mig_gross"); n1 <- fit(d, c(NP, x), "mig_gross"); a <- hc(m, x); b <- hc(m, "delta_oadr_10"); c1 <- hc(n1, x)
    cat(sprintf("   %-15s %-6s %d-2019 %3d %8.4f (%5s) %8.4f (%5s) %8.2f %8.4f (%5s) %8.2f\n", v, tim, min(d$year), nobs(m), a[1], pf2(a[2]), b[1], pf2(b[2]), AIC(m) - AIC(m0), c1[1], pf2(c1[2]), AIC(n1) - AIC(n0))) } }
cat("   growth_fc_t = the HMT start-of-year growth forecast already in the dataset (from 1976), for comparison.\nDone.\n")
