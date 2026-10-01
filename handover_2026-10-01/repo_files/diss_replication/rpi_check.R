# =============================================================================
# rpi_check.R -- does the inflation measure in the 'real' debt-interest terms matter? (Sam, 1 Oct 2026: index-linked gilts are uplifted by RPI)
# What the data already do:
#   * Debt interest (ONS PUSF JW2P, public sector excl. banks; NMFX + MU74 for central government net of the APF) is on an ACCRUALS basis and
#     already INCLUDES the RPI uplift on index-linked gilts (the capital uplift is recorded as interest as it accrues), so the RPI link is
#     in the nominal series.
#   * fiscal_ratio.R turns nominal interest into a 'real' cost by subtracting inflation x net debt at the start of the year (the erosion of the
#     nominal stock), with GDP-deflator inflation -- the deflator already used for the taxes (tax_workpop = national accounts taxes, OBR HPFD,
#     over the HMT GDP deflator, 2024-25 = 100, per working-age person), so the one-term burden is in one price basis.
# Here the erosion term uses RPI inflation instead (Millennium A47 RPI-based index to 2016; ONS RPI annual inflation 2017-19: 3.6, 3.3, 2.6;
# calendar years converted to the fiscal year ending in March of t with weights 0.75 / 0.25, as in fiscal_ratio.R). With RPI the whole stock
# is treated as RPI-protected; with the deflator, the RPI-deflator wedge on index-linked debt counts as a real cost. The exact split needs
# the index-linked share of the gilt stock (DMO), which is not in the data; the two versions bracket it.
# Compared: the ratio rint_xb_tme (real interest / TME, in place of int_tme beside the 6-parameter core) and the one-term burden taxrint_xb (in
# place of tax per WAP), deflator vs RPI, both periods, with and without the projection.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
invisible(capture.output(source("fiscal_ratio.R")))
mf <- "../a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx"
a47 <- suppressMessages(as.data.frame(readxl::read_excel(mf, "A47. Wages and prices", col_names = FALSE, .name_repair = "minimal")))
yy <- suppressWarnings(as.numeric(a47[[1]])); rpi <- suppressWarnings(as.numeric(a47[[7]])); k <- !is.na(yy) & !is.na(rpi)
R <- data.frame(year = yy[k], rpi = rpi[k]); R$rpi_infl <- 100 * c(NA, diff(log(R$rpi))); R <- rbind(R[R$year <= 2016, ], data.frame(year = 2017:2019, rpi = NA, rpi_infl = c(3.6, 3.3, 2.6)))
cal <- R$rpi_infl[match(A$year, R$year)]; A$rpi_fy <- 0.75 * pv(cal) + 0.25 * cal                                    # fiscal year ending in March of t
cat(sprintf("RPI vs GDP-deflator inflation (fiscal years), 1965-2019: mean %.2f vs %.2f; correlation %.3f; largest gaps %s\n", mean(A$rpi_fy[A$year %in% 1965:2019], na.rm = TRUE),
            mean(A$inflation[A$year %in% 1965:2019], na.rm = TRUE), cor(A$rpi_fy, A$inflation, use = "pair"),
            paste(sprintf("%d %+.1f", A$year, A$rpi_fy - A$inflation)[order(-abs(A$rpi_fy - A$inflation))][1:4], collapse = ", ")))
adj_r <- A$rpi_fy / 100 * pv(A$psnd_gdp); A$taxrint_xb_rpi <- bw(A$int_xb_gdp - adj_r); A$taxrint_cg_rpi <- bw(A$int_cg_gdp - adj_r)
A$rint_xb_tme_rpi <- A$int_xb_tme - 100 * (nx(A$rpi_fy) / 100 * A$psnd_gdp) / A$tme_fs
for (v in c("taxrint_xb_rpi", "taxrint_cg_rpi", "rint_xb_tme_rpi")) { A[[paste0("L_", v)]] <- lag1(A[[v]]); S[[paste0("L_", v)]] <- A[[paste0("L_", v)]][match(S$year, A$year)] }
CORE6 <- setdiff(CORE_A, "L_int_tme"); hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); ct[v, c(1, 4)] }
cat(sprintf("\n%-36s %-10s %3s %18s %18s %8s %18s %8s\n", "term", "period", "N", "term (p)", "projection (p)", "dAIC", "no proj: term", "dAIC"))
for (sp in list(list("rint_xb_tme  (deflator)", "add", "L_rint_xb_tme"), list("rint_xb_tme  (RPI)", "add", "L_rint_xb_tme_rpi"),
                list("taxrint_xb   (deflator) for tax", "tax", "L_taxrint_xb"), list("taxrint_xb   (RPI) for tax", "tax", "L_taxrint_xb_rpi"),
                list("taxrint_cg   (deflator) for tax", "tax", "L_taxrint_cg"), list("taxrint_cg   (RPI) for tax", "tax", "L_taxrint_cg_rpi")))
  for (y0 in c(1965, 1977)) { d <- subset(S, year >= y0); x <- sp[[3]]; xs <- if (sp[[2]] == "add") c(CORE6, x) else sub("^L_tax_workpop$", x, CORE6)
    b0 <- if (sp[[2]] == "add") CORE6 else CORE6; d <- d[complete.cases(d[, c("mig_gross", unique(c(xs, CORE6)))]), ]
    m <- lm(reformulate(xs, "mig_gross"), d); m0 <- lm(reformulate(b0, "mig_gross"), d); n <- lm(reformulate(setdiff(xs, "delta_oadr_10"), "mig_gross"), d); n0 <- lm(reformulate(setdiff(b0, "delta_oadr_10"), "mig_gross"), d)
    a <- hc(m, x); b <- hc(m, "delta_oadr_10"); c1 <- hc(n, x)
    cat(sprintf("%-36s %d-2019 %3d %9.4f (%5s) %9.4f (%5s) %8.2f %9.4f (%5s) %8.2f\n", sp[[1]], min(d$year), nobs(m), a[1], pf2(a[2]), b[1], pf2(b[2]), AIC(m) - AIC(m0), c1[1], pf2(c1[2]), AIC(n) - AIC(n0))) }
cat("dAIC: 'add' rows against the 6-parameter core; 'for tax' rows against the same core with tax per WAP (same number of parameters).\nDone.\n")
