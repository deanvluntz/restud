# =============================================================================
# psnb_ratios.R -- ratios that contain at least one borrowing or spending measure, with growth and inflation forecasts among the partners
# (Sam, 1 Oct 2026: "not necessarily borrowing over spending, just a ratio which includes one measure of either. Or even worth trying
# growth/inflation forecasts"). Everything is information available at the start of a year s (the spring Budget of s and outturns of s-1),
# entered at t-1 (s = t-1, int_tme's timing) and at t (s = t, start-of-year information).
# Borrowing / spending measures (numerators):
#   p0      planned PSNB/GDP for the current fiscal year (Budget of s; from 1970)      chg2   planned change in PSNB/GDP over two years (1980)
#   cum2    PSNB/GDP planned over the next two years (1980)                            psnb   outturn PSNB/GDP, fiscal year s-1
#   tme     outturn TME/GDP, fiscal year s-1 (OBR PSF)                                   tmefc  planned TME/GDP for the current fiscal year (1990)
# Partners (denominators; strictly positive series only, so that a ratio never divides by zero or changes sign through the denominator):
#   tme, tax (tax/GDP), debt (net debt/GDP), intsh (clean debt interest / TME), gilt (constructed 10-year GRY, 1976), gnom_obr (forecast
#   nominal GDP growth for the current fiscal year, OBR/HMT NGDP forecasts, 1982), gnom_hmt (HMT start-of-year real growth forecast + NIESR
#   short-term inflation expectation, 1976-2016), infl_exp (NIESR short-term inflation expectation, 1965-2016), gnom_out (mean outturn
#   nominal GDP growth of the five years to s-1, all years).
# Grounded constructs (debt arithmetic): stab_*  planned borrowing p0 over the debt-stabilising deficit, debt x g / (1 + g), with g = each
#   nominal growth measure (> 1 = borrowing that raises the debt ratio); rg_fc = gilt GRY minus forecast nominal growth (a difference).
# Years before a ratio exists: Sam prefers an indicator to zeros. Three treatments: (a) only the years where it exists; (b) zeros + an
#   indicator for the missing years (the indicator takes those years' own mean, so the slope is estimated from real values only); both
#   samples (from 1965 and from 1977).
# Rule (Sam's criterion): the ratio's own HAC p < .10 and lower AIC (the indicator counted as a parameter), with AND without the projection.
# Number of rows is printed: with ~50 ratios x 2 timings x 3 windows, chance alone produces passes.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
options(restud.paths_only = TRUE); invisible(capture.output(source("psnb_paths.R"))); options(restud.paths_only = FALSE)
CORE6 <- setdiff(CORE_A, "L_int_tme"); NP <- setdiff(CORE6, "delta_oadr_10")
ng <- hofd("NGDP"); ng$value[ng$value == 0] <- NA; ng <- ng[!is.na(ng$value), ]; NV <- split(ng, ng$vint); nvd <- as.Date(names(NV))
gnobr <- function(s) { ok <- which(nvd <= as.Date(sprintf("%d-07-31", s)) & nvd >= as.Date(sprintf("%d-02-01", s - 1)))
  for (k in rev(ok)) { v <- NV[[k]]; a <- v$value[v$target == s]; b <- v$value[v$target == s - 1]; if (length(a) && length(b)) return(100 * (a / b - 1)) }; NA }
TMEg <- hofd("TME"); spring <- function(D, s) { d <- D[as.integer(format(D$vint, "%Y")) == s & as.integer(format(D$vint, "%m")) <= 7, ]; if (!nrow(d)) NULL else d[d$vint == max(d$vint), ] }
B <- data.frame(s = 1964:2019); Ay <- function(v, lagby = 1) A[[v]][match(B$s - lagby, A$year)]
B$p0 <- sapply(B$s, function(s) { x <- path(s, 0); if (is.null(x)) NA else unname(x[2]) })
B$chg2 <- PV$chg_2[match(B$s, PV$year)]; B$cum2 <- PV$cum_2[match(B$s, PV$year)]
B$psnb <- Ay("psnb_gdp"); B$tme <- ot$tme_gdp[match(B$s - 1, ot$year)]; B$tmefc <- sapply(B$s, function(s) { d <- spring(TMEg, s); if (is.null(d)) NA else { x <- d$value[d$target == s]; if (length(x)) x[1] else NA } })
B$tax <- Ay("tax_gdp"); B$debt <- Ay("psnd_gdp"); B$intsh <- Ay("int_xb_tme"); B$gilt <- gy$gry_10[match(B$s - 1, gy$year)]
B$gnom_obr <- sapply(B$s, gnobr); B$infl_exp <- A$st_inf_exp[match(B$s, A$year)]; B$gnom_hmt <- A$growth_fc[match(B$s, A$year)] + B$infl_exp
B$gnom_out <- sapply(B$s, function(s) mean(gdpg[as.character((s - 5):(s - 1))]))
NUM <- c("p0", "chg2", "cum2", "psnb", "tme", "tmefc"); DEN <- c("tme", "tax", "debt", "intsh", "gilt", "gnom_obr", "gnom_hmt", "infl_exp", "gnom_out")
RAT <- list(); for (n in NUM) for (dn in DEN) if (n != dn) { RAT[[paste0(n, "/", dn)]] <- ifelse(B[[dn]] > 0, B[[n]] / B[[dn]], NA) }
for (g in c("gnom_obr", "gnom_hmt", "gnom_out")) { stab <- B$debt * (B[[g]] / 100) / (1 + B[[g]] / 100); RAT[[paste0("stab_", g)]] <- ifelse(stab > 0, B$p0 / stab, NA)
  RAT[[paste0("rg_", g)]] <- B$gilt - B[[g]] }
cat(sprintf("Ratios built: %d; first year available: %s\n", length(RAT), paste(sprintf("%s %s", names(RAT), sapply(RAT, function(x) if (all(is.na(x))) "none" else min(B$s[!is.na(x)]))), collapse = "; ")))
jtp <- function(m, add) { V <- tryCatch(vcovHAC(m), error = function(e) NULL); b <- coef(m)[add]; if (is.null(V) || any(is.na(b))) return(NA)
  W <- tryCatch(drop(t(b) %*% solve(V[add, add, drop = FALSE], b)), error = function(e) NA); stats::pf(W / length(add), length(add), df.residual(m), lower.tail = FALSE) }
one <- function(dd, x, ind) { add <- c(x, if (!is.null(ind) && length(unique(dd[[ind]])) > 1) ind); base <- CORE6
  m <- lm(reformulate(c(base, add), "mig_gross"), dd); m0 <- lm(reformulate(base, "mig_gross"), dd); n <- lm(reformulate(c(NP, add), "mig_gross"), dd); n0 <- lm(reformulate(NP, "mig_gross"), dd)
  if (any(is.na(coef(m))) || any(is.na(coef(n)))) return(NULL); a <- coeftest(m, vcov. = vcovHAC(m)); e <- coeftest(n, vcov. = vcovHAC(n))
  data.frame(N = nrow(dd), y0 = min(dd$year), b = a[x, 1], p = a[x, 4], dA = AIC(m) - AIC(m0), bn = e[x, 1], pn = e[x, 4], dAn = AIC(n) - AIC(n0), proj = a["delta_oadr_10", 1], pp = a["delta_oadr_10", 4],
             pass = a[x, 4] < .10 && AIC(m) < AIC(m0) && e[x, 4] < .10 && AIC(n) < AIC(n0) && sign(a[x, 1]) == sign(e[x, 1])) }
R <- list()
for (nm in names(RAT)) for (tim in c("t-1", "t")) { lagby <- if (tim == "t-1") 1 else 0; S$.x <- RAT[[nm]][match(S$year - lagby, B$s)]
  for (y0 in c(1965, 1977)) { dd <- subset(S, year >= y0); dd <- dd[complete.cases(dd[, c("mig_gross", CORE6)]), ]; if (sum(!is.na(dd$.x)) < 25) next
    av <- dd[!is.na(dd$.x), ]; r1 <- if (y0 == 1965 || min(av$year) <= 1977) one(av, ".x", NULL) else NULL
    dd$.miss <- as.numeric(is.na(dd$.x)); dd$.x[is.na(dd$.x)] <- 0; r2 <- if (any(dd$.miss == 1)) one(dd, ".x", ".miss") else NULL
    if (!is.null(r1) && y0 == 1965) R[[length(R) + 1]] <- cbind(ratio = nm, timing = tim, treat = "where it exists", r1)
    if (!is.null(r2)) R[[length(R) + 1]] <- cbind(ratio = nm, timing = tim, treat = sprintf("zeros+indicator from %d", y0), r2)
    if (is.null(r2) && !is.null(r1) && y0 == 1977) R[[length(R) + 1]] <- cbind(ratio = nm, timing = tim, treat = "complete from 1977", r1) } }
R <- do.call(rbind, R); rownames(R) <- NULL; write.csv(R, "psnb_ratios_screen.csv", row.names = FALSE)
cat(sprintf("\nRows: %d. Pass (own p < .10, lower AIC, same sign, with and without the projection): %d (%.0f%%)\n", nrow(R), sum(R$pass), 100 * mean(R$pass)))
R$fam <- sub("_.*|/.*", "", R$ratio)
both <- function(nm, tim) { k <- R$ratio == nm & R$timing == tim; a <- R[k & R$treat == "zeros+indicator from 1965", ]; b <- R[k & R$treat %in% c("zeros+indicator from 1977", "complete from 1977"), ]
  nrow(a) == 1 && nrow(b) == 1 && a$pass && b$pass }
key <- unique(R[, c("ratio", "timing")]); key$both <- mapply(both, key$ratio, key$timing)
cat(sprintf("Ratios passing on BOTH samples (zeros + indicator from 1965, and from 1977): %d -> %s\n", sum(key$both), paste(apply(key[key$both, 1:2], 1, paste, collapse = " @ "), collapse = "; ")))
pr2 <- function(r) cat(sprintf("   %-18s %-4s %-26s %d-2019 N %2d  %9.4f (p %5s) dAIC %6.2f | no proj %9.4f (p %5s) dAIC %6.2f | proj %.4f (p %s)%s\n", r$ratio, r$timing, r$treat, r$y0, r$N, r$b, pf2(r$p), r$dA,
                                 r$bn, pf2(r$pn), r$dAn, r$proj, pf2(r$pp), if (r$pass) "  PASS" else ""))
cat("\n-- every row of the ratios passing on both samples --\n"); for (k in which(paste(R$ratio, R$timing) %in% paste(key$ratio, key$timing)[key$both])) pr2(R[k, ])
cat("\n-- all other passing rows, best AIC beside the projection first --\n"); o <- R[R$pass & !paste(R$ratio, R$timing) %in% paste(key$ratio, key$timing)[key$both], ]; o <- o[order(o$dA), ]
for (k in seq_len(nrow(o))) pr2(o[k, ])
cat("\n-- growth / inflation forecasts and the debt-arithmetic constructs (every row) --\n")
for (k in which(grepl("^stab_|^rg_|gnom|infl_exp", R$ratio))) pr2(R[k, ])
cat("\nDone.\n")
