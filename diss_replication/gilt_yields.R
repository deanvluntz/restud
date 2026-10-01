# =============================================================================
# gilt_yields.R -- gross redemption yields for every conventional gilt from the Bank of England's monthly price file, by the DMO's
# reference-price method (Sam, 1 Oct 2026; source file BGSPrices.xlsx, "Prices per GBP100 nominal", monthly from November 1975,
# saved as determinants/boe_bgs_prices.xlsx).
# The DMO published, for each gilt, the average mid-market closing price with "an associated gross redemption yield" (DMO, Historical
# Prices and Yields). The Bank's file gives the price; the yield is computed here with the DMO conventions for conventional gilts:
#   semi-annual coupons of c/2 paid on the redemption day and month and six months either side; the quoted price is CLEAN, so the
#   accrued interest c/2 x (days since the last coupon / days in the coupon period) is added; the yield y solves
#   dirty price = sum_i (c/2) v^(t_i) + 100 v^(t_n), v = 1 / (1 + y/2), t_i in half-years from settlement (actual/actual).
# Ex-dividend periods are ignored (small at annual frequency). If the Bank's pre-1986 quotes include accrued interest, yields of short stocks
# are biased up by at most the accrued coupon; the check against the Millennium 10-year yield below bounds the problem.
# Stocks used: conventional, dated, at least one year to redemption. Excluded: index-linked (real prices), convertibles, floating/variable
# rate, quarterly-interest and 'small' stocks and undated stocks (consols, War Loan: their flat yield c/P is reported separately).
# Double-dated stocks: the earliest redemption date if the price is at or above par (the Treasury would call), the latest if below.
# Outputs (calendar-year means of the monthly values; determinants/gilt_yields_bgs_1976_2025.csv):
#   gry_all     equal-weighted mean GRY of all eligible stocks       gry_short / gry_med / gry_long  1-5, 5-15, 15+ years to redemption
#   gry_10      10-year yield: linear interpolation of the month's yield-maturity scatter at 10 years (local mean of stocks 7-13 years)
#   slope       gry_long - gry_short                                 flat_undated  mean flat yield of the undated stocks
#   px_avg      equal-weighted mean clean price per GBP100 (conventional dated): below 100 = the stock trades at a discount
#   n_stocks    stocks used (average per month)
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
f <- "determinants/boe_bgs_prices.xlsx"; if (!file.exists(f)) stop("save BGSPrices.xlsx as determinants/boe_bgs_prices.xlsx")
a <- suppressMessages(as.data.frame(readxl::read_excel(f, 1, col_names = FALSE, .name_repair = "minimal", col_types = "text")))
xd <- function(x) as.Date(as.numeric(x), origin = "1899-12-30")
mc <- which(!is.na(suppressWarnings(as.numeric(unlist(a[4, ])))) & seq_len(ncol(a)) > 10)          # month columns (settlement row 4)
sett <- xd(unlist(a[4, mc]))
st <- a[-(1:5), ]; st <- st[!is.na(st[[1]]) & st[[1]] != "END", ]
cpn <- as.numeric(st[[2]]); nm <- st[[3]]; fy <- as.numeric(st[[5]]); lastred <- xd(st[[8]]); feat <- ifelse(is.na(st[[10]]), "", st[[10]])
undated <- is.na(st[[6]]) | is.na(lastred)
bad <- grepl("Index|IL |convertible|Convertible|^UK[0-9]", nm) | grepl("lag|Convertible|Floating|Variable|Quarterly|Small|Not always", feat)
ok <- !bad & !undated & !is.na(cpn)
firstred <- as.Date(ifelse(!is.na(fy) & !is.na(lastred), as.Date(sprintf("%d-%s", as.integer(fy), format(lastred, "%m-%d")), optional = TRUE), NA), origin = "1970-01-01")
addm <- function(d, m) { y <- as.integer(format(d, "%Y")); mo <- as.integer(format(d, "%m")) + m; y <- y + (mo - 1) %/% 12; mo <- (mo - 1) %% 12 + 1
  dd <- pmin(as.integer(format(d, "%d")), 28); as.Date(sprintf("%04d-%02d-%02d", y, mo, dd)) }
gry <- function(P, c, M, S) {                                                                      # clean price P, coupon c (%), maturity M, settlement S
  if (M <= S) return(NA_real_); k <- 0; nxt <- M; while (addm(M, -6 * (k + 1)) > S) k <- k + 1; nxt <- addm(M, -6 * k); prv <- addm(M, -6 * (k + 1))
  frac <- as.numeric(nxt - S) / as.numeric(nxt - prv); ai <- c / 2 * (1 - frac); dirty <- P + ai; t <- frac + 0:k
  pv <- function(y) sum(c / 2 / (1 + y / 2)^t) + 100 / (1 + y / 2)^t[length(t)] - dirty
  r <- tryCatch(uniroot(pv, c(-0.05, 1.5))$root, error = function(e) NA_real_); 100 * r }
rows <- list()
for (j in seq_along(mc)) { S <- sett[j]; p <- suppressWarnings(as.numeric(st[[mc[j]]]))
  for (i in which(ok & !is.na(p))) { M <- if (!is.na(firstred[i]) && firstred[i] < lastred[i] && p[i] >= 100 && firstred[i] > S) firstred[i] else lastred[i]
    tm <- as.numeric(M - S) / 365.25; if (tm < 1) next
    rows[[length(rows) + 1]] <- c(j, i, tm, gry(p[i], cpn[i], M, S), p[i]) } }
G <- as.data.frame(do.call(rbind, rows)); names(G) <- c("m", "i", "tm", "y", "p"); G$date <- sett[G$m]; G$year <- as.integer(format(G$date, "%Y"))
U <- do.call(rbind, lapply(seq_along(mc), function(j) { p <- suppressWarnings(as.numeric(st[[mc[j]]])); k <- which(undated & !is.na(p) & !is.na(cpn) & !grepl("Quarterly|Small|Death", feat))
  if (!length(k)) NULL else data.frame(m = j, flat = mean(100 * cpn[k] / p[k])) })); U$year <- as.integer(format(sett[U$m], "%Y"))
mon <- do.call(rbind, lapply(split(G, G$m), function(g) { g <- g[!is.na(g$y), ]; s10 <- g$y[g$tm >= 7 & g$tm <= 13]
  data.frame(m = g$m[1], year = g$year[1], gry_all = mean(g$y), gry_short = mean(g$y[g$tm < 5]), gry_med = mean(g$y[g$tm >= 5 & g$tm < 15]),
             gry_long = mean(g$y[g$tm >= 15]), gry_10 = if (length(s10)) mean(s10) else NA, px_avg = mean(g$p), n_stocks = nrow(g)) }))
mon$flat_undated <- U$flat[match(mon$m, U$m)]
Y <- aggregate(. ~ year, data = mon[, -1], FUN = function(z) mean(z, na.rm = TRUE), na.action = na.pass); Y$slope <- Y$gry_long - Y$gry_short
Y <- Y[Y$year >= 1976 & Y$year <= 2025, ]; Y[, -1] <- round(Y[, -1], 4)
write.csv(Y, "determinants/gilt_yields_bgs_1976_2025.csv", row.names = FALSE)
cat(sprintf("Gilt yields from the BoE price file: %d stock-months, %d stocks, %d-%d\n", nrow(G), length(unique(G$i)), min(Y$year), max(Y$year)))
print(Y[Y$year %in% c(1976, 1980, 1985, 1990, 1995, 2000, 2005, 2008, 2010, 2015, 2019, 2022, 2025), ], row.names = FALSE)
mil <- read.csv("determinants/boe_extras_1962_2019.csv"); k <- match(Y$year, mil$year)
if ("gilt10_yield" %in% names(mil)) cat(sprintf("Check against the Millennium 10-year yield (A31), common years: correlation %.3f, mean gap %+.2f points (gry_10 - Millennium)\n",
  cor(Y$gry_10, mil$gilt10_yield[k], use = "pair"), mean(Y$gry_10 - mil$gilt10_yield[k], na.rm = TRUE)))
if ("consol_yield" %in% names(mil)) cat(sprintf("Undated flat yield vs Millennium consol yield: correlation %.3f, mean gap %+.2f\n", cor(Y$flat_undated, mil$consol_yield[k], use = "pair"),
  mean(Y$flat_undated - mil$consol_yield[k], na.rm = TRUE)))
