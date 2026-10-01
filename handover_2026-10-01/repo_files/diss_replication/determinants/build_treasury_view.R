# build_treasury_view.R -- "Treasury view" candidates (Sam, 28 Sep 2026, after Macpherson's 2014 list): trade openness, broad
# money growth and the workforce from the Bank of England Millennium dataset (v3.1, Thomas and Dimsdale, 2017), 1955-2016.
# Run from diss_replication/:  source("determinants/build_treasury_view.R")   -> determinants/treasury_view_1955_2016.csv
suppressPackageStartupMessages(library(readxl))
f <- "../a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx"
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
rd <- function(sheet) read_excel(f, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
a36 <- rd("A36. Trade values and BOP"); a9 <- rd("A9. Nominal GDP (A)"); a24 <- rd("A24. Monetary aggregates"); a50 <- rd("A50. Employment & unemployment"); a31 <- rd(grep("^A31", excel_sheets(f), value = TRUE)[1])   # sheet name carries trailing spaces
yr36 <- num(a36[[1]]); yr9 <- num(a9[[1]]); yr24 <- num(a24[[1]]); yr50 <- num(a50[[1]]); yr31 <- num(a31[[1]])
years <- 1955:2016
g <- function(v, yr) v[match(years, yr)]
exports <- g(num(a36[[7]]), yr36); imports <- g(num(a36[[16]]), yr36)          # value of goods and services, GBP mn
gdp_mp  <- g(num(a9[[4]]), yr9)                                               # nominal GDP at market prices, GBP mn
m4      <- num(a24[[55]]); m4_growth <- g(100 * c(NA, diff(log(m4))), yr24)  # spliced broad money (M3/M4/M4x), break-adjusted, year end
workforce_k <- g(num(a50[[9]]), yr50)                                         # total workforce (participation), thousands
bank_rate <- g(num(a31[[3]]), yr31)                                           # Bank Rate / MLR / repo rate, annual average, %
out <- data.frame(year = years, openness = 100 * (exports + imports) / gdp_mp, m4_growth = m4_growth, workforce_k = workforce_k, bank_rate = bank_rate,
                  source = "Millennium v3.1: openness = (A36 exports + imports of goods and services) / A9 nominal GDP at market prices, %; m4_growth = annual log change of A24 col 55 spliced broad money (M3/M4/M4x, break-adjusted, year end), %; workforce_k = A50 col I total workforce, thousands; bank_rate = A31 col C Bank Rate / Minimum Lending Rate / repo rate, annual average, %")
write.csv(out, "determinants/treasury_view_1955_2016.csv", row.names = FALSE)
print(out[out$year %in% c(1965, 1975, 1985, 1995, 2005, 2016), 1:4])
