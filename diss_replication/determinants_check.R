# =============================================================================
# determinants_check.R -- is the projection standing in for a determinant of
# migration the model leaves out? (Sam, 23 Sep 2026)
# Determinants from the UK migration literature (Hatton, 2005; Mitchell and Pain,
# 2003; Mitchell, Pain and Riley, 2011; Forte and Portes, 2017), all lagged one year:
#   relinc_dev    log UK GDP per head minus mean log GDP per head of seven large
#                 non-EU origins (India, Pakistan, Bangladesh, China, Nigeria,
#                 Philippines, South Africa); Maddison Project Database 2023
#   relinc_rich   the same against the United States and Australia
#   youth_origin  mean share aged 15-24 in the seven origins, per cent; UN WPP 2024
#   uk_growth     UK real GDP per head growth, per cent (dataset rgdp_pc)
#   network       non-EU inflows in t-2..t-4 per 1,000 working-age persons (from 1980)
# Part 1 (balance): are the projection's REVISIONS -- the news each new vintage
#   brings -- predicted by the determinants? Classical F, new-vintage years only.
# Part 2 (horse race): each determinant added to tax_spec; HAC p, dAIC vs tax_spec.
# Data: determinants/maddison_gdppc.csv, determinants/unwpp_youth_share.csv (both via
# Our World in Data grapher downloads); NPP vintage workbooks as in replication.R.
# Run from diss_replication/:   START <- 1979; source("determinants_check.R")
# =============================================================================
library(readxl); library(lmtest); library(sandwich); library(car)
if (!exists("START")) START <- 1979

raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
g   <- read.csv("determinants/maddison_gdppc.csv")
ys  <- read.csv("determinants/unwpp_youth_share.csv")
DEV <- c("IND", "PAK", "BGD", "CHN", "NGA", "PHL", "ZAF")

det <- data.frame(year = g$year,
  relinc_dev   = log(g$GBR) - rowMeans(log(g[, DEV])),
  relinc_rich  = log(g$GBR) - rowMeans(log(g[, c("USA", "AUS")])),
  youth_origin = 100 * rowMeans(ys[match(g$year, ys$year), paste0(DEV, "_1524")]))
ry <- function(v, y) raw[[v]][match(y, raw$year)]
det$uk_growth <- 100 * (ry("ln_rgdp_pc", det$year) - ry("ln_rgdp_pc", det$year - 1))
det$network   <- 1000 * (ry("nEU_inflows", det$year - 2) + ry("nEU_inflows", det$year - 3) +
                         ry("nEU_inflows", det$year - 4)) / ry("workpop", det$year - 1)

# --- projection revisions from the NPP vintages (as in replication.R, CHECK 4) ---
FILES <- c("1975" = "wuk75cc_tcm77-395111.xls", "1977" = "wuk77cc_tcm77-395021.xls",
  "1979" = "wuk79cc_tcm77-394992.xls", "1981" = "wuk81cc_tcm77-384614.xls",
  "1983" = "wuk83cc_tcm77-384103.xls", "1985" = "wuk85cc_tcm77-384028.xls",
  "1987" = "wuk87cc_tcm77-383757.xls", "1989" = "wuk89cc_tcm77-383486.xls",
  "1991" = "wuk91cc_tcm77-383156.xls", "1992" = "wuk92cc_tcm77-382886.xls",
  "1994" = "wuk94cc_tcm77-382733.xls", "1996" = "wuk96cc_tcm77-382563.xls",
  "1998" = "wuk98cc_tcm77-382317.xls", "2000" = "wuk00cc_tcm77-379749.xls",
  "2002" = "wuk02cc_tcm77-379889.xls", "2004" = "wuk04cc_tcm77-380371.xls",
  "2006" = "wuk06cc_tcm77-303310.xls", "2008" = "wUK08cc_tcm77-160543.xls",
  "2010" = "wuk10c_tcm77-235404.xls",  "2012" = "wuk12cc_tcm77-329789.xls",
  "2014" = "2014 npp.xls", "2016" = "2016 NPP.xls", "2018" = "ukpppsummary18.xls")
num_cell <- function(x) suppressWarnings(as.numeric(x))
run_of <- function(row, from) { out <- numeric(0)
  for (j in from:length(row)) { v <- num_cell(row[j]); if (is.na(v)) break; out <- c(out, v) }; out }
parse_oadr <- function(path) {
  m <- as.data.frame(suppressMessages(read_excel(path, sheet = "PERSONS", col_names = FALSE,
                                                 col_types = "text")))
  lab <- tolower(trimws(ifelse(is.na(m[[1]]), "", m[[1]])))
  years <- run_of(unlist(m[which(lab == "ages")[1], ]), 2)
  dep <- which(startsWith(lab, "dependants per"))[1]
  orow <- which(lab %in% c("pension age", "65/60+", "pensionable age") &
                seq_along(lab) > dep & seq_along(lab) <= dep + 4)[1]
  oadr <- run_of(unlist(m[orow, ]), 2); n <- min(length(years), length(oadr))
  list(years = years[1:n], oadr = oadr[1:n])
}
V <- lapply(FILES, parse_oadr); bases <- as.integer(names(V))
at <- function(v, y) { i <- y - v$years[1] + 1; if (i < 1 || i > length(v$years)) NA else v$oadr[i] }
rev_df <- do.call(rbind, lapply(1975:2019, function(y) {
  b <- max(bases[bases <= y]); v <- V[[as.character(b)]]; f <- at(v, y + 10) - at(v, y)
  prev <- bases[bases < b]
  inh <- if (y == b && length(prev)) { p <- V[[as.character(max(prev))]]; at(p, y + 10) - at(p, y) } else f
  data.frame(year = y, revision = f - inh) }))

lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK, delta_oadr_10 = delta_oadr_10,
                           tax_workpop = tax_workpop, unem = unem, ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "tax_workpop", "unem", "ln_debt_gdp")) df[[paste0("L_", v)]] <- lag1(df[[v]])
for (v in names(det)[-1]) df[[paste0("L_", v)]] <- det[[v]][match(df$year - 1, det$year)]
df <- merge(df, rev_df, by = "year")
D4 <- c("L_relinc_dev", "L_relinc_rich", "L_youth_origin", "L_uk_growth")
S  <- subset(df, year >= START & year <= 2019); S <- S[complete.cases(S[, D4]), ]
pf <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)

cat(sprintf("\n===== Part 1: do the determinants predict the projection's revisions? %d-%d =====\n",
            min(S$year), max(S$year)))
bal <- function(d, xs) { m1 <- lm(reformulate(xs, "revision"), data = d); m0 <- lm(revision ~ 1, data = d)
  a <- anova(m0, m1); cat(sprintf("  %-55s F(%d,%d) = %.2f, p = %s, N = %d\n", paste(sub("L_", "", xs), collapse = ", "),
      a$Df[2], a$Res.Df[2], a$F[2], pf(a$`Pr(>F)`[2]), nobs(m1))) }
bal(subset(S, revision != 0), D4)
S5 <- S[!is.na(S$L_network), ]
bal(subset(S5, revision != 0), c(D4, "L_network"))

cat(sprintf("\n===== Part 2: determinants added to tax_spec (HAC p) =====\n"))
BASE <- c("L_log_mig_rate", "L_tax_workpop", "delta_oadr_10", "L_ln_debt_gdp", "L_unem")
race <- function(d, add, label) {
  m0 <- lm(reformulate(BASE, "log_mig_rate"), data = d); m1 <- lm(reformulate(c(BASE, add), "log_mig_rate"), data = d)
  ct <- coeftest(m1, vcov. = vcovHAC(m1)); p <- ct["delta_oadr_10", ]
  x <- if (length(add) == 1) sprintf("%+.4f (%s)", ct[add, 1], pf(ct[add, 4])) else ""
  cat(sprintf("  %-14s N=%d  proj %.4f (%s)  %-17s dAIC %+.2f  VIF %.1f\n", label, nobs(m1), p[1], pf(p[4]),
              x, AIC(m1) - AIC(m0), max(vif(m1)))) }
for (x in D4) race(S, x, sub("L_", "", x))
race(S, D4, "all four")
race(S5, "L_network", "network")
race(S5, c(D4, "L_network"), "all five")
cat("dAIC vs tax_spec on the same sample. 'network' rows start in 1980 (inflows from 1975).\n")
