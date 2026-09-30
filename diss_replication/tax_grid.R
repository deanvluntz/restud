# =============================================================================
# tax_grid.R -- every numerator x denominator combination of the tax-burden term
# (Sam, 23 Sep 2026). The fiscal specification is re-estimated with the lagged
# tax term measured 18 ways; everything else (lagged DV, projection, lagged
# debt log change, lagged unemployment) is unchanged.
#
#   Numerators (OBR Historical Public Finances Database, 2024-25 prices via the
#   HM Treasury March 2026 deflator; nominal for the ratio measures):
#     NAT     national accounts taxes
#     Lab     income tax + National Insurance contributions
#     Lab+VAT income tax + NICs + VAT (taxes borne directly by households)
#   Denominators:
#     WAP       working-age population (ONS)                -> GBP thousand per person
#     Pop       total population (ONS)                      -> GBP thousand per person
#     OADR      the current old-age dependency ratio (per 1,000; NPP, GAD/ONS), lagged
#               one year like every measure here; NAT/OADR is the workbook's L_tax_oadr_0
#     GDP       nominal GDP (OBR HPFD)                      -> per cent
#     Wages     compensation of employees ("wages", as defined in Section 7 of the
#               thesis; ONS DTWM), recovered from the workbook's
#               lab_tax_comp = (IT + NICs) / compensation   -> per cent
#     Taxpayer  income-tax payers (HMRC Tables 2.1 and 2.1a; 2008-09 interpolated)
#               -> GBP thousand per payer. Counts start in 1978-79, so these rows
#               run from 1980 and carry the 1991 independent-taxation step
#               ('indep'); dAIC is against NAT/WAP on the same sample with the step.
#
# dAIC is relative to the Table 2 measure (NAT/WAP); |dAIC| < 2 means the two
# fit indistinguishably. HAC = sandwich::vcovHAC defaults, as everywhere else.
#
# Run from diss_replication/:   START <- 1979; source("tax_grid.R")   (thesis)
#                               START <- 1977; source("tax_grid.R")   (manuscript)
# =============================================================================
library(readxl); library(lmtest); library(sandwich); library(car)
if (!exists("START")) START <- 1979

raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
stopifnot(!anyDuplicated(raw$year), all(diff(raw$year) == 1))
fb  <- read.csv("fiscal_block_HPFD.csv"); fb <- fb[order(fb$year), ]

lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK,
                           delta_oadr_10 = delta_oadr_10, unem = unem,
                           ln_debt_gdp = ln_debt_gdp))
for (v in c("log_mig_rate", "unem", "ln_debt_gdp")) df[[paste0("L_", v)]] <- lag1(df[[v]])

# --- the 18 measures, by fiscal year label = workbook year ------------------
g   <- function(col, yrs) fb[[col]][match(yrs, fb$year)]
yrs <- fb$year
num_real <- list(NAT = g("tax_nat_real_gbp_bn", yrs), Lab = g("tax_lab_real_gbp_bn", yrs),
                 "Lab+VAT" = g("tax_lab_real_gbp_bn", yrs) + g("tax_vat_real_gbp_bn", yrs))
num_nom  <- list(NAT = g("tax_nat_gbp_bn", yrs), Lab = g("tax_lab_gbp_bn", yrs),
                 "Lab+VAT" = g("tax_lab_gbp_bn", yrs) + g("tax_vat_gbp_bn", yrs))
comp_nom <- g("tax_lab_gbp_bn", yrs) / raw$lab_tax_comp[match(yrs, raw$year)]
oadr0    <- raw$oadr_0[match(yrs, raw$year)]            # per 1,000, as in L_tax_oadr_0
tp <- raw$inc_taxpayersK[match(yrs, raw$year)]
ok <- !is.na(tp); tp[yrs >= min(yrs[ok]) & yrs <= max(yrs[ok])] <-
  approx(yrs[ok], tp[ok], xout = yrs[yrs >= min(yrs[ok]) & yrs <= max(yrs[ok])])$y
MEAS <- list()
for (k in names(num_real)) {
  MEAS[[paste0(k, "/WAP")]]      <- num_real[[k]] / g("wap_m", yrs)
  MEAS[[paste0(k, "/Pop")]]      <- num_real[[k]] / g("totpop_m", yrs)
  MEAS[[paste0(k, "/OADR")]]     <- num_real[[k]] / oadr0
  MEAS[[paste0(k, "/GDP")]]      <- 100 * num_nom[[k]] / g("gdp_nominal_gbp_bn", yrs)
  MEAS[[paste0(k, "/Wages")]]     <- 100 * num_nom[[k]] / comp_nom
  MEAS[[paste0(k, "/Taxpayer")]] <- num_real[[k]] / (tp / 1000)
}
safe <- function(s) gsub("[^A-Za-z]", "_", s)
for (k in names(MEAS)) df[[paste0("L_", safe(k))]] <- MEAS[[k]][match(df$year - 1, yrs)]
df$indep <- as.integer(df$year >= 1991)

S <- subset(df, year >= START & year <= 2019)
CTRL <- c("delta_oadr_10", "L_ln_debt_gdp", "L_unem")
fitm <- function(tax, d, extra = NULL)
  lm(reformulate(c("L_log_mig_rate", tax, extra, CTRL), "log_mig_rate"), data = d)
pf <- function(p) if (p < 0.001) "<.001" else sprintf("%.3f", p)

ref_full <- fitm("L_NAT_WAP", S)
ref_tp   <- fitm("L_NAT_WAP", subset(S, year >= 1980), "indep")
cat(sprintf("\n===== Tax-burden term, all numerator x denominator combinations, %d-2019 =====\n\n", START))
cat(sprintf("%-16s %3s %8s %6s %8s %6s %7s %7s %6s %5s %6s\n",
            "measure", "N", "proj", "p", "tax", "p", "adjR2", "AIC", "dAIC", "VIF", "RESET"))
for (den in c("WAP", "Pop", "OADR", "GDP", "Wages", "Taxpayer")) {
  for (nm in c("NAT", "Lab", "Lab+VAT")) {
    v <- paste0("L_", safe(paste0(nm, "/", den)))
    if (den == "Taxpayer") { d <- subset(S, year >= 1980); m <- fitm(v, d, "indep"); ref <- ref_tp }
    else                   { d <- S;                        m <- fitm(v, d);          ref <- ref_full }
    ct <- coeftest(m, vcov. = vcovHAC(m))
    cat(sprintf("%-16s %3d %8.4f %6s %8.4f %6s %7.4f %7.2f %+6.2f %5.1f %6s\n",
                paste0(nm, "/", den), nobs(m), ct["delta_oadr_10", 1], pf(ct["delta_oadr_10", 4]),
                ct[v, 1], pf(ct[v, 4]), summary(m)$adj.r.squared, AIC(m), AIC(m) - AIC(ref),
                max(vif(m)), pf(resettest(m)$p.value)))
  }
}
cat("HAC (vcovHAC) p-values. dAIC vs NAT/WAP on the same sample (Taxpayer rows: 1980-2019\n",
    "with the 1991 'indep' step in both). Ratio measures in per cent; per-head in GBP thousand.\n",
    "Wages = compensation of employees (ONS series DTWM). OADR = current old-age dependency\n",
    "ratio (per 1,000), lagged one year like every tax measure.\n", sep = "")
