# =============================================================================
# permits_long.R -- MANUSCRIPT (Sam, 27 Sep 2026): the policy lever on the longest sample the projections allow.
#   Work permits and first permissions (Bauer and Salt, 2020, Fig. 1) run from 1946. The binding constraint is the
#   projection: the earliest national population projection held is now the 1958-based projection printed in the
#   Government Actuary's Second Quinquennial Review (1960), Table B (GB, 5-year age groups, 1958/68/78/88/98), which
#   is in force for 1958-62 (the 1963-based takes over in 1963). Fiscal controls before 1962 from the OBR Historical
#   Public Finances Database (FY t-1/t -> t) and the HMT deflator; working-age population from the same Table B,
#   scaled to the 1963-based UK level; unemployment from the Millennium dataset (A1/A50).
#   Built by hand into determinants/pre1963_block_1956_1962.csv (method in its 'source' column).
#   Coverage of the lever: before 1962 permits cover aliens only (Commonwealth citizens entered freely); 1962-72
#   Commonwealth workers entered on employment vouchers (Bauer and Salt, 2020, Table 9: determinants/
#   commonwealth_vouchers_1962_1972.csv); from 1973 permits cover both. 'lever' = permits + vouchers.
#   Panel: permits 1965-2008 | permits 1959-2008 | +post1973 | lever 1964-2008 | lever +post1973
# Run from diss_replication/:  source("permits_long.R")
# =============================================================================
source("tables_common.R"); SAMPLE <- "1964"; source("data_common.R")
pre <- read.csv("determinants/pre1963_block_1956_1962.csv"); vo <- read.csv("determinants/commonwealth_vouchers_1962_1972.csv"); wp <- read.csv("determinants/work_permits.csv")
Z <- data.frame(year = 1956:2019)
Z$delta_oadr_10 <- df$delta_oadr_10[match(Z$year, df$year)]; Z$tax_workpop <- df$tax_workpop[match(Z$year, df$year)]
Z$ln_debt_gdp <- df$ln_debt_gdp[match(Z$year, df$year)]; Z$unem <- df$unem[match(Z$year, df$year)]; Z$labour_gov <- df$labour_gov[match(Z$year, df$year)]
for (v in c("delta_oadr_10", "tax_workpop", "ln_debt_gdp", "unem")) { i <- is.na(Z[[v]]) & Z$year %in% pre$year; Z[[v]][i] <- pre[[v]][match(Z$year[i], pre$year)] }
if (anyNA(Z$labour_gov)) { p1 <- read.csv("determinants/boe_prop1_1955_2019.csv"); Z$labour_gov <- p1$labour_gov[match(Z$year, p1$year)] }
Z$permits <- wp$permits[match(Z$year, wp$year)] / 1000
Z$vouchers <- ifelse(Z$year < 1962, 0, ifelse(Z$year > 1972, 0, vo$vouchers[match(Z$year, vo$year)] / 1000))
Z$lever <- Z$permits + Z$vouchers; Z$post1973 <- as.numeric(Z$year >= 1973)
for (v in c("permits", "lever", "tax_workpop", "ln_debt_gdp", "unem")) Z[[paste0("L_", v)]] <- lag1(Z[[v]])
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
DEFS[["lever"]] <- "work permits and first permissions plus Commonwealth employment vouchers (1962-72), thousands"; DEFS[["L_lever"]] <- "lever lagged one year"
DEFS[["post1973"]] <- "indicator from 1973: Immigration Act 1971 in force (Commonwealth workers move from vouchers to permits)"
P <- function(y0, y1) subset(Z, year >= y0 & year <= y1)
mods <- list("permits 1965-2008" = fit(P(1965, 2008), c("L_permits", "delta_oadr_10", CT), "permits"),
             "permits 1959-2008" = fit(P(1959, 2008), c("L_permits", "delta_oadr_10", CT), "permits"),
             "+post1973" = fit(P(1959, 2008), c("L_permits", "delta_oadr_10", "post1973", CT), "permits"),
             "lever 1964-2008" = fit(P(1964, 2008), c("L_lever", "delta_oadr_10", CT), "lever"),
             "lever +post1973" = fit(P(1964, 2008), c("L_lever", "delta_oadr_10", "post1973", CT), "lever")
             )
panel(mods, "The policy lever, 1959-2008", c("delta_oadr_10", "post1973", CT, "L_permits", "L_lever"), w = 19, lw = 15,
      c("delta_oadr_10 for 1958-62: the 1958-based projection (GAD Second Quinquennial Review, 1960, Table B); it barely moves (29.5-29.9), so the",
        "pre-1963 years add observations on the lever rather than identifying variation in the projection. Tier 2 (2009-) is a different basis and is not spliced."))
