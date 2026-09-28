# =============================================================================
# delay_counterfactual_1977.R -- Online Appendix C (manuscript, 1977-2019 sample)
#
# Same two calculations as delay_counterfactual.R (which stays at 1979 for the
# thesis), with the realised counterfactual cumulated from START = 1977:
#   (a) the migration wedge ASSUMED by each vintage: delta_zm_10 - delta_pub_10
#       from zero_migration_deltas_R_1977.csv (written by replication_1977.R);
#   (b) the REALISED zero-net-migration counterfactual of the working-age
#       population and OADR, START-2018, using actual net migration from
#       net_migration.csv (ONS, 2020: LTIM table 2.00, All citizenships,
#       Balance; pre-1991 values are IPS-only). Rows 1975-78 were added from
#       2.00ltimcitizenship1964to2019.xls on 22 Sep 2026: -41, -19, -46, -5.
#
# Conventions as in Online Appendix A: all net migrants treated as working-age,
# migrants' UK-born children excluded. The points gap is the robust statistic;
# the years-of-delay translation uses the zero-migration pace of ageing.
#
# Run from diss_replication/:  source("delay_counterfactual_1977.R")
# Then send the printed output; Table C2 and the Appendix C text are updated
# from it.
# =============================================================================

library(readxl)
START <- 1977
END   <- 2018

ds  <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
ds  <- ds[!is.na(ds$year), ]
net <- read.csv("net_migration.csv")
zmd <- read.csv(if (file.exists("zero_migration_deltas_R_1977.csv"))
                  "zero_migration_deltas_R_1977.csv" else "zero_migration_deltas_R.csv")
stopifnot(all(START:END %in% net$year), !anyDuplicated(net$year))

# WAP(t) in thousands, recovered from the dataset's own rate construction:
# nEU_inflows_WAPlagK(t+1) = nEU_inflows(t+1) / WAP(t) * 1000
wap <- function(t) {
  r <- ds[ds$year == t + 1, ]
  as.numeric(r$nEU_inflows / r$nEU_inflows_WAPlagK)
}

cat("--- (a) the wedge each vintage ASSUMED (points of 10-yr OADR deterioration",
    "\n    absorbed by that vintage's migration assumptions) ---\n")
for (y in c(1977, 1979, 1985, 1991, 1997, 2003, 2009, 2018)) {
  r <- zmd[zmd$year == y, ]
  if (nrow(r)) cat(sprintf("  %d: %6.2f points/decade\n", y, r$delta_zm_10 - r$delta_pub_10))
}

cat(sprintf("\n--- (b) realised counterfactual: OADR without net migration since %d ---\n", START))
cum <- 0
out <- data.frame()
for (t in START:END) {
  cum  <- cum + net$net_migration_thousands[net$year == t]
  oadr <- ds$oadr_0[ds$year == t]
  w    <- wap(t)
  cf   <- oadr * w / (w - cum)
  out  <- rbind(out, data.frame(year = t, oadr = oadr, wap = w, cum_net = cum,
                                oadr_cf = cf, gap = cf - oadr))
}
stopifnot(!anyNA(out))
show_years <- sort(unique(c(seq(START, END, by = 5), END)))
cat("Rows for Table C2 (every fifth year from the start, and the final year):\n")
print(round(subset(out, year %in% show_years), 1), row.names = FALSE)

early <- out$cum_net[out$year == 1993]
cat(sprintf("\nCumulative net migration %d-1993: %.0f thousand (%.2f%% of the 1993 working-age population).",
            START, early, 100 * early / out$wap[out$year == 1993]))
g <- out$gap[out$year == END]
pace_zm <- zmd$delta_zm_10[zmd$year == END] / 10
cat(sprintf("\n%d: gap %.1f points (counterfactual %.1f vs actual %.1f); cumulative net",
            END, g, out$oadr_cf[out$year == END], out$oadr[out$year == END]))
cat(sprintf("\nmigration of %.2f million is %.1f%% of the %d working-age population.",
            out$cum_net[out$year == END] / 1000, 100 * out$cum_net[out$year == END] / out$wap[out$year == END], END))
cat(sprintf("\nAt the zero-migration pace of ageing (%.2f points/yr), the gap is worth %.0f years.\n",
            pace_zm, g / pace_zm))
