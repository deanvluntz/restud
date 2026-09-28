# =============================================================================
# delay_counterfactual.R -- Online Appendix C: the delay, quantified.
#
# Two calculations, both OPTIONAL to the main replication:
#   (a) the migration wedge ASSUMED by each vintage: delta_zm_10 - delta_pub_10
#       from zero_migration_deltas_R.csv (run replication.R first to create it);
#   (b) the REALISED zero-net-migration counterfactual of the working-age
#       population and OADR, 1979-2018, using actual net migration from
#       net_migration.csv (ONS, 2020: LTIM table 2.00, All citizenships
#       Balance, revised headline series; pre-1991 values are IPS-only).
#
# Conventions (identical to Online Appendix A, and conservative for the OADR):
# all net migrants are treated as working-age, and migrants' UK-born children
# are excluded. Cumulative net migration 1979-1993 is only 56 thousand, so the
# working-age convention is empirically innocuous: virtually all cumulated
# migration is post-1994 and still of working age by 2018.
#
# Caveats printed with the results: the actual OADR path includes the female
# state-pension-age equalisation (hence its fall after 2014), and the
# published-projection pace is flattened by legislated SPA rises, so the
# years-of-delay translation uses the zero-migration pace; the points gap is
# the robust statistic.
# =============================================================================

library(readxl)

ds  <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
ds  <- ds[!is.na(ds$year), ]
net <- read.csv("net_migration.csv")
zmd <- read.csv("zero_migration_deltas_R.csv")

# WAP(t) in thousands, recovered from the dataset's own rate construction:
# nEU_inflows_WAPlagK(t+1) = nEU_inflows(t+1) / WAP(t) * 1000
wap <- function(t) {
  r <- ds[ds$year == t + 1, ]
  as.numeric(r$nEU_inflows / r$nEU_inflows_WAPlagK)
}

cat("--- (a) the wedge each vintage ASSUMED (points of 10-yr OADR deterioration",
    "\n    absorbed by that vintage's migration assumptions) ---\n")
for (y in c(1985, 1991, 1997, 2003, 2009)) {
  r <- zmd[zmd$year == y, ]
  cat(sprintf("  %d: %6.2f points/decade\n", y, r$delta_zm_10 - r$delta_pub_10))
}

cat("\n--- (b) realised counterfactual: OADR without net migration since 1979 ---\n")
cum <- 0
out <- data.frame()
for (t in 1979:2018) {
  cum  <- cum + net$net_migration_thousands[net$year == t]
  oadr <- ds$oadr_0[ds$year == t]
  w    <- wap(t)
  cf   <- oadr * w / (w - cum)
  out  <- rbind(out, data.frame(year = t, oadr = oadr, wap = w, cum_net = cum,
                                oadr_cf = cf, gap = cf - oadr))
}
print(round(subset(out, year %in% c(1979, 1994, 2004, 2014, 2018)), 1), row.names = FALSE)

g18 <- out$gap[out$year == 2018]
pace_zm <- zmd$delta_zm_10[zmd$year == 2018] / 10
cat(sprintf("\n2018: gap %.1f points (counterfactual %.1f vs actual %.1f); cumulative net",
            g18, out$oadr_cf[out$year == 2018], out$oadr[out$year == 2018]))
cat(sprintf("\nmigration is %.1f%% of the 2018 working-age population.",
            100 * out$cum_net[out$year == 2018] / out$wap[out$year == 2018]))
cat(sprintf("\nAt the zero-migration pace of ageing (%.2f points/yr), the gap is worth %.0f years.\n",
            pace_zm, g18 / pace_zm))
