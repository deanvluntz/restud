# =============================================================================
# robust_extras.R -- three standard checks not yet run (Sam, 1 Oct 2026)
# A. SPECIFICATION CURVE (Simonsohn, Simmons and Nelson, 2020): the projection coefficient across every combination of the defensible choices
#    already made or tested along the way, nothing else --
#      outcome   gross inflows (log persons; main) | the published per-WAP rate (log) | gross inflows with LTIM from 1991 (robustness series)
#      tax       per WAP (main) | per adult | per resident | real taxes + public-sector debt interest per WAP (fiscal_ratio.R)
#      labour    log V/U (main) | ILO unemployment | log vacancies / WAP
#      debt      log change in debt/GDP (main) | change in debt/GDP in points
#      sample    1965-2019 (main) | 1977-2019
#      timing    projection at t (main) | at t-1
#    = 288 specifications, each with its own lagged outcome. Reported: share positive, share with HAC p < .05, median and range; medians by
#    choice; the curve itself (figure: specification_curve.png) and every row (specification_curve.csv).
# B. NET MIGRATION as the outcome (MPR, 2011, report gross and net): net non-EU migration (IPS 3.01, non-EU citizens to and from non-EU
#    countries) per 1,000 population, 1977-2019; and 1965-2019 with the pre-1975 non-British net balance from LTIM 2.00 (inflows minus
#    all-citizenship outflows plus British outflows), the same splice as the inflow series. Level form (net flows can approach zero).
# C. PRECISION WEIGHTING: the IPS publishes 95% confidence intervals for each estimate; weighted least squares with weights 1 / var(log inflow),
#    var = (CI / 1.96 / inflow)^2, 1977-2019.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
invisible(capture.output(source("fiscal_ratio.R")))                                  # S with L_taxrint_xb and the canonical variables
A2 <- { SAMPLE <- "1964"; source("data_common.R"); df[df$year <= 2019, ] }; A2 <- A2[order(A2$year), ]
A2$ltim_gross <- A2$ltim_rate + log(lag1(A2$workpop)); k91 <- which(A2$year == 1991)
A2$splice_gross <- A2$mig_gross; A2$splice_gross[A2$year >= 1991] <- A2$ltim_gross[A2$year >= 1991] - (A2$ltim_gross[k91] - A2$mig_gross[k91]); A2$L_splice_gross <- lag1(A2$splice_gross)
ips <- read.csv("determinants/ips_301_citizenship_by_residence_1975_2019.csv", check.names = FALSE)
A2$net_k <- ips[["Balance_NON-EU_nonEU"]][match(A2$year, ips$year)]
pre <- A2$year < 1975; A2$net_k[pre] <- (A2$dv_A - (A2$all_out - A2$brit_out))[pre]                  # LTIM non-British net balance before 1975 (thousands)
A2$net_pk <- 1000 * A2$net_k / (A2$totpop / 1000); A2$L_net_pk <- lag1(A2$net_pk)                # per 1,000 population
A2$ci_in <- ips[["Inflow_NON-EU_nonEU_ci"]][match(A2$year, ips$year)]; A2$in_k <- ips[["Inflow_NON-EU_nonEU"]][match(A2$year, ips$year)]
for (v in c("splice_gross", "L_splice_gross", "net_pk", "L_net_pk", "ci_in", "in_k", "net_k")) S[[v]] <- A2[[v]][match(S$year, A2$year)]
hc <- function(m, v) { ct <- coeftest(m, vcov. = vcovHAC(m)); if (v %in% rownames(ct)) ct[v, c(1, 2, 4)] else c(NA, NA, NA) }

# ---- A. specification curve ---------------------------------------------------------------------------------------------------------------------
OUTS <- c(gross = "mig_gross", rate_wap = "mig_rate", ltim_splice = "splice_gross"); TAX <- c(per_wap = "L_tax_workpop", per_adult = "L_tax_voter", per_resident = "L_tax_res", tax_real_int = "L_taxrint_xb")
LAB <- c(ln_vu = "L_ln_vac_unem", unem = "L_unem", ln_vac_wap = "L_ln_vac_wap"); DEBT <- c(ln_change = "L_ln_debt_gdp", pp_change = "L_d_psnd_gdp")
SMP <- c("1965-2019" = 1965, "1977-2019" = 1977); TIM <- c(t = "delta_oadr_10", "t-1" = "L_delta_oadr_10")
G <- expand.grid(outcome = names(OUTS), tax = names(TAX), labour = names(LAB), debt = names(DEBT), sample = names(SMP), timing = names(TIM), stringsAsFactors = FALSE)
R <- do.call(rbind, lapply(seq_len(nrow(G)), function(i) { g <- G[i, ]; y <- OUTS[[g$outcome]]; P <- TIM[[g$timing]]
  xs <- c(paste0("L_", y), P, TAX[[g$tax]], LAB[[g$labour]], DEBT[[g$debt]]); d <- subset(S, year >= SMP[[g$sample]])
  m <- fit(d, xs, y); h <- hc(m, P); data.frame(g, b = h[1], se = h[2], p = h[3], n = nobs(m)) }))
main <- with(R, outcome == "gross" & tax == "per_wap" & labour == "ln_vu" & debt == "ln_change" & timing == "t")
write.csv(R, "specification_curve.csv", row.names = FALSE)
cat(sprintf("A. Specification curve: %d specifications\n   projection positive in %d (%.0f%%); HAC p < .05 in %d (%.0f%%); p < .10 in %d (%.0f%%)\n", nrow(R), sum(R$b > 0), 100 * mean(R$b > 0),
            sum(R$p < .05), 100 * mean(R$p < .05), sum(R$p < .10), 100 * mean(R$p < .10)))
cat(sprintf("   median %.4f; range %.4f to %.4f; the main specification: %s\n", median(R$b), min(R$b), max(R$b), paste(sprintf("%s %.4f (p %s)", R$sample[main], R$b[main], vapply(R$p[main], pf2, "")), collapse = "; ")))
cat("   by choice -- median projection [share p < .05]:\n")
for (dim in c("outcome", "tax", "labour", "debt", "sample", "timing")) cat(sprintf("   %-8s %s\n", dim, paste(vapply(unique(R[[dim]]), function(v) { k <- R[[dim]] == v; sprintf("%s %.4f [%.0f%%]", v, median(R$b[k]), 100 * mean(R$p[k] < .05)) }, ""), collapse = "; ")))
cat(sprintf("   specifications with p >= .05: %s\n", if (any(R$p >= .05)) paste(apply(R[R$p >= .05, c("outcome", "tax", "labour", "debt", "sample", "timing")], 1, paste, collapse = "/"), collapse = "; ") else "none"))
o <- order(R$b); png("specification_curve.png", width = 1800, height = 1500, res = 200)
layout(matrix(1:2, 2), heights = c(1.1, 1)); par(mar = c(0.5, 4.5, 1.5, 1), cex = 0.8)
plot(seq_along(o), R$b[o], pch = 19, cex = 0.45, col = ifelse(R$p[o] < .05, "grey15", "grey60"), ylim = range(c(R$b - 1.96 * R$se, R$b + 1.96 * R$se, 0)), xaxt = "n", xlab = "", ylab = "projection coefficient",
     main = "Specification curve: the projection coefficient across 288 specifications")
segments(seq_along(o), (R$b - 1.96 * R$se)[o], seq_along(o), (R$b + 1.96 * R$se)[o], col = adjustcolor(ifelse(R$p[o] < .05, "grey15", "grey60"), 0.35)); abline(h = 0, lty = 2)
points(which(main[o]), R$b[o][main[o]], pch = 17, cex = 1.1, col = "black")
par(mar = c(3, 9, 0.5, 1)); rows <- c(paste0("outcome: ", names(OUTS)), paste0("tax: ", names(TAX)), paste0("labour: ", names(LAB)), paste0("debt: ", names(DEBT)), paste0("sample: ", names(SMP)), paste0("timing: ", names(TIM)))
plot(NA, xlim = c(1, nrow(R)), ylim = c(0.5, length(rows) + 0.5), yaxt = "n", xlab = "specification (ordered by coefficient)", ylab = "")
axis(2, at = rev(seq_along(rows)), labels = rows, las = 1, cex.axis = 0.7)
for (j in seq_along(rows)) { dim <- sub(":.*", "", rows[j]); val <- sub(".*: ", "", rows[j]); x <- which(R[[dim]][o] == val); points(x, rep(length(rows) + 1 - j, length(x)), pch = "|", cex = 0.5) }
dev.off(); cat("   figure: specification_curve.png; every row: specification_curve.csv\n")

# ---- B. net migration -----------------------------------------------------------------------------------------------------------------------------
cat("\nB. Net non-EU migration per 1,000 population as the outcome (own lag + core terms; level form)\n")
CORE6 <- setdiff(CORE_A, "L_int_tme")
for (y0 in c(1977, 1965)) { d <- subset(S, year >= y0); xs <- c("L_net_pk", setdiff(CORE6, "L_mig_gross")); m <- fit(d, xs, "net_pk"); np <- fit(d, setdiff(xs, "delta_oadr_10"), "net_pk")
  md <- fit(d, sub("^delta_oadr_10$", "delta_dr_10", xs), "net_pk"); h <- hc(m, "delta_oadr_10")
  cat(sprintf("   %d-2019 (N %d)%s: projection %.4f (p %s); mean of outcome %.2f per 1,000 -> %.1f%% of the mean per OADR point; dropping it costs %.2f AIC; total DR in place: %.4f (p %s)\n",
              y0, nobs(m), if (y0 == 1965) " [pre-1975 non-British LTIM net balance]" else "", h[1], pf2(h[3]), mean(attr(m, "dd")$net_pk), 100 * h[1] / mean(attr(m, "dd")$net_pk), AIC(np) - AIC(m),
              hc(md, "delta_dr_10")[1], pf2(hc(md, "delta_dr_10")[3])))
  cat(sprintf("      %s\n      RESET %s BG(1) %s BG(2) %s sup-F %s\n", coefline(m), pf2(resettest(m)$p.value), pf2(bgtest(m, 1)$p.value), pf2(bgtest(m, 2)$p.value),
              pf2(suppressWarnings(sctest(Fstats(formula(m), data = attr(m, "dd"), from = 0.2))$p.value)))) }

# ---- C. precision weighting -----------------------------------------------------------------------------------------------------------------------
cat("\nC. Weighted by IPS precision (weights 1 / var(log inflow) from the published 95% confidence intervals), 1977-2019\n")
d <- subset(S, year >= 1977); d$w <- 1 / (d$ci_in / 1.96 / d$in_k)^2; d <- d[complete.cases(d[, c("mig_gross", CORE6, "w")]), ]
cat(sprintf("   relative standard error of the inflow estimate: %.1f%% (1977) to %.1f%% (2019); range %.1f%%-%.1f%%\n", 100 * (d$ci_in / 1.96 / d$in_k)[1], 100 * tail(d$ci_in / 1.96 / d$in_k, 1),
            100 * min(d$ci_in / 1.96 / d$in_k), 100 * max(d$ci_in / 1.96 / d$in_k)))
mo <- lm(reformulate(CORE6, "mig_gross"), data = d); mw <- lm(reformulate(CORE6, "mig_gross"), data = d, weights = w)
for (mm in list(list(mo, "OLS"), list(mw, "WLS"))) { ct <- coeftest(mm[[1]], vcov. = vcovHAC(mm[[1]]))
  cat(sprintf("   %s: projection %.4f (p %s); %s\n", mm[[2]], ct["delta_oadr_10", 1], pf2(ct["delta_oadr_10", 4]), paste(sprintf("%s %.4f (%s)", sub("^L_", "", rownames(ct)[3:6]), ct[3:6, 1], vapply(ct[3:6, 4], pf2, "")), collapse = "; "))) }
cat("\nDone.\n")
