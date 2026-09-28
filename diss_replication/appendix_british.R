# =============================================================================
# appendix_british.R -- Appendix A panels: uncontrolled flows and British citizens
# (Sam, 25 Sep 2026). Same printing conventions as appendix_exhibits.R (HAC
# p-values; classical nested F against the model without its projection term).
# Data: determinants/ips_301_citizenship_by_residence_1975_2019.csv, extracted
# from ONS table 3.01 (IPS, citizenship by country of last or next residence),
# the table from which the non-EU dependent variable is built. Every flow is
# entered as the log of the flow per 1,000 of last year's working-age population,
# on the fiscal specification's right-hand side, 1979-2019.
#   Panel 1  Uncontrolled flows.
#   Panel 2  British citizens arriving from non-EU countries, by region of last residence.
#   Panel 3  Non-EU inflows with the British flow as a control; the British equation by half-sample.
#   Then the IPS sampling-error comparison quoted in Section 9.
# Run from diss_replication/:   source("appendix_british.R")
# =============================================================================
suppressPackageStartupMessages({library(lmtest); library(sandwich); library(car); library(readxl)})
raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
ips <- read.csv("determinants/ips_301_citizenship_by_residence_1975_2019.csv", check.names = FALSE)
lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK, delta_oadr_10 = delta_oadr_10,
                           tax_workpop = tax_workpop, unem = unem, ln_debt_gdp = ln_debt_gdp, workpop = workpop))
g <- function(col, sign = 1) sign * ips[[col]][match(df$year, ips$year)]

# Flows: internal (syntactic) name = column in the IPS extract; label = column heading in the panel.
FLOWS <- list(
  nonEU_in      = list(g("Inflow_NON-EU_nonEU"),      "non-EU in"),
  brit_in_nonEU = list(g("Inflow_NON-EU_brit"),       "Brit in nonEU"),
  brit_in_EU    = list(g("Inflow_EU_brit"),           "Brit in EU"),
  brit_out      = list(g("Outflow_NON-EU_brit", -1),  "Brit out"),
  nonEU_out     = list(g("Outflow_NON-EU_nonEU", -1), "non-EU out"),
  EU_in_nonEU   = list(g("Inflow_NON-EU_EU"),         "EU in nonEU"),
  br_oceania    = list(g("Inflow_OCEANIA_brit"),            "Oceania"),
  br_namerica   = list(g("Inflow_NORTH AMERICA_brit"),      "N America"),
  br_oeurope    = list(g("Inflow_OTHER EUROPE_brit"),       "Oth Europe"),
  br_asia       = list(g("Inflow_ASIA_brit"),               "Asia"),
  br_ssa        = list(g("Inflow_SUB-SAHARAN AFRICA_brit"), "SS Africa"),
  br_rest       = list(g("Inflow_REST OF THE WORLD_brit"),  "Rest of world"))
LABEL <- vapply(FLOWS, `[[`, character(1), 2)
for (nm in names(FLOWS)) {                     # log flow per 1,000 WAP (t-1), and its lag
  v <- 1000 * FLOWS[[nm]][[1]] * 1000 / lag1(df$workpop); v[!is.finite(v) | v <= 0] <- NA
  df[[nm]] <- log(v); df[[paste0("L_", nm)]] <- lag1(df[[nm]])
}
for (v in c("log_mig_rate", "tax_workpop", "unem", "ln_debt_gdp")) df[[paste0("L_", v)]] <- lag1(df[[v]])
S <- subset(df, year >= 1979 & year <= 2019); stopifnot(nrow(S) == 41)

stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
pf <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
fit <- function(dv, d = S, extra = NULL) lm(reformulate(c(paste0("L_", dv), "delta_oadr_10", extra, CT), dv), data = d)

panel <- function(models, title, notes, w = 15, lw = 16) {
  cat(sprintf("\n===== %s =====\n\n", title))
  cts <- lapply(models, function(m) coeftest(m, vcov. = vcovHAC(m)))
  hdr <- sprintf("%-*s%s", lw, "", paste(sprintf("%*s", w, names(models)), collapse = "")); rule <- strrep("-", nchar(hdr))
  cat(hdr, "\n", rule, "\n", sep = "")
  for (v in c("delta_oadr_10", "lagged DV", CT)) {
    cell <- function(k, what) { ct <- cts[[k]]; vv <- if (v == "lagged DV") rownames(ct)[2] else v
      if (!(vv %in% rownames(ct))) return(if (what == "b") "--" else "")
      if (what == "b") sprintf("%.4f%s", ct[vv, 1], stars(ct[vv, 4])) else paste0("(", pf(ct[vv, 4]), ")") }
    cat(sprintf("%-*s%s\n", lw, v, paste(sprintf("%*s", w, vapply(seq_along(models), cell, character(1), what = "b")), collapse = "")))
    cat(sprintf("%-*s%s\n", lw, "", paste(sprintf("%*s", w, vapply(seq_along(models), cell, character(1), what = "p")), collapse = "")))
  }
  cat(rule, "\n", sep = "")
  stat <- function(lab, f) cat(sprintf("%-*s%s\n", lw, lab, paste(vapply(models, function(m) sprintf("%*s", w, f(m)), character(1)), collapse = "")))
  stat("N",       function(m) sprintf("%d", nobs(m)))
  stat("adj. R2", function(m) sprintf("%.4f", summary(m)$adj.r.squared))
  stat("AIC",     function(m) sprintf("%.2f", AIC(m)))
  stat("RESET p", function(m) pf(resettest(m)$p.value))
  stat("BG(1) p", function(m) pf(bgtest(m, order = 1)$p.value))
  stat("max VIF", function(m) sprintf("%.1f", max(vif(m))))
  F <- P <- D <- character(length(models))
  for (k in seq_along(models)) {
    r <- lm(update(formula(models[[k]]), . ~ . - delta_oadr_10), data = model.frame(models[[k]])); a <- anova(r, models[[k]])
    F[k] <- sprintf("%.2f", a$F[2]); P[k] <- paste0("(", pf(a$`Pr(>F)`[2]), ")"); D[k] <- sprintf("%+.2f", AIC(models[[k]]) - AIC(r))
  }
  for (x in list(c("F vs no proj", F), c("  p", P), c("  dAIC", D)))
    cat(sprintf("%-*s%s\n", lw, x[1], paste(sprintf("%*s", w, x[-1]), collapse = "")))
  cat(rule, "\n", sep = "")
  cat("HAC (vcovHAC) p-values in parentheses. *** p<0.01, ** p<0.05, * p<0.10.\n",
      "F rows: classical nested F (iid errors). Each column's lagged DV is its own series.\n", sep = "")
  cat(notes, sep = "\n")
}
mk <- function(keys, d = S) { ms <- lapply(keys, fit, d = d); names(ms) <- LABEL[keys]; ms }

# --- Panel 1: uncontrolled flows ----------------------------------------------
panel(mk(c("nonEU_in", "brit_in_nonEU", "brit_in_EU", "brit_out", "nonEU_out", "EU_in_nonEU")),
      "Uncontrolled flows: log flow per 1,000 working-age persons on the fiscal specification, 1979-2019",
      c("Columns (citizenship, direction, country of last or next residence; ONS table 3.01):",
        "  non-EU in     non-EU citizens arriving from non-EU countries -- the dependent variable of Figure 8.1, for reference.",
        "  Brit in nonEU British citizens arriving from non-EU countries. No entry control applies.",
        "  Brit in EU    British citizens arriving from EU countries. No entry control applies.",
        "  Brit out      British citizens leaving for non-EU countries. Not subject to policy.",
        "  non-EU out    non-EU citizens leaving the UK for non-EU countries. Entry policy does not govern departures.",
        "  EU in nonEU   EU citizens arriving from non-EU countries. Free movement: no entry control.",
        "Under the fiscal mechanism only the first column should respond to the projection."))

# --- Panel 2: British citizens in from non-EU, by region ----------------------
panel(mk(c("br_oceania", "br_namerica", "br_oeurope", "br_asia", "br_ssa", "br_rest")),
      "British citizens arriving from non-EU countries, by region of last residence", w = 14,
      notes = "Each column: British citizens arriving from that region of last residence (ONS table 3.01).")

# --- Panel 3: lower bound and half-samples ------------------------------------
m_base <- fit("log_mig_rate")
m_ctrl <- fit("log_mig_rate", extra = "brit_in_nonEU")
p3 <- list(m_base, m_ctrl, fit("brit_in_nonEU", d = subset(S, year <= 2003)), fit("brit_in_nonEU", d = subset(S, year >= 1995)))
names(p3) <- c("non-EU (Fig 8.1)", "+ Brit control", "Brit 1979-2003", "Brit 1995-2019")
panel(p3, "Non-EU inflows with the British flow as a control; the British equation by half-sample", w = 17,
      notes = c("Col 1: the fiscal specification. Col 2: the same with British-in-from-non-EU added as a regressor;",
                "the projection coefficient here (0.0057) is the lower bound quoted in Section 9.",
                "Cols 3-4: the Brit-in-nonEU equation of Panel 1 estimated on each half of the sample."))
ct <- coeftest(m_ctrl, vcov. = vcovHAC(m_ctrl))
cat(sprintf("British-in-from-non-EU control in the non-EU equation: %.3f (p %s)\n", ct["brit_in_nonEU", 1], pf(ct["brit_in_nonEU", 4])))

# --- IPS sampling error --------------------------------------------------------
w <- subset(ips, year >= 1979 & year <= 2019)
cat("\n===== IPS sampling error, 1979-2019 (published 95% intervals, table 3.01) =====\n")
for (x in list(c("Inflow_NON-EU_brit", "British citizens in from non-EU"), c("Inflow_NON-EU_nonEU", "non-EU citizens in from non-EU"))) {
  est <- w[[x[1]]]; ci <- w[[paste0(x[1], "_ci")]]
  cat(sprintf("%-34s mean %3.0f thousand, CI +/- %4.1f (%2.0f%%); sampling SD of the log %.3f against a series SD of %.3f (%.0f%%)\n",
              x[2], mean(est), mean(ci), 100 * mean(ci / est), mean(ci / 1.96 / est), sd(log(est)), 100 * mean(ci / 1.96 / est) / sd(log(est))))
}
