# =============================================================================
# appendix_permits.R -- Appendix A panel: the policy lever (work permits)
# (Sam, 25 Sep 2026). Same printing conventions as appendix_exhibits.R (HAC
# p-values, classical nested F against the model without its projection term).
#   Dependent variable: work permits and first permissions (Bauer and Salt, 2020;
#   column `permits` in the workbook).
#     col 1  permits (k)      gross permits, thousands            <- the reported form
#     col 2  permits/WAP      permits per 1,000 working-age persons (t-1)
#     col 3  log permits/WAP  natural log of col 2 (the log-rate form used for inflows)
#     col 4  zero-mig         col 1 with the zero-net-migration projection
#     col 5  total DR         col 1 with the total dependency ratio in place of the OADR
#   Sample 1979-2008: the last year of the work-permit system before the
#   points-based system.
# Run from diss_replication/:   source("appendix_permits.R")
# =============================================================================
suppressPackageStartupMessages({library(lmtest); library(sandwich); library(car); library(readxl)})
raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
drc <- read.csv("dr_corrected.csv")
lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, delta_oadr_10 = delta_oadr_10, delta_zm_10 = delta_zm_10,
                           tax_workpop = tax_workpop, unem = unem, ln_debt_gdp = ln_debt_gdp,
                           permits = permits, workpop = workpop))
df$delta_dr_10 <- drc$delta_dr_10[match(df$year, drc$year)]
df$permit_rate <- 1000 * df$permits / lag1(df$workpop)      # permits per 1,000 WAP (t-1)
df$log_permit_rate <- log(df$permit_rate)
df$permits_k <- df$permits / 1000                          # gross permits, thousands
for (v in c("permit_rate", "log_permit_rate", "permits_k", "tax_workpop", "unem", "ln_debt_gdp")) df[[paste0("L_", v)]] <- lag1(df[[v]])
reg_df <- subset(df, year >= 1979 & year <= 2008)
stopifnot(nrow(reg_df) == 30, !anyNA(reg_df$permit_rate))
cat("sample:", min(reg_df$year), "-", max(reg_df$year), " N =", nrow(reg_df), "\n")

stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
pf <- function(p) if (is.na(p)) "--" else if (p < 0.001) "<.001" else sprintf("%.3f", p)
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
M  <- function(dv, lag, proj) lm(reformulate(c(lag, proj, CT), dv), data = reg_df)

models <- list("permits (k)"     = M("permits_k",       "L_permits_k",       "delta_oadr_10"),
               "permits/WAP"     = M("permit_rate",     "L_permit_rate",     "delta_oadr_10"),
               "log permits/WAP" = M("log_permit_rate", "L_log_permit_rate", "delta_oadr_10"),
               "zero-mig"        = M("permits_k",       "L_permits_k",       "delta_zm_10"),
               "total DR"        = M("permits_k",       "L_permits_k",       "delta_dr_10"))
focal <- c("delta_oadr_10", "delta_zm_10", "delta_dr_10", "L_permits_k",
           "L_permit_rate", "L_log_permit_rate", "L_tax_workpop", "L_ln_debt_gdp", "L_unem")
w <- 16; lw <- 18
cat("\n===== Policy lever: work permits and first permissions, 1979-2008 =====\n\n")
cts <- lapply(models, function(m) coeftest(m, vcov. = vcovHAC(m)))
hdr <- sprintf("%-*s%s", lw, "", paste(sprintf("%*s", w, names(models)), collapse = "")); rule <- strrep("-", nchar(hdr))
cat(hdr, "\n", rule, "\n", sep = "")
for (v in focal) {
  if (!any(vapply(cts, function(ct) v %in% rownames(ct), logical(1)))) next
  cat(sprintf("%-*s%s\n", lw, v, paste(vapply(cts, function(ct) if (v %in% rownames(ct))
      sprintf("%*s", w, sprintf("%.4f%s", ct[v, 1], stars(ct[v, 4]))) else sprintf("%*s", w, "--"), character(1)), collapse = "")))
  cat(sprintf("%-*s%s\n", lw, "", paste(vapply(cts, function(ct) if (v %in% rownames(ct))
      sprintf("%*s", w, paste0("(", pf(ct[v, 4]), ")")) else sprintf("%*s", w, ""), character(1)), collapse = "")))
}
cat(rule, "\n", sep = "")
stat <- function(lab, f) cat(sprintf("%-*s%s\n", lw, lab, paste(vapply(models, function(m) sprintf("%*s", w, f(m)), character(1)), collapse = "")))
stat("N",       function(m) sprintf("%d", nobs(m)))
stat("adj. R2", function(m) sprintf("%.4f", summary(m)$adj.r.squared))
stat("AIC",     function(m) sprintf("%.2f", AIC(m)))
stat("RESET p", function(m) pf(resettest(m)$p.value))
stat("BG(1) p", function(m) pf(bgtest(m, order = 1)$p.value))
stat("BP p",    function(m) pf(bptest(m)$p.value))
stat("JB p",    function(m) pf(tseries::jarque.bera.test(residuals(m))$p.value))
stat("max VIF", function(m) sprintf("%.1f", max(vif(m))))
PROJ <- c("delta_oadr_10", "delta_zm_10", "delta_dr_10")
F <- P <- D <- character(length(models))
for (k in seq_along(models)) {
  term <- intersect(PROJ, names(coef(models[[k]]))); r <- update(models[[k]], as.formula(paste(". ~ . -", term)))
  a <- anova(r, models[[k]]); F[k] <- sprintf("%.2f", a$F[2]); P[k] <- paste0("(", pf(a$`Pr(>F)`[2]), ")")
  D[k] <- sprintf("%+.2f", AIC(models[[k]]) - AIC(r))
}
for (x in list(c("F vs no proj", F), c("  p", P), c("  dAIC", D)))
  cat(sprintf("%-*s%s\n", lw, x[1], paste(vapply(x[-1], function(s) sprintf("%*s", w, s), character(1)), collapse = "")))
cat(rule, "\n", sep = "")
cat("HAC (vcovHAC) p-values in parentheses. *** p<0.01, ** p<0.05, * p<0.10.\n",
    "F rows: classical nested F (iid errors). Permits: Bauer and Salt (2020), Figure 1.\n",
    "permits (k): gross permits, thousands; permits/WAP: per 1,000 working-age persons (t-1).\n",
    "zero-mig and total DR use permits (k). Each column's lagged dependent variable is its own series.\n", sep = "")
cat(sprintf("\npermits (k) coefficient: about %.0f permits per projected point of ten-year OADR rise;\n",
            1000 * coef(models$`permits (k)`)["delta_oadr_10"]))
cat(sprintf("permits/WAP coefficient x mid-sample working-age population (%.1f m): about %.0f permits per point.\n",
            mean(reg_df$workpop) / 1e6, coef(models$`permits/WAP`)["delta_oadr_10"] * mean(reg_df$workpop) / 1000))
