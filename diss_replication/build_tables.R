# =============================================================================
# build_tables.R -- emit the numbers behind Tables 2, 3, 4 and B2 as JSON.
#
# Purpose: the document builder currently carries every table cell as a
# hand-typed string. That is where transcription errors enter (a constant
# reading 0.355 where R returned 0.371; a column mixing OLS and HAC errors).
# This script computes the same quantities from the fitted models and writes
# them to tables.json, so the builder can format rather than retype.
#
# Usage, from the replication folder: source("build_tables.R")
#     
#
# It sources replication.R with output suppressed to obtain reg_df and the
# models, refits the few Table 4 columns replication.R does not assign to
# names, and writes tables.json. Nothing in replication.R is modified.
#
# Sourcing runs the FULL pipeline, bootstraps and Monte Carlo included
# (about two minutes on the author's machine; far longer in constrained
# sandboxes -- do not source this file from a tool with a short timeout).
# The cost is deliberate: the models are harvested from exactly the published
# pipeline, with no option-gated fast path that could silently drift from it.
#
# The formulas below MUST match replication.R. They are duplicated here only
# for the models replication.R discards. Nothing forces the two copies to stay
# in step, so verify.R compares tables.json against a stored baseline and halts
# on any cell that moves; run it after this script.
#
# Sourcing replication.R also regenerates figure1.png, figure2.png and
# zero_migration_deltas_R.csv as a side effect.
# =============================================================================

if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("jsonlite is required: install.packages('jsonlite')")

# --- run the replication quietly to get reg_df and the named models ----------
message("running replication.R (output suppressed) ...")
# A message sink needs an OPEN CONNECTION, not a path; and both sinks must be
# unwound even if replication.R stops, or the console stays silent afterwards.
.con <- file(tempfile(), open = "wt")
.warns <- character(0)
sink(.con); sink(.con, type = "message")
.ok <- tryCatch(
  withCallingHandlers(
    { source("replication.R", local = FALSE); TRUE },
    warning = function(w) {
      .warns <<- c(.warns, conditionMessage(w)); invokeRestart("muffleWarning")
    }),
  error = function(e) conditionMessage(e))
sink(type = "message"); sink(); close(.con)
if (!isTRUE(.ok)) stop("replication.R did not complete: ", .ok)
# Warnings are DEFERRED to the message stream, which the sink above discards.
# replication.R signals a failed Appendix A tolerance check with warning(), not
# stop(); without this, that failure would vanish and tables.json would be
# written from a reconstruction that did not reconcile. The script runs clean,
# so any warning at all is worth halting on.
# tseries::adf.test / kpss.test warn "p-value greater/smaller than printed
# p-value" whenever a statistic falls outside the tabled range; the Appendix
# B1 classification reports those as >0.10 / <0.01 by design, so they are
# benign and are filtered here. Anything else still halts.
.warns <- .warns[!grepl("than printed p-value", .warns)]
if (length(.warns))
  stop("replication.R emitted ", length(.warns), " warning(s) that the output ",
       "sink would otherwise have hidden:\n  ", paste(.warns, collapse = "\n  "))

stopifnot(exists("reg_df"), nrow(reg_df) == 41,
          exists("tax_spec"), exists("trend_spec"), exists("m_ardl"))
message("  reg_df: ", nrow(reg_df), " rows; models in scope")

# --- one extractor, used for every column ------------------------------------
hac <- function(m) lmtest::coeftest(m, vcov. = sandwich::vcovHAC(m))

grab <- function(m) {
  ct <- hac(m)
  nm <- rownames(ct)
  list(
    coef   = as.list(stats::setNames(ct[, 1], nm)),
    se     = as.list(stats::setNames(ct[, 2], nm)),
    p      = as.list(stats::setNames(ct[, 4], nm)),
    n      = unname(stats::nobs(m)),
    adjr2  = summary(m)$adj.r.squared,
    # bg2 is the PUBLISHED diagnostic: the manuscript's only Breusch-Godfrey
    # table cell is "Breusch-Godfrey (2), p" (bgtest order = 2, a joint test of
    # orders up to 2). run_model()'s console BG(1) line is console-only and is
    # never typeset, so it is deliberately not carried in tables.json.
    bg2    = lmtest::bgtest(m, order = 2)$p.value,
    bp     = lmtest::bptest(m)$p.value,
    reset  = lmtest::resettest(m)$p.value,
    jb     = tseries::jarque.bera.test(stats::residuals(m))$p.value,
    adf    = suppressWarnings(tseries::adf.test(stats::residuals(m))$p.value),
    maxvif = tryCatch(max(car::vif(m)), error = function(e) NA_real_)
  )
}

# --- Table 4 columns that replication.R runs without assigning ---------------
t4_dr    <- lm(log_mig_rate ~ L_log_mig_rate + trend + delta_dr_10 +
                 L_ln_debt_gdp + L_unem, data = reg_df)
t4_eu    <- lm(eu_log_mig_rate ~ L_eu_log_mig_rate + trend + delta_oadr_10 +
                 L_ln_debt_gdp + L_unem, data = reg_df)
t4_gross <- lm(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
                 trend + L_ln_debt_gdp, data = reg_df)
t4_level <- lm(log_mig_rate ~ L_log_mig_rate + trend + level_oadr_10 +
                 L_unem + L_ln_debt_gdp, data = reg_df)

for (m in list(t4_dr, t4_eu, t4_gross, t4_level))
  stopifnot(stats::nobs(m) == 41)          # same guard as run_model(n_expect)

# --- Table 4 in the FISCAL specification (the manuscript's preferred model) ---
# Same five columns with the linear trend replaced by lagged tax per working-age
# person, plus a sixth: the labour-tax variant (income tax + NICs per working-age
# person, from fiscal_block_HPFD.csv). The trend set above is kept as
# table4_trend for the online appendix.
t4f_dr    <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + delta_dr_10 +
                  L_ln_debt_gdp + L_unem, data = reg_df)
t4f_eu    <- lm(eu_log_mig_rate ~ L_eu_log_mig_rate + L_tax_workpop + delta_oadr_10 +
                  L_ln_debt_gdp + L_unem, data = reg_df)
t4f_gross <- lm(alt_log_mig_rate ~ L_alt_log_mig_rate + delta_oadr_10 + L_unem +
                  L_tax_workpop + L_ln_debt_gdp, data = reg_df)
t4f_level <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_workpop + level_oadr_10 +
                  L_unem + L_ln_debt_gdp, data = reg_df)
t4f_lab <- NULL
if (file.exists("fiscal_block_HPFD.csv")) {
  fb <- read.csv("fiscal_block_HPFD.csv"); fb <- fb[order(fb$year), ]
  reg_df$L_tax_lab_pw <- fb$tax_lab_pw[match(reg_df$year - 1, fb$year)]
  stopifnot(!anyNA(reg_df$L_tax_lab_pw))
  t4f_lab <- lm(log_mig_rate ~ L_log_mig_rate + L_tax_lab_pw + delta_oadr_10 +
                  L_ln_debt_gdp + L_unem, data = reg_df)
} else message("  fiscal_block_HPFD.csv not found: Table 4 labour-tax column omitted")
for (m in c(list(t4f_dr, t4f_eu, t4f_gross, t4f_level), if (is.null(t4f_lab)) list() else list(t4f_lab)))
  stopifnot(stats::nobs(m) == 41)

# --- Online Appendix Tables B4/B5: alternative measures of the tax burden -----
# Read from the workbook (single source); skipped, with a message, on a workbook
# that predates the rebuilt fiscal block. Each model replaces L_tax_workpop in
# the Table 2 specification with one alternative measure, lagged one year.
TAXM <- c("tax_workpop", "lab_tax_workpop", "hh_tax_workpop",
          "tax_totpop", "lab_tax_totpop", "hh_tax_totpop",
          "tax_gdp", "hh_tax_gdp", "lab_tax_comp")     # hh_tax_gdp = lab_tax_gdp + vat_gdp
tableB4 <- NULL
# hh_tax_gdp is not a workbook column: build it from the same OBR HPFD block
# (income tax + NICs + VAT, per cent of GDP), exactly as appendix_exhibits.R does.
if (!"hh_tax_gdp" %in% names(raw) && file.exists("fiscal_block_HPFD.csv")) {
  fb_h <- read.csv("fiscal_block_HPFD.csv")
  raw$hh_tax_gdp <- (fb_h$tax_lab_gdp + fb_h$vat_gdp)[match(raw$year, fb_h$year)]
  if ("lab_tax_gdp" %in% names(raw))     # same source: the labour share must agree
    stopifnot(max(abs(raw$lab_tax_gdp - fb_h$tax_lab_gdp[match(raw$year, fb_h$year)]), na.rm = TRUE) < 1e-3)
}
if (all(TAXM %in% names(raw))) {
  rawo <- raw[order(raw$year), ]
  taxdf <- reg_df
  for (v in TAXM) {
    x <- rawo[[v]]; if (v == "lab_tax_comp") x <- 100 * x        # per cent
    taxdf[[paste0("L_", v)]] <- x[match(taxdf$year - 1, rawo$year)]
  }
  stopifnot(!anyNA(taxdf[paste0("L_", TAXM)]))
  tax_model <- function(v) {
    m <- lm(reformulate(c("L_log_mig_rate", paste0("L_", v), "delta_oadr_10", "L_ln_debt_gdp", "L_unem"),
                        "log_mig_rate"), data = taxdf)
    stopifnot(stats::nobs(m) == 41); grab(m)
  }
  tableB4 <- setNames(lapply(TAXM, tax_model), TAXM)
} else message("  tax-measure columns missing: ", paste(setdiff(TAXM, names(raw)), collapse = ", "), " -- Tables B4/B5 omitted")

# --- Table 3: long-run multipliers and bounds tests --------------------------
lr <- ARDL::multipliers(m_ardl, type = "lr")
set.seed(20260802); bf <- ARDL::bounds_f_test(m_ardl, case = 3, exact = TRUE)
set.seed(20260802); bt <- ARDL::bounds_t_test(m_ardl, case = 3, exact = TRUE)

# --- Appendix Table B2: three inference methods on the fiscal specification --
ols <- lmtest::coeftest(tax_spec)
h   <- hac(tax_spec)
nw  <- lmtest::coeftest(tax_spec, vcov. = sandwich::NeweyWest(
         tax_spec, lag = 1, prewhite = FALSE, adjust = TRUE))

out <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  R         = paste(R.version$major, R.version$minor, sep = "."),
  table2 = list(fiscal = grab(tax_spec), trend = grab(trend_spec)),
  table3 = list(
    terms = as.character(lr$Term),
    est   = lr$Estimate,
    se    = lr$`Std. Error`,
    p     = lr$`Pr(>|t|)`,
    boundsF = list(stat = unname(bf$statistic), p = unname(bf$p.value)),
    boundsT = list(stat = unname(bt$statistic), p = unname(bt$p.value))
  ),
  table4 = list(                            # fiscal specification (typeset as Table 4)
    dr    = grab(t4f_dr),
    eu    = grab(t4f_eu),
    gross = grab(t4f_gross),
    level = grab(t4f_level),
    zm    = grab(ardl_zm),                  # zero-migration, fiscal spec
    lab   = if (is.null(t4f_lab)) NULL else grab(t4f_lab),
    spec  = "fiscal"
  ),
  table4_trend = list(                      # trend specification (online appendix)
    dr    = grab(t4_dr),
    eu    = grab(t4_eu),
    gross = grab(t4_gross),
    level = grab(t4_level),
    zm    = grab(ardl_zm2),
    spec  = "trend"
  ),
  tableB2 = list(
    terms = rownames(ols),
    coef  = unname(ols[, 1]),
    ols   = unname(ols[, 2]),
    hac   = unname(h[, 2]),
    nw    = unname(nw[, 2])
  ),
  tableB4 = tableB4                         # NULL on a workbook without the measures
)

jsonlite::write_json(out, "tables.json", auto_unbox = TRUE, digits = 12,
                     pretty = TRUE)

message("\nwrote tables.json")
message("  Table 2 fiscal, OADR : ",
        signif(out$table2$fiscal$coef$delta_oadr_10, 6), "  se ",
        signif(out$table2$fiscal$se$delta_oadr_10, 6))
message("  Table 3 bounds F     : ", signif(out$table3$boundsF$stat, 6),
        "  p ", signif(out$table3$boundsF$p, 6))
message("  Table 4 zero-mig     : ",
        signif(out$table4$zm$coef$delta_zm_10, 6), "  se ",
        signif(out$table4$zm$se$delta_zm_10, 6))
message("\nNext: source('verify.R') to compare against the stored baseline.")
