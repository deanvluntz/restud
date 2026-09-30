# =============================================================================
# composition_reason.R -- IPS 3.08 inflows by main reason for migration (1977-2019) and the OBR historical output gap (Sam, 29 Sep 2026).
#   A  non-EU inflows by reason: the projection's coefficient on each reason's log inflow (core specification controls) -- the
#      composition test: which route responds?
#   B  British inflows by reason: work / study / not-work-or-study (consistent from 1977), going home (1980-) and joining (1982-)
#   C  the output gap as a cyclical control: British equation (published and core controls) and the main equation, 1977-2010; the
#      extended sample runs 1973-2010 for the main equation only (3.08 begins in 1977).
# Core sample only for A and B (3.08 begins 1977). RStudio: working directory = diss_replication/.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
source("tables_common.R")
load_sample <- function(s) { SAMPLE <<- s; source("data_common.R", local = TRUE); df$ln_vac_unem <- log(df$vac_unem); df$ln_int_tme <- log(df$int_tme)
  for (v in c("noneu", "brit")) for (r in c("work", "study", "join", "other", "all", "nonwork", "home")) { k <- paste0(v, "_", r, "_k"); if (k %in% names(df)) { x <- df[[k]]; x[!is.na(x) & x <= 0] <- NA; df[[paste0("ln_", v, "_", r)]] <- log(1000 * x); df[[paste0("L_ln_", v, "_", r)]] <- lag1(df[[paste0("ln_", v, "_", r)]]) } }
  list(df = df, Y0 = Y0, LAB = LAB) }
SMP <- list(extended = load_sample("1964"), core = load_sample("1977"))
CORE <- c("L_tax_workpop", "L_ln_debt_gdp", "L_ln_vac_unem", "L_ln_int_tme"); PUB <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem")
S <- subset(SMP$core$df, year >= 1977 & year <= 2019)
# --- A non-EU by reason -------------------------------------------------------------------------------------------------------------------
cat("\n##### A. Non-EU inflows by main reason for migration, core sample 1977-2019: log inflow on its own lag, the projection and the core controls #####\n")
for (ctl in list(core = CORE, published = PUB)) { p <- list(); for (r in c("all", "work", "study", "join", "other")) { dv <- paste0("ln_noneu_", r); p[[r]] <- fit(S, c(paste0("L_", dv), "delta_oadr_10", ctl), dv) }
  panel(p, paste0("A. Non-EU inflows by reason (", if (identical(ctl, CORE)) "core" else "published", " controls)"), c("delta_oadr_10", ctl), w = 13, lw = 16, nested_F = TRUE,
        "Each column: the log inflow of non-EU citizens giving the reason named, on its own lag, the projection and the controls; accompany/join and other are coded throughout for non-EU citizens.")
  for (r in names(p)) if (!is.null(p[[r]])) { ct <- coeftest(p[[r]], vcov. = vcovHAC(p[[r]])); cat(sprintf("  %-6s lagged DV %.2f; projection %.4f (p %.3f); N %d\n", r, coef(p[[r]])[2], ct["delta_oadr_10", 1], ct["delta_oadr_10", 4], nobs(p[[r]]))) } }
# --- B British by reason ---------------------------------------------------------------------------------------------------------------------
cat("\n\n##### B. British inflows by main reason for migration, core sample 1977-2019 #####\n")
for (ctl in list(core = CORE, published = PUB)) { p <- list(); for (r in c("all", "work", "study", "nonwork", "home", "join", "other")) { dv <- paste0("ln_brit_", r); p[[r]] <- fit(S, c(paste0("L_", dv), "delta_oadr_10", ctl), dv) }
  panel(p, paste0("B. British inflows by reason (", if (identical(ctl, CORE)) "core" else "published", " controls)"), c("delta_oadr_10", ctl), w = 13, lw = 16, nested_F = TRUE,
        "British 'going home to live' is coded from 1980 and 'accompany / join' from 1982 (earlier years fold them into 'other'); nonwork = all minus work minus study is consistent throughout.")
  for (r in names(p)) if (!is.null(p[[r]])) { ct <- coeftest(p[[r]], vcov. = vcovHAC(p[[r]])); cat(sprintf("  %-8s lagged DV %.2f; projection %.4f (p %.3f); N %d\n", r, coef(p[[r]])[2], ct["delta_oadr_10", 1], ct["delta_oadr_10", 4], nobs(p[[r]]))) } }
# --- C output gap -------------------------------------------------------------------------------------------------------------------------------
cat("\n\n##### C. The OBR historical output gap as a cyclical control (digitised from Working Paper No. 1, 1972-2010) #####\n")
for (k in names(SMP)) { d <- subset(SMP[[k]]$df, year >= SMP[[k]]$Y0 & year <= 2010); cat("\n===", SMP[[k]]$LAB, "2010 ===\n")
  if (k == "core") for (ctl in list(published = PUB, core = CORE)) { BR <- c("L_brit_rate", "delta_oadr_10", ctl)
    p <- list("brit_rate" = fit(d, BR, "brit_rate"), "+L_gap_pca" = fit(d, c(BR, "L_gap_pca"), "brit_rate"), "+gap_pca" = fit(d, c(BR, "gap_pca"), "brit_rate"), "+L_gap_agg" = fit(d, c(BR, "L_gap_agg"), "brit_rate"), "gap_pca for unem" = if (identical(ctl, PUB)) fit(d, c(setdiff(BR, "L_unem"), "L_gap_pca"), "brit_rate") else NULL,
              "brit_desc" = fit(d, c("L_brit_rate", "delta_oadr_10", ctl, "L_gap_pca"), "brit_desc_noneu"))
    panel(p, paste0("C. British inflows and the output gap, 1977-2010 (", if (identical(ctl, PUB)) "published" else "core", " controls)"), c("delta_oadr_10", "L_gap_pca", "gap_pca", "L_gap_agg", ctl, "L_brit_rate"), w = 14, lw = 16, nested_F = TRUE,
          "brit_rate: British-citizen inflows, all origins (LTIM 2.00) per 1,000 working-age persons; brit_desc: British citizens from outside the EU not born in the UK (IPS 3.01 minus 3.18), with the lagged gap.") }
  G <- c("L_mig_gross", "delta_oadr_10", CORE); p <- list("main (to 2010)" = fit(d, G, "mig_gross"), "+L_gap_pca" = fit(d, c(G, "L_gap_pca"), "mig_gross"), "+gap_pca" = fit(d, c(G, "gap_pca"), "mig_gross"), "+L_gap_agg" = fit(d, c(G, "L_gap_agg"), "mig_gross"), "gap_pca for ln_vac_unem" = fit(d, c(setdiff(G, "L_ln_vac_unem"), "L_gap_pca"), "mig_gross"))
  panel(p, paste0("C. The main equation and the output gap (", SMP[[k]]$LAB, "2010)"), c("delta_oadr_10", "L_gap_pca", "gap_pca", "L_gap_agg", CORE, "L_mig_gross"), w = 14, lw = 16, nested_F = TRUE, "Core specification on the years the digitised gap covers.") }
