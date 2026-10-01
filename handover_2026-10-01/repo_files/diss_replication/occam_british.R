# =============================================================================
# occam_british.R -- Madigan and Raftery (1994, JASA) 'Occam's window' applied to the British-inflow determinants (Sam, 1 Oct 2026)
# M&R: instead of conditioning on one selected model, average over the models that predict the data well. Two rules: (i) discard any model
# whose posterior probability is below 1/c of the best (c = 20, their analogue of the .05 cut-off; Jeffreys suggests 10-100); (ii) Occam's
# razor: discard any model that has a simpler sub-model with a higher posterior probability. Posterior model probabilities here use the BIC
# approximation with equal prior model probabilities (exact Bayes factors in M&R are for contingency tables; the BIC route is the standard one
# for linear regression), so rule (i) keeps models within 2 ln 20 = 6.0 BIC points of the best.
# Model space: the outcome's own lag and the core terms (tax per WAP, log change in debt/GDP, log V/U) always in; the projection and MPR-type
# determinants in or out: relinc_dev, relinc_rich, youth, UK unemployment, destination unemployment, UK Gini, Gini ratio UK/(US, Canada), return
# pool (10-year British emigration). 2^9 = 512 models per outcome, 1977-2019 (the birthplace split starts in 1975).
# Reported: posterior inclusion probabilities (all 512 models, and within Occam's window), the models in the window, and the model-averaged
# projection coefficient. Outcomes: non-UK-born British from non-EU, UK-born British from non-EU, British all origins (LTIM), and the main
# (controlled) non-EU inflows.
# RStudio: working directory = diss_replication/, then source() this file.
# =============================================================================
if (!file.exists("tables_common.R")) stop("set the working directory to diss_replication/")
invisible(capture.output(source("british_mpr.R")))                     # builds S with every determinant (Parts 1 and 2)
CAND <- c("delta_oadr_10", "L_relinc_dev", "L_relinc_rich", "L_youth", "L_unem", "L_unem_dest", "L_gini_uk", "L_gini_ratio", "L_pool10")
FIXED <- setdiff(X, "delta_oadr_10"); C_OCC <- 20
occam <- function(o, lab) { d <- subset(S, year >= 1977 & year <= 2019); d <- d[complete.cases(d[, c(o, paste0("L_", o), FIXED, CAND)]), ]
  grid <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), length(CAND)))); colnames(grid) <- CAND
  res <- lapply(seq_len(nrow(grid)), function(i) { xs <- c(paste0("L_", o), FIXED, CAND[grid[i, ]]); m <- lm(reformulate(xs, o), data = d)
    list(bic = BIC(m), b = if (grid[i, 1]) coef(m)[["delta_oadr_10"]] else 0) })
  bic <- vapply(res, `[[`, 0, "bic"); b <- vapply(res, `[[`, 0, "b"); w <- exp(-(bic - min(bic)) / 2); post <- w / sum(w)
  inA1 <- post >= max(post) / C_OCC                                                                   # rule (i)
  is_sub <- function(i, j) all(grid[j, ] <= grid[i, ]) && any(grid[j, ] < grid[i, ])                     # j is a sub-model of i
  inB <- vapply(which(inA1), function(i) any(vapply(which(inA1), function(j) is_sub(i, j) && post[j] > post[i], logical(1))), logical(1))
  A <- which(inA1)[!inB]; pa <- post[A] / sum(post[A])
  cat(sprintf("\n-- %s (N %d; 512 models)\n", lab, nrow(d)))
  cat(sprintf("   %-16s %10s %16s\n", "term", "PIP (all)", "PIP (Occam win.)"))
  for (k in CAND) cat(sprintf("   %-16s %10.2f %16.2f\n", sub("^L_", "", k), sum(post[grid[, k]]), sum(pa[grid[A, k]])))
  cat(sprintf("   model-averaged projection: all models %.4f; Occam's window %.4f; window = %d model(s) after rule (i) %d:\n", sum(post * b), sum(pa * b[A]), length(A), sum(inA1)))
  for (i in A[order(-pa)]) cat(sprintf("     p %.2f  dBIC %5.2f  %s\n", pa[which(A == i)], bic[i] - min(bic), paste(sub("^L_", "", CAND[grid[i, ]]), collapse = " + "))) }
for (o in list(c("brit_desc_gross", "non-UK-born British from non-EU"), c("ukborn_gross", "UK-born British from non-EU"), c("brit_gross", "British, all origins (LTIM)"),
               c("mig_gross", "MAIN: non-EU inflows (controlled)"))) occam(o[1], o[2])
cat("\nDone.\n")
