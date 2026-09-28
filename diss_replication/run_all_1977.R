# >>> 1977 VARIANT (manuscript only, Sam 22 Sep 2026) <<<
# Generated from run_all.R: identical except that the estimation sample starts in
# 1977 (N = 43), the earliest year the lagged log change in debt/GDP allows.
# Outputs carry a _1977 suffix so the canonical 1979 run is never overwritten.
# The thesis stays on 1979-2019 and uses run_all.R.
# run_all.R -- source every script the document sweep needs, in order, each in
# a clean environment, with all console output captured to run_all_output.txt
# in this folder (also echoed to the console). Run from diss_replication/:
#     source("run_all.R")
# then send run_all_output.txt. build_tables.R / verify.R are not included:
# tables.json is already baselined; re-run those separately if the data change.
run_all <- function(scripts = c("replication_1977.R", "appendix_exhibits_1977.R",
                                "robustness_both_specs_1977.R", "mpr_dummies_replication_1977.R",
                                "sample_start_1977.R", "robustness_tables.R", "core_1977_boe.R", "extra_tests.R", "gad_obr_tests.R", "boe_appendix.R"),
                    out = "run_all_1977_output.txt") {
  con <- file(out, open = "wt")
  sink(con, split = TRUE)                     # split = TRUE keeps the console echo
  sink(con, type = "message")                 # warnings/messages too
  on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)   # inside a function: runs at the end
  options(width = 130, scipen = 999)
  cat("run_all.R --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-- wd:", getwd(), "\n")
  if (exists("DEFINED", envir = globalenv())) rm("DEFINED", envir = globalenv())   # variable definitions print once per document run (tables_common.R)
  for (s in scripts) {
    cat("\n\n", strrep("#", 100), "\n##  ", s, "\n", strrep("#", 100), "\n\n", sep = "")
    t0 <- Sys.time()
    assign("SAMPLE", "1977", envir = globalenv())   # core-sample switch for the shared scripts
    ok <- tryCatch({ source(s, local = new.env(), echo = FALSE); TRUE },
                   error = function(e) { cat("\n*** ", s, " FAILED: ", conditionMessage(e), "\n", sep = ""); FALSE })
    cat(sprintf("\n[%s %s in %.0f s]\n", s, if (ok) "completed" else "FAILED",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  cat("\nAll output written to", normalizePath(out), "\n")
  invisible(out)
}
run_all()
