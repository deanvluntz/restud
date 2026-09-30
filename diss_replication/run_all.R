# run_all.R -- source every script the document sweep needs, in order, each in
# a clean environment, with all console output captured to run_all_output.txt
# in this folder (also echoed to the console). Run from diss_replication/:
#     source("run_all.R")
# then send run_all_output.txt. build_tables.R / verify.R are not included:
# tables.json is already baselined; re-run those separately if the data change.
run_all <- function(scripts = c("replication.R", "thesis_replication.R", "appendix_exhibits.R",
                                "robustness_both_specs.R", "mpr_dummies_replication.R"),
                    out = "run_all_output.txt") {
  con <- file(out, open = "wt")
  sink(con, split = TRUE)                     # split = TRUE keeps the console echo
  sink(con, type = "message")                 # warnings/messages too
  on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)   # inside a function: runs at the end
  options(width = 110, scipen = 999)
  cat("run_all.R --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-- wd:", getwd(), "\n")
  for (s in scripts) {
    cat("\n\n", strrep("#", 100), "\n##  ", s, "\n", strrep("#", 100), "\n\n", sep = "")
    t0 <- Sys.time()
    ok <- tryCatch({ source(s, local = new.env(), echo = FALSE); TRUE },
                   error = function(e) { cat("\n*** ", s, " FAILED: ", conditionMessage(e), "\n", sep = ""); FALSE })
    cat(sprintf("\n[%s %s in %.0f s]\n", s, if (ok) "completed" else "FAILED",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  cat("\nAll output written to", normalizePath(out), "\n")
  invisible(out)
}
run_all()
