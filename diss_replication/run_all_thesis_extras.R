# run_all_thesis_extras.R -- the Section 9 and Appendix A additions to the THESIS
# (1979-2019) made in September 2026, each in a clean environment, output captured
# to run_all_thesis_extras_output.txt. Run from diss_replication/ AFTER run_all.R:
#     source("run_all_thesis_extras.R")
# spending_check.R      Section 9 'Realised spending'
# determinants_check.R  Section 9 'Other determinants of migration' (set START <- 1979 inside)
# stability_check.R     Section 9 'Stability over time' (+ stability.png)
# permits_check.R       Section 9 'The policy lever' (numbers)
# appendix_permits.R    Appendix A 'Policy lever' panel
# appendix_british.R    Appendix A 'British citizens and uncontrolled flows' panels
# tax_grid.R            Appendix A 18-way tax grid
run_all <- function(scripts = c("spending_check.R", "determinants_check.R", "stability_check.R",
                                "permits_check.R", "appendix_permits.R", "appendix_british.R", "tax_grid.R"),
                    out = "run_all_thesis_extras_output.txt") {
  con <- file(out, open = "wt"); sink(con, split = TRUE); sink(con, type = "message")
  on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)
  options(width = 130, scipen = 999)
  cat("run_all_thesis_extras.R --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-- wd:", getwd(), "\n")
  for (s in scripts) {
    cat("\n\n", strrep("#", 100), "\n##  ", s, "\n", strrep("#", 100), "\n\n", sep = "")
    t0 <- Sys.time()
    ok <- tryCatch({ source(s, local = new.env(), echo = FALSE); TRUE },
                   error = function(e) { cat("\n*** ", s, " FAILED: ", conditionMessage(e), "\n", sep = ""); FALSE })
    cat(sprintf("\n[%s %s in %.0f s]\n", s, if (ok) "completed" else "FAILED", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  cat("\nAll output written to", normalizePath(out), "\n"); invisible(out)
}
run_all()
