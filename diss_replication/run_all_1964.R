# run_all_1964.R -- every script behind the 1964-2019 manuscript and the combined
# (core 1977-2019 + extended 1965-2019) manuscript, each in a clean environment,
# output captured to run_all_1964_output.txt. Run from diss_replication/:
#     source("run_all_1964.R")
# Order (Sam, 28 Sep 2026): the fiscal specification with the basis step; the robustness tables; the fiscal model without the
# projection; further economic and fiscal controls (parts 1-3); appendix tables; the private tests; the exploratory int_tme-as-primary run; the three parallel specification runs (parallel_spec.R engine).
# Prerequisites: sheet `1964` of Dissertation_Dataset_Final.xlsx (extension + extras),
# determinants/extension_1962_2019_extras.csv, tables_1977.json (from run_all_1977.R
# + build_tables_1977.R) for the core-sample columns of the combined document.
# Afterwards copy tables_1964.json to ../manuscript_1964/ and run
#     node build_docx_1964.js        (1964-2019 document)
#     node build_docx_combined.js    (combined document)
run_all <- function(scripts = c("extension_1964.R", "robustness_tables.R", "without_projection.R", "controls_tables.R", "manuscript_1964.R", "private_tests.R", "primary_int_tme.R", "parallel_gross_vac.R", "parallel_gross.R", "parallel_rate_vac.R"),
                    out = "run_all_1964_output.txt") {
  con <- file(out, open = "wt"); sink(con, split = TRUE); sink(con, type = "message")
  on.exit({ sink(type = "message"); sink(); close(con) }, add = TRUE)
  options(width = 130, scipen = 999)
  cat("run_all_1964.R --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-- wd:", getwd(), "\n")
  if (exists("DEFINED", envir = globalenv())) rm("DEFINED", envir = globalenv())   # variable definitions print once per document run (tables_common.R)
  for (s in scripts) {
    cat("\n\n", strrep("#", 100), "\n##  ", s, "\n", strrep("#", 100), "\n\n", sep = "")
    t0 <- Sys.time(); assign("SAMPLE", "1964", envir = globalenv())   # every script here runs on the EXTENDED sample, whatever SAMPLE was in the session
    ok <- tryCatch({ source(s, local = new.env(), echo = FALSE); TRUE },
                   error = function(e) { cat("\n*** ", s, " FAILED: ", conditionMessage(e), "\n", sep = ""); FALSE })
    cat(sprintf("\n[%s %s in %.0f s]\n", s, if (ok) "completed" else "FAILED", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  cat("\nAll output written to", normalizePath(out), "\n"); invisible(out)
}
run_all()
