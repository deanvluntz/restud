# =============================================================================
# export_dataset.R -- writes the assembled dataset of each sample into the workbook (sheets `data_1964`, `data_1977`) so that
# every script needs only Dissertation_Dataset_Final.xlsx (Sam, 28 Sep 2026: "all the data should be added to the dataset so
# only one is needed"). Re-run after any change to a determinants/*.csv source or to the assembly in data_common.R:
#     source("export_dataset.R")
# The sheets hold every canonical variable BEFORE lagging/logging (data_common.R adds L_ / L_ln_ forms on reading), plus the
# raw source columns they were built from and `splice_logratio`. Sam's own sheets are left untouched; the original sheets are
# re-read after saving and compared cell by cell with what was there before.
# =============================================================================
suppressPackageStartupMessages({library(readxl); library(openxlsx)}); source("tables_common.R")   # lag1(), stars() used by the assembly
WB <- "Dissertation_Dataset_Final.xlsx"; keep <- setdiff(excel_sheets(WB), c("data_1964", "data_1977"))
before <- lapply(setNames(keep, keep), function(s) as.data.frame(read_excel(WB, sheet = s, .name_repair = "minimal")))
wb <- loadWorkbook(WB); options(restud.rebuild = TRUE); on.exit(options(restud.rebuild = FALSE), add = TRUE)   # rebuild from determinants/*.csv while this script runs only
for (s in c("1964", "1977")) { SAMPLE <- s; source("data_common.R", local = TRUE)
  out <- raw[order(raw$year), ]; out <- out[, c("year", setdiff(names(out), "year"))]
  sh <- paste0("data_", s); if (sh %in% names(wb)) removeWorksheet(wb, sh); addWorksheet(wb, sh); writeData(wb, sh, out); freezePane(wb, sh, firstRow = TRUE, firstCol = TRUE)
  cat(sprintf("%s: %d rows (%d-%d), %d columns\n", sh, nrow(out), min(out$year), max(out$year), ncol(out))) }
saveWorkbook(wb, WB, overwrite = TRUE)
after <- lapply(setNames(keep, keep), function(s) as.data.frame(read_excel(WB, sheet = s, .name_repair = "minimal")))
for (s in keep) { ok <- isTRUE(all.equal(before[[s]], after[[s]], check.attributes = FALSE)); cat(sprintf("sheet %-16s unchanged: %s\n", s, ok)); if (!ok) print(all.equal(before[[s]], after[[s]])) }
cat("written", WB, "\n")
