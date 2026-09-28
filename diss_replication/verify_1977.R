# >>> 1977 VARIANT (manuscript only, Sam 22 Sep 2026) <<<
# Generated from verify.R: identical except that the estimation sample starts in
# 1977 (N = 43), the earliest year the lagged log change in debt/GDP allows.
# Outputs carry a _1977 suffix so the canonical 1979 run is never overwritten.
# The thesis stays on 1979-2019 and uses verify.R.
# =============================================================================
# verify.R -- regression test for tables.json.
#
# build_tables.R re-fits four Table 4 models whose formulas are duplicated from
# replication.R. Nothing forces those duplicates to stay in step: edit one file
# and not the other and you get a perfectly valid regression, no error, and a
# wrong number in the paper. This script is the guard against that.
#
# It compares the current tables.json against a stored baseline. Any cell that
# moves is reported and the script stops. If the change is intended -- a new
# specification, corrected data -- refresh the baseline deliberately:
#
#     source("verify.R")            # compare against the baseline
#     verify_accept()               # adopt the current values as the new one
#
# Deliberate is the point. A number should change because you decided it
# should, not because two copies of a formula drifted apart.
#
# NOTE: an earlier version of flatten_num() skipped unnamed JSON arrays and so
# compared only 186 of the 225 numeric values, silently omitting the Table 3
# multipliers and all of Table B2. Because the baseline is a verbatim copy of
# tables.json, those values were always present in both files -- only the
# comparison ignored them -- so the fix took effect without disturbing the
# baseline: the count simply rose to 225.
# =============================================================================

if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("jsonlite is required: install.packages('jsonlite')")

CURRENT  <- "tables_1977.json"
BASELINE <- "tables_expected_1977.json"
TOL      <- 1e-10        # deterministic cells; only real change should show
# Exception: the two bounds-test p-values are computed by response-surface
# simulation and are exactly reproducible only for a given ARDL version (the
# baseline was built under ARDL 0.2.5; the observed spread across 0.2.4/0.2.5
# draws was F 0.0281-0.0303, t 0.0682-0.0715). They are held to TOL_LOOSE --
# the paper's two-decimal precision -- so a package upgrade cannot false-fail
# the strict check that guards the other 223 cells.
LOOSE     <- c("table3.boundsF.p", "table3.boundsT.p")
TOL_LOOSE <- 5e-3

if (!file.exists(CURRENT))
  stop(CURRENT, " not found -- run source('build_tables.R') first")

# --- flatten the nested json to one named vector of numbers ------------------
# Iterate by POSITION, not by names(): fromJSON(simplifyVector = FALSE) returns
# JSON arrays as UNNAMED lists, and `for (nm in names(x))` runs zero times on
# those -- which silently excluded every array-valued entry (the Table 3
# multipliers, standard errors and p-values, and all of Table B2: 39 of the 225
# numeric values) from the comparison. Unnamed elements are keyed by index.
flatten_num <- function(x, prefix = "") {
  out <- c()
  nms <- names(x)
  for (i in seq_along(x)) {
    v   <- x[[i]]
    nm  <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else sprintf("[%d]", i)
    key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
    if (is.list(v)) {
      out <- c(out, flatten_num(v, key))
    } else if (is.numeric(v)) {
      out[key] <- v
    }
  }
  out
}

cur <- flatten_num(jsonlite::fromJSON(CURRENT, simplifyVector = FALSE))

verify_accept <- function() {
  file.copy(CURRENT, BASELINE, overwrite = TRUE)
  message("baseline updated from ", CURRENT, " (", length(cur), " values)")
  invisible(TRUE)
}

if (!file.exists(BASELINE)) {
  verify_accept()
  message("No baseline existed. Current values recorded as the reference.\n",
          "From now on verify.R will report any cell that moves.")
} else {
  # `ref`, not `base`: replication.R once used `base` for a horse-race model
  # (renamed hr_base when the collision was found), and `base` is in any case a
  # package name. The distinct name stays because both scripts share sessions.
  ref <- flatten_num(jsonlite::fromJSON(BASELINE, simplifyVector = FALSE))

  gone  <- setdiff(names(ref), names(cur))
  added <- setdiff(names(cur), names(ref))
  both  <- intersect(names(ref), names(cur))
  moved <- both[abs(cur[both] - ref[both]) > TOL]

  # apply the loose tolerance to the two stochastic p-values (see header)
  loose_in <- intersect(LOOSE, both)
  moved <- setdiff(moved, loose_in)
  moved <- c(moved, loose_in[abs(cur[loose_in] - ref[loose_in]) > TOL_LOOSE])

  cat(sprintf("verify.R: %d values compared against %s\n", length(both), BASELINE))

  if (length(gone))  cat(sprintf("  MISSING now: %s\n", paste(gone,  collapse = ", ")))
  if (length(added)) cat(sprintf("  NEW:         %s\n", paste(added, collapse = ", ")))
  if (length(moved)) {
    cat("  CHANGED:\n")
    for (k in moved)
      cat(sprintf("    %-42s %-18s -> %s\n", k,
                  format(ref[k], digits = 10), format(cur[k], digits = 10)))
  }

  if (!length(gone) && !length(added) && !length(moved)) {
    cat("  no differences -- every table value is unchanged.\n")
  } else {
    stop(sprintf(paste0("tables.json differs from the baseline (%d changed, ",
                        "%d new, %d missing).\nIf this is intended, run ",
                        "verify_accept() to adopt the new values."),
                 length(moved), length(added), length(gone)))
  }
}
