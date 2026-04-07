#!/usr/bin/env Rscript
# dev/fetch_fred_data.R
# Fetches U.S. Treasury CMT yields from FRED and writes
# inst/extdata/fred_zero_curve.feather + a timestamp file.
#
# Run by GitHub Actions nightly; also usable locally:
#   Rscript dev/fetch_fred_data.R
#
# No API key required — uses FRED's public CSV endpoint.

pkgs <- c("dplyr", "purrr", "arrow")
new  <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org", quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

source("R/fct_fred_data.R")

message("=== FRED fetch started: ", format(Sys.time(), tz = "UTC"), " UTC ===")

tryCatch({
  df <- fetch_fred_zero_curve(from = "2000-01-01")

  if (nrow(df) == 0L) stop("fetch_fred_zero_curve returned 0 rows.")

  out_dir <- "inst/extdata"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  arrow::write_feather(df, file.path(out_dir, "fred_zero_curve.feather"))
  writeLines(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    file.path(out_dir, "fred_zero_curve_last_updated.txt")
  )

  message("Saved ", nrow(df), " rows | ",
          length(unique(df$date)), " dates | ",
          length(unique(df$maturity)), " maturities")
  message("=== Done ===")

}, error = function(e) {
  message("FATAL: ", conditionMessage(e))
  quit(status = 1L)
})
