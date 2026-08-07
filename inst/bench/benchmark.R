# Scalability benchmark for datagrade.
#
# Anchor data set: ggplot2::diamonds -- 53,940 rows x 10 columns, shipped under
# the MIT licence with ggplot2 itself, and one of the most widely recognised
# example data sets in R. Larger sizes are that same real data replicated with
# jitter on the numeric columns, so the column count, the type mix and the
# correlation structure stay realistic while the row count grows.
#
# Run:  Rscript inst/bench/benchmark.R
# Writes: inst/bench/results.csv

suppressPackageStartupMessages({
  library(datagrade)
  library(ggplot2)
})

set.seed(1)

grow <- function(base, target_rows) {
  if (target_rows <= nrow(base)) return(base[seq_len(target_rows), , drop = FALSE])
  reps <- ceiling(target_rows / nrow(base))
  out <- base[rep(seq_len(nrow(base)), reps)[seq_len(target_rows)], , drop = FALSE]
  # Jitter the numeric columns so the replicas are not literal copies, which
  # would make duplicate detection and the correlation structure degenerate.
  for (col in names(out)[vapply(out, is.numeric, logical(1))]) {
    out[[col]] <- out[[col]] * stats::rnorm(nrow(out), mean = 1, sd = 0.01)
  }
  row.names(out) <- NULL
  out
}

timed <- function(expr) {
  gc(verbose = FALSE)
  t <- system.time(value <- force(expr))
  list(value = value, seconds = unname(t[["elapsed"]]),
       peak_mb = round(sum(gc(verbose = FALSE)[, "used"] *
                           c(8, 56)[seq_len(2)]) / 1e6, 1))
}

base <- as.data.frame(ggplot2::diamonds)
sizes <- c(5.394e4, 1e5, 5e5, 1e6, 5e6, 1e7)

rows <- lapply(sizes, function(n) {
  n <- as.integer(n)
  cat(sprintf("--- %s rows ---\n", format(n, big.mark = ",")))
  data <- grow(base, n)
  size_mb <- round(as.numeric(utils::object.size(data)) / 1e6, 1)

  full <- timed(dg_assess(data, verbose = FALSE))
  fast <- timed(dg_assess(data, verbose = FALSE, check_duplicates = FALSE))
  figs <- timed(dg_plots(full$value))

  report <- full$value
  cat(sprintf("    full %.2fs | no-dup %.2fs | plots %.2fs | overall %.1f\n",
              full$seconds, fast$seconds, figs$seconds, report$overall))

  data.frame(
    rows = n,
    columns = ncol(data),
    input_mb = size_mb,
    seconds_full = round(full$seconds, 2),
    seconds_no_duplicates = round(fast$seconds, 2),
    seconds_plots = round(figs$seconds, 2),
    us_per_row = round(fast$seconds / n * 1e6, 3),
    report_kb = round(as.numeric(utils::object.size(report)) / 1e3, 1),
    completeness = round(report$scores[["completeness"]], 2),
    accuracy = round(report$scores[["accuracy"]], 2),
    consistency = round(report$scores[["consistency"]], 2),
    overall = round(report$overall, 2),
    stringsAsFactors = FALSE)
})

results <- do.call(rbind, rows)
out <- file.path("inst", "bench", "results.csv")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
write.csv(results, out, row.names = FALSE)

cat("\n")
print(results, row.names = FALSE)
cat(sprintf("\nR %s | %s\n", getRversion(), Sys.info()[["sysname"]]))
