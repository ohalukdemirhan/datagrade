#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

# Vectorised, allocation-free column predicate. vapply() is used throughout in
# preference to sapply() so that column loops keep a known type and never
# silently collapse to a list on edge-case inputs (zero columns, all-NA).
which_cols <- function(data, predicate) {
  if (length(data) == 0L) return(character(0))
  names(data)[vapply(data, predicate, logical(1L))]
}

numeric_cols <- function(data) which_cols(data, is.numeric)

is_date_col <- function(x) inherits(x, c("Date", "POSIXct", "POSIXlt"))

date_cols <- function(data) which_cols(data, is_date_col)

# A numeric matrix of the numeric columns, dropping zero-variance and all-NA
# columns. Those columns are the source of most NaN/NA propagation downstream
# (cor() returns NA rows, scale() returns NaN), so they are removed once here
# and reported separately rather than being handled at every call site.
numeric_matrix <- function(data, drop_constant = TRUE) {
  cols <- numeric_cols(data)
  if (length(cols) == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = 0L,
                  dimnames = list(NULL, character(0))))
  }
  m <- as.matrix(as.data.frame(data[cols], stringsAsFactors = FALSE))
  storage.mode(m) <- "double"
  if (drop_constant && ncol(m) > 0L) {
    keep <- apply(m, 2L, function(col) {
      finite <- col[is.finite(col)]
      length(finite) > 1L && stats::var(finite) > 0
    })
    m <- m[, keep, drop = FALSE]
  }
  m
}

# Deterministic row subsample. Every expensive step (Shapiro-Wilk, random
# forest, plot rendering) routes through this so that a 5,000,000-row input
# costs the same as a 100,000-row input in those steps, and so that repeated
# runs on the same input give the same answer.
subsample <- function(data, n, seed = 1L) {
  total <- if (is.data.frame(data)) nrow(data) else length(data)
  if (is.na(n) || n <= 0 || total <= n) return(data)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  set.seed(seed)
  idx <- sort(sample.int(total, n))
  if (!is.null(old)) assign(".Random.seed", old, envir = globalenv())
  if (is.data.frame(data)) data[idx, , drop = FALSE] else data[idx]
}

# Scores are held on the dissertation's 0-10 scale everywhere internally.
# The 0-100 conversion happens once, in as_pct(), which is the fix for the
# scale mismatch that made the dissertation's consistency figure ten times
# too small (2.5% where 25% was meant).
as_pct <- function(score) round(score * 10, 1)

clamp <- function(x, lo = 0, hi = 10) max(lo, min(hi, x))

say <- function(verbose, fun, ...) {
  if (isTRUE(verbose)) fun(...)
  invisible(NULL)
}
