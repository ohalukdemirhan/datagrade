# Outlier flags for one numeric vector. NA-safe by construction: a constant
# column has sd 0, and the original code's scale() returned NaN there, which
# turned the whole report's outlier count into NA.
outlier_flags <- function(x, method = c("z_score", "iqr"), threshold = 3) {
  method <- match.arg(method)
  ok <- is.finite(x)
  flags <- rep(FALSE, length(x))
  if (sum(ok) < 3L) return(flags)
  v <- x[ok]
  if (method == "z_score") {
    s <- stats::sd(v)
    if (!is.finite(s) || s == 0) return(flags)
    flags[ok] <- abs((v - mean(v)) / s) > threshold
  } else {
    q <- stats::quantile(v, c(0.25, 0.75), names = FALSE)
    iqr <- q[2L] - q[1L]
    if (!is.finite(iqr) || iqr == 0) return(flags)
    flags[ok] <- v < (q[1L] - 1.5 * iqr) | v > (q[2L] + 1.5 * iqr)
  }
  flags
}

#' Detect outliers in numeric columns
#'
#' @param data A data frame.
#' @param numeric_columns Columns to test. Defaults to every numeric column.
#' @param method `"z_score"` (default) or `"iqr"`.
#' @param category_column Optional grouping column; outliers are then counted
#'   within each group.
#' @param threshold Z-score cut-off. Ignored by the IQR method, which uses the
#'   conventional 1.5 x IQR fence.
#' @return A list with the total count, the share of tested values that were
#'   flagged, and a per-column breakdown.
#' @examples
#' detect_outliers(iris)$total
#' @export
detect_outliers <- function(data, numeric_columns = NULL,
                            method = c("z_score", "iqr"),
                            category_column = NULL, threshold = 3) {
  method <- match.arg(method)
  cols <- numeric_columns %||% numeric_cols(data)
  cols <- setdiff(cols, category_column)
  if (length(cols) == 0L) {
    return(list(total = 0L, rate = 0,
                by_column = data.frame(column = character(0), n_outliers = integer(0),
                                       n_tested = integer(0), rate = numeric(0),
                                       stringsAsFactors = FALSE)))
  }
  groups <- if (!is.null(category_column) && category_column %in% names(data)) {
    split(seq_len(nrow(data)), data[[category_column]], drop = TRUE)
  } else {
    list(seq_len(nrow(data)))
  }
  rows <- lapply(cols, function(col) {
    x <- data[[col]]
    n_out <- 0L
    for (idx in groups) n_out <- n_out + sum(outlier_flags(x[idx], method, threshold))
    tested <- sum(is.finite(x))
    data.frame(column = col, n_outliers = as.integer(n_out),
               n_tested = as.integer(tested),
               rate = if (tested > 0L) n_out / tested else 0,
               stringsAsFactors = FALSE)
  })
  by_column <- do.call(rbind, rows)
  total_tested <- sum(by_column$n_tested)
  list(total = sum(by_column$n_outliers),
       rate = if (total_tested > 0L) sum(by_column$n_outliers) / total_tested else 0,
       by_column = by_column)
}

# Box-plot statistics computed once per column, so plotting a 5,000,000-row
# column costs the same as plotting a 500-row one. geom_boxplot() with the
# default stat would carry every observation into the plot object.
boxplot_stats <- function(data, cols, max_points = 500L) {
  rows <- lapply(cols, function(col) {
    v <- data[[col]]
    v <- v[is.finite(v)]
    if (length(v) < 5L) return(NULL)
    q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
    iqr <- q[3L] - q[1L]
    lower <- q[1L] - 1.5 * iqr
    upper <- q[3L] + 1.5 * iqr
    inside <- v[v >= lower & v <= upper]
    out <- v[v < lower | v > upper]
    data.frame(column = col,
               ymin = if (length(inside)) min(inside) else q[1L],
               lower = q[1L], middle = q[2L], upper = q[3L],
               ymax = if (length(inside)) max(inside) else q[3L],
               n_outliers = length(out),
               stringsAsFactors = FALSE)
    })
  stats_df <- do.call(rbind, Filter(Negate(is.null), rows))
  points <- lapply(cols, function(col) {
    v <- data[[col]]
    v <- v[is.finite(v)]
    if (length(v) < 5L) return(NULL)
    q <- stats::quantile(v, c(0.25, 0.75), names = FALSE)
    iqr <- q[2L] - q[1L]
    out <- v[v < q[1L] - 1.5 * iqr | v > q[2L] + 1.5 * iqr]
    if (length(out) == 0L) return(NULL)
    data.frame(column = col, value = subsample(out, max_points),
               stringsAsFactors = FALSE)
  })
  points_df <- do.call(rbind, Filter(Negate(is.null), points))
  list(stats = stats_df, points = points_df)
}
