#' Summarise missing-data patterns
#'
#' @param data A data frame.
#' @param high_threshold Share of missing values above which a column or row is
#'   flagged as heavily incomplete.
#' @return A list with a per-column summary data frame, the names of heavily
#'   incomplete columns, the count of heavily incomplete rows, and the number of
#'   rows that are complete in every column.
#' @examples
#' analyze_missing_data_patterns(airquality)$columns
#' @export
analyze_missing_data_patterns <- function(data, high_threshold = 0.3) {
  n <- nrow(data)
  if (length(data) == 0L || n == 0L) {
    return(list(columns = data.frame(column = character(0), n_missing = integer(0),
                                     rate = numeric(0), stringsAsFactors = FALSE),
                high_missing_columns = character(0),
                n_high_missing_rows = 0L, n_complete_rows = 0L))
  }
  # colSums() over a logical matrix is a single vectorised pass; the original
  # sapply(data, function(col) sum(is.na(col))) allocated one logical vector
  # per column and dominated runtime on wide inputs.
  na_matrix <- is.na(data)
  n_missing <- colSums(na_matrix)
  rate <- n_missing / n
  row_missing <- rowSums(na_matrix)
  list(
    columns = data.frame(column = names(data), n_missing = as.integer(n_missing),
                         rate = as.numeric(rate), row.names = NULL,
                         stringsAsFactors = FALSE),
    high_missing_columns = names(data)[rate > high_threshold],
    n_high_missing_rows = sum(row_missing > ncol(data) * high_threshold),
    n_complete_rows = sum(row_missing == 0L)
  )
}

impute_column <- function(column, method) {
  if (!is.numeric(column)) return(column)
  na <- is.na(column)
  if (!any(na)) return(column)
  fill <- switch(method,
                 mean = mean(column, na.rm = TRUE),
                 median = stats::median(column, na.rm = TRUE),
                 NA_real_)
  # A column that is entirely missing yields NaN here; leaving it NA is honest,
  # whereas writing NaN back would poison every downstream statistic.
  if (!is.finite(fill)) return(column)
  column[na] <- fill
  column
}

#' Impute missing numeric values
#'
#' @param data A data frame.
#' @param method One of `"mean"`, `"median"` or `"none"`.
#' @param group_by Optional column name. When supplied, imputation is performed
#'   within each level of that column, falling back to the overall statistic for
#'   groups that are entirely missing.
#' @return The data frame with numeric `NA`s filled.
#' @examples
#' colSums(is.na(impute_missing_values(airquality, "median")))
#' @export
impute_missing_values <- function(data, method = c("mean", "median", "none"),
                                  group_by = NULL) {
  method <- match.arg(method)
  if (method == "none") return(data)
  num <- numeric_cols(data)
  if (length(num) == 0L) return(data)

  if (!is.null(group_by) && group_by %in% names(data)) {
    groups <- split(seq_len(nrow(data)), data[[group_by]], drop = TRUE)
    for (col in num) {
      x <- data[[col]]
      overall <- impute_column(x, method)
      for (idx in groups) x[idx] <- impute_column(x[idx], method)
      # Groups that were wholly missing are still NA; the overall statistic is
      # a better answer than leaving them out of every later calculation.
      still_na <- is.na(x)
      x[still_na] <- overall[still_na]
      data[[col]] <- x
    }
  } else {
    for (col in num) data[[col]] <- impute_column(data[[col]], method)
  }
  data
}
