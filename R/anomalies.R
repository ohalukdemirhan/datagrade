#' Flag rows whose variable relationships look anomalous
#'
#' Fits a random forest predicting the first numeric column from the others and
#' flags rows whose residual exceeds `sd_threshold` standard deviations. The
#' model is always fitted on a deterministic subsample, then applied to the full
#' data, so cost is bounded by `sample_size` rather than by the input size.
#'
#' Requires the suggested \pkg{randomForest} package.
#'
#' @param data A data frame.
#' @param target Column to predict. Defaults to the first numeric column.
#' @param sample_size Rows used to fit the model.
#' @param sd_threshold Residual cut-off in standard deviations.
#' @param ntree Number of trees.
#' @param seed Seed for both the subsample and the forest.
#' @return A list with the number of anomalies, their share, and example rows;
#'   or `NULL` when the input has too few numeric columns.
#' @export
flag_anomalies_in_relationships <- function(data, target = NULL,
                                            sample_size = 20000L,
                                            sd_threshold = 2, ntree = 100L,
                                            seed = 1L) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    cli::cli_alert_warning(
      "Package {.pkg randomForest} is not installed; skipping anomaly detection.")
    return(NULL)
  }
  m <- numeric_matrix(data)
  if (ncol(m) < 2L) return(NULL)
  complete <- stats::complete.cases(m)
  m <- m[complete, , drop = FALSE]
  if (nrow(m) < 20L) return(NULL)
  target <- target %||% colnames(m)[1L]
  if (!target %in% colnames(m)) return(NULL)

  train <- subsample(as.data.frame(m), sample_size, seed = seed)
  set.seed(seed)
  model <- randomForest::randomForest(
    x = train[, setdiff(colnames(m), target), drop = FALSE],
    y = train[[target]], ntree = ntree)
  predicted <- stats::predict(
    model, as.data.frame(m)[, setdiff(colnames(m), target), drop = FALSE])
  residuals <- abs(predicted - m[, target])
  s <- stats::sd(residuals)
  if (!is.finite(s) || s == 0) return(NULL)
  flagged <- residuals > mean(residuals) + sd_threshold * s
  rows <- which(complete)[flagged]
  list(target = target, n = length(rows), rate = length(rows) / nrow(data),
       examples = utils::head(data[rows, , drop = FALSE], 5L),
       trained_on = nrow(train))
}
