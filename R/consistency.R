#' Variance inflation factors
#'
#' Computed from the diagonal of the inverted correlation matrix, which is
#' algebraically identical to `1 / (1 - R^2_j)` — the definition given in the
#' dissertation — but costs `O(p^3)` in the number of columns and is independent
#' of the number of rows. Fitting `p` auxiliary regressions instead, as the
#' original implementation did through a dummy response, is `O(n p^2)` and does
#' not finish in reasonable time on millions of rows.
#'
#' @param data A data frame or numeric matrix.
#' @return A named numeric vector of VIF values. Perfectly collinear columns are
#'   reported as `Inf`.
#' @examples
#' dg_vif(iris)
#' @export
dg_vif <- function(data) {
  m <- if (is.matrix(data)) data else numeric_matrix(data)
  if (ncol(m) < 2L) return(stats::setNames(numeric(0), character(0)))
  R <- stats::cor(m, use = "pairwise.complete.obs")
  R[!is.finite(R)] <- 0
  diag(R) <- 1
  aliased <- character(0)
  repeat {
    keep <- setdiff(colnames(R), aliased)
    if (length(keep) < 2L) break
    sub <- R[keep, keep, drop = FALSE]
    inv <- tryCatch(solve(sub), error = function(e) NULL)
    if (!is.null(inv)) {
      vif <- diag(inv)
      names(vif) <- keep
      # Columns dropped for perfect collinearity have infinite inflation by
      # definition; reporting them as Inf keeps them visible instead of
      # silently removing them from the denominator of the score.
      out <- c(vif, stats::setNames(rep(Inf, length(aliased)), aliased))
      return(out[colnames(R)])
    }
    # solve() failed: one column is an exact linear combination of the others.
    # Drop the one with the strongest off-diagonal correlation and retry.
    off <- abs(sub); diag(off) <- 0
    aliased <- c(aliased, keep[which.max(apply(off, 2L, max))])
  }
  stats::setNames(rep(Inf, ncol(R)), colnames(R))
}

#' Consistency score from multicollinearity
#'
#' @param data A data frame.
#' @param vif_threshold VIF above which a column counts as inconsistent.
#' @return A list with the score as a ratio in `[0, 1]`, the VIF vector, and the
#'   names of the offending columns.
#' @examples
#' calculate_consistency_score(iris)$score
#' @export
calculate_consistency_score <- function(data, vif_threshold = 5) {
  vif <- dg_vif(data)
  if (length(vif) == 0L) {
    return(list(score = NA_real_, vif = vif, inflated = character(0),
                note = "fewer than two usable numeric columns"))
  }
  inflated <- names(vif)[vif > vif_threshold]
  # A ratio in [0, 1], the same scale as every other dimension. The dissertation
  # computed this proportion and then multiplied it by 10 as if it were already
  # a 0-10 score, which is why it reports 2.5% for the iris data where 25% was
  # meant.
  score <- sum(vif <= vif_threshold) / length(vif)
  list(score = score, vif = vif, inflated = inflated, note = NULL)
}

#' Flag redundant columns by pairwise correlation
#'
#' @param data A data frame.
#' @param correlation_threshold Absolute correlation above which a pair is
#'   considered redundant.
#' @return A list with the correlation matrix, a data frame of offending pairs,
#'   and the columns proposed for removal.
#' @examples
#' flag_redundant_columns(iris, 0.9)$columns
#' @export
flag_redundant_columns <- function(data, correlation_threshold = 0.5) {
  m <- numeric_matrix(data)
  empty <- data.frame(column_1 = character(0), column_2 = character(0),
                      correlation = numeric(0), stringsAsFactors = FALSE)
  if (ncol(m) < 2L) {
    return(list(matrix = NULL, pairs = empty, columns = character(0)))
  }
  cm <- stats::cor(m, use = "pairwise.complete.obs")
  idx <- which(abs(cm) > correlation_threshold & upper.tri(cm), arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(list(matrix = cm, pairs = empty, columns = character(0)))
  }
  # The row/column names of the *value* matrix, not of the index matrix. The
  # original took colnames() of the index matrix, whose columns are literally
  # named "row" and "col", so those two strings were reported as redundant
  # data columns — including on inputs that had no redundancy at all.
  pairs <- data.frame(
    column_1 = colnames(cm)[idx[, "row"]],
    column_2 = colnames(cm)[idx[, "col"]],
    correlation = cm[idx],
    stringsAsFactors = FALSE
  )
  pairs <- pairs[order(-abs(pairs$correlation)), , drop = FALSE]
  row.names(pairs) <- NULL
  # Greedy removal: keep dropping the column that appears in the most surviving
  # pairs until none are left, so one member of each pair is retained.
  drop <- character(0)
  live <- pairs
  while (nrow(live) > 0L) {
    counts <- sort(table(c(live$column_1, live$column_2)), decreasing = TRUE)
    worst <- names(counts)[1L]
    drop <- c(drop, worst)
    live <- live[live$column_1 != worst & live$column_2 != worst, , drop = FALSE]
  }
  list(matrix = cm, pairs = pairs, columns = drop)
}

#' Correlation matrix, optionally per category
#'
#' @param data A data frame.
#' @param category_column Optional grouping column.
#' @return A list with the overall correlation matrix and, if requested, one
#'   matrix per group.
#' @examples
#' analyze_correlation(iris, "Species")$overall
#' @export
analyze_correlation <- function(data, category_column = NULL) {
  m <- numeric_matrix(data)
  overall <- if (ncol(m) >= 2L) stats::cor(m, use = "pairwise.complete.obs") else NULL
  by_group <- NULL
  if (!is.null(category_column) && category_column %in% names(data)) {
    groups <- split(seq_len(nrow(data)), data[[category_column]], drop = TRUE)
    by_group <- lapply(groups, function(idx) {
      sub <- numeric_matrix(data[idx, , drop = FALSE])
      if (ncol(sub) >= 2L) stats::cor(sub, use = "pairwise.complete.obs") else NULL
    })
  }
  list(overall = overall, by_group = by_group)
}
