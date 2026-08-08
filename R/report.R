#' Print a quality report
#'
#' @param x A `dg_report`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.dg_report <- function(x, ...) {
  cli::cli_h1("Data quality report")
  cli::cli_text("{.strong Source:} {x$source}")
  cli::cli_text("{.strong Size:} {.val {x$n_row}} rows x {.val {x$n_col}} columns")
  cli::cli_text("{.strong Elapsed:} {round(x$elapsed, 2)}s")

  cli::cli_h2("ISO/IEC 25012 characteristics")
  n_measured <- table(factor(x$measures$characteristic[x$measures$applicable],
                             levels = dg_characteristics))
  n_total <- table(x$measures$characteristic)
  for (nm in names(x$scores)) {
    score <- x$scores[[nm]]
    of <- sprintf("%s/%s measures", n_measured[[nm]], n_total[[nm]])
    if (is.na(score)) {
      cli::cli_li("{.field {nm}}: not applicable ({of})")
    } else {
      # The band name is printed next to every score, so the reading never
      # depends on a colour alone.
      cli::cli_li("{.field {nm}}: {.strong {sprintf('%.3f', score)}} ({as_pct(score)}%) - {score_band(score)} ({of})")
    }
  }
  if (!is.na(x$overall)) {
    cli::cli_text("")
    cli::cli_text("{.strong Overall: {sprintf('%.3f', x$overall)} ({as_pct(x$overall)}%) - {score_band(x$overall)}}")
  }
  if (!is.na(x$plausibility)) {
    cli::cli_text("")
    cli::cli_text("{.emph Not ISO:} plausibility {sprintf('%.3f', x$plausibility)} ({as_pct(x$plausibility)}%) - outliers against a derived interval, excluded from the overall score.")
  }
  n_na <- sum(!x$measures$applicable)
  if (n_na > 0L) {
    cli::cli_text("")
    cli::cli_alert_info(
      "{.val {n_na}} of {.val {nrow(x$measures)}} measures need a declared expectation. See {.code report$measures} and {.fn dg_spec}.")
  }

  cli::cli_h2("Findings")
  n_missing <- sum(x$missing$columns$n_missing)
  cli::cli_li("Missing values: {.val {n_missing}} across {.val {sum(x$missing$columns$n_missing > 0)}} column{?s}")
  if (length(x$missing$high_missing_columns)) {
    cli::cli_li("Heavily incomplete columns: {.field {x$missing$high_missing_columns}}")
  }
  if (!is.null(x$duplicates)) {
    cli::cli_li("Duplicate rows: {.val {x$duplicates$n}} ({sprintf('%.2f', x$duplicates$rate * 100)}%)")
  }
  cli::cli_li("Outliers ({x$settings$outlier_method}): {.val {x$outliers$total}} of {.val {sum(x$outliers$by_column$n_tested)}} numeric values")
  if (length(x$identifiers)) {
    cli::cli_li("Identifier column{?s} (excluded from statistics): {.field {x$identifiers}}")
  } else {
    cli::cli_li("No identifier column detected")
  }
  if (nrow(x$redundancy$pairs)) {
    top <- x$redundancy$pairs[1L, ]
    cli::cli_li("Redundant pairs above |r| > {x$settings$correlation_threshold}: {.val {nrow(x$redundancy$pairs)}} (strongest {.field {top$column_1}}~{.field {top$column_2}} at {sprintf('%.2f', top$correlation)})")
    cli::cli_li("Suggested to drop: {.field {x$redundancy$columns}}")
  } else {
    cli::cli_li("No redundant column pairs")
  }
  if (length(x$consistency$inflated)) {
    cli::cli_li("VIF above {x$settings$vif_threshold}: {.field {x$consistency$inflated}}")
  }
  if (nrow(x$dates)) {
    cli::cli_li("Date columns: {.val {nrow(x$dates)}}, future dates {.val {sum(x$dates$n_future)}}, stale {.val {sum(x$dates$n_stale)}}")
  }
  non_normal <- x$distribution$column[!x$distribution$normal]
  if (length(non_normal)) {
    cli::cli_li("Non-normal columns (Shapiro-Wilk p < 0.05): {.field {non_normal}}")
  }
  if (!is.null(x$anomalies)) {
    cli::cli_li("Relationship anomalies: {.val {x$anomalies$n}} rows ({sprintf('%.2f', x$anomalies$rate * 100)}%)")
  }
  cli::cli_text("")
  cli::cli_alert_info("{.code summary()} for tables, {.code dg_plots()} for figures.")
  invisible(x)
}

#' Tabular summary of a quality report
#'
#' @param object A `dg_report`.
#' @param ... Unused.
#' @return A list of data frames: `scores`, `measures`, `columns`,
#'   `redundant_pairs` and `distribution`.
#' @export
summary.dg_report <- function(object, ...) {
  scores <- data.frame(
    characteristic = names(object$scores),
    score = unname(object$scores),
    percent = as_pct(unname(object$scores)),
    band = vapply(unname(object$scores), score_band, character(1L)),
    weight = unname(object$weights[names(object$scores)]),
    stringsAsFactors = FALSE)
  scores <- rbind(scores, data.frame(
    characteristic = "overall", score = object$overall,
    percent = as_pct(object$overall), band = score_band(object$overall),
    weight = NA_real_, stringsAsFactors = FALSE))

  cols <- merge(
    data.frame(column = names(object$types), type = unname(object$types),
               stringsAsFactors = FALSE),
    object$missing$columns, by = "column", all.x = TRUE)
  cols <- merge(cols, object$outliers$by_column[, c("column", "n_outliers")],
                by = "column", all.x = TRUE)
  vif <- object$consistency$vif
  cols$vif <- if (length(vif)) unname(vif[cols$column]) else NA_real_
  cols <- cols[order(match(cols$column, names(object$types))), , drop = FALSE]
  row.names(cols) <- NULL

  structure(list(scores = scores, measures = object$measures, columns = cols,
                 redundant_pairs = object$redundancy$pairs,
                 distribution = object$distribution,
                 source = object$source),
            class = "summary.dg_report")
}

#' @export
print.summary.dg_report <- function(x, ...) {
  cli::cli_h2("Characteristics")
  print(x$scores, row.names = FALSE)
  cli::cli_h2("Measures")
  print(x$measures[, c("code", "property", "characteristic", "value", "a", "b")],
        row.names = FALSE)
  cli::cli_h2("Columns")
  print(x$columns, row.names = FALSE)
  if (nrow(x$redundant_pairs)) {
    cli::cli_h2("Redundant pairs")
    print(utils::head(x$redundant_pairs, 10L), row.names = FALSE)
  }
  cli::cli_h2("Distribution")
  print(x$distribution, row.names = FALSE)
  invisible(x)
}

#' Coerce a quality report to a one-row data frame
#'
#' Convenient for assessing many files and stacking the results.
#'
#' @param x A `dg_report`.
#' @param row.names Passed to [base::as.data.frame()].
#' @param optional Passed to [base::as.data.frame()].
#' @param ... Unused.
#' @return A one-row data frame.
#' @export
as.data.frame.dg_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    source = x$source, rows = x$n_row, columns = x$n_col,
    accuracy = x$scores[["accuracy"]],
    completeness = x$scores[["completeness"]],
    consistency = x$scores[["consistency"]],
    credibility = x$scores[["credibility"]],
    currentness = x$scores[["currentness"]],
    overall = x$overall,
    plausibility = x$plausibility,
    n_measures_applicable = sum(x$measures$applicable),
    n_missing = sum(x$missing$columns$n_missing),
    n_duplicates = if (is.null(x$duplicates)) NA_integer_ else x$duplicates$n,
    n_outliers = x$outliers$total,
    n_redundant_pairs = nrow(x$redundancy$pairs),
    elapsed_seconds = x$elapsed,
    row.names = row.names, stringsAsFactors = FALSE)
}
