#' Compare two deliveries of the same data set
#'
#' Assesses two versions of a data set — last month's extract and this
#' month's, the file before and after a pipeline change — and reports what
#' changed, property by property, on the ISO/IEC 25012 model.
#'
#' Both versions are scored against the same [dg_spec()] and the same settings,
#' so a change in a score is a change in the data and not in the yardstick.
#' When two existing `dg_report` objects are passed instead, they are compared
#' as they are, and a warning is raised if they were produced with different
#' specifications or settings.
#'
#' A measure is reported as `declined` or `improved` only when its value moves
#' by more than `tolerance`. A measure that is computable in one version and not
#' the other — a date column that disappeared, a declared column that was
#' dropped — is reported as `lost` or `gained` rather than as a numeric change,
#' because "not applicable" is not a score of zero.
#'
#' Alongside the scores the comparison reports the structural changes that most
#' often explain them: columns added or removed, columns whose detected type
#' changed, the row count, the duplicate rate and the missing-value rate of
#' every column the two versions share.
#'
#' @param old,new Two data frames, or two `dg_report` objects from
#'   [dg_assess()]. `old` is the reference.
#' @param spec A [dg_spec()] applied to both versions. Ignored when reports are
#'   passed.
#' @param tolerance Smallest absolute change in a measure, characteristic or
#'   overall score that counts as a change. Defaults to one percentage point.
#' @param ... Further arguments passed to [dg_assess()] for both versions, such
#'   as `outlier_method` or `check_duplicates`. Ignored when reports are
#'   passed.
#'
#' @return An object of class `dg_comparison`: a list with `overall`,
#'   `scores` and `measures` (each with `old`, `new`, `change` and `status`
#'   columns), `schema` (added, removed and retyped columns), `rows`,
#'   `duplicates`, `missing` (per shared column) and the two reports as `old`
#'   and `new`. `as.data.frame()` returns the measure table.
#'
#' @examples
#' old <- airquality
#' new <- airquality
#' new$Ozone[1:40] <- NA          # a sensor stopped reporting
#' new$Wind <- NULL               # a column was dropped upstream
#' cmp <- dg_compare(old, new)
#' cmp
#' cmp$scores
#' cmp$missing[cmp$missing$status != "unchanged", ]
#'
#' # With a declared expectation, the accuracy measures are compared too.
#' spec <- dg_spec(ranges = list(Temp = c(50, 100)))
#' hot <- airquality
#' hot$Temp[1:10] <- 120
#' dg_compare(airquality, hot, spec = spec)$measures[1:3, ]
#' @export
dg_compare <- function(old, new, spec = NULL, tolerance = 0.01, ...) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) ||
      tolerance < 0) {
    cli::cli_abort("{.arg tolerance} must be a single non-negative number.")
  }
  is_report <- c(inherits(old, "dg_report"), inherits(new, "dg_report"))
  if (all(is_report)) {
    if (!is.null(spec) || length(list(...))) {
      cli::cli_warn(
        "{.arg spec} and {.arg ...} are ignored when two reports are compared.")
    }
    warn_if_incomparable(old, new)
    r_old <- old
    r_new <- new
  } else if (!any(is_report)) {
    if (!is.data.frame(old) || !is.data.frame(new)) {
      cli::cli_abort(
        "{.arg old} and {.arg new} must both be data frames or both be {.cls dg_report} objects.")
    }
    r_old <- dg_assess(old, spec = spec, verbose = FALSE, ...)
    r_new <- dg_assess(new, spec = spec, verbose = FALSE, ...)
  } else {
    cli::cli_abort(
      "{.arg old} and {.arg new} must both be data frames or both be {.cls dg_report} objects.")
  }

  measures <- data.frame(
    code = r_old$measures$code,
    property = r_old$measures$property,
    characteristic = r_old$measures$characteristic,
    old = r_old$measures$value,
    new = r_new$measures$value[match(r_old$measures$code, r_new$measures$code)],
    stringsAsFactors = FALSE)
  measures <- add_change(measures, tolerance)

  scores <- data.frame(
    characteristic = names(r_old$scores),
    old = unname(r_old$scores),
    new = unname(r_new$scores[names(r_old$scores)]),
    stringsAsFactors = FALSE)
  scores <- add_change(scores, tolerance)

  overall <- add_change(data.frame(old = r_old$overall, new = r_new$overall),
                        tolerance)

  old_cols <- names(r_old$types)
  new_cols <- names(r_new$types)
  shared <- intersect(old_cols, new_cols)
  retyped <- shared[r_old$types[shared] != r_new$types[shared]]
  schema <- list(
    added = setdiff(new_cols, old_cols),
    removed = setdiff(old_cols, new_cols),
    retyped = data.frame(column = retyped,
                         old = unname(r_old$types[retyped]),
                         new = unname(r_new$types[retyped]),
                         stringsAsFactors = FALSE))

  miss_old <- r_old$missing$columns
  miss_new <- r_new$missing$columns
  missing <- data.frame(
    column = shared,
    old = miss_old$rate[match(shared, miss_old$column)],
    new = miss_new$rate[match(shared, miss_new$column)],
    stringsAsFactors = FALSE)
  # Missingness is a rate where lower is better, the opposite of a score.
  missing <- add_change(missing, tolerance, higher_is_better = FALSE)
  missing <- missing[order(-abs(missing$change), missing$column), , drop = FALSE]
  rownames(missing) <- NULL

  duplicates <- if (!is.null(r_old$duplicates) && !is.null(r_new$duplicates)) {
    add_change(data.frame(old = r_old$duplicates$rate,
                          new = r_new$duplicates$rate),
               tolerance, higher_is_better = FALSE)
  } else NULL

  structure(
    list(
      overall = overall,
      scores = scores,
      measures = measures,
      schema = schema,
      rows = c(old = r_old$n_row, new = r_new$n_row),
      duplicates = duplicates,
      missing = missing,
      tolerance = tolerance,
      old = r_old,
      new = r_new),
    class = "dg_comparison")
}

# Adds `change` and `status` to a frame with numeric `old` and `new` columns.
add_change <- function(df, tolerance, higher_is_better = TRUE) {
  df$change <- df$new - df$old
  better <- if (higher_is_better) df$change > tolerance else df$change < -tolerance
  worse <- if (higher_is_better) df$change < -tolerance else df$change > tolerance
  df$status <- ifelse(is.na(df$old) & is.na(df$new), "not applicable",
               ifelse(is.na(df$old), "gained",
               ifelse(is.na(df$new), "lost",
               ifelse(better, "improved",
               ifelse(worse, "declined", "unchanged")))))
  df
}

warn_if_incomparable <- function(old, new) {
  differs <- character(0)
  if (!identical(unclass(old$spec), unclass(new$spec))) differs <- c(differs, "specification")
  if (!identical(old$settings[names(old$settings) != "sample_size"],
                 new$settings[names(new$settings) != "sample_size"])) {
    differs <- c(differs, "settings")
  }
  if (length(differs)) {
    cli::cli_warn(c(
      "The two reports were produced with different {differs}.",
      "i" = "Score changes may reflect the yardstick rather than the data. Pass data frames to compare under one {.fn dg_spec}."))
  }
}

#' Print a comparison of two deliveries
#'
#' @param x A `dg_comparison` from [dg_compare()].
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.dg_comparison <- function(x, ...) {
  cli::cli_h1("Data quality comparison")
  cli::cli_text("{.strong Rows:} {.val {x$rows[['old']]}} -> {.val {x$rows[['new']]}}")
  fmt <- function(v) ifelse(is.na(v), "n/a", sprintf("%.3f", v))
  arrow <- function(row) {
    sprintf("%s -> %s (%s)", fmt(row$old), fmt(row$new), row$status)
  }

  cli::cli_h2("ISO/IEC 25012 characteristics")
  for (i in seq_len(nrow(x$scores))) {
    cli::cli_li("{.field {x$scores$characteristic[i]}}: {arrow(x$scores[i, ])}")
  }
  cli::cli_text("")
  cli::cli_text("{.strong Overall:} {arrow(x$overall)}")

  moved <- x$measures[x$measures$status %in% c("declined", "improved", "lost", "gained"), ,
                      drop = FALSE]
  cli::cli_h2("Measures that changed")
  if (nrow(moved)) {
    moved <- moved[order(match(moved$status, c("declined", "lost", "gained", "improved"))), ]
    for (i in seq_len(nrow(moved))) {
      cli::cli_li("{.code {moved$code[i]}} {moved$property[i]}: {arrow(moved[i, ])}")
    }
  } else {
    cli::cli_text("None beyond the tolerance of {x$tolerance}.")
  }

  cli::cli_h2("What changed in the data")
  if (length(x$schema$added)) cli::cli_li("Columns added: {.field {x$schema$added}}")
  if (length(x$schema$removed)) cli::cli_li("Columns removed: {.field {x$schema$removed}}")
  if (nrow(x$schema$retyped)) {
    cli::cli_li("Type changed: {.field {paste0(x$schema$retyped$column, ' (', x$schema$retyped$old, ' -> ', x$schema$retyped$new, ')')}}")
  }
  worse <- x$missing[x$missing$status == "declined", , drop = FALSE]
  if (nrow(worse)) {
    cli::cli_li("More missing values in: {.field {paste0(worse$column, ' (', as_pct(worse$old), '% -> ', as_pct(worse$new), '%)')}}")
  }
  if (!is.null(x$duplicates) && x$duplicates$status != "unchanged") {
    cli::cli_li("Duplicate rate: {sprintf('%.2f%% -> %.2f%%', x$duplicates$old * 100, x$duplicates$new * 100)} ({x$duplicates$status})")
  }
  if (!length(x$schema$added) && !length(x$schema$removed) &&
      !nrow(x$schema$retyped) && !nrow(worse) &&
      (is.null(x$duplicates) || x$duplicates$status == "unchanged")) {
    cli::cli_text("No structural change detected.")
  }
  invisible(x)
}

#' @rdname dg_compare
#' @param x A `dg_comparison`.
#' @param row.names,optional Unused; present for compatibility with the
#'   generic.
#' @export
as.data.frame.dg_comparison <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$measures
}
