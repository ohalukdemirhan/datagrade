#' Assess the quality of a data set
#'
#' Runs every check in the package and returns a `dg_report` object scoring the
#' data on the five inherent characteristics of the ISO/IEC 25012 data quality
#' model — accuracy, completeness, consistency, credibility and currentness —
#' through the fifteen quality properties beneath them. Each property is
#' measured as a ratio `A / B` over counted data items, the form ISO/IEC 25024
#' measurement functions take.
#'
#' Six of the fifteen properties are computable from a table alone. The other
#' nine have a denominator that counts *the items for which an expectation is
#' declared*, which no data set contains; supply a [dg_spec()] to make them
#' measurable. Without one they are reported as not applicable and dropped from
#' the aggregate, never scored as zero.
#'
#' A sixth, non-ISO score is reported alongside them. `plausibility` is the
#' outlier-based measure this package used before it followed the standard: it
#' compares values against an interval *derived* from the data rather than one
#' declared in a specification. That makes it useful and cheap on data with no
#' documentation, but it is not accuracy in the ISO sense — an outlier need not
#' be wrong, and a wrong value can sit exactly on the mean — so it is reported
#' separately and excluded from the overall score.
#'
#' The function never modifies the caller's data. Text normalisation and
#' imputation are applied to an internal copy used for the statistics, and the
#' imputed frame is returned only when `keep_data = TRUE`.
#'
#' @param data A data frame. Either `data` or `path` must be supplied.
#' @param path Path to a `.csv`, `.xls` or `.xlsx` file.
#' @param spec A [dg_spec()] declaring what the data is supposed to look like.
#'   Optional; without it the nine specification-dependent measures report as
#'   not applicable.
#' @param outlier_method `"z_score"` (default) or `"iqr"`.
#' @param imputation_method `"mean"` (default), `"median"` or `"none"`.
#' @param correlation_threshold Absolute correlation above which a column pair
#'   is reported as redundant.
#' @param vif_threshold Variance inflation factor above which a column counts
#'   against the consistency score.
#' @param category_column Optional grouping column for outlier and correlation
#'   analysis.
#' @param weights Named weights for the five characteristics in the overall
#'   score. Characteristics that do not apply to the data are dropped and the
#'   remaining weights renormalised. `timeliness` is accepted as an alias for
#'   `currentness`.
#' @param clean_text Normalise whitespace in character columns.
#' @param strip_punctuation Also remove punctuation. Unicode-aware; off by
#'   default because it is destructive.
#' @param parse_dates Convert character columns that parse cleanly as dates.
#'   Required for the timeliness dimension to be measurable from a CSV.
#' @param check_duplicates Whether to run the duplicate scan, the only check
#'   whose cost is superlinear in row count.
#' @param anomaly_detection Fit a random forest to flag anomalous rows.
#' @param sample_size Row budget for the checks that subsample.
#' @param seed Seed used by every subsampling step.
#' @param keep_data Retain the imputed data frame on the returned object.
#' @param verbose Print progress to the console.
#'
#' @return An object of class `dg_report`. See [print.dg_report()],
#'   [summary.dg_report()] and [dg_plots()].
#'
#' @examples
#' report <- dg_assess(iris, verbose = FALSE)
#' report$scores
#' report$measures[, c("code", "value", "a", "b")]
#'
#' # Declaring an expectation makes the accuracy measures computable.
#' spec <- dg_spec(ranges = list(Sepal.Length = c(4, 8)),
#'                 domains = list(Species = levels(iris$Species)))
#' dg_assess(iris, spec = spec, verbose = FALSE)$scores
#' @export
dg_assess <- function(data = NULL, path = NULL,
                       spec = NULL,
                       outlier_method = c("z_score", "iqr"),
                       imputation_method = c("mean", "median", "none"),
                       correlation_threshold = 0.5,
                       vif_threshold = 5,
                       category_column = NULL,
                       weights = c(accuracy = 1, completeness = 1,
                                   consistency = 1, credibility = 1,
                                   currentness = 1),
                       clean_text = TRUE,
                       strip_punctuation = FALSE,
                       parse_dates = TRUE,
                       check_duplicates = TRUE,
                       anomaly_detection = FALSE,
                       sample_size = 1e5,
                       seed = 1L,
                       keep_data = FALSE,
                       verbose = TRUE) {
  outlier_method <- match.arg(outlier_method)
  imputation_method <- match.arg(imputation_method)
  if (is.null(spec)) spec <- dg_spec()
  if (!inherits(spec, "dg_spec")) {
    cli::cli_abort("{.arg spec} must come from {.fn dg_spec}, not {.cls {class(spec)[1]}}.")
  }
  # `timeliness` was the dissertation's name for what ISO calls currentness.
  # Existing weight vectors keep working.
  names(weights)[names(weights) == "timeliness"] <- "currentness"
  started <- Sys.time()
  timings <- c()
  step <- function(label, expr) {
    t0 <- Sys.time()
    on.exit(timings[[label]] <<- as.numeric(difftime(Sys.time(), t0, units = "secs")))
    expr
  }

  data <- dg_read(data, path, verbose)
  source_label <- if (!is.null(path)) basename(path) else "in-memory data frame"
  n_row <- nrow(data)
  n_col <- ncol(data)
  say(verbose, cli::cli_h1, "Data quality assessment")
  say(verbose, cli::cli_alert_info,
      "{.val {n_row}} rows x {.val {n_col}} columns from {source_label}")

  # Dates are parsed before any text normalisation, otherwise punctuation
  # stripping turns "2024-01-15" into "20240115" and no date column ever
  # exists to measure timeliness against.
  parsed_dates <- character(0)
  if (parse_dates) {
    data <- step("parse_dates", parse_date_columns(data))
    parsed_dates <- attr(data, "dg_parsed_dates") %||% character(0)
    attr(data, "dg_parsed_dates") <- NULL
    if (length(parsed_dates)) {
      say(verbose, cli::cli_alert_success,
          "Parsed {.val {length(parsed_dates)}} column{?s} as dates: {.field {parsed_dates}}")
    }
  }
  if (clean_text) {
    data <- step("clean_text",
                 clean_text_columns(data, strip_punctuation = strip_punctuation))
  }

  types <- step("types", detect_data_types(data))
  identifiers <- names(types)[types == "ID"]
  # Identifier columns are excluded from every statistical test. A surrogate
  # key is perfectly correlated with row order and would otherwise dominate
  # the correlation and multicollinearity results.
  analysis_cols <- setdiff(names(data), identifiers)
  analysis <- data[, analysis_cols, drop = FALSE]

  say(verbose, cli::cli_alert_info, "Analysing missing data...")
  missing <- step("missing", analyze_missing_data_patterns(data))

  duplicates <- if (check_duplicates) {
    say(verbose, cli::cli_alert_info, "Scanning for duplicate rows...")
    step("duplicates", detect_duplicates(data))
  } else NULL

  dates <- step("dates", validate_dates(data))

  imputed <- step("impute",
                  impute_missing_values(analysis, imputation_method,
                                        group_by = category_column))

  say(verbose, cli::cli_alert_info, "Detecting outliers...")
  # Outliers are measured on observed values, not imputed ones. Mean imputation
  # adds mass at the centre and shrinks the standard deviation, which mechanically
  # suppresses the very z-scores this step is looking for.
  outliers <- step("outliers",
                   detect_outliers(analysis, method = outlier_method,
                                   category_column = category_column))
  # Not accuracy. The interval is derived from the data, where every ISO
  # accuracy measure requires one that was declared, so this is reported on its
  # own and kept out of the aggregate.
  plausibility <- if (nrow(outliers$by_column) == 0L) NA_real_ else
    clamp(1 - outliers$rate)

  say(verbose, cli::cli_alert_info, "Checking consistency...")
  redundancy <- step("redundancy",
                     flag_redundant_columns(imputed, correlation_threshold))
  consistency <- step("consistency",
                      calculate_consistency_score(imputed, vif_threshold))
  correlation <- step("correlation", analyze_correlation(imputed, category_column))

  say(verbose, cli::cli_alert_info, "Assessing distributions...")
  distribution <- step("distribution",
                       analyze_distribution(analysis, sample_size = 5000L, seed = seed))

  anomalies <- if (anomaly_detection) {
    say(verbose, cli::cli_alert_info, "Fitting anomaly model...")
    step("anomalies",
         flag_anomalies_in_relationships(imputed, sample_size = sample_size, seed = seed))
  } else NULL

  say(verbose, cli::cli_alert_info, "Measuring the ISO/IEC 25012 properties...")
  # Measured on the parsed and normalised frame but before imputation: imputing
  # a mean into an empty cell would make the completeness measures report the
  # data as complete when it is not.
  measures <- step("measures",
                   dg_measure_table(data, spec, consistency, vif_threshold))
  scores <- dg_characteristic_scores(measures)
  overall <- dg_overall(scores, weights)

  report <- structure(
    list(
      source = source_label,
      n_row = n_row, n_col = n_col,
      dimensions = c(rows = n_row, columns = n_col),
      types = types,
      identifiers = identifiers,
      parsed_dates = parsed_dates,
      missing = missing,
      duplicates = duplicates,
      dates = dates,
      outliers = outliers,
      redundancy = redundancy,
      consistency = consistency,
      correlation = correlation,
      distribution = distribution,
      anomalies = anomalies,
      measures = measures,
      scores = scores,
      plausibility = plausibility,
      # ISO's word for this characteristic is currentness; `timeliness` is the
      # dissertation's, kept so that code written against v1 still reads.
      timeliness = unname(scores[["currentness"]]),
      overall = overall,
      spec = spec,
      weights = weights,
      settings = list(outlier_method = outlier_method,
                      imputation_method = imputation_method,
                      correlation_threshold = correlation_threshold,
                      vif_threshold = vif_threshold,
                      category_column = category_column,
                      sample_size = sample_size, seed = seed),
      plot_data = build_plot_data(analysis, missing, redundancy, distribution,
                                  sample_size, seed),
      data = if (keep_data) imputed else NULL,
      timings = timings,
      elapsed = as.numeric(difftime(Sys.time(), started, units = "secs"))
    ),
    class = "dg_report")

  if (verbose) print(report)
  invisible(report)
}

#' @rdname dg_assess
#' @param ... Passed to [dg_assess()].
#' @details `data_quality_assessment()` is the name used in the dissertation and
#'   is kept as an alias.
#' @export
data_quality_assessment <- function(...) dg_assess(...)

dg_read <- function(data, path, verbose = TRUE) {
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      cli::cli_abort("{.arg data} must be a data frame, not {.cls {class(data)[1]}}.")
    }
    return(as.data.frame(data, stringsAsFactors = FALSE))
  }
  if (is.null(path)) {
    cli::cli_abort("Supply either {.arg data} or {.arg path}.")
  }
  if (!file.exists(path)) cli::cli_abort("File not found: {.file {path}}")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      cli::cli_abort("Reading {.file .{ext}} needs the {.pkg readxl} package.")
    }
    return(as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE))
  }
  if (ext %in% c("csv", "txt", "tsv")) {
    sep <- if (ext == "tsv") "\t" else ","
    return(utils::read.csv(path, sep = sep, stringsAsFactors = FALSE,
                           check.names = FALSE))
  }
  cli::cli_abort(
    "Unsupported format {.val {ext}}. Use .csv, .tsv, .txt, .xls or .xlsx.")
}

# Dimensions that do not apply (no dates, no numeric columns) are dropped and
# the surviving weights renormalised, so a data set without dates is not
# punished for it.
dg_overall <- function(scores, weights) {
  usable <- !is.na(scores)
  if (!any(usable)) return(NA_real_)
  w <- weights[names(scores)]
  w[is.na(w)] <- 0
  w <- w[usable]
  if (sum(w) == 0) return(mean(scores[usable]))
  sum(scores[usable] * w) / sum(w)
}

build_plot_data <- function(data, missing, redundancy, distribution,
                            sample_size, seed) {
  cols <- numeric_cols(data)
  probe <- subsample(data, sample_size, seed = seed)
  list(
    missing = missing$columns,
    correlation = redundancy$matrix,
    boxplots = if (length(cols)) boxplot_stats(probe, cols) else NULL,
    histograms = if (length(cols)) histogram_bins(probe, cols) else NULL,
    distribution = distribution
  )
}
