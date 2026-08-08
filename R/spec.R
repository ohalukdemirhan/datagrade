#' Declare what the data is supposed to look like
#'
#' Nine of the fifteen ISO/IEC 25012 quality properties cannot be computed from
#' a table alone, because their denominator counts *the data items for which an
#' expectation is defined* — and an expectation is something a person declares,
#' not something a data set contains. `dg_spec()` is where those declarations
#' go. Pass the result to [dg_assess()] as `spec`.
#'
#' Every argument is optional. Each one you supply switches on the measures that
#' depend on it; each one you leave out leaves those measures reported as *not
#' applicable* rather than guessed at. Supplying nothing is legal and gives the
#' same six zero-configuration measures [dg_assess()] computes on its own.
#'
#' @param required Character vector of columns that must be populated. Drives
#'   record completeness (`COMP_REG`) and data value completeness
#'   (`COMP_VAL_ESP`). Defaults to every column when absent.
#' @param expected_records Expected number of records in a complete file.
#'   Drives file completeness (`COMP_FICH`).
#' @param null_equivalents Character vector of strings that are structurally
#'   present but semantically empty. Drives false completeness
#'   (`FAL_COMP_FICH`). A conservative default list is used when absent; pass
#'   `character(0)` to disable the check.
#' @param patterns Named list of regular expressions, one per column. A value
#'   is syntactically accurate when it matches. Drives `EXAC_SINT`.
#' @param domains Named list of permitted value sets, one per column. Also
#'   drives `EXAC_SINT`; a column may have a pattern, a domain, or both.
#' @param ranges Named list of `c(min, max)` numeric intervals, one per column.
#'   Drives accuracy range (`RAN_EXAC`). This is the *declared* interval ISO
#'   requires, as distinct from the interval [dg_assess()] derives from the data
#'   for its non-ISO `plausibility` measure.
#' @param reference A data frame holding known-correct values, and `reference_key`
#'   the column(s) joining it to the assessed data. Drives semantic accuracy
#'   (`EXAC_SEMAN`).
#' @param reference_key Character vector of join columns for `reference`.
#' @param foreign_keys Named list mapping a column in the data to the permitted
#'   value set it must draw from — either a vector of values, or a
#'   `list(data = <data frame>, column = <name>)` pair. Drives referential
#'   integrity (`INT_REF`).
#' @param rules Named list of one-sided formulas evaluated per record in the
#'   data, each of which must be `TRUE` for a record to be semantically
#'   consistent, e.g. `list(order = ~ start_date <= end_date)`. Drives
#'   `CONS_SEMAN`.
#' @param formats Named list of regular expressions describing the required
#'   format of a column. Drives format consistency (`CONS_FORM`). Absent
#'   columns fall back to the dominant observed format, which is what makes
#'   `CONS_FORM` computable with no spec at all.
#' @param credible_ranges Named list of `c(min, max)` intervals outside which a
#'   value is not believable even if it is inside the valid range. Drives data
#'   values credibility (`CRED_VAL_DAT`).
#' @param credible_domains Named list of believable value sets per column. Also
#'   drives `CRED_VAL_DAT`.
#' @param source_trust Named list, or named character vector, giving the
#'   provenance of each column, together with `trusted_sources` naming the ones
#'   considered credible. Drives source credibility (`CRED_FUEN`).
#' @param trusted_sources Character vector of source names deemed credible.
#' @param update_column Name of the column carrying the update timestamp used by
#'   the currentness measures.
#' @param update_interval Longest acceptable gap between consecutive updates, as
#'   a `difftime` or a number of days. Drives update frequency (`FREC_ACT`).
#' @param max_age Longest acceptable age of a record, as a `difftime` or a
#'   number of days. Drives timeliness of update (`CONV_ACT`). Defaults to ten
#'   years, which is what makes `CONV_ACT` computable with no spec.
#' @param as_of Date the assessment is made relative to. Defaults to today.
#'
#' @return An object of class `dg_spec`.
#'
#' @examples
#' spec <- dg_spec(
#'   required = c("Sepal.Length", "Species"),
#'   ranges   = list(Sepal.Length = c(4, 8)),
#'   domains  = list(Species = c("setosa", "versicolor", "virginica"))
#' )
#' spec
#' dg_assess(iris, spec = spec, verbose = FALSE)$scores
#' @export
dg_spec <- function(required = NULL,
                    expected_records = NULL,
                    null_equivalents = NULL,
                    patterns = NULL,
                    domains = NULL,
                    ranges = NULL,
                    reference = NULL,
                    reference_key = NULL,
                    foreign_keys = NULL,
                    rules = NULL,
                    formats = NULL,
                    credible_ranges = NULL,
                    credible_domains = NULL,
                    source_trust = NULL,
                    trusted_sources = NULL,
                    update_column = NULL,
                    update_interval = NULL,
                    max_age = 3652.5,
                    as_of = Sys.Date()) {
  named_list <- function(x, arg) {
    if (is.null(x)) return(NULL)
    if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
      cli::cli_abort("{.arg {arg}} must be a named list, one entry per column.")
    }
    x
  }
  interval_list <- function(x, arg) {
    x <- named_list(x, arg)
    if (is.null(x)) return(NULL)
    bad <- names(x)[!vapply(x, function(r) is.numeric(r) && length(r) == 2L &&
                              !anyNA(r) && r[1L] <= r[2L], logical(1L))]
    if (length(bad)) {
      cli::cli_abort(c(
        "Every entry of {.arg {arg}} must be {.code c(min, max)} with {.code min <= max}.",
        x = "Offending: {.field {bad}}"))
    }
    x
  }
  regex_list <- function(x, arg) {
    x <- named_list(x, arg)
    if (is.null(x)) return(NULL)
    for (nm in names(x)) {
      if (!is.character(x[[nm]]) || length(x[[nm]]) != 1L) {
        cli::cli_abort("{.arg {arg}}${.field {nm}} must be a single regular expression.")
      }
      # Fail here, with the column name, rather than inside a vapply() over
      # millions of values where the message would name neither. The compile
      # failure arrives as a warning alongside the error; only the error is
      # worth surfacing.
      tryCatch(suppressWarnings(grepl(x[[nm]], "", perl = TRUE)),
               error = function(e) cli::cli_abort(
                 "{.arg {arg}}${.field {nm}} is not a valid regular expression: {conditionMessage(e)}"))
    }
    x
  }

  if (!is.null(required) && !is.character(required)) {
    cli::cli_abort("{.arg required} must be a character vector of column names.")
  }
  if (!is.null(expected_records) &&
      (!is.numeric(expected_records) || length(expected_records) != 1L ||
       is.na(expected_records) || expected_records <= 0)) {
    cli::cli_abort("{.arg expected_records} must be a single positive number.")
  }
  if (!is.null(reference)) {
    if (!is.data.frame(reference)) {
      cli::cli_abort("{.arg reference} must be a data frame of known-correct values.")
    }
    if (is.null(reference_key) || !is.character(reference_key)) {
      cli::cli_abort("{.arg reference_key} must name the columns joining {.arg reference} to the data.")
    }
    missing_key <- setdiff(reference_key, names(reference))
    if (length(missing_key)) {
      cli::cli_abort("{.arg reference} has no column{?s} {.field {missing_key}}.")
    }
  }
  rules <- named_list(rules, "rules")
  if (!is.null(rules)) {
    bad <- names(rules)[!vapply(rules, function(r) inherits(r, "formula") &&
                                  length(r) == 2L, logical(1L))]
    if (length(bad)) {
      cli::cli_abort(c(
        "Every entry of {.arg rules} must be a one-sided formula.",
        i = "For example {.code list(order = ~ start_date <= end_date)}.",
        x = "Offending: {.field {bad}}"))
    }
  }
  foreign_keys <- named_list(foreign_keys, "foreign_keys")

  as_days <- function(x, arg) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "difftime")) return(as.numeric(x, units = "days"))
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
      cli::cli_abort("{.arg {arg}} must be a positive number of days or a {.cls difftime}.")
    }
    as.numeric(x)
  }

  structure(
    list(
      required = required,
      expected_records = expected_records,
      null_equivalents = null_equivalents %||% dg_null_equivalents,
      patterns = regex_list(patterns, "patterns"),
      domains = named_list(domains, "domains"),
      ranges = interval_list(ranges, "ranges"),
      reference = reference,
      reference_key = reference_key,
      foreign_keys = foreign_keys,
      rules = rules,
      formats = regex_list(formats, "formats"),
      credible_ranges = interval_list(credible_ranges, "credible_ranges"),
      credible_domains = named_list(credible_domains, "credible_domains"),
      source_trust = source_trust,
      trusted_sources = trusted_sources,
      update_column = update_column,
      update_interval = as_days(update_interval, "update_interval"),
      max_age = as_days(max_age, "max_age"),
      as_of = as.Date(as_of)
    ),
    class = "dg_spec")
}

# Values that occupy a cell without carrying information. Kept deliberately
# short: every entry here is a string that can also be a legitimate value in
# some data set, and a false positive silently lowers a completeness score.
# Matching is case-insensitive and after whitespace trimming.
dg_null_equivalents <- c(
  "", "-", "--", "?", ".", "n/a", "na", "n.a.", "nan", "nil", "none",
  "null", "unknown", "unspecified", "missing", "not applicable",
  "not available", "-999", "-9999", "9999"
)

#' @export
print.dg_spec <- function(x, ...) {
  cli::cli_h2("Data quality specification")
  declared <- list(
    "required columns"  = x$required,
    "expected records"  = x$expected_records,
    "patterns"          = names(x$patterns),
    "domains"           = names(x$domains),
    "ranges"            = names(x$ranges),
    "reference source"  = if (!is.null(x$reference))
      sprintf("%s rows keyed on %s", nrow(x$reference),
              paste(x$reference_key, collapse = ", ")),
    "foreign keys"      = names(x$foreign_keys),
    "rules"             = names(x$rules),
    "formats"           = names(x$formats),
    "credible ranges"   = names(x$credible_ranges),
    "credible domains"  = names(x$credible_domains),
    "source trust"      = names(as.list(x$source_trust)),
    "update column"     = x$update_column,
    "update interval"   = if (!is.null(x$update_interval))
      sprintf("%s days", x$update_interval)
  )
  declared <- declared[!vapply(declared, function(v) is.null(v) || length(v) == 0L,
                               logical(1L))]
  if (length(declared) == 0L) {
    cli::cli_alert_info("Nothing declared. Only the zero-configuration measures will be computed.")
  } else {
    for (nm in names(declared)) {
      cli::cli_li("{.strong {nm}}: {.val {declared[[nm]]}}")
    }
  }
  cli::cli_text("")
  cli::cli_alert_info("Maximum record age {.val {x$max_age}} days, assessed as of {.val {format(x$as_of)}}.")
  invisible(x)
}
