#' Detect the semantic type of every column
#'
#' Unlike a plain `class()` lookup this distinguishes an identifier column from
#' an ordinary numeric or text column, and a low-cardinality text column from
#' free text.
#'
#' The uniqueness test is evaluated **before** the storage-type tests. In the
#' dissertation's pseudocode (Appendix A.3) it came last, which made the `"ID"`
#' branch unreachable: a character key matched `is.character()` and returned
#' `"Text"` first.
#'
#' @param data A data frame.
#' @param categorical_max Columns with at most this many distinct values are
#'   reported as `"Categorical"` rather than `"Text"`.
#' @return A named character vector, one entry per column, with values
#'   `"ID"`, `"Numeric"`, `"Text"`, `"Date"`, `"Categorical"` or `"Logical"`.
#' @examples
#' detect_data_types(data.frame(id = c("a", "b"), n = 1:2))
#' @export
detect_data_types <- function(data, categorical_max = 20L) {
  stopifnot(is.data.frame(data))
  n <- nrow(data)
  vapply(data, function(column) {
    non_na <- column[!is.na(column)]
    # An identifier is complete and distinct in every row. Requiring
    # completeness stops a mostly-empty column from being called a key.
    #
    # Distinctness alone is not enough: a continuous measurement is distinct in
    # every row by construction, so a bare uniqueness test classifies every
    # rnorm() column as a key and then excludes it from the statistics. A
    # numeric column therefore qualifies only when its values are whole
    # numbers, which is what a surrogate key looks like.
    if (n > 1L && length(non_na) == n && length(unique(non_na)) == n) {
      looks_like_key <- if (is.numeric(column)) {
        all(is.finite(non_na)) && all(non_na == trunc(non_na))
      } else {
        !is.logical(column)
      }
      if (looks_like_key) return("ID")
    }
    if (is_date_col(column)) return("Date")
    if (is.logical(column)) return("Logical")
    if (is.factor(column)) return("Categorical")
    if (is.numeric(column)) return("Numeric")
    if (is.character(column)) {
      if (length(unique(non_na)) <= categorical_max) return("Categorical")
      return("Text")
    }
    "Categorical"
  }, character(1L))
}

#' Normalise text columns without destroying information
#'
#' Trims whitespace and collapses internal runs of spaces. Case folding and
#' punctuation stripping are opt-in and, when enabled, are Unicode-aware.
#'
#' The dissertation's implementation applied `gsub("[^a-zA-Z0-9 ]", "")`
#' unconditionally, which deleted every accented and non-Latin character,
#' every date separator and every `@` in an e-mail address. Assessment is a
#' diagnosis, not a treatment, so nothing here is destructive by default.
#'
#' @param data A data frame.
#' @param lowercase Fold to lower case.
#' @param strip_punctuation Remove characters that are neither alphanumeric nor
#'   whitespace. Unicode letters and digits are preserved.
#' @return The data frame with its character columns normalised.
#' @examples
#' clean_text_columns(data.frame(city = c("  Agri ", "Corum")))
#' @export
clean_text_columns <- function(data, lowercase = FALSE,
                               strip_punctuation = FALSE) {
  stopifnot(is.data.frame(data))
  for (col in which_cols(data, is.character)) {
    x <- trimws(data[[col]])
    x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
    if (lowercase) x <- tolower(x)
    # \p{L} and \p{N} are Unicode letter and number properties, so accented and
    # non-Latin characters survive. POSIX [:alnum:] would not do: inside a
    # bracket expression it is resolved against the C locale and strips exactly
    # the characters this is meant to protect.
    if (strip_punctuation) {
      x <- gsub("[^\\p{L}\\p{N}\\s]", "", x, perl = TRUE)
    }
    data[[col]] <- x
  }
  data
}

# Character columns that look like dates, parsed before any text normalisation
# runs. Without this step a CSV never has a Date column at all and the
# timeliness dimension is silently unmeasurable.
parse_date_columns <- function(data, min_success = 0.9, sample_n = 5000L) {
  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
               "%Y-%m-%d %H:%M:%S", "%d.%m.%Y")
  converted <- character(0)
  for (col in which_cols(data, is.character)) {
    x <- data[[col]]
    probe <- subsample(x[!is.na(x)], sample_n)
    if (length(probe) == 0L) next
    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(probe, format = fmt))
      if (mean(!is.na(parsed)) >= min_success) {
        data[[col]] <- suppressWarnings(as.Date(x, format = fmt))
        converted <- c(converted, col)
        break
      }
    }
  }
  attr(data, "dg_parsed_dates") <- converted
  data
}

#' Check date columns for impossible and stale values
#'
#' @param data A data frame.
#' @param stale_years Dates older than this many years are counted as stale.
#' @return A data frame with one row per date column: number of future dates,
#'   number of stale dates, and the observed range.
#' @export
validate_dates <- function(data, stale_years = 10) {
  cols <- date_cols(data)
  if (length(cols) == 0L) {
    return(data.frame(column = character(0), n_future = integer(0),
                      n_stale = integer(0), min = as.Date(character(0)),
                      max = as.Date(character(0)),
                      stringsAsFactors = FALSE))
  }
  today <- Sys.Date()
  cutoff <- today - stale_years * 365.25
  rows <- lapply(cols, function(col) {
    x <- as.Date(data[[col]])
    x <- x[!is.na(x)]
    data.frame(
      column = col,
      n_future = sum(x > today),
      n_stale = sum(x < cutoff),
      min = if (length(x)) min(x) else as.Date(NA),
      max = if (length(x)) max(x) else as.Date(NA),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Identify columns that could serve as a unique key
#'
#' @param data A data frame.
#' @return A character vector of candidate key columns, possibly empty.
#' @export
check_unique_identifiers <- function(data) {
  types <- detect_data_types(data)
  names(types)[types == "ID"]
}

#' Count duplicate rows
#'
#' Returns a count and a small sample rather than the full set of duplicated
#' rows. On a data set with millions of near-identical rows, materialising
#' every duplicate can be larger than the input itself.
#'
#' @param data A data frame.
#' @param columns Optional subset of columns defining a duplicate.
#' @param max_examples Number of example rows to retain.
#' @return A list with `n` (count), `rate` (share of rows) and `examples`.
#' @export
detect_duplicates <- function(data, columns = NULL, max_examples = 5L) {
  target <- if (is.null(columns)) data else data[, columns, drop = FALSE]
  dup <- duplicated_rows(target)
  n <- sum(dup)
  examples <- if (n > 0L) {
    utils::head(data[which(dup), , drop = FALSE], max_examples)
  } else {
    data[0, , drop = FALSE]
  }
  list(n = n,
       rate = if (nrow(data) > 0L) n / nrow(data) else 0,
       examples = examples)
}

# duplicated() on a data frame builds one list per row before comparing, which
# is the only step in the package whose cost grows faster than the row count:
# at ten million rows it took 494s against 9.6s for everything else combined.
# Encoding each column to integer codes first and hashing one short key per row
# gives an identical answer in linear time.
duplicated_rows <- function(data) {
  n <- nrow(data)
  if (n == 0L) return(logical(0))
  if (ncol(data) == 0L) return(c(FALSE, rep(TRUE, n - 1L)))
  codes <- lapply(data, function(col) {
    if (is.factor(col)) as.integer(col) else match(col, unique(col))
  })
  duplicated(do.call(paste, c(codes, sep = "\r")))
}
