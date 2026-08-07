# Sample skewness, and *excess* kurtosis. The dissertation's formula (Figure 7)
# subtracts 3 so that a normal distribution scores 0; moments::kurtosis() does
# not subtract it and returns 3, so the published formula and the published
# code disagreed by exactly 3. The formula is the contract, so it wins.
sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(NA_real_)
  mean(((x - mean(x)) / s)^3)
}

sample_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(NA_real_)
  mean(((x - mean(x)) / s)^4) - 3
}

#' Assess the distribution of every numeric column
#'
#' Reports skewness, excess kurtosis and a Shapiro-Wilk normality test. The test
#' is limited to 5,000 observations by its own definition, so larger columns are
#' tested on a deterministic subsample and the sample size used is reported.
#'
#' @param data A data frame.
#' @param sample_size Observations used for the normality test.
#' @param seed Seed for the subsample, so repeated runs agree.
#' @return A data frame with one row per numeric column.
#' @examples
#' analyze_distribution(iris)
#' @export
analyze_distribution <- function(data, sample_size = 5000L, seed = 1L) {
  cols <- numeric_cols(data)
  if (length(cols) == 0L) {
    return(data.frame(column = character(0), n = integer(0), skewness = numeric(0),
                      excess_kurtosis = numeric(0), shapiro_p = numeric(0),
                      shapiro_n = integer(0), normal = logical(0),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(cols, function(col) {
    v <- data[[col]]
    v <- v[is.finite(v)]
    probe <- subsample(v, min(sample_size, 5000L), seed = seed)
    p <- NA_real_
    if (length(unique(probe)) > 2L && length(probe) >= 3L) {
      p <- tryCatch(stats::shapiro.test(probe)$p.value,
                    error = function(e) NA_real_)
    }
    data.frame(column = col, n = length(v),
               skewness = sample_skewness(v),
               excess_kurtosis = sample_kurtosis(v),
               shapiro_p = p, shapiro_n = length(probe),
               normal = !is.na(p) & p >= 0.05,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# Histogram bins computed once per column so the plot object never carries the
# raw observations. This is what makes the distribution figures renderable at
# tens of millions of rows.
histogram_bins <- function(data, cols, bins = 30L) {
  rows <- lapply(cols, function(col) {
    v <- data[[col]]
    v <- v[is.finite(v)]
    if (length(v) < 2L) return(NULL)
    rng <- range(v)
    if (diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
    breaks <- seq(rng[1L], rng[2L], length.out = bins + 1L)
    counts <- tabulate(findInterval(v, breaks, rightmost.closed = TRUE,
                                    all.inside = TRUE), nbins = bins)
    data.frame(column = col,
               mid = (breaks[-1L] + breaks[-(bins + 1L)]) / 2,
               width = diff(breaks), count = counts,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}
