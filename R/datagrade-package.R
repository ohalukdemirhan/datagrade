#' datagrade: scored data quality assessment for open data sets
#'
#' One call, [dg_assess()], profiles a data set and scores it on the ISO/IEC
#' 25024 dimensions of completeness, accuracy, consistency and timeliness. It
#' returns a structured `dg_report` object with tables and figures rather than
#' console text alone.
#'
#' The scoring framework implements the methodology of Demirhan, O. H. (2024),
#' *Assessing Data Quality Management Issues in Open-Sourced Data Sets with
#' Statistical Methodology*, MSc dissertation, University of Greenwich. Where the
#' package deviates from the dissertation, the deviation is recorded in
#' `NEWS.md` and in the "Deviations from the dissertation" section of the README.
#'
#' @keywords internal
#' @importFrom ggplot2 .data
"_PACKAGE"
