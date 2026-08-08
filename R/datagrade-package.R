#' datagrade: scored data quality assessment for open data sets
#'
#' One call, [dg_assess()], profiles a data set and scores it on the five
#' inherent characteristics of the ISO/IEC 25012 data quality model — accuracy,
#' completeness, consistency, credibility and currentness — through the fifteen
#' quality properties beneath them, each measured as a ratio `A / B` over
#' counted data items in the form ISO/IEC 25024 measurement functions take. It
#' returns a structured `dg_report` object with tables and figures rather than
#' console text alone.
#'
#' Six of the fifteen properties are computable from a table alone; the rest
#' need a [dg_spec()] declaring what the data is supposed to look like, because
#' their denominator counts the items for which an expectation exists. Without
#' one they report as not applicable rather than being approximated.
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
