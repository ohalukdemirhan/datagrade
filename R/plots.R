#' Figures for a quality report
#'
#' Returns a named list of \pkg{ggplot2} objects. Every figure is built from
#' statistics precomputed during [dg_assess()] — bin counts, five-number
#' summaries, a correlation matrix — so the plot objects never carry the raw
#' observations and rendering cost does not grow with the number of rows.
#'
#' @param x A `dg_report`.
#' @param which Names of the figures to build. Defaults to all available.
#' @return A named list of ggplot objects: `scores`, `missing`, `correlation`,
#'   `outliers`, `distribution`.
#' @examples
#' report <- dg_assess(airquality, verbose = FALSE)
#' figures <- dg_plots(report)
#' names(figures)
#' @export
dg_plots <- function(x, which = NULL) {
  stopifnot(inherits(x, "dg_report"))
  builders <- list(
    scores = plot_scores, missing = plot_missing, correlation = plot_correlation,
    outliers = plot_outliers, distribution = plot_distribution)
  if (!is.null(which)) builders <- builders[intersect(which, names(builders))]
  out <- lapply(builders, function(f) f(x))
  out[!vapply(out, is.null, logical(1L))]
}

#' @rdname dg_plots
#' @param y Unused.
#' @param ... Unused.
#' @export
plot.dg_report <- function(x, y, ...) {
  figures <- dg_plots(x)
  for (fig in figures) print(fig)
  invisible(figures)
}

plot_scores <- function(x) {
  scores <- c(x$scores, overall = x$overall)
  df <- data.frame(dimension = names(scores), score = unname(scores),
                   stringsAsFactors = FALSE)
  # A characteristic with no applicable measure has no bar to draw. Dropping it
  # is the same choice the aggregate makes, so the figure and the score agree.
  df <- df[!is.na(df$score), , drop = FALSE]
  if (nrow(df) == 0L) return(NULL)
  df$band <- vapply(df$score, score_band, character(1L))
  df$label <- sprintf("%.3f  %s", df$score, df$band)
  df$dimension <- factor(df$dimension, levels = rev(df$dimension))
  bands <- dg_palette$status[c("critical", "serious", "warning", "good")]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$score, y = .data$dimension,
                                   fill = .data$band)) +
    ggplot2::geom_col(width = 0.55) +
    # The band name is written on every bar, so the status colour is a
    # reinforcement and never the only carrier of meaning.
    ggplot2::geom_text(ggplot2::aes(label = .data$label), hjust = -0.08,
                       size = 3.2, colour = dg_palette$ink_muted) +
    ggplot2::scale_fill_manual(values = bands, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 1.25), breaks = seq(0, 1, 0.2),
                                expand = c(0, 0)) +
    ggplot2::labs(title = "Quality scores by characteristic",
                  subtitle = sprintf("%s | ISO/IEC 25012 inherent characteristics, %s of %s measures applicable",
                                     x$source, sum(x$measures$applicable),
                                     nrow(x$measures)),
                  x = "Score (ratio, 0-1)", y = NULL) +
    theme_dg() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_missing <- function(x) {
  df <- x$plot_data$missing
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  df <- df[order(-df$rate), , drop = FALSE]
  df$column <- factor(df$column, levels = rev(df$column))
  df$pct <- df$rate * 100
  complete <- all(df$n_missing == 0L)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$pct, y = .data$column)) +
    ggplot2::geom_col(width = 0.55, fill = dg_palette$sequential[5L]) +
    ggplot2::geom_text(
      data = df[df$n_missing > 0L, , drop = FALSE],
      ggplot2::aes(label = sprintf("%.1f%% (%s)", .data$pct, .data$n_missing)),
      hjust = -0.08, size = 3, colour = dg_palette$ink_muted) +
    ggplot2::scale_x_continuous(
      limits = c(0, max(5, max(df$pct) * 1.35)), expand = c(0, 0)) +
    ggplot2::labs(
      title = "Missing values by column",
      subtitle = if (complete) "No missing values in any column" else
        sprintf("%s of %s columns affected",
                sum(df$n_missing > 0L), nrow(df)),
      x = "Share missing (%)", y = NULL) +
    theme_dg() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_correlation <- function(x) {
  cm <- x$plot_data$correlation
  if (is.null(cm) || nrow(cm) < 2L) return(NULL)
  # Ordered by the leading eigenvector so correlated blocks sit together
  # instead of being scattered by alphabetical column order.
  ord <- tryCatch(order(eigen(cm, symmetric = TRUE)$vectors[, 1L]),
                  error = function(e) seq_len(ncol(cm)))
  cm <- cm[ord, ord, drop = FALSE]
  df <- expand.grid(row = colnames(cm), col = colnames(cm),
                    stringsAsFactors = FALSE)
  df$value <- as.vector(cm)
  df$row <- factor(df$row, levels = colnames(cm))
  df$col <- factor(df$col, levels = rev(colnames(cm)))
  small <- ncol(cm) <= 12L
  thr <- x$settings$correlation_threshold

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$row, y = .data$col,
                                        fill = .data$value)) +
    # 2px surface gap between cells; the grid never reads as a solid block.
    ggplot2::geom_tile(colour = dg_palette$surface, linewidth = 1) +
    # Diverging, because the quantity has a meaningful zero and two opposite
    # poles. Neutral grey midpoint, never a hue.
    ggplot2::scale_fill_gradient2(
      low = dg_palette$diverging[["low"]],
      mid = dg_palette$diverging[["mid"]],
      high = dg_palette$diverging[["high"]],
      midpoint = 0, limits = c(-1, 1), name = "r")
  if (small) {
    df$text <- ifelse(abs(df$value) > thr, sprintf("%.2f", df$value), "")
    p <- p + ggplot2::geom_text(
      data = df, ggplot2::aes(label = .data$text), size = 2.7,
      colour = dg_palette$ink)
  }
  p +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "Correlation between numeric columns",
      subtitle = sprintf("Labelled where |r| > %.2f | %s redundant pair%s found",
                         thr, nrow(x$redundancy$pairs),
                         if (nrow(x$redundancy$pairs) == 1L) "" else "s"),
      x = NULL, y = NULL) +
    theme_dg() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_outliers <- function(x) {
  box <- x$plot_data$boxplots
  if (is.null(box) || is.null(box$stats) || nrow(box$stats) == 0L) return(NULL)
  stats_df <- box$stats
  stats_df$column <- factor(stats_df$column, levels = stats_df$column)

  p <- ggplot2::ggplot(stats_df, ggplot2::aes(x = .data$column)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(ymin = .data$ymin, lower = .data$lower, middle = .data$middle,
                   upper = .data$upper, ymax = .data$ymax),
      stat = "identity", width = 0.5,
      fill = dg_palette$sequential[2L],
      colour = dg_palette$sequential[6L], linewidth = 0.5)
  if (!is.null(box$points) && nrow(box$points) > 0L) {
    pts <- box$points
    pts$column <- factor(pts$column, levels = levels(stats_df$column))
    p <- p + ggplot2::geom_point(
      data = pts, ggplot2::aes(x = .data$column, y = .data$value),
      colour = dg_palette$status[["critical"]], size = 1.4, alpha = 0.55,
      position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1))
  }
  p +
    ggplot2::facet_wrap(~ .data$column, scales = "free", ncol = 3) +
    ggplot2::labs(
      title = sprintf("Outliers by column (%s method)", x$settings$outlier_method),
      subtitle = sprintf("%s flagged values | red points are the flagged observations",
                         x$outliers$total),
      x = NULL, y = NULL) +
    theme_dg() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
}

plot_distribution <- function(x) {
  df <- x$plot_data$histograms
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  dist <- x$plot_data$distribution
  labels <- stats::setNames(
    sprintf("%s\nskew %.2f | kurtosis %.2f", dist$column, dist$skewness,
            dist$excess_kurtosis),
    dist$column)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$mid, y = .data$count)) +
    ggplot2::geom_col(width = df$width * 0.92,
                      fill = dg_palette$sequential[4L]) +
    ggplot2::facet_wrap(~ .data$column, scales = "free", ncol = 3,
                        labeller = ggplot2::as_labeller(labels)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = "Distribution of numeric columns",
      subtitle = sprintf("Excess kurtosis: 0 is normal | %s of %s columns pass Shapiro-Wilk",
                         sum(dist$normal, na.rm = TRUE), nrow(dist)),
      x = NULL, y = "Count") +
    theme_dg()
}

#' Save every report figure to a directory
#'
#' @param x A `dg_report`.
#' @param dir Output directory, created if needed.
#' @param width,height,dpi Passed to [ggplot2::ggsave()].
#' @param device File format.
#' @return The file paths, invisibly.
#' @export
dg_save_plots <- function(x, dir = "datagrade-figures", width = 9, height = 5.5,
                           dpi = 150, device = "png") {
  figures <- dg_plots(x)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- vapply(names(figures), function(nm) {
    path <- file.path(dir, sprintf("%s.%s", nm, device))
    h <- if (nm %in% c("outliers", "distribution")) height * 1.25 else height
    suppressWarnings(ggplot2::ggsave(path, figures[[nm]], width = width,
                                     height = h, dpi = dpi, bg = dg_palette$surface))
    path
  }, character(1L))
  invisible(paths)
}
