# Colour roles, not colour names. Each slot below is used for exactly one job:
# categorical = identity, sequential = magnitude, diverging = polarity,
# status = state. Nothing is reused across jobs, which is what keeps a status
# red from ever being read as "series 8".
dg_palette <- list(
  categorical = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948"),
  sequential  = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5",
                  "#256abf", "#184f95", "#0d366b"),
  diverging   = c(low = "#2a78d6", mid = "#f0efec", high = "#e34948"),
  status      = c(good = "#0ca30c", warning = "#fab219",
                  serious = "#ec835a", critical = "#d03b3b"),
  ink         = "#0b0b0b",
  ink_muted   = "#52514e",
  surface     = "#fcfcfb",
  grid        = "#e4e3df"
)

#' A recessive ggplot2 theme for quality-report figures
#'
#' Grid and axes are deliberately low-contrast so the marks carry the reading.
#'
#' @param base_size Base font size in points.
#' @return A [ggplot2::theme()] object.
#' @export
theme_dg <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size * 1.15,
        colour = dg_palette$ink, margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 0.9, colour = dg_palette$ink_muted,
        margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.75, colour = dg_palette$ink_muted, hjust = 0),
      axis.title = ggplot2::element_text(
        size = base_size * 0.85, colour = dg_palette$ink_muted),
      axis.text = ggplot2::element_text(
        size = base_size * 0.8, colour = dg_palette$ink_muted),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = dg_palette$grid, linewidth = 0.3),
      plot.background = ggplot2::element_rect(
        fill = dg_palette$surface, colour = NA),
      panel.background = ggplot2::element_rect(
        fill = dg_palette$surface, colour = NA),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = base_size * 0.8),
      legend.text = ggplot2::element_text(size = base_size * 0.75),
      strip.text = ggplot2::element_text(
        size = base_size * 0.8, colour = dg_palette$ink, face = "bold")
    )
}

# Status is never carried by colour alone: every caller of this function also
# prints the band name next to the mark.
score_band <- function(score) {
  if (is.na(score)) return("not applicable")
  if (score >= 0.9) "good" else if (score >= 0.7) "warning" else
    if (score >= 0.5) "serious" else "critical"
}

score_colour <- function(score) {
  band <- score_band(score)
  if (band == "not applicable") dg_palette$ink_muted else
    unname(dg_palette$status[band])
}
