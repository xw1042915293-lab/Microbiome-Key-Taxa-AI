# Plot helpers shared across analysis modules.

get_app_palette <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("get_app_palette(): n must be a positive number.", call. = FALSE)
  }

  base_palette <- c(
    "#1F6F8B", "#2A9D8F", "#43AA8B", "#7CB342",
    "#C0CA33", "#F4A261", "#F28482", "#E76F51",
    "#A44A82", "#7E57C2", "#4C78A8", "#6C757D"
  )

  n <- as.integer(ceiling(n))
  if (n <= length(base_palette)) {
    return(base_palette[seq_len(n)])
  }

  grDevices::colorRampPalette(base_palette)(n)
}

get_report_plot_theme <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#16324F", hjust = 0),
      axis.title = ggplot2::element_text(color = "#22313F"),
      axis.text = ggplot2::element_text(color = "#22313F"),
      legend.title = ggplot2::element_text(color = "#22313F"),
      legend.text = ggplot2::element_text(color = "#22313F"),
      panel.border = ggplot2::element_rect(color = "#C9D2DC", linewidth = 0.8),
      panel.grid.major.x = ggplot2::element_line(color = "#E7EDF3", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(color = "#E7EDF3", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#F4F7FA", color = "#D8E0E8"),
      plot.margin = ggplot2::margin(t = 14, r = 16, b = 14, l = 16)
    )
}

save_plot_pdf_png <- function(plot, pdf_path, png_path, width = 7, height = 5, dpi = 300) {
  assert_non_empty_string(pdf_path, "pdf_path")
  assert_non_empty_string(png_path, "png_path")
  ensure_dir(dirname(pdf_path))
  ensure_dir(dirname(png_path))
  ggplot2::ggsave(pdf_path, plot = plot, device = "pdf", width = width, height = height)
  ggplot2::ggsave(png_path, plot = plot, device = "png", width = width, height = height, dpi = dpi)
  invisible(list(
    pdf = normalizePath(pdf_path, winslash = "/", mustWork = TRUE),
    png = normalizePath(png_path, winslash = "/", mustWork = TRUE)
  ))
}
