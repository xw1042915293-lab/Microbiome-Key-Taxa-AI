# Alpha diversity analysis (Phase 2).
# Outputs:
# - tables/alpha_diversity.csv
# - tables/alpha_stats.csv
# - figures/alpha_shannon_boxplot.pdf + .png

save_plot_pdf_png <- function(plot, pdf_path, png_path, width = 7, height = 5, dpi = 300) {
  assert_non_empty_string(pdf_path, "pdf_path")
  assert_non_empty_string(png_path, "png_path")
  ensure_dir(dirname(pdf_path))
  ensure_dir(dirname(png_path))
  ggplot2::ggsave(pdf_path, plot = plot, device = "pdf", width = width, height = height)
  ggplot2::ggsave(png_path, plot = plot, device = "png", width = width, height = height, dpi = dpi)
  invisible(list(pdf = normalizePath(pdf_path, winslash = "/", mustWork = TRUE), png = normalizePath(png_path, winslash = "/", mustWork = TRUE)))
}

run_alpha_analysis <- function(dataset, group_var, job_dir) {
  if (is.null(dataset)) stop("run_alpha_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_alpha_analysis(): job_dir not found: ", job_dir, call. = FALSE)

  samp <- dataset$sample_table
  if (is.null(samp) || !is.data.frame(samp)) stop("run_alpha_analysis(): dataset$sample_table missing.", call. = FALSE)
  if (!group_var %in% names(samp)) stop("run_alpha_analysis(): group_var not found in sample_table: ", group_var, call. = FALSE)

  # microeco provides alpha diversity computation on microtable
  dataset$cal_alphadiv()
  alpha_tbl <- dataset$alpha_diversity
  if (is.null(alpha_tbl) || !is.data.frame(alpha_tbl)) stop("run_alpha_analysis(): alpha_diversity not generated.", call. = FALSE)

  # Ensure SampleID exists as a column for saving/joining.
  alpha_tbl <- tibble::as_tibble(alpha_tbl, rownames = "SampleID")
  alpha_tbl <- dplyr::left_join(alpha_tbl, samp[, c("SampleID", group_var), drop = FALSE], by = "SampleID")

  out_alpha <- file.path(job_dir, "tables", "alpha_diversity.csv")
  ensure_dir(dirname(out_alpha))
  readr::write_csv(alpha_tbl, out_alpha)

  # Stats on Shannon index (required plot)
  if (!"Shannon" %in% names(alpha_tbl)) stop("run_alpha_analysis(): Shannon column not found in alpha_diversity.", call. = FALSE)
  df <- alpha_tbl
  df <- df[!is.na(df[[group_var]]), , drop = FALSE]
  df$group <- as.factor(df[[group_var]])

  # Wilcoxon for 2 groups; Kruskal-Wallis for >=3 groups
  g_levels <- levels(df$group)
  stats_row <- list(
    index = "Shannon",
    test = NA_character_,
    p_value = NA_real_,
    n_samples = nrow(df),
    n_groups = length(g_levels)
  )
  if (length(g_levels) == 2) {
    stats_row$test <- "wilcoxon"
    stats_row$p_value <- tryCatch(stats::wilcox.test(Shannon ~ group, data = df)$p.value, error = function(e) NA_real_)
  } else if (length(g_levels) >= 3) {
    stats_row$test <- "kruskal"
    stats_row$p_value <- tryCatch(stats::kruskal.test(Shannon ~ group, data = df)$p.value, error = function(e) NA_real_)
  }

  alpha_stats <- tibble::tibble(!!!stats_row)
  alpha_stats$fdr <- ifelse(is.na(alpha_stats$p_value), NA_real_, p.adjust(alpha_stats$p_value, method = "fdr"))

  out_stats <- file.path(job_dir, "tables", "alpha_stats.csv")
  readr::write_csv(alpha_stats, out_stats)

  # Plot Shannon boxplot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = Shannon)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.7) +
    ggplot2::labs(x = group_var, y = "Shannon", title = "Alpha diversity (Shannon)") +
    ggplot2::theme_minimal(base_size = 12)

  fig_pdf <- file.path(job_dir, "figures", "alpha_shannon_boxplot.pdf")
  fig_png <- file.path(job_dir, "figures", "alpha_shannon_boxplot.png")
  fig_paths <- save_plot_pdf_png(p, fig_pdf, fig_png)

  list(
    alpha_table_path = normalizePath(out_alpha, winslash = "/", mustWork = TRUE),
    alpha_stats_path = normalizePath(out_stats, winslash = "/", mustWork = TRUE),
    figure_paths = fig_paths
  )
}
