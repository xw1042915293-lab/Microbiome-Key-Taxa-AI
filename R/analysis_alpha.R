# Alpha diversity analysis.
# Outputs:
# - alpha/tables/alpha_diversity.csv
# - alpha/tables/alpha_stats.csv
# - alpha/figures/<metric>/<metric>_<plot_type>.pdf + .png
# - alpha/figures/overview/alpha_overview_<plot_type>.pdf + .png

alpha_metric_spec <- function() {
  data.frame(
    index = c("Observed", "Chao1", "Shannon", "Simpson"),
    label = c("Observed richness", "Chao1 richness", "Shannon diversity", "Simpson diversity"),
    file_slug = c("observed", "chao1", "shannon", "simpson"),
    stringsAsFactors = FALSE
  )
}

alpha_plot_type_spec <- function() {
  data.frame(
    type = c("boxplot", "violin", "violin_box", "jitter_median", "mean_se", "mean_ci95", "median_iqr"),
    label = c(
      "箱线图 + 原始点",
      "小提琴图 + 原始点",
      "雨云式：小提琴 + 箱线 + 原始点",
      "原始散点 + 中位数",
      "均值 ± 标准误",
      "均值 ± 95% 置信区间",
      "中位数 ± 四分位距"
    ),
    stringsAsFactors = FALSE
  )
}

alpha_summary_ci95 <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  center <- if (n > 0) mean(x) else NA_real_
  margin <- if (n > 1) stats::qt(0.975, df = n - 1) * stats::sd(x) / sqrt(n) else NA_real_
  data.frame(y = center, ymin = center - margin, ymax = center + margin)
}

alpha_summary_iqr <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))
  qs <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  data.frame(y = qs[[2]], ymin = qs[[1]], ymax = qs[[3]])
}

alpha_output_path <- function(job_dir, kind = c("tables", "figures"), ..., legacy_filename = NULL, existing = FALSE) {
  kind <- match.arg(kind)
  assert_non_empty_string(job_dir, "job_dir")
  preferred <- file.path(job_dir, "alpha", kind, ...)
  if (isTRUE(existing) && !file.exists(preferred) && !is.null(legacy_filename)) {
    legacy <- file.path(job_dir, kind, legacy_filename)
    if (file.exists(legacy)) return(legacy)
  }
  preferred
}

alpha_metric_figure_path <- function(job_dir, metric, plot_type, extension = "png", existing = FALSE) {
  spec <- alpha_metric_spec()
  row <- spec[spec$index == metric, , drop = FALSE]
  if (nrow(row) != 1) stop("Unsupported Alpha metric: ", metric, call. = FALSE)
  if (!plot_type %in% alpha_plot_type_spec()$type) stop("Unsupported Alpha plot type: ", plot_type, call. = FALSE)
  slug <- row$file_slug[[1]]
  filename <- paste0(slug, "_", plot_type, ".", extension)
  legacy <- if (identical(plot_type, "boxplot")) paste0("alpha_", slug, "_boxplot.", extension) else NULL
  alpha_output_path(job_dir, "figures", slug, filename, legacy_filename = legacy, existing = existing)
}

alpha_overview_figure_path <- function(job_dir, plot_type, extension = "png", existing = FALSE) {
  if (!plot_type %in% alpha_plot_type_spec()$type) stop("Unsupported Alpha plot type: ", plot_type, call. = FALSE)
  filename <- paste0("alpha_overview_", plot_type, ".", extension)
  legacy <- if (identical(plot_type, "boxplot")) paste0("alpha_diversity_overview.", extension) else NULL
  alpha_output_path(job_dir, "figures", "overview", filename, legacy_filename = legacy, existing = existing)
}

get_alpha_plot_dims <- function(n_groups, overview = FALSE) {
  if (!is.numeric(n_groups) || length(n_groups) != 1 || is.na(n_groups) || n_groups < 1) {
    stop("get_alpha_plot_dims(): n_groups must be a positive number.", call. = FALSE)
  }

  if (isTRUE(overview)) {
    if (n_groups <= 8) return(list(width = 12, height = 8))
    return(list(width = 13, height = max(9, 0.7 * n_groups)))
  }

  if (n_groups <= 8) return(list(width = 7, height = 5))
  list(width = 8, height = max(5, 0.35 * n_groups))
}

alpha_metric_stats <- function(alpha_tbl, group_var, metrics) {
  rows <- lapply(metrics, function(metric) {
    df <- data.frame(
      value = suppressWarnings(as.numeric(alpha_tbl[[metric]])),
      group = alpha_tbl[[group_var]],
      stringsAsFactors = FALSE
    )
    df <- df[is.finite(df$value) & !is.na(df$group) & nzchar(trimws(as.character(df$group))), , drop = FALSE]
    df$group <- droplevels(as.factor(df$group))
    n_groups <- nlevels(df$group)
    test_name <- NA_character_
    p_value <- NA_real_

    if (n_groups == 2) {
      test_name <- "wilcoxon"
      p_value <- tryCatch(
        stats::wilcox.test(value ~ group, data = df, exact = FALSE)$p.value,
        error = function(e) NA_real_
      )
    } else if (n_groups >= 3) {
      test_name <- "kruskal"
      p_value <- tryCatch(
        stats::kruskal.test(value ~ group, data = df)$p.value,
        error = function(e) NA_real_
      )
    }

    data.frame(
      index = metric,
      test = test_name,
      p_value = p_value,
      n_samples = nrow(df),
      n_groups = n_groups,
      stringsAsFactors = FALSE
    )
  })

  out <- dplyr::bind_rows(rows)
  out$fdr <- stats::p.adjust(out$p_value, method = "fdr")
  tibble::as_tibble(out)
}

add_alpha_distribution_geom <- function(plot, plot_type, point_size = 2.2) {
  if (identical(plot_type, "boxplot")) {
    return(plot +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.88, width = 0.72, linewidth = 0.55) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.16, height = 0),
        shape = 21, size = point_size, stroke = 0.35, alpha = 0.8
      ))
  }

  if (identical(plot_type, "violin")) {
    return(plot +
      ggplot2::geom_violin(trim = FALSE, scale = "width", adjust = 0.9, alpha = 0.68, linewidth = 0.5) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.12, height = 0),
        shape = 21, size = point_size, stroke = 0.35, alpha = 0.72
      ))
  }

  if (identical(plot_type, "violin_box")) {
    return(plot +
      ggplot2::geom_violin(trim = FALSE, scale = "width", adjust = 0.9, alpha = 0.52, linewidth = 0.45) +
      ggplot2::geom_boxplot(outlier.shape = NA, width = 0.18, alpha = 0.9, linewidth = 0.5) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.1, height = 0),
        shape = 21, size = point_size * 0.9, stroke = 0.3, alpha = 0.68
      ))
  }

  if (identical(plot_type, "jitter_median")) {
    return(plot +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.16, height = 0),
        shape = 21, size = point_size, stroke = 0.35, alpha = 0.68
      ) +
      ggplot2::stat_summary(fun = stats::median, geom = "crossbar", width = 0.52, linewidth = 0.75))
  }

  if (identical(plot_type, "mean_se")) {
    return(plot +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.13, height = 0),
        shape = 21, size = point_size * 0.85, stroke = 0.3, alpha = 0.42
      ) +
      ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "errorbar", width = 0.18, linewidth = 0.75) +
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = point_size * 1.45, stroke = 0.7))
  }


  if (identical(plot_type, "mean_ci95")) {
    return(plot +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.13, height = 0),
        shape = 21, size = point_size * 0.85, stroke = 0.3, alpha = 0.34
      ) +
      ggplot2::stat_summary(fun.data = alpha_summary_ci95, geom = "errorbar", width = 0.2, linewidth = 0.8) +
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = point_size * 1.5, stroke = 0.75))
  }

  if (identical(plot_type, "median_iqr")) {
    return(plot +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.13, height = 0),
        shape = 21, size = point_size * 0.85, stroke = 0.3, alpha = 0.34
      ) +
      ggplot2::stat_summary(fun.data = alpha_summary_iqr, geom = "errorbar", width = 0.2, linewidth = 0.8) +
      ggplot2::stat_summary(fun = stats::median, geom = "point", shape = 23, size = point_size * 1.5, stroke = 0.75))
  }

  stop("Unsupported Alpha plot type: ", plot_type, call. = FALSE)
}

build_alpha_metric_plot <- function(alpha_tbl, group_var, metric, metric_label, plot_type = "boxplot") {
  df <- data.frame(
    value = suppressWarnings(as.numeric(alpha_tbl[[metric]])),
    group = alpha_tbl[[group_var]],
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$value) & !is.na(df$group) & nzchar(trimws(as.character(df$group))), , drop = FALSE]
  df$group <- stats::reorder(as.factor(df$group), df$value, FUN = stats::median, na.rm = TRUE)
  n_groups <- nlevels(df$group)
  palette_values <- get_app_palette(n_groups)
  names(palette_values) <- levels(df$group)
  use_horizontal <- n_groups > 8

  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = value, fill = group, color = group))
  p <- add_alpha_distribution_geom(p, plot_type, point_size = 2.2) +
    ggplot2::labs(x = group_var, y = metric, title = metric_label) +
    ggplot2::scale_fill_manual(values = palette_values) +
    ggplot2::scale_color_manual(values = palette_values) +
    get_report_plot_theme(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(color = "#22313F"),
      axis.text.y = ggplot2::element_text(color = "#22313F")
    )

  if (use_horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 9),
        plot.margin = ggplot2::margin(t = 14, r = 18, b = 14, l = 22)
      )
  } else {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))
  }

  list(plot = p, n_groups = n_groups, horizontal = use_horizontal)
}

build_alpha_overview_plot <- function(alpha_tbl, group_var, metrics, metric_labels, plot_type = "boxplot") {
  long_rows <- lapply(metrics, function(metric) {
    data.frame(
      group = alpha_tbl[[group_var]],
      index = unname(metric_labels[[metric]]),
      value = suppressWarnings(as.numeric(alpha_tbl[[metric]])),
      stringsAsFactors = FALSE
    )
  })
  df <- dplyr::bind_rows(long_rows)
  df <- df[is.finite(df$value) & !is.na(df$group) & nzchar(trimws(as.character(df$group))), , drop = FALSE]

  group_order_metric <- if ("Shannon" %in% metrics) "Shannon diversity" else unname(metric_labels[[metrics[[1]]]])
  order_df <- df[df$index == group_order_metric, , drop = FALSE]
  group_levels <- names(sort(tapply(order_df$value, order_df$group, stats::median, na.rm = TRUE)))
  df$group <- factor(df$group, levels = group_levels)
  df$index <- factor(df$index, levels = unname(metric_labels[metrics]))
  n_groups <- nlevels(df$group)
  palette_values <- get_app_palette(n_groups)
  names(palette_values) <- levels(df$group)
  use_horizontal <- n_groups > 8

  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = value, fill = group, color = group))
  p <- add_alpha_distribution_geom(p, plot_type, point_size = 1.8) +
    ggplot2::facet_wrap(~index, scales = "free_y", ncol = 2) +
    ggplot2::scale_fill_manual(values = palette_values) +
    ggplot2::scale_color_manual(values = palette_values) +
    ggplot2::labs(x = group_var, y = NULL, title = "Alpha diversity overview") +
    get_report_plot_theme(base_size = 11) +
    ggplot2::theme(legend.position = "none")

  if (use_horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.5))
  } else {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))
  }

  list(plot = p, n_groups = n_groups, horizontal = use_horizontal)
}

run_alpha_analysis <- function(dataset, group_var, job_dir) {
  if (is.null(dataset)) stop("run_alpha_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_alpha_analysis(): job_dir not found: ", job_dir, call. = FALSE)

  samp <- dataset$sample_table
  if (is.null(samp) || !is.data.frame(samp)) stop("run_alpha_analysis(): dataset$sample_table missing.", call. = FALSE)
  if (!group_var %in% names(samp)) stop("run_alpha_analysis(): group_var not found in sample_table: ", group_var, call. = FALSE)

  dataset$cal_alphadiv()
  alpha_tbl <- dataset$alpha_diversity
  if (is.null(alpha_tbl) || !is.data.frame(alpha_tbl)) stop("run_alpha_analysis(): alpha_diversity not generated.", call. = FALSE)

  alpha_tbl <- tibble::as_tibble(alpha_tbl, rownames = "SampleID")
  alpha_tbl <- dplyr::left_join(alpha_tbl, samp[, c("SampleID", group_var), drop = FALSE], by = "SampleID")

  out_alpha <- alpha_output_path(job_dir, "tables", "alpha_diversity.csv")
  ensure_dir(dirname(out_alpha))
  readr::write_csv(alpha_tbl, out_alpha)

  spec <- alpha_metric_spec()
  metrics <- spec$index[spec$index %in% names(alpha_tbl)]
  metrics <- metrics[vapply(metrics, function(metric) {
    values <- suppressWarnings(as.numeric(alpha_tbl[[metric]]))
    groups <- alpha_tbl[[group_var]]
    any(is.finite(values) & !is.na(groups) & nzchar(trimws(as.character(groups))))
  }, logical(1))]
  if (length(metrics) == 0) {
    stop("run_alpha_analysis(): none of the supported Alpha indices were generated.", call. = FALSE)
  }

  metric_labels <- stats::setNames(spec$label, spec$index)
  alpha_stats <- alpha_metric_stats(alpha_tbl, group_var, metrics)
  out_stats <- alpha_output_path(job_dir, "tables", "alpha_stats.csv")
  readr::write_csv(alpha_stats, out_stats)

  plot_types <- alpha_plot_type_spec()$type
  metric_figures <- list()
  max_groups <- 0L
  any_horizontal <- FALSE
  for (metric in metrics) {
    metric_figures[[metric]] <- list()
    for (plot_type in plot_types) {
      built <- build_alpha_metric_plot(alpha_tbl, group_var, metric, metric_labels[[metric]], plot_type = plot_type)
      dims <- get_alpha_plot_dims(built$n_groups)
      paths <- save_plot_pdf_png(
        built$plot,
        alpha_metric_figure_path(job_dir, metric, plot_type, "pdf"),
        alpha_metric_figure_path(job_dir, metric, plot_type, "png"),
        width = dims$width,
        height = dims$height,
        dpi = 300
      )
      metric_figures[[metric]][[plot_type]] <- paths
      max_groups <- max(max_groups, built$n_groups)
      any_horizontal <- any_horizontal || isTRUE(built$horizontal)
    }
  }

  overview_paths <- list()
  for (plot_type in plot_types) {
    overview <- build_alpha_overview_plot(alpha_tbl, group_var, metrics, metric_labels, plot_type = plot_type)
    overview_dims <- get_alpha_plot_dims(overview$n_groups, overview = TRUE)
    overview_paths[[plot_type]] <- save_plot_pdf_png(
      overview$plot,
      alpha_overview_figure_path(job_dir, plot_type, "pdf"),
      alpha_overview_figure_path(job_dir, plot_type, "png"),
      width = overview_dims$width,
      height = overview_dims$height,
      dpi = 300
    )
  }

  alpha_warning <- NULL
  if (max_groups > 8) {
    alpha_warning <- paste0(
      "当前分组变量包含 ", max_groups,
      " 个水平，图形已自动切换为横向显示。若这些水平是多个实验因素的组合，建议选择单一、具有明确研究含义的分组变量。"
    )
    warning(alpha_warning, call. = FALSE)
  }

  shannon_paths <- (metric_figures$Shannon %||% metric_figures[[1]])$boxplot
  list(
    alpha_table_path = normalizePath(out_alpha, winslash = "/", mustWork = TRUE),
    alpha_stats_path = normalizePath(out_stats, winslash = "/", mustWork = TRUE),
    figure_paths = shannon_paths,
    metric_figure_paths = metric_figures,
    overview_figure_paths = overview_paths,
    metrics = metrics,
    plot_types = plot_types,
    n_groups = max_groups,
    alpha_plot_warning = alpha_warning,
    alpha_plot_layout = if (any_horizontal) "horizontal" else "vertical"
  )
}
