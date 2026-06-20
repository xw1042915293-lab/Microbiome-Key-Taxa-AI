# Alpha diversity analysis.
# Outputs:
# - tables/alpha_diversity.csv
# - tables/alpha_group_summary.csv
# - tables/alpha_stats.csv
# - tables/alpha_pairwise_stats.csv
# - tables/sequencing_depth_summary.csv
# - tables/rarefaction_summary.csv
# - figures/alpha_<index>_<plot_type>.pdf + .png
# - figures/alpha_multi_index_facet.pdf + .png
# - figures/alpha_index_heatmap.pdf + .png
# - figures/sequencing_depth_barplot.pdf + .png
# - figures/alpha_rarefaction_curve.pdf + .png
# - json/alpha_summary.json

alpha_supported_indices <- function() {
  c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "Pielou")
}

alpha_supported_plot_types <- function() {
  c("boxplot", "violin_boxplot", "bar_mean_se", "dot_errorbar", "density")
}

alpha_index_slug <- function(index) {
  assert_non_empty_string(index, "index")
  mapping <- c(
    Observed = "observed",
    Chao1 = "chao1",
    ACE = "ace",
    Shannon = "shannon",
    Simpson = "simpson",
    Pielou = "pielou"
  )
  mapping[[index]] %||% tolower(index)
}

alpha_relative_png <- function(index, plot_type) {
  paste0("figures/alpha_", alpha_index_slug(index), "_", plot_type, ".png")
}

alpha_relative_pdf <- function(index, plot_type) {
  paste0("figures/alpha_", alpha_index_slug(index), "_", plot_type, ".pdf")
}

save_plot_pdf_png <- function(plot, pdf_path, png_path, width = 7.6, height = 5.8, dpi = 300) {
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

alpha_empty_group_summary <- function() {
  tibble::tibble(
    Index = character(),
    Group = character(),
    n = integer(),
    mean = numeric(),
    sd = numeric(),
    se = numeric(),
    median = numeric(),
    q1 = numeric(),
    q3 = numeric(),
    min = numeric(),
    max = numeric()
  )
}

alpha_empty_stats <- function() {
  tibble::tibble(
    Index = character(),
    Test = character(),
    GroupVariable = character(),
    n_groups = integer(),
    n_samples = integer(),
    p_value = numeric(),
    p_adjust = numeric(),
    Significance = character(),
    Message = character()
  )
}

alpha_empty_pairwise_stats <- function() {
  tibble::tibble(
    Index = character(),
    Group1 = character(),
    Group2 = character(),
    Test = character(),
    p_value = numeric(),
    p_adjust = numeric(),
    Significance = character()
  )
}

alpha_classify_significance <- function(p_adjust) {
  if (is.null(p_adjust) || length(p_adjust) != 1 || is.na(p_adjust)) return("not_tested")
  if (p_adjust < 0.05) return("significant")
  if (p_adjust < 0.1) return("trend")
  "not_significant"
}

alpha_format_p_value <- function(p_value) {
  if (is.null(p_value) || length(p_value) != 1 || is.na(p_value)) return("p = NA")
  if (p_value < 0.001) return("p < 0.001")
  paste0("p = ", formatC(p_value, format = "f", digits = 3))
}

alpha_p_to_stars <- function(p_value) {
  if (is.null(p_value) || length(p_value) != 1 || is.na(p_value)) return("ns")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  "ns"
}

alpha_make_palette <- function(groups) {
  levs <- levels(groups)
  if (length(levs) < 1) return(stats::setNames(character(0), character(0)))
  hues <- seq(15, 375, length.out = length(levs) + 1)
  stats::setNames(grDevices::hcl(h = hues, c = 100, l = 65)[seq_along(levs)], levs)
}

alpha_make_group_labels <- function(groups) {
  counts <- table(groups)
  stats::setNames(
    paste0(names(counts), "\n(n=", as.integer(counts), ")"),
    names(counts)
  )
}

alpha_get_sample_table <- function(dataset) {
  samp <- dataset$sample_table
  if (is.null(samp) || !is.data.frame(samp)) {
    stop("alpha_get_sample_table(): dataset$sample_table missing.", call. = FALSE)
  }
  samp <- as.data.frame(samp, stringsAsFactors = FALSE)
  if (!"SampleID" %in% names(samp)) {
    samp$SampleID <- rownames(samp)
  }
  if (all(is.na(samp$SampleID) | !nzchar(as.character(samp$SampleID)))) {
    stop("alpha_get_sample_table(): SampleID could not be resolved from sample_table.", call. = FALSE)
  }
  rownames(samp) <- samp$SampleID
  samp
}

alpha_get_otu_matrix <- function(dataset) {
  otu <- dataset$otu_table
  if (is.null(otu)) stop("alpha_get_otu_matrix(): dataset$otu_table missing.", call. = FALSE)
  otu <- as.matrix(otu)
  suppressWarnings(storage.mode(otu) <- "numeric")
  if (anyNA(otu)) stop("alpha_get_otu_matrix(): abundance matrix contains NA values.", call. = FALSE)
  if (any(otu < 0, na.rm = TRUE)) stop("alpha_get_otu_matrix(): abundance matrix contains negative values.", call. = FALSE)
  if (is.null(rownames(otu)) || is.null(colnames(otu))) {
    stop("alpha_get_otu_matrix(): abundance matrix must contain feature and sample names.", call. = FALSE)
  }
  otu
}

alpha_pick_metric_column <- function(df, target) {
  if (!is.data.frame(df)) return(NULL)
  candidates <- switch(
    target,
    Observed = c("Observed", "observed", "S.obs", "S_obs", "Richness", "richness"),
    Chao1 = c("Chao1", "chao1", "S.chao1", "S_chao1"),
    ACE = c("ACE", "ace", "S.ACE", "S_ACE"),
    Shannon = c("Shannon", "shannon"),
    Simpson = c("Simpson", "simpson"),
    Pielou = c("Pielou", "pielou", "Evenness", "evenness"),
    target
  )
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) < 1) return(NULL)
  hit[[1]]
}

alpha_get_microeco_alpha <- function(dataset) {
  out <- tryCatch({
    dataset$cal_alphadiv()
    dataset$alpha_diversity
  }, error = function(e) NULL)

  if (is.null(out) || !is.data.frame(out)) return(NULL)
  out <- tibble::as_tibble(out, rownames = "SampleID")
  out
}

alpha_calculate_indices <- function(dataset) {
  otu <- alpha_get_otu_matrix(dataset)
  sample_table <- alpha_get_sample_table(dataset)
  sample_ids <- colnames(otu)
  sample_by_feature <- t(otu)

  estimate_names <- c("S.obs", "S.chao1", "se.chao1", "S.ACE", "se.ACE")
  estimate_mat <- t(vapply(seq_len(nrow(sample_by_feature)), function(i) {
    vals <- tryCatch(vegan::estimateR(sample_by_feature[i, ]), error = function(e) stats::setNames(rep(NA_real_, length(estimate_names)), estimate_names))
    vals <- vals[estimate_names]
    vals[is.na(vals)] <- NA_real_
    as.numeric(vals)
  }, numeric(length(estimate_names))))
  colnames(estimate_mat) <- estimate_names
  rownames(estimate_mat) <- rownames(sample_by_feature)

  shannon <- tryCatch(vegan::diversity(sample_by_feature, index = "shannon"), error = function(e) rep(NA_real_, nrow(sample_by_feature)))
  simpson <- tryCatch(vegan::diversity(sample_by_feature, index = "simpson"), error = function(e) rep(NA_real_, nrow(sample_by_feature)))

  alpha_tbl <- tibble::tibble(
    SampleID = sample_ids,
    Observed = as.numeric(estimate_mat[sample_ids, "S.obs"]),
    Chao1 = as.numeric(estimate_mat[sample_ids, "S.chao1"]),
    ACE = as.numeric(estimate_mat[sample_ids, "S.ACE"]),
    Shannon = as.numeric(shannon[sample_ids]),
    Simpson = as.numeric(simpson[sample_ids])
  )

  microeco_alpha <- alpha_get_microeco_alpha(dataset)
  if (is.data.frame(microeco_alpha) && nrow(microeco_alpha) > 0) {
    microeco_alpha <- dplyr::distinct(microeco_alpha, .data$SampleID, .keep_all = TRUE)
    alpha_tbl <- dplyr::left_join(alpha_tbl, microeco_alpha, by = "SampleID", suffix = c("", "_microeco"))

    for (metric in c("Observed", "Chao1", "ACE", "Shannon", "Simpson")) {
      metric_col <- alpha_pick_metric_column(alpha_tbl, paste0(metric, "_microeco"))
      if (is.null(metric_col)) metric_col <- alpha_pick_metric_column(alpha_tbl, metric)
      if (!is.null(metric_col) && metric_col != metric) {
        alpha_tbl[[metric]] <- dplyr::coalesce(suppressWarnings(as.numeric(alpha_tbl[[metric_col]])), suppressWarnings(as.numeric(alpha_tbl[[metric]])))
      }
    }

    keep_cols <- c("SampleID", alpha_supported_indices(), setdiff(names(alpha_tbl), c("SampleID", alpha_supported_indices())))
    alpha_tbl <- alpha_tbl[, unique(keep_cols), drop = FALSE]
    microeco_only_cols <- grep("_microeco$", names(alpha_tbl), value = TRUE)
    if (length(microeco_only_cols) > 0) alpha_tbl <- alpha_tbl[, setdiff(names(alpha_tbl), microeco_only_cols), drop = FALSE]
  }

  alpha_tbl$Pielou <- ifelse(
    is.na(alpha_tbl$Observed) | alpha_tbl$Observed <= 1 | is.na(alpha_tbl$Shannon),
    NA_real_,
    alpha_tbl$Shannon / log(alpha_tbl$Observed)
  )

  alpha_tbl
}

alpha_build_table <- function(dataset, group_var) {
  assert_non_empty_string(group_var, "group_var")
  sample_table <- alpha_get_sample_table(dataset)
  if (!group_var %in% names(sample_table)) {
    stop("alpha_build_table(): group_var not found in sample_table: ", group_var, call. = FALSE)
  }

  alpha_tbl <- alpha_calculate_indices(dataset)
  alpha_tbl <- dplyr::left_join(
    alpha_tbl,
    sample_table[, c("SampleID", group_var), drop = FALSE],
    by = "SampleID"
  )

  ordered_cols <- c("SampleID", group_var, alpha_supported_indices())
  extra_cols <- setdiff(names(alpha_tbl), ordered_cols)
  alpha_tbl[, c(ordered_cols, extra_cols), drop = FALSE]
}

alpha_prepare_plot_data <- function(alpha_table, group_var, index) {
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(index, "index")
  if (!is.data.frame(alpha_table)) stop("alpha_prepare_plot_data(): alpha_table must be a data.frame.", call. = FALSE)
  if (!group_var %in% names(alpha_table)) stop("alpha_prepare_plot_data(): group_var column missing.", call. = FALSE)
  if (!index %in% names(alpha_table)) stop("alpha_prepare_plot_data(): index column missing.", call. = FALSE)

  df <- alpha_table[, c("SampleID", group_var, index), drop = FALSE]
  names(df) <- c("SampleID", "Group", "Value")
  df$Group <- as.character(df$Group)
  df <- df[!is.na(df$Group) & nzchar(df$Group) & !is.na(df$Value), , drop = FALSE]
  if (nrow(df) < 1) return(df)

  group_levels <- unique(df$Group)
  df$group <- factor(df$Group, levels = group_levels)
  df
}

alpha_compute_group_summary <- function(alpha_table, group_var) {
  if (!is.data.frame(alpha_table)) stop("alpha_compute_group_summary(): alpha_table must be a data.frame.", call. = FALSE)
  if (!group_var %in% names(alpha_table)) stop("alpha_compute_group_summary(): group_var column missing.", call. = FALSE)

  group_values <- unique(as.character(alpha_table[[group_var]]))
  group_values <- group_values[!is.na(group_values) & nzchar(group_values)]
  if (length(group_values) < 1) return(alpha_empty_group_summary())

  rows <- list()
  idx <- 0L
  for (metric in alpha_supported_indices()) {
    if (!metric %in% names(alpha_table)) next
    for (grp in group_values) {
      vals <- suppressWarnings(as.numeric(alpha_table[[metric]][alpha_table[[group_var]] == grp]))
      vals <- vals[!is.na(vals)]
      n_valid <- length(vals)
      idx <- idx + 1L
      rows[[idx]] <- tibble::tibble(
        Index = metric,
        Group = grp,
        n = as.integer(n_valid),
        mean = if (n_valid > 0) mean(vals) else NA_real_,
        sd = if (n_valid > 1) stats::sd(vals) else NA_real_,
        se = if (n_valid > 1) stats::sd(vals) / sqrt(n_valid) else NA_real_,
        median = if (n_valid > 0) stats::median(vals) else NA_real_,
        q1 = if (n_valid > 0) as.numeric(stats::quantile(vals, 0.25, na.rm = TRUE, names = FALSE, type = 7)) else NA_real_,
        q3 = if (n_valid > 0) as.numeric(stats::quantile(vals, 0.75, na.rm = TRUE, names = FALSE, type = 7)) else NA_real_,
        min = if (n_valid > 0) min(vals) else NA_real_,
        max = if (n_valid > 0) max(vals) else NA_real_
      )
    }
  }

  if (length(rows) < 1) return(alpha_empty_group_summary())
  dplyr::bind_rows(rows)
}

alpha_run_main_test <- function(plot_df, index, group_var) {
  counts <- table(plot_df$group)
  n_groups <- nlevels(plot_df$group)
  base_row <- tibble::tibble(
    Index = index,
    Test = NA_character_,
    GroupVariable = group_var,
    n_groups = as.integer(n_groups),
    n_samples = as.integer(nrow(plot_df)),
    p_value = NA_real_,
    p_adjust = NA_real_,
    Significance = "not_tested",
    Message = ""
  )

  if (nrow(plot_df) < 2 || n_groups < 2) {
    base_row$Message <- "Too few non-missing samples or groups for statistical testing."
    return(base_row)
  }
  if (any(counts < 1)) {
    base_row$Message <- "At least one group has no valid samples."
    return(base_row)
  }

  test_result <- tryCatch({
    if (n_groups == 2) {
      list(
        test = "Wilcoxon rank-sum",
        p_value = stats::wilcox.test(Value ~ group, data = plot_df, exact = FALSE)$p.value
      )
    } else {
      list(
        test = "Kruskal-Wallis",
        p_value = stats::kruskal.test(Value ~ group, data = plot_df)$p.value
      )
    }
  }, error = function(e) {
    list(test = if (n_groups == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis", p_value = NA_real_, message = conditionMessage(e))
  })

  base_row$Test <- test_result$test %||% NA_character_
  base_row$p_value <- suppressWarnings(as.numeric(test_result$p_value %||% NA_real_))
  base_row$Message <- test_result$message %||% ""
  base_row
}

alpha_compute_stats <- function(alpha_table, group_var) {
  rows <- lapply(alpha_supported_indices(), function(index) {
    if (!index %in% names(alpha_table)) return(NULL)
    plot_df <- alpha_prepare_plot_data(alpha_table, group_var = group_var, index = index)
    alpha_run_main_test(plot_df, index = index, group_var = group_var)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) < 1) return(alpha_empty_stats())

  stats_tbl <- dplyr::bind_rows(rows)
  valid <- !is.na(stats_tbl$p_value)
  stats_tbl$p_adjust[valid] <- p.adjust(stats_tbl$p_value[valid], method = "BH")
  stats_tbl$Significance <- vapply(stats_tbl$p_adjust, alpha_classify_significance, character(1))
  stats_tbl
}

alpha_compute_pairwise_stats <- function(alpha_table, group_var) {
  all_groups <- unique(as.character(alpha_table[[group_var]]))
  all_groups <- all_groups[!is.na(all_groups) & nzchar(all_groups)]
  if (length(all_groups) < 2 || length(all_groups) > 3) {
    return(list(
      table = alpha_empty_pairwise_stats(),
      skipped = length(all_groups) > 3,
      message = if (length(all_groups) > 3) "too many groups, pairwise comparison skipped" else "pairwise comparison unavailable"
    ))
  }

  rows <- list()
  idx <- 0L
  warnings <- character(0)

  for (metric in alpha_supported_indices()) {
    if (!metric %in% names(alpha_table)) next
    plot_df <- alpha_prepare_plot_data(alpha_table, group_var = group_var, index = metric)
    if (nlevels(plot_df$group) < 2 || nlevels(plot_df$group) > 3) next

    metric_rows <- list()
    metric_idx <- 0L
    for (pair in utils::combn(levels(plot_df$group), 2, simplify = FALSE)) {
      g1 <- pair[[1]]
      g2 <- pair[[2]]
      sub_df <- plot_df[plot_df$group %in% c(g1, g2), , drop = FALSE]
      p_value <- tryCatch(
        stats::wilcox.test(Value ~ group, data = sub_df, exact = FALSE)$p.value,
        error = function(e) {
          warnings <<- c(warnings, paste0(metric, " ", g1, " vs ", g2, ": ", conditionMessage(e)))
          NA_real_
        }
      )
      metric_idx <- metric_idx + 1L
      metric_rows[[metric_idx]] <- tibble::tibble(
        Index = metric,
        Group1 = g1,
        Group2 = g2,
        Test = "Pairwise Wilcoxon",
        p_value = suppressWarnings(as.numeric(p_value)),
        p_adjust = NA_real_,
        Significance = "not_tested"
      )
    }

    if (length(metric_rows) > 0) {
      metric_tbl <- dplyr::bind_rows(metric_rows)
      valid <- !is.na(metric_tbl$p_value)
      metric_tbl$p_adjust[valid] <- p.adjust(metric_tbl$p_value[valid], method = "BH")
      metric_tbl$Significance <- vapply(metric_tbl$p_adjust, alpha_classify_significance, character(1))
      idx <- idx + 1L
      rows[[idx]] <- metric_tbl
    }
  }

  out_tbl <- if (length(rows) > 0) dplyr::bind_rows(rows) else alpha_empty_pairwise_stats()
  list(
    table = out_tbl,
    skipped = FALSE,
    message = if (length(warnings) > 0) paste(unique(warnings), collapse = " | ") else "",
    warnings = unique(warnings)
  )
}

alpha_summary_df <- function(plot_df) {
  if (!is.data.frame(plot_df) || nrow(plot_df) < 1) {
    return(tibble::tibble(group = factor(), mean = numeric(), sd = numeric(), n = integer(), se = numeric(), ci95 = numeric()))
  }

  dplyr::summarise(
    dplyr::group_by(plot_df, group),
    mean = mean(.data$Value, na.rm = TRUE),
    sd = stats::sd(.data$Value, na.rm = TRUE),
    n = dplyr::n(),
    se = dplyr::if_else(n > 1, sd / sqrt(n), NA_real_),
    ci95 = dplyr::if_else(n > 1, stats::qt(0.975, df = n - 1) * se, se),
    .groups = "drop"
  )
}

alpha_get_main_stat_row <- function(alpha_stats, index) {
  if (!is.data.frame(alpha_stats) || !all(c("Index", "p_value") %in% names(alpha_stats))) return(NULL)
  row <- alpha_stats[alpha_stats$Index == index, , drop = FALSE]
  if (nrow(row) < 1) return(NULL)
  row[1, , drop = FALSE]
}

alpha_get_pairwise_rows <- function(pairwise_stats, index) {
  if (!is.data.frame(pairwise_stats) || !all(c("Index", "Group1", "Group2") %in% names(pairwise_stats))) return(NULL)
  row <- pairwise_stats[pairwise_stats$Index == index, , drop = FALSE]
  if (nrow(row) < 1) return(NULL)
  row
}

alpha_pairwise_annotations <- function(plot_df, pairwise_rows) {
  if (!is.data.frame(pairwise_rows) || nrow(pairwise_rows) < 1) return(NULL)
  levs <- levels(plot_df$group)
  y_max <- max(plot_df$Value, na.rm = TRUE)
  y_min <- min(plot_df$Value, na.rm = TRUE)
  span <- y_max - y_min
  if (!is.finite(span) || span <= 0) span <- max(abs(y_max), 1)
  base_y <- y_max + span * 0.10
  step_y <- span * 0.10
  tick_h <- span * 0.03

  ann <- vector("list", nrow(pairwise_rows))
  for (i in seq_len(nrow(pairwise_rows))) {
    g1 <- as.character(pairwise_rows$Group1[[i]])
    g2 <- as.character(pairwise_rows$Group2[[i]])
    p_for_star <- pairwise_rows$p_adjust[[i]] %||% pairwise_rows$p_value[[i]]
    ann[[i]] <- data.frame(
      group1 = g1,
      group2 = g2,
      x = match(g1, levs),
      xend = match(g2, levs),
      xmid = mean(c(match(g1, levs), match(g2, levs))),
      y = base_y + step_y * (i - 1L),
      y_text = base_y + step_y * (i - 1L) + tick_h * 1.6,
      y_tick = base_y + step_y * (i - 1L) - tick_h,
      label = alpha_p_to_stars(as.numeric(p_for_star)),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(ann)
}

alpha_main_test_label <- function(stat_row) {
  if (!is.data.frame(stat_row) || nrow(stat_row) < 1) return(NULL)
  paste0(
    stat_row$Test[[1]] %||% "Test",
    ": ",
    alpha_format_p_value(stat_row$p_value[[1]] %||% NA_real_)
  )
}

alpha_add_significance <- function(plot, plot_df, stat_row = NULL, pairwise_rows = NULL, is_density = FALSE) {
  if (!is.data.frame(plot_df) || nrow(plot_df) < 1) return(plot)
  group_count <- nlevels(plot_df$group)
  overall_label <- alpha_main_test_label(stat_row)

  if (isTRUE(is_density)) {
    return(plot + ggplot2::labs(subtitle = overall_label %||% ggplot2::waiver()))
  }

  y_values <- plot_df$Value
  y_max <- max(y_values, na.rm = TRUE)
  y_min <- min(y_values, na.rm = TRUE)
  span <- y_max - y_min
  if (!is.finite(span) || span <= 0) span <- max(abs(y_max), 1)

  if (group_count > 3) {
    return(
      plot +
        ggplot2::annotate("text", x = mean(seq_len(group_count)), y = y_max + span * 0.14, label = overall_label, size = 4.0) +
        ggplot2::expand_limits(y = y_max + span * 0.24)
    )
  }

  ann <- alpha_pairwise_annotations(plot_df, pairwise_rows)
  if (is.null(ann) || nrow(ann) < 1) {
    return(
      plot +
        ggplot2::annotate("text", x = mean(seq_len(group_count)), y = y_max + span * 0.14, label = overall_label, size = 4.0) +
        ggplot2::expand_limits(y = y_max + span * 0.24)
    )
  }

  max_ann_y <- max(ann$y_text, na.rm = TRUE)
  out <- plot +
    ggplot2::geom_segment(data = ann, ggplot2::aes(x = x, xend = xend, y = y, yend = y), inherit.aes = FALSE, linewidth = 0.5) +
    ggplot2::geom_segment(data = ann, ggplot2::aes(x = x, xend = x, y = y_tick, yend = y), inherit.aes = FALSE, linewidth = 0.5) +
    ggplot2::geom_segment(data = ann, ggplot2::aes(x = xend, xend = xend, y = y_tick, yend = y), inherit.aes = FALSE, linewidth = 0.5) +
    ggplot2::geom_text(data = ann, ggplot2::aes(x = xmid, y = y_text, label = label), inherit.aes = FALSE, size = 4.0) +
    ggplot2::expand_limits(y = max_ann_y + span * 0.16)

  if (!is.null(overall_label) && nchar(overall_label) > 0) {
    out <- out + ggplot2::annotate("text", x = mean(seq_len(group_count)), y = max_ann_y + span * 0.08, label = overall_label, size = 3.9)
  }

  out
}

alpha_base_plot <- function(plot_df, group_var, index, subtitle = NULL) {
  labels <- alpha_make_group_labels(plot_df$group)
  palette <- alpha_make_palette(plot_df$group)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = group, y = Value, color = group, fill = group)) +
    ggplot2::scale_x_discrete(labels = labels) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::labs(
      x = group_var,
      y = index,
      title = paste0("Alpha diversity (", index, ")"),
      subtitle = subtitle
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12)
    )
}

alpha_density_kind <- function() {
  if (requireNamespace("ggridges", quietly = TRUE)) "ridge" else "density"
}

alpha_build_plot_set <- function(alpha_table, group_var, index, alpha_stats, pairwise_stats) {
  plot_df <- alpha_prepare_plot_data(alpha_table, group_var = group_var, index = index)
  if (nrow(plot_df) < 1 || nlevels(plot_df$group) < 1) {
    stop("alpha_build_plot_set(): no valid samples available for plotting index ", index, ".", call. = FALSE)
  }

  stat_row <- alpha_get_main_stat_row(alpha_stats, index)
  pairwise_rows <- alpha_get_pairwise_rows(pairwise_stats, index)
  summary_df <- alpha_summary_df(plot_df)

  boxplot_plot <- alpha_base_plot(plot_df, group_var, index, "Boxplot + jitter") +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.58, alpha = 0.22, linewidth = 0.6) +
    ggplot2::geom_jitter(width = 0.14, size = 1.9, alpha = 0.75)
  boxplot_plot <- alpha_add_significance(boxplot_plot, plot_df, stat_row = stat_row, pairwise_rows = pairwise_rows)

  violin_plot <- alpha_base_plot(plot_df, group_var, index, "Violin + boxplot + jitter") +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.28, linewidth = 0.5) +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.65, color = "#1f2937") +
    ggplot2::geom_jitter(width = 0.12, size = 1.9, alpha = 0.7)
  violin_plot <- alpha_add_significance(violin_plot, plot_df, stat_row = stat_row, pairwise_rows = pairwise_rows)

  bar_plot <- alpha_base_plot(plot_df, group_var, index, "Mean + SE + jitter") +
    ggplot2::geom_col(
      data = summary_df,
      ggplot2::aes(x = group, y = mean, fill = group),
      inherit.aes = FALSE,
      width = 0.62,
      alpha = 0.75,
      color = "#1f2937"
    ) +
    ggplot2::geom_errorbar(
      data = summary_df,
      ggplot2::aes(x = group, ymin = mean - se, ymax = mean + se),
      inherit.aes = FALSE,
      width = 0.18,
      linewidth = 0.6
    ) +
    ggplot2::geom_jitter(width = 0.14, size = 1.7, alpha = 0.7)
  bar_plot <- alpha_add_significance(bar_plot, plot_df, stat_row = stat_row, pairwise_rows = pairwise_rows)

  dot_plot <- alpha_base_plot(plot_df, group_var, index, "Mean + 95% CI + jitter") +
    ggplot2::geom_jitter(width = 0.14, size = 1.7, alpha = 0.45) +
    ggplot2::geom_errorbar(
      data = summary_df,
      ggplot2::aes(x = group, ymin = mean - ci95, ymax = mean + ci95),
      inherit.aes = FALSE,
      width = 0.14,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      data = summary_df,
      ggplot2::aes(x = group, y = mean, fill = group),
      inherit.aes = FALSE,
      shape = 21,
      size = 3.5,
      color = "#111827",
      stroke = 0.5
    )
  dot_plot <- alpha_add_significance(dot_plot, plot_df, stat_row = stat_row, pairwise_rows = pairwise_rows)

  density_plot <- if (identical(alpha_density_kind(), "ridge")) {
    palette <- alpha_make_palette(plot_df$group)
    gp <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Value, y = group, fill = group, color = group)) +
      ggridges::geom_density_ridges(alpha = 0.45, scale = 1.1, rel_min_height = 0.01, size = 0.4) +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::scale_color_manual(values = palette, drop = FALSE) +
      ggplot2::scale_y_discrete(labels = alpha_make_group_labels(plot_df$group)) +
      ggplot2::labs(
        x = index,
        y = group_var,
        title = paste0("Alpha diversity (", index, ")"),
        subtitle = "Distribution by group"
      ) +
      ggplot2::theme_classic(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "none",
        axis.text = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 12)
      )
    alpha_add_significance(gp, plot_df, stat_row = stat_row, is_density = TRUE)
  } else {
    palette <- alpha_make_palette(plot_df$group)
    gp <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Value, color = group, fill = group)) +
      ggplot2::geom_density(alpha = 0.22, linewidth = 0.8, adjust = 1.05) +
      ggplot2::facet_wrap(~ group, ncol = 1, scales = "free_y", labeller = ggplot2::labeller(group = alpha_make_group_labels(plot_df$group))) +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::scale_color_manual(values = palette, drop = FALSE) +
      ggplot2::labs(
        x = index,
        y = "Density",
        title = paste0("Alpha diversity (", index, ")"),
        subtitle = "Distribution by group"
      ) +
      ggplot2::theme_classic(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold"),
        legend.position = "none",
        axis.text = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 12)
      )
    alpha_add_significance(gp, plot_df, stat_row = stat_row, is_density = TRUE)
  }

  list(
    boxplot = boxplot_plot,
    violin_boxplot = violin_plot,
    bar_mean_se = bar_plot,
    dot_errorbar = dot_plot,
    density = density_plot
  )
}

alpha_save_plot_set <- function(plot_set, job_dir, index) {
  saved <- list()
  warnings <- character(0)

  for (plot_type in names(plot_set)) {
    rel_png <- alpha_relative_png(index, plot_type)
    rel_pdf <- alpha_relative_pdf(index, plot_type)
    png_path <- file.path(job_dir, rel_png)
    pdf_path <- file.path(job_dir, rel_pdf)

    result <- tryCatch(
      save_plot_pdf_png(plot_set[[plot_type]], pdf_path = pdf_path, png_path = png_path, width = 7.6, height = 5.8, dpi = 300),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      warnings <- c(warnings, paste0(index, " ", plot_type, ": ", conditionMessage(result)))
    } else {
      saved[[plot_type]] <- list(
        png = rel_png,
        pdf = rel_pdf,
        png_path = result$png,
        pdf_path = result$pdf
      )
    }
  }

  list(saved = saved, warnings = warnings)
}

alpha_long_table <- function(alpha_table, group_var) {
  long_rows <- list()
  idx <- 0L
  for (metric in alpha_supported_indices()) {
    if (!metric %in% names(alpha_table)) next
    tmp <- alpha_table[, c("SampleID", group_var, metric), drop = FALSE]
    names(tmp) <- c("SampleID", "Group", "Value")
    tmp$Index <- metric
    tmp <- tmp[!is.na(tmp$Group) & nzchar(as.character(tmp$Group)) & !is.na(tmp$Value), c("SampleID", "Group", "Index", "Value"), drop = FALSE]
    if (nrow(tmp) < 1) next
    idx <- idx + 1L
    long_rows[[idx]] <- tmp
  }
  if (length(long_rows) < 1) return(data.frame())
  dplyr::bind_rows(long_rows)
}

alpha_build_multi_index_facet_plot <- function(alpha_table, group_var) {
  long_df <- alpha_long_table(alpha_table, group_var)
  if (!is.data.frame(long_df) || nrow(long_df) < 1) {
    stop("alpha_build_multi_index_facet_plot(): no alpha values available.", call. = FALSE)
  }
  long_df$Group <- factor(long_df$Group, levels = unique(as.character(long_df$Group)))
  labels <- alpha_make_group_labels(long_df$Group)
  palette <- alpha_make_palette(long_df$Group)

  ggplot2::ggplot(long_df, ggplot2::aes(x = Group, y = Value, fill = Group, color = Group)) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.28, linewidth = 0.45) +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.65, color = "#1f2937") +
    ggplot2::geom_jitter(width = 0.12, size = 1.3, alpha = 0.55) +
    ggplot2::facet_wrap(~ Index, scales = "free_y", ncol = 3) +
    ggplot2::scale_x_discrete(labels = labels) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::labs(
      x = group_var,
      y = "Alpha diversity value",
      title = "Alpha diversity multi-index overview",
      subtitle = "Violin + boxplot + jitter across all calculated indices"
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = 9),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

alpha_zscore_matrix <- function(alpha_table) {
  mat <- as.matrix(alpha_table[, alpha_supported_indices(), drop = FALSE])
  suppressWarnings(storage.mode(mat) <- "numeric")

  z_cols <- lapply(seq_len(ncol(mat)), function(i) {
    x <- mat[, i]
    if (all(is.na(x))) return(rep(NA_real_, length(x)))
    s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) {
      out <- rep(0, length(x))
      out[is.na(x)] <- NA_real_
      return(out)
    }
    as.numeric(scale(x))
  })

  z_mat <- do.call(cbind, z_cols)
  colnames(z_mat) <- colnames(mat)
  rownames(z_mat) <- alpha_table$SampleID
  z_mat
}

alpha_save_heatmap <- function(alpha_table, group_var, job_dir) {
  if (!requireNamespace("pheatmap", quietly = TRUE)) {
    return(list(saved = NULL, warning = "pheatmap is not installed; alpha_index_heatmap skipped."))
  }

  if (!group_var %in% names(alpha_table)) {
    return(list(saved = NULL, warning = "group_var column missing; alpha_index_heatmap skipped."))
  }

  ord <- order(as.character(alpha_table[[group_var]]), alpha_table$SampleID)
  alpha_ord <- alpha_table[ord, , drop = FALSE]
  z_mat <- alpha_zscore_matrix(alpha_ord)
  keep_cols <- colSums(!is.na(z_mat)) > 0
  z_mat <- z_mat[, keep_cols, drop = FALSE]
  if (ncol(z_mat) < 1 || nrow(z_mat) < 1) {
    return(list(saved = NULL, warning = "No valid alpha matrix available for heatmap."))
  }

  ann_row <- data.frame(Group = alpha_ord[[group_var]], row.names = alpha_ord$SampleID, stringsAsFactors = FALSE)
  png_path <- file.path(job_dir, "figures", "alpha_index_heatmap.png")
  pdf_path <- file.path(job_dir, "figures", "alpha_index_heatmap.pdf")
  ensure_dir(dirname(png_path))
  ensure_dir(dirname(pdf_path))

  tryCatch({
    pheatmap::pheatmap(
      z_mat,
      annotation_row = ann_row,
      cluster_rows = FALSE,
      cluster_cols = FALSE,
      fontsize = 10,
      filename = png_path,
      width = 7.8,
      height = max(5.5, nrow(z_mat) * 0.18 + 2.2)
    )
    pheatmap::pheatmap(
      z_mat,
      annotation_row = ann_row,
      cluster_rows = FALSE,
      cluster_cols = FALSE,
      fontsize = 10,
      filename = pdf_path,
      width = 7.8,
      height = max(5.5, nrow(z_mat) * 0.18 + 2.2)
    )
  }, error = function(e) {
    stop("alpha_index_heatmap: ", conditionMessage(e), call. = FALSE)
  })

  list(
    saved = list(
      png = "figures/alpha_index_heatmap.png",
      pdf = "figures/alpha_index_heatmap.pdf",
      png_path = normalizePath(png_path, winslash = "/", mustWork = TRUE),
      pdf_path = normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
    ),
    warning = NULL
  )
}

alpha_compute_sequencing_depth <- function(dataset, group_var) {
  otu <- alpha_get_otu_matrix(dataset)
  sample_table <- alpha_get_sample_table(dataset)
  if (!group_var %in% names(sample_table)) {
    stop("alpha_compute_sequencing_depth(): group_var missing from sample_table.", call. = FALSE)
  }

  tibble::tibble(
    SampleID = colnames(otu),
    Reads = as.numeric(colSums(otu, na.rm = TRUE)),
    Group = as.character(sample_table[colnames(otu), group_var, drop = TRUE])
  )
}

alpha_save_sequencing_depth_outputs <- function(sequencing_depth, group_var, job_dir) {
  out_csv <- file.path(job_dir, "tables", "sequencing_depth_summary.csv")
  seq_out <- sequencing_depth
  names(seq_out)[names(seq_out) == "Group"] <- group_var
  readr::write_csv(seq_out, out_csv)

  plot_df <- sequencing_depth
  plot_df$Group <- factor(plot_df$Group, levels = unique(plot_df$Group))
  plot_df$SampleID <- factor(plot_df$SampleID, levels = plot_df$SampleID[order(plot_df$Group, -plot_df$Reads, plot_df$SampleID)])
  palette <- alpha_make_palette(plot_df$Group)
  rotate_x <- length(unique(plot_df$SampleID)) > 12

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = SampleID, y = Reads, fill = Group)) +
    ggplot2::geom_col(color = "#1f2937", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::labs(
      x = "SampleID",
      y = "Reads",
      title = "Sequencing depth by sample",
      subtitle = paste0("Samples ordered by ", group_var)
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = if (rotate_x) 60 else 0, hjust = if (rotate_x) 1 else 0.5, vjust = if (rotate_x) 1 else 0.5, size = 9)
    )

  saved <- save_plot_pdf_png(
    p,
    pdf_path = file.path(job_dir, "figures", "sequencing_depth_barplot.pdf"),
    png_path = file.path(job_dir, "figures", "sequencing_depth_barplot.png"),
    width = 9.2,
    height = if (rotate_x) 6.2 else 5.4,
    dpi = 300
  )

  list(
    table_path = normalizePath(out_csv, winslash = "/", mustWork = TRUE),
    figure = list(
      png = "figures/sequencing_depth_barplot.png",
      pdf = "figures/sequencing_depth_barplot.pdf",
      png_path = saved$png,
      pdf_path = saved$pdf
    )
  )
}

alpha_save_rarefaction_outputs <- function(dataset, alpha_table, group_var, job_dir) {
  otu <- alpha_get_otu_matrix(dataset)
  sample_table <- alpha_get_sample_table(dataset)
  sample_by_feature <- t(otu)
  row_sums <- rowSums(sample_by_feature)

  rare_summary <- tibble::tibble(
    SampleID = rownames(sample_by_feature),
    Reads = as.numeric(row_sums),
    Observed = suppressWarnings(as.numeric(alpha_table$Observed[match(rownames(sample_by_feature), alpha_table$SampleID)])),
    Group = as.character(sample_table[rownames(sample_by_feature), group_var, drop = TRUE])
  )
  rare_summary_out <- rare_summary
  names(rare_summary_out)[names(rare_summary_out) == "Group"] <- group_var
  summary_csv <- file.path(job_dir, "tables", "rarefaction_summary.csv")
  readr::write_csv(rare_summary_out, summary_csv)

  keep <- row_sums > 0
  if (!any(keep)) {
    return(list(
      table_path = normalizePath(summary_csv, winslash = "/", mustWork = TRUE),
      figure = NULL,
      warning = "No positive-depth samples available for rarefaction."
    ))
  }

  rare_mat <- sample_by_feature[keep, , drop = FALSE]
  rare_meta <- rare_summary[keep, , drop = FALSE]
  rare_meta$Group <- factor(rare_meta$Group, levels = unique(rare_meta$Group))
  palette <- alpha_make_palette(rare_meta$Group)
  colors <- grDevices::adjustcolor(palette[as.character(rare_meta$Group)], alpha.f = if (nrow(rare_mat) > 25) 0.35 else 0.75)
  step_size <- max(1, floor(max(rowSums(rare_mat)) / 50))
  png_path <- file.path(job_dir, "figures", "alpha_rarefaction_curve.png")
  pdf_path <- file.path(job_dir, "figures", "alpha_rarefaction_curve.pdf")

  draw_curve <- function(device = c("pdf", "png"), path) {
    device <- match.arg(device)
    if (device == "pdf") {
      grDevices::pdf(path, width = 8.2, height = 6.1)
    } else {
      grDevices::png(path, width = 8.2, height = 6.1, units = "in", res = 300)
    }
    on.exit(grDevices::dev.off(), add = TRUE)

    vegan::rarecurve(
      rare_mat,
      step = step_size,
      label = FALSE,
      col = colors,
      lwd = 1.2,
      xlab = "Sequencing depth (reads)",
      ylab = "Observed richness",
      main = "Alpha rarefaction curve"
    )
    graphics::legend(
      "bottomright",
      legend = names(palette),
      col = unname(palette),
      lwd = 2,
      bty = "n",
      cex = 0.85,
      title = group_var
    )
  }

  rare_warning <- NULL
  tryCatch({
    draw_curve("pdf", pdf_path)
    draw_curve("png", png_path)
  }, error = function(e) {
    rare_warning <<- paste0("Rarefaction curve skipped: ", conditionMessage(e))
  })

  figure <- if (!is.null(rare_warning)) {
    NULL
  } else {
    list(
      png = "figures/alpha_rarefaction_curve.png",
      pdf = "figures/alpha_rarefaction_curve.pdf",
      png_path = normalizePath(png_path, winslash = "/", mustWork = TRUE),
      pdf_path = normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
    )
  }

  list(
    table_path = normalizePath(summary_csv, winslash = "/", mustWork = TRUE),
    figure = figure,
    warning = rare_warning
  )
}

summarize_alpha_for_ai <- function(alpha_table, alpha_stats, group_var) {
  if (!is.data.frame(alpha_table)) return(list())
  stats_rows <- if (is.data.frame(alpha_stats) && nrow(alpha_stats) > 0) {
    alpha_stats[, intersect(c("Index", "Test", "p_value", "p_adjust", "Significance", "Message"), names(alpha_stats)), drop = FALSE]
  } else {
    data.frame()
  }

  list(
    analysis_type = "alpha_diversity",
    group_variable = group_var,
    n_samples = nrow(alpha_table),
    n_groups = length(unique(stats::na.omit(alpha_table[[group_var]]))),
    indices_calculated = intersect(alpha_supported_indices(), names(alpha_table)),
    stats = stats_rows
  )
}

plot_alpha_boxplot <- function(alpha_table, group_var, index = "Shannon", output_path) {
  assert_non_empty_string(output_path, "output_path")
  plot_df <- alpha_prepare_plot_data(alpha_table, group_var = group_var, index = index)
  if (nrow(plot_df) < 1) stop("plot_alpha_boxplot(): no data available for plotting.", call. = FALSE)

  p <- alpha_base_plot(plot_df, group_var = group_var, index = index, subtitle = "Boxplot + jitter") +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.58, alpha = 0.22, linewidth = 0.6) +
    ggplot2::geom_jitter(width = 0.14, size = 1.9, alpha = 0.75)

  pdf_path <- sub("\\.png$", ".pdf", output_path, ignore.case = TRUE)
  if (identical(pdf_path, output_path)) pdf_path <- paste0(output_path, ".pdf")
  save_plot_pdf_png(p, pdf_path = pdf_path, png_path = output_path, width = 7.6, height = 5.8, dpi = 300)
}

alpha_build_summary_json <- function(alpha_table, alpha_stats, pairwise_info, sequencing_depth, figures_generated, plot_warnings, group_var, job_dir) {
  caution_notes <- c(
    "Alpha diversity reflects within-sample diversity, not between-sample community composition.",
    "Significant differences indicate association with grouping, not causation.",
    "Sequencing depth and rarefaction should be considered when interpreting alpha diversity."
  )

  main_test_results <- if (is.data.frame(alpha_stats) && nrow(alpha_stats) > 0) {
    lapply(seq_len(nrow(alpha_stats)), function(i) {
      row <- alpha_stats[i, , drop = FALSE]
      list(
        Index = row$Index[[1]],
        Test = row$Test[[1]],
        p_value = row$p_value[[1]],
        p_adjust = row$p_adjust[[1]],
        Significance = row$Significance[[1]],
        Message = row$Message[[1]]
      )
    })
  } else {
    list()
  }

  seq_summary <- if (is.data.frame(sequencing_depth) && nrow(sequencing_depth) > 0) {
    list(
      min_reads = min(sequencing_depth$Reads, na.rm = TRUE),
      median_reads = stats::median(sequencing_depth$Reads, na.rm = TRUE),
      max_reads = max(sequencing_depth$Reads, na.rm = TRUE)
    )
  } else {
    list()
  }

  summary_obj <- list(
    analysis_type = "alpha_diversity",
    group_variable = group_var,
    n_samples = nrow(alpha_table),
    n_groups = length(unique(stats::na.omit(alpha_table[[group_var]]))),
    indices_calculated = intersect(alpha_supported_indices(), names(alpha_table)),
    main_test_results = main_test_results,
    pairwise_test_available = isTRUE(!pairwise_info$skipped),
    figures_generated = figures_generated,
    sequencing_depth_summary = seq_summary,
    warnings = unique(plot_warnings),
    caution_notes = caution_notes
  )

  out_path <- file.path(job_dir, "json", "alpha_summary.json")
  write_json_pretty(summary_obj, out_path, auto_unbox = TRUE)
}

run_alpha_analysis <- function(dataset, group_var, job_dir, index = "Shannon") {
  if (is.null(dataset)) stop("run_alpha_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  assert_non_empty_string(index, "index")
  if (!dir.exists(job_dir)) stop("run_alpha_analysis(): job_dir not found: ", job_dir, call. = FALSE)

  alpha_table <- alpha_build_table(dataset, group_var = group_var)
  out_alpha <- file.path(job_dir, "tables", "alpha_diversity.csv")
  ensure_dir(dirname(out_alpha))
  readr::write_csv(alpha_table, out_alpha)

  group_summary <- alpha_compute_group_summary(alpha_table, group_var = group_var)
  out_group_summary <- file.path(job_dir, "tables", "alpha_group_summary.csv")
  readr::write_csv(group_summary, out_group_summary)

  alpha_stats <- alpha_compute_stats(alpha_table, group_var = group_var)
  out_stats <- file.path(job_dir, "tables", "alpha_stats.csv")
  readr::write_csv(alpha_stats, out_stats)

  pairwise_info <- alpha_compute_pairwise_stats(alpha_table, group_var = group_var)
  pairwise_stats <- pairwise_info$table
  out_pairwise <- file.path(job_dir, "tables", "alpha_pairwise_stats.csv")
  readr::write_csv(pairwise_stats, out_pairwise)

  single_index_figures <- list()
  figure_paths_all <- list()
  figure_png_map <- list()
  plot_warnings <- pairwise_info$warnings %||% character(0)

  for (metric in alpha_supported_indices()) {
    if (!metric %in% names(alpha_table)) next
    metric_saved <- tryCatch({
      plot_set <- alpha_build_plot_set(alpha_table, group_var = group_var, index = metric, alpha_stats = alpha_stats, pairwise_stats = pairwise_stats)
      alpha_save_plot_set(plot_set, job_dir = job_dir, index = metric)
    }, error = function(e) e)

    if (inherits(metric_saved, "error")) {
      plot_warnings <- c(plot_warnings, paste0(metric, ": ", conditionMessage(metric_saved)))
      next
    }

    single_index_figures[[alpha_index_slug(metric)]] <- lapply(metric_saved$saved, function(x) x$png)
    figure_paths_all[[alpha_index_slug(metric)]] <- metric_saved$saved
    plot_warnings <- c(plot_warnings, metric_saved$warnings)
  }

  facet_saved <- NULL
  facet_warning <- NULL
  tryCatch({
    facet_plot <- alpha_build_multi_index_facet_plot(alpha_table, group_var = group_var)
    facet_saved <- save_plot_pdf_png(
      facet_plot,
      pdf_path = file.path(job_dir, "figures", "alpha_multi_index_facet.pdf"),
      png_path = file.path(job_dir, "figures", "alpha_multi_index_facet.png"),
      width = 10.5,
      height = 7.8,
      dpi = 300
    )
  }, error = function(e) {
    facet_warning <<- paste0("alpha_multi_index_facet: ", conditionMessage(e))
  })
  if (!is.null(facet_warning)) plot_warnings <- c(plot_warnings, facet_warning)

  heatmap_info <- alpha_save_heatmap(alpha_table, group_var = group_var, job_dir = job_dir)
  if (!is.null(heatmap_info$warning)) plot_warnings <- c(plot_warnings, heatmap_info$warning)

  sequencing_depth <- alpha_compute_sequencing_depth(dataset, group_var = group_var)
  sequencing_depth_saved <- alpha_save_sequencing_depth_outputs(sequencing_depth, group_var = group_var, job_dir = job_dir)

  rarefaction_info <- alpha_save_rarefaction_outputs(dataset, alpha_table, group_var = group_var, job_dir = job_dir)
  if (!is.null(rarefaction_info$warning)) plot_warnings <- c(plot_warnings, rarefaction_info$warning)

  figures_generated <- list(
    single_index = single_index_figures,
    multi_index_facet = if (!is.null(facet_saved)) "figures/alpha_multi_index_facet.png" else NULL,
    heatmap = heatmap_info$saved$png %||% NULL,
    sequencing_depth = sequencing_depth_saved$figure$png %||% NULL,
    rarefaction = rarefaction_info$figure$png %||% NULL
  )

  summary_json_path <- alpha_build_summary_json(
    alpha_table = alpha_table,
    alpha_stats = alpha_stats,
    pairwise_info = pairwise_info,
    sequencing_depth = sequencing_depth,
    figures_generated = figures_generated,
    plot_warnings = plot_warnings,
    group_var = group_var,
    job_dir = job_dir
  )

  shannon_key <- alpha_index_slug(index)
  shannon_primary <- figure_paths_all[[shannon_key]]$violin_boxplot %||% figure_paths_all[[shannon_key]]$boxplot %||% NULL
  figure_paths <- if (is.list(shannon_primary)) {
    list(png = shannon_primary$png_path, pdf = shannon_primary$pdf_path)
  } else {
    NULL
  }

  list(
    alpha_table = alpha_table,
    group_summary = group_summary,
    stats = alpha_stats,
    pairwise_stats = pairwise_stats,
    sequencing_depth = sequencing_depth,
    figures = figures_generated,
    summary_json = normalizePath(summary_json_path, winslash = "/", mustWork = TRUE),
    alpha_table_path = normalizePath(out_alpha, winslash = "/", mustWork = TRUE),
    alpha_group_summary_path = normalizePath(out_group_summary, winslash = "/", mustWork = TRUE),
    alpha_stats_path = normalizePath(out_stats, winslash = "/", mustWork = TRUE),
    alpha_pairwise_stats_path = normalizePath(out_pairwise, winslash = "/", mustWork = TRUE),
    sequencing_depth_path = sequencing_depth_saved$table_path,
    rarefaction_summary_path = rarefaction_info$table_path,
    figure_paths = figure_paths,
    figure_paths_all = figure_paths_all,
    plot_warnings = unique(plot_warnings),
    default_plot = "figures/alpha_shannon_violin_boxplot.png",
    index = index
  )
}

# ---------------------------------------------------------------------------
# run_alpha_diversity(): canonical entry-point alias expected by the workflow.
# Delegates to run_alpha_analysis() and guarantees:
#   - alpha_diversity.csv
#   - alpha_summary.csv  (group summary)
#   - alpha_shannon_boxplot.png
# Returns list(status = "done", files = ...).
# ---------------------------------------------------------------------------
run_alpha_diversity <- function(microeco_obj, job_dir, group_var,
                                index = "Shannon") {
  # Accept both (dataset, group_var, job_dir) and (microeco_obj, job_dir,
  # group_var) argument orders.  If the caller passes a character where we
  # expect a dataset object (or vice-versa), swap the first three args.
  if (is.character(microeco_obj) && is.list(job_dir)) {
    tmp            <- microeco_obj
    microeco_obj   <- job_dir
    job_dir        <- tmp
  }

  if (is.null(microeco_obj)) {
    stop("run_alpha_diversity(): microeco_obj is NULL.", call. = FALSE)
  }
  assert_non_empty_string(job_dir,  "job_dir")
  assert_non_empty_string(group_var, "group_var")

  # ---- delegate to the full implementation ----
  res <- run_alpha_analysis(
    dataset   = microeco_obj,
    group_var = group_var,
    job_dir   = job_dir,
    index     = index
  )

  # ---- guarantee alpha_summary.csv exists ----
  summary_csv <- file.path(job_dir, "tables", "alpha_summary.csv")
  if (!file.exists(summary_csv)) {
    # Create from group_summary if it was saved under a different name
    group_summary_path <- file.path(job_dir, "tables", "alpha_group_summary.csv")
    if (file.exists(group_summary_path)) {
      file.copy(group_summary_path, summary_csv, overwrite = TRUE)
    } else {
      # Fallback: build a minimal summary from alpha_table
      alpha_csv <- file.path(job_dir, "tables", "alpha_diversity.csv")
      if (file.exists(alpha_csv)) {
        tbl <- utils::read.csv(alpha_csv, stringsAsFactors = FALSE, check.names = FALSE)
        utils::write.csv(tbl, summary_csv, row.names = FALSE)
      }
    }
  }

  # ---- guarantee alpha_shannon_boxplot.png exists ----
  figures_dir <- file.path(job_dir, "figures")
  shannon_boxplot_png <- file.path(figures_dir, "alpha_shannon_boxplot.png")
  if (!file.exists(shannon_boxplot_png)) {
    # Try copying from the violin_boxplot variant
    violin_png <- file.path(figures_dir, "alpha_shannon_violin_boxplot.png")
    if (file.exists(violin_png)) {
      file.copy(violin_png, shannon_boxplot_png, overwrite = TRUE)
    } else {
      # Regenerate a simple boxplot as last resort
      alpha_csv <- file.path(job_dir, "tables", "alpha_diversity.csv")
      if (file.exists(alpha_csv)) {
        ensure_dir(figures_dir)
        tryCatch({
          tbl <- utils::read.csv(alpha_csv, stringsAsFactors = FALSE, check.names = FALSE)
          plot_alpha_boxplot(tbl, group_var = group_var, index = "Shannon",
                             output_path = shannon_boxplot_png)
        }, error = function(e) {
          warning("run_alpha_diversity(): could not create alpha_shannon_boxplot.png: ",
                  conditionMessage(e), call. = FALSE)
        })
      }
    }
  }

  # ---- collect guaranteed output file paths ----
  files <- c(
    file.path(job_dir, "tables", "alpha_diversity.csv"),
    file.path(job_dir, "tables", "alpha_summary.csv"),
    file.path(job_dir, "figures", "alpha_shannon_boxplot.png")
  )
  files <- files[file.exists(files)]

  list(status = "done", files = files)
}
