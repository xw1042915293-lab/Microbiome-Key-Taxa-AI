# Differential abundance analysis.
# Outputs:
# - tables/differential_taxa.csv
# - tables/differential_taxa_significant.csv
# - tables/diff_effect_size.csv
# - tables/diff_heatmap_matrix.csv
# - tables/diff_lefse.csv
# - tables/diff_ancombc.csv
# - tables/diff_consensus.csv
# - figures/diff_top_barplot.pdf + .png
# - figures/diff_taxa_barplot.pdf + .png
# - figures/diff_volcano.pdf + .png
# - figures/diff_heatmap.pdf + .png
# - figures/diff_lefse_lda.pdf + .png (optional)
# - json/diff_summary.json

if (!exists("clean_taxon_label", mode = "function")) {
  clean_taxon_label <- function(x) {
    if (is.null(x)) return(character(0))
    x <- as.character(x)
    vapply(x, function(s) {
      if (is.na(s)) return(NA_character_)
      s <- trimws(s)
      if (!nzchar(s)) return(s)
      parts <- strsplit(s, "\\|")[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) s else parts[[length(parts)]]
    }, character(1))
  }
}

diff_empty_results_table <- function() {
  data.frame(
    taxon = character(),
    Taxon = character(),
    tax_level = character(),
    Level = character(),
    test_method = character(),
    method = character(),
    group_variable = character(),
    GroupVariable = character(),
    comparison = character(),
    Comparison = character(),
    contrast = character(),
    group_mean_abundance = character(),
    group1 = character(),
    Group1 = character(),
    group2 = character(),
    Group2 = character(),
    mean_group1 = numeric(),
    Mean_Group1 = numeric(),
    mean_group2 = numeric(),
    Mean_Group2 = numeric(),
    mean_abundance_difference = numeric(),
    mean_abundance = numeric(),
    prevalence = numeric(),
    log2FC = numeric(),
    log2fc = numeric(),
    p_value = numeric(),
    fdr = numeric(),
    FDR = numeric(),
    significance = character(),
    Significance = character(),
    direction = character(),
    Direction = character(),
    effect_size_method = character(),
    effect_size = numeric(),
    EffectSize = numeric(),
    significant = logical(),
    trend = logical(),
    exploratory_only = logical(),
    taxon_label = character(),
    display_taxon = character(),
    display_taxon_short = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

diff_empty_effect_size_table <- function() {
  data.frame(
    taxon = character(),
    tax_level = character(),
    group_variable = character(),
    comparison = character(),
    test_method = character(),
    effect_size_method = character(),
    effect_size = numeric(),
    p_value = numeric(),
    fdr = numeric(),
    significance = character(),
    direction = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

diff_empty_consensus_table <- function() {
  data.frame(
    taxon = character(),
    display_taxon = character(),
    tax_level = character(),
    group_variable = character(),
    evidence_count = integer(),
    evidence_methods = character(),
    primary_method = logical(),
    lefse = logical(),
    ancombc = logical(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

diff_empty_lefse_table <- function() {
  data.frame(
    taxon = character(),
    display_taxon = character(),
    tax_level = character(),
    group_variable = character(),
    comparison = character(),
    enriched_group = character(),
    lda_score = numeric(),
    p_value = numeric(),
    fdr = numeric(),
    significance = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

diff_empty_ancombc_table <- function() {
  data.frame(
    taxon = character(),
    display_taxon = character(),
    tax_level = character(),
    group_variable = character(),
    comparison = character(),
    log_fold_change = numeric(),
    p_value = numeric(),
    fdr = numeric(),
    significance = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

diff_top_n_choices <- function() c(10L, 20L, 30L, 50L)

diff_direction_palette <- function() {
  c(
    higher_in_group1 = "#4C78A8",
    higher_in_group2 = "#E45756",
    higher_in_other = "#72B7B2",
    no_direction = "#9AA5B1"
  )
}

diff_significance_palette <- function() {
  c(
    significant = "#D62828",
    trend = "#F4A261",
    not_significant = "#A8B0B9"
  )
}

diff_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#1F2937"),
      plot.subtitle = ggplot2::element_text(color = "#4B5563"),
      axis.title = ggplot2::element_text(color = "#1F2937"),
      axis.text = ggplot2::element_text(color = "#111827"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_blank()
    )
}

diff_add_warning <- function(warnings, message) {
  message <- trimws(as.character(message)[1] %||% "")
  if (!nzchar(message)) return(warnings)
  unique(c(warnings, message))
}

diff_safe_write_csv <- function(df, path) {
  assert_non_empty_string(path, "path")
  ensure_dir(dirname(path))
  readr::write_csv(df, path, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

diff_safe_write_json <- function(x, path) {
  write_json_pretty(x, path, auto_unbox = TRUE)
}

diff_safe_save_plot <- function(plot, pdf_path, png_path, width = 8, height = 6, dpi = 300) {
  assert_non_empty_string(pdf_path, "pdf_path")
  assert_non_empty_string(png_path, "png_path")
  ensure_dir(dirname(pdf_path))
  ensure_dir(dirname(png_path))

  warnings <- character(0)

  png_saved <- tryCatch({
    ggplot2::ggsave(
      filename = png_path,
      plot = plot,
      device = "png",
      width = width,
      height = height,
      dpi = dpi
    )
    normalizePath(png_path, winslash = "/", mustWork = TRUE)
  }, error = function(e) {
    warnings <<- c(warnings, paste0("PNG save failed: ", conditionMessage(e)))
    NULL
  })

  pdf_saved <- tryCatch({
    ggplot2::ggsave(
      filename = pdf_path,
      plot = plot,
      device = "pdf",
      width = width,
      height = height
    )
    normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
  }, error = function(e) {
    warnings <<- c(warnings, paste0("PDF save failed: ", conditionMessage(e)))
    NULL
  })

  if (is.null(png_saved)) {
    stop("Unable to save the differential abundance figure as PNG.", call. = FALSE)
  }

  list(
    png = png_saved,
    pdf = pdf_saved,
    warnings = unique(warnings)
  )
}

diff_wrap_label <- function(x, width = 28) {
  x <- as.character(x)
  vapply(x, function(one) paste(strwrap(one, width = width), collapse = "\n"), character(1))
}

diff_placeholder_plot <- function(title, subtitle = NULL, body = "No result available.") {
  ggplot2::ggplot(data.frame(x = 0, y = 0, label = body), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 4.5, color = "#4B5563") +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    diff_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

diff_significance_from_fdr <- function(fdr) {
  vapply(fdr, function(x) {
    if (!is.finite(x)) return("not_significant")
    if (x < 0.05) return("significant")
    if (x < 0.10) return("trend")
    "not_significant"
  }, character(1))
}

diff_order_table <- function(df) {
  if (!is.data.frame(df) || nrow(df) < 1) return(df)
  n <- nrow(df)
  p  <- suppressWarnings(as.numeric(df$p_value %||% rep(NA_real_, n)))
  fdr <- suppressWarnings(as.numeric(df$fdr %||% rep(NA_real_, n)))
  fc  <- suppressWarnings(as.numeric(df$log2FC %||% rep(NA_real_, n)))
  es  <- suppressWarnings(as.numeric(df$effect_size %||% rep(NA_real_, n)))
  ord_p   <- ifelse(is.finite(p), p, Inf)
  ord_fdr <- ifelse(is.finite(fdr), fdr, Inf)
  ord_fc  <- ifelse(is.finite(fc), abs(fc), 0)
  ord_eff <- ifelse(is.finite(es), abs(es), 0)
  df[order(ord_fdr, ord_p, -ord_fc, -ord_eff), , drop = FALSE]
}

diff_group_mean_string <- function(group_means) {
  if (length(group_means) < 1) return("")
  parts <- paste0(names(group_means), "=", formatC(group_means, digits = 4, format = "f"))
  paste(parts, collapse = "; ")
}

diff_pick_top <- function(df, top_n = 20) {
  if (!is.data.frame(df) || nrow(df) < 1) {
    return(list(data = diff_empty_results_table(), exploratory = TRUE))
  }

  top_n <- suppressWarnings(as.integer(top_n[[1]]))
  if (!is.finite(top_n) || top_n < 1) top_n <- 20L

  sig <- df[df$significance == "significant", , drop = FALSE]
  if (nrow(sig) > 0) {
    sig <- diff_order_table(sig)
    return(list(data = utils::head(sig, top_n), exploratory = FALSE))
  }

  n <- nrow(df)
  pv <- suppressWarnings(as.numeric(df$p_value %||% rep(NA_real_, n)))
  pv[!is.finite(pv)] <- Inf
  fc <- suppressWarnings(as.numeric(df$log2FC %||% rep(NA_real_, n)))
  es <- suppressWarnings(as.numeric(df$effect_size %||% rep(NA_real_, n)))
  fallback <- ifelse(is.finite(fc), abs(fc), ifelse(is.finite(es), abs(es), 0))
  sel <- df[order(pv, -fallback), , drop = FALSE]
  list(data = utils::head(sel, top_n), exploratory = TRUE)
}

diff_cliffs_delta <- function(group1, group2) {
  group1 <- group1[is.finite(group1)]
  group2 <- group2[is.finite(group2)]
  if (length(group1) < 1 || length(group2) < 1) return(NA_real_)
  mean(sign(outer(group2, group1, "-")))
}

diff_epsilon_squared <- function(statistic, n_total, n_groups) {
  if (!is.finite(statistic) || !is.finite(n_total) || !is.finite(n_groups) || n_total <= n_groups) return(NA_real_)
  value <- (statistic - n_groups + 1) / (n_total - n_groups)
  max(min(value, 1), 0)
}

diff_compute_log2fc <- function(mean1, mean2, pseudo_count = 1e-6) {
  if (!is.finite(mean1) || !is.finite(mean2)) return(NA_real_)
  log2((mean2 + pseudo_count) / (mean1 + pseudo_count))
}

diff_safe_tax_label <- function(x, max_chars = 35) {
  out <- make_taxon_display_label(x, max_chars = max_chars)
  out[is.na(out) | !nzchar(out)] <- clean_taxon_label(x)[is.na(out) | !nzchar(out)]
  out
}

diff_clone_dataset <- function(dataset) {
  if (!is.null(dataset) && is.function(dataset$clone)) {
    return(dataset$clone(deep = TRUE))
  }
  dataset
}

diff_resolve_tax_level <- function(dataset, tax_level) {
  dataset$cal_abund()
  available <- names(dataset$taxa_abund %||% list())
  if (length(available) < 1) {
    stop("run_diff_analysis(): no aggregated taxonomic abundance table is available after cal_abund().", call. = FALSE)
  }

  exact <- available[match(tax_level, available, nomatch = 0)]
  if (length(exact) > 0) {
    return(list(level = exact[[1]], available = available))
  }

  idx <- match(tolower(tax_level), tolower(available), nomatch = 0)
  if (idx > 0) {
    return(list(level = available[[idx]], available = available))
  }

  stop(
    "run_diff_analysis(): tax_level '", tax_level, "' was not found. Available tax levels: ",
    paste(available, collapse = ", "),
    call. = FALSE
  )
}

diff_prepare_inputs <- function(dataset, group_var, tax_level) {
  if (is.null(dataset)) stop("run_diff_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")

  sample_table <- dataset$sample_table
  if (is.null(sample_table) || !is.data.frame(sample_table)) {
    stop("run_diff_analysis(): sample_table is missing from the dataset.", call. = FALSE)
  }
  if (!group_var %in% names(sample_table)) {
    stop("run_diff_analysis(): group_var '", group_var, "' was not found in sample_table.", call. = FALSE)
  }

  level_info <- diff_resolve_tax_level(dataset, tax_level)
  abund <- as.data.frame(dataset$taxa_abund[[level_info$level]])
  if (nrow(abund) < 1 || ncol(abund) < 1) {
    stop("run_diff_analysis(): abundance table is empty at tax_level '", level_info$level, "'.", call. = FALSE)
  }

  common_samples <- intersect(colnames(abund), rownames(sample_table))
  if (length(common_samples) < 2) {
    stop("run_diff_analysis(): fewer than two overlapping samples were found between abundance and metadata.", call. = FALSE)
  }

  sample_table <- sample_table[common_samples, , drop = FALSE]
  group_values <- as.character(sample_table[[group_var]])
  keep <- !is.na(group_values) & nzchar(trimws(group_values))
  if (sum(keep) < 2) {
    stop("run_diff_analysis(): fewer than two samples remain after removing missing group assignments.", call. = FALSE)
  }

  sample_table <- sample_table[keep, , drop = FALSE]
  abund <- as.matrix(abund[, rownames(sample_table), drop = FALSE])
  groups <- droplevels(as.factor(sample_table[[group_var]]))
  names(groups) <- rownames(sample_table)
  if (length(levels(groups)) < 2) {
    stop("run_diff_analysis(): at least two groups are required for differential abundance analysis.", call. = FALSE)
  }

  list(
    abundance = abund,
    sample_table = sample_table,
    groups = groups,
    tax_level = level_info$level,
    available_levels = level_info$available
  )
}

diff_run_primary_tests <- function(abundance, groups, group_var, tax_level, warnings = character(0)) {
  stopifnot(is.matrix(abundance))
  group_levels <- levels(groups)
  group_counts <- table(groups)
  if (any(group_counts < 2)) {
    warnings <- diff_add_warning(
      warnings,
      paste0(
        "Some groups have fewer than 2 samples (",
        paste(names(group_counts), group_counts, sep = "=", collapse = ", "),
        "); inferential results should be interpreted with caution."
      )
    )
  }

  results <- diff_empty_results_table()
  effect_tbl <- diff_empty_effect_size_table()
  pseudo_count <- 1e-6
  is_two_group <- length(group_levels) == 2
  primary_method <- if (is_two_group) "Wilcoxon rank-sum" else "Kruskal-Wallis"
  overall_comparison <- paste(group_levels, collapse = " vs ")

  for (taxon_name in rownames(abundance)) {
    values <- as.numeric(abundance[taxon_name, names(groups)])
    sample_groups <- groups[names(groups)]
    keep <- is.finite(values) & !is.na(sample_groups)
    values <- values[keep]
    sample_groups <- droplevels(sample_groups[keep])
    if (length(values) < 2 || length(levels(sample_groups)) < 2) next

    split_values <- split(values, sample_groups)
    means <- vapply(group_levels, function(one_group) {
      group_vec <- split_values[[one_group]]
      if (is.null(group_vec) || length(group_vec) < 1) return(NA_real_)
      mean(group_vec, na.rm = TRUE)
    }, numeric(1))

    comparison <- overall_comparison
    contrast <- overall_comparison
    group1 <- group_levels[[1]]
    group2 <- if (length(group_levels) >= 2) group_levels[[2]] else ""
    mean_group1 <- means[[group1]] %||% NA_real_
    mean_group2 <- if (nzchar(group2)) means[[group2]] %||% NA_real_ else NA_real_
    mean_diff <- NA_real_
    log2fc <- NA_real_
    p_value <- NA_real_
    effect_size <- NA_real_
    effect_method <- if (is_two_group) "Cliff's delta" else "Epsilon-squared"
    direction <- "no_direction"

    if (is_two_group) {
      g1 <- split_values[[group1]]
      g2 <- split_values[[group2]]
      if (!is.null(g1) && !is.null(g2) && length(g1) > 0 && length(g2) > 0) {
        p_value <- tryCatch(stats::wilcox.test(g1, g2, exact = FALSE)$p.value, error = function(e) NA_real_)
        mean_diff <- mean_group2 - mean_group1
        log2fc <- diff_compute_log2fc(mean_group1, mean_group2, pseudo_count = pseudo_count)
        effect_size <- diff_cliffs_delta(g1, g2)
        if (is.finite(log2fc) && log2fc > 0) {
          direction <- "higher_in_group2"
        } else if (is.finite(log2fc) && log2fc < 0) {
          direction <- "higher_in_group1"
        }
      }
    } else {
      kw <- tryCatch(stats::kruskal.test(values ~ sample_groups), error = function(e) NULL)
      if (!is.null(kw)) {
        p_value <- kw$p.value %||% NA_real_
        max_group <- names(which.max(means))[1]
        min_group <- names(which.min(means))[1]
        max_mean <- suppressWarnings(as.numeric(means[[max_group]]))
        min_mean <- suppressWarnings(as.numeric(means[[min_group]]))
        mean_diff <- if (is.finite(max_mean) && is.finite(min_mean)) max_mean - min_mean else NA_real_
        log2fc <- diff_compute_log2fc(min_mean, max_mean, pseudo_count = pseudo_count)
        effect_size <- diff_epsilon_squared(
          statistic = suppressWarnings(as.numeric(kw$statistic[[1]] %||% NA_real_)),
          n_total = length(values),
          n_groups = length(levels(sample_groups))
        )
        comparison <- overall_comparison
        contrast <- paste(max_group, "vs", min_group)
        direction <- "higher_in_other"
        if (is.finite(max_mean) && is.finite(min_mean) && !identical(max_group, min_group)) {
          direction <- paste0("highest_in_", gsub("[^A-Za-z0-9]+", "_", max_group))
        }
      }
    }

    group_mean_abundance <- diff_group_mean_string(means)
    display_taxon <- diff_safe_tax_label(taxon_name, max_chars = 60)
    display_short <- diff_safe_tax_label(taxon_name, max_chars = 35)

    row_df <- data.frame(
      taxon = taxon_name,
      Taxon = taxon_name,
      tax_level = tax_level,
      Level = tax_level,
      test_method = primary_method,
      method = primary_method,
      group_variable = group_var,
      GroupVariable = group_var,
      comparison = comparison,
      Comparison = comparison,
      contrast = contrast,
      group_mean_abundance = group_mean_abundance,
      group1 = group1,
      Group1 = group1,
      group2 = group2,
      Group2 = group2,
      mean_group1 = mean_group1,
      Mean_Group1 = mean_group1,
      mean_group2 = mean_group2,
      Mean_Group2 = mean_group2,
      mean_abundance_difference = mean_diff,
      mean_abundance = mean(values, na.rm = TRUE),
      prevalence = mean(values > 0, na.rm = TRUE),
      log2FC = log2fc,
      log2fc = log2fc,
      p_value = p_value,
      fdr = NA_real_,
      FDR = NA_real_,
      significance = NA_character_,
      Significance = NA_character_,
      direction = direction,
      Direction = direction,
      effect_size_method = effect_method,
      effect_size = effect_size,
      EffectSize = effect_size,
      significant = FALSE,
      trend = FALSE,
      exploratory_only = FALSE,
      taxon_label = display_taxon,
      display_taxon = display_taxon,
      display_taxon_short = display_short,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    results <- rbind(results, row_df)

    effect_row <- data.frame(
      taxon = taxon_name,
      tax_level = tax_level,
      group_variable = group_var,
      comparison = contrast,
      test_method = primary_method,
      effect_size_method = effect_method,
      effect_size = effect_size,
      p_value = p_value,
      fdr = NA_real_,
      significance = NA_character_,
      direction = direction,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    effect_tbl <- rbind(effect_tbl, effect_row)
  }

  if (nrow(results) < 1) {
    warnings <- diff_add_warning(warnings, "No taxa could be tested after filtering non-finite values.")
    return(list(
      results = diff_empty_results_table(),
      effect_size = diff_empty_effect_size_table(),
      method = primary_method,
      group_counts = group_counts,
      warnings = warnings
    ))
  }

  if (all(is.na(results$p_value))) {
    warnings <- diff_add_warning(
      warnings,
      "All differential abundance p values were NA. Exploratory ranking will fall back to effect sizes and abundance differences."
    )
    results$fdr <- NA_real_
    effect_tbl$fdr <- NA_real_
  } else {
    results$fdr <- stats::p.adjust(results$p_value, method = "BH")
    effect_tbl$fdr <- stats::p.adjust(effect_tbl$p_value, method = "BH")
  }

  results$FDR <- results$fdr
  results$significance <- diff_significance_from_fdr(results$fdr)
  results$Significance <- results$significance
  results$significant <- results$significance == "significant"
  results$trend <- results$significance == "trend"
  results$exploratory_only <- !any(results$significant, na.rm = TRUE)

  effect_tbl$significance <- diff_significance_from_fdr(effect_tbl$fdr)

  results <- diff_order_table(results)
  effect_tbl <- effect_tbl[match(results$taxon, effect_tbl$taxon), , drop = FALSE]
  rownames(results) <- NULL
  rownames(effect_tbl) <- NULL

  list(
    results = results,
    effect_size = effect_tbl,
    method = primary_method,
    group_counts = group_counts,
    warnings = warnings
  )
}

plot_diff_top_barplot <- function(diff_table, group_var, tax_level, job_dir, top_n = 20, base_name = "diff_top_barplot") {
  if (!is.data.frame(diff_table)) stop("plot_diff_top_barplot(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  assert_non_empty_string(job_dir, "job_dir")
  assert_non_empty_string(base_name, "base_name")

  selected <- diff_pick_top(diff_table, top_n = top_n)
  plot_df <- selected$data
  plot_df <- plot_df[is.finite(plot_df$log2FC) | is.finite(plot_df$mean_abundance_difference), , drop = FALSE]

  title_text <- paste0("Differential abundance barplot (", tax_level, ")")
  subtitle_text <- if (selected$exploratory) {
    "No FDR-significant taxa; showing exploratory top taxa by raw p-value"
  } else {
    paste0("Top ", min(top_n, nrow(plot_df)), " taxa ranked by FDR significance")
  }

  if (nrow(plot_df) < 1) {
    saved <- diff_safe_save_plot(
      plot = diff_placeholder_plot(title_text, subtitle_text, "No taxa available for barplot."),
      pdf_path = file.path(job_dir, "figures", paste0(base_name, ".pdf")),
      png_path = file.path(job_dir, "figures", paste0(base_name, ".png")),
      width = 10,
      height = 6
    )
    return(saved)
  }

  metric <- ifelse(is.finite(plot_df$log2FC), plot_df$log2FC, plot_df$mean_abundance_difference)
  plot_df$metric_value <- metric
  plot_df$direction_group <- plot_df$direction
  plot_df$direction_group[grepl("^highest_in_", plot_df$direction_group)] <- "higher_in_other"
  plot_df$display_taxon_wrapped <- diff_wrap_label(plot_df$display_taxon_short, width = 24)
  plot_df <- plot_df[order(plot_df$metric_value), , drop = FALSE]
  plot_df$display_taxon_wrapped <- factor(plot_df$display_taxon_wrapped, levels = unique(plot_df$display_taxon_wrapped))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = display_taxon_wrapped, y = metric_value, fill = direction_group)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = diff_direction_palette(), drop = FALSE) +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = "log2 fold change / mean abundance difference"
    ) +
    diff_theme()

  diff_safe_save_plot(
    plot = p,
    pdf_path = file.path(job_dir, "figures", paste0(base_name, ".pdf")),
    png_path = file.path(job_dir, "figures", paste0(base_name, ".png")),
    width = 10,
    height = max(6, 0.32 * nrow(plot_df) + 2.2)
  )
}

plot_diff_taxa_barplot <- function(diff_table, group_var, job_dir, exploratory = NULL, top_n = 20, tax_level = NULL, base_name = "diff_taxa_barplot") {
  tax_level <- tax_level %||% (if (is.data.frame(diff_table) && "tax_level" %in% names(diff_table) && nrow(diff_table) > 0) as.character(diff_table$tax_level[[1]]) else "Genus")
  plot_diff_top_barplot(
    diff_table = diff_table,
    group_var = group_var,
    tax_level = tax_level,
    job_dir = job_dir,
    top_n = top_n,
    base_name = base_name
  )
}

plot_diff_volcano <- function(diff_table, group_var, tax_level, job_dir, base_name = "diff_volcano") {
  if (!is.data.frame(diff_table)) stop("plot_diff_volcano(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  assert_non_empty_string(job_dir, "job_dir")

  title_text <- paste0("Differential abundance volcano plot (", tax_level, ")")
  plot_df <- diff_table
  plot_df$display_taxon_wrapped <- diff_wrap_label(plot_df$display_taxon_short, width = 20)
  plot_df$y_value <- ifelse(is.finite(plot_df$fdr) & plot_df$fdr > 0, -log10(plot_df$fdr), NA_real_)
  plot_df$x_value <- ifelse(is.finite(plot_df$log2FC), plot_df$log2FC, 0)
  plot_df$label <- ""

  has_sig <- any(plot_df$significance == "significant", na.rm = TRUE)
  label_rows <- if (has_sig) {
    head(plot_df[plot_df$significance == "significant", , drop = FALSE], 10)
  } else {
    ord_p <- ifelse(is.finite(plot_df$p_value), plot_df$p_value, Inf)
    head(plot_df[order(ord_p, -abs(plot_df$x_value)), , drop = FALSE], 10)
  }
  if (nrow(label_rows) > 0) {
    plot_df$label[match(label_rows$taxon, plot_df$taxon)] <- label_rows$display_taxon_short
  }

  subtitle_text <- if (has_sig) {
    "Colors indicate FDR significance class"
  } else {
    "Exploratory view: no FDR-significant taxa; labels show raw p-value top taxa"
  }

  if (nrow(plot_df) < 1) {
    saved <- diff_safe_save_plot(
      plot = diff_placeholder_plot(title_text, subtitle_text, "No taxa available for volcano plot."),
      pdf_path = file.path(job_dir, "figures", paste0(base_name, ".pdf")),
      png_path = file.path(job_dir, "figures", paste0(base_name, ".png"))
    )
    return(saved)
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x_value, y = y_value, color = significance)) +
    ggplot2::geom_point(size = 2.3, alpha = 0.85, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = diff_significance_palette(), drop = FALSE) +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "log2 fold change",
      y = "-log10(FDR)"
    ) +
    diff_theme()

  lab_df <- plot_df[nzchar(plot_df$label), , drop = FALSE]
  if (nrow(lab_df) > 0) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        data = lab_df,
        ggplot2::aes(label = label),
        size = 3,
        max.overlaps = 30,
        show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = lab_df,
        ggplot2::aes(label = label),
        size = 2.8,
        vjust = -0.6,
        show.legend = FALSE
      )
    }
  }

  diff_safe_save_plot(
    plot = p,
    pdf_path = file.path(job_dir, "figures", paste0(base_name, ".pdf")),
    png_path = file.path(job_dir, "figures", paste0(base_name, ".png")),
    width = 9,
    height = 6.5
  )
}

plot_diff_heatmap <- function(diff_table, abundance_matrix, groups, group_var, tax_level, job_dir, top_n = 20, base_name = "diff_heatmap") {
  if (!is.data.frame(diff_table)) stop("plot_diff_heatmap(): diff_table must be a data.frame.", call. = FALSE)
  if (!is.matrix(abundance_matrix) && !is.data.frame(abundance_matrix)) stop("plot_diff_heatmap(): abundance_matrix must be matrix-like.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  assert_non_empty_string(job_dir, "job_dir")

  selected <- diff_pick_top(diff_table, top_n = top_n)
  chosen <- selected$data
  chosen <- chosen[chosen$taxon %in% rownames(abundance_matrix), , drop = FALSE]
  if (nrow(chosen) > 20) chosen <- utils::head(chosen, 20)

  title_text <- paste0("Differential taxa heatmap (", tax_level, ")")
  subtitle_text <- if (selected$exploratory) {
    "No FDR-significant taxa; showing exploratory top taxa by raw p-value"
  } else {
    "Heatmap of FDR-significant taxa"
  }

  if (nrow(chosen) < 1) {
    return(diff_safe_save_plot(
      plot = diff_placeholder_plot(title_text, subtitle_text, "No taxa available for heatmap."),
      pdf_path = file.path(job_dir, "figures", paste0(base_name, ".pdf")),
      png_path = file.path(job_dir, "figures", paste0(base_name, ".png")),
      width = 10,
      height = 6
    ))
  }

  sample_order <- order(as.character(groups), names(groups))
  sample_ids <- names(groups)[sample_order]
  mat <- as.matrix(abundance_matrix[chosen$taxon, sample_ids, drop = FALSE])
  rownames(mat) <- make.unique(chosen$display_taxon_short, sep = " ")
  z_mat <- t(scale(t(mat)))
  z_mat[is.na(z_mat)] <- 0

  heatmap_matrix_out <- data.frame(
    taxon = chosen$taxon,
    display_taxon = rownames(z_mat),
    z_mat,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (identical(base_name, "diff_heatmap")) {
    diff_safe_write_csv(heatmap_matrix_out, file.path(job_dir, "tables", "diff_heatmap_matrix.csv"))
  }

  ann_col <- data.frame(Group = as.character(groups[sample_ids]), row.names = sample_ids, stringsAsFactors = FALSE)
  pdf_path <- file.path(job_dir, "figures", paste0(base_name, ".pdf"))
  png_path <- file.path(job_dir, "figures", paste0(base_name, ".png"))
  ensure_dir(dirname(pdf_path))
  ensure_dir(dirname(png_path))

  if (requireNamespace("pheatmap", quietly = TRUE)) {
    warnings <- character(0)
    draw_heatmap <- function(filename, device_type) {
      if (identical(device_type, "png")) {
        grDevices::png(filename, width = 10, height = max(6, 0.24 * nrow(z_mat) + 3), units = "in", res = 300)
      } else {
        grDevices::pdf(filename, width = 10, height = max(6, 0.24 * nrow(z_mat) + 3), onefile = FALSE)
      }
      tryCatch(
        {
          ph <- pheatmap::pheatmap(
            mat = z_mat,
            annotation_col = ann_col,
            cluster_cols = FALSE,
            cluster_rows = TRUE,
            border_color = NA,
            show_colnames = FALSE,
            fontsize = 10,
            fontsize_row = 9,
            color = colorRampPalette(c("#244C7C", "#F6F7F9", "#C44536"))(100),
            main = paste(title_text, "-", subtitle_text),
            silent = TRUE
          )
          grid::grid.newpage()
          grid::grid.draw(ph$gtable)
        },
        finally = grDevices::dev.off()
      )
    }

    png_saved <- tryCatch({
      draw_heatmap(png_path, "png")
      normalizePath(png_path, winslash = "/", mustWork = TRUE)
    }, error = function(e) {
      warnings <<- c(warnings, paste0("PNG save failed: ", conditionMessage(e)))
      NULL
    })

    pdf_saved <- tryCatch({
      draw_heatmap(pdf_path, "pdf")
      normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
    }, error = function(e) {
      warnings <<- c(warnings, paste0("PDF save failed: ", conditionMessage(e)))
      NULL
    })

    if (is.null(png_saved)) {
      stop("Unable to save the differential heatmap as PNG.", call. = FALSE)
    }

    return(list(png = png_saved, pdf = pdf_saved, warnings = unique(warnings)))
  }

  long_df <- reshape(
    data.frame(t(z_mat), SampleID = colnames(z_mat), check.names = FALSE),
    varying = rownames(z_mat),
    v.names = "value",
    timevar = "Taxon",
    times = rownames(z_mat),
    direction = "long"
  )
  long_df$Taxon <- factor(long_df$Taxon, levels = rev(rownames(z_mat)))
  long_df$SampleID <- factor(long_df$SampleID, levels = sample_ids)

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = SampleID, y = Taxon, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#244C7C", mid = "#F6F7F9", high = "#C44536") +
    ggplot2::labs(title = title_text, subtitle = subtitle_text, x = "Sample", y = NULL, fill = "Z-score") +
    diff_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 8))

  diff_safe_save_plot(
    plot = p,
    pdf_path = pdf_path,
    png_path = png_path,
    width = 10,
    height = max(6, 0.24 * nrow(z_mat) + 3)
  )
}

diff_run_lefse <- function(dataset, group_var, tax_level, job_dir, warnings = character(0)) {
  out_csv <- file.path(job_dir, "tables", "diff_lefse.csv")
  out_empty <- diff_empty_lefse_table()

  if (!requireNamespace("microeco", quietly = TRUE)) {
    warnings <- diff_add_warning(warnings, "LEfSe was skipped because package 'microeco' is not available.")
    diff_safe_write_csv(out_empty, out_csv)
    return(list(table = out_empty, available = FALSE, warnings = warnings, figure = NULL))
  }

  result <- tryCatch({
    ds <- diff_clone_dataset(dataset)
    microeco::trans_diff$new(
      dataset = ds,
      method = "lefse",
      group = group_var,
      taxa_level = tax_level,
      alpha = 0.05
    )
  }, error = function(e) e)

  if (inherits(result, "error")) {
    msg <- conditionMessage(result)
    if (grepl("No significant feature found", msg, fixed = TRUE)) {
      warnings <- diff_add_warning(warnings, "LEfSe was available but did not identify significant taxa under the default threshold.")
      diff_safe_write_csv(out_empty, out_csv)
      return(list(table = out_empty, available = TRUE, warnings = warnings, figure = NULL))
    }
    warnings <- diff_add_warning(warnings, paste0("LEfSe was skipped: ", msg))
    diff_safe_write_csv(out_empty, out_csv)
    return(list(table = out_empty, available = TRUE, warnings = warnings, figure = NULL))
  }

  lefse_raw <- as.data.frame(result$res_diff)
  if (nrow(lefse_raw) < 1) {
    diff_safe_write_csv(out_empty, out_csv)
    return(list(table = out_empty, available = TRUE, warnings = warnings, figure = NULL))
  }

  out <- data.frame(
    taxon = as.character(lefse_raw$Taxa %||% rownames(lefse_raw)),
    display_taxon = diff_safe_tax_label(as.character(lefse_raw$Taxa %||% rownames(lefse_raw))),
    tax_level = tax_level,
    group_variable = group_var,
    comparison = as.character(lefse_raw$Comparison %||% ""),
    enriched_group = as.character(lefse_raw$Group %||% ""),
    lda_score = suppressWarnings(as.numeric(lefse_raw$LDA %||% NA_real_)),
    p_value = suppressWarnings(as.numeric(lefse_raw$P.unadj %||% NA_real_)),
    fdr = suppressWarnings(as.numeric(lefse_raw$P.adj %||% NA_real_)),
    significance = if ("Significance" %in% names(lefse_raw)) as.character(lefse_raw$Significance) else ifelse((lefse_raw$P.adj %||% Inf) < 0.05, "significant", "not_significant"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out <- out[order(-out$lda_score, out$p_value), , drop = FALSE]
  rownames(out) <- NULL
  diff_safe_write_csv(out, out_csv)

  fig_saved <- NULL
  if (nrow(out) > 0) {
    top_df <- utils::head(out, 20)
    { lbl <- diff_wrap_label(top_df$display_taxon, width = 24); top_df$label <- factor(lbl, levels = rev(unique(lbl))) }
    p <- ggplot2::ggplot(top_df, ggplot2::aes(x = label, y = lda_score, fill = enriched_group)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = paste0("LEfSe LDA score plot (", tax_level, ")"), x = NULL, y = "LDA score") +
      diff_theme()
    fig_saved <- diff_safe_save_plot(
      plot = p,
      pdf_path = file.path(job_dir, "figures", "diff_lefse_lda.pdf"),
      png_path = file.path(job_dir, "figures", "diff_lefse_lda.png"),
      width = 10,
      height = max(6, 0.3 * nrow(top_df) + 2.5)
    )
    warnings <- unique(c(warnings, fig_saved$warnings %||% character(0)))
  }

  list(table = out, available = TRUE, warnings = warnings, figure = fig_saved)
}

diff_run_ancombc <- function(dataset, group_var, tax_level, job_dir, warnings = character(0)) {
  out_csv <- file.path(job_dir, "tables", "diff_ancombc.csv")
  out_empty <- diff_empty_ancombc_table()

  if (!requireNamespace("ANCOMBC", quietly = TRUE)) {
    warnings <- diff_add_warning(warnings, "ANCOM-BC was skipped because package 'ANCOMBC' is not installed.")
    diff_safe_write_csv(out_empty, out_csv)
    return(list(table = out_empty, available = FALSE, warnings = warnings))
  }

  result <- tryCatch({
    ds <- diff_clone_dataset(dataset)
    microeco::trans_diff$new(
      dataset = ds,
      method = "ancombc2",
      group = group_var,
      taxa_level = tax_level,
      alpha = 0.05
    )
  }, error = function(e) e)

  if (inherits(result, "error")) {
    warnings <- diff_add_warning(warnings, paste0("ANCOM-BC was skipped: ", conditionMessage(result)))
    diff_safe_write_csv(out_empty, out_csv)
    return(list(table = out_empty, available = TRUE, warnings = warnings))
  }

  raw <- as.data.frame(result$res_diff %||% data.frame())
  if (nrow(raw) < 1) {
    diff_safe_write_csv(out_empty, out_csv)
    return(list(table = out_empty, available = TRUE, warnings = warnings))
  }

  taxon_col <- if ("Taxa" %in% names(raw)) "Taxa" else names(raw)[1]
  p_col <- intersect(names(raw), c("p_val", "P.val", "pvalue", "p_value", "Pvalue", "W.p_val"))
  q_col <- intersect(names(raw), c("q_val", "qvalue", "q_value", "fdr", "adj_p", "W.q_val"))
  lfc_col <- intersect(names(raw), c("lfc", "log_fold_change", "logFC", "coef"))

  out <- data.frame(
    taxon = as.character(raw[[taxon_col]]),
    display_taxon = diff_safe_tax_label(as.character(raw[[taxon_col]])),
    tax_level = tax_level,
    group_variable = group_var,
    comparison = paste(unique(as.character(raw$Comparison %||% group_var)), collapse = "; "),
    log_fold_change = if (length(lfc_col) > 0) suppressWarnings(as.numeric(raw[[lfc_col[[1]]]])) else NA_real_,
    p_value = if (length(p_col) > 0) suppressWarnings(as.numeric(raw[[p_col[[1]]]])) else NA_real_,
    fdr = if (length(q_col) > 0) suppressWarnings(as.numeric(raw[[q_col[[1]]]])) else NA_real_,
    significance = NA_character_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out$significance <- diff_significance_from_fdr(out$fdr)
  out <- out[order(out$fdr, out$p_value), , drop = FALSE]
  rownames(out) <- NULL
  diff_safe_write_csv(out, out_csv)

  list(table = out, available = TRUE, warnings = warnings)
}

diff_build_consensus <- function(diff_table, lefse_table, ancombc_table, group_var, tax_level, primary_method, job_dir) {
  out <- diff_empty_consensus_table()
  collectors <- list()

  primary_sig <- diff_table[diff_table$significance == "significant", , drop = FALSE]
  if (nrow(primary_sig) > 0) {
    collectors$primary <- data.frame(taxon = primary_sig$taxon, method = primary_method, stringsAsFactors = FALSE)
  }

  if (is.data.frame(lefse_table) && nrow(lefse_table) > 0) {
    lefse_sig <- lefse_table
    if ("fdr" %in% names(lefse_sig)) {
      lefse_sig <- lefse_sig[!is.na(lefse_sig$fdr) & lefse_sig$fdr < 0.05, , drop = FALSE]
    }
    if (nrow(lefse_sig) > 0) {
      collectors$lefse <- data.frame(taxon = lefse_sig$taxon, method = "LEfSe", stringsAsFactors = FALSE)
    }
  }

  if (is.data.frame(ancombc_table) && nrow(ancombc_table) > 0) {
    ancom_sig <- ancombc_table
    if ("fdr" %in% names(ancom_sig)) {
      ancom_sig <- ancom_sig[!is.na(ancom_sig$fdr) & ancom_sig$fdr < 0.05, , drop = FALSE]
    }
    if (nrow(ancom_sig) > 0) {
      collectors$ancombc <- data.frame(taxon = ancom_sig$taxon, method = "ANCOM-BC", stringsAsFactors = FALSE)
    }
  }

  if (length(collectors) > 0) {
    merged <- do.call(rbind, collectors)
    split_methods <- split(merged$method, merged$taxon)
    out <- do.call(rbind, lapply(names(split_methods), function(taxon_name) {
      methods <- unique(as.character(split_methods[[taxon_name]]))
      src_row <- diff_table[match(taxon_name, diff_table$taxon), , drop = FALSE]
      data.frame(
        taxon = taxon_name,
        display_taxon = src_row$display_taxon[[1]] %||% diff_safe_tax_label(taxon_name),
        tax_level = tax_level,
        group_variable = group_var,
        evidence_count = length(methods),
        evidence_methods = paste(methods, collapse = "; "),
        primary_method = primary_method %in% methods,
        lefse = "LEfSe" %in% methods,
        ancombc = "ANCOM-BC" %in% methods,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
    out <- out[order(-out$evidence_count, out$display_taxon), , drop = FALSE]
    rownames(out) <- NULL
  }

  diff_safe_write_csv(out, file.path(job_dir, "tables", "diff_consensus.csv"))
  out
}

summarize_diff_for_ai <- function(diff_table, group_var, summary_obj = NULL) {
  if (!is.data.frame(diff_table)) stop("summarize_diff_for_ai(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")

  summary_obj <- summary_obj %||% list()
  tax_level <- summary_obj$tax_level %||% (if (nrow(diff_table) > 0 && "tax_level" %in% names(diff_table)) as.character(diff_table$tax_level[[1]]) else NA_character_)
  test_method <- summary_obj$test_method %||% summary_obj$method %||% (if (nrow(diff_table) > 0 && "test_method" %in% names(diff_table)) as.character(diff_table$test_method[[1]]) else NA_character_)
  n_sig <- sum(diff_table$significance == "significant", na.rm = TRUE)
  n_trend <- sum(diff_table$significance == "trend", na.rm = TRUE)

  sig <- diff_table[diff_table$significance == "significant", , drop = FALSE]
  sig_top <- prepare_diff_taxa_for_ai(sig, max_n = 10, drop_unclassified = FALSE, keep_single_unclassified = TRUE)

  exploratory_pool <- diff_table[diff_table$significance != "significant", , drop = FALSE]
  ord_p <- ifelse(is.finite(exploratory_pool$p_value), exploratory_pool$p_value, Inf)
  exploratory_pool <- exploratory_pool[order(ord_p), , drop = FALSE]
  exploratory_top <- prepare_diff_taxa_for_ai(
    exploratory_pool,
    max_n = 20,
    drop_unclassified = TRUE,
    keep_single_unclassified = TRUE
  )

  caution_notes <- unique(c(
    summary_obj$caution_notes %||% character(0),
    "Differential abundance findings are association-based and must not be interpreted as causal effects.",
    "Candidate taxa require experimental or independent-cohort validation."
  ))

  if (n_sig < 1) {
    caution_notes <- unique(c(
      caution_notes,
      "No FDR-corrected significant taxa were detected; exploratory top taxa are shown by raw p-value only."
    ))
  }

  reliability_notes <- unique(c(
    summary_obj$reliability_notes %||% character(0),
    if (n_sig > 0) {
      "FDR-controlled differential taxa were detected, but interpretation remains correlative."
    } else {
      "No FDR-significant taxa were detected; interpretation should remain exploratory."
    }
  ))

  list(
    analysis_type = "differential_abundance",
    group_variable = group_var,
    tax_level = tax_level,
    test_method = test_method,
    n_total_taxa = nrow(diff_table),
    n_significant_taxa = n_sig,
    n_trend_taxa = n_trend,
    n_exploratory_taxa = if (n_sig > 0) 0L else min(20L, nrow(diff_table)),
    top_taxa = if (n_sig > 0) sig_top else exploratory_top,
    lefse_available = isTRUE(summary_obj$lefse_available),
    ancombc_available = isTRUE(summary_obj$ancombc_available),
    consensus_available = isTRUE(summary_obj$consensus_available),
    caution_notes = caution_notes,
    reliability_notes = reliability_notes,
    n_significant = n_sig,
    significant_taxa = sig_top,
    exploratory_top_taxa = exploratory_top,
    no_significant_message = paste(
      "No FDR-significant taxa were detected.",
      "The following taxa are exploratory trends only and should not be interpreted as statistically significant."
    ),
    method = test_method
  )
}

save_diff_summary_json <- function(diff_table, group_var, tax_level, job_dir, figures_generated = list(), warnings = character(0), extra_summary = NULL) {
  if (!is.data.frame(diff_table)) stop("save_diff_summary_json(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  assert_non_empty_string(job_dir, "job_dir")

  extra_summary <- extra_summary %||% list()
  n_total <- nrow(diff_table)
  n_sig <- sum(diff_table$significance == "significant", na.rm = TRUE)
  n_trend <- sum(diff_table$significance == "trend", na.rm = TRUE)
  ai_preview <- summarize_diff_for_ai(diff_table, group_var = group_var, summary_obj = extra_summary)

  top_taxa <- ai_preview$top_taxa
  message <- if (n_sig > 0) {
    paste0("Detected ", n_sig, " significant taxa under FDR < 0.05.")
  } else {
    "No significant taxa were detected under FDR < 0.05."
  }

  caution_notes <- unique(c(
    extra_summary$caution_notes %||% character(0),
    if (n_sig < 1) "No FDR-corrected significant taxa were detected; exploratory top taxa are shown by raw p-value only." else NULL,
    "Do not interpret differential abundance associations as causal mechanisms.",
    "Independent validation and targeted experiments are recommended."
  ))

  reliability_notes <- unique(c(
    extra_summary$reliability_notes %||% character(0),
    if (n_sig > 0) "FDR-significant taxa were detected in the primary test." else "Primary differential test yielded no FDR-significant taxa."
  ))

  summary <- list(
    analysis_type = "differential_abundance",
    group_variable = group_var,
    tax_level = tax_level,
    test_method = extra_summary$test_method %||% extra_summary$method %||% (if (n_total > 0) as.character(diff_table$test_method[[1]]) else NA_character_),
    method = extra_summary$test_method %||% extra_summary$method %||% (if (n_total > 0) as.character(diff_table$test_method[[1]]) else NA_character_),
    p_adjust_method = "BH/FDR",
    n_total_taxa = n_total,
    n_total_taxa_tested = n_total,
    n_significant_taxa = n_sig,
    n_significant_fdr_0_05 = n_sig,
    n_trend_taxa = n_trend,
    n_trend_fdr_0_1 = n_trend,
    n_exploratory_taxa = if (n_sig > 0) 0L else min(20L, n_total),
    top_taxa = top_taxa,
    lefse_available = isTRUE(extra_summary$lefse_available),
    ancombc_available = isTRUE(extra_summary$ancombc_available),
    consensus_available = isTRUE(extra_summary$consensus_available),
    warnings = unique(warnings),
    caution_notes = caution_notes,
    reliability_notes = reliability_notes,
    message = message,
    figures_generated = figures_generated
  )

  out_path <- file.path(job_dir, "json", "diff_summary.json")
  diff_safe_write_json(summary, out_path)
}

run_diff_analysis <- function(dataset, group_var, tax_level, job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_diff_analysis(): job_dir not found: ", job_dir, call. = FALSE)

  prepared <- diff_prepare_inputs(dataset = dataset, group_var = group_var, tax_level = tax_level)
  warnings <- character(0)
  if (ncol(prepared$abundance) < 4) {
    warnings <- diff_add_warning(warnings, "Fewer than four samples were available for differential abundance analysis; findings are highly exploratory.")
  }

  primary <- diff_run_primary_tests(
    abundance = prepared$abundance,
    groups = prepared$groups,
    group_var = group_var,
    tax_level = prepared$tax_level,
    warnings = warnings
  )
  warnings <- primary$warnings
  diff_table <- primary$results
  effect_table <- primary$effect_size

  tables_dir <- file.path(job_dir, "tables")
  figs_dir <- file.path(job_dir, "figures")
  ensure_dir(tables_dir)
  ensure_dir(figs_dir)

  diff_table_path <- diff_safe_write_csv(diff_table, file.path(tables_dir, "differential_taxa.csv"))
  sig_table <- diff_table[diff_table$significance == "significant", , drop = FALSE]
  sig_path <- diff_safe_write_csv(sig_table, file.path(tables_dir, "differential_taxa_significant.csv"))
  effect_path <- diff_safe_write_csv(effect_table, file.path(tables_dir, "diff_effect_size.csv"))

  top_bar_saved <- plot_diff_top_barplot(
    diff_table = diff_table,
    group_var = group_var,
    tax_level = prepared$tax_level,
    job_dir = job_dir,
    top_n = 20,
    base_name = "diff_top_barplot"
  )
  warnings <- unique(c(warnings, top_bar_saved$warnings %||% character(0)))

  compat_bar_saved <- plot_diff_taxa_barplot(
    diff_table = diff_table,
    group_var = group_var,
    job_dir = job_dir,
    top_n = 20,
    tax_level = prepared$tax_level,
    base_name = "diff_taxa_barplot"
  )
  warnings <- unique(c(warnings, compat_bar_saved$warnings %||% character(0)))

  volcano_saved <- plot_diff_volcano(
    diff_table = diff_table,
    group_var = group_var,
    tax_level = prepared$tax_level,
    job_dir = job_dir,
    base_name = "diff_volcano"
  )
  warnings <- unique(c(warnings, volcano_saved$warnings %||% character(0)))

  heatmap_saved <- plot_diff_heatmap(
    diff_table = diff_table,
    abundance_matrix = prepared$abundance,
    groups = prepared$groups,
    group_var = group_var,
    tax_level = prepared$tax_level,
    job_dir = job_dir,
    top_n = 20,
    base_name = "diff_heatmap"
  )
  warnings <- unique(c(warnings, heatmap_saved$warnings %||% character(0)))

  lefse_res <- diff_run_lefse(
    dataset = dataset,
    group_var = group_var,
    tax_level = prepared$tax_level,
    job_dir = job_dir,
    warnings = warnings
  )
  warnings <- lefse_res$warnings

  ancombc_res <- diff_run_ancombc(
    dataset = dataset,
    group_var = group_var,
    tax_level = prepared$tax_level,
    job_dir = job_dir,
    warnings = warnings
  )
  warnings <- ancombc_res$warnings

  consensus_tbl <- diff_build_consensus(
    diff_table = diff_table,
    lefse_table = lefse_res$table,
    ancombc_table = ancombc_res$table,
    group_var = group_var,
    tax_level = prepared$tax_level,
    primary_method = primary$method,
    job_dir = job_dir
  )

  if (!file.exists(file.path(tables_dir, "diff_heatmap_matrix.csv"))) {
    diff_safe_write_csv(
      data.frame(stringsAsFactors = FALSE, check.names = FALSE),
      file.path(tables_dir, "diff_heatmap_matrix.csv")
    )
  }

  reliability_notes <- character(0)
  if (sum(diff_table$significance == "significant", na.rm = TRUE) > 0) {
    reliability_notes <- c(reliability_notes, "FDR-significant taxa were detected in the primary test.")
  } else {
    reliability_notes <- c(reliability_notes, "No FDR-significant taxa were detected; interpretation should remain exploratory.")
  }
  if (nrow(consensus_tbl) > 0 && any(consensus_tbl$evidence_count >= 2, na.rm = TRUE)) {
    reliability_notes <- c(reliability_notes, "Some taxa were supported by more than one differential method.")
  }

  caution_notes <- character(0)
  if (sum(diff_table$significance == "significant", na.rm = TRUE) < 1) {
    caution_notes <- c(caution_notes, "No FDR-corrected significant taxa were detected; displayed top taxa are exploratory only.")
  }
  caution_notes <- c(
    caution_notes,
    "Do not infer biological mechanism or causality from taxon names alone.",
    "Differential abundance candidates require validation in independent samples or targeted experiments."
  )

  figures_generated <- list(
    diff_top_barplot = "figures/diff_top_barplot.png",
    diff_taxa_barplot = "figures/diff_taxa_barplot.png",
    diff_volcano = "figures/diff_volcano.png",
    diff_heatmap = "figures/diff_heatmap.png",
    diff_lefse_lda = if (!is.null(lefse_res$figure$png)) "figures/diff_lefse_lda.png" else NULL
  )

  summary_path <- save_diff_summary_json(
    diff_table = diff_table,
    group_var = group_var,
    tax_level = prepared$tax_level,
    job_dir = job_dir,
    figures_generated = figures_generated,
    warnings = warnings,
    extra_summary = list(
      test_method = primary$method,
      lefse_available = isTRUE(lefse_res$available),
      ancombc_available = isTRUE(ancombc_res$available),
      consensus_available = nrow(consensus_tbl) > 0,
      reliability_notes = unique(reliability_notes),
      caution_notes = unique(caution_notes)
    )
  )

  list(
    diff_table = diff_table,
    effect_size = effect_table,
    lefse = lefse_res$table,
    ancombc = ancombc_res$table,
    consensus = consensus_tbl,
    diff_table_path = diff_table_path,
    significant_table_path = sig_path,
    effect_size_path = effect_path,
    summary_path = summary_path,
    figure_paths = figures_generated,
    warnings = warnings
  )
}
