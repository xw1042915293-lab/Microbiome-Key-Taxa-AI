# Differential abundance analysis (Phase 3).
# Methods: Wilcoxon (2 groups), Kruskal-Wallis (>= 3 groups), FDR correction.
# Outputs:
# - tables/differential_taxa.csv
# - tables/differential_taxa_significant.csv
# - json/diff_summary.json
# - figures/diff_volcano.pdf + .png
# - figures/diff_taxa_barplot.pdf + .png

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

run_diff_analysis <- function(dataset, group_var, tax_level, job_dir) {
  if (is.null(dataset)) stop("run_diff_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_diff_analysis(): job_dir not found: ", job_dir, call. = FALSE)

  samp <- dataset$sample_table
  if (!group_var %in% names(samp)) stop("run_diff_analysis(): group_var not found in sample_table: ", group_var, call. = FALSE)

  # Aggregate abundance at the requested taxonomic level.
  dataset$cal_abund()
  if (!tax_level %in% names(dataset$taxa_abund)) {
    stop("run_diff_analysis(): tax_level '", tax_level, "' not available. Choose from: ",
         paste(names(dataset$taxa_abund), collapse = ", "), call. = FALSE)
  }
  abund <- as.data.frame(dataset$taxa_abund[[tax_level]])

  groups <- as.factor(samp[[group_var]])
  names(groups) <- rownames(samp)
  glev <- levels(groups)
  if (length(glev) < 2) stop("run_diff_analysis(): need >= 2 groups, got ", length(glev), call. = FALSE)

  # For each taxon, run test and compute log2FC.
  taxa_names <- rownames(abund)
  results <- data.frame(
    taxon = taxa_names,
    tax_level = tax_level,
    p_value = NA_real_,
    log2fc = NA_real_,
    mean_abundance = NA_real_,
    prevalence = NA_real_,
    stringsAsFactors = FALSE
  )

  sample_ids <- colnames(abund)
  for (i in seq_along(taxa_names)) {
    vals <- as.numeric(abund[i, ])
    names(vals) <- sample_ids

    # Split by group
    grp_vals <- split(vals, groups[names(vals)])

    results$mean_abundance[i] <- mean(vals, na.rm = TRUE)
    results$prevalence[i] <- sum(vals > 0, na.rm = TRUE) / length(vals)

    if (length(glev) == 2) {
      g1 <- grp_vals[[glev[1]]]
      g2 <- grp_vals[[glev[2]]]
      if (length(g1) < 2 || length(g2) < 2) next
      results$p_value[i] <- tryCatch(
        stats::wilcox.test(g1, g2, exact = FALSE)$p.value,
        error = function(e) NA_real_
      )
      m1 <- mean(g1, na.rm = TRUE)
      m2 <- mean(g2, na.rm = TRUE)
      results$log2fc[i] <- if (m1 > 0 && m2 > 0) log2(m2 / m1) else NA_real_
    } else {
      grp_list <- lapply(grp_vals, function(x) if (length(x) >= 2) x else numeric(0))
      grp_list <- Filter(function(x) length(x) >= 2, grp_list)
      if (length(grp_list) < 2) next
      results$p_value[i] <- tryCatch(
        stats::kruskal.test(vals ~ groups[names(vals)])$p.value,
        error = function(e) NA_real_
      )
      results$log2fc[i] <- NA_real_
    }
  }

  results <- results[!is.na(results$p_value), , drop = FALSE]
  results$fdr <- stats::p.adjust(results$p_value, method = "fdr")
  results$significant <- results$fdr < 0.05
  results$display_taxon <- clean_taxon_label(results$taxon)
  results <- results[order(results$fdr), ]
  rownames(results) <- NULL

  out_csv <- file.path(job_dir, "tables", "differential_taxa.csv")
  ensure_dir(dirname(out_csv))
  readr::write_csv(results, out_csv)

  # Always create a "significant-only" CSV (may be empty, but must keep columns)
  sig_csv <- file.path(job_dir, "tables", "differential_taxa_significant.csv")
  ensure_dir(dirname(sig_csv))
  sig <- results[isTRUE(results$significant), , drop = FALSE]
  if (nrow(sig) == 0) {
    # Empty table with full column set
    sig <- results[0, , drop = FALSE]
  }
  readr::write_csv(sig, sig_csv)

  # Volcano plot (always)
  volcano_paths <- plot_diff_volcano(results, group_var, job_dir)

  # Barplot: significant taxa if available; otherwise exploratory top by raw p-value
  exploratory <- nrow(results) == 0 || !any(results$significant)
  barplot_paths <- plot_diff_taxa_barplot(
    diff_table = results,
    group_var = group_var,
    job_dir = job_dir,
    exploratory = exploratory
  )

  # Summary JSON (always)
  summary_path <- save_diff_summary_json(
    diff_table = results,
    group_var = group_var,
    tax_level = tax_level,
    job_dir = job_dir
  )

  list(
    diff_table_path = normalizePath(out_csv, winslash = "/", mustWork = TRUE),
    significant_table_path = normalizePath(sig_csv, winslash = "/", mustWork = TRUE),
    diff_summary_path = summary_path,
    figure_paths = list(
      volcano = volcano_paths,
      barplot = barplot_paths
    ),
    diff_table = results
  )
}

plot_diff_volcano <- function(diff_table, group_var, job_dir) {
  if (!is.data.frame(diff_table)) stop("plot_diff_volcano(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")

  df <- diff_table

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$log2fc, y = -log10(pmax(.data$fdr, 1e-300))))

  if (nrow(df) > 0 && any(df$significant)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data$significant), alpha = 0.7, size = 2) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "#E74C3C", "FALSE" = "#7F8C8D"),
        labels = c("TRUE" = "FDR < 0.05", "FALSE" = "Not significant")
      )
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.7, size = 2, color = "#7F8C8D")
  }

  p <- p +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title = "Differential abundance (volcano plot)",
      x = "log2 Fold Change",
      y = "-log10(FDR)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)

  fig_pdf <- file.path(job_dir, "figures", "diff_volcano.pdf")
  fig_png <- file.path(job_dir, "figures", "diff_volcano.png")
  save_plot_pdf_png(p, fig_pdf, fig_png)
}

summarize_diff_for_ai <- function(diff_table, group_var) {
  if (!is.data.frame(diff_table)) stop("summarize_diff_for_ai(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")

  sig <- diff_table[diff_table$significant, , drop = FALSE]
  list(
    analysis_type = "differential_abundance",
    group_variable = group_var,
    n_tested = nrow(diff_table),
    n_significant = nrow(sig),
    top_taxa = head(sig, 20)[, c("taxon", "tax_level", "p_value", "fdr", "log2fc", "mean_abundance")],
    method = if (any(!is.na(diff_table$log2fc))) "wilcoxon" else "kruskal-wallis"
  )
}

plot_diff_taxa_barplot <- function(diff_table, group_var, job_dir, exploratory = FALSE, top_n = 20) {
  if (!is.data.frame(diff_table)) stop("plot_diff_taxa_barplot(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  if (!is.logical(exploratory) || length(exploratory) != 1) stop("plot_diff_taxa_barplot(): exploratory must be TRUE/FALSE.", call. = FALSE)
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) stop("plot_diff_taxa_barplot(): top_n must be >= 1.", call. = FALSE)

  df <- diff_table
  if (nrow(df) == 0) {
    # Create an empty placeholder plot (still saved)
    p <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::labs(
        title = "Differential taxa (barplot)",
        subtitle = "No taxa available for plotting"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    if (exploratory) {
      sel <- df[order(df$p_value), , drop = FALSE]
      sel <- utils::head(sel, top_n)
      subtitle <- "Exploratory: no FDR-significant taxa"
    } else {
      sel <- df[df$significant, , drop = FALSE]
      sel <- sel[order(sel$fdr), , drop = FALSE]
      sel <- utils::head(sel, top_n)
      subtitle <- NULL
    }

    sel$neglog10_p <- -log10(pmax(sel$p_value, 1e-300))
    sel$taxon <- factor(sel$taxon, levels = rev(sel$taxon))

    p <- ggplot2::ggplot(sel, ggplot2::aes(x = taxon, y = neglog10_p, fill = significant)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#7F8C8D")) +
      ggplot2::labs(
        title = "Differential taxa (barplot)",
        subtitle = subtitle,
        x = NULL,
        y = "-log10(raw p-value)",
        fill = NULL
      ) +
      ggplot2::theme_minimal(base_size = 12)
  }

  fig_pdf <- file.path(job_dir, "figures", "diff_taxa_barplot.pdf")
  fig_png <- file.path(job_dir, "figures", "diff_taxa_barplot.png")
  save_plot_pdf_png(p, fig_pdf, fig_png)
}

save_diff_summary_json <- function(diff_table, group_var, tax_level, job_dir) {
  if (!is.data.frame(diff_table)) stop("save_diff_summary_json(): diff_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("save_diff_summary_json(): job_dir not found: ", job_dir, call. = FALSE)

  sig <- diff_table[isTRUE(diff_table$significant), , drop = FALSE]
  n_sig <- nrow(sig)

  msg <- if (n_sig == 0) {
    "No significant taxa were detected under FDR < 0.05."
  } else {
    "Significant taxa were detected under FDR < 0.05."
  }

  top <- sig
  if (n_sig == 0 && nrow(diff_table) > 0) {
    top <- diff_table[order(diff_table$p_value), , drop = FALSE]
  }
  top <- utils::head(top, 20)

  # Keep JSON small and stable.
  top_taxa <- if (nrow(top) == 0) {
    list()
  } else {
    split(
      top[, intersect(c("taxon", "tax_level", "p_value", "fdr", "log2fc", "mean_abundance", "prevalence"), names(top)), drop = FALSE],
      seq_len(nrow(top))
    )
  }

  summary <- list(
    analysis_type = "differential_abundance",
    group_variable = group_var,
    tax_level = tax_level,
    method = if (any(!is.na(diff_table$log2fc))) "wilcoxon" else "kruskal-wallis",
    p_adjust_method = "fdr",
    significance_cutoff = list(fdr = 0.05),
    n_total_taxa = nrow(diff_table),
    n_significant_taxa = n_sig,
    top_taxa = top_taxa,
    message = msg
  )

  out_path <- file.path(job_dir, "json", "diff_summary.json")
  write_json_pretty(summary, out_path, auto_unbox = TRUE)
}
