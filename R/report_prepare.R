# Phase 8: Report preparation helpers (no new analysis).
#
# This file only collects already-generated outputs under job_dir and prepares
# lightweight metadata for Quarto rendering. It must not call any LLM API.

prepare_report_context <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("prepare_report_context(): job_dir not found: ", job_dir, call. = FALSE)

  p <- function(...) normalizePath(file.path(job_dir, ...), winslash = "/", mustWork = FALSE)
  e <- function(x) isTRUE(file.exists(x))

  # Expected inputs/outputs for the final report.
  paths <- list(
    tables = list(
      data_check_summary = p("tables", "data_check_summary.csv"),
      alpha_diversity = alpha_output_path(job_dir, "tables", "alpha_diversity.csv", legacy_filename = "alpha_diversity.csv", existing = TRUE),
      alpha_stats = alpha_output_path(job_dir, "tables", "alpha_stats.csv", legacy_filename = "alpha_stats.csv", existing = TRUE),
      beta_pcoa_coordinates = beta_output_path(job_dir, "tables", "beta_pcoa_coordinates.csv", legacy_filename = "beta_pcoa_coordinates.csv", existing = TRUE),
      beta_permanova = beta_output_path(job_dir, "tables", "beta_permanova.csv", legacy_filename = "beta_permanova.csv", existing = TRUE),
      beta_dispersion = beta_output_path(job_dir, "tables", "beta_dispersion.csv", existing = TRUE),
      beta_dispersion_distances = beta_output_path(job_dir, "tables", "beta_dispersion_distances.csv", existing = TRUE),
      differential_taxa = p("tables", "differential_taxa.csv"),
      differential_taxa_significant = p("tables", "differential_taxa_significant.csv"),
      differential_taxa_group_summary = p("tables", "differential_taxa_group_summary.csv"),
      differential_taxa_pairwise = p("tables", "differential_taxa_pairwise.csv"),
      ml_feature_importance = p("tables", "ml_feature_importance.csv"),
      ml_model_metrics = p("tables", "ml_model_metrics.csv"),
      ml_cross_validation_metrics = p("tables", "ml_cross_validation_metrics.csv"),
      ml_out_of_fold_predictions = p("tables", "ml_out_of_fold_predictions.csv"),
      ml_confusion_matrix = p("tables", "ml_confusion_matrix.csv"),
      ml_feature_importance_stability = p("tables", "ml_feature_importance_stability.csv"),
      ml_top_taxa_abundance_statistics = p("tables", "ml_top_taxa_abundance_statistics.csv"),
      ml_permutation_test = p("tables", "ml_permutation_test.csv"),
      network_nodes = p("tables", "network_nodes.csv"),
      network_edges = p("tables", "network_edges.csv"),
      network_statistics = p("tables", "network_statistics.csv"),
      key_taxa_score = p("tables", "key_taxa_score.csv"),
      key_taxa_top20 = p("tables", "key_taxa_top20.csv"),
      integration_debug = p("logs", "integration_debug.csv")
    ),
    json = list(
      diff_summary = p("json", "diff_summary.json"),
      ml_summary = p("json", "ml_summary.json"),
      network_summary = p("json", "network_summary.json"),
      key_taxa_summary = p("json", "key_taxa_summary.json"),
      llm_request_diff = p("json", "llm_request_diff.json"),
      llm_response_diff = p("json", "llm_response_diff.json"),
      reproducibility = p("reproducibility.json")
    ),
    ai = list(
      diff_interpretation = p("ai", "diff_interpretation.md"),
      key_taxa_interpretation = p("ai", "key_taxa_interpretation.md"),
      methods = p("ai", "methods.md"),
      figure_legends = p("ai", "figure_legends.md"),
      llm_diff_interpretation = p("ai", "llm_diff_interpretation.md"),
      llm_methods = p("ai", "llm_methods.md"),
      llm_figure_legends = p("ai", "llm_figure_legends.md")
    ),
    figures = list(
      alpha_diversity_overview = alpha_overview_figure_path(job_dir, "violin_box", "png", existing = TRUE),
      alpha_observed_boxplot = alpha_metric_figure_path(job_dir, "Observed", "boxplot", "png", existing = TRUE),
      alpha_chao1_boxplot = alpha_metric_figure_path(job_dir, "Chao1", "boxplot", "png", existing = TRUE),
      alpha_shannon_boxplot = alpha_metric_figure_path(job_dir, "Shannon", "boxplot", "png", existing = TRUE),
      alpha_simpson_boxplot = alpha_metric_figure_path(job_dir, "Simpson", "boxplot", "png", existing = TRUE),
      beta_pcoa_bray = {
        path <- beta_figure_path(job_dir, "ellipse_centroid", "png", existing = TRUE)
        if (!file.exists(path)) path <- beta_figure_path(job_dir, "points", "png", existing = TRUE)
        path
      },
      beta_dispersion = beta_figure_path(job_dir, "dispersion", "png", existing = TRUE),
      diff_taxa_directional = p("figures", "diff_taxa_directional.png"),
      diff_taxa_balanced = p("figures", "diff_taxa_balanced.png"),
      diff_taxa_heatmap = p("figures", "diff_taxa_heatmap.png"),
      diff_taxa_barplot = p("figures", "diff_taxa_barplot.png"),
      ml_importance = p("figures", "ml_importance.png"),
      ml_confusion_matrix = p("figures", "ml_confusion_matrix.png"),
      ml_roc = p("figures", "ml_figure_roc.png"),
      ml_pr = p("figures", "ml_figure_pr.png"),
      ml_feature_importance_stability = p("figures", "ml_figure_feature_importance.png"),
      ml_top_taxa = p("figures", "ml_figure_top_taxa.png"),
      ml_performance_distribution = p("figures", "ml_figure_performance_distribution.png"),
      ml_combined = p("figures", "ml_figure_combined.png"),
      network_plot = p("figures", "network_plot.png"),
      network_combined = p("figures", "network_figure_combined.png"),
      network_centrality = p("figures", "network_centrality.png"),
      network_edge_composition = p("figures", "network_edge_composition.png"),
      key_taxa_score_barplot = p("figures", "key_taxa_score_barplot.png")
    )
  )

  # Existence map for quick conditionals in the template.
  exists <- list(
    tables = lapply(paths$tables, e),
    json = lapply(paths$json, e),
    ai = lapply(paths$ai, e),
    figures = lapply(paths$figures, e)
  )

  # Minimal reproducibility fields (best-effort, never fail report rendering on missing keys).
  repro <- NULL
  if (exists$json$reproducibility) {
    repro <- tryCatch(jsonlite::read_json(paths$json$reproducibility, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.list(repro)) repro <- NULL
  }

  # Ensure data_check_summary.csv is consistent with final parameters (e.g., group_var).
  # This is report-only housekeeping: it does not change any analysis results.
  .update_data_check_summary <- function(check_csv_path, repro_obj) {
    if (!is.character(check_csv_path) || length(check_csv_path) != 1) return(invisible(FALSE))
    if (!file.exists(check_csv_path)) return(invisible(FALSE))
    if (!is.list(repro_obj) || is.null(repro_obj$parameters) || is.null(repro_obj$parameters$group_var)) return(invisible(FALSE))

    group_var <- as.character(repro_obj$parameters$group_var)
    if (!nzchar(group_var)) return(invisible(FALSE))

    checks <- tryCatch(readr::read_csv(check_csv_path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    if (is.null(checks) || !is.data.frame(checks)) {
      checks <- data.frame(check_name = character(), status = character(), message = character(), stringsAsFactors = FALSE)
    }
    if (!all(c("check_name", "status", "message") %in% names(checks))) {
      # Do not try to coerce unknown formats.
      return(invisible(FALSE))
    }

    if (nrow(checks) > 0) {
      msg <- as.character(checks$message)
      # Fix the common contradictory message when a final group_var is present.
      msg[grepl("No group variable selected yet", msg, fixed = TRUE)] <- paste0("group_var = ", group_var)
      checks$message <- msg
    }

    has_gv <- nrow(checks) > 0 && any(checks$check_name == "group_variable" & !is.na(checks$check_name))
    if (!has_gv) {
      checks <- rbind(
        checks,
        data.frame(
          check_name = "group_variable",
          status = "info",
          message = paste0("group_var = ", group_var),
          stringsAsFactors = FALSE
        )
      )
    }

    tryCatch(readr::write_csv(checks, check_csv_path, na = ""), error = function(e) NULL)
    invisible(TRUE)
  }
  .update_data_check_summary(paths$tables$data_check_summary, repro)

  list(
    job_dir = normalizePath(job_dir, winslash = "/", mustWork = TRUE),
    paths = paths,
    exists = exists,
    reproducibility = repro
  )
}
