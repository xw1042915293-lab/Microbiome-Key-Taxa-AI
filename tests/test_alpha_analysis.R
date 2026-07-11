source("R/02_utils_file.R")
source("R/03_utils_plot.R")
source("R/analysis_alpha.R")

local({
  job_dir <- tempfile("alpha_analysis_test_")
  dir.create(job_dir, recursive = TRUE)
  on.exit(unlink(job_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sample_ids <- paste0("S", seq_len(12))
  groups <- rep(c("Control", "TreatmentA", "TreatmentB"), each = 4)
  dataset <- new.env(parent = emptyenv())
  dataset$sample_table <- data.frame(SampleID = sample_ids, Group = groups, stringsAsFactors = FALSE)
  dataset$alpha_diversity <- data.frame(
    Observed = c(90, 94, 92, 96, 110, 112, 108, 114, 125, 128, 122, 130),
    Chao1 = c(94, 98, 96, 100, 115, 117, 113, 119, 132, 135, 129, 137),
    Shannon = c(3.1, 3.2, 3.0, 3.3, 3.7, 3.8, 3.6, 3.9, 4.1, 4.2, 4.0, 4.3),
    Simpson = c(0.88, 0.89, 0.87, 0.90, 0.93, 0.94, 0.92, 0.95, 0.96, 0.97, 0.95, 0.98),
    row.names = sample_ids,
    check.names = FALSE
  )
  dataset$cal_alphadiv <- function() invisible(NULL)

  result <- run_alpha_analysis(dataset, group_var = "Group", job_dir = job_dir)
  stats <- readr::read_csv(file.path(job_dir, "alpha", "tables", "alpha_stats.csv"), show_col_types = FALSE)

  stopifnot(identical(stats$index, c("Observed", "Chao1", "Shannon", "Simpson")))
  stopifnot(all(stats$test == "kruskal"))
  stopifnot(all(stats$n_samples == 12L))
  stopifnot(all(stats$n_groups == 3L))
  stopifnot(all(is.finite(stats$p_value)))
  stopifnot(all(is.finite(stats$fdr)))
  stopifnot(identical(result$metrics, c("Observed", "Chao1", "Shannon", "Simpson")))

  plot_types <- alpha_plot_type_spec()$type
  expected <- c(
    unlist(lapply(alpha_metric_spec()$index, function(metric) {
      unlist(lapply(plot_types, function(type) {
        c(
          alpha_metric_figure_path(job_dir, metric, type, "png"),
          alpha_metric_figure_path(job_dir, metric, type, "pdf")
        )
      }), use.names = FALSE)
    }), use.names = FALSE),
    unlist(lapply(plot_types, function(type) {
      c(
        alpha_overview_figure_path(job_dir, type, "png"),
        alpha_overview_figure_path(job_dir, type, "pdf")
      )
    }), use.names = FALSE)
  )
  stopifnot(all(file.exists(expected)))
  stopifnot(!file.exists(file.path(job_dir, "tables", "alpha_diversity.csv")))
  stopifnot(!file.exists(file.path(job_dir, "figures", "alpha_shannon_boxplot.png")))
})
