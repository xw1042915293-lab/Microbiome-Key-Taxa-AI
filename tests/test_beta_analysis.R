source("R/02_utils_file.R")
source("R/03_utils_plot.R")
source("R/analysis_beta.R")

local({
  set.seed(42)
  job_dir <- tempfile("beta_analysis_test_")
  dir.create(job_dir, recursive = TRUE)
  on.exit(unlink(job_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sample_ids <- paste0("S", seq_len(18))
  groups <- rep(c("Control", "TreatmentA", "TreatmentB"), each = 6)
  otu <- matrix(stats::rpois(40 * 18, lambda = 20), nrow = 40, ncol = 18)
  otu[1:8, groups == "TreatmentA"] <- otu[1:8, groups == "TreatmentA"] + 25
  otu[9:16, groups == "TreatmentB"] <- otu[9:16, groups == "TreatmentB"] + 35
  rownames(otu) <- paste0("F", seq_len(nrow(otu)))
  colnames(otu) <- sample_ids

  dataset <- new.env(parent = emptyenv())
  dataset$otu_table <- as.data.frame(otu, check.names = FALSE)
  dataset$sample_table <- data.frame(SampleID = sample_ids, Group = groups, stringsAsFactors = FALSE)

  result <- run_beta_analysis(dataset, group_var = "Group", job_dir = job_dir, distance = "bray")
  permanova <- readr::read_csv(result$permanova_path, show_col_types = FALSE)
  dispersion <- readr::read_csv(result$dispersion_path, show_col_types = FALSE)
  coords <- readr::read_csv(result$pcoa_coordinates_path, show_col_types = FALSE)

  stopifnot(nrow(coords) == 18L)
  stopifnot(all(c("PCo1", "PCo2", "SampleID", "Group") %in% names(coords)))
  stopifnot(any(!permanova$term %in% c("Residual", "Total")))
  stopifnot("R2" %in% names(permanova))
  stopifnot("p_value" %in% names(dispersion))
  stopifnot(file.exists(file.path(job_dir, "beta", "tables", "beta_dispersion_distances.csv")))

  expected <- unlist(lapply(beta_plot_view_spec()$view, function(view) {
    c(beta_figure_path(job_dir, view, "png"), beta_figure_path(job_dir, view, "pdf"))
  }), use.names = FALSE)
  stopifnot(all(file.exists(expected)))
  stopifnot(!file.exists(file.path(job_dir, "tables", "beta_permanova.csv")))
  stopifnot(!file.exists(file.path(job_dir, "figures", "beta_pcoa_bray.png")))
})
