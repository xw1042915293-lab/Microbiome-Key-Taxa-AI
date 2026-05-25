setwd("D:/Microbiome Key Taxa AI")

source("global.R")

results_root <- file.path(getwd(), "results")
job_dirs <- list.dirs(results_root, recursive = FALSE, full.names = TRUE)
job_dirs <- sort(job_dirs, decreasing = TRUE)

required_inputs <- c(file.path("objects", "microeco_dataset.rds"))

job_dir <- NULL
for (candidate in job_dirs) {
  if (all(file.exists(file.path(candidate, required_inputs)))) {
    job_dir <- candidate
    break
  }
}

if (is.null(job_dir)) {
  message("FAIL: no Phase 2 job_dir found.")
  stop("Phase 6 smoke test failed.", call. = FALSE)
}

dataset <- readRDS(file.path(job_dir, "objects", "microeco_dataset.rds"))

result <- tryCatch(
  run_phase6_workflow(
    dataset = dataset,
    job_dir = job_dir,
    tax_level = "Genus",
    rho_cutoff = 0.6,
    p_cutoff = 0.05
  ),
  error = function(e) e
)

required_outputs <- c(
  file.path(job_dir, "tables", "network_nodes.csv"),
  file.path(job_dir, "tables", "network_edges.csv"),
  file.path(job_dir, "json", "network_summary.json"),
  file.path(job_dir, "figures", "network_plot.png"),
  file.path(job_dir, "figures", "network_plot.pdf")
)

if (inherits(result, "error")) {
  message("FAIL: ", conditionMessage(result))
  stop(result, call. = FALSE)
}

if (!all(file.exists(required_outputs))) {
  message("FAIL: one or more Phase 6 outputs were not created.")
  stop("Phase 6 smoke test failed.", call. = FALSE)
}

summary <- jsonlite::read_json(file.path(job_dir, "json", "network_summary.json"), simplifyVector = TRUE)
if (!is.null(summary$n_edges) && identical(as.integer(summary$n_edges), 0L)) {
  stopifnot(file.exists(file.path(job_dir, "figures", "network_plot.png")))
  stopifnot(file.exists(file.path(job_dir, "figures", "network_plot.pdf")))
}

message("PASS")
