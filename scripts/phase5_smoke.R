setwd("D:/Microbiome Key Taxa AI")

source("global.R")

results_root <- file.path(getwd(), "results")
job_dirs <- list.dirs(results_root, recursive = FALSE, full.names = TRUE)
job_dirs <- sort(job_dirs, decreasing = TRUE)

required_inputs <- c(
  file.path("objects", "microeco_dataset.rds")
)

job_dir <- NULL
for (candidate in job_dirs) {
  if (all(file.exists(file.path(candidate, required_inputs)))) {
    job_dir <- candidate
    break
  }
}

if (is.null(job_dir)) {
  message("FAIL: no Phase 2 job_dir found.")
  stop("Phase 5 smoke test failed.", call. = FALSE)
}

dataset <- readRDS(file.path(job_dir, "objects", "microeco_dataset.rds"))

result <- tryCatch(
  run_phase5_workflow(
    dataset = dataset,
    job_dir = job_dir,
    group_var = "Group",
    tax_level = "Genus"
  ),
  error = function(e) e
)

required_outputs <- c(
  file.path(job_dir, "tables", "ml_feature_importance.csv"),
  file.path(job_dir, "tables", "ml_model_metrics.csv"),
  file.path(job_dir, "json", "ml_summary.json"),
  file.path(job_dir, "figures", "ml_importance.png"),
  file.path(job_dir, "figures", "ml_importance.pdf"),
  file.path(job_dir, "figures", "ml_confusion_matrix.png"),
  file.path(job_dir, "figures", "ml_confusion_matrix.pdf")
)

if (inherits(result, "error")) {
  message("FAIL: ", conditionMessage(result))
  stop(result, call. = FALSE)
}

if (!all(file.exists(required_outputs))) {
  message("FAIL: one or more Phase 5 outputs were not created.")
  stop("Phase 5 smoke test failed.", call. = FALSE)
}

if (file.exists(file.path(job_dir, "json", "ml_summary.json"))) {
  summary <- jsonlite::read_json(file.path(job_dir, "json", "ml_summary.json"), simplifyVector = TRUE)
  if (identical(summary$n_classes, 2L)) {
    stopifnot(file.exists(file.path(job_dir, "figures", "ml_roc.png")))
    stopifnot(file.exists(file.path(job_dir, "figures", "ml_roc.pdf")))
  }
}

message("PASS")
