results_root <- file.path(getwd(), "results")
job_dirs <- list.dirs(results_root, recursive = FALSE, full.names = TRUE)
job_dirs <- sort(job_dirs, decreasing = TRUE)

required_inputs <- c(
  file.path("alpha", "tables", "alpha_stats.csv"),
  file.path("beta", "tables", "beta_permanova.csv"),
  file.path("tables", "differential_taxa.csv"),
  file.path("tables", "differential_taxa_significant.csv"),
  file.path("json", "diff_summary.json")
)

job_dir <- NULL
for (candidate in job_dirs) {
  if (all(file.exists(file.path(candidate, required_inputs)))) {
    job_dir <- candidate
    break
  }
}

if (is.null(job_dir)) {
  message("FAIL: no completed Phase 3 job_dir found.")
  stop("Phase 4 smoke test failed.", call. = FALSE)
}

source(file.path(getwd(), "R", "ai_rules.R"))
source(file.path(getwd(), "R", "ai_interpretation.R"))

result <- tryCatch(
  write_ai_outputs(job_dir),
  error = function(e) e
)

required_outputs <- file.path(job_dir, "ai", c(
  "diff_interpretation.md",
  "methods.md",
  "figure_legends.md"
))

if (inherits(result, "error")) {
  message("FAIL: ", conditionMessage(result))
  stop(result, call. = FALSE)
}

if (!all(file.exists(required_outputs))) {
  message("FAIL: one or more AI markdown files were not created.")
  stop("Phase 4 smoke test failed.", call. = FALSE)
}

message("PASS")
