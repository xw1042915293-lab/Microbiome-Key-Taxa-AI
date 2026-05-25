results_root <- file.path(getwd(), "results")
job_dirs <- list.dirs(results_root, recursive = FALSE, full.names = TRUE)
job_dirs <- sort(job_dirs, decreasing = TRUE)

required_inputs <- c(
  file.path("json", "diff_summary.json"),
  file.path("ai", "diff_interpretation.md"),
  file.path("ai", "methods.md"),
  file.path("ai", "figure_legends.md")
)

job_dir <- NULL
for (candidate in job_dirs) {
  if (all(file.exists(file.path(candidate, required_inputs)))) {
    job_dir <- candidate
    break
  }
}

if (is.null(job_dir)) {
  message("FAIL: no completed Phase 4A job_dir found.")
  stop("Phase 4B smoke test failed.", call. = FALSE)
}

source(file.path(getwd(), "R", "ai_rules.R"))
source(file.path(getwd(), "R", "ai_interpretation.R"))
source(file.path(getwd(), "R", "ai_prompt.R"))
source(file.path(getwd(), "R", "ai_client.R"))

result <- tryCatch(
  write_llm_outputs(job_dir, config_path = file.path(getwd(), "config.yml")),
  error = function(e) e
)

required_outputs <- c(
  file.path(job_dir, "json", "llm_request_diff.json"),
  file.path(job_dir, "json", "llm_response_diff.json"),
  file.path(job_dir, "ai", "llm_diff_interpretation.md"),
  file.path(job_dir, "ai", "llm_methods.md"),
  file.path(job_dir, "ai", "llm_figure_legends.md")
)

if (inherits(result, "error")) {
  message("FAIL: ", conditionMessage(result))
  stop(result, call. = FALSE)
}

if (!all(file.exists(required_outputs))) {
  message("FAIL: one or more Phase 4B outputs were not created.")
  stop("Phase 4B smoke test failed.", call. = FALSE)
}

if (isTRUE(result$api_key_present)) {
  message("PASS")
} else {
  message("SKIPPED: KKAI_API_KEY is not set; used local Phase 4A text as fallback.")
  message("PASS")
}

