# Run only input validation, microeco construction and differential abundance.
# Usage:
#   Rscript scripts/run_diff_only.R [data_dir] [group_var] [tax_level]

source("global.R")

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "data"
group_var <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else "Location"
tax_level <- if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else "Genus"

data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
source_paths <- c(
  abundance = file.path(data_dir, "abundance.tsv"),
  metadata = file.path(data_dir, "metadata.tsv"),
  taxonomy = file.path(data_dir, "taxonomy.tsv")
)
missing_paths <- source_paths[!file.exists(source_paths)]
if (length(missing_paths)) {
  stop("Missing input file(s): ", paste(basename(missing_paths), collapse = ", "), call. = FALSE)
}

job_dir <- create_job_dir()
job_id <- basename(job_dir)
log_path <- file.path(job_dir, "logs", "run.log")
db_upsert_job(job_id, job_dir, status = "diff_only_running")

tryCatch({
  saved_paths <- c(
    abundance = copy_to_job_input(source_paths[["abundance"]], file.path(job_dir, "input", "abundance.tsv")),
    metadata = copy_to_job_input(source_paths[["metadata"]], file.path(job_dir, "input", "metadata.tsv")),
    taxonomy = copy_to_job_input(source_paths[["taxonomy"]], file.path(job_dir, "input", "taxonomy.tsv"))
  )
  for (nm in names(saved_paths)) {
    db_insert_job_file(job_id, nm, basename(source_paths[[nm]]),
                       file.path("input", paste0(nm, ".tsv")), file_md5(saved_paths[[nm]]))
  }

  inputs <- read_microbiome_inputs(
    abundance_path = saved_paths[["abundance"]],
    metadata_path = saved_paths[["metadata"]],
    taxonomy_path = saved_paths[["taxonomy"]]
  )
  checks <- run_all_data_checks(inputs, group_var = group_var)
  save_data_check_summary(checks, job_dir)
  if (identical(checks$status, "error")) {
    stop("Input validation failed; see tables/data_check_summary.csv.", call. = FALSE)
  }

  dataset <- build_microeco_dataset(inputs$abundance, inputs$metadata, inputs$taxonomy)
  dataset_path <- save_microeco_dataset(dataset, job_dir)
  diff <- run_phase3_workflow(
    dataset = dataset,
    job_dir = job_dir,
    group_var = group_var,
    tax_level = tax_level,
    log_path = log_path
  )$diff

  append_reproducibility(job_dir, list(
    parameters = list(group_var = group_var, tax_level = tax_level),
    diff_only = list(
      source_data_dir = data_dir,
      completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  ))
  db_upsert_job(job_id, job_dir, status = "diff_only_done")

  result <- diff$diff_table
  cat("JOB_ID=", job_id, "\n", sep = "")
  cat("JOB_DIR=", job_dir, "\n", sep = "")
  cat("GROUP_VAR=", group_var, "\n", sep = "")
  cat("TAX_LEVEL=", tax_level, "\n", sep = "")
  cat("N_TAXA=", nrow(result), "\n", sep = "")
  cat("N_TESTED=", sum(result$tested), "\n", sep = "")
  cat("N_SIGNIFICANT=", sum(result$significant), "\n", sep = "")
  cat("FIGURE=", diff$figure_paths$barplot$png, "\n", sep = "")
}, error = function(e) {
  db_upsert_job(job_id, job_dir, status = "diff_only_failed")
  workflow_log_step(log_path, "failed", "diff_only", conditionMessage(e))
  stop(conditionMessage(e), call. = FALSE)
})
