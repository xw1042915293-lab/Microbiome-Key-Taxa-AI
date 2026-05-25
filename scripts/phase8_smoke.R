# Phase 8 smoke test (render final reproducible HTML report)
#
# Usage:
#   Rscript scripts/phase8_smoke.R <job_dir>
#
# Or set env var:
#   JOB_DIR=/path/to/job Rscript scripts/phase8_smoke.R
#
# Rules:
# - Run at most once.
# - If FAIL, stop and report the error (no auto-retry here).

args <- commandArgs(trailingOnly = TRUE)
job_dir <- Sys.getenv("JOB_DIR", unset = "")
if (nchar(job_dir) < 1 && length(args) >= 1) job_dir <- args[[1]]

pick_latest_job_dir <- function(results_dir = "results") {
  if (!dir.exists(results_dir)) return(NULL)
  dirs <- list.dirs(results_dir, full.names = TRUE, recursive = FALSE)
  if (length(dirs) < 1) return(NULL)
  info <- file.info(dirs)
  info$path <- rownames(info)
  info <- info[order(info$mtime, decreasing = TRUE), , drop = FALSE]
  as.character(info$path[[1]])
}

if (nchar(job_dir) < 1) job_dir <- pick_latest_job_dir("results")
if (is.null(job_dir) || nchar(job_dir) < 1) {
  stop("Phase8 smoke: job_dir not provided and no results/job_* directory found.", call. = FALSE)
}

job_dir <- normalizePath(job_dir, winslash = "/", mustWork = TRUE)
if (!dir.exists(job_dir)) stop("Phase8 smoke: job_dir not found: ", job_dir, call. = FALSE)

source("R/00_packages.R", local = TRUE)
source("R/01_config.R", local = TRUE)
source("R/02_utils_file.R", local = TRUE)
source("R/04_utils_json.R", local = TRUE)
source("R/report_render.R", local = TRUE)

cat("Phase8 smoke: job_dir =", job_dir, "\n")

report_path <- render_report_html(job_dir)
if (!file.exists(report_path)) stop("Phase8 smoke: report not found: ", report_path, call. = FALSE)

html <- paste(readLines(report_path, warn = FALSE), collapse = "\n")

need_sections <- c(
  "Alpha Diversity",
  "Beta Diversity",
  "Differential Abundance",
  "Machine Learning",
  "Network Analysis",
  "Key Taxa Score",
  "Reproducibility Record"
)

missing <- need_sections[!vapply(need_sections, function(s) grepl(s, html, fixed = TRUE), logical(1))]
if (length(missing) > 0) {
  stop("Phase8 smoke: report missing sections: ", paste(missing, collapse = ", "), call. = FALSE)
}

cat("Phase8 smoke: PASS\n")

