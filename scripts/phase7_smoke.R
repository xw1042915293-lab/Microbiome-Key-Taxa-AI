# Phase 7 smoke test
#
# Usage:
#   Rscript scripts/phase7_smoke.R <job_dir>
#
# Or set env var:
#   JOB_DIR=/path/to/job Rscript scripts/phase7_smoke.R

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

if (nchar(job_dir) < 1) {
  job_dir <- pick_latest_job_dir("results")
}

if (is.null(job_dir) || nchar(job_dir) < 1) {
  stop("Phase7 smoke: job_dir not provided and no results/job_* directory found.", call. = FALSE)
}

job_dir <- normalizePath(job_dir, winslash = "/", mustWork = TRUE)
if (!dir.exists(job_dir)) stop("Phase7 smoke: job_dir not found: ", job_dir, call. = FALSE)

source("R/00_packages.R", local = TRUE)
source("R/01_config.R", local = TRUE)
source("R/02_utils_file.R", local = TRUE)
source("R/04_utils_json.R", local = TRUE)
source("R/key_taxa_score.R", local = TRUE)

cat("Phase7 smoke: job_dir =", job_dir, "\n")

res <- calculate_key_taxa_score(
  diff_table = NULL,
  ml_table = NULL,
  network_nodes = NULL,
  job_dir = job_dir
)

required_outputs <- c(
  "tables/key_taxa_score.csv",
  "tables/key_taxa_top20.csv",
  "json/key_taxa_summary.json",
  "figures/key_taxa_score_barplot.png",
  "figures/key_taxa_score_barplot.pdf"
)
for (rel in required_outputs) {
  p <- file.path(job_dir, rel)
  if (!file.exists(p)) stop("Phase7 smoke: missing output: ", p, call. = FALSE)
}

score_path <- file.path(job_dir, "tables", "key_taxa_score.csv")
score <- readr::read_csv(score_path, show_col_types = FALSE, progress = FALSE)

required_cols <- c(
  "taxon",
  "tax_level",
  "differential_score",
  "ml_importance_score",
  "network_centrality_score",
  "key_taxa_score",
  "diff_fdr",
  "log2fc",
  "rf_importance",
  "degree",
  "betweenness",
  "evidence_count",
  "evidence_sources",
  "rank",
  "recommendation_level"
)
missing_cols <- setdiff(required_cols, names(score))
if (length(missing_cols) > 0) {
  stop("Phase7 smoke: key_taxa_score.csv missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

sum_path <- file.path(job_dir, "json", "key_taxa_summary.json")
sumj <- jsonlite::read_json(sum_path, simplifyVector = TRUE)
need_fields <- c("used_sources", "weights", "n_candidate_taxa", "top_taxa", "reliability")
missing_fields <- setdiff(need_fields, names(sumj))
if (length(missing_fields) > 0) {
  stop("Phase7 smoke: key_taxa_summary.json missing fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
}

cat("Phase7 smoke: PASS\n")

