# File and job directory helpers.

assert_non_empty_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || nchar(x) < 1) {
    stop(sprintf("'%s' must be a non-empty string.", name), call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ensure_dir <- function(path) {
  assert_non_empty_string(path, "path")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

job_id_now <- function() {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  rand <- paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  paste0("job_", ts, "_", rand)
}

create_job_dir <- function(job_id = NULL, results_dir = NULL) {
  if (is.null(job_id)) job_id <- job_id_now()
  assert_non_empty_string(job_id, "job_id")

  if (is.null(results_dir)) results_dir <- get_cfg("paths.results_dir", "results")
  assert_non_empty_string(results_dir, "results_dir")

  job_dir <- file.path(results_dir, job_id)

  ensure_dir(job_dir)
  ensure_dir(file.path(job_dir, "input"))
  ensure_dir(file.path(job_dir, "objects"))
  ensure_dir(file.path(job_dir, "tables"))
  ensure_dir(file.path(job_dir, "figures"))
  ensure_dir(file.path(job_dir, "json"))
  ensure_dir(file.path(job_dir, "ai"))
  ensure_dir(file.path(job_dir, "report"))
  ensure_dir(file.path(job_dir, "logs"))

  normalizePath(job_dir, winslash = "/", mustWork = TRUE)
}

file_md5 <- function(path) {
  assert_non_empty_string(path, "path")
  if (!file.exists(path)) stop("file_md5(): file does not exist: ", path, call. = FALSE)
  digest::digest(file = path, algo = "md5", serialize = FALSE)
}

copy_to_job_input <- function(uploaded_path, dest_path) {
  assert_non_empty_string(uploaded_path, "uploaded_path")
  assert_non_empty_string(dest_path, "dest_path")
  if (!file.exists(uploaded_path)) stop("Uploaded file not found: ", uploaded_path, call. = FALSE)

  ensure_dir(dirname(dest_path))
  ok <- file.copy(uploaded_path, dest_path, overwrite = TRUE)
  if (!isTRUE(ok)) stop("Failed to copy file to: ", dest_path, call. = FALSE)
  normalizePath(dest_path, winslash = "/", mustWork = TRUE)
}
