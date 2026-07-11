# JSON and reproducibility record helpers.

write_json_pretty <- function(x, path, auto_unbox = TRUE) {
  assert_non_empty_string(path, "path")
  ensure_dir(dirname(path))
  x <- sanitize_strings_for_output(x)
  jsonlite::write_json(
    x,
    path = path,
    auto_unbox = auto_unbox,
    pretty = TRUE,
    null = "null"
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

append_reproducibility <- function(job_dir, params) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("append_reproducibility(): job_dir does not exist.", call. = FALSE)

  repro_path <- file.path(job_dir, "reproducibility.json")
  cur <- list()
  if (file.exists(repro_path)) {
    cur <- tryCatch(jsonlite::read_json(repro_path, simplifyVector = TRUE), error = function(e) list())
    if (!is.list(cur)) cur <- list()
  }

  # Shallow merge: later keys override earlier keys.
  if (!is.list(params)) stop("append_reproducibility(): params must be a list.", call. = FALSE)
  merged <- modifyList(cur, params, keep.null = TRUE)
  write_json_pretty(merged, repro_path, auto_unbox = TRUE)
}
