# Download/export helpers for Shiny delivery (Phase 10).
# These helpers only package existing artifacts; they do not modify workflow outputs.

safe_zip_job_results <- function(job_dir, job_id, include_dirs = NULL, zip_basename = NULL) {
  if (is.null(include_dirs)) {
    include_dirs <- c("tables", "figures", "json", "ai", "report", "logs")
  }
  if (is.null(zip_basename)) {
    zip_basename <- paste0("microbiome_key_taxa_ai_", job_id, ".zip")
  }

  if (is.null(job_dir) || !nzchar(job_dir) || !dir.exists(job_dir)) {
    return(list(ok = FALSE, zip_path = NULL, message = "job_dir not found."))
  }

  zip_path <- file.path(tempdir(), zip_basename)
  if (file.exists(zip_path)) {
    # Avoid reusing stale file in long-running Shiny sessions.
    try(unlink(zip_path), silent = TRUE)
  }

  # Prefer zipping whole directories to preserve structure, and avoid external 'zip' dependency.
  dirs_abs <- vapply(include_dirs, function(d) file.path(job_dir, d), character(1))
  dirs_abs <- dirs_abs[dir.exists(dirs_abs)]
  if (length(dirs_abs) == 0) {
    return(list(ok = FALSE, zip_path = NULL, message = "No artifact directories found to ZIP."))
  }

  # Helper: single-quote for PowerShell and escape embedded single quotes.
  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  ok <- TRUE
  msg <- NULL

  # First try PowerShell Compress-Archive (available on modern Windows).
  tryCatch({
    paths_arg <- paste(vapply(dirs_abs, ps_quote, character(1)), collapse = ",")
    dest_arg <- ps_quote(zip_path)
    cmd <- paste0("Compress-Archive -Path ", paths_arg, " -DestinationPath ", dest_arg, " -Force")
    system2("powershell", args = c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = TRUE)
  }, error = function(e) {
    ok <<- FALSE
    msg <<- conditionMessage(e)
  })

  # Fallback: utils::zip if PowerShell failed and 'zip' is available.
  if ((!ok || !file.exists(zip_path)) && nzchar(Sys.which("zip"))) {
    ok <- TRUE
    msg <- NULL
    rel_dirs <- include_dirs[dir.exists(file.path(job_dir, include_dirs))]
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(job_dir)
    tryCatch({
      utils::zip(zipfile = zip_path, files = rel_dirs, flags = "-r9Xq")
    }, error = function(e) {
      ok <<- FALSE
      msg <<- conditionMessage(e)
    })
  }

  if (!ok || !file.exists(zip_path)) {
    return(list(ok = FALSE, zip_path = NULL, message = msg %||% "ZIP creation failed."))
  }
  list(ok = TRUE, zip_path = normalizePath(zip_path, winslash = "/", mustWork = FALSE), message = "OK")
}

safe_zip_selected_files <- function(base_dir, rel_files, zip_basename) {
  if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
    return(list(ok = FALSE, zip_path = NULL, message = "base_dir not found."))
  }
  if (is.null(rel_files) || length(rel_files) == 0) {
    return(list(ok = FALSE, zip_path = NULL, message = "No files requested."))
  }

  rel_files <- unique(rel_files[nzchar(rel_files)])
  rel_files <- rel_files[file.exists(file.path(base_dir, rel_files))]
  if (length(rel_files) == 0) {
    return(list(ok = FALSE, zip_path = NULL, message = "None of the requested files exist."))
  }

  zip_path <- file.path(tempdir(), zip_basename)
  if (file.exists(zip_path)) {
    try(unlink(zip_path), silent = TRUE)
  }

  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  abs_files <- normalizePath(file.path(base_dir, rel_files), winslash = "\\", mustWork = TRUE)
  ok <- TRUE
  msg <- NULL

  tryCatch({
    paths_arg <- paste(vapply(abs_files, ps_quote, character(1)), collapse = ",")
    dest_arg <- ps_quote(zip_path)
    cmd <- paste0("Compress-Archive -Path ", paths_arg, " -DestinationPath ", dest_arg, " -Force")
    system2("powershell", args = c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = TRUE)
  }, error = function(e) {
    ok <<- FALSE
    msg <<- conditionMessage(e)
  })

  if ((!ok || !file.exists(zip_path)) && nzchar(Sys.which("zip"))) {
    ok <- TRUE
    msg <- NULL
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(base_dir)
    tryCatch({
      utils::zip(zipfile = zip_path, files = rel_files, flags = "-r9Xq")
    }, error = function(e) {
      ok <<- FALSE
      msg <<- conditionMessage(e)
    })
  }

  if (!ok || !file.exists(zip_path)) {
    return(list(ok = FALSE, zip_path = NULL, message = msg %||% "ZIP creation failed."))
  }
  list(ok = TRUE, zip_path = normalizePath(zip_path, winslash = "/", mustWork = FALSE), message = "OK")
}
