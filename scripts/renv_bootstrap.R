# Bootstraps renv for this project.
# If a valid renv.lock exists: restore().
# Otherwise: init() + install minimal dependencies + snapshot() to generate a valid lockfile.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) == 1) sub("^--file=", "", file_arg) else NULL
if (!is.null(script_path) && nzchar(script_path)) {
  project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
  setwd(project_root)
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

lock_ok <- FALSE
if (file.exists("renv.lock")) {
  lf <- try(renv::lockfile_read("renv.lock"), silent = TRUE)
  lock_ok <- !inherits(lf, "try-error")
  if (lock_ok) {
    # Basic sanity: every recorded package should have Source + Version.
    pkgs <- lf$Packages
    if (is.null(pkgs) || !is.list(pkgs) || length(pkgs) == 0) {
      lock_ok <- FALSE
    } else {
      has_fields <- vapply(pkgs, function(rec) {
        is.list(rec) && !is.null(rec$Source) && nzchar(as.character(rec$Source)) &&
          !is.null(rec$Version) && nzchar(as.character(rec$Version))
      }, logical(1))
      lock_ok <- all(has_fields)
    }
  }
  if (!lock_ok) {
    bak <- paste0("renv.lock.invalid.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.rename("renv.lock", bak)
  }
}

if (!file.exists("renv/activate.R")) {
  renv::init(bare = TRUE)
}

if (lock_ok) {
  renv::restore(prompt = FALSE)
} else {
  pkgs <- c(
    "shiny",
    "bslib",
    "DT",
    "data.table",
    "dplyr",
    "readr",
    "stringr",
    "tibble",
    "purrr",
    "jsonlite",
    "yaml",
    "DBI",
    "RSQLite",
    "digest"
  )
  install.packages(pkgs, repos = "https://cloud.r-project.org")
  renv::snapshot(prompt = FALSE)
}
