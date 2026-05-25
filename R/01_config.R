# Central config loader.

get_app_config <- function() {
  cfg_path <- "config.yml"
  if (!file.exists(cfg_path)) {
    stop("config.yml not found at project root.", call. = FALSE)
  }

  cfg <- yaml::read_yaml(cfg_path)
  if (is.null(cfg$default)) {
    stop("config.yml must have a top-level 'default:' section.", call. = FALSE)
  }
  cfg$default
}

APP_CONFIG <- get_app_config()

get_cfg <- function(path, default = NULL) {
  if (!is.character(path) || length(path) != 1 || nchar(path) < 1) {
    stop("get_cfg(path): 'path' must be a non-empty string like 'paths.results_dir'.", call. = FALSE)
  }
  keys <- strsplit(path, ".", fixed = TRUE)[[1]]
  cur <- APP_CONFIG
  for (k in keys) {
    if (is.null(cur[[k]])) return(default)
    cur <- cur[[k]]
  }
  cur
}

