port <- suppressWarnings(as.integer(Sys.getenv("SHINY_SMOKE_PORT", "38999")))
if (!is.finite(port) || port < 1024L || port > 65535L) {
  stop("SHINY_SMOKE_PORT must be an integer between 1024 and 65535.", call. = FALSE)
}
shiny::runApp(".", port = port, host = "127.0.0.1", launch.browser = FALSE)
