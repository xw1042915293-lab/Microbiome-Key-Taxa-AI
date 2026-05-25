# Package loading with clear errors.

required_packages <- c(
  "shiny",
  "bslib",
  "DT",
  "data.table",
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "purrr",
  "ggplot2",
  "jsonlite",
  "yaml",
  "DBI",
  "RSQLite",
  "digest",
  "microeco",
  "vegan",
  "quarto"
)

missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing, collapse = ", "),
    "\nPlease install them (and/or run renv::restore()).",
    call. = FALSE
  )
}

# Attach core packages (keep it minimal; use pkg::fun elsewhere when possible)
library(shiny)
