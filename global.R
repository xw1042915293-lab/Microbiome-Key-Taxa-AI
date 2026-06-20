# Global wiring: packages, config, and module/function sourcing.
# Keep logic in R/ and UI logic in modules/.

options(shiny.autoreload = FALSE)

source("R/00_packages.R", local = TRUE)
source("R/01_config.R", local = TRUE)

# Apply runtime options from config
max_mb <- get_cfg("app.max_upload_mb", 200)
if (is.numeric(max_mb) && length(max_mb) == 1 && !is.na(max_mb) && max_mb > 0) {
  options(shiny.maxRequestSize = max_mb * 1024^2)
}

# Utilities / workflow state
source("R/02_utils_file.R", local = TRUE)
source("R/04_utils_json.R", local = TRUE)
source("R/05_database.R", local = TRUE)
source("R/workflow_state.R", local = TRUE)
source("R/download_helpers.R", local = TRUE)

# Phase 1: import + validation
source("R/data_import.R", local = TRUE)
source("R/data_check.R", local = TRUE)
source("R/build_microeco.R", local = TRUE)
source("R/analysis_alpha.R", local = TRUE)
source("R/analysis_beta.R", local = TRUE)
source("R/analysis_diff.R", local = TRUE)
source("R/analysis_network.R", local = TRUE)
source("R/ai_prompt.R", local = TRUE)
source("R/report_render.R", local = TRUE)
source("R/workflow_run.R", local = TRUE)

# Shiny modules (UI + server)
source("modules/mod_home.R", local = TRUE)
source("modules/mod_demo.R", local = TRUE)
source("modules/mod_quick_start.R", local = TRUE)
source("modules/mod_results_overview.R", local = TRUE)
source("modules/mod_upload.R", local = TRUE)
source("modules/mod_data_check.R", local = TRUE)
source("modules/mod_parameters.R", local = TRUE)
source("modules/mod_run_analysis.R", local = TRUE)
source("modules/mod_alpha.R", local = TRUE)
source("modules/mod_beta.R", local = TRUE)
source("modules/mod_diff.R", local = TRUE)
source("modules/mod_ai_interpretation.R", local = TRUE)
source("modules/mod_ml.R", local = TRUE)
source("modules/mod_network.R", local = TRUE)
source("modules/mod_key_taxa.R", local = TRUE)
source("modules/mod_report.R", local = TRUE)
source("modules/mod_job_history.R", local = TRUE)

# UI helpers (no analysis logic here).
ui_status_badge <- function(label, kind = c("success", "warning", "error")) {
  kind <- match.arg(kind)
  cls <- switch(
    kind,
    success = "status-badge badge-success",
    warning = "status-badge badge-warning",
    error = "status-badge badge-error"
  )
  shiny::tags$span(class = cls, label %||% "")
}
