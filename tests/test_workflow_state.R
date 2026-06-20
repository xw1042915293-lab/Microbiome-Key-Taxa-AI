# Minimal smoke tests for shared workflow state helpers.

source("global.R", local = TRUE)

state <- create_analysis_state()
empty_job <- shiny::isolate(workflow_get_active_job(state))
stopifnot(is.null(empty_job$job_dir))
stopifnot(identical(empty_job$source, ""))

job_dir <- tempfile("workflow_state_job_")
dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(job_dir, "report"), recursive = TRUE, showWarnings = FALSE)
html_path <- file.path(job_dir, "report", "report.html")
writeLines("<html><body>ok</body></html>", html_path)

shiny::isolate({
  state$job_id <- "job_current"
  state$job_dir <- job_dir
  state$status <- "job_created"
  state$report_paths <- workflow_report_paths(html_path = html_path, pdf_path = NULL)
  workflow_sync_active_job(state, source = "current_run")
})

current_job <- shiny::isolate(workflow_get_active_job(state))
stopifnot(identical(current_job$job_id, "job_current"))
stopifnot(identical(current_job$job_dir, job_dir))
stopifnot(identical(current_job$source, "current_run"))
stopifnot(isTRUE(current_job$report_paths$html_exists))

history_dir <- tempfile("workflow_state_history_")
dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(history_dir, "report"), recursive = TRUE, showWarnings = FALSE)
history_html <- file.path(history_dir, "report", "report.html")
writeLines("<html><body>history</body></html>", history_html)

shiny::isolate({
  workflow_set_active_job(
    state = state,
    job_id = "job_history",
    job_dir = history_dir,
    source = "history_loaded",
    status = "completed"
  )
})

history_job <- shiny::isolate(workflow_get_active_job(state))
stopifnot(isTRUE(history_job$is_active))
stopifnot(identical(history_job$job_id, "job_history"))
stopifnot(identical(history_job$job_dir, history_dir))
stopifnot(identical(history_job$source, "history_loaded"))
stopifnot(isTRUE(history_job$report_paths$html_exists))
