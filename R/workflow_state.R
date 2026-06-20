# Central reactive state (must not be mutated arbitrarily outside workflow/server wrappers).

create_analysis_state <- function() {
  shiny::reactiveValues(
    job_id = NULL,
    job_dir = NULL,
    input_paths = NULL,
    input_data = NULL,
    check_result = NULL,
    parameters = NULL,
    dataset = NULL,
    alpha_result = NULL,
    beta_result = NULL,
    diff_result = NULL,
    ml_result = NULL,
    network_result = NULL,
    key_taxa_result = NULL,
    ai_result = NULL,
    report_paths = NULL,
    active_source = "",
    status = "idle",
    wf_error = NULL
  )
}

workflow_set_status <- function(state, status) {
  if (!is.environment(state) && !inherits(state, "reactivevalues")) {
    stop("workflow_set_status(): state must be a reactiveValues object.", call. = FALSE)
  }
  assert_non_empty_string(status, "status")
  state$status <- status
}

# ---------------------------------------------------------------------------
# workflow_report_paths(): build a standardised report-paths list.
# ---------------------------------------------------------------------------
workflow_report_paths <- function(html_path = NULL, pdf_path = NULL) {
  html_ok <- !is.null(html_path) && is.character(html_path) && nzchar(html_path) && file.exists(html_path)
  pdf_ok  <- !is.null(pdf_path)  && is.character(pdf_path)  && nzchar(pdf_path)  && file.exists(pdf_path)
  list(
    html       = if (html_ok) normalizePath(html_path, winslash = "/", mustWork = FALSE) else NULL,
    pdf        = if (pdf_ok)  normalizePath(pdf_path,  winslash = "/", mustWork = FALSE) else NULL,
    html_exists = isTRUE(html_ok),
    pdf_exists  = isTRUE(pdf_ok)
  )
}

# ---------------------------------------------------------------------------
# workflow_resolve_report_paths(): look up report files under a job directory.
# ---------------------------------------------------------------------------
workflow_resolve_report_paths <- function(job_dir) {
  if (is.null(job_dir) || !is.character(job_dir) || !nzchar(job_dir) || !dir.exists(job_dir)) {
    return(workflow_report_paths(NULL, NULL))
  }
  html_path <- file.path(job_dir, "report", "report.html")
  pdf_path  <- file.path(job_dir, "report", "report.pdf")
  workflow_report_paths(
    html_path = if (file.exists(html_path)) html_path else NULL,
    pdf_path  = if (file.exists(pdf_path))  pdf_path  else NULL
  )
}

# ---------------------------------------------------------------------------
# workflow_get_active_job(): return the current active job as a list.
# ---------------------------------------------------------------------------
workflow_get_active_job <- function(state) {
  job_id     <- state$job_id     %||% NULL
  job_dir    <- state$job_dir    %||% NULL
  source_val <- state$active_source %||% ""
  status_val <- state$status     %||% "idle"

  report_paths <- state$report_paths %||% NULL
  if (is.null(report_paths) && !is.null(job_dir) && nzchar(job_dir) && dir.exists(job_dir)) {
    report_paths <- workflow_resolve_report_paths(job_dir)
  }

  list(
    job_id       = job_id,
    job_dir      = job_dir,
    source       = source_val,
    status       = status_val,
    is_active    = !is.null(job_dir) && nzchar(job_dir),
    report_paths = report_paths
  )
}

# ---------------------------------------------------------------------------
# workflow_set_active_job(): explicitly set the active job from external
# callers (e.g. history module loading a past job).
# ---------------------------------------------------------------------------
workflow_set_active_job <- function(state, job_id, job_dir, source = "",
                                    status = "active") {
  state$job_id        <- job_id
  state$job_dir       <- job_dir
  state$active_source <- source %||% ""
  state$status        <- status %||% "active"
  state$report_paths  <- workflow_resolve_report_paths(job_dir)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# workflow_sync_active_job(): re-sync the active job record from the current
# reactive state fields (job_id, job_dir, report_paths).
# ---------------------------------------------------------------------------
workflow_sync_active_job <- function(state, source = "") {
  state$active_source <- source %||% ""
  # Resolve report paths from the current job_dir if not already set.
  if (is.null(state$report_paths) && !is.null(state$job_dir) &&
      nzchar(state$job_dir) && dir.exists(state$job_dir)) {
    state$report_paths <- workflow_resolve_report_paths(state$job_dir)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# workflow_same_job_dir(): compare two job directories for equality.
# ---------------------------------------------------------------------------
workflow_same_job_dir <- function(dir_a, dir_b) {
  if (is.null(dir_a) || is.null(dir_b)) return(FALSE)
  normalizePath(dir_a, winslash = "/", mustWork = FALSE) ==
    normalizePath(dir_b, winslash = "/", mustWork = FALSE)
}

# ---------------------------------------------------------------------------
# restore_analysis_state_from_job(): reload key state fields from a
# previously saved job directory so that all modules can view its results.
# ---------------------------------------------------------------------------
restore_analysis_state_from_job <- function(state, job_dir, job_id = NULL) {
  if (is.null(job_dir) || !dir.exists(job_dir)) {
    warning("restore_analysis_state_from_job(): job_dir not found: ",
            job_dir, call. = FALSE)
    return(invisible(FALSE))
  }

  state$job_id  <- job_id %||% basename(job_dir)
  state$job_dir <- normalizePath(job_dir, winslash = "/", mustWork = FALSE)
  state$status  <- "restored"

  # Restore dataset
  ds_path <- file.path(job_dir, "objects", "microeco_dataset.rds")
  if (file.exists(ds_path)) {
    tryCatch({
      state$dataset <- readRDS(ds_path)
    }, error = function(e) {
      warning("restore_analysis_state_from_job(): could not load dataset: ",
              conditionMessage(e), call. = FALSE)
    })
  }

  # Restore alpha result path
  alpha_csv <- file.path(job_dir, "tables", "alpha_diversity.csv")
  if (file.exists(alpha_csv)) {
    state$alpha_result <- list(alpha_table_path = normalizePath(alpha_csv, winslash = "/", mustWork = FALSE))
  }

  # Restore report paths
  state$report_paths <- workflow_resolve_report_paths(job_dir)

  # Restore reproducibility / parameters if available
  repro_path <- file.path(job_dir, "reproducibility.json")
  if (file.exists(repro_path)) {
    tryCatch({
      repro <- jsonlite::read_json(repro_path, simplifyVector = TRUE)
      if (!is.null(repro$parameters)) {
        state$parameters <- repro$parameters
      }
    }, error = function(e) NULL)
  }

  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# setup_analysis_state_machine(): bootstrap hook called from app.R server().
# active_source is already initialised in create_analysis_state(), so no
# reactive access is needed here.
# ---------------------------------------------------------------------------
setup_analysis_state_machine <- function(state, session) {
  invisible(TRUE)
}

workflow_steps_spec <- function() {
  data.frame(
    step_id = c("data_prep", "alpha", "beta", "diff", "ai", "ml", "network", "key_taxa", "report"),
    label = c(
      "Data preparation",
      "Alpha diversity",
      "Beta diversity",
      "Differential abundance",
      "AI interpretation",
      "Machine learning",
      "Network analysis",
      "Key Taxa Score",
      "Report generation"
    ),
    stringsAsFactors = FALSE
  )
}

reset_workflow_results <- function(state) {
  state$dataset <- NULL
  state$alpha_result <- NULL
  state$beta_result <- NULL
  state$diff_result <- NULL
  state$ml_result <- NULL
  state$network_result <- NULL
  state$key_taxa_result <- NULL
  state$ai_result <- NULL
  state$report_paths <- NULL
  state$wf_error <- NULL
  invisible(TRUE)
}

reset_workflow_steps <- function(state) {
  state$wf_error <- NULL
  invisible(TRUE)
}

trigger_analysis_state_machine <- function(input_data, job_dir, group_var,
                                           beta_distance = "bray",
                                           tax_level = "Genus",
                                           config_path = "config.yml",
                                           progress_cb = NULL,
                                           log_path = NULL,
                                           state = NULL,
                                           demo_mode = FALSE,
                                           status_running = "running_full_workflow",
                                           status_done = "full_workflow_done",
                                           status_error = "full_workflow_error") {
  if (!is.null(state)) {
    workflow_set_status(state, status_running)
    state$wf_error <- NULL
  }

  result <- tryCatch(
    run_full_analysis_workflow(
      input_data = input_data,
      job_dir = job_dir,
      group_var = group_var,
      beta_distance = beta_distance,
      tax_level = tax_level,
      config_path = config_path,
      progress_cb = progress_cb,
      log_path = log_path
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    if (!is.null(state)) {
      state$wf_error <- conditionMessage(result)
      workflow_set_status(state, status_error)
    }
    stop(result)
  }

  if (!is.null(state)) {
    state$dataset <- result$dataset %||% NULL
    state$alpha_result <- result$alpha %||% NULL
    state$beta_result <- result$beta %||% NULL
    state$diff_result <- result$diff %||% NULL
    state$ml_result <- result$ml %||% NULL
    state$network_result <- result$network %||% NULL
    state$key_taxa_result <- result$key_taxa %||% NULL
    state$ai_result <- result$phase4b %||% NULL
    state$report_paths <- workflow_report_paths(html_path = result$report_path %||% NULL, pdf_path = NULL)
    state$wf_error <- NULL
    workflow_set_status(state, status_done)
  }

  invisible(result)
}
