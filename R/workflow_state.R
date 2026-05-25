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
    status = "idle"
  )
}

workflow_set_status <- function(state, status) {
  if (!is.environment(state) && !inherits(state, "reactivevalues")) {
    stop("workflow_set_status(): state must be a reactiveValues object.", call. = FALSE)
  }
  assert_non_empty_string(status, "status")
  state$status <- status
}

