# Workflow orchestration for Phase 2 (business layer).
#
# Rules:
# - Must NOT accept reactiveValues.
# - Must operate on plain inputs and return a plain list.

source("R/analysis_ml.R", local = TRUE)
source("R/analysis_network.R", local = TRUE)
source("R/ai_rules.R", local = TRUE)
source("R/ai_client.R", local = TRUE)
source("R/ai_interpretation.R", local = TRUE)
source("R/key_taxa_score.R", local = TRUE)

wf_emit_progress <- function(progress_cb, log_path, step_id, status, detail = NULL) {
  # progress_cb signature: function(step_id, status, detail)
  if (is.function(progress_cb)) {
    try(progress_cb(step_id = step_id, status = status, detail = detail), silent = TRUE)
  }

  if (!is.null(log_path) && is.character(log_path) && length(log_path) == 1 && nzchar(log_path)) {
    # Keep logging best-effort; never break the workflow.
    try({
      ensure_dir(dirname(log_path))
      ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      line <- paste0(ts, "\t", step_id, "\t", status, if (!is.null(detail) && nzchar(detail)) paste0("\t", detail) else "")
      cat(line, file = log_path, sep = "\n", append = TRUE)
    }, silent = TRUE)
  }

  invisible(TRUE)
}

run_basic_analysis <- function(input_data, job_dir, group_var, beta_distance = "bray",
                               progress_cb = NULL, log_path = NULL) {
  if (!is.list(input_data) || !all(c("abundance", "metadata", "taxonomy") %in% names(input_data))) {
    stop("run_basic_analysis(): input_data must be a list with abundance/metadata/taxonomy.", call. = FALSE)
  }
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_basic_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(beta_distance, "beta_distance")

  # Record key parameters for reproducibility.
  append_reproducibility(job_dir, list(
    parameters = list(
      group_var = group_var,
      beta_distance = beta_distance
    ),
    phase2 = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  wf_emit_progress(progress_cb, log_path, "data_prep", "running", "Building dataset")
  dataset <- build_microeco_dataset(
    abundance = input_data$abundance,
    metadata = input_data$metadata,
    taxonomy = input_data$taxonomy
  )
  dataset_path <- save_microeco_dataset(dataset, job_dir)
  wf_emit_progress(progress_cb, log_path, "data_prep", "done", NULL)

  wf_emit_progress(progress_cb, log_path, "alpha", "running", NULL)
  alpha <- run_alpha_analysis(dataset = dataset, group_var = group_var, job_dir = job_dir)
  wf_emit_progress(progress_cb, log_path, "alpha", "done", NULL)

  wf_emit_progress(progress_cb, log_path, "beta", "running", NULL)
  beta <- run_beta_analysis(dataset = dataset, group_var = group_var, job_dir = job_dir, distance = beta_distance)
  wf_emit_progress(progress_cb, log_path, "beta", "done", NULL)

  append_reproducibility(job_dir, list(
    phase2 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(
    dataset = dataset,
    dataset_path = dataset_path,
    alpha = alpha,
    beta = beta
  )
}

# Phase 3: differential abundance + Quarto HTML report.
run_phase3_workflow <- function(dataset, job_dir, group_var, tax_level = "Genus",
                                progress_cb = NULL, log_path = NULL) {
  if (is.null(dataset)) stop("run_phase3_workflow(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase3_workflow(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")

  append_reproducibility(job_dir, list(
    phase3 = list(
      started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tax_level = tax_level
    )
  ))

  wf_emit_progress(progress_cb, log_path, "diff", "running", NULL)
  diff <- run_diff_analysis(
    dataset = dataset,
    group_var = group_var,
    tax_level = tax_level,
    job_dir = job_dir
  )
  wf_emit_progress(progress_cb, log_path, "diff", "done", NULL)

  wf_emit_progress(progress_cb, log_path, "report", "running", "Generating report")
  report_path <- render_report_html(job_dir)
  wf_emit_progress(progress_cb, log_path, "report", "done", NULL)

  append_reproducibility(job_dir, list(
    phase3 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(
    diff = diff,
    report_path = report_path
  )
}

run_phase5_workflow <- function(dataset, job_dir, group_var, tax_level = "Genus") {
  if (is.null(dataset)) stop("run_phase5_workflow(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase5_workflow(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")

  append_reproducibility(job_dir, list(
    phase5 = list(
      started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tax_level = tax_level,
      group_var = group_var
    )
  ))

  ml <- run_ml_analysis(
    dataset = dataset,
    group_var = group_var,
    tax_level = tax_level,
    job_dir = job_dir
  )

  append_reproducibility(job_dir, list(
    phase5 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(
    ml = ml
  )
}

run_phase6_workflow <- function(dataset, job_dir, tax_level = "Genus", rho_cutoff = 0.6, p_cutoff = 0.05) {
  if (is.null(dataset)) stop("run_phase6_workflow(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase6_workflow(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(tax_level, "tax_level")

  network <- run_network_analysis(
    dataset = dataset,
    tax_level = tax_level,
    job_dir = job_dir,
    rho_cutoff = rho_cutoff,
    p_cutoff = p_cutoff
  )

  list(network = network)
}

run_phase4a_workflow <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase4a_workflow(): job_dir not found: ", job_dir, call. = FALSE)

  append_reproducibility(job_dir, list(
    phase4a = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  # Local (non-LLM) constrained interpretation artifacts.
  # Must read json/diff_summary.json and write ai/*.md files.
  write_ai_outputs(job_dir = job_dir)

  append_reproducibility(job_dir, list(
    phase4a = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  invisible(TRUE)
}

run_phase4b_workflow <- function(job_dir, config_path = "config.yml") {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase4b_workflow(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(config_path, "config_path")

  append_reproducibility(job_dir, list(
    phase4b = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  # LLM-constrained interpretation artifacts.
  # Must NOT fail the full workflow if API key is missing: write_llm_outputs()
  # is responsible for generating placeholder outputs and request/response JSON.
  out <- write_llm_outputs(job_dir = job_dir, config_path = config_path)

  append_reproducibility(job_dir, list(
    phase4b = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(skipped = isFALSE(out$api_key_present), outputs = out)
}

run_phase7_workflow <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase7_workflow(): job_dir not found: ", job_dir, call. = FALSE)

  diff_path <- file.path(job_dir, "tables", "differential_taxa.csv")
  ml_path <- file.path(job_dir, "tables", "ml_feature_importance.csv")
  nodes_path <- file.path(job_dir, "tables", "network_nodes.csv")
  if (!file.exists(diff_path)) stop("run_phase7_workflow(): missing: ", diff_path, call. = FALSE)
  if (!file.exists(ml_path)) stop("run_phase7_workflow(): missing: ", ml_path, call. = FALSE)
  if (!file.exists(nodes_path)) stop("run_phase7_workflow(): missing: ", nodes_path, call. = FALSE)

  append_reproducibility(job_dir, list(
    phase7 = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  diff_table <- utils::read.csv(diff_path, stringsAsFactors = FALSE, check.names = FALSE)
  ml_table <- utils::read.csv(ml_path, stringsAsFactors = FALSE, check.names = FALSE)
  network_nodes <- utils::read.csv(nodes_path, stringsAsFactors = FALSE, check.names = FALSE)

  # calculate_key_taxa_score() returns a result list (not a data.frame) and
  # also writes all required Phase 7 artifacts (CSV/JSON/figures) internally.
  score_result <- calculate_key_taxa_score(
    diff_table = diff_table,
    ml_table = ml_table,
    network_nodes = network_nodes,
    job_dir = job_dir
  )

  # Do not re-plot here; calculate_key_taxa_score() already generates the plot.

  append_reproducibility(job_dir, list(
    phase7 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(score_result = score_result)
}

run_phase8_workflow <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase8_workflow(): job_dir not found: ", job_dir, call. = FALSE)

  append_reproducibility(job_dir, list(
    phase8 = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  report_path <- render_report_html(job_dir)

  append_reproducibility(job_dir, list(
    phase8 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(report_path = report_path)
}

run_full_analysis_workflow <- function(input_data, job_dir, group_var,
                                       beta_distance = "bray",
                                       tax_level = "Genus",
                                       config_path = "config.yml",
                                       progress_cb = NULL,
                                       log_path = NULL) {
  res2 <- run_basic_analysis(
    input_data = input_data,
    job_dir = job_dir,
    group_var = group_var,
    beta_distance = beta_distance,
    progress_cb = progress_cb,
    log_path = log_path
  )

  res3 <- run_phase3_workflow(
    dataset = res2$dataset,
    job_dir = job_dir,
    group_var = group_var,
    tax_level = tax_level,
    progress_cb = progress_cb,
    log_path = log_path
  )

  wf_emit_progress(progress_cb, log_path, "ai", "running", NULL)
  run_phase4a_workflow(job_dir = job_dir)

  # LLM step is optional; missing API key must not fail the full workflow.
  res4b <- tryCatch(
    run_phase4b_workflow(job_dir = job_dir, config_path = config_path),
    error = function(e) list(skipped = TRUE, reason = conditionMessage(e))
  )
  if (isTRUE(res4b$skipped)) {
    wf_emit_progress(progress_cb, log_path, "ai", "done", "LLM skipped (KKAI_API_KEY missing or LLM unavailable)")
  } else {
    wf_emit_progress(progress_cb, log_path, "ai", "done", NULL)
  }

  wf_emit_progress(progress_cb, log_path, "ml", "running", NULL)
  res5 <- run_phase5_workflow(
    dataset = res2$dataset,
    job_dir = job_dir,
    group_var = group_var,
    tax_level = tax_level
  )
  wf_emit_progress(progress_cb, log_path, "ml", "done", NULL)

  wf_emit_progress(progress_cb, log_path, "network", "running", NULL)
  res6 <- run_phase6_workflow(
    dataset = res2$dataset,
    job_dir = job_dir,
    tax_level = tax_level
  )
  wf_emit_progress(progress_cb, log_path, "network", "done", NULL)

  wf_emit_progress(progress_cb, log_path, "key_taxa", "running", NULL)
  res7 <- run_phase7_workflow(job_dir = job_dir)
  wf_emit_progress(progress_cb, log_path, "key_taxa", "done", NULL)

  wf_emit_progress(progress_cb, log_path, "report", "running", NULL)
  res8 <- run_phase8_workflow(job_dir = job_dir)
  wf_emit_progress(progress_cb, log_path, "report", "done", NULL)

  list(
    dataset = res2$dataset,
    alpha = res2$alpha,
    beta = res2$beta,
    diff = res3$diff,
    phase4b = res4b,
    ml = res5$ml,
    network = res6$network,
    key_taxa = res7,
    report_path = res8$report_path
  )
}
