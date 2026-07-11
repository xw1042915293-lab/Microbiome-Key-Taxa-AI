# Central reactive state (must not be mutated arbitrarily outside workflow/server wrappers).

workflow_step_levels <- c("waiting", "running", "done", "warning", "skipped", "failed")

workflow_steps_spec <- function() {
  data.frame(
    step_id = c(
      "data_check",
      "build_dataset",
      "alpha",
      "beta",
      "diff",
      "ai",
      "ml",
      "network",
      "key_taxa",
      "report"
    ),
    label = c(
      "Data Check",
      "Build microeco Object",
      "Alpha Diversity",
      "Beta Diversity",
      "Differential Abundance",
      "AI Interpretation",
      "Machine Learning",
      "Network Analysis",
      "Key Taxa Scoring",
      "Report Generation"
    ),
    label_zh = c(
      "数据检查",
      "构建 microeco 对象",
      "Alpha 多样性",
      "Beta 多样性",
      "差异丰度",
      "AI 解释",
      "机器学习",
      "网络分析",
      "关键菌评分",
      "报告生成"
    ),
    placeholder = c(
      "Waiting to validate the uploaded abundance, metadata, and taxonomy tables.",
      "Waiting to construct the microeco dataset object.",
      "Will run after the microeco object is ready.",
      "Will run after Alpha diversity is complete.",
      "Will summarize group-associated taxa after Beta diversity.",
      "Will generate local interpretation and optional LLM outputs after differential abundance.",
      "Will screen microbial features after the AI interpretation step.",
      "Will build the core Spearman co-occurrence network after machine learning.",
      "Will integrate differential abundance, machine learning, and network evidence.",
      "Will render the report after all analysis modules finish or settle."
    ),
    stringsAsFactors = FALSE
  )
}

workflow_step_ids <- function() {
  workflow_steps_spec()$step_id
}

workflow_default_step_status <- function() {
  stats::setNames(as.list(rep("waiting", length(workflow_step_ids()))), workflow_step_ids())
}

workflow_default_step_message <- function() {
  stats::setNames(as.list(rep("", length(workflow_step_ids()))), workflow_step_ids())
}

workflow_report_paths <- function(html_path = NULL, pdf_path = NULL) {
  list(
    html = html_path,
    pdf = pdf_path,
    html_exists = !is.null(html_path) && file.exists(html_path),
    pdf_exists = !is.null(pdf_path) && file.exists(pdf_path)
  )
}

workflow_step_status_snapshot <- function(state = NULL, step_status = NULL) {
  base <- workflow_default_step_status()
  src <- step_status
  if (is.null(src) && !is.null(state)) {
    src <- state$step_status
  }
  if (!is.list(src)) return(base)

  for (step_id in workflow_step_ids()) {
    value <- src[[step_id]] %||% NULL
    if (is.character(value) && length(value) >= 1 && nzchar(value[[1]]) && value[[1]] %in% workflow_step_levels) {
      base[[step_id]] <- value[[1]]
    }
  }

  base
}

workflow_step_message_snapshot <- function(state = NULL, step_message = NULL) {
  base <- workflow_default_step_message()
  src <- step_message
  if (is.null(src) && !is.null(state)) {
    src <- state$step_message
  }
  if (!is.list(src)) return(base)

  for (step_id in workflow_step_ids()) {
    value <- src[[step_id]] %||% NULL
    if (is.character(value) && length(value) >= 1) {
      base[[step_id]] <- as.character(value[[1]] %||% "")
    }
  }

  base
}

workflow_result_fields <- function() {
  c(
    "check_result",
    "dataset",
    "alpha_result",
    "beta_result",
    "diff_result",
    "ml_result",
    "network_result",
    "key_taxa_result",
    "ai_result",
    "report_paths"
  )
}

workflow_assert_state <- function(state, fn = "workflow_assert_state") {
  if (!is.environment(state) && !inherits(state, "reactivevalues")) {
    stop(fn, "(): state must be a reactiveValues object.", call. = FALSE)
  }
  invisible(state)
}

.workflow_normalize_job_dir <- function(job_dir) {
  if (!is.character(job_dir) || length(job_dir) != 1 || !nzchar(job_dir)) return(NULL)
  tryCatch(normalizePath(job_dir, winslash = "/", mustWork = FALSE), error = function(e) job_dir)
}

workflow_same_job_dir <- function(job_dir_a, job_dir_b) {
  a <- .workflow_normalize_job_dir(job_dir_a)
  b <- .workflow_normalize_job_dir(job_dir_b)
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(a, b)
}

workflow_resolve_report_paths <- function(job_dir = NULL, report_paths = NULL) {
  html_path <- if (is.list(report_paths)) report_paths$html %||% NULL else NULL
  pdf_path <- if (is.list(report_paths)) report_paths$pdf %||% NULL else NULL

  if ((is.null(html_path) || !nzchar(as.character(html_path))) && is.character(job_dir) && length(job_dir) == 1 && nzchar(job_dir)) {
    html_path <- file.path(job_dir, "report", "report.html")
  }
  if ((is.null(pdf_path) || !nzchar(as.character(pdf_path))) && is.character(job_dir) && length(job_dir) == 1 && nzchar(job_dir)) {
    pdf_path <- file.path(job_dir, "report", "report.pdf")
  }

  workflow_report_paths(html_path = html_path, pdf_path = pdf_path)
}

workflow_get_active_job <- function(state, prefer_active = TRUE) {
  workflow_assert_state(state, "workflow_get_active_job")

  has_active <- isTRUE(prefer_active) &&
    is.character(state$active_job_dir) &&
    length(state$active_job_dir) == 1 &&
    nzchar(state$active_job_dir)

  job_id <- if (has_active) state$active_job_id %||% state$job_id else state$job_id %||% state$active_job_id
  job_dir <- if (has_active) state$active_job_dir %||% state$job_dir else state$job_dir %||% state$active_job_dir
  same_as_current <- workflow_same_job_dir(job_dir, state$job_dir %||% NULL)

  source <- if (has_active) {
    state$active_source %||% "当前选中任务"
  } else if (!is.null(job_dir) && nzchar(job_dir)) {
    "当前运行任务"
  } else {
    ""
  }

  status <- if (isTRUE(same_as_current)) {
    state$status %||% state$active_status %||% NULL
  } else {
    state$active_status %||% state$status %||% NULL
  }

  report_paths <- workflow_resolve_report_paths(
    job_dir = job_dir,
    report_paths = if (isTRUE(same_as_current)) state$report_paths else NULL
  )

  list(
    job_id = job_id,
    job_dir = job_dir,
    source = source,
    status = status,
    report_paths = report_paths,
    is_active = has_active
  )
}

workflow_set_active_job <- function(state, job_id = NULL, job_dir = NULL, source = NULL, status = NULL, report_paths = NULL) {
  workflow_assert_state(state, "workflow_set_active_job")

  state$active_job_id <- job_id %||% state$job_id %||% NULL
  state$active_job_dir <- job_dir %||% state$job_dir %||% NULL
  state$active_source <- source %||% ""
  state$active_status <- status %||% state$status %||% NULL

  resolved_paths <- workflow_resolve_report_paths(job_dir = state$active_job_dir, report_paths = report_paths)
  state$active_report_path <- resolved_paths$html %||% NULL

  invisible(workflow_get_active_job(state))
}

workflow_sync_active_job <- function(state, source = "当前运行任务") {
  workflow_assert_state(state, "workflow_sync_active_job")
  workflow_set_active_job(
    state = state,
    job_id = state$job_id %||% NULL,
    job_dir = state$job_dir %||% NULL,
    source = source,
    status = state$status %||% NULL,
    report_paths = state$report_paths %||% NULL
  )
}

create_analysis_state <- function() {
  shiny::reactiveValues(
    job_id = NULL,
    job_dir = NULL,
    active_job_id = NULL,
    active_job_dir = NULL,
    active_source = NULL,
    active_status = NULL,
    active_report_path = NULL,
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
    status = "idle",
    current_step = NULL,
    step_status = workflow_default_step_status(),
    step_message = workflow_default_step_message()
  )
}

workflow_set_status <- function(state, status) {
  workflow_assert_state(state, "workflow_set_status")
  assert_non_empty_string(status, "status")
  state$status <- status
}

reset_workflow_steps <- function(state, current_step = NULL) {
  workflow_assert_state(state, "reset_workflow_steps")
  state$current_step <- current_step
  state$step_status <- workflow_default_step_status()
  state$step_message <- workflow_default_step_message()
  invisible(TRUE)
}

reset_workflow_results <- function(state, keep_check_result = FALSE) {
  workflow_assert_state(state, "reset_workflow_results")
  for (field in workflow_result_fields()) {
    if (isTRUE(keep_check_result) && identical(field, "check_result")) next
    state[[field]] <- NULL
  }
  invisible(TRUE)
}

set_step_status <- function(state, step, status, message = NULL) {
  workflow_assert_state(state, "set_step_status")
  assert_non_empty_string(step, "step")
  assert_non_empty_string(status, "status")

  valid_steps <- workflow_step_ids()
  if (!step %in% valid_steps) {
    stop("set_step_status(): unknown step: ", step, call. = FALSE)
  }
  if (!status %in% workflow_step_levels) {
    stop(
      "set_step_status(): invalid status '", status,
      "'. Expected one of: ", paste(workflow_step_levels, collapse = ", "),
      call. = FALSE
    )
  }

  step_status <- workflow_step_status_snapshot(state = state)
  step_message <- workflow_step_message_snapshot(state = state)

  step_status[[step]] <- status
  if (!is.null(message)) {
    step_message[[step]] <- as.character(message)[1] %||% ""
  } else if (is.null(step_message[[step]])) {
    step_message[[step]] <- ""
  }

  state$step_status <- step_status
  state$step_message <- step_message
  state$current_step <- step

  invisible(TRUE)
}

workflow_safe_read_csv <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
}

workflow_safe_read_json <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
}

workflow_status_from_artifact <- function(exists, warning = FALSE) {
  if (!isTRUE(exists)) return("waiting")
  if (isTRUE(warning)) "warning" else "done"
}

workflow_last_available_step <- function(step_status) {
  ordered <- workflow_step_ids()
  snap <- workflow_step_status_snapshot(step_status = step_status)
  ready <- ordered[vapply(snap[ordered], function(x) x %in% c("done", "warning", "skipped", "failed"), logical(1))]
  if (length(ready) < 1) return(NULL)
  utils::tail(ready, 1)[[1]]
}

workflow_read_job_snapshot <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("workflow_read_job_snapshot(): job_dir not found: ", job_dir, call. = FALSE)

  repro <- workflow_safe_read_json(file.path(job_dir, "reproducibility.json")) %||% list()
  params <- repro$parameters %||% list()

  abundance_path <- file.path(job_dir, "input", "abundance.tsv")
  metadata_path <- file.path(job_dir, "input", "metadata.tsv")
  taxonomy_path <- file.path(job_dir, "input", "taxonomy.tsv")
  abundance <- tryCatch(if (file.exists(abundance_path)) read_table_auto(abundance_path) else NULL, error = function(e) NULL)
  metadata <- tryCatch(if (file.exists(metadata_path)) read_table_auto(metadata_path) else NULL, error = function(e) NULL)
  taxonomy <- tryCatch(if (file.exists(taxonomy_path)) read_table_auto(taxonomy_path) else NULL, error = function(e) NULL)

  data_check_csv <- workflow_safe_read_csv(file.path(job_dir, "tables", "data_check_summary.csv"))
  alpha_stats <- workflow_safe_read_csv(alpha_output_path(
    job_dir, "tables", "alpha_stats.csv", legacy_filename = "alpha_stats.csv", existing = TRUE
  ))
  beta_permanova <- workflow_safe_read_csv(beta_output_path(
    job_dir, "tables", "beta_permanova.csv", legacy_filename = "beta_permanova.csv", existing = TRUE
  ))
  diff_table <- workflow_safe_read_csv(file.path(job_dir, "tables", "differential_taxa.csv"))
  diff_summary <- workflow_safe_read_json(file.path(job_dir, "json", "diff_summary.json"))
  ml_summary <- workflow_safe_read_json(file.path(job_dir, "json", "ml_summary.json"))
  network_summary <- workflow_safe_read_json(file.path(job_dir, "json", "network_summary.json"))
  key_taxa_summary <- workflow_safe_read_json(file.path(job_dir, "json", "key_taxa_summary.json"))

  ai_local_exists <- file.exists(file.path(job_dir, "ai", "diff_interpretation.md")) ||
    file.exists(file.path(job_dir, "ai", "methods.md")) ||
    file.exists(file.path(job_dir, "ai", "figure_legends.md"))
  ai_llm_exists <- file.exists(file.path(job_dir, "ai", "llm_diff_interpretation.md")) ||
    file.exists(file.path(job_dir, "ai", "llm_methods.md")) ||
    file.exists(file.path(job_dir, "ai", "llm_figure_legends.md"))

  html_path <- file.path(job_dir, "report", "report.html")
  pdf_path <- file.path(job_dir, "report", "report.pdf")
  report_paths <- workflow_report_paths(
    html_path = if (file.exists(html_path)) html_path else NULL,
    pdf_path = if (file.exists(pdf_path)) pdf_path else NULL
  )

  n_samples <- if (is.data.frame(abundance) && ncol(abundance) >= 2) ncol(abundance) - 1L else NA_integer_
  n_features <- if (is.data.frame(abundance)) nrow(abundance) else NA_integer_
  metadata_samples <- if (is.data.frame(metadata)) nrow(metadata) else NA_integer_
  taxonomy_features <- if (is.data.frame(taxonomy)) nrow(taxonomy) else NA_integer_

  data_check_status <- "waiting"
  if (is.data.frame(data_check_csv) && "status" %in% names(data_check_csv)) {
    check_values <- unique(tolower(as.character(data_check_csv$status)))
    if (any(check_values %in% "error")) {
      data_check_status <- "failed"
    } else if (any(check_values %in% "warning")) {
      data_check_status <- "warning"
    } else {
      data_check_status <- "done"
    }
  } else if (file.exists(file.path(job_dir, "objects", "microeco_dataset.rds")) || !is.null(repro$phase2$finished_at)) {
    data_check_status <- "done"
  }

  step_status <- workflow_default_step_status()
  step_message <- workflow_default_step_message()

  step_status$data_check <- data_check_status
  step_message$data_check <- if (identical(data_check_status, "warning")) {
    "历史任务已完成数据检查，但存在警告。"
  } else if (identical(data_check_status, "done")) {
    "历史任务已完成数据检查。"
  } else if (identical(data_check_status, "failed")) {
    "历史任务的数据检查结果显示存在错误。"
  } else {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "data_check"][[1]]
  }

  build_exists <- file.exists(file.path(job_dir, "objects", "microeco_dataset.rds")) || !is.null(repro$phase2$finished_at)
  step_status$build_dataset <- workflow_status_from_artifact(build_exists)
  step_message$build_dataset <- if (isTRUE(build_exists)) "已从历史任务恢复 microeco 对象状态。" else workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "build_dataset"][[1]]

  alpha_exists <- file.exists(alpha_overview_figure_path(job_dir, "violin_box", "png", existing = TRUE)) ||
    file.exists(alpha_metric_figure_path(job_dir, "Shannon", "boxplot", "png", existing = TRUE)) ||
    is.data.frame(alpha_stats)
  step_status$alpha <- workflow_status_from_artifact(alpha_exists)
  step_message$alpha <- if (isTRUE(alpha_exists)) "历史任务已生成 Alpha 多样性结果。" else workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "alpha"][[1]]

  beta_exists <- file.exists(beta_figure_path(job_dir, "ellipse_centroid", "png", existing = TRUE)) ||
    file.exists(beta_figure_path(job_dir, "points", "png", existing = TRUE)) ||
    is.data.frame(beta_permanova)
  step_status$beta <- workflow_status_from_artifact(beta_exists)
  step_message$beta <- if (isTRUE(beta_exists)) "历史任务已生成 Beta 多样性结果。" else workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "beta"][[1]]

  diff_exists <- file.exists(file.path(job_dir, "figures", "diff_taxa_barplot.png")) || is.data.frame(diff_table) || is.list(diff_summary)
  diff_warning <- is.list(diff_summary) && isTRUE((diff_summary$n_significant_taxa %||% 0) < 1)
  step_status$diff <- workflow_status_from_artifact(diff_exists, warning = diff_warning)
  step_message$diff <- if (!isTRUE(diff_exists)) {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "diff"][[1]]
  } else if (isTRUE(diff_warning)) {
    "历史任务已完成差异丰度分析，但没有显著结果。"
  } else {
    paste0("历史任务已完成差异丰度分析；显著特征数：", diff_summary$n_significant_taxa %||% "n/a")
  }

  ai_exists <- ai_local_exists || ai_llm_exists
  ai_warning <- ai_local_exists && !ai_llm_exists
  step_status$ai <- workflow_status_from_artifact(ai_exists, warning = ai_warning)
  step_message$ai <- if (!isTRUE(ai_exists)) {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "ai"][[1]]
  } else if (isTRUE(ai_warning)) {
    "历史任务已生成本地 AI 说明，未发现 LLM 扩展结果。"
  } else {
    "历史任务已生成本地与 LLM AI 解读结果。"
  }

  ml_exists <- file.exists(file.path(job_dir, "figures", "ml_importance.png")) || is.list(ml_summary)
  ml_warning <- is.list(ml_summary) && identical(ml_summary$reliability %||% "", "caution")
  ml_warning <- isTRUE(ml_warning) || (is.list(ml_summary) && identical(ml_summary$reliability %||% "", "exploratory only"))
  step_status$ml <- workflow_status_from_artifact(ml_exists, warning = ml_warning)
  step_message$ml <- if (!isTRUE(ml_exists)) {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "ml"][[1]]
  } else if (isTRUE(ml_warning)) {
    paste0("历史任务机器学习结果已生成，可靠性：", ml_summary$reliability %||% "warning")
  } else {
    "历史任务已生成机器学习结果。"
  }

  network_exists <- file.exists(file.path(job_dir, "figures", "network_plot.png")) || is.list(network_summary)
  network_warning <- is.list(network_summary) && (isTRUE((network_summary$n_nodes %||% 0) < 3) || isTRUE((network_summary$n_edges %||% 0) < 1))
  step_status$network <- workflow_status_from_artifact(network_exists, warning = network_warning)
  step_message$network <- if (!isTRUE(network_exists)) {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "network"][[1]]
  } else if (isTRUE(network_warning)) {
    "历史任务网络分析结果已生成，但网络较稀疏。"
  } else {
    paste0("历史任务网络分析已生成 ", network_summary$n_nodes %||% "n/a", " 个节点、", network_summary$n_edges %||% "n/a", " 条边。")
  }

  key_taxa_exists <- file.exists(file.path(job_dir, "figures", "key_taxa_score_barplot.png")) || is.list(key_taxa_summary)
  used_sources <- key_taxa_summary$used_sources %||% character(0)
  key_taxa_warning <- isTRUE(length(used_sources) < 2)
  step_status$key_taxa <- workflow_status_from_artifact(key_taxa_exists, warning = key_taxa_warning)
  step_message$key_taxa <- if (!isTRUE(key_taxa_exists)) {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "key_taxa"][[1]]
  } else if (isTRUE(key_taxa_warning)) {
    "历史任务关键菌评分已生成，但证据来源较少。"
  } else {
    paste0("历史任务关键菌评分已整合证据来源：", paste(used_sources, collapse = ", "), "。")
  }

  report_exists <- !is.null(report_paths$html) && file.exists(report_paths$html)
  report_warning <- report_exists && (is.null(report_paths$pdf) || !file.exists(report_paths$pdf))
  step_status$report <- workflow_status_from_artifact(report_exists, warning = report_warning)
  step_message$report <- if (!isTRUE(report_exists)) {
    workflow_steps_spec()$placeholder[workflow_steps_spec()$step_id == "report"][[1]]
  } else if (isTRUE(report_warning)) {
    "历史任务已生成 HTML 报告，但未发现 PDF。"
  } else {
    "历史任务已生成 HTML 与 PDF 报告。"
  }

  terminal_statuses <- unlist(step_status, use.names = FALSE)
  workflow_status <- if (all(terminal_statuses %in% c("done", "warning", "skipped"))) {
    if (any(terminal_statuses == "warning")) "history_loaded_warning" else "history_loaded_done"
  } else if (any(terminal_statuses %in% c("done", "warning", "skipped", "failed"))) {
    "history_loaded_partial"
  } else {
    "history_loaded_waiting"
  }

  pseudo_check_result <- NULL
  if (!identical(data_check_status, "waiting")) {
    pseudo_check_result <- list(
      status = if (identical(data_check_status, "done")) "pass" else data_check_status,
      summary = list(
        n_samples = n_samples,
        n_features = n_features,
        metadata_samples = metadata_samples,
        taxonomy_features = taxonomy_features
      ),
      checks = if (is.data.frame(data_check_csv)) data_check_csv else data.frame()
    )
  }

  list(
    parameters = params,
    report_paths = report_paths,
    step_status = step_status,
    step_message = step_message,
    current_step = workflow_last_available_step(step_status),
    workflow_status = workflow_status,
    check_result = pseudo_check_result,
    overview = list(
      n_samples = n_samples,
      n_features = n_features,
      metadata_samples = metadata_samples,
      taxonomy_features = taxonomy_features,
      data_check_status = data_check_status
    ),
    alpha_stats = alpha_stats,
    beta_permanova = beta_permanova,
    diff_summary = diff_summary,
    diff_table = diff_table,
    ml_summary = ml_summary,
    network_summary = network_summary,
    key_taxa_summary = key_taxa_summary,
    ai = list(
      local_exists = ai_local_exists,
      llm_exists = ai_llm_exists
    )
  )
}

restore_analysis_state_from_job <- function(state, job_dir, job_id = NULL) {
  workflow_assert_state(state, "restore_analysis_state_from_job")
  snapshot <- workflow_read_job_snapshot(job_dir)

  reset_workflow_results(state)
  state$job_dir <- job_dir
  if (!is.null(job_id)) state$job_id <- job_id
  state$parameters <- modifyList(state$parameters %||% list(), snapshot$parameters %||% list())
  state$step_status <- snapshot$step_status
  state$step_message <- snapshot$step_message
  state$current_step <- snapshot$current_step
  state$status <- snapshot$workflow_status
  state$report_paths <- snapshot$report_paths
  state$check_result <- snapshot$check_result

  invisible(snapshot)
}
