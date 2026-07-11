# Workflow orchestration for Phases 2-8.

source("R/analysis_ml.R", local = TRUE)
source("R/analysis_network.R", local = TRUE)
source("R/ai_prompt.R", local = TRUE)
source("R/ai_rules.R", local = TRUE)
source("R/ai_client.R", local = TRUE)
source("R/ai_interpretation.R", local = TRUE)
source("R/key_taxa_score.R", local = TRUE)

workflow_resolve_log_path <- function(job_dir, log_path = NULL) {
  if (!is.null(log_path) && is.character(log_path) && length(log_path) == 1 && nzchar(log_path)) {
    return(log_path)
  }
  file.path(job_dir, "logs", "run.log")
}

workflow_log_step <- function(log_path, status, step_id, detail = NULL) {
  if (is.null(log_path) || !is.character(log_path) || length(log_path) != 1 || !nzchar(log_path)) {
    return(invisible(FALSE))
  }

  mapped_status <- switch(
    tolower(status %||% ""),
    running = "START",
    done = "DONE",
    warning = "WARNING",
    skipped = "SKIPPED",
    failed = "FAILED",
    waiting = NA_character_,
    toupper(status %||% "")
  )
  if (is.na(mapped_status) || !nzchar(mapped_status)) return(invisible(FALSE))

  try({
    ensure_dir(dirname(log_path))
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    suffix <- if (!is.null(detail) && nzchar(trimws(detail))) paste0(": ", trimws(detail)) else ""
    cat(
      paste0("[", ts, "] ", mapped_status, " ", step_id, suffix),
      file = log_path,
      sep = "\n",
      append = TRUE
    )
  }, silent = TRUE)

  invisible(TRUE)
}

wf_emit_progress <- function(progress_cb, log_path, step_id, status, detail = NULL) {
  if (is.function(progress_cb)) {
    try(progress_cb(step_id = step_id, status = status, detail = detail), silent = TRUE)
  }
  workflow_log_step(log_path, status = status, step_id = step_id, detail = detail)
  invisible(TRUE)
}

workflow_set_step <- function(state, progress_cb, log_path, step_id, status, detail = NULL) {
  if (!is.null(state)) {
    set_step_status(state, step_id, status, detail)
  }
  wf_emit_progress(progress_cb, log_path, step_id, status, detail)
  invisible(TRUE)
}

workflow_trim_message <- function(x, max_chars = 180) {
  msg <- trimws(as.character(x %||% ""))
  if (!nzchar(msg)) return("")
  if (nchar(msg, type = "chars") <= max_chars) return(msg)
  paste0(substr(msg, 1, max_chars - 3), "...")
}

workflow_count_checks <- function(check_result, level = c("warning", "error")) {
  level <- match.arg(level)
  if (!is.list(check_result) || is.null(check_result$checks) || !is.data.frame(check_result$checks)) return(0L)
  sum(check_result$checks$status %in% level, na.rm = TRUE)
}

workflow_has_result_rows <- function(x, field) {
  if (!is.list(x) || is.null(x[[field]]) || !is.data.frame(x[[field]])) return(FALSE)
  nrow(x[[field]]) > 0
}

workflow_nonfatal_step_failed <- function(step_status) {
  if (!is.list(step_status)) return(FALSE)
  any(vapply(step_status, identical, logical(1), "failed"))
}

workflow_make_ai_result <- function(local_outputs = NULL, llm_outputs = NULL, status = "done", message = NULL) {
  list(
    status = status,
    message = message %||% "",
    local_outputs = local_outputs,
    llm_outputs = llm_outputs
  )
}

workflow_generate_ai_fallback <- function(job_dir, config_path = "config.yml") {
  cfg <- read_llm_config(config_path)
  key_env <- cfg$api_key_env %||% "KKAI_API_KEY"
  old_key <- Sys.getenv(key_env, unset = NA_character_)
  do.call(Sys.setenv, stats::setNames(list(""), key_env))
  on.exit({
    if (is.na(old_key)) {
      do.call(Sys.unsetenv, list(key_env))
    } else {
      do.call(Sys.setenv, stats::setNames(list(old_key), key_env))
    }
  }, add = TRUE)

  write_llm_outputs(job_dir = job_dir, config_path = config_path)
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

  append_reproducibility(job_dir, list(
    parameters = list(
      group_var = group_var,
      beta_distance = beta_distance
    ),
    phase2 = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  wf_emit_progress(progress_cb, log_path, "build_dataset", "running", "Building dataset")
  dataset <- build_microeco_dataset(
    abundance = input_data$abundance,
    metadata = input_data$metadata,
    taxonomy = input_data$taxonomy
  )
  dataset_path <- save_microeco_dataset(dataset, job_dir)
  wf_emit_progress(progress_cb, log_path, "build_dataset", "done", "microeco object saved")

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

  append_reproducibility(job_dir, list(
    phase3 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(diff = diff)
}

run_phase5_workflow <- function(dataset, job_dir, group_var, tax_level = "Genus",
                                folds = 5L, repeats = 20L, permutations = 99L,
                                seed = 1234L) {
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
    job_dir = job_dir,
    folds = folds,
    repeats = repeats,
    permutations = permutations,
    seed = seed
  )

  append_reproducibility(job_dir, list(
    phase5 = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(ml = ml)
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

  out <- write_ai_outputs(job_dir = job_dir)

  append_reproducibility(job_dir, list(
    phase4a = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  out
}

run_phase4b_workflow <- function(job_dir, config_path = "config.yml") {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase4b_workflow(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(config_path, "config_path")

  append_reproducibility(job_dir, list(
    phase4b = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  out <- write_llm_outputs(job_dir = job_dir, config_path = config_path)

  append_reproducibility(job_dir, list(
    phase4b = list(finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  list(skipped = isFALSE(out$api_key_present), outputs = out)
}

run_phase7_workflow <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_phase7_workflow(): job_dir not found: ", job_dir, call. = FALSE)

  append_reproducibility(job_dir, list(
    phase7 = list(started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))

  score_result <- calculate_key_taxa_score(
    diff_table = NULL,
    ml_table = NULL,
    network_nodes = NULL,
    job_dir = job_dir
  )

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

  html_path <- render_report_html(job_dir)
  pdf_path <- NULL
  pdf_error <- NULL

  tryCatch({
    pdf_path <- render_report_pdf(job_dir)
  }, error = function(e) {
    pdf_error <<- conditionMessage(e)
  })

  append_reproducibility(job_dir, list(
    phase8 = list(
      finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      html_report = basename(html_path),
      pdf_report = if (!is.null(pdf_path)) basename(pdf_path) else NULL,
      pdf_error = pdf_error
    )
  ))

  list(
    report_path = html_path,
    html_path = html_path,
    pdf_path = pdf_path,
    pdf_error = pdf_error
  )
}

run_full_analysis_workflow <- function(input_data, job_dir, group_var,
                                       beta_distance = "bray",
                                       tax_level = "Genus",
                                       config_path = "config.yml",
                                       progress_cb = NULL,
                                       log_path = NULL,
                                       state = NULL) {
  if (!is.list(input_data) || !all(c("abundance", "metadata", "taxonomy") %in% names(input_data))) {
    stop("run_full_analysis_workflow(): input_data must contain abundance/metadata/taxonomy.", call. = FALSE)
  }
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_full_analysis_workflow(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")

  log_path <- workflow_resolve_log_path(job_dir, log_path)

  results <- list(
    dataset = NULL,
    alpha = NULL,
    beta = NULL,
    diff = NULL,
    phase4a = NULL,
    phase4b = NULL,
    ml = NULL,
    network = NULL,
    key_taxa = NULL,
    report_path = NULL,
    report_pdf = NULL
  )

  if (!is.null(state)) {
    workflow_assert_state(state, "run_full_analysis_workflow")
  }

  workflow_set_step(state, progress_cb, log_path, "data_check", "running", "姝ｅ湪妫€鏌ヨ緭鍏ユ暟鎹�")
  check_result <- tryCatch(
    run_all_data_checks(input_data, group_var = group_var),
    error = function(e) e
  )
  if (inherits(check_result, "error")) {
    msg <- workflow_trim_message(conditionMessage(check_result))
    workflow_set_step(state, progress_cb, log_path, "data_check", "failed", msg)
    stop("Data check failed: ", conditionMessage(check_result), call. = FALSE)
  }
  save_data_check_summary(check_result, job_dir)
  if (!is.null(state)) state$check_result <- check_result
  if (identical(check_result$status, "error")) {
    msg <- paste0("鏁版嵁妫€鏌ュけ璐ワ細鍙戠幇 ", workflow_count_checks(check_result, "error"), " 涓�敊璇�")
    workflow_set_step(state, progress_cb, log_path, "data_check", "failed", msg)
    stop("Data check reported validation errors. Please fix the uploaded files and run again.", call. = FALSE)
  }
  if (identical(check_result$status, "warning")) {
    msg <- paste0("鏁版嵁妫€鏌ュ畬鎴愶紝瀛樺湪 ", workflow_count_checks(check_result, "warning"), " 涓��鍛�")
    workflow_set_step(state, progress_cb, log_path, "data_check", "warning", msg)
  } else {
    workflow_set_step(state, progress_cb, log_path, "data_check", "done", "杈撳叆鏁版嵁妫€鏌ュ畬鎴�")
  }

  workflow_set_step(state, progress_cb, log_path, "build_dataset", "running", "姝ｅ湪鏋勫缓 microeco 瀵硅薄")
  dataset <- tryCatch(
    build_microeco_dataset(
      abundance = input_data$abundance,
      metadata = input_data$metadata,
      taxonomy = input_data$taxonomy
    ),
    error = function(e) e
  )
  if (inherits(dataset, "error")) {
    msg <- workflow_trim_message(conditionMessage(dataset))
    workflow_set_step(state, progress_cb, log_path, "build_dataset", "failed", msg)
    stop("Failed to build the microeco dataset: ", conditionMessage(dataset), call. = FALSE)
  }
  dataset_path <- save_microeco_dataset(dataset, job_dir)
  results$dataset <- dataset
  if (!is.null(state)) state$dataset <- dataset
  workflow_set_step(state, progress_cb, log_path, "build_dataset", "done", "microeco 瀵硅薄宸叉瀯寤�")

  workflow_set_step(state, progress_cb, log_path, "alpha", "running", "姝ｅ湪璁＄畻 Alpha 澶氭牱鎬�")
  alpha_res <- tryCatch(
    run_alpha_analysis(dataset = dataset, group_var = group_var, job_dir = job_dir),
    error = function(e) e
  )
  if (inherits(alpha_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "alpha", "failed", workflow_trim_message(conditionMessage(alpha_res)))
  } else {
    results$alpha <- alpha_res
    if (!is.null(state)) state$alpha_result <- alpha_res
    workflow_set_step(state, progress_cb, log_path, "alpha", "done", "Alpha 澶氭牱鎬у畬鎴�")
  }

  workflow_set_step(state, progress_cb, log_path, "beta", "running", "姝ｅ湪璁＄畻 Beta 澶氭牱鎬�")
  beta_res <- tryCatch(
    run_beta_analysis(dataset = dataset, group_var = group_var, job_dir = job_dir, distance = beta_distance),
    error = function(e) e
  )
  if (inherits(beta_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "beta", "failed", workflow_trim_message(conditionMessage(beta_res)))
  } else {
    results$beta <- beta_res
    if (!is.null(state)) state$beta_result <- beta_res
    workflow_set_step(state, progress_cb, log_path, "beta", "done", "Beta 澶氭牱鎬у畬鎴�")
  }

  workflow_set_step(state, progress_cb, log_path, "diff", "running", "姝ｅ湪杩涜�宸�紓涓板害鍒嗘瀽")
  diff_res <- tryCatch(
    run_diff_analysis(
      dataset = dataset,
      group_var = group_var,
      tax_level = tax_level,
      job_dir = job_dir
    ),
    error = function(e) e
  )
  if (inherits(diff_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "diff", "failed", workflow_trim_message(conditionMessage(diff_res)))
  } else {
    results$diff <- diff_res
    if (!is.null(state)) state$diff_result <- diff_res
    diff_tbl <- diff_res$diff_table %||% data.frame()
    n_sig <- if (is.data.frame(diff_tbl) && "significant" %in% names(diff_tbl)) sum(diff_tbl$significant, na.rm = TRUE) else 0L
    if (!is.data.frame(diff_tbl) || nrow(diff_tbl) < 1) {
      workflow_set_step(state, progress_cb, log_path, "diff", "warning", "宸�紓涓板害缁撴灉涓虹┖")
    } else if (n_sig > 0) {
      workflow_set_step(state, progress_cb, log_path, "diff", "done", paste0("宸�紓涓板害瀹屾垚锛�", n_sig, " 涓�樉钁楃壒寰�"))
    } else {
      workflow_set_step(state, progress_cb, log_path, "diff", "warning", "宸�紓涓板害瀹屾垚锛屼絾娌℃湁鏄捐憲缁撴灉")
    }
  }

  workflow_set_step(state, progress_cb, log_path, "ai", "running", "姝ｅ湪鐢熸垚 AI 瑙ｈ�")
  ai_local <- tryCatch(run_phase4a_workflow(job_dir = job_dir), error = function(e) e)
  if (inherits(ai_local, "error")) {
    ai_local <- NULL
  }
  ai_cfg <- read_llm_config(config_path)
  api_key_present <- !is.null(read_api_key(ai_cfg$api_key_env))
  if (!isTRUE(api_key_present)) {
    ai_llm <- tryCatch(run_phase4b_workflow(job_dir = job_dir, config_path = config_path), error = function(e) e)
    if (inherits(ai_llm, "error")) {
      ai_result <- workflow_make_ai_result(
        local_outputs = ai_local,
        llm_outputs = NULL,
        status = "skipped",
        message = "鏈�厤缃� API key锛岃烦杩� LLM 瑙ｈ�"
      )
    } else {
      ai_result <- workflow_make_ai_result(
        local_outputs = ai_local,
        llm_outputs = ai_llm$outputs,
        status = "skipped",
        message = "鏈�厤缃� API key锛屽凡鐢熸垚鏈�湴璇存槑"
      )
      results$phase4b <- ai_llm
    }
    if (!is.null(state)) state$ai_result <- ai_result
    workflow_set_step(state, progress_cb, log_path, "ai", "skipped", ai_result$message)
  } else {
    ai_llm <- tryCatch(run_phase4b_workflow(job_dir = job_dir, config_path = config_path), error = function(e) e)
    if (inherits(ai_llm, "error")) {
      fallback <- tryCatch(workflow_generate_ai_fallback(job_dir = job_dir, config_path = config_path), error = function(e) NULL)
      warning_msg <- "LLM call failed; fallback markdown generated"
      ai_result <- workflow_make_ai_result(
        local_outputs = ai_local,
        llm_outputs = fallback,
        status = "warning",
        message = "AI 瑙ｈ�澶辫触锛屽凡淇濈暀鍥為€€璇存槑"
      )
      if (!is.null(state)) state$ai_result <- ai_result
      workflow_set_step(state, progress_cb, log_path, "ai", "warning", "AI 瑙ｈ�澶辫触锛屽凡淇濈暀鍥為€€璇存槑")
    } else {
      results$phase4b <- ai_llm
      ai_result <- workflow_make_ai_result(
        local_outputs = ai_local,
        llm_outputs = ai_llm$outputs,
        status = "done",
        message = "AI 瑙ｈ�瀹屾垚"
      )
      if (!is.null(state)) state$ai_result <- ai_result
      workflow_set_step(state, progress_cb, log_path, "ai", "done", "AI 瑙ｈ�瀹屾垚")
    }
  }

  workflow_set_step(state, progress_cb, log_path, "ml", "running", "姝ｅ湪杩愯�鏈哄櫒瀛︿範鍒嗘瀽")
  ml_res <- tryCatch(
    run_phase5_workflow(
      dataset = dataset,
      job_dir = job_dir,
      group_var = group_var,
      tax_level = tax_level
    ),
    error = function(e) e
  )
  if (inherits(ml_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "ml", "failed", workflow_trim_message(conditionMessage(ml_res)))
  } else {
    results$ml <- ml_res$ml
    if (!is.null(state)) state$ml_result <- ml_res$ml
    reliability <- ml_res$ml$reliability %||% ""
    performance_status <- ml_res$ml$performance_status %||% ""
    if (identical(performance_status, "not_stably_better_than_random")) {
      workflow_set_step(state, progress_cb, log_path, "ml", "warning", "机器学习完成，但未显示稳定且显著优于随机分类的性能")
    } else if (identical(reliability, "exploratory only")) {
      workflow_set_step(state, progress_cb, log_path, "ml", "warning", "鏈哄櫒瀛︿範瀹屾垚锛屼絾鏍锋湰閲忓亸灏�")
    } else if (identical(reliability, "caution")) {
      workflow_set_step(state, progress_cb, log_path, "ml", "warning", "鏈哄櫒瀛︿範瀹屾垚锛岃�璋ㄦ厧瑙ｉ噴")
    } else {
      workflow_set_step(state, progress_cb, log_path, "ml", "done", "鏈哄櫒瀛︿範鍒嗘瀽瀹屾垚")
    }
  }

  workflow_set_step(state, progress_cb, log_path, "network", "running", "姝ｅ湪杩愯�缃戠粶鍒嗘瀽")
  network_res <- tryCatch(
    run_phase6_workflow(
      dataset = dataset,
      job_dir = job_dir,
      tax_level = tax_level
    ),
    error = function(e) e
  )
  if (inherits(network_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "network", "failed", workflow_trim_message(conditionMessage(network_res)))
  } else {
    results$network <- network_res$network
    if (!is.null(state)) state$network_result <- network_res$network
    n_nodes <- network_res$network$summary$n_nodes %||% nrow(network_res$network$node_table %||% data.frame())
    n_edges <- network_res$network$summary$n_edges %||% nrow(network_res$network$edge_table %||% data.frame())
    if (isTRUE(n_nodes < 3) || isTRUE(n_edges < 1)) {
      workflow_set_step(state, progress_cb, log_path, "network", "warning", "缃戠粶鍒嗘瀽瀹屾垚锛屼絾缃戠粶杈冪█鐤�")
    } else {
      workflow_set_step(state, progress_cb, log_path, "network", "done", "缃戠粶鍒嗘瀽瀹屾垚")
    }
  }

  workflow_set_step(state, progress_cb, log_path, "key_taxa", "running", "姝ｅ湪璁＄畻鍏抽敭鑿岃瘎鍒�")
  key_taxa_res <- tryCatch(run_phase7_workflow(job_dir = job_dir), error = function(e) e)
  if (inherits(key_taxa_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "key_taxa", "failed", workflow_trim_message(conditionMessage(key_taxa_res)))
  } else {
    results$key_taxa <- key_taxa_res
    if (!is.null(state)) state$key_taxa_result <- key_taxa_res
    used_sources <- key_taxa_res$score_result$used_sources %||% character(0)
    if (length(used_sources) >= 2) {
      workflow_set_step(state, progress_cb, log_path, "key_taxa", "done", "鍏抽敭鑿岃瘎鍒嗗畬鎴�")
    } else if (length(used_sources) == 1) {
      workflow_set_step(state, progress_cb, log_path, "key_taxa", "warning", "鍏抽敭鑿岃瘎鍒嗗畬鎴愶紝浣嗚瘉鎹�潵婧愯緝灏�")
    } else {
      workflow_set_step(state, progress_cb, log_path, "key_taxa", "warning", "鍏抽敭鑿岃瘎鍒嗙粨鏋滀负绌�")
    }
  }

  workflow_set_step(state, progress_cb, log_path, "report", "running", "姝ｅ湪鐢熸垚鎶ュ憡")
  report_res <- tryCatch(run_phase8_workflow(job_dir = job_dir), error = function(e) e)
  if (inherits(report_res, "error")) {
    workflow_set_step(state, progress_cb, log_path, "report", "failed", workflow_trim_message(conditionMessage(report_res)))
  } else {
    results$report_path <- report_res$html_path
    results$report_pdf <- report_res$pdf_path
    report_paths <- workflow_report_paths(html_path = report_res$html_path, pdf_path = report_res$pdf_path)
    if (!is.null(state)) state$report_paths <- report_paths
    if (!is.null(report_res$pdf_error) && nzchar(report_res$pdf_error)) {
      workflow_set_step(state, progress_cb, log_path, "report", "warning", "鎶ュ憡宸茬敓鎴� HTML锛屼絾 PDF 瀵煎嚭澶辫触")
    } else {
      workflow_set_step(state, progress_cb, log_path, "report", "done", "鎶ュ憡鐢熸垚瀹屾垚")
    }
  }

  results
}

setup_analysis_state_machine <- function(state, session) {
  workflow_assert_state(state, "setup_analysis_state_machine")
  shiny::observeEvent(state$wf_idx, {
    idx <- state$wf_idx
    if (is.null(idx)) return()
    
    if (isTRUE(state$cancel_run)) {
      workflow_set_global_status(state, state$wf_args$progress_cb, state$wf_args$log_path, "warning", "用户已终止任务")
      state$cancel_run <- FALSE
      state$wf_idx <- NULL
      state$status <- "full_workflow_error"
      state$wf_error <- "用户手动终止了任务"
      return()
    }
    
    steps <- state$wf_steps
    args <- state$wf_args
    if (idx > length(steps)) {
      workflow_set_status(state, args$status_done %||% "full_workflow_done")
      state$wf_idx <- NULL
      return()
    }
    
    step_id <- steps[idx]

    
    tryCatch({
      if (step_id == "data_check") {
        workflow_set_step(state, args$progress_cb, args$log_path, "data_check", "running", "正在检查输入数据")
        check_result <- run_all_data_checks(args$input_data, group_var = args$group_var)
        save_data_check_summary(check_result, args$job_dir)
        state$check_result <- check_result
        if (identical(check_result$status, "error")) {
          msg <- paste0("数据检查失败：发现 ", workflow_count_checks(check_result, "error"), " 个错误")
          workflow_set_step(state, args$progress_cb, args$log_path, "data_check", "failed", msg)
          stop("Data check reported validation errors.", call. = FALSE)
        } else if (identical(check_result$status, "warning")) {
          msg <- paste0("数据检查完成，存在 ", workflow_count_checks(check_result, "warning"), " 个警告")
          workflow_set_step(state, args$progress_cb, args$log_path, "data_check", "warning", msg)
        } else {
          workflow_set_step(state, args$progress_cb, args$log_path, "data_check", "done", "输入数据检查完成")
        }
      } else if (step_id == "build_dataset") {
        workflow_set_step(state, args$progress_cb, args$log_path, "build_dataset", "running", "正在构建 microeco 对象")
        dataset <- build_microeco_dataset(
          abundance = args$input_data$abundance,
          metadata = args$input_data$metadata,
          taxonomy = args$input_data$taxonomy
        )
        save_microeco_dataset(dataset, args$job_dir)
        state$dataset <- dataset
        workflow_set_step(state, args$progress_cb, args$log_path, "build_dataset", "done", "microeco 对象已构建")
      } else if (step_id == "alpha") {
        workflow_set_step(state, args$progress_cb, args$log_path, "alpha", "running", "正在计算 Alpha 多样性")
        alpha_res <- run_alpha_analysis(dataset = state$dataset, group_var = args$group_var, job_dir = args$job_dir)
        state$alpha_result <- alpha_res
        workflow_set_step(state, args$progress_cb, args$log_path, "alpha", "done", "Alpha 多样性完成")
      } else if (step_id == "beta") {
        workflow_set_step(state, args$progress_cb, args$log_path, "beta", "running", "正在计算 Beta 多样性")
        beta_res <- run_beta_analysis(dataset = state$dataset, group_var = args$group_var, job_dir = args$job_dir, distance = args$beta_distance)
        state$beta_result <- beta_res
        workflow_set_step(state, args$progress_cb, args$log_path, "beta", "done", "Beta 多样性完成")
      } else if (step_id == "diff") {
        workflow_set_step(state, args$progress_cb, args$log_path, "diff", "running", "正在进行差异丰度分析")
        diff_res <- run_diff_analysis(
          dataset = state$dataset,
          group_var = args$group_var,
          tax_level = args$tax_level,
          job_dir = args$job_dir
        )
        state$diff_result <- diff_res
        diff_tbl <- diff_res$diff_table %||% data.frame()
        n_sig <- if (is.data.frame(diff_tbl) && "significant" %in% names(diff_tbl)) sum(diff_tbl$significant, na.rm = TRUE) else 0L
        if (!is.data.frame(diff_tbl) || nrow(diff_tbl) < 1) {
          workflow_set_step(state, args$progress_cb, args$log_path, "diff", "warning", "差异丰度结果为空")
        } else if (n_sig > 0) {
          workflow_set_step(state, args$progress_cb, args$log_path, "diff", "done", paste0("差异丰度完成：", n_sig, " 个显著特征"))
        } else {
          workflow_set_step(state, args$progress_cb, args$log_path, "diff", "warning", "差异丰度完成，但没有显著结果")
        }
      } else if (step_id == "ai") {
        workflow_set_step(state, args$progress_cb, args$log_path, "ai", "running", "正在生成 AI 解读")
        ai_local <- tryCatch(run_phase4a_workflow(job_dir = args$job_dir), error = function(e) NULL)
        
        ai_cfg <- read_llm_config(args$config_path)
        key_env <- ai_cfg$api_key_env %||% "KKAI_API_KEY"
        
        # In demo mode, suppress API calls
        if (isTRUE(args$demo_mode)) {
          api_key_present <- FALSE
        } else {
          api_key_present <- !is.null(read_api_key(key_env))
        }
        
        if (!isTRUE(api_key_present)) {
          ai_llm <- tryCatch(run_phase4b_workflow(job_dir = args$job_dir, config_path = args$config_path), error = function(e) e)
          if (inherits(ai_llm, "error")) {
            ai_result <- workflow_make_ai_result(local_outputs = ai_local, llm_outputs = NULL, status = "skipped", message = "未配置 API key，跳过 LLM 解读")
          } else {
            ai_result <- workflow_make_ai_result(local_outputs = ai_local, llm_outputs = ai_llm$outputs, status = "skipped", message = "未配置 API key，已生成本地说明")
          }
          state$ai_result <- ai_result
          workflow_set_step(state, args$progress_cb, args$log_path, "ai", "skipped", ai_result$message)
        } else {
          ai_llm <- tryCatch(run_phase4b_workflow(job_dir = args$job_dir, config_path = args$config_path), error = function(e) e)
          if (inherits(ai_llm, "error")) {
            fallback <- tryCatch(workflow_generate_ai_fallback(job_dir = args$job_dir, config_path = args$config_path), error = function(e) NULL)
            ai_result <- workflow_make_ai_result(local_outputs = ai_local, llm_outputs = fallback, status = "warning", message = "AI 解读失败，已保留回退说明")
            state$ai_result <- ai_result
            workflow_set_step(state, args$progress_cb, args$log_path, "ai", "warning", "AI 解读失败，已保留回退说明")
          } else {
            ai_result <- workflow_make_ai_result(local_outputs = ai_local, llm_outputs = ai_llm$outputs, status = "done", message = "AI 解读完成")
            state$ai_result <- ai_result
            workflow_set_step(state, args$progress_cb, args$log_path, "ai", "done", "AI 解读完成")
          }
        }
      } else if (step_id == "ml") {
        workflow_set_step(state, args$progress_cb, args$log_path, "ml", "running", "正在运行机器学习分析")
        ml_res <- run_phase5_workflow(dataset = state$dataset, job_dir = args$job_dir, group_var = args$group_var, tax_level = args$tax_level)
        state$ml_result <- ml_res$ml
        reliability <- ml_res$ml$reliability %||% ""
        performance_status <- ml_res$ml$performance_status %||% ""
        if (identical(performance_status, "not_stably_better_than_random")) {
          workflow_set_step(state, args$progress_cb, args$log_path, "ml", "warning", "机器学习完成，但未显示稳定且显著优于随机分类的性能")
        } else if (identical(reliability, "exploratory only")) {
          workflow_set_step(state, args$progress_cb, args$log_path, "ml", "warning", "机器学习完成，但样本量偏小")
        } else if (identical(reliability, "caution")) {
          workflow_set_step(state, args$progress_cb, args$log_path, "ml", "warning", "机器学习完成，请谨慎解释")
        } else {
          workflow_set_step(state, args$progress_cb, args$log_path, "ml", "done", "机器学习分析完成")
        }
      } else if (step_id == "network") {
        workflow_set_step(state, args$progress_cb, args$log_path, "network", "running", "正在运行网络分析")
        network_res <- run_phase6_workflow(dataset = state$dataset, job_dir = args$job_dir, tax_level = args$tax_level)
        state$network_result <- network_res$network
        n_nodes <- network_res$network$summary$n_nodes %||% nrow(network_res$network$node_table %||% data.frame())
        n_edges <- network_res$network$summary$n_edges %||% nrow(network_res$network$edge_table %||% data.frame())
        if (isTRUE(n_nodes < 3) || isTRUE(n_edges < 1)) {
          workflow_set_step(state, args$progress_cb, args$log_path, "network", "warning", "网络分析完成，但网络较稀疏")
        } else {
          workflow_set_step(state, args$progress_cb, args$log_path, "network", "done", "网络分析完成")
        }
      } else if (step_id == "key_taxa") {
        workflow_set_step(state, args$progress_cb, args$log_path, "key_taxa", "running", "正在计算关键菌评分")
        key_taxa_res <- run_phase7_workflow(job_dir = args$job_dir)
        state$key_taxa_result <- key_taxa_res
        ai_refresh <- tryCatch(run_phase4a_workflow(job_dir = args$job_dir), error = function(e) NULL)
        if (!is.null(ai_refresh)) {
          if (is.null(state$ai_result)) state$ai_result <- list()
          state$ai_result$local_outputs <- ai_refresh
        }
        used_sources <- key_taxa_res$score_result$used_sources %||% character(0)
        if (length(used_sources) >= 2) {
          workflow_set_step(state, args$progress_cb, args$log_path, "key_taxa", "done", "关键菌评分完成")
        } else if (length(used_sources) == 1) {
          workflow_set_step(state, args$progress_cb, args$log_path, "key_taxa", "warning", "关键菌评分完成，但证据来源较少")
        } else {
          workflow_set_step(state, args$progress_cb, args$log_path, "key_taxa", "warning", "关键菌评分结果为空")
        }
      } else if (step_id == "report") {
        workflow_set_step(state, args$progress_cb, args$log_path, "report", "running", "正在生成报告")
        report_res <- run_phase8_workflow(job_dir = args$job_dir)
        report_paths <- workflow_report_paths(html_path = report_res$html_path, pdf_path = report_res$pdf_path)
        state$report_paths <- report_paths
        if (!is.null(report_res$pdf_error) && nzchar(report_res$pdf_error)) {
          workflow_set_step(state, args$progress_cb, args$log_path, "report", "warning", "报告已生成 HTML，但 PDF 导出失败")
        } else {
          workflow_set_step(state, args$progress_cb, args$log_path, "report", "done", "报告生成完成")
        }
      }
      
      # Next step
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          state$wf_idx <- idx + 1
        }, delay = 0.05)
      } else {
        state$wf_idx <- idx + 1
      }
      
    }, error = function(e) {
      msg <- workflow_trim_message(conditionMessage(e))
      workflow_set_step(state, args$progress_cb, args$log_path, step_id, "failed", msg)
      
      state$wf_error <- conditionMessage(e)
      workflow_set_status(state, args$status_error %||% "full_workflow_error")
      
      err_path <- file.path(args$job_dir, "logs", "error.log")
      ensure_dir(dirname(err_path))
      writeLines(
        c(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", conditionMessage(e)),
          "",
          "Call stack:",
          capture.output(sys.calls())
        ),
        err_path
      )
      
      state$wf_idx <- NULL
      return()
    })
    
  }, ignoreInit = TRUE, once = FALSE, domain = session)
}

trigger_analysis_state_machine <- function(input_data, job_dir, group_var, beta_distance = "bray", tax_level = "Genus", config_path = "config.yml", progress_cb = NULL, log_path = NULL, state = NULL, demo_mode = FALSE, status_running = "running_full_workflow", status_done = "full_workflow_done", status_error = "full_workflow_error") {
  if (!is.list(input_data) || !all(c("abundance", "metadata", "taxonomy") %in% names(input_data))) {
    stop("trigger_analysis_state_machine(): input_data must contain abundance/metadata/taxonomy.", call. = FALSE)
  }
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("trigger_analysis_state_machine(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")

  log_path <- workflow_resolve_log_path(job_dir, log_path)

  state$wf_args <- list(
    input_data = input_data,
    job_dir = job_dir,
    group_var = group_var,
    beta_distance = beta_distance,
    tax_level = tax_level,
    config_path = config_path,
    progress_cb = progress_cb,
    log_path = log_path,
    demo_mode = demo_mode,
    status_done = status_done,
    status_error = status_error
  )
  
  state$wf_steps <- c("data_check", "build_dataset", "alpha", "beta", "diff", "ai", "ml", "network", "key_taxa", "report")
  workflow_set_status(state, status_running)
  state$wf_idx <- 1
  
  invisible(TRUE)
}

