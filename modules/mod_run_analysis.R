# Run analysis module: orchestrates the full workflow with shared step state.

mod_run_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-page-header",
        shiny::tags$h2("运行完整分析"),
        shiny::tags$p("按步骤查看完整分析流程，结果会在对应模块完成后立即出现在结果总览中。")
      ),
      shiny::tags$div(
        class = "kkai-workflow-layout",
        bslib::card(
          class = "dashboard-card kkai-workflow-summary-card",
          bslib::card_header("任务概览"),
          shiny::uiOutput(ns("run_button")),
          shiny::uiOutput(ns("summary_panel")),
          shiny::uiOutput(ns("run_result_panel"))
        ),
        bslib::card(
          class = "dashboard-card kkai-workflow-steps-card",
          bslib::card_header("步骤状态"),
          shiny::uiOutput(ns("steps_list"))
        )
      ),
      bslib::card(
        class = "kkai-card",
        bslib::card_header("核心产物检查"),
        shiny::tableOutput(ns("artifact_table"))
      ),
      shiny::uiOutput(ns("error_panel"))
    )
  )
}

mod_run_analysis_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    artifact_checks <- shiny::reactiveVal(NULL)
    user_error <- shiny::reactiveVal(NULL)
    dev_error <- shiny::reactiveVal(NULL)
    zip_status <- shiny::reactiveVal("")
    prog_val <- shiny::reactiveVal(NULL)
    toast_id <- paste0(session$ns("workflow_toast"))

    steps_spec <- workflow_steps_spec()

    step_label_zh <- function(step_id) {
      row <- steps_spec[steps_spec$step_id == step_id, , drop = FALSE]
      if (!nrow(row)) return(step_id)
      row$label_zh[[1]]
    }

    step_placeholder <- function(step_id) {
      row <- steps_spec[steps_spec$step_id == step_id, , drop = FALSE]
      if (!nrow(row)) return("")
      row$placeholder[[1]]
    }

    overall_status <- shiny::reactive({
      sts <- unlist(workflow_step_status_snapshot(state = state), use.names = TRUE)
      if (identical(state$status %||% "idle", "running_full_workflow") || any(sts == "running")) return("running")
      if (any(sts == "failed") && !any(sts %in% c("waiting", "running"))) return("completed_with_issues")
      if (any(sts == "warning") || any(sts == "failed")) return("completed_with_issues")
      if (all(sts %in% c("done", "skipped"))) return("done")
      if (all(sts == "waiting")) return("idle")
      "ready"
    })

    build_artifact_table <- function(job_dir) {
      if (is.null(job_dir) || !dir.exists(job_dir)) return(NULL)
      core_files <- c(
        "tables/data_check_summary.csv",
        "objects/microeco_dataset.rds",
        "alpha/tables/alpha_diversity.csv",
        "alpha/tables/alpha_stats.csv",
        "alpha/figures/overview/alpha_overview_violin_box.png",
        "beta/tables/beta_pcoa_coordinates.csv",
        "beta/tables/beta_permanova.csv",
        "beta/tables/beta_dispersion.csv",
        "beta/figures/pcoa/pcoa_ellipse_centroid.png",
        "tables/differential_taxa.csv",
        "tables/differential_taxa_significant.csv",
        "ai/diff_interpretation.md",
        "ai/llm_diff_interpretation.md",
        "tables/ml_feature_importance.csv",
        "tables/ml_model_metrics.csv",
        "tables/network_nodes.csv",
        "tables/network_edges.csv",
        "tables/key_taxa_score.csv",
        "tables/key_taxa_top20.csv",
        "json/diff_summary.json",
        "json/ml_summary.json",
        "json/network_summary.json",
        "json/key_taxa_summary.json",
        "figures/network_plot.png",
        "report/report.html",
        "report/report.pdf"
      )

      data.frame(
        artifact = core_files,
        exists = vapply(core_files, function(p) file.exists(file.path(job_dir, p)), logical(1)),
        stringsAsFactors = FALSE
      )
    }

    friendly_error_message <- function(e) {
      msg <- conditionMessage(e) %||% "Unknown error."

      if (inherits(e, "utf8_input_error") || grepl("invalid UTF-8", msg, ignore.case = TRUE) || grepl("non UTF-8", msg, ignore.case = TRUE)) {
        return("当前输入表包含非 UTF-8 字符，常见原因是 Excel 普通 CSV 使用 GBK 编码。请另存为 CSV UTF-8，或检查 taxonomy / metadata 中的中文和特殊符号。")
      }

      if (grepl("Missing required R packages:", msg, fixed = TRUE)) {
        pkgs <- trimws(sub(".*Missing required R packages:\\s*", "", msg))
        pkgs <- strsplit(pkgs, "[,\n]")[[1]]
        pkgs <- trimws(pkgs[nzchar(pkgs)])
        if (length(pkgs) > 0) {
          return(paste0(
            "Some required R packages are missing. Please install them, for example:\n",
            "install.packages(c(", paste(sprintf("\"%s\"", pkgs), collapse = ", "), "))"
          ))
        }
      }

      m <- stringr::str_match(msg, "there is no package called ['\"]([^'\"]+)['\"]")
      if (!is.na(m[1, 2])) {
        pkg <- m[1, 2]
        return(paste0("Missing R package: ", pkg, ". Please run: install.packages(\"", pkg, "\")"))
      }

      if (grepl("group_var not set", msg, fixed = TRUE)) {
        return("Please set a grouping variable in Parameters before running the full workflow.")
      }

      if (grepl("read_microbiome_inputs\\(", msg) || (grepl("input", msg, ignore.case = TRUE) && grepl("format", msg, ignore.case = TRUE))) {
        return("Input tables may be malformed. Please check that abundance, metadata, and taxonomy files share consistent SampleID and FeatureID fields.")
      }

      if (grepl("quarto", msg, ignore.case = TRUE) || grepl("render_report_", msg, fixed = TRUE)) {
        return("Report generation failed. Please confirm Quarto is installed and available on PATH, then try again.")
      }

      if (grepl("randomForest", msg, fixed = TRUE) && (grepl("not found", msg, fixed = TRUE) || grepl("no package", msg, ignore.case = TRUE))) {
        return("Machine learning requires the randomForest package. Please run: install.packages(\"randomForest\")")
      }

      paste0("Analysis failed. Please review the current inputs and parameters, then try again.\n\nDetails: ", msg)
    }

    step_badge_ui <- function(status) {
      status <- tolower(status %||% "waiting")
      cls <- switch(
        status,
        waiting = "kkai-badge kkai-badge--waiting",
        running = "kkai-badge kkai-badge--running",
        done = "kkai-badge kkai-badge--done",
        warning = "kkai-badge kkai-badge--warning",
        skipped = "kkai-badge kkai-badge--skipped",
        failed = "kkai-badge kkai-badge--failed",
        "kkai-badge kkai-badge--waiting"
      )

      shiny::tags$span(
        class = cls,
        if (identical(status, "running")) shiny::tags$span(class = "spinner-border spinner-border-sm kkai-inline-spinner", role = "status", `aria-hidden` = "true") else NULL,
        shiny::tags$span(status)
      )
    }

    summary_badge_ui <- function(status) {
      cls <- switch(
        status,
        idle = "kkai-badge kkai-badge--waiting",
        ready = "kkai-badge kkai-badge--waiting",
        running = "kkai-badge kkai-badge--running",
        done = "kkai-badge kkai-badge--done",
        completed_with_issues = "kkai-badge kkai-badge--warning",
        "kkai-badge kkai-badge--waiting"
      )
      shiny::tags$span(class = cls, status)
    }

    output$run_button <- shiny::renderUI({
      is_running <- identical(state$status %||% "", "running_full_workflow")
      shiny::tagList(
        shiny::actionButton(
          session$ns("run_full"),
          if (is_running) "分析运行中..." else "运行完整分析",
          class = "btn btn-primary primary-button kkai-run-full-btn",
          disabled = is_running
        ),
        if (is_running) {
          shiny::actionButton(
            session$ns("cancel_run"),
            "终止任务",
            class = "btn btn-danger",
            style = "margin-left: 10px;",
            onclick = "this.disabled=true; this.innerText='正在终止…';"
          )
        }
      )
    })

    shiny::observeEvent(input$cancel_run, {
      stopped <- cancel_background_analysis_workflow(state)
      shiny::showNotification(
        if (isTRUE(stopped)) "任务已终止。" else "已收到终止请求，将在当前步骤结束后停止。",
        type = "warning"
      )
    })

    output$summary_panel <- shiny::renderUI({
      step_message <- workflow_step_message_snapshot(state = state)
      current_step <- state$current_step %||% NULL
      current_label <- if (!is.null(current_step)) step_label_zh(current_step) else "未开始"
      current_message <- if (!is.null(current_step)) (step_message[[current_step]] %||% "") else ""

      shiny::tags$div(
        class = "kkai-workflow-summary",
        shiny::tags$div(class = "kkai-workflow-kv", shiny::tags$span("任务 ID"), shiny::tags$code(state$job_id %||% "(none)")),
        shiny::tags$div(class = "kkai-workflow-kv", shiny::tags$span("任务目录"), shiny::tags$code(state$job_dir %||% "(none)")),
        shiny::tags$div(class = "kkai-workflow-kv", shiny::tags$span("总体状态"), summary_badge_ui(overall_status())),
        shiny::tags$div(class = "kkai-workflow-kv", shiny::tags$span("当前步骤"), shiny::tags$strong(current_label)),
        if (nzchar(current_message)) shiny::tags$div(class = "kkai-workflow-current-message", current_message) else NULL
      )
    })

    output$steps_list <- shiny::renderUI({
      step_status <- workflow_step_status_snapshot(state = state)
      step_message <- workflow_step_message_snapshot(state = state)

      shiny::tags$div(
        class = "kkai-steps",
        lapply(seq_len(nrow(steps_spec)), function(i) {
          step_id <- steps_spec$step_id[[i]]
          status <- step_status[[step_id]] %||% "waiting"
          message <- step_message[[step_id]] %||% ""
          if (!nzchar(message) && identical(status, "waiting")) {
            message <- step_placeholder(step_id)
          }

          shiny::tags$div(
            class = paste("kkai-step-row", paste0("kkai-step-row--", status)),
            shiny::tags$div(
              class = "kkai-step-copy",
              shiny::tags$div(class = "kkai-step-name", steps_spec$label_zh[[i]]),
              if (nzchar(message)) shiny::tags$div(class = "kkai-step-detail", message) else NULL
            ),
            shiny::tags$div(class = "kkai-step-meta", step_badge_ui(status))
          )
        })
      )
    })

    output$artifact_table <- shiny::renderTable({
      artifact_checks()
    })

    output$run_result_panel <- shiny::renderUI({
      if (is.null(state$job_dir) || is.null(state$job_id)) return(NULL)

      report_paths <- state$report_paths %||% list()
      html_path <- report_paths$html %||% file.path(state$job_dir, "report", "report.html")
      pdf_path <- report_paths$pdf %||% file.path(state$job_dir, "report", "report.pdf")
      html_exists <- file.exists(html_path)
      pdf_exists <- file.exists(pdf_path)

      if (!html_exists && !pdf_exists && overall_status() %in% c("idle", "ready")) return(NULL)

      bslib::card(
        class = "kkai-card kkai-run-result-card",
        bslib::card_header("报告与下载"),
        shiny::tags$div(class = "kkai-results-summary",
          shiny::tags$div(shiny::tags$b("HTML"), " ", summary_badge_ui(if (html_exists) "done" else "ready")),
          shiny::tags$div(shiny::tags$b("PDF"), " ", summary_badge_ui(if (pdf_exists) "done" else if (html_exists) "completed_with_issues" else "ready"))
        ),
        if (html_exists) shiny::tags$div(class = "kkai-muted", shiny::tags$code(html_path)) else NULL,
        if (pdf_exists) shiny::tags$div(class = "kkai-muted", shiny::tags$code(pdf_path)) else NULL,
        shiny::tags$div(class = "kkai-quick-actions",
          if (html_exists) shiny::downloadButton(session$ns("dl_run_report"), "下载 HTML 报告", class = "btn btn-outline-primary") else NULL,
          shiny::downloadButton(session$ns("dl_run_zip"), "下载完整结果压缩包", class = "btn btn-outline-dark")
        ),
        if (nzchar(zip_status() %||% "")) shiny::tags$div(class = "kkai-muted", zip_status()) else NULL
      )
    })

    output$dl_run_report <- shiny::downloadHandler(
      filename = function() {
        paste0(state$job_id %||% "job", "_report.html")
      },
      content = function(file) {
        shiny::req(state$job_dir)
        report_path <- file.path(state$job_dir, "report", "report.html")
        if (!file.exists(report_path)) {
          writeLines("report.html not found for the current job.", file)
          return(invisible(NULL))
        }
        file.copy(report_path, file, overwrite = TRUE)
      },
      contentType = "text/html"
    )

    output$dl_run_zip <- shiny::downloadHandler(
      filename = function() {
        paste0("microbiome_key_taxa_ai_", state$job_id %||% "job", ".zip")
      },
      content = function(file) {
        shiny::req(state$job_dir)
        job_id <- state$job_id %||% "job"
        res <- safe_zip_job_results(job_dir = state$job_dir, job_id = job_id)
        zip_status(if (isTRUE(res$ok)) paste0("ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("ZIP failed: ", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    output$error_panel <- shiny::renderUI({
      ue <- user_error()
      if (is.null(ue) || !nzchar(ue)) return(NULL)

      de <- dev_error() %||% ""
      log_hint <- if (!is.null(state$job_dir) && dir.exists(state$job_dir)) {
        shiny::tags$div(
          class = "kkai-muted",
          "Developer log:",
          shiny::tags$code(file.path(normalizePath(state$job_dir, winslash = "/", mustWork = FALSE), "logs", "run.log"))
        )
      } else {
        NULL
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("分析失败"),
        shiny::tags$pre(class = "runanalysis-user-error", ue),
        log_hint,
        shiny::tags$details(
          shiny::tags$summary("开发者详情"),
          shiny::tags$pre(class = "runanalysis-dev-error", de)
        )
      )
    })

    shiny::observe({
      artifact_checks(build_artifact_table(state$job_dir))
    })

    shiny::observe({
      if (!identical(state$status %||% "", "running_full_workflow")) return()
      step_message <- workflow_step_message_snapshot(state = state)
      current_step <- state$current_step %||% NULL
      if (is.null(current_step)) return()
      message <- step_message[[current_step]] %||% ""
      label <- step_label_zh(current_step)
      shiny::showNotification(
        ui = shiny::tags$div(
          shiny::tags$strong(label),
          if (nzchar(message)) shiny::tags$div(message) else NULL
        ),
        id = toast_id,
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )
    })

    shiny::observe({
      if (identical(state$status %||% "", "running_full_workflow")) return()
      shiny::removeNotification(id = toast_id)
    })

    shiny::observeEvent(input$run_full, {
      shiny::req(state$job_dir, state$check_result)

      tryCatch({
        # Preserve status of data_check and build_dataset if they were already done
        current_status <- workflow_step_status_snapshot(state = state)
        keep_data <- identical(current_status$data_check, "done") || identical(current_status$data_check, "warning")
        keep_build <- identical(current_status$build_dataset, "done")

        reset_workflow_results(state, keep_check_result = TRUE)
        
        for (st in workflow_step_ids()) {
          if (st %in% c("data_check", "build_dataset") && keep_data && keep_build) next
          set_step_status(state, st, "waiting", "")
        }
        state$current_step <- if (keep_data && keep_build) "alpha" else "data_check"
        
        artifact_checks(build_artifact_table(state$job_dir))
        user_error(NULL)
        dev_error(NULL)
        zip_status("")

        if (is.null(state$input_data)) {
          state$input_data <- read_microbiome_inputs(
            abundance_path = state$input_paths$abundance_path,
            metadata_path = state$input_paths$metadata_path,
            taxonomy_path = state$input_paths$taxonomy_path
          )
        }

        group_var <- state$parameters$group_var %||% NULL
        if (is.null(group_var) || !nzchar(group_var)) {
          stop("group_var not set. Please save it in Parameters.", call. = FALSE)
        }

        workflow_set_status(state, "running_full_workflow")

        n_steps <- nrow(steps_spec)
        if (!is.null(prog_val())) {
          try(prog_val()$close(), silent = TRUE)
        }
        prog <- shiny::Progress$new(session, min = 0, max = n_steps)
        prog$set(value = 0, message = "正在运行完整分析", detail = "初始化")
        prog_val(prog)

        progress_cb <- function(step_id, status, detail = NULL) {
          status <- tolower(status %||% "")
          step_status <- workflow_step_status_snapshot(state = state)
          done_n <- sum(unlist(step_status, use.names = FALSE) %in% c("done", "warning", "skipped", "failed"))
          label <- step_label_zh(step_id)
          p <- prog_val()
          if (!is.null(p)) {
            p$set(
              value = min(done_n + if (identical(status, "running")) 0.2 else 0, n_steps),
              message = paste0("正在运行：", label),
              detail = detail %||% status
            )
          }
        }

        start_background_analysis_workflow(
          input_data = state$input_data,
          job_dir = state$job_dir,
          group_var = group_var,
          beta_distance = state$parameters$beta_distance %||% "bray",
          tax_level = state$parameters$tax_level %||% "Genus",
          config_path = "config.yml",
          progress_cb = progress_cb,
          state = state
        )

      }, error = function(e) {
        workflow_set_status(state, "full_workflow_error")
        current_step <- state$current_step %||% NULL
        if (!is.null(current_step)) {
          set_step_status(state, current_step, "failed", workflow_trim_message(conditionMessage(e)))
        }

        user_error(friendly_error_message(e))
        dev_error(paste0("Raw error:\n", conditionMessage(e)))
        artifact_checks(build_artifact_table(state$job_dir))

        shiny::showNotification("运行启动失败，请查看本页错误详情。", type = "error", duration = NULL)
        if (!is.null(state$job_dir) && dir.exists(state$job_dir)) {
          err_path <- file.path(state$job_dir, "logs", "error.log")
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
        }
      })
    })

    shiny::observeEvent(state$status, {
      st <- state$status %||% ""
      if (st == "full_workflow_done" || st == "full_workflow_error") {
        p <- prog_val()
        if (!is.null(p)) {
          try(p$close(), silent = TRUE)
          prog_val(NULL)
        }
        
        artifact_checks(build_artifact_table(state$job_dir))
        
        if (st == "full_workflow_error") {
          err_msg <- state$wf_error %||% "未知错误"
          
          current_step <- state$current_step %||% NULL
          if (!is.null(current_step)) {
            set_step_status(state, current_step, "failed", workflow_trim_message(err_msg))
          }
          
          user_error(paste0("运行失败：", err_msg))
          dev_error(paste0("Raw error:\n", err_msg))
          shiny::showNotification("运行失败，请查看本页错误详情。", type = "error", duration = NULL)
        } else {
          final_step_status <- workflow_step_status_snapshot(state = state)
          terminal_statuses <- unlist(final_step_status, use.names = FALSE)
          if (identical(final_step_status$build_dataset, "failed")) {
            workflow_set_status(state, "full_workflow_error")
          } else if (any(terminal_statuses == "failed")) {
            shiny::showNotification("完整分析已完成，但部分步骤失败或需要人工复核。", type = "warning", duration = NULL)
          } else if (any(terminal_statuses == "warning")) {
            shiny::showNotification("完整分析已完成，部分步骤带有警告。", type = "warning", duration = NULL)
          } else {
            shiny::showNotification("完整分析已完成。", type = "message", duration = NULL)
          }
        }
      }
    }, ignoreInit = TRUE)
  })
}
