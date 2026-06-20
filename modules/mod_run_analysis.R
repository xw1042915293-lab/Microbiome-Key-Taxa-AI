# Run analysis module: orchestrates the full Phase 2-8 workflow.

mod_run_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("运行分析"),
    shiny::p("运行完整工作流，并将所有结果写入当前任务目录。"),
    shiny::actionButton(ns("run_full"), "运行完整工作流", class = "btn-primary kkai-btn-lg"),
    shiny::hr(),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("工作流步骤"),
      shiny::uiOutput(ns("steps_cards")),
      shiny::uiOutput(ns("current_step"))
    ),
    shiny::hr(),
    shiny::verbatimTextOutput(ns("status")),
    shiny::hr(),
    shiny::uiOutput(ns("run_result_panel")),
    shiny::hr(),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("核心产物检查"),
      shiny::tableOutput(ns("artifact_table"))
    ),
    shiny::hr(),
    shiny::uiOutput(ns("error_panel"))
  )
}

mod_run_analysis_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    artifact_checks <- shiny::reactiveVal(NULL)
    user_error <- shiny::reactiveVal(NULL)
    dev_error <- shiny::reactiveVal(NULL)
    current_step_id <- shiny::reactiveVal(NULL)
    current_step_detail <- shiny::reactiveVal(NULL)
    run_done <- shiny::reactiveVal(FALSE)
    zip_status <- shiny::reactiveVal("")

    steps_spec <- data.frame(
      step_id = c("data_prep", "alpha", "beta", "diff", "ai", "ml", "network", "key_taxa", "report"),
      step = c(
        "数据准备",
        "Alpha 多样性",
        "Beta 多样性",
        "差异丰度",
        "AI 解释",
        "机器学习",
        "网络分析",
        "关键菌评分",
        "报告生成"
      ),
      stringsAsFactors = FALSE
    )

    init_steps <- function() {
      data.frame(
        step_id = steps_spec$step_id,
        step = steps_spec$step,
        status = rep("pending", nrow(steps_spec)),
        detail = rep("", nrow(steps_spec)),
        stringsAsFactors = FALSE
      )
    }

    steps_state <- shiny::reactiveVal(init_steps())

    set_step_status <- function(step_id, status, detail = NULL) {
      df <- steps_state()
      if (!is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
      i <- which(df$step_id == step_id)
      if (length(i) == 1) {
        df$status[i] <- status
        if (!is.null(detail)) df$detail[i] <- detail %||% ""
        steps_state(df)
      }
      invisible(TRUE)
    }

    friendly_error_message <- function(e) {
      msg <- conditionMessage(e) %||% "Unknown error."

      # Missing R packages (from our startup check or runtime "there is no package called ...")
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
        return("Some required R packages are missing. Please install them via install.packages(...).")
      }
      m <- stringr::str_match(msg, "there is no package called ['\"]([^'\"]+)['\"]")
      if (!is.na(m[1, 2])) {
        pkg <- m[1, 2]
        return(paste0("Missing R package: ", pkg, ". Please run: install.packages(\"", pkg, "\")"))
      }

      # group_var not set
      if (grepl("group_var not set", msg, fixed = TRUE)) {
        return("未选择分组变量。请先到参数设置中选择 group_var，再重新运行。")
      }

      # Input table issues
      if (grepl("read_microbiome_inputs\\(", msg) || grepl("input", msg, ignore.case = TRUE) && grepl("format", msg, ignore.case = TRUE)) {
        return("输入表格式可能无效。请检查丰度表、样本信息表和物种注释表中的样本 ID 与特征 ID 是否一致。")
      }

      # Quarto/report issues
      if (grepl("quarto", msg, ignore.case = TRUE) || grepl("render_report_html", msg, fixed = TRUE)) {
        return("报告生成 failed. Please confirm Quarto is installed and available on PATH, then try again.")
      }

      # randomForest missing (common for ML)
      if (grepl("randomForest", msg, fixed = TRUE) && (grepl("not found", msg, fixed = TRUE) || grepl("no package", msg, ignore.case = TRUE))) {
        return("机器学习 requires the randomForest package. Please run: install.packages(\"randomForest\")")
      }

      paste0("Analysis failed. Please check inputs and parameters, then try again.\n\nDetails: ", msg)
    }

    output$status <- shiny::renderText({
      report_path <- NULL
      if (!is.null(state$job_dir)) {
        report_path <- file.path(state$job_dir, "report", "report.html")
      }
      paste0(
        "任务 ID：", state$job_id %||% "(none)", "\n",
        "任务目录：", state$job_dir %||% "(none)", "\n",
        "状态：", state$status %||% "(none)", "\n",
        "报告：", report_path %||% "(none)"
      )
    })

    .status_badge <- function(status) {
      status <- tolower(status %||% "pending")
      cls <- switch(
        status,
        done = "kkai-badge kkai-badge--done",
        running = "kkai-badge kkai-badge--running",
        warning = "kkai-badge kkai-badge--skipped",
        failed = "kkai-badge kkai-badge--failed",
        skipped = "kkai-badge kkai-badge--skipped",
        pending = "kkai-badge kkai-badge--pending",
        "kkai-badge kkai-badge--pending"
      )
      shiny::tags$span(class = cls, status)
    }

    output$steps_cards <- shiny::renderUI({
      df <- steps_state()
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      shiny::tags$div(
        class = "kkai-steps",
        lapply(seq_len(nrow(df)), function(i) {
          st <- df$status[[i]] %||% "pending"
          detail <- df$detail[[i]] %||% ""
          shiny::tags$div(
            class = "kkai-step-row",
            shiny::tags$div(class = "kkai-step-name", df$step[[i]]),
            shiny::tags$div(
              class = "kkai-step-meta",
              .status_badge(st),
              if (nzchar(detail)) shiny::tags$span(class = "kkai-step-detail", detail) else NULL
            )
          )
        })
      )
    })

    output$current_step <- shiny::renderUI({
      sid <- current_step_id()
      if (is.null(sid)) {
        return(shiny::tags$div(class = "text-muted", "当前步骤：（空闲）"))
      }
      label <- steps_spec$step[steps_spec$step_id == sid][1] %||% sid
      detail <- current_step_detail()
      shiny::tags$div(
        shiny::tags$b("当前步骤："), " ", label,
        if (!is.null(detail) && nzchar(detail)) shiny::tags$span(class = "text-muted", paste0(" (", detail, ")")) else NULL
      )
    })

    output$artifact_table <- shiny::renderTable({
      artifact_checks()
    })

    output$run_result_panel <- shiny::renderUI({
      if (!isTRUE(run_done())) return(NULL)
      if (is.null(state$job_dir) || is.null(state$job_id)) return(NULL)

      job_id <- state$job_id
      job_dir <- normalizePath(state$job_dir, winslash = "/", mustWork = FALSE)
      report_path <- file.path(job_dir, "report", "report.html")
      report_exists <- file.exists(report_path)

      report_line <- if (report_exists) {
        shiny::tags$div(
          class = "kkai-alert kkai-alert--success",
          shiny::tags$b("report.html 可用"),
          shiny::tags$div(shiny::tags$code(report_path))
        )
      } else {
        shiny::tags$div(
          class = "kkai-alert kkai-alert--warning",
          shiny::tags$b("未找到 report.html"),
          shiny::tags$div("如仍然存在，请检查 Quarto 是否可用。")
        )
      }

      bslib::card(
        bslib::card_header("运行完成"),
        shiny::tags$div(shiny::tags$b("job_id:"), " ", shiny::tags$code(job_id)),
        shiny::tags$div(shiny::tags$b("job_dir:"), shiny::tags$br(), shiny::tags$code(job_dir)),
        shiny::tags$div(shiny::tags$b("报告状态："), " ", as.character(report_exists)),
        shiny::tags$div(shiny::tags$b("压缩包状态："), " ", shiny::tags$code(zip_status() %||% "")),
         shiny::tags$hr(),
         report_line,
         shiny::tags$hr(),
         shiny::tags$div(
           if (report_exists) shiny::downloadButton(session$ns("dl_run_report"), "下载 report.html", class = "btn-primary") else NULL,
           shiny::span(style = "margin-left: 0.5rem;"),
           shiny::downloadButton(session$ns("dl_run_zip"), "下载完整结果压缩包", class = "btn-outline-dark")
         )
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
          writeLines("未找到 report.html for the current job.", file)
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
        zip_status(if (isTRUE(res$ok)) paste0("full ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("full ZIP failed: ", res$message))
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
          class = "text-muted",
          "开发者日志：",
          shiny::tags$code(file.path(normalizePath(state$job_dir, winslash = "/", mustWork = FALSE), "logs", "error.log"))
        )
      } else NULL

      bslib::card(
        bslib::card_header("分析失败"),
        shiny::tags$pre(class = "runanalysis-user-error", ue),
        log_hint,
        shiny::tags$details(
          shiny::tags$summary("开发者详情"),
          shiny::tags$pre(class = "runanalysis-dev-error", de)
        )
      )
    })

    shiny::observeEvent(input$run_full, {
      shiny::req(state$job_dir, state$check_result)
      tryCatch({
        # Reset UI state.
        steps_state(init_steps())
        artifact_checks(NULL)
        user_error(NULL)
        dev_error(NULL)
        current_step_id(NULL)
        current_step_detail(NULL)
        run_done(FALSE)
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
        prog <- shiny::Progress$new(session, min = 0, max = n_steps)
        on.exit(prog$close(), add = TRUE)
        prog$set(value = 0, message = "正在运行分析", detail = "开始中…")

        progress_cb <- function(step_id, status, detail = NULL) {
          status <- tolower(status %||% "")
          if (!status %in% c("pending", "running", "done", "failed", "skipped", "warning")) status <- "running"

          if (identical(status, "running")) {
            current_step_id(step_id)
            current_step_detail(detail %||% "")
          }
          set_step_status(step_id, status, detail = detail %||% "")

          # Force Shiny to flush reactive updates so the step cards re-render
          try(session$flushReact(), silent = TRUE)

          done_n <- sum(steps_state()$status %in% c("done", "skipped"))
          label <- steps_spec$step[steps_spec$step_id == step_id][1] %||% step_id
          prog$set(
            value = min(done_n + if (identical(status, "running")) 0.2 else 0, n_steps),
            message = paste0("正在运行：", label),
            detail = detail %||% status
          )
        }

        log_path <- file.path(state$job_dir, "logs", "analysis_log.txt")

        res <- run_full_analysis_workflow(
          input_data = state$input_data,
          job_dir = state$job_dir,
          group_var = group_var,
          beta_distance = "bray",
          tax_level = "Genus",
          config_path = "config.yml",
          progress_cb = progress_cb,
          log_path = log_path
        )

        state$dataset <- res$dataset
        state$alpha_result <- res$alpha
        state$beta_result <- res$beta
        state$diff_result <- res$diff
        state$report_paths <- list(html = res$report_path)

        # Build an artifact existence table for the UI.
        core_files <- c(
          "tables/alpha_diversity.csv",
          "tables/alpha_stats.csv",
          "tables/beta_pcoa_coordinates.csv",
          "tables/beta_permanova.csv",
          "tables/differential_taxa.csv",
          "tables/differential_taxa_significant.csv",
          "ai/diff_interpretation.md",
          "ai/methods.md",
          "ai/figure_legends.md",
          "json/llm_request_diff.json",
          "json/llm_response_diff.json",
          "ai/llm_diff_interpretation.md",
          "ai/llm_methods.md",
          "ai/llm_figure_legends.md",
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
          "figures/ml_importance.png",
          "figures/network_plot.png",
          "figures/key_taxa_score_barplot.png",
          "report/report.html"
        )

        tbl <- data.frame(
          artifact = core_files,
          exists = vapply(core_files, function(p) file.exists(file.path(state$job_dir, p)), logical(1)),
          stringsAsFactors = FALSE
        )
        artifact_checks(tbl)

        workflow_set_status(state, "full_workflow_done")
        run_done(TRUE)

        # If LLM was skipped, reflect it as a "skipped" note without marking the whole AI step failed.
        if (is.list(res$phase4b) && isTRUE(res$phase4b$skipped)) {
          current_step_id("ai")
          current_step_detail("LLM 已跳过（未检测到 KKAI_API_KEY 或 LLM 不可用）")
        }

        shiny::showNotification("完整分析已完成。请在结果总览和报告页查看输出。", type = "message", duration = NULL)
      }, error = function(e) {
        workflow_set_status(state, "full_workflow_error")

        # Mark current step failed (best effort) and show friendly message.
        sid <- current_step_id()
        if (!is.null(sid)) set_step_status(sid, "failed")
        user_error(friendly_error_message(e))
        dev_error(paste0("Raw error:\n", conditionMessage(e)))

        shiny::showNotification("运行失败，请查看本页详情。", type = "error", duration = NULL)
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
  })
}
