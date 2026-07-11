# 示例模式 module: one-click load example data and run the existing workflow.
#
# Rules for this module:
# - No new analysis algorithms.
# - No changes to core workflow; only call existing functions.
# - Never crash if demo data is missing.

mod_demo_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h3("示例模式 (Demo)"),
    shiny::p("在这里，您可以一键加载内置示例数据，快速体验并学习本平台的完整分析流程。"),

    shiny::tags$div(
      style = "margin-bottom: 20px;",
      bslib::accordion(
        id = ns("demo_guide_accordion"),
        open = "guide_panel",
        bslib::accordion_panel(
          title = "工作流操作指南 (如何使用本平台)",
          value = "guide_panel",
          icon = shiny::icon("book-open"),
          shiny::tags$div(
            class = "kkai-workflow-guide",
            shiny::tags$h5(shiny::icon("file-upload"), " 1. 上传数据"),
            shiny::tags$p("进入左侧【快速开始】面板，您需要准备并上传三个核心文件：物种丰度表 (OTU/ASV Table)、样本元数据 (Metadata) 与物种注释表 (Taxonomy)。请确保您的数据为 .csv 或 .tsv 格式。"),
            
            shiny::tags$h5(shiny::icon("check-double"), " 2. 数据检查"),
            shiny::tags$p("上传完成后，系统会自动进行数据对齐校验。如果您的丰度表中包含某些在元数据中找不到的样本，系统会提出警告。只有对齐成功的数据才能继续。"),
            
            shiny::tags$h5(shiny::icon("sliders-h"), " 3. 参数设置"),
            shiny::tags$p("在面板中，您需要指定：", 
              shiny::tags$ul(
                shiny::tags$li("分类层级：合并物种丰度的层级（如 Phylum, Genus 等）。"),
                shiny::tags$li("Beta 多样性距离度量：常用的有 Bray-Curtis。"),
                shiny::tags$li("分组变量（极其重要）：选择您要比较的组别列（例如 Treatment）。")
              )
            ),
            
            shiny::tags$h5(shiny::icon("play-circle"), " 4. 运行分析"),
            shiny::tags$p("设置好参数后点击“一键执行全流程”。系统会在后台创建独立任务，由于包含机器学习与网络分析，大数据集可能需要几分钟时间。"),
            
            shiny::tags$h5(shiny::icon("chart-pie"), " 5. 结果总览与报告"),
            shiny::tags$p("分析完成后，您可以随时在【结果总览】中查看核心结论，或者点击【生成报告】下载完整的 HTML/PDF 格式报告。")
          )
        )
      )
    ),

    bslib::card(
      class = "kkai-card",
      bslib::card_header("操作"),
      shiny::tags$div(
        class = "kkai-demo-actions",
        shiny::actionButton(ns("load_demo"), "加载示例数据", class = "btn-primary kkai-btn-lg"),
        shiny::span(style = "margin-left: 0.5rem;"),
        shiny::actionButton(ns("run_demo"), "运行示例分析", class = "btn-success kkai-btn-lg"),
        shiny::span(style = "margin-left: 0.5rem;"),
        shiny::uiOutput(ns("open_report_btn"))
      ),
      shiny::tags$div(style = "margin-top: 0.75rem;"),
      shiny::uiOutput(ns("demo_status"))
    ),

    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("示例摘要"),
      shiny::uiOutput(ns("summary_ui"))
    ),

    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("预期输出（保存在 results/job_*/ 下）"),
      shiny::tags$ul(
        shiny::tags$li("报告：report/report.html"),
        shiny::tags$li("Alpha 图形：alpha/figures/ 下按概览和四项指标分类；其他图形继续保存在 figures/。"),
        shiny::tags$li("Beta 图形：beta/figures/pcoa/ 下保存论文级排序图，beta/figures/dispersion/ 保存离散度诊断图。"),
        shiny::tags$li("表格：tables/differential_taxa.csv、tables/ml_model_metrics.csv、tables/network_nodes.csv、tables/key_taxa_top20.csv 等"),
        shiny::tags$li("AI markdown：ai/diff_interpretation.md（以及可选的 ai/llm_diff_interpretation.md）")
      )
    )
  )
}

mod_demo_server <- function(id, state, results_dir = "results") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results_dir <- normalizePath(results_dir, winslash = "/", mustWork = FALSE)
    added_resources <- new.env(parent = emptyenv())

    demo_msg <- shiny::reactiveVal(NULL)
    demo_loaded <- shiny::reactiveVal(FALSE)
    demo_job <- shiny::reactiveVal(NULL) # list(job_id, job_dir)
    run_done <- shiny::reactiveVal(FALSE)
    zip_status <- shiny::reactiveVal("")
    prog_val <- shiny::reactiveVal(NULL)


    # Prefer user-provided data/demo/* if present; otherwise fall back to built-in data/example_*.tsv.
    find_demo_files <- function() {
      candidates <- list(
        list(
          label = "data/demo/*",
          abundance = c("data/demo/abundance.tsv", "data/demo/abundance.csv"),
          metadata = c("data/demo/metadata.tsv", "data/demo/metadata.csv"),
          taxonomy = c("data/demo/taxonomy.tsv", "data/demo/taxonomy.csv")
        ),
        list(
          label = "data/example_*.tsv",
          abundance = c("data/example_abundance.tsv"),
          metadata = c("data/example_metadata.tsv"),
          taxonomy = c("data/example_taxonomy.tsv")
        )
      )

      pick_first <- function(paths) {
        for (p in paths) if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = TRUE))
        NULL
      }

      for (c in candidates) {
        a <- pick_first(c$abundance)
        m <- pick_first(c$metadata)
        t <- pick_first(c$taxonomy)
        if (!is.null(a) && !is.null(m) && !is.null(t)) {
          return(list(ok = TRUE, label = c$label, abundance = a, metadata = m, taxonomy = t))
        }
      }

      list(ok = FALSE, label = NULL, abundance = NULL, metadata = NULL, taxonomy = NULL)
    }

    read_demo_group_var <- function(metadata_path) {
      # Heuristic: prefer Treatment if present, else Group, else first non-SampleID column.
      md <- tryCatch(read_table_auto(metadata_path), error = function(e) NULL)
      if (is.null(md) || !is.data.frame(md)) return(NULL)
      cols <- names(md)
      cols <- setdiff(cols, "SampleID")
      if ("Treatment" %in% cols) return("Treatment")
      if ("Group" %in% cols) return("Group")
      if (length(cols) > 0) return(cols[[1]])
      NULL
    }

    infer_latest_demo_job <- function() {
      if (!dir.exists(results_dir)) return(NULL)
      job_dirs <- list.dirs(results_dir, full.names = TRUE, recursive = FALSE)
      job_dirs <- job_dirs[grepl("^job_", basename(job_dirs))]
      if (length(job_dirs) == 0) return(NULL)

      rows <- lapply(job_dirs, function(job_dir) {
        repro_path <- file.path(job_dir, "reproducibility.json")
        if (!file.exists(repro_path)) return(NULL)
        x <- tryCatch(jsonlite::read_json(repro_path, simplifyVector = TRUE), error = function(e) NULL)
        if (!is.list(x)) return(NULL)
        if (!isTRUE(x$demo_mode$is_demo %||% FALSE)) return(NULL)

        created_at <- x$created_at %||% NA_character_
        t <- suppressWarnings(as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%S", tz = ""))
        if (is.na(t)) t <- file.info(job_dir)$mtime
        list(job_id = basename(job_dir), job_dir = normalizePath(job_dir, winslash = "/", mustWork = FALSE), time = t)
      })

      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(NULL)
      ord <- order(vapply(rows, function(r) as.numeric(r$time), numeric(1)), decreasing = TRUE)
      rows[[ord[[1]]]]
    }

    latest_demo <- shiny::reactiveVal(infer_latest_demo_job())

    demo_files <- shiny::reactive(find_demo_files())
    demo_available <- shiny::reactive(isTRUE(demo_files()$ok))

    output$demo_status <- shiny::renderUI({
      df <- demo_files()
      msg <- demo_msg()

      if (!isTRUE(df$ok)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("未找到示例数据。"),
            shiny::tags$div(
              "Place ",
              shiny::tags$code("abundance.csv"),
              ", ",
              shiny::tags$code("metadata.csv"),
              ", ",
              shiny::tags$code("taxonomy.csv"),
              " into ",
              shiny::tags$code("data/demo/"),
              " (or keep using the built-in example files under ",
              shiny::tags$code("data/"),
              " if they exist)."
            )
          )
        )
      }

      shiny::tags$div(
        class = if (isTRUE(demo_loaded())) "kkai-alert kkai-alert--success" else "kkai-alert kkai-alert--info",
        shiny::tags$div(
          shiny::tags$b("示例来源："),
          shiny::tags$code(df$label %||% "(unknown)")
        ),
        shiny::tags$div(shiny::tags$b("丰度表："), " ", shiny::tags$code(df$abundance)),
        shiny::tags$div(shiny::tags$b("样本信息表："), " ", shiny::tags$code(df$metadata)),
        shiny::tags$div(shiny::tags$b("物种注释表："), " ", shiny::tags$code(df$taxonomy)),
        if (!is.null(msg) && nzchar(msg)) shiny::tags$div(style = "margin-top: 0.5rem;", shiny::tags$b("状态："), msg) else NULL
      )
    })

    output$summary_ui <- shiny::renderUI({
      latest <- latest_demo()
      current_job <- workflow_get_active_job(state)
      active_job_dir <- current_job$job_dir %||% NULL
      active_job_id <- current_job$job_id %||% NULL
      gv <- state$parameters$group_var %||% NA_character_

      report_path <- if (!is.null(active_job_dir)) file.path(active_job_dir, "report", "report.html") else NULL
      report_exists <- if (!is.null(report_path)) file.exists(report_path) else FALSE

      latest_line <- if (is.null(latest)) {
        shiny::tags$div(class = "text-muted", "最新示例任务目录：（暂无）")
      } else {
        shiny::tags$div(shiny::tags$b("最新示例任务目录："), shiny::tags$br(), shiny::tags$code(latest$job_dir))
      }

      shiny::tags$div(
        class = "kkai-kv",
        shiny::tags$div(shiny::tags$b("示例已加载："), " ", shiny::tags$code(as.character(isTRUE(demo_loaded())))),
        shiny::tags$div(shiny::tags$b("已选择分组变量："), " ", shiny::tags$code(as.character(gv))),
        shiny::tags$hr(),
        shiny::tags$div(shiny::tags$b("当前任务 ID："), " ", shiny::tags$code(active_job_id %||% "(none)")),
        shiny::tags$div(
          shiny::tags$b("当前任务目录："),
          shiny::tags$br(),
          shiny::tags$code(if (is.null(active_job_dir) || !nzchar(active_job_dir)) "(none)" else normalizePath(active_job_dir, winslash = "/", mustWork = FALSE))
        ),
        shiny::tags$div(shiny::tags$b("报告路径："), shiny::tags$br(), shiny::tags$code(report_path %||% "(none)")),
        shiny::tags$div(shiny::tags$b("报告是否存在："), " ", shiny::tags$code(as.character(report_exists))),
        shiny::tags$div(shiny::tags$b("压缩包状态："), " ", shiny::tags$code(zip_status() %||% "")),
        shiny::tags$hr(),
        latest_line,
        shiny::tags$div(style = "margin-top: 0.75rem;",
          shiny::downloadButton(ns("dl_demo_report"), "下载 report.html", class = "btn-outline-primary"),
          shiny::span(style = "margin-left: 0.5rem;"),
          shiny::downloadButton(ns("dl_demo_zip"), "下载完整结果压缩包", class = "btn-outline-dark")
        )
      )
    })

    output$open_report_btn <- shiny::renderUI({
      # Prefer current active job if it has a report; otherwise fall back to the latest demo job.
      current_job <- workflow_get_active_job(state)
      job_id <- current_job$job_id %||% NULL
      job_dir <- current_job$job_dir %||% NULL
      report_path <- current_job$report_paths$html %||% NULL

      if (is.null(report_path) || !file.exists(report_path)) {
        latest <- latest_demo()
        if (is.null(latest)) return(NULL)
        job_id <- latest$job_id
        job_dir <- latest$job_dir
        report_path <- file.path(job_dir, "report", "report.html")
        if (!file.exists(report_path)) return(NULL)
      }

      prefix <- paste0("demo_report_", job_id)
      report_dir <- dirname(report_path)
      if (dir.exists(report_dir) && !isTRUE(added_resources[[prefix]])) {
        shiny::addResourcePath(prefix = prefix, directoryPath = report_dir)
        added_resources[[prefix]] <- TRUE
      }

      shiny::tags$a(
        href = paste0(prefix, "/report.html"),
        "打开示例报告",
        target = "_blank",
        class = "btn btn-outline-primary kkai-btn-lg"
      )
    })

    output$dl_demo_report <- shiny::downloadHandler(
      filename = function() {
        current_job <- workflow_get_active_job(state)
        paste0((current_job$job_id %||% (latest_demo()$job_id %||% "demo_job")), "_report.html")
      },
      content = function(file) {
        current_job <- workflow_get_active_job(state)
        target_dir <- current_job$job_dir %||% (latest_demo()$job_dir %||% NULL)
        if (is.null(target_dir)) {
          writeLines("未找到示例任务，请先加载示例数据并运行工作流。", file)
          return(invisible(NULL))
        }
        report_path <- file.path(target_dir, "report", "report.html")
        if (!file.exists(report_path)) {
          writeLines("report.html not found for the demo job.", file)
          return(invisible(NULL))
        }
        file.copy(report_path, file, overwrite = TRUE)
      },
      contentType = "text/html"
    )

    output$dl_demo_zip <- shiny::downloadHandler(
      filename = function() {
        current_job <- workflow_get_active_job(state)
        paste0("microbiome_key_taxa_ai_", (current_job$job_id %||% (latest_demo()$job_id %||% "demo_job")), ".zip")
      },
      content = function(file) {
        current_job <- workflow_get_active_job(state)
        target_dir <- current_job$job_dir %||% (latest_demo()$job_dir %||% NULL)
        target_id <- current_job$job_id %||% (latest_demo()$job_id %||% "demo_job")
        if (is.null(target_dir)) {
          writeLines("未找到示例任务，请先加载示例数据并运行工作流。", file)
          return(invisible(NULL))
        }
        res <- safe_zip_job_results(job_dir = target_dir, job_id = target_id)
        zip_status(if (isTRUE(res$ok)) paste0("full ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("full ZIP failed: ", res$message))
        if (!is.null(res$zip_path)) {
          file.copy(res$zip_path, file, overwrite = TRUE)
        }
      }
    )

    shiny::observeEvent(input$load_demo, {
      df <- demo_files()
      if (!isTRUE(df$ok)) {
        demo_msg("缺少示例数据，请查看上方说明。")
        shiny::showNotification("未找到示例数据。 Please add files under data/demo/.", type = "error", duration = NULL)
        return()
      }

      # Create a new job dir (do not affect the normal upload workflow).
      job_dir <- create_job_dir()
      job_id <- basename(job_dir)

      # Copy demo files into job_dir/input/ using the same filenames as the normal upload flow.
      paths <- list(
        abundance = file.path(job_dir, "input", "abundance.tsv"),
        metadata = file.path(job_dir, "input", "metadata.tsv"),
        taxonomy = file.path(job_dir, "input", "taxonomy.tsv")
      )
      abund_saved <- copy_to_job_input(df$abundance, paths$abundance)
      meta_saved <- copy_to_job_input(df$metadata, paths$metadata)
      tax_saved <- copy_to_job_input(df$taxonomy, paths$taxonomy)

      state$job_id <- job_id
      state$job_dir <- job_dir
      state$input_paths <- list(
        abundance_path = abund_saved,
        metadata_path = meta_saved,
        taxonomy_path = tax_saved
      )

      # Auto-select a recommended group_var.
      gv <- read_demo_group_var(meta_saved)
      if (is.null(gv) || !nzchar(gv)) gv <- "Treatment"
      state$parameters <- modifyList(state$parameters %||% list(), list(group_var = gv))

      # Reset derived fields so downstream tabs reflect the new job.
      state$input_data <- NULL
      reset_workflow_results(state)
      reset_workflow_steps(state)
      workflow_set_status(state, "demo_loaded")

      # Record reproducibility + DB like the upload step does (plus a demo marker).
      append_reproducibility(job_dir, list(
        job_id = job_id,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        demo_mode = list(is_demo = TRUE, source = df$label),
        input_files = list(
          abundance = list(path = "input/abundance.tsv", md5 = file_md5(abund_saved), original_name = basename(df$abundance)),
          metadata = list(path = "input/metadata.tsv", md5 = file_md5(meta_saved), original_name = basename(df$metadata)),
          taxonomy = list(path = "input/taxonomy.tsv", md5 = file_md5(tax_saved), original_name = basename(df$taxonomy))
        ),
        parameters = state$parameters
      ))

      db_upsert_job(job_id = job_id, job_dir = job_dir, status = "demo_loaded")
      db_insert_job_file(job_id, "abundance", basename(df$abundance), "input/abundance.tsv", file_md5(abund_saved))
      db_insert_job_file(job_id, "metadata", basename(df$metadata), "input/metadata.tsv", file_md5(meta_saved))
      db_insert_job_file(job_id, "taxonomy", basename(df$taxonomy), "input/taxonomy.tsv", file_md5(tax_saved))

      demo_job(list(job_id = job_id, job_dir = job_dir))
      latest_demo(list(job_id = job_id, job_dir = job_dir, time = Sys.time()))
      demo_loaded(TRUE)
      run_done(FALSE)
      zip_status("")
      demo_msg(paste0("Demo data loaded. job_id=", job_id, "  group_var=", gv))
      shiny::showNotification("示例数据已加载，现在可以运行示例分析。", type = "message", duration = NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_demo, {
      if (is.null(state$job_dir) || is.null(state$input_paths)) {
        shiny::showNotification("No active demo job. Click '加载示例数据' first.", type = "error", duration = NULL)
        return()
      }

      # Run data check first (same logic as the Data Check tab; no algorithm change, just reuse).
      inputs <- tryCatch(read_microbiome_inputs(
        abundance_path = state$input_paths$abundance_path,
        metadata_path = state$input_paths$metadata_path,
        taxonomy_path = state$input_paths$taxonomy_path
      ), error = function(e) e)
      if (inherits(inputs, "error")) {
        demo_msg(paste0("Failed to read demo inputs: ", conditionMessage(inputs)))
        shiny::showNotification("读取示例输入失败。", type = "error", duration = NULL)
        return()
      }
      state$input_data <- inputs

      gv <- state$parameters$group_var %||% NULL
      if (is.null(gv) || !nzchar(gv)) {
        gv <- read_demo_group_var(state$input_paths$metadata_path) %||% "Treatment"
        state$parameters <- modifyList(state$parameters %||% list(), list(group_var = gv))
      }      # Ensure we do NOT call any LLM API in demo mode
      # The state machine handles demo_mode = TRUE by skipping LLM API calls internally.

      # Match the Run Analysis tab behavior, but keep it demo-specific.
      steps_spec <- workflow_steps_spec()[, c("step_id", "label"), drop = FALSE]
      names(steps_spec)[names(steps_spec) == "label"] <- "step"

      n_steps <- nrow(steps_spec)
      if (!is.null(prog_val())) {
        try(prog_val()$close(), silent = TRUE)
      }
      prog <- shiny::Progress$new(session, min = 0, max = n_steps)
      prog$set(value = 0, message = "正在运行示例工作流", detail = "开始中…")
      prog_val(prog)

      done_flags <- new.env(parent = emptyenv())
      progress_cb <- function(step_id, status, detail = NULL) {
        status <- tolower(status %||% "")
        if (status %in% c("done", "warning", "skipped", "failed")) done_flags[[as.character(step_id)]] <- TRUE
        done_n <- length(ls(done_flags))
        label_match <- steps_spec$step[steps_spec$step_id == step_id]
        label <- if (length(label_match) >= 1) label_match[[1]] else step_id
        
        p <- prog_val()
        if (!is.null(p)) {
          p$set(
            value = min(done_n + if (identical(status, "running")) 0.2 else 0, n_steps),
            message = paste0("正在运行：", label),
            detail = detail %||% status
          )
        }
      }

      log_path <- file.path(state$job_dir, "logs", "run.log")
      
      trigger_analysis_state_machine(
        input_data = state$input_data,
        job_dir = state$job_dir,
        group_var = gv,
        beta_distance = "bray",
        tax_level = "Genus",
        config_path = "config.yml",
        progress_cb = progress_cb,
        log_path = log_path,
        state = state,
        demo_mode = TRUE,
        status_running = "running_demo_workflow",
        status_done = "demo_workflow_done",
        status_error = "demo_workflow_error"
      )

    }, ignoreInit = TRUE)

    shiny::observeEvent(state$status, {
      st <- state$status %||% ""
      if (st == "demo_workflow_done" || st == "demo_workflow_error") {
        p <- prog_val()
        if (!is.null(p)) {
          try(p$close(), silent = TRUE)
          prog_val(NULL)
        }
        
        if (st == "demo_workflow_error") {
          err_msg <- state$wf_error %||% "未知错误"
          demo_msg(paste0("Demo workflow failed: ", err_msg))
          shiny::showNotification("Demo workflow failed. See the 示例模式 page for details.", type = "error", duration = NULL)
        } else {
          db_upsert_job(state$job_id, state$job_dir, status = "demo_workflow_done")
          run_done(TRUE)
          latest_demo(list(job_id = state$job_id, job_dir = state$job_dir, time = Sys.time()))
          demo_msg("示例工作流已完成，您可以在本页打开或下载报告。")
          shiny::showNotification("示例工作流已完成。", type = "message", duration = NULL)
        }
      }
    }, ignoreInit = TRUE)
  })
}
