# 快速开始仪表盘：上传、检查、运行与下载一体化。

mod_quick_start_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "hero-card kkai-qs-hero",
        shiny::tags$div(
          shiny::tags$h2("快速开始"),
          shiny::tags$p("在一个仪表盘里完成上传、检查、参数设置、运行和下载。")
        ),
        shiny::tags$div(
          class = "kkai-qs-hero-kv",
          shiny::tags$div(shiny::tags$span("任务"), shiny::textOutput(ns("job_id"), container = shiny::tags$code)),
          shiny::tags$div(shiny::tags$span("状态"), shiny::textOutput(ns("status"), container = shiny::tags$code))
        )
      ),
      shiny::uiOutput(ns("message_box")),

      shiny::tags$div(class = "kkai-section-title", "上传文件"),
      shiny::tags$div(
        class = "card-grid-3",
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("丰度表（CSV/TSV）"),
          shiny::tags$div(class = "kkai-control kkai-file-input", shiny::fileInput(ns("abundance"), NULL, accept = c(".tsv", ".csv", ".txt"))),
          shiny::uiOutput(ns("upload_abundance_badge"))
        ),
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("样本信息表（CSV/TSV）"),
          shiny::tags$div(class = "kkai-control kkai-file-input", shiny::fileInput(ns("metadata"), NULL, accept = c(".tsv", ".csv", ".txt"))),
          shiny::uiOutput(ns("upload_metadata_badge"))
        ),
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("物种注释表（CSV/TSV）"),
          shiny::tags$div(class = "kkai-control kkai-file-input", shiny::fileInput(ns("taxonomy"), NULL, accept = c(".tsv", ".csv", ".txt"))),
          shiny::uiOutput(ns("upload_taxonomy_badge"))
        )
      ),
      bslib::card(
        class = "dashboard-card kkai-upload-actions",
        bslib::card_header("保存上传文件"),
        shiny::tags$div(class = "kkai-quick-actions",
          shiny::actionButton(ns("create_job"), "保存到任务目录", class = "btn btn-primary primary-button"),
          shiny::tags$span(class = "kkai-muted", "需要同时选择 3 个文件后再保存。")
        ),
        shiny::uiOutput(ns("upload_status"))
      ),

      shiny::tags$div(class = "kkai-section-title", "数据检查与参数"),
      shiny::tags$div(
        class = "card-grid-2",
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("数据检查"),
          shiny::tags$div(class = "kkai-quick-actions", shiny::actionButton(ns("run_check"), "开始检查", class = "btn btn-primary primary-button")),
          shiny::uiOutput(ns("check_summary")),
          shiny::uiOutput(ns("check_status"))
        ),
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("参数设置"),
          shiny::tags$div(class = "kkai-control", shiny::uiOutput(ns("group_var_ui"))),
          shiny::tags$div(class = "kkai-control", shiny::selectInput(ns("tax_level"), "分类层级", choices = c("Genus", "Family", "Order", "Class", "Phylum", "Kingdom"), selected = "Genus")),
          shiny::tags$div(class = "kkai-control", shiny::selectInput(ns("beta_distance"), "Beta 距离", choices = c("bray", "jaccard", "ayc"), selected = "bray")),
          shiny::uiOutput(ns("param_summary"))
        )
      ),

      shiny::tags$div(class = "kkai-section-title", "运行分析与结果"),
      shiny::tags$div(
        class = "card-grid-2",
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("运行完整分析"),
          shiny::tags$div(class = "kkai-quick-actions", shiny::actionButton(ns("run_full"), "运行完整分析", class = "btn btn-primary primary-button")),
          shiny::uiOutput(ns("workflow_status")),
          shiny::uiOutput(ns("step_list"))
        ),
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("分析结果"),
          shiny::uiOutput(ns("results_summary")),
          shiny::uiOutput(ns("results_actions"))
        )
      )
    )
  )
}

mod_quick_start_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    added_resources <- new.env(parent = emptyenv())
    user_message <- shiny::reactiveVal("")
    check_result_rv <- shiny::reactiveVal(NULL)
    run_steps <- shiny::reactiveVal(NULL)
    run_done <- shiny::reactiveVal(FALSE)
    run_error <- shiny::reactiveVal(NULL)
    zip_status <- shiny::reactiveVal("")
    upload_saved <- shiny::reactiveVal(FALSE)

    steps_spec <- data.frame(
      step_id = c("data_prep", "alpha", "beta", "diff", "ai", "ml", "network", "key_taxa", "report"),
      step = c(
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

    init_steps <- function() {
      data.frame(
        step_id = steps_spec$step_id,
        step = steps_spec$step,
        status = rep("pending", nrow(steps_spec)),
        detail = rep("", nrow(steps_spec)),
        stringsAsFactors = FALSE
      )
    }

    set_message <- function(msg = "") {
      user_message(msg %||% "")
    }

    set_step_status <- function(step_id, status, detail = NULL) {
      df <- run_steps()
      if (!is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
      i <- which(df$step_id == step_id)
      if (length(i) == 1) {
        df$status[i] <- tolower(status %||% "pending")
        if (!is.null(detail)) df$detail[i] <- detail %||% ""
        run_steps(df)
      }
      invisible(TRUE)
    }

    ensure_report_resource <- function(job_id, job_dir) {
      if (is.null(job_dir) || is.null(job_id)) return(invisible(FALSE))
      report_dir <- file.path(job_dir, "report")
      if (!dir.exists(report_dir)) return(invisible(FALSE))
      prefix <- paste0("quickstart_report_", job_id)
      if (!isTRUE(added_resources[[prefix]])) {
        shiny::addResourcePath(prefix = prefix, directoryPath = report_dir)
        added_resources[[prefix]] <- TRUE
      }
      invisible(TRUE)
    }

    current_report_path <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      file.path(state$job_dir, "report", "report.html")
    })

    output$message_box <- shiny::renderUI({
      msg <- user_message()
      if (!nzchar(msg)) return(NULL)
      cls <- "kkai-alert kkai-alert--info"
      if (grepl("missing|failed|error|select", msg, ignore.case = TRUE)) {
        cls <- "kkai-alert kkai-alert--warning"
      }
      shiny::tags$div(class = cls, msg)
    })

    output$job_id <- shiny::renderText({
      state$job_id %||% "(none)"
    })

    output$status <- shiny::renderText({
      state$status %||% "idle"
    })

    output$upload_status <- shiny::renderUI({
      if (is.null(state$input_paths)) {
        return(ui_status_badge("尚未上传文件", kind = "warning"))
      }
      shiny::tags$div(
        class = "kkai-stack",
        shiny::tags$div(class = "kkai-kv", shiny::tags$div(shiny::tags$b("Abundance:"), " ", shiny::tags$code(basename(state$input_paths$abundance_path %||% "")))),
        shiny::tags$div(class = "kkai-kv", shiny::tags$div(shiny::tags$b("Metadata:"), " ", shiny::tags$code(basename(state$input_paths$metadata_path %||% "")))),
        shiny::tags$div(class = "kkai-kv", shiny::tags$div(shiny::tags$b("Taxonomy:"), " ", shiny::tags$code(basename(state$input_paths$taxonomy_path %||% "")))),
        ui_status_badge("已保存到任务输入目录", kind = "success")
      )
    })

    .picked_badge <- function(file_input) {
      if (is.null(file_input) || is.null(file_input$name) || !nzchar(file_input$name)) {
        return(ui_status_badge("未选择文件", kind = "warning"))
      }
      shiny::tags$div(
        class = "kkai-stack",
        ui_status_badge("已选择", kind = "success"),
        shiny::tags$div(class = "kkai-muted", shiny::tags$code(file_input$name))
      )
    }

    output$upload_abundance_badge <- shiny::renderUI({ .picked_badge(input$abundance) })
    output$upload_metadata_badge <- shiny::renderUI({ .picked_badge(input$metadata) })
    output$upload_taxonomy_badge <- shiny::renderUI({ .picked_badge(input$taxonomy) })

    output$group_var_ui <- shiny::renderUI({
      if (is.null(state$input_paths) || is.null(state$input_paths$metadata_path) || !file.exists(state$input_paths$metadata_path)) {
        return(shiny::helpText("请先上传并保存样本信息表。"))
      }
      md <- tryCatch(read_table_auto(state$input_paths$metadata_path), error = function(e) NULL)
      if (is.null(md) || !"SampleID" %in% names(md)) {
        return(shiny::helpText("样本信息表尚未就绪，或缺少 SampleID。"))
      }
      choices <- setdiff(names(md), "SampleID")
      if (length(choices) == 0) {
        return(shiny::helpText("除 SampleID 外，样本信息表至少需要一个分组变量。"))
      }
      shiny::selectInput(
        session$ns("group_var"),
        "分组变量",
        choices = choices,
        selected = state$parameters$group_var %||% choices[[1]]
      )
    })

    output$param_summary <- shiny::renderUI({
      params <- list(
        group_var = input$group_var %||% state$parameters$group_var %||% "(none)",
        tax_level = input$tax_level %||% "Genus",
        beta_distance = input$beta_distance %||% "bray"
      )
      shiny::tags$div(
        class = "kkai-param-summary",
        shiny::tags$div(class = "kkai-results-summary",
          shiny::tags$div(shiny::tags$b("分组变量："), " ", shiny::tags$code(params$group_var)),
          shiny::tags$div(shiny::tags$b("分类层级："), " ", shiny::tags$code(params$tax_level)),
          shiny::tags$div(shiny::tags$b("Beta 距离："), " ", shiny::tags$code(params$beta_distance))
        ),
        shiny::tags$details(
          shiny::tags$summary("开发者信息"),
          shiny::tags$div(class = "kkai-codeblock", jsonlite::toJSON(params, auto_unbox = TRUE, pretty = TRUE))
        )
      )
    })

    output$check_summary <- shiny::renderUI({
      res <- check_result_rv()
      if (is.null(res)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "请先执行数据检查。"))
      }
      metadata_samples <- if (!is.null(state$input_data) && is.data.frame(state$input_data$metadata)) nrow(state$input_data$metadata) else NA_integer_
      shiny::tags$div(
        class = "kkai-kv",
        shiny::tags$div(shiny::tags$b("样本数："), " ", shiny::tags$code(as.character(res$summary$n_samples %||% "(n/a)"))),
        shiny::tags$div(shiny::tags$b("特征数："), " ", shiny::tags$code(as.character(res$summary$n_features %||% "(n/a)"))),
        shiny::tags$div(shiny::tags$b("样本信息表样本数："), " ", shiny::tags$code(if (is.na(metadata_samples)) "(n/a)" else as.character(metadata_samples))),
        shiny::tags$div(
          shiny::tags$b("总体状态："),
          " ",
          ui_status_badge(
            res$status %||% "unknown",
            kind = if ((res$status %||% "") == "pass") "success" else if ((res$status %||% "") == "warning") "warning" else "error"
          )
        )
      )
    })

    output$check_status <- shiny::renderUI({
      res <- check_result_rv()
      if (is.null(res) || !nrow(res$checks)) return(NULL)
      shiny::tags$div(
        class = "kkai-check-preview",
        DT::DTOutput(session$ns("check_table"))
      )
    })

    output$check_table <- DT::renderDT({
      res <- check_result_rv()
      if (is.null(res) || !nrow(res$checks)) return(NULL)
      DT::datatable(res$checks, rownames = FALSE, options = list(pageLength = 5, dom = "tip", autoWidth = TRUE))
    })

    output$workflow_status <- shiny::renderUI({
      if (is.null(run_steps())) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "运行完整分析后，这里会显示每一步状态。"))
      }
      if (!is.null(run_error())) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--warning", run_error()))
      }
      shiny::tags$div(
        class = "kkai-alert kkai-alert--success",
        shiny::tags$div(
          class = "kkai-results-summary",
          shiny::tags$span("状态："),
          ui_status_badge(state$status %||% "idle", kind = if (grepl("error|fail", state$status %||% "", ignore.case = TRUE)) "error" else if (grepl("running", state$status %||% "", ignore.case = TRUE)) "warning" else "success")
        )
      )
    })

    output$step_list <- shiny::renderUI({
      df <- run_steps()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      shiny::tags$div(
        class = "kkai-steps",
        lapply(seq_len(nrow(df)), function(i) {
          st <- df$status[[i]] %||% "pending"
          badge_kind <- switch(
            st,
            done = "success",
            running = "warning",
            failed = "error",
            skipped = "warning",
            pending = "warning",
            "warning"
          )
          shiny::tags$div(
            class = "kkai-step-row",
            shiny::tags$div(class = "kkai-step-name", df$step[[i]]),
            shiny::tags$div(
              class = "kkai-step-meta",
              ui_status_badge(st, kind = badge_kind),
              if (nzchar(df$detail[[i]] %||% "")) shiny::tags$span(class = "kkai-step-detail", df$detail[[i]]) else NULL
            )
          )
        })
      )
    })

    output$results_summary <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "尚未完成任务。"))
      }
      report_path <- current_report_path()
      report_exists <- !is.null(report_path) && file.exists(report_path)
      shiny::tags$div(
        class = "kkai-kv",
        shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code(state$job_id %||% "(none)")),
        shiny::tags$div(
          shiny::tags$b("报告状态："),
          " ",
          ui_status_badge(if (isTRUE(report_exists)) "available" else "missing", kind = if (isTRUE(report_exists)) "success" else "warning")
        ),
        shiny::tags$details(
          shiny::tags$summary("开发者信息"),
          shiny::tags$div(class = "kkai-details-grid",
            shiny::tags$div(shiny::tags$b("任务目录：")),
            shiny::tags$div(class = "kkai-codeblock", normalizePath(state$job_dir, winslash = "/", mustWork = FALSE))
          )
        )
      )
    })

    output$results_actions <- shiny::renderUI({
      if (is.null(state$job_dir)) return(NULL)
      report_path <- current_report_path()
      report_exists <- !is.null(report_path) && file.exists(report_path)
      if (isTRUE(report_exists)) ensure_report_resource(state$job_id, state$job_dir)

      shiny::tagList(
        shiny::tags$div(class = "kkai-quick-actions",
          if (isTRUE(report_exists)) {
            shiny::tags$a(href = paste0("quickstart_report_", state$job_id, "/report.html"), target = "_blank", class = "btn btn-outline-primary", "打开报告")
          } else {
            shiny::span(class = "kkai-muted", "报告生成完成后可打开。")
          }
        ),
        shiny::tags$div(style = "margin-top: 0.5rem;", shiny::downloadButton(session$ns("dl_report"), "下载报告", class = "btn btn-outline-primary")),
        shiny::tags$div(style = "margin-top: 0.5rem;", shiny::downloadButton(session$ns("dl_zip"), "下载完整结果", class = "btn btn-outline-dark")),
        shiny::tags$div(style = "margin-top: 0.5rem;", class = "kkai-muted", "压缩包使用现有任务打包逻辑。")
      )
    })

    output$dl_report <- shiny::downloadHandler(
      filename = function() paste0(state$job_id %||% "job", "_report.html"),
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

    output$dl_zip <- shiny::downloadHandler(
      filename = function() paste0("microbiome_key_taxa_ai_", state$job_id %||% "job", ".zip"),
      content = function(file) {
        shiny::req(state$job_dir)
        res <- safe_zip_job_results(job_dir = state$job_dir, job_id = state$job_id %||% "job")
        zip_status(if (isTRUE(res$ok)) paste0("ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("ZIP failed: ", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    shiny::observeEvent(input$create_job, {
      shiny::req(input$abundance, input$metadata, input$taxonomy)
      set_message("")
      tryCatch({
        job_dir <- create_job_dir()
        job_id <- basename(job_dir)
        state$job_id <- job_id
        state$job_dir <- job_dir
        workflow_set_status(state, "job_created")

        state$active_job_id <- job_id
        state$active_job_dir <- job_dir
        state$active_report_path <- file.path(job_dir, "report", "report.html")
        state$active_status <- state$status %||% "job_created"
        state$active_source <- "当前新运行任务"

        paths <- list(
          abundance = file.path(job_dir, "input", "abundance.tsv"),
          metadata = file.path(job_dir, "input", "metadata.tsv"),
          taxonomy = file.path(job_dir, "input", "taxonomy.tsv")
        )

        abund_saved <- copy_to_job_input(input$abundance$datapath, paths$abundance)
        meta_saved <- copy_to_job_input(input$metadata$datapath, paths$metadata)
        tax_saved <- copy_to_job_input(input$taxonomy$datapath, paths$taxonomy)

        state$input_paths <- list(
          abundance_path = abund_saved,
          metadata_path = meta_saved,
          taxonomy_path = tax_saved
        )
        state$input_data <- NULL
        state$check_result <- NULL
        state$parameters <- state$parameters %||% list()
        upload_saved(TRUE)

        append_reproducibility(job_dir, list(
          job_id = job_id,
          created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          input_files = list(
            abundance = list(path = "input/abundance.tsv", md5 = file_md5(abund_saved), original_name = input$abundance$name),
            metadata = list(path = "input/metadata.tsv", md5 = file_md5(meta_saved), original_name = input$metadata$name),
            taxonomy = list(path = "input/taxonomy.tsv", md5 = file_md5(tax_saved), original_name = input$taxonomy$name)
          )
        ))

        db_upsert_job(job_id = job_id, job_dir = job_dir, status = "inputs_saved")
        db_insert_job_file(job_id, "abundance", input$abundance$name, "input/abundance.tsv", file_md5(abund_saved))
        db_insert_job_file(job_id, "metadata", input$metadata$name, "input/metadata.tsv", file_md5(meta_saved))
        db_insert_job_file(job_id, "taxonomy", input$taxonomy$name, "input/taxonomy.tsv", file_md5(tax_saved))

        set_message("文件上传完成")
        shiny::showNotification("文件上传完成。", type = "message")
      }, error = function(e) {
        set_message(paste0("Upload failed: ", conditionMessage(e)))
        shiny::showNotification(paste0("Upload failed: ", conditionMessage(e)), type = "error", duration = NULL)
      })
    })

    shiny::observeEvent(input$run_check, {
      shiny::req(state$job_dir, state$input_paths)
      tryCatch({
        if (is.null(state$input_data)) {
          state$input_data <- read_microbiome_inputs(
            abundance_path = state$input_paths$abundance_path,
            metadata_path = state$input_paths$metadata_path,
            taxonomy_path = state$input_paths$taxonomy_path
          )
        }
        group_var <- input$group_var %||% state$parameters$group_var %||% NULL
        check_res <- run_all_data_checks(state$input_data, group_var = group_var)
        state$check_result <- check_res
        check_result_rv(check_res)
        workflow_set_status(state, "data_checked")
        save_data_check_summary(check_res, state$job_dir)
        db_upsert_job(state$job_id, state$job_dir, status = paste0("data_check_", check_res$status))
        set_message(paste0("Data check complete: ", check_res$status))
        shiny::showNotification(paste0("Data check done (", check_res$status, ")."), type = "message")
      }, error = function(e) {
        set_message(paste0("Data check failed: ", conditionMessage(e)))
        shiny::showNotification(paste0("Data check failed: ", conditionMessage(e)), type = "error", duration = NULL)
      })
    })

    shiny::observeEvent(input$group_var, {
      if (!is.null(input$group_var) && nzchar(input$group_var)) {
        state$parameters <- modifyList(state$parameters %||% list(), list(group_var = input$group_var))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$tax_level, input$beta_distance), {
      state$parameters <- modifyList(state$parameters %||% list(), list(
        tax_level = input$tax_level %||% "Genus",
        beta_distance = input$beta_distance %||% "bray"
      ))
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$run_full, {
      shiny::req(state$job_dir, state$input_paths)
      if (is.null(state$check_result)) {
        set_message("请先执行数据检查。")
        shiny::showNotification("请先执行数据检查。", type = "warning")
        return()
      }

      group_var <- input$group_var %||% state$parameters$group_var %||% NULL
      if (is.null(group_var) || !nzchar(group_var)) {
        set_message("请先选择分组变量。")
        shiny::showNotification("请先选择分组变量。", type = "warning")
        return()
      }

      tryCatch({
        run_steps(init_steps())
        run_done(FALSE)
        run_error(NULL)
        zip_status("")
        workflow_set_status(state, "running_full_workflow")

        if (is.null(state$input_data)) {
          state$input_data <- read_microbiome_inputs(
            abundance_path = state$input_paths$abundance_path,
            metadata_path = state$input_paths$metadata_path,
            taxonomy_path = state$input_paths$taxonomy_path
          )
        }
        params <- modifyList(state$parameters %||% list(), list(
          group_var = group_var,
          tax_level = input$tax_level %||% "Genus",
          beta_distance = input$beta_distance %||% "bray"
        ))
        state$parameters <- params

        progress_cb <- function(step_id, status, detail = NULL) {
          set_step_status(step_id, status, detail)
        }

        res <- run_full_analysis_workflow(
          input_data = state$input_data,
          job_dir = state$job_dir,
          group_var = group_var,
          beta_distance = input$beta_distance %||% "bray",
          tax_level = input$tax_level %||% "Genus",
          progress_cb = progress_cb,
          log_path = file.path(state$job_dir, "logs", "analysis_log.txt")
        )

        state$dataset <- res$dataset
        state$alpha_result <- res$alpha
        state$beta_result <- res$beta
        state$diff_result <- res$diff
        state$ml_result <- res$ml
        state$network_result <- res$network
        state$key_taxa_result <- res$key_taxa
        state$ai_result <- res$phase4b
        state$report_paths <- list(html = res$report_path)
        ensure_report_resource(state$job_id, state$job_dir)

        if (isTRUE(res$phase4b$skipped)) {
          set_step_status("ai", "skipped", "LLM skipped or unavailable; fallback outputs generated.")
        }
        set_step_status("report", "done")
        run_done(TRUE)
        workflow_set_status(state, "full_workflow_done")
        set_message("完整分析已完成，报告和结果压缩包已生成。")
        shiny::showNotification("完整分析已完成。", type = "message", duration = NULL)
      }, error = function(e) {
        workflow_set_status(state, "full_workflow_error")
        run_error(paste0("运行失败：", conditionMessage(e)))
        set_message(paste0("运行失败：", conditionMessage(e)))
        shiny::showNotification("运行失败，请查看快速开始页面中的详情。", type = "error", duration = NULL)
      })
    })
  })
}
