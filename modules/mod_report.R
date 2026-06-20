# 报告页：生成并预览当前任务的交付产物。

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-page-header",
        shiny::tags$h2("报告中心"),
        shiny::tags$p("集中管理当前任务的 HTML 报告、图形、关键表格和完整结果压缩包。")
      ),
      shiny::uiOutput(ns("summary_bar")),
      shiny::tags$div(
        class = "kkai-report-layout",
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("报告操作"),
          shiny::uiOutput(ns("report_action_panel"))
        ),
        bslib::card(
          class = "dashboard-card",
          bslib::card_header("下载交付物"),
          shiny::uiOutput(ns("downloads_panel"))
        )
      ),
      bslib::card(
        class = "dashboard-card",
        bslib::card_header("任务信息"),
        shiny::uiOutput(ns("task_info_panel"))
      )
    )
  )
}

mod_report_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    zip_status <- shiny::reactiveVal("")
    added_resources <- new.env(parent = emptyenv())

    current_job <- shiny::reactive({
      job <- workflow_get_active_job(state)
      if (is.null(job$job_id) || is.null(job$job_dir) || !nzchar(job$job_dir)) return(NULL)
      job
    })

    current_paths <- shiny::reactive({
      job <- current_job()
      if (is.null(job)) return(NULL)
      job$report_paths %||% workflow_resolve_report_paths(job_dir = job$job_dir)
    })

    ensure_report_resource <- function(job_id, report_dir) {
      if (is.null(job_id) || is.null(report_dir) || !dir.exists(report_dir)) {
        return(invisible(FALSE))
      }
      prefix <- paste0("report_", job_id)
      if (!isTRUE(added_resources[[prefix]])) {
        shiny::addResourcePath(prefix = prefix, directoryPath = report_dir)
        added_resources[[prefix]] <- TRUE
      }
      invisible(TRUE)
    }

    count_files <- function(path) {
      if (!dir.exists(path)) return(0L)
      length(list.files(path, recursive = TRUE, full.names = TRUE, no.. = TRUE))
    }

    output$summary_bar <- shiny::renderUI({
      job <- current_job()
      if (is.null(job)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            "请先在快速开始运行分析，或在历史任务中加载一个任务。"
          )
        )
      }

      paths <- current_paths()
      report_dir <- file.path(job$job_dir, "report")
      figures_dir <- file.path(job$job_dir, "figures")
      tables_dir <- file.path(job$job_dir, "tables")

      shiny::tags$div(
        class = "kkai-stat-grid",
        shiny::tags$div(
          class = "kkai-stat-card",
          shiny::tags$div(class = "kkai-stat-label", "当前任务"),
          shiny::tags$div(class = "kkai-stat-value", shiny::tags$code(job$job_id)),
          shiny::tags$div(class = "kkai-stat-meta", if (nzchar(job$source)) job$source else "当前运行任务")
        ),
        shiny::tags$div(
          class = "kkai-stat-card",
          shiny::tags$div(class = "kkai-stat-label", "HTML 报告"),
          shiny::tags$div(
            class = "kkai-stat-value",
            ui_status_badge(if (!is.null(paths$html)) "已生成" else "未生成", kind = if (!is.null(paths$html)) "success" else "warning")
          ),
          shiny::tags$div(class = "kkai-stat-meta", normalizePath(report_dir, winslash = "/", mustWork = FALSE))
        ),
        shiny::tags$div(
          class = "kkai-stat-card",
          shiny::tags$div(class = "kkai-stat-label", "结果产物"),
          shiny::tags$div(class = "kkai-stat-value", paste0(count_files(figures_dir), " 图 / ", count_files(tables_dir), " 表")),
          shiny::tags$div(class = "kkai-stat-meta", "用于报告展示与结果归档")
        )
      )
    })

    output$report_action_panel <- shiny::renderUI({
      job <- current_job()
      if (is.null(job)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有可用于生成报告的任务。"))
      }

      paths <- current_paths()
      report_dir <- file.path(job$job_dir, "report")
      if (!is.null(paths$html)) {
        ensure_report_resource(job$job_id, report_dir)
      }

      shiny::tagList(
        shiny::tags$div(
          class = "kkai-stack",
          shiny::tags$div(
            class = "kkai-results-summary",
            shiny::tags$div(shiny::tags$b("HTML："), " ", ui_status_badge(if (!is.null(paths$html)) "已生成" else "未生成", kind = if (!is.null(paths$html)) "success" else "warning")),
            shiny::tags$div(shiny::tags$b("PDF："), " ", ui_status_badge(if (!is.null(paths$pdf)) "已生成" else "未生成", kind = if (!is.null(paths$pdf)) "success" else "warning"))
          ),
          shiny::tags$div(
            class = "kkai-quick-actions",
            shiny::actionButton(ns("render_report"), "生成 HTML 报告", class = "btn btn-primary primary-button"),
            if (!is.null(paths$html)) {
              shiny::tags$a(
                href = paste0("report_", job$job_id, "/report.html"),
                target = "_blank",
                class = "btn btn-outline-primary",
                "打开报告"
              )
            } else {
              shiny::span(class = "kkai-muted", "生成成功后可在浏览器中直接打开报告。")
            }
          ),
          shiny::tags$div(
            class = "kkai-kv",
            shiny::tags$div(shiny::tags$b("HTML 路径："), " ", shiny::tags$code(paths$html %||% file.path(job$job_dir, "report", "report.html"))),
            shiny::tags$div(shiny::tags$b("PDF 路径："), " ", shiny::tags$code(paths$pdf %||% file.path(job$job_dir, "report", "report.pdf")))
          )
        )
      )
    })

    output$downloads_panel <- shiny::renderUI({
      job <- current_job()
      if (is.null(job)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "请先准备任务，再下载交付物。"))
      }

      paths <- current_paths()

      shiny::tagList(
        shiny::tags$div(
          class = "kkai-quick-actions",
          if (!is.null(paths$html)) shiny::downloadButton(session$ns("dl_report"), "下载 HTML 报告", class = "btn btn-outline-primary") else NULL,
          shiny::downloadButton(session$ns("dl_key_tables"), "下载关键结果表", class = "btn btn-outline-dark"),
          shiny::downloadButton(session$ns("dl_figures"), "下载图形", class = "btn btn-outline-dark"),
          shiny::downloadButton(session$ns("dl_job_zip"), "下载完整结果 ZIP", class = "btn btn-primary")
        ),
        shiny::tags$div(
          class = "kkai-kv",
          shiny::tags$div(shiny::tags$b("压缩包状态："), " ", shiny::tags$code(zip_status() %||% "尚未打包")),
          shiny::tags$div(class = "kkai-muted", "建议软著答辩时同时展示报告、关键表格压缩包和完整任务 ZIP。")
        )
      )
    })

    output$task_info_panel <- shiny::renderUI({
      job <- current_job()
      if (is.null(job)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务。"))
      }

      paths <- current_paths()
      report_dir <- file.path(job$job_dir, "report")
      tables_dir <- file.path(job$job_dir, "tables")
      figures_dir <- file.path(job$job_dir, "figures")
      ai_dir <- file.path(job$job_dir, "ai")
      json_dir <- file.path(job$job_dir, "json")

      shiny::tags$div(
        class = "kkai-grid kkai-grid--2",
        shiny::tags$div(
          class = "kkai-kv",
          shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code(job$job_id)),
          shiny::tags$div(shiny::tags$b("任务目录："), " ", shiny::tags$code(normalizePath(job$job_dir, winslash = "/", mustWork = FALSE))),
          shiny::tags$div(shiny::tags$b("来源："), " ", if (nzchar(job$source)) job$source else "当前运行任务"),
          shiny::tags$div(shiny::tags$b("报告目录："), " ", shiny::tags$code(normalizePath(report_dir, winslash = "/", mustWork = FALSE)))
        ),
        shiny::tags$div(
          class = "kkai-kv",
          shiny::tags$div(shiny::tags$b("表格数量："), " ", shiny::tags$code(as.character(count_files(tables_dir)))),
          shiny::tags$div(shiny::tags$b("图形数量："), " ", shiny::tags$code(as.character(count_files(figures_dir)))),
          shiny::tags$div(shiny::tags$b("AI 文本数量："), " ", shiny::tags$code(as.character(count_files(ai_dir)))),
          shiny::tags$div(shiny::tags$b("JSON 数量："), " ", shiny::tags$code(as.character(count_files(json_dir))))
        ),
        shiny::tags$div(
          class = "kkai-grid-span-2",
          shiny::tags$details(
            shiny::tags$summary("开发者信息"),
            shiny::tags$div(
              class = "kkai-codeblock",
              jsonlite::toJSON(
                list(
                  job_id = job$job_id,
                  source = job$source,
                  report_paths = paths
                ),
                auto_unbox = TRUE,
                pretty = TRUE
              )
            )
          )
        )
      )
    })

    output$dl_report <- shiny::downloadHandler(
      filename = function() {
        job <- current_job()
        paste0((job$job_id %||% "job"), "_report.html")
      },
      content = function(file) {
        paths <- current_paths()
        if (is.null(paths$html) || !file.exists(paths$html)) {
          writeLines("report.html not found for the current job.", file)
          return(invisible(NULL))
        }
        file.copy(paths$html, file, overwrite = TRUE)
      },
      contentType = "text/html"
    )

    output$dl_key_tables <- shiny::downloadHandler(
      filename = function() {
        job <- current_job()
        paste0("microbiome_key_taxa_ai_", (job$job_id %||% "job"), "_key_tables.zip")
      },
      content = function(file) {
        job <- current_job()
        shiny::req(job)
        rel <- c(
          "tables/alpha_diversity.csv",
          "tables/beta_permanova.csv",
          "tables/differential_taxa.csv",
          "tables/ml_feature_importance.csv",
          "tables/network_nodes.csv",
          "tables/key_taxa_score.csv"
        )
        res <- safe_zip_selected_files(
          base_dir = job$job_dir,
          rel_files = rel,
          zip_basename = paste0("microbiome_key_taxa_ai_", job$job_id, "_key_tables.zip")
        )
        zip_status(if (isTRUE(res$ok)) paste0("关键表格压缩包已生成：", format(Sys.time(), "%H:%M:%S")) else paste0("关键表格压缩包失败：", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    output$dl_figures <- shiny::downloadHandler(
      filename = function() {
        job <- current_job()
        paste0("microbiome_key_taxa_ai_", (job$job_id %||% "job"), "_figures.zip")
      },
      content = function(file) {
        job <- current_job()
        shiny::req(job)
        figs_dir <- file.path(job$job_dir, "figures")
        rel <- character(0)
        if (dir.exists(figs_dir)) {
          rel <- file.path("figures", list.files(figs_dir, recursive = TRUE, full.names = FALSE, no.. = TRUE))
        }
        res <- safe_zip_selected_files(
          base_dir = job$job_dir,
          rel_files = rel,
          zip_basename = paste0("microbiome_key_taxa_ai_", job$job_id, "_figures.zip")
        )
        zip_status(if (isTRUE(res$ok)) paste0("图形压缩包已生成：", format(Sys.time(), "%H:%M:%S")) else paste0("图形压缩包失败：", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    output$dl_job_zip <- shiny::downloadHandler(
      filename = function() {
        job <- current_job()
        paste0("microbiome_key_taxa_ai_", (job$job_id %||% "job"), ".zip")
      },
      content = function(file) {
        job <- current_job()
        shiny::req(job)
        res <- safe_zip_job_results(job_dir = job$job_dir, job_id = job$job_id)
        zip_status(if (isTRUE(res$ok)) paste0("完整结果压缩包已生成：", format(Sys.time(), "%H:%M:%S")) else paste0("完整结果压缩包失败：", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    shiny::observeEvent(current_job(), {
      job <- current_job()
      if (is.null(job)) return()
      report_dir <- file.path(job$job_dir, "report")
      if (dir.exists(report_dir)) {
        ensure_report_resource(job$job_id, report_dir)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$render_report, {
      job <- current_job()
      req(job)
      tryCatch({
        report_path <- render_report_html(job$job_dir)
        pdf_path <- file.path(job$job_dir, "report", "report.pdf")
        state$report_paths <- workflow_report_paths(
          html_path = report_path,
          pdf_path = if (file.exists(pdf_path)) pdf_path else NULL
        )
        if (workflow_same_job_dir(job$job_dir, state$job_dir %||% NULL)) {
          workflow_sync_active_job(state, source = job$source %||% "当前运行任务")
        }
        ensure_report_resource(job$job_id, dirname(report_path))
        db_upsert_job(job$job_id, job$job_dir, status = "report_done")
        shiny::showNotification("HTML 报告已生成。", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste0("报告生成失败：", conditionMessage(e)), type = "error", duration = NULL)
      })
    })
  })
}
