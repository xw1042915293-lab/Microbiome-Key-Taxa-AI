# 报告页：生成并预览 Quarto HTML 报告。

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("报告"),
    shiny::p("从当前任务结果生成 Quarto HTML 报告。"),
    shiny::actionButton(ns("render_report"), "生成 HTML 报告", class = "btn-primary"),
    shiny::hr(),
    shiny::uiOutput(ns("download_ui")),
    shiny::hr(),
    shiny::uiOutput(ns("downloads_panel"))
  )
}

mod_report_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    zip_status <- shiny::reactiveVal("")
    added_resources <- new.env(parent = emptyenv())

    output$download_ui <- shiny::renderUI({
      job_id <- state$active_job_id %||% state$job_id
      job_dir <- state$active_job_dir %||% state$job_dir
      if (is.null(job_dir)) return(NULL)
      report_path <- file.path(job_dir, "report", "report.html")
      if (!file.exists(report_path)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--warning", "当前任务尚未生成报告。"))
      }
      shiny::tags$a(
        href = paste0("report_", job_id, "/report.html"),
        "打开报告",
        target = "_blank",
        class = "btn btn-outline-primary"
      )
    })

    output$downloads_panel <- shiny::renderUI({
      job_id <- state$active_job_id %||% state$job_id
      job_dir <- state$active_job_dir %||% state$job_dir
      src <- state$active_source %||% ""

      if (is.null(job_dir)) {
        return(
          bslib::card(
            bslib::card_header("下载"),
            shiny::p("请先在快速开始运行分析，或在历史任务中加载一个任务。")
          )
        )
      }

      report_path <- file.path(job_dir, "report", "report.html")
      report_exists <- file.exists(report_path)

      open_btn <- if (report_exists) {
        shiny::tags$a(
          href = paste0("report_", job_id, "/report.html"),
          "打开报告",
          target = "_blank",
          class = "btn btn-outline-primary"
        )
      } else {
        shiny::span(class = "text-muted", "当前任务尚未生成报告。")
      }

      report_btn <- if (report_exists) shiny::downloadButton(session$ns("dl_report"), "下载报告", class = "btn-outline-primary") else NULL

      bslib::card(
        bslib::card_header("下载"),
        shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code(job_id %||% "(none)")),
        shiny::tags$div(shiny::tags$b("任务目录："), " ", shiny::tags$code(normalizePath(job_dir, winslash = "/", mustWork = FALSE))),
        shiny::tags$div(shiny::tags$b("数据来源："), " ", src %||% ""),
        shiny::tags$div(shiny::tags$b("报告路径："), shiny::tags$br(), shiny::tags$code(report_path)),
        shiny::tags$div(shiny::tags$b("报告状态："), " ", as.character(report_exists)),
        shiny::tags$div(shiny::tags$b("压缩包状态："), " ", shiny::tags$code(zip_status() %||% "")),
        shiny::tags$hr(),
        shiny::tags$div(class = "kkai-job-actions", open_btn, report_btn),
        shiny::tags$div(style = "margin-top: 0.5rem;",
          shiny::downloadButton(session$ns("dl_key_tables"), "下载关键结果表", class = "btn-outline-secondary")
        ),
        shiny::tags$div(style = "margin-top: 0.5rem;",
          shiny::downloadButton(session$ns("dl_figures"), "下载图形", class = "btn-outline-secondary")
        ),
        shiny::tags$div(style = "margin-top: 0.5rem;",
          shiny::downloadButton(session$ns("dl_job_zip"), "下载完整结果 ZIP", class = "btn-outline-dark")
        )
      )
    })

    output$dl_report <- shiny::downloadHandler(
      filename = function() {
        job_id <- state$active_job_id %||% state$job_id
        paste0(job_id %||% "job", "_report.html")
      },
      content = function(file) {
        job_dir <- state$active_job_dir %||% state$job_dir
        shiny::req(job_dir)
        report_path <- file.path(job_dir, "report", "report.html")
        if (!file.exists(report_path)) {
          writeLines("report.html not found for the current job.", file)
          return(invisible(NULL))
        }
        file.copy(report_path, file, overwrite = TRUE)
      },
      contentType = "text/html"
    )

    output$dl_key_tables <- shiny::downloadHandler(
      filename = function() {
        job_id <- state$active_job_id %||% state$job_id
        paste0("microbiome_key_taxa_ai_", job_id %||% "job", "_key_tables.zip")
      },
      content = function(file) {
        job_dir <- state$active_job_dir %||% state$job_dir
        shiny::req(job_dir)
        job_id <- state$active_job_id %||% state$job_id %||% "job"
        rel <- c(
          "tables/alpha_diversity.csv",
          "tables/beta_permanova.csv",
          "tables/differential_taxa.csv",
          "tables/ml_feature_importance.csv",
          "tables/network_nodes.csv",
          "tables/key_taxa_score.csv"
        )
        res <- safe_zip_selected_files(
          base_dir = job_dir,
          rel_files = rel,
          zip_basename = paste0("microbiome_key_taxa_ai_", job_id, "_key_tables.zip")
        )
        zip_status(if (isTRUE(res$ok)) paste0("key tables ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("key tables ZIP failed: ", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    output$dl_figures <- shiny::downloadHandler(
      filename = function() {
        job_id <- state$active_job_id %||% state$job_id
        paste0("microbiome_key_taxa_ai_", job_id %||% "job", "_figures.zip")
      },
      content = function(file) {
        job_dir <- state$active_job_dir %||% state$job_dir
        shiny::req(job_dir)
        job_id <- state$active_job_id %||% state$job_id %||% "job"
        figs_dir <- file.path(job_dir, "figures")
        rel <- character(0)
        if (dir.exists(figs_dir)) {
          rel <- file.path("figures", list.files(figs_dir, recursive = TRUE, full.names = FALSE, no.. = TRUE))
        }
        res <- safe_zip_selected_files(
          base_dir = job_dir,
          rel_files = rel,
          zip_basename = paste0("microbiome_key_taxa_ai_", job_id, "_figures.zip")
        )
        zip_status(if (isTRUE(res$ok)) paste0("figures ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("figures ZIP failed: ", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    output$dl_job_zip <- shiny::downloadHandler(
      filename = function() {
        job_id <- state$active_job_id %||% state$job_id
        paste0("microbiome_key_taxa_ai_", job_id %||% "job", ".zip")
      },
      content = function(file) {
        job_dir <- state$active_job_dir %||% state$job_dir
        shiny::req(job_dir)
        job_id <- state$active_job_id %||% state$job_id %||% "job"
        res <- safe_zip_job_results(job_dir = job_dir, job_id = job_id)
        zip_status(if (isTRUE(res$ok)) paste0("full ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("full ZIP failed: ", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    shiny::observeEvent(state$job_dir, {
      job_id <- state$active_job_id %||% state$job_id
      job_dir <- state$active_job_dir %||% state$job_dir
      req(job_dir, job_id)
      report_dir <- file.path(job_dir, "report")
      prefix <- paste0("report_", job_id)
      if (dir.exists(report_dir) && !isTRUE(added_resources[[prefix]])) {
        shiny::addResourcePath(
          prefix = prefix,
          directoryPath = report_dir
        )
        added_resources[[prefix]] <- TRUE
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$render_report, {
      job_id <- state$active_job_id %||% state$job_id
      job_dir <- state$active_job_dir %||% state$job_dir
      req(job_dir, job_id)
      tryCatch({
        report_path <- render_report_html(job_dir)
        state$report_paths <- list(html = report_path)
        prefix <- paste0("report_", job_id)
        if (!isTRUE(added_resources[[prefix]])) {
          shiny::addResourcePath(
            prefix = prefix,
            directoryPath = dirname(report_path)
          )
          added_resources[[prefix]] <- TRUE
        }
        db_upsert_job(job_id, job_dir, status = "report_done")
        shiny::showNotification("HTML 报告已生成。", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste0("报告生成失败：", conditionMessage(e)), type = "error", duration = NULL)
      })
    })
  })
}
