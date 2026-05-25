# 历史任务模块：扫描 results/job_* 并汇总产物可用性。

mod_job_history_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h3("历史任务"),
    shiny::p("浏览保存在 results/job_* 下的历史分析任务。"),
    shiny::actionButton(ns("refresh"), "刷新", class = "btn-outline-primary"),
    shiny::hr(),
    shiny::tags$div(
      class = "kkai-history-layout",
      shiny::tags$div(class = "kkai-history-left", DT::DTOutput(ns("jobs_table"))),
      shiny::tags$div(class = "kkai-history-right", shiny::uiOutput(ns("job_detail")))
    ),
    shiny::hr(),
    shiny::uiOutput(ns("file_checks"))
  )
}

mod_job_history_server <- function(id, state, results_dir = "results") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    added_resources <- new.env(parent = emptyenv())
    zip_status <- shiny::reactiveVal("")
    detail_open <- shiny::reactiveVal(FALSE)

    results_dir <- normalizePath(results_dir, winslash = "/", mustWork = FALSE)

    core_files <- c(
      "tables/alpha_diversity.csv",
      "tables/beta_permanova.csv",
      "tables/differential_taxa.csv",
      "tables/ml_feature_importance.csv",
      "tables/network_nodes.csv",
      "tables/key_taxa_score.csv",
      "figures/alpha_shannon_boxplot.png",
      "figures/beta_pcoa_bray.png",
      "figures/diff_taxa_barplot.png",
      "figures/ml_importance.png",
      "figures/network_plot.png",
      "figures/key_taxa_score_barplot.png",
      "report/report.html"
    )

    safe_read_json <- function(path) {
      if (!file.exists(path)) return(NULL)
      tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
    }

    infer_created_time <- function(job_id, job_dir) {
      # Prefer reproducibility.json created_at if available.
      repro <- safe_read_json(file.path(job_dir, "reproducibility.json"))
      if (is.list(repro) && !is.null(repro$created_at) && nzchar(repro$created_at)) {
        t <- as.POSIXct(repro$created_at, format = "%Y-%m-%d %H:%M:%S", tz = "")
        if (!is.na(t)) return(t)
      }

      # Next, parse folder name: job_YYYYMMDD_HHMMSS_xxxxxx
      m <- stringr::str_match(job_id, "^job_(\\d{8})_(\\d{6})_")
      if (is.matrix(m) && nrow(m) == 1 && !is.na(m[1, 2]) && !is.na(m[1, 3])) {
        ymd <- m[1, 2]
        hms <- m[1, 3]
        t <- as.POSIXct(
          paste0(ymd, hms),
          format = "%Y%m%d%H%M%S",
          tz = ""
        )
        if (!is.na(t)) return(t)
      }

      # Fallback to filesystem timestamp.
      fi <- file.info(job_dir)
      t <- fi$ctime %||% fi$mtime
      as.POSIXct(t, tz = "")
    }

    infer_group_var <- function(job_dir) {
      repro <- safe_read_json(file.path(job_dir, "reproducibility.json"))
      if (is.list(repro)) {
        gv <- NULL
        if (is.list(repro$parameters)) gv <- repro$parameters$group_var %||% NULL
        if (is.null(gv) && is.list(repro$phase5)) gv <- repro$phase5$group_var %||% NULL
        if (is.null(gv) && is.list(repro$phase3)) gv <- repro$phase3$group_var %||% NULL
        if (!is.null(gv) && is.character(gv) && nzchar(gv)) return(gv)
      }

      # Some installs may store group_var elsewhere; attempt to read from summary JSONs.
      kts <- safe_read_json(file.path(job_dir, "json", "key_taxa_summary.json"))
      if (is.list(kts) && !is.null(kts$group_var) && is.character(kts$group_var) && nzchar(kts$group_var)) {
        return(kts$group_var)
      }

      NA_character_
    }

    scan_jobs <- function() {
      if (!dir.exists(results_dir)) {
        return(data.frame())
      }

      job_dirs <- list.dirs(results_dir, full.names = TRUE, recursive = FALSE)
      job_dirs <- job_dirs[grepl("job_", basename(job_dirs), fixed = TRUE)]
      job_dirs <- job_dirs[dir.exists(job_dirs)]

      if (length(job_dirs) == 0) return(data.frame())

      rows <- lapply(job_dirs, function(job_dir) {
        job_id <- basename(job_dir)

        report_path <- file.path(job_dir, "report", "report.html")
        report_exists <- file.exists(report_path)

        key_taxa_score_exists <- file.exists(file.path(job_dir, "tables", "key_taxa_score.csv"))
        ml_exists <- file.exists(file.path(job_dir, "tables", "ml_feature_importance.csv")) ||
          file.exists(file.path(job_dir, "figures", "ml_importance.png"))
        network_exists <- file.exists(file.path(job_dir, "tables", "network_nodes.csv")) ||
          file.exists(file.path(job_dir, "figures", "network_plot.png"))

        core_exist_count <- sum(file.exists(file.path(job_dir, core_files)))
        status <- if (isTRUE(report_exists)) {
          "completed"
        } else if (core_exist_count == 0) {
          "incomplete"
        } else {
          "partial"
        }

        created_time <- infer_created_time(job_id, job_dir)

        data.frame(
          job_id = job_id,
          created_time = format(created_time, "%Y-%m-%d %H:%M:%S"),
          job_dir = normalizePath(job_dir, winslash = "/", mustWork = FALSE),
          group_var = infer_group_var(job_dir),
          report_exists = report_exists,
          key_taxa_score_exists = key_taxa_score_exists,
          ml_exists = ml_exists,
          network_exists = network_exists,
          status = status,
          stringsAsFactors = FALSE
        )
      })

      df <- do.call(rbind, rows)
      if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

      # Sort newest first when created_time parses; otherwise keep stable ordering.
      suppressWarnings({
        df$created_time_posix <- as.POSIXct(df$created_time, format = "%Y-%m-%d %H:%M:%S", tz = "")
      })
      ord <- order(df$created_time_posix, decreasing = TRUE, na.last = TRUE)
      df <- df[ord, , drop = FALSE]
      df$created_time_posix <- NULL

      df
    }

    jobs <- shiny::reactiveVal(scan_jobs())

    shiny::observeEvent(input$refresh, {
      jobs(scan_jobs())
    }, ignoreInit = TRUE)

    output$jobs_table <- DT::renderDT({
      df <- jobs()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "results/ 下未找到任务。"), rownames = FALSE))
      }

      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = 10,
          order = list(list(1, "desc")),
          autoWidth = TRUE
        )
      )
    })

    selected_job <- shiny::reactive({
      df <- jobs()
      idx <- input$jobs_table_rows_selected %||% integer(0)
      if (!is.data.frame(df) || nrow(df) == 0 || length(idx) != 1) return(NULL)
      df[idx, , drop = FALSE]
    })

    proxy_jobs <- DT::dataTableProxy("jobs_table", session = session)

    shiny::observeEvent(input$jobs_table_rows_selected, {
      if (length(input$jobs_table_rows_selected %||% integer(0)) == 1) detail_open(TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$close_detail, {
      detail_open(FALSE)
      try(DT::selectRows(proxy_jobs, integer(0)), silent = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_job, {
      sj <- selected_job()
      shiny::req(sj)

      job_id <- sj$job_id[[1]]
      job_dir <- sj$job_dir[[1]]
      report_path <- file.path(job_dir, "report", "report.html")

      state$job_id <- job_id
      state$job_dir <- job_dir

      state$active_job_id <- job_id
      state$active_job_dir <- job_dir
      state$active_report_path <- report_path
      state$active_status <- sj$status[[1]] %||% "active"
      state$active_source <- "当前选中的历史任务"

      parent_session <- if (!is.null(session$parent)) session$parent else session
      shiny::updateTabsetPanel(session = parent_session, inputId = "main_nav", selected = "results")
      try(bslib::nav_select(id = "main_nav", selected = "results", session = parent_session), silent = TRUE)

      shiny::showNotification(paste0("已加载任务：", job_id), type = "message")
    }, ignoreInit = TRUE)

    output$job_detail <- shiny::renderUI({
      sj <- selected_job()
      if (!isTRUE(detail_open()) || is.null(sj)) {
        return(
          bslib::card(
            bslib::card_header("任务详情"),
            shiny::p("请选择一行查看详情，或点击“加载此任务”切换当前任务。")
          )
        )
      }

      job_id <- sj$job_id[[1]]
      job_dir <- sj$job_dir[[1]]
      report_path <- file.path(job_dir, "report", "report.html")
      report_exists <- isTRUE(sj$report_exists[[1]])

      actions_ui <- shiny::tags$div(
        class = "kkai-job-actions",
        shiny::actionButton(ns("load_job"), "加载此任务", class = "btn btn-sm btn-primary"),
        shiny::actionButton(ns("close_detail"), "关闭详情", class = "btn btn-sm btn-outline-secondary")
      )

      downloads_ui <- shiny::tagList(
        shiny::tags$div(shiny::tags$b("压缩包状态："), " ", shiny::tags$code(zip_status() %||% "")),
        shiny::tags$div(style = "margin-top: 0.5rem;",
          if (report_exists) {
            shiny::downloadButton(ns("dl_hist_report"), "下载报告", class = "btn btn-sm btn-outline-primary")
          } else {
            shiny::span(class = "text-muted", "未找到 report.html，无法下载报告。")
          }
        ),
        shiny::tags$div(style = "margin-top: 0.5rem;",
          shiny::downloadButton(ns("dl_hist_zip"), "下载完整结果 ZIP", class = "btn btn-sm btn-outline-dark")
        )
      )

      report_ui <- shiny::tagList(
        shiny::tags$div(shiny::tags$b("报告路径："), shiny::tags$br(), shiny::tags$code(report_path)),
        if (!isTRUE(report_exists)) shiny::tags$div(class = "kkai-alert kkai-alert--warning", "当前任务未找到报告（report/report.html）。")
      )

      if (report_exists) {
        # Serve report/ via a resource path for browser-friendly opening.
        prefix <- paste0("history_report_", job_id)
        report_dir <- dirname(report_path)
        if (dir.exists(report_dir) && !isTRUE(added_resources[[prefix]])) {
          shiny::addResourcePath(prefix = prefix, directoryPath = report_dir)
          added_resources[[prefix]] <- TRUE
        }
        report_ui <- shiny::tagList(
          report_ui,
          shiny::tags$div(style = "margin-top: 0.5rem;",
            shiny::tags$a(
              href = paste0(prefix, "/report.html"),
              "打开报告",
              target = "_blank",
              class = "btn btn-sm btn-outline-primary"
            )
          )
        )
      }

      bslib::card(
        bslib::card_header(
          shiny::tags$div(
            class = "kkai-job-top",
            shiny::tags$span("选中的任务"),
            actions_ui
          )
        ),
        shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code(job_id)),
        shiny::tags$div(shiny::tags$b("创建时间："), " ", sj$created_time[[1]] %||% "(unknown)"),
        shiny::tags$div(shiny::tags$b("分组变量："), " ", sj$group_var[[1]] %||% NA_character_),
        shiny::tags$div(shiny::tags$b("状态："), " ", sj$status[[1]]),
        shiny::tags$div(shiny::tags$b("报告状态："), " ", as.character(report_exists)),
        shiny::tags$hr(),
        shiny::tags$div(shiny::tags$b("任务目录："), shiny::tags$br(), shiny::tags$code(job_dir)),
        shiny::tags$hr(),
        report_ui,
        shiny::tags$hr(),
        downloads_ui
      )
    })

    output$dl_hist_report <- shiny::downloadHandler(
      filename = function() {
        sj <- selected_job()
        job_id <- if (is.null(sj)) "job" else sj$job_id[[1]]
        paste0(job_id, "_report.html")
      },
      content = function(file) {
        sj <- selected_job()
        if (is.null(sj)) {
          writeLines("未选择任务。", file)
          return(invisible(NULL))
        }
        job_dir <- sj$job_dir[[1]]
        report_path <- file.path(job_dir, "report", "report.html")
        if (!file.exists(report_path)) {
          writeLines("所选任务未找到 report.html。", file)
          return(invisible(NULL))
        }
        file.copy(report_path, file, overwrite = TRUE)
      },
      contentType = "text/html"
    )

    output$dl_hist_zip <- shiny::downloadHandler(
      filename = function() {
        sj <- selected_job()
        job_id <- if (is.null(sj)) "job" else sj$job_id[[1]]
        paste0("microbiome_key_taxa_ai_", job_id, ".zip")
      },
      content = function(file) {
        sj <- selected_job()
        if (is.null(sj)) {
          writeLines("未选择任务。", file)
          return(invisible(NULL))
        }
        job_dir <- sj$job_dir[[1]]
        job_id <- sj$job_id[[1]]
        res <- safe_zip_job_results(job_dir = job_dir, job_id = job_id)
        zip_status(if (isTRUE(res$ok)) paste0("full ZIP ready: ", format(Sys.time(), "%H:%M:%S")) else paste0("full ZIP failed: ", res$message))
        if (!isTRUE(res$ok)) {
          writeLines(paste0("ZIP failed: ", res$message), file)
          return(invisible(NULL))
        }
        file.copy(res$zip_path, file, overwrite = TRUE)
      }
    )

    output$file_checks <- shiny::renderUI({
      sj <- selected_job()
      if (is.null(sj)) return(NULL)

      job_dir <- sj$job_dir[[1]]
      checks <- data.frame(
        artifact = core_files,
        exists = vapply(core_files, function(p) file.exists(file.path(job_dir, p)), logical(1)),
        stringsAsFactors = FALSE
      )

      bslib::card(
        bslib::card_header("Core Artifact Checks"),
        DT::DTOutput(ns("checks_table"))
      )
    })

    output$checks_table <- DT::renderDT({
      sj <- selected_job()
      if (is.null(sj)) return(NULL)

      job_dir <- sj$job_dir[[1]]
      df <- data.frame(
        artifact = core_files,
        exists = vapply(core_files, function(p) file.exists(file.path(job_dir, p)), logical(1)),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 25, dom = "tip", autoWidth = TRUE)
      )
    })
  })
}
