# 历史任务模块：扫描 results/job_* 并汇总任务状态与交付物可用性。

mod_job_history_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      id = ns("history_page"),
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-page-header",
        shiny::tags$h2("历史任务"),
        shiny::tags$p("浏览已保存的分析任务，快速筛选可展示、可复核、可下载的结果目录。")
      ),
      shiny::uiOutput(ns("summary_bar")),
      shiny::tags$div(
        class = "kkai-history-toolbar",
        shiny::actionButton(ns("refresh"), "刷新任务列表", class = "btn btn-outline-primary")
      ),
      shiny::tags$div(
        class = "kkai-history-layout",
        shiny::tags$div(
          class = "kkai-history-left",
          bslib::card(
            class = "dashboard-card kkai-history-list-card",
            bslib::card_header("任务列表"),
            shiny::uiOutput(ns("table_caption")),
            DT::DTOutput(ns("jobs_table"))
          )
        ),
        shiny::tags$div(
          class = "kkai-history-right",
          shiny::uiOutput(ns("job_detail"))
        )
      ),
      shiny::uiOutput(ns("file_checks")),
      shiny::tags$script(shiny::HTML(sprintf(
        "
        (function() {
          const inputId = %s;
          const pageId = %s;
          const jobsTableId = %s;
          const closeButtonSelector = '[data-kkai-history-close=\"true\"]';
          const closingClass = 'kkai-history-detail-closing';
          const closeWithAnimation = function() {
            const detail = document.querySelector('#' + pageId + ' #job_history-job_detail');
            if (!detail || detail.classList.contains(closingClass)) return;
            detail.classList.add(closingClass);
            window.setTimeout(function() {
              if (window.Shiny) {
                Shiny.setInputValue(inputId, Date.now(), {priority: 'event'});
              }
              detail.classList.remove(closingClass);
            }, 180);
          };

          document.addEventListener('click', function(event) {
            const historyPage = event.target.closest('#%s');
            if (!historyPage) return;
            if (event.target.closest(closeButtonSelector)) {
              event.preventDefault();
              if (event.stopImmediatePropagation) event.stopImmediatePropagation();
              event.stopPropagation();
              closeWithAnimation();
              return;
            }
            const insidePinnedPanel = event.target.closest('#' + jobsTableId + ', .kkai-history-detail-card');
            if (insidePinnedPanel) return;
            closeWithAnimation();
          }, true);
        })();
        ",
        jsonlite::toJSON(ns("outside_close_detail"), auto_unbox = TRUE),
        jsonlite::toJSON(ns("history_page"), auto_unbox = TRUE),
        jsonlite::toJSON(ns("jobs_table"), auto_unbox = TRUE),
        ns("history_page")
      )))
    )
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
      "alpha/tables/alpha_diversity.csv",
      "beta/tables/beta_permanova.csv",
      "tables/differential_taxa.csv",
      "tables/ml_feature_importance.csv",
      "tables/network_nodes.csv",
      "tables/key_taxa_score.csv",
      "alpha/figures/overview/alpha_overview_violin_box.png",
      "beta/figures/pcoa/pcoa_ellipse_centroid.png",
      "figures/diff_taxa_barplot.png",
      "figures/ml_importance.png",
      "figures/network_plot.png",
      "figures/key_taxa_score_barplot.png",
      "report/report.html"
    )

    core_file_exists <- function(job_dir, relative_path) {
      preferred <- file.path(job_dir, relative_path)
      if (file.exists(preferred)) return(TRUE)
      fallback <- switch(
        relative_path,
        "alpha/tables/alpha_diversity.csv" = file.path(job_dir, "tables", "alpha_diversity.csv"),
        "alpha/figures/overview/alpha_overview_violin_box.png" = file.path(job_dir, "figures", "alpha_shannon_boxplot.png"),
        "beta/tables/beta_permanova.csv" = file.path(job_dir, "tables", "beta_permanova.csv"),
        "beta/figures/pcoa/pcoa_ellipse_centroid.png" = file.path(job_dir, "figures", "beta_pcoa_bray.png"),
        NULL
      )
      !is.null(fallback) && file.exists(fallback)
    }

    safe_read_json <- function(path) {
      if (!file.exists(path)) return(NULL)
      tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
    }

    count_files <- function(path) {
      if (!dir.exists(path)) return(0L)
      length(list.files(path, recursive = TRUE, full.names = TRUE, no.. = TRUE))
    }

    infer_created_time <- function(job_id, job_dir) {
      repro <- safe_read_json(file.path(job_dir, "reproducibility.json"))
      if (is.list(repro) && !is.null(repro$created_at) && nzchar(repro$created_at)) {
        t <- as.POSIXct(repro$created_at, format = "%Y-%m-%d %H:%M:%S", tz = "")
        if (!is.na(t)) return(t)
      }

      m <- stringr::str_match(job_id, "^job_(\\d{8})_(\\d{6})_")
      if (is.matrix(m) && nrow(m) == 1 && !is.na(m[1, 2]) && !is.na(m[1, 3])) {
        t <- as.POSIXct(paste0(m[1, 2], m[1, 3]), format = "%Y%m%d%H%M%S", tz = "")
        if (!is.na(t)) return(t)
      }

      fi <- file.info(job_dir)
      as.POSIXct(fi$ctime %||% fi$mtime, tz = "")
    }

    infer_parameters <- function(job_dir) {
      repro <- safe_read_json(file.path(job_dir, "reproducibility.json"))
      out <- list(group_var = NA_character_, tax_level = NA_character_, beta_distance = NA_character_)
      if (!is.list(repro)) return(out)

      params <- repro$parameters %||% list()
      out$group_var <- params$group_var %||% repro$phase5$group_var %||% repro$phase3$group_var %||% NA_character_
      out$tax_level <- params$tax_level %||% repro$phase5$tax_level %||% repro$phase3$tax_level %||% NA_character_
      out$beta_distance <- params$beta_distance %||% repro$phase3$beta_distance %||% NA_character_
      out
    }

    detect_status <- function(report_exists, core_exist_count) {
      if (isTRUE(report_exists)) return("completed")
      if (core_exist_count == 0) return("incomplete")
      "partial"
    }

    scan_jobs <- function() {
      if (!dir.exists(results_dir)) return(data.frame())

      job_dirs <- list.dirs(results_dir, full.names = TRUE, recursive = FALSE)
      job_dirs <- job_dirs[grepl("^job_", basename(job_dirs))]
      job_dirs <- job_dirs[dir.exists(job_dirs)]
      if (length(job_dirs) == 0) return(data.frame())

      rows <- lapply(job_dirs, function(job_dir) {
        job_id <- basename(job_dir)
        params <- infer_parameters(job_dir)

        report_path <- file.path(job_dir, "report", "report.html")
        report_exists <- file.exists(report_path)
        key_taxa_score_exists <- file.exists(file.path(job_dir, "tables", "key_taxa_score.csv"))
        ml_exists <- file.exists(file.path(job_dir, "tables", "ml_feature_importance.csv")) ||
          file.exists(file.path(job_dir, "figures", "ml_importance.png"))
        network_exists <- file.exists(file.path(job_dir, "tables", "network_nodes.csv")) ||
          file.exists(file.path(job_dir, "figures", "network_plot.png"))

        core_exist_count <- sum(vapply(core_files, function(path) core_file_exists(job_dir, path), logical(1)))
        status <- detect_status(report_exists, core_exist_count)
        created_time <- infer_created_time(job_id, job_dir)

        data.frame(
          job_id = job_id,
          created_time = format(created_time, "%Y-%m-%d %H:%M:%S"),
          job_dir = normalizePath(job_dir, winslash = "/", mustWork = FALSE),
          group_var = params$group_var,
          tax_level = params$tax_level,
          beta_distance = params$beta_distance,
          report_exists = report_exists,
          key_taxa_score_exists = key_taxa_score_exists,
          ml_exists = ml_exists,
          network_exists = network_exists,
          status = status,
          core_exist_count = core_exist_count,
          core_total = length(core_files),
          table_count = count_files(file.path(job_dir, "tables")) + count_files(file.path(job_dir, "alpha", "tables")) + count_files(file.path(job_dir, "beta", "tables")),
          figure_count = count_files(file.path(job_dir, "figures")) + count_files(file.path(job_dir, "alpha", "figures")) + count_files(file.path(job_dir, "beta", "figures")),
          json_count = count_files(file.path(job_dir, "json")),
          ai_count = count_files(file.path(job_dir, "ai")),
          stringsAsFactors = FALSE
        )
      })

      df <- do.call(rbind, rows)
      if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

      suppressWarnings({
        df$created_time_posix <- as.POSIXct(df$created_time, format = "%Y-%m-%d %H:%M:%S", tz = "")
      })
      ord <- order(df$created_time_posix, decreasing = TRUE, na.last = TRUE)
      df <- df[ord, , drop = FALSE]
      df$created_time_posix <- NULL
      rownames(df) <- NULL
      df
    }

    jobs <- shiny::reactiveVal(scan_jobs())

    badge_html <- function(label, cls) {
      sprintf("<span class=\"%s\">%s</span>", cls, htmltools::htmlEscape(label))
    }

    bool_badge_html <- function(value) {
      if (isTRUE(value)) {
        badge_html("已生成", "kkai-badge kkai-badge--done")
      } else {
        badge_html("未生成", "kkai-badge kkai-badge--waiting")
      }
    }

    status_badge_html <- function(status) {
      switch(
        status %||% "incomplete",
        completed = badge_html("已完成", "kkai-badge kkai-badge--done"),
        partial = badge_html("部分完成", "kkai-badge kkai-badge--warning"),
        incomplete = badge_html("未完成", "kkai-badge kkai-badge--failed"),
        badge_html("未知", "kkai-badge kkai-badge--waiting")
      )
    }

    shiny::observeEvent(input$refresh, {
      jobs(scan_jobs())
    }, ignoreInit = TRUE)

    output$summary_bar <- shiny::renderUI({
      df <- jobs()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前 results/ 目录下还没有历史任务。"))
      }

      shiny::tags$div(
        class = "kkai-stat-grid",
        shiny::tags$div(
          class = "kkai-stat-card",
          shiny::tags$div(class = "kkai-stat-label", "任务总数"),
          shiny::tags$div(class = "kkai-stat-value", nrow(df)),
          shiny::tags$div(class = "kkai-stat-meta", "按创建时间倒序展示")
        ),
        shiny::tags$div(
          class = "kkai-stat-card",
          shiny::tags$div(class = "kkai-stat-label", "完整报告任务"),
          shiny::tags$div(class = "kkai-stat-value", sum(df$report_exists, na.rm = TRUE)),
          shiny::tags$div(class = "kkai-stat-meta", "含 report/report.html")
        ),
        shiny::tags$div(
          class = "kkai-stat-card",
          shiny::tags$div(class = "kkai-stat-label", "关键菌评分任务"),
          shiny::tags$div(class = "kkai-stat-value", sum(df$key_taxa_score_exists, na.rm = TRUE)),
          shiny::tags$div(class = "kkai-stat-meta", "可直接用于结果展示")
        )
      )
    })

    output$table_caption <- shiny::renderUI({
      df <- jobs()
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      shiny::tags$div(
        class = "kkai-muted",
        paste0("共 ", nrow(df), " 个任务。点击任意一行可查看详情、加载当前任务或下载交付物。")
      )
    })

    output$jobs_table <- DT::renderDT({
      df <- jobs()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "results/ 下未找到任务。"), rownames = FALSE, options = list(dom = "t")))
      }

      tbl <- data.frame(
        任务ID = df$job_id,
        创建时间 = df$created_time,
        分组变量 = ifelse(is.na(df$group_var) | !nzchar(df$group_var), "-", df$group_var),
        分类层级 = ifelse(is.na(df$tax_level) | !nzchar(df$tax_level), "-", df$tax_level),
        报告 = vapply(df$report_exists, bool_badge_html, character(1)),
        KeyTaxa = vapply(df$key_taxa_score_exists, bool_badge_html, character(1)),
        机器学习 = vapply(df$ml_exists, bool_badge_html, character(1)),
        网络 = vapply(df$network_exists, bool_badge_html, character(1)),
        状态 = vapply(df$status, status_badge_html, character(1)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        tbl,
        rownames = FALSE,
        escape = FALSE,
        selection = "single",
        options = list(
          pageLength = 10,
          order = list(list(1, "desc")),
          autoWidth = TRUE,
          scrollX = TRUE
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

    shiny::observeEvent(input$outside_close_detail, {
      if (isTRUE(detail_open())) {
        detail_open(FALSE)
        try(DT::selectRows(proxy_jobs, integer(0)), silent = TRUE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_job, {
      sj <- selected_job()
      shiny::req(sj)

      job_id <- sj$job_id[[1]]
      job_dir <- sj$job_dir[[1]]
      restore_analysis_state_from_job(state = state, job_dir = job_dir, job_id = job_id)
      workflow_set_active_job(
        state = state,
        job_id = job_id,
        job_dir = job_dir,
        source = "当前选中的历史任务",
        status = sj$status[[1]] %||% "active"
      )
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
            class = "dashboard-card kkai-history-detail-card",
            bslib::card_header("任务详情"),
            shiny::tags$div(class = "kkai-alert kkai-alert--info", "请选择一行查看详情，或点击“加载此任务”切换当前任务。")
          )
        )
      }

      job_id <- sj$job_id[[1]]
      job_dir <- sj$job_dir[[1]]
      report_path <- file.path(job_dir, "report", "report.html")
      report_exists <- isTRUE(sj$report_exists[[1]])
      prefix <- paste0("history_report_", job_id)
      report_dir <- dirname(report_path)

      if (report_exists && dir.exists(report_dir) && !isTRUE(added_resources[[prefix]])) {
        shiny::addResourcePath(prefix = prefix, directoryPath = report_dir)
        added_resources[[prefix]] <- TRUE
      }

      bslib::card(
        class = "dashboard-card kkai-history-detail-card",
        bslib::card_header(
          shiny::tags$div(
            class = "kkai-job-top",
            shiny::tags$span("任务详情"),
            shiny::tags$div(
              class = "kkai-job-actions",
              shiny::actionButton(ns("load_job"), "加载此任务", class = "btn btn-sm btn-primary"),
              shiny::tags$button(
                type = "button",
                class = "btn btn-sm btn-outline-secondary",
                `data-kkai-history-close` = "true",
                "关闭详情"
              )
            )
          )
        ),
        shiny::tags$div(
          class = "kkai-kv",
          shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code(job_id)),
          shiny::tags$div(shiny::tags$b("创建时间："), " ", sj$created_time[[1]] %||% "(unknown)"),
          shiny::tags$div(shiny::tags$b("任务状态："), " ", shiny::HTML(status_badge_html(sj$status[[1]]))),
          shiny::tags$div(shiny::tags$b("分组变量："), " ", shiny::tags$code(sj$group_var[[1]] %||% "-")),
          shiny::tags$div(shiny::tags$b("分类层级："), " ", shiny::tags$code(sj$tax_level[[1]] %||% "-")),
          shiny::tags$div(shiny::tags$b("Beta 距离："), " ", shiny::tags$code(sj$beta_distance[[1]] %||% "-"))
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "kkai-grid kkai-grid--2",
          shiny::tags$div(
            class = "kkai-kv",
            shiny::tags$div(shiny::tags$b("任务目录：")),
            shiny::tags$div(class = "kkai-codeblock", job_dir),
            shiny::tags$div(shiny::tags$b("报告路径：")),
            shiny::tags$div(class = "kkai-codeblock", report_path)
          ),
          shiny::tags$div(
            class = "kkai-kv",
            shiny::tags$div(shiny::tags$b("核心产物："), " ", shiny::tags$code(paste0(sj$core_exist_count[[1]], " / ", sj$core_total[[1]]))),
            shiny::tags$div(shiny::tags$b("表格数量："), " ", shiny::tags$code(as.character(sj$table_count[[1]]))),
            shiny::tags$div(shiny::tags$b("图形数量："), " ", shiny::tags$code(as.character(sj$figure_count[[1]]))),
            shiny::tags$div(shiny::tags$b("JSON 数量："), " ", shiny::tags$code(as.character(sj$json_count[[1]]))),
            shiny::tags$div(shiny::tags$b("AI 文本数量："), " ", shiny::tags$code(as.character(sj$ai_count[[1]])))
          )
        ),
        shiny::tags$hr(),
        shiny::tags$div(
          class = "kkai-quick-actions",
          if (report_exists) {
            shiny::tags$a(
              href = paste0(prefix, "/report.html"),
              target = "_blank",
              class = "btn btn-outline-primary",
              "打开报告"
            )
          } else {
            shiny::span(class = "kkai-muted", "当前任务尚未生成报告。")
          },
          if (report_exists) shiny::downloadButton(ns("dl_hist_report"), "下载报告", class = "btn btn-outline-primary") else NULL,
          shiny::downloadButton(ns("dl_hist_zip"), "下载完整结果 ZIP", class = "btn btn-outline-dark")
        ),
        shiny::tags$div(style = "margin-top: 0.75rem;", class = "kkai-muted", paste0("压缩包状态：", zip_status() %||% "尚未打包"))
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
        report_path <- file.path(sj$job_dir[[1]], "report", "report.html")
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
        res <- safe_zip_job_results(job_dir = sj$job_dir[[1]], job_id = sj$job_id[[1]])
        zip_status(if (isTRUE(res$ok)) paste0("完整结果压缩包已生成：", format(Sys.time(), "%H:%M:%S")) else paste0("完整结果压缩包失败：", res$message))
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

      bslib::card(
        class = "dashboard-card kkai-history-check-card",
        bslib::card_header("核心产物检查"),
        shiny::tags$div(class = "kkai-muted", "用于核对任务是否具备完整的展示与归档材料。"),
        DT::DTOutput(ns("checks_table"))
      )
    })

    output$checks_table <- DT::renderDT({
      sj <- selected_job()
      if (is.null(sj)) return(NULL)

      df <- data.frame(
        产物 = core_files,
        状态 = vapply(
          core_files,
          function(p) {
            if (core_file_exists(sj$job_dir[[1]], p)) {
              badge_html("已生成", "kkai-badge kkai-badge--done")
            } else {
              badge_html("缺失", "kkai-badge kkai-badge--failed")
            }
          },
          character(1)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        options = list(pageLength = 25, dom = "tip", autoWidth = TRUE, scrollX = TRUE)
      )
    })
  })
}
