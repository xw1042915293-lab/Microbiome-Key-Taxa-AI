# Results overview: render module cards incrementally from shared workflow state.

mod_results_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-page-header",
        shiny::tags$h2("结果总览"),
        shiny::tags$p("分析完成到哪一步，这里就同步显示到哪一步；运行中的模块也会展示当前状态。")
      ),
      shiny::uiOutput(ns("summary_bar")),
      shiny::uiOutput(ns("module_nav")),
      shiny::tags$div(
        class = "kkai-results-grid",
        shiny::uiOutput(ns("cards"))
      )
    )
  )
}

mod_results_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    steps_spec <- workflow_steps_spec()
    selected_section <- shiny::reactiveVal("overview")
    step_spec_lookup <- function(step_id, field) {
      row <- steps_spec[steps_spec$step_id == step_id, , drop = FALSE]
      if (!nrow(row)) return(NULL)
      row[[field]][[1]]
    }

    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_figures")
    })

    alpha_fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_alpha_figures")
    })

    beta_fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_beta_figures")
    })

    beta_figure_root <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      preferred <- file.path(state$job_dir, "beta", "figures")
      if (dir.exists(preferred)) preferred else file.path(state$job_dir, "figures")
    })

    alpha_figure_root <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      preferred <- file.path(state$job_dir, "alpha", "figures")
      if (dir.exists(preferred)) preferred else file.path(state$job_dir, "figures")
    })

    job_snapshot <- shiny::reactive({
      shiny::req(state$job_dir)
      tryCatch(workflow_read_job_snapshot(state$job_dir), error = function(e) NULL)
    })

    safe_add_resource_path <- function(prefix, directory_path) {
      if (is.null(prefix) || !nzchar(prefix) || is.null(directory_path) || !dir.exists(directory_path)) {
        return(invisible(FALSE))
      }
      rp <- tryCatch(shiny::resourcePaths(), error = function(e) NULL)
      if (is.list(rp) && !is.null(rp[[prefix]])) return(invisible(FALSE))
      shiny::addResourcePath(prefix = prefix, directoryPath = directory_path)
      invisible(TRUE)
    }

    shiny::observe({
      shiny::req(state$job_dir, fig_prefix())
      safe_add_resource_path(fig_prefix(), file.path(state$job_dir, "figures"))
    })

    shiny::observe({
      shiny::req(alpha_figure_root(), alpha_fig_prefix())
      safe_add_resource_path(alpha_fig_prefix(), alpha_figure_root())
    })

    shiny::observe({
      shiny::req(beta_figure_root(), beta_fig_prefix())
      safe_add_resource_path(beta_fig_prefix(), beta_figure_root())
    })

    alpha_metric_labels <- c(
      overview = "四项核心指标概览",
      Observed = "Observed（观测丰富度）",
      Chao1 = "Chao1（估计丰富度）",
      Shannon = "Shannon（丰富度与均匀度）",
      Simpson = "Simpson（优势度敏感）"
    )
    alpha_plot_type_labels <- stats::setNames(alpha_plot_type_spec()$label, alpha_plot_type_spec()$type)

    alpha_figure_path <- function(metric, plot_type) {
      if (is.null(state$job_dir)) return(NULL)
      if (identical(metric, "overview")) {
        alpha_overview_figure_path(state$job_dir, plot_type, "png", existing = TRUE)
      } else {
        alpha_metric_figure_path(state$job_dir, metric, plot_type, "png", existing = TRUE)
      }
    }

    alpha_available_metrics <- shiny::reactive({
      candidates <- names(alpha_metric_labels)
      candidates[vapply(candidates, function(metric) {
        any(vapply(alpha_plot_type_spec()$type, function(type) file.exists(alpha_figure_path(metric, type)), logical(1)))
      }, logical(1))]
    })

    alpha_available_plot_types <- shiny::reactive({
      metrics <- alpha_available_metrics()
      if (length(metrics) == 0) return(character(0))
      metric <- input$alpha_metric %||% if ("overview" %in% metrics) "overview" else metrics[[1]]
      if (!metric %in% metrics) metric <- metrics[[1]]
      types <- alpha_plot_type_spec()$type
      types[vapply(types, function(type) file.exists(alpha_figure_path(metric, type)), logical(1))]
    })

    output$alpha_metric_control <- shiny::renderUI({
      metrics <- alpha_available_metrics()
      if (length(metrics) == 0) return(NULL)
      shiny::selectInput(
        session$ns("alpha_metric"), "选择 Alpha 指标",
        choices = stats::setNames(metrics, unname(alpha_metric_labels[metrics])),
        selected = if ("overview" %in% metrics) "overview" else metrics[[1]]
      )
    })

    output$alpha_plot_type_control <- shiny::renderUI({
      types <- alpha_available_plot_types()
      if (length(types) == 0) return(NULL)
      shiny::selectInput(
        session$ns("alpha_plot_type"), "选择科研图形",
        choices = stats::setNames(types, unname(alpha_plot_type_labels[types])),
        selected = if ("violin_box" %in% types) "violin_box" else types[[1]]
      )
    })

    output$alpha_dynamic_plot <- shiny::renderUI({
      metrics <- alpha_available_metrics()
      if (length(metrics) == 0 || is.null(alpha_fig_prefix())) return(shiny::helpText("尚未生成 Alpha 图形。"))
      metric <- input$alpha_metric %||% if ("overview" %in% metrics) "overview" else metrics[[1]]
      if (!metric %in% metrics) metric <- metrics[[1]]
      types <- alpha_available_plot_types()
      if (length(types) == 0) return(shiny::helpText("当前指标没有可用图形。"))
      plot_type <- input$alpha_plot_type %||% if ("violin_box" %in% types) "violin_box" else types[[1]]
      if (!plot_type %in% types) plot_type <- types[[1]]
      full <- alpha_figure_path(metric, plot_type)
      root <- normalizePath(alpha_figure_root(), winslash = "/", mustWork = TRUE)
      full_norm <- normalizePath(full, winslash = "/", mustWork = TRUE)
      rel <- substring(full_norm, nchar(root) + 2L)
      src <- paste0(alpha_fig_prefix(), "/", rel)

      shiny::tagList(
        shiny::tags$p(class = "kkai-muted", paste0(alpha_metric_labels[[metric]], " · ", alpha_plot_type_labels[[plot_type]])),
        shiny::tags$a(
          href = src, target = "_blank",
          shiny::tags$div(
            class = "kkai-result-image-wrap",
            shiny::tags$img(src = src, class = "kkai-result-img kkai-result-img--fit")
          )
        )
      )
    })

    beta_view_labels <- stats::setNames(beta_plot_view_spec()$label, beta_plot_view_spec()$view)

    beta_available_views <- shiny::reactive({
      if (is.null(state$job_dir)) return(character(0))
      views <- beta_plot_view_spec()$view
      views[vapply(views, function(view) file.exists(beta_figure_path(state$job_dir, view, "png", existing = TRUE)), logical(1))]
    })

    output$beta_view_control <- shiny::renderUI({
      views <- beta_available_views()
      if (length(views) == 0) return(NULL)
      shiny::selectInput(
        session$ns("beta_view"), "选择论文图形",
        choices = stats::setNames(views, unname(beta_view_labels[views])),
        selected = if ("ellipse_centroid" %in% views) "ellipse_centroid" else views[[1]]
      )
    })

    output$beta_dynamic_plot <- shiny::renderUI({
      views <- beta_available_views()
      if (length(views) == 0 || is.null(beta_fig_prefix())) return(shiny::helpText("尚未生成 Beta 图形。"))
      view <- input$beta_view %||% if ("ellipse_centroid" %in% views) "ellipse_centroid" else views[[1]]
      if (!view %in% views) view <- views[[1]]
      full <- beta_figure_path(state$job_dir, view, "png", existing = TRUE)
      root <- normalizePath(beta_figure_root(), winslash = "/", mustWork = TRUE)
      full_norm <- normalizePath(full, winslash = "/", mustWork = TRUE)
      rel <- substring(full_norm, nchar(root) + 2L)
      src <- paste0(beta_fig_prefix(), "/", rel)

      shiny::tagList(
        shiny::tags$p(class = "kkai-muted", beta_view_labels[[view]]),
        shiny::tags$a(
          href = src, target = "_blank",
          shiny::tags$div(
            class = "kkai-result-image-wrap",
            shiny::tags$img(src = src, class = "kkai-result-img kkai-result-img--fit")
          )
        )
      )
    })

    step_label_zh <- function(step_id) {
      step_spec_lookup(step_id, "label_zh") %||% step_id
    }

    step_placeholder <- function(step_id) {
      step_spec_lookup(step_id, "placeholder") %||% ""
    }

    step_status <- function(step_id) {
      workflow_step_status_snapshot(state = state)[[step_id]] %||% "waiting"
    }

    step_message <- function(step_id) {
      workflow_step_message_snapshot(state = state)[[step_id]] %||% ""
    }

    status_is <- function(step_id, values) {
      step_status(step_id) %in% values
    }

    shiny::observeEvent(state$job_id, {
      selected_section("overview")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$select_section, {
      payload <- input$select_section
      section <- if (is.list(payload)) payload$section %||% "" else payload
      scroll_y <- if (is.list(payload)) suppressWarnings(as.numeric(payload$scrollY %||% NA_real_)) else NA_real_
      if (is.character(section) && length(section) == 1 && nzchar(section)) {
        selected_section(section)
        if (is.finite(scroll_y)) {
          session$onFlushed(function() {
            session$sendCustomMessage("kkai-restore-scroll", list(y = scroll_y))
          }, once = TRUE)
        }
      }
    }, ignoreInit = TRUE)

    nav_class <- function(status, active = FALSE) {
      base <- switch(
        status,
        running = "kkai-nav-chip kkai-nav-chip--running",
        done = "kkai-nav-chip kkai-nav-chip--done",
        warning = "kkai-nav-chip kkai-nav-chip--warning",
        failed = "kkai-nav-chip kkai-nav-chip--failed",
        skipped = "kkai-nav-chip kkai-nav-chip--waiting",
        waiting = "kkai-nav-chip kkai-nav-chip--waiting",
        "kkai-nav-chip kkai-nav-chip--waiting"
      )
      if (isTRUE(active)) paste(base, "kkai-nav-chip--active") else base
    }

    status_badge <- function(status) {
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

    result_card <- function(id, title, summary, img_file = NULL, details = NULL, status = "done", extra_class = NULL) {
      image_ui <- NULL
      if (!is.null(img_file) && !is.null(state$job_dir)) {
        full <- file.path(state$job_dir, "figures", img_file)
        if (file.exists(full)) {
          image_ui <- shiny::tags$div(
            class = "kkai-result-image-wrap",
            shiny::tags$img(src = paste0(fig_prefix(), "/", img_file), class = "kkai-result-img kkai-result-img--fit")
          )
        }
      }

      if (identical(id, "ai") && !is.null(state$job_dir)) {
        diff_md <- file.path(state$job_dir, "ai", "diff_interpretation.md")
        key_md <- file.path(state$job_dir, "ai", "key_taxa_interpretation.md")
        methods_md <- file.path(state$job_dir, "ai", "methods.md")
        legends_md <- file.path(state$job_dir, "ai", "figure_legends.md")
        llm_request_path <- file.path(state$job_dir, "json", "llm_request_diff.json")

        llm_skipped <- FALSE
        if (file.exists(llm_request_path)) {
          llm_req <- tryCatch(jsonlite::read_json(llm_request_path, simplifyVector = TRUE), error = function(e) NULL)
          llm_skipped <- is.list(llm_req) && identical(llm_req$status %||% "", "skipped")
        }

        preview_nodes <- Filter(Negate(is.null), list(
          markdown_preview_ui(diff_md, max_chars = 1100L),
          markdown_preview_ui(key_md, max_chars = 800L),
          markdown_preview_ui(methods_md, max_chars = 400L),
          markdown_preview_ui(legends_md, max_chars = 400L)
        ))

        if (length(preview_nodes) > 0 && is.null(details)) {
          details <- shiny::tags$div(class = "kkai-result-summary", preview_nodes)
        }

        summary <- if (length(preview_nodes) < 1) {
          "AI 解读尚未生成"
        } else if (file.exists(diff_md) && llm_skipped) {
          "当前为本地规则解释，未调用 LLM 扩展解释。"
        } else if (file.exists(diff_md) && file.exists(file.path(state$job_dir, "ai", "llm_diff_interpretation.md"))) {
          "已生成本地规则解释与 LLM 扩展解释。"
        } else {
          "AI 解读已生成。"
        }
      }

      bslib::card(
        class = paste("dashboard-card kkai-result-card", extra_class %||% ""),
        bslib::card_header(
          shiny::tags$div(
            class = "kkai-card-header-row",
            shiny::tags$span(title),
            status_badge(status)
          )
        ),
        shiny::tags$div(class = "kkai-result-summary", summary),
        image_ui,
        details
      )
    }

    markdown_preview_ui <- function(path, max_chars = 1000L) {
      if (is.null(path) || !file.exists(path)) return(NULL)
      lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
      if (length(lines) < 1) return(NULL)
      txt <- paste(lines, collapse = "\n")
      txt <- sub("^#\\s+.*?(\\r?\\n)+", "", txt)
      txt <- trimws(txt)
      if (!nzchar(txt)) return(NULL)
      if (nchar(txt, type = "chars") > max_chars) {
        txt <- paste0(substr(txt, 1, max_chars), "\n\n...")
      }
      html <- tryCatch(markdown::markdownToHTML(text = txt, fragment.only = TRUE), error = function(e) NULL)
      if (is.null(html)) return(shiny::tags$pre(txt))
      shiny::HTML(html)
    }

    placeholder_card <- function(id, title, step_id, default_message = NULL) {
      status <- step_status(step_id)
      message <- step_message(step_id)
      if (!nzchar(message)) {
        message <- default_message %||% step_placeholder(step_id)
      }

      class_name <- switch(
        status,
        running = "kkai-alert kkai-alert--info",
        failed = "kkai-alert kkai-alert--danger",
        warning = "kkai-alert kkai-alert--warning",
        skipped = "kkai-alert kkai-alert--info",
        "kkai-alert kkai-alert--info kkai-alert--muted"
      )

      bslib::card(
        class = "dashboard-card kkai-result-card kkai-result-card--placeholder",
        bslib::card_header(
          shiny::tags$div(
            class = "kkai-card-header-row",
            shiny::tags$span(title),
            status_badge(status)
          )
        ),
        shiny::tags$div(class = class_name, message)
      )
    }

    output$diff_overview_status <- shiny::renderUI({
      snapshot <- job_snapshot()
      diff_tbl <- snapshot$diff_table %||% data.frame()
      if (!is.data.frame(diff_tbl) || nrow(diff_tbl) < 1) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前任务还没有差异丰度结果。"))
      }

      n_sig <- snapshot$diff_summary$n_significant_taxa %||%
        if ("significant" %in% names(diff_tbl)) sum(diff_tbl$significant %in% TRUE, na.rm = TRUE) else 0L

      if (n_sig > 0) {
        return(shiny::tags$div(
          class = "kkai-alert kkai-alert--success",
          paste0("检测到 ", n_sig, " 个 FDR < 0.05 的显著差异菌。")
        ))
      }

      shiny::tags$div(
        class = "kkai-alert kkai-alert--warning",
        "未检测到 FDR 显著差异菌。下方为 raw p-value 排名前列的探索性候选菌，仅用于后续筛选参考，不能表述为显著差异。"
      )
    })

    output$diff_overview_table <- DT::renderDT({
      snapshot <- job_snapshot()
      diff_tbl <- snapshot$diff_table %||% data.frame()
      if (!is.data.frame(diff_tbl) || nrow(diff_tbl) < 1) {
        return(DT::datatable(data.frame(Message = "当前任务还没有差异丰度结果。"), rownames = FALSE, options = list(dom = "t")))
      }

      if (!"taxon_label" %in% names(diff_tbl) && "taxon" %in% names(diff_tbl)) {
        diff_tbl$taxon_label <- make_taxon_display_label(diff_tbl$taxon)
      }
      if (!"direction" %in% names(diff_tbl)) {
        diff_tbl$direction <- ifelse(is.na(diff_tbl$log2fc), "Undetermined", ifelse(diff_tbl$log2fc >= 0, "Positive log2FC", "Negative log2FC"))
      }
      if (!"significance" %in% names(diff_tbl)) {
        diff_tbl$significance <- ifelse(!is.na(diff_tbl$fdr) & diff_tbl$fdr < 0.05, "significant", "exploratory")
      }

      show_cols <- intersect(c("taxon_label", "p_value", "fdr", "log2fc", "direction", "significance"), names(diff_tbl))
      df_show <- diff_tbl[, show_cols, drop = FALSE]

      dt <- DT::datatable(
        df_show,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE
        )
      )

      if ("p_value" %in% names(df_show)) dt <- DT::formatSignif(dt, "p_value", digits = 3)
      if ("fdr" %in% names(df_show)) dt <- DT::formatSignif(dt, "fdr", digits = 3)
      if ("log2fc" %in% names(df_show)) dt <- DT::formatRound(dt, "log2fc", digits = 3)
      dt
    })

    output$summary_bar <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "请先创建任务并运行分析。"))
      }

      snapshot <- job_snapshot()
      report_paths <- snapshot$report_paths %||% state$report_paths %||% list()
      html_ready <- !is.null(report_paths$html) && file.exists(report_paths$html)
      all_status <- unlist(workflow_step_status_snapshot(state = state), use.names = FALSE)
      overall <- if (identical(state$status %||% "", "running_full_workflow")) {
        "running"
      } else if (any(all_status == "failed")) {
        "failed"
      } else if (all(all_status %in% c("done", "warning", "skipped"))) {
        if (any(all_status == "warning")) "warning" else "done"
      } else if (any(all_status %in% c("done", "warning", "skipped"))) {
        "warning"
      } else {
        "waiting"
      }

      current_step_id <- state$current_step %||% (snapshot$current_step %||% NULL)

      shiny::tags$div(
        class = "kkai-results-summary kkai-results-summary-bar",
        shiny::tags$div(shiny::tags$b("任务 ID"), " ", shiny::tags$code(state$job_id %||% "(none)")),
        shiny::tags$div(shiny::tags$b("任务状态"), " ", status_badge(overall)),
        shiny::tags$div(shiny::tags$b("当前步骤"), " ", shiny::tags$strong(if (is.null(current_step_id)) "未开始" else step_label_zh(current_step_id))),
        shiny::tags$div(shiny::tags$b("报告"), " ", status_badge(if (html_ready) "done" else step_status("report")))
      )
    })

    output$module_nav <- shiny::renderUI({
      nav_items <- list(
        list(id = "overview", label = "数据概览", status = if (is.null(state$job_dir)) "waiting" else "done"),
        list(id = "alpha", label = "Alpha", status = step_status("alpha")),
        list(id = "beta", label = "Beta", status = step_status("beta")),
        list(id = "diff", label = "差异丰度", status = step_status("diff")),
        list(id = "ml", label = "机器学习", status = step_status("ml")),
        list(id = "network", label = "网络分析", status = step_status("network")),
        list(id = "key_taxa", label = "关键菌评分", status = step_status("key_taxa")),
        list(id = "ai", label = "AI 解读", status = step_status("ai")),
        list(id = "report", label = "报告", status = step_status("report"))
      )

      shiny::tags$div(
        class = "kkai-module-nav",
        lapply(nav_items, function(item) {
          shiny::tags$button(
            type = "button",
            class = nav_class(item$status, active = identical(selected_section(), item$id)),
            onclick = sprintf("window.kkaiSelectResultsSection('%s', '%s');", session$ns("select_section"), item$id),
            item$label
          )
        })
      )
    })

    output$cards <- shiny::renderUI({
      snapshot <- job_snapshot()
      if (is.null(snapshot)) {
        return(
          bslib::card(
            class = "dashboard-card kkai-result-card kkai-result-card--placeholder",
            bslib::card_header("数据概览"),
            shiny::tags$div(class = "kkai-alert kkai-alert--warning", "当前任务快照读取失败，请重新加载历史任务或重新运行分析。")
          )
        )
      }

      report_paths <- snapshot$report_paths %||% state$report_paths %||% list()
      html_ready <- !is.null(report_paths$html) && file.exists(report_paths$html)
      pdf_ready <- !is.null(report_paths$pdf) && file.exists(report_paths$pdf)

      overview <- snapshot$overview %||% list()
      overview_card <- bslib::card(
        class = "dashboard-card kkai-result-card",
        bslib::card_header("数据概览"),
        shiny::tags$div(
          class = "kkai-kv",
          shiny::tags$div(shiny::tags$b("任务目录"), shiny::tags$br(), shiny::tags$code(state$job_dir %||% "(none)")),
          shiny::tags$div(
            shiny::tags$b("数据检查"),
            " ",
            status_badge(
              switch(
                overview$data_check_status %||% "waiting",
                done = "done",
                pass = "done",
                warning = "warning",
                failed = "failed",
                "waiting"
              )
            )
          ),
          if (!is.na(overview$n_samples %||% NA_integer_) || !is.na(overview$n_features %||% NA_integer_)) {
            shiny::tags$div(
              class = "kkai-muted",
              paste0(
                "样本数: ", overview$n_samples %||% "n/a",
                " | 特征数: ", overview$n_features %||% "n/a",
                " | 元数据样本数: ", overview$metadata_samples %||% "n/a"
              )
            )
          } else {
            NULL
          }
        )
      )

      diff_tbl <- snapshot$diff_table %||% data.frame()
      n_sig <- snapshot$diff_summary$n_significant_taxa %||%
        if (is.data.frame(diff_tbl) && "significant" %in% names(diff_tbl)) {
          sum(diff_tbl$significant, na.rm = TRUE)
        } else {
          0L
        }

      alpha_ready <- status_is("alpha", c("done", "warning")) &&
        (length(alpha_available_metrics()) > 0 || is.data.frame(snapshot$alpha_stats))
      alpha_card <- if (isTRUE(alpha_ready)) {
        summary <- if (!is.data.frame(diff_tbl) || nrow(diff_tbl) < 1) {
          "差异丰度结果为空。"
        } else if (n_sig > 0) {
          paste0("检测到 ", n_sig, " 个 FDR < 0.05 的差异特征。")
        } else {
          "未检测到 FDR 显著差异菌，当前展示为探索性结果。"
        }
        result_card(
          "alpha",
          "Alpha 多样性",
          "Observed、Chao1、Shannon、Simpson 指标及组间统计结果已生成。",
          status = step_status("alpha"),
          details = shiny::tagList(
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::uiOutput(session$ns("alpha_metric_control")),
              shiny::uiOutput(session$ns("alpha_plot_type_control"))
            ),
            shiny::uiOutput(session$ns("alpha_dynamic_plot"))
          )
        )
      } else {
        NULL
      }

      beta_ready <- status_is("beta", c("done", "warning")) &&
        (length(beta_available_views()) > 0 || is.data.frame(snapshot$beta_permanova))
      beta_card <- if (isTRUE(beta_ready)) {
        result_card(
          "beta",
          "Beta 多样性",
          "论文级 PCoA、PERMANOVA 与 PERMDISP 诊断已生成。",
          status = step_status("beta"),
          details = shiny::tagList(
            shiny::uiOutput(session$ns("beta_view_control")),
            shiny::uiOutput(session$ns("beta_dynamic_plot"))
          )
        )
      } else {
        NULL
      }

      diff_card <- if (status_is("diff", c("done", "warning")) && (is.list(snapshot$diff_summary) || is.data.frame(snapshot$diff_table))) {
        diff_tbl <- snapshot$diff_table %||% data.frame()
        n_sig <- snapshot$diff_summary$n_significant_taxa %||% if (is.data.frame(diff_tbl) && "significant" %in% names(diff_tbl)) sum(diff_tbl$significant, na.rm = TRUE) else 0L
        summary <- if (!is.data.frame(diff_tbl) || nrow(diff_tbl) < 1) {
          "差异丰度结果为空。"
        } else if (n_sig > 0) {
          paste0("检测到 ", n_sig, " 个 FDR < 0.05 的差异特征。")
        } else {
          "未检测到显著差异，但已保留探索性结果。"
        }
        result_card(
          "diff",
          "差异丰度",
          summary,
          img_file = "diff_taxa_barplot.png",
          status = step_status("diff"),
          details = shiny::tagList(
            shiny::uiOutput(session$ns("diff_overview_status")),
            DT::DTOutput(session$ns("diff_overview_table"))
          )
        )
      } else {
        NULL
      }

      ml_card <- if (status_is("ml", c("done", "warning")) && is.list(snapshot$ml_summary)) {
        reliability <- snapshot$ml_summary$reliability %||% ""
        summary <- if (identical(reliability, "exploratory only")) {
          "样本量较小，机器学习结果仅供探索性参考。"
        } else if (identical(reliability, "caution")) {
          "机器学习结果已生成，建议结合样本量谨慎解释。"
        } else {
          "Random Forest 特征筛选结果已生成。"
        }
        result_card(
          "ml",
          "机器学习",
          summary,
          img_file = "ml_importance.png",
          status = step_status("ml")
        )
      } else {
        NULL
      }

      network_card <- if (identical(step_status("network"), "running")) {
        placeholder_card("network", "网络分析", "network", "网络分析正在运行")
      } else if (status_is("network", c("done", "warning")) && is.list(snapshot$network_summary)) {
        n_nodes <- snapshot$network_summary$n_nodes %||% "n/a"
        n_edges <- snapshot$network_summary$n_edges %||% "n/a"
        result_card(
          "network",
          "网络分析",
          paste0("核心网络已生成，当前保存 ", n_nodes, " 个节点和 ", n_edges, " 条边。"),
          img_file = "network_plot.png",
          status = step_status("network")
        )
      } else {
        NULL
      }

      key_taxa_card <- if (status_is("key_taxa", c("done", "warning")) && is.list(snapshot$key_taxa_summary)) {
        used_sources <- snapshot$key_taxa_summary$used_sources %||% character(0)
        summary <- if (length(used_sources) > 0) {
          paste0("已整合证据来源: ", paste(used_sources, collapse = ", "), "。")
        } else {
          "关键菌评分结果已生成。"
        }
        result_card(
          "key_taxa",
          "关键菌评分",
          summary,
          img_file = "key_taxa_score_barplot.png",
          status = step_status("key_taxa")
        )
      } else {
        NULL
      }

      ai_card <- if (status_is("ai", c("done", "warning", "skipped")) && (isTRUE(snapshot$ai$local_exists) || isTRUE(snapshot$ai$llm_exists))) {
        ai_message <- if (isTRUE(snapshot$ai$local_exists) && isTRUE(snapshot$ai$llm_exists)) {
          "本地 AI 说明与 LLM 扩展解读均已生成。"
        } else if (isTRUE(snapshot$ai$local_exists)) {
          "已生成本地 AI 说明；未发现 LLM 扩展解读。"
        } else {
          "AI 解读产物已生成。"
        }
        result_card(
          "ai",
          "AI 解读",
          ai_message,
          status = step_status("ai")
        )
      } else {
        NULL
      }

      report_card <- if (status_is("report", c("done", "warning")) && !is.null(report_paths$html)) {
        result_card(
          "report",
          "报告",
          if (html_ready && pdf_ready) {
            "HTML 和 PDF 报告均已生成。"
          } else if (html_ready) {
            "HTML 报告已生成，PDF 导出未完成。"
          } else {
            "报告步骤已完成，正在同步最终文件。"
          },
          status = step_status("report"),
          details = shiny::tags$div(
            class = "kkai-kv",
            if (!is.null(report_paths$html)) shiny::tags$div(shiny::tags$b("HTML"), shiny::tags$br(), shiny::tags$code(report_paths$html)) else NULL,
            if (!is.null(report_paths$pdf)) shiny::tags$div(shiny::tags$b("PDF"), shiny::tags$br(), shiny::tags$code(report_paths$pdf)) else NULL
          )
        )
      } else {
        NULL
      }

      cards_by_id <- list(
        overview = overview_card,
        alpha = alpha_card %||% placeholder_card("alpha", "Alpha 多样性", "alpha"),
        beta = beta_card %||% placeholder_card("beta", "Beta 多样性", "beta"),
        diff = diff_card %||% placeholder_card("diff", "差异丰度", "diff"),
        ml = ml_card %||% placeholder_card("ml", "机器学习", "ml"),
        network = network_card %||% placeholder_card("network", "网络分析", "network"),
        key_taxa = key_taxa_card %||% placeholder_card("key_taxa", "关键菌评分", "key_taxa"),
        ai = ai_card %||% placeholder_card("ai", "AI 解读", "ai"),
        report = report_card %||% placeholder_card("report", "报告", "report")
      )

      current_section <- selected_section() %||% "overview"
      active_card <- cards_by_id[[current_section]] %||% overview_card

      shiny::tags$div(
        class = "kkai-results-panel",
        active_card
      )
    })
  })
}
