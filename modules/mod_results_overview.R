# 结果总览：以仪表盘方式汇总已有分析输出。

mod_results_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-page-header",
        shiny::tags$h2("结果总览"),
        shiny::tags$p("集中查看工作流各阶段已生成的结果。")
      ),
      shiny::uiOutput(ns("summary_bar")),
      shiny::tags$div(
        class = "kkai-results-grid",
        shiny::uiOutput(ns("alpha_card")),
        shiny::uiOutput(ns("beta_card")),
        shiny::uiOutput(ns("diff_card")),
        shiny::uiOutput(ns("ai_card")),
        shiny::uiOutput(ns("ml_card")),
        shiny::uiOutput(ns("network_card")),
        shiny::uiOutput(ns("key_taxa_card"))
      )
    )
  )
}

mod_results_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    report_path <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      file.path(state$job_dir, "report", "report.html")
    })

    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_figures")
    })

    safe_add_resource_path <- function(prefix, directoryPath) {
      if (is.null(prefix) || !nzchar(prefix)) return(invisible(FALSE))
      rp <- tryCatch(shiny::resourcePaths(), error = function(e) NULL)
      if (is.list(rp) && !is.null(rp[[prefix]])) return(invisible(FALSE))
      shiny::addResourcePath(prefix = prefix, directoryPath = directoryPath)
      invisible(TRUE)
    }

    shiny::observeEvent(state$job_dir, {
      shiny::req(state$job_dir, fig_prefix())
      figs_dir <- file.path(state$job_dir, "figures")
      if (dir.exists(figs_dir)) safe_add_resource_path(fig_prefix(), figs_dir)
    }, ignoreInit = TRUE)

    .status_badge <- function(ok, label = NULL) {
      ui_status_badge(label %||% if (isTRUE(ok)) "ready" else "missing", kind = if (isTRUE(ok)) "success" else "warning")
    }

    .img_card <- function(title, img_file, summary, details_ui = NULL) {
      if (is.null(state$job_dir)) {
        return(
          bslib::card(
            class = "kkai-card kkai-card--quick",
            bslib::card_header(title),
            shiny::tags$div(class = "kkai-alert kkai-alert--info", "No active job yet.")
          )
        )
      }
      full <- file.path(state$job_dir, "figures", img_file)
      has_img <- file.exists(full)
      bslib::card(
        class = "dashboard-card kkai-result-card",
        bslib::card_header(title),
        shiny::tags$div(class = "kkai-result-summary", summary),
        if (isTRUE(has_img)) {
          shiny::tags$div(class = "kkai-result-image-wrap",
            shiny::tags$img(src = paste0(fig_prefix(), "/", img_file), class = "kkai-result-img kkai-result-img--fit")
          )
        } else {
          shiny::tags$div(class = "kkai-alert kkai-alert--warning", "Figure not found yet.")
        },
        details_ui
      )
    }

    output$summary_bar <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "请先在快速开始中创建任务并生成结果。"))
      }

      rp <- report_path()
      report_exists <- !is.null(rp) && file.exists(rp)
      shiny::tags$div(
        class = "kkai-results-summary",
        shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code(state$job_id %||% "(none)")),
        shiny::tags$div(shiny::tags$b("任务状态："), " ", .status_badge(TRUE, "active")),
        shiny::tags$div(shiny::tags$b("报告："), " ", .status_badge(report_exists, if (report_exists) "available" else "missing"))
      )
    })

    output$alpha_card <- shiny::renderUI({
      .img_card(
        "Alpha 多样性",
        "alpha_shannon_boxplot.png",
        "Shannon diversity summary for the selected group variable.",
        shiny::tags$details(
          shiny::tags$summary("更多"),
          shiny::tags$div(
            class = "kkai-details-grid",
            shiny::tags$div(
              shiny::tags$b("详情："),
              shiny::tags$div(shiny::tags$a(href = "#", "可在“更多”中打开 Alpha 多样性页面。"))
            )
          )
        )
      )
    })

    output$beta_card <- shiny::renderUI({
      .img_card(
        "Beta 多样性",
        "beta_pcoa_bray.png",
        "Bray-Curtis ordination overview.",
        shiny::tags$details(shiny::tags$summary("更多"), shiny::tags$div("可在“更多”中打开 Beta 多样性页面。"))
      )
    })

    output$diff_card <- shiny::renderUI({
      .img_card(
        "差异丰度",
        "diff_volcano.png",
        "Taxa-level differential signal summary.",
        shiny::tags$details(shiny::tags$summary("更多"), shiny::tags$div("可在“更多”中打开差异丰度页面。"))
      )
    })

    output$ai_card <- shiny::renderUI({
      .img_card(
        "AI 解释",
        "diff_volcano.png",
        "Interpretation artifacts and fallback notes.",
        shiny::tags$details(shiny::tags$summary("更多"), shiny::tags$div("可在“更多”中打开 AI 解释页面。"))
      )
    })

    output$ml_card <- shiny::renderUI({
      .img_card(
        "机器学习",
        "ml_importance.png",
        "Random Forest screening summary.",
        shiny::tags$details(shiny::tags$summary("更多"), shiny::tags$div("可在“更多”中打开机器学习页面。"))
      )
    })

    output$network_card <- shiny::renderUI({
      .img_card(
        "网络分析",
        "network_plot.png",
        "Exploratory co-occurrence network summary.",
        shiny::tags$details(shiny::tags$summary("更多"), shiny::tags$div("可在“更多”中打开网络分析页面。"))
      )
    })

    output$key_taxa_card <- shiny::renderUI({
      .img_card(
        "关键菌评分",
        "key_taxa_score_barplot.png",
        "Integrated ranking across differential, ML, and network evidence.",
        shiny::tags$details(shiny::tags$summary("更多"), shiny::tags$div("可在“更多”中打开关键菌评分页面。"))
      )
    })
  })
}
