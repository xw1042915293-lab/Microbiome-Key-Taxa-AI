# 首页：展示工作流入口与核心模块概览。

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-home-hero",
        shiny::tags$div(
          class = "hero-card",
          shiny::tags$h2("Microbiome Key Taxa AI", class = "kkai-title"),
          shiny::tags$p("面向微生物组下游分析与关键菌筛选的 AI 平台", class = "kkai-subtitle"),
          shiny::tags$div(
            class = "kkai-hero-actions",
            shiny::actionButton(ns("start_analysis"), "开始分析", class = "btn btn-primary primary-button"),
            shiny::actionButton(ns("open_demo"), "打开示例模式", class = "btn btn-outline-primary")
          )
        ),
        shiny::tags$div(class = "kkai-hero-side", shiny::uiOutput(ns("active_job_card")))
      ),

      shiny::tags$div(class = "kkai-section-title", "工作流"),
      shiny::tags$div(
        class = "workflow-grid",
        shiny::tags$div(class = "workflow-step-card", shiny::tags$div(class = "kkai-workflow-kicker", "01"), shiny::tags$div(class = "kkai-workflow-name", "上传数据"), shiny::tags$div(class = "kkai-workflow-desc", "导入丰度表、样本信息与注释表")),
        shiny::tags$div(class = "workflow-step-card", shiny::tags$div(class = "kkai-workflow-kicker", "02"), shiny::tags$div(class = "kkai-workflow-name", "数据检查"), shiny::tags$div(class = "kkai-workflow-desc", "一致性、缺失值与分组变量校验")),
        shiny::tags$div(class = "workflow-step-card", shiny::tags$div(class = "kkai-workflow-kicker", "03"), shiny::tags$div(class = "kkai-workflow-name", "参数设置"), shiny::tags$div(class = "kkai-workflow-desc", "分类层级、距离度量与分组变量")),
        shiny::tags$div(class = "workflow-step-card", shiny::tags$div(class = "kkai-workflow-kicker", "04"), shiny::tags$div(class = "kkai-workflow-name", "运行分析"), shiny::tags$div(class = "kkai-workflow-desc", "一键执行全流程并跟踪进度")),
        shiny::tags$div(class = "workflow-step-card", shiny::tags$div(class = "kkai-workflow-kicker", "05"), shiny::tags$div(class = "kkai-workflow-name", "结果总览"), shiny::tags$div(class = "kkai-workflow-desc", "卡片化查看图表与关键结论")),
        shiny::tags$div(class = "workflow-step-card", shiny::tags$div(class = "kkai-workflow-kicker", "06"), shiny::tags$div(class = "kkai-workflow-name", "生成报告"), shiny::tags$div(class = "kkai-workflow-desc", "导出可复现 HTML 报告与压缩包"))
      ),

      shiny::tags$div(class = "kkai-section-title", "核心模块"),
      shiny::tags$div(
        class = "card-grid-3",
        bslib::card(class = "dashboard-card", shiny::tags$h5("Alpha/Beta 多样性"), shiny::tags$p("多样性指标、PCoA 与基础统计汇总。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card", shiny::tags$h5("差异丰度"), shiny::tags$p("火山图与显著菌筛选。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card", shiny::tags$h5("关键菌评分"), shiny::tags$p("融合差异、机器学习和网络证据的综合排序。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card", shiny::tags$h5("AI 解释"), shiny::tags$p("基于统计与模型输出的约束式解释。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card", shiny::tags$h5("机器学习"), shiny::tags$p("筛选判别特征并输出重要性与性能概览。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card", shiny::tags$h5("网络分析"), shiny::tags$p("共现网络可视化与结构特征汇总。", class = "kkai-muted"))
      )
    )
  )
}

mod_home_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$start_analysis, {
      parent_session <- if (!is.null(session$parent)) session$parent else session
      shiny::updateTabsetPanel(session = parent_session, inputId = "main_nav", selected = "quick_start")
      try(bslib::nav_select(id = "main_nav", selected = "quick_start", session = parent_session), silent = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$open_demo, {
      # Switch the top-level navbar (defined in app.R) from within this module.
      # Use the parent session because the navbar lives outside the module namespace.
      parent_session <- if (!is.null(session$parent)) session$parent else session
      shiny::updateTabsetPanel(session = parent_session, inputId = "main_nav", selected = "demo")
      # Best-effort fallback for bslib navsets (harmless if redundant).
      try(bslib::nav_select(id = "main_nav", selected = "demo", session = parent_session), silent = TRUE)
    }, ignoreInit = TRUE)

    output$active_job_card <- shiny::renderUI({
      if (is.null(state$job_id) || is.null(state$job_dir)) {
        return(
          bslib::card(
            class = "dashboard-card kkai-job-card",
            bslib::card_header("当前任务"),
            shiny::tags$div(class = "kkai-kv", shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code("(none)"))),
            shiny::tags$div(class = "kkai-muted", "从“快速开始”开始创建任务与运行分析。")
          )
        )
      }

      st <- state$status %||% "idle"
      badge_kind <- if (grepl("error|fail", st, ignore.case = TRUE)) {
        "error"
      } else if (grepl("running", st, ignore.case = TRUE) || st %in% c("running_full_workflow", "running")) {
        "warning"
      } else {
        "success"
      }

      bslib::card(
        class = "dashboard-card kkai-job-card",
        bslib::card_header("当前任务"),
        shiny::tags$div(
          class = "kkai-job-top",
          shiny::tags$div(class = "kkai-job-id", shiny::tags$b("任务 ID："), " ", shiny::tags$code(state$job_id)),
          ui_status_badge(st, kind = badge_kind)
        ),
        shiny::tags$div(
          class = "kkai-job-actions",
          shiny::actionButton(session$ns("open_results"), "打开结果总览", class = "btn btn-outline-primary"),
          shiny::actionButton(session$ns("open_report"), "打开报告", class = "btn btn-outline-dark")
        ),
        shiny::tags$details(
          shiny::tags$summary("开发者信息"),
          shiny::tags$div(
            class = "kkai-details-grid",
            shiny::tags$div(shiny::tags$b("任务目录：")),
            shiny::tags$div(class = "kkai-codeblock", normalizePath(state$job_dir, winslash = "/", mustWork = FALSE))
          )
        )
      )
    })

    shiny::observeEvent(input$open_results, {
      parent_session <- if (!is.null(session$parent)) session$parent else session
      shiny::updateTabsetPanel(session = parent_session, inputId = "main_nav", selected = "results")
      try(bslib::nav_select(id = "main_nav", selected = "results", session = parent_session), silent = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$open_report, {
      parent_session <- if (!is.null(session$parent)) session$parent else session
      shiny::updateTabsetPanel(session = parent_session, inputId = "main_nav", selected = "report")
      try(bslib::nav_select(id = "main_nav", selected = "report", session = parent_session), silent = TRUE)
    }, ignoreInit = TRUE)
  })
}
