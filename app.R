#!/usr/bin/env Rscript

# Microbiome Key Taxa AI (Shiny)
# UI wiring + module composition only.

source("global.R", local = TRUE)

ui <- bslib::page_navbar(
  title = "Microbiome Key Taxa AI",
  id = "main_nav",
  theme = bslib::bs_theme(
    version = 5,
    bg = "#f3f4f6",
    fg = "#111827",
    primary = "#2563eb",
    base_font = bslib::font_google("IBM Plex Sans")
  ),
  header = shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
  ),

  bslib::nav_panel("首页", value = "home", mod_home_ui("home")),
  bslib::nav_panel("快速开始", value = "quick_start", mod_quick_start_ui("quick_start")),
  bslib::nav_panel("示例模式", value = "demo", mod_demo_ui("demo")),
  bslib::nav_panel(
    "结果总览",
    value = "results",
    shiny::tagList(
      shiny::uiOutput("active_job_banner"),
      mod_results_overview_ui("results")
    )
  ),
  bslib::nav_panel("报告", value = "report", mod_report_ui("report")),
  bslib::nav_panel("历史任务", value = "job_history", mod_job_history_ui("job_history")),
  bslib::nav_menu(
    "更多",
    bslib::nav_panel("上传数据", value = "upload", mod_upload_ui("upload")),
    bslib::nav_panel("数据检查", value = "data_check", mod_data_check_ui("data_check")),
    bslib::nav_panel("参数设置", value = "params", mod_parameters_ui("params")),
    bslib::nav_panel("运行分析", value = "run_analysis", mod_run_analysis_ui("run_analysis")),
    bslib::nav_panel("Alpha 多样性", value = "alpha", mod_alpha_ui("alpha")),
    bslib::nav_panel("Beta 多样性", value = "beta", mod_beta_ui("beta")),
    bslib::nav_panel("差异丰度", value = "diff", mod_diff_ui("diff")),
    bslib::nav_panel("AI 解释", value = "ai_interp", mod_ai_interpretation_ui("ai_interp")),
    bslib::nav_panel("机器学习", value = "ml", mod_ml_ui("ml")),
    bslib::nav_panel("网络分析", value = "network", mod_network_ui("network")),
    bslib::nav_panel("关键菌评分", value = "key_taxa", mod_key_taxa_ui("key_taxa"))
  )
)

server <- function(input, output, session) {
  analysis_state <- create_analysis_state()

  output$active_job_banner <- shiny::renderUI({
    job_id <- analysis_state$active_job_id %||% analysis_state$job_id
    job_dir <- analysis_state$active_job_dir %||% analysis_state$job_dir
    src <- analysis_state$active_source %||% ""

    if (is.null(job_dir) || !nzchar(job_dir)) {
      return(
        shiny::tags$div(
          class = "kkai-alert kkai-alert--info",
          "请先在快速开始运行分析，或在历史任务中加载一个任务。"
        )
      )
    }

    shiny::tags$div(
      class = "kkai-alert kkai-alert--success",
      shiny::tags$div(shiny::tags$b("当前任务 ID："), " ", shiny::tags$code(job_id %||% "(none)")),
      shiny::tags$div(shiny::tags$b("当前任务目录："), " ", shiny::tags$code(normalizePath(job_dir, winslash = "/", mustWork = FALSE))),
      shiny::tags$div(shiny::tags$b("数据来源："), " ", src %||% "")
    )
  })

  mod_home_server("home", state = analysis_state)
  mod_demo_server("demo", state = analysis_state)
  mod_quick_start_server("quick_start", state = analysis_state)
  mod_results_overview_server("results", state = analysis_state)
  mod_upload_server("upload", state = analysis_state)
  mod_data_check_server("data_check", state = analysis_state)
  mod_parameters_server("params", state = analysis_state)
  mod_run_analysis_server("run_analysis", state = analysis_state)
  mod_alpha_server("alpha", state = analysis_state)
  mod_beta_server("beta", state = analysis_state)
  mod_diff_server("diff", state = analysis_state)
  mod_ai_interpretation_server("ai_interp", state = analysis_state)
  mod_ml_server("ml", state = analysis_state)
  mod_network_server("network", state = analysis_state)
  mod_key_taxa_server("key_taxa", state = analysis_state)
  mod_report_server("report", state = analysis_state)
  mod_job_history_server("job_history", state = analysis_state)
}

shiny::shinyApp(ui, server)
