# Phase 0 placeholder: AI interpretation comes in Phase 4+.

mod_ai_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("AI 解释"),
    shiny::p("AI 输入使用 JSON，AI 输出使用 Markdown，且不直接读取原始丰度表。"),
    shiny::verbatimTextOutput(ns("info"))
  )
}

mod_ai_report_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$info <- shiny::renderText({
      if (is.null(state$job_dir)) return("当前没有活动任务。")
      paste0("AI 输出将保存到：", file.path(state$job_dir, "ai"))
    })
  })
}
