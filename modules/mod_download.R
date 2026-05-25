# Phase 0 placeholder: download bundling comes later.

mod_download_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("下载"),
    shiny::p("此页提供报告和结果压缩包下载链接。"),
    shiny::verbatimTextOutput(ns("info"))
  )
}

mod_download_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$info <- shiny::renderText({
      if (is.null(state$job_dir)) return("当前没有活动任务。")
      paste0("结果根目录：", state$job_dir)
    })
  })
}
