# 差异丰度结果页：火山图与表格预览。

mod_diff_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("差异丰度"),
    shiny::p("展示火山图和显著菌表格，结果已保存到任务目录。"),
    shiny::uiOutput(ns("plot_ui")),
    shiny::h4("显著菌"),
    DT::DTOutput(ns("diff_table")),
    shiny::tags$details(
      shiny::tags$summary("详情"),
      shiny::tags$p("相关文件保存在当前任务目录，“结果总览”会提供更简洁的汇总。")
    )
  )
}

mod_diff_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_figures")
    })

    shiny::observeEvent(state$job_dir, {
      req(state$job_dir, fig_prefix())
      if (dir.exists(file.path(state$job_dir, "figures"))) {
        shiny::addResourcePath(prefix = fig_prefix(), directoryPath = file.path(state$job_dir, "figures"))
      }
    }, ignoreInit = TRUE)

    output$plot_ui <- shiny::renderUI({
      if (is.null(state$job_dir)) return(shiny::helpText("当前没有活动任务。"))
      png_file <- "diff_volcano.png"
      full <- file.path(state$job_dir, "figures", png_file)
      if (!file.exists(full)) return(shiny::helpText("尚未生成火山图。"))
      bslib::card(
        class = "kkai-card",
        bslib::card_header("差异丰度图"),
        shiny::tags$div(class = "kkai-result-image-wrap",
          shiny::tags$img(src = file.path(fig_prefix(), png_file), class = "kkai-result-img kkai-result-img--small")
        )
      )
    })

    output$diff_table <- DT::renderDT({
      if (is.null(state$diff_result) || is.null(state$diff_result$diff_table)) return(NULL)
      sig <- state$diff_result$diff_table[state$diff_result$diff_table$significant, , drop = FALSE]
      if (nrow(sig) == 0) return(DT::datatable(data.frame(Message = "No significant taxa found.")))
      DT::datatable(sig, options = list(pageLength = 10))
    })
  })
}
