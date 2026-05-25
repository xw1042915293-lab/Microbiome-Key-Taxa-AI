# Alpha 结果页：预览 job_dir/figures 下的 Shannon 图。

mod_alpha_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("Alpha 多样性"),
    shiny::p("展示 Shannon 箱线图，图像已保存到任务目录中。"),
    shiny::tags$div(class = "kkai-alert kkai-alert--info", "该页仅用于查看已有结果。"),
    shiny::uiOutput(ns("plot_ui")),
    shiny::tags$details(
      shiny::tags$summary("详情"),
      shiny::tags$p("结果写入当前任务目录，并会在“结果总览”中汇总展示。")
    )
  )
}

mod_alpha_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    # Serve job_dir/figures via a per-job resource prefix.
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
      png_file <- "alpha_shannon_boxplot.png"
      full <- file.path(state$job_dir, "figures", png_file)
      if (!file.exists(full)) return(shiny::helpText("尚未生成 Alpha 图。"))
      bslib::card(
        class = "kkai-card",
        bslib::card_header("Alpha 多样性图"),
        shiny::tags$div(class = "kkai-result-image-wrap",
          shiny::tags$img(src = file.path(fig_prefix(), png_file), class = "kkai-result-img kkai-result-img--small")
        )
      )
    })

  })
}
