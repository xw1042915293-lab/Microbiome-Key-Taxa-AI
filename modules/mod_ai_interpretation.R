# AI 解释页：读取当前任务目录下的 AI Markdown 结果。

mod_ai_interpretation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("AI 解释"),
    shiny::p("仅展示当前任务目录中已生成的 AI 结果。"),
    shiny::uiOutput(ns("diff_md")),
    shiny::tags$div(style = "margin-top: 1rem;"),
    shiny::uiOutput(ns("llm_md")),
    shiny::tags$details(
      shiny::tags$summary("详情"),
      shiny::tags$p("当 API 不可用时，会使用本地解释或跳过在线 LLM 调用。")
    )
  )
}

mod_ai_interpretation_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$diff_md <- shiny::renderUI({
      if (!isTRUE(get_cfg("app.ai_enabled", FALSE))) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("AI ????????"),
            shiny::tags$div("????????? config.yml ?? app.ai_enabled ?? true?")
          )
        )
      }
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务，请先运行分析。"))
      }

      path <- file.path(state$job_dir, "ai", "diff_interpretation.md")
      if (!file.exists(path)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("未找到 diff_interpretation.md。"),
            shiny::tags$div("请先生成工作流输出。")
          )
        )
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("AI 约束解释"),
        shiny::includeMarkdown(path)
      )
    })

    output$llm_md <- shiny::renderUI({
      if (!isTRUE(get_cfg("app.ai_enabled", FALSE))) return(NULL)
      if (is.null(state$job_dir)) return(NULL)

      path <- file.path(state$job_dir, "ai", "llm_diff_interpretation.md")
      if (!file.exists(path)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("当前任务没有可用的 LLM 解释。"),
            shiny::tags$div("可选的 LLM 步骤可能已跳过。")
          )
        )
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("LLM 解释（可选）"),
        shiny::includeMarkdown(path)
      )
    })
  })
}
