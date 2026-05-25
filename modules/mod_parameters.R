# 参数设置模块：从样本信息表中选择分组变量。

mod_parameters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("参数设置"),
    shiny::p("从样本信息表中选择分组变量，并写入可复现记录。"),
    shiny::uiOutput(ns("group_var_ui")),
    shiny::actionButton(ns("save_params"), "保存参数", class = "btn-primary"),
    shiny::hr(),
    shiny::verbatimTextOutput(ns("params_preview"))
  )
}

mod_parameters_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$group_var_ui <- shiny::renderUI({
      if (is.null(state$input_paths)) {
        return(shiny::helpText("请先上传数据。"))
      }
      md <- tryCatch(read_table_auto(state$input_paths$metadata_path), error = function(e) NULL)
      if (is.null(md) || !"SampleID" %in% names(md)) {
        return(shiny::helpText("样本信息表暂不可读，或缺少 SampleID。请先修正输入并重新执行数据检查。"))
      }
      choices <- setdiff(names(md), "SampleID")
      shiny::selectInput(
        ns("group_var"),
        "分组变量",
        choices = choices,
        selected = state$parameters$group_var %||% (if (length(choices) > 0) choices[[1]] else NULL)
      )
    })

    output$params_preview <- shiny::renderText({
      if (is.null(state$parameters)) return("尚未保存参数。")
      jsonlite::toJSON(state$parameters, auto_unbox = TRUE, pretty = TRUE)
    })

    shiny::observeEvent(input$save_params, {
      req(state$job_dir)
      group_var <- input$group_var
      if (is.null(group_var) || !nzchar(group_var)) {
        shiny::showNotification("请先选择分组变量。", type = "error")
        return()
      }

      params <- modifyList(state$parameters %||% list(), list(group_var = group_var))
      state$parameters <- params
      append_reproducibility(state$job_dir, list(parameters = params))
      db_upsert_job(state$job_id, state$job_dir, status = "parameters_saved")
      shiny::showNotification("参数已保存。", type = "message")
    })
  })
}
