# 数据检查模块：读取当前任务输入并进行校验。

mod_data_check_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("数据检查"),
    shiny::p("数据校验是必需步骤，结果会保存到任务目录中的 data_check_summary.csv。"),
    shiny::actionButton(ns("run_check"), "开始检查", class = "btn-primary"),
    shiny::hr(),
    shiny::uiOutput(ns("status_ui")),
    DT::DTOutput(ns("check_table"))
  )
}

mod_data_check_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$status_ui <- shiny::renderUI({
      if (is.null(state$check_result)) return(NULL)
      st <- state$check_result$status
      cls <- switch(st, pass = "alert-success", warning = "alert-warning", error = "alert-danger", "alert-secondary")
      shiny::div(class = paste("alert", cls), paste0("总体状态：", st))
    })

    output$check_table <- DT::renderDT({
      if (is.null(state$check_result)) return(NULL)
      DT::datatable(state$check_result$checks, options = list(pageLength = 15))
    })

    shiny::observeEvent(input$run_check, {
      req(state$job_dir, state$input_paths)

      inputs <- read_microbiome_inputs(
        abundance_path = state$input_paths$abundance_path,
        metadata_path = state$input_paths$metadata_path,
        taxonomy_path = state$input_paths$taxonomy_path
      )
      state$input_data <- inputs

      group_var <- state$parameters$group_var %||% NULL
      check_res <- run_all_data_checks(inputs, group_var = group_var)
      state$check_result <- check_res
      workflow_set_status(state, "data_checked")

      save_data_check_summary(check_res, state$job_dir)
      db_upsert_job(state$job_id, state$job_dir, status = paste0("data_check_", check_res$status))

      shiny::showNotification(paste0("数据检查完成（", check_res$status, "）。结果已保存。"), type = "message")
    })
  })
}
