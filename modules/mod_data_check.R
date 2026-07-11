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
      shiny::div(class = paste("alert", cls), paste0("整体状态：", st))
    })

    output$check_table <- DT::renderDT({
      if (is.null(state$check_result)) return(NULL)
      DT::datatable(state$check_result$checks, options = list(pageLength = 15))
    })

    shiny::observeEvent(input$run_check, {
      req(state$job_dir, state$input_paths)

      tryCatch({
        inputs <- read_microbiome_inputs(
          abundance_path = state$input_paths$abundance_path,
          metadata_path = state$input_paths$metadata_path,
          taxonomy_path = state$input_paths$taxonomy_path
        )
        state$input_data <- inputs

        group_var <- state$parameters$group_var %||% NULL

        set_step_status(state, "data_check", "running", "正在检查输入数据")
        check_res <- run_all_data_checks(inputs, group_var = group_var)
        state$check_result <- check_res

        if (check_res$status == "error") {
          set_step_status(state, "data_check", "failed", "数据检查发现错误")
        } else if (check_res$status == "warning") {
          set_step_status(state, "data_check", "warning", "数据检查完成，存在警告")
        } else {
          set_step_status(state, "data_check", "done", "数据检查完成")
        }

        save_data_check_summary(check_res, state$job_dir)
        db_upsert_job(state$job_id, state$job_dir, status = paste0("data_check_", check_res$status))

        if (check_res$status != "error") {
          set_step_status(state, "build_dataset", "running", "正在构建 microeco 对象")
          dataset <- build_microeco_dataset(
            abundance = state$input_data$abundance,
            metadata = state$input_data$metadata,
            taxonomy = state$input_data$taxonomy
          )
          state$dataset <- dataset
          save_microeco_dataset(dataset, state$job_dir)
          set_step_status(state, "build_dataset", "done", "microeco 对象已构建")
        }

        workflow_set_status(state, "data_checked")
        shiny::showNotification(paste0("数据检查完成（", check_res$status, "）。结果已保存。"), type = "message")
      }, error = function(e) {
        msg <- if (inherits(e, "utf8_input_error") || grepl("invalid UTF-8", conditionMessage(e), ignore.case = TRUE)) {
          "当前输入表包含非 UTF-8 字符，常见原因是 Excel 普通 CSV 使用 GBK 编码。请另存为 CSV UTF-8，或检查 taxonomy / metadata 中的中文和特殊符号。"
        } else {
          paste0("数据检查失败：", conditionMessage(e))
        }
        set_step_status(state, "data_check", "failed", msg)
        shiny::showNotification(msg, type = "error", duration = NULL)
      })
    })
  })
}
