# 机器学习结果页：预览当前任务中的 ML 结果。

mod_ml_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("机器学习"),
    shiny::tags$div(
      class = "kkai-alert kkai-alert--warning",
      shiny::tags$b("探索性提示："),
      "小样本机器学习结果仅供探索性参考，不能替代正式验证。"
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    shiny::uiOutput(ns("importance_plot")),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("模型指标"),
      DT::DTOutput(ns("metrics_tbl"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("前 20 个特征重要性"),
      DT::DTOutput(ns("importance_tbl"))
    ),
    shiny::tags$details(
      shiny::tags$summary("详情"),
      shiny::tags$p("该页仅用于查看已有的机器学习结果。")
    )
  )
}

mod_ml_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_figures")
    })

    safe_add_resource_path <- function(prefix, directoryPath) {
      if (is.null(prefix) || !nzchar(prefix)) return(invisible(FALSE))
      rp <- tryCatch(shiny::resourcePaths(), error = function(e) NULL)
      if (is.list(rp) && !is.null(rp[[prefix]])) return(invisible(FALSE))
      shiny::addResourcePath(prefix = prefix, directoryPath = directoryPath)
      invisible(TRUE)
    }

    shiny::observeEvent(state$job_dir, {
      shiny::req(state$job_dir, fig_prefix())
      figs_dir <- file.path(state$job_dir, "figures")
      if (dir.exists(figs_dir)) safe_add_resource_path(fig_prefix(), figs_dir)
    }, ignoreInit = TRUE)

    output$importance_plot <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务，请先运行分析。"))
      }

      png_file <- "ml_importance.png"
      full <- file.path(state$job_dir, "figures", png_file)
      if (!file.exists(full)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("未找到机器学习重要性图。"),
            shiny::tags$div("请先生成工作流输出。")
          )
        )
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("特征重要性图"),
        shiny::tags$div(class = "kkai-result-image-wrap",
          shiny::tags$img(src = file.path(fig_prefix(), png_file), class = "kkai-result-img kkai-result-img--small")
        )
      )
    })

    output$metrics_tbl <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(DT::datatable(data.frame(Message = "当前没有活动任务。"), rownames = FALSE, options = list(dom = "t")))
      }
      p <- file.path(state$job_dir, "tables", "ml_model_metrics.csv")
      if (!file.exists(p)) {
        return(DT::datatable(data.frame(Message = "未找到 tables/ml_model_metrics.csv。"), rownames = FALSE, options = list(dom = "t")))
      }
      df <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df)) {
        return(DT::datatable(data.frame(Message = "读取 ml_model_metrics.csv 失败。"), rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE))
    })

    output$importance_tbl <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(DT::datatable(data.frame(Message = "当前没有活动任务。"), rownames = FALSE, options = list(dom = "t")))
      }
      p <- file.path(state$job_dir, "tables", "ml_feature_importance.csv")
      if (!file.exists(p)) {
        return(DT::datatable(data.frame(Message = "未找到 tables/ml_feature_importance.csv。"), rownames = FALSE, options = list(dom = "t")))
      }
      df <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(DT::datatable(data.frame(Message = "ml_feature_importance.csv 中没有数据。"), rownames = FALSE, options = list(dom = "t")))
      }
      df <- utils::head(df, 20)
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, dom = "tip", autoWidth = TRUE))
    })
  })
}
