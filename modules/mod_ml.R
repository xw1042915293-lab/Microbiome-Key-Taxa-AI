# Publication-grade machine-learning analysis and result viewer.

mod_ml_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("机器学习"),
    shiny::uiOutput(ns("status_ui")),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "分析参数",
        shiny::uiOutput(ns("group_var_ui")),
        shiny::uiOutput(ns("tax_level_ui")),
        shiny::sliderInput(ns("min_prevalence"), "最低检出率", min = 0, max = 1, value = 0.20, step = 0.05),
        shiny::numericInput(ns("min_mean_abundance"), "最低平均相对丰度 (%)", value = 0.01, min = 0, step = 0.01),
        shiny::selectInput(
          ns("transformation"), "数据变换",
          choices = c("CLR transformation" = "clr", "Relative abundance" = "relative", "log10(relative abundance + pseudocount)" = "log10"),
          selected = "clr"
        ),
        shiny::numericInput(ns("pseudocount"), "Pseudocount", value = 1e-06, min = 1e-12, step = 1e-06),
        shiny::numericInput(ns("folds"), "交叉验证 folds", value = 3, min = 3, max = 10, step = 1),
        shiny::numericInput(ns("repeats"), "交叉验证重复次数", value = 3, min = 1, max = 100, step = 1),
        shiny::numericInput(ns("permutations"), "置换检验次数", value = 19, min = 0, max = 9999, step = 1),
        shiny::numericInput(ns("top_n"), "Top taxa 数量", value = 15, min = 5, max = 50, step = 1),
        shiny::numericInput(ns("seed"), "随机种子", value = 1234, min = 1, step = 1),
        shiny::uiOutput(ns("run_button_ui")),
        shiny::tags$p("当前为快速探索默认值（3-fold × 3 repeats，19 次置换）。论文最终分析建议改为 5 × 20 和 999 次置换。", class = "kkai-muted")
      ),
      bslib::navset_card_tab(
        bslib::nav_panel("数据概况", DT::DTOutput(ns("sample_summary_tbl"))),
        bslib::nav_panel(
          "模型性能",
          DT::DTOutput(ns("metrics_tbl")),
          shiny::tags$hr(),
          shiny::uiOutput(ns("performance_plot"))
        ),
        bslib::nav_panel(
          "ROC / PR",
          bslib::layout_columns(shiny::uiOutput(ns("roc_plot")), shiny::uiOutput(ns("pr_plot")), col_widths = c(6, 6))
        ),
        bslib::nav_panel("混淆矩阵", shiny::uiOutput(ns("confusion_plot")), DT::DTOutput(ns("confusion_tbl"))),
        bslib::nav_panel("特征重要性", shiny::uiOutput(ns("importance_plot")), DT::DTOutput(ns("importance_tbl"))),
        bslib::nav_panel("关键菌丰度", shiny::uiOutput(ns("top_taxa_plot")), DT::DTOutput(ns("top_taxa_tbl"))),
        bslib::nav_panel("方法说明", shiny::verbatimTextOutput(ns("methods_text")), shiny::verbatimTextOutput(ns("results_text"))),
        bslib::nav_panel(
          "结果下载",
          shiny::tags$p("下载包包含 OOF 预测、交叉验证指标、稳定性重要性、置换检验、支持性统计、Methods/Results 及全部论文图。"),
          shiny::downloadButton(ns("download_all"), "下载全部机器学习结果 (.zip)", class = "btn-primary"),
          shiny::tags$span(style = "margin-left: .5rem;"),
          shiny::downloadButton(ns("download_combined_pdf"), "论文组合图 PDF"),
          shiny::downloadButton(ns("download_combined_tiff"), "论文组合图 TIFF 600 dpi")
        )
      )
    )
  )
}

mod_ml_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    running <- shiny::reactiveVal(FALSE)
    local_error <- shiny::reactiveVal(NULL)
    run_counter <- shiny::reactiveVal(0L)

    available_metadata <- shiny::reactive({
      if (!is.null(state$dataset$sample_table) && is.data.frame(state$dataset$sample_table)) {
        return(state$dataset$sample_table)
      }
      if (!is.null(state$input_data$metadata) && is.data.frame(state$input_data$metadata)) {
        return(state$input_data$metadata)
      }
      NULL
    })

    output$group_var_ui <- shiny::renderUI({
      md <- available_metadata()
      if (is.null(md)) return(shiny::helpText("请先上传并构建分析数据。"))
      choices <- setdiff(names(md), "SampleID")
      shiny::selectInput(
        session$ns("group_var"), "分组变量", choices = choices,
        selected = state$parameters$group_var %||% (if (length(choices)) choices[1L] else NULL)
      )
    })

    output$tax_level_ui <- shiny::renderUI({
      tax <- state$dataset$tax_table
      choices <- if (is.data.frame(tax)) intersect(c("Phylum", "Class", "Order", "Family", "Genus", "Species"), names(tax)) else character()
      if (!length(choices)) choices <- c("Genus")
      shiny::selectInput(
        session$ns("tax_level"), "分类层级", choices = choices,
        selected = state$parameters$tax_level %||% if ("Genus" %in% choices) "Genus" else choices[1L]
      )
    })

    output$run_button_ui <- shiny::renderUI({
      shiny::actionButton(
        session$ns("run_ml"),
        if (running()) "分析运行中…" else "运行机器学习分析",
        class = "btn-primary", disabled = if (running()) "disabled" else NULL
      )
    })

    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_ml_figures")
    })
    safe_add_resource_path <- function(prefix, directory) {
      if (is.null(prefix) || !nzchar(prefix) || !dir.exists(directory)) return(invisible(FALSE))
      existing <- tryCatch(shiny::resourcePaths(), error = function(e) list())
      if (prefix %in% names(existing)) shiny::removeResourcePath(prefix)
      shiny::addResourcePath(prefix, directory)
      invisible(TRUE)
    }
    shiny::observe({
      shiny::req(state$job_dir, fig_prefix())
      run_counter()
      safe_add_resource_path(fig_prefix(), file.path(state$job_dir, "figures"))
    })

    shiny::observeEvent(input$run_ml, {
      if (isTRUE(running())) return()
      if (is.null(state$dataset) || is.null(state$job_dir)) {
        local_error("尚未生成可用的 microeco 数据对象。请先完成上传、数据检查并运行基础分析。")
        return()
      }
      running(TRUE)
      local_error(NULL)
      on.exit(running(FALSE), add = TRUE)
      stage_names <- c("数据校验", "预处理", "交叉验证", "模型训练", "性能评估", "特征重要性", "置换检验", "保存表格", "绘图", "完成")
      result <- tryCatch(
        shiny::withProgress(message = "机器学习分析", value = 0, {
          progress_cb <- function(stage, detail = NULL) {
            idx <- match(stage, stage_names)
            if (is.na(idx)) idx <- 1L
            shiny::incProgress(1 / length(stage_names), detail = paste(c(stage, detail), collapse = "："))
          }
          run_ml_analysis(
            dataset = state$dataset,
            group_var = input$group_var,
            tax_level = input$tax_level,
            job_dir = state$job_dir,
            min_prevalence = input$min_prevalence,
            min_mean_abundance = input$min_mean_abundance / 100,
            transformation = input$transformation,
            pseudocount = input$pseudocount,
            folds = input$folds,
            repeats = input$repeats,
            permutations = input$permutations,
            top_n = input$top_n,
            seed = input$seed,
            progress = progress_cb
          )
        }),
        error = identity
      )
      if (inherits(result, "error")) {
        msg <- conditionMessage(result)
        local_error(paste0("机器学习分析失败：", msg, " 请检查分组样本量、过滤阈值和输入数据后重试。"))
        try(workflow_set_step(state, NULL, file.path(state$job_dir, "logs", "run.log"), "ml", "failed", msg), silent = TRUE)
        return()
      }
      state$ml_result <- result
      state$parameters <- modifyList(state$parameters %||% list(), list(
        group_var = input$group_var, tax_level = input$tax_level,
        ml_min_prevalence = input$min_prevalence,
        ml_min_mean_abundance = input$min_mean_abundance / 100,
        ml_transformation = input$transformation, ml_pseudocount = input$pseudocount,
        ml_folds = input$folds, ml_repeats = input$repeats,
        ml_permutations = input$permutations, ml_top_n = input$top_n, ml_seed = input$seed
      ))
      append_reproducibility(state$job_dir, list(parameters = state$parameters))
      status <- if (identical(result$performance_status, "better_than_permuted_labels") && !identical(result$reliability, "exploratory only")) "done" else "warning"
      message <- if (status == "done") "机器学习分析完成，性能显著优于置换标签。" else "机器学习分析完成，但结果需作为探索性证据谨慎解释。"
      try(workflow_set_step(state, NULL, file.path(state$job_dir, "logs", "run.log"), "ml", status, message), silent = TRUE)
      run_counter(run_counter() + 1L)
      shiny::showNotification(message, type = if (status == "done") "message" else "warning", duration = 8)
    }, ignoreInit = TRUE)

    ml_summary <- shiny::reactive({
      run_counter()
      if (is.null(state$job_dir)) return(NULL)
      p <- file.path(state$job_dir, "json", "ml_summary.json")
      if (!file.exists(p)) return(NULL)
      tryCatch(jsonlite::read_json(p, simplifyVector = TRUE), error = function(e) NULL)
    })

    output$status_ui <- shiny::renderUI({
      if (!is.null(local_error())) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--error", shiny::tags$b("错误："), local_error()))
      }
      if (running()) return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "正在运行机器学习分析，请勿重复提交。"))
      summary <- ml_summary()
      if (is.null(summary)) return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "尚无机器学习结果。请设置参数并运行分析。"))
      small <- identical(summary$reliability %||% "", "exploratory only")
      weak <- identical(summary$performance_status %||% "", "not_stably_better_than_random")
      if (small || weak) {
        text <- if (weak) "当前模型未显示出稳定且显著优于随机分类的预测能力，不建议据此筛选生物标志物。" else "当前样本量较小，模型结果应视为探索性结果，并建议通过独立数据集或扩增样本进行验证。"
        return(shiny::tags$div(class = "kkai-alert kkai-alert--warning", shiny::tags$b("谨慎解释："), text))
      }
      shiny::tags$div(class = "kkai-alert kkai-alert--success", "机器学习分析已完成；结果仍代表预测关联而非因果关系。")
    })

    read_ml_csv <- function(filename) {
      shiny::reactive({
        run_counter()
        if (is.null(state$job_dir)) return(NULL)
        path <- file.path(state$job_dir, "tables", filename)
        if (!file.exists(path)) return(NULL)
        tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      })
    }
    sample_summary <- read_ml_csv("ml_sample_summary.csv")
    metrics <- read_ml_csv("ml_model_metrics.csv")
    confusion <- read_ml_csv("ml_confusion_matrix.csv")
    importance <- read_ml_csv("ml_feature_importance_stability.csv")
    top_taxa <- read_ml_csv("ml_top_taxa_abundance_statistics.csv")

    render_table <- function(data_rx, page_length = 15L) {
      DT::renderDT({
        df <- data_rx()
        if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
          return(DT::datatable(data.frame(Message = "结果尚未生成或文件读取失败。"), rownames = FALSE, options = list(dom = "t")))
        }
        DT::datatable(df, rownames = FALSE, options = list(pageLength = page_length, scrollX = TRUE, autoWidth = TRUE))
      })
    }
    output$sample_summary_tbl <- render_table(sample_summary)
    output$metrics_tbl <- render_table(metrics)
    output$confusion_tbl <- render_table(confusion)
    output$importance_tbl <- render_table(importance, 20L)
    output$top_taxa_tbl <- render_table(top_taxa)

    image_ui <- function(filename, alt) {
      shiny::renderUI({
        run_counter()
        if (is.null(state$job_dir) || is.null(fig_prefix())) return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "图形尚未生成。"))
        full <- file.path(state$job_dir, "figures", filename)
        if (!file.exists(full)) return(shiny::tags$div(class = "kkai-alert kkai-alert--info", paste0(alt, "尚未生成。")))
        version <- as.numeric(file.info(full)$mtime)
        shiny::tags$div(class = "kkai-result-image-wrap", shiny::tags$img(src = paste0(file.path(fig_prefix(), filename), "?v=", version), alt = alt, class = "kkai-result-img"))
      })
    }
    output$roc_plot <- image_ui("ml_figure_roc.png", "ROC 图")
    output$pr_plot <- image_ui("ml_figure_pr.png", "PR 图")
    output$confusion_plot <- image_ui("ml_figure_confusion_matrix.png", "混淆矩阵")
    output$importance_plot <- image_ui("ml_figure_feature_importance.png", "稳定性特征重要性图")
    output$top_taxa_plot <- image_ui("ml_figure_top_taxa.png", "关键菌丰度图")
    output$performance_plot <- image_ui("ml_figure_performance_distribution.png", "模型性能分布图")

    read_ml_text <- function(filename) shiny::renderText({
      run_counter()
      if (is.null(state$job_dir)) return("结果尚未生成。")
      path <- file.path(state$job_dir, filename)
      if (!file.exists(path)) return("结果尚未生成。")
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    })
    output$methods_text <- read_ml_text("ml_methods.txt")
    output$results_text <- read_ml_text("ml_results_summary.txt")

    output$download_all <- shiny::downloadHandler(
      filename = function() paste0("ml_results_", state$job_id %||% "current", ".zip"),
      content = function(file) {
        shiny::req(state$job_dir)
        manifest <- c(
          list.files(file.path(state$job_dir, "tables"), pattern = "^ml_", full.names = TRUE),
          list.files(file.path(state$job_dir, "figures"), pattern = "^ml_figure_", full.names = TRUE),
          file.path(state$job_dir, c("ml_methods.txt", "ml_results_summary.txt")),
          file.path(state$job_dir, "json", "ml_summary.json")
        )
        manifest <- manifest[file.exists(manifest)]
        shiny::validate(shiny::need(length(manifest) > 0L, "机器学习结果尚未生成，无法下载。"))
        old <- setwd(state$job_dir); on.exit(setwd(old), add = TRUE)
        utils::zip(file, files = substring(normalizePath(manifest, winslash = "/"), nchar(normalizePath(state$job_dir, winslash = "/")) + 2L))
      }
    )
    download_figure <- function(filename, label) shiny::downloadHandler(
      filename = function() filename,
      content = function(file) {
        shiny::req(state$job_dir)
        src <- file.path(state$job_dir, "figures", filename)
        shiny::validate(shiny::need(file.exists(src), paste0(label, "尚未生成，无法下载。")))
        if (!file.copy(src, file, overwrite = TRUE)) stop("下载文件复制失败。", call. = FALSE)
      }
    )
    output$download_combined_pdf <- download_figure("ml_figure_combined.pdf", "论文组合图")
    output$download_combined_tiff <- download_figure("ml_figure_combined.tiff", "论文组合图")
  })
}
