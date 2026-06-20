mod_alpha_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h3("Alpha 多样性"),
    shiny::p("支持按 Alpha 指数与图类型切换结果，并查看统计表、综合图、测序深度和稀释曲线。"),
    shiny::tags$div(
      class = "kkai-alert kkai-alert--info",
      "该页面只负责读取当前任务目录中已经生成的 Alpha 结果；如果文件不存在，会显示友好提示而不会报错。"
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectInput(
          ns("alpha_index"),
          label = "选择 Alpha 指数",
          choices = c(
            "Observed richness" = "observed",
            "Chao1" = "chao1",
            "ACE" = "ace",
            "Shannon" = "shannon",
            "Simpson" = "simpson",
            "Pielou evenness" = "pielou"
          ),
          selected = "shannon"
        )
      ),
      shiny::column(
        width = 4,
        shiny::selectInput(
          ns("plot_type"),
          label = "选择图类型",
          choices = c(
            "箱线图" = "boxplot",
            "小提琴 + 箱线图（论文推荐）" = "violin_boxplot",
            "均值柱状图 + SE" = "bar_mean_se",
            "均值点图 + 误差线" = "dot_errorbar",
            "密度分布图" = "density"
          ),
          selected = "violin_boxplot"
        )
      )
    ),
    shiny::uiOutput(ns("selected_plot_ui")),
    shiny::tags$details(open = "open", shiny::tags$summary("Alpha 指数总表"), DT::DTOutput(ns("alpha_table_dt"))),
    shiny::tags$details(shiny::tags$summary("分组统计摘要"), DT::DTOutput(ns("group_summary_dt"))),
    shiny::tags$details(shiny::tags$summary("组间检验结果"), DT::DTOutput(ns("alpha_stats_dt"))),
    shiny::tags$details(shiny::tags$summary("两两比较结果"), DT::DTOutput(ns("pairwise_stats_dt"))),
    shiny::tags$details(open = "open", shiny::tags$summary("多指标综合图"), shiny::uiOutput(ns("facet_plot_ui"))),
    shiny::tags$details(shiny::tags$summary("测序深度图"), shiny::uiOutput(ns("depth_plot_ui"))),
    shiny::tags$details(shiny::tags$summary("稀释曲线"), shiny::uiOutput(ns("rarefaction_plot_ui"))),
    shiny::tags$details(shiny::tags$summary("Alpha 指数热图"), shiny::uiOutput(ns("heatmap_plot_ui")))
  )
}

mod_alpha_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("job_", state$job_id, "_figures")
    })

    safe_add_resource_path <- function(prefix, directory_path) {
      if (is.null(prefix) || !nzchar(prefix) || is.null(directory_path) || !dir.exists(directory_path)) {
        return(invisible(FALSE))
      }
      rp <- tryCatch(shiny::resourcePaths(), error = function(e) NULL)
      if (is.list(rp) && !is.null(rp[[prefix]])) return(invisible(FALSE))
      shiny::addResourcePath(prefix = prefix, directoryPath = directory_path)
      invisible(TRUE)
    }

    safe_read_csv <- function(path) {
      if (is.null(path) || !is.character(path) || length(path) != 1 || !file.exists(path)) return(NULL)
      tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    }

    render_table_or_message <- function(path, empty_message) {
      df <- safe_read_csv(path)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(
          DT::datatable(
            data.frame(Message = empty_message, stringsAsFactors = FALSE),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE)
          )
        )
      }

      DT::datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      )
    }

    render_image_card <- function(filename, title, empty_message) {
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务。"))
      }

      full_path <- file.path(state$job_dir, "figures", filename)
      if (!file.exists(full_path)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", empty_message))
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header(title),
        shiny::tags$div(
          class = "kkai-result-image-wrap",
          style = "border: 1px solid rgba(148, 163, 184, 0.25); border-radius: 14px; padding: 0.75rem; background: #fff;",
          shiny::tags$img(
            src = file.path(fig_prefix(), filename),
            class = "kkai-result-img kkai-result-img--fit",
            style = "max-width: 100%; height: auto;"
          )
        )
      )
    }

    shiny::observe({
      shiny::req(state$job_dir, fig_prefix())
      safe_add_resource_path(fig_prefix(), file.path(state$job_dir, "figures"))
    })

    selected_plot_file <- shiny::reactive({
      plot_type <- input$plot_type %||% "violin_boxplot"
      alpha_index <- input$alpha_index %||% "shannon"
      paste0("alpha_", alpha_index, "_", plot_type, ".png")
    })

    output$selected_plot_ui <- shiny::renderUI({
      render_image_card(
        filename = selected_plot_file(),
        title = "当前选择的 Alpha 图",
        empty_message = "该图尚未生成，请先运行 Alpha 多样性分析。"
      )
    })

    output$facet_plot_ui <- shiny::renderUI({
      render_image_card(
        filename = "alpha_multi_index_facet.png",
        title = "Alpha 多指标综合图",
        empty_message = "该图尚未生成，请先运行 Alpha 多样性分析。"
      )
    })

    output$depth_plot_ui <- shiny::renderUI({
      render_image_card(
        filename = "sequencing_depth_barplot.png",
        title = "测序深度柱状图",
        empty_message = "该图尚未生成，请先运行 Alpha 多样性分析。"
      )
    })

    output$rarefaction_plot_ui <- shiny::renderUI({
      render_image_card(
        filename = "alpha_rarefaction_curve.png",
        title = "稀释曲线",
        empty_message = "该图尚未生成，或者当前数据不适合绘制稀释曲线。"
      )
    })

    output$heatmap_plot_ui <- shiny::renderUI({
      render_image_card(
        filename = "alpha_index_heatmap.png",
        title = "Alpha 指数热图",
        empty_message = "该图尚未生成，或者当前环境缺少热图依赖包。"
      )
    })

    output$alpha_table_dt <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(render_table_or_message(NULL, "当前没有活动任务。"))
      }
      render_table_or_message(
        file.path(state$job_dir, "tables", "alpha_diversity.csv"),
        "alpha_diversity.csv 尚未生成，请先运行 Alpha 多样性分析。"
      )
    })

    output$group_summary_dt <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(render_table_or_message(NULL, "当前没有活动任务。"))
      }
      render_table_or_message(
        file.path(state$job_dir, "tables", "alpha_group_summary.csv"),
        "alpha_group_summary.csv 尚未生成，请先运行 Alpha 多样性分析。"
      )
    })

    output$alpha_stats_dt <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(render_table_or_message(NULL, "当前没有活动任务。"))
      }
      render_table_or_message(
        file.path(state$job_dir, "tables", "alpha_stats.csv"),
        "alpha_stats.csv 尚未生成，请先运行 Alpha 多样性分析。"
      )
    })

    output$pairwise_stats_dt <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(render_table_or_message(NULL, "当前没有活动任务。"))
      }
      render_table_or_message(
        file.path(state$job_dir, "tables", "alpha_pairwise_stats.csv"),
        "alpha_pairwise_stats.csv 尚未生成，或者当前分组超过 3 组而被自动跳过。"
      )
    })
  })
}
