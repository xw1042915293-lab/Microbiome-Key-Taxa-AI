# Alpha results page: selectable metrics, scientific plot styles and result tables.
mod_alpha_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::tags$div(
      class = "kkai-page-header",
      shiny::tags$h2("Alpha 多样性"),
      shiny::tags$p("选择指标与科研图形样式。所有图形均来自当前任务的 Alpha 专属目录。")
    ),
    shiny::uiOutput(ns("summary_ui")),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("Alpha 图形浏览"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::uiOutput(ns("metric_control")),
        shiny::uiOutput(ns("plot_type_control"))
      ),
      shiny::uiOutput(ns("plot_ui"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("组间总体检验"),
      shiny::tags$p(
        class = "kkai-muted",
        "两组使用 Wilcoxon 检验，多组使用 Kruskal-Wallis 检验；FDR 在核心 Alpha 指标之间统一校正。"
      ),
      DT::DTOutput(ns("stats_table"))
    ),
    shiny::tags$details(
      style = "margin-top: 1rem;",
      shiny::tags$summary("查看样本级 Alpha 指标"),
      DT::DTOutput(ns("sample_table"))
    )
  )
}

mod_alpha_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    metric_labels <- c(
      overview = "四项核心指标概览",
      Observed = "Observed（观测丰富度）",
      Chao1 = "Chao1（估计丰富度）",
      Shannon = "Shannon（丰富度与均匀度）",
      Simpson = "Simpson（优势度敏感）"
    )
    plot_type_labels <- stats::setNames(alpha_plot_type_spec()$label, alpha_plot_type_spec()$type)
    registered_prefixes <- new.env(parent = emptyenv())

    alpha_figure_root <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      preferred <- file.path(state$job_dir, "alpha", "figures")
      if (dir.exists(preferred)) preferred else file.path(state$job_dir, "figures")
    })

    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("alpha_", gsub("[^A-Za-z0-9_-]", "_", state$job_id), "_plots")
    })

    shiny::observe({
      shiny::req(alpha_figure_root(), fig_prefix())
      root <- alpha_figure_root()
      prefix <- fig_prefix()
      if (dir.exists(root) && !isTRUE(registered_prefixes[[prefix]])) {
        shiny::addResourcePath(prefix = prefix, directoryPath = root)
        registered_prefixes[[prefix]] <- TRUE
      }
    })

    alpha_table <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      path <- alpha_output_path(
        state$job_dir, "tables", "alpha_diversity.csv",
        legacy_filename = "alpha_diversity.csv", existing = TRUE
      )
      if (!file.exists(path)) return(NULL)
      tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    })

    alpha_stats <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      path <- alpha_output_path(
        state$job_dir, "tables", "alpha_stats.csv",
        legacy_filename = "alpha_stats.csv", existing = TRUE
      )
      if (!file.exists(path)) return(NULL)
      tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    })

    figure_path <- function(metric, plot_type) {
      if (is.null(state$job_dir)) return(NULL)
      if (identical(metric, "overview")) {
        alpha_overview_figure_path(state$job_dir, plot_type, "png", existing = TRUE)
      } else {
        alpha_metric_figure_path(state$job_dir, metric, plot_type, "png", existing = TRUE)
      }
    }

    available_metrics <- shiny::reactive({
      candidates <- names(metric_labels)
      available <- vapply(candidates, function(metric) {
        any(vapply(alpha_plot_type_spec()$type, function(type) file.exists(figure_path(metric, type)), logical(1)))
      }, logical(1))
      candidates[available]
    })

    available_plot_types <- shiny::reactive({
      metrics <- available_metrics()
      if (length(metrics) == 0) return(character(0))
      metric <- input$metric %||% if ("overview" %in% metrics) "overview" else metrics[[1]]
      if (!metric %in% metrics) metric <- metrics[[1]]
      types <- alpha_plot_type_spec()$type
      types[vapply(types, function(type) file.exists(figure_path(metric, type)), logical(1))]
    })

    output$summary_ui <- shiny::renderUI({
      tbl <- alpha_table()
      stats <- alpha_stats()
      if (!is.data.frame(tbl)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前任务尚未生成 Alpha 多样性结果。"))
      }

      n_groups <- if (is.data.frame(stats) && "n_groups" %in% names(stats)) suppressWarnings(max(stats$n_groups, na.rm = TRUE)) else NA_real_
      if (!is.finite(n_groups)) n_groups <- NA_real_
      n_sig <- if (is.data.frame(stats) && "fdr" %in% names(stats)) sum(stats$fdr < 0.05, na.rm = TRUE) else 0L
      warning_text <- state$alpha_result$alpha_plot_warning %||% NULL
      if ((is.null(warning_text) || !nzchar(warning_text)) && !is.na(n_groups) && n_groups > 8) {
        warning_text <- paste0("当前分组变量包含 ", n_groups, " 个水平。建议确认它是否为多个实验因素拼接后的复合分组。")
      }

      shiny::tagList(
        shiny::tags$div(
          class = "kkai-results-summary",
          shiny::tags$div(shiny::tags$b("样本数："), nrow(tbl)),
          shiny::tags$div(shiny::tags$b("核心指标："), length(intersect(names(tbl), alpha_metric_spec()$index))),
          shiny::tags$div(shiny::tags$b("分组水平："), if (is.na(n_groups)) "—" else n_groups),
          shiny::tags$div(shiny::tags$b("FDR < 0.05 指标："), n_sig)
        ),
        if (!is.null(warning_text) && nzchar(warning_text)) shiny::tags$div(class = "kkai-alert kkai-alert--warning", warning_text)
      )
    })

    output$metric_control <- shiny::renderUI({
      metrics <- available_metrics()
      if (length(metrics) == 0) return(NULL)
      selected <- if ("overview" %in% metrics) "overview" else metrics[[1]]
      shiny::tags$div(
        class = "kkai-control",
        shiny::selectInput(
          session$ns("metric"), "选择 Alpha 指标",
          choices = stats::setNames(metrics, unname(metric_labels[metrics])), selected = selected
        )
      )
    })

    output$plot_type_control <- shiny::renderUI({
      types <- available_plot_types()
      if (length(types) == 0) return(NULL)
      selected <- if ("violin_box" %in% types) "violin_box" else types[[1]]
      shiny::tags$div(
        class = "kkai-control",
        shiny::selectInput(
          session$ns("plot_type"), "选择图形类型",
          choices = stats::setNames(types, unname(plot_type_labels[types])), selected = selected
        )
      )
    })

    output$plot_ui <- shiny::renderUI({
      if (is.null(state$job_dir) || is.null(fig_prefix())) return(shiny::helpText("当前没有活动任务。"))
      metrics <- available_metrics()
      if (length(metrics) == 0) return(shiny::helpText("尚未生成可展示的 Alpha 图。"))
      metric <- input$metric %||% if ("overview" %in% metrics) "overview" else metrics[[1]]
      if (!metric %in% metrics) metric <- metrics[[1]]
      types <- available_plot_types()
      if (length(types) == 0) return(shiny::helpText("当前指标尚无可用图形。"))
      plot_type <- input$plot_type %||% if ("violin_box" %in% types) "violin_box" else types[[1]]
      if (!plot_type %in% types) plot_type <- types[[1]]
      full <- figure_path(metric, plot_type)
      root <- normalizePath(alpha_figure_root(), winslash = "/", mustWork = TRUE)
      full_norm <- normalizePath(full, winslash = "/", mustWork = TRUE)
      rel <- substring(full_norm, nchar(root) + 2L)
      src <- paste0(fig_prefix(), "/", rel)

      shiny::tagList(
        shiny::tags$p(
          class = "kkai-muted",
          paste0(metric_labels[[metric]], " · ", plot_type_labels[[plot_type]], "。点击图形可查看原尺寸。")
        ),
        shiny::tags$a(
          href = src, target = "_blank",
          shiny::tags$div(
            class = "kkai-result-image-wrap",
            shiny::tags$img(src = src, class = "kkai-result-img kkai-result-img--fit")
          )
        )
      )
    })

    output$stats_table <- DT::renderDT({
      df <- alpha_stats()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "尚未生成 Alpha 统计结果。"), rownames = FALSE, options = list(dom = "t")))
      }
      if ("p_value" %in% names(df)) df$p_value <- signif(df$p_value, 4)
      if ("fdr" %in% names(df)) df$fdr <- signif(df$fdr, 4)
      DT::datatable(df, rownames = FALSE, options = list(dom = "t", autoWidth = TRUE))
    })

    output$sample_table <- DT::renderDT({
      df <- alpha_table()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "尚未生成样本级 Alpha 指标。"), rownames = FALSE, options = list(dom = "t")))
      }
      preferred <- c("SampleID", alpha_metric_spec()$index)
      group_cols <- setdiff(names(df), c(preferred, "ACE", "se.chao1", "se.ACE", "InvSimpson", "Fisher", "Pielou", "Coverage"))
      keep <- intersect(c("SampleID", group_cols, alpha_metric_spec()$index), names(df))
      DT::datatable(df[, keep, drop = FALSE], rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
    })
  })
}
