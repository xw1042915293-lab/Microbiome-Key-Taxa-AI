# Differential abundance results: publication-oriented figures and tables.

mod_diff_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("差异丰度"),
    shiny::p("按所选分类层级聚合后进行非参数检验，并报告 BH-FDR、效应量和组内描述统计。"),
    shiny::uiOutput(ns("diff_notice")),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("差异丰度主图"),
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("plot_type_control"))),
        shiny::column(2, shiny::numericInput(ns("top_n"), "显示 taxa 数量", value = 10, min = 5, max = 30, step = 1)),
        shiny::column(3, shiny::radioButtons(
          ns("sort_by"), "棒棒糖图排序",
          choices = c("Effect size" = "effect_size", "BH-FDR" = "fdr"),
          selected = "effect_size", inline = TRUE
        )),
        shiny::column(2, shiny::checkboxInput(ns("show_unclassified"), "显示完全未分类 taxa", value = FALSE)),
        shiny::column(2,
          shiny::checkboxInput(ns("show_fdr_labels"), "显示校正 P 值", value = FALSE),
          shiny::checkboxInput(ns("show_significance"), "显示显著性", value = TRUE)
        )
      ),
      shiny::uiOutput(ns("plot_explanation")),
      shiny::uiOutput(ns("main_plot_ui")),
      shiny::div(
        class = "kkai-download-row",
        shiny::downloadButton(ns("download_figure_png"), "当前图 PNG (300 dpi)", class = "btn btn-outline-secondary btn-sm"),
        shiny::downloadButton(ns("download_figure_pdf"), "当前图 PDF", class = "btn btn-outline-secondary btn-sm")
      )
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    shiny::div(
      class = "kkai-download-row",
      shiny::downloadButton(ns("download_all"), "完整结果 CSV", class = "btn btn-outline-primary btn-sm"),
      shiny::downloadButton(ns("download_sig"), "显著结果 CSV", class = "btn btn-outline-primary btn-sm"),
      shiny::downloadButton(ns("download_group"), "组内统计 CSV", class = "btn btn-outline-primary btn-sm"),
      shiny::downloadButton(ns("download_pairwise"), "事后比较 CSV", class = "btn btn-outline-primary btn-sm")
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("差异分析完整结果"),
      shiny::tags$p(class = "kkai-muted", "下表保留全部聚合后的分类单元，支持检索、排序和分页。"),
      DT::DTOutput(ns("diff_table"))
    )
  )
}

mod_diff_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    table_path <- function(name) {
      if (is.null(state$job_dir)) return(NULL)
      file.path(state$job_dir, "tables", name)
    }

    read_table <- function(name) {
      path <- table_path(name)
      if (is.null(path) || !file.exists(path)) return(NULL)
      tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
    }

    diff_table_data <- shiny::reactive(read_table("differential_taxa.csv"))
    group_summary_data <- shiny::reactive(read_table("differential_taxa_group_summary.csv"))
    comparison_groups <- shiny::reactive({
      gs <- group_summary_data()
      if (!is.data.frame(gs) || !"group" %in% names(gs)) return(character())
      unique(as.character(gs$group))
    })

    output$plot_type_control <- shiny::renderUI({
      groups <- comparison_groups()
      choices <- c(
        "多组均衡效应图" = "balanced",
        "组丰度热图" = "heatmap",
        "原始棒棒糖图" = "lollipop",
        "两组方向效应图" = "directional"
      )
      selected <- if (length(groups) == 2) "directional" else "balanced"
      shiny::tags$div(
        class = "kkai-control",
        shiny::selectInput(session$ns("plot_type"), "选择图形类型", choices = choices, selected = selected)
      )
    })

    lollipop_data <- shiny::reactive({
      df <- diff_table_data()
      if (is.null(df)) return(data.frame())
      prepare_diff_lollipop_data(
        df, top_n = as.integer(input$top_n %||% 10L),
        show_unclassified = isTRUE(input$show_unclassified),
        sort_by = input$sort_by %||% "effect_size"
      )
    })

    directional_data <- shiny::reactive({
      df <- diff_table_data()
      if (is.null(df)) return(data.frame())
      suppressWarnings(prepare_diff_directional_data(
        df, group_levels = comparison_groups(),
        top_n = as.integer(input$top_n %||% 10L),
        show_unclassified = isTRUE(input$show_unclassified)
      ))
    })

    balanced_data <- shiny::reactive({
      df <- diff_table_data()
      if (is.null(df)) return(data.frame())
      prepare_diff_balanced_data(
        df, group_levels = comparison_groups(),
        top_n = as.integer(input$top_n %||% 10L),
        show_unclassified = isTRUE(input$show_unclassified)
      )
    })

    heatmap_data <- shiny::reactive({
      df <- diff_table_data()
      gs <- group_summary_data()
      if (is.null(df) || is.null(gs)) return(data.frame())
      prepare_diff_heatmap_data(
        df, gs, group_levels = comparison_groups(),
        top_n = as.integer(input$top_n %||% 10L),
        show_unclassified = isTRUE(input$show_unclassified)
      )
    })

    current_plot_data <- shiny::reactive({
      switch(
        input$plot_type %||% if (length(comparison_groups()) == 2) "directional" else "balanced",
        directional = directional_data(),
        lollipop = lollipop_data(),
        heatmap = heatmap_data(),
        balanced_data()
      )
    })

    current_plot <- shiny::reactive({
      plot_type <- input$plot_type %||% if (length(comparison_groups()) == 2) "directional" else "balanced"
      switch(
        plot_type,
        directional = build_diff_directional_plot(
          directional_data(), show_fdr = isTRUE(input$show_fdr_labels),
          show_significance = isTRUE(input$show_significance)
        ),
        lollipop = build_diff_lollipop_plot(
          lollipop_data(), show_fdr_labels = isTRUE(input$show_fdr_labels),
          sort_by = input$sort_by %||% "effect_size"
        ),
        heatmap = build_diff_heatmap_plot(heatmap_data()),
        build_diff_balanced_plot(
          balanced_data(), show_fdr = isTRUE(input$show_fdr_labels),
          show_significance = isTRUE(input$show_significance)
        )
      )
    })

    plot_height_inches <- shiny::reactive(max(5.5, min(10, 2.7 + 0.38 * nrow(current_plot_data()))))

    output$diff_notice <- shiny::renderUI({
      df <- diff_table_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前任务尚无差异丰度结果。"))
      }
      fdr <- if ("fdr" %in% names(df)) suppressWarnings(as.numeric(df$fdr)) else rep(NA_real_, nrow(df))
      n_sig <- sum(is.finite(fdr) & fdr < 0.05)
      if (n_sig == 0) {
        shiny::tags$div(class = "kkai-alert kkai-alert--warning", "未检测到 FDR < 0.05 的显著差异菌。请查看完整结果表确认筛选与检验状态。")
      } else {
        shiny::tags$div(class = "kkai-alert kkai-alert--success", paste0("检测到 ", n_sig, " 个 FDR < 0.05 的显著差异菌。"))
      }
    })

    output$plot_explanation <- shiny::renderUI({
      plot_type <- input$plot_type %||% if (length(comparison_groups()) == 2) "directional" else "balanced"
      if (identical(plot_type, "lollipop")) {
        return(shiny::tags$p(class = "kkai-muted", "原棒棒糖图：线段和点表示效应量，颜色表示富集组，点大小表示 −log10(FDR)。"))
      }
      if (identical(plot_type, "balanced")) {
        return(shiny::tags$p(class = "kkai-muted", "多组均衡效应图：Top N 名额优先平均分配到各富集组，再按效应量补足，因此 HEB、NMG、XJ 均可展示。"))
      }
      if (identical(plot_type, "heatmap")) {
        return(shiny::tags$p(class = "kkai-muted", "组丰度热图：使用真实组中位相对丰度并按 taxon 行标准化，用于比较 HEB、NMG、XJ 的丰度模式。"))
      }
      groups <- comparison_groups()
      direction <- resolve_direction_groups(groups)
      if (!isTRUE(direction$valid)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--warning",
          paste0("当前比较包含 ", length(groups), " 个组，无法为 signed effect 定义唯一左右方向；请切换 Original lollipop plot。")))
      }
      shiny::tags$p(class = "kkai-muted",
        paste0("方向定义：", direction$negative, " 为负方向，", direction$positive,
               " 为正方向。富集组优先由显著 post-hoc 比较确定，否则使用唯一最高组中位数；并列时记为 Undetermined。"))
    })

    output$main_plot_ui <- shiny::renderUI({
      height_px <- max(440, min(820, 220 + 34 * max(1, nrow(current_plot_data()))))
      shiny::plotOutput(session$ns("main_plot"), height = paste0(height_px, "px"))
    })

    output$main_plot <- shiny::renderPlot({ current_plot() }, res = 110, bg = "white")

    make_download <- function(output_id, source_name, download_name) {
      output[[output_id]] <- shiny::downloadHandler(
        filename = function() download_name,
        content = function(file) {
          src <- table_path(source_name)
          shiny::req(src, file.exists(src))
          file.copy(src, file, overwrite = TRUE)
        }
      )
    }
    make_download("download_all", "differential_taxa.csv", "differential_taxa_all.csv")
    make_download("download_sig", "differential_taxa_significant.csv", "differential_taxa_significant.csv")
    make_download("download_group", "differential_taxa_group_summary.csv", "differential_taxa_group_summary.csv")
    make_download("download_pairwise", "differential_taxa_pairwise.csv", "differential_taxa_pairwise.csv")

    download_stem <- shiny::reactive({
      groups <- comparison_groups()
      comparison <- if (length(groups) == 2) paste(groups, collapse = "_vs_") else paste0(length(groups), "_groups")
      comparison <- gsub("[^A-Za-z0-9._-]+", "_", comparison)
      plot_name <- switch(
        input$plot_type %||% if (length(groups) == 2) "directional" else "balanced",
        directional = "directional_effect", balanced = "group_balanced_effect",
        heatmap = "group_abundance_heatmap", "lollipop"
      )
      paste0("differential_taxa_", comparison, "_", plot_name)
    })

    output$download_figure_png <- shiny::downloadHandler(
      filename = function() paste0(download_stem(), ".png"),
      content = function(file) {
        ggplot2::ggsave(file, plot = current_plot(), device = "png", width = 9,
                        height = plot_height_inches(), units = "in", dpi = 300, bg = "white")
      }
    )
    output$download_figure_pdf <- shiny::downloadHandler(
      filename = function() paste0(download_stem(), ".pdf"),
      content = function(file) {
        ggplot2::ggsave(file, plot = current_plot(), device = "pdf", width = 9,
                        height = plot_height_inches(), units = "in", bg = "white")
      }
    )

    output$diff_table <- DT::renderDT({
      df <- diff_table_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(DT::datatable(data.frame(提示 = "未找到可展示的差异丰度结果。"), rownames = FALSE, options = list(dom = "t")))
      }
      show_cols <- intersect(c(
        "display_taxon", "taxon_label", "tax_level", "tested", "prevalence", "mean_abundance", "p_value", "fdr",
        "epsilon_squared", "signed_epsilon2", "log2fc", "effect_size", "effect_size_metric",
        "enriched_group", "enriched_group_method", "significance_stars",
        "effect_magnitude", "direction", "significance", "exclusion_reason"
      ), names(df))
      df_show <- df[, show_cols, drop = FALSE]
      labels <- c(
        display_taxon = "显示名称", taxon_label = "原分类名称", tax_level = "分类层级", tested = "已检验",
        prevalence = "总流行率", mean_abundance = "平均相对丰度", p_value = "P 值", fdr = "BH-FDR",
        epsilon_squared = "epsilon-squared", signed_epsilon2 = "signed epsilon²",
        log2fc = "log2FC", effect_size = "效应量", effect_size_metric = "效应量指标",
        enriched_group = "富集组", enriched_group_method = "富集组判定", significance_stars = "显著性",
        effect_magnitude = "效应等级", direction = "方向", significance = "显著状态", exclusion_reason = "过滤原因"
      )
      names(df_show) <- unname(labels[show_cols])
      dt <- DT::datatable(df_show, rownames = FALSE, filter = "top", extensions = "Buttons",
                          options = list(pageLength = 20, scrollX = TRUE, searchHighlight = TRUE,
                                         dom = "Bfrtip", buttons = c("copy", "csv")))
      numeric_cols <- intersect(c("总流行率", "平均相对丰度", "P 值", "BH-FDR", "epsilon-squared", "signed epsilon²", "log2FC", "效应量"), names(df_show))
      if (length(numeric_cols)) dt <- DT::formatSignif(dt, numeric_cols, digits = 4)
      dt
    })
  })
}
