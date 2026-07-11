# 网络分析结果页：预览当前任务中的网络结果。

mod_network_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("网络分析"),
    shiny::tags$div(
      class = "kkai-alert kkai-alert--warning",
      shiny::tags$b("解释提示："),
      "共现网络不代表直接相互作用或因果关系，仅作为探索性证据。"
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("核心网络图"),
      shiny::tags$p(
        class = "kkai-muted",
        "前端仅展示核心网络，完整节点和边请下载 network_nodes.csv 和 network_edges.csv。"
      ),
      shiny::tags$div(
        class = "kkai-control",
        shiny::selectInput(
          ns("plot_choice"),
          "图形视图",
          choices = c(
            "论文四联组合图" = "paper",
            "核心网络总览" = "overview",
            "Hub taxa 标注图" = "labelled",
            "Hub taxa degree 排名" = "degree",
            "Degree–betweenness 中心性" = "centrality",
            "正负相关边构成" = "edges"
          ),
          selected = "paper"
        )
      ),
      shiny::uiOutput(ns("network_plot")),
      shiny::uiOutput(ns("plot_actions"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("网络统计概况"),
      DT::DTOutput(ns("stats_tbl"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("Hub taxa 表格（Top 20 by degree）"),
      DT::DTOutput(ns("hub_tbl"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("最强相关边预览（Top 20 by |rho|）"),
      DT::DTOutput(ns("edges_tbl"))
    ),
    shiny::tags$details(
      shiny::tags$summary("详情"),
      shiny::tags$p("该页仅用于查看已有的网络结果。核心图用于展示，完整网络信息请以表格输出为准。")
    )
  )
}

mod_network_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
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

    resolve_plot_file <- function(choice) {
      choice <- choice %||% "paper"
      fig_dir <- state$job_dir %||% NULL
      if (is.null(fig_dir)) return(NULL)
      figs_dir <- file.path(fig_dir, "figures")
      candidates <- switch(
        choice,
        paper = c("network_figure_combined.png", "network_plot.png"),
        overview = c("network_plot_overview.png", "network_plot.png"),
        labelled = c("network_plot_labelled.png"),
        degree = c("network_degree_barplot.png"),
        centrality = c("network_centrality.png"),
        edges = c("network_edge_composition.png"),
        c("network_figure_combined.png", "network_plot.png")
      )
      for (nm in candidates) {
        if (file.exists(file.path(figs_dir, nm))) return(nm)
      }
      NULL
    }

    resolve_plot_pdf <- function(choice) {
      png_file <- resolve_plot_file(choice)
      if (is.null(png_file)) return(NULL)
      sub("\\.png$", ".pdf", png_file)
    }

    selected_plot_file <- shiny::reactive({
      resolve_plot_file(input$plot_choice %||% "paper")
    })

    selected_plot_pdf <- shiny::reactive({
      resolve_plot_pdf(input$plot_choice %||% "paper")
    })

    shiny::observeEvent(state$job_dir, {
      shiny::req(state$job_dir, fig_prefix())
      figs_dir <- file.path(state$job_dir, "figures")
      if (dir.exists(figs_dir)) safe_add_resource_path(fig_prefix(), figs_dir)
    }, ignoreInit = TRUE)

    output$network_plot <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务，请先运行分析。"))
      }

      png_file <- selected_plot_file()
      if (is.null(png_file)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("未找到网络图。"),
            shiny::tags$div("请先生成工作流输出。")
          )
        )
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("论文级网络图预览"),
        shiny::tags$div(class = "kkai-result-image-wrap",
          shiny::tags$a(
            href = paste0(file.path(fig_prefix(), png_file), "?v=", as.numeric(file.info(file.path(state$job_dir, "figures", png_file))$mtime)),
            target = "_blank",
            shiny::tags$img(src = paste0(file.path(fig_prefix(), png_file), "?v=", as.numeric(file.info(file.path(state$job_dir, "figures", png_file))$mtime)), class = "kkai-result-img")
          )
        )
      )
    })

    output$plot_actions <- shiny::renderUI({
      if (is.null(state$job_dir) || is.null(selected_plot_file())) return(NULL)
      shiny::tags$div(
        class = "kkai-quick-actions",
        shiny::tags$a(
          href = file.path(fig_prefix(), selected_plot_file()),
          target = "_blank",
          class = "btn btn-outline-primary",
          "打开大图"
        ),
        shiny::downloadButton(ns("dl_plot_png"), "下载 PNG", class = "btn btn-outline-primary"),
        shiny::downloadButton(ns("dl_plot_pdf"), "下载 PDF", class = "btn btn-outline-primary"),
        shiny::downloadButton(ns("dl_plot_svg"), "下载 SVG", class = "btn btn-outline-primary"),
        shiny::downloadButton(ns("dl_plot_tiff"), "下载 TIFF 600 dpi", class = "btn btn-outline-primary"),
        shiny::downloadButton(ns("dl_nodes_csv"), "下载 network_nodes.csv", class = "btn btn-outline-dark"),
        shiny::downloadButton(ns("dl_edges_csv"), "下载 network_edges.csv", class = "btn btn-outline-dark")
      )
    })

    output$dl_plot_png <- shiny::downloadHandler(
      filename = function() basename(selected_plot_file() %||% "network_plot.png"),
      content = function(file) {
        shiny::req(state$job_dir, selected_plot_file())
        file.copy(file.path(state$job_dir, "figures", selected_plot_file()), file, overwrite = TRUE)
      },
      contentType = "image/png"
    )

    output$dl_plot_pdf <- shiny::downloadHandler(
      filename = function() basename(selected_plot_pdf() %||% "network_plot.pdf"),
      content = function(file) {
        shiny::req(state$job_dir, selected_plot_pdf())
        file.copy(file.path(state$job_dir, "figures", selected_plot_pdf()), file, overwrite = TRUE)
      },
      contentType = "application/pdf"
    )

    selected_plot_variant <- function(extension) {
      png <- selected_plot_file()
      if (is.null(png)) return(NULL)
      sub("\\.png$", paste0(".", extension), png)
    }

    output$dl_plot_svg <- shiny::downloadHandler(
      filename = function() basename(selected_plot_variant("svg") %||% "network_figure.svg"),
      content = function(file) {
        shiny::req(state$job_dir)
        src <- file.path(state$job_dir, "figures", selected_plot_variant("svg"))
        shiny::validate(shiny::need(file.exists(src), "该历史任务没有 SVG 图，请重新运行网络分析。"))
        file.copy(src, file, overwrite = TRUE)
      },
      contentType = "image/svg+xml"
    )

    output$dl_plot_tiff <- shiny::downloadHandler(
      filename = function() basename(selected_plot_variant("tiff") %||% "network_figure.tiff"),
      content = function(file) {
        shiny::req(state$job_dir)
        src <- file.path(state$job_dir, "figures", selected_plot_variant("tiff"))
        shiny::validate(shiny::need(file.exists(src), "该历史任务没有 TIFF 图，请重新运行网络分析。"))
        file.copy(src, file, overwrite = TRUE)
      },
      contentType = "image/tiff"
    )

    output$dl_nodes_csv <- shiny::downloadHandler(
      filename = function() "network_nodes.csv",
      content = function(file) {
        shiny::req(state$job_dir)
        file.copy(file.path(state$job_dir, "tables", "network_nodes.csv"), file, overwrite = TRUE)
      },
      contentType = "text/csv"
    )

    output$dl_edges_csv <- shiny::downloadHandler(
      filename = function() "network_edges.csv",
      content = function(file) {
        shiny::req(state$job_dir)
        file.copy(file.path(state$job_dir, "tables", "network_edges.csv"), file, overwrite = TRUE)
      },
      contentType = "text/csv"
    )

    output$hub_tbl <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(DT::datatable(data.frame(Message = "当前没有活动任务。"), rownames = FALSE, options = list(dom = "t")))
      }
      p <- file.path(state$job_dir, "tables", "network_nodes.csv")
      if (!file.exists(p)) {
        return(DT::datatable(data.frame(Message = "未找到 tables/network_nodes.csv。"), rownames = FALSE, options = list(dom = "t")))
      }
      df <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(DT::datatable(data.frame(Message = "network_nodes.csv 中没有数据。"), rownames = FALSE, options = list(dom = "t")))
      }

      ord <- order(df$degree, df$betweenness, decreasing = TRUE, na.last = TRUE)
      df <- utils::head(df[ord, , drop = FALSE], 20)
      keep_cols <- intersect(c("display_taxon", "phylum", "degree", "betweenness", "closeness", "component", "mean_abundance", "prevalence"), names(df))
      df <- df[, keep_cols, drop = FALSE]
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, dom = "tip", autoWidth = TRUE))
    })

    output$stats_tbl <- DT::renderDT({
      if (is.null(state$job_dir)) return(DT::datatable(data.frame(Message = "当前没有活动任务。"), rownames = FALSE, options = list(dom = "t")))
      path <- file.path(state$job_dir, "tables", "network_statistics.csv")
      if (!file.exists(path)) return(DT::datatable(data.frame(Message = "历史任务暂无网络统计表，请重新运行网络分析。"), rownames = FALSE, options = list(dom = "t")))
      df <- tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df)) return(DT::datatable(data.frame(Message = "网络统计表读取失败。"), rownames = FALSE, options = list(dom = "t")))
      DT::datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 15, autoWidth = TRUE))
    })

    output$edges_tbl <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(DT::datatable(data.frame(Message = "当前没有活动任务。"), rownames = FALSE, options = list(dom = "t")))
      }
      p <- file.path(state$job_dir, "tables", "network_edges.csv")
      if (!file.exists(p)) {
        return(DT::datatable(data.frame(Message = "未找到 tables/network_edges.csv。"), rownames = FALSE, options = list(dom = "t")))
      }
      df <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(DT::datatable(data.frame(Message = "network_edges.csv 中没有数据。"), rownames = FALSE, options = list(dom = "t")))
      }
      ord <- order(df$abs_rho, df$fdr, decreasing = c(TRUE, FALSE), na.last = TRUE)
      df <- utils::head(df[ord, , drop = FALSE], 20)
      keep_cols <- intersect(c("source_display", "target_display", "rho", "abs_rho", "fdr", "sign"), names(df))
      df <- df[, keep_cols, drop = FALSE]
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, dom = "tip", autoWidth = TRUE))
    })
  })
}
