# Beta results page: publication-oriented PCoA views and dispersion diagnostics.
mod_beta_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::tags$div(
      class = "kkai-page-header",
      shiny::tags$h2("Beta 多样性"),
      shiny::tags$p("论文级 PCoA、PERMANOVA 与 PERMDISP 组内离散度诊断。")
    ),
    shiny::uiOutput(ns("summary_ui")),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("Beta 图形浏览"),
      shiny::uiOutput(ns("view_control")),
      shiny::uiOutput(ns("plot_ui"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        class = "kkai-card",
        bslib::card_header("PERMANOVA"),
        shiny::tags$p(class = "kkai-muted", "检验各组群落中心是否存在整体差异。"),
        DT::DTOutput(ns("permanova_table"))
      ),
      bslib::card(
        class = "kkai-card",
        bslib::card_header("PERMDISP"),
        shiny::tags$p(class = "kkai-muted", "检验组内离散度是否一致，是解释 PERMANOVA 的必要诊断。"),
        DT::DTOutput(ns("dispersion_table"))
      )
    )
  )
}

mod_beta_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    view_labels <- stats::setNames(beta_plot_view_spec()$label, beta_plot_view_spec()$view)
    registered_prefixes <- new.env(parent = emptyenv())

    beta_figure_root <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      preferred <- file.path(state$job_dir, "beta", "figures")
      if (dir.exists(preferred)) preferred else file.path(state$job_dir, "figures")
    })

    fig_prefix <- shiny::reactive({
      if (is.null(state$job_id)) return(NULL)
      paste0("beta_", gsub("[^A-Za-z0-9_-]", "_", state$job_id), "_plots")
    })

    shiny::observe({
      shiny::req(beta_figure_root(), fig_prefix())
      prefix <- fig_prefix()
      if (dir.exists(beta_figure_root()) && !isTRUE(registered_prefixes[[prefix]])) {
        shiny::addResourcePath(prefix = prefix, directoryPath = beta_figure_root())
        registered_prefixes[[prefix]] <- TRUE
      }
    })

    beta_table <- function(filename) {
      if (is.null(state$job_dir)) return(NULL)
      path <- beta_output_path(state$job_dir, "tables", filename, legacy_filename = filename, existing = TRUE)
      if (!file.exists(path)) return(NULL)
      tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    }

    permanova <- shiny::reactive(beta_table("beta_permanova.csv"))
    dispersion <- shiny::reactive(beta_table("beta_dispersion.csv"))
    coordinates <- shiny::reactive(beta_table("beta_pcoa_coordinates.csv"))

    available_views <- shiny::reactive({
      if (is.null(state$job_dir)) return(character(0))
      views <- beta_plot_view_spec()$view
      views[vapply(views, function(view) file.exists(beta_figure_path(state$job_dir, view, "png", existing = TRUE)), logical(1))]
    })

    output$summary_ui <- shiny::renderUI({
      perm <- permanova()
      disp <- dispersion()
      coords <- coordinates()
      if (!is.data.frame(perm) && !is.data.frame(coords)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前任务尚未生成 Beta 多样性结果。"))
      }
      model <- if (is.data.frame(perm)) perm[!perm$term %in% c("Residual", "Total"), , drop = FALSE] else data.frame()
      r2 <- if (nrow(model) && "R2" %in% names(model)) model$R2[[1]] else NA_real_
      p_perm <- if (nrow(model) && "Pr(>F)" %in% names(model)) model[["Pr(>F)"]][[1]] else NA_real_
      p_disp <- if (is.data.frame(disp) && nrow(disp) && "p_value" %in% names(disp)) disp$p_value[[1]] else NA_real_
      fmt <- function(x, digits = 3) if (!is.finite(x)) "—" else if (x < 0.001) "<0.001" else formatC(x, digits = digits, format = "f")

      shiny::tagList(
        shiny::tags$div(
          class = "kkai-results-summary",
          shiny::tags$div(shiny::tags$b("样本数："), if (is.data.frame(coords)) nrow(coords) else "—"),
          shiny::tags$div(shiny::tags$b("PERMANOVA R²："), fmt(r2)),
          shiny::tags$div(shiny::tags$b("PERMANOVA p："), fmt(p_perm)),
          shiny::tags$div(shiny::tags$b("PERMDISP p："), fmt(p_disp))
        ),
        if (is.finite(p_disp) && p_disp < 0.05) {
          shiny::tags$div(
            class = "kkai-alert kkai-alert--warning",
            "PERMDISP 显著：各组离散度并不一致。PERMANOVA 的显著性可能同时受到组中心和组内变异程度影响，请谨慎表述。"
          )
        }
      )
    })

    output$view_control <- shiny::renderUI({
      views <- available_views()
      if (length(views) == 0) return(NULL)
      selected <- if ("ellipse_centroid" %in% views) "ellipse_centroid" else views[[1]]
      shiny::tags$div(
        class = "kkai-control",
        shiny::selectInput(
          session$ns("view"), "选择论文图形",
          choices = stats::setNames(views, unname(view_labels[views])), selected = selected
        )
      )
    })

    output$plot_ui <- shiny::renderUI({
      views <- available_views()
      if (length(views) == 0 || is.null(fig_prefix())) return(shiny::helpText("尚未生成 Beta 图形。"))
      view <- input$view %||% if ("ellipse_centroid" %in% views) "ellipse_centroid" else views[[1]]
      if (!view %in% views) view <- views[[1]]
      full <- beta_figure_path(state$job_dir, view, "png", existing = TRUE)
      root <- normalizePath(beta_figure_root(), winslash = "/", mustWork = TRUE)
      full_norm <- normalizePath(full, winslash = "/", mustWork = TRUE)
      rel <- substring(full_norm, nchar(root) + 2L)
      src <- paste0(fig_prefix(), "/", rel)

      shiny::tagList(
        shiny::tags$p(class = "kkai-muted", paste0(view_labels[[view]], "。点击图形可查看原尺寸。")),
        shiny::tags$a(
          href = src, target = "_blank",
          shiny::tags$div(
            class = "kkai-result-image-wrap",
            shiny::tags$img(src = src, class = "kkai-result-img kkai-result-img--fit")
          )
        )
      )
    })

    output$permanova_table <- DT::renderDT({
      df <- permanova()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "尚未生成 PERMANOVA 结果。"), rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE))
    })

    output$dispersion_table <- DT::renderDT({
      df <- dispersion()
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "旧任务未包含 PERMDISP 结果。"), rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE))
    })
  })
}
