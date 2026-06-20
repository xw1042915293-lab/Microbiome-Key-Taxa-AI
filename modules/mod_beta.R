mod_beta_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h3("Beta Diversity"),
    shiny::p("展示当前任务目录中已生成的 Beta diversity 结果，包括 ordination、群落差异检验、距离结构与 AI 摘要。"),
    shiny::tags$div(
      class = "kkai-alert kkai-alert--info",
      "页面只读取当前 job_dir 中已经保存的文件；若某个结果尚未生成，会显示友好提示而不是报错。"
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Ordination",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            class = "kkai-card",
            bslib::card_header("PCoA"),
            shiny::uiOutput(ns("pcoa_ui"))
          ),
          bslib::card(
            class = "kkai-card",
            bslib::card_header("NMDS"),
            shiny::uiOutput(ns("nmds_meta_ui")),
            shiny::uiOutput(ns("nmds_ui"))
          )
        )
      ),
      bslib::nav_panel(
        "Statistics",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(class = "kkai-card", bslib::card_header("PERMANOVA"), DT::DTOutput(ns("permanova_dt"))),
          bslib::card(class = "kkai-card", bslib::card_header("Beta Dispersion"), DT::DTOutput(ns("dispersion_dt"))),
          bslib::card(class = "kkai-card", bslib::card_header("ANOSIM"), DT::DTOutput(ns("anosim_dt"))),
          bslib::card(class = "kkai-card", bslib::card_header("NMDS Summary"), DT::DTOutput(ns("nmds_summary_dt")))
        )
      ),
      bslib::nav_panel(
        "Distance Structure",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            class = "kkai-card",
            bslib::card_header("Distance Heatmap"),
            shiny::uiOutput(ns("heatmap_ui"))
          ),
          bslib::card(
            class = "kkai-card",
            bslib::card_header("Within vs Between Distances"),
            shiny::uiOutput(ns("within_between_ui")),
            DT::DTOutput(ns("within_between_stats_dt"))
          )
        ),
        shiny::tags$details(
          shiny::tags$summary("Pairwise distance table"),
          DT::DTOutput(ns("within_between_pairs_dt"))
        )
      ),
      bslib::nav_panel(
        "AI Summary",
        shiny::uiOutput(ns("beta_ai_ui")),
        shiny::tags$details(
          open = "open",
          shiny::tags$summary("beta_summary.json"),
          shiny::verbatimTextOutput(ns("beta_json_preview"))
        )
      )
    )
  )
}

mod_beta_server <- function(id, state) {
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

    safe_read_json <- function(path) {
      if (is.null(path) || !is.character(path) || length(path) != 1 || !file.exists(path)) return(NULL)
      tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
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
        options = list(pageLength = 8, autoWidth = TRUE, scrollX = TRUE)
      )
    }

    render_image_or_message <- function(filename, empty_message) {
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务。"))
      }

      full_path <- file.path(state$job_dir, "figures", filename)
      if (!file.exists(full_path)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", empty_message))
      }

      shiny::tags$div(
        class = "kkai-result-image-wrap",
        style = "border: 1px solid rgba(148, 163, 184, 0.25); border-radius: 14px; padding: 0.75rem; background: #fff;",
        shiny::tags$img(
          src = file.path(fig_prefix(), filename),
          class = "kkai-result-img kkai-result-img--fit",
          style = "max-width: 100%; height: auto;"
        )
      )
    }

    beta_summary <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      safe_read_json(file.path(state$job_dir, "json", "beta_summary.json"))
    })

    beta_distance_slug_rx <- shiny::reactive({
      summary <- beta_summary()
      beta_distance_slug(summary$distance_method %||% state$parameters$beta_distance %||% "bray")
    })

    beta_distance_label_rx <- shiny::reactive({
      summary <- beta_summary()
      beta_distance_label(summary$distance_method %||% state$parameters$beta_distance %||% "bray")
    })

    shiny::observe({
      shiny::req(state$job_dir, fig_prefix())
      safe_add_resource_path(fig_prefix(), file.path(state$job_dir, "figures"))
    })

    output$pcoa_ui <- shiny::renderUI({
      render_image_or_message(
        filename = paste0("beta_pcoa_", beta_distance_slug_rx(), ".png"),
        empty_message = "PCoA 图尚未生成，请先运行 Beta diversity 分析。"
      )
    })

    output$nmds_meta_ui <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务。"))
      }
      nmds_summary <- safe_read_csv(file.path(state$job_dir, "tables", "beta_nmds_summary.csv"))
      if (is.null(nmds_summary) || !is.data.frame(nmds_summary) || nrow(nmds_summary) < 1) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "NMDS summary 尚未生成。"))
      }

      stress <- if ("stress" %in% names(nmds_summary)) suppressWarnings(as.numeric(nmds_summary$stress[[1]])) else NA_real_
      message <- if ("message" %in% names(nmds_summary)) as.character(nmds_summary$message[[1]]) else ""

      shiny::tags$div(
        class = "kkai-alert kkai-alert--info",
        shiny::tags$div(shiny::tags$b("Distance:"), " ", shiny::tags$code(beta_distance_label_rx())),
        shiny::tags$div(shiny::tags$b("Stress:"), " ", if (is.finite(stress)) formatC(stress, digits = 3, format = "f") else "NA"),
        if (nzchar(message)) shiny::tags$div(shiny::tags$b("Note:"), " ", message) else NULL
      )
    })

    output$nmds_ui <- shiny::renderUI({
      render_image_or_message(
        filename = paste0("beta_nmds_", beta_distance_slug_rx(), ".png"),
        empty_message = "NMDS 图尚未生成，或者当前数据不满足 NMDS 计算条件。"
      )
    })

    output$heatmap_ui <- shiny::renderUI({
      render_image_or_message(
        filename = paste0("beta_distance_heatmap_", beta_distance_slug_rx(), ".png"),
        empty_message = "Distance heatmap 尚未生成。"
      )
    })

    output$within_between_ui <- shiny::renderUI({
      render_image_or_message(
        filename = "beta_within_between_boxplot.png",
        empty_message = "Within vs between distance boxplot 尚未生成。"
      )
    })

    output$permanova_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "当前没有活动任务。"))
      render_table_or_message(
        file.path(state$job_dir, "tables", "beta_permanova.csv"),
        "beta_permanova.csv 尚未生成。"
      )
    })

    output$dispersion_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "当前没有活动任务。"))
      render_table_or_message(
        file.path(state$job_dir, "tables", "beta_dispersion.csv"),
        "beta_dispersion.csv 尚未生成。"
      )
    })

    output$anosim_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "当前没有活动任务。"))
      render_table_or_message(
        file.path(state$job_dir, "tables", "beta_anosim.csv"),
        "beta_anosim.csv 尚未生成。"
      )
    })

    output$nmds_summary_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "当前没有活动任务。"))
      render_table_or_message(
        file.path(state$job_dir, "tables", "beta_nmds_summary.csv"),
        "beta_nmds_summary.csv 尚未生成。"
      )
    })

    output$within_between_stats_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "当前没有活动任务。"))
      render_table_or_message(
        file.path(state$job_dir, "tables", "beta_within_between_stats.csv"),
        "beta_within_between_stats.csv 尚未生成。"
      )
    })

    output$within_between_pairs_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "当前没有活动任务。"))
      render_table_or_message(
        file.path(state$job_dir, "tables", "beta_within_between_distance.csv"),
        "beta_within_between_distance.csv 尚未生成。"
      )
    })

    output$beta_ai_ui <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务。"))
      }

      beta_md <- file.path(state$job_dir, "ai", "beta_interpretation.md")
      if (file.exists(beta_md)) {
        return(
          bslib::card(
            class = "kkai-card",
            bslib::card_header("Local Beta Interpretation"),
            shiny::includeMarkdown(beta_md)
          )
        )
      }

      if (!file.exists(file.path(state$job_dir, "json", "beta_summary.json"))) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "beta_summary.json 尚未生成。"))
      }

      shiny::tags$div(
        class = "kkai-alert kkai-alert--info",
        "未找到 beta_interpretation.md，下面提供 beta_summary.json 预览。"
      )
    })

    output$beta_json_preview <- shiny::renderText({
      if (is.null(state$job_dir)) return("当前没有活动任务。")
      json_path <- file.path(state$job_dir, "json", "beta_summary.json")
      if (!file.exists(json_path)) return("beta_summary.json 尚未生成。")
      out <- tryCatch(jsonlite::prettify(paste(readLines(json_path, warn = FALSE), collapse = "\n")), error = function(e) NULL)
      out %||% paste(readLines(json_path, warn = FALSE), collapse = "\n")
    })
  })
}
