mod_diff_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h3("Differential Abundance"),
    shiny::p("Display saved differential abundance outputs from the current job directory, including statistical tables, plots, optional multi-method results, and AI-ready summaries."),
    shiny::tags$div(
      class = "kkai-alert kkai-alert--info",
      "This page only reads files already written under job_dir. Missing files are shown as friendly notices instead of errors."
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Figures",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            class = "kkai-card",
            bslib::card_header("Differential Taxa Barplot"),
            shiny::uiOutput(ns("barplot_meta_ui")),
            shiny::uiOutput(ns("barplot_ui"))
          ),
          bslib::card(
            class = "kkai-card",
            bslib::card_header("Volcano Plot"),
            shiny::uiOutput(ns("volcano_ui"))
          ),
          bslib::card(
            class = "kkai-card",
            bslib::card_header("Differential Heatmap"),
            shiny::uiOutput(ns("heatmap_ui"))
          ),
          bslib::card(
            class = "kkai-card",
            bslib::card_header("LEfSe LDA Plot"),
            shiny::uiOutput(ns("lefse_plot_ui"))
          )
        )
      ),
      bslib::nav_panel(
        "Statistics",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(class = "kkai-card", bslib::card_header("Differential Taxa"), DT::DTOutput(ns("diff_dt"))),
          bslib::card(class = "kkai-card", bslib::card_header("Effect Size"), DT::DTOutput(ns("effect_dt"))),
          bslib::card(class = "kkai-card", bslib::card_header("Consensus"), DT::DTOutput(ns("consensus_dt"))),
          bslib::card(class = "kkai-card", bslib::card_header("Heatmap Matrix"), DT::DTOutput(ns("heatmap_matrix_dt")))
        )
      ),
      bslib::nav_panel(
        "Optional Methods",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            class = "kkai-card",
            bslib::card_header("LEfSe"),
            shiny::uiOutput(ns("lefse_notice_ui")),
            DT::DTOutput(ns("lefse_dt"))
          ),
          bslib::card(
            class = "kkai-card",
            bslib::card_header("ANCOM-BC"),
            shiny::uiOutput(ns("ancombc_notice_ui")),
            DT::DTOutput(ns("ancombc_dt"))
          )
        )
      ),
      bslib::nav_panel(
        "AI Summary",
        shiny::uiOutput(ns("summary_card_ui")),
        shiny::tags$details(
          open = "open",
          shiny::tags$summary("diff_summary.json"),
          shiny::verbatimTextOutput(ns("diff_json_preview"))
        )
      )
    )
  )
}

mod_diff_server <- function(id, state) {
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

    render_table_or_message <- function(path, empty_message, page_length = 8) {
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
        options = list(pageLength = page_length, autoWidth = TRUE, scrollX = TRUE)
      )
    }

    render_image_or_message <- function(filename, empty_message) {
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "No active job is selected."))
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

    diff_summary <- shiny::reactive({
      if (is.null(state$job_dir)) return(NULL)
      safe_read_json(file.path(state$job_dir, "json", "diff_summary.json"))
    })

    shiny::observe({
      shiny::req(state$job_dir, fig_prefix())
      safe_add_resource_path(fig_prefix(), file.path(state$job_dir, "figures"))
    })

    output$barplot_meta_ui <- shiny::renderUI({
      summary_obj <- diff_summary()
      if (is.null(summary_obj)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "diff_summary.json is not available yet."))
      }

      n_sig <- summary_obj$n_significant_taxa %||% 0
      n_trend <- summary_obj$n_trend_taxa %||% 0
      note <- if (n_sig > 0) {
        paste0("FDR-significant taxa detected: ", n_sig, ". Trend taxa: ", n_trend, ".")
      } else {
        "No FDR-significant taxa were detected; the displayed taxa are exploratory only."
      }

      shiny::tags$div(
        class = if (n_sig > 0) "kkai-alert kkai-alert--success" else "kkai-alert kkai-alert--warning",
        shiny::tags$div(note),
        if (length(summary_obj$reliability_notes %||% character(0)) > 0) {
          shiny::tags$div(
            style = "margin-top: 0.4rem;",
            shiny::tags$b("Reliability notes:"),
            shiny::tags$ul(lapply(summary_obj$reliability_notes, shiny::tags$li))
          )
        } else NULL
      )
    })

    output$barplot_ui <- shiny::renderUI({
      render_image_or_message(
        filename = "diff_taxa_barplot.png",
        empty_message = "diff_taxa_barplot.png has not been generated yet."
      )
    })

    output$volcano_ui <- shiny::renderUI({
      render_image_or_message(
        filename = "diff_volcano.png",
        empty_message = "diff_volcano.png has not been generated yet."
      )
    })

    output$heatmap_ui <- shiny::renderUI({
      render_image_or_message(
        filename = "diff_heatmap.png",
        empty_message = "diff_heatmap.png has not been generated yet."
      )
    })

    output$lefse_plot_ui <- shiny::renderUI({
      render_image_or_message(
        filename = "diff_lefse_lda.png",
        empty_message = "LEfSe LDA plot is unavailable for this job."
      )
    })

    output$diff_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "No active job is selected."))
      render_table_or_message(
        file.path(state$job_dir, "tables", "differential_taxa.csv"),
        "differential_taxa.csv is not available."
      )
    })

    output$effect_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "No active job is selected."))
      render_table_or_message(
        file.path(state$job_dir, "tables", "diff_effect_size.csv"),
        "diff_effect_size.csv is not available."
      )
    })

    output$consensus_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "No active job is selected."))
      render_table_or_message(
        file.path(state$job_dir, "tables", "diff_consensus.csv"),
        "diff_consensus.csv is not available."
      )
    })

    output$heatmap_matrix_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "No active job is selected.", page_length = 6))
      render_table_or_message(
        file.path(state$job_dir, "tables", "diff_heatmap_matrix.csv"),
        "diff_heatmap_matrix.csv is not available.",
        page_length = 6
      )
    })

    output$lefse_notice_ui <- shiny::renderUI({
      summary_obj <- diff_summary()
      if (is.null(summary_obj)) return(NULL)
      if (isTRUE(summary_obj$lefse_available)) return(NULL)
      shiny::tags$div(class = "kkai-alert kkai-alert--info", "LEfSe was not available for this run or did not complete successfully.")
    })

    output$ancombc_notice_ui <- shiny::renderUI({
      summary_obj <- diff_summary()
      if (is.null(summary_obj)) return(NULL)
      if (isTRUE(summary_obj$ancombc_available)) return(NULL)
      shiny::tags$div(class = "kkai-alert kkai-alert--info", "ANCOM-BC was not available for this run.")
    })

    output$lefse_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "No active job is selected."))
      render_table_or_message(
        file.path(state$job_dir, "tables", "diff_lefse.csv"),
        "diff_lefse.csv is not available."
      )
    })

    output$ancombc_dt <- DT::renderDT({
      if (is.null(state$job_dir)) return(render_table_or_message(NULL, "No active job is selected."))
      render_table_or_message(
        file.path(state$job_dir, "tables", "diff_ancombc.csv"),
        "diff_ancombc.csv is not available."
      )
    })

    output$summary_card_ui <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "No active job is selected."))
      }

      summary_obj <- diff_summary()
      if (is.null(summary_obj)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "diff_summary.json has not been generated yet."))
      }

      caution_notes <- summary_obj$caution_notes %||% character(0)
      reliability_notes <- summary_obj$reliability_notes %||% character(0)

      bslib::card(
        class = "kkai-card",
        bslib::card_header("AI-ready differential summary"),
        shiny::tags$div(shiny::tags$b("Group variable:"), " ", shiny::tags$code(summary_obj$group_variable %||% "NA")),
        shiny::tags$div(shiny::tags$b("Tax level:"), " ", shiny::tags$code(summary_obj$tax_level %||% "NA")),
        shiny::tags$div(shiny::tags$b("Primary test:"), " ", shiny::tags$code(summary_obj$test_method %||% summary_obj$method %||% "NA")),
        shiny::tags$div(shiny::tags$b("Total taxa tested:"), " ", summary_obj$n_total_taxa %||% "NA"),
        shiny::tags$div(shiny::tags$b("FDR-significant taxa:"), " ", summary_obj$n_significant_taxa %||% "NA"),
        shiny::tags$div(shiny::tags$b("Trend taxa:"), " ", summary_obj$n_trend_taxa %||% "NA"),
        shiny::tags$div(shiny::tags$b("Exploratory taxa shown:"), " ", summary_obj$n_exploratory_taxa %||% "NA"),
        shiny::tags$div(shiny::tags$b("LEfSe available:"), " ", if (isTRUE(summary_obj$lefse_available)) "Yes" else "No"),
        shiny::tags$div(shiny::tags$b("ANCOM-BC available:"), " ", if (isTRUE(summary_obj$ancombc_available)) "Yes" else "No"),
        shiny::tags$div(shiny::tags$b("Consensus available:"), " ", if (isTRUE(summary_obj$consensus_available)) "Yes" else "No"),
        if (length(reliability_notes) > 0) {
          shiny::tags$div(
            style = "margin-top: 0.75rem;",
            shiny::tags$b("Reliability notes"),
            shiny::tags$ul(lapply(reliability_notes, shiny::tags$li))
          )
        } else NULL,
        if (length(caution_notes) > 0) {
          shiny::tags$div(
            style = "margin-top: 0.75rem;",
            shiny::tags$b("Caution notes"),
            shiny::tags$ul(lapply(caution_notes, shiny::tags$li))
          )
        } else NULL
      )
    })

    output$diff_json_preview <- shiny::renderText({
      if (is.null(state$job_dir)) return("No active job is selected.")
      json_path <- file.path(state$job_dir, "json", "diff_summary.json")
      if (!file.exists(json_path)) return("diff_summary.json has not been generated yet.")
      out <- tryCatch(jsonlite::prettify(paste(readLines(json_path, warn = FALSE), collapse = "\n")), error = function(e) NULL)
      out %||% paste(readLines(json_path, warn = FALSE), collapse = "\n")
    })
  })
}
