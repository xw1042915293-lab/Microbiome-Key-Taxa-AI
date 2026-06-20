# Key Taxa Hub page: multi-evidence key taxa screening center
# Displays Top20 barplot, UpSet/evidence intersection, evidence heatmap,
# score table, evidence cards, and AI summary JSON.

mod_key_taxa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("Key Taxa Hub"),
    shiny::p("Multi-evidence comprehensive key taxa screening center."),

    bslib::navset_tab(
      bslib::nav_panel("Top 20 Barplot",
        shiny::uiOutput(ns("score_plot_ui"))
      ),
      bslib::nav_panel("Evidence Intersection",
        shiny::uiOutput(ns("upset_plot_ui"))
      ),
      bslib::nav_panel("Evidence Heatmap",
        shiny::uiOutput(ns("heatmap_ui"))
      ),
      bslib::nav_panel("Score Table",
        shiny::uiOutput(ns("score_table_ui"))
      ),
      bslib::nav_panel("Evidence Cards",
        shiny::uiOutput(ns("cards_ui"))
      ),
      bslib::nav_panel("AI Summary",
        shiny::uiOutput(ns("summary_ui"))
      )
    )
  )
}

mod_key_taxa_server <- function(id, state) {
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

    # Helper to show file or friendly missing message
    show_image_or_msg <- function(filename, title) {
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info",
          "No active task. Please run an analysis first."))
      }
      full <- file.path(state$job_dir, "figures", filename)
      if (!file.exists(full)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info",
          shiny::tags$b(paste0(title, " not found.")),
          shiny::tags$div("Please run the Key Taxa analysis first.")))
      }
      bslib::card(class = "kkai-card",
        bslib::card_header(title),
        shiny::tags$div(class = "kkai-result-image-wrap",
          shiny::tags$img(src = file.path(fig_prefix(), filename), class = "kkai-result-img"))
      )
    }

    show_table_or_msg <- function(filename, message_text) {
      if (is.null(state$job_dir)) {
        return(DT::datatable(data.frame(Message = "No active task."), rownames = FALSE, options = list(dom = "t")))
      }
      p <- file.path(state$job_dir, "tables", filename)
      if (!file.exists(p)) {
        return(DT::datatable(data.frame(Message = message_text), rownames = FALSE, options = list(dom = "t")))
      }
      df <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(DT::datatable(data.frame(Message = paste(filename, "is empty.")), rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))
    }

    # Tab 1: Top 20 barplot
    output$score_plot_ui <- shiny::renderUI({
      show_image_or_msg("key_taxa_score_barplot.png", "Top 20 Key Taxa Score Barplot")
    })

    # Tab 2: UpSet / evidence intersection
    output$upset_plot_ui <- shiny::renderUI({
      show_image_or_msg("key_taxa_upset.png", "Evidence Intersection Plot")
    })

    # Tab 3: Evidence heatmap
    output$heatmap_ui <- shiny::renderUI({
      show_image_or_msg("key_taxa_evidence_heatmap.png", "Evidence Heatmap")
    })

    # Tab 4: Score table
    output$score_table_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("Key Taxa Score Table"),
        DT::DTOutput(session$ns("score_tbl"))
      )
    })
    output$score_tbl <- DT::renderDT({
      show_table_or_msg("key_taxa_score.csv", "No key_taxa_score.csv found.")
    })

    # Tab 5: Evidence cards
    output$cards_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("Candidate Key Taxa Evidence Cards"),
        DT::DTOutput(session$ns("cards_tbl"))
      )
    })
    output$cards_tbl <- DT::renderDT({
      show_table_or_msg("key_taxa_evidence_cards.csv", "No key_taxa_evidence_cards.csv found.")
    })

    # Tab 6: AI summary JSON
    output$summary_ui <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "No active task."))
      }
      p <- file.path(state$job_dir, "json", "key_taxa_summary.json")
      if (!file.exists(p)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info",
          shiny::tags$b("key_taxa_summary.json not found."),
          shiny::tags$div("Expected: ", shiny::tags$code("json/key_taxa_summary.json"))))
      }
      x <- tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE), error = function(e) NULL)
      if (!is.list(x)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "Failed to read key_taxa_summary.json."))
      }

      # Build summary display
      hc <- x$high_confidence_count %||% 0
      mc <- x$moderate_confidence_count %||% 0
      ex <- x$exploratory_count %||% 0
      we <- x$weak_evidence_count %||% 0

      caution_ui <- if (!is.null(x$caution_notes) && length(x$caution_notes) > 0) {
        shiny::tags$ul(lapply(x$caution_notes, function(n) shiny::tags$li(n)))
      } else {
        shiny::tags$p("No caution notes available.")
      }

      top_taxa_ui <- if (!is.null(x$top_key_taxa) && length(x$top_key_taxa) > 0) {
        top_df <- tryCatch({
          rows <- lapply(x$top_key_taxa, function(t) {
            data.frame(
              Taxon = as.character(t$display_taxon %||% t$taxon),
              Score = round(as.numeric(t$key_taxa_score), 4),
              Evidence = as.integer(t$evidence_count),
              Reliability = as.character(t$reliability_label),
              stringsAsFactors = FALSE
            )
          })
          do.call(rbind, rows)
        }, error = function(e) NULL)
        if (!is.null(top_df)) DT::datatable(top_df, rownames = FALSE, options = list(dom = "t", pageLength = 10))
        else shiny::tags$p("Top taxa data unavailable.")
      } else {
        shiny::tags$p("No top key taxa available.")
      }

      bslib::card(class = "kkai-card",
        bslib::card_header("Key Taxa AI Summary"),
        shiny::tags$div(
          shiny::tags$div(shiny::tags$b("Analysis type: "), shiny::code(x$analysis_type %||% "key_taxa_score")),
          shiny::tags$div(shiny::tags$b("Scoring formula: "), shiny::tags$code(x$scoring_formula %||% "N/A")),
          shiny::tags$div(shiny::tags$b("Available evidence: "),
            shiny::code(paste(x$available_evidence_modules %||% "none", collapse = ", "))),
          shiny::tags$div(shiny::tags$b("Missing evidence: "),
            shiny::code(paste(x$missing_evidence_modules %||% "none", collapse = ", "))),
          shiny::tags$hr(),
          shiny::tags$div(shiny::tags$b("Confidence counts:"),
            shiny::tags$ul(
              shiny::tags$li("High confidence: ", shiny::tags$b(as.character(hc))),
              shiny::tags$li("Moderate confidence: ", shiny::tags$b(as.character(mc))),
              shiny::tags$li("Exploratory: ", shiny::tags$b(as.character(ex))),
              shiny::tags$li("Weak evidence: ", shiny::tags$b(as.character(we)))
            )
          ),
          shiny::tags$hr(),
          shiny::tags$h5("Top Key Taxa"),
          top_taxa_ui,
          shiny::tags$hr(),
          shiny::tags$h5("Caution Notes"),
          caution_ui
        )
      )
    })
  })
}
