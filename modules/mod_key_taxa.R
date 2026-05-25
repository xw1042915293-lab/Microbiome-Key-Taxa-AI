# 关键菌评分结果页：预览当前任务中的关键菌结果。

mod_key_taxa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("关键菌评分"),
    shiny::p("仅展示当前任务目录中已生成的关键菌评分结果。"),
    shiny::uiOutput(ns("score_plot")),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("前 20 个结果"),
      DT::DTOutput(ns("top20_tbl"))
    ),
    shiny::tags$div(style = "margin-top: 1rem;"),
    shiny::uiOutput(ns("summary_card")),
    shiny::tags$div(style = "margin-top: 1rem;"),
    bslib::card(
      class = "kkai-card",
      bslib::card_header("关键菌评分公式"),
      shiny::tags$pre(
        class = "kkai-codeblock",
        paste(
          "Let d, m, n be normalized sub-scores in [0, 1]:",
          "  d = differential_score",
          "  m = ml_importance_score",
          "  n = network_centrality_score",
          "",
          "Let weights be (w_diff, w_ml, w_network), and let I_d/I_m/I_n indicate whether a taxon has that evidence (finite value).",
          "",
          "KeyTaxaScore = (I_d*d*w_diff + I_m*m*w_ml + I_n*n*w_network) / (I_d*w_diff + I_m*w_ml + I_n*w_network)",
          "",
          "Note: the denominator uses only available evidence per taxon (missing evidence does not force NA).",
          sep = "\n"
        )
      ),
      shiny::tags$details(
        shiny::tags$summary("详情"),
        shiny::tags$p("该页仅用于查看当前任务的关键菌输出。")
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

    output$score_plot <- shiny::renderUI({
      if (is.null(state$job_dir)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "当前没有活动任务，请先运行分析。"))
      }

      png_file <- "key_taxa_score_barplot.png"
      full <- file.path(state$job_dir, "figures", png_file)
      if (!file.exists(full)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("未找到关键菌评分图。"),
            shiny::tags$div("请先生成工作流输出。")
          )
        )
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("关键菌评分图"),
        shiny::tags$div(class = "kkai-result-image-wrap",
          shiny::tags$img(src = file.path(fig_prefix(), png_file), class = "kkai-result-img kkai-result-img--small")
        )
      )
    })

    output$top20_tbl <- DT::renderDT({
      if (is.null(state$job_dir)) {
        return(DT::datatable(data.frame(Message = "当前没有活动任务。"), rownames = FALSE, options = list(dom = "t")))
      }
      p <- file.path(state$job_dir, "tables", "key_taxa_top20.csv")
      if (!file.exists(p)) {
        return(DT::datatable(data.frame(Message = "未找到 tables/key_taxa_top20.csv。"), rownames = FALSE, options = list(dom = "t")))
      }
      df <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) {
        return(DT::datatable(data.frame(Message = "key_taxa_top20.csv 中没有数据。"), rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, dom = "tip", autoWidth = TRUE))
    })

    output$summary_card <- shiny::renderUI({
      if (is.null(state$job_dir)) return(NULL)
      p <- file.path(state$job_dir, "json", "key_taxa_summary.json")
      if (!file.exists(p)) {
        return(
          shiny::tags$div(
            class = "kkai-alert kkai-alert--info",
            shiny::tags$b("未找到 key_taxa_summary.json。"),
            shiny::tags$div("预期路径：", shiny::tags$code(file.path("json", "key_taxa_summary.json")))
          )
        )
      }

      x <- tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE), error = function(e) NULL)
      if (!is.list(x)) {
        return(shiny::tags$div(class = "kkai-alert kkai-alert--info", "读取 key_taxa_summary.json 失败。"))
      }

      used_sources <- x$used_sources %||% character(0)
      weights <- x$weights %||% list()
      reliability <- x$reliability %||% NA_character_

      fmt_weights <- function(w) {
        if (is.null(w)) return(character(0))
        if (is.atomic(w) && !is.null(names(w))) {
          return(sprintf("%s = %s", names(w), as.character(w)))
        }
        if (is.list(w) && length(w) > 0) {
          nms <- names(w) %||% rep("", length(w))
          return(mapply(function(nm, val) sprintf("%s = %s", nm, as.character(val)), nms, w, USE.NAMES = FALSE))
        }
        character(0)
      }

      bslib::card(
        class = "kkai-card",
        bslib::card_header("摘要"),
        shiny::tags$div(
          class = "kkai-kv",
          shiny::tags$div(shiny::tags$b("使用来源："), shiny::tags$code(paste(used_sources, collapse = ", "))),
          shiny::tags$div(shiny::tags$b("权重："), shiny::tags$code(paste(fmt_weights(weights), collapse = "; "))),
          shiny::tags$div(shiny::tags$b("可靠性："), shiny::tags$code(as.character(reliability)))
        )
      )
    })
  })
}
