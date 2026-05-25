# 上传模块：创建任务并将文件保存到 results/job_xxx/input/。

mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h3("上传数据"),
    shiny::p("上传丰度表、样本信息表和物种注释表，文件会保存到新的任务目录。"),
    shiny::fileInput(ns("abundance"), "丰度表（tsv/csv/txt）", accept = c(".tsv", ".csv", ".txt")),
    shiny::fileInput(ns("metadata"), "样本信息表（tsv/csv/txt）", accept = c(".tsv", ".csv", ".txt")),
    shiny::fileInput(ns("taxonomy"), "物种注释表（tsv/csv/txt）", accept = c(".tsv", ".csv", ".txt")),
    shiny::actionButton(ns("create_job"), "创建任务并保存文件", class = "btn-primary"),
    shiny::hr(),
    shiny::verbatimTextOutput(ns("job_info"))
  )
}

mod_upload_server <- function(id, state) {
  # NOTE: `missing(state)` can only be used in the function that defines `state`
  # (not inside the nested moduleServer function), otherwise Shiny throws:
  # "'missing(state)' did not find an argument".
  if (missing(state) || (!inherits(state, "reactivevalues") && !is.environment(state))) {
    stop("mod_upload_server(): 'state' must be a reactiveValues object.", call. = FALSE)
  }

  shiny::moduleServer(id, function(input, output, session) {

    output$job_info <- shiny::renderText({
      if (is.null(state$job_id)) return("尚无活动任务。")
      paste0("任务 ID：", state$job_id, "\n任务目录：", state$job_dir)
    })

    shiny::observeEvent(input$create_job, {
      shiny::req(input$abundance, input$metadata, input$taxonomy)

      job_dir <- create_job_dir()
      job_id <- basename(job_dir)
      state$job_id <- job_id
      state$job_dir <- job_dir
      workflow_set_status(state, "job_created")

      # Persist inputs to job_dir/input/
      paths <- list(
        abundance = file.path(job_dir, "input", "abundance.tsv"),
        metadata = file.path(job_dir, "input", "metadata.tsv"),
        taxonomy = file.path(job_dir, "input", "taxonomy.tsv")
      )

      abund_saved <- copy_to_job_input(input$abundance$datapath, paths$abundance)
      meta_saved <- copy_to_job_input(input$metadata$datapath, paths$metadata)
      tax_saved <- copy_to_job_input(input$taxonomy$datapath, paths$taxonomy)

      state$input_paths <- list(
        abundance_path = abund_saved,
        metadata_path = meta_saved,
        taxonomy_path = tax_saved
      )

      # Record reproducibility basics + file MD5 + DB rows
      append_reproducibility(job_dir, list(
        job_id = job_id,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        input_files = list(
          abundance = list(path = "input/abundance.tsv", md5 = file_md5(abund_saved), original_name = input$abundance$name),
          metadata = list(path = "input/metadata.tsv", md5 = file_md5(meta_saved), original_name = input$metadata$name),
          taxonomy = list(path = "input/taxonomy.tsv", md5 = file_md5(tax_saved), original_name = input$taxonomy$name)
        )
      ))

      db_upsert_job(job_id = job_id, job_dir = job_dir, status = "inputs_saved")
      db_insert_job_file(job_id, "abundance", input$abundance$name, "input/abundance.tsv", file_md5(abund_saved))
      db_insert_job_file(job_id, "metadata", input$metadata$name, "input/metadata.tsv", file_md5(meta_saved))
      db_insert_job_file(job_id, "taxonomy", input$taxonomy$name, "input/taxonomy.tsv", file_md5(tax_saved))

      shiny::showNotification("任务已创建，文件已保存到 results/。", type = "message")
    })
  })
}
