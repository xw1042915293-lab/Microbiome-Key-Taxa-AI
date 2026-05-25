# Quarto HTML report rendering (Phase 8).
# Renders templates/report_template.qmd into job_dir/report/report.html.
# Phase 8 must NOT call any LLM API and must NOT introduce new analysis methods.

render_report_html <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("render_report_html(): job_dir not found: ", job_dir, call. = FALSE)

  # Prepare report context (paths + existence checks). This is pure orchestration.
  source("R/report_prepare.R", local = TRUE)
  ctx <- prepare_report_context(job_dir)

  template <- normalizePath("templates/report_template.qmd", winslash = "/", mustWork = TRUE)
  out_dir <- ensure_dir(file.path(job_dir, "report"))

  quarto::quarto_render(
    input = template,
    output_format = "html",
    execute_params = list(
      job_dir = job_dir,
      report_ctx = ctx
    ),
    quiet = TRUE
  )

  rendered <- file.path(dirname(template), "report_template.html")
  if (!file.exists(rendered)) {
    stop("render_report_html(): Quarto did not produce report_template.html", call. = FALSE)
  }

  dest <- file.path(out_dir, "report.html")
  file.copy(rendered, dest, overwrite = TRUE)
  normalizePath(dest, winslash = "/", mustWork = TRUE)
}
