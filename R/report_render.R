# Quarto HTML report rendering (Phase 8).
# Renders templates/report_template.qmd into job_dir/report/report.html.
# Phase 8 must NOT call any LLM API and must NOT introduce new analysis methods.

.prepare_report_render <- function(job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop(".prepare_report_render(): job_dir not found: ", job_dir, call. = FALSE)
  job_dir <- normalizePath(job_dir, winslash = "/", mustWork = TRUE)

  source("R/report_prepare.R", local = TRUE)
  ctx <- prepare_report_context(job_dir)
  template <- normalizePath("templates/report_template.qmd", winslash = "/", mustWork = TRUE)
  out_dir <- ensure_dir(file.path(job_dir, "report"))

  list(job_dir = job_dir, ctx = ctx, template = template, out_dir = out_dir)
}

.copy_report_template <- function(template, out_dir, stem) {
  assert_non_empty_string(template, "template")
  assert_non_empty_string(out_dir, "out_dir")
  assert_non_empty_string(stem, "stem")

  temp_template <- file.path(out_dir, paste0(stem, ".qmd"))
  ok <- file.copy(template, temp_template, overwrite = TRUE)
  if (!isTRUE(ok) || !file.exists(temp_template)) {
    stop(".copy_report_template(): failed to copy template into ", out_dir, call. = FALSE)
  }

  temp_template
}

.with_quarto_runtime_env <- function(job_dir, expr) {
  assert_non_empty_string(job_dir, "job_dir")
  runtime_root <- ensure_dir(file.path(job_dir, ".quarto_runtime"))
  local_appdata <- ensure_dir(file.path(runtime_root, "AppData", "Local"))
  roaming_appdata <- ensure_dir(file.path(runtime_root, "AppData", "Roaming"))
  home_dir <- ensure_dir(file.path(runtime_root, "home"))
  xdg_cache_home <- ensure_dir(file.path(runtime_root, ".cache"))
  xdg_data_home <- ensure_dir(file.path(runtime_root, ".local", "share"))

  env_names <- c("LOCALAPPDATA", "APPDATA", "HOME", "XDG_CACHE_HOME", "XDG_DATA_HOME")
  old_env <- Sys.getenv(env_names, unset = NA_character_)
  restore_env <- function() {
    for (i in seq_along(env_names)) {
      val <- old_env[[i]]
      if (is.na(val)) {
        do.call(Sys.unsetenv, list(env_names[[i]]))
      } else {
        do.call(Sys.setenv, stats::setNames(list(val), env_names[[i]]))
      }
    }
  }

  do.call(
    Sys.setenv,
    as.list(c(
      LOCALAPPDATA = local_appdata,
      APPDATA = roaming_appdata,
      HOME = home_dir,
      XDG_CACHE_HOME = xdg_cache_home,
      XDG_DATA_HOME = xdg_data_home
    ))
  )
  on.exit(restore_env(), add = TRUE)

  force(expr)
}

render_report_html <- function(job_dir) {
  prep <- .prepare_report_render(job_dir)
  temp_template <- .copy_report_template(prep$template, prep$out_dir, "report_template_html")
  temp_output <- file.path(prep$out_dir, "report_template_html.html")

  on.exit({
    if (file.exists(temp_template)) file.remove(temp_template)
    if (file.exists(temp_output)) file.remove(temp_output)
  }, add = TRUE)

  .with_quarto_runtime_env(prep$job_dir, quarto::quarto_render(
    input = temp_template,
    output_format = "html",
    output_file = basename(temp_output),
    execute_params = list(
      job_dir = prep$job_dir,
      report_ctx = prep$ctx
    ),
    quiet = TRUE
  ))

  if (!file.exists(temp_output)) {
    stop("render_report_html(): Quarto did not produce report_template_html.html", call. = FALSE)
  }

  dest <- file.path(prep$out_dir, "report.html")
  file.copy(temp_output, dest, overwrite = TRUE)
  normalizePath(dest, winslash = "/", mustWork = TRUE)
}

render_report_pdf <- function(job_dir) {
  prep <- .prepare_report_render(job_dir)
  
  # Copy template to job_dir to avoid Typst root restriction issues
  temp_template <- .copy_report_template(prep$template, prep$job_dir, "report_template_pdf")
  
  # Ensure we clean up the temporary template afterwards
  on.exit({
    if (file.exists(temp_template)) file.remove(temp_template)
  }, add = TRUE)

  .with_quarto_runtime_env(prep$job_dir, quarto::quarto_render(
    input = temp_template,
    output_format = "typst",
    execute_params = list(
      job_dir = prep$job_dir,
      report_ctx = prep$ctx
    ),
    quiet = TRUE
  ))

  rendered <- file.path(prep$job_dir, "report_template_pdf.pdf")
  if (!file.exists(rendered)) {
    stop("render_report_pdf(): Quarto typst did not produce report_template_pdf.pdf", call. = FALSE)
  }

  dest <- file.path(prep$out_dir, "report.pdf")
  file.copy(rendered, dest, overwrite = TRUE)
  
  # Clean up generated typst and pdf in the job_dir
  if (file.exists(rendered)) file.remove(rendered)
  typ_file <- file.path(prep$job_dir, "report_template_pdf.typ")
  if (file.exists(typ_file)) file.remove(typ_file)
  
  normalizePath(dest, winslash = "/", mustWork = TRUE)
}
