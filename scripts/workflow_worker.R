args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 8L) stop("workflow_worker.R requires 8 arguments.", call. = FALSE)

project_dir <- args[1L]
job_dir <- args[2L]
input_path <- args[3L]
group_var <- args[4L]
beta_distance <- args[5L]
tax_level <- args[6L]
config_path <- args[7L]
demo_mode <- identical(args[8L], "1")

setwd(project_dir)
source("global.R")
paths <- workflow_background_paths(job_dir)

status <- tryCatch({
  input_data <- readRDS(input_path)
  result <- run_full_analysis_workflow(
    input_data = input_data,
    job_dir = job_dir,
    group_var = group_var,
    beta_distance = beta_distance,
    tax_level = tax_level,
    config_path = config_path,
    log_path = paths$run_log,
    state = NULL,
    demo_mode = demo_mode
  )
  saveRDS(result, paths$result)
  list(ok = TRUE, finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}, error = function(e) {
  list(
    ok = FALSE,
    message = conditionMessage(e),
    calls = capture.output(sys.calls()),
    finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
})

saveRDS(status, paths$status)
if (!isTRUE(status$ok)) {
  message(status$message)
  quit(save = "no", status = 1L)
}
