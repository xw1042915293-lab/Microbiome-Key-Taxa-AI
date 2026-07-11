# Background workflow cancellation must stop the worker without blocking Shiny.

source("global.R", local = TRUE)

sample_ids <- paste0("S", seq_len(6))
feature_ids <- paste0("F", seq_len(4))
input_data <- list(
  abundance = data.frame(
    FeatureID = feature_ids,
    matrix(seq_len(24), nrow = 4, dimnames = list(NULL, sample_ids)),
    check.names = FALSE
  ),
  metadata = data.frame(
    SampleID = sample_ids,
    Group = rep(c("A", "B"), each = 3),
    stringsAsFactors = FALSE
  ),
  taxonomy = data.frame(
    FeatureID = feature_ids,
    Kingdom = "Bacteria",
    Phylum = "TestPhylum",
    Class = "TestClass",
    Order = "TestOrder",
    Family = "TestFamily",
    Genus = paste0("Genus", seq_len(4)),
    stringsAsFactors = FALSE
  )
)

job_dir <- tempfile("workflow_cancel_")
dir.create(job_dir)
for (directory in c("objects", "logs", "tables", "figures", "json", "report")) {
  dir.create(file.path(job_dir, directory))
}

state <- create_analysis_state()
shiny::isolate({
  state$job_dir <- job_dir
  state$job_id <- "cancel_test"
  pid <- start_background_analysis_workflow(input_data, job_dir, "Group", state = state)
  process <- state$wf_process
})
Sys.sleep(0.75)
stopifnot(is.numeric(pid), process$is_alive())

elapsed <- system.time(
  shiny::isolate(stopped <- cancel_background_analysis_workflow(state))
)[["elapsed"]]
shiny::isolate({
  stopifnot(
    isTRUE(stopped),
    !process$is_alive(),
    identical(state$status, "full_workflow_error"),
    identical(state$wf_error, "用户手动终止了任务")
  )
})
stopifnot(elapsed < 2)

cat("test_workflow_cancel: ok\n")
