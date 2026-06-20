source("R/ai_rules.R")
source("R/ai_client.R")
source("R/ai_prompt.R")

tmp_cfg <- tempfile(fileext = ".yml")
writeLines(c(
  "default:",
  "  ai:",
  "    timeout_sec: 90",
  "  llm:",
  "    provider: \"anthropic\"",
  "    base_url: \"https://api.deepseek.com/anthropic\"",
  "    model: \"deepseek-v4-flash\"",
  "    api_key_env: \"DEEPSEEK_API_KEY\"",
  "    temperature: 0.2",
  "    max_tokens: 1200"
), tmp_cfg, useBytes = TRUE)

cfg <- read_llm_config(tmp_cfg)
stopifnot(identical(cfg$provider, "anthropic"))
stopifnot(identical(cfg$base_url, "https://api.deepseek.com/anthropic"))
stopifnot(identical(cfg$model, "deepseek-v4-flash"))
stopifnot(identical(cfg$api_key_env, "DEEPSEEK_API_KEY"))
stopifnot(isTRUE(all.equal(cfg$timeout_sec, 90)))

# Use the most recent job directory that has diff_summary.json
pick_latest_job_with_diff <- function(results_dir = "results") {
  if (!dir.exists(results_dir)) return(NULL)
  dirs <- list.dirs(results_dir, full.names = TRUE, recursive = FALSE)
  for (d in dirs[order(list.dirs(results_dir, full.names = FALSE, recursive = FALSE), decreasing = TRUE)]) {
    if (file.exists(file.path(d, "json", "diff_summary.json"))) return(d)
  }
  NULL
}
test_job_dir <- pick_latest_job_with_diff()
if (!is.null(test_job_dir)) {
  payload <- build_llm_request_payload(job_dir = test_job_dir, config = cfg)
  stopifnot(identical(payload$model, "deepseek-v4-flash"))
  stopifnot(identical(payload$messages[[1]]$content[[1]]$type, "text"))
  stopifnot(nzchar(payload$system))
} else {
  cat("SKIPPED: No job with diff_summary.json found; build_llm_request_payload not tested.\n")
}

Sys.unsetenv("DEEPSEEK_API_KEY")
stopifnot(is.null(read_api_key("")))
stopifnot(is.null(read_api_key("DEEPSEEK_API_KEY")))
