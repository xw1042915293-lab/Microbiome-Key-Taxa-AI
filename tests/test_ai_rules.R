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

job_dir <- tempfile("ai_rules_job_")
dir.create(file.path(job_dir, "json"), recursive = TRUE)
jsonlite::write_json(
  list(
    analysis_type = "differential_abundance",
    method = "Kruskal-Wallis",
    p_adjust_method = "Benjamini-Hochberg",
    n_significant_taxa = 0,
    message = "No significant taxa were detected under FDR < 0.05."
  ),
  file.path(job_dir, "json", "diff_summary.json"),
  auto_unbox = TRUE
)

payload <- build_llm_request_payload(job_dir = job_dir, config = cfg)
stopifnot(identical(payload$model, "deepseek-v4-flash"))
stopifnot(identical(payload$messages[[1]]$content[[1]]$type, "text"))
stopifnot(nzchar(payload$system))

Sys.unsetenv("DEEPSEEK_API_KEY")
stopifnot(is.null(read_api_key("")))
stopifnot(is.null(read_api_key("DEEPSEEK_API_KEY")))
