setwd("D:/Microbiome Key Taxa AI")

source("R/00_packages.R")
source("R/01_config.R")
source("R/02_utils_file.R")
source("R/04_utils_json.R")
source("R/ai_rules.R")
source("R/ai_client.R")
source("R/ai_prompt.R")

args <- commandArgs(trailingOnly = TRUE)
job_dir <- if (length(args) >= 1) args[[1]] else file.path("results", "job_20260613_185942_3pv5di")
job_dir <- normalizePath(job_dir, winslash = "/", mustWork = TRUE)

cfg <- read_llm_config("config.yml")
api_key <- read_api_key(cfg$api_key_env)

cat("provider=", cfg$provider, "\n", sep = "")
cat("base_url=", cfg$base_url, "\n", sep = "")
cat("model=", cfg$model, "\n", sep = "")
cat("api_key_env=", cfg$api_key_env, "\n", sep = "")
cat("api_key_present=", !is.null(api_key), "\n", sep = "")

payload <- build_llm_request_payload(job_dir, cfg)
resp <- invoke_llm_request(cfg, api_key, payload, timeout_sec = cfg$timeout_sec %||% 120)

cat("response_names=", paste(names(resp), collapse = " | "), "\n", sep = "")
text <- extract_llm_json_text(resp)
cat("response_text_nchar=", nchar(text), "\n", sep = "")
cat("response_text_preview=\n", sep = "")
cat(substr(text, 1, 2000), "\n", sep = "")

parsed <- parse_llm_response_text(text)
cat("parsed_keys=", paste(names(parsed$parsed), collapse = " | "), "\n", sep = "")
