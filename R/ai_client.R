normalize_base_url <- function(base_url) {
  sub("/+$", "", base_url)
}

read_llm_config <- function(config_path = "config.yml") {
  if (!file.exists(config_path)) {
    stop("read_llm_config(): config file not found: ", config_path, call. = FALSE)
  }

  cfg <- yaml::read_yaml(config_path)
  llm <- cfg$llm
  if (is.null(llm) && !is.null(cfg$default)) {
    llm <- cfg$default$llm
  }
  if (is.null(llm)) llm <- list()

  list(
    provider = llm$provider %||% "kkai",
    base_url = llm$base_url %||% "https://api.kkrich.ltd/v1",
    model = llm$model %||% "gpt-5.4",
    api_key_env = llm$api_key_env %||% "KKAI_API_KEY",
    temperature = llm$temperature %||% 0.2,
    max_tokens = llm$max_tokens %||% 1200
  )
}

read_api_key <- function(api_key_env = "KKAI_API_KEY") {
  key <- Sys.getenv(api_key_env, unset = "")
  if (!nzchar(key)) return(NULL)
  key
}

power_shell_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

invoke_kkai_chat_completions <- function(base_url, api_key, payload, timeout_sec = 120) {
  endpoint <- paste0(normalize_base_url(base_url), "/chat/completions")
  body_path <- tempfile(fileext = ".json")
  on.exit(unlink(body_path), add = TRUE)
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"), body_path, useBytes = TRUE)

  endpoint_ps <- power_shell_escape(endpoint)
  body_ps <- power_shell_escape(normalizePath(body_path, winslash = "\\\\", mustWork = TRUE))
  key_ps <- power_shell_escape(api_key)

  script <- paste0(
    "$headers = @{ Authorization = 'Bearer ", key_ps, "'; 'Content-Type' = 'application/json' }\n",
    "$body = Get-Content -Raw -LiteralPath '", body_ps, "'\n",
    "$resp = Invoke-RestMethod -Method Post -Uri '", endpoint_ps, "' -Headers $headers -Body $body -ContentType 'application/json' -TimeoutSec ", as.integer(timeout_sec), "\n",
    "$resp | ConvertTo-Json -Depth 50 -Compress\n"
  )

  out <- system2(
    "powershell",
    args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", script),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("LLM request failed with status ", status, ": ", paste(out, collapse = "\n"), call. = FALSE)
  }

  raw_text <- paste(out, collapse = "\n")
  if (!nzchar(trimws(raw_text))) {
    stop("LLM request returned an empty response.", call. = FALSE)
  }

  jsonlite::fromJSON(raw_text, simplifyDataFrame = TRUE)
}
