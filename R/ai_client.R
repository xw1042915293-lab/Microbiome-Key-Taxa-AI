normalize_base_url <- function(base_url) {
  sub("/+$", "", base_url)
}

value_or_default <- function(x, default) {
  if (is.null(x) || length(x) == 0) return(default)
  value <- as.character(x[[1]])
  if (!nzchar(trimws(value))) return(default)
  value
}

normalize_llm_provider <- function(provider) {
  provider <- tolower(trimws(value_or_default(provider, "openai")))
  if (provider %in% c("anthropic", "deepseek_anthropic", "deepseek-anthropic")) {
    return("anthropic")
  }
  "openai"
}

read_llm_config <- function(config_path = "config.yml") {
  if (!file.exists(config_path)) {
    stop("read_llm_config(): config file not found: ", config_path, call. = FALSE)
  }

  cfg <- yaml::read_yaml(config_path)
  llm <- cfg$llm
  ai <- cfg$ai
  if (is.null(llm) && !is.null(cfg$default)) {
    llm <- cfg$default$llm
    ai <- cfg$default$ai %||% ai
  }
  if (is.null(llm)) llm <- list()
  if (is.null(ai)) ai <- list()

  list(
    provider = normalize_llm_provider(llm$provider),
    base_url = value_or_default(llm$base_url, "https://api.deepseek.com"),
    model = value_or_default(llm$model, "deepseek-v4-pro"),
    api_key_env = value_or_default(llm$api_key_env, "DEEPSEEK_API_KEY"),
    temperature = llm$temperature %||% 0.2,
    max_tokens = llm$max_tokens %||% 1200,
    timeout_sec = llm$timeout_sec %||% ai$timeout_sec %||% 120
  )
}

read_api_key <- function(api_key_env = "DEEPSEEK_API_KEY") {
  api_key_env <- value_or_default(api_key_env, "DEEPSEEK_API_KEY")
  key <- Sys.getenv(api_key_env, unset = "")
  if (!nzchar(key)) return(NULL)
  key
}

power_shell_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

invoke_openai_chat_completions <- function(base_url, api_key, payload, timeout_sec = 120) {
  endpoint <- paste0(normalize_base_url(base_url), "/chat/completions")
  body_path <- tempfile(fileext = ".json")
  on.exit(unlink(body_path), add = TRUE)
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"), body_path, useBytes = TRUE)

  endpoint_ps <- power_shell_escape(endpoint)
  body_ps <- power_shell_escape(normalizePath(body_path, winslash = "\\", mustWork = TRUE))
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

invoke_anthropic_messages <- function(base_url, api_key, payload, timeout_sec = 120) {
  endpoint <- paste0(normalize_base_url(base_url), "/messages")
  body_path <- tempfile(fileext = ".json")
  on.exit(unlink(body_path), add = TRUE)
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"), body_path, useBytes = TRUE)

  endpoint_ps <- power_shell_escape(endpoint)
  body_ps <- power_shell_escape(normalizePath(body_path, winslash = "\\", mustWork = TRUE))
  key_ps <- power_shell_escape(api_key)

  script <- paste0(
    "$headers = @{ 'x-api-key' = '", key_ps, "'; 'Content-Type' = 'application/json' }\n",
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

invoke_llm_request <- function(config, api_key, payload, timeout_sec = NULL) {
  timeout_sec <- timeout_sec %||% config$timeout_sec %||% 120
  provider <- normalize_llm_provider(config$provider)

  if (identical(provider, "anthropic")) {
    return(invoke_anthropic_messages(
      base_url = config$base_url,
      api_key = api_key,
      payload = payload,
      timeout_sec = timeout_sec
    ))
  }

  invoke_openai_chat_completions(
    base_url = config$base_url,
    api_key = api_key,
    payload = payload,
    timeout_sec = timeout_sec
  )
}
