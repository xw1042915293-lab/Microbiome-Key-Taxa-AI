read_text_file <- function(path) {
  if (!file.exists(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

read_diff_summary_only <- function(job_dir) {
  summary_path <- file.path(job_dir, "json", "diff_summary.json")
  if (!file.exists(summary_path)) {
    stop("read_diff_summary_only(): missing diff_summary.json.", call. = FALSE)
  }
  jsonlite::fromJSON(summary_path, simplifyDataFrame = TRUE)
}

build_llm_prompt_bundle <- function(job_dir) {
  summary <- read_diff_summary_only(job_dir)
  phase4a_diff <- read_text_file(file.path(job_dir, "ai", "diff_interpretation.md"))
  phase4a_methods <- read_text_file(file.path(job_dir, "ai", "methods.md"))
  phase4a_legends <- read_text_file(file.path(job_dir, "ai", "figure_legends.md"))

  list(
    summary = summary,
    phase4a = list(
      diff_interpretation = phase4a_diff,
      methods = phase4a_methods,
      figure_legends = phase4a_legends
    )
  )
}

build_llm_messages <- function(prompt_bundle) {
  system_text <- paste(
    c(
      "You are a cautious microbiome interpretation assistant.",
      "Use only the provided statistical JSON and Phase 4A local interpretation text.",
      "Do not mention raw abundance tables.",
      "Do not change statistical conclusions.",
      "Do not turn non-significant results into significant results.",
      "Do not infer causation or mechanisms.",
      "Return a single JSON object with keys diff_interpretation, methods, and figure_legends.",
      "Each value must be Markdown text."
    ),
    collapse = " "
  )

  user_text <- jsonlite::toJSON(prompt_bundle, auto_unbox = TRUE, pretty = TRUE, null = "null")

  list(
    list(role = "system", content = system_text),
    list(role = "user", content = user_text)
  )
}

build_llm_request_payload <- function(job_dir, config) {
  bundle <- build_llm_prompt_bundle(job_dir)
  list(
    model = config$model,
    temperature = config$temperature,
    max_tokens = config$max_tokens,
    messages = build_llm_messages(bundle)
  )
}

extract_llm_json_text <- function(response) {
  if (is.list(response) && !is.null(response$choices) && length(response$choices) > 0) {
    content <- response$choices[[1]]$message$content %||% ""
  } else {
    content <- ""
  }
  if (!nzchar(content) && !is.null(response$content)) {
    content <- response$content
  }
  as.character(content)[1]
}

extract_json_object_text <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) return(text)
  start <- regexpr("\\{", text, fixed = FALSE)[1]
  end <- regexpr("\\}(?!.*\\})", text, perl = TRUE)[1]
  if (start > 0 && end > start) {
    substr(text, start, end)
  } else {
    text
  }
}

parse_llm_response_text <- function(response_text) {
  json_text <- extract_json_object_text(response_text)
  parsed <- jsonlite::fromJSON(json_text, simplifyDataFrame = TRUE)
  list(
    raw_text = response_text,
    parsed = parsed
  )
}

coerce_llm_markdown <- function(parsed, key, fallback_text) {
  value <- parsed[[key]]
  if (is.null(value) || !nzchar(as.character(value)[1])) return(fallback_text)
  as.character(value)[1]
}
