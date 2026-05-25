`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

read_optional_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

load_diff_inputs <- function(job_dir) {
  if (!dir.exists(job_dir)) {
    stop("load_diff_inputs(): job_dir not found: ", job_dir, call. = FALSE)
  }

  alpha_path <- file.path(job_dir, "tables", "alpha_stats.csv")
  beta_path <- file.path(job_dir, "tables", "beta_permanova.csv")
  diff_path <- file.path(job_dir, "tables", "differential_taxa.csv")
  sig_path <- file.path(job_dir, "tables", "differential_taxa_significant.csv")
  summary_path <- file.path(job_dir, "json", "diff_summary.json")

  if (!file.exists(diff_path)) stop("Missing differential_taxa.csv in job_dir.", call. = FALSE)
  if (!file.exists(summary_path)) stop("Missing diff_summary.json in job_dir.", call. = FALSE)

  list(
    alpha = read_optional_csv(alpha_path),
    beta = read_optional_csv(beta_path),
    diff = read.csv(diff_path, stringsAsFactors = FALSE, check.names = FALSE),
    sig = read_optional_csv(sig_path),
    summary = jsonlite::fromJSON(summary_path, simplifyDataFrame = TRUE)
  )
}

build_diff_interpretation <- function(job_dir) {
  inputs <- load_diff_inputs(job_dir)
  diff_summary <- inputs$summary
  diff_table <- inputs$diff
  sig_table <- inputs$sig

  n_sig <- diff_summary$n_significant_taxa %||% sum(diff_table$fdr < 0.05, na.rm = TRUE)
  method <- diff_summary$method %||% "the configured differential test"
  tax_level <- diff_summary$tax_level %||% "the selected taxonomic level"
  group_var <- diff_summary$group_variable %||% "the grouping variable"

  sig_rows <- if (!is.null(sig_table) && nrow(sig_table) > 0) {
    sig_table
  } else {
    diff_table[!is.na(diff_table$fdr) & diff_table$fdr < 0.05, , drop = FALSE]
  }

  trend_rows <- diff_table[!is.na(diff_table$fdr) & diff_table$fdr >= 0.05 & diff_table$fdr < 0.1, , drop = FALSE]

  lines <- c(
    "# Differential Taxa Interpretation",
    "",
    paste0(
      "This local interpretation summarizes the differential abundance results for `",
      group_var, "` at the `", tax_level, "` level using `", method, "`."
    )
  )

  if (!is.null(n_sig) && n_sig > 0 && nrow(sig_rows) > 0) {
    sig_text <- vapply(seq_len(nrow(sig_rows)), function(i) {
      format_taxon_sentence(sig_rows[i, , drop = FALSE], exploratory = FALSE)
    }, character(1))
    lines <- c(lines, "", "## Significant taxa", "", sig_text)
  } else {
    lines <- c(lines, "", "## Significant taxa", "", "No FDR-significant taxa were detected in this analysis.")
  }

  if (nrow(trend_rows) > 0) {
    trend_text <- vapply(seq_len(nrow(trend_rows)), function(i) {
      format_taxon_sentence(trend_rows[i, , drop = FALSE], exploratory = TRUE)
    }, character(1))
    lines <- c(lines, "", "## Exploratory trends", "", trend_text)
  }

  lines <- c(
    lines,
    "",
    "## Caution",
    "",
    "These statements are statistically constrained summaries. They do not imply causation or mechanism."
  )

  paste(lines, collapse = "\n")
}

build_methods_text <- function(job_dir) {
  inputs <- load_diff_inputs(job_dir)
  diff_summary <- inputs$summary
  method <- diff_summary$method %||% "Wilcoxon/Kruskal-Wallis"
  tax_level <- diff_summary$tax_level %||% "taxonomic"
  group_var <- diff_summary$group_variable %||% "group"

  paste(
    c(
      "# Methods",
      "",
      paste0(
        "Differential abundance was evaluated at the `", tax_level,
        "` level across `", group_var, "` using `", method, "`."
      ),
      "P values were adjusted using FDR correction.",
      "Interpretation was restricted by local rule-based checks: FDR < 0.05 was treated as significant, 0.05 <= FDR < 0.1 as a trend, and FDR >= 0.1 as not significant.",
      "No real LLM API was called in this phase."
    ),
    collapse = "\n"
  )
}

build_figure_legends_text <- function(job_dir) {
  inputs <- load_diff_inputs(job_dir)
  diff_summary <- inputs$summary
  alpha_has <- !is.null(inputs$alpha) && nrow(inputs$alpha) > 0
  beta_has <- !is.null(inputs$beta) && nrow(inputs$beta) > 0
  diff_has <- !is.null(inputs$diff) && nrow(inputs$diff) > 0

  lines <- c(
    "# Figure Legends",
    "",
    paste0(
      "Figure legend text is generated locally from Phase 2 and Phase 3 outputs for `",
      diff_summary$group_variable %||% "group", "`."
    )
  )

  if (alpha_has) {
    lines <- c(lines, "Alpha diversity summary is described from the saved `alpha_stats.csv` output.")
  }
  if (beta_has) {
    lines <- c(lines, "Beta diversity summary is described from the saved `beta_permanova.csv` output.")
  }
  if (diff_has) {
    lines <- c(lines, "Differential taxa summaries are described from the saved `differential_taxa.csv` output.")
  }

  paste(lines, collapse = "\n")
}

write_ai_outputs <- function(job_dir) {
  ai_dir <- file.path(job_dir, "ai")
  dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)

  diff_path <- file.path(ai_dir, "diff_interpretation.md")
  methods_path <- file.path(ai_dir, "methods.md")
  legend_path <- file.path(ai_dir, "figure_legends.md")

  writeLines(build_diff_interpretation(job_dir), diff_path, useBytes = TRUE)
  writeLines(build_methods_text(job_dir), methods_path, useBytes = TRUE)
  writeLines(build_figure_legends_text(job_dir), legend_path, useBytes = TRUE)

  list(
    diff_interpretation_path = normalizePath(diff_path, winslash = "/", mustWork = TRUE),
    methods_path = normalizePath(methods_path, winslash = "/", mustWork = TRUE),
    figure_legends_path = normalizePath(legend_path, winslash = "/", mustWork = TRUE)
  )
}

write_llm_outputs <- function(job_dir, config_path = "config.yml") {
  config <- read_llm_config(config_path)
  api_key <- read_api_key(config$api_key_env)
  ai_dir <- file.path(job_dir, "ai")
  dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)

  request_path <- file.path(job_dir, "json", "llm_request_diff.json")
  response_path <- file.path(job_dir, "json", "llm_response_diff.json")
  out_diff_path <- file.path(ai_dir, "llm_diff_interpretation.md")
  out_methods_path <- file.path(ai_dir, "llm_methods.md")
  out_legends_path <- file.path(ai_dir, "llm_figure_legends.md")

  read_text_optional <- function(path) {
    if (is.null(path) || !is.character(path) || length(path) != 1 || !file.exists(path)) return(character(0))
    if (exists("read_text_file", mode = "function")) {
      return(tryCatch(read_text_file(path), error = function(e) character(0)))
    }
    tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  }

  if (is.null(api_key)) {
    message("SKIPPED: API key ", config$api_key_env, " is not set; LLM request was not sent.")
    # When API key is missing, we must still generate Phase 4B placeholder artifacts.
    # Do NOT build prompts/payloads here to avoid any chance of reading raw inputs.
    prompt_bundle <- list(status = "not_built", reason = paste0("API key ", config$api_key_env, " is not set."))
    request_payload <- NULL

    request_record <- list(
      status = "skipped",
      api_key_env = config$api_key_env,
      provider = config$provider,
      base_url = config$base_url,
      model = config$model,
      temperature = config$temperature,
      max_tokens = config$max_tokens,
      prompt_bundle = prompt_bundle,
      request = request_payload
    )

    response_record <- list(
      status = "skipped",
      reason = paste0("API key ", config$api_key_env, " is not set.")
    )

    phase4a_diff <- read_text_optional(file.path(job_dir, "ai", "diff_interpretation.md"))
    phase4a_methods <- read_text_optional(file.path(job_dir, "ai", "methods.md"))
    phase4a_legends <- read_text_optional(file.path(job_dir, "ai", "figure_legends.md"))

    writeLines(
      c(
        "# LLM Diff Interpretation",
        "",
        paste0("LLM request skipped because `", config$api_key_env, "` is not set."),
        "",
        phase4a_diff
      ),
      out_diff_path,
      useBytes = TRUE
    )
    writeLines(
      c(
        "# LLM Methods",
        "",
        paste0("LLM request skipped because `", config$api_key_env, "` is not set."),
        "",
        phase4a_methods
      ),
      out_methods_path,
      useBytes = TRUE
    )
    writeLines(
      c(
        "# LLM Figure Legends",
        "",
        paste0("LLM request skipped because `", config$api_key_env, "` is not set."),
        "",
        phase4a_legends
      ),
      out_legends_path,
      useBytes = TRUE
    )
  } else {
    request_payload <- build_llm_request_payload(job_dir, config)
    prompt_bundle <- build_llm_prompt_bundle(job_dir)
    request_record <- list(
      status = "sent",
      api_key_env = config$api_key_env,
      provider = config$provider,
      base_url = config$base_url,
      model = config$model,
      temperature = config$temperature,
      max_tokens = config$max_tokens,
      prompt_bundle = prompt_bundle,
      request = request_payload
    )

    response <- invoke_kkai_chat_completions(
      base_url = config$base_url,
      api_key = api_key,
      payload = request_payload,
      timeout_sec = 120
    )
    response_record <- list(status = "success", response = response)

    response_text <- extract_llm_json_text(response)
    parsed <- parse_llm_response_text(response_text)$parsed

    writeLines(coerce_llm_markdown(parsed, "diff_interpretation", read_text_file(file.path(job_dir, "ai", "diff_interpretation.md"))), out_diff_path, useBytes = TRUE)
    writeLines(coerce_llm_markdown(parsed, "methods", read_text_file(file.path(job_dir, "ai", "methods.md"))), out_methods_path, useBytes = TRUE)
    writeLines(coerce_llm_markdown(parsed, "figure_legends", read_text_file(file.path(job_dir, "ai", "figure_legends.md"))), out_legends_path, useBytes = TRUE)
  }

  dir.create(file.path(job_dir, "json"), showWarnings = FALSE, recursive = TRUE)
  writeLines(jsonlite::toJSON(request_record, auto_unbox = TRUE, pretty = TRUE, null = "null"), request_path, useBytes = TRUE)
  writeLines(jsonlite::toJSON(response_record, auto_unbox = TRUE, pretty = TRUE, null = "null"), response_path, useBytes = TRUE)

  list(
    request_path = normalizePath(request_path, winslash = "/", mustWork = TRUE),
    response_path = normalizePath(response_path, winslash = "/", mustWork = TRUE),
    diff_interpretation_path = normalizePath(out_diff_path, winslash = "/", mustWork = TRUE),
    methods_path = normalizePath(out_methods_path, winslash = "/", mustWork = TRUE),
    figure_legends_path = normalizePath(out_legends_path, winslash = "/", mustWork = TRUE),
    api_key_present = !is.null(api_key)
  )
}
