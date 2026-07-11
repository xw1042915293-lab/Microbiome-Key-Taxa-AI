`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

detect_interpretation_language <- function(job_dir = NULL) {
  override <- tolower(getOption("kkai.report_language", ""))
  if (override %in% c("zh", "zh-cn", "cn", "chinese")) return("zh")
  if (override %in% c("en", "en-us", "english")) return("en")

  locale_text <- paste(
    Sys.getlocale("LC_CTYPE"),
    tryCatch(Sys.getlocale("LC_MESSAGES"), error = function(e) ""),
    Sys.getenv("LANG", unset = ""),
    sep = " "
  )
  if (grepl("zh|chinese", locale_text, ignore.case = TRUE)) return("zh")
  "en"
}

detect_report_language <- function() {
  detect_interpretation_language()
}

read_optional_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}

read_text_optional <- function(path) {
  if (is.null(path) || !is.character(path) || length(path) != 1 || !file.exists(path)) return(character(0))
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
}

load_diff_inputs <- function(job_dir) {
  if (!dir.exists(job_dir)) {
    stop("load_diff_inputs(): job_dir not found: ", job_dir, call. = FALSE)
  }

  alpha_path <- alpha_output_path(
    job_dir, "tables", "alpha_stats.csv", legacy_filename = "alpha_stats.csv", existing = TRUE
  )
  beta_path <- beta_output_path(
    job_dir, "tables", "beta_permanova.csv", legacy_filename = "beta_permanova.csv", existing = TRUE
  )
  beta_dispersion_path <- beta_output_path(job_dir, "tables", "beta_dispersion.csv", existing = TRUE)
  diff_path <- file.path(job_dir, "tables", "differential_taxa.csv")
  sig_path <- file.path(job_dir, "tables", "differential_taxa_significant.csv")
  summary_path <- file.path(job_dir, "json", "diff_summary.json")

  if (!file.exists(diff_path)) stop("Missing differential_taxa.csv in job_dir.", call. = FALSE)
  if (!file.exists(summary_path)) stop("Missing diff_summary.json in job_dir.", call. = FALSE)

  list(
    alpha = read_optional_csv(alpha_path),
    beta = read_optional_csv(beta_path),
    beta_dispersion = read_optional_csv(beta_dispersion_path),
    diff = read.csv(diff_path, stringsAsFactors = FALSE, check.names = FALSE),
    sig = read_optional_csv(sig_path),
    summary = jsonlite::fromJSON(summary_path, simplifyDataFrame = TRUE)
  )
}

ai_row_label <- function(row) {
  if (!is.data.frame(row) || nrow(row) < 1) return("Unknown taxon")
  cols <- c("taxon_label", "display_taxon", "display_taxon_short", "label", "Genus", "taxon", "Taxon", "feature", "FeatureID", "name")
  for (nm in cols) {
    if (!nm %in% names(row)) next
    value <- as.character(row[[nm]][1])
    value <- trimws(value)
    if (!nzchar(value)) next
    if (grepl("\\|", value)) return(taxon_display_from_any(value)[1])
    return(value)
  }
  "Unknown taxon"
}

ai_row_original_taxon <- function(row) {
  if (!is.data.frame(row) || nrow(row) < 1) return(NA_character_)
  cols <- c("taxon", "Taxon", "feature", "FeatureID", "name", "taxon_label", "display_taxon")
  for (nm in cols) {
    if (!nm %in% names(row)) next
    value <- as.character(row[[nm]][1])
    value <- trimws(value)
    if (nzchar(value)) return(value)
  }
  NA_character_
}

ai_row_key <- function(row) {
  normalize_taxon_name(ai_row_original_taxon(row))
}

ai_format_numeric <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("NA")
  value <- suppressWarnings(as.numeric(x[1]))
  if (!is.finite(value)) return("NA")
  formatC(value, digits = digits, format = "f")
}

ai_markdown_table <- function(df) {
  if (!is.data.frame(df) || nrow(df) < 1) return("")
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- apply(df, 1, function(r) paste0("| ", paste(as.character(r), collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

build_significant_taxa_text <- function(sig_rows, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (!is.data.frame(sig_rows) || nrow(sig_rows) < 1) {
    if (lang == "zh") {
      return("未检测到 FDR < 0.05 的显著差异菌。以下趋势菌仅作为探索性线索，不应表述为统计学显著。")
    }
    return("No FDR-significant taxa were detected. The following taxa are exploratory trends only and should not be interpreted as statistically significant.")
  }

  taxa_text <- format_taxa_list(sig_rows$.taxon_name, lang = lang)
  if (lang == "zh") {
    paste0("检测到 FDR 显著差异菌：", taxa_text, "。这些结果满足 FDR < 0.05。")
  } else {
    paste0("FDR-significant taxa included ", taxa_text, " (FDR < 0.05).")
  }
}

build_exploratory_taxa_text <- function(exploratory_rows, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (!is.data.frame(exploratory_rows) || nrow(exploratory_rows) < 1) {
    if (lang == "zh") {
      return("未观察到需要重点说明的探索性趋势菌。")
    }
    return("No exploratory taxa were prioritized for narrative interpretation.")
  }

  taxa_text <- format_taxa_list(exploratory_rows$.taxon_name, lang = lang)
  fdr_text <- format_fdr_range(exploratory_rows$fdr, lang = lang)
  if (lang == "zh") {
    paste0(
      "探索性趋势菌 Top 5 为：", taxa_text, "。", fdr_text,
      "。这些结果仅用于提出后续筛选假设，不应解释为显著差异。"
    )
  } else {
    paste0(
      "Top exploratory taxa were ", taxa_text, ". ", fdr_text,
      ". These findings are descriptive only and should not be interpreted as statistically significant."
    )
  }
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
  beta_dispersion_has <- !is.null(inputs$beta_dispersion) && nrow(inputs$beta_dispersion) > 0
  diff_has <- !is.null(inputs$diff) && nrow(inputs$diff) > 0

  lines <- c(
    "# Figure Legends",
    "",
    paste0(
      "Figure legend text is generated locally from Phase 2 and Phase 3 outputs for `",
      diff_summary$group_variable %||% "group", "`."
    )
  )

  if (alpha_has) lines <- c(lines, "Alpha diversity summary is described from the saved `alpha_stats.csv` output.")
  if (beta_has) lines <- c(lines, "Beta diversity summary is described from the saved `beta_permanova.csv` output.")
  if (beta_dispersion_has) lines <- c(lines, "Beta dispersion is checked from `beta_dispersion.csv`; a significant PERMDISP result requires cautious PERMANOVA interpretation.")
  if (diff_has) lines <- c(lines, "Differential taxa summaries are described from the saved `differential_taxa.csv` output.")

  paste(lines, collapse = "\n")
}

ai_prepare_ranked_taxa <- function(df, order_index, max_n = 20) {
  if (!is.data.frame(df) || nrow(df) < 1) return(data.frame())
  df <- df[order(order_index, na.last = TRUE), , drop = FALSE]
  out <- data.frame(
    label = character(),
    key = character(),
    rank = integer(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    key <- ai_row_key(row)
    label <- ai_row_label(row)
    if (is.na(key) || !nzchar(key) || key %in% out$key) next
    out[nrow(out) + 1, ] <- list(label, key, nrow(out) + 1L)
    if (nrow(out) >= max_n) break
  }

  out
}

ai_build_source_evidence <- function(job_dir) {
  ml_df <- read_optional_csv(file.path(job_dir, "tables", "ml_feature_importance.csv"))
  net_df <- read_optional_csv(file.path(job_dir, "tables", "network_nodes.csv"))
  key_df <- read_optional_csv(file.path(job_dir, "tables", "key_taxa_score.csv"))

  ml_top <- if (!is.null(ml_df) && nrow(ml_df) > 0) {
    imp_col <- if ("importance" %in% names(ml_df)) "importance" else if ("rf_importance" %in% names(ml_df)) "rf_importance" else NULL
    if (is.null(imp_col)) {
      data.frame()
    } else {
      ai_prepare_ranked_taxa(ml_df, -suppressWarnings(as.numeric(ml_df[[imp_col]])), max_n = 20)
    }
  } else {
    data.frame()
  }

  net_top <- if (!is.null(net_df) && nrow(net_df) > 0) {
    score <- rep(0, nrow(net_df))
    if ("degree" %in% names(net_df)) score <- score + suppressWarnings(as.numeric(net_df$degree))
    if ("betweenness" %in% names(net_df)) score <- score + suppressWarnings(as.numeric(net_df$betweenness))
    ai_prepare_ranked_taxa(net_df, -score, max_n = 20)
  } else {
    data.frame()
  }

  key_top <- if (!is.null(key_df) && nrow(key_df) > 0) {
    if ("rank" %in% names(key_df)) {
      ai_prepare_ranked_taxa(key_df, suppressWarnings(as.numeric(key_df$rank)), max_n = 20)
    } else if ("key_taxa_score" %in% names(key_df)) {
      ai_prepare_ranked_taxa(key_df, -suppressWarnings(as.numeric(key_df$key_taxa_score)), max_n = 20)
    } else {
      data.frame()
    }
  } else {
    data.frame()
  }

  to_rank_map <- function(df) {
    if (!is.data.frame(df) || nrow(df) < 1) return(integer(0))
    stats::setNames(as.integer(df$rank), df$key)
  }

  list(
    ml = ml_top,
    network = net_top,
    key = key_top,
    ml_map = to_rank_map(ml_top),
    network_map = to_rank_map(net_top),
    key_map = to_rank_map(key_top)
  )
}

ai_build_exploratory_table <- function(rows, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (!is.data.frame(rows) || nrow(rows) < 1) return("")

  med_abund <- suppressWarnings(stats::median(as.numeric(rows$mean_abundance), na.rm = TRUE))
  med_prev <- suppressWarnings(stats::median(as.numeric(rows$prevalence), na.rm = TRUE))

  out <- data.frame(
    Taxon = character(),
    FDR = character(),
    `Mean abundance` = character(),
    Prevalence = character(),
    Interpretation = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    taxon <- ai_row_label(row)
    fdr <- suppressWarnings(as.numeric(row$fdr[1]))
    p_value <- suppressWarnings(as.numeric(row$p_value[1]))
    mean_abundance <- suppressWarnings(as.numeric(row$mean_abundance[1]))
    prevalence <- suppressWarnings(as.numeric(row$prevalence[1]))

    if (lang == "zh") {
      reason <- character()
      if (is.finite(fdr) && fdr <= 0.1) reason <- c(reason, "FDR 接近 0.05")
      if (is.finite(p_value)) reason <- c(reason, "原始 p 值较低")
      if (is.finite(mean_abundance) && is.finite(med_abund) && mean_abundance >= med_abund) reason <- c(reason, "平均丰度相对较高")
      if (is.finite(prevalence) && is.finite(med_prev) && prevalence >= med_prev) reason <- c(reason, "出现率相对较高")
      if (length(reason) < 1) reason <- "作为探索性候选仍值得后续观察"
      interp <- paste(reason, collapse = "；")
    } else {
      reason <- character()
      if (is.finite(fdr) && fdr <= 0.1) reason <- c(reason, "FDR is close to 0.05")
      if (is.finite(p_value)) reason <- c(reason, "raw p value is relatively low")
      if (is.finite(mean_abundance) && is.finite(med_abund) && mean_abundance >= med_abund) reason <- c(reason, "mean abundance is relatively high")
      if (is.finite(prevalence) && is.finite(med_prev) && prevalence >= med_prev) reason <- c(reason, "prevalence is relatively high")
      if (length(reason) < 1) reason <- "still worth tracking as an exploratory lead"
      interp <- paste(reason, collapse = "; ")
    }

    out[nrow(out) + 1, ] <- list(
      taxon,
      ai_format_numeric(fdr, 3),
      ai_format_numeric(mean_abundance, 4),
      ai_format_numeric(prevalence, 3),
      interp
    )
  }

  ai_markdown_table(utils::head(out, 5))
}

ai_build_biological_relevance <- function(rows, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (!is.data.frame(rows) || nrow(rows) < 1) return("")
  lines <- character()
  for (i in seq_len(nrow(rows))) {
    taxon <- ai_row_label(rows[i, , drop = FALSE])
    if (lang == "zh") {
      lines <- c(lines, paste0("- ", taxon, "：可作为与根际环境适应、营养循环或植物互作相关的候选类群进一步关注。"))
    } else {
      lines <- c(lines, paste0("- ", taxon, ": may warrant follow-up attention as a candidate taxon related to rhizosphere adaptation, nutrient cycling, or plant interaction."))
    }
  }
  paste(lines, collapse = "\n")
}

ai_priority_bundle <- function(in_ml, in_network, in_key, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (isTRUE(in_key) && (isTRUE(in_ml) || isTRUE(in_network))) {
    if (lang == "zh") {
      return(list(priority = "高优先级候选", recommendation = "优先进入候选核心菌验证"))
    }
    return(list(priority = "high-priority candidate", recommendation = "prioritize for core candidate validation"))
  }
  if (isTRUE(in_key)) {
    if (lang == "zh") {
      return(list(priority = "中等优先级候选", recommendation = "建议结合丰度、出现率和分离菌株结果进一步筛选"))
    }
    return(list(priority = "medium-priority candidate", recommendation = "combine abundance, prevalence, and isolate evidence for further screening"))
  }
  if (isTRUE(in_ml) || isTRUE(in_network)) {
    if (lang == "zh") {
      return(list(priority = "中等优先级候选", recommendation = "建议结合丰度、出现率和分离菌株结果进一步筛选"))
    }
    return(list(priority = "medium-priority exploratory candidate", recommendation = "combine abundance, prevalence, and isolate evidence for further screening"))
  }
  if (lang == "zh") {
    return(list(priority = "低可信候选", recommendation = "仅作为探索性线索，不建议单独作为核心菌依据"))
  }
  list(priority = "low-confidence candidate", recommendation = "treat only as an exploratory lead, not as standalone evidence for a core taxon")
}

ai_yes_no <- function(flag, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (lang == "zh") {
    ifelse(isTRUE(flag), "是", "否")
  } else {
    ifelse(isTRUE(flag), "Yes", "No")
  }
}

ai_build_integration_suggestions <- function(rows, source_maps, job_dir, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (!is.data.frame(rows) || nrow(rows) < 1) return("")

  ml_map <- source_maps$ml_map %||% integer(0)
  net_map <- source_maps$network_map %||% integer(0)
  key_map <- source_maps$key_map %||% integer(0)

  out <- data.frame(
    Taxon = character(),
    `Evidence from differential trend` = character(),
    `In ML Top20` = character(),
    `In Network Top20` = character(),
    `In Key Taxa Top20` = character(),
    Priority = character(),
    Recommendation = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  debug <- data.frame(
    original_taxon = character(),
    normalized_taxon = character(),
    matched_ml = logical(),
    matched_network = logical(),
    matched_key_taxa = logical(),
    priority = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    original_taxon <- ai_row_original_taxon(row)
    normalized_taxon <- normalize_taxon_name(original_taxon)
    in_ml <- !is.na(match(normalized_taxon, names(ml_map)))
    in_network <- !is.na(match(normalized_taxon, names(net_map)))
    in_key <- !is.na(match(normalized_taxon, names(key_map)))
    bundle <- ai_priority_bundle(in_ml, in_network, in_key, lang = lang)

    evidence <- if (lang == "zh") {
      paste0(
        "差异趋势 Top5（FDR ", ai_format_numeric(row$fdr[1], 3),
        "；原始 p ", ai_format_numeric(row$p_value[1], 3), "）"
      )
    } else {
      paste0(
        "Top 5 differential trend (FDR ", ai_format_numeric(row$fdr[1], 3),
        "; raw p ", ai_format_numeric(row$p_value[1], 3), ")"
      )
    }

    out[nrow(out) + 1, ] <- list(
      ai_row_label(row),
      evidence,
      ai_yes_no(in_ml, lang = lang),
      ai_yes_no(in_network, lang = lang),
      ai_yes_no(in_key, lang = lang),
      bundle$priority,
      bundle$recommendation
    )

    debug[nrow(debug) + 1, ] <- list(
      ifelse(is.na(original_taxon), "", original_taxon),
      ifelse(is.na(normalized_taxon), "", normalized_taxon),
      in_ml,
      in_network,
      in_key,
      bundle$priority
    )
  }

  ensure_dir(file.path(job_dir, "logs"))
  readr::write_csv(debug, file.path(job_dir, "logs", "integration_debug.csv"), na = "")

  has_overlap <- any(debug$matched_ml | debug$matched_network | debug$matched_key_taxa)
  intro <- if (lang == "zh") {
    if (has_overlap) {
      "以下菌群在多个证据来源中重复出现，建议优先关注。"
    } else {
      "当前趋势菌未与机器学习、网络中心性或 Key Taxa Score 的 Top 候选重叠，因此其证据支持较弱，不建议单独作为关键菌结论。"
    }
  } else {
    if (has_overlap) {
      "The following taxa recur across multiple evidence sources and should be prioritized."
    } else {
      "Current trend taxa do not overlap with ML, network centrality, or Key Taxa Score Top candidates, so their support is weak and they should not be used alone as key-taxon conclusions."
    }
  }

  paste(intro, "", ai_markdown_table(utils::head(out, 5)), sep = "\n")
}

build_key_taxa_interpretation <- function(job_dir) {
  key_path <- file.path(job_dir, "tables", "key_taxa_score.csv")
  key_summary_path <- file.path(job_dir, "json", "key_taxa_summary.json")
  if (!file.exists(key_path)) return(NULL)

  key_df <- read_optional_csv(key_path)
  if (is.null(key_df) || nrow(key_df) < 1) return(NULL)

  if ("rank" %in% names(key_df)) {
    key_df <- key_df[order(suppressWarnings(as.numeric(key_df$rank))), , drop = FALSE]
  } else if ("key_taxa_score" %in% names(key_df)) {
    key_df <- key_df[order(-suppressWarnings(as.numeric(key_df$key_taxa_score))), , drop = FALSE]
  }
  key_df <- utils::head(key_df, 5)

  labels <- if ("display_taxon" %in% names(key_df)) as.character(key_df$display_taxon) else vapply(seq_len(nrow(key_df)), function(i) ai_row_label(key_df[i, , drop = FALSE]), character(1))
  scores <- if ("key_taxa_score" %in% names(key_df)) vapply(key_df$key_taxa_score, ai_format_numeric, character(1), digits = 3) else rep("NA", length(labels))
  evidence <- if ("evidence_sources" %in% names(key_df)) as.character(key_df$evidence_sources) else rep("", length(labels))

  kt_sum <- if (file.exists(key_summary_path)) {
    tryCatch(jsonlite::read_json(key_summary_path, simplifyVector = TRUE), error = function(e) NULL)
  } else {
    NULL
  }
  used_sources <- kt_sum$used_sources %||% character(0)

  out <- data.frame(
    Taxon = labels,
    Score = scores,
    Evidence = evidence,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  paste(
    c(
      "# Key Taxa Interpretation",
      "",
      if (length(used_sources) > 0) {
        paste0("Current Key Taxa Score integrates: ", paste(used_sources, collapse = ", "), ".")
      } else {
        "Current Key Taxa Score summary is generated from available evidence only."
      },
      "",
      "Top ranked candidate taxa are summarized below:",
      "",
      ai_markdown_table(out)
    ),
    collapse = "\n"
  )
}

build_diff_interpretation <- function(job_dir) {
  inputs <- load_diff_inputs(job_dir)
  diff_summary <- inputs$summary
  diff_table <- inputs$diff
  sig_table <- inputs$sig
  lang <- detect_report_language()
  source_maps <- ai_build_source_evidence(job_dir)

  n_sig <- diff_summary$n_significant_taxa %||% sum(diff_table$fdr < 0.05, na.rm = TRUE)
  method <- diff_summary$method %||% "the configured differential test"
  tax_level <- diff_summary$tax_level %||% "the selected taxonomic level"
  group_var <- diff_summary$group_variable %||% "the grouping variable"

  sig_rows <- if (!is.null(sig_table) && nrow(sig_table) > 0) {
    sig_table
  } else {
    diff_table[!is.na(diff_table$fdr) & diff_table$fdr < 0.05, , drop = FALSE]
  }
  exploratory_pool <- diff_table[!is.na(diff_table$fdr) & diff_table$fdr >= 0.05, , drop = FALSE]
  exploratory_pool <- exploratory_pool[order(exploratory_pool$p_value, exploratory_pool$fdr), , drop = FALSE]

  sig_rows <- prepare_diff_taxa_for_ai(sig_rows, max_n = 10, drop_unclassified = FALSE, keep_single_unclassified = TRUE)
  exploratory_rows <- prepare_diff_taxa_for_ai(exploratory_pool, max_n = 5, drop_unclassified = TRUE, keep_single_unclassified = TRUE)

  if (lang != "zh") {
    return(paste(
      c(
        "# Differential Taxa Interpretation",
        "",
        paste0(
          "This local interpretation summarizes the differential abundance results for `",
          group_var, "` at the `", tax_level, "` level using `", method, "`."
        ),
        "",
        "## Significant taxa",
        "",
        build_significant_taxa_text(sig_rows, lang = "en"),
        "",
        "## Exploratory trends",
        "",
        build_exploratory_taxa_text(exploratory_rows, lang = "en"),
        "",
        "## Biological relevance",
        "",
        ai_build_biological_relevance(exploratory_rows, lang = "en"),
        "",
        "## Integration suggestions",
        "",
        ai_build_integration_suggestions(exploratory_rows, source_maps, job_dir = job_dir, lang = "en"),
        "",
        "## Caution",
        "",
        "These statements are statistically constrained summaries. They do not imply causation or mechanism.",
        "Exploratory trends are hypothesis-generating and require experimental validation."
      ),
      collapse = "\n"
    ))
  }

  intro <- if (n_sig > 0) {
    paste0(
      "本次在 `", tax_level, "` 水平基于 `", group_var, "` 分组采用 `", method,
      "` 进行差异丰度分析，检测到 ", n_sig,
      " 个 FDR < 0.05 的显著差异菌。这些结果可作为优先候选，但仍建议结合机器学习、网络中心性和 Key Taxa Score 共同判断。"
    )
  } else {
    paste0(
      "本次在 `", tax_level, "` 水平基于 `", group_var, "` 分组采用 `", method,
      "` 进行差异丰度分析，未检测到 FDR < 0.05 的显著差异菌。因此，当前结果更适合作为候选筛选线索，而不是单独形成关键菌结论。"
    )
  }

  paste(
    c(
      "# Differential Taxa Interpretation",
      "",
      intro,
      "",
      "## 显著差异菌",
      "",
      build_significant_taxa_text(sig_rows, lang = "zh"),
      "",
      "## 探索性趋势菌",
      "",
      "下表列出前 5 个探索性趋势菌，用于后续综合筛选，不应直接表述为显著差异结果。",
      "",
      ai_build_exploratory_table(exploratory_rows, lang = "zh"),
      "",
      "## 潜在生物学意义",
      "",
      ai_build_biological_relevance(exploratory_rows, lang = "zh"),
      "",
      "## 综合筛选建议",
      "",
      ai_build_integration_suggestions(exploratory_rows, source_maps, job_dir = job_dir, lang = "zh"),
      "",
      "## 结果解释限制",
      "",
      "本结果为统计关联层面的候选筛选，不代表因果关系或作用机制。趋势性菌群仍需结合分离菌株、功能测定、盆栽实验或宏基因组注释进一步验证。"
    ),
    collapse = "\n"
  )
}

write_ai_outputs <- function(job_dir) {
  ai_dir <- file.path(job_dir, "ai")
  dir.create(ai_dir, showWarnings = FALSE, recursive = TRUE)

  diff_path <- file.path(ai_dir, "diff_interpretation.md")
  methods_path <- file.path(ai_dir, "methods.md")
  legend_path <- file.path(ai_dir, "figure_legends.md")
  key_taxa_path <- file.path(ai_dir, "key_taxa_interpretation.md")

  writeLines(build_diff_interpretation(job_dir), diff_path, useBytes = TRUE)
  writeLines(build_methods_text(job_dir), methods_path, useBytes = TRUE)
  writeLines(build_figure_legends_text(job_dir), legend_path, useBytes = TRUE)

  key_taxa_md <- build_key_taxa_interpretation(job_dir)
  if (is.character(key_taxa_md) && nzchar(paste(key_taxa_md, collapse = ""))) {
    writeLines(key_taxa_md, key_taxa_path, useBytes = TRUE)
  } else if (file.exists(key_taxa_path)) {
    file.remove(key_taxa_path)
  }

  outputs <- list(
    diff_interpretation_path = normalizePath(diff_path, winslash = "/", mustWork = TRUE),
    methods_path = normalizePath(methods_path, winslash = "/", mustWork = TRUE),
    figure_legends_path = normalizePath(legend_path, winslash = "/", mustWork = TRUE)
  )
  if (file.exists(key_taxa_path)) {
    outputs$key_taxa_interpretation_path <- normalizePath(key_taxa_path, winslash = "/", mustWork = TRUE)
  }
  outputs
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

  if (is.null(api_key)) {
    message("SKIPPED: API key ", config$api_key_env, " is not set; LLM request was not sent.")
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

    writeLines(
      c(
        "# LLM Diff Interpretation",
        "",
        paste0("LLM request skipped because `", config$api_key_env, "` is not set."),
        "",
        read_text_optional(file.path(job_dir, "ai", "diff_interpretation.md"))
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
        read_text_optional(file.path(job_dir, "ai", "methods.md"))
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
        read_text_optional(file.path(job_dir, "ai", "figure_legends.md"))
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

    response <- invoke_llm_request(
      config = config,
      api_key = api_key,
      payload = request_payload,
      timeout_sec = config$timeout_sec %||% 120
    )
    response_record <- list(status = "success", response = response)

    response_text <- extract_llm_json_text(response)
    parsed <- parse_llm_response_text(response_text)$parsed

    writeLines(coerce_llm_markdown(parsed, "diff_interpretation", paste(read_text_optional(file.path(job_dir, "ai", "diff_interpretation.md")), collapse = "\n")), out_diff_path, useBytes = TRUE)
    writeLines(coerce_llm_markdown(parsed, "methods", paste(read_text_optional(file.path(job_dir, "ai", "methods.md")), collapse = "\n")), out_methods_path, useBytes = TRUE)
    writeLines(coerce_llm_markdown(parsed, "figure_legends", paste(read_text_optional(file.path(job_dir, "ai", "figure_legends.md")), collapse = "\n")), out_legends_path, useBytes = TRUE)
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
