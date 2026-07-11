# Phase 7: Key Taxa Score (business logic)
#
# This file must be runnable in plain Rscript mode (no Shiny runtime).
# It integrates 3 evidence sources:
# - differential abundance (Phase 3)
# - ML feature importance (Phase 5)
# - network centrality (Phase 6)

normalize_score <- function(x, higher_is_better = TRUE) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  x <- suppressWarnings(as.numeric(x))
  if (!isTRUE(higher_is_better)) x <- -x

  ok <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (!any(ok)) return(out)

  rng <- range(x[ok], na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    out[ok] <- 0
    return(out)
  }
  out[ok] <- (x[ok] - rng[1]) / (rng[2] - rng[1])
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x

if (!exists("clean_taxon_label", mode = "function")) {
  clean_taxon_label <- function(x) {
    # Display-only label cleaner; does not change any score computations.
    if (is.null(x)) return(character(0))
    x <- as.character(x)
    vapply(x, function(s) {
      if (is.na(s)) return(NA_character_)
      s <- trimws(s)
      if (!nzchar(s)) return(s)
      parts <- strsplit(s, "\\|")[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) s else parts[[length(parts)]]
    }, character(1))
  }
}

.wrap_plot_label <- function(x, width = 40) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (!nzchar(s)) return(s)
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1))
}

.pick_first_col <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  for (nm in candidates) if (nm %in% names(df)) return(nm)
  NULL
}

.safe_read_csv <- function(path) {
  if (is.null(path) || !is.character(path) || length(path) != 1) return(NULL)
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
  if (is.null(out) || !is.data.frame(out) || nrow(out) < 1) return(NULL)
  out
}

.standardize_taxon_keys <- function(df) {
  if (is.null(df)) return(NULL)

  taxon_col <- find_taxon_column(df, c("taxon_label", "display_taxon", "taxon", "Taxon", "FeatureID", "feature", "Feature", "Genus", "label", "name", "Name", "node", "Node", "taxa", "Taxa"))
  level_col <- .pick_first_col(df, c("tax_level", "tax_level_name", "tax_level_id", "TaxLevel", "level", "Level", "rank", "Rank"))
  raw_taxon_col <- .pick_first_col(df, c("taxon", "Taxon", "feature", "Feature", "FeatureID", "name", "Name", "node", "Node"))

  if (is.null(taxon_col)) return(NULL)
  out <- df
  out$match_taxon <- as.character(out[[taxon_col]])
  out$match_taxon <- trimws(out$match_taxon)
  out$match_taxon[out$match_taxon == ""] <- NA_character_
  out$normalized_taxon <- normalize_taxon_name(out$match_taxon)
  out$display_taxon <- taxon_display_from_any(out$match_taxon)
  out$original_taxon <- if (!is.null(raw_taxon_col)) as.character(out[[raw_taxon_col]]) else as.character(out[[taxon_col]])
  out$original_taxon <- trimws(out$original_taxon)
  out$original_taxon[out$original_taxon == ""] <- NA_character_

  if (!is.null(level_col)) {
    out$tax_level <- as.character(out[[level_col]])
    out$tax_level <- trimws(out$tax_level)
    out$tax_level[out$tax_level == ""] <- NA_character_
  } else {
    out$tax_level <- NA_character_
  }

  out
}

.collapse_taxon_rows <- function(df, primary_col) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(df)
  groups <- split(df, df$taxon, drop = TRUE)
  out <- lapply(groups, function(g) {
    primary <- suppressWarnings(as.numeric(g[[primary_col]]))
    idx <- if (any(is.finite(primary))) which.max(replace(primary, !is.finite(primary), -Inf)) else 1L
    g[idx[1], , drop = FALSE]
  })
  out <- dplyr::bind_rows(out)
  rownames(out) <- NULL
  out
}

calculate_differential_score <- function(diff_table) {
  if (is.null(diff_table) || !is.data.frame(diff_table) || nrow(diff_table) < 1) return(NULL)
  x <- .standardize_taxon_keys(diff_table)
  if (is.null(x)) return(NULL)

  fdr_col <- .pick_first_col(x, c("FDR", "fdr", "padj", "p_adj", "adj_p", "qvalue", "q_value"))
  lfc_col <- .pick_first_col(x, c("log2FC", "log2fc", "logFC", "log_fc", "LFC", "lfc"))
  effect_col <- .pick_first_col(x, c("effect_size", "effect", "epsilon_squared", "cliffs_delta"))
  if (is.null(fdr_col) || (is.null(lfc_col) && is.null(effect_col))) return(NULL)

  x$diff_fdr <- suppressWarnings(as.numeric(x[[fdr_col]]))
  x$log2fc <- if (is.null(lfc_col)) NA_real_ else suppressWarnings(as.numeric(x[[lfc_col]]))
  x$effect_size <- if (is.null(effect_col)) NA_real_ else suppressWarnings(as.numeric(x[[effect_col]]))

  magnitude <- ifelse(is.finite(x$log2fc), abs(x$log2fc), abs(x$effect_size))
  raw <- (-log10(pmax(x$diff_fdr, 1e-300))) * magnitude
  x$differential_score <- normalize_score(raw, higher_is_better = TRUE)

  dplyr::tibble(
    taxon = x$normalized_taxon,
    display_taxon = x$display_taxon,
    original_taxon = x$original_taxon,
    tax_level = x$tax_level,
    differential_score = x$differential_score,
    diff_fdr = x$diff_fdr,
    log2fc = x$log2fc,
    effect_size = x$effect_size
  ) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("differential_score")
}

calculate_ml_score <- function(ml_table) {
  if (is.null(ml_table) || !is.data.frame(ml_table) || nrow(ml_table) < 1) return(NULL)
  x <- .standardize_taxon_keys(ml_table)
  if (is.null(x)) return(NULL)

  imp_col <- .pick_first_col(x, c("importance", "rf_importance", "RF_importance", "MeanDecreaseAccuracy", "MeanDecreaseGini", "gain", "Gain", "score", "Score"))
  if (is.null(imp_col)) return(NULL)

  x$rf_importance <- suppressWarnings(as.numeric(x[[imp_col]]))
  x$ml_importance_score <- normalize_score(x$rf_importance, higher_is_better = TRUE)

  dplyr::tibble(
    taxon = x$normalized_taxon,
    display_taxon = x$display_taxon,
    original_taxon = x$original_taxon,
    tax_level = x$tax_level,
    ml_importance_score = x$ml_importance_score,
    rf_importance = x$rf_importance
  ) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("ml_importance_score")
}

calculate_network_score <- function(network_nodes) {
  if (is.null(network_nodes) || !is.data.frame(network_nodes) || nrow(network_nodes) < 1) return(NULL)
  x <- .standardize_taxon_keys(network_nodes)
  if (is.null(x)) return(NULL)

  deg_col <- .pick_first_col(x, c("degree", "Degree", "deg", "Deg"))
  bet_col <- .pick_first_col(x, c("betweenness", "Betweenness", "between", "Between"))
  if (is.null(deg_col) && is.null(bet_col)) return(NULL)

  x$degree <- if (!is.null(deg_col)) suppressWarnings(as.numeric(x[[deg_col]])) else NA_real_
  x$betweenness <- if (!is.null(bet_col)) suppressWarnings(as.numeric(x[[bet_col]])) else NA_real_
  raw <- (x$degree %||% 0) + (x$betweenness %||% 0)
  x$network_centrality_score <- normalize_score(raw, higher_is_better = TRUE)

  dplyr::tibble(
    taxon = x$normalized_taxon,
    display_taxon = x$display_taxon,
    original_taxon = x$original_taxon,
    tax_level = x$tax_level,
    network_centrality_score = x$network_centrality_score,
    degree = x$degree,
    betweenness = x$betweenness
  ) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("network_centrality_score")
}

.resolve_weights <- function(has_diff, has_ml, has_net) {
  if (isTRUE(has_diff) && isTRUE(has_ml) && isTRUE(has_net)) {
    return(list(weights = c(diff = 0.4, ml = 0.4, network = 0.2), used_sources = c("diff", "ml", "network")))
  }
  if (isTRUE(has_diff) && isTRUE(has_ml) && !isTRUE(has_net)) {
    return(list(weights = c(diff = 0.5, ml = 0.5), used_sources = c("diff", "ml")))
  }
  if (isTRUE(has_diff) && !isTRUE(has_ml) && isTRUE(has_net)) {
    return(list(weights = c(diff = 0.6, network = 0.4), used_sources = c("diff", "network")))
  }
  if (!isTRUE(has_diff) && isTRUE(has_ml) && isTRUE(has_net)) {
    return(list(weights = c(ml = 0.6, network = 0.4), used_sources = c("ml", "network")))
  }
  if (isTRUE(has_diff) && !isTRUE(has_ml) && !isTRUE(has_net)) {
    return(list(weights = c(diff = 1.0), used_sources = c("diff")))
  }
  if (!isTRUE(has_diff) && isTRUE(has_ml) && !isTRUE(has_net)) {
    return(list(weights = c(ml = 1.0), used_sources = c("ml")))
  }
  if (!isTRUE(has_diff) && !isTRUE(has_ml) && isTRUE(has_net)) {
    return(list(weights = c(network = 1.0), used_sources = c("network")))
  }
  list(weights = c(), used_sources = character())
}

.weight_value <- function(weights, key) {
  if (is.null(weights) || !length(weights) || is.null(names(weights)) || !key %in% names(weights)) {
    return(0)
  }
  suppressWarnings(as.numeric(weights[[key]]))
}

merge_key_taxa_evidence <- function(diff_score, ml_score, network_score) {
  # Start from the union of taxa across available sources.
  tabs <- list(diff = diff_score, ml = ml_score, network = network_score)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  if (length(tabs) == 0) {
    return(dplyr::tibble(
      taxon = character(0),
      tax_level = character(0),
      differential_score = numeric(0),
      ml_importance_score = numeric(0),
      network_centrality_score = numeric(0),
      diff_fdr = numeric(0),
      log2fc = numeric(0),
      effect_size = numeric(0),
      rf_importance = numeric(0),
      degree = numeric(0),
      betweenness = numeric(0)
    ))
  }

  join_with_labels <- function(a, b) {
    out <- dplyr::full_join(a, b, by = c("taxon", "tax_level"), suffix = c(".x", ".y"))
    for (base_col in c("display_taxon", "original_taxon")) {
      x_col <- paste0(base_col, ".x")
      y_col <- paste0(base_col, ".y")
      if (x_col %in% names(out) && y_col %in% names(out)) {
        out[[base_col]] <- dplyr::coalesce(out[[x_col]], out[[y_col]])
        out[[x_col]] <- NULL
        out[[y_col]] <- NULL
      }
    }
    out
  }
  out <- Reduce(join_with_labels, tabs)

  # Keep taxon keys clean.
  out <- out |>
    dplyr::filter(!is.na(.data$taxon)) |>
    dplyr::mutate(
      tax_level = dplyr::if_else(is.na(.data$tax_level), NA_character_, .data$tax_level)
    )

  # Ensure required numeric columns exist.
  needed <- c(
    "differential_score", "ml_importance_score", "network_centrality_score",
    "diff_fdr", "log2fc", "effect_size", "rf_importance", "degree", "betweenness"
  )
  for (nm in needed) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  out
}

.compute_evidence_fields <- function(df) {
  has_diff <- is.finite(df$differential_score)
  has_ml <- is.finite(df$ml_importance_score)
  has_net <- is.finite(df$network_centrality_score)

  df$evidence_count <- as.integer(has_diff) + as.integer(has_ml) + as.integer(has_net)
  df$evidence_sources <- vapply(seq_len(nrow(df)), function(i) {
    src <- character()
    if (isTRUE(has_diff[i])) src <- c(src, "diff")
    if (isTRUE(has_ml[i])) src <- c(src, "ml")
    if (isTRUE(has_net[i])) src <- c(src, "network")
    if (length(src) == 0) "" else paste(src, collapse = "|")
  }, character(1))
  df
}

.recommend_level <- function(score) {
  if (!is.finite(score)) return(NA_character_)
  if (score >= 0.75) return("High")
  if (score >= 0.50) return("Medium")
  "Low"
}

rank_key_taxa <- function(score_table, top_n = 20) {
  if (is.null(score_table) || !is.data.frame(score_table)) stop("rank_key_taxa(): score_table must be a data.frame.", call. = FALSE)
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) top_n <- 20

  score_table <- score_table |>
    dplyr::arrange(dplyr::desc(.data$key_taxa_score), dplyr::desc(.data$evidence_count), .data$taxon) |>
    dplyr::mutate(rank = dplyr::row_number())

  top <- score_table |>
    dplyr::filter(.data$rank <= top_n)

  list(score_table = score_table, top_table = top)
}

plot_key_taxa_score <- function(score_table, output_png, output_pdf, top_n = 20) {
  if (is.null(score_table) || !is.data.frame(score_table)) stop("plot_key_taxa_score(): score_table must be a data.frame.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("plot_key_taxa_score(): ggplot2 is required.", call. = FALSE)

  r <- rank_key_taxa(score_table, top_n = top_n)
  top <- r$top_table

  # If no rows, still write empty plot canvas to keep pipeline stable.
  if (nrow(top) < 1) {
    ensure_dir(dirname(output_png))
    ensure_dir(dirname(output_pdf))
    grDevices::png(output_png, width = 1600, height = 1000, res = 180)
    plot.new()
    text(0.5, 0.5, "No taxa available for Key Taxa Score", cex = 1.2)
    grDevices::dev.off()
    grDevices::pdf(output_pdf, width = 11, height = 7)
    plot.new()
    text(0.5, 0.5, "No taxa available for Key Taxa Score", cex = 1.2)
    grDevices::dev.off()
    return(invisible(list(png = output_png, pdf = output_pdf)))
  }

  # Taxon names can repeat (e.g., different tax_level or duplicates from upstream).
  # Use a unique label for plotting to avoid duplicated factor levels / overplotting.
  top <- top |>
    dplyr::mutate(
      .taxon_label = dplyr::if_else(
        is.na(.data$tax_level) | .data$tax_level == "",
        clean_taxon_label(as.character(.data$taxon)),
        paste0(clean_taxon_label(as.character(.data$taxon)), " [", as.character(.data$tax_level), "]")
      ),
      .taxon_label = .wrap_plot_label(.data$.taxon_label, width = 40),
      .taxon_label = make.unique(.data$.taxon_label, sep = " #")
    ) |>
    dplyr::mutate(.taxon_label = factor(.data$.taxon_label, levels = rev(.data$.taxon_label)))

  p <- ggplot2::ggplot(top, ggplot2::aes(x = .data$.taxon_label, y = .data$key_taxa_score, fill = .data$recommendation_level)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_fill_manual(values = c(High = "#D1495B", Medium = "#EDAE49", Low = "#00798C"), drop = FALSE) +
    ggplot2::labs(
      title = sprintf("Key Taxa Score (Top %d)", min(top_n, nrow(top))),
      x = NULL,
      y = "KeyTaxaScore (0-1)",
      fill = "Recommendation"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ensure_dir(dirname(output_png))
  ensure_dir(dirname(output_pdf))
  ggplot2::ggsave(output_png, plot = p, width = 11, height = 7, dpi = 220)
  ggplot2::ggsave(output_pdf, plot = p, width = 11, height = 7, device = grDevices::cairo_pdf)

  invisible(list(png = output_png, pdf = output_pdf))
}

summarize_key_taxa_for_ai <- function(score_table, used_sources, weights) {
  if (is.null(score_table) || !is.data.frame(score_table)) {
    score_table <- dplyr::tibble()
  }
  if (is.null(used_sources)) used_sources <- character()
  if (is.null(weights)) weights <- list()

  # Reliability rule: fewer than 2 evidence sources => exploratory.
  reliability <- if (length(used_sources) < 2) "exploratory" else "standard"

  top <- score_table |>
    dplyr::arrange(dplyr::desc(.data$key_taxa_score), dplyr::desc(.data$evidence_count), .data$taxon) |>
    dplyr::slice_head(n = 20) |>
    dplyr::mutate(
      key_taxa_score = round(.data$key_taxa_score, 4),
      differential_score = round(.data$differential_score, 4),
      ml_importance_score = round(.data$ml_importance_score, 4),
      network_centrality_score = round(.data$network_centrality_score, 4)
    )

  list(
    used_sources = used_sources,
    weights = weights,
    n_candidate_taxa = nrow(score_table),
    top_taxa = top |>
      dplyr::select("taxon", "tax_level", "key_taxa_score", "evidence_count", "evidence_sources", "recommendation_level") |>
      base::as.data.frame(),
    reliability = reliability
  )
}

calculate_key_taxa_score <- function(diff_table, ml_table, network_nodes, job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("calculate_key_taxa_score(): job_dir not found: ", job_dir, call. = FALSE)

  # Allow passing NULL or file paths; when NULL, try job_dir defaults.
  if (is.null(diff_table) || is.character(diff_table)) {
    diff_table <- .safe_read_csv(if (is.character(diff_table)) diff_table else file.path(job_dir, "tables", "differential_taxa.csv"))
  }
  if (is.null(ml_table) || is.character(ml_table)) {
    ml_table <- .safe_read_csv(if (is.character(ml_table)) ml_table else file.path(job_dir, "tables", "ml_feature_importance.csv"))
  }
  if (is.null(network_nodes) || is.character(network_nodes)) {
    network_nodes <- .safe_read_csv(if (is.character(network_nodes)) network_nodes else file.path(job_dir, "tables", "network_nodes.csv"))
  }

  diff_score <- calculate_differential_score(diff_table)
  ml_score <- calculate_ml_score(ml_table)
  net_score <- calculate_network_score(network_nodes)

  has_diff <- !is.null(diff_score) && nrow(diff_score) > 0
  has_ml <- !is.null(ml_score) && nrow(ml_score) > 0
  has_net <- !is.null(net_score) && nrow(net_score) > 0

  w <- .resolve_weights(has_diff, has_ml, has_net)
  weights <- w$weights
  used_sources <- w$used_sources

  merged <- merge_key_taxa_evidence(diff_score, ml_score, net_score)
  merged <- .compute_evidence_fields(merged)

  # Weighted score using only available evidence per taxon.
  # This prevents NA scores when (for example) differential evidence exists in the run
  # but a particular taxon only has ML/network evidence.
  if (length(weights) == 0) {
    merged$key_taxa_score <- NA_real_
  } else {
    w_diff <- .weight_value(weights, "diff")
    w_ml <- .weight_value(weights, "ml")
    w_net <- .weight_value(weights, "network")

    d <- merged$differential_score
    m <- merged$ml_importance_score
    n <- merged$network_centrality_score

    has_d <- is.finite(d)
    has_m <- is.finite(m)
    has_n <- is.finite(n)

    denom <- (has_d * w_diff) + (has_m * w_ml) + (has_n * w_net)
    num <- (ifelse(has_d, d, 0) * w_diff) + (ifelse(has_m, m, 0) * w_ml) + (ifelse(has_n, n, 0) * w_net)
    merged$key_taxa_score <- ifelse(denom > 0, num / denom, NA_real_)
  }

  merged$recommendation_level <- vapply(merged$key_taxa_score, .recommend_level, character(1))
  merged$display_taxon <- ifelse(
    is.na(merged$display_taxon) | !nzchar(merged$display_taxon),
    taxon_display_from_any(as.character(merged$original_taxon %||% merged$taxon)),
    as.character(merged$display_taxon)
  )
  merged$full_taxon <- as.character(merged$original_taxon %||% merged$taxon)
  merged$normalized_taxon <- as.character(merged$taxon)
  merged$tax_level_display <- ifelse(is.na(merged$tax_level) | merged$tax_level == "", "Feature-level", as.character(merged$tax_level))

  ranked <- rank_key_taxa(merged, top_n = 20)
  score_table <- ranked$score_table
  top20 <- ranked$top_table

  # Required columns (ensure exist and order them).
  required_cols <- c(
    "taxon",
    "display_taxon",
    "normalized_taxon",
    "original_taxon",
    "tax_level",
    "tax_level_display",
    "differential_score",
    "ml_importance_score",
    "network_centrality_score",
    "key_taxa_score",
    "diff_fdr",
    "log2fc",
    "rf_importance",
    "degree",
    "betweenness",
    "evidence_count",
    "evidence_sources",
    "rank",
    "recommendation_level",
    "full_taxon"
  )
  for (nm in required_cols) if (!nm %in% names(score_table)) score_table[[nm]] <- NA
  score_table <- score_table[, required_cols]

  # Outputs.
  tables_dir <- file.path(job_dir, "tables")
  figs_dir <- file.path(job_dir, "figures")
  json_dir <- file.path(job_dir, "json")
  ensure_dir(tables_dir)
  ensure_dir(figs_dir)
  ensure_dir(json_dir)

  out_score_csv <- file.path(tables_dir, "key_taxa_score.csv")
  out_top_csv <- file.path(tables_dir, "key_taxa_top20.csv")
  out_json <- file.path(json_dir, "key_taxa_summary.json")
  out_png <- file.path(figs_dir, "key_taxa_score_barplot.png")
  out_pdf <- file.path(figs_dir, "key_taxa_score_barplot.pdf")

  readr::write_csv(score_table, out_score_csv, na = "")
  readr::write_csv(top20, out_top_csv, na = "")

  plot_key_taxa_score(score_table, out_png, out_pdf, top_n = 20)

  summary <- summarize_key_taxa_for_ai(score_table, used_sources = used_sources, weights = as.list(weights))
  write_json_pretty(summary, out_json, auto_unbox = TRUE)

  append_reproducibility(job_dir, list(
    phase7 = list(
      started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      used_sources = used_sources,
      weights = as.list(weights),
      reliability = summary$reliability,
      n_candidate_taxa = summary$n_candidate_taxa,
      finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  ))

  list(
    used_sources = used_sources,
    weights = as.list(weights),
    reliability = summary$reliability,
    n_candidate_taxa = summary$n_candidate_taxa,
    outputs = list(
      key_taxa_score_csv = normalizePath(out_score_csv, winslash = "/", mustWork = TRUE),
      key_taxa_top20_csv = normalizePath(out_top_csv, winslash = "/", mustWork = TRUE),
      key_taxa_summary_json = normalizePath(out_json, winslash = "/", mustWork = TRUE),
      barplot_png = normalizePath(out_png, winslash = "/", mustWork = TRUE),
      barplot_pdf = normalizePath(out_pdf, winslash = "/", mustWork = TRUE)
    )
  )
}
