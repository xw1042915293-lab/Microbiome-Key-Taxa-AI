# Phase 7: Key Taxa Score (business logic)
# Integrates 4 evidence sources:
#   - differential abundance, ML importance, network centrality, consensus
# Default: KeyTaxaScore = 0.35*Diff + 0.30*ML + 0.25*Network + 0.10*Consensus
# Weights auto-redistribute when a module is missing.

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


# --- Evidence score calculators ---

calculate_differential_score <- function(diff_table) {
  if (is.null(diff_table) || !is.data.frame(diff_table) || nrow(diff_table) < 1) return(NULL)
  x <- .standardize_taxon_keys(diff_table)
  if (is.null(x)) return(NULL)
  fdr_col <- .pick_first_col(x, c("FDR","fdr","padj","p_adj","adj_p","qvalue","q_value"))
  lfc_col <- .pick_first_col(x, c("log2FC","log2fc","logFC","log_fc","LFC","lfc"))
  if (is.null(fdr_col) || is.null(lfc_col)) return(NULL)
  x$diff_fdr <- suppressWarnings(as.numeric(x[[fdr_col]]))
  x$log2fc <- suppressWarnings(as.numeric(x[[lfc_col]]))
  raw <- (-log10(pmax(x$diff_fdr, 1e-300))) * abs(x$log2fc)
  x$differential_score <- normalize_score(raw, higher_is_better = TRUE)
  dplyr::tibble(taxon = x$normalized_taxon, display_taxon = x$display_taxon, original_taxon = x$original_taxon,
    tax_level = x$tax_level, differential_score = x$differential_score, diff_fdr = x$diff_fdr, log2fc = x$log2fc) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("differential_score")
}

calculate_ml_score <- function(ml_table) {
  if (is.null(ml_table) || !is.data.frame(ml_table) || nrow(ml_table) < 1) return(NULL)
  x <- .standardize_taxon_keys(ml_table)
  if (is.null(x)) return(NULL)
  imp_col <- .pick_first_col(x, c("importance","Importance","MeanDecreaseGini","mean_decrease_gini","feature_importance","rf_importance","value","score"))
  if (is.null(imp_col)) return(NULL)
  x$raw_importance <- suppressWarnings(as.numeric(x[[imp_col]]))
  x$ml_importance_score <- normalize_score(x$raw_importance, higher_is_better = TRUE)
  dplyr::tibble(taxon = x$normalized_taxon, display_taxon = x$display_taxon, original_taxon = x$original_taxon,
    tax_level = x$tax_level, ml_importance_score = x$ml_importance_score, rf_importance = x$raw_importance) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("ml_importance_score")
}

calculate_network_score <- function(network_nodes) {
  if (is.null(network_nodes) || !is.data.frame(network_nodes) || nrow(network_nodes) < 1) return(NULL)
  x <- .standardize_taxon_keys(network_nodes)
  if (is.null(x)) return(NULL)
  deg_col <- .pick_first_col(x, c("degree","Degree","deg","Deg"))
  bet_col <- .pick_first_col(x, c("betweenness","Betweenness","between","Between"))
  if (is.null(deg_col) && is.null(bet_col)) return(NULL)
  x$degree <- if (!is.null(deg_col)) suppressWarnings(as.numeric(x[[deg_col]])) else NA_real_
  x$betweenness <- if (!is.null(bet_col)) suppressWarnings(as.numeric(x[[bet_col]])) else NA_real_
  raw <- (x$degree %||% 0) + (x$betweenness %||% 0)
  x$network_centrality_score <- normalize_score(raw, higher_is_better = TRUE)
  dplyr::tibble(taxon = x$normalized_taxon, display_taxon = x$display_taxon, original_taxon = x$original_taxon,
    tax_level = x$tax_level, network_centrality_score = x$network_centrality_score, degree = x$degree, betweenness = x$betweenness) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("network_centrality_score")
}

calculate_consensus_score <- function(consensus_table) {
  if (is.null(consensus_table) || !is.data.frame(consensus_table) || nrow(consensus_table) < 1) return(NULL)
  x <- .standardize_taxon_keys(consensus_table)
  if (is.null(x)) return(NULL)
  ec_col <- .pick_first_col(x, c("evidence_count","evidence_methods_count","n_methods","n_evidence"))
  if (is.null(ec_col)) return(NULL)
  x$raw_evidence_count <- suppressWarnings(as.numeric(x[[ec_col]]))
  em_col <- .pick_first_col(x, c("evidence_methods","methods","evidence_sources"))
  x$evidence_methods_raw <- if (!is.null(em_col)) as.character(x[[em_col]]) else NA_character_
  x$consensus_score <- normalize_score(x$raw_evidence_count, higher_is_better = TRUE)
  dplyr::tibble(taxon = x$normalized_taxon, display_taxon = x$display_taxon, original_taxon = x$original_taxon,
    tax_level = x$tax_level, consensus_score = x$consensus_score,
    consensus_evidence_count = x$raw_evidence_count, consensus_evidence_methods = x$evidence_methods_raw) |>
    dplyr::filter(!is.na(.data$taxon), nzchar(.data$taxon)) |>
    .collapse_taxon_rows("consensus_score")
}


# --- Weight resolution (4 sources) ---
.resolve_weights <- function(has_diff, has_ml, has_net, has_consensus) {
  default_w <- c(diff = 0.35, ml = 0.30, network = 0.25, consensus = 0.10)
  available <- c(diff = isTRUE(has_diff), ml = isTRUE(has_ml), network = isTRUE(has_net), consensus = isTRUE(has_consensus))
  used_sources <- names(available)[available]
  if (sum(available) == 0) return(list(weights = numeric(0), used_sources = character(0)))
  w <- default_w[available]
  w <- w / sum(w)
  list(weights = w, used_sources = used_sources)
}

.weight_value <- function(weights, key) {
  if (is.null(weights) || !length(weights) || is.null(names(weights)) || !key %in% names(weights)) return(0)
  suppressWarnings(as.numeric(weights[[key]]))
}

.recommend_level <- function(score) {
  if (!is.finite(score)) return(NA_character_)
  if (score >= 0.75) return("High")
  if (score >= 0.50) return("Medium")
  "Low"
}

# --- Merge evidence ---
merge_key_taxa_evidence <- function(diff_score, ml_score, network_score, consensus_score) {
  tabs <- list(diff = diff_score, ml = ml_score, network = network_score, consensus = consensus_score)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  if (length(tabs) == 0) {
    return(dplyr::tibble(taxon = character(0), tax_level = character(0),
      differential_score = numeric(0), ml_importance_score = numeric(0),
      network_centrality_score = numeric(0), consensus_score = numeric(0),
      diff_fdr = numeric(0), log2fc = numeric(0), rf_importance = numeric(0),
      degree = numeric(0), betweenness = numeric(0),
      consensus_evidence_count = numeric(0), consensus_evidence_methods = character(0)))
  }
  join_with_labels <- function(a, b) {
    out <- dplyr::full_join(a, b, by = c("taxon", "tax_level"), suffix = c(".x", ".y"))
    for (base_col in c("display_taxon", "original_taxon")) {
      x_col <- paste0(base_col, ".x"); y_col <- paste0(base_col, ".y")
      if (x_col %in% names(out) && y_col %in% names(out)) {
        out[[base_col]] <- dplyr::coalesce(out[[x_col]], out[[y_col]])
        out[[x_col]] <- NULL; out[[y_col]] <- NULL
      }
    }
    out
  }
  out <- Reduce(join_with_labels, tabs)
  out <- out |> dplyr::filter(!is.na(.data$taxon))
  needed <- c("differential_score","ml_importance_score","network_centrality_score","consensus_score",
    "diff_fdr","log2fc","rf_importance","degree","betweenness","consensus_evidence_count","consensus_evidence_methods")
  for (nm in needed) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  if (!"display_taxon" %in% names(out)) out$display_taxon <- NA_character_
  if (!"original_taxon" %in% names(out)) out$original_taxon <- NA_character_
  out
}

.compute_evidence_fields <- function(df) {
  has_diff <- is.finite(df$differential_score)
  has_ml <- is.finite(df$ml_importance_score)
  has_net <- is.finite(df$network_centrality_score)
  has_cons <- is.finite(df$consensus_score)
  df$evidence_count <- as.integer(has_diff) + as.integer(has_ml) + as.integer(has_net) + as.integer(has_cons)
  df$evidence_methods <- vapply(seq_len(nrow(df)), function(i) {
    src <- character()
    if (isTRUE(has_diff[i])) src <- c(src, "Differential")
    if (isTRUE(has_ml[i])) src <- c(src, "ML")
    if (isTRUE(has_net[i])) src <- c(src, "Network")
    if (isTRUE(has_cons[i])) src <- c(src, "Consensus")
    if (length(src) == 0) "none" else paste(src, collapse = "; ")
  }, character(1))
  df$reliability_label <- vapply(df$evidence_count, function(ec) {
    if (!is.finite(ec) || ec == 0) return("weak_evidence")
    if (ec == 1) return("exploratory")
    if (ec == 2) return("moderate_confidence")
    "high_confidence"
  }, character(1))
  df
}

# --- Ranking ---
rank_key_taxa <- function(score_table, top_n = 20) {
  if (is.null(score_table) || !is.data.frame(score_table)) stop("rank_key_taxa(): score_table must be a data.frame.", call. = FALSE)
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) top_n <- 20
  score_table <- score_table |>
    dplyr::arrange(dplyr::desc(.data$key_taxa_score), dplyr::desc(.data$evidence_count), .data$taxon) |>
    dplyr::mutate(rank = dplyr::row_number())
  top <- score_table |> dplyr::filter(.data$rank <= top_n)
  list(score_table = score_table, top_table = top)
}
# --- Plotting ---
plot_key_taxa_score <- function(score_table, output_png, output_pdf, top_n = 20) {
  if (is.null(score_table) || !is.data.frame(score_table) || nrow(score_table) < 1) {
    message("plot_key_taxa_score: no data to plot."); return(invisible(NULL))
  }
  top <- utils::head(score_table[order(score_table$key_taxa_score, decreasing = TRUE, na.last = TRUE), ], top_n)
  if (nrow(top) < 1) return(invisible(NULL))
  top$plot_label <- .wrap_plot_label(top$display_taxon, width = 30)
  top$plot_label <- factor(top$plot_label, levels = rev(top$plot_label))
  if (!"reliability_label" %in% names(top)) top$reliability_label <- "weak_evidence"
  p <- ggplot2::ggplot(top, ggplot2::aes(x = .data$key_taxa_score, y = .data$plot_label, fill = .data$reliability_label)) +
    ggplot2::geom_col(width = 0.7, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c(high_confidence = "#1a9641", moderate_confidence = "#a6d96a",
      exploratory = "#fee08b", weak_evidence = "#d73027"), name = "Reliability") +
    ggplot2::labs(x = "Key Taxa Score", y = NULL, title = paste0("Top ", top_n, " Key Taxa Score")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9), plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
  tryCatch(ggplot2::ggsave(output_png, p, width = 10, height = max(4, nrow(top) * 0.35 + 1.5), dpi = 150, bg = "white"),
    error = function(e) warning("Failed to save PNG barplot: ", conditionMessage(e), call. = FALSE))
  tryCatch(ggplot2::ggsave(output_pdf, p, width = 10, height = max(4, nrow(top) * 0.35 + 1.5), bg = "white"),
    error = function(e) warning("Failed to save PDF barplot (PNG saved): ", conditionMessage(e), call. = FALSE))
  invisible(p)
}
plot_key_taxa_upset <- function(score_table, out_png, out_pdf) {
  if (is.null(score_table) || !is.data.frame(score_table) || nrow(score_table) < 1) {
    message("plot_key_taxa_upset: no data."); return(invisible(NULL))
  }
  set_mat <- data.frame(
    Differential = as.integer(!is.na(score_table$differential_score)),
    ML = as.integer(!is.na(score_table$ml_importance_score)),
    Network = as.integer(!is.na(score_table$network_centrality_score)),
    Consensus = as.integer(!is.na(score_table$consensus_score)),
    stringsAsFactors = FALSE)
  upset_plot <- NULL
  if (requireNamespace("UpSetR", quietly = TRUE)) {
    upset_plot <- tryCatch(UpSetR::upset(set_mat, order.by = "freq", empty.intersections = "on",
      main.bar.color = "#2563eb", sets.bar.color = "#6b7280", text.scale = 1.2),
      error = function(e) { message("UpSetR failed, using ggplot2 fallback"); NULL })
  }
  if (is.null(upset_plot)) {
    set_sizes <- colSums(set_mat)
    sd <- data.frame(Evidence = names(set_sizes), Count = as.integer(set_sizes), stringsAsFactors = FALSE)
    sd$Evidence <- factor(sd$Evidence, levels = sd$Evidence[order(sd$Count, decreasing = TRUE)])
    p <- ggplot2::ggplot(sd, ggplot2::aes(x = .data$Evidence, y = .data$Count, fill = .data$Evidence)) +
      ggplot2::geom_col(width = 0.6) +
      ggplot2::scale_fill_manual(values = c(Differential = "#e41a1c", ML = "#377eb8", Network = "#4daf4a", Consensus = "#984ea3"), guide = "none") +
      ggplot2::labs(x = "Evidence Type", y = "Number of Taxa", title = "Evidence Support by Type") +
      ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    combo <- apply(set_mat, 1, function(r) {
      parts <- names(set_mat)[which(r == 1)]
      if (length(parts) == 0) return("none")
      paste(sort(parts), collapse = " + ")
    })
    combo_tab <- sort(table(combo), decreasing = TRUE)
    cd <- data.frame(Combination = names(combo_tab), Count = as.integer(combo_tab), stringsAsFactors = FALSE)
    cd$Combination <- factor(cd$Combination, levels = cd$Combination)
    q <- ggplot2::ggplot(cd, ggplot2::aes(x = .data$Combination, y = .data$Count)) +
      ggplot2::geom_col(width = 0.7, fill = "#2563eb") +
      ggplot2::labs(x = "Evidence Combination", y = "Number of Taxa", title = "Taxa by Evidence Intersection") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8), plot.title = ggplot2::element_text(face = "bold"))
    if (requireNamespace("patchwork", quietly = TRUE)) { upset_plot <- p + q + patchwork::plot_layout(ncol = 1) }
    else { upset_plot <- q }
  }
  tryCatch({
    if (inherits(upset_plot, "gg")) ggplot2::ggsave(out_png, upset_plot, width = 10, height = 7, dpi = 150, bg = "white")
    else { grDevices::png(out_png, width = 10, height = 7, units = "in", res = 150); print(upset_plot); grDevices::dev.off() }
  }, error = function(e) warning("Failed to save PNG upset: ", conditionMessage(e), call. = FALSE))
  tryCatch({
    if (inherits(upset_plot, "gg")) ggplot2::ggsave(out_pdf, upset_plot, width = 10, height = 7, bg = "white")
    else { grDevices::pdf(out_pdf, width = 10, height = 7); print(upset_plot); grDevices::dev.off() }
  }, error = function(e) warning("Failed to save PDF upset (PNG saved): ", conditionMessage(e), call. = FALSE))
  invisible(upset_plot)
}
plot_key_taxa_evidence_heatmap <- function(score_table, evidence_matrix, out_png, out_pdf) {
  if (is.null(evidence_matrix) || !is.data.frame(evidence_matrix) || nrow(evidence_matrix) < 1) {
    message("plot_key_taxa_evidence_heatmap: no data."); return(invisible(NULL))
  }
  em <- evidence_matrix
  taxon_col <- .pick_first_col(em, c("taxon","display_taxon"))
  if (is.null(taxon_col)) return(invisible(NULL))
  mat_cols <- intersect(c("differential_score","ml_importance_score","network_centrality_score","consensus_score"), names(em))
  if (length(mat_cols) == 0) return(invisible(NULL))
  mat <- as.matrix(em[, mat_cols, drop = FALSE]); mode(mat) <- "numeric"; mat[is.na(mat)] <- 0
  rownames(mat) <- as.character(em[[taxon_col]])
  display_map <- c(differential_score = "Differential", ml_importance_score = "ML",
    network_centrality_score = "Network", consensus_score = "Consensus")
  colnames(mat) <- unname(display_map[mat_cols])
  if (nrow(mat) > 30) mat <- mat[1:30, , drop = FALSE]
  p <- tryCatch({
    mdf <- reshape2::melt(mat); names(mdf) <- c("Taxon","Evidence","Score")
    mdf$Taxon <- factor(mdf$Taxon, levels = rev(rownames(mat)))
    ggplot2::ggplot(mdf, ggplot2::aes(x = .data$Evidence, y = .data$Taxon, fill = .data$Score)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient(low = "#f7f7f7", high = "#2166ac", name = "Score") +
      ggplot2::labs(x = "Evidence Type", y = NULL, title = "Key Taxa Evidence Heatmap") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8), plot.title = ggplot2::element_text(face = "bold"))
  }, error = function(e) { warning("Heatmap plot failed: ", conditionMessage(e)); NULL })
  if (is.null(p)) return(invisible(NULL))
  tryCatch(ggplot2::ggsave(out_png, p, width = 8, height = max(5, nrow(mat) * 0.3 + 2), dpi = 150, bg = "white"),
    error = function(e) warning("Failed to save PNG heatmap: ", conditionMessage(e), call. = FALSE))
  tryCatch(ggplot2::ggsave(out_pdf, p, width = 8, height = max(5, nrow(mat) * 0.3 + 2), bg = "white"),
    error = function(e) warning("Failed to save PDF heatmap (PNG saved): ", conditionMessage(e), call. = FALSE))
  invisible(p)
}

# --- Evidence cards ---
build_evidence_cards <- function(score_table) {
  if (is.null(score_table) || !is.data.frame(score_table) || nrow(score_table) < 1) {
    return(dplyr::tibble(taxon = character(), rank = integer(), key_taxa_score = numeric(),
      reliability_label = character(), differential_summary = character(), ml_summary = character(),
      network_summary = character(), consensus_summary = character(), caution_note = character()))
  }
  cards <- lapply(seq_len(nrow(score_table)), function(i) {
    row <- score_table[i, , drop = FALSE]
    tax <- as.character(row$taxon)
    diff_sum <- if (is.finite(row$differential_score)) {
      sprintf("score=%.3f, FDR=%s, log2FC=%s", row$differential_score,
        if (is.finite(row$diff_fdr)) formatC(row$diff_fdr, format = "e", digits = 2) else "NA",
        if (is.finite(row$log2fc)) formatC(row$log2fc, digits = 3, format = "f") else "NA")
    } else { "No evidence" }
    ml_sum <- if (is.finite(row$ml_importance_score)) {
      sprintf("score=%.3f, importance=%s", row$ml_importance_score,
        if (is.finite(row$rf_importance)) formatC(row$rf_importance, digits = 4, format = "f") else "NA")
    } else { "No evidence" }
    net_sum <- if (is.finite(row$network_centrality_score)) {
      sprintf("score=%.3f, degree=%s, betweenness=%s", row$network_centrality_score,
        if (is.finite(row$degree)) formatC(row$degree, digits = 1) else "NA",
        if (is.finite(row$betweenness)) formatC(row$betweenness, digits = 2) else "NA")
    } else { "No evidence" }
    cons_sum <- if (is.finite(row$consensus_score)) {
      sprintf("score=%.3f, ec=%s, methods=%s", row$consensus_score,
        as.character(row$consensus_evidence_count %||% "NA"), as.character(row$consensus_evidence_methods %||% "NA"))
    } else { "No evidence" }
    ec <- if (is.finite(row$evidence_count)) as.integer(row$evidence_count) else 0L
    caution <- if (ec < 2) "Exploratory: <2 evidence sources. Experimental validation required."
      else "Key Taxa Score is a ranking metric, not validated function. Experimental verification required."
    dplyr::tibble(taxon = tax, rank = if (is.finite(row$rank)) as.integer(row$rank) else NA_integer_,
      key_taxa_score = row$key_taxa_score, reliability_label = as.character(row$reliability_label %||% "weak_evidence"),
      differential_summary = diff_sum, ml_summary = ml_sum, network_summary = net_sum,
      consensus_summary = cons_sum, caution_note = caution)
  })
  dplyr::bind_rows(cards)
}
# --- AI summary JSON ---
summarize_key_taxa_for_ai <- function(score_table, used_sources, weights) {
  if (is.null(score_table) || !is.data.frame(score_table) || nrow(score_table) < 1) {
    return(list(analysis_type = "key_taxa_score",
      scoring_formula = "KeyTaxaScore = 0.35*Diff + 0.30*ML + 0.25*Network + 0.10*Consensus (auto-redistributed if module missing)",
      available_evidence_modules = character(0),
      missing_evidence_modules = c("Differential","ML","Network","Consensus"),
      n_candidate_taxa = 0, top_key_taxa = list(),
      high_confidence_count = 0, moderate_confidence_count = 0, exploratory_count = 0, weak_evidence_count = 0,
      caution_notes = list("No candidate key taxa identified.",
        "Key Taxa are candidates, not validated functional taxa. Experimental verification required.",
        "Do not interpret correlation as causation."),
      reliability_notes = list("No results available for reliability assessment."),
      reliability = "none", weights = weights, used_sources = used_sources,
      top_taxa = list()))
  }
  all_modules <- c("Differential","ML","Network","Consensus")
  name_map <- c(diff = "Differential", ml = "ML", network = "Network", consensus = "Consensus")
  available_display <- unname(name_map[used_sources %||% character(0)])
  available_display <- available_display[!is.na(available_display)]
  missing <- setdiff(all_modules, available_display)
  hc <- sum(score_table$reliability_label == "high_confidence", na.rm = TRUE)
  mc <- sum(score_table$reliability_label == "moderate_confidence", na.rm = TRUE)
  ex <- sum(score_table$reliability_label == "exploratory", na.rm = TRUE)
  we <- sum(score_table$reliability_label == "weak_evidence", na.rm = TRUE)
  top10 <- utils::head(score_table, 10)
  top_list <- lapply(seq_len(nrow(top10)), function(i) {
    list(taxon = as.character(top10$taxon[i]),
      display_taxon = as.character(top10$display_taxon[i] %||% top10$taxon[i]),
      key_taxa_score = top10$key_taxa_score[i],
      evidence_count = as.integer(top10$evidence_count[i]),
      evidence_methods = as.character(top10$evidence_methods[i]),
      reliability_label = as.character(top10$reliability_label[i]))
  })
  caution_notes <- list(
    "Key Taxa are candidate taxa, NOT validated functional taxa.",
    "Do not interpret Key Taxa Score or evidence co-occurrence as causal relationships.",
    "All candidate key taxa require experimental validation before biological conclusions can be drawn.")
  if (ex > 0 || we > 0) caution_notes <- c(caution_notes, list(
    paste0(sum(ex, we), " taxa with evidence_count < 2 are exploratory/weak. Interpret with extreme caution.")))
  if (length(missing) > 0) caution_notes <- c(caution_notes, list(
    paste0("Missing evidence modules: ", paste(missing, collapse = ", "), ". Reliability is reduced.")))
  reliability_notes <- list(
    "high_confidence (>=3 evidence): supported by multiple independent evidence types.",
    "moderate_confidence (2 evidence): supported by two evidence types.",
    "exploratory (1 evidence): hypothesis-generating only.",
    "weak_evidence (0 evidence): exclude from interpretation.")
  overall <- if (hc > 0) "high_confidence_candidates_present" else if (mc > 0) "moderate_only" else "exploratory_only"
  list(analysis_type = "key_taxa_score",
    scoring_formula = "KeyTaxaScore = 0.35*DifferentialScore + 0.30*MLImportanceScore + 0.25*NetworkCentralityScore + 0.10*ConsensusScore (auto-redistributed)",
    available_evidence_modules = available_display, missing_evidence_modules = missing,
    n_candidate_taxa = nrow(score_table), top_key_taxa = top_list,
    high_confidence_count = hc, moderate_confidence_count = mc, exploratory_count = ex, weak_evidence_count = we,
    caution_notes = caution_notes, reliability_notes = reliability_notes,
    reliability = overall, weights = weights, used_sources = available_display,
    top_taxa = top_list)
}

# --- Main orchestrator ---
calculate_key_taxa_score <- function(diff_table = NULL, ml_table = NULL, network_nodes = NULL, consensus_table = NULL, job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("calculate_key_taxa_score(): job_dir not found: ", job_dir, call. = FALSE)
  if (is.null(diff_table) || is.character(diff_table)) {
    diff_table <- .safe_read_csv(if (is.character(diff_table)) diff_table else file.path(job_dir, "tables", "differential_taxa.csv"))
  }
  if (is.null(ml_table) || is.character(ml_table)) {
    ml_table <- .safe_read_csv(if (is.character(ml_table)) ml_table else file.path(job_dir, "tables", "ml_feature_importance.csv"))
  }
  if (is.null(network_nodes) || is.character(network_nodes)) {
    network_nodes <- .safe_read_csv(if (is.character(network_nodes)) network_nodes else file.path(job_dir, "tables", "network_nodes.csv"))
  }
  if (is.null(consensus_table) || is.character(consensus_table)) {
    consensus_table <- .safe_read_csv(if (is.character(consensus_table)) consensus_table else file.path(job_dir, "tables", "diff_consensus.csv"))
  }
  diff_score <- calculate_differential_score(diff_table)
  ml_score <- calculate_ml_score(ml_table)
  net_score <- calculate_network_score(network_nodes)
  cons_score <- calculate_consensus_score(consensus_table)
  has_diff <- !is.null(diff_score) && nrow(diff_score) > 0
  has_ml <- !is.null(ml_score) && nrow(ml_score) > 0
  has_net <- !is.null(net_score) && nrow(net_score) > 0
  has_cons <- !is.null(cons_score) && nrow(cons_score) > 0
  w <- .resolve_weights(has_diff, has_ml, has_net, has_cons)
  weights <- w$weights; used_sources <- w$used_sources
  merged <- merge_key_taxa_evidence(diff_score, ml_score, net_score, cons_score)
  if (is.null(merged) || !is.data.frame(merged) || nrow(merged) < 1) {
    message("calculate_key_taxa_score: no candidate taxa found.")
    tables_dir <- file.path(job_dir, "tables"); figs_dir <- file.path(job_dir, "figures"); json_dir <- file.path(job_dir, "json")
    ensure_dir(tables_dir); ensure_dir(figs_dir); ensure_dir(json_dir)
    empty <- dplyr::tibble(taxon = character(), display_taxon = character(), tax_level = character(),
      differential_score = numeric(), ml_importance_score = numeric(), network_centrality_score = numeric(),
      consensus_score = numeric(), key_taxa_score = numeric(), evidence_count = integer(),
      evidence_methods = character(), rank = integer(), reliability_label = character())
    readr::write_csv(empty, file.path(tables_dir, "key_taxa_score.csv"), na = "")
    readr::write_csv(empty, file.path(tables_dir, "key_taxa_top20.csv"), na = "")
    readr::write_csv(empty, file.path(tables_dir, "key_taxa_evidence_matrix.csv"), na = "")
    readr::write_csv(empty, file.path(tables_dir, "key_taxa_evidence_cards.csv"), na = "")
    summary <- summarize_key_taxa_for_ai(NULL, used_sources = used_sources, weights = as.list(weights))
    write_json_pretty(summary, file.path(json_dir, "key_taxa_summary.json"), auto_unbox = TRUE)
    return(list(score_result = list(used_sources = used_sources, weights = as.list(weights)), summary = summary))
  }
  merged <- .compute_evidence_fields(merged)
  if (length(weights) == 0) {
    merged$key_taxa_score <- NA_real_
  } else {
    w_diff <- .weight_value(weights, "diff"); w_ml <- .weight_value(weights, "ml")
    w_net <- .weight_value(weights, "network"); w_cons <- .weight_value(weights, "consensus")
    d <- merged$differential_score; m <- merged$ml_importance_score
    n <- merged$network_centrality_score; c2 <- merged$consensus_score
    has_d <- is.finite(d); has_m <- is.finite(m); has_n <- is.finite(n); has_c2 <- is.finite(c2)
    denom <- (has_d * w_diff) + (has_m * w_ml) + (has_n * w_net) + (has_c2 * w_cons)
    num <- (ifelse(has_d, d, 0) * w_diff) + (ifelse(has_m, m, 0) * w_ml) + (ifelse(has_n, n, 0) * w_net) + (ifelse(has_c2, c2, 0) * w_cons)
    merged$key_taxa_score <- ifelse(denom > 0, num / denom, NA_real_)
  }
  merged$recommendation_level <- vapply(merged$key_taxa_score, .recommend_level, character(1))
  merged$display_taxon <- ifelse(is.na(merged$display_taxon) | !nzchar(merged$display_taxon),
    taxon_display_from_any(as.character(merged$original_taxon %||% merged$taxon)), as.character(merged$display_taxon))
  merged$full_taxon <- as.character(merged$original_taxon %||% merged$taxon)
  merged$normalized_taxon <- as.character(merged$taxon)
  merged$tax_level_display <- ifelse(is.na(merged$tax_level) | merged$tax_level == "", "Feature-level", as.character(merged$tax_level))
  ranked <- rank_key_taxa(merged, top_n = 20)
  score_table <- ranked$score_table; top20 <- ranked$top_table
  evidence_matrix <- dplyr::tibble(taxon = score_table$display_taxon,
    differential_score = score_table$differential_score, ml_importance_score = score_table$ml_importance_score,
    network_centrality_score = score_table$network_centrality_score, consensus_score = score_table$consensus_score)
  evidence_cards <- build_evidence_cards(score_table)
  required_cols <- c("taxon","display_taxon","normalized_taxon","original_taxon","tax_level","tax_level_display",
    "differential_score","ml_importance_score","network_centrality_score","consensus_score",
    "diff_fdr","log2fc","rf_importance","degree","betweenness",
    "consensus_evidence_count","consensus_evidence_methods",
    "evidence_count","evidence_methods","evidence_sources","reliability_label","rank","recommendation_level","full_taxon","key_taxa_score")
  for (nm in required_cols) if (!nm %in% names(score_table)) score_table[[nm]] <- NA
  # evidence_sources is an alias for evidence_methods for external consumers
  if ("evidence_methods" %in% names(score_table) && !"evidence_sources" %in% names(score_table)) {
    score_table$evidence_sources <- score_table$evidence_methods
  }
  score_table <- score_table[, intersect(required_cols, names(score_table)), drop = FALSE]
  tables_dir <- file.path(job_dir, "tables"); figs_dir <- file.path(job_dir, "figures"); json_dir <- file.path(job_dir, "json")
  ensure_dir(tables_dir); ensure_dir(figs_dir); ensure_dir(json_dir)
  out_score_csv <- file.path(tables_dir, "key_taxa_score.csv")
  out_top_csv <- file.path(tables_dir, "key_taxa_top20.csv")
  out_matrix_csv <- file.path(tables_dir, "key_taxa_evidence_matrix.csv")
  out_cards_csv <- file.path(tables_dir, "key_taxa_evidence_cards.csv")
  out_json <- file.path(json_dir, "key_taxa_summary.json")
  out_png <- file.path(figs_dir, "key_taxa_score_barplot.png"); out_pdf <- file.path(figs_dir, "key_taxa_score_barplot.pdf")
  out_upset_png <- file.path(figs_dir, "key_taxa_upset.png"); out_upset_pdf <- file.path(figs_dir, "key_taxa_upset.pdf")
  out_heat_png <- file.path(figs_dir, "key_taxa_evidence_heatmap.png"); out_heat_pdf <- file.path(figs_dir, "key_taxa_evidence_heatmap.pdf")
  readr::write_csv(score_table, out_score_csv, na = "")
  readr::write_csv(top20, out_top_csv, na = "")
  readr::write_csv(evidence_matrix, out_matrix_csv, na = "")
  readr::write_csv(evidence_cards, out_cards_csv, na = "")
  plot_key_taxa_score(score_table, out_png, out_pdf, top_n = 20)
  plot_key_taxa_upset(score_table, out_upset_png, out_upset_pdf)
  plot_key_taxa_evidence_heatmap(score_table, evidence_matrix, out_heat_png, out_heat_pdf)
  summary <- summarize_key_taxa_for_ai(score_table, used_sources = used_sources, weights = as.list(weights))
  write_json_pretty(summary, out_json, auto_unbox = TRUE)
  append_reproducibility(job_dir, list(phase7 = list(
    started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), used_sources = used_sources, weights = as.list(weights),
    reliability = summary$reliability, n_candidate_taxa = summary$n_candidate_taxa,
    scoring_formula = summary$scoring_formula, finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
  list(score_result = list(used_sources = used_sources, weights = as.list(weights)),
    summary = summary,
    outputs = list(
      key_taxa_score_csv = normalizePath(out_score_csv, winslash = "/", mustWork = TRUE),
      key_taxa_top20_csv = normalizePath(out_top_csv, winslash = "/", mustWork = TRUE),
      key_taxa_evidence_matrix_csv = normalizePath(out_matrix_csv, winslash = "/", mustWork = TRUE),
      key_taxa_evidence_cards_csv = normalizePath(out_cards_csv, winslash = "/", mustWork = TRUE),
      key_taxa_summary_json = normalizePath(out_json, winslash = "/", mustWork = TRUE),
      barplot_png = normalizePath(out_png, winslash = "/", mustWork = TRUE), barplot_pdf = normalizePath(out_pdf, winslash = "/", mustWork = TRUE),
      upset_png = normalizePath(out_upset_png, winslash = "/", mustWork = TRUE), upset_pdf = normalizePath(out_upset_pdf, winslash = "/", mustWork = TRUE),
      heatmap_png = normalizePath(out_heat_png, winslash = "/", mustWork = TRUE), heatmap_pdf = normalizePath(out_heat_pdf, winslash = "/", mustWork = TRUE)))
}
