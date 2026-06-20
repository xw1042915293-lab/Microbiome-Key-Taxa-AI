import os

out = r"D:\Microbiome Key Taxa AI\R\key_taxa_score.R"

# Read the backup for shared helpers (lines 1-111 of original)
backup = open(out + ".bak", encoding="utf-8").read()
backup_lines = backup.split("\n")

# Get helpers: lines 1-8 (header) + lines 9-111 (helpers through .collapse_taxon_rows)
# In 0-indexed: 0-110
helpers = "\n".join(backup_lines[8:111])  # lines 9-111

# Now write the complete new file
content = """# Phase 7: Key Taxa Score (business logic)
# Integrates 4 evidence sources:
#   - differential abundance, ML importance, network centrality, consensus
# Default: KeyTaxaScore = 0.35*Diff + 0.30*ML + 0.25*Network + 0.10*Consensus
# Weights auto-redistribute when a module is missing.

""" + helpers + "\n\n"

# Add evidence calculators
content += """
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

"""

# Now append the rest of the new code (from the current file starting at line 659 "# --- Ranking ---")
current = open(out, encoding="utf-8").read()
current_lines = current.split("\n")

# Find "# --- Ranking ---"
rank_start = -1
for i, line in enumerate(current_lines):
    if line.strip() == "# --- Ranking ---":
        rank_start = i
        break

if rank_start >= 0:
    rest = "\n".join(current_lines[rank_start:])
    content += rest
else:
    # Fallback: just add the required functions manually
    print("ERROR: Could not find ranking section")

with open(out, 'w', encoding='utf-8') as f:
    f.write(content)
print(f"Written {os.path.getsize(out)} bytes, {content.count(chr(10))+1} lines")
