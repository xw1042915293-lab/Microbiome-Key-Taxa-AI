# Minimal smoke test for Phase 7 scoring with partial evidence sources.

source("global.R", local = TRUE)

job_dir <- create_job_dir()

ml_table <- data.frame(
  taxon = c("TaxonA", "TaxonB"),
  tax_level = c("Genus", "Genus"),
  importance = c(0.9, 0.3),
  stringsAsFactors = FALSE
)

network_nodes <- data.frame(
  taxon = c("TaxonA", "TaxonC"),
  tax_level = c("Genus", "Genus"),
  degree = c(5, 2),
  betweenness = c(0.8, 0.1),
  stringsAsFactors = FALSE
)

res <- calculate_key_taxa_score(
  diff_table = NULL,
  ml_table = ml_table,
  network_nodes = network_nodes,
  job_dir = job_dir
)

stopifnot(identical(res$score_result$used_sources, c("ml", "network")))
w <- unname(unlist(res$score_result$weights[c("ml", "network")]))
stopifnot(isTRUE(all.equal(sum(w), 1.0)))
stopifnot(w[1] > w[2])  # ml weight > network weight by default formula
stopifnot(file.exists(file.path(job_dir, "tables", "key_taxa_score.csv")))
stopifnot(file.exists(file.path(job_dir, "json", "key_taxa_summary.json")))
