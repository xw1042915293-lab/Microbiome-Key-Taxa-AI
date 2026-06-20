source("global.R", local = TRUE)

diff_df <- data.frame(
  taxon = c(
    "ASV1|Bacteria|Proteobacteria|Alphaproteobacteria|Rhizobiales|Xanthobacteraceae|Unclassified",
    "ASV2|Bacteria|Proteobacteria|Gammaproteobacteria|Burkholderiales|Comamonadaceae|Unclassified",
    "ASV3|Bacteria|Proteobacteria|Gammaproteobacteria|Burkholderiales|Comamonadaceae|Unclassified",
    "ASV4|Bacteria|Bacteroidota|Bacteroidia|Flavobacteriales|Flavobacteriaceae|Flavobacterium",
    "ASV5|Bacteria|Actinobacteriota|Actinobacteria|Propionibacteriales|Nocardioidaceae|Nocardioides",
    "ASV6|Bacteria|Proteobacteria|Alphaproteobacteria|Rhizobiales|Devosiaceae|Devosia"
  ),
  tax_level = rep("Genus", 6),
  p_value = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006),
  log2fc = rep(NA_real_, 6),
  mean_abundance = c(0.20, 0.18, 0.17, 0.12, 0.08, 0.05),
  prevalence = c(0.80, 0.75, 0.74, 0.60, 0.40, 0.35),
  fdr = c(0.08, 0.082, 0.083, 0.085, 0.087, 0.089),
  significant = rep(FALSE, 6),
  taxon_label = c("Xanthobacteraceae", "Comamonadaceae", "Comamonadaceae", "Flavobacterium", "Nocardioides", "Devosia"),
  display_taxon = c("Xanthobacteraceae", "Comamonadaceae", "Comamonadaceae", "Flavobacterium", "Nocardioides", "Devosia"),
  direction = rep("Multi-group comparison", 6),
  significance = rep("exploratory", 6),
  stringsAsFactors = FALSE
)

ai_summary <- summarize_diff_for_ai(diff_df, "Group")
stopifnot(ai_summary$n_significant == 0)
stopifnot(nrow(ai_summary$exploratory_top_taxa) <= 5)
stopifnot(length(unique(ai_summary$exploratory_top_taxa$taxon_label)) == nrow(ai_summary$exploratory_top_taxa))
stopifnot(!any(duplicated(ai_summary$exploratory_top_taxa$taxon_label)))
stopifnot(identical(
  ai_summary$no_significant_message,
  paste(
    "No FDR-significant taxa were detected.",
    "The following taxa are exploratory trends only and should not be interpreted as statistically significant."
  )
))

job_dir <- create_job_dir()
readr::write_csv(diff_df, file.path(job_dir, "tables", "differential_taxa.csv"))
readr::write_csv(diff_df[0, , drop = FALSE], file.path(job_dir, "tables", "differential_taxa_significant.csv"))
write_json_pretty(
  list(
    analysis_type = "differential_abundance",
    group_variable = "Group",
    tax_level = "Genus",
    method = "kruskal-wallis",
    p_adjust_method = "fdr",
    significance_cutoff = list(fdr = 0.05),
    n_total_taxa = nrow(diff_df),
    n_significant_taxa = 0,
    top_taxa = list(),
    message = "No significant taxa were detected under FDR < 0.05."
  ),
  file.path(job_dir, "json", "diff_summary.json"),
  auto_unbox = TRUE
)

md_en <- build_diff_interpretation(job_dir)

stopifnot(grepl("No FDR-significant taxa were detected", md_en))
stopifnot(grepl("Exploratory trend", md_en, ignore.case = TRUE))
stopifnot(!grepl("Mean abundance was", md_en, fixed = TRUE))
stopifnot(!grepl("prevalence was", md_en, fixed = TRUE))
stopifnot(grepl("Caution", md_en, fixed = TRUE))
