# Smoke tests for differential abundance display helpers and exploratory outputs.

source("global.R", local = TRUE)

labels <- make_taxon_display_label(c(
  "ASV1|Bacteria|Firmicutes|Bacilli|Lactobacillales|Lactobacillaceae|Lactobacillus|acidophilus",
  "ASV2|Bacteria|Firmicutes|Bacilli|Lactobacillales|Lactobacillaceae|Unclassified|Unclassified",
  "ASV3|Bacteria|Firmicutes|Bacilli|Lactobacillales|Unclassified|Unclassified|Unclassified",
  "StandaloneFeature",
  "ASV4|Bacteria|Firmicutes|Bacilli|OrderWithVeryLongNameForDisplayTesting|FamilyWithVeryLongNameForDisplayTesting|GenusWithVeryLongNameForDisplayTesting|species"
))

stopifnot(identical(labels[[1]], "Lactobacillus"))
stopifnot(identical(labels[[2]], "Lactobacillaceae"))
stopifnot(identical(labels[[3]], "Lactobacillales"))
stopifnot(identical(labels[[4]], "Standalone..."))
stopifnot(nchar(labels[[5]], type = "chars") == 35)
stopifnot(grepl("\\.\\.\\.$", labels[[5]]))

job_dir <- create_job_dir()

taxa <- paste0(
  "ASV", seq_len(25),
  "|Bacteria|Firmicutes|Bacilli|Lactobacillales|Lactobacillaceae|Taxon",
  seq_len(25),
  "|Unclassified"
)

diff_df <- data.frame(
  taxon = taxa,
  tax_level = rep("Genus", 25),
  p_value = seq(0.001, 0.05, length.out = 25),
  log2fc = seq(-2, 2, length.out = 25),
  mean_abundance = seq(0.1, 0.5, length.out = 25),
  prevalence = seq(0.2, 0.8, length.out = 25),
  fdr = seq(0.2, 0.8, length.out = 25),
  significant = rep(FALSE, 25),
  taxon_label = make_taxon_display_label(taxa),
  display_taxon = make_taxon_display_label(taxa),
  direction = ifelse(seq(-2, 2, length.out = 25) >= 0, "Higher in Case", "Higher in Control"),
  significance = rep("exploratory", 25),
  stringsAsFactors = FALSE
)

plot_paths <- plot_diff_taxa_barplot(
  diff_table = diff_df,
  group_var = "Group",
  job_dir = job_dir,
  exploratory = TRUE,
  top_n = 20
)

stopifnot(file.exists(file.path(job_dir, "figures", "diff_taxa_barplot.png")))
stopifnot(file.exists(file.path(job_dir, "figures", "diff_taxa_barplot.pdf")))
stopifnot(file.exists(plot_paths$png))
stopifnot(file.exists(plot_paths$pdf))

summary_path <- save_diff_summary_json(
  diff_table = diff_df,
  group_var = "Group",
  tax_level = "Genus",
  job_dir = job_dir
)

summary_obj <- jsonlite::fromJSON(summary_path, simplifyVector = FALSE)
stopifnot(identical(as.integer(summary_obj$n_significant_taxa), 0L))
stopifnot(identical(summary_obj$message, "No significant taxa were detected under FDR < 0.05."))
stopifnot(length(summary_obj$top_taxa) == 20)
stopifnot(all(vapply(summary_obj$top_taxa, function(x) "taxon_label" %in% names(x), logical(1))))
