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
stopifnot(all(vapply(summary_obj$top_taxa, function(x) "taxon_label" %in% names(x[[1]]), logical(1))))

# Regression: multiple ASVs assigned to the same genus must be aggregated
# before differential testing; ASV identifiers must not leak into genus rows.
mock_otu <- data.frame(
  S1 = c(10, 20, 5), S2 = c(12, 18, 4), S3 = c(11, 22, 6), S4 = c(9, 19, 5),
  S5 = c(2, 3, 30), S6 = c(1, 4, 28), S7 = c(2, 2, 32), S8 = c(3, 3, 29),
  row.names = c("asv_a", "asv_b", "asv_c"), check.names = FALSE
)
mock_tax <- data.frame(
  Kingdom = rep("Bacteria", 3), Phylum = rep("Firmicutes", 3),
  Class = rep("Bacilli", 3), Order = rep("Lactobacillales", 3),
  Family = c("FamilyA", "FamilyA", "FamilyB"), Genus = c("GenusA", "GenusA", "GenusB"),
  row.names = rownames(mock_otu), stringsAsFactors = FALSE
)
mock_samples <- data.frame(SampleID = paste0("S", 1:8), Group = rep(c("Control", "Case"), each = 4),
                           row.names = paste0("S", 1:8), stringsAsFactors = FALSE)
mock_dataset <- list(otu_table = mock_otu, tax_table = mock_tax, sample_table = mock_samples)
aggregated <- prepare_diff_abundance(mock_dataset, "Genus")
stopifnot(nrow(aggregated$abundance) == 2)
stopifnot(all(c("GenusA", "GenusB") %in% unname(aggregated$labels)))
stopifnot(!any(grepl("asv_", rownames(aggregated$abundance), fixed = TRUE)))

# Publication lollipop regression scenarios: significant results, no results,
# FDR = 0, long labels, multiple enriched groups, missing effects, and exports.
lollipop_df <- data.frame(
  taxon = paste0("tax_", 1:22),
  taxon_label = c(
    "Unclassified_Unclassified", "Unclassified_Rhodobacteraceae",
    "SAR324_clade(Marine_group_B)",
    "Dactylosporangium_with_a_very_long_species_style_label_for_wrapping",
    paste0("Genus", 5:22)
  ),
  display_taxon = c(
    "Unclassified_Unclassified", "Unclassified_Rhodobacteraceae",
    "SAR324_clade(Marine_group_B)",
    "Dactylosporangium_with_a_very_long_species_style_label_for_wrapping",
    paste0("Genus", 5:22)
  ),
  tested = TRUE,
  p_value = seq(1e-06, 0.02, length.out = 22),
  fdr = c(0, seq(0.001, 0.021, length.out = 21)),
  effect_size = c(seq(0.80, 0.20, length.out = 21), NA_real_),
  effect_size_metric = "epsilon-squared",
  significant = TRUE,
  direction = "See group summaries",
  enriched_group = rep(c("North", "South", "West"), length.out = 22),
  enriched_group_method = "highest group median (mean used for ties)",
  stringsAsFactors = FALSE
)

missing_warning <- NULL
lollipop_data <- withCallingHandlers(
  prepare_diff_lollipop_data(lollipop_df, top_n = 15, sort_by = "effect_size"),
  warning = function(w) {
    missing_warning <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
stopifnot(nrow(lollipop_data) == 15)
stopifnot(grepl("effect_size is missing", missing_warning, fixed = TRUE))
stopifnot(!"Unclassified Unclassified" %in% lollipop_data$display_taxon)
stopifnot("Unclassified Rhodobacteraceae" %in% lollipop_data$display_taxon)
stopifnot(all(diff(lollipop_data$effect_size) <= 0))
stopifnot(any(is.finite(lollipop_data$neg_log10_fdr)))

with_unclassified <- suppressWarnings(prepare_diff_lollipop_data(
  lollipop_df, top_n = 20, show_unclassified = TRUE, sort_by = "fdr"
))
stopifnot("Unclassified Unclassified" %in% with_unclassified$display_taxon)
stopifnot(with_unclassified$neg_log10_fdr[with_unclassified$fdr == 0][1] == 300)
stopifnot(any(grepl("\\n", as.character(lollipop_data$axis_taxon))))

empty_data <- prepare_diff_lollipop_data(transform(lollipop_df, fdr = 0.5), top_n = 15)
stopifnot(nrow(empty_data) == 0)
empty_plot <- build_diff_lollipop_plot(empty_data)
stopifnot(inherits(empty_plot, "ggplot"))

lollipop_plot <- build_diff_lollipop_plot(lollipop_data, show_fdr_labels = TRUE)
stopifnot(inherits(lollipop_plot, "ggplot"))
publication_png <- file.path(job_dir, "figures", "lollipop_regression.png")
publication_pdf <- file.path(job_dir, "figures", "lollipop_regression.pdf")
ggplot2::ggsave(publication_png, lollipop_plot, device = "png", width = 9, height = 7, dpi = 300, bg = "white")
ggplot2::ggsave(publication_pdf, lollipop_plot, device = "pdf", width = 9, height = 7, bg = "white")
stopifnot(file.exists(publication_png), file.info(publication_png)$size > 0)
stopifnot(file.exists(publication_pdf), file.info(publication_pdf)$size > 0)

enrichment_input <- data.frame(taxon = c("tax_a", "tax_b", "tax_c"), stringsAsFactors = FALSE)
enrichment_summary <- data.frame(
  taxon = rep(c("tax_a", "tax_b", "tax_c"), each = 3), group = rep(c("North", "South", "West"), 3),
  median_abundance = c(0.1, 0.4, 0.2, 0.5, 0.2, 0.1, 0.3, 0.3, 0.1),
  mean_abundance = c(0.1, 0.45, 0.2, 0.5, 0.2, 0.1, 0.4, 0.35, 0.1), stringsAsFactors = FALSE
)
enrichment_pairwise <- data.frame(
  taxon = "tax_a", group_1 = "North", group_2 = "South",
  cliffs_delta = 0.8, p_adjust_holm = 0.01, stringsAsFactors = FALSE
)
enrichment <- derive_enriched_groups(enrichment_input, enrichment_summary, enrichment_pairwise)
stopifnot(identical(enrichment$enriched_group, c("South", "North", "Undetermined")))
stopifnot(identical(enrichment$enriched_group_method[[1]], "post-hoc determined"))
stopifnot(identical(enrichment$enriched_group_method[[2]], "highest group median"))
stopifnot(identical(enrichment$enriched_group_method[[3]], "tied group medians"))

# Directional effect plot: HEB is positive, NMG is negative, point size is
# fixed, q labels/stars are optional, and arbitrary two-group names are dynamic.
forest_df <- data.frame(
  taxon = paste0("forest_", 1:5),
  taxon_label = c("Herminiimonas", "Pelomonas", "Methylotenera", "Unclassified_Rhodobacteraceae", "Tie_taxon"),
  display_taxon = c("Herminiimonas", "Pelomonas", "Methylotenera", "Unclassified_Rhodobacteraceae", "Tie_taxon"),
  tested = TRUE, p_value = c(1e-05, 0.001, 0.01, 0.02, 0.03),
  fdr = c(0.0005, 0.005, 0.02, 0.03, 0.04), significant = TRUE,
  effect_size = c(0.8, 0.7, 0.6, 0.5, 0.4), effect_size_metric = "epsilon-squared",
  enriched_group = c("HEB", "NMG", "HEB", "NMG", "Undetermined"),
  enriched_group_method = c(rep("post-hoc determined", 4), "tied group medians"),
  direction = "See group summaries", stringsAsFactors = FALSE
)
forest_columns <- add_directional_effect_columns(forest_df, c("HEB", "NMG"))
stopifnot(all(c("epsilon_squared", "signed_epsilon2", "enriched_group", "FDR", "significance", "significance_stars") %in% names(forest_columns)))
stopifnot(forest_columns$signed_epsilon2[forest_columns$enriched_group == "HEB"] > 0)
stopifnot(forest_columns$signed_epsilon2[forest_columns$enriched_group == "NMG"] < 0)
stopifnot(is.na(forest_columns$signed_epsilon2[forest_columns$enriched_group == "Undetermined"]))
stopifnot(identical(forest_columns$significance_stars[1:3], c("***", "**", "*")))

forest_data <- suppressWarnings(prepare_diff_directional_data(forest_columns, c("HEB", "NMG"), top_n = 10))
stopifnot(nrow(forest_data) == 4)
stopifnot(all(diff(abs(forest_data$signed_effect_size)) <= 0))
stopifnot(identical(attr(forest_data, "group_negative"), "NMG"))
stopifnot(identical(attr(forest_data, "group_positive"), "HEB"))
stopifnot(identical(forest_data$q_label[[1]], "q < 0.001"))

forest_plot <- build_diff_directional_plot(forest_data, show_fdr = TRUE, show_significance = TRUE)
stopifnot(inherits(forest_plot, "ggplot"))
stopifnot(identical(forest_plot$layers[[2]]$aes_params$size, 4.2))
stopifnot(is.null(forest_plot$layers[[2]]$mapping$size))
forest_png <- file.path(job_dir, "figures", "directional_regression.png")
forest_pdf <- file.path(job_dir, "figures", "directional_regression.pdf")
ggplot2::ggsave(forest_png, forest_plot, device = "png", width = 9, height = 6, dpi = 300, bg = "white")
ggplot2::ggsave(forest_pdf, forest_plot, device = "pdf", width = 9, height = 6, bg = "white")
stopifnot(file.exists(forest_png), file.info(forest_png)$size > 0)
stopifnot(file.exists(forest_pdf), file.info(forest_pdf)$size > 0)

dynamic_df <- forest_df[1:2, , drop = FALSE]
dynamic_df$enriched_group <- c("East", "West")
dynamic_data <- prepare_diff_directional_data(dynamic_df, c("East", "West"), top_n = 10)
stopifnot(dynamic_data$signed_effect_size[dynamic_data$enriched_group == "East"] < 0)
stopifnot(dynamic_data$signed_effect_size[dynamic_data$enriched_group == "West"] > 0)

multi_direction <- suppressWarnings(prepare_diff_directional_data(forest_df, c("North", "South", "West"), top_n = 10))
stopifnot(nrow(multi_direction) == 0)
stopifnot(grepl("exactly two", attr(multi_direction, "empty_message"), fixed = TRUE))
stopifnot(inherits(build_diff_directional_plot(multi_direction), "ggplot"))

# End-to-end multi-group Kruskal-Wallis run: appended CSV fields and legacy
# report figure paths must remain available.
multi_samples <- paste0("M", 1:18)
multi_groups <- rep(c("HEB", "NMG", "XJ"), each = 6)
multi_otu <- rbind(
  TaxonA = c(rep(100, 6), rep(2, 12)),
  TaxonB = c(rep(2, 6), rep(100, 6), rep(2, 6)),
  TaxonC = c(rep(2, 12), rep(100, 6)),
  TaxonD = rep(c(15, 16, 14, 15, 16, 14), 3)
)
colnames(multi_otu) <- multi_samples
multi_tax <- data.frame(
  Kingdom = "Bacteria", Phylum = "Firmicutes", Class = "Bacilli",
  Order = "Lactobacillales", Family = c("FamilyA", "FamilyB", "FamilyC", "FamilyD"),
  Genus = c("Genus_A", "Genus_B", "Genus_C", "Unclassified_Unclassified"),
  row.names = rownames(multi_otu), stringsAsFactors = FALSE
)
multi_meta <- data.frame(Group = multi_groups, row.names = multi_samples, stringsAsFactors = FALSE)
multi_job <- create_job_dir()
multi_result <- run_diff_analysis(
  list(otu_table = as.data.frame(multi_otu), tax_table = multi_tax, sample_table = multi_meta),
  group_var = "Group", tax_level = "Genus", job_dir = multi_job, min_prevalence = 0, top_n = 15
)
multi_csv <- readr::read_csv(multi_result$diff_table_path, show_col_types = FALSE)
stopifnot(any(multi_csv$fdr < 0.05, na.rm = TRUE))
stopifnot(all(c("display_taxon", "epsilon_squared", "signed_epsilon2", "enriched_group", "FDR", "significance", "enriched_group_method") %in% names(multi_csv)))
stopifnot(all(c("HEB", "NMG", "XJ") %in% multi_csv$enriched_group[multi_csv$fdr < 0.05]))
stopifnot(all(is.na(multi_csv$signed_epsilon2)))
multi_balanced <- prepare_diff_balanced_data(multi_csv, c("HEB", "NMG", "XJ"), top_n = 10)
stopifnot(all(c("HEB", "NMG", "XJ") %in% as.character(multi_balanced$enriched_group)))
stopifnot(inherits(build_diff_balanced_plot(multi_balanced, show_significance = TRUE), "ggplot"))
multi_group_summary <- readr::read_csv(multi_result$group_summary_path, show_col_types = FALSE)
multi_heatmap <- prepare_diff_heatmap_data(multi_csv, multi_group_summary, c("HEB", "NMG", "XJ"), top_n = 10)
stopifnot(all(c("HEB", "NMG", "XJ") %in% as.character(multi_heatmap$group)))
stopifnot(inherits(build_diff_heatmap_plot(multi_heatmap), "ggplot"))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_barplot.png")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_barplot.pdf")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_directional.png")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_directional.pdf")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_balanced.png")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_balanced.pdf")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_heatmap.png")))
stopifnot(file.exists(file.path(multi_job, "figures", "diff_taxa_heatmap.pdf")))
