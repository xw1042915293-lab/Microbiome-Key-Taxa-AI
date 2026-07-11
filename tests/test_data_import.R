# Minimal smoke tests for Phase 1 modules (no testthat dependency yet).

source("global.R", local = TRUE)

ab_path <- "data/abundance.tsv"
md_path <- "data/metadata.tsv"
tx_path <- "data/taxonomy.tsv"

inputs <- read_microbiome_inputs(ab_path, md_path, tx_path)
stopifnot(is.list(inputs), all(c("abundance", "metadata", "taxonomy") %in% names(inputs)))
stopifnot("FeatureID" %in% names(inputs$abundance))
stopifnot("SampleID" %in% names(inputs$metadata))
