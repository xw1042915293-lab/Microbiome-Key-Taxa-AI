# Minimal smoke tests for data checks.

source("global.R", local = TRUE)

inputs <- read_microbiome_inputs(
  "data/abundance.tsv",
  "data/metadata.tsv",
  "data/taxonomy.tsv"
)

res <- run_all_data_checks(inputs, group_var = "Group")
stopifnot(is.list(res), res$status %in% c("pass", "warning", "error"))
stopifnot(is.data.frame(res$checks))
stopifnot(all(c("row", "FeatureID", "column", "value") %in% names(res$checks)))

bad_inputs <- list(
  abundance = tibble::tibble(
    FeatureID = c("F1", "F2"),
    S1 = c("1", ""),
    Taxon = c("k__Bacteria", "k__Archaea")
  ),
  metadata = tibble::tibble(
    SampleID = "S1",
    Group = "A"
  ),
  taxonomy = tibble::tibble(
    FeatureID = c("F1", "F2"),
    Kingdom = c("Bacteria", "Archaea")
  )
)

bad_res <- run_all_data_checks(bad_inputs, group_var = "Group")
stopifnot(identical(bad_res$status, "error"))
stopifnot(any(bad_res$checks$check_name == "abundance_non_numeric_columns", na.rm = TRUE))
stopifnot(any(bad_res$checks$check_name == "abundance_taxonomy_columns_in_abundance", na.rm = TRUE))
stopifnot(any(bad_res$checks$check_name == "abundance_missing_like_values", na.rm = TRUE))
stopifnot(sum(bad_res$checks$check_name == "abundance_invalid_cell", na.rm = TRUE) >= 2)

build_err <- tryCatch(
  {
    build_microeco_dataset(bad_inputs$abundance, bad_inputs$metadata, bad_inputs$taxonomy)
    NULL
  },
  error = function(e) conditionMessage(e)
)
stopifnot(is.character(build_err), length(build_err) == 1)
stopifnot(grepl("丰度表校验失败", build_err, fixed = TRUE))
stopifnot(grepl("Taxon", build_err, fixed = TRUE))
