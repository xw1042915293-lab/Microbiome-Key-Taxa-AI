# Minimal smoke tests for data checks.

source("global.R", local = TRUE)

inputs <- read_microbiome_inputs(
  "data/example_abundance.tsv",
  "data/example_metadata.tsv",
  "data/example_taxonomy.tsv"
)

res <- run_all_data_checks(inputs, group_var = "Group")
stopifnot(is.list(res), res$status %in% c("pass", "warning", "error"))
stopifnot(is.data.frame(res$checks))

