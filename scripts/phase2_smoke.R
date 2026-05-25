setwd("D:/Microbiome Key Taxa AI")

source("global.R")

job_dir <- create_job_dir()

# Mimic upload persistence
ab_path <- copy_to_job_input("data/example_abundance.tsv", file.path(job_dir, "input", "abundance.tsv"))
md_path <- copy_to_job_input("data/example_metadata.tsv", file.path(job_dir, "input", "metadata.tsv"))
tx_path <- copy_to_job_input("data/example_taxonomy.tsv", file.path(job_dir, "input", "taxonomy.tsv"))

input_data <- read_microbiome_inputs(ab_path, md_path, tx_path)
check_res <- run_all_data_checks(input_data, group_var = "Group")
stopifnot(check_res$status != "error")

res <- run_basic_analysis(
  input_data = input_data,
  job_dir = job_dir,
  group_var = "Group",
  beta_distance = "bray"
)

stopifnot(file.exists(file.path(job_dir, "objects", "microeco_dataset.rds")))
stopifnot(file.exists(file.path(job_dir, "tables", "alpha_diversity.csv")))
stopifnot(file.exists(file.path(job_dir, "figures", "alpha_shannon_boxplot.png")))
stopifnot(file.exists(file.path(job_dir, "tables", "beta_pcoa_coordinates.csv")))
stopifnot(file.exists(file.path(job_dir, "tables", "beta_permanova.csv")))
stopifnot(file.exists(file.path(job_dir, "figures", "beta_pcoa_bray.png")))

cat("phase2 ok\n")
