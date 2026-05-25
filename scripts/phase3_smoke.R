setwd("D:/Microbiome Key Taxa AI")

source("global.R")

job_dir <- create_job_dir()

# Phase 1+2: import, check, build, alpha, beta
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

# Phase 3: diff + report
res3 <- run_phase3_workflow(
  dataset = res$dataset,
  job_dir = job_dir,
  group_var = "Group",
  tax_level = "Genus"
)

stopifnot(file.exists(file.path(job_dir, "tables", "differential_taxa.csv")))
stopifnot(file.exists(file.path(job_dir, "tables", "differential_taxa_significant.csv")))
stopifnot(file.exists(file.path(job_dir, "json", "diff_summary.json")))
stopifnot(file.exists(file.path(job_dir, "figures", "diff_taxa_barplot.png")))
stopifnot(file.exists(file.path(job_dir, "figures", "diff_taxa_barplot.pdf")))
stopifnot(file.exists(file.path(job_dir, "figures", "diff_volcano.png")))
stopifnot(file.exists(file.path(job_dir, "figures", "diff_volcano.pdf")))
stopifnot(file.exists(file.path(job_dir, "report", "report.html")))

cat("phase3 ok\n")
