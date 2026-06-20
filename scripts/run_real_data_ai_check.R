setwd("D:/Microbiome Key Taxa AI")

source("global.R")

job_dir <- create_job_dir()
cat("JOB_DIR=", normalizePath(job_dir, winslash = "/", mustWork = TRUE), "\n", sep = "")

ab_path <- copy_to_job_input("data/abundance.tsv", file.path(job_dir, "input", "abundance.tsv"))
md_path <- copy_to_job_input("data/metadata.tsv", file.path(job_dir, "input", "metadata.tsv"))
tx_path <- copy_to_job_input("data/taxonomy.tsv", file.path(job_dir, "input", "taxonomy.tsv"))

cat("Step: read inputs\n")
input_data <- read_microbiome_inputs(ab_path, md_path, tx_path)

cat("Step: data check\n")
check_res <- run_all_data_checks(input_data, group_var = "Group")
cat("Data check status: ", check_res$status, "\n", sep = "")
if (!is.null(check_res$checks)) print(check_res$checks)
if (identical(check_res$status, "error")) stop("Data check failed", call. = FALSE)

cat("Step: phase2 basic analysis\n")
res2 <- run_basic_analysis(
  input_data = input_data,
  job_dir = job_dir,
  group_var = "Group",
  beta_distance = "bray"
)
cat("Phase2 OK\n")

cat("Step: phase3 diff\n")
res3 <- run_phase3_workflow(
  dataset = res2$dataset,
  job_dir = job_dir,
  group_var = "Group",
  tax_level = "Genus"
)
cat("Phase3 OK\n")

cat("Step: phase4a local AI\n")
res4a <- run_phase4a_workflow(job_dir)
print(res4a)
cat("Phase4A OK\n")

cat("Step: phase4b LLM or fallback\n")
res4b <- run_phase4b_workflow(job_dir, config_path = "config.yml")
print(res4b)
cat("Phase4B OK\n")

cat("AI outputs:\n")
print(list.files(file.path(job_dir, "ai"), full.names = TRUE))

cat("JSON outputs:\n")
print(list.files(file.path(job_dir, "json"), full.names = TRUE))
