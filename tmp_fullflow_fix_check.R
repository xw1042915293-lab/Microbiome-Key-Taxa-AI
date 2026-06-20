source('global.R', local = TRUE)
job_dir <- create_job_dir('fullflow_fix_check')
ab_path <- copy_to_job_input('data/example_abundance.tsv', file.path(job_dir, 'input', 'abundance.tsv'))
md_path <- copy_to_job_input('data/example_metadata.tsv', file.path(job_dir, 'input', 'metadata.tsv'))
tx_path <- copy_to_job_input('data/example_taxonomy.tsv', file.path(job_dir, 'input', 'taxonomy.tsv'))
input_data <- read_microbiome_inputs(ab_path, md_path, tx_path)
res <- run_full_analysis_workflow(
  input_data = input_data,
  job_dir = job_dir,
  group_var = 'Group',
  beta_distance = 'bray',
  tax_level = 'Genus',
  progress_cb = function(step_id, status, detail = NULL) cat(step_id, status, if (is.null(detail)) '' else detail, '\n'),
  log_path = file.path(job_dir, 'logs', 'analysis_log.txt')
)
cat('JOB=', normalizePath(job_dir, winslash='/', mustWork=TRUE), '\n', sep='')
cat('REPORT=', file.exists(file.path(job_dir, 'report', 'report.html')), '\n', sep='')