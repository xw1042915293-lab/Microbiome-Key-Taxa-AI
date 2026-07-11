# Data import (Phase 1).
# Reads abundance/metadata/taxonomy tables and standardizes column names.

detect_delimiter <- function(file_path) {
  assert_non_empty_string(file_path, "file_path")
  if (!file.exists(file_path)) stop("detect_delimiter(): file not found: ", file_path, call. = FALSE)

  first_line <- readLines(file_path, n = 1, warn = FALSE)
  if (length(first_line) == 0) stop("detect_delimiter(): empty file: ", file_path, call. = FALSE)

  # Heuristic: prefer tab if it exists and is more frequent.
  n_tab <- stringr::str_count(first_line, "\t")
  n_comma <- stringr::str_count(first_line, ",")
  n_semicolon <- stringr::str_count(first_line, ";")
  n_space <- stringr::str_count(first_line, " ")

  if (n_tab >= max(n_comma, n_semicolon, n_space)) return("\t")
  if (n_comma >= max(n_semicolon, n_space)) return(",")
  if (n_semicolon >= n_space) return(";")
  " "
}

read_table_auto <- function(file_path) {
  assert_non_empty_string(file_path, "file_path")
  if (!file.exists(file_path)) stop("read_table_auto(): file not found: ", file_path, call. = FALSE)

  sep <- detect_delimiter(file_path)
  dt <- data.table::fread(
    file_path,
    sep = sep,
    header = TRUE,
    data.table = FALSE,
    check.names = FALSE
  )
  tibble::as_tibble(dt)
}

sanitize_import_table <- function(df, file_type) {
  assert_non_empty_string(file_type, "file_type")
  cleaned <- sanitize_utf8_data_frame(df, file_type = file_type)
  if (nrow(cleaned$diagnostics) > 0) {
    stop_invalid_utf8_input(
      cleaned$diagnostics,
      message = paste0(
        "当前输入表包含非 UTF-8 字符，常见原因是 Excel 普通 CSV 使用 GBK 编码。请另存为 CSV UTF-8，或检查 taxonomy / metadata 中的中文和特殊符号。\n",
        "检测到问题文件：", file_type
      )
    )
  }
  cleaned$data
}

standardize_input_tables <- function(input_list) {
  if (!is.list(input_list)) stop("standardize_input_tables(): input_list must be a list.", call. = FALSE)
  needed <- c("abundance", "metadata", "taxonomy")
  missing <- setdiff(needed, names(input_list))
  if (length(missing) > 0) stop("standardize_input_tables(): missing: ", paste(missing, collapse = ", "), call. = FALSE)

  abundance <- input_list$abundance
  metadata <- input_list$metadata
  taxonomy <- input_list$taxonomy

  abundance <- sanitize_import_table(abundance, "abundance")
  metadata <- sanitize_import_table(metadata, "metadata")
  taxonomy <- sanitize_import_table(taxonomy, "taxonomy")

  if (!is.data.frame(abundance) || ncol(abundance) < 2) stop("abundance must be a data.frame with >=2 columns.", call. = FALSE)
  if (!is.data.frame(metadata) || ncol(metadata) < 2) stop("metadata must be a data.frame with >=2 columns.", call. = FALSE)
  if (!is.data.frame(taxonomy) || ncol(taxonomy) < 2) stop("taxonomy must be a data.frame with >=2 columns.", call. = FALSE)

  # Normalize key id column names.
  nm <- names(abundance)
  nm[1] <- "FeatureID"
  names(abundance) <- nm

  # For metadata: find SampleID column (case-insensitive), otherwise assume first col.
  md_nm <- names(metadata)
  sample_col <- which(tolower(md_nm) == "sampleid")
  if (length(sample_col) == 0) sample_col <- 1
  md_nm[sample_col[1]] <- "SampleID"
  names(metadata) <- md_nm

  tx_nm <- names(taxonomy)
  fid_col <- which(tolower(tx_nm) == "featureid")
  if (length(fid_col) == 0) fid_col <- 1
  tx_nm[fid_col[1]] <- "FeatureID"
  names(taxonomy) <- tx_nm

  abundance <- sanitize_import_table(abundance, "abundance")
  metadata <- sanitize_import_table(metadata, "metadata")
  taxonomy <- sanitize_import_table(taxonomy, "taxonomy")

  list(
    abundance = abundance,
    metadata = metadata,
    taxonomy = taxonomy
  )
}

read_microbiome_inputs <- function(abundance_path, metadata_path, taxonomy_path) {
  assert_non_empty_string(abundance_path, "abundance_path")
  assert_non_empty_string(metadata_path, "metadata_path")
  assert_non_empty_string(taxonomy_path, "taxonomy_path")

  abundance <- read_table_auto(abundance_path)
  metadata <- read_table_auto(metadata_path)
  taxonomy <- read_table_auto(taxonomy_path)

  standardize_input_tables(list(abundance = abundance, metadata = metadata, taxonomy = taxonomy))
}
