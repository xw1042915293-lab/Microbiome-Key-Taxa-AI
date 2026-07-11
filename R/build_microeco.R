# Build microeco dataset (Phase 2).
# Responsible for constructing a microeco::microtable object and persisting it to job_dir.

build_microeco_dataset <- function(abundance, metadata, taxonomy) {
  if (!is.data.frame(abundance) || ncol(abundance) < 2) stop("build_microeco_dataset(): 'abundance' must be a data.frame with >=2 columns.", call. = FALSE)
  if (!is.data.frame(metadata) || !"SampleID" %in% names(metadata)) stop("build_microeco_dataset(): 'metadata' must contain SampleID.", call. = FALSE)
  if (!is.data.frame(taxonomy) || !"FeatureID" %in% names(taxonomy)) stop("build_microeco_dataset(): 'taxonomy' must contain FeatureID.", call. = FALSE)

  abundance <- sanitize_utf8_data_frame(abundance, file_type = "abundance")
  metadata <- sanitize_utf8_data_frame(metadata, file_type = "metadata")
  taxonomy <- sanitize_utf8_data_frame(taxonomy, file_type = "taxonomy")

  utf8_diagnostics <- dplyr::bind_rows(
    abundance$diagnostics,
    metadata$diagnostics,
    taxonomy$diagnostics
  )
  if (nrow(utf8_diagnostics) > 0) {
    stop_invalid_utf8_input(utf8_diagnostics)
  }

  abundance <- abundance$data
  metadata <- metadata$data
  taxonomy <- taxonomy$data

  # OTU table: rows = FeatureID, cols = SampleID
  abundance_details <- validate_abundance_table_details(abundance, max_issue_cells = 20L)
  if (!isTRUE(abundance_details$ok)) {
    detail_msgs <- unique(stats::na.omit(as.character(abundance_details$checks$message)))
    preview <- abundance_details$issue_preview
    preview_msg <- character()
    if (is.data.frame(preview) && nrow(preview) > 0) {
      preview_lines <- apply(preview[, c("row", "FeatureID", "column", "value"), drop = FALSE], 1, function(x) {
        paste0("row=", x[["row"]], ", FeatureID=", x[["FeatureID"]], ", column=", x[["column"]], ", value=", x[["value"]])
      })
      preview_msg <- paste0("前 20 个问题单元格：", paste(preview_lines, collapse = "; "))
    }
    stop(
      paste(
        c("build_microeco_dataset(): 丰度表校验失败。", detail_msgs, preview_msg),
        collapse = " "
      ),
      call. = FALSE
    )
  }

  otu <- as.data.frame(abundance_details$numeric_data)
  rownames(otu) <- safe_utf8_label(abundance$FeatureID, fallback = "")
  colnames(otu) <- safe_utf8_label(colnames(otu), fallback = "")

  # sample table: rownames = SampleID
  samp <- as.data.frame(metadata)
  samp[] <- lapply(samp, sanitize_strings_for_output)
  rownames(samp) <- safe_utf8_label(samp$SampleID, fallback = "")

  # tax table: rownames = FeatureID
  tax <- as.data.frame(taxonomy)
  tax[] <- lapply(tax, sanitize_strings_for_output)
  rownames(tax) <- safe_utf8_label(tax$FeatureID, fallback = "")

  # Match ordering: columns(otu) must match rownames(sample_table)
  samples_abund <- colnames(otu)
  if (!all(samples_abund %in% rownames(samp))) {
    missing <- setdiff(samples_abund, rownames(samp))
    stop("build_microeco_dataset(): Samples missing in metadata: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  samp <- samp[samples_abund, , drop = FALSE]

  # Features: keep those present in both; if mismatch, validation already warned, but for object we take intersection.
  common_features <- intersect(rownames(otu), rownames(tax))
  if (length(common_features) < 2) stop("build_microeco_dataset(): Too few features matched between abundance and taxonomy.", call. = FALSE)
  otu <- otu[common_features, , drop = FALSE]
  tax <- tax[common_features, , drop = FALSE]

  # Construct microeco microtable.
  microeco::microtable$new(
    otu_table = otu,
    sample_table = samp,
    tax_table = tax
  )
}

save_microeco_dataset <- function(dataset, job_dir) {
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("save_microeco_dataset(): job_dir not found: ", job_dir, call. = FALSE)
  if (is.null(dataset)) stop("save_microeco_dataset(): dataset is NULL.", call. = FALSE)

  out_path <- file.path(job_dir, "objects", "microeco_dataset.rds")
  ensure_dir(dirname(out_path))
  saveRDS(dataset, out_path)
  normalizePath(out_path, winslash = "/", mustWork = TRUE)
}
