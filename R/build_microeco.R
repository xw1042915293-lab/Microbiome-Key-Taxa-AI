# Build microeco dataset (Phase 2).
# Responsible for constructing a microeco::microtable object and persisting it to job_dir.

build_microeco_dataset <- function(abundance, metadata, taxonomy) {
  if (!is.data.frame(abundance) || ncol(abundance) < 2) stop("build_microeco_dataset(): 'abundance' must be a data.frame with >=2 columns.", call. = FALSE)
  if (!is.data.frame(metadata) || !"SampleID" %in% names(metadata)) stop("build_microeco_dataset(): 'metadata' must contain SampleID.", call. = FALSE)
  if (!is.data.frame(taxonomy) || !"FeatureID" %in% names(taxonomy)) stop("build_microeco_dataset(): 'taxonomy' must contain FeatureID.", call. = FALSE)

  # OTU table: rows = FeatureID, cols = SampleID
  if (!identical(names(abundance)[1], "FeatureID")) stop("build_microeco_dataset(): abundance first column must be FeatureID.", call. = FALSE)
  otu <- as.data.frame(abundance[, -1, drop = FALSE])
  rownames(otu) <- abundance$FeatureID

  # Ensure numeric.
  for (nm in names(otu)) {
    if (!is.numeric(otu[[nm]])) {
      suppressWarnings(otu[[nm]] <- as.numeric(otu[[nm]]))
    }
  }
  if (anyNA(otu)) stop("build_microeco_dataset(): abundance contains NA after numeric coercion.", call. = FALSE)
  if (any(otu < 0, na.rm = TRUE)) stop("build_microeco_dataset(): abundance contains negative values.", call. = FALSE)

  # sample table: rownames = SampleID
  samp <- as.data.frame(metadata)
  rownames(samp) <- samp$SampleID

  # tax table: rownames = FeatureID
  tax <- as.data.frame(taxonomy)
  rownames(tax) <- tax$FeatureID

  # Match ordering: columns(otu) must match rownames(sample_table)
  samples_abund <- colnames(otu)
  if (!all(samples_abund %in% rownames(samp), na.rm = TRUE)) {
    missing <- setdiff(samples_abund, rownames(samp))
    stop("build_microeco_dataset(): Samples missing in metadata: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  samp <- samp[samples_abund, , drop = FALSE]

  # Features: keep those present in both; if mismatch, validation already warned, but for object we take intersection.
  common_features <- intersect(rownames(otu), rownames(tax))
  if (length(common_features) == 0) stop("build_microeco_dataset(): No features matched between abundance and taxonomy (intersection is 0).", call. = FALSE)
  if (length(common_features) < 2) warning("build_microeco_dataset(): Only ", length(common_features), " feature(s) matched between abundance and taxonomy.")
  if (length(common_features) < nrow(otu)) message("[build_microeco_dataset] Note: ", nrow(otu) - length(common_features), " features in abundance not in taxonomy (dropped).")
  if (length(common_features) < nrow(tax)) message("[build_microeco_dataset] Note: ", nrow(tax) - length(common_features), " features in taxonomy not in abundance (dropped).")
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

