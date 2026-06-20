# Data validation (Phase 1).
# Never skip validation. Always produce a standardized check result.

new_check_result <- function() {
  list(
    status = "pass",
    checks = tibble::tibble(
      check_name = character(),
      status = character(),
      message = character()
    ),
    summary = list(
      n_samples = NA_integer_,
      n_features = NA_integer_,
      groups = list()
    )
  )
}

add_check <- function(res, check_name, status, message) {
  if (!is.list(res) || is.null(res$checks)) stop("add_check(): invalid res.", call. = FALSE)
  assert_non_empty_string(check_name, "check_name")
  assert_non_empty_string(status, "status")
  if (!status %in% c("pass", "warning", "error")) stop("Invalid check status: ", status, call. = FALSE)
  if (!is.character(message) || length(message) != 1) message <- as.character(message)[1]

  res$checks <- dplyr::bind_rows(
    res$checks,
    tibble::tibble(check_name = check_name, status = status, message = message)
  )

  # Escalate overall status.
  if (status == "error") res$status <- "error"
  if (status == "warning" && res$status == "pass") res$status <- "warning"
  res
}

check_abundance_table <- function(abundance) {
  res <- new_check_result()

  if (!is.data.frame(abundance)) {
    return(add_check(res, "abundance_is_dataframe", "error", "Abundance table must be a data.frame."))
  }
  if (ncol(abundance) < 2) {
    return(add_check(res, "abundance_ncol", "error", "Abundance table must have FeatureID + at least 1 sample column."))
  }
  if (!identical(names(abundance)[1], "FeatureID")) {
    res <- add_check(res, "abundance_featureid_colname", "warning", "First column should be 'FeatureID'. It will be standardized.")
  }

  feature_ids <- abundance[[1]]
  if (any(is.na(feature_ids)) || any(!is.na(feature_ids) & feature_ids == "")) {
    res <- add_check(res, "abundance_featureid_missing", "error", "FeatureID contains missing/empty values.")
  }
  if (anyDuplicated(feature_ids) > 0) {
    res <- add_check(res, "abundance_featureid_duplicate", "error", "FeatureID contains duplicates.")
  }

  mat <- abundance[, -1, drop = FALSE]
  # fread may import as character if mixed; attempt numeric coercion check.
  is_num <- vapply(mat, is.numeric, logical(1))
  if (!all(is_num)) {
    res <- add_check(
      res,
      "abundance_numeric",
      "error",
      paste0("Non-numeric abundance columns detected: ", paste(names(mat)[!is_num], collapse = ", "))
    )
  } else {
    if (any(mat < 0, na.rm = TRUE)) res <- add_check(res, "abundance_nonnegative", "error", "Abundance values must be non-negative.")
    if (any(is.na(mat))) res <- add_check(res, "abundance_missing_values", "warning", "Abundance has missing values; downstream analyses may fail.")
  }

  res
}

check_metadata_table <- function(metadata) {
  res <- new_check_result()

  if (!is.data.frame(metadata)) {
    return(add_check(res, "metadata_is_dataframe", "error", "Metadata must be a data.frame."))
  }
  if (!"SampleID" %in% names(metadata)) {
    return(add_check(res, "metadata_sampleid_col", "error", "Metadata must contain 'SampleID' column."))
  }
  if (any(is.na(metadata$SampleID)) || any(!is.na(metadata$SampleID) & metadata$SampleID == "")) {
    res <- add_check(res, "metadata_sampleid_missing", "error", "SampleID contains missing/empty values.")
  }
  if (anyDuplicated(metadata$SampleID) > 0) {
    res <- add_check(res, "metadata_sampleid_duplicate", "error", "SampleID contains duplicates.")
  }

  # Must have at least one grouping variable besides SampleID.
  if (ncol(metadata) < 2) {
    res <- add_check(res, "metadata_group_var", "error", "Metadata must contain at least one grouping variable (besides SampleID).")
  }

  res
}

check_taxonomy_table <- function(taxonomy) {
  res <- new_check_result()

  if (!is.data.frame(taxonomy)) {
    return(add_check(res, "taxonomy_is_dataframe", "error", "Taxonomy must be a data.frame."))
  }
  if (!"FeatureID" %in% names(taxonomy)) {
    return(add_check(res, "taxonomy_featureid_col", "error", "Taxonomy must contain 'FeatureID' column."))
  }
  if (any(is.na(taxonomy$FeatureID)) || any(!is.na(taxonomy$FeatureID) & taxonomy$FeatureID == "")) {
    res <- add_check(res, "taxonomy_featureid_missing", "error", "Taxonomy FeatureID contains missing/empty values.")
  }
  if (anyDuplicated(taxonomy$FeatureID) > 0) {
    res <- add_check(res, "taxonomy_featureid_duplicate", "error", "Taxonomy FeatureID contains duplicates.")
  }

  suggested <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
  missing_suggested <- setdiff(suggested, names(taxonomy))
  if (length(missing_suggested) > 0) {
    res <- add_check(
      res,
      "taxonomy_levels_suggested",
      "warning",
      paste0("Suggested taxonomy columns missing: ", paste(missing_suggested, collapse = ", "))
    )
  }

  res
}

check_sample_matching <- function(abundance, metadata) {
  res <- new_check_result()
  if (!is.data.frame(abundance) || !is.data.frame(metadata)) {
    return(add_check(res, "sample_matching_inputs", "error", "abundance/metadata must be data.frames."))
  }
  if (ncol(abundance) < 2) return(add_check(res, "sample_matching_abundance_ncol", "error", "Abundance must have sample columns."))
  if (!"SampleID" %in% names(metadata)) return(add_check(res, "sample_matching_metadata_sampleid", "error", "Metadata missing SampleID."))

  samples_abund <- names(abundance)[-1]
  samples_meta <- metadata$SampleID
  missing_in_meta <- setdiff(samples_abund, samples_meta)
  missing_in_abund <- setdiff(samples_meta, samples_abund)

  if (length(missing_in_meta) > 0) {
    res <- add_check(res, "sample_matching_missing_in_metadata", "error", paste0("Samples missing in metadata: ", paste(missing_in_meta, collapse = ", ")))
  }
  if (length(missing_in_abund) > 0) {
    res <- add_check(res, "sample_matching_missing_in_abundance", "warning", paste0("Metadata samples not in abundance: ", paste(missing_in_abund, collapse = ", ")))
  }

  res
}

check_feature_matching <- function(abundance, taxonomy) {
  res <- new_check_result()
  if (!is.data.frame(abundance) || !is.data.frame(taxonomy)) {
    return(add_check(res, "feature_matching_inputs", "error", "abundance/taxonomy must be data.frames."))
  }
  if (!"FeatureID" %in% names(taxonomy)) return(add_check(res, "feature_matching_taxonomy_featureid", "error", "Taxonomy missing FeatureID."))

  f_abund <- abundance[[1]]
  f_tax <- taxonomy$FeatureID
  missing_in_tax <- setdiff(f_abund, f_tax)
  missing_in_abund <- setdiff(f_tax, f_abund)

  if (length(missing_in_tax) > 0) {
    res <- add_check(res, "feature_matching_missing_in_taxonomy", "warning", paste0("Features missing in taxonomy: ", length(missing_in_tax)))
  }
  if (length(missing_in_abund) > 0) {
    res <- add_check(res, "feature_matching_extra_in_taxonomy", "warning", paste0("Taxonomy features not in abundance: ", length(missing_in_abund)))
  }

  res
}

check_group_variable <- function(metadata, group_var) {
  res <- new_check_result()
  if (!is.data.frame(metadata)) return(add_check(res, "group_var_metadata_type", "error", "metadata must be a data.frame."))
  if (is.null(group_var) || length(group_var) != 1 || !is.character(group_var) || isTRUE(is.na(group_var)) || !nzchar(group_var)) {
    return(add_check(res, "group_var_provided", "warning", "No group variable selected yet."))
  }
  if (!group_var %in% names(metadata)) {
    return(add_check(res, "group_var_exists", "error", paste0("Group variable not found in metadata: ", group_var)))
  }

  grp <- metadata[[group_var]]
  if (all(is.na(grp))) return(add_check(res, "group_var_all_missing", "error", "Group variable is all NA."))

  tab <- table(grp, useNA = "ifany")
  if (length(tab) < 2) {
    res <- add_check(res, "group_var_n_groups", "error", "Group variable must have at least 2 groups.")
  }
  if (any(tab < 3, na.rm = TRUE)) {
    res <- add_check(res, "group_var_min_n_per_group", "warning", "Some groups have n < 3; statistical power may be insufficient.")
  }

  res$summary$groups <- as.list(tab)
  res
}

run_all_data_checks <- function(input_list, group_var = NULL) {
  if (!is.list(input_list)) stop("run_all_data_checks(): input_list must be a list.", call. = FALSE)
  if (!all(c("abundance", "metadata", "taxonomy") %in% names(input_list))) {
    stop("run_all_data_checks(): input_list must contain abundance, metadata, taxonomy.", call. = FALSE)
  }

  abundance <- input_list$abundance
  metadata <- input_list$metadata
  taxonomy <- input_list$taxonomy

  out <- new_check_result()

  for (r in list(
    check_abundance_table(abundance),
    check_metadata_table(metadata),
    check_taxonomy_table(taxonomy),
    check_sample_matching(abundance, metadata),
    check_feature_matching(abundance, taxonomy),
    check_group_variable(metadata, group_var)
  )) {
    out$checks <- dplyr::bind_rows(out$checks, r$checks)
    r_status <- as.character(r$status %||% "pass")
    if (identical(r_status, "error")) out$status <- "error"
    if (identical(r_status, "warning") && identical(out$status, "pass")) out$status <- "warning"
    if (!is.null(r$summary$groups) && length(r$summary$groups) > 0) out$summary$groups <- r$summary$groups
  }

  out$summary$n_samples <- if (is.data.frame(abundance) && ncol(abundance) >= 2) ncol(abundance) - 1 else NA_integer_
  out$summary$n_features <- if (is.data.frame(abundance) && nrow(abundance) >= 0) nrow(abundance) else NA_integer_

  out
}

save_data_check_summary <- function(check_result, job_dir) {
  if (!is.list(check_result) || is.null(check_result$checks)) stop("save_data_check_summary(): invalid check_result.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("save_data_check_summary(): job_dir not found: ", job_dir, call. = FALSE)

  out_path <- file.path(job_dir, "tables", "data_check_summary.csv")
  ensure_dir(dirname(out_path))
  readr::write_csv(check_result$checks, out_path)
  normalizePath(out_path, winslash = "/", mustWork = TRUE)
}

# Safe wrapper around run_all_data_checks for the workflow state machine.
# Catches exceptions and returns a valid check_result with error status,
# so that "missing value where TRUE/FALSE needed" is surfaced with context.
run_data_check <- function(abundance, metadata, taxonomy, group_var = NULL, job_dir = NULL) {
  tryCatch(
    {
      result <- run_all_data_checks(
        list(abundance = abundance, metadata = metadata, taxonomy = taxonomy),
        group_var = group_var
      )
      # Safety: ensure check_df$status has no NA values
      if (is.list(result) && !is.null(result$checks) && is.data.frame(result$checks)) {
        result$checks$status[is.na(result$checks$status)] <- "warning"
      }
      result
    },
    error = function(e) {
      msg <- conditionMessage(e)
      warning("[run_data_check] Caught error: ", msg)
      message("[run_data_check] DATA_CHECK_ERROR: ", msg)
      tryCatch({
        message("[run_data_check] TRACEBACK:")
        tr <- sys.calls()
        for (i in seq_along(tr)) {
          message("  ", i, ": ", deparse(tr[[i]], width.cutoff = 200)[1])
        }
      }, error = function(e2) NULL)
      list(
        status = "error",
        checks = tibble::tibble(
          check_name = "data_check_exception",
          status = "error",
          message = paste0("run_data_check exception: ", msg)
        ),
        summary = list(n_samples = NA_integer_, n_features = NA_integer_, groups = list())
      )
    }
  )
}
