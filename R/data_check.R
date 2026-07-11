# Data validation (Phase 1).
# Never skip validation. Always produce a standardized check result.

new_check_result <- function() {
  list(
    status = "pass",
    checks = tibble::tibble(
      check_name = character(),
      status = character(),
      message = character(),
      file_type = character(),
      row = integer(),
      FeatureID = character(),
      column = character(),
      value = character(),
      raw_value = character()
    ),
    summary = list(
      n_samples = NA_integer_,
      n_features = NA_integer_,
      groups = list()
    )
  )
}

add_check <- function(res, check_name, status, message,
                      file_type = NA_character_,
                      row = NA_integer_, FeatureID = NA_character_,
                      column = NA_character_, value = NA_character_,
                      raw_value = NA_character_) {
  if (!is.list(res) || is.null(res$checks)) stop("add_check(): invalid res.", call. = FALSE)
  assert_non_empty_string(check_name, "check_name")
  assert_non_empty_string(status, "status")
  if (!status %in% c("pass", "warning", "error")) stop("Invalid check status: ", status, call. = FALSE)

  res$checks <- dplyr::bind_rows(
    res$checks,
    tibble::tibble(
      check_name = check_name,
      status = status,
      message = as.character(message %||% "")[1],
      file_type = as.character(file_type)[1],
      row = as.integer(row)[1],
      FeatureID = as.character(FeatureID)[1],
      column = as.character(column)[1],
      value = as.character(value)[1],
      raw_value = as.character(raw_value)[1]
    )
  )

  if (status == "error") res$status <- "error"
  if (status == "warning" && res$status == "pass") res$status <- "warning"
  res
}

abundance_taxonomy_hint_columns <- function() {
  c("Taxon", "taxonomy", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Confidence")
}

format_abundance_issue_value <- function(x) {
  x_chr <- safe_utf8_label(x, fallback = "")
  x_chr[is.na(x)] <- "NA"
  x_chr[!is.na(x_chr) & x_chr == ""] <- "<empty>"
  x_chr
}

is_abundance_missing_like <- function(x) {
  x_chr <- safe_utf8_label(x, fallback = "")
  upper_chr <- toupper(x_chr)
  is.na(x) | is.na(x_chr) | x_chr == "" | x_chr == "-" | upper_chr %in% c("NA", "N/A")
}

find_abundance_taxonomy_like_columns <- function(col_names) {
  if (length(col_names) < 1) return(character())
  col_names[tolower(col_names) %in% tolower(abundance_taxonomy_hint_columns())]
}

format_abundance_column_list <- function(col_names, max_show = 10L) {
  col_names <- unique(safe_utf8_label(col_names, fallback = ""))
  col_names <- col_names[nzchar(col_names)]
  if (length(col_names) < 1) return("")
  if (length(col_names) <= max_show) return(paste(col_names, collapse = ", "))
  paste0(paste(utils::head(col_names, max_show), collapse = ", "), " ... 共 ", length(col_names), " 列")
}

check_utf8_table <- function(df, file_type) {
  res <- new_check_result()
  if (!is.data.frame(df)) {
    return(add_check(res, "utf8_table_type", "error", "输入表必须是 data.frame。", file_type = file_type))
  }

  cleaned <- sanitize_utf8_data_frame(df, file_type = file_type)
  if (nrow(cleaned$diagnostics) < 1) return(res)

  for (i in seq_len(nrow(cleaned$diagnostics))) {
    res <- add_check(
      res,
      check_name = "utf8_invalid_string",
      status = "error",
      message = "当前输入表包含非 UTF-8 字符，常见原因是 Excel 普通 CSV 使用 GBK 编码。请另存为 CSV UTF-8，或检查 taxonomy / metadata 中的中文和特殊符号。",
      file_type = cleaned$diagnostics$file_type[[i]],
      row = cleaned$diagnostics$row[[i]],
      column = cleaned$diagnostics$column[[i]],
      raw_value = cleaned$diagnostics$raw_value[[i]],
      value = cleaned$diagnostics$raw_value[[i]]
    )
  }

  res
}

inspect_abundance_numeric_content <- function(abundance, max_issue_cells = 20L) {
  if (!is.data.frame(abundance) || ncol(abundance) < 2) {
    empty_tbl <- tibble::tibble(
      check_name = character(),
      status = character(),
      message = character(),
      file_type = character(),
      row = integer(),
      FeatureID = character(),
      column = character(),
      value = character(),
      raw_value = character()
    )
    return(list(
      numeric_data = NULL,
      non_numeric_columns = character(),
      taxonomy_like_columns = character(),
      missing_like_columns = character(),
      negative_columns = character(),
      issue_cells = empty_tbl,
      issue_preview = empty_tbl
    ))
  }

  feature_ids <- format_abundance_issue_value(abundance[[1]])
  mat <- abundance[, -1, drop = FALSE]
  converted <- vector("list", length(mat))
  names(converted) <- names(mat)

  issue_rows <- list()
  non_numeric_columns <- character()
  missing_like_columns <- character()
  negative_columns <- character()

  for (nm in names(mat)) {
    col_data <- mat[[nm]]
    col_chr <- safe_utf8_label(col_data, fallback = "")
    is_missing_like <- is_abundance_missing_like(col_data)
    numeric_col <- rep(NA_real_, length(col_data))

    missing_idx <- which(is_missing_like)
    if (length(missing_idx) > 0) {
      missing_like_columns <- c(missing_like_columns, nm)
      issue_rows[[length(issue_rows) + 1L]] <- tibble::tibble(
        check_name = "abundance_invalid_cell",
        status = "error",
        message = "丰度表中存在缺失样式的值（空字符串、-、NA、N/A）。",
        file_type = "abundance",
        row = missing_idx,
        FeatureID = feature_ids[missing_idx],
        column = nm,
        value = format_abundance_issue_value(col_data[missing_idx]),
        raw_value = utf8_safe_raw_value(col_data[missing_idx])
      )
    }

    value_idx <- which(!is_missing_like)
    if (length(value_idx) > 0) {
      coerced <- suppressWarnings(as.numeric(col_chr[value_idx]))
      invalid_idx <- value_idx[is.na(coerced)]
      valid_idx <- value_idx[!is.na(coerced)]

      if (length(valid_idx) > 0) {
        numeric_col[valid_idx] <- coerced[!is.na(coerced)]
      }

      if (length(invalid_idx) > 0) {
        non_numeric_columns <- c(non_numeric_columns, nm)
        issue_rows[[length(issue_rows) + 1L]] <- tibble::tibble(
          check_name = "abundance_invalid_cell",
          status = "error",
          message = "该单元格无法转换为数值。",
          file_type = "abundance",
          row = invalid_idx,
          FeatureID = feature_ids[invalid_idx],
          column = nm,
          value = format_abundance_issue_value(col_data[invalid_idx]),
          raw_value = utf8_safe_raw_value(col_data[invalid_idx])
        )
      }
    }

    negative_idx <- which(!is.na(numeric_col) & numeric_col < 0)
    if (length(negative_idx) > 0) {
      negative_columns <- c(negative_columns, nm)
      issue_rows[[length(issue_rows) + 1L]] <- tibble::tibble(
        check_name = "abundance_invalid_cell",
        status = "error",
        message = "丰度值不能为负数。",
        file_type = "abundance",
        row = negative_idx,
        FeatureID = feature_ids[negative_idx],
        column = nm,
        value = format_abundance_issue_value(col_data[negative_idx]),
        raw_value = utf8_safe_raw_value(col_data[negative_idx])
      )
    }

    converted[[nm]] <- numeric_col
  }

  issue_cells <- if (length(issue_rows) > 0) {
    dplyr::bind_rows(issue_rows) |>
      dplyr::arrange(.data$row, .data$column)
  } else {
    tibble::tibble(
      check_name = character(),
      status = character(),
      message = character(),
      file_type = character(),
      row = integer(),
      FeatureID = character(),
      column = character(),
      value = character(),
      raw_value = character()
    )
  }

  list(
    numeric_data = as.data.frame(converted, check.names = FALSE, stringsAsFactors = FALSE),
    non_numeric_columns = unique(non_numeric_columns),
    taxonomy_like_columns = find_abundance_taxonomy_like_columns(unique(non_numeric_columns)),
    missing_like_columns = unique(missing_like_columns),
    negative_columns = unique(negative_columns),
    issue_cells = issue_cells,
    issue_preview = utils::head(issue_cells, max_issue_cells)
  )
}

validate_abundance_table_details <- function(abundance, max_issue_cells = 20L) {
  empty_tbl <- tibble::tibble(
    check_name = character(),
    status = character(),
    message = character(),
    file_type = character(),
    row = integer(),
    FeatureID = character(),
    column = character(),
    value = character(),
    raw_value = character()
  )

  details <- list(
    ok = TRUE,
    numeric_data = NULL,
    checks = empty_tbl,
    issue_preview = empty_tbl
  )

  add_detail <- function(check_name, message,
                         file_type = NA_character_,
                         row = NA_integer_, FeatureID = NA_character_,
                         column = NA_character_, value = NA_character_,
                         raw_value = NA_character_) {
    details$checks <<- dplyr::bind_rows(
      details$checks,
      tibble::tibble(
        check_name = check_name,
        status = "error",
        message = message,
        file_type = as.character(file_type)[1],
        row = as.integer(row)[1],
        FeatureID = as.character(FeatureID)[1],
        column = as.character(column)[1],
        value = as.character(value)[1],
        raw_value = as.character(raw_value)[1]
      )
    )
    details$ok <<- FALSE
  }

  utf8_check <- check_utf8_table(abundance, "abundance")
  if (nrow(utf8_check$checks) > 0) {
    details$checks <- dplyr::bind_rows(details$checks, utf8_check$checks)
    details$ok <- FALSE
  }

  if (!is.data.frame(abundance)) {
    add_detail("abundance_is_dataframe", "Abundance table must be a data.frame.", file_type = "abundance")
    return(details)
  }
  if (ncol(abundance) < 2) {
    add_detail("abundance_ncol", "Abundance table must have FeatureID + at least 1 sample column.", file_type = "abundance")
    return(details)
  }
  if (!identical(names(abundance)[1], "FeatureID")) {
    add_detail("abundance_featureid_colname", "abundance 表第一列必须是 FeatureID。", file_type = "abundance", column = names(abundance)[1])
  }

  feature_ids <- safe_utf8_label(abundance[[1]], fallback = "")
  if (any(is.na(feature_ids)) || any(feature_ids == "")) {
    add_detail("abundance_featureid_missing", "FeatureID contains missing/empty values.", file_type = "abundance", column = "FeatureID")
  }
  if (anyDuplicated(feature_ids) > 0) {
    add_detail("abundance_featureid_duplicate", "FeatureID contains duplicates.", file_type = "abundance", column = "FeatureID")
  }

  numeric_scan <- inspect_abundance_numeric_content(abundance, max_issue_cells = max_issue_cells)
  details$numeric_data <- numeric_scan$numeric_data
  details$issue_preview <- numeric_scan$issue_preview

  if (length(numeric_scan$non_numeric_columns) > 0) {
    add_detail(
      "abundance_non_numeric_columns",
      paste0(
        "丰度表中存在无法转换为数值的内容。请检查是否把 Taxon/分类信息列放进了 abundance 表。受影响列：",
        format_abundance_column_list(numeric_scan$non_numeric_columns, max_show = 20L),
        "。"
      ),
      file_type = "abundance"
    )

    if (length(numeric_scan$taxonomy_like_columns) > 0) {
      add_detail(
        "abundance_taxonomy_columns_in_abundance",
        paste0(
          "除 FeatureID 外，所有列都必须是样本丰度列。以下列更像 taxonomy 信息，应移到 taxonomy 表：",
          format_abundance_column_list(numeric_scan$taxonomy_like_columns, max_show = 20L),
          "。"
        ),
        file_type = "abundance"
      )
    }
  }

  if (length(numeric_scan$missing_like_columns) > 0) {
    add_detail(
      "abundance_missing_like_values",
      paste0(
        "丰度表中存在缺失样式的值（空字符串、-、NA、N/A），当前不会自动填充为 0。受影响列：",
        format_abundance_column_list(numeric_scan$missing_like_columns, max_show = 10L),
        "。"
      ),
      file_type = "abundance"
    )
  }

  if (length(numeric_scan$negative_columns) > 0) {
    add_detail(
      "abundance_negative_values",
      paste0(
        "Abundance values must be non-negative. Affected columns: ",
        format_abundance_column_list(numeric_scan$negative_columns, max_show = 20L),
        "."
      ),
      file_type = "abundance"
    )
  }

  if (nrow(numeric_scan$issue_preview) > 0) {
    details$checks <- dplyr::bind_rows(details$checks, numeric_scan$issue_preview)
  }

  details
}

check_abundance_table <- function(abundance) {
  res <- new_check_result()
  details <- validate_abundance_table_details(abundance, max_issue_cells = 20L)
  if (nrow(details$checks) > 0) res$checks <- dplyr::bind_rows(res$checks, details$checks)
  if (nrow(details$issue_preview) > 0) res$summary$abundance_issue_preview <- details$issue_preview
  if (!isTRUE(details$ok)) res$status <- "error"
  res
}

check_metadata_table <- function(metadata) {
  res <- check_utf8_table(metadata, "metadata")

  if (!is.data.frame(metadata)) {
    return(add_check(res, "metadata_is_dataframe", "error", "Metadata must be a data.frame.", file_type = "metadata"))
  }
  if (!"SampleID" %in% names(metadata)) {
    return(add_check(res, "metadata_sampleid_col", "error", "Metadata must contain 'SampleID' column.", file_type = "metadata"))
  }

  sample_ids <- safe_utf8_label(metadata$SampleID, fallback = "")
  if (any(is.na(sample_ids)) || any(sample_ids == "")) {
    res <- add_check(res, "metadata_sampleid_missing", "error", "SampleID contains missing/empty values.", file_type = "metadata", column = "SampleID")
  }
  if (anyDuplicated(sample_ids) > 0) {
    res <- add_check(res, "metadata_sampleid_duplicate", "error", "SampleID contains duplicates.", file_type = "metadata", column = "SampleID")
  }
  if (ncol(metadata) < 2) {
    res <- add_check(res, "metadata_group_var", "error", "Metadata must contain at least one grouping variable (besides SampleID).", file_type = "metadata")
  }

  res
}

check_taxonomy_table <- function(taxonomy) {
  res <- check_utf8_table(taxonomy, "taxonomy")

  if (!is.data.frame(taxonomy)) {
    return(add_check(res, "taxonomy_is_dataframe", "error", "Taxonomy must be a data.frame.", file_type = "taxonomy"))
  }
  if (!"FeatureID" %in% names(taxonomy)) {
    return(add_check(res, "taxonomy_featureid_col", "error", "Taxonomy must contain 'FeatureID' column.", file_type = "taxonomy"))
  }

  feature_ids <- safe_utf8_label(taxonomy$FeatureID, fallback = "")
  if (any(is.na(feature_ids)) || any(feature_ids == "")) {
    res <- add_check(res, "taxonomy_featureid_missing", "error", "Taxonomy FeatureID contains missing/empty values.", file_type = "taxonomy", column = "FeatureID")
  }
  if (anyDuplicated(feature_ids) > 0) {
    res <- add_check(res, "taxonomy_featureid_duplicate", "error", "Taxonomy FeatureID contains duplicates.", file_type = "taxonomy", column = "FeatureID")
  }

  suggested <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
  missing_suggested <- setdiff(suggested, names(taxonomy))
  if (length(missing_suggested) > 0) {
    res <- add_check(
      res,
      "taxonomy_levels_suggested",
      "warning",
      paste0("Suggested taxonomy columns missing: ", paste(missing_suggested, collapse = ", ")),
      file_type = "taxonomy"
    )
  }

  res
}

check_sample_matching <- function(abundance, metadata) {
  res <- new_check_result()
  if (!is.data.frame(abundance) || !is.data.frame(metadata)) {
    return(add_check(res, "sample_matching_inputs", "error", "abundance/metadata must be data.frames."))
  }
  if (ncol(abundance) < 2) return(add_check(res, "sample_matching_abundance_ncol", "error", "Abundance must have sample columns.", file_type = "abundance"))
  if (!"SampleID" %in% names(metadata)) return(add_check(res, "sample_matching_metadata_sampleid", "error", "Metadata missing SampleID.", file_type = "metadata"))

  samples_abund <- safe_utf8_label(names(abundance)[-1], fallback = "")
  samples_meta <- safe_utf8_label(metadata$SampleID, fallback = "")
  missing_in_meta <- setdiff(samples_abund, samples_meta)
  missing_in_abund <- setdiff(samples_meta, samples_abund)

  if (length(missing_in_meta) > 0) {
    res <- add_check(
      res, "sample_matching_missing_in_metadata", "error",
      paste0("Samples missing in metadata: ", paste(missing_in_meta, collapse = ", ")),
      file_type = "metadata"
    )
  }
  if (length(missing_in_abund) > 0) {
    res <- add_check(
      res, "sample_matching_missing_in_abundance", "warning",
      paste0("Metadata samples not in abundance: ", paste(missing_in_abund, collapse = ", ")),
      file_type = "abundance"
    )
  }

  res
}

check_feature_matching <- function(abundance, taxonomy) {
  res <- new_check_result()
  if (!is.data.frame(abundance) || !is.data.frame(taxonomy)) {
    return(add_check(res, "feature_matching_inputs", "error", "abundance/taxonomy must be data.frames."))
  }
  if (!"FeatureID" %in% names(taxonomy)) return(add_check(res, "feature_matching_taxonomy_featureid", "error", "Taxonomy missing FeatureID.", file_type = "taxonomy"))

  f_abund <- safe_utf8_label(abundance[[1]], fallback = "")
  f_tax <- safe_utf8_label(taxonomy$FeatureID, fallback = "")
  missing_in_tax <- setdiff(f_abund, f_tax)
  missing_in_abund <- setdiff(f_tax, f_abund)

  if (length(missing_in_tax) > 0) {
    res <- add_check(res, "feature_matching_missing_in_taxonomy", "warning", paste0("Features missing in taxonomy: ", length(missing_in_tax)), file_type = "taxonomy")
  }
  if (length(missing_in_abund) > 0) {
    res <- add_check(res, "feature_matching_extra_in_taxonomy", "warning", paste0("Taxonomy features not in abundance: ", length(missing_in_abund)), file_type = "taxonomy")
  }

  res
}

check_group_variable <- function(metadata, group_var) {
  res <- new_check_result()
  if (!is.data.frame(metadata)) return(add_check(res, "group_var_metadata_type", "error", "metadata must be a data.frame.", file_type = "metadata"))
  if (is.null(group_var) || is.na(group_var) || !is.character(group_var) || length(group_var) != 1 || nchar(group_var) < 1) {
    return(add_check(res, "group_var_provided", "warning", "No group variable selected yet.", file_type = "metadata"))
  }
  if (!group_var %in% names(metadata)) {
    return(add_check(res, "group_var_exists", "error", paste0("Group variable not found in metadata: ", safe_utf8_label(group_var, fallback = group_var)), file_type = "metadata", column = group_var))
  }

  grp <- safe_utf8_label(metadata[[group_var]], fallback = "")
  if (all(is.na(grp))) return(add_check(res, "group_var_all_missing", "error", "Group variable is all NA.", file_type = "metadata", column = group_var))

  tab <- table(grp, useNA = "ifany")
  if (length(tab) < 2) {
    res <- add_check(res, "group_var_n_groups", "error", "Group variable must have at least 2 groups.", file_type = "metadata", column = group_var)
  }
  if (any(tab < 3, na.rm = TRUE)) {
    res <- add_check(res, "group_var_min_n_per_group", "warning", "Some groups have n < 3; statistical power may be insufficient.", file_type = "metadata", column = group_var)
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
    if (r$status == "error") out$status <- "error"
    if (r$status == "warning" && out$status == "pass") out$status <- "warning"
    if (!is.null(r$summary$groups) && length(r$summary$groups) > 0) out$summary$groups <- r$summary$groups
    if (!is.null(r$summary$abundance_issue_preview) && nrow(r$summary$abundance_issue_preview) > 0) {
      out$summary$abundance_issue_preview <- r$summary$abundance_issue_preview
    }
  }

  out$summary$n_samples <- if (is.data.frame(abundance) && ncol(abundance) >= 2) ncol(abundance) - 1 else NA_integer_
  out$summary$n_features <- if (is.data.frame(abundance)) nrow(abundance) else NA_integer_
  out
}

save_data_check_summary <- function(check_result, job_dir) {
  if (!is.list(check_result) || is.null(check_result$checks)) stop("save_data_check_summary(): invalid check_result.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("save_data_check_summary(): job_dir not found: ", job_dir, call. = FALSE)

  out_path <- file.path(job_dir, "tables", "data_check_summary.csv")
  write_csv_utf8(check_result$checks, out_path, na = "")
}
