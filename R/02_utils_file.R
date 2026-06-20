# File and job directory helpers.

assert_non_empty_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || nchar(x) < 1) {
    stop(sprintf("'%s' must be a non-empty string.", name), call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ensure_dir <- function(path) {
  assert_non_empty_string(path, "path")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.normalize_taxon_component <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^D_[0-9]+__", "", x)
  x <- sub("^[A-Za-z]__", "", x)
  trimws(x)
}

.is_missing_taxon_component <- function(x) {
  x <- .normalize_taxon_component(x)
  invalid_values <- c(
    "", "na", "unclassified", "unknown", "unknown_family",
    "uncultured", "metagenome", "ambiguous", "other", "n/a",
    "null", "none"
  )

  is.na(x) |
    !nzchar(x) |
    tolower(x) %in% invalid_values |
    grepl("^[A-Za-z]__$", x) |
    grepl("^D_[0-9]+__$", x)
}

.truncate_taxon_label <- function(x, max_chars = 35) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  vapply(x, function(label) {
    if (is.na(label)) return(NA_character_)
    label <- trimws(label)
    if (nchar(label, type = "chars") <= max_chars) return(label)
    paste0(substr(label, 1, max_chars - 3), "...")
  }, character(1))
}

.truncate_feature_id <- function(x, max_chars = 10) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  vapply(x, function(label) {
    if (is.na(label)) return(NA_character_)
    label <- trimws(label)
    if (nchar(label, type = "chars") <= max_chars) return(label)
    paste0(substr(label, 1, max_chars), "...")
  }, character(1))
}

make_taxon_display_label <- function(x, max_chars = 35) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)

  kingdom_values <- c("bacteria", "archaea", "eukaryota", "eukaryote", "fungi", "viruses", "virus")

  vapply(x, function(label) {
    if (is.na(label)) return(NA_character_)
    label <- trimws(label)
    if (!nzchar(label)) return("")

    if (!grepl("\\|", label)) {
      if (.is_missing_taxon_component(label)) return("")
      return(.truncate_taxon_label(.truncate_feature_id(label), max_chars = max_chars))
    }

    parts <- strsplit(label, "\\|")[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (length(parts) < 1) return("")

    first_part <- .normalize_taxon_component(parts[[1]])
    first_is_kingdom <- tolower(first_part) %in% kingdom_values
    taxonomy_parts <- if (first_is_kingdom) parts else parts[-1]
    feature_id <- if (first_is_kingdom) NA_character_ else first_part
    taxonomy_parts <- vapply(taxonomy_parts, .normalize_taxon_component, character(1))

    pick_part <- function(idx) {
      if (!is.numeric(idx) || length(idx) != 1 || is.na(idx) || idx < 1 || idx > length(taxonomy_parts)) {
        return(NA_character_)
      }
      taxonomy_parts[[idx]]
    }

    genus_label <- if (length(taxonomy_parts) >= 6) pick_part(6) else NA_character_
    family_label <- if (length(taxonomy_parts) >= 5) pick_part(5) else NA_character_
    order_label <- if (length(taxonomy_parts) >= 4) pick_part(4) else NA_character_

    valid_taxonomy <- taxonomy_parts[!.is_missing_taxon_component(taxonomy_parts)]
    last_valid_taxonomy <- if (length(valid_taxonomy) > 0) {
      utils::tail(valid_taxonomy, 1)
    } else {
      NA_character_
    }

    candidates <- c(genus_label, family_label, order_label)
    selected <- candidates[which(!.is_missing_taxon_component(candidates))]

    if (length(selected) < 1 && !.is_missing_taxon_component(last_valid_taxonomy)) {
      selected <- last_valid_taxonomy
    }
    if (length(selected) < 1 && !.is_missing_taxon_component(feature_id)) {
      selected <- .truncate_feature_id(feature_id)
    }
    if (length(selected) < 1) {
      selected <- .truncate_feature_id(parts[[1]])
    }

    .truncate_taxon_label(selected[[1]], max_chars = max_chars)
  }, character(1))
}

find_taxon_column <- function(df, candidates = c("taxon", "Taxon", "taxon_label", "FeatureID", "feature", "Genus", "label", "name")) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  for (nm in candidates) {
    if (nm %in% names(df)) return(nm)
  }
  NULL
}

normalize_taxon_name <- function(x, mark_low_information = FALSE) {
  if (is.null(x)) {
    return(if (isTRUE(mark_low_information)) list(normalized = character(0), low_information = logical(0)) else character(0))
  }

  x <- as.character(x)
  normalized <- vapply(x, function(label) {
    if (is.na(label)) return(NA_character_)
    label <- trimws(label)
    if (!nzchar(label)) return("")

    label <- gsub("[\\[\\]\"'`]", "", label)
    parts <- strsplit(label, "\\|", fixed = FALSE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (length(parts) < 1) parts <- label

    cleaned <- vapply(parts, function(part) {
      part <- gsub("[\\[\\]\"'`]", "", part)
      part <- trimws(part)
      part <- sub("^D_[0-9]+__", "", part)
      part <- sub("^[kpcfogs]__", "", part, ignore.case = TRUE)
      part <- trimws(part)
    }, character(1))

    cleaned <- cleaned[nzchar(cleaned)]
    if (length(cleaned) < 1) return("")

    last <- cleaned[[length(cleaned)]]
    if (tolower(last) %in% c("unclassified", "uncultured", "unknown", "na", "n/a", "none", "null", "")) {
      informative <- cleaned[!(tolower(cleaned) %in% c("unclassified", "uncultured", "unknown", "na", "n/a", "none", "null", ""))]
      if (length(informative) > 0) {
        last <- informative[[length(informative)]]
      }
    }

    last <- gsub("\\s+", "", last)
    tolower(last)
  }, character(1))

  low_information <- is.na(normalized) |
    !nzchar(normalized) |
    normalized %in% c("unclassified", "uncultured", "unknown")

  if (isTRUE(mark_low_information)) {
    return(list(normalized = normalized, low_information = low_information))
  }
  normalized
}

taxon_is_low_information <- function(x) {
  normalize_taxon_name(x, mark_low_information = TRUE)$low_information
}

taxon_display_from_any <- function(x, fallback = "Unknown taxon") {
  if (is.null(x)) return(fallback)
  label <- make_taxon_display_label(x)
  label <- as.character(label)
  label[is.na(label) | !nzchar(trimws(label))] <- fallback
  label
}

job_id_now <- function() {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  rand <- paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  paste0("job_", ts, "_", rand)
}

create_job_dir <- function(job_id = NULL, results_dir = NULL) {
  if (is.null(job_id)) job_id <- job_id_now()
  assert_non_empty_string(job_id, "job_id")

  if (is.null(results_dir)) results_dir <- get_cfg("paths.results_dir", "results")
  assert_non_empty_string(results_dir, "results_dir")

  job_dir <- file.path(results_dir, job_id)

  ensure_dir(job_dir)
  ensure_dir(file.path(job_dir, "input"))
  ensure_dir(file.path(job_dir, "objects"))
  ensure_dir(file.path(job_dir, "tables"))
  ensure_dir(file.path(job_dir, "figures"))
  ensure_dir(file.path(job_dir, "json"))
  ensure_dir(file.path(job_dir, "ai"))
  ensure_dir(file.path(job_dir, "report"))
  ensure_dir(file.path(job_dir, "logs"))

  normalizePath(job_dir, winslash = "/", mustWork = TRUE)
}

file_md5 <- function(path) {
  assert_non_empty_string(path, "path")
  if (!file.exists(path)) stop("file_md5(): file does not exist: ", path, call. = FALSE)
  digest::digest(file = path, algo = "md5", serialize = FALSE)
}

copy_to_job_input <- function(uploaded_path, dest_path) {
  assert_non_empty_string(uploaded_path, "uploaded_path")
  assert_non_empty_string(dest_path, "dest_path")
  if (!file.exists(uploaded_path)) stop("Uploaded file not found: ", uploaded_path, call. = FALSE)

  ensure_dir(dirname(dest_path))
  ok <- file.copy(uploaded_path, dest_path, overwrite = TRUE)
  if (!isTRUE(ok)) stop("Failed to copy file to: ", dest_path, call. = FALSE)
  normalizePath(dest_path, winslash = "/", mustWork = TRUE)
}
