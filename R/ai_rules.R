safe_num <- function(x, digits = 4) {
  if (length(x) == 0 || all(is.na(x))) return("NA")
  formatC(as.numeric(x[1]), digits = digits, format = "f")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

clean_taxon_name <- function(taxon) {
  if (length(taxon) == 0 || is.na(taxon) || !nzchar(as.character(taxon))) {
    return("Unknown taxon")
  }
  parts <- strsplit(as.character(taxon), "\\|")[[1]]
  tail <- parts[length(parts)]
  if (nzchar(tail)) tail else as.character(taxon)
}

pick_taxon_name_for_ai <- function(row) {
  candidates <- c(
    row[["taxon_label"]] %||% NULL,
    row[["display_taxon"]] %||% NULL,
    clean_taxon_name(row[["taxon"]] %||% NA_character_)
  )
  candidates <- as.character(candidates)
  candidates <- trimws(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) < 1) return("Unknown taxon")
  candidates[[1]]
}

is_unclassified_taxon_name <- function(x) {
  x <- trimws(as.character(x %||% ""))
  invalid_values <- c("unclassified", "unknown", "unknown_family", "uncultured", "metagenome", "na")
  tolower(x) %in% invalid_values
}

format_taxa_list <- function(labels, lang = c("en", "zh")) {
  lang <- match.arg(lang)
  labels <- unique(trimws(as.character(labels)))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  n <- length(labels)
  if (n < 1) return("")
  if (n == 1) return(labels[[1]])
  if (lang == "zh") return(paste(labels, collapse = "、"))
  if (n == 2) return(paste(labels, collapse = " and "))
  paste0(paste(labels[-n], collapse = ", "), ", and ", labels[[n]])
}

format_fdr_range <- function(fdr, lang = c("en", "zh"), digits = 3) {
  lang <- match.arg(lang)
  fdr <- suppressWarnings(as.numeric(fdr))
  fdr <- fdr[is.finite(fdr)]
  if (length(fdr) < 1) {
    return(if (lang == "zh") "FDR 不可用" else "FDR not available")
  }

  lo <- formatC(min(fdr), digits = digits, format = "f")
  hi <- formatC(max(fdr), digits = digits, format = "f")
  if (identical(lo, hi)) {
    return(if (lang == "zh") paste0("FDR = ", lo) else paste0("FDR = ", lo))
  }
  if (lang == "zh") {
    paste0("FDR 范围 ", lo, "–", hi)
  } else {
    paste0("FDR range ", lo, "-", hi)
  }
}

prepare_diff_taxa_for_ai <- function(diff_table, max_n = 5, drop_unclassified = TRUE, keep_single_unclassified = TRUE) {
  if (!is.data.frame(diff_table) || nrow(diff_table) < 1) {
    return(data.frame())
  }

  df <- diff_table
  df$.taxon_name <- vapply(seq_len(nrow(df)), function(i) {
    pick_taxon_name_for_ai(df[i, , drop = FALSE])
  }, character(1))
  df$.taxon_key <- tolower(trimws(df$.taxon_name))
  df$.is_unclassified <- vapply(df$.taxon_name, is_unclassified_taxon_name, logical(1))

  order_cols <- c()
  if ("significant" %in% names(df)) {
    order_cols <- c(order_cols, "significant")
    df$significant <- as.logical(df$significant)
  }
  if ("fdr" %in% names(df)) order_cols <- c(order_cols, "fdr")
  if ("p_value" %in% names(df)) order_cols <- c(order_cols, "p_value")

  if (length(order_cols) > 0) {
    ord_args <- lapply(order_cols, function(col) {
      values <- df[[col]]
      if (is.logical(values)) return(!values)
      if (is.numeric(values)) return(ifelse(is.na(values), Inf, values))
      values
    })
    df <- df[do.call(order, ord_args), , drop = FALSE]
  }

  if (isTRUE(drop_unclassified)) {
    non_unclassified <- df[!df$.is_unclassified, , drop = FALSE]
    if (nrow(non_unclassified) > 0) {
      df <- non_unclassified
    } else if (isTRUE(keep_single_unclassified)) {
      df <- df[1, , drop = FALSE]
    }
  }

  df <- df[!duplicated(df$.taxon_key), , drop = FALSE]
  if (is.finite(max_n) && nrow(df) > max_n) {
    df <- utils::head(df, max_n)
  }
  rownames(df) <- NULL
  df
}

classify_fdr <- function(fdr) {
  if (is.na(fdr)) return("unknown")
  if (fdr < 0.05) return("significant")
  if (fdr < 0.1) return("trend")
  "not_significant"
}

describe_fdr <- function(fdr) {
  status <- classify_fdr(fdr)
  if (status == "significant") return("significant (FDR < 0.05)")
  if (status == "trend") return("a trend (0.05 <= FDR < 0.1)")
  if (status == "not_significant") return("not significant (FDR >= 0.1)")
  "not available"
}

taxon_direction <- function(log2fc) {
  if (is.na(log2fc)) return("showed no clear direction")
  if (log2fc > 0) return("was higher in one group than the other")
  if (log2fc < 0) return("was lower in one group than the other")
  "showed no clear direction"
}

format_taxon_sentence <- function(row, exploratory = FALSE) {
  taxon <- pick_taxon_name_for_ai(row)
  direction <- taxon_direction(row[["log2fc"]])
  fdr_desc <- describe_fdr(row[["fdr"]])
  if (exploratory) {
    # Exploratory taxa: keep concise, no abundance details
    paste0("Exploratory top taxon: ", taxon, " ", direction, ", with ", fdr_desc, ".")
  } else {
    paste0(
      taxon, " ", direction,
      ", with ", fdr_desc, ". ",
      "Mean abundance was ", safe_num(row[["mean_abundance"]]),
      " and prevalence was ", safe_num(row[["prevalence"]]), "."
    )
  }
}

rule_summary_text <- function(diff_summary) {
  n_sig <- diff_summary$n_significant_taxa %||% NA_integer_
  if (!is.na(n_sig) && n_sig == 0) {
    "No FDR-significant taxa were detected in this analysis."
  } else if (!is.na(n_sig) && n_sig > 0) {
    paste0(n_sig, " FDR-significant taxa were detected in this analysis.")
  } else {
    "FDR-significant taxa status was not available."
  }
}
