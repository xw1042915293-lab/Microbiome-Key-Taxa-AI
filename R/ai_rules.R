safe_num <- function(x, digits = 4) {
  if (length(x) == 0 || all(is.na(x))) return("NA")
  formatC(as.numeric(x[1]), digits = digits, format = "f")
}

clean_taxon_name <- function(taxon) {
  if (length(taxon) == 0 || is.na(taxon) || !nzchar(as.character(taxon))) {
    return("Unknown taxon")
  }
  parts <- strsplit(as.character(taxon), "\\|")[[1]]
  tail <- parts[length(parts)]
  if (nzchar(tail)) tail else as.character(taxon)
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
  taxon <- clean_taxon_name(row[["taxon"]])
  base <- paste0(
    taxon, " ", taxon_direction(row[["log2fc"]]),
    ", with ", describe_fdr(row[["fdr"]]), ". ",
    "Mean abundance was ", safe_num(row[["mean_abundance"]]),
    " and prevalence was ", safe_num(row[["prevalence"]]), "."
  )
  if (exploratory) {
    paste0("Exploratory top taxon: ", base)
  } else {
    base
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
