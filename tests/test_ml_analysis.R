# Machine-learning validation and fold-local preprocessing tests.

source("global.R", local = TRUE)

set.seed(7)
sample_ids <- paste0("S", seq_len(12))
metadata <- data.frame(
  SampleID = sample_ids,
  Group = rep(c("Control", "Treatment"), each = 6),
  stringsAsFactors = FALSE
)
x <- data.frame(
  TaxonA = c(rpois(6, 3), rpois(6, 12)),
  TaxonB = rpois(12, 5),
  AllZero = 0,
  Rare = c(1, rep(0, 11)),
  stringsAsFactors = FALSE,
  row.names = sample_ids,
  check.names = FALSE
)

validated <- validate_ml_data(x, metadata, "Group")
stopifnot(validated$orientation == "samples_by_features")
stopifnot(nrow(validated$x) == 12L, ncol(validated$x) == 4L)
stopifnot(validated$summary$n_groups == 2L)

validated_t <- validate_ml_data(as.data.frame(t(x), check.names = FALSE), metadata, "Group")
stopifnot(validated_t$orientation == "features_by_samples")
stopifnot(identical(rownames(validated_t$x), sample_ids))

prepared <- prepare_ml_data(
  x_train = validated$x[1:8, , drop = FALSE],
  x_assess = validated$x[9:12, , drop = FALSE],
  min_prevalence = 0.20,
  min_mean_abundance = 0,
  transformation = "clr",
  pseudocount = 1e-06
)
stopifnot(!"AllZero" %in% prepared$kept_features)
stopifnot(!"Rare" %in% prepared$kept_features)
stopifnot(nrow(prepared$train) == 8L, nrow(prepared$assess) == 4L)
stopifnot(all(abs(rowMeans(prepared$train)) < 1e-10))

one_group <- metadata
one_group$Group <- "Only"
err <- tryCatch(validate_ml_data(x, one_group, "Group"), error = identity)
stopifnot(inherits(err, "error"), grepl("at least two", conditionMessage(err), fixed = TRUE))

small_group <- metadata[1:7, , drop = FALSE]
small_group$Group <- c(rep("A", 2), rep("B", 5))
err <- tryCatch(validate_ml_data(x[small_group$SampleID, , drop = FALSE], small_group, "Group"), error = identity)
stopifnot(inherits(err, "error"), grepl("组内样本量过少", conditionMessage(err), fixed = TRUE))

missing_group <- metadata
missing_group$Group[1] <- NA_character_
err <- tryCatch(validate_ml_data(x, missing_group, "Group"), error = identity)
stopifnot(inherits(err, "error"), grepl("missing value", conditionMessage(err), fixed = TRUE))

all_filtered <- tryCatch(
  prepare_ml_data(validated$x, min_prevalence = 1, min_mean_abundance = 2, transformation = "relative"),
  error = identity
)
stopifnot(inherits(all_filtered, "error"), grepl("all features were removed", conditionMessage(all_filtered), fixed = TRUE))

cv <- run_repeated_cv_rf(
  x = validated$x[, c("TaxonA", "TaxonB"), drop = FALSE],
  y = validated$y,
  sample_ids = validated$sample_ids,
  folds = 3,
  repeats = 2,
  seed = 99,
  trees = 50,
  min_prevalence = 0,
  min_mean_abundance = 0,
  transformation = "relative"
)
stopifnot(nrow(cv$predictions) == 24L)
stopifnot(length(unique(paste(cv$predictions[["repeat"]], cv$predictions$fold))) == 6L)
stopifnot(all(table(cv$predictions$sample_id) == 2L))
stopifnot(all(c("roc_auc", "accuracy", "balanced_accuracy", "f1", "mcc") %in% cv$metrics$metric))
stopifnot(nrow(cv$tuning) == 6L, nrow(cv$importance) >= 12L)

performance <- summarize_oof_performance(cv$predictions, cv$levels)
stopifnot(all(c("roc_auc", "pr_auc", "balanced_accuracy") %in% performance$pooled$metric))
stopifnot(nrow(performance$confusion) == 4L)
stopifnot(sum(performance$confusion$n) == 12L)
observed_auc <- performance$pooled$estimate[performance$pooled$metric == "roc_auc"]
perm <- run_ml_permutation_test(
  validated$x[, c("TaxonA", "TaxonB"), drop = FALSE], validated$y,
  observed_metric = observed_auc, metric_name = "roc_auc",
  permutations = 3, folds = 3, seed = 101, trees = 50,
  min_prevalence = 0, min_mean_abundance = 0, transformation = "relative"
)
stopifnot(nrow(perm) == 1L, perm$permutations == 3L)
stopifnot(is.finite(perm$p_value), perm$p_value >= 0, perm$p_value <= 1)

tax_table <- data.frame(
  Kingdom = "Bacteria", Phylum = "P", Class = "C", Order = "O",
  Family = c("FamA", "FamA", "FamB"),
  Genus = c("unclassified", "unclassified", "KnownGenus"),
  row.names = c("F1", "F2", "F3"), stringsAsFactors = FALSE
)
tax_map <- build_ml_taxonomy_map(list(tax_table = tax_table), c("unclassified", "KnownGenus"), "Genus")
stopifnot(any(grepl("Unclassified_f__FamA", tax_map$display_label, fixed = TRUE)))
stopifnot(any(tax_map$display_label == "KnownGenus"))
encoded <- "ASV1|Bacteria|Proteobacteria|Gammaproteobacteria|ExampleGenus"
encoded_map <- build_ml_taxonomy_map(list(tax_table = tax_table), encoded, "Genus")
stopifnot(encoded_map$display_label == "ExampleGenus", encoded_map$taxonomy == encoded)

importance_summary <- summarize_feature_importance(cv$importance, data.frame(
  feature_id = c("TaxonA", "TaxonB"), display_label = c("TaxonA", "TaxonB"), taxonomy = c("L1", "L2")
))
stopifnot(all(c("mean_importance", "ci_lower", "ci_upper", "top10_frequency", "stability_category") %in% names(importance_summary)))
stopifnot(all(importance_summary$top10_frequency >= 0 & importance_summary$top10_frequency <= 1))

taxa_stats <- calculate_top_taxa_statistics(
  validated$x, validated$y, importance_summary$feature_id, max_taxa = 2
)
stopifnot(nrow(taxa_stats$statistics) == 2L)
stopifnot(all(c("p_value", "fdr_bh", "effect_size") %in% names(taxa_stats$statistics)))
stopifnot(nrow(taxa_stats$abundance_long) == 24L)

# Multiclass path and macro metrics.
set.seed(17)
multi_ids <- paste0("M", seq_len(15))
multi_md <- data.frame(SampleID = multi_ids, Group = rep(c("A", "B", "C"), each = 5))
multi_x <- as.data.frame(matrix(rpois(15 * 8, lambda = 6), nrow = 15, dimnames = list(multi_ids, paste0("T", 1:8))))
multi_valid <- validate_ml_data(multi_x, multi_md, "Group")
multi_cv <- run_repeated_cv_rf(multi_valid$x, multi_valid$y, multi_valid$sample_ids, folds = 3, repeats = 1, trees = 50, min_prevalence = 0, min_mean_abundance = 0, transformation = "relative")
multi_perf <- summarize_oof_performance(multi_cv$predictions, multi_cv$levels)
stopifnot(all(c("accuracy", "balanced_accuracy", "macro_f1", "macro_ovr_auc") %in% multi_perf$pooled$metric))
stopifnot(nrow(multi_perf$confusion) == 9L)

# Imbalance and p >> n diagnostics remain analyzable without leaking full-data selection.
imb_ids <- paste0("I", seq_len(18))
imb_md <- data.frame(SampleID = imb_ids, Group = c(rep("Minor", 6), rep("Major", 12)))
imb_x <- as.data.frame(matrix(rpois(18 * 30, 4), nrow = 18, dimnames = list(imb_ids, paste0("F", 1:30))))
imb_valid <- validate_ml_data(imb_x, imb_md, "Group")
stopifnot(isTRUE(imb_valid$summary$class_imbalance), ncol(imb_valid$x) > nrow(imb_valid$x))

cat("test_ml_analysis: ok\n")
