# Random Forest-based microbial biomarker screening (Phase 5).
# Outputs:
# - tables/ml_feature_importance.csv
# - tables/ml_model_metrics.csv
# - json/ml_summary.json
# - figures/ml_importance.png + .pdf
# - figures/ml_confusion_matrix.png + .pdf
# - figures/ml_roc.png + .pdf (binary only)

clean_taxon_label <- function(x) {
  # Display-only label cleaner for taxa like "ASV|Kingdom|Phylum|...|Genus".
  # Keeps the original string intact for all computations and CSV outputs.
  if (is.null(x)) return(character(0))
  x <- as.character(x)

  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (!nzchar(s)) return(s)
    parts <- strsplit(s, "\\|")[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) s else parts[[length(parts)]]
  }, character(1))
}

.wrap_axis_label <- function(x, width = 40) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (!nzchar(s)) return(s)
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1))
}

# Validate and orient a machine-learning matrix without changing the upload API.
# Returned x always uses samples as rows and numeric taxa/features as columns.
validate_ml_data <- function(x, metadata, group_var) {
  if (!is.data.frame(metadata)) {
    stop("validate_ml_data(): metadata must be a data.frame.", call. = FALSE)
  }
  assert_non_empty_string(group_var, "group_var")
  if (!group_var %in% names(metadata)) {
    stop("validate_ml_data(): grouping variable not found in metadata: ", group_var, call. = FALSE)
  }

  sample_ids <- if ("SampleID" %in% names(metadata)) {
    trimws(as.character(metadata$SampleID))
  } else {
    rownames(metadata)
  }
  if (is.null(sample_ids) || length(sample_ids) != nrow(metadata) || anyNA(sample_ids) || any(!nzchar(sample_ids))) {
    stop("validate_ml_data(): metadata sample names are missing or empty.", call. = FALSE)
  }
  if (anyDuplicated(sample_ids)) {
    stop("validate_ml_data(): metadata sample names contain duplicates.", call. = FALSE)
  }
  rownames(metadata) <- sample_ids

  x <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  row_matches <- !is.null(rownames(x)) && sum(rownames(x) %in% sample_ids)
  col_matches <- !is.null(colnames(x)) && sum(colnames(x) %in% sample_ids)
  if (row_matches == 0L && col_matches == 0L) {
    stop(
      "validate_ml_data(): abundance sample names do not match metadata sample names. ",
      "Check SampleID values and the abundance-table orientation.",
      call. = FALSE
    )
  }
  orientation <- if (row_matches >= col_matches) "samples_by_features" else "features_by_samples"
  if (identical(orientation, "features_by_samples")) {
    x <- as.data.frame(t(as.matrix(x)), check.names = FALSE, stringsAsFactors = FALSE)
  }
  original_feature_count <- ncol(x)

  common <- intersect(sample_ids, rownames(x))
  missing_in_abundance <- setdiff(sample_ids, rownames(x))
  missing_in_metadata <- setdiff(rownames(x), sample_ids)
  if (length(common) < 1L) {
    stop("validate_ml_data(): no matched samples remain after orientation detection.", call. = FALSE)
  }
  if (length(missing_in_metadata) > 0L) {
    stop(
      "validate_ml_data(): abundance contains samples absent from metadata: ",
      paste(utils::head(missing_in_metadata, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  metadata <- metadata[common, , drop = FALSE]
  x <- x[common, , drop = FALSE]
  matched_sample_ids <- rownames(x)
  raw_group <- metadata[[group_var]]
  missing_group <- is.na(raw_group) | !nzchar(trimws(as.character(raw_group)))
  if (any(missing_group)) {
    stop(
      "validate_ml_data(): grouping variable contains ", sum(missing_group),
      " missing value(s). Remove or complete these labels before analysis.",
      call. = FALSE
    )
  }
  y <- droplevels(factor(raw_group))
  group_counts <- table(y)
  if (nlevels(y) < 2L) {
    stop("validate_ml_data(): grouping variable must contain at least two classes.", call. = FALSE)
  }
  if (min(group_counts) < 3L) {
    stop("组内样本量过少，无法进行可靠的交叉验证。", call. = FALSE)
  }

  numeric_x <- lapply(x, function(z) suppressWarnings(as.numeric(as.character(z))))
  numeric_ok <- vapply(seq_along(x), function(i) {
    original_missing <- is.na(x[[i]]) | !nzchar(trimws(as.character(x[[i]])))
    all(original_missing | is.finite(numeric_x[[i]])) && any(is.finite(numeric_x[[i]]))
  }, logical(1))
  removed_non_numeric <- names(x)[!numeric_ok]
  x <- as.data.frame(numeric_x[numeric_ok], check.names = FALSE)
  rownames(x) <- matched_sample_ids
  if (ncol(x) < 1L) {
    stop("validate_ml_data(): no numeric abundance features are available for modelling.", call. = FALSE)
  }
  if (any(vapply(x, function(z) any(z < 0, na.rm = TRUE), logical(1)))) {
    stop("validate_ml_data(): abundance values must be non-negative.", call. = FALSE)
  }

  imbalance_ratio <- max(group_counts) / min(group_counts)
  list(
    x = x,
    y = y,
    metadata = metadata,
    sample_ids = common,
    orientation = orientation,
    removed_non_numeric = removed_non_numeric,
    summary = list(
      total_samples = nrow(x),
      n_groups = nlevels(y),
      group_counts = stats::setNames(as.integer(group_counts), names(group_counts)),
      original_features = original_feature_count,
      numeric_features = ncol(x),
      missing_samples = length(missing_in_abundance),
      class_imbalance = isTRUE(imbalance_ratio >= 1.5),
      imbalance_ratio = as.numeric(imbalance_ratio)
    )
  )
}

.ml_near_zero_variance <- function(z) {
  z <- z[is.finite(z)]
  if (length(z) < 2L || length(unique(z)) <= 1L) return(TRUE)
  freq <- sort(table(z), decreasing = TRUE)
  freq_ratio <- if (length(freq) > 1L) as.numeric(freq[1L] / freq[2L]) else Inf
  unique_pct <- 100 * length(freq) / length(z)
  isTRUE(freq_ratio > 19 && unique_pct <= 10)
}

# Fit filtering/imputation only on x_train, then apply the learned recipe to x_assess.
prepare_ml_data <- function(x_train, x_assess = NULL,
                            min_prevalence = 0.20,
                            min_mean_abundance = 0.0001,
                            transformation = c("clr", "relative", "log10"),
                            pseudocount = 1e-06) {
  transformation <- match.arg(transformation)
  if (!is.numeric(min_prevalence) || length(min_prevalence) != 1L ||
      !is.finite(min_prevalence) || min_prevalence < 0 || min_prevalence > 1) {
    stop("prepare_ml_data(): min_prevalence must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(min_mean_abundance) || length(min_mean_abundance) != 1L ||
      !is.finite(min_mean_abundance) || min_mean_abundance < 0) {
    stop("prepare_ml_data(): min_mean_abundance must be non-negative.", call. = FALSE)
  }
  if (!is.numeric(pseudocount) || length(pseudocount) != 1L ||
      !is.finite(pseudocount) || pseudocount <= 0) {
    stop("prepare_ml_data(): pseudocount must be a positive number.", call. = FALSE)
  }

  train <- as.data.frame(x_train, check.names = FALSE)
  assess <- if (is.null(x_assess)) NULL else as.data.frame(x_assess, check.names = FALSE)
  if (nrow(train) < 2L || ncol(train) < 1L) {
    stop("prepare_ml_data(): training data require at least two samples and one feature.", call. = FALSE)
  }
  if (!is.null(assess) && !identical(colnames(train), colnames(assess))) {
    stop("prepare_ml_data(): training and assessment feature columns must match.", call. = FALSE)
  }
  train[] <- lapply(train, function(z) suppressWarnings(as.numeric(as.character(z))))
  if (!is.null(assess)) assess[] <- lapply(assess, function(z) suppressWarnings(as.numeric(as.character(z))))
  if (any(vapply(train, function(z) any(z < 0, na.rm = TRUE), logical(1))) ||
      (!is.null(assess) && any(vapply(assess, function(z) any(z < 0, na.rm = TRUE), logical(1))))) {
    stop("prepare_ml_data(): CLR/relative-abundance transformations require non-negative data.", call. = FALSE)
  }

  row_relative <- function(df) {
    mat <- as.matrix(df)
    totals <- rowSums(mat, na.rm = TRUE)
    if (any(!is.finite(totals) | totals <= 0)) {
      stop("prepare_ml_data(): at least one sample has zero or invalid total abundance.", call. = FALSE)
    }
    sweep(mat, 1L, totals, "/")
  }
  train_rel <- row_relative(train)
  prevalence <- colMeans(train_rel > 0, na.rm = TRUE)
  mean_abundance <- colMeans(train_rel, na.rm = TRUE)
  keep_threshold <- is.finite(prevalence) & prevalence >= min_prevalence &
    is.finite(mean_abundance) & mean_abundance >= min_mean_abundance
  threshold_features <- colnames(train)[keep_threshold]
  if (length(threshold_features) < 1L) {
    stop(
      "prepare_ml_data(): all features were removed by prevalence/mean-abundance filtering. ",
      "Lower the thresholds or inspect data sparsity.",
      call. = FALSE
    )
  }

  train <- train[, threshold_features, drop = FALSE]
  if (!is.null(assess)) assess <- assess[, threshold_features, drop = FALSE]
  medians <- vapply(train, function(z) {
    value <- stats::median(z[is.finite(z)], na.rm = TRUE)
    if (is.finite(value)) value else 0
  }, numeric(1))
  impute <- function(df) {
    for (j in seq_along(df)) {
      bad <- !is.finite(df[[j]])
      if (any(bad)) df[[j]][bad] <- medians[[j]]
    }
    df
  }
  train <- impute(train)
  if (!is.null(assess)) assess <- impute(assess)

  nzv <- vapply(train, .ml_near_zero_variance, logical(1))
  kept <- names(nzv)[!nzv]
  if (length(kept) < 1L) {
    stop(
      "prepare_ml_data(): all features have zero or near-zero variance in the training fold.",
      call. = FALSE
    )
  }
  train <- train[, kept, drop = FALSE]
  if (!is.null(assess)) assess <- assess[, kept, drop = FALSE]

  transform_one <- function(df) {
    rel <- row_relative(df)
    if (identical(transformation, "relative")) return(rel)
    if (identical(transformation, "log10")) return(log10(rel + pseudocount))
    logged <- log(rel + pseudocount)
    logged - rowMeans(logged)
  }
  train_out <- as.data.frame(transform_one(train), check.names = FALSE)
  assess_out <- if (is.null(assess)) NULL else as.data.frame(transform_one(assess), check.names = FALSE)

  list(
    train = train_out,
    assess = assess_out,
    kept_features = kept,
    removed_threshold = setdiff(colnames(x_train), threshold_features),
    removed_nzv = names(nzv)[nzv],
    medians = medians[kept],
    prevalence = prevalence,
    mean_abundance = mean_abundance,
    transformation = transformation,
    pseudocount = pseudocount
  )
}

check_ml_sample_size <- function(metadata, group_var) {
  if (!is.data.frame(metadata)) stop("check_ml_sample_size(): metadata must be a data.frame.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  if (!group_var %in% names(metadata)) stop("check_ml_sample_size(): group_var not found in metadata: ", group_var, call. = FALSE)

  df <- metadata[!is.na(metadata[[group_var]]), , drop = FALSE]
  n <- nrow(df)
  reliability <- if (n < 20) {
    "exploratory only"
  } else if (n < 50) {
    "caution"
  } else {
    "acceptable"
  }

  list(
    n_samples = n,
    reliability = reliability
  )
}

prepare_ml_matrix <- function(dataset, group_var, tax_level = "Genus") {
  if (is.null(dataset)) stop("prepare_ml_matrix(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")

  samp <- dataset$sample_table
  if (is.null(samp) || !is.data.frame(samp)) stop("prepare_ml_matrix(): dataset$sample_table missing.", call. = FALSE)
  if (!group_var %in% names(samp)) stop("prepare_ml_matrix(): group_var not found in sample_table: ", group_var, call. = FALSE)

  dataset$cal_abund()
  if (is.null(dataset$taxa_abund) || !tax_level %in% names(dataset$taxa_abund)) {
    stop("prepare_ml_matrix(): tax_level '", tax_level, "' not available. Choose from: ",
         paste(names(dataset$taxa_abund), collapse = ", "), call. = FALSE)
  }

  abund <- as.data.frame(dataset$taxa_abund[[tax_level]])
  if (!is.data.frame(abund) || nrow(abund) < 1 || ncol(abund) < 2) {
    stop("prepare_ml_matrix(): invalid abundance matrix for tax_level ", tax_level, call. = FALSE)
  }

  validated <- validate_ml_data(abund, samp, group_var)
  x <- validated$x
  y <- validated$y

  list(
    x = x,
    y = y,
    sample_ids = rownames(x),
    feature_names = colnames(x),
    n_samples = nrow(x),
    n_features = ncol(x),
    n_classes = length(levels(y)),
    metadata = validated$metadata,
    validation_summary = validated$summary,
    orientation = validated$orientation,
    raw_relative_abundance = x
  )
}

.ml_make_stratified_folds <- function(y, folds = 5L, repeats = 20L, seed = 1234L) {
  y <- droplevels(factor(y))
  min_class <- min(table(y))
  folds <- min(as.integer(folds), as.integer(min_class))
  if (!is.finite(folds) || folds < 3L) {
    stop("组内样本量过少，无法进行可靠的交叉验证。", call. = FALSE)
  }
  repeats <- as.integer(repeats)
  if (!is.finite(repeats) || repeats < 1L) stop("repeats must be at least 1.", call. = FALSE)
  set.seed(as.integer(seed))
  out <- vector("list", folds * repeats)
  at <- 0L
  for (r in seq_len(repeats)) {
    assignments <- integer(length(y))
    for (lev in levels(y)) {
      idx <- sample(which(y == lev))
      assignments[idx] <- rep(seq_len(folds), length.out = length(idx))
    }
    for (f in seq_len(folds)) {
      at <- at + 1L
      assess <- which(assignments == f)
      analysis <- setdiff(seq_along(y), assess)
      if (nlevels(droplevels(y[analysis])) != nlevels(y) || nlevels(droplevels(y[assess])) != nlevels(y)) {
        stop("Cross-validation split lost one or more classes; reduce folds.", call. = FALSE)
      }
      out[[at]] <- list(repeat_id = r, fold = f, analysis = analysis, assessment = assess)
    }
  }
  attr(out, "folds") <- folds
  attr(out, "repeats") <- repeats
  out
}

.ml_safe_div <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)

.ml_binary_pr_auc <- function(truth, probability, positive) {
  truth <- factor(truth)
  keep <- !is.na(truth) & is.finite(probability)
  truth <- truth[keep]
  probability <- probability[keep]
  if (length(unique(truth)) < 2L) return(NA_real_)
  ord <- order(probability, decreasing = TRUE)
  is_pos <- truth[ord] == positive
  tp <- cumsum(is_pos)
  fp <- cumsum(!is_pos)
  recall <- tp / sum(is_pos)
  precision <- tp / (tp + fp)
  recall_prev <- c(0, head(recall, -1L))
  sum((recall - recall_prev) * precision)
}

.ml_binary_roc_df <- function(truth, probability, positive) {
  truth <- factor(truth)
  keep <- !is.na(truth) & is.finite(probability)
  truth <- truth[keep]
  probability <- probability[keep]
  if (length(unique(truth)) < 2L) return(data.frame())
  ord <- order(probability, decreasing = TRUE)
  pos <- truth[ord] == positive
  tp <- c(0, cumsum(pos))
  fp <- c(0, cumsum(!pos))
  data.frame(fpr = fp / sum(!pos), tpr = tp / sum(pos), threshold_rank = seq_along(tp) - 1L)
}

.ml_classification_metrics <- function(truth, predicted, probabilities = NULL, positive = NULL) {
  truth <- droplevels(factor(truth))
  predicted <- factor(predicted, levels = levels(truth))
  lev <- levels(truth)
  cm <- table(truth = truth, predicted = predicted)
  accuracy <- sum(diag(cm)) / sum(cm)
  per_class <- lapply(lev, function(cls) {
    tp <- cm[cls, cls]
    fn <- sum(cm[cls, ]) - tp
    fp <- sum(cm[, cls]) - tp
    tn <- sum(cm) - tp - fn - fp
    precision <- .ml_safe_div(tp, tp + fp)
    recall <- .ml_safe_div(tp, tp + fn)
    specificity <- .ml_safe_div(tn, tn + fp)
    f1 <- .ml_safe_div(2 * precision * recall, precision + recall)
    data.frame(class = cls, precision = precision, recall = recall,
               sensitivity = recall, specificity = specificity, f1 = f1)
  })
  per_class <- do.call(rbind, per_class)

  if (length(lev) == 2L) {
    positive <- positive %||% lev[2L]
    row <- per_class[per_class$class == positive, , drop = FALSE]
    negative <- setdiff(lev, positive)[1L]
    tn <- cm[negative, negative]
    tp <- cm[positive, positive]
    fp <- cm[negative, positive]
    fn <- cm[positive, negative]
    mcc_den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
    mcc <- if (is.finite(mcc_den) && mcc_den > 0) (tp * tn - fp * fn) / mcc_den else NA_real_
    prob <- if (is.null(probabilities)) NULL else probabilities[, positive]
    auc <- if (is.null(prob)) NA_real_ else tryCatch(
      as.numeric(pROC::auc(pROC::roc(truth, prob, levels = lev, direction = "<", quiet = TRUE))),
      error = function(e) NA_real_
    )
    values <- c(
      roc_auc = auc,
      pr_auc = if (is.null(prob)) NA_real_ else .ml_binary_pr_auc(truth, prob, positive),
      accuracy = accuracy,
      balanced_accuracy = mean(c(row$recall, row$specificity), na.rm = TRUE),
      sensitivity = row$sensitivity,
      specificity = row$specificity,
      precision = row$precision,
      recall = row$recall,
      f1 = row$f1,
      mcc = mcc
    )
  } else {
    aucs <- rep(NA_real_, length(lev))
    if (!is.null(probabilities)) {
      for (i in seq_along(lev)) {
        binary_truth <- factor(ifelse(truth == lev[i], lev[i], paste0("not_", lev[i])),
                               levels = c(paste0("not_", lev[i]), lev[i]))
        aucs[i] <- tryCatch(
          as.numeric(pROC::auc(pROC::roc(binary_truth, probabilities[, lev[i]], direction = "<", quiet = TRUE))),
          error = function(e) NA_real_
        )
      }
    }
    values <- c(
      accuracy = accuracy,
      balanced_accuracy = mean(per_class$recall, na.rm = TRUE),
      macro_f1 = mean(per_class$f1, na.rm = TRUE),
      macro_sensitivity = mean(per_class$sensitivity, na.rm = TRUE),
      macro_specificity = mean(per_class$specificity, na.rm = TRUE),
      macro_ovr_auc = mean(aucs, na.rm = TRUE)
    )
  }
  list(values = values, confusion = cm, per_class = per_class)
}

.ml_extract_importance <- function(fit) {
  imp <- randomForest::importance(fit, type = 1, scale = FALSE)
  if (is.null(dim(imp))) return(stats::setNames(as.numeric(imp), names(imp)))
  column <- if ("MeanDecreaseAccuracy" %in% colnames(imp)) "MeanDecreaseAccuracy" else colnames(imp)[ncol(imp)]
  stats::setNames(as.numeric(imp[, column]), rownames(imp))
}

.ml_class_weights <- function(y) {
  counts <- table(y)
  raw <- length(y) / (length(counts) * counts)
  stats::setNames(as.numeric(raw), names(counts))
}

.ml_tune_random_forest <- function(x, y, trees = 200L, seed = 1L, use_class_weights = FALSE) {
  p <- ncol(x)
  mtry_grid <- unique(pmax(1L, pmin(p, as.integer(round(c(sqrt(p), p / 5, p / 3))))))
  nodesize_grid <- unique(c(1L, 5L))
  grid <- expand.grid(mtry = mtry_grid, nodesize = nodesize_grid, KEEP.OUT.ATTRS = FALSE)
  classwt <- if (isTRUE(use_class_weights)) .ml_class_weights(y) else NULL
  fits <- vector("list", nrow(grid))
  errors <- rep(Inf, nrow(grid))
  for (i in seq_len(nrow(grid))) {
    set.seed(as.integer(seed) + i)
    fits[[i]] <- randomForest::randomForest(
      x = x, y = y, ntree = min(100L, as.integer(trees)),
      mtry = grid$mtry[i], nodesize = grid$nodesize[i],
      classwt = classwt, importance = FALSE
    )
    errors[i] <- tail(fits[[i]]$err.rate[, "OOB"], 1L)
  }
  best <- which.min(errors)
  list(mtry = grid$mtry[best], nodesize = grid$nodesize[best], oob_error = errors[best], grid = transform(grid, oob_error = errors))
}

run_repeated_cv_rf <- function(x, y, sample_ids = rownames(x), folds = 3L, repeats = 3L,
                               seed = 1234L, trees = 200L, min_prevalence = 0.20,
                               min_mean_abundance = 0.0001, transformation = "clr",
                               pseudocount = 1e-06, progress = NULL) {
  x <- as.data.frame(x, check.names = FALSE)
  y <- droplevels(factor(y))
  if (nrow(x) != length(y)) stop("run_repeated_cv_rf(): x/y size mismatch.", call. = FALSE)
  if (is.null(sample_ids) || length(sample_ids) != nrow(x)) sample_ids <- rownames(x)
  splits <- .ml_make_stratified_folds(y, folds = folds, repeats = repeats, seed = seed)
  use_weights <- max(table(y)) / min(table(y)) >= 1.5
  prediction_rows <- vector("list", length(splits))
  importance_rows <- vector("list", length(splits))
  metric_rows <- vector("list", length(splits))
  tuning_rows <- vector("list", length(splits))
  lev <- levels(y)

  for (i in seq_along(splits)) {
    split <- splits[[i]]
    if (is.function(progress)) progress(i, length(splits), split)
    recipe <- prepare_ml_data(
      x_train = x[split$analysis, , drop = FALSE],
      x_assess = x[split$assessment, , drop = FALSE],
      min_prevalence = min_prevalence,
      min_mean_abundance = min_mean_abundance,
      transformation = transformation,
      pseudocount = pseudocount
    )
    train_y <- droplevels(y[split$analysis])
    tuned <- .ml_tune_random_forest(
      recipe$train, train_y, trees = trees,
      seed = as.integer(seed) + i * 100L,
      use_class_weights = use_weights
    )
    set.seed(as.integer(seed) + i * 1000L)
    fit <- randomForest::randomForest(
      x = recipe$train, y = train_y, ntree = as.integer(trees),
      mtry = tuned$mtry, nodesize = tuned$nodesize,
      classwt = if (use_weights) .ml_class_weights(train_y) else NULL,
      importance = TRUE
    )
    probs <- stats::predict(fit, newdata = recipe$assess, type = "prob")
    probs <- probs[, lev, drop = FALSE]
    predicted <- factor(lev[max.col(probs, ties.method = "first")], levels = lev)
    fold_metrics <- .ml_classification_metrics(y[split$assessment], predicted, probs, positive = lev[min(2L, length(lev))])
    metric_rows[[i]] <- data.frame(
      repeat_id = split$repeat_id, fold = split$fold,
      metric = names(fold_metrics$values), estimate = as.numeric(fold_metrics$values),
      stringsAsFactors = FALSE
    )
    pred <- data.frame(
      sample_id = sample_ids[split$assessment], repeat_id = split$repeat_id, fold = split$fold,
      truth = as.character(y[split$assessment]), predicted = as.character(predicted),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    for (cls in lev) pred[[paste0("prob_", make.names(cls))]] <- probs[, cls]
    prediction_rows[[i]] <- pred

    importance <- .ml_extract_importance(fit)
    ord <- order(importance, decreasing = TRUE, na.last = TRUE)
    importance_rows[[i]] <- data.frame(
      repeat_id = split$repeat_id, fold = split$fold,
      feature_id = names(importance), importance = as.numeric(importance),
      rank = match(seq_along(importance), ord),
      stringsAsFactors = FALSE
    )
    tuning_rows[[i]] <- data.frame(
      repeat_id = split$repeat_id, fold = split$fold, mtry = tuned$mtry,
      min_node_size = tuned$nodesize, trees = as.integer(trees),
      training_oob_error = tuned$oob_error, n_features = ncol(recipe$train),
      stringsAsFactors = FALSE
    )
  }

  predictions <- do.call(rbind, prediction_rows)
  metrics <- do.call(rbind, metric_rows)
  importance <- do.call(rbind, importance_rows)
  tuning <- do.call(rbind, tuning_rows)
  names(predictions)[names(predictions) == "repeat_id"] <- "repeat"
  names(metrics)[names(metrics) == "repeat_id"] <- "repeat"
  names(importance)[names(importance) == "repeat_id"] <- "repeat"
  names(tuning)[names(tuning) == "repeat_id"] <- "repeat"
  rownames(predictions) <- rownames(metrics) <- rownames(importance) <- rownames(tuning) <- NULL
  list(
    predictions = predictions,
    metrics = metrics,
    importance = importance,
    tuning = tuning,
    folds = attr(splits, "folds"),
    repeats = attr(splits, "repeats"),
    used_class_weights = use_weights,
    levels = lev
  )
}

.ml_probabilities_from_predictions <- function(predictions, levels) {
  cols <- paste0("prob_", make.names(levels))
  if (!all(cols %in% names(predictions))) {
    stop("OOF prediction table is missing one or more probability columns.", call. = FALSE)
  }
  out <- as.matrix(predictions[, cols, drop = FALSE])
  storage.mode(out) <- "double"
  colnames(out) <- levels
  out
}

summarize_oof_performance <- function(predictions, levels) {
  if (!is.data.frame(predictions) || nrow(predictions) < 1L) {
    stop("summarize_oof_performance(): predictions are empty.", call. = FALSE)
  }
  required <- c("sample_id", "repeat", "fold", "truth", "predicted")
  if (!all(required %in% names(predictions))) {
    stop("summarize_oof_performance(): required OOF columns are missing.", call. = FALSE)
  }
  probs <- .ml_probabilities_from_predictions(predictions, levels)
  pooled <- .ml_classification_metrics(
    factor(predictions$truth, levels = levels),
    factor(predictions$predicted, levels = levels),
    probs,
    positive = levels[min(2L, length(levels))]
  )
  repeat_ids <- sort(unique(predictions[["repeat"]]))
  repeat_rows <- lapply(repeat_ids, function(r) {
    idx <- predictions[["repeat"]] == r
    m <- .ml_classification_metrics(
      factor(predictions$truth[idx], levels = levels),
      factor(predictions$predicted[idx], levels = levels),
      probs[idx, , drop = FALSE],
      positive = levels[min(2L, length(levels))]
    )$values
    data.frame(repeat_id = r, metric = names(m), estimate = as.numeric(m), stringsAsFactors = FALSE)
  })
  by_repeat <- do.call(rbind, repeat_rows)
  names(by_repeat)[1L] <- "repeat"
  ci <- do.call(rbind, lapply(split(by_repeat$estimate, by_repeat$metric), function(z) {
    z <- z[is.finite(z)]
    data.frame(
      mean = if (length(z)) mean(z) else NA_real_,
      sd = if (length(z) > 1L) stats::sd(z) else NA_real_,
      ci_lower = if (length(z)) as.numeric(stats::quantile(z, 0.025, names = FALSE)) else NA_real_,
      ci_upper = if (length(z)) as.numeric(stats::quantile(z, 0.975, names = FALSE)) else NA_real_
    )
  }))
  ci$metric <- rownames(ci)
  rownames(ci) <- NULL
  ci <- ci[, c("metric", "mean", "sd", "ci_lower", "ci_upper")]

  # One consensus prediction per sample for an interpretable n-sample confusion matrix.
  consensus_rows <- lapply(split(seq_len(nrow(predictions)), predictions$sample_id), function(idx) {
    mean_prob <- colMeans(probs[idx, , drop = FALSE], na.rm = TRUE)
    data.frame(
      sample_id = predictions$sample_id[idx[1L]],
      truth = predictions$truth[idx[1L]],
      predicted = levels[which.max(mean_prob)],
      stringsAsFactors = FALSE
    )
  })
  consensus <- do.call(rbind, consensus_rows)
  rownames(consensus) <- NULL
  consensus_cm <- table(
    truth = factor(consensus$truth, levels = levels),
    predicted = factor(consensus$predicted, levels = levels)
  )
  confusion <- as.data.frame(consensus_cm, stringsAsFactors = FALSE)
  names(confusion) <- c("truth", "predicted", "n")
  totals <- ave(confusion$n, confusion$truth, FUN = sum)
  confusion$proportion <- ifelse(totals > 0, confusion$n / totals, NA_real_)

  pooled_table <- data.frame(
    metric = names(pooled$values), estimate = as.numeric(pooled$values),
    evaluation = "pooled_repeated_out_of_fold", stringsAsFactors = FALSE
  )
  list(
    pooled = pooled_table,
    by_repeat = by_repeat,
    empirical_ci = ci,
    per_class = pooled$per_class,
    consensus_predictions = consensus,
    confusion = confusion
  )
}

run_ml_permutation_test <- function(x, y, observed_metric, metric_name,
                                    permutations = 999L, folds = 5L, seed = 1234L,
                                    trees = 100L, min_prevalence = 0.20,
                                    min_mean_abundance = 0.0001,
                                    transformation = "clr", pseudocount = 1e-06,
                                    progress = NULL) {
  permutations <- as.integer(permutations)
  if (!is.finite(permutations) || permutations < 0L) {
    stop("permutations must be a non-negative integer.", call. = FALSE)
  }
  if (permutations == 0L) {
    return(data.frame(
      metric = metric_name, observed = observed_metric, permutations = 0L,
      null_mean = NA_real_, null_sd = NA_real_, p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  x <- as.data.frame(x, check.names = FALSE)
  y <- droplevels(factor(y))
  lev <- levels(y)
  null_values <- rep(NA_real_, permutations)
  for (b in seq_len(permutations)) {
    if (is.function(progress)) progress(b, permutations)
    set.seed(as.integer(seed) + b * 7919L)
    perm_y <- factor(sample(as.character(y)), levels = lev)
    splits <- .ml_make_stratified_folds(perm_y, folds = folds, repeats = 1L, seed = as.integer(seed) + b)
    pred_rows <- vector("list", length(splits))
    for (i in seq_along(splits)) {
      split <- splits[[i]]
      recipe <- prepare_ml_data(
        x[split$analysis, , drop = FALSE], x[split$assessment, , drop = FALSE],
        min_prevalence = min_prevalence, min_mean_abundance = min_mean_abundance,
        transformation = transformation, pseudocount = pseudocount
      )
      train_y <- droplevels(perm_y[split$analysis])
      set.seed(as.integer(seed) + b * 1000L + i)
      fit <- randomForest::randomForest(
        x = recipe$train, y = train_y,
        ntree = max(50L, min(as.integer(trees), 200L)),
        mtry = max(1L, floor(sqrt(ncol(recipe$train)))),
        classwt = if (max(table(train_y)) / min(table(train_y)) >= 1.5) .ml_class_weights(train_y) else NULL
      )
      prob <- stats::predict(fit, recipe$assess, type = "prob")[, lev, drop = FALSE]
      predicted <- factor(lev[max.col(prob, ties.method = "first")], levels = lev)
      row <- data.frame(truth = as.character(perm_y[split$assessment]), predicted = as.character(predicted), check.names = FALSE)
      for (cls in lev) row[[paste0("prob_", make.names(cls))]] <- prob[, cls]
      pred_rows[[i]] <- row
    }
    pred <- do.call(rbind, pred_rows)
    prob <- .ml_probabilities_from_predictions(pred, lev)
    m <- .ml_classification_metrics(
      factor(pred$truth, levels = lev), factor(pred$predicted, levels = lev), prob,
      positive = lev[min(2L, length(lev))]
    )$values
    null_values[b] <- unname(m[metric_name])
  }
  finite_null <- null_values[is.finite(null_values)]
  p_value <- if (length(finite_null)) (1 + sum(finite_null >= observed_metric)) / (1 + length(finite_null)) else NA_real_
  data.frame(
    metric = metric_name,
    observed = observed_metric,
    permutations = permutations,
    null_mean = if (length(finite_null)) mean(finite_null) else NA_real_,
    null_sd = if (length(finite_null) > 1L) stats::sd(finite_null) else NA_real_,
    p_value = p_value,
    stringsAsFactors = FALSE
  )
}

.ml_unknown_taxon <- function(x) {
  x <- tolower(trimws(as.character(x)))
  is.na(x) | !nzchar(x) |
    grepl("^(unclassified|uncultured|norank|no_rank|unknown|na|n/a)([_ ;]|$)", x)
}

format_taxon_label <- function(feature_id, taxonomy_row = NULL, tax_level = "Genus") {
  ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  prefixes <- c(Kingdom = "k", Phylum = "p", Class = "c", Order = "o", Family = "f", Genus = "g", Species = "s")
  feature_id <- as.character(feature_id)[1L]
  current <- clean_taxon_label(feature_id)
  if (!is.null(taxonomy_row) && tax_level %in% names(taxonomy_row)) current <- as.character(taxonomy_row[[tax_level]][1L])
  if (!.ml_unknown_taxon(current)) return(sub("^[a-z]__", "", current, ignore.case = TRUE))

  if (!is.null(taxonomy_row)) {
    level_index <- match(tax_level, ranks)
    if (is.na(level_index)) level_index <- length(ranks)
    for (idx in rev(seq_len(level_index - 1L))) {
      rank <- ranks[idx]
      if (!rank %in% names(taxonomy_row)) next
      value <- as.character(taxonomy_row[[rank]][1L])
      if (!.ml_unknown_taxon(value)) {
        value <- sub("^[a-z]__", "", value, ignore.case = TRUE)
        return(paste0("Unclassified_", prefixes[[rank]], "__", value))
      }
    }
  }
  feature_id
}

build_ml_taxonomy_map <- function(dataset, feature_ids, tax_level = "Genus") {
  tax <- dataset$tax_table
  if (is.null(tax) || !is.data.frame(tax)) tax <- data.frame()
  ranks <- intersect(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), names(tax))
  rows <- lapply(as.character(feature_ids), function(fid) {
    idx <- integer()
    if (tax_level %in% names(tax)) {
      direct <- as.character(tax[[tax_level]]) == fid
      stripped <- sub("^[a-z]__", "", as.character(tax[[tax_level]]), ignore.case = TRUE) == sub("^[a-z]__", "", fid, ignore.case = TRUE)
      idx <- which(direct | stripped)
    }
    if (!length(idx) && fid %in% rownames(tax)) idx <- match(fid, rownames(tax))
    tax_row <- if (length(idx)) tax[idx[1L], , drop = FALSE] else NULL
    lineage <- if (length(idx) && length(ranks)) {
      unique_lineages <- unique(apply(tax[idx, ranks, drop = FALSE], 1L, function(z) paste(z, collapse = ";")))
      paste(unique_lineages, collapse = " | ")
    } else if (grepl("|", fid, fixed = TRUE)) {
      fid
    } else {
      NA_character_
    }
    data.frame(
      feature_id = fid,
      display_label = format_taxon_label(fid, tax_row, tax_level),
      taxonomy = lineage,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  duplicate_label <- duplicated(out$display_label) | duplicated(out$display_label, fromLast = TRUE)
  out$display_label[duplicate_label] <- paste0(out$display_label[duplicate_label], " [", out$feature_id[duplicate_label], "]")
  out
}

summarize_feature_importance <- function(importance_long, taxonomy_map = NULL) {
  if (!is.data.frame(importance_long) || nrow(importance_long) < 1L) {
    stop("summarize_feature_importance(): no fold-level importance values are available.", call. = FALSE)
  }
  required <- c("repeat", "fold", "feature_id", "importance", "rank")
  if (!all(required %in% names(importance_long))) {
    stop("summarize_feature_importance(): required columns are missing.", call. = FALSE)
  }
  split_values <- split(seq_len(nrow(importance_long)), importance_long$feature_id)
  total_models <- length(unique(paste(importance_long[["repeat"]], importance_long$fold, sep = ":")))
  rows <- lapply(names(split_values), function(feature) {
    idx <- split_values[[feature]]
    values <- importance_long$importance[idx]
    values <- values[is.finite(values)]
    ranks <- importance_long$rank[idx]
    data.frame(
      feature_id = feature,
      mean_importance = if (length(values)) mean(values) else NA_real_,
      median_importance = if (length(values)) stats::median(values) else NA_real_,
      sd_importance = if (length(values) > 1L) stats::sd(values) else NA_real_,
      ci_lower = if (length(values)) as.numeric(stats::quantile(values, 0.025, names = FALSE)) else NA_real_,
      ci_upper = if (length(values)) as.numeric(stats::quantile(values, 0.975, names = FALSE)) else NA_real_,
      top10_frequency = sum(ranks <= 10, na.rm = TRUE) / total_models,
      top20_frequency = sum(ranks <= 20, na.rm = TRUE) / total_models,
      evaluated_frequency = length(unique(paste(importance_long[["repeat"]][idx], importance_long$fold[idx], sep = ":"))) / total_models,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$stability_category <- ifelse(
    out$top10_frequency >= 0.50, "relatively_stable",
    ifelse(out$top10_frequency >= 0.20, "moderately_stable", "unstable")
  )
  if (!is.null(taxonomy_map) && is.data.frame(taxonomy_map)) {
    out <- merge(out, taxonomy_map, by = "feature_id", all.x = TRUE, sort = FALSE)
  }
  if (!"display_label" %in% names(out)) out$display_label <- out$feature_id
  if (!"taxonomy" %in% names(out)) out$taxonomy <- NA_character_
  out$display_label[is.na(out$display_label) | !nzchar(out$display_label)] <- out$feature_id[is.na(out$display_label) | !nzchar(out$display_label)]
  out <- out[order(out$mean_importance, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  # Compatibility columns used by Key Taxa Score, AI interpretation, and historical reports.
  out$feature <- out$feature_id
  out$importance <- out$mean_importance
  out[, c(
    "feature_id", "feature", "display_label", "taxonomy", "importance",
    "mean_importance", "median_importance", "sd_importance", "ci_lower", "ci_upper",
    "top10_frequency", "top20_frequency", "evaluated_frequency", "stability_category"
  )]
}

calculate_top_taxa_statistics <- function(relative_abundance, y, feature_ids, taxonomy_map = NULL, max_taxa = 5L) {
  x <- as.data.frame(relative_abundance, check.names = FALSE)
  y <- droplevels(factor(y))
  feature_ids <- intersect(as.character(feature_ids), colnames(x))
  feature_ids <- utils::head(feature_ids, as.integer(max_taxa))
  if (!length(feature_ids)) {
    stop("calculate_top_taxa_statistics(): selected features are absent from the abundance matrix.", call. = FALSE)
  }
  totals <- rowSums(x, na.rm = TRUE)
  if (any(!is.finite(totals) | totals <= 0)) stop("Top-taxa statistics require positive sample totals.", call. = FALSE)
  rel <- sweep(as.matrix(x[, feature_ids, drop = FALSE]), 1L, totals, "/")
  stats_rows <- vector("list", length(feature_ids))
  long_rows <- vector("list", length(feature_ids))
  for (i in seq_along(feature_ids)) {
    feature <- feature_ids[i]
    values <- rel[, feature]
    if (nlevels(y) == 2L) {
      test <- tryCatch(stats::wilcox.test(values ~ y, exact = FALSE), error = function(e) NULL)
      n1 <- sum(y == levels(y)[1L])
      n2 <- sum(y == levels(y)[2L])
      ranks <- rank(values, ties.method = "average")
      u2 <- sum(ranks[y == levels(y)[2L]]) - n2 * (n2 + 1) / 2
      effect <- 2 * u2 / (n1 * n2) - 1
      method <- "Wilcoxon rank-sum; rank-biserial correlation"
      statistic <- if (is.null(test)) NA_real_ else unname(test$statistic)
      p_value <- if (is.null(test)) NA_real_ else test$p.value
      effect_name <- "rank_biserial_correlation"
    } else {
      test <- tryCatch(stats::kruskal.test(values ~ y), error = function(e) NULL)
      h <- if (is.null(test)) NA_real_ else unname(test$statistic)
      effect <- if (is.finite(h) && length(y) > nlevels(y)) max(0, (h - nlevels(y) + 1) / (length(y) - nlevels(y))) else NA_real_
      method <- "Kruskal-Wallis; epsilon-squared"
      statistic <- h
      p_value <- if (is.null(test)) NA_real_ else test$p.value
      effect_name <- "epsilon_squared"
    }
    stats_rows[[i]] <- data.frame(
      feature_id = feature, method = method, statistic = statistic,
      p_value = p_value, effect_size = effect, effect_size_name = effect_name,
      stringsAsFactors = FALSE
    )
    long_rows[[i]] <- data.frame(
      sample_id = rownames(x), feature_id = feature,
      group = as.character(y), relative_abundance = values,
      stringsAsFactors = FALSE
    )
  }
  statistics <- do.call(rbind, stats_rows)
  statistics$fdr_bh <- stats::p.adjust(statistics$p_value, method = "BH")
  abundance_long <- do.call(rbind, long_rows)
  if (!is.null(taxonomy_map) && is.data.frame(taxonomy_map)) {
    statistics <- merge(statistics, taxonomy_map, by = "feature_id", all.x = TRUE, sort = FALSE)
    abundance_long <- merge(abundance_long, taxonomy_map[, c("feature_id", "display_label"), drop = FALSE], by = "feature_id", all.x = TRUE, sort = FALSE)
  }
  list(statistics = statistics, abundance_long = abundance_long)
}

plot_rf_importance <- function(importance_table, output_png, output_pdf) {
  if (!is.data.frame(importance_table)) stop("plot_rf_importance(): importance_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(output_png, "output_png")
  assert_non_empty_string(output_pdf, "output_pdf")

  df <- importance_table
  if (nrow(df) == 0) {
    p <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::theme_minimal(base_size = 12)
  } else {
    df <- df[order(df$importance, decreasing = TRUE), , drop = FALSE]
    df <- utils::head(df, 20)

    # Display label (do not change the original feature column used in CSVs).
    df$display_label <- clean_taxon_label(df$feature)
    df$display_label <- .wrap_axis_label(df$display_label, width = 40)
    df$display_label <- make.unique(df$display_label, sep = " #")
    df$display_label <- factor(df$display_label, levels = rev(df$display_label))

    has_negative <- any(is.finite(df$importance) & df$importance < 0)
    title <- if (has_negative) {
      "Random Forest feature importance (exploratory: negative importance present)"
    } else {
      "Random Forest feature importance"
    }
    subtitle <- "Exploratory analysis when sample size is small"

    p <- ggplot2::ggplot(df, ggplot2::aes(x = display_label, y = importance)) +
      ggplot2::geom_col(fill = "#2C7FB8") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Mean decrease in accuracy") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 10, color = "grey20")
      )
  }

  n <- if (is.data.frame(df)) max(1, min(20, nrow(df))) else 1
  height <- max(6, min(14, 3 + 0.45 * n))
  save_plot_pdf_png(p, output_pdf, output_png, width = 10, height = height)
}

plot_confusion_matrix <- function(confusion_table, output_png, output_pdf) {
  if (!is.data.frame(confusion_table)) stop("plot_confusion_matrix(): confusion_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(output_png, "output_png")
  assert_non_empty_string(output_pdf, "output_pdf")

  df <- confusion_table
  if (!all(c("truth", "predicted", "n") %in% names(df))) {
    stop("plot_confusion_matrix(): confusion_table must contain truth, predicted, n.", call. = FALSE)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = truth, fill = n)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = n), size = 4) +
    ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#08519C") +
    ggplot2::labs(title = "Confusion matrix", x = "Predicted", y = "Observed", fill = "n") +
    ggplot2::theme_minimal(base_size = 12)

  save_plot_pdf_png(p, output_pdf, output_png, width = 5.5, height = 4.8)
}

summarize_ml_for_ai <- function(model_metrics, importance_table, reliability) {
  if (!is.data.frame(model_metrics)) stop("summarize_ml_for_ai(): model_metrics must be a data.frame.", call. = FALSE)
  if (!is.data.frame(importance_table)) stop("summarize_ml_for_ai(): importance_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(reliability, "reliability")

  top <- head(importance_table[order(importance_table$importance, decreasing = TRUE), , drop = FALSE], 20)
  list(
    analysis_type = "random_forest_biomarker_screening",
    reliability = reliability,
    model_metrics = model_metrics,
    top_features = top
  )
}

run_ml_analysis <- function(dataset, group_var, tax_level = "Genus", job_dir) {
  if (is.null(dataset)) stop("run_ml_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_ml_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")

  prep <- prepare_ml_matrix(dataset = dataset, group_var = group_var, tax_level = tax_level)
  reliability <- check_ml_sample_size(metadata = dataset$sample_table, group_var = group_var)$reliability

  set.seed(123)
  rf_fit <- randomForest::randomForest(
    x = prep$x,
    y = prep$y,
    importance = TRUE
  )

  pred_class <- stats::predict(rf_fit, newdata = prep$x, type = "class")
  pred_prob <- NULL
  if (prep$n_classes == 2) {
    pred_prob <- stats::predict(rf_fit, newdata = prep$x, type = "prob")[, 2]
  }

  truth <- prep$y
  lev <- levels(truth)
  conf_mat <- table(truth = truth, predicted = pred_class)
  conf_df <- as.data.frame(conf_mat, stringsAsFactors = FALSE)
  names(conf_df) <- c("truth", "predicted", "n")

  overall_acc <- sum(diag(conf_mat)) / sum(conf_mat)
  metrics <- data.frame(
    model = "random_forest",
    tax_level = tax_level,
    n_samples = prep$n_samples,
    n_features = prep$n_features,
    n_classes = prep$n_classes,
    accuracy = overall_acc,
    reliability = reliability,
    stringsAsFactors = FALSE
  )

  if (prep$n_classes == 2) {
    tp <- conf_mat[lev[2], lev[2]]
    tn <- conf_mat[lev[1], lev[1]]
    fp <- conf_mat[lev[1], lev[2]]
    fn <- conf_mat[lev[2], lev[1]]
    metrics$sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    metrics$specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    metrics$balanced_accuracy <- mean(c(metrics$sensitivity, metrics$specificity), na.rm = TRUE)
    metrics$roc_auc <- NA_real_
    if (!is.null(pred_prob)) {
      roc_obj <- tryCatch(
        pROC::roc(response = truth, predictor = pred_prob, levels = lev, direction = "<", quiet = TRUE),
        error = function(e) NULL
      )
      if (!is.null(roc_obj)) {
        metrics$roc_auc <- as.numeric(pROC::auc(roc_obj))
        roc_df <- data.frame(
          fpr = 1 - roc_obj$specificities,
          tpr = roc_obj$sensitivities
        )
      } else {
        roc_df <- data.frame(fpr = numeric(), tpr = numeric())
      }
    } else {
      roc_df <- data.frame(fpr = numeric(), tpr = numeric())
    }
  } else {
    metrics$roc_auc <- NA_real_
    metrics$balanced_accuracy <- NA_real_
    metrics$sensitivity <- NA_real_
    metrics$specificity <- NA_real_
    roc_df <- NULL
  }

  imp <- randomForest::importance(rf_fit, type = 1, scale = FALSE)
  if (is.null(dim(imp))) {
    imp_vec <- as.numeric(imp)
  } else if ("MeanDecreaseAccuracy" %in% colnames(imp)) {
    imp_vec <- as.numeric(imp[, "MeanDecreaseAccuracy"])
  } else {
    imp_vec <- as.numeric(imp[, ncol(imp)])
  }
  imp_df <- data.frame(
    feature = rownames(imp),
    importance = imp_vec,
    stringsAsFactors = FALSE
  )
  imp_df <- imp_df[order(imp_df$importance, decreasing = TRUE), , drop = FALSE]
  rownames(imp_df) <- NULL

  out_importance <- file.path(job_dir, "tables", "ml_feature_importance.csv")
  out_metrics <- file.path(job_dir, "tables", "ml_model_metrics.csv")
  ensure_dir(dirname(out_importance))
  readr::write_csv(imp_df, out_importance)
  readr::write_csv(metrics, out_metrics)

  imp_paths <- plot_rf_importance(
    importance_table = imp_df,
    output_png = file.path(job_dir, "figures", "ml_importance.png"),
    output_pdf = file.path(job_dir, "figures", "ml_importance.pdf")
  )
  cm_paths <- plot_confusion_matrix(
    confusion_table = conf_df,
    output_png = file.path(job_dir, "figures", "ml_confusion_matrix.png"),
    output_pdf = file.path(job_dir, "figures", "ml_confusion_matrix.pdf")
  )

  roc_paths <- NULL
  if (prep$n_classes == 2 && is.data.frame(roc_df) && nrow(roc_df) > 0) {
    roc_plot <- ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr)) +
      ggplot2::geom_line(color = "#2C7FB8", linewidth = 1) +
      ggplot2::geom_abline(linetype = "dashed", color = "grey50") +
      ggplot2::coord_equal() +
      ggplot2::labs(title = "ROC curve", x = "False positive rate", y = "True positive rate") +
      ggplot2::theme_minimal(base_size = 12)
    roc_paths <- save_plot_pdf_png(
      roc_plot,
      pdf_path = file.path(job_dir, "figures", "ml_roc.pdf"),
      png_path = file.path(job_dir, "figures", "ml_roc.png"),
      width = 5.5,
      height = 5
    )
  }

  summary <- list(
    analysis_type = "random_forest",
    tax_level = tax_level,
    group_variable = group_var,
    sample_size = prep$n_samples,
    n_samples = prep$n_samples,
    n_features = prep$n_features,
    n_classes = prep$n_classes,
    reliability = reliability,
    reliability_rule = list(
      exploratory_only = "n < 20",
      caution = "20 <= n < 50",
      acceptable = "n >= 50"
    ),
    outputs = list(
      feature_importance = "tables/ml_feature_importance.csv",
      model_metrics = "tables/ml_model_metrics.csv",
      importance_plot = c("figures/ml_importance.png", "figures/ml_importance.pdf"),
      confusion_matrix_plot = c("figures/ml_confusion_matrix.png", "figures/ml_confusion_matrix.pdf"),
      roc_plot = if (prep$n_classes == 2) c("figures/ml_roc.png", "figures/ml_roc.pdf") else NULL
    ),
    caution = "Machine learning outputs describe predictive patterns and must not be interpreted as causal evidence."
  )

  summary_path <- write_json_pretty(summary, file.path(job_dir, "json", "ml_summary.json"), auto_unbox = TRUE)

  list(
    model = rf_fit,
    importance_table = imp_df,
    model_metrics = metrics,
    summary_path = summary_path,
    figure_paths = list(
      importance = imp_paths,
      confusion_matrix = cm_paths,
      roc = roc_paths
    ),
    reliability = reliability
  )
}

# ---- Publication-grade figures and Phase 5 entry point (v2) -----------------

.ml_theme <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, color = "grey30"),
      strip.background = ggplot2::element_rect(fill = "white", color = "grey70"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

.ml_save_plot_formats <- function(plot, base_path, width = 7, height = 5) {
  ensure_dir(dirname(base_path))
  paths <- c(
    pdf = paste0(base_path, ".pdf"),
    svg = paste0(base_path, ".svg"),
    tiff = paste0(base_path, ".tiff"),
    png = paste0(base_path, ".png")
  )
  ggplot2::ggsave(paths[["pdf"]], plot, width = width, height = height, units = "in", device = grDevices::cairo_pdf)
  ggplot2::ggsave(paths[["svg"]], plot, width = width, height = height, units = "in", device = grDevices::svg)
  ggplot2::ggsave(paths[["tiff"]], plot, width = width, height = height, units = "in", dpi = 600, compression = "lzw")
  ggplot2::ggsave(paths[["png"]], plot, width = width, height = height, units = "in", dpi = 300)
  normalizePath(paths, winslash = "/", mustWork = TRUE)
}

plot_ml_roc <- function(predictions, levels, empirical_ci = NULL) {
  probs <- .ml_probabilities_from_predictions(predictions, levels)
  if (length(levels) == 2L) {
    positive <- levels[2L]
    curve <- .ml_binary_roc_df(factor(predictions$truth, levels = levels), probs[, positive], positive)
    if (!nrow(curve)) stop("ROC curve cannot be calculated because OOF predictions do not contain both classes.", call. = FALSE)
    auc <- .ml_classification_metrics(factor(predictions$truth, levels = levels), factor(predictions$predicted, levels = levels), probs)$values["roc_auc"]
    ci_row <- if (is.data.frame(empirical_ci)) empirical_ci[empirical_ci$metric == "roc_auc", , drop = FALSE] else data.frame()
    subtitle <- if (nrow(ci_row)) sprintf("Pooled OOF AUC = %.3f; empirical 95%% interval %.3f–%.3f", auc, ci_row$ci_lower, ci_row$ci_upper) else sprintf("Pooled OOF AUC = %.3f", auc)
    ggplot2::ggplot(curve, ggplot2::aes(fpr, tpr)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
      ggplot2::geom_line(linewidth = 0.9, color = "#176B87") +
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      ggplot2::labs(title = "Cross-validated ROC curve", subtitle = subtitle, x = "False positive rate", y = "True positive rate") +
      .ml_theme()
  } else {
    curves <- lapply(levels, function(cls) {
      binary <- factor(ifelse(predictions$truth == cls, cls, paste0("not_", cls)), levels = c(paste0("not_", cls), cls))
      df <- .ml_binary_roc_df(binary, probs[, cls], cls)
      df$class <- cls
      df
    })
    curves <- do.call(rbind, curves)
    if (!nrow(curves)) stop("One-vs-rest ROC curves cannot be calculated from the OOF predictions.", call. = FALSE)
    ggplot2::ggplot(curves, ggplot2::aes(fpr, tpr, color = class)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::labs(title = "Cross-validated one-vs-rest ROC curves", x = "False positive rate", y = "True positive rate", color = "Class") +
      .ml_theme()
  }
}

plot_ml_pr <- function(predictions, levels) {
  probs <- .ml_probabilities_from_predictions(predictions, levels)
  classes <- if (length(levels) == 2L) levels[2L] else levels
  curves <- lapply(classes, function(cls) {
    truth <- predictions$truth == cls
    ord <- order(probs[, cls], decreasing = TRUE)
    tp <- cumsum(truth[ord])
    fp <- cumsum(!truth[ord])
    data.frame(recall = tp / sum(truth), precision = tp / (tp + fp), class = cls)
  })
  curves <- do.call(rbind, curves)
  baseline <- mean(predictions$truth == classes[1L])
  ggplot2::ggplot(curves, ggplot2::aes(recall, precision, color = class)) +
    ggplot2::geom_hline(yintercept = baseline, linetype = "dashed", color = "grey55") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(title = "Cross-validated precision–recall curve", subtitle = if (length(classes) == 1L) sprintf("No-skill baseline prevalence = %.3f", baseline) else "One-vs-rest curves", x = "Recall", y = "Precision", color = "Class") +
    .ml_theme()
}

plot_ml_confusion <- function(confusion) {
  confusion$label <- sprintf("%d\n(%.1f%%)", confusion$n, 100 * confusion$proportion)
  ggplot2::ggplot(confusion, ggplot2::aes(predicted, truth, fill = proportion)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.7) +
    ggplot2::scale_fill_gradient(low = "#F4F7F8", high = "#176B87", limits = c(0, 1), na.value = "white") +
    ggplot2::labs(title = "Cross-validated confusion matrix", subtitle = "Counts and row-normalized percentages from per-sample consensus predictions", x = "Predicted class", y = "True class", fill = "Proportion") +
    ggplot2::coord_equal() + .ml_theme()
}

plot_ml_importance_stability <- function(importance_table, top_n = 15L) {
  df <- utils::head(importance_table[order(importance_table$mean_importance, decreasing = TRUE), , drop = FALSE], as.integer(top_n))
  if (!nrow(df)) stop("No stable feature-importance results are available for plotting.", call. = FALSE)
  df$label <- .wrap_axis_label(df$display_label, 34)
  df$label <- factor(df$label, levels = rev(df$label))
  ggplot2::ggplot(df, ggplot2::aes(mean_importance, label, color = stability_category)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey75", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), orientation = "y", width = 0.15, linewidth = 0.55) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_color_manual(values = c(relatively_stable = "#176B87", moderately_stable = "#B26A00", unstable = "#777777")) +
    ggplot2::labs(title = "Stable discriminative taxa identified by random forest", subtitle = "Points show mean fold-wise permutation importance; intervals are empirical 2.5%–97.5% quantiles", x = "Permutation importance\n(mean decrease in predictive performance)", y = NULL, color = "Stability") +
    .ml_theme()
}

plot_ml_top_taxa <- function(abundance_long) {
  abundance_long$display_label <- factor(abundance_long$display_label, levels = unique(abundance_long$display_label))
  ggplot2::ggplot(abundance_long, ggplot2::aes(group, relative_abundance, color = group)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.58, linewidth = 0.5) +
    ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.75, size = 1.4) +
    ggplot2::facet_wrap(~display_label, scales = "free_y", ncol = 2) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(title = "Relative abundance of top discriminative taxa", subtitle = "Supportive analysis on the same dataset; not independent validation", x = NULL, y = "Relative abundance", color = "Group") +
    .ml_theme(base_size = 10) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
}

plot_ml_performance_distribution <- function(by_repeat) {
  keep <- by_repeat$metric %in% c("roc_auc", "balanced_accuracy", "f1", "macro_f1")
  df <- by_repeat[keep & is.finite(by_repeat$estimate), , drop = FALSE]
  if (!nrow(df)) stop("No repeated cross-validation performance distribution is available.", call. = FALSE)
  ggplot2::ggplot(df, ggplot2::aes(metric, estimate, color = metric)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.55) +
    ggplot2::geom_jitter(width = 0.10, alpha = 0.7, size = 1.5) +
    ggplot2::scale_color_brewer(palette = "Dark2", guide = "none") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = "Repeated cross-validation performance", x = NULL, y = "Estimate") + .ml_theme()
}

.ml_save_combined <- function(plots, base_path, width = 11, height = 8.5) {
  draw <- function() {
    grid::grid.newpage()
    layout <- grid::grid.layout(2, 2)
    grid::pushViewport(grid::viewport(layout = layout))
    for (i in seq_len(min(4L, length(plots)))) {
      row <- if (i <= 2L) 1L else 2L
      col <- if (i %% 2L == 1L) 1L else 2L
      print(plots[[i]] + ggplot2::labs(tag = LETTERS[i]), vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
    }
    grid::popViewport()
  }
  ensure_dir(dirname(base_path))
  grDevices::cairo_pdf(paste0(base_path, ".pdf"), width = width, height = height); draw(); grDevices::dev.off()
  grDevices::png(paste0(base_path, ".png"), width = width, height = height, units = "in", res = 300); draw(); grDevices::dev.off()
  grDevices::tiff(paste0(base_path, ".tiff"), width = width, height = height, units = "in", res = 600, compression = "lzw"); draw(); grDevices::dev.off()
  grDevices::svg(paste0(base_path, ".svg"), width = width, height = height); draw(); grDevices::dev.off()
  normalizePath(paste0(base_path, c(".pdf", ".png", ".tiff", ".svg")), winslash = "/", mustWork = TRUE)
}

.ml_write_text <- function(lines, path) {
  ensure_dir(dirname(path))
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Replaces the exploratory training-set evaluation above while retaining its public API.
run_ml_analysis <- function(dataset, group_var, tax_level = "Genus", job_dir,
                            min_prevalence = 0.20, min_mean_abundance = 0.0001,
                            transformation = "clr", pseudocount = 1e-06,
                            folds = 3L, repeats = 3L, permutations = 19L,
                            top_n = 15L, seed = 1234L, trees = 200L,
                            progress = NULL) {
  if (is.null(dataset)) stop("run_ml_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_ml_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(tax_level, "tax_level")
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Machine learning requires the randomForest package.", call. = FALSE)
  if (!requireNamespace("pROC", quietly = TRUE)) stop("Machine learning requires the pROC package.", call. = FALSE)

  notify <- function(stage, detail = NULL) if (is.function(progress)) progress(stage, detail)
  notify("数据校验")
  prep <- prepare_ml_matrix(dataset, group_var, tax_level)
  group_counts <- table(prep$y)
  reliability <- if (prep$n_samples < 30L || min(group_counts) < 10L) "exploratory only" else "caution"
  actual_folds <- min(as.integer(folds), min(group_counts))

  notify("交叉验证", sprintf("%d-fold × %d repeats", actual_folds, as.integer(repeats)))
  cv <- run_repeated_cv_rf(
    prep$x, prep$y, prep$sample_ids,
    folds = actual_folds, repeats = repeats, seed = seed, trees = trees,
    min_prevalence = min_prevalence, min_mean_abundance = min_mean_abundance,
    transformation = transformation, pseudocount = pseudocount
  )
  notify("性能评估")
  performance <- summarize_oof_performance(cv$predictions, cv$levels)
  taxonomy_map <- build_ml_taxonomy_map(dataset, unique(cv$importance$feature_id), tax_level)
  importance <- summarize_feature_importance(cv$importance, taxonomy_map)
  top_stats <- calculate_top_taxa_statistics(prep$raw_relative_abundance, prep$y, importance$feature_id, taxonomy_map, max_taxa = min(5L, top_n))

  primary_metric <- if (prep$n_classes == 2L) "roc_auc" else "balanced_accuracy"
  observed <- performance$pooled$estimate[match(primary_metric, performance$pooled$metric)]
  notify("置换检验", sprintf("%d permutations", as.integer(permutations)))
  permutation <- run_ml_permutation_test(
    prep$x, prep$y, observed_metric = observed, metric_name = primary_metric,
    permutations = permutations, folds = actual_folds, seed = seed + 50000L,
    trees = min(trees, 100L), min_prevalence = min_prevalence,
    min_mean_abundance = min_mean_abundance, transformation = transformation,
    pseudocount = pseudocount
  )

  notify("保存表格")
  tables_dir <- ensure_dir(file.path(job_dir, "tables"))
  figures_dir <- ensure_dir(file.path(job_dir, "figures"))
  sample_summary <- data.frame(
    item = c("total_samples", "group_count", paste0("group_n_", names(group_counts)), "original_features", "cv_folds", "cv_repeats", "class_imbalance", "missing_samples"),
    value = c(prep$n_samples, prep$n_classes, as.integer(group_counts), prep$n_features, cv$folds, cv$repeats, prep$validation_summary$class_imbalance, prep$validation_summary$missing_samples),
    stringsAsFactors = FALSE
  )
  parameters <- data.frame(
    parameter = c("algorithm", "package", "tax_level", "group_variable", "min_prevalence", "min_mean_relative_abundance", "transformation", "pseudocount", "folds", "repeats", "trees", "permutations", "top_taxa", "seed", "nested_cv", "tuning"),
    value = c("Random Forest", paste0("randomForest ", as.character(utils::packageVersion("randomForest"))), tax_level, group_var, min_prevalence, min_mean_abundance, transformation, pseudocount, cv$folds, cv$repeats, trees, permutations, top_n, seed, FALSE, "Training-fold OOB tuning inside each outer CV split"),
    stringsAsFactors = FALSE
  )
  pooled_wide <- as.data.frame(as.list(stats::setNames(performance$pooled$estimate, performance$pooled$metric)), check.names = FALSE)
  pooled_wide$model <- "random_forest"
  pooled_wide$evaluation <- "pooled_repeated_out_of_fold"
  pooled_wide$n_samples <- prep$n_samples
  pooled_wide$n_features <- prep$n_features
  pooled_wide$n_classes <- prep$n_classes
  pooled_wide$reliability <- reliability
  readr::write_csv(sample_summary, file.path(tables_dir, "ml_sample_summary.csv"))
  readr::write_csv(parameters, file.path(tables_dir, "ml_model_parameters.csv"))
  readr::write_csv(cv$metrics, file.path(tables_dir, "ml_cross_validation_metrics.csv"))
  readr::write_csv(cv$predictions, file.path(tables_dir, "ml_out_of_fold_predictions.csv"))
  readr::write_csv(performance$confusion, file.path(tables_dir, "ml_confusion_matrix.csv"))
  readr::write_csv(importance, file.path(tables_dir, "ml_feature_importance_stability.csv"))
  readr::write_csv(top_stats$statistics, file.path(tables_dir, "ml_top_taxa_abundance_statistics.csv"))
  readr::write_csv(top_stats$abundance_long, file.path(tables_dir, "ml_top_taxa_abundance_long.csv"))
  readr::write_csv(permutation, file.path(tables_dir, "ml_permutation_test.csv"))
  readr::write_csv(performance$by_repeat, file.path(tables_dir, "ml_performance_by_repeat.csv"))
  readr::write_csv(performance$empirical_ci, file.path(tables_dir, "ml_performance_empirical_ci.csv"))
  readr::write_csv(performance$per_class, file.path(tables_dir, "ml_per_class_metrics.csv"))
  readr::write_csv(cv$tuning, file.path(tables_dir, "ml_tuning_parameters_by_fold.csv"))
  # Backward-compatible artifacts consumed by existing modules.
  readr::write_csv(importance, file.path(tables_dir, "ml_feature_importance.csv"))
  readr::write_csv(pooled_wide, file.path(tables_dir, "ml_model_metrics.csv"))

  notify("绘图")
  roc_plot <- plot_ml_roc(cv$predictions, cv$levels, performance$empirical_ci)
  pr_plot <- plot_ml_pr(cv$predictions, cv$levels)
  cm_plot <- plot_ml_confusion(performance$confusion)
  imp_plot <- plot_ml_importance_stability(importance, top_n)
  taxa_plot <- plot_ml_top_taxa(top_stats$abundance_long)
  perf_plot <- plot_ml_performance_distribution(performance$by_repeat)
  figure_paths <- list(
    roc = .ml_save_plot_formats(roc_plot, file.path(figures_dir, "ml_figure_roc"), 6.7, 5.5),
    pr = .ml_save_plot_formats(pr_plot, file.path(figures_dir, "ml_figure_pr"), 6.7, 5.5),
    confusion = .ml_save_plot_formats(cm_plot, file.path(figures_dir, "ml_figure_confusion_matrix"), 6.4, 5.4),
    importance = .ml_save_plot_formats(imp_plot, file.path(figures_dir, "ml_figure_feature_importance"), 7.2, max(5.5, min(10, 2.8 + 0.34 * top_n))),
    top_taxa = .ml_save_plot_formats(taxa_plot, file.path(figures_dir, "ml_figure_top_taxa"), 7.2, 7),
    performance = .ml_save_plot_formats(perf_plot, file.path(figures_dir, "ml_figure_performance_distribution"), 6.5, 5)
  )
  figure_paths$combined <- .ml_save_combined(list(roc_plot, cm_plot, imp_plot, taxa_plot), file.path(figures_dir, "ml_figure_combined"))
  file.copy(file.path(figures_dir, "ml_figure_feature_importance.png"), file.path(figures_dir, "ml_importance.png"), overwrite = TRUE)
  file.copy(file.path(figures_dir, "ml_figure_feature_importance.pdf"), file.path(figures_dir, "ml_importance.pdf"), overwrite = TRUE)
  file.copy(file.path(figures_dir, "ml_figure_confusion_matrix.png"), file.path(figures_dir, "ml_confusion_matrix.png"), overwrite = TRUE)
  file.copy(file.path(figures_dir, "ml_figure_confusion_matrix.pdf"), file.path(figures_dir, "ml_confusion_matrix.pdf"), overwrite = TRUE)
  file.copy(file.path(figures_dir, "ml_figure_roc.png"), file.path(figures_dir, "ml_roc.png"), overwrite = TRUE)
  file.copy(file.path(figures_dir, "ml_figure_roc.pdf"), file.path(figures_dir, "ml_roc.pdf"), overwrite = TRUE)

  methods <- c(
    "Machine-learning methods",
    sprintf("Random Forest classification used randomForest %s with %d trees.", as.character(utils::packageVersion("randomForest")), as.integer(trees)),
    sprintf("Taxa were evaluated at the %s level. Within every outer training fold, features required prevalence >= %.3f and mean relative abundance >= %.6f.", tax_level, min_prevalence, min_mean_abundance),
    sprintf("The transformation was %s (pseudocount %.8g where applicable). Filtering, imputation, transformation, and tuning were repeated using training-fold data only.", transformation, pseudocount),
    sprintf("Generalization performance used stratified repeated %d-fold cross-validation with %d repeats; all reported performance values derive from out-of-fold predictions.", cv$folds, cv$repeats),
    "This implementation does not claim independent validation or full nested cross-validation. mtry and minimum node size were selected by OOB error inside each outer training split.",
    sprintf("Class weights were %s. Feature importance is fold-wise permutation-based mean decrease in accuracy and is summarized across outer splits.", if (cv$used_class_weights) "used" else "not required"),
    sprintf("The label permutation test used %d permutations; p = (1 + number of null scores >= observed score)/(1 + valid permutations).", as.integer(permutations)),
    sprintf("The random seed was %d. Empirical 95%% intervals are the 2.5%% and 97.5%% quantiles across repeated cross-validation runs.", as.integer(seed)),
    "Differential abundance testing was performed on the same dataset and should be interpreted as supportive rather than independent validation."
  )
  methods_path <- .ml_write_text(methods, file.path(job_dir, "ml_methods.txt"))
  perm_significant <- is.finite(permutation$p_value) && permutation$p_value < 0.05
  weak <- !is.finite(observed) || observed < if (primary_metric == "roc_auc") 0.60 else 0.55 || !perm_significant
  results_text <- if (weak) {
    c("Machine-learning results", sprintf("Observed cross-validated %s: %.3f; permutation p-value: %s.", primary_metric, observed, ifelse(is.finite(permutation$p_value), sprintf("%.4f", permutation$p_value), "not available")), "当前模型未显示出稳定且显著优于随机分类的预测能力，不建议据此筛选生物标志物。", "Associations do not establish causality, and external validation is required.")
  } else {
    c("Machine-learning results", sprintf("Observed cross-validated %s: %.3f; permutation p-value: %.4f.", primary_metric, observed, permutation$p_value), sprintf("The highest-ranked relatively stable taxa included: %s.", paste(utils::head(importance$display_label[importance$stability_category == "relatively_stable"], 5L), collapse = ", ")), "These taxa are candidate discriminative features, not confirmed biomarkers or causal organisms. Independent validation is required.")
  }
  results_path <- .ml_write_text(results_text, file.path(job_dir, "ml_results_summary.txt"))

  summary <- list(
    analysis_type = "random_forest_repeated_cross_validation",
    tax_level = tax_level, group_variable = group_var,
    n_samples = prep$n_samples, n_features = prep$n_features, n_classes = prep$n_classes,
    group_counts = as.list(as.integer(group_counts)), group_names = names(group_counts),
    reliability = reliability, performance_status = if (weak) "not_stably_better_than_random" else "better_than_permuted_labels",
    primary_metric = primary_metric, primary_metric_estimate = observed,
    permutation_p_value = permutation$p_value,
    validation = "repeated stratified cross-validation; not independent validation",
    leakage_controls = "filtering, imputation, transformation, and tuning fitted within outer training folds",
    caution = "Machine learning outputs describe predictive associations and must not be interpreted as causal evidence.",
    outputs = list(feature_importance = "tables/ml_feature_importance.csv", model_metrics = "tables/ml_model_metrics.csv", oof_predictions = "tables/ml_out_of_fold_predictions.csv", combined_figure = "figures/ml_figure_combined.pdf")
  )
  summary_path <- write_json_pretty(summary, file.path(job_dir, "json", "ml_summary.json"), auto_unbox = TRUE)
  notify("完成")
  list(
    cv = cv, importance_table = importance, model_metrics = pooled_wide,
    performance = performance, permutation_test = permutation,
    top_taxa_statistics = top_stats$statistics,
    summary_path = summary_path, methods_path = methods_path, results_path = results_path,
    figure_paths = figure_paths, reliability = reliability,
    performance_status = summary$performance_status
  )
}
