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

  # microeco stores taxa x samples here, so transpose to samples x features.
  x <- t(as.matrix(abund))
  if (!is.numeric(x)) storage.mode(x) <- "double"
  x <- as.data.frame(x, check.names = FALSE)
  x <- x[rownames(samp), , drop = FALSE]

  y <- samp[[group_var]]
  names(y) <- rownames(samp)
  y <- y[rownames(x)]
  y <- as.factor(y)

  keep <- !is.na(y)
  x <- x[keep, , drop = FALSE]
  y <- droplevels(y[keep])

  if (nrow(x) < 2) stop("prepare_ml_matrix(): need at least 2 samples after filtering missing labels.", call. = FALSE)
  if (ncol(x) < 1) stop("prepare_ml_matrix(): need at least 1 genus feature.", call. = FALSE)
  if (length(levels(y)) < 2) stop("prepare_ml_matrix(): group_var must have at least 2 classes.", call. = FALSE)

  list(
    x = x,
    y = y,
    sample_ids = rownames(x),
    feature_names = colnames(x),
    n_samples = nrow(x),
    n_features = ncol(x),
    n_classes = length(levels(y))
  )
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
