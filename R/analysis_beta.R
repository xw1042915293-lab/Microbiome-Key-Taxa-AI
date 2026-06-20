# Beta diversity analysis.
# Outputs:
# - tables/beta_pcoa_coordinates.csv
# - tables/beta_permanova.csv
# - tables/beta_dispersion.csv
# - tables/beta_nmds_coordinates.csv
# - tables/beta_nmds_summary.csv
# - tables/beta_distance_matrix_<distance>.csv
# - tables/beta_within_between_distance.csv
# - tables/beta_within_between_stats.csv
# - tables/beta_anosim.csv
# - figures/beta_pcoa_<distance>.pdf + .png
# - figures/beta_nmds_<distance>.pdf + .png
# - figures/beta_distance_heatmap_<distance>.pdf + .png
# - figures/beta_within_between_boxplot.pdf + .png
# - json/beta_summary.json

beta_supported_distances <- function() {
  c("bray", "jaccard", "weighted_unifrac", "unweighted_unifrac")
}

beta_distance_slug <- function(distance) {
  distance <- tolower(trimws(as.character(distance %||% "bray")))
  gsub("[^a-z0-9]+", "_", distance)
}

beta_distance_label <- function(distance) {
  switch(
    beta_distance_slug(distance),
    bray = "Bray-Curtis",
    jaccard = "Jaccard",
    weighted_unifrac = "Weighted UniFrac",
    unweighted_unifrac = "Unweighted UniFrac",
    distance
  )
}

beta_normalize_distance <- function(distance) {
  requested <- tolower(trimws(as.character(distance %||% "bray")))
  warnings <- character(0)

  if (!nzchar(requested)) requested <- "bray"
  if (identical(requested, "ayc")) {
    warnings <- c(warnings, "Distance method 'ayc' is not supported in this module; fell back to Bray-Curtis.")
    requested <- "bray"
  }

  if (!requested %in% beta_supported_distances()) {
    stop(
      "run_beta_analysis(): unsupported distance method '", distance,
      "'. Supported methods: ", paste(beta_supported_distances(), collapse = ", "),
      call. = FALSE
    )
  }

  list(
    method = requested,
    slug = beta_distance_slug(requested),
    label = beta_distance_label(requested),
    warnings = unique(warnings)
  )
}

beta_make_palette <- function(groups) {
  group_levels <- unique(as.character(groups))
  group_levels <- group_levels[!is.na(group_levels) & nzchar(group_levels)]
  if (length(group_levels) < 1) return(stats::setNames(character(0), character(0)))

  hues <- seq(15, 375, length.out = length(group_levels) + 1)
  stats::setNames(
    grDevices::hcl(h = hues, c = 100, l = 60)[seq_along(group_levels)],
    group_levels
  )
}

beta_pick_column <- function(df, patterns) {
  if (!is.data.frame(df)) return(NULL)
  for (pattern in patterns) {
    hit <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[[1]])
  }
  NULL
}

beta_as_numeric <- function(x) {
  if (is.null(x) || length(x) < 1) return(NA_real_)
  suppressWarnings(as.numeric(x[[1]] %||% NA_real_))
}

beta_safe_write_csv <- function(df, path) {
  assert_non_empty_string(path, "path")
  ensure_dir(dirname(path))
  readr::write_csv(df, path, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

beta_safe_write_json <- function(x, path) {
  write_json_pretty(x, path, auto_unbox = TRUE)
}

beta_safe_save_plot <- function(plot, pdf_path, png_path, width = 8, height = 6, dpi = 300) {
  assert_non_empty_string(pdf_path, "pdf_path")
  assert_non_empty_string(png_path, "png_path")
  ensure_dir(dirname(pdf_path))
  ensure_dir(dirname(png_path))

  warnings <- character(0)
  png_saved <- NULL
  pdf_saved <- NULL

  png_saved <- tryCatch({
    withCallingHandlers(
      ggplot2::ggsave(
        filename = png_path,
        plot = plot,
        device = "png",
        width = width,
        height = height,
        dpi = dpi
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    normalizePath(png_path, winslash = "/", mustWork = TRUE)
  }, error = function(e) {
    warnings <<- c(warnings, paste0("PNG save failed: ", conditionMessage(e)))
    NULL
  })

  pdf_saved <- tryCatch({
    withCallingHandlers(
      ggplot2::ggsave(
        filename = pdf_path,
        plot = plot,
        device = "pdf",
        width = width,
        height = height
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
  }, error = function(e) {
    warnings <<- c(warnings, paste0("PDF save failed: ", conditionMessage(e)))
    NULL
  })

  if (is.null(png_saved)) {
    stop("Unable to save the beta diversity figure as PNG.", call. = FALSE)
  }

  list(
    png = png_saved,
    pdf = pdf_saved,
    warnings = unique(warnings)
  )
}

beta_empty_coordinates <- function(axis1 = "Axis1", axis2 = "Axis2", group_var = "Group") {
  out <- data.frame(
    SampleID = character(),
    axis_1 = numeric(),
    axis_2 = numeric(),
    group_value = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- c("SampleID", axis1, axis2, group_var)
  tibble::as_tibble(out)
}

beta_not_tested_row <- function(distance, group_var, interpretation, extra = list()) {
  out <- list(
    distance = beta_distance_label(distance),
    group_var = group_var,
    interpretation = interpretation
  )
  modifyList(out, extra, keep.null = TRUE)
}

beta_prepare_inputs <- function(dataset, group_var) {
  sample_table <- alpha_get_sample_table(dataset)
  if (!group_var %in% names(sample_table)) {
    stop("run_beta_analysis(): group_var not found in sample_table: ", group_var, call. = FALSE)
  }

  otu <- alpha_get_otu_matrix(dataset)
  sample_ids <- intersect(colnames(otu), rownames(sample_table))
  if (length(sample_ids) < 3) {
    stop("run_beta_analysis(): too few matched samples for beta diversity analysis.", call. = FALSE)
  }

  sample_matrix <- t(otu[, sample_ids, drop = FALSE])
  sample_matrix <- as.matrix(sample_matrix)
  suppressWarnings(storage.mode(sample_matrix) <- "numeric")

  meta <- sample_table[sample_ids, , drop = FALSE]
  meta$SampleID <- rownames(meta)
  meta[[group_var]] <- as.character(meta[[group_var]])

  warnings <- character(0)

  keep_group <- !is.na(meta[[group_var]]) & nzchar(meta[[group_var]])
  if (any(!keep_group)) {
    warnings <- c(
      warnings,
      paste0("Excluded ", sum(!keep_group), " samples with missing or empty group labels before beta diversity analysis.")
    )
    meta <- meta[keep_group, , drop = FALSE]
    sample_matrix <- sample_matrix[meta$SampleID, , drop = FALSE]
  }

  row_totals <- rowSums(sample_matrix, na.rm = TRUE)
  keep_nonzero <- is.finite(row_totals) & row_totals > 0
  if (any(!keep_nonzero)) {
    warnings <- c(
      warnings,
      paste0("Excluded ", sum(!keep_nonzero), " all-zero samples before beta diversity distance calculation.")
    )
    sample_matrix <- sample_matrix[keep_nonzero, , drop = FALSE]
    meta <- meta[rownames(sample_matrix), , drop = FALSE]
  }

  if (nrow(sample_matrix) < 3) {
    stop("run_beta_analysis(): too few non-empty samples remain for beta diversity analysis.", call. = FALSE)
  }

  if (length(unique(meta[[group_var]])) < 2) {
    warnings <- c(warnings, "Only one group remains after filtering; inferential beta diversity tests will be skipped.")
  }

  group_counts <- table(meta[[group_var]])
  if (any(group_counts < 3)) {
    warnings <- c(
      warnings,
      "At least one group has fewer than 3 samples; confidence ellipses and permutation-based inferences should be interpreted cautiously."
    )
  }

  list(
    matrix = sample_matrix,
    metadata = meta,
    group_counts = group_counts,
    warnings = unique(warnings)
  )
}

beta_compute_distance <- function(sample_matrix, distance, dataset = NULL) {
  distance <- beta_distance_slug(distance)

  if (distance %in% c("bray", "jaccard")) {
    dist_obj <- vegan::vegdist(
      sample_matrix,
      method = distance,
      binary = identical(distance, "jaccard")
    )
  } else {
    phylo_tree <- dataset$phylo_tree %||% dataset$tree %||% NULL
    if (is.null(phylo_tree)) {
      stop(
        "The selected UniFrac distance requires a phylogenetic tree in the dataset, but no tree was found.",
        call. = FALSE
      )
    }
    if (!requireNamespace("GUniFrac", quietly = TRUE)) {
      stop("Package 'GUniFrac' is required for UniFrac distance calculation.", call. = FALSE)
    }
    if (!requireNamespace("ape", quietly = TRUE)) {
      stop("Package 'ape' is required for UniFrac distance calculation.", call. = FALSE)
    }

    otu_int <- round(sample_matrix)
    phylo_tree <- ape::reorder.phylo(phylo_tree, order = "postorder")
    guf <- GUniFrac::GUniFrac(otu_int, phylo_tree, alpha = c(0, 1))$unifracs
    dist_obj <- if (identical(distance, "weighted_unifrac")) {
      stats::as.dist(guf[, , "d_1"])
    } else {
      stats::as.dist(guf[, , "d_UW"])
    }
  }

  dist_matrix <- as.matrix(dist_obj)
  if (anyNA(dist_matrix)) {
    stop("The beta diversity distance matrix contains NA values after calculation.", call. = FALSE)
  }
  if (nrow(dist_matrix) < 3) {
    stop("Too few samples remain after distance matrix calculation.", call. = FALSE)
  }

  dist_obj
}

beta_order_samples <- function(meta, group_var) {
  ord <- order(as.character(meta[[group_var]]), meta$SampleID)
  meta[ord, , drop = FALSE]
}

beta_build_ordination_plot <- function(coords, group_var, axis1, axis2, axis1_pct = NA_real_, axis2_pct = NA_real_, title, subtitle = NULL) {
  groups <- factor(coords[[group_var]], levels = unique(as.character(coords[[group_var]])))
  coords[[group_var]] <- groups
  palette <- beta_make_palette(groups)
  group_count <- length(levels(groups))

  p <- ggplot2::ggplot(
    coords,
    ggplot2::aes(
      x = .data[[axis1]],
      y = .data[[axis2]],
      color = .data[[group_var]]
    )
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.9) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = if (is.na(axis1_pct)) axis1 else sprintf("%s (%.1f%%)", axis1, 100 * axis1_pct),
      y = if (is.na(axis2_pct)) axis2 else sprintf("%s (%.1f%%)", axis2, 100 * axis2_pct),
      color = group_var
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = if (group_count > 8) "bottom" else "right",
      legend.box = "vertical"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        nrow = if (group_count > 8) ceiling(group_count / 4) else 1,
        byrow = TRUE,
        override.aes = list(size = 3.5, alpha = 1)
      )
    )

  ellipse_counts <- table(coords[[group_var]])
  ellipse_groups <- names(ellipse_counts)[ellipse_counts >= 3]
  ellipse_df <- coords[as.character(coords[[group_var]]) %in% ellipse_groups, , drop = FALSE]
  if (nrow(ellipse_df) > 0 && length(unique(ellipse_df[[group_var]])) >= 1) {
    p <- p +
      ggplot2::stat_ellipse(
        data = ellipse_df,
        ggplot2::aes(fill = .data[[group_var]]),
        geom = "polygon",
        level = 0.95,
        type = "t",
        alpha = 0.12,
        linewidth = 0.5,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE)
  }

  p
}

beta_run_pcoa <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  ord <- stats::cmdscale(dist_obj, k = 2, eig = TRUE, add = TRUE)
  points <- ord$points
  if (is.null(points)) {
    stop("PCoA calculation failed to produce coordinates.", call. = FALSE)
  }
  if (is.null(dim(points))) {
    points <- matrix(points, ncol = 1)
  }
  if (ncol(points) < 2) {
    points <- cbind(points, 0)
  }

  coords <- tibble::tibble(
    SampleID = rownames(points),
    PCo1 = as.numeric(points[, 1]),
    PCo2 = as.numeric(points[, 2])
  )
  coords <- dplyr::left_join(coords, meta[, c("SampleID", group_var), drop = FALSE], by = "SampleID")

  eig <- ord$eig
  pos_sum <- sum(eig[eig > 0], na.rm = TRUE)
  axis1_pct <- if (is.finite(pos_sum) && pos_sum > 0 && length(eig) >= 1) eig[[1]] / pos_sum else NA_real_
  axis2_pct <- if (is.finite(pos_sum) && pos_sum > 0 && length(eig) >= 2) eig[[2]] / pos_sum else NA_real_

  coord_path <- beta_safe_write_csv(coords, file.path(job_dir, "tables", "beta_pcoa_coordinates.csv"))
  fig_stub <- paste0("beta_pcoa_", distance_info$slug)
  plot_obj <- beta_build_ordination_plot(
    coords = coords,
    group_var = group_var,
    axis1 = "PCo1",
    axis2 = "PCo2",
    axis1_pct = axis1_pct,
    axis2_pct = axis2_pct,
    title = paste0("PCoA (", distance_info$label, " distance)"),
    subtitle = "Points are colored by group; ellipses are shown only for groups with >= 3 samples."
  )
  saved <- suppressWarnings(
    beta_safe_save_plot(
      plot_obj,
      pdf_path = file.path(job_dir, "figures", paste0(fig_stub, ".pdf")),
      png_path = file.path(job_dir, "figures", paste0(fig_stub, ".png")),
      width = 8.2,
      height = 6.2,
      dpi = 320
    )
  )

  list(
    coordinates = coords,
    coordinates_path = coord_path,
    explained = c(axis1 = axis1_pct, axis2 = axis2_pct),
    figure = list(
      png = paste0("figures/", fig_stub, ".png"),
      pdf = paste0("figures/", fig_stub, ".pdf"),
      png_path = saved$png,
      pdf_path = saved$pdf
    ),
    warnings = saved$warnings
  )
}

beta_run_permanova <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  group_factor <- factor(meta[[group_var]], levels = unique(as.character(meta[[group_var]])))
  if (nlevels(group_factor) < 2) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = "PERMANOVA was skipped because fewer than two groups were available.",
      extra = list(
        term = group_var,
        df = NA_real_,
        sum_of_squares = NA_real_,
        R2 = NA_real_,
        F_value = NA_real_,
        p_value = NA_real_
      )
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_permanova.csv"))
    return(list(table = out, path = out_path, warnings = "PERMANOVA skipped: fewer than two groups."))
  }

  meta_df <- data.frame(group = group_factor, row.names = meta$SampleID, check.names = FALSE)
  perm <- tryCatch(
    vegan::adonis2(dist_obj ~ group, data = meta_df, permutations = 999),
    error = function(e) e
  )
  if (inherits(perm, "error")) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = paste0("PERMANOVA failed: ", conditionMessage(perm)),
      extra = list(
        term = group_var,
        df = NA_real_,
        sum_of_squares = NA_real_,
        R2 = NA_real_,
        F_value = NA_real_,
        p_value = NA_real_
      )
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_permanova.csv"))
    return(list(table = out, path = out_path, warnings = out$interpretation[[1]]))
  }
  perm_tbl <- as.data.frame(perm)
  perm_tbl$term <- rownames(perm_tbl)
  row <- perm_tbl[!tolower(perm_tbl$term) %in% c("residual", "residuals", "total"), , drop = FALSE]
  if (nrow(row) < 1) row <- perm_tbl[1, , drop = FALSE]

  r2_col <- beta_pick_column(row, c("^R2$", "^r2$"))
  f_col <- beta_pick_column(row, c("^F$", "^F\\.Model$", "F"))
  p_col <- beta_pick_column(row, c("^Pr\\(>F\\)$", "p_value", "p"))
  df_col <- beta_pick_column(row, c("^Df$", "df"))
  ss_col <- beta_pick_column(row, c("SumOfSqs", "Sum Sq", "SumsOfSqs"))

  p_value <- beta_as_numeric(row[[p_col]])
  r2_value <- beta_as_numeric(row[[r2_col]])
  interpretation <- if (is.finite(p_value) && p_value < 0.05) {
    "Community composition differs significantly across groups based on PERMANOVA."
  } else {
    "PERMANOVA did not detect a statistically significant between-group community composition difference."
  }

  out <- tibble::tibble(
    distance = distance_info$label,
    group_var = group_var,
    term = group_var,
    df = beta_as_numeric(row[[df_col]]),
    sum_of_squares = beta_as_numeric(row[[ss_col]]),
    R2 = r2_value,
    F_value = beta_as_numeric(row[[f_col]]),
    p_value = p_value,
    interpretation = interpretation
  )

  out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_permanova.csv"))
  list(table = out, path = out_path, warnings = character(0))
}

beta_run_dispersion <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  group_factor <- factor(meta[[group_var]], levels = unique(as.character(meta[[group_var]])))
  if (nlevels(group_factor) < 2) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = "Beta dispersion was skipped because fewer than two groups were available.",
      extra = list(F_value = NA_real_, p_value = NA_real_)
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_dispersion.csv"))
    return(list(table = out, path = out_path, warnings = "Beta dispersion skipped: fewer than two groups."))
  }

  disp <- tryCatch(vegan::betadisper(dist_obj, group_factor), error = function(e) e)
  if (inherits(disp, "error")) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = paste0("Beta dispersion failed: ", conditionMessage(disp)),
      extra = list(F_value = NA_real_, p_value = NA_real_)
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_dispersion.csv"))
    return(list(table = out, path = out_path, warnings = out$interpretation[[1]]))
  }
  disp_perm <- tryCatch(vegan::permutest(disp, permutations = 999), error = function(e) e)
  if (inherits(disp_perm, "error")) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = paste0("Beta dispersion permutation test failed: ", conditionMessage(disp_perm)),
      extra = list(F_value = NA_real_, p_value = NA_real_)
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_dispersion.csv"))
    return(list(table = out, path = out_path, warnings = out$interpretation[[1]]))
  }
  disp_tab <- as.data.frame(disp_perm$tab)
  disp_tab$term <- rownames(disp_tab)
  row <- disp_tab[!tolower(disp_tab$term) %in% c("residuals", "total"), , drop = FALSE]
  if (nrow(row) < 1) row <- disp_tab[1, , drop = FALSE]

  f_col <- beta_pick_column(row, c("^F$", "^F value$", "F"))
  p_col <- beta_pick_column(row, c("^Pr\\(>F\\)$", "p_value", "p"))
  p_value <- beta_as_numeric(row[[p_col]])
  interpretation <- if (is.finite(p_value) && p_value < 0.05) {
    "Group dispersions differ significantly; PERMANOVA may be influenced by unequal within-group dispersion."
  } else {
    "No significant dispersion difference was detected; PERMANOVA results are more robust to unequal within-group dispersion."
  }

  out <- tibble::tibble(
    distance = distance_info$label,
    group_var = group_var,
    F_value = beta_as_numeric(row[[f_col]]),
    p_value = p_value,
    interpretation = interpretation
  )
  out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_dispersion.csv"))
  list(table = out, path = out_path, warnings = character(0))
}

beta_run_nmds <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  coord_path <- file.path(job_dir, "tables", "beta_nmds_coordinates.csv")
  summary_path <- file.path(job_dir, "tables", "beta_nmds_summary.csv")

  out_summary <- tibble::tibble(
    distance = distance_info$label,
    group_var = group_var,
    stress = NA_real_,
    converged = NA,
    message = "NMDS was not run."
  )
  out_coords <- beta_empty_coordinates("NMDS1", "NMDS2", group_var)
  warnings <- character(0)
  figure <- NULL

  nmds <- tryCatch(
    withCallingHandlers(
      vegan::metaMDS(dist_obj, k = 2, trymax = 50, autotransform = FALSE, trace = FALSE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(nmds, "error")) {
    out_summary$message[[1]] <- paste0("NMDS failed: ", conditionMessage(nmds))
    warnings <- c(warnings, out_summary$message[[1]])
  } else {
    pts <- tryCatch(vegan::scores(nmds, display = "sites"), error = function(e) NULL)
    if (!is.null(pts)) {
      pts <- as.matrix(pts)
      if (ncol(pts) < 2) pts <- cbind(pts, 0)
      out_coords <- tibble::tibble(
        SampleID = rownames(pts),
        NMDS1 = as.numeric(pts[, 1]),
        NMDS2 = as.numeric(pts[, 2])
      )
      out_coords <- dplyr::left_join(out_coords, meta[, c("SampleID", group_var), drop = FALSE], by = "SampleID")
    }

    converged <- isTRUE(nmds$converged)
    stress <- suppressWarnings(as.numeric(nmds$stress))
    out_summary <- tibble::tibble(
      distance = distance_info$label,
      group_var = group_var,
      stress = stress,
      converged = converged,
      message = dplyr::case_when(
        !converged ~ "NMDS did not fully converge; interpret the ordination cautiously.",
        is.finite(stress) && stress > 0.2 ~ "NMDS stress is > 0.2; the ordination may be unstable.",
        TRUE ~ "NMDS completed successfully."
      )
    )

    if (!converged) warnings <- c(warnings, "NMDS did not fully converge.")
    if (is.finite(stress) && stress > 0.2) warnings <- c(warnings, "NMDS stress exceeded 0.2 and may be unstable.")

    if (nrow(out_coords) > 0) {
      plot_obj <- beta_build_ordination_plot(
        coords = out_coords,
        group_var = group_var,
        axis1 = "NMDS1",
        axis2 = "NMDS2",
        axis1_pct = NA_real_,
        axis2_pct = NA_real_,
        title = paste0("NMDS (", distance_info$label, " distance)"),
        subtitle = paste0("Stress = ", formatC(stress, format = "f", digits = 3))
      )
      fig_stub <- paste0("beta_nmds_", distance_info$slug)
      saved <- suppressWarnings(
        beta_safe_save_plot(
          plot_obj,
          pdf_path = file.path(job_dir, "figures", paste0(fig_stub, ".pdf")),
          png_path = file.path(job_dir, "figures", paste0(fig_stub, ".png")),
          width = 8.2,
          height = 6.2,
          dpi = 320
        )
      )
      figure <- list(
        png = paste0("figures/", fig_stub, ".png"),
        pdf = paste0("figures/", fig_stub, ".pdf"),
        png_path = saved$png,
        pdf_path = saved$pdf
      )
      warnings <- c(warnings, saved$warnings)
    }
  }

  coord_out <- beta_safe_write_csv(out_coords, coord_path)
  summary_out <- beta_safe_write_csv(out_summary, summary_path)

  list(
    coordinates = out_coords,
    summary = out_summary,
    coordinates_path = coord_out,
    summary_path = summary_out,
    figure = figure,
    warnings = unique(warnings)
  )
}

beta_save_distance_matrix <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  dist_matrix <- as.matrix(dist_obj)
  meta_ord <- beta_order_samples(meta, group_var = group_var)
  dist_matrix <- dist_matrix[meta_ord$SampleID, meta_ord$SampleID, drop = FALSE]
  out_df <- data.frame(SampleID = rownames(dist_matrix), dist_matrix, check.names = FALSE)
  out_path <- beta_safe_write_csv(
    out_df,
    file.path(job_dir, "tables", paste0("beta_distance_matrix_", distance_info$slug, ".csv"))
  )

  list(
    matrix = dist_matrix,
    path = out_path
  )
}

beta_save_heatmap <- function(dist_matrix, meta, group_var, distance_info, job_dir) {
  ordered_meta <- beta_order_samples(meta, group_var = group_var)
  dist_matrix <- dist_matrix[ordered_meta$SampleID, ordered_meta$SampleID, drop = FALSE]

  png_path <- file.path(job_dir, "figures", paste0("beta_distance_heatmap_", distance_info$slug, ".png"))
  pdf_path <- file.path(job_dir, "figures", paste0("beta_distance_heatmap_", distance_info$slug, ".pdf"))
  ensure_dir(dirname(png_path))
  ensure_dir(dirname(pdf_path))

  warnings <- character(0)
  png_saved <- NULL
  pdf_saved <- NULL

  if (requireNamespace("pheatmap", quietly = TRUE)) {
    ann <- data.frame(Group = ordered_meta[[group_var]], row.names = ordered_meta$SampleID, stringsAsFactors = FALSE)
    names(ann) <- group_var

    png_saved <- tryCatch({
      pheatmap::pheatmap(
        dist_matrix,
        annotation_row = ann,
        annotation_col = ann,
        main = paste0(distance_info$label, " distance heatmap"),
        filename = png_path,
        width = 8.8,
        height = max(6.5, nrow(dist_matrix) * 0.24 + 2.5)
      )
      normalizePath(png_path, winslash = "/", mustWork = TRUE)
    }, error = function(e) {
      warnings <<- c(warnings, paste0("Heatmap PNG save failed with pheatmap: ", conditionMessage(e)))
      NULL
    })

    pdf_saved <- tryCatch({
      pheatmap::pheatmap(
        dist_matrix,
        annotation_row = ann,
        annotation_col = ann,
        main = paste0(distance_info$label, " distance heatmap"),
        filename = pdf_path,
        width = 8.8,
        height = max(6.5, nrow(dist_matrix) * 0.24 + 2.5)
      )
      normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
    }, error = function(e) {
      warnings <<- c(warnings, paste0("Heatmap PDF save failed with pheatmap: ", conditionMessage(e)))
      NULL
    })
  }

  if (is.null(png_saved)) {
    label_map <- paste0(ordered_meta$SampleID, " | ", ordered_meta[[group_var]])
    names(label_map) <- ordered_meta$SampleID
    long_df <- as.data.frame(as.table(dist_matrix), stringsAsFactors = FALSE)
    names(long_df) <- c("Sample1", "Sample2", "Distance")
    long_df$Sample1 <- factor(label_map[long_df$Sample1], levels = label_map[rownames(dist_matrix)])
    long_df$Sample2 <- factor(label_map[long_df$Sample2], levels = rev(label_map[colnames(dist_matrix)]))

    plot_obj <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data$Sample1, y = .data$Sample2, fill = .data$Distance)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "#f8fafc", high = "#0f766e") +
      ggplot2::labs(
        title = paste0(distance_info$label, " distance heatmap"),
        subtitle = paste0("Samples are ordered by ", group_var),
        x = "Sample",
        y = "Sample",
        fill = "Distance"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = ggplot2::element_text(size = 7)
      )

    saved <- beta_safe_save_plot(plot_obj, pdf_path = pdf_path, png_path = png_path, width = 9.4, height = 8.2, dpi = 320)
    png_saved <- saved$png
    pdf_saved <- saved$pdf
    warnings <- c(warnings, saved$warnings)
    warnings <- c(warnings, "pheatmap was unavailable or failed; a ggplot heatmap fallback was used.")
  }

  list(
    figure = list(
      png = paste0("figures/beta_distance_heatmap_", distance_info$slug, ".png"),
      pdf = paste0("figures/beta_distance_heatmap_", distance_info$slug, ".pdf"),
      png_path = png_saved,
      pdf_path = pdf_saved
    ),
    warnings = unique(warnings)
  )
}

beta_run_within_between <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  dist_matrix <- as.matrix(dist_obj)
  idx <- which(upper.tri(dist_matrix), arr.ind = TRUE)
  pair_tbl <- tibble::tibble(
    sample_1 = rownames(dist_matrix)[idx[, 1]],
    sample_2 = colnames(dist_matrix)[idx[, 2]],
    group_1 = meta[rownames(dist_matrix)[idx[, 1]], group_var, drop = TRUE],
    group_2 = meta[colnames(dist_matrix)[idx[, 2]], group_var, drop = TRUE],
    distance = as.numeric(dist_matrix[idx]),
    pair_type = ifelse(
      meta[rownames(dist_matrix)[idx[, 1]], group_var, drop = TRUE] ==
        meta[colnames(dist_matrix)[idx[, 2]], group_var, drop = TRUE],
      "within_group",
      "between_group"
    )
  )

  pair_path <- beta_safe_write_csv(pair_tbl, file.path(job_dir, "tables", "beta_within_between_distance.csv"))

  n_within <- sum(pair_tbl$pair_type == "within_group", na.rm = TRUE)
  n_between <- sum(pair_tbl$pair_type == "between_group", na.rm = TRUE)
  within_values <- pair_tbl$distance[pair_tbl$pair_type == "within_group"]
  between_values <- pair_tbl$distance[pair_tbl$pair_type == "between_group"]

  p_value <- NA_real_
  statistic <- NA_real_
  interpretation <- "Within-group versus between-group distance comparison was not tested."

  if (n_within > 0 && n_between > 0) {
    test <- tryCatch(
      stats::wilcox.test(within_values, between_values, exact = FALSE),
      error = function(e) e
    )
    if (!inherits(test, "error")) {
      p_value <- suppressWarnings(as.numeric(test$p.value))
      statistic <- suppressWarnings(as.numeric(test$statistic[[1]]))
      interpretation <- if (is.finite(p_value) && p_value < 0.05) {
        "Within-group and between-group distances differ significantly."
      } else {
        "Within-group and between-group distances did not differ significantly."
      }
    } else {
      interpretation <- paste0("Wilcoxon test failed: ", conditionMessage(test))
    }
  }

  stats_tbl <- tibble::tibble(
    distance = distance_info$label,
    group_var = group_var,
    within_n = n_within,
    between_n = n_between,
    within_median = if (n_within > 0) stats::median(within_values, na.rm = TRUE) else NA_real_,
    between_median = if (n_between > 0) stats::median(between_values, na.rm = TRUE) else NA_real_,
    wilcoxon_statistic = statistic,
    p_value = p_value,
    interpretation = interpretation
  )
  stats_path <- beta_safe_write_csv(stats_tbl, file.path(job_dir, "tables", "beta_within_between_stats.csv"))

  plot_df <- pair_tbl
  plot_df$pair_type <- factor(plot_df$pair_type, levels = c("within_group", "between_group"))
  plot_obj <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$pair_type, y = .data$distance, fill = .data$pair_type)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.55) +
    ggplot2::geom_jitter(width = 0.12, alpha = 0.45, size = 1.4) +
    ggplot2::scale_fill_manual(values = c(within_group = "#2563eb", between_group = "#d97706")) +
    ggplot2::labs(
      title = paste0("Within-group vs between-group distances (", distance_info$label, ")"),
      subtitle = if (is.finite(p_value)) paste0("Wilcoxon p = ", formatC(p_value, digits = 3, format = "f")) else "Wilcoxon test not available",
      x = NULL,
      y = "Pairwise distance",
      fill = NULL
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )

  fig_saved <- beta_safe_save_plot(
    plot_obj,
    pdf_path = file.path(job_dir, "figures", "beta_within_between_boxplot.pdf"),
    png_path = file.path(job_dir, "figures", "beta_within_between_boxplot.png"),
    width = 7.6,
    height = 5.8,
    dpi = 320
  )

  list(
    pairwise = pair_tbl,
    stats = stats_tbl,
    pairwise_path = pair_path,
    stats_path = stats_path,
    figure = list(
      png = "figures/beta_within_between_boxplot.png",
      pdf = "figures/beta_within_between_boxplot.pdf",
      png_path = fig_saved$png,
      pdf_path = fig_saved$pdf
    ),
    warnings = fig_saved$warnings
  )
}

beta_run_anosim <- function(dist_obj, meta, group_var, distance_info, job_dir) {
  group_factor <- factor(meta[[group_var]], levels = unique(as.character(meta[[group_var]])))
  if (nlevels(group_factor) < 2) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = "ANOSIM was skipped because fewer than two groups were available.",
      extra = list(R_statistic = NA_real_, p_value = NA_real_)
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_anosim.csv"))
    return(list(table = out, path = out_path, warnings = "ANOSIM skipped: fewer than two groups."))
  }

  anos <- tryCatch(vegan::anosim(dist_obj, grouping = group_factor, permutations = 999), error = function(e) e)
  if (inherits(anos, "error")) {
    out <- tibble::as_tibble(beta_not_tested_row(
      distance = distance_info$method,
      group_var = group_var,
      interpretation = paste0("ANOSIM failed: ", conditionMessage(anos)),
      extra = list(R_statistic = NA_real_, p_value = NA_real_)
    ))
    out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_anosim.csv"))
    return(list(table = out, path = out_path, warnings = out$interpretation[[1]]))
  }
  p_value <- suppressWarnings(as.numeric(anos$signif))
  r_value <- suppressWarnings(as.numeric(anos$statistic))
  interpretation <- if (is.finite(p_value) && p_value < 0.05) {
    "ANOSIM detected significant between-group separation."
  } else {
    "ANOSIM did not detect statistically significant between-group separation."
  }

  out <- tibble::tibble(
    distance = distance_info$label,
    group_var = group_var,
    R_statistic = r_value,
    p_value = p_value,
    interpretation = interpretation
  )
  out_path <- beta_safe_write_csv(out, file.path(job_dir, "tables", "beta_anosim.csv"))
  list(table = out, path = out_path, warnings = character(0))
}

summarize_beta_for_ai <- function(beta_result, permanova_table = NULL, group_var = NULL) {
  if (!is.list(beta_result)) return(list())

  permanova_tbl <- permanova_table %||% beta_result$permanova
  dispersion_tbl <- beta_result$dispersion %||% data.frame()
  anosim_tbl <- beta_result$anosim %||% data.frame()
  nmds_tbl <- beta_result$nmds_summary %||% data.frame()

  permanova_p <- if (is.data.frame(permanova_tbl) && "p_value" %in% names(permanova_tbl) && nrow(permanova_tbl) > 0) suppressWarnings(as.numeric(permanova_tbl$p_value[[1]])) else NA_real_
  permanova_r2 <- if (is.data.frame(permanova_tbl) && "R2" %in% names(permanova_tbl) && nrow(permanova_tbl) > 0) suppressWarnings(as.numeric(permanova_tbl$R2[[1]])) else NA_real_
  dispersion_p <- if (is.data.frame(dispersion_tbl) && "p_value" %in% names(dispersion_tbl) && nrow(dispersion_tbl) > 0) suppressWarnings(as.numeric(dispersion_tbl$p_value[[1]])) else NA_real_
  anosim_p <- if (is.data.frame(anosim_tbl) && "p_value" %in% names(anosim_tbl) && nrow(anosim_tbl) > 0) suppressWarnings(as.numeric(anosim_tbl$p_value[[1]])) else NA_real_
  anosim_r <- if (is.data.frame(anosim_tbl) && "R_statistic" %in% names(anosim_tbl) && nrow(anosim_tbl) > 0) suppressWarnings(as.numeric(anosim_tbl$R_statistic[[1]])) else NA_real_
  nmds_stress <- if (is.data.frame(nmds_tbl) && "stress" %in% names(nmds_tbl) && nrow(nmds_tbl) > 0) suppressWarnings(as.numeric(nmds_tbl$stress[[1]])) else NA_real_

  reliability_notes <- character(0)
  caution_notes <- unique(c(beta_result$warnings %||% character(0), "Beta diversity associations should not be interpreted as causal effects."))

  if (is.finite(permanova_p) && permanova_p < 0.05 && is.finite(dispersion_p) && dispersion_p >= 0.05) {
    reliability_notes <- c(reliability_notes, "PERMANOVA detected significant group-wise compositional differences without significant dispersion heterogeneity; the community separation is relatively reliable.")
  }
  if (is.finite(permanova_p) && permanova_p < 0.05 && is.finite(dispersion_p) && dispersion_p < 0.05) {
    reliability_notes <- c(reliability_notes, "PERMANOVA was significant, but dispersion heterogeneity was also significant; group separation may be partly driven by unequal within-group dispersion.")
    caution_notes <- c(caution_notes, "PERMANOVA significance may be influenced by unequal within-group dispersion.")
  }
  if (is.finite(permanova_p) && permanova_p >= 0.05) {
    reliability_notes <- c(reliability_notes, "PERMANOVA did not detect statistically significant group-wise community separation.")
  }
  if (is.finite(nmds_stress) && nmds_stress > 0.2) {
    caution_notes <- c(caution_notes, "NMDS stress exceeded 0.2, so the ordination layout may be unstable.")
  }

  caution_notes <- unique(caution_notes)
  reliability_notes <- unique(reliability_notes)

  list(
    analysis_type = "beta_diversity",
    group_variable = group_var %||% beta_result$group_var %||% NA_character_,
    distance_method = beta_result$distance %||% NA_character_,
    distance_label = beta_result$distance_label %||% NA_character_,
    permanova_r2 = permanova_r2,
    permanova_p = permanova_p,
    dispersion_p = dispersion_p,
    anosim_r = anosim_r,
    anosim_p = anosim_p,
    nmds_stress = nmds_stress,
    reliability_notes = reliability_notes,
    caution_notes = caution_notes,
    n_samples_used = beta_result$n_samples %||% NA_integer_,
    group_counts = beta_result$group_counts %||% list()
  )
}

run_beta_analysis <- function(dataset, group_var, job_dir, distance = "bray") {
  if (is.null(dataset)) stop("run_beta_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_beta_analysis(): job_dir not found: ", job_dir, call. = FALSE)

  distance_info <- beta_normalize_distance(distance)
  prepared <- beta_prepare_inputs(dataset, group_var = group_var)
  dist_obj <- beta_compute_distance(prepared$matrix, distance = distance_info$method, dataset = dataset)
  meta <- prepared$metadata[labels(dist_obj), , drop = FALSE]

  warnings_all <- unique(c(distance_info$warnings, prepared$warnings))

  distance_matrix_info <- beta_save_distance_matrix(
    dist_obj,
    meta = meta,
    group_var = group_var,
    distance_info = distance_info,
    job_dir = job_dir
  )
  pcoa_info <- beta_run_pcoa(dist_obj, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)
  permanova_info <- beta_run_permanova(dist_obj, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)
  dispersion_info <- beta_run_dispersion(dist_obj, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)
  nmds_info <- beta_run_nmds(dist_obj, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)
  heatmap_info <- beta_save_heatmap(distance_matrix_info$matrix, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)
  within_between_info <- beta_run_within_between(dist_obj, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)
  anosim_info <- beta_run_anosim(dist_obj, meta = meta, group_var = group_var, distance_info = distance_info, job_dir = job_dir)

  warnings_all <- unique(c(
    warnings_all,
    pcoa_info$warnings %||% character(0),
    permanova_info$warnings %||% character(0),
    dispersion_info$warnings %||% character(0),
    nmds_info$warnings %||% character(0),
    heatmap_info$warnings %||% character(0),
    within_between_info$warnings %||% character(0),
    anosim_info$warnings %||% character(0)
  ))

  beta_result <- list(
    analysis_type = "beta_diversity",
    distance = distance_info$method,
    distance_label = distance_info$label,
    distance_slug = distance_info$slug,
    group_var = group_var,
    n_samples = nrow(meta),
    group_counts = as.list(as.integer(prepared$group_counts)),
    group_count_names = names(prepared$group_counts),
    permanova = permanova_info$table,
    dispersion = dispersion_info$table,
    anosim = anosim_info$table,
    nmds_summary = nmds_info$summary,
    warnings = warnings_all
  )
  names(beta_result$group_counts) <- names(prepared$group_counts)

  summary_obj <- summarize_beta_for_ai(beta_result, permanova_table = permanova_info$table, group_var = group_var)
  summary_path <- beta_safe_write_json(summary_obj, file.path(job_dir, "json", "beta_summary.json"))

  list(
    pcoa_coordinates = pcoa_info$coordinates,
    permanova_table = permanova_info$table,
    permanova = permanova_info$table,
    dispersion_table = dispersion_info$table,
    dispersion = dispersion_info$table,
    nmds_coordinates = nmds_info$coordinates,
    nmds_summary = nmds_info$summary,
    anosim_table = anosim_info$table,
    anosim = anosim_info$table,
    within_between_distance = within_between_info$pairwise,
    within_between_stats = within_between_info$stats,
    distance_matrix_path = distance_matrix_info$path,
    pcoa_coordinates_path = pcoa_info$coordinates_path,
    permanova_path = permanova_info$path,
    dispersion_path = dispersion_info$path,
    nmds_coordinates_path = nmds_info$coordinates_path,
    nmds_summary_path = nmds_info$summary_path,
    anosim_path = anosim_info$path,
    within_between_distance_path = within_between_info$pairwise_path,
    within_between_stats_path = within_between_info$stats_path,
    summary_json = summary_path,
    figures = list(
      pcoa = pcoa_info$figure,
      nmds = nmds_info$figure,
      heatmap = heatmap_info$figure,
      within_between = within_between_info$figure
    ),
    figure_paths = pcoa_info$figure,
    distance = distance_info$method,
    distance_label = distance_info$label,
    distance_slug = distance_info$slug,
    group_var = group_var,
    warnings = warnings_all
  )
}
