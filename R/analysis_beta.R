# Publication-oriented Beta diversity analysis.
# Outputs are isolated under job_dir/beta/.

beta_plot_view_spec <- function() {
  data.frame(
    view = c("points", "ellipse", "hull", "centroid", "ellipse_centroid", "dispersion"),
    label = c(
      "PCoA 样本点",
      "PCoA + 95% 置信椭圆",
      "PCoA + 分组凸包",
      "PCoA + 组中心连线",
      "PCoA + 置信椭圆 + 组中心",
      "PERMDISP 组内离散度诊断"
    ),
    stringsAsFactors = FALSE
  )
}

beta_output_path <- function(job_dir, kind = c("tables", "figures"), ..., legacy_filename = NULL, existing = FALSE) {
  kind <- match.arg(kind)
  assert_non_empty_string(job_dir, "job_dir")
  preferred <- file.path(job_dir, "beta", kind, ...)
  if (isTRUE(existing) && !file.exists(preferred) && !is.null(legacy_filename)) {
    legacy <- file.path(job_dir, kind, legacy_filename)
    if (file.exists(legacy)) return(legacy)
  }
  preferred
}

beta_figure_path <- function(job_dir, view, extension = "png", existing = FALSE) {
  if (!view %in% beta_plot_view_spec()$view) stop("Unsupported Beta view: ", view, call. = FALSE)
  if (identical(view, "dispersion")) {
    return(beta_output_path(
      job_dir, "figures", "dispersion", paste0("beta_dispersion.", extension),
      existing = existing
    ))
  }
  legacy <- if (identical(view, "points")) paste0("beta_pcoa_bray.", extension) else NULL
  beta_output_path(
    job_dir, "figures", "pcoa", paste0("pcoa_", view, ".", extension),
    legacy_filename = legacy, existing = existing
  )
}

beta_pcoa_hulls <- function(coords) {
  groups <- split(coords, coords$group, drop = TRUE)
  hulls <- lapply(groups, function(df) {
    if (nrow(df) < 3 || length(unique(df$PCo1)) < 2 || length(unique(df$PCo2)) < 2) return(NULL)
    df[grDevices::chull(df$PCo1, df$PCo2), , drop = FALSE]
  })
  dplyr::bind_rows(hulls)
}

beta_pcoa_centroids <- function(coords) {
  dplyr::summarise(
    dplyr::group_by(coords, group),
    centroid_x = mean(PCo1, na.rm = TRUE),
    centroid_y = mean(PCo2, na.rm = TRUE),
    .groups = "drop"
  )
}

beta_stats_caption <- function(permanova_tbl, dispersion_tbl) {
  model <- permanova_tbl[!permanova_tbl$term %in% c("Residual", "Total"), , drop = FALSE]
  r2 <- if (nrow(model) && "R2" %in% names(model)) model$R2[[1]] else NA_real_
  p_perm <- if (nrow(model) && "Pr(>F)" %in% names(model)) model[["Pr(>F)"]][[1]] else NA_real_
  p_disp <- if (is.data.frame(dispersion_tbl) && nrow(dispersion_tbl) && "p_value" %in% names(dispersion_tbl)) dispersion_tbl$p_value[[1]] else NA_real_
  fmt_p <- function(x) if (!is.finite(x)) "NA" else if (x < 0.001) "<0.001" else formatC(x, digits = 3, format = "f")
  paste0(
    "PERMANOVA: R² = ", if (is.finite(r2)) formatC(r2, digits = 3, format = "f") else "NA",
    ", p ", if (is.finite(p_perm) && p_perm < 0.001) "< 0.001" else paste0("= ", fmt_p(p_perm)),
    " (999 permutations); PERMDISP: p ",
    if (is.finite(p_disp) && p_disp < 0.001) "< 0.001" else paste0("= ", fmt_p(p_disp))
  )
}

build_beta_pcoa_plot <- function(coords, group_var, distance, p1, p2, permanova_tbl, dispersion_tbl, view = "ellipse_centroid") {
  if (!view %in% setdiff(beta_plot_view_spec()$view, "dispersion")) stop("Unsupported PCoA view: ", view, call. = FALSE)
  coords <- as.data.frame(coords)
  coords$group <- droplevels(as.factor(coords[[group_var]]))
  n_groups <- nlevels(coords$group)
  palette_values <- get_app_palette(n_groups)
  names(palette_values) <- levels(coords$group)
  shape_values <- rep(c(21, 22, 24, 23, 25), length.out = n_groups)
  names(shape_values) <- levels(coords$group)
  distance_label <- switch(distance, bray = "Bray–Curtis", jaccard = "Jaccard", distance)
  caption <- beta_stats_caption(permanova_tbl, dispersion_tbl)

  p <- ggplot2::ggplot(coords, ggplot2::aes(x = PCo1, y = PCo2, color = group, fill = group, shape = group))

  if (view %in% c("ellipse", "ellipse_centroid")) {
    ellipse_groups <- names(which(table(coords$group) >= 3))
    ellipse_data <- coords[coords$group %in% ellipse_groups, , drop = FALSE]
    if (nrow(ellipse_data) > 0) {
      p <- p + ggplot2::stat_ellipse(
        data = ellipse_data,
        ggplot2::aes(x = PCo1, y = PCo2, color = group, fill = group, group = group),
        type = "norm", level = 0.95, geom = "polygon",
        alpha = 0.13, linewidth = 0.65, show.legend = FALSE, inherit.aes = FALSE
      )
    }
  }

  if (identical(view, "hull")) {
    hulls <- beta_pcoa_hulls(coords)
    if (nrow(hulls) > 0) {
      p <- p + ggplot2::geom_polygon(
        data = hulls,
        ggplot2::aes(x = PCo1, y = PCo2, color = group, fill = group, group = group),
        alpha = 0.13, linewidth = 0.65, show.legend = FALSE, inherit.aes = FALSE
      )
    }
  }

  if (view %in% c("centroid", "ellipse_centroid")) {
    centroids <- beta_pcoa_centroids(coords)
    coords_with_centroid <- dplyr::left_join(coords, centroids, by = "group")
    p <- p +
      ggplot2::geom_segment(
        data = coords_with_centroid,
        ggplot2::aes(x = PCo1, y = PCo2, xend = centroid_x, yend = centroid_y, color = group),
        linewidth = 0.35, alpha = 0.28, show.legend = FALSE, inherit.aes = FALSE
      ) +
      ggplot2::geom_point(
        data = centroids,
        ggplot2::aes(x = centroid_x, y = centroid_y, color = group, fill = group, shape = group),
        size = 5, stroke = 1.2, show.legend = FALSE, inherit.aes = FALSE
      )
  }

  p +
    ggplot2::geom_hline(yintercept = 0, color = "#AAB4BF", linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = "#AAB4BF", linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_point(size = 3.2, alpha = 0.9, stroke = 0.7) +
    ggplot2::scale_color_manual(values = palette_values, name = group_var) +
    ggplot2::scale_fill_manual(values = palette_values, name = group_var) +
    ggplot2::scale_shape_manual(values = shape_values, name = group_var) +
    ggplot2::labs(
      title = paste0(distance_label, " PCoA"),
      subtitle = paste0("n = ", nrow(coords), "; grouping variable: ", group_var),
      x = sprintf("PCoA1 (%.1f%%)", 100 * p1),
      y = sprintf("PCoA2 (%.1f%%)", 100 * p2),
      caption = caption
    ) +
    ggplot2::coord_equal() +
    get_report_plot_theme(base_size = 12) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "#E7EDF3", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      legend.key.height = grid::unit(0.55, "cm"),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#566573"),
      plot.caption = ggplot2::element_text(size = 9.5, color = "#34495E", hjust = 0),
      plot.caption.position = "plot"
    )
}

build_beta_dispersion_plot <- function(distance_tbl, group_var, dispersion_tbl) {
  df <- as.data.frame(distance_tbl)
  df$group <- stats::reorder(as.factor(df[[group_var]]), df$distance_to_centroid, FUN = stats::median, na.rm = TRUE)
  n_groups <- nlevels(df$group)
  palette_values <- get_app_palette(n_groups)
  names(palette_values) <- levels(df$group)
  p_value <- if (nrow(dispersion_tbl) && "p_value" %in% names(dispersion_tbl)) dispersion_tbl$p_value[[1]] else NA_real_
  subtitle <- if (is.finite(p_value)) {
    paste0("PERMDISP permutation test: p ", if (p_value < 0.001) "< 0.001" else paste0("= ", formatC(p_value, digits = 3, format = "f")))
  } else {
    "PERMDISP permutation test unavailable"
  }
  horizontal <- n_groups > 8

  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = distance_to_centroid, fill = group, color = group)) +
    ggplot2::geom_violin(trim = FALSE, scale = "width", alpha = 0.38, linewidth = 0.45) +
    ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.85, linewidth = 0.5) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.1, height = 0),
      shape = 21, size = 2, stroke = 0.3, alpha = 0.65
    ) +
    ggplot2::scale_fill_manual(values = palette_values) +
    ggplot2::scale_color_manual(values = palette_values) +
    ggplot2::labs(
      title = "Multivariate dispersion",
      subtitle = subtitle,
      x = group_var,
      y = "Distance to group median"
    ) +
    get_report_plot_theme(base_size = 12) +
    ggplot2::theme(legend.position = "none")

  if (horizontal) {
    p <- p + ggplot2::coord_flip() + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }
  p
}

run_beta_analysis <- function(dataset, group_var, job_dir, distance = "bray") {
  if (is.null(dataset)) stop("run_beta_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_beta_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(distance, "distance")
  if (!distance %in% c("bray", "jaccard")) stop("run_beta_analysis(): supported distances are 'bray' and 'jaccard'.", call. = FALSE)

  samp <- dataset$sample_table
  if (is.null(samp) || !is.data.frame(samp)) stop("run_beta_analysis(): dataset$sample_table missing.", call. = FALSE)
  if (!group_var %in% names(samp)) stop("run_beta_analysis(): group_var not found in sample_table: ", group_var, call. = FALSE)
  otu <- dataset$otu_table
  if (is.null(otu) || !is.data.frame(otu)) stop("run_beta_analysis(): dataset$otu_table missing.", call. = FALSE)

  x <- t(as.matrix(otu))
  if (!is.numeric(x)) storage.mode(x) <- "double"
  dist_obj <- vegan::vegdist(x, method = distance, binary = identical(distance, "jaccard"))

  ord <- stats::cmdscale(dist_obj, k = 2, eig = TRUE, add = TRUE)
  coords <- as.data.frame(ord$points)
  colnames(coords) <- c("PCo1", "PCo2")
  coords$SampleID <- rownames(coords)
  coords <- dplyr::left_join(tibble::as_tibble(coords), samp[, c("SampleID", group_var), drop = FALSE], by = "SampleID")
  coords <- coords[!is.na(coords[[group_var]]), , drop = FALSE]

  eig <- ord$eig
  var_expl <- eig / sum(eig[eig > 0])
  p1 <- if (length(var_expl) >= 1) var_expl[[1]] else NA_real_
  p2 <- if (length(var_expl) >= 2) var_expl[[2]] else NA_real_

  out_coords <- beta_output_path(job_dir, "tables", "beta_pcoa_coordinates.csv")
  ensure_dir(dirname(out_coords))
  readr::write_csv(coords, out_coords)

  meta <- samp[, c("SampleID", group_var), drop = FALSE]
  meta[[group_var]] <- droplevels(as.factor(meta[[group_var]]))
  rownames(meta) <- meta$SampleID
  meta <- meta[labels(dist_obj), , drop = FALSE]

  set.seed(20260711)
  perm <- vegan::adonis2(dist_obj ~ group, data = data.frame(group = meta[[group_var]]), permutations = 999)
  perm_tbl <- as.data.frame(perm)
  perm_tbl$term <- rownames(perm_tbl)
  perm_tbl$permutations <- 999L
  perm_tbl <- dplyr::select(tibble::as_tibble(perm_tbl), term, dplyr::everything())
  out_perm <- beta_output_path(job_dir, "tables", "beta_permanova.csv")
  readr::write_csv(perm_tbl, out_perm)

  dispersion <- tryCatch({
    bd <- vegan::betadisper(dist_obj, meta[[group_var]], type = "median", bias.adjust = TRUE)
    set.seed(20260711)
    bd_perm <- vegan::permutest(bd, permutations = 999)
    tab <- as.data.frame(bd_perm$tab)
    stats_tbl <- tibble::tibble(
      term = "Groups",
      df = tab$Df[[1]],
      sum_sq = tab$`Sum Sq`[[1]],
      mean_sq = tab$`Mean Sq`[[1]],
      f_value = tab$F[[1]],
      p_value = tab$`Pr(>F)`[[1]],
      permutations = 999L
    )
    distance_tbl <- tibble::tibble(
      SampleID = names(bd$distances),
      distance_to_centroid = as.numeric(bd$distances)
    )
    distance_tbl <- dplyr::left_join(distance_tbl, samp[, c("SampleID", group_var), drop = FALSE], by = "SampleID")
    list(stats = stats_tbl, distances = distance_tbl)
  }, error = function(e) {
    list(
      stats = tibble::tibble(
        term = "Groups", df = NA_real_, sum_sq = NA_real_, mean_sq = NA_real_,
        f_value = NA_real_, p_value = NA_real_, permutations = 999L,
        note = conditionMessage(e)
      ),
      distances = tibble::tibble(SampleID = character(0), distance_to_centroid = numeric(0))
    )
  })

  out_disp <- beta_output_path(job_dir, "tables", "beta_dispersion.csv")
  out_disp_dist <- beta_output_path(job_dir, "tables", "beta_dispersion_distances.csv")
  readr::write_csv(dispersion$stats, out_disp)
  readr::write_csv(dispersion$distances, out_disp_dist)

  pcoa_paths <- list()
  for (view in setdiff(beta_plot_view_spec()$view, "dispersion")) {
    plot <- build_beta_pcoa_plot(coords, group_var, distance, p1, p2, perm_tbl, dispersion$stats, view = view)
    pcoa_paths[[view]] <- save_plot_pdf_png(
      plot,
      beta_figure_path(job_dir, view, "pdf"),
      beta_figure_path(job_dir, view, "png"),
      width = 8.8, height = 6.8, dpi = 300
    )
  }

  dispersion_paths <- NULL
  if (nrow(dispersion$distances) > 0) {
    dispersion_plot <- build_beta_dispersion_plot(dispersion$distances, group_var, dispersion$stats)
    n_groups <- length(unique(dispersion$distances[[group_var]]))
    plot_height <- if (n_groups > 8) max(5, 0.35 * n_groups) else 5
    dispersion_paths <- save_plot_pdf_png(
      dispersion_plot,
      beta_figure_path(job_dir, "dispersion", "pdf"),
      beta_figure_path(job_dir, "dispersion", "png"),
      width = 8, height = plot_height, dpi = 300
    )
  }

  list(
    pcoa_coordinates_path = normalizePath(out_coords, winslash = "/", mustWork = TRUE),
    permanova_path = normalizePath(out_perm, winslash = "/", mustWork = TRUE),
    dispersion_path = normalizePath(out_disp, winslash = "/", mustWork = TRUE),
    dispersion_distances_path = normalizePath(out_disp_dist, winslash = "/", mustWork = TRUE),
    figure_paths = pcoa_paths$ellipse_centroid %||% pcoa_paths$points,
    pcoa_figure_paths = pcoa_paths,
    dispersion_figure_paths = dispersion_paths,
    variance_explained = c(PCoA1 = p1, PCoA2 = p2),
    distance = distance
  )
}
