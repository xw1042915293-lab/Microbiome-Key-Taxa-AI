# Beta diversity analysis (Phase 2).
# Outputs:
# - tables/beta_pcoa_coordinates.csv
# - tables/beta_permanova.csv
# - figures/beta_pcoa_bray.pdf + .png

run_beta_analysis <- function(dataset, group_var, job_dir, distance = "bray") {
  if (is.null(dataset)) stop("run_beta_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(group_var, "group_var")
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_beta_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(distance, "distance")
  if (!distance %in% c("bray")) stop("run_beta_analysis(): only 'bray' is supported in Phase 2.", call. = FALSE)

  samp <- dataset$sample_table
  if (is.null(samp) || !is.data.frame(samp)) stop("run_beta_analysis(): dataset$sample_table missing.", call. = FALSE)
  if (!group_var %in% names(samp)) stop("run_beta_analysis(): group_var not found in sample_table: ", group_var, call. = FALSE)

  otu <- dataset$otu_table
  if (is.null(otu) || !is.data.frame(otu)) stop("run_beta_analysis(): dataset$otu_table missing.", call. = FALSE)

  # vegan expects samples as rows, features as columns
  x <- t(as.matrix(otu))
  if (!is.numeric(x)) storage.mode(x) <- "double"

  dist_obj <- vegan::vegdist(x, method = "bray")

  # PCoA via cmdscale (2D)
  ord <- stats::cmdscale(dist_obj, k = 2, eig = TRUE)
  coords <- as.data.frame(ord$points)
  colnames(coords) <- c("PCo1", "PCo2")
  coords$SampleID <- rownames(coords)
  coords <- tibble::as_tibble(coords)
  coords <- dplyr::left_join(coords, samp[, c("SampleID", group_var), drop = FALSE], by = "SampleID")

  # Explained variance
  eig <- ord$eig
  var_expl <- eig / sum(eig[eig > 0])
  p1 <- ifelse(length(var_expl) >= 1, var_expl[[1]], NA_real_)
  p2 <- ifelse(length(var_expl) >= 2, var_expl[[2]], NA_real_)

  out_coords <- file.path(job_dir, "tables", "beta_pcoa_coordinates.csv")
  ensure_dir(dirname(out_coords))
  readr::write_csv(coords, out_coords)

  # PERMANOVA (adonis2)
  meta <- samp[, c("SampleID", group_var), drop = FALSE]
  meta[[group_var]] <- as.factor(meta[[group_var]])
  rownames(meta) <- meta$SampleID
  meta <- meta[labels(dist_obj), , drop = FALSE]

  perm <- vegan::adonis2(dist_obj ~ meta[[group_var]], permutations = 999)
  perm_tbl <- as.data.frame(perm)
  perm_tbl$term <- rownames(perm_tbl)
  perm_tbl <- tibble::as_tibble(perm_tbl)
  perm_tbl <- dplyr::select(perm_tbl, term, dplyr::everything())

  out_perm <- file.path(job_dir, "tables", "beta_permanova.csv")
  readr::write_csv(perm_tbl, out_perm)

  # Plot PCoA
  p <- ggplot2::ggplot(coords, ggplot2::aes(x = PCo1, y = PCo2, color = .data[[group_var]])) +
    ggplot2::geom_point(size = 3, alpha = 0.9) +
    ggplot2::labs(
      title = "Beta diversity PCoA (Bray-Curtis)",
      x = sprintf("PCo1 (%.1f%%)", 100 * p1),
      y = sprintf("PCo2 (%.1f%%)", 100 * p2),
      color = group_var
    ) +
    ggplot2::theme_minimal(base_size = 12)

  fig_pdf <- file.path(job_dir, "figures", "beta_pcoa_bray.pdf")
  fig_png <- file.path(job_dir, "figures", "beta_pcoa_bray.png")
  fig_paths <- save_plot_pdf_png(p, fig_pdf, fig_png)

  list(
    pcoa_coordinates_path = normalizePath(out_coords, winslash = "/", mustWork = TRUE),
    permanova_path = normalizePath(out_perm, winslash = "/", mustWork = TRUE),
    figure_paths = fig_paths
  )
}
