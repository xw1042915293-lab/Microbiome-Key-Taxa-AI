# Spearman co-occurrence network analysis (Phase 6).
# Outputs:
# - tables/network_nodes.csv (full node table)
# - tables/network_edges.csv (full threshold-passed edge table)
# - json/network_summary.json
# - figures/network_plot.png + .pdf (alias to overview)
# - figures/network_plot_overview.png + .pdf
# - figures/network_plot_labelled.png + .pdf
# - figures/network_degree_barplot.png + .pdf

if (!exists("clean_taxon_label", mode = "function")) {
  clean_taxon_label <- function(x) {
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
}

.short_node_label <- function(x, max_chars = 40) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (nchar(s, type = "chars") <= max_chars) return(s)
    paste0(substr(s, 1, max_chars - 3), "...")
  }, character(1))
}

.network_taxonomy_levels <- c("FeatureID", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

.extract_taxonomy_level <- function(x, level = "Phylum") {
  idx <- match(level, .network_taxonomy_levels)
  if (is.na(idx)) stop(".extract_taxonomy_level(): unsupported level: ", level, call. = FALSE)
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    parts <- trimws(strsplit(s, "\\|", fixed = FALSE)[[1]])
    if (length(parts) < idx) return(NA_character_)
    out <- parts[[idx]]
    out <- trimws(out)
    if (!nzchar(out)) NA_character_ else out
  }, character(1))
}

.is_missing_taxonomy <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | !nzchar(x) | grepl("unknown|unclassified|uncultured|ambiguous", x, ignore.case = TRUE)
}

prepare_network_matrix <- function(dataset, tax_level = "Genus") {
  if (is.null(dataset)) stop("prepare_network_matrix(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(tax_level, "tax_level")

  dataset$cal_abund()
  if (is.null(dataset$taxa_abund) || !tax_level %in% names(dataset$taxa_abund)) {
    stop(
      "prepare_network_matrix(): tax_level '", tax_level, "' not available. Choose from: ",
      paste(names(dataset$taxa_abund), collapse = ", "),
      call. = FALSE
    )
  }

  abund <- as.data.frame(dataset$taxa_abund[[tax_level]])
  if (!is.data.frame(abund) || nrow(abund) < 1 || ncol(abund) < 1) {
    stop("prepare_network_matrix(): invalid abundance matrix for tax_level ", tax_level, call. = FALSE)
  }

  mat <- t(as.matrix(abund))
  storage.mode(mat) <- "double"
  mat <- as.data.frame(mat, check.names = FALSE)
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  if (nrow(mat) < 3) stop("prepare_network_matrix(): need at least 3 samples.", call. = FALSE)
  if (ncol(mat) < 1) stop("prepare_network_matrix(): need at least 1 taxon feature.", call. = FALSE)

  as.matrix(mat)
}

build_spearman_network <- function(abund_matrix, rho_cutoff = 0.6, p_cutoff = 0.05) {
  if (!is.matrix(abund_matrix) && !is.data.frame(abund_matrix)) {
    stop("build_spearman_network(): abund_matrix must be a matrix/data.frame.", call. = FALSE)
  }
  abund_matrix <- as.matrix(abund_matrix)
  storage.mode(abund_matrix) <- "double"
  if (nrow(abund_matrix) < 3 || ncol(abund_matrix) < 1) {
    stop("build_spearman_network(): abund_matrix must have >=3 samples and >=1 taxon.", call. = FALSE)
  }
  if (!is.numeric(rho_cutoff) || length(rho_cutoff) != 1 || is.na(rho_cutoff) || rho_cutoff <= 0 || rho_cutoff > 1) {
    stop("build_spearman_network(): rho_cutoff must be in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(p_cutoff) || length(p_cutoff) != 1 || is.na(p_cutoff) || p_cutoff <= 0 || p_cutoff > 1) {
    stop("build_spearman_network(): p_cutoff must be in (0, 1].", call. = FALSE)
  }

  taxa <- colnames(abund_matrix)
  if (is.null(taxa) || any(!nzchar(taxa))) {
    taxa <- paste0("Taxon_", seq_len(ncol(abund_matrix)))
    colnames(abund_matrix) <- taxa
  }

  if (ncol(abund_matrix) == 1) {
    edge_table <- data.frame(
      source = character(),
      target = character(),
      rho = numeric(),
      p_value = numeric(),
      fdr = numeric(),
      abs_rho = numeric(),
      sign = character(),
      stringsAsFactors = FALSE
    )
  } else {
    rho_mat <- suppressWarnings(stats::cor(abund_matrix, method = "spearman", use = "pairwise.complete.obs"))
    upper_mask <- upper.tri(rho_mat, diag = FALSE)
    rho_vec <- rho_mat[upper_mask]
    rm(rho_mat)

    valid <- is.finite(rho_vec)
    rho_valid <- rho_vec[valid]
    sample_n <- nrow(abund_matrix)
    denom <- pmax(1 - rho_valid^2, .Machine$double.eps)
    t_stat <- rho_valid * sqrt((sample_n - 2) / denom)
    p_valid <- 2 * stats::pt(abs(t_stat), df = sample_n - 2, lower.tail = FALSE)

    p_vec <- rep(NA_real_, length(rho_vec))
    p_vec[valid] <- p_valid
    fdr_vec <- rep(NA_real_, length(rho_vec))
    if (any(valid)) {
      fdr_vec[valid] <- stats::p.adjust(p_valid, method = "fdr")
    }

    abs_rho_vec <- abs(rho_vec)
    keep <- valid & is.finite(abs_rho_vec) & abs_rho_vec >= rho_cutoff & is.finite(fdr_vec) & fdr_vec < p_cutoff

    if (!any(keep)) {
      edge_table <- data.frame(
        source = character(),
        target = character(),
        rho = numeric(),
        p_value = numeric(),
        fdr = numeric(),
        abs_rho = numeric(),
        sign = character(),
        stringsAsFactors = FALSE
      )
    } else {
      upper_pos <- which(upper_mask, arr.ind = TRUE)
      keep_pos <- upper_pos[keep, , drop = FALSE]
      edge_table <- data.frame(
        source = taxa[keep_pos[, 1]],
        target = taxa[keep_pos[, 2]],
        rho = as.numeric(rho_vec[keep]),
        p_value = as.numeric(p_vec[keep]),
        fdr = as.numeric(fdr_vec[keep]),
        abs_rho = as.numeric(abs_rho_vec[keep]),
        sign = ifelse(rho_vec[keep] >= 0, "positive", "negative"),
        stringsAsFactors = FALSE
      )
      edge_table <- edge_table[order(edge_table$fdr, -edge_table$abs_rho), , drop = FALSE]
      rownames(edge_table) <- NULL
    }
  }

  nodes <- data.frame(name = taxa, stringsAsFactors = FALSE)
  graph <- igraph::graph_from_data_frame(
    d = if (nrow(edge_table) == 0) data.frame(from = character(), to = character(), stringsAsFactors = FALSE) else edge_table,
    directed = FALSE,
    vertices = nodes
  )

  list(
    graph = graph,
    edge_table = edge_table
  )
}

calculate_network_centrality <- function(graph) {
  if (!inherits(graph, "igraph")) stop("calculate_network_centrality(): graph must be an igraph object.", call. = FALSE)
  if (igraph::vcount(graph) == 0) {
    return(data.frame(
      name = character(),
      degree = numeric(),
      betweenness = numeric(),
      closeness = numeric(),
      eigenvector = numeric(),
      component = integer(),
      module = integer(),
      stringsAsFactors = FALSE
    ))
  }

  degree <- as.numeric(igraph::degree(graph, mode = "all"))
  betweenness <- as.numeric(igraph::betweenness(graph, directed = FALSE, normalized = TRUE))
  closeness <- suppressWarnings(as.numeric(igraph::closeness(graph, normalized = TRUE)))
  closeness[!is.finite(closeness)] <- 0

  ev <- tryCatch(
    igraph::eigen_centrality(graph, directed = FALSE)$vector,
    error = function(e) rep(0, igraph::vcount(graph))
  )
  ev <- as.numeric(ev)
  ev[!is.finite(ev)] <- 0

  comp <- igraph::components(graph)$membership
  module <- if (igraph::ecount(graph) < 1L) {
    seq_len(igraph::vcount(graph))
  } else {
    tryCatch(
      as.integer(igraph::membership(igraph::cluster_louvain(
        graph,
        weights = if ("abs_rho" %in% igraph::edge_attr_names(graph)) igraph::E(graph)$abs_rho else NULL
      ))),
      error = function(e) as.integer(comp)
    )
  }

  data.frame(
    name = igraph::V(graph)$name,
    degree = degree,
    betweenness = betweenness,
    closeness = closeness,
    eigenvector = ev,
    component = as.integer(comp),
    module = as.integer(module),
    stringsAsFactors = FALSE
  )
}

augment_network_node_table <- function(node_table) {
  if (!is.data.frame(node_table)) stop("augment_network_node_table(): node_table must be a data.frame.", call. = FALSE)
  if (nrow(node_table) < 1) return(node_table)

  out <- node_table
  .paper_taxon_label <- function(value) {
    parts <- trimws(strsplit(as.character(value), "\\|")[[1L]])
    if (!length(parts)) return(as.character(value))
    last <- parts[length(parts)]
    if (!.is_missing_taxonomy(last)) return(last)
    rank_prefix <- c("k", "p", "c", "o", "f", "g", "s")
    taxonomy <- if (length(parts) > 1L) parts[-1L] else parts
    for (i in rev(seq_along(taxonomy))) {
      if (!.is_missing_taxonomy(taxonomy[i])) {
        prefix <- rank_prefix[min(i, length(rank_prefix))]
        return(paste0("Unclassified_", prefix, "__", taxonomy[i]))
      }
    }
    parts[1L]
  }
  out$display_taxon <- vapply(out$name, .paper_taxon_label, character(1))
  out$display_taxon_short <- .short_node_label(out$display_taxon, max_chars = 36)
  out$phylum <- .extract_taxonomy_level(out$name, "Phylum")
  out$cluster_label <- paste0("Cluster ", out$component %||% NA_integer_)
  out$module_label <- paste0("Module ", out$module %||% out$component %||% NA_integer_)
  out$color_group <- ifelse(.is_missing_taxonomy(out$phylum), out$cluster_label, out$phylum)

  ord <- order(out$degree, out$betweenness, decreasing = TRUE, na.last = TRUE)
  rank_vec <- integer(nrow(out))
  rank_vec[ord] <- seq_len(nrow(out))
  out$hub_rank <- rank_vec
  out
}

select_core_network <- function(node_table, edge_table, top_n_nodes = 30, top_n_labels = 10, top_n_edges = 100) {
  if (!is.data.frame(node_table)) stop("select_core_network(): node_table must be a data.frame.", call. = FALSE)
  if (!is.data.frame(edge_table)) stop("select_core_network(): edge_table must be a data.frame.", call. = FALSE)

  if (nrow(node_table) < 1) {
    return(list(nodes = node_table, edges = edge_table, labelled_nodes = node_table))
  }

  connected <- node_table[is.finite(node_table$degree) & node_table$degree > 0, , drop = FALSE]
  if (!nrow(connected)) connected <- node_table
  ord <- order(connected$degree, connected$betweenness, connected$eigenvector, decreasing = TRUE, na.last = TRUE)
  connected <- connected[ord, , drop = FALSE]
  module_col <- if ("module" %in% names(connected)) "module" else "component"
  module_ids <- unique(connected[[module_col]])
  per_module <- max(1L, floor(top_n_nodes / max(1L, length(module_ids))))
  balanced <- unlist(lapply(module_ids, function(module_id) {
    which(connected[[module_col]] == module_id)[seq_len(min(per_module, sum(connected[[module_col]] == module_id)))]
  }), use.names = FALSE)
  fill <- setdiff(seq_len(nrow(connected)), balanced)
  selected <- utils::head(c(balanced, fill), min(top_n_nodes, nrow(connected)))
  core_nodes <- connected[selected, , drop = FALSE]
  core_nodes <- core_nodes[order(core_nodes$degree, core_nodes$betweenness, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  core_names <- core_nodes$name

  core_edges <- edge_table[edge_table$source %in% core_names & edge_table$target %in% core_names, , drop = FALSE]
  if (nrow(core_edges) > top_n_edges) {
    core_edges <- core_edges[order(core_edges$abs_rho, core_edges$fdr, decreasing = c(TRUE, FALSE), na.last = TRUE), , drop = FALSE]
    core_edges <- utils::head(core_edges, top_n_edges)
  }

  labelled_nodes <- utils::head(core_nodes, top_n_labels)
  list(nodes = core_nodes, edges = core_edges, labelled_nodes = labelled_nodes)
}

compute_core_layout <- function(node_table, edge_table) {
  if (!is.data.frame(node_table)) stop("compute_core_layout(): node_table must be a data.frame.", call. = FALSE)
  if (!is.data.frame(edge_table)) stop("compute_core_layout(): edge_table must be a data.frame.", call. = FALSE)

  if (nrow(node_table) < 1) {
    return(list(nodes = node_table, edges = edge_table))
  }

  graph <- igraph::graph_from_data_frame(
    d = if (nrow(edge_table) == 0) data.frame(from = character(), to = character(), stringsAsFactors = FALSE) else edge_table[, c("source", "target"), drop = FALSE],
    directed = FALSE,
    vertices = node_table[, "name", drop = FALSE]
  )

  layout <- if (igraph::vcount(graph) <= 1) {
    matrix(c(0, 0), ncol = 2)
  } else if (igraph::ecount(graph) == 0) {
    igraph::layout_in_circle(graph)
  } else {
    set.seed(42)
    igraph::layout_with_fr(graph)
  }

  node_pos <- data.frame(
    name = igraph::V(graph)$name,
    x = layout[, 1],
    y = layout[, 2],
    stringsAsFactors = FALSE
  )
  node_plot <- merge(node_table, node_pos, by = "name", all.x = TRUE, sort = FALSE)

  if (nrow(edge_table) < 1) {
    edge_plot <- edge_table
  } else {
    edge_plot <- merge(edge_table, node_pos, by.x = "source", by.y = "name", all.x = TRUE, sort = FALSE)
    names(edge_plot)[names(edge_plot) %in% c("x", "y")] <- c("x_source", "y_source")
    edge_plot <- merge(edge_plot, node_pos, by.x = "target", by.y = "name", all.x = TRUE, sort = FALSE)
    names(edge_plot)[names(edge_plot) %in% c("x", "y")] <- c("x_target", "y_target")
  }

  list(nodes = node_plot, edges = edge_plot)
}

make_empty_network_plot <- function(title, subtitle) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = 0, y = 0.1, label = title, size = 6, fontface = "bold", color = "#0f172a") +
    ggplot2::annotate("text", x = 0, y = -0.1, label = subtitle, size = 4.3, color = "#475569") +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1)
}

make_core_network_plot <- function(layout_data, label_nodes = FALSE, title = "Core co-occurrence network") {
  nodes <- layout_data$nodes
  edges <- layout_data$edges

  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("make_core_network_plot(): ggrepel is required. Please install.packages('ggrepel').", call. = FALSE)
  }

  if (!is.data.frame(nodes) || nrow(nodes) < 1) {
    return(make_empty_network_plot(title, "No taxa available for network plotting."))
  }
  if (!is.data.frame(edges)) {
    edges <- data.frame()
  }

  color_levels <- unique(nodes$color_group)
  color_levels <- color_levels[!is.na(color_levels) & nzchar(color_levels)]
  if (length(color_levels) < 1) color_levels <- "Cluster 1"
  palette <- stats::setNames(grDevices::hcl.colors(length(color_levels), "Dark 3"), color_levels)

  p <- ggplot2::ggplot()

  if (nrow(edges) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(
          x = .data$x_source,
          y = .data$y_source,
          xend = .data$x_target,
          yend = .data$y_target,
          color = .data$sign,
          linewidth = .data$abs_rho
        ),
        alpha = 0.55,
        lineend = "round"
      ) +
      ggplot2::scale_color_manual(values = c(positive = "#D1495B", negative = "#00798C"), drop = FALSE)
  }

  p <- p +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$degree, fill = .data$color_group),
      shape = 21,
      color = "#ffffff",
      stroke = 0.8,
      alpha = 0.95
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::scale_size_continuous(range = c(4, 12)) +
    ggplot2::scale_linewidth_continuous(range = c(0.5, 2.5), guide = "none") +
    ggplot2::labs(
      title = title,
      subtitle = "Overview uses Top 30 degree nodes and up to Top 100 strongest edges among them",
      size = "Degree",
      fill = "Phylum / Cluster",
      color = "Correlation"
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", color = "#0f172a"),
      plot.subtitle = ggplot2::element_text(color = "#475569"),
      legend.title = ggplot2::element_text(face = "bold")
    )

  if (isTRUE(label_nodes)) {
    label_df <- nodes[nodes$hub_rank <= 10, , drop = FALSE]
    if (nrow(label_df) > 0) {
      p <- p +
        ggrepel::geom_text_repel(
          data = label_df,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$display_taxon_short),
          size = 3.6,
          color = "#0f172a",
          box.padding = 0.45,
          point.padding = 0.25,
          max.overlaps = Inf,
          seed = 42,
          min.segment.length = 0,
          segment.color = "#94a3b8"
        )
    }
  }

  p
}

make_network_degree_barplot <- function(node_table, top_n = 20) {
  if (!is.data.frame(node_table) || nrow(node_table) < 1) {
    return(make_empty_network_plot("Hub taxa degree barplot", "No hub taxa available."))
  }

  ord <- order(node_table$degree, node_table$betweenness, decreasing = TRUE, na.last = TRUE)
  top_nodes <- utils::head(node_table[ord, , drop = FALSE], top_n)
  top_nodes$plot_label <- .short_node_label(top_nodes$display_taxon, max_chars = 36)
  top_nodes$plot_label <- make.unique(top_nodes$plot_label, sep = " ")
  top_nodes$plot_label <- factor(top_nodes$plot_label, levels = rev(top_nodes$plot_label))

  color_levels <- unique(top_nodes$color_group)
  color_levels <- color_levels[!is.na(color_levels) & nzchar(color_levels)]
  palette <- stats::setNames(grDevices::hcl.colors(max(1, length(color_levels)), "Dark 3"), color_levels)

  ggplot2::ggplot(top_nodes, ggplot2::aes(x = .data$plot_label, y = .data$degree, fill = .data$color_group)) +
    ggplot2::geom_col(width = 0.78) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::labs(
      title = "Top 20 hub taxa by degree",
      x = NULL,
      y = "Degree",
      fill = "Phylum / Cluster"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", color = "#0f172a"),
      axis.text.y = ggplot2::element_text(color = "#0f172a")
    )
}

save_network_plot <- function(plot_obj, output_png, output_pdf, width = 10, height = 8, dpi = 300) {
  assert_non_empty_string(output_png, "output_png")
  assert_non_empty_string(output_pdf, "output_pdf")
  ensure_dir(dirname(output_png))
  ensure_dir(dirname(output_pdf))

  ggplot2::ggsave(output_png, plot = plot_obj, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(output_pdf, plot = plot_obj, width = width, height = height, device = grDevices::cairo_pdf)

  invisible(list(
    pdf = normalizePath(output_pdf, winslash = "/", mustWork = TRUE),
    png = normalizePath(output_png, winslash = "/", mustWork = TRUE)
  ))
}

plot_network <- function(graph, node_table, edge_table, output_png, output_pdf) {
  if (!inherits(graph, "igraph")) stop("plot_network(): graph must be an igraph object.", call. = FALSE)
  if (!is.data.frame(node_table)) stop("plot_network(): node_table must be a data.frame.", call. = FALSE)
  if (!is.data.frame(edge_table)) stop("plot_network(): edge_table must be a data.frame.", call. = FALSE)
  assert_non_empty_string(output_png, "output_png")
  assert_non_empty_string(output_pdf, "output_pdf")

  core <- select_core_network(node_table, edge_table, top_n_nodes = 30, top_n_labels = 10, top_n_edges = 100)
  layout_data <- compute_core_layout(core$nodes, core$edges)

  overview_plot <- make_core_network_plot(layout_data, label_nodes = FALSE, title = "Core co-occurrence network overview")
  labelled_plot <- make_core_network_plot(layout_data, label_nodes = TRUE, title = "Core co-occurrence network with Top 10 hub taxa")
  degree_plot <- make_network_degree_barplot(core$nodes, top_n = 20)

  figs_dir <- dirname(output_png)
  base_stub <- tools::file_path_sans_ext(basename(output_png))
  ext_pdf_stub <- tools::file_path_sans_ext(basename(output_pdf))
  if (!identical(base_stub, ext_pdf_stub)) {
    stop("plot_network(): output_png and output_pdf must share the same basename.", call. = FALSE)
  }

  out_overview_png <- file.path(figs_dir, "network_plot_overview.png")
  out_overview_pdf <- file.path(figs_dir, "network_plot_overview.pdf")
  out_label_png <- file.path(figs_dir, "network_plot_labelled.png")
  out_label_pdf <- file.path(figs_dir, "network_plot_labelled.pdf")
  out_degree_png <- file.path(figs_dir, "network_degree_barplot.png")
  out_degree_pdf <- file.path(figs_dir, "network_degree_barplot.pdf")

  overview_paths <- save_network_plot(overview_plot, out_overview_png, out_overview_pdf, width = 10, height = 8, dpi = 300)
  labelled_paths <- save_network_plot(labelled_plot, out_label_png, out_label_pdf, width = 10, height = 8, dpi = 300)
  degree_paths <- save_network_plot(degree_plot, out_degree_png, out_degree_pdf, width = 10, height = 8, dpi = 300)
  alias_paths <- save_network_plot(overview_plot, output_png, output_pdf, width = 10, height = 8, dpi = 300)

  invisible(list(
    png = alias_paths$png,
    pdf = alias_paths$pdf,
    overview_png = overview_paths$png,
    overview_pdf = overview_paths$pdf,
    labelled_png = labelled_paths$png,
    labelled_pdf = labelled_paths$pdf,
    degree_png = degree_paths$png,
    degree_pdf = degree_paths$pdf,
    core_node_count = nrow(core$nodes),
    core_edge_count = nrow(core$edges)
  ))
}

# Publication plotting helpers. These override the exploratory plotting entry
# point above while preserving all historical filenames and return fields.
.network_paper_palette <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7",
  "#E69F00", "#56B4E9", "#F0E442", "#6A3D9A"
)

compute_publication_network_layout <- function(node_table, edge_table) {
  if (!is.data.frame(node_table) || !nrow(node_table)) return(list(nodes = node_table, edges = edge_table))
  graph <- igraph::graph_from_data_frame(
    if (nrow(edge_table)) edge_table else data.frame(from = character(), to = character()),
    directed = FALSE,
    vertices = node_table[, "name", drop = FALSE]
  )
  membership <- igraph::components(graph)$membership
  component_ids <- sort(unique(membership))
  component_sizes <- table(membership)
  max_size <- max(component_sizes)
  centers <- if (length(component_ids) == 1L) {
    matrix(c(0, 0), ncol = 2L)
  } else {
    2.4 * igraph::layout_in_circle(igraph::make_ring(length(component_ids)))
  }
  coordinates <- matrix(0, nrow = igraph::vcount(graph), ncol = 2L)
  rownames(coordinates) <- igraph::V(graph)$name
  for (i in seq_along(component_ids)) {
    vertices <- which(membership == component_ids[i])
    subgraph <- igraph::induced_subgraph(graph, vids = vertices)
    n <- igraph::vcount(subgraph)
    sparse_circle <- n > 1L && max(igraph::degree(subgraph)) <= 2L
    local <- if (n <= 1L) {
      matrix(c(0, 0), ncol = 2L)
    } else if (sparse_circle) {
      igraph::layout_in_circle(subgraph)
    } else {
      set.seed(42L + i)
      weights <- if ("abs_rho" %in% igraph::edge_attr_names(subgraph)) pmax(igraph::E(subgraph)$abs_rho, 0.01) else NULL
      igraph::layout_with_fr(subgraph, weights = weights, niter = 1000L)
    }
    scale <- 0.65 + 0.65 * sqrt(n / max_size)
    if (n > 1L) {
      local <- if (sparse_circle) local * scale else igraph::norm_coords(local, xmin = -scale, xmax = scale, ymin = -scale, ymax = scale)
    }
    local[, 1L] <- local[, 1L] + centers[i, 1L]
    local[, 2L] <- local[, 2L] + centers[i, 2L]
    coordinates[igraph::V(subgraph)$name, ] <- local
  }
  positions <- data.frame(name = rownames(coordinates), x = coordinates[, 1L], y = coordinates[, 2L], stringsAsFactors = FALSE)
  nodes <- merge(node_table, positions, by = "name", all.x = TRUE, sort = FALSE)
  if (!nrow(edge_table)) return(list(nodes = nodes, edges = edge_table))
  edges <- merge(edge_table, positions, by.x = "source", by.y = "name", all.x = TRUE, sort = FALSE)
  names(edges)[names(edges) %in% c("x", "y")] <- c("x_source", "y_source")
  edges <- merge(edges, positions, by.x = "target", by.y = "name", all.x = TRUE, sort = FALSE)
  names(edges)[names(edges) %in% c("x", "y")] <- c("x_target", "y_target")
  list(nodes = nodes, edges = edges)
}

.network_prepare_color_groups <- function(nodes, max_groups = 7L) {
  group <- as.character(nodes$color_group)
  group[is.na(group) | !nzchar(group)] <- "Unclassified"
  counts <- sort(table(group), decreasing = TRUE)
  keep <- names(utils::head(counts, max_groups))
  group[!group %in% keep] <- "Other"
  levels <- unique(c(keep, if (any(group == "Other")) "Other"))
  nodes$paper_group <- factor(group, levels = levels)
  nodes
}

make_publication_network_plot <- function(layout_data, label_nodes = TRUE,
                                          title = "Core microbial co-occurrence network") {
  nodes <- layout_data$nodes
  edges <- layout_data$edges
  if (!is.data.frame(nodes) || !nrow(nodes)) {
    return(make_empty_network_plot(title, "No connected taxa passed the network thresholds.") + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA)))
  }
  nodes <- .network_prepare_color_groups(nodes)
  group_levels <- levels(nodes$paper_group)
  palette <- stats::setNames(rep(.network_paper_palette, length.out = length(group_levels)), group_levels)
  if ("Other" %in% group_levels) palette["Other"] <- "#999999"
  module_count <- length(unique(nodes$module %||% nodes$component))
  subtitle <- sprintf("%d connected taxa, %d strongest significant edges, %d modules; node size = degree", nrow(nodes), nrow(edges), module_count)

  p <- ggplot2::ggplot()
  if (is.data.frame(edges) && nrow(edges)) {
    p <- p + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(
        x = .data$x_source, y = .data$y_source,
        xend = .data$x_target, yend = .data$y_target,
        color = .data$sign, linewidth = .data$abs_rho,
        linetype = .data$sign
      ),
      alpha = 0.48,
      lineend = "round"
    ) +
      ggplot2::scale_color_manual(values = c(positive = "#C44E52", negative = "#2C7FB8"), drop = FALSE) +
      ggplot2::scale_linetype_manual(values = c(positive = "solid", negative = "22"), guide = "none") +
      ggplot2::scale_linewidth_continuous(range = c(0.35, 1.7), guide = "none")
  }
  p <- p +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(.data$x, .data$y, size = .data$degree, fill = .data$paper_group),
      shape = 21, color = "white", stroke = 0.65, alpha = 0.98
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::scale_size_continuous(range = c(3.2, 9)) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      size = "Degree", fill = "Phylum", color = "Association"
    ) +
    ggplot2::theme_void(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#17202A"),
      plot.subtitle = ggplot2::element_text(size = 9.5, color = "#4D5B66"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(12, 20, 12, 12)
    )
  if (isTRUE(label_nodes)) {
    label_df <- nodes[order(nodes$hub_rank), , drop = FALSE]
    label_df <- utils::head(label_df[!duplicated(label_df$display_taxon_short), , drop = FALSE], min(8L, nrow(label_df)))
    if (nrow(label_df)) {
      p <- p + ggrepel::geom_text_repel(
        data = label_df,
        ggplot2::aes(.data$x, .data$y, label = .data$display_taxon_short),
        size = 3.2, color = "#17202A", fontface = "italic",
        box.padding = 0.5, point.padding = 0.25,
        min.segment.length = 0, max.overlaps = Inf, seed = 42,
        segment.color = "#7F8C8D", segment.size = 0.35
      )
    }
  }
  p
}

make_network_centrality_plot <- function(node_table, top_n = 15L) {
  nodes <- node_table[is.finite(node_table$degree) & node_table$degree > 0, , drop = FALSE]
  if (!nrow(nodes)) return(make_empty_network_plot("Hub centrality", "No connected taxa available."))
  nodes <- .network_prepare_color_groups(nodes)
  group_levels <- levels(nodes$paper_group)
  palette <- stats::setNames(rep(.network_paper_palette, length.out = length(group_levels)), group_levels)
  if ("Other" %in% group_levels) palette["Other"] <- "#999999"
  labels <- utils::head(nodes[order(nodes$hub_rank), , drop = FALSE], min(8L, nrow(nodes)))
  ggplot2::ggplot(nodes, ggplot2::aes(.data$degree, .data$betweenness)) +
    ggplot2::geom_point(ggplot2::aes(size = .data$eigenvector, fill = .data$paper_group), shape = 21, color = "white", stroke = 0.5, alpha = 0.9) +
    ggrepel::geom_text_repel(data = labels, ggplot2::aes(label = .data$display_taxon_short), size = 3, seed = 42, max.overlaps = Inf) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::scale_size_continuous(range = c(2.5, 7), guide = "none") +
    ggplot2::labs(title = "Hub taxa centrality", subtitle = "Candidates with high degree and betweenness occupy the upper-right region", x = "Degree", y = "Normalized betweenness") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA), plot.title = ggplot2::element_text(face = "bold"))
}

make_network_edge_summary_plot <- function(edge_table) {
  if (!is.data.frame(edge_table) || !nrow(edge_table)) return(make_empty_network_plot("Association composition", "No significant edges available."))
  values <- split(edge_table$abs_rho, edge_table$sign)
  plot_df <- data.frame(
    sign = names(values),
    n = vapply(values, length, integer(1)),
    median_abs_rho = vapply(values, stats::median, numeric(1), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  plot_df$label <- sprintf("n = %d\nmedian |rho| = %.2f", plot_df$n, plot_df$median_abs_rho)
  ggplot2::ggplot(plot_df, ggplot2::aes(.data$sign, .data$n, fill = .data$sign)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.25, size = 3.4) +
    ggplot2::scale_fill_manual(values = c(positive = "#C44E52", negative = "#2C7FB8"), guide = "none") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.22))) +
    ggplot2::labs(title = "Significant association composition", subtitle = "Edges satisfy both |rho| and FDR thresholds", x = NULL, y = "Number of edges") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA), plot.title = ggplot2::element_text(face = "bold"))
}

save_network_plot_formats <- function(plot_obj, base_path, width = 7, height = 5.5) {
  ensure_dir(dirname(base_path))
  paths <- c(pdf = paste0(base_path, ".pdf"), svg = paste0(base_path, ".svg"), tiff = paste0(base_path, ".tiff"), png = paste0(base_path, ".png"))
  ggplot2::ggsave(paths[["pdf"]], plot_obj, width = width, height = height, device = grDevices::cairo_pdf, bg = "white")
  ggplot2::ggsave(paths[["svg"]], plot_obj, width = width, height = height, device = grDevices::svg, bg = "white")
  ggplot2::ggsave(paths[["tiff"]], plot_obj, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
  ggplot2::ggsave(paths[["png"]], plot_obj, width = width, height = height, dpi = 300, bg = "white")
  normalized <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  names(normalized) <- names(paths)
  normalized
}

save_network_combined_figure <- function(plots, base_path, width = 11, height = 8.5) {
  draw <- function() {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2L, 2L)))
    for (i in seq_len(4L)) {
      row <- if (i <= 2L) 1L else 2L
      col <- if (i %% 2L) 1L else 2L
      print(plots[[i]] + ggplot2::labs(tag = LETTERS[i]), vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
    }
    grid::popViewport()
  }
  ensure_dir(dirname(base_path))
  grDevices::cairo_pdf(paste0(base_path, ".pdf"), width = width, height = height, bg = "white"); draw(); grDevices::dev.off()
  grDevices::png(paste0(base_path, ".png"), width = width, height = height, units = "in", res = 300, bg = "white"); draw(); grDevices::dev.off()
  grDevices::tiff(paste0(base_path, ".tiff"), width = width, height = height, units = "in", res = 600, compression = "lzw", bg = "white"); draw(); grDevices::dev.off()
  grDevices::svg(paste0(base_path, ".svg"), width = width, height = height, bg = "white"); draw(); grDevices::dev.off()
  normalizePath(paste0(base_path, c(".pdf", ".png", ".tiff", ".svg")), winslash = "/", mustWork = TRUE)
}

plot_network <- function(graph, node_table, edge_table, output_png, output_pdf) {
  if (!inherits(graph, "igraph")) stop("plot_network(): graph must be an igraph object.", call. = FALSE)
  core <- select_core_network(node_table, edge_table, top_n_nodes = 30L, top_n_labels = 8L, top_n_edges = 100L)
  layout_data <- compute_publication_network_layout(core$nodes, core$edges)
  overview <- make_publication_network_plot(layout_data, label_nodes = FALSE, title = "Core microbial co-occurrence network")
  labelled <- make_publication_network_plot(layout_data, label_nodes = TRUE, title = "Core network and candidate hub taxa")
  degree <- make_network_degree_barplot(core$nodes, top_n = min(15L, nrow(core$nodes))) +
    ggplot2::labs(title = "Top hub taxa by degree") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA), plot.title = ggplot2::element_text(face = "bold"))
  centrality <- make_network_centrality_plot(node_table)
  edge_summary <- make_network_edge_summary_plot(edge_table)
  degree_combined <- degree + ggplot2::theme(legend.position = "none")
  figs_dir <- dirname(output_png)
  overview_paths <- save_network_plot_formats(overview, file.path(figs_dir, "network_plot_overview"), 8.2, 6.5)
  labelled_paths <- save_network_plot_formats(labelled, file.path(figs_dir, "network_plot_labelled"), 8.2, 6.5)
  degree_paths <- save_network_plot_formats(degree, file.path(figs_dir, "network_degree_barplot"), 7.2, 6)
  centrality_paths <- save_network_plot_formats(centrality, file.path(figs_dir, "network_centrality"), 7, 5.5)
  edge_paths <- save_network_plot_formats(edge_summary, file.path(figs_dir, "network_edge_composition"), 6.5, 5)
  combined_paths <- save_network_combined_figure(list(labelled, degree_combined, centrality, edge_summary), file.path(figs_dir, "network_figure_combined"))
  file.copy(file.path(figs_dir, "network_figure_combined.png"), output_png, overwrite = TRUE)
  file.copy(file.path(figs_dir, "network_figure_combined.pdf"), output_pdf, overwrite = TRUE)
  invisible(list(
    png = normalizePath(output_png, winslash = "/", mustWork = TRUE),
    pdf = normalizePath(output_pdf, winslash = "/", mustWork = TRUE),
    overview_png = overview_paths[["png"]], overview_pdf = overview_paths[["pdf"]],
    labelled_png = labelled_paths[["png"]], labelled_pdf = labelled_paths[["pdf"]],
    degree_png = degree_paths[["png"]], degree_pdf = degree_paths[["pdf"]],
    centrality_png = centrality_paths[["png"]], centrality_pdf = centrality_paths[["pdf"]],
    edge_composition_png = edge_paths[["png"]], edge_composition_pdf = edge_paths[["pdf"]],
    combined = combined_paths,
    core_node_count = nrow(core$nodes), core_edge_count = nrow(core$edges)
  ))
}

summarize_network_for_ai <- function(node_table, edge_table, rho_cutoff, p_cutoff, figure_paths = NULL) {
  if (!is.data.frame(node_table)) stop("summarize_network_for_ai(): node_table must be a data.frame.", call. = FALSE)
  if (!is.data.frame(edge_table)) stop("summarize_network_for_ai(): edge_table must be a data.frame.", call. = FALSE)
  if (!is.numeric(rho_cutoff) || length(rho_cutoff) != 1) stop("summarize_network_for_ai(): rho_cutoff must be numeric.", call. = FALSE)
  if (!is.numeric(p_cutoff) || length(p_cutoff) != 1) stop("summarize_network_for_ai(): p_cutoff must be numeric.", call. = FALSE)

  top_nodes <- node_table
  if (nrow(top_nodes) > 0 && "degree" %in% names(top_nodes)) {
    top_nodes <- top_nodes[order(top_nodes$degree, top_nodes$betweenness, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    top_nodes <- utils::head(top_nodes, 20)
  }

  list(
    analysis_type = "spearman_cooccurrence_network",
    method = "spearman",
    thresholds = list(
      abs_rho = rho_cutoff,
      fdr = p_cutoff
    ),
    n_nodes = nrow(node_table),
    n_edges = nrow(edge_table),
    n_positive_edges = if ("sign" %in% names(edge_table)) sum(edge_table$sign == "positive", na.rm = TRUE) else NA_integer_,
    n_negative_edges = if ("sign" %in% names(edge_table)) sum(edge_table$sign == "negative", na.rm = TRUE) else NA_integer_,
    top_nodes = top_nodes[, intersect(c("name", "display_taxon", "phylum", "degree", "betweenness", "closeness", "eigenvector", "component"), names(top_nodes)), drop = FALSE],
    display_network = list(
      overview_nodes = 30L,
      labelled_nodes = 10L,
      max_core_edges = 100L,
      figure_paths = figure_paths
    ),
    caution = "Correlation is not causation; front-end figures show only the core network, while network_nodes.csv and network_edges.csv retain the full analysis results."
  )
}

run_network_analysis <- function(dataset, tax_level = "Genus", job_dir, rho_cutoff = 0.6, p_cutoff = 0.05, max_taxa = NULL) {
  if (is.null(dataset)) stop("run_network_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_network_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(tax_level, "tax_level")

  append_reproducibility(job_dir, list(
    phase6 = list(
      started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tax_level = tax_level,
      rho_cutoff = rho_cutoff,
      p_cutoff = p_cutoff,
      max_taxa = max_taxa
    )
  ))

  abund_matrix <- prepare_network_matrix(dataset = dataset, tax_level = tax_level)
  
  if (is.null(max_taxa)) {
    max_taxa <- 500
  }
  if (!is.null(max_taxa) && max_taxa > 0 && ncol(abund_matrix) > max_taxa) {
    mean_abund <- colMeans(abund_matrix, na.rm = TRUE)
    top_idx <- order(mean_abund, decreasing = TRUE)[seq_len(max_taxa)]
    abund_matrix <- abund_matrix[, top_idx, drop = FALSE]
  }

  network <- build_spearman_network(
    abund_matrix = abund_matrix,
    rho_cutoff = rho_cutoff,
    p_cutoff = p_cutoff
  )

  graph <- network$graph
  edge_table <- network$edge_table
  node_table <- calculate_network_centrality(graph)

  if (nrow(node_table) > 0) {
    stats_df <- data.frame(
      name = colnames(abund_matrix),
      mean_abundance = colMeans(abund_matrix, na.rm = TRUE),
      prevalence = colMeans(abund_matrix > 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    node_table <- merge(node_table, stats_df, by = "name", all.x = TRUE, sort = FALSE)
    node_table <- node_table[match(colnames(abund_matrix), node_table$name), , drop = FALSE]
    rownames(node_table) <- NULL
    node_table <- augment_network_node_table(node_table)
  } else {
    node_table <- data.frame(
      name = character(),
      degree = numeric(),
      betweenness = numeric(),
      closeness = numeric(),
      eigenvector = numeric(),
      component = integer(),
      mean_abundance = numeric(),
      prevalence = numeric(),
      display_taxon = character(),
      display_taxon_short = character(),
      phylum = character(),
      cluster_label = character(),
      module_label = character(),
      color_group = character(),
      hub_rank = integer(),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(edge_table) > 0) {
    edge_table$source_display <- clean_taxon_label(edge_table$source)
    edge_table$target_display <- clean_taxon_label(edge_table$target)
  }

  out_nodes <- file.path(job_dir, "tables", "network_nodes.csv")
  out_edges <- file.path(job_dir, "tables", "network_edges.csv")
  ensure_dir(dirname(out_nodes))
  readr::write_csv(node_table, out_nodes)
  readr::write_csv(edge_table, out_edges)

  connected_nodes <- sum(node_table$degree > 0, na.rm = TRUE)
  n_components <- if (igraph::vcount(graph)) igraph::components(graph)$no else 0L
  n_modules <- if (nrow(node_table) && "module" %in% names(node_table)) length(unique(node_table$module[node_table$degree > 0])) else 0L
  network_density <- if (igraph::vcount(graph) > 1L) igraph::edge_density(graph, loops = FALSE) else 0
  modularity_value <- if (igraph::ecount(graph) > 0L && "module" %in% names(node_table)) tryCatch(
    igraph::modularity(
      graph,
      membership = stats::setNames(node_table$module, node_table$name)[igraph::V(graph)$name],
      weights = if ("abs_rho" %in% igraph::edge_attr_names(graph)) igraph::E(graph)$abs_rho else NULL
    ),
    error = function(e) NA_real_
  ) else NA_real_
  network_statistics <- data.frame(
    metric = c("samples", "taxa_evaluated", "connected_taxa", "isolated_taxa", "significant_edges", "positive_edges", "negative_edges", "components", "modules", "density", "modularity", "rho_threshold", "fdr_threshold"),
    value = c(
      nrow(abund_matrix), ncol(abund_matrix), connected_nodes, nrow(node_table) - connected_nodes,
      nrow(edge_table), sum(edge_table$sign == "positive", na.rm = TRUE), sum(edge_table$sign == "negative", na.rm = TRUE),
      n_components, n_modules, network_density, modularity_value, rho_cutoff, p_cutoff
    ),
    stringsAsFactors = FALSE
  )
  readr::write_csv(network_statistics, file.path(job_dir, "tables", "network_statistics.csv"))

  plot_paths <- plot_network(
    graph = graph,
    node_table = node_table,
    edge_table = edge_table,
    output_png = file.path(job_dir, "figures", "network_plot.png"),
    output_pdf = file.path(job_dir, "figures", "network_plot.pdf")
  )

  summary <- summarize_network_for_ai(
    node_table = node_table,
    edge_table = edge_table,
    rho_cutoff = rho_cutoff,
    p_cutoff = p_cutoff,
    figure_paths = list(
      network_plot = c("figures/network_plot.png", "figures/network_plot.pdf"),
      overview = c("figures/network_plot_overview.png", "figures/network_plot_overview.pdf"),
      labelled = c("figures/network_plot_labelled.png", "figures/network_plot_labelled.pdf"),
      degree_barplot = c("figures/network_degree_barplot.png", "figures/network_degree_barplot.pdf"),
      centrality = c("figures/network_centrality.png", "figures/network_centrality.pdf"),
      edge_composition = c("figures/network_edge_composition.png", "figures/network_edge_composition.pdf"),
      combined = c("figures/network_figure_combined.png", "figures/network_figure_combined.pdf", "figures/network_figure_combined.svg", "figures/network_figure_combined.tiff")
    )
  )
  summary$tax_level <- tax_level
  summary$n_samples <- nrow(abund_matrix)
  summary$n_taxa <- ncol(abund_matrix)
  summary$n_connected_nodes <- connected_nodes
  summary$n_isolated_nodes <- nrow(node_table) - connected_nodes
  summary$n_components <- n_components
  summary$n_modules <- n_modules
  summary$density <- network_density
  summary$modularity <- modularity_value
  summary$computation_scope <- list(
    retained_all_taxa = TRUE,
    original_taxa_count = ncol(abund_matrix)
  )
  summary$edge_rate <- if (summary$n_nodes > 1) summary$n_edges / (summary$n_nodes * (summary$n_nodes - 1) / 2) else 0
  summary$positive_edge_rate <- if (summary$n_edges > 0) summary$n_positive_edges / summary$n_edges else NA_real_
  summary$negative_edge_rate <- if (summary$n_edges > 0) summary$n_negative_edges / summary$n_edges else NA_real_
  summary$outputs <- list(
    nodes = "tables/network_nodes.csv",
    edges = "tables/network_edges.csv",
    statistics = "tables/network_statistics.csv",
    summary = "json/network_summary.json",
    plot = c("figures/network_plot.png", "figures/network_plot.pdf"),
    overview_plot = c("figures/network_plot_overview.png", "figures/network_plot_overview.pdf"),
    labelled_plot = c("figures/network_plot_labelled.png", "figures/network_plot_labelled.pdf"),
    degree_barplot = c("figures/network_degree_barplot.png", "figures/network_degree_barplot.pdf"),
    centrality_plot = c("figures/network_centrality.png", "figures/network_centrality.pdf"),
    edge_composition_plot = c("figures/network_edge_composition.png", "figures/network_edge_composition.pdf"),
    combined_figure = c("figures/network_figure_combined.png", "figures/network_figure_combined.pdf", "figures/network_figure_combined.svg", "figures/network_figure_combined.tiff")
  )

  methods_lines <- c(
    "Network analysis methods",
    sprintf("Pairwise Spearman correlations were calculated across %d samples for %d taxa at the %s level.", nrow(abund_matrix), ncol(abund_matrix), tax_level),
    sprintf("Edges required |rho| >= %.2f and Benjamini-Hochberg FDR < %.3f.", rho_cutoff, p_cutoff),
    "Node degree, normalized betweenness, closeness, eigenvector centrality, connected components, and Louvain modules were calculated with igraph.",
    "The publication figure displays a degree-balanced core of up to 30 connected taxa and the 100 strongest threshold-passing edges; complete results remain in network_nodes.csv and network_edges.csv.",
    "Node color represents phylum, node size represents degree, edge color and line type represent association sign, and edge width represents absolute Spearman correlation.",
    "Co-occurrence indicates statistical association and does not establish direct interaction or causality."
  )
  writeLines(methods_lines, file.path(job_dir, "network_methods.txt"), useBytes = TRUE)
  results_lines <- c(
    "Network analysis results",
    sprintf("The thresholded network contained %d connected taxa and %d significant edges across %d detected modules.", connected_nodes, nrow(edge_table), n_modules),
    sprintf("Positive and negative associations accounted for %d and %d edges, respectively.", sum(edge_table$sign == "positive", na.rm = TRUE), sum(edge_table$sign == "negative", na.rm = TRUE)),
    sprintf("Network density was %.4f%s.", network_density, if (is.finite(modularity_value)) sprintf(" and modularity was %.3f", modularity_value) else ""),
    "Hub taxa are association-network candidates and require validation; these results do not demonstrate ecological interaction or causality."
  )
  writeLines(results_lines, file.path(job_dir, "network_results_summary.txt"), useBytes = TRUE)

  summary_path <- write_json_pretty(summary, file.path(job_dir, "json", "network_summary.json"), auto_unbox = TRUE)

  append_reproducibility(job_dir, list(
    phase6 = list(
      computation_scope = summary$computation_scope,
      display_network = summary$display_network,
      finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  ))

  list(
    graph = graph,
    node_table = node_table,
    edge_table = edge_table,
    summary = summary,
    summary_path = summary_path,
    figure_paths = plot_paths
  )
}
