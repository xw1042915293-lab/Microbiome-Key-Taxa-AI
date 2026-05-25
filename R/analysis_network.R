# Spearman co-occurrence network analysis (Phase 6).
# Outputs:
# - tables/network_nodes.csv
# - tables/network_edges.csv
# - json/network_summary.json
# - figures/network_plot.png + .pdf

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
  if (nrow(mat) < 2) stop("prepare_network_matrix(): need at least 2 samples.", call. = FALSE)
  if (ncol(mat) < 1) stop("prepare_network_matrix(): need at least 1 genus feature.", call. = FALSE)

  as.matrix(mat)
}

build_spearman_network <- function(abund_matrix, rho_cutoff = 0.6, p_cutoff = 0.05) {
  if (!is.matrix(abund_matrix) && !is.data.frame(abund_matrix)) {
    stop("build_spearman_network(): abund_matrix must be a matrix/data.frame.", call. = FALSE)
  }
  abund_matrix <- as.matrix(abund_matrix)
  storage.mode(abund_matrix) <- "double"
  if (nrow(abund_matrix) < 2 || ncol(abund_matrix) < 1) {
    stop("build_spearman_network(): abund_matrix must have >=2 samples and >=1 taxon.", call. = FALSE)
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

  edge_rows <- list()
  idx <- 0L
  for (i in seq_len(ncol(abund_matrix) - 1L)) {
    x <- abund_matrix[, i]
    for (j in seq.int(i + 1L, ncol(abund_matrix))) {
      y <- abund_matrix[, j]
      ct <- tryCatch(
        stats::cor.test(x, y, method = "spearman", exact = FALSE),
        error = function(e) NULL
      )
      if (is.null(ct) || is.na(ct$estimate) || is.na(ct$p.value)) next
      idx <- idx + 1L
      edge_rows[[idx]] <- data.frame(
        source = taxa[i],
        target = taxa[j],
        rho = unname(as.numeric(ct$estimate)),
        p_value = as.numeric(ct$p.value),
        stringsAsFactors = FALSE
      )
    }
  }

  edge_table <- if (length(edge_rows) == 0) {
    data.frame(
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
    edge_table <- do.call(rbind, edge_rows)
    edge_table$fdr <- stats::p.adjust(edge_table$p_value, method = "fdr")
    edge_table$abs_rho <- abs(edge_table$rho)
    edge_table$sign <- ifelse(edge_table$rho >= 0, "positive", "negative")
    edge_table <- edge_table[edge_table$abs_rho >= rho_cutoff & edge_table$fdr < p_cutoff, , drop = FALSE]
    edge_table <- edge_table[order(edge_table$fdr, -edge_table$abs_rho), , drop = FALSE]
    rownames(edge_table) <- NULL
    edge_table
  }

  nodes <- data.frame(name = taxa, stringsAsFactors = FALSE)
  graph <- igraph::graph_from_data_frame(
    d = if (nrow(edge_table) == 0) data.frame(from = character(), to = character(), stringsAsFactors = FALSE) else edge_table[, c("source", "target"), drop = FALSE],
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
      stringsAsFactors = FALSE
    ))
  }

  degree <- as.numeric(igraph::degree(graph, mode = "all"))
  betweenness <- as.numeric(igraph::betweenness(graph, directed = FALSE, normalized = TRUE))
  closeness <- suppressWarnings(as.numeric(igraph::closeness(graph, normalized = TRUE)))
  closeness[!is.finite(closeness)] <- 0

  ev <- tryCatch(
    igraph::eigen_centrality(graph, directed = FALSE, scale = TRUE)$vector,
    error = function(e) rep(0, igraph::vcount(graph))
  )
  ev <- as.numeric(ev)
  ev[!is.finite(ev)] <- 0

  comp <- igraph::components(graph)$membership

  data.frame(
    name = igraph::V(graph)$name,
    degree = degree,
    betweenness = betweenness,
    closeness = closeness,
    eigenvector = ev,
    component = as.integer(comp),
    stringsAsFactors = FALSE
  )
}

save_network_plot <- function(plot_fun, output_png, output_pdf, width = 8, height = 6, dpi = 300) {
  assert_non_empty_string(output_png, "output_png")
  assert_non_empty_string(output_pdf, "output_pdf")
  ensure_dir(dirname(output_png))
  ensure_dir(dirname(output_pdf))

  grDevices::pdf(output_pdf, width = width, height = height)
  tryCatch(
    {
      plot_fun()
    },
    finally = grDevices::dev.off()
  )

  grDevices::png(output_png, width = width, height = height, units = "in", res = dpi)
  tryCatch(
    {
      plot_fun()
    },
    finally = grDevices::dev.off()
  )

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

  draw_fun <- function() {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    graphics::par(mar = c(0, 0, 3, 0))

    if (igraph::vcount(graph) == 0) {
      graphics::plot.new()
      graphics::title("Spearman co-occurrence network")
      graphics::text(0.5, 0.5, "No taxa available for network plotting.")
      return(invisible(NULL))
    }

    if (igraph::ecount(graph) == 0) {
      graphics::plot.new()
      graphics::title("Spearman co-occurrence network")
      graphics::text(0.5, 0.5, "No edges passed the thresholds.")
      return(invisible(NULL))
    }

    deg <- node_table$degree
    if (!is.numeric(deg) || length(deg) != igraph::vcount(graph)) {
      deg <- rep(0, igraph::vcount(graph))
    }
    vsize <- 6 + 2.5 * sqrt(deg + 1)
    vcolor <- "#2C7FB8"
    if ("component" %in% names(node_table) && length(unique(node_table$component)) > 1) {
      comp_ids <- as.integer(factor(node_table$component))
      palette <- grDevices::hcl.colors(max(comp_ids), "Set 2")
      vcolor <- palette[comp_ids]
    }

    ecol <- if ("sign" %in% names(edge_table)) {
      ifelse(edge_table$sign == "positive", "#D73027", "#4575B4")
    } else {
      "#636363"
    }
    ewidth <- 1 + 3 * if ("abs_rho" %in% names(edge_table)) edge_table$abs_rho else abs(edge_table$rho)

    layout <- igraph::layout_with_fr(graph)
    layout <- layout * 1.15

    # Display labels: keep original vertex names unchanged; only shorten for plotting.
    vname <- igraph::V(graph)$name
    display_label <- clean_taxon_label(vname)
    display_label <- .short_node_label(display_label, max_chars = 40)

    # Heuristic label "repel": place labels outside the graph center.
    ctr <- colMeans(layout)
    ang <- atan2(layout[, 2] - ctr[2], layout[, 1] - ctr[1])

    # If too many nodes, label only the most connected to avoid overlaps.
    label_vec <- display_label
    if (igraph::vcount(graph) > 40) {
      ord <- order(deg, decreasing = TRUE)
      keep <- ord[seq_len(min(40, length(ord)))]
      label_vec[-keep] <- NA_character_
    }

    sparse <- igraph::ecount(graph) < 3
    if (sparse) {
      # In very sparse graphs, focus labels on nodes with at least one edge.
      label_vec[deg < 1] <- NA_character_
    }
    main_title <- if (sparse) "Exploratory sparse co-occurrence network" else "Spearman co-occurrence network"

    igraph::plot.igraph(
      graph,
      layout = layout,
      vertex.size = vsize,
      vertex.color = vcolor,
      vertex.label = label_vec,
      vertex.label.cex = if (sparse) 0.85 else 0.7,
      vertex.label.color = "#2C3E50",
      vertex.label.dist = if (sparse) 0.9 else 0.75,
      vertex.label.degree = ang,
      vertex.frame.color = "#FFFFFF",
      edge.color = ecol,
      edge.width = ewidth,
      margin = 0.05,
      main = main_title
    )
  }

  save_network_plot(draw_fun, output_png, output_pdf, width = 10, height = 7, dpi = 300)
}

summarize_network_for_ai <- function(node_table, edge_table, rho_cutoff, p_cutoff) {
  if (!is.data.frame(node_table)) stop("summarize_network_for_ai(): node_table must be a data.frame.", call. = FALSE)
  if (!is.data.frame(edge_table)) stop("summarize_network_for_ai(): edge_table must be a data.frame.", call. = FALSE)
  if (!is.numeric(rho_cutoff) || length(rho_cutoff) != 1) stop("summarize_network_for_ai(): rho_cutoff must be numeric.", call. = FALSE)
  if (!is.numeric(p_cutoff) || length(p_cutoff) != 1) stop("summarize_network_for_ai(): p_cutoff must be numeric.", call. = FALSE)

  top_nodes <- node_table
  if (nrow(top_nodes) > 0 && "degree" %in% names(top_nodes)) {
    top_nodes <- top_nodes[order(top_nodes$degree, decreasing = TRUE), , drop = FALSE]
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
    top_nodes = top_nodes[, intersect(c("name", "degree", "betweenness", "closeness", "eigenvector", "component"), names(top_nodes)), drop = FALSE],
    caution = "Correlation is not causation; network structure should not be interpreted as direct interaction evidence."
  )
}

run_network_analysis <- function(dataset, tax_level = "Genus", job_dir, rho_cutoff = 0.6, p_cutoff = 0.05) {
  if (is.null(dataset)) stop("run_network_analysis(): dataset is NULL.", call. = FALSE)
  assert_non_empty_string(job_dir, "job_dir")
  if (!dir.exists(job_dir)) stop("run_network_analysis(): job_dir not found: ", job_dir, call. = FALSE)
  assert_non_empty_string(tax_level, "tax_level")

  append_reproducibility(job_dir, list(
    phase6 = list(
      started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tax_level = tax_level,
      rho_cutoff = rho_cutoff,
      p_cutoff = p_cutoff
    )
  ))

  abund_matrix <- prepare_network_matrix(dataset = dataset, tax_level = tax_level)
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
      stringsAsFactors = FALSE
    )
  }

  # Display-only columns for report readability.
  if (nrow(node_table) > 0 && "name" %in% names(node_table)) {
    node_table$display_taxon <- clean_taxon_label(node_table$name)
  }
  if (nrow(edge_table) > 0) {
    if ("source" %in% names(edge_table)) edge_table$source_display <- clean_taxon_label(edge_table$source)
    if ("target" %in% names(edge_table)) edge_table$target_display <- clean_taxon_label(edge_table$target)
  }

  out_nodes <- file.path(job_dir, "tables", "network_nodes.csv")
  out_edges <- file.path(job_dir, "tables", "network_edges.csv")
  ensure_dir(dirname(out_nodes))
  readr::write_csv(node_table, out_nodes)
  readr::write_csv(edge_table, out_edges)

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
    p_cutoff = p_cutoff
  )
  summary$tax_level <- tax_level
  summary$n_samples <- nrow(abund_matrix)
  summary$n_taxa <- ncol(abund_matrix)
  summary$edge_rate <- if (summary$n_nodes > 1) summary$n_edges / (summary$n_nodes * (summary$n_nodes - 1) / 2) else 0
  summary$positive_edge_rate <- if (summary$n_edges > 0) summary$n_positive_edges / summary$n_edges else NA_real_
  summary$negative_edge_rate <- if (summary$n_edges > 0) summary$n_negative_edges / summary$n_edges else NA_real_
  summary$outputs <- list(
    nodes = "tables/network_nodes.csv",
    edges = "tables/network_edges.csv",
    summary = "json/network_summary.json",
    plot = c("figures/network_plot.png", "figures/network_plot.pdf")
  )

  summary_path <- write_json_pretty(summary, file.path(job_dir, "json", "network_summary.json"), auto_unbox = TRUE)

  append_reproducibility(job_dir, list(
    phase6 = list(
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
