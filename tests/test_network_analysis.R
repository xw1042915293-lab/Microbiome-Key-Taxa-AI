# Minimal smoke test for core-network plotting outputs.

source("global.R", local = TRUE)

job_dir <- create_job_dir()
graph <- igraph::make_ring(35)
igraph::V(graph)$name <- paste0(
  "feat", seq_len(igraph::vcount(graph)),
  "|Bacteria|",
  rep(c("Firmicutes", "Proteobacteria", "Actinobacteriota", "Bacteroidota", "Chloroflexi"), length.out = igraph::vcount(graph)),
  "|Class|Order|Family|Taxon",
  seq_len(igraph::vcount(graph))
)

node_table <- data.frame(
  name = igraph::V(graph)$name,
  degree = as.numeric(igraph::degree(graph)),
  betweenness = as.numeric(igraph::betweenness(graph, normalized = TRUE)),
  closeness = as.numeric(igraph::closeness(graph, normalized = TRUE)),
  eigenvector = as.numeric(igraph::eigen_centrality(graph, directed = FALSE)$vector),
  component = 1L,
  mean_abundance = seq_len(igraph::vcount(graph)) / 100,
  prevalence = rep(1, igraph::vcount(graph)),
  stringsAsFactors = FALSE
)
node_table <- augment_network_node_table(node_table)

edge_table <- data.frame(
  source = igraph::as_data_frame(graph, what = "edges")$from,
  target = igraph::as_data_frame(graph, what = "edges")$to,
  rho = seq(0.61, 0.95, length.out = igraph::ecount(graph)),
  p_value = seq(0.001, 0.02, length.out = igraph::ecount(graph)),
  fdr = seq(0.002, 0.03, length.out = igraph::ecount(graph)),
  abs_rho = seq(0.61, 0.95, length.out = igraph::ecount(graph)),
  sign = rep(c("positive", "negative"), length.out = igraph::ecount(graph)),
  stringsAsFactors = FALSE
)
edge_table$source_display <- clean_taxon_label(edge_table$source)
edge_table$target_display <- clean_taxon_label(edge_table$target)

paths <- plot_network(
  graph = graph,
  node_table = node_table,
  edge_table = edge_table,
  output_png = file.path(job_dir, "figures", "network_plot.png"),
  output_pdf = file.path(job_dir, "figures", "network_plot.pdf")
)

stopifnot(file.exists(file.path(job_dir, "figures", "network_plot.png")))
stopifnot(file.exists(file.path(job_dir, "figures", "network_plot_overview.png")))
stopifnot(file.exists(file.path(job_dir, "figures", "network_plot_labelled.png")))
stopifnot(file.exists(file.path(job_dir, "figures", "network_degree_barplot.png")))
stopifnot(identical(as.integer(paths$core_node_count), 30L))
