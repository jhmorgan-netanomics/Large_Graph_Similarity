# Compare_clustering.R
# Jonathan H. Morgan, Ph.D.
# 23 October 2025

# Notes:
# Compare Barrat (undirected) vs. Clemente–Grassi (directed) local clustering.
# Toy Network Comparison

# Clear Out Console Script
  cat("\014")
  rm(list = ls(all.names = TRUE))

# Options
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())

# Setting Working Directory to the Test Data Directory
  setwd("D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data")
  getwd()
  
# Parameters
  args <- commandArgs(trailingOnly = TRUE)
  EDGE_CSV <- if (length(args) >= 1) args[1] else ""  # path/to/edges.csv
  ROUND    <- 6                                       # rounding for display
  OUT_CSV  <- "clustering_comparison.csv"
  
################################################
#   IMPORT AGENT X AGENT - ALL-COMMUNICATION   #
################################################

# Import & Check Graph
  all_comm <- igraph::read_graph("D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/agent_agent_all_communication.graphml", format = "graphml")
  summary(all_comm)
  
################
#   PACKAGES   #
################

suppressPackageStartupMessages({
  library(igraph)
  library(DirectedClustering)
})
  
############################
#   CREATING TEST GRAPHS   #
############################

# Create Edgelist
  if (nzchar(EDGE_CSV) && file.exists(EDGE_CSV)) {
    edges <- read.csv(EDGE_CSV, stringsAsFactors = FALSE)
  } else {
    message("No CSV provided. Using a small toy example…")
    edges <- data.frame(
      src = c("A","A","B","B","C","D","E"),
      dst = c("B","C","C","D","A","A","B"),
      weight = c(1, 5, 2, 3, 1, 4, 1),
      stringsAsFactors = FALSE
    )
  }

# Ensure columns exist
  if (!all(c("src","dst") %in% names(edges))) {
    stop("Edge list must contain 'src' and 'dst' columns.")
  }
  has_wt <- "weight" %in% names(edges)
  
# Build graphs
  g_dir <- graph_from_data_frame(edges, directed = TRUE)
  if (has_wt) E(g_dir)$weight <- edges$weight

# Adding Weights
  A_dir <- as.matrix(as_adjacency_matrix(g_dir, sparse = FALSE,
                                         attr = if (has_wt) "weight" else NULL))
  
# Undirected Graph for Barrat
  comb <- list(weight = "max")
  g_undir <- as_undirected(g_dir, mode = "collapse", edge.attr.comb = comb)
  A_undir <- as.matrix(as_adjacency_matrix(g_undir, sparse = FALSE, attr = "weight"))
  
##############################
#   CALCULATING TRANSIVITY   #
##############################

# Calculating Classic Transitivity Measure
  igraph::transitivity(all_comm, type ="global")
  
# Calculating Local
  igraph::transitivity(all_comm, type ="localaverage")

#################################################
#   CALCULATING LOCAL CLUSTERING COEFFICIENTS   #
#################################################

# Directed: Clemente & Grassi (returns cycle/middleman/in/out/total)
  cg <- ClustBCG(A_dir, type = "directed")
  print(c(cg$GlobalcycleCC, cg$GlobalmiddlemanCC, cg$GlobalinCC, cg$GlobaloutCC, cg$GlobaltotalCC))

# Barret Local Weighted Clustering Coefficient 
  barr <- ClustBCG(A_undir, type = "undirected")  # list(LocalCC, GlobalCC)
  barr_local <- barr$LocalCC

#########################
#   COMPARING METHODS   #
#########################

# Isolating Vertex names
  nodes_dir  <- rownames(A_dir)
  nodes_u    <- rownames(A_undir)

# Align by node name
  all_nodes <- sort(unique(c(nodes_dir, nodes_u)))
  lookup <- function(x, nm) { y <- rep(NA_real_, length(all_nodes)); names(y) <- all_nodes; y[names(x)] <- x; y }

# Create Comparison Table
  cmp <- data.frame(
    node            = all_nodes,
    cg_cycle =  cg$cycleCC,
    cg_middleman = cg$middlemanCC,
    cg_in = cg$inCC,
    cg_out = cg$outCC,
    cg_total = cg$totalCC,
    barrat_local = barr_local,
    stringsAsFactors = FALSE
  )

# Round for display
  cmp_round <- within(cmp, {
    cg_cycle     <- round(cg_cycle,     ROUND)
    cg_middleman <- round(cg_middleman, ROUND)
    cg_in        <- round(cg_in,        ROUND)
    cg_out       <- round(cg_out,       ROUND)
    cg_total     <- round(cg_total,     ROUND)
    barrat_local <- round(barrat_local, ROUND)
  })

# Output to Console
  cat("\n--- Head (rounded) ---\n")
  print(head(cmp_round, 10), row.names = FALSE)

# Save to CSV
  readr::write_csv(cmp, file=OUT_CSV )
  cat("\nWrote:", OUT_CSV, "\n")

# Notes
  cat("\nNotes:\n")
  cat("- cg_* columns are Clemente & Grassi (2018) directed weighted local clustering components.\n")
  cat("- barrat_local is Barrat (2004) undirected weighted local clustering (LocalCC).\n")
  cat("- Undirected edge weights were combined with 'max' when collapsing directions.\n")
  cat("- If your study prefers 'sum' or 'mean', change edge.attr.comb above.\n")
