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
  library(centiserve)
  library(Matrix)
})
  
#################
#   FUNCTIONS   #
#################
  
# SALSA Power Function
  salsa_power <- function(g, score = c("hub","authority"), weights = NULL,
                          tol = 1e-9, maxit = 1e4, norm = c("l1","l2")) {
    # Creating Matrix Elements & Parameters
      score <- match.arg(score); norm <- match.arg(norm)
      A <- as_adjacency_matrix(g, attr = weights, sparse = TRUE)   # weights=NULL ⇒ binary
      n <- nrow(A)
    
      outdeg <- rowSums(A); outdeg[outdeg == 0] <- 1
      indeg  <- colSums(A); indeg[indeg == 0]  <- 1
    
      x <- rep(1/n, n)
      normfn <- if (norm == "l1") function(v){ s <- sum(abs(v)); if (s>0) v/s else v }
      else               function(v){ s <- sqrt(sum(v*v)); if (s>0) v/s else v }
    
    # Applying Iterative Max Power Method
      for (it in 1:maxit) {
        if (score == "hub") {
          # y = D_out^{-1} * A * D_in^{-1} * A^T * x
            y <- as.numeric(A %*% ( (as.numeric(crossprod(A, x)) / indeg) )) / outdeg
        } else {
          # y = D_in^{-1} * A^T * D_out^{-1} * A * x
            y <- (as.numeric(crossprod(A, (as.numeric(A %*% x) / outdeg))) ) / indeg
        }
        y <- normfn(y)
        if (sum(abs(y - x)) < tol) { x <- y; break }
        x <- y
      }
      
    # Returning Retuls
      names(x) <- V(g)$name
      return(x)
  }
  
  salsa_power_cs <- function(g, score = c("hub","authority"), weights = NULL,
                             tol = 1e-9, maxit = 1e4, norm = c("l1","l2")) {
    # Matrix Elements
      score <- match.arg(score); norm <- match.arg(norm)
      A <- as_adjacency_matrix(g, attr = weights, sparse = TRUE)
      n <- nrow(A)
      outdeg <- as.numeric(rowSums(A))
      indeg  <- as.numeric(colSums(A))
    
      x <- rep(1/n, n)
      normfn <- if (norm == "l1") function(v){ s <- sum(abs(v)); if (s>0) v/s else v }
      else               function(v){ s <- sqrt(sum(v*v)); if (s>0) v/s else v }
      
    # Applying Power Method
      for (it in 1:maxit) {
        if (score == "hub") {
          # tmp1 = A^T x ; divide by indeg, but zero where indeg==0
            tmp1 <- as.numeric(crossprod(A, x))
            zin  <- indeg == 0
            if (any(!zin)) tmp1[!zin] <- tmp1[!zin] / indeg[!zin]
            tmp1[zin] <- 0.0
            
          # tmp2 = A tmp1 ; divide by outdeg, but zero where outdeg==0
            tmp2 <- as.numeric(A %*% tmp1)
            zout <- outdeg == 0
            if (any(!zout)) tmp2[!zout] <- tmp2[!zout] / outdeg[!zout]
            y <- tmp2
            y[zout] <- 0.0
        } else {
          # tmp1 = A x ; divide by outdeg (zero if outdeg==0)
            tmp1 <- as.numeric(A %*% x)
            zout <- outdeg == 0
            if (any(!zout)) tmp1[!zout] <- tmp1[!zout] / outdeg[!zout]
            tmp1[zout] <- 0.0
            
          # tmp2 = A^T tmp1 ; divide by indeg (zero if indeg==0)
            tmp2 <- as.numeric(crossprod(A, tmp1))
            zin  <- indeg == 0
            if (any(!zin)) tmp2[!zin] <- tmp2[!zin] / indeg[!zin]
            y <- tmp2
            y[zin] <- 0.0
        }
        y <- normfn(y)
        if (sum(abs(y - x)) < tol) { x <- y; break }
        x <- y
      }
      
    # Return Results
      names(x) <- V(g)$name
      return(x)
  }
  
# Helper Function for Normalization for the Purposes of Comparison
  l1norm <- function(v) { s <- sum(abs(v)); if (s > 0) v / s else v }
  
# Generate Matrices & Print Them
  score_metrics <- function(x, y) {
    stopifnot(!is.null(names(x)), !is.null(names(y)))
    cmn <- intersect(names(x), names(y))
    x <- x[cmn]; y <- y[cmn]
    list(
      n        = length(cmn),
      L1       = sum(abs(x - y)),
      L2       = sqrt(sum((x - y)^2)),
      pearson  = suppressWarnings(cor(x, y, method = "pearson")),
      spearman = suppressWarnings(cor(x, y, method = "spearman"))
    )
  }
  
  print_metrics <- function(label, m) {
    cat(sprintf("[%s] n=%d  L1: %.6g  L2: %.6g  Pearson: %.6f  Spearman: %.6f\n",
                label, m$n, m$L1, m$L2, m$pearson, m$spearman))
    invisible(m)
  }
  
# Test Function
  run_salsa_scc_check <- function(g, weights_attr = NULL,
                                  tol = 1e-9, maxit = 1e4, norm = "l1",
                                  pass_L1 = 1e-6, pass_corr = 0.999) {
    # Matrix Elements
      comp   <- igraph::components(g, mode = "strong")
      scc_id <- which.max(comp$csize)
      idx    <- which(comp$membership == scc_id)
      gscc   <- igraph::induced_subgraph(g, idx)
    
    # Reporting Test Initiation
      cat(sprintf("\nLargest SCC has %d nodes and %d edges.\n",
                  igraph::vcount(gscc), igraph::ecount(gscc)))
    
    # centiserve SALSA on SCC (binary)
      hub_cs  <- l1norm(centiserve::salsa(gscc, score = "hub"))
      auth_cs <- l1norm(centiserve::salsa(gscc, score = "authority"))
      names(hub_cs)  <- V(gscc)$name
      names(auth_cs) <- V(gscc)$name
      
    # power method SALSA on SCC (use same binary assumption for parity)
      hub_pm  <- l1norm(salsa_power(gscc, score = "hub",        weights = NULL,
                                    tol = tol, maxit = maxit, norm = norm))
      auth_pm <- l1norm(salsa_power(gscc, score = "authority",  weights = NULL,
                                    tol = tol, maxit = maxit, norm = norm))
    
    # Pring Results
      cat("\nComparing SALSA hub on SCC (centiserve vs power-method):\n")
      m_hub  <- print_metrics("Hub", score_metrics(hub_cs, hub_pm))
    
      cat("Comparing SALSA authority on SCC (centiserve vs power-method):\n")
      m_auth <- print_metrics("Authority", score_metrics(auth_cs, auth_pm))
      
      ok <- (m_hub$L1 <= pass_L1 && m_auth$L1 <= pass_L1) ||
        (m_hub$pearson >= pass_corr && m_auth$pearson >= pass_corr)
      
      if (ok) cat("\n✅ SCC parity looks good. Proceeding to full-graph power-method…\n")
      invisible(list(ok = ok,
                     hub_centiserve = hub_cs, auth_centiserve = auth_cs,
                     hub_power = hub_pm, auth_power = auth_pm,
                     hub_metrics = m_hub, auth_metrics = m_auth,
                     scc_graph = gscc))
  }
  
# Compute Salsa Results for Export after Tests
  compute_full_graph_salsa <- function(g, weights_attr = NULL,
                                       tol = 1e-9, maxit = 1e4, norm = "l1") {
    hub_full  <- salsa_power(g, score = "hub",        weights = weights_attr,
                             tol = tol, maxit = maxit, norm = norm)
    auth_full <- salsa_power(g, score = "authority",  weights = weights_attr,
                             tol = tol, maxit = maxit, norm = norm)
    list(hub = hub_full, authority = auth_full)
  }

##############################
#   CALCULATING TRANSIVITY   #
##############################
  
# Calculating Classic Transitivity Measure
  igraph::transitivity(all_comm, type ="global")
  
# Calculating Local
  igraph::transitivity(all_comm, type ="localaverage")
  
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
  
#############################################################################
#   CALCULATING WEIGHTED DIRECTED CLUSTERING: BALIKATAN ALL COMMUNICATION   #
#############################################################################
  
# Creating Adjacency Matrix
  name_index = data.frame(node_id = as.character(V(all_comm)$name), node = as.character(V(all_comm)$label))
  all_com_adj <- as.matrix(as_adjacency_matrix(all_comm, sparse = FALSE,
                           attr = if (has_wt) "weight" else NULL))
  
# Directed: Clemente & Grassi (returns cycle/middleman/in/out/total)
  all_comm_cg <- ClustBCG(all_com_adj, type = "directed")
  print(c(all_comm_cg$GlobalcycleCC, all_comm_cg$GlobalmiddlemanCC, all_comm_cg$GlobalinCC, 
          all_comm_cg$GlobaloutCC, all_comm_cg$GlobaltotalCC))
  
# Barret Local Weighted Clustering Coefficient 
  comb <- list(weight = "max")
  all_comm_undir <- as_undirected(all_comm, mode = "collapse", edge.attr.comb = comb)
  all_comm_undir_adj <- as.matrix(as_adjacency_matrix(all_comm_undir, sparse = FALSE, attr = "weight"))
  
  barr <- ClustBCG(all_comm_undir_adj, type = "undirected")  
  barr_local <- barr$LocalCC
  
# Isolating Vertex names
  nodes_dir  <- rownames(all_com_adj)
  nodes_u    <- rownames(all_comm_undir_adj)
  
# Align by node name
  all_nodes <- sort(unique( as.numeric(c(nodes_dir, nodes_u))))
  all_nodes <- data.frame(node_id = as.character(all_nodes))
  all_nodes <- dplyr::left_join(all_nodes, name_index, by=c("node_id"))
  
# Create Comparison Table
  cmp <- data.frame(
    node = all_nodes$node,
    cg_cycle = all_comm_cg$cycleCC,
    cg_middleman = all_comm_cg$middlemanCC,
    cg_in = all_comm_cg$inCC,
    cg_out =  all_comm_cg$outCC,
    cg_total = all_comm_cg$totalCC,
    barrat_local = barr_local,
    stringsAsFactors = FALSE
  )
  
# Writing-Out File for Comarison
  save_dir <- c("D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data")
  file_name <- c("Balikatan_Clemente_Grassi.csv")
  readr::write_csv(cmp, file=paste0(save_dir,"/",file_name))
  
###########################################################
#   Calculating Local Clustering Coefficients: Strogatz   #
###########################################################
  
# Transforming the Directed Graph to Meet the Measure Requirements
  all_comm_no_loops <- simplify(all_comm, remove.multiple = TRUE, remove.loops = TRUE)
  all_comm_no_loops <- as_undirected(all_comm_no_loops, mode = "collapse")  # ignore direction for WS local CC

# Calculating Strogratz's Local Clustering Coefficients
  lc <- transitivity(all_comm_no_loops, type = "local", vids = V(all_comm_no_loops), isolates = "zero")
  
# Creating Output Table
  name_index = data.frame(node_id = as.character(V(all_comm_no_loops)$name), node = as.character(V(all_comm_no_loops)$label))
  local_clustering_scores <- data.frame(node_id=names(lc), local_clustering = as.numeric(lc))
  local_clustering_scores <- dplyr::left_join(local_clustering_scores, name_index, by=c("node_id"))
  local_clustering_scores <- local_clustering_scores[c(3,2)]
  
# Outputting for Comparison with Julia Functions
  file_name <- c("Balikatan_Local_Clustering.csv")
  readr::write_csv(local_clustering_scores, file= paste0(save_dir,"/",file_name)) 
  
######################################################
#   CALCULATING SALSA HUB & AUTHORITY CENTRALITIES   #
######################################################
  
# COME BACK HERE TOMORROW!!!!

# Basic Test Function (Barabási–Albert) 
  set.seed(42)
  g <- sample_pa(10, directed = TRUE)   # same as your example
  
# centiserve SALSA (hub) on g
  scores_cs <- centiserve::salsa(g, score = "hub")
  stopifnot(length(scores_cs) == vcount(g))
  stopifnot(all(names(scores_cs) %in% V(g)$name))
  stopifnot(min(scores_cs) >= 0)
  
# power-method SALSA (hub) on the same graph, binary
  scores_cs   <- centiserve::salsa(g, score = "hub"); names(scores_cs) <- V(g)$name
  scores_pm   <- salsa_power_cs(g, score = "hub", weights = NULL)
  
# normalize BOTH to L1 before comparison (centiserve scaling can differ)
  scores_cs_n <- l1norm(scores_cs)
  scores_pm_n <- l1norm(scores_pm)
  
# Print Results
  cat("\n--- Basic Test: BA(10), Hub ---\n")
  print_metrics("Hub (centiserve vs power, L1)", score_metrics(scores_cs_n, scores_pm_n))
  
# Also check authority on the same graph
  scores_cs_a  <- l1norm(centiserve::salsa(g, score = "authority"))
  scores_pm_a  <- l1norm(salsa_power(g, score = "authority", weights = NULL))
  print_metrics("Authority (centiserve vs power, L1)", score_metrics(scores_cs_a, scores_pm_a))

  

