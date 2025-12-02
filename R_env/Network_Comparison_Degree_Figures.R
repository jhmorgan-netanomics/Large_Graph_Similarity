# Degree Figures
# Jonathan H. Morgan, Ph.D.
# 1 December 2025

# Clear Out Console Script
  cat("\014")
  rm(list = ls(all.names = TRUE))

# Options
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())

# Setting Working Directory to the Test Data Directory
  setwd("D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Example_Outputs")
  getwd()
  
#################
#   FUNCTIONS   #
#################
  
# Ticks helper (matches your Hmisc usage; installs if needed)
  dataplot_tick_function <- function(major_tick_length = 0.035, minor_tick_ratio = 0.25) {
    if (!requireNamespace("Hmisc", quietly = TRUE)) {
      install.packages("Hmisc", repos = "https://cloud.r-project.org")
    }
    Hmisc::minor.tick(nx = 2, ny = 2, tick.ratio = minor_tick_ratio)
    Hmisc::minor.tick(nx = 2, ny = 2, tick.ratio = -minor_tick_ratio)
    axis(2, tck = 1, tck = -major_tick_length, labels = FALSE)
    axis(1, tck = 1, tck = -major_tick_length, labels = FALSE)
  }
  
###################
#   IMPORT DATA   #
###################
  
# Balikatan Node Level Measures
  balikatatan_node_level <- readr::read_csv(file="D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Example_Outputs/balikatan_pac_rim_dir/Balikatan_2022_node_measures.csv")
  
# Pac Rim Node Level Measures
  pac_rim_node_level <- readr::read_csv(file="D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Example_Outputs/balikatan_pac_rim_dir/PacRim_Day1_node_measures.csv")
  
# Pac Sentry Node Level Measures
  pac_sentry_node_level <- readr::read_csv(file="D:/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Example_Outputs/balikatan_pac_sentry_dir/PacSentry_2025_node_measures.csv")
  
######################################
#   VISUALIZE DEGREE DISTRIBUTIONS   #
######################################
   
# Create Total Degree Density Variables
  balikatan_x <- density(asinh(balikatatan_node_level$total_degree))$x
  balikatan_y <- density(asinh(balikatatan_node_level$total_degree))$y
  balikatan_normalized_y <- balikatan_y/max(balikatan_y)
  
  pac_rim_x <- density(asinh(pac_rim_node_level$total_degree))$x
  pac_rim_y <- density(asinh(pac_rim_node_level$total_degree))$y
  pac_rim_normalized_y <- pac_rim_y /max(pac_rim_y)
  
  pac_sentry_x <- density(asinh(pac_sentry_node_level$total_degree))$x
  pac_sentry_y <- density(asinh(pac_sentry_node_level$total_degree))$y
  pac_sentry_normalized_y <- pac_sentry_y /max(pac_sentry_y)
  max(pac_sentry_x)
  
# Create Weighted Total Degree Density Variables
  balikatan_w_x <- density(asinh(balikatatan_node_level$weighted_total_degree))$x
  balikatan_w_y <- density(asinh(balikatatan_node_level$weighted_total_degree))$y
  balikatan_normalized_w_y <- balikatan_w_y/max(balikatan_w_y)
  
  pac_rim_w_x <- density(asinh(pac_rim_node_level$weighted_total_degree))$x
  pac_rim_w_y <- density(asinh(pac_rim_node_level$weighted_total_degree))$y
  pac_rim_normalized_w_y <- pac_rim_w_y /max(pac_rim_w_y)
  
  pac_sentry_w_x <- density(asinh(pac_sentry_node_level$weighted_total_degree))$x
  pac_sentry_w_y <- density(asinh(pac_sentry_node_level$weighted_total_degree))$y
  pac_sentry_normalized_w_y <- pac_sentry_w_y /max(pac_sentry_w_y)

# Plotting Degree
  
  # Visualization Matrix
    layout(matrix(1:2, nrow = 2, ncol = 1))
  
  # Total Degree
    par(mar = c(4, 4, 2.5, 1.5), family = "serif")
    plot(NA, type = "n",
         xlim=c(min(pac_sentry_x), max(pac_sentry_x)), ylim=c(min(pac_sentry_normalized_y),max(pac_sentry_normalized_y)),
         xlab = " ", ylab = "Normalized Density", tck = 0.015, xaxt = 'n', bty = 'L', las = 1,
         main = "", family = 'serif')
    grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
    
    mtext(side = 1, text = "Inverse Hyperbolic Sine of Total Degree", col = "black", line = 2.75, cex = 1, family = 'serif')
    axis(1, padj = 0.75, tck = 0.015, family="serif")
    dataplot_tick_function(0.015, 0.40)
    
    lines(x= balikatan_x, y = balikatan_normalized_y, col="#4477AA", lwd=2, lty=1)
    lines(x=pac_rim_x, y = pac_rim_normalized_y, col="#EE6677", lwd=2, lty=1)
    lines(x=pac_sentry_x, y = pac_sentry_normalized_y, col="#228833", lwd=2, lty=1)
    
    legend("topright", legend=c("Balikatan 2022", "Pac Rim 2024", "Pac Sentry 2025"), col=c("#4477AA", "#EE6677", "#228833"), 
           lty=1:1, cex=0.95, bty='n')
    
  # Weighted Total Degree
    plot(NA, type = "n",
         xlim=c(min(pac_sentry_w_x), max(pac_sentry_w_x)), ylim=c(min(pac_sentry_normalized_w_y),max(pac_sentry_normalized_w_y)),
         xlab = " ", ylab = "Normalized Density", tck = 0.015, xaxt = 'n', bty = 'L', las = 1,
         main = "", family = 'serif')
    grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
    
    mtext(side = 1, text = "Inverse Hyperbolic Sine of Weighted Total Degree", col = "black", line = 2.75, cex = 1, family = 'serif')
    axis(1, padj = 0.75, tck = 0.015, family="serif")
    dataplot_tick_function(0.015, 0.40)
    
    lines(x= balikatan_w_x, y = balikatan_normalized_w_y, col="#4477AA", lwd=2, lty=1)
    lines(x=pac_rim_w_x, y = pac_rim_normalized_w_y, col="#EE6677", lwd=2, lty=1)
    lines(x=pac_sentry_w_x, y = pac_sentry_normalized_w_y, col="#228833", lwd=2, lty=1)
    
    legend("topright", legend=c("Balikatan 2022", "Pac Rim 2024", "Pac Sentry 2025"), col=c("#4477AA", "#EE6677", "#228833"), 
           lty=1:1, cex=0.95, bty='n')

####################################
#   VISUALIZE INFLUENCE MEASURES   #
####################################
    
# Create Page Rank Density Variables
  balikatan_x <- density(balikatatan_node_level$page_rank)$x
  balikatan_y <- density(balikatatan_node_level$page_rank)$y
  balikatan_normalized_y <- balikatan_y/max(balikatan_y)
  range(balikatatan_node_level$page_rank)
  
  pac_rim_x <- density(pac_rim_node_level$page_rank)$x
  pac_rim_y <- density(pac_rim_node_level$page_rank)$y
  pac_rim_normalized_y <- pac_rim_y /max(pac_rim_y)
  range(pac_rim_node_level$page_rank)
  
  pac_sentry_x <- density(pac_sentry_node_level$page_rank)$x
  pac_sentry_y <- density(pac_sentry_node_level$page_rank)$y
  pac_sentry_normalized_y <- pac_sentry_y /max(pac_sentry_y)
  range(pac_sentry_node_level$page_rank)
  
# SALSA Hub Density Variables
  balikatan_h_x <- density(balikatatan_node_level$salsa_hub)$x
  balikatan_h_y <- density(balikatatan_node_level$salsa_hub)$y
  balikatan_normalized_h_y <- balikatan_h_y/max(balikatan_h_y)
  range(balikatatan_node_level$salsa_hub)
  
  pac_rim_h_x <- density(pac_rim_node_level$salsa_hub)$x
  pac_rim_h_y <- density(pac_rim_node_level$salsa_hub)$y
  pac_rim_normalized_h_y <- pac_rim_h_y /max(pac_rim_h_y)
  range(pac_rim_node_level$salsa_hub)
  
  pac_sentry_h_x <- density(pac_sentry_node_level$salsa_hub)$x
  pac_sentry_h_y <- density(pac_sentry_node_level$salsa_hub)$y
  pac_sentry_normalized_h_y <- pac_sentry_h_y /max(pac_sentry_h_y)
  range(pac_sentry_node_level$salsa_hub)
  
# Plotting Influence Metrics
  
  # Visualization Matrix
    layout(matrix(1:2, nrow = 2, ncol = 1))

  # Page Rank
    par(mar = c(4, 4, 2.5, 1.5), family = "serif")
    plot(NA, type = "n",
         xlim=c(0, max(pac_rim_x)), ylim=c(min(pac_rim_normalized_y),max(pac_rim_normalized_y)),
         xlab = " ", ylab = "Normalized Density", tck = 0.015, xaxt = 'n', bty = 'L', las = 1,
         main = "", family = 'serif')
    grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
    
    mtext(side = 1, text = "Page Rank", col = "black", line = 2.75, cex = 1, family = 'serif')
    axis(1, padj = 0.75, tck = 0.015, family="serif")
    dataplot_tick_function(0.015, 0.40)
    
    lines(x= balikatan_x, y = balikatan_normalized_y, col="#4477AA", lwd=2, lty=1)
    lines(x=pac_rim_x, y = pac_rim_normalized_y, col="#EE6677", lwd=2, lty=1)
    lines(x=pac_sentry_x, y = pac_sentry_normalized_y, col="#228833", lwd=2, lty=1)
    
    legend("topright", legend=c("Balikatan 2022", "Pac Rim 2024", "Pac Sentry 2025"), col=c("#4477AA", "#EE6677", "#228833"), 
           lty=1:1, cex=0.95, bty='n')
    
  # SALSA Hub Centrality
    plot(NA, type = "n",
         xlim=c(0, max(pac_rim_h_x)), ylim=c(min(pac_rim_normalized_h_y),max(pac_rim_normalized_h_y)),
         xlab = " ", ylab = "Normalized Density", tck = 0.015, xaxt = 'n', bty = 'L', las = 1,
         main = "", family = 'serif')
    grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
    
    mtext(side = 1, text = "Hub Centrality", col = "black", line = 2.75, cex = 1, family = 'serif')
    axis(1, padj = 0.75, tck = 0.015, family="serif")
    dataplot_tick_function(0.015, 0.40)
    
    lines(x= balikatan_h_x, y = balikatan_normalized_h_y, col="#4477AA", lwd=2, lty=1)
    lines(x=pac_rim_h_x, y = pac_rim_normalized_h_y, col="#EE6677", lwd=2, lty=1)
    lines(x=pac_sentry_h_x, y = pac_sentry_normalized_h_y, col="#228833", lwd=2, lty=1)
    
    legend("topright", legend=c("Balikatan 2022", "Pac Rim 2024", "Pac Sentry 2025"), col=c("#4477AA", "#EE6677", "#228833"), 
           lty=1:1, cex=0.95, bty='n')
