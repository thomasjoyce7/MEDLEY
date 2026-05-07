# Agglomerative hierarchical clustering (AHC) for the POD application

# Load libraries
library(readr)
library(tidyverse)
library(cluster)
library(clusterCrit)
library(ClusterR)
library(uwot)
library(proxy)
library(ggdendro)
library(dbscan)
library(patchwork)

# Load helper functions
source("scripts/00_utils/00_utils.R")

# Option to use large encoder models (instead of smaller models)
large_encoders <- TRUE

# Provide embedding names to load and process 
if (large_encoders == TRUE){
  embedding_names <- c("qwen_list", "qwen_text", "qwen_llm",
                       "sfr_list", "sfr_text", "sfr_llm")
} else {
  embedding_names <- c("cl_list", "cl_text","cl_llm",
                       "gt_base_2k_list", "gt_base_2k_text","gt_base_2k_llm",
                       "lf_list", "lf_text", "lf_llm") 
}

# Load embeddings from embedding_names using helper function 
embeddings_all_templates <- load_pod_embeddings(embedding_names)

# Settings --------------------------------------------------------------------------------------

# Silhouette plot settings for determining the optimal cluster number 
silhouette_plot_settings <- list(n_pcs=20, omit_first_pc = FALSE, center=TRUE, scale=FALSE, k_range=2:20)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# k_range: Range of cluster numbers to plot 

# AHC clustering settings 
ahc_settings <- list(optimal_k=6, n_pcs=5, omit_first_pc = FALSE, center=TRUE, scale=FALSE)
# n_pcs: Number of principle components to use for clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# optimal_k: optimal cluster number determined by the Silhouette method

# Paths to save clustering results, Silhouette plots, and dendrograms
silhouette_plot_suffix <- encode_silhouette_plot_settings(silhouette_plot_settings)
silhouette_plot_output_path <- file.path(
  "figures/clustering/01_POD/AHC",
  paste0("pod_ahc_silhouette_grid_", silhouette_plot_suffix, ".pdf")
)

if (large_encoders == TRUE){
  clustering_suffix <- encode_ahc_clustering_settings(ahc_settings)
  clustering_output_path <- file.path(
    "data/results/clustering/01_POD/AHC",
    paste0("pod_ahc_results_large_encoders_", clustering_suffix, ".csv"))
} else {
  clustering_suffix <- encode_ahc_clustering_settings(ahc_settings)
  clustering_output_path <- file.path(
    "data/results/clustering/01_POD/AHC",
    paste0("pod_ahc_results_", clustering_suffix, ".csv"))
}

dendrogram_output_path <- file.path(
  "figures/clustering/01_POD/AHC",
  paste0("pod_ahc_dendrogram_grid_", 
         ahc_settings$n_pcs, "pcs_",
         if (ahc_settings$center) "centered_" else "uncentered_",
         if (ahc_settings$scale) "scaled" else "unscaled", ".pdf")
)

# Indication of whether to run the code for Silhouette plots (to determine the optimal cluster number),
# AHC clustering, and dendrogram plots 
run_silhouette_method <- FALSE
run_clustering <- TRUE
run_dendrograms <- FALSE 

# Run clustering -------------------------------------------------------------------------------

ahc_silhouette_plot <- function(embeddings_df, n_pcs = silhouette_plot_settings$n_pcs,
                                omit_first_pc = silhouette_plot_settings$omit_first_pc, 
                         center = silhouette_plot_settings$center, scale = silhouette_plot_settings$scale, 
                         k_range = silhouette_plot_settings$k_range){
  
  short_name <- attr(embeddings_df, "short_name")
  
  # PCA
  pca_result <- prcomp(embeddings_df, center = TRUE, scale. = FALSE)
  
  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
  # Compute the distance matrix for AHC using Euclidean distance
  #pca_data_norm <- pca_data / sqrt(rowSums(pca_data^2) + 1e-8)  # Normalize rows
  dist_matrix <- dist(pca_data, method = "euclidean")
  
  # Apply AHC
  hc <- hclust(dist_matrix, method = "ward.D2") 
  
  # Initialize vector to store Silhouette scores across a range of cluster numbers 
  sil_scores <- numeric(length(k_range))
  
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    clusters <- cutree(hc, k = k)
    clusters <- as.integer(clusters)
    
    # Silhouette
    if (length(unique(clusters)) > 1 && all(table(clusters) > 1)) {
      sil <- silhouette(clusters, dist(pca_data, method="euclidean"))
      sil_scores[i] <- mean(sil[, 3])
    } else {
      sil_scores[i] <- NA
    }}
  
  # Build results table
  results <- data.frame(
    k = k_range,
    silhouette = sil_scores)
  
  # Build silhouette plot
  silhouette_plot <- ggplot(results, aes(x = k, y = silhouette)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Method:", short_name),
      x = "Number of clusters (k)",
      y = "Silhouette score"
    ) +
    scale_y_continuous(limits = c(0, 1))+
    theme_minimal(base_size = 8, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", size = 0.4),
      axis.ticks = element_line(color = "black", size = 0.4),
      plot.title = element_text(hjust = 0, face = "bold", size = 9),
    )
  
  return(silhouette_plot)
  
}

if (run_silhouette_method == TRUE){
  
  # Create the Silhouette plots 
  silhouette_plots <- lapply(embeddings_all_templates$X, ahc_silhouette_plot)
  
  # Grid layout: 
  silhouette_grid_all <- 
    (silhouette_plots$cl_list | silhouette_plots$cl_text | silhouette_plots$cl_llm) /
    (silhouette_plots$gt_base_2k_list | silhouette_plots$gt_base_2k_text | silhouette_plots$gt_base_2k_llm) / 
    (silhouette_plots$lf_list | silhouette_plots$lf_text | silhouette_plots$lf_llm)
  
  # Save plots to results 
  ggsave(silhouette_plot_output_path, silhouette_grid_all, width = 9, height = 6, units = "in", dpi = 300, device = cairo_pdf)
  
  message("Silhouette plots for AHC clustering saved successfully.")
}

# Function to perform PCA + AHC
# Note: PCA and AHC are both deterministic, so no need to perform multiple repetitions
pca_ahc <- function(embeddings_df, optimal_k = ahc_settings$optimal_k, 
                    n_pcs = ahc_settings$n_pcs,
                    omit_first_pc = ahc_settings$omit_first_pc, 
                    center = ahc_settings$center, 
                    scale = ahc_settings$scale) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # First, apply PCA for dimension reduction 
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  
  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
  # Compute the distance matrix for AHC using Euclidean distance
  #pca_data_norm <- pca_data / sqrt(rowSums(pca_data^2) + 1e-8)  # Normalize rows
  dist_matrix <- dist(pca_data, method = "euclidean")
  
  # Apply AHC
  hc <- hclust(dist_matrix, method = "ward.D2") 
  
  # Cut dendrogram to obtain cluster assignments 
  clusters <- cutree(hc, k = optimal_k)
  
  # Evaluation metrics in PCA space
  
  # Silhouette score 
  sil <- silhouette(clusters, dist(pca_data, method="euclidean"))
  sil_score <- mean(sil[, 3])
  
  # CHI
  chi <- intCriteria(traj = as.matrix(pca_data),
                     part = clusters,
                     crit = "Calinski_Harabasz")
  chi_value <- chi$calinski_harabasz
  
  # DBI
  dbi <- intCriteria(traj = as.matrix(pca_data),
                     part = clusters,
                     crit = "Davies_Bouldin")
  dbi_value <- dbi$davies_bouldin
  
  # Return performance summary
  return(list(
    method = short_name,
    silhouette = sil_score,
    chi = chi_value,
    dbi = dbi_value,
    num_clusters = optimal_k
  ))
}

if (run_clustering == TRUE){
  
  clustering_results <- lapply(embeddings_all_templates$X, pca_ahc)
  
  # Save results
  pca_ahc_results <- bind_rows(clustering_results)
  pca_ahc_results <- as.data.frame(pca_ahc_results)
  write_csv(pca_ahc_results, clustering_output_path)
  
  message("PCA + AHC clustering complete.")
  
}

# AHC Dendrograms ---------------------------------------------------------------------------

# Function to create AHC dendrograms for cluster hierarchy visualization 
ahc_dendrogram <- function(embeddings_df, 
                           n_pcs = ahc_settings$n_pcs, 
                           omit_first_pc = ahc_settings$omit_first_pc, 
                           center = ahc_settings$center, 
                           scale = ahc_settings$scale) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # First, apply PCA for dimension reduction 
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  
  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
  # Compute the distance matrix for AHC using Euclidean distance
  #pca_data_norm <- pca_data / sqrt(rowSums(pca_data^2) + 1e-8)  # Normalize rows
  dist_matrix <- dist(pca_data, method = "euclidean")
  
  # Apply AHC
  hc <- hclust(dist_matrix, method = "ward.D2") 
  
  # Create dendrogram using ggplot
  dendrogram <- ggdendrogram(hc, rotate = FALSE, size = 0.3, theme_dendro = FALSE) +
    ggtitle(paste("Method:", short_name)) + 
    ylab("Ward linkage height") +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(hjust = 0, face="bold", size=9),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 8)
    )
  
  return(dendrogram)
}

if (run_dendrograms == TRUE){
  
  # Create AHC dendrograms  
  dendrogram_plots <- lapply(embeddings_all_templates$X, ahc_dendrogram)

  # Grid layout: 
  dendrogram_grid_all <- 
    (dendrogram_plots$cl_list | dendrogram_plots$cl_text | dendrogram_plots$cl_llm) /
    (dendrogram_plots$gt_base_2k_list | dendrogram_plots$gt_base_2k_text | dendrogram_plots$gt_base_2k_llm) / 
    (dendrogram_plots$lf_list | dendrogram_plots$lf_text | dendrogram_plots$lf_llm)
  
  # Save plots to results 
  ggsave(dendrogram_output_path, dendrogram_grid_all, width = 9, height = 6, units = "in", dpi = 300, device = cairo_pdf)
  
  message("Dendrograms for AHC clustering saved successfully.")
}



