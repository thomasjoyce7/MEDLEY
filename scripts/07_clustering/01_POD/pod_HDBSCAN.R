# HDBSCAN clustering for the POD application

# Load libraries
library(readr)
library(tidyverse)
library(cluster)
library(clusterCrit)
library(dbscan)
library(uwot)
library(proxy)

# Load helper functions
source("scripts/00_utils/00_utils.R")

# Option to use large encoder models (instead of smaller models)
large_encoders <- FALSE

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

# HDBSCAN clustering settings 
hdbscan_settings <- list(n_pcs=2, omit_first_pc=FALSE, center=TRUE, scale=FALSE, minPts=100)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# minPts: The minimum number of points to assign to a cluster

if (large_encoders == TRUE){
  clustering_suffix <- encode_hdbscan_clustering_settings(hdbscan_settings)
  clustering_output_path <- file.path(
    "data/results/clustering/01_POD/HDBSCAN",
    paste0("pod_hdbscan_results_large_encoders_", clustering_suffix, ".csv"))
} else {
  clustering_suffix <- encode_hdbscan_clustering_settings(hdbscan_settings)
  clustering_output_path <- file.path(
    "data/results/clustering/01_POD/HDBSCAN",
    paste0("pod_hdbscan_results_", clustering_suffix, ".csv"))
}

# Run clustering ------------------------------------------------------------------------------

# Function to perform multiple repetitions of PCA + HDBSCAN
pca_hdbscan <- function(embeddings_df, minPts = hdbscan_settings$minPts, 
                           n_pcs = hdbscan_settings$n_pcs,
                        omit_first_pc = hdbscan_settings$omit_first_pc, 
                        center = hdbscan_settings$center, 
                           scale = hdbscan_settings$scale) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # Apply PCA for dimension reduction
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  
  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
  # HDBSCAN
  hdb <- hdbscan(pca_data, minPts=minPts)
  clusters <- hdb$cluster
  
  # Extract the number of clusters
  num_clusters <- length(unique(clusters[clusters > 0]))
  
  # Remove noise points (cluster == 0)
  non_noise_idx <- which(clusters > 0)
  
  pca_clean <- pca_data[non_noise_idx, ]
  clusters_clean <- clusters[non_noise_idx]
  clusters_clean <- as.integer(clusters_clean)
  
  # Silhouette
  sil <- silhouette(clusters_clean, dist(pca_clean, method="euclidean"))
  sil_score <- mean(sil[, 3])
  
  # CHI
  chi <- intCriteria(traj = as.matrix(pca_clean),
                     part = clusters_clean,
                     crit = "Calinski_Harabasz")
  chi_value <- chi$calinski_harabasz
  
  # DBI
  dbi <- intCriteria(traj = as.matrix(pca_clean),
                     part = clusters_clean,
                     crit = "Davies_Bouldin")
  dbi_value <- dbi$davies_bouldin
  
  # Return performance summary
  return(list(
    method = short_name,
    silhouette = sil_score,
    chi = chi_value,
    dbi = dbi_value,
    num_clusters = num_clusters))
}


  
clustering_results <- lapply(embeddings_all_templates$X, pca_hdbscan)
  
# Save results
pca_hdbscan_results <- bind_rows(clustering_results)
pca_hdbscan_results <- as.data.frame(pca_hdbscan_results)
write_csv(pca_hdbscan_results, clustering_output_path)
  
message("PCA + HDBSCAN clustering complete.")



