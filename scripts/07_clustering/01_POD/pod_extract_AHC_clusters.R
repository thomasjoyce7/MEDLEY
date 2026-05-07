# Script to extract and save AHC clusters for subgroup analysis 

# Load libraries
library(readr)
library(tidyverse)
library(cluster)
library(clusterCrit)
library(ClusterR)
library(uwot)
library(proxy)
library(dbscan)

# Load helper functions
source("scripts/00_utils/00_utils.R")

embedding_names <- c("cl_list")

# Provide embedding names to load and process 
# embedding_names <- c("cl_list", "cl_text",
#                       "gt_base_2k_list", "gt_base_2k_text",
#                       "lf_list", "lf_text", 
#                       "bio_clin_bert_list", "bio_clin_bert_text", 
#                       "gt_base_list", "gt_base_text", 
#                       "bert_list", "bert_text")

# Load embeddings from embedding_names using helper function 
embeddings_all_templates <- load_pod_embeddings(embedding_names)

# Settings --------------------------------------------------------------------------------------

# AHC clustering settings 
ahc_settings <- list(optimal_k=6, n_pcs=2, center=TRUE, scale=FALSE)
# n_pcs: Number of principle components to use for clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# optimal_k: optimal cluster number determined by the Silhouette method

# Run AHC clustering ------------------------------------------------------------------------------

# Function to perform PCA + AHC
# Note: PCA and AHC are both deterministic, so no need to perform multiple repetitions
pca_ahc <- function(embeddings_df, optimal_k = ahc_settings$optimal_k, 
                    n_pcs = ahc_settings$n_pcs, 
                    center = ahc_settings$center, 
                    scale = ahc_settings$scale) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # First, apply PCA for dimension reduction 
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  pca_data <- pca_result$x[, 1:n_pcs]
  
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
    cluster_assignments = clusters, 
    method = short_name,
    silhouette = sil_score,
    chi = chi_value,
    dbi = dbi_value,
    num_clusters = optimal_k
  ))
}

# Obtain clustering results 
all_clustering_results <- lapply(embeddings_all_templates$X, pca_ahc)

# Store subject_id as a data frame 
subject_id <- data.frame(subject_id = embeddings_all_templates$subject_id)

# Save AHC cluster assignments for each template -------------------------------------

# Function to write cluster assignments to a CSV file 
save_clusters <- function(embedding_name, all_clustering_results, subject_id) {
  
  # Extract cluster assignments correctly
  clusters <- all_clustering_results[[embedding_name]]$cluster_assignments
  
  final_clusters <- tibble(
    subject_id = subject_id$subject_id,
    cluster    = clusters
  )
  
  clustering_suffix <- encode_ahc_clustering_settings(ahc_settings)
  
  output_path <- file.path(
    "data/cohorts/02_POD/AHC_clusters",
    paste0(
      "pod_ahc_", embedding_name,
      "_clusters_", clustering_suffix, ".csv"
    )
  )
  
  write_csv(final_clusters, output_path)
}

for (embedding in embedding_names){
  save_clusters(embedding_name = embedding, all_clustering_results = all_clustering_results, subject_id = subject_id)
}