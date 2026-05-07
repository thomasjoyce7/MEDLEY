# Script to extract and save k-means++ clusters for subgroup analysis 

# Load libraries
library(readr)
library(tidyverse)
library(cluster)
library(clusterCrit)
library(ClusterR)
library(uwot)
library(proxy)
library(scales)
library(patchwork)

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

# k-means clustering settings 
k_means_settings <- list(optimal_k=5, n_pcs=2, center=TRUE, scale=FALSE, n_repeats=1)
# n_pcs: Number of principle components to use for clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# optimal_k: optimal cluster number determined by the elbow method
# n_repeats: number of clustering repetitions to perform 

# Run k-means++ clustering ------------------------------------------------------------------------------

# Function to perform multiple repetitions of PCA + k-means++
pca_kmeans_repeat <- function(embeddings_df, optimal_k = k_means_settings$optimal_k, 
                              n_pcs = k_means_settings$n_pcs, center = k_means_settings$center, 
                              scale = k_means_settings$scale, n_repeats = k_means_settings$n_repeats) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # Initialize vectors to store internal validation metrics for clustering 
  sil_scores <- numeric(n_repeats)
  chi_values <- numeric(n_repeats)
  dbi_values <- numeric(n_repeats)
  
  # Apply PCA for embedding dimension reduction
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  pca_data <- pca_result$x[, 1:n_pcs]
  
  # k-means++ loop 
  for (i in 1:n_repeats) {
    
    kmeans_result <- KMeans_rcpp(pca_data,
                                 clusters = optimal_k,
                                 num_init = 1, # One init per repetition
                                 initializer = "kmeans++",
                                 max_iters = 100,
                                 seed = 42 + i) # Change the seed at each repetition
    
    clusters <- as.integer(kmeans_result$clusters)
    
    # Evaluation metrics in PCA space 
    
    # Silhouette score
    sil <- silhouette(clusters, dist(pca_data, method="euclidean"))
    sil_scores[i] <- mean(sil[, 3])
    
    # CHI (Higher is better)
    chi <- intCriteria(traj = as.matrix(pca_data),
                       part = clusters,
                       crit = "Calinski_Harabasz")
    chi_values[i] <- chi$calinski_harabasz
    
    # DBI (Lower is better)
    dbi <- intCriteria(traj = as.matrix(pca_data),
                       part = clusters,
                       crit = "Davies_Bouldin")
    dbi_values[i] <- dbi$davies_bouldin
  }
  
  # Return summary statistics
  return(list(
    cluster_assignments = clusters, 
    method = short_name, 
    mean_silhouette = mean(sil_scores),
    sd_silhouette = sd(sil_scores),
    mean_chi = mean(chi_values),
    sd_chi = sd(chi_values),
    mean_dbi = mean(dbi_values),
    sd_dbi = sd(dbi_values),
    num_clusters = optimal_k 
  ))
}

# Obtain clustering results 
all_clustering_results <- lapply(embeddings_all_templates$X, pca_kmeans_repeat)

# Store subject_id as a data frame 
subject_id <- data.frame(subject_id = embeddings_all_templates$subject_id)

# Save k-means cluster assignments for each template -------------------------------------

# Function to write cluster assignments to a CSV file 
save_clusters <- function(embedding_name, all_clustering_results, subject_id) {
  
  # Extract cluster assignments correctly
  clusters <- all_clustering_results[[embedding_name]]$cluster_assignments
  
  final_clusters <- tibble(
    subject_id = subject_id$subject_id,
    cluster    = clusters
  )
  
  clustering_suffix <- encode_kmeans_clustering_settings(k_means_settings)
  
  output_path <- file.path(
    "data/cohorts/01_POD/kmeans_clusters",
    paste0(
      "pod_kmeans_", embedding_name,
      "_clusters_", clustering_suffix, ".csv"
    )
  )
  
  write_csv(final_clusters, output_path)
}

for (embedding in embedding_names){
  save_clusters(embedding_name = embedding, all_clustering_results = all_clustering_results, subject_id = subject_id)
}