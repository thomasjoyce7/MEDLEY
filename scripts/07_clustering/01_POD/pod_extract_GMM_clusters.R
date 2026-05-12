# Script to extract and save GMM clusters for subgroup analysis 

# Load libraries
library(readr)
library(uwot)       
library(mclust)  
library(cluster)
library(clusterCrit)
library(ClusterR)
library(tidyverse)  
library(proxy)
library(patchwork)

# Load helper functions
source("scripts/00_utils/00_utils.R")

# Provide embedding names to load and process (condense list if desired) 
embedding_names <- c("cl_list", "cl_text",
                       "gt_base_2k_list", "gt_base_2k_text",
                       "lf_list", "lf_text", 
                       "bio_clin_bert_list", "bio_clin_bert_text", 
                       "gt_base_list", "gt_base_text", 
                       "bert_list", "bert_text")

# Load embeddings from embedding_names using helper function 
embeddings_all_templates <- load_pod_embeddings(embedding_names)

# Settings --------------------------------------------------------------------------------------

# GMM clustering settings 
gmm_settings <- list(optimal_G=6, n_pcs=2, omit_first_pc=FALSE, center=TRUE, scale=FALSE, n_repeats=1)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# optimal_k: optimal number of Gaussian components (i.e., clusters) determined by the elbow method
# n_repeats: number of clustering repetitions to perform 

# Run GMM clustering ------------------------------------------------------------------------------

# Function to perform multiple repetitions of PCA + GMM
pca_gmm_repeat <- function(embeddings_df, optimal_G = gmm_settings$optimal_G, 
                           n_pcs = gmm_settings$n_pcs, center = gmm_settings$center, 
                           scale = gmm_settings$scale, n_repeats = gmm_settings$n_repeats) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # Initialize vectors to store internal validation metrics for clustering 
  sil_scores <- numeric(n_repeats)
  chi_values <- numeric(n_repeats)
  dbi_values <- numeric(n_repeats)
  
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  pca_data <- pca_result$x[, 1:n_pcs]
  
  for (i in 1:n_repeats) {
    
    # Change the random seed for each repetition 
    set.seed(42 + i)
    gmm_fit <- Mclust(pca_data, G = optimal_G, modelNames = c("VVV"))
    
    clusters <- as.integer(gmm_fit$classification)
    
    # Evaluation metrics in PCA space
    
    # Silhouette score 
    sil <- silhouette(clusters, dist(pca_data, method="euclidean"))
    sil_scores[i] <- mean(sil[, 3])
    
    # CHI
    chi <- intCriteria(traj = as.matrix(pca_data),
                       part = clusters,
                       crit = "Calinski_Harabasz")
    chi_values[i] <- chi$calinski_harabasz
    
    # DBI
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
    num_clusters = optimal_G 
  ))
}

# Obtain clustering results 
all_clustering_results <- lapply(embeddings_all_templates$X, pca_gmm_repeat)

# Store subject_id as a data frame 
subject_id <- data.frame(subject_id = embeddings_all_templates$subject_id)

# Save GMM cluster assignments for each template -------------------------------------

# Function to write cluster assignments to a CSV file 
save_clusters <- function(embedding_name, all_clustering_results, subject_id) {
  
  # Extract cluster assignments correctly
  clusters <- all_clustering_results[[embedding_name]]$cluster_assignments
  
  final_clusters <- tibble(
    subject_id = subject_id$subject_id,
    cluster    = clusters
  )
  
  clustering_suffix <- encode_gmm_clustering_settings(gmm_settings)
  
  output_path <- file.path(
    "data/cohorts/01_POD/GMM_clusters",
    paste0(
      "pod_gmm_", embedding_name,
      "_clusters_", clustering_suffix, ".csv"
    )
  )
  
  write_csv(final_clusters, output_path)
}

for (embedding in embedding_names){
  save_clusters(embedding_name = embedding, all_clustering_results = all_clustering_results, subject_id = subject_id)
}