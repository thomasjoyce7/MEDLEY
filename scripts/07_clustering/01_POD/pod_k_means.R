# k-means++ clustering for the POD application

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

# Elbow method settings for determining the optimal cluster number 
elbow_method_settings <- list(n_pcs=20, omit_first_pc=FALSE, center=TRUE, scale=FALSE, k_max=20)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# k_max: Maximum number of clusters to include in the elbow plot 

# k-means clustering settings 
k_means_settings <- list(optimal_k=5, omit_first_pc=FALSE, n_pcs=5, center=TRUE, scale=FALSE, n_repeats=50)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# optimal_k: optimal cluster number determined by the elbow method
# n_repeats: number of clustering repetitions to perform 

# Paths to save elbow method plots and clustering results
elbow_method_suffix <- encode_elbow_method_settings(elbow_method_settings)
elbow_method_output_path <- file.path(
  "figures/clustering/01_POD/k_means",
  paste0("pod_kmeans_elbow_method_grid_", elbow_method_suffix, ".pdf")
)

if (large_encoders == TRUE){
  clustering_suffix <- encode_kmeans_clustering_settings(k_means_settings)
  clustering_output_path <- file.path(
    "data/results/clustering/01_POD/k_means",
    paste0("pod_kmeans_results_large_encoders_", clustering_suffix, ".csv")
  )
} else {
  clustering_suffix <- encode_kmeans_clustering_settings(k_means_settings)
  clustering_output_path <- file.path(
    "data/results/clustering/01_POD/k_means",
    paste0("pod_kmeans_results_", clustering_suffix, ".csv")
  )
}

# Indication of whether the elbow method and/or k-means++ clustering should be performed
run_elbow_method <- FALSE
run_clustering <- TRUE 


# Run clustering code ---------------------------------------------------------------------------

# Function to use the elbow method to find the optimal value of k for each embedding type
elbow_method <- function(embeddings_df, n_pcs = elbow_method_settings$n_pcs,
                         omit_first_pc = elbow_method_settings$omit_first_pc, 
                             center = elbow_method_settings$center, scale = elbow_method_settings$scale, 
                             k_max = elbow_method_settings$k_max){
  
  short_name <- attr(embeddings_df, "short_name")
  
  # First, apply PCA for dimension reduction
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  
  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
  # Elbow method using k-means++ initialization
  wss <- numeric(k_max)
  for (k in 1:k_max) {
    kmeans_result <- KMeans_rcpp(pca_data,
                                 clusters = k,
                                 num_init = 30, #30 initializations, keep the best one based on lowest WCSS
                                 initializer = "kmeans++",
                                 seed = 42)
    wss[k] <- sum(kmeans_result$WCSS_per_cluster)
  }
  
  # Create data frame for plotting
  elbow_df <- data.frame(k = 1:k_max, WSS = wss)
  
  # Return ggplot
  elbow_plot <- ggplot(elbow_df, aes(x = k, y = WSS)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Method:", short_name),
      x = "Number of clusters (k)",
      y = "Total WCSS"
    )+
    theme_minimal(base_size = 8, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", size = 0.4),
      axis.ticks = element_line(color = "black", size = 0.4),
      plot.title = element_text(hjust = 0, face = "bold", size = 9),
    )
  
  return(elbow_plot)
  
}

if (run_elbow_method == TRUE){
  
  # Create the elbow method plots 
  elbow_method_plots <- lapply(embeddings_all_templates$X, elbow_method)
  
  # Grid layout:
  elbow_method_grid_all <- 
    (elbow_method_plots$cl_list | elbow_method_plots$cl_text | elbow_method_plots$cl_llm) /
    (elbow_method_plots$gt_base_2k_list | elbow_method_plots$gt_base_2k_text | elbow_method_plots$gt_base_2k_llm) / 
    (elbow_method_plots$lf_list | elbow_method_plots$lf_text | elbow_method_plots$lf_llm)
  
  # Save plots to results 
  ggsave(elbow_method_output_path, elbow_method_grid_all, width = 9, height = 6, units = "in", dpi = 300, device = cairo_pdf)

  message("Elbow method plots for k-means++ clustering saved successfully.")
  }


# Function to perform multiple repetitions of PCA + k-means++
pca_kmeans_repeat <- function(embeddings_df, optimal_k = k_means_settings$optimal_k, 
                              n_pcs = k_means_settings$n_pcs, 
                              omit_first_pc = k_means_settings$omit_first_pc, 
                              center = k_means_settings$center, 
                              scale = k_means_settings$scale, n_repeats = k_means_settings$n_repeats) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # Initialize vectors to store internal validation metrics for clustering 
  sil_scores <- numeric(n_repeats)
  chi_values <- numeric(n_repeats)
  dbi_values <- numeric(n_repeats)
  
  # Apply PCA for embedding dimension reduction
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)

  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
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

if (run_clustering == TRUE){
  
  clustering_results <- lapply(embeddings_all_templates$X, pca_kmeans_repeat)
  
  # Save results
  pca_kmeans_results <- bind_rows(clustering_results)
  pca_kmeans_results <- as.data.frame(pca_kmeans_results)
  write_csv(pca_kmeans_results, clustering_output_path)
  
  message("PCA + k-means++ clustering complete.")
  
}


