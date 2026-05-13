# Gaussian Mixture Models (GMM) clustering for the PONV application

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
library(arrow)

# Load helper functions
source("scripts/00_utils/01_utils_ponv_4windows.R")

# Provide embedding names to load and process 
embedding_names <- c("cl_list", "cl_text",
                      "gt_base_2k_list", "gt_base_2k_text",
                      "lf_list", "lf_text",
                      "bio_clin_bert_list", "bio_clin_bert_text",
                      "gt_base_list", "gt_base_text",
                      "bert_list", "bert_text")

# Load embeddings from embedding_names using helper function 
embeddings_all_templates <- load_ponv_embeddings(embedding_names)

# Settings --------------------------------------------------------------------------------------

# Elbow method settings for determining the optimal cluster number 
elbow_method_settings <- list(n_pcs=5, omit_first_pc=FALSE, center=TRUE, scale=FALSE, k_max=20)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering 
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# k_max: Maximum number of clusters to include in the elbow plot 

# GMM clustering settings 
gmm_settings <- list(optimal_G=8, n_pcs=5, omit_first_pc=FALSE, center=TRUE, scale=FALSE, n_repeats=50)
# n_pcs: Number of principle components to use for clustering 
# omit_first_pc: Whether to remove the first PC before clustering
# center: Whether to center the embeddings prior to applying PCA for embedding dimension reduction
# scale: Whether to scale the embeddings prior to applying PCA for embedding dimension reduction 
# optimal_k: optimal number of Gaussian components (i.e., clusters) determined by the elbow method
# n_repeats: number of clustering repetitions to perform 

# Paths to save elbow method plots and clustering results
elbow_method_suffix <- encode_elbow_method_settings(elbow_method_settings)
elbow_method_output_path <- file.path(
  "figures/clustering/02_PONV/GMM",
  paste0("ponv_GMM_elbow_method_grid_4windows_", elbow_method_suffix, ".pdf")
)

elbow_method_long_seq_encoders_output_path <- file.path(
  "figures/clustering/02_PONV/GMM",
  paste0("ponv_GMM_elbow_method_grid_long_seq_encoders_4windows_", elbow_method_suffix, ".pdf")
)

clustering_suffix <- encode_gmm_clustering_settings(gmm_settings)
clustering_output_path <- file.path(
  "data/results/clustering/02_PONV/GMM",
  paste0("ponv_gmm_results_4windows_", clustering_suffix, ".csv")
)

# Indication of whether the elbow method and/or GMM clustering should be performed
run_elbow_method <- TRUE
run_clustering <- TRUE

# Run clustering code ---------------------------------------------------------------------------

# Function to use the elbow method to find the optimal number of clusters for each embedding type
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
  
  # GMM
  set.seed(42)
  gmm_fit <- Mclust(pca_data, G = 1:k_max, modelNames = c("EEI"))
  print(gmm_fit$BIC)
  
  # Extract BIC matrix from gmm_fit
  bic_matrix <- gmm_fit$BIC
  class(bic_matrix) <- NULL
  
  BIC_df <- as.data.frame(bic_matrix) %>% mutate(G = 1:nrow(bic_matrix))
  
  BIC_long <- BIC_df %>%
    pivot_longer(cols = -G, names_to = "model", values_to = "BIC") %>% filter(model == "EEI")
  
  # Line plot
  elbow_plot <- ggplot(BIC_long, aes(x = G, y = -BIC)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Method:", short_name),
      x = "Number of components",
      y = "BIC"
    ) +
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
  elbow_method_grid_long_seq_encoders <- 
    (elbow_method_plots$cl_list | elbow_method_plots$cl_text | elbow_method_plots$cl_llm) /
    (elbow_method_plots$gt_base_2k_list | elbow_method_plots$gt_base_2k_text | elbow_method_plots$gt_base_2k_llm) / 
    (elbow_method_plots$lf_list | elbow_method_plots$lf_text | elbow_method_plots$lf_llm)
  
  elbow_method_grid_normal_seq_encoders <- 
    (elbow_method_plots$bio_clin_bert_list | elbow_method_plots$bio_clin_bert_text | elbow_method_plots$bio_clin_bert_llm) /
    (elbow_method_plots$gt_base_list | elbow_method_plots$gt_base_text | elbow_method_plots$gt_base_llm) / 
    (elbow_method_plots$bert_list | elbow_method_plots$bert_text | elbow_method_plots$bert_llm)
  
  # Combine plots
  elbow_method_grid_all <- 
    wrap_elements(elbow_method_grid_long_seq_encoders) /
    wrap_elements(elbow_method_grid_normal_seq_encoders) +
    plot_layout(nrow = 2, heights = c(3, 3))
  
  # Save plots to results 
  ggsave(elbow_method_output_path, elbow_method_grid_all, width = 9, height = 12, units = "in", dpi = 300, device = cairo_pdf)
  
  # Save elbow method plots for long sequence encoders
  ggsave(elbow_method_long_seq_encoders_output_path, elbow_method_grid_long_seq_encoders, width = 9, height = 6, units = "in", dpi = 300, device = cairo_pdf)
  
  message("Elbow method plots for GMM clustering saved successfully.")
}

# Function to perform multiple repetitions of PCA + GMM
pca_gmm_repeat <- function(embeddings_df, optimal_G = gmm_settings$optimal_G, 
                              n_pcs = gmm_settings$n_pcs,
                           omit_first_pc = gmm_settings$omit_first_pc, 
                           center = gmm_settings$center, 
                              scale = gmm_settings$scale, n_repeats = gmm_settings$n_repeats) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  # Initialize vectors to store internal validation metrics for clustering 
  sil_scores <- numeric(n_repeats)
  chi_values <- numeric(n_repeats)
  dbi_values <- numeric(n_repeats)
  
  pca_result <- prcomp(embeddings_df, center = center, scale. = scale)
  
  if (omit_first_pc == TRUE){
    pca_data <- pca_result$x[, 2:n_pcs]
  } else {
    pca_data <- pca_result$x[, 1:n_pcs]
  }
  
  for (i in 1:n_repeats) {
    
    # Change the random seed for each repetition 
    set.seed(42 + i)
    gmm_fit <- Mclust(pca_data, G = optimal_G, modelNames = c("EEI"))
    
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

if (run_clustering == TRUE){
  
  clustering_results <- lapply(embeddings_all_templates$X, pca_gmm_repeat)
  
  # Save results
  pca_gmm_results <- bind_rows(clustering_results)
  pca_gmm_results <- as.data.frame(pca_gmm_results)
  write_csv(pca_gmm_results, clustering_output_path)
  
  message("PCA + GMM clustering complete.")
  
}
