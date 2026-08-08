# Benchmarking with HDBSCAN clustering for the POD application

# Load libraries
library(readr)
library(tidyverse)
library(cluster)
library(clusterCrit)
library(dbscan)
library(uwot)
library(proxy)
library(yaml)

# Load config
config <- yaml::yaml.load_file("config/paths.yml")
pod_derived_dir <- file.path(config$paths$derived_data, "01_POD")
benchmarking_output_dir <- file.path("data", "results", "clustering", "01_POD", "benchmarking")

# Load binary exposure indicator representation
binary_representation <- read_csv(file.path(pod_derived_dir, "pod_med_binary_rep.csv"))
count_representation <- read_csv(file.path(pod_derived_dir, "pod_med_count_rep.csv"))
cumulative_dose_representation <- read_csv(file.path(pod_derived_dir, "pod_med_cumulative_dose_rep.csv"))

# Run clustering ------------------------------------------------------------------------------

# Function to perform PCA + HDBSCAN
run_hdbscan <- function(clustering_data,
                        method = "representation",
                        n_pcs = 2,
                        center = TRUE,
                        scale = FALSE,
                        minPts = 100) {
  
  X <- as.matrix(clustering_data)
  
  pca_result <- prcomp(X, center = center, scale. = scale)
  n_pcs <- min(n_pcs, ncol(pca_result$x))
  clustering_input <- pca_result$x[, 1:n_pcs, drop = FALSE]
  preprocessing <- paste0("PCA_", n_pcs, "_PCs")
  
  # HDBSCAN
  hdb <- dbscan::hdbscan(clustering_input, minPts = minPts)
  clusters <- hdb$cluster
  
  # Number of non-noise clusters
  num_clusters <- length(unique(clusters[clusters > 0]))
  
  # Remove noise points
  non_noise_idx <- which(clusters > 0)
  
  # Return NA metrics if fewer than 2 non-noise clusters
  if (num_clusters < 2 || length(non_noise_idx) == 0) {
    return(list(
      method = method,
      preprocessing = preprocessing,
      silhouette = NA,
      chi = NA,
      dbi = NA,
      num_clusters = num_clusters,
      noise_n = sum(clusters == 0),
      noise_prop = mean(clusters == 0),
      clusters = clusters,
      pca_result = pca_result
    ))
  }
  
  X_clean <- clustering_input[non_noise_idx, , drop = FALSE]
  clusters_clean <- as.integer(clusters[non_noise_idx])
  
  # Silhouette
  sil <- cluster::silhouette(
    clusters_clean,
    dist(X_clean, method = "euclidean")
  )
  sil_score <- mean(sil[, 3])
  
  # CHI
  chi <- clusterCrit::intCriteria(
    traj = as.matrix(X_clean),
    part = clusters_clean,
    crit = "Calinski_Harabasz"
  )
  
  # DBI
  dbi <- clusterCrit::intCriteria(
    traj = as.matrix(X_clean),
    part = clusters_clean,
    crit = "Davies_Bouldin"
  )
  
  return(list(
    method = method,
    preprocessing = preprocessing,
    silhouette = sil_score,
    chi = chi$calinski_harabasz,
    dbi = dbi$davies_bouldin,
    num_clusters = num_clusters,
    noise_n = sum(clusters == 0),
    noise_prop = mean(clusters == 0),
    clusters = clusters,
    hdbscan = hdb,
    pca_result = pca_result
  ))
}

save_cluster_assignments <- function(original_data,
                                     clustering_result,
                                     output_path) {
  
  cluster_df <- original_data %>%
    select(subject_id, hadm_id, stay_id) %>%
    mutate(
      method = clustering_result$method,
      preprocessing = clustering_result$preprocessing,
      cluster = clustering_result$clusters,
      is_noise = cluster == 0
    )
  
  write_csv(cluster_df, output_path)
  
  return(cluster_df)
}

# Prepare data
binary_X <- binary_representation %>%
  select(-subject_id, -hadm_id, -stay_id)

count_X <- count_representation %>%
  select(-subject_id, -hadm_id, -stay_id) %>%
  log1p()

dose_X <- cumulative_dose_representation %>%
  select(-subject_id, -hadm_id, -stay_id) %>%
  log1p()

# With PCA
binary_pca_results <- run_hdbscan(binary_X, method = "binary_med")
count_pca_results <- run_hdbscan(count_X, method = "count_med")
dose_pca_results <- run_hdbscan(dose_X, method = "cumulative_dose_med")

clustering_summary <- bind_rows(
  as.data.frame(binary_pca_results[1:8]),
  as.data.frame(count_pca_results[1:8]),
  as.data.frame(dose_pca_results[1:8])
)

# Save clustering results -------------------------------------------------------------------------
dir.create(benchmarking_output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  clustering_summary,
  file.path(benchmarking_output_dir, "pod_hdbscan_benchmark_results_hdbscan_100minPts.csv")
)


# Save cluster assignments ----------------------------------------------------
binary_pca_clusters <- save_cluster_assignments(
  binary_representation,
  binary_pca_results,
  file.path(benchmarking_output_dir, "binary_med_pca2d_hdbscan_clusters_100minPts.csv")
)

count_pca_clusters <- save_cluster_assignments(
  count_representation,
  count_pca_results,
  file.path(benchmarking_output_dir, "count_med_pca2d_hdbscan_clusters_100minPts.csv")
)

dose_pca_clusters <- save_cluster_assignments(
  cumulative_dose_representation,
  dose_pca_results,
  file.path(benchmarking_output_dir, "cumulative_dose_med_pca2d_hdbscan_clusters_100minPts.csv")
)
