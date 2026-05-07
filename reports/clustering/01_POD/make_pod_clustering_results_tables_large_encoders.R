# Make and save POD clustering results tables for large encoder models 

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(stringr)

# Load clustering results (50 repetitions; 2pcs, centered and unscaled)
ahc_results <- read_csv("data/results/clustering/01_POD/AHC/pod_ahc_results_large_encoders_10pcs_centered_unscaled_6clusters.csv")
gmm_results <- read_csv("data/results/clustering/01_POD/GMM/pod_gmm_results_large_encoders_50rep_10pcs_centered_unscaled_6clusters.csv")
hdbscan_results <- read_csv("data/results/clustering/01_POD/HDBSCAN/pod_hdbscan_results_large_encoders_10pcs_centered_unscaled_100minPts.csv")
kmeans_results <- read_csv("data/results/clustering/01_POD/k_means/pod_kmeans_results_large_encoders_50rep_10pcs_centered_unscaled_5clusters.csv")

# Helper function to format mean ± sd
fmt <- function(mean, sd) {
  if (!is.null(sd) && !all(is.na(sd))) {
    sprintf("%.3f ± %.3f", mean, sd)
  } else {
    sprintf("%.3f", mean)
  }
}

# Helper function to express metrics in scientific notation
fmt_sci <- function(mean, sd = NULL) {
  
  clean_e <- function(x) {
    s <- sprintf("%.2e", x)
    
    # Remove "+" sign
    s <- gsub("e\\+", "e", s)
    
    # Remove leading zero in exponent (e04 → e4)
    s <- gsub("e(-?)0+", "e\\1", s)
    
    # Remove trailing .00 in mantissa
    s <- sub("\\.00e", "e", s)
    
    s
  }
  
  if (!is.null(sd) && !all(is.na(sd))) {
    paste0(clean_e(mean), " ± ", clean_e(sd))
  } else {
    clean_e(mean)
  }
}

# Helper function to apply formatting by rank (bold the best method, underline the second best)
format_by_rank <- function(value, rank) {
  ifelse(
    rank == 1, paste0("\\textbf{", value, "}"),
    ifelse(rank == 2, paste0("\\underline{", value, "}"), value)
  )
}

# Add algorithm labels and produce unified table
process_df <- function(df, algorithm_name) {
  
  # Identify whether SD columns exist
  has_sd <- all(c("sd_silhouette", "sd_chi", "sd_dbi") %in% names(df))
  
  if (algorithm_name=="K-means++"){
    num_clusters <-  rep(5,nrow(df))
  } else if (algorithm_name=="GMM"){
    num_clusters <- rep(6, nrow(df))
  } else if (algorithm_name=="HDBSCAN"){
    num_clusters <- df$num_clusters 
  } else if (algorithm_name=="AHC"){
    num_clusters <- rep(6, nrow(df))
  }
  
  df %>%
    mutate(
      Algorithm = algorithm_name,
      SS_raw  = if (has_sd) mean_silhouette else silhouette,
      CHI_raw = if (has_sd) mean_chi        else chi,
      DBI_raw = if (has_sd) mean_dbi        else dbi,
      SS  = if (has_sd) fmt(mean_silhouette, sd_silhouette) else sprintf("%.3f", silhouette),
      CHI = if (has_sd) fmt_sci(mean_chi, sd_chi) else fmt_sci(chi),
      DBI = if (has_sd) fmt(mean_dbi,        sd_dbi)        else sprintf("%.3f", dbi),
      Clusters = num_clusters
    ) %>%
    select(Algorithm, Template = method, SS, CHI, DBI, SS_raw, CHI_raw, DBI_raw, Clusters)
}

# Process each dataframe
df_kmeans  <- process_df(kmeans_results, "K-means++")
df_gmm     <- process_df(gmm_results,    "GMM")
df_hdbscan <- process_df(hdbscan_results,"HDBSCAN")
df_ahc     <- process_df(ahc_results,    "AHC")

# Combine all into a single table
final_table <- bind_rows(df_kmeans, df_gmm, df_hdbscan, df_ahc)

# Remove LLM New Template from final_table
final_table <- final_table %>% filter(!(Template %in% c("CL LLM New", "GT LLM New", "LF LLM New")))

# -------------------------------
# Apply formatting (bold best values per algorithm, underline the second best)
# -------------------------------
formatted_table <- final_table %>%
  group_by(Algorithm) %>%
  mutate(
    SS_rank  = dense_rank(desc(SS_raw)),
    CHI_rank = dense_rank(desc(CHI_raw)),
    DBI_rank = dense_rank(DBI_raw),
    
    SS  = format_by_rank(SS,  SS_rank),
    CHI = format_by_rank(CHI, CHI_rank),
    DBI = format_by_rank(DBI, DBI_rank),
    
    Algorithm = ifelse(row_number() == 1, Algorithm, "")
  ) %>%
  ungroup() %>%
  select(Algorithm, Template, SS, CHI, DBI, Clusters)

# Set column names 
colnames(formatted_table) <- c("Algorithm", "Template", "SS", "CHI", "DBI", "Number of Clusters")

# -------------------------------
# Add horizontal lines after each algorithm block
# -------------------------------
# Determine rows after which to add lines
algorithm_breaks <- cumsum(table(final_table$Algorithm))

# Build the LaTeX table
kbl <- kable(
  formatted_table,
  format = "latex",
  escape = FALSE,       # allow bold text to render
  booktabs = TRUE,
  caption = "Clustering performance across algorithms and embedding types for large encoder models"
)

# Add horizontal lines after each algorithm row set
kbl <- kbl %>%
  kableExtra::kable_styling(latex_options = c("hold_position"))

for (br in algorithm_breaks) {
  kbl <- kbl %>% kableExtra::row_spec(br, hline_after = TRUE)
}

save_kable(kbl, "tables/01_POD/pod_clustering_results_large_encoders_pca10d_centered_unscaled.tex")

