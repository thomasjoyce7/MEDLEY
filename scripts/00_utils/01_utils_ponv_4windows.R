# Helper functions


# Log output messages
log_message <- function(msg, logfile = "data/logs/cohort_building.log") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- sprintf("[%s] %s", timestamp, msg)
  
  # Write to log file
  con <- file(logfile, open = "a")
  writeLines(full_msg, con)
  close(con)
  
  # Display in terminal
  message(full_msg)
}



# Load 4 window-based embeddings for the PONV application 
load_ponv_embeddings <- function(embedding_names) {
  
  embedding_map <- list(
    cl_list = list(
      path  = "data/embeddings/02_PONV/ponv_clinical_longformer_list_template_concat4w_meanpool_l2norm.parquet",
      short = "CL List",
      label = "Clinical-Longformer (List template)"
    ),
    cl_text = list(
      path  = "data/embeddings/02_PONV/ponv_clinical_longformer_text_template_concat4w_meanpool_l2norm.parquet",
      short = "CL Text",
      label = "Clinical-Longformer (Text template)"
    ),
    gt_base_2k_list = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_2k_list_template_concat4w_meanpool_l2norm.parquet",
      short = "GT-base-2k List",
      label = "GatorTron-base-2k (List template)"
    ),
    gt_base_2k_text = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_2k_text_template_concat4w_meanpool_l2norm.parquet",
      short = "GT-base-2k Text",
      label = "GatorTron-base-2k (Text template)"
    ),
    lf_list = list(
      path  = "data/embeddings/02_PONV/ponv_longformer_base_list_template_concat4w_meanpool_l2norm.parquet",
      short = "LF List",
      label = "Longformer-base (List template)"
    ),
    lf_text = list(
      path  = "data/embeddings/02_PONV/ponv_longformer_base_text_template_concat4w_meanpool_l2norm.parquet",
      short = "LF Text",
      label = "Longformer-base (Text template)"
    ),
    bio_clin_bert_list = list(
      path  = "data/embeddings/02_PONV/ponv_bio_clinical_bert_list_template_concat4w_meanpool_l2norm.parquet",
      short = "BC BERT List",
      label = "Bio + Clinical BERT (List template)"
    ),
    bio_clin_bert_text = list(
      path  = "data/embeddings/02_PONV/ponv_bio_clinical_bert_text_template_concat4w_meanpool_l2norm.parquet",
      short = "BC BERT Text",
      label = "Bio + Clinical BERT (Text template)"
    ),
    gt_base_list = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_list_template_concat4w_meanpool_l2norm.parquet",
      short = "GT-base List",
      label = "Gatortron-base (List template)"
    ),
    gt_base_text = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_text_template_concat4w_meanpool_l2norm.parquet",
      short = "GT-base Text",
      label = "Gatortron-base (Text template)"
    ),
    bert_list = list(
      path  = "data/embeddings/02_PONV/ponv_bert_base_uncased_list_template_concat4w_meanpool_l2norm.parquet",
      short = "BERT List",
      label = "BERT (List template)"
    ),
    bert_text = list(
      path  = "data/embeddings/02_PONV/ponv_bert_base_uncased_text_template_concat4w_meanpool_l2norm.parquet",
      short = "BERT Text",
      label = "BERT (Text template)"
    )
  )
  
  # Validate input
  unknown <- setdiff(embedding_names, names(embedding_map))
  if (length(unknown) > 0) {
    stop("Unknown embedding names: ", paste(unknown, collapse = ", "))
  }
  
  # Read the first template to obtain the subject_id column 
  first_key <- embedding_names[1]
  first_df  <- read_parquet(embedding_map[[first_key]]$path, show_col_types = FALSE)
  
  stopifnot("subject_id" %in% names(first_df))
  subject_id <- data.frame(subject_id = first_df$subject_id)
  
  # Load all templates and verify alignment 
  embeddings <- lapply(embedding_names, function(key) {
    df <- read_parquet(embedding_map[[key]]$path, show_col_types = FALSE)
    
    stopifnot("subject_id" %in% names(df))
    stopifnot(identical(df$subject_id, subject_id$subject_id))
    
    df <- dplyr::select(df, -subject_id)
    
    attr(df, "short_name") <- embedding_map[[key]]$short
    attr(df, "label")      <- embedding_map[[key]]$label
    
    df
  })
  
  names(embeddings) <- embedding_names
  
  list(
    X = embeddings,
    subject_id = subject_id
  )
  
}


# Encode the PCA cumulative proportion of variance explained
# settings for output file names 
encode_PCA_cumvar_settings <- function(settings) {
  paste0(
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled" else "unscaled"
  )
}


# Encode elbow method settings for k-means++ and GMM clustering 
encode_elbow_method_settings <- function(settings) {
  paste0(
    settings$n_pcs, "pcs_",
    if (settings$omit_first_pc) "omitpc1_" else "",
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled" else "unscaled"
  )
}


# Encode Silhouette plot settings for AHC 
encode_silhouette_plot_settings <- function(settings) {
  paste0(
    settings$n_pcs, "pcs_",
    if (settings$omit_first_pc) "omitpc1_" else "",
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled" else "unscaled"
  )
}


# Encode k-means++ clustering settings 
encode_kmeans_clustering_settings <- function(settings) {
  paste0(
    settings$n_repeats, "rep_",
    settings$n_pcs, "pcs_",
    if (settings$omit_first_pc) "omitpc1_" else "",
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled_" else "unscaled_",
    settings$optimal_k, "clusters"
  )
}


# Encode GMM clustering settings 
encode_gmm_clustering_settings <- function(settings) {
  paste0(
    settings$n_repeats, "rep_",
    settings$n_pcs, "pcs_",
    if (settings$omit_first_pc) "omitpc1_" else "",
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled_" else "unscaled_",
    settings$optimal_G, "clusters"
  )
}


# Encode HDBSCAN clustering settings 
encode_hdbscan_clustering_settings <- function(settings) {
  paste0(
    settings$n_pcs, "pcs_",
    if (settings$omit_first_pc) "omitpc1_" else "",
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled_" else "unscaled_",
    settings$minPts, "minPts"
  )
}


# Encode AHC clustering settings 
encode_ahc_clustering_settings <- function(settings) {
  paste0(
    settings$n_pcs, "pcs_",
    if (settings$omit_first_pc) "omitpc1_" else "",
    if (settings$center) "centered_" else "uncentered_",
    if (settings$scale) "scaled_" else "unscaled_",
    settings$optimal_k, "clusters"
  )
}




