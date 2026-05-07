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


# Load all embeddings for the POD application
load_pod_embeddings <- function(embedding_names) {
  
  embedding_map <- list(
    cl_list = list(
      path  = "data/embeddings/01_POD/pod_clinical_longformer_list_template_embeddings.csv",
      short = "CL List",
      label = "Clinical-Longformer (List template)"
    ),
    cl_text = list(
      path  = "data/embeddings/01_POD/pod_clinical_longformer_text_template_embeddings.csv",
      short = "CL Text",
      label = "Clinical-Longformer (Text template)"
    ),
    cl_llm = list(
      path  = "data/embeddings/01_POD/pod_clinical_longformer_llm_template_embeddings.csv",
      short = "CL LLM",
      label = "Clinical-Longformer (LLM template)"
    ),
    gt_base_2k_list = list(
      path  = "data/embeddings/01_POD/pod_gatortron_base_2k_list_template_embeddings.csv",
      short = "GT-base-2k List",
      label = "GatorTron-base-2k (List template)"
    ),
    gt_base_2k_text = list(
      path  = "data/embeddings/01_POD/pod_gatortron_base_2k_text_template_embeddings.csv",
      short = "GT-base-2k Text",
      label = "GatorTron-base-2k (Text template)"
    ),
    gt_base_2k_llm = list(
      path  = "data/embeddings/01_POD/pod_gatortron_base_2k_llm_template_embeddings.csv",
      short = "GT-base-2k LLM",
      label = "GatorTron-base-2k (LLM template)"
    ),
    lf_list = list(
      path  = "data/embeddings/01_POD/pod_longformer_base_list_template_embeddings.csv",
      short = "LF List",
      label = "Longformer-base (List template)"
    ),
    lf_text = list(
      path  = "data/embeddings/01_POD/pod_longformer_base_text_template_embeddings.csv",
      short = "LF Text",
      label = "Longformer-base (Text template)"
    ),
    lf_llm = list(
      path  = "data/embeddings/01_POD/pod_longformer_base_llm_template_embeddings.csv",
      short = "LF LLM",
      label = "Longformer-base (LLM template)"
    ),
    qwen_list = list(
      path  = "data/embeddings/01_POD/pod_Qwen3_embeddings_8B_list_template.csv",
      short = "Qwen List",
      label = "Qwen3-Embedding-8B (List template)"
    ),
    qwen_text = list(
      path  = "data/embeddings/01_POD/pod_Qwen3_embeddings_8B_text_template.csv",
      short = "Qwen Text",
      label = "Qwen3-Embedding-8B (Text template)"
    ),
    qwen_llm = list(
      path  = "data/embeddings/01_POD/pod_Qwen3_embeddings_8B_llm_template.csv",
      short = "Qwen LLM",
      label = "Qwen3-Embedding-8B (LLM template)"
    ),
    sfr_list = list(
      path  = "data/embeddings/01_POD/pod_SFR_embedding_mistral_list_template.csv",
      short = "SFR List",
      label = "SFR-Embedding-Mistral (List template)"
    ),
    sfr_text = list(
      path  = "data/embeddings/01_POD/pod_SFR_embedding_mistral_text_template.csv",
      short = "SFR Text",
      label = "SFR-Embedding-Mistral (Text template)"
    ),
    sfr_llm = list(
      path  = "data/embeddings/01_POD/pod_SFR_embedding_mistral_llm_template.csv",
      short = "SFR LLM",
      label = "SFR-Embedding-Mistral (LLM template)"
    )
  )
  
  # Validate input
  unknown <- setdiff(embedding_names, names(embedding_map))
  if (length(unknown) > 0) {
    stop("Unknown embedding names: ", paste(unknown, collapse = ", "))
  }
  
  # Read the first template to obtain the subject_id column 
  first_key <- embedding_names[1]
  first_df  <- read_csv(embedding_map[[first_key]]$path, show_col_types = FALSE)
  
  stopifnot("subject_id" %in% names(first_df))
  subject_id <- data.frame(subject_id = first_df$subject_id)
  
  # Load all templates and verify alignment 
  embeddings <- lapply(embedding_names, function(key) {
    df <- read_csv(embedding_map[[key]]$path, show_col_types = FALSE)
    
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


# Load all embeddings for the PONV application 
load_ponv_embeddings <- function(embedding_names) {
  
  embedding_map <- list(
    cl_list = list(
      path  = "data/embeddings/02_PONV/ponv_clinical_longformer_list_template_embeddings.csv",
      short = "CL List",
      label = "Clinical-Longformer (List template)"
    ),
    cl_text = list(
      path  = "data/embeddings/02_PONV/ponv_clinical_longformer_text_template_embeddings.csv",
      short = "CL Text",
      label = "Clinical-Longformer (Text template)"
    ),
    cl_llm = list(
      path  = "data/embeddings/02_PONV/ponv_clinical_longformer_llm_template_embeddings.csv",
      short = "CL LLM",
      label = "Clinical-Longformer (LLM template)"
    ),
    gt_base_2k_list = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_2k_list_template_embeddings.csv",
      short = "GT-base-2k List",
      label = "GatorTron-base-2k (List template)"
    ),
    gt_base_2k_text = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_2k_text_template_embeddings.csv",
      short = "GT-base-2k Text",
      label = "GatorTron-base-2k (Text template)"
    ),
    gt_base_2k_llm = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_2k_llm_template_embeddings.csv",
      short = "GT-base-2k LLM",
      label = "GatorTron-base-2k (LLM template)"
    ),
    lf_list = list(
      path  = "data/embeddings/02_PONV/ponv_longformer_base_list_template_embeddings.csv",
      short = "LF List",
      label = "Longformer-base (List template)"
    ),
    lf_text = list(
      path  = "data/embeddings/02_PONV/ponv_longformer_base_text_template_embeddings.csv",
      short = "LF Text",
      label = "Longformer-base (Text template)"
    ),
    lf_llm = list(
      path  = "data/embeddings/02_PONV/ponv_longformer_base_llm_template_embeddings.csv",
      short = "LF LLM",
      label = "Longformer-base (LLM template)"
    ),
    bio_clin_bert_list = list(
      path  = "data/embeddings/02_PONV/ponv_bio_clinical_bert_list_template_embeddings.csv",
      short = "BC BERT List",
      label = "Bio + Clinical BERT (List template)"
    ),
    bio_clin_bert_text = list(
      path  = "data/embeddings/02_PONV/ponv_bio_clinical_bert_text_template_embeddings.csv",
      short = "BC BERT Text",
      label = "Bio + Clinical BERT (Text template)"
    ),
    bio_clin_bert_llm = list(
      path  = "data/embeddings/02_PONV/ponv_bio_clinical_bert_llm_template_embeddings.csv",
      short = "BC BERT LLM",
      label = "Bio + Clinical BERT (LLM template)"
    ),
    gt_base_list = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_list_template_embeddings.csv",
      short = "GT-base List",
      label = "Gatortron-base (List template)"
    ),
    gt_base_text = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_text_template_embeddings.csv",
      short = "GT-base Text",
      label = "Gatortron-base (Text template)"
    ),
    gt_base_llm = list(
      path  = "data/embeddings/02_PONV/ponv_gatortron_base_llm_template_embeddings.csv",
      short = "GT-base LLM",
      label = "Gatortron-base (LLM template)"
    ),
    bert_list = list(
      path  = "data/embeddings/02_PONV/ponv_bert_base_uncased_list_template_embeddings.csv",
      short = "BERT List",
      label = "BERT (List template)"
    ),
    bert_text = list(
      path  = "data/embeddings/02_PONV/ponv_bert_base_uncased_text_template_embeddings.csv",
      short = "BERT Text",
      label = "BERT (Text template)"
    ),
    bert_llm = list(
      path  = "data/embeddings/02_PONV/ponv_bert_base_uncased_llm_template_embeddings.csv",
      short = "BERT LLM",
      label = "BERT (LLM template)"
    )
  )
  
  # Validate input
  unknown <- setdiff(embedding_names, names(embedding_map))
  if (length(unknown) > 0) {
    stop("Unknown embedding names: ", paste(unknown, collapse = ", "))
  }
  
  # Read the first template to obtain the subject_id column 
  first_key <- embedding_names[1]
  first_df  <- read_csv(embedding_map[[first_key]]$path, show_col_types = FALSE)
  
  stopifnot("subject_id" %in% names(first_df))
  subject_id <- data.frame(subject_id = first_df$subject_id)
  
  # Load all templates and verify alignment 
  embeddings <- lapply(embedding_names, function(key) {
    df <- read_csv(embedding_map[[key]]$path, show_col_types = FALSE)
    
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




