# Run PCA on the 4 window-based LLM embeddings for the PONV application and plot the cumulative 
# proportion of variance explained by the first 50 principle components 

# Load libraries
library(readr)
library(tidyverse)
library(scales)
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

# PCA settings
PCA_settings <- list(center = TRUE, scale = FALSE)

# Output file paths for the plots
PCA_suffix <- encode_PCA_cumvar_settings(PCA_settings)
PCA_plots_output_path <- file.path(
  "figures/clustering/02_PONV/PCA",
  paste0("ponv_pca_variance_grid_4windows_", PCA_suffix, ".pdf")
)

PCA_plots_long_seq_encoders_output_path <- file.path(
  "figures/clustering/02_PONV/PCA",
  paste0("ponv_pca_variance_grid_long_seq_encoders_4windows_", PCA_suffix, ".pdf")
)

# Run PCA and check the cumulative proportion of variance explained 
pca_variance_summary <- function(embeddings_df, plot = TRUE) {
  
  short_name <- attr(embeddings_df, "short_name")
  
  pca_result <- prcomp(embeddings_df, center = PCA_settings$center, scale. = PCA_settings$scale)
  
  var_explained <- pca_result$sdev^2
  prop_var <- var_explained / sum(var_explained)
  cumvar <- cumsum(prop_var)
  
  pc_counts <- seq_len(min(50, length(cumvar)))
  
  df_plot <- data.frame(
    PC = pc_counts,
    CumulativeVariance = cumvar[pc_counts]
  )
  
  variance_plot <- NULL
  if (plot) {
    variance_plot <- ggplot(df_plot, aes(x = PC, y = CumulativeVariance)) +
      geom_line(color = "#0072B2", size = 0.25) +
      geom_point(color = "black", size=0.25) +
      scale_y_continuous(limits = c(0.5,1), labels = percent_format(accuracy = 1)) +
      labs(
        title = paste("Method:", short_name), 
        x = "Number of Principal Components",
        y = "Cumulative Proportion of Variance")+
      theme_minimal(base_size = 7.5, base_family = "") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.4),
        axis.ticks = element_line(color = "black", size = 0.4),
        plot.title = element_text(hjust = 0, face = "bold", size = 9),
      )
  }
  
  list(
    cumvar = cumvar,
    plot   = variance_plot
  )
}

# Create the cumulative proportion of variance plots 
pca_variance_plots <- lapply(embeddings_all_templates$X, pca_variance_summary)
  
# Grid layout: 
variance_grid_long_seq_encoders <- 
  (pca_variance_plots$cl_list$plot | pca_variance_plots$cl_text$plot) /
  (pca_variance_plots$gt_base_2k_list$plot | pca_variance_plots$gt_base_2k_text$plot) / 
  (pca_variance_plots$lf_list$plot | pca_variance_plots$lf_text$plot)

variance_grid_normal_seq_encoders <- 
  (pca_variance_plots$bio_clin_bert_list$plot | pca_variance_plots$bio_clin_bert_text$plot) /
  (pca_variance_plots$gt_base_list$plot | pca_variance_plots$gt_base_text$plot) / 
  (pca_variance_plots$bert_list$plot | pca_variance_plots$bert_text$plot)

# Combine plots
variance_grid_all <-
  wrap_elements(full = variance_grid_long_seq_encoders) /
  wrap_elements(full = variance_grid_normal_seq_encoders) +
  plot_layout(nrow = 2, heights = c(3, 3))

# Save all plots to results 
ggsave(PCA_plots_output_path, variance_grid_all, width = 9, height = 12, units = "in", dpi = 300, device = cairo_pdf)

# Save long sequence encoder PCA grid separately
ggsave(PCA_plots_long_seq_encoders_output_path, variance_grid_long_seq_encoders, width = 9, height = 6, units = "in", dpi = 300, device = cairo_pdf)
