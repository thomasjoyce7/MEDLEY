# Medication use summary by cluster for the POD positive cohort
# Save a latex table as the final output

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)

# Load final cohort cluster assignments 
final_cohort_features <- read_csv("data/results/clustering/final_cohort_pca2d_center_hdbscan_cl_list_clusters.csv")

# Filter to non-noise observations who took at least one POD medication 
pod_obs <- final_cohort_features %>% filter(cluster %in% c(2,3,4,5,6))

# POD medications 
pod_medications_cleaned <- read_csv("data/derived/pod_medications_cleaned.csv")

# Filter POD medications to subjects in pod_obs
pod_obs_med <- pod_medications_cleaned %>% filter(subject_id %in% pod_obs$subject_id)

# Add a column for cluster assignment to pod_obs_med
pod_obs_med <- left_join(pod_obs_med, pod_obs %>% select(subject_id, cluster), by = "subject_id")

# Rename Rivastigmine Tartrate as Rivastigmine
pod_obs_med <- pod_obs_med %>% mutate(medication = case_when(medication == "Rivastigmine Tartrate" ~ "Rivastigmine",
                                                              TRUE ~ medication))

# Medications table for POD positive patients --------------------------------------------------------

med_counts <- pod_obs_med %>% group_by(cluster) %>% count(medication) %>% ungroup()

med_counts <- med_counts %>% group_by(cluster) %>% mutate(summary = sprintf("%d (%.1f)", n, 100*(n/sum(n)))) %>% select(-n) %>% ungroup()

medications_list <- c("Dexmedetomidine", "Quetiapine", "Haloperidol", "Olanzapine", "Risperidone", "Chlorpromazine", "Rivastigmine")

med_ref <- tibble(medication = medications_list)

cluster_med_summary <- function(clust){
    clust <- med_counts %>% filter(cluster == clust) %>%
    right_join(med_ref, by = "medication") %>%
    mutate(summary = replace_na(summary, "0 (0.0)")) %>%
    arrange(match(medication, medications_list)) %>%
    select(-cluster)
  }

c2_med <- cluster_med_summary(2)
c3_med <- cluster_med_summary(3)
c4_med <- cluster_med_summary(4)
c5_med <- cluster_med_summary(5)
c6_med <- cluster_med_summary(6)

final_med_counts <- cbind(c2_med, c3_med %>% select(-medication), c4_med %>% select(-medication), c5_med %>% select(-medication), c6_med %>% select(-medication))
colnames(final_med_counts) <- c("Medication", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")

med_table <- kable(
  final_med_counts,
  caption = "HDBSCAN clusters medication use summary",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(med_table, "tables/01_POD/pod_hdbscan_cluster_medications.tex")
