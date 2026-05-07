# Anitemetic medication use (and drug class) summary by GMM cluster for the PONV cohort 
# Save latex tables as the final output

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)

# Load cohort data and cluster assignments
final_cohort_features <- read_csv("data/cohorts/02_PONV/ponv_study_cohort_features.csv")

cluster_assignments <- read_csv("data/cohorts/02_PONV/GMM_clusters_4windows/ponv_gmm_4windows_gt_base_text_clusters_1rep_5pcs_centered_unscaled_8clusters.csv")

# Load antiemetic medications 
ponv_medications_cleaned <- read_csv("data/derived/02_PONV/ponv_medications_cleaned.csv")

# Join final cohort features and cluster assignments
final_cohort_features <- left_join(final_cohort_features, cluster_assignments, by = "subject_id")

# Filter final_cohort_features to non-noise observations who received at least one antiemetic medication
ponv_obs <- final_cohort_features %>% filter(cluster != 0) %>% filter(antiemetics_use == "Yes")

# Filter PONV medications to subjects in ponv_obs
ponv_obs_med <- ponv_medications_cleaned %>% filter(subject_id %in% ponv_obs$subject_id)

# Add a column for cluster assignment to ponv_obs_med
ponv_obs_med <- left_join(ponv_obs_med, ponv_obs %>% select(subject_id, cluster), by = "subject_id")


# Medications table for the PONV cohort --------------------------------------------------------

med_counts <- ponv_obs_med %>% group_by(cluster) %>% count(medication) %>% ungroup()

med_counts <- med_counts %>% group_by(cluster) %>% mutate(summary = sprintf("%d (%.1f)", n, 100*(n/sum(n)))) %>% select(-n) %>% ungroup()

medications_list <- c("Ondansetron", "Metoclopramide", "Dexamethasone", "Diphenhydramine",
                      "Prochlorperazine", "Scopolamine", "Promethazine", "Haloperidol",
                      "Chlorpromazine", "Fosaprepitant", "Aprepitant", "Palonosetron")

med_ref <- tibble(medication = medications_list)

cluster_med_summary <- function(clust){
  clust <- med_counts %>% filter(cluster == clust) %>%
    right_join(med_ref, by = "medication") %>%
    mutate(summary = replace_na(summary, "0 (0.0)")) %>%
    arrange(match(medication, medications_list)) %>%
    select(-cluster)
}

c1_med <- cluster_med_summary(1)
c3_med <- cluster_med_summary(3)
c4_med <- cluster_med_summary(4)
c5_med <- cluster_med_summary(5)
c6_med <- cluster_med_summary(6)
c7_med <- cluster_med_summary(7)
c8_med <- cluster_med_summary(8)

final_med_counts <- cbind(c1_med, c3_med %>% select(-medication), c4_med %>% select(-medication), c5_med %>% select(-medication), c6_med %>% select(-medication), c7_med %>% select(-medication), c8_med %>% select(-medication))
colnames(final_med_counts) <- c("Medication", "Cluster 1", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")

med_table <- kable(
  final_med_counts,
  caption = "GMM clusters antiemetic medication use summary for the PONV cohort",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(med_table, "tables/02_PONV/ponv_gmm_gt_base_text_4windows_cluster_medications.tex")


# Drug class table for the PONV cohort --------------------------------------------------------

drug_class_counts <- ponv_obs_med %>% group_by(cluster) %>% count(drug_class) %>% ungroup()

drug_class_counts <- drug_class_counts %>% group_by(cluster) %>% mutate(summary = sprintf("%d (%.1f)", n, 100*(n/sum(n)))) %>% select(-n) %>% ungroup()

drug_class_list <- c("5-HT3 receptor antagonists", "Benzamides", "Corticosteroids", "Antihistamines",
                     "Phenothiazines", "Anticholinergics", "Butyrophenones", "Neurokinin-1 antagonists")

drug_class_ref <- tibble(drug_class = drug_class_list)

cluster_drug_class_summary <- function(clust){
  clust <- drug_class_counts %>% filter(cluster == clust) %>%
    right_join(drug_class_ref, by = "drug_class") %>%
    mutate(summary = replace_na(summary, "0 (0.0)")) %>%
    arrange(match(drug_class, drug_class_list)) %>%
    select(-cluster)
}

c1_drug_class <- cluster_drug_class_summary(1)
c3_drug_class <- cluster_drug_class_summary(3)
c4_drug_class <- cluster_drug_class_summary(4)
c5_drug_class <- cluster_drug_class_summary(5)
c6_drug_class <- cluster_drug_class_summary(6)
c7_drug_class <- cluster_drug_class_summary(7)
c8_drug_class <- cluster_drug_class_summary(8)

final_drug_class_counts <- cbind(c1_drug_class, c3_drug_class %>% select(-drug_class), c4_drug_class %>% select(-drug_class), c5_drug_class %>% select(-drug_class), c6_drug_class %>% select(-drug_class), c7_drug_class %>% select(-drug_class), c8_drug_class %>% select(-drug_class))
colnames(final_drug_class_counts) <- c("Drug Class", "Cluster 1", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")

drug_class_table <- kable(
  final_drug_class_counts,
  caption = "GMM clusters antiemetic drug class summary for the PONV cohort",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(drug_class_table, "tables/02_PONV/ponv_gmm_gt_base_text_4windows_cluster_drug_classes.tex")
