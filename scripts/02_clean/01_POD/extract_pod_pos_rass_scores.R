# Script to extract RASS scores for POD positive patients

# Import libraries
library(tidyverse)
library(readr)

# Import data 
pod_positive_identifiers <- read_csv("data/derived/pod_positive_identifiers.csv")
icu_chartevents_rass_filtered <- read_csv("data/raw/icu_chartevents_rass_filtered.csv")

# Filter icu_chartevents_rass_filtered to ICU stays in pod_positive_identifiers
pod_pos_rass_scores <- icu_chartevents_rass_filtered %>% filter(stay_id %in% pod_positive_identifiers$stay_id)

# Add labels for RASS itemids
pod_pos_rass_scores <- pod_pos_rass_scores %>% 
  mutate(label = case_when(itemid == 228096 ~ "Richmond-RAS Scale",
                           itemid == 228299 ~ "Goal Richmond-RAS Scale"))

# Tidy columns
pod_pos_rass_scores_cleaned <- pod_pos_rass_scores %>% select(subject_id, hadm_id, stay_id,
                                                              label, charttime, value, valuenum)

# Save as a csv file
write_csv(pod_pos_rass_scores_cleaned, "data/derived/01_POD/pod_pos_rass_scores_cleaned.csv")
