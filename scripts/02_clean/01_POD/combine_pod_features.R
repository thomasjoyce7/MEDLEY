# Script to combine cleaned demographic features, clinical features, binary POD medication
# status, and POD duration variables, and save as a csv file 

# Import libraries
library(tidyverse)
library(readr)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Load files
pod_positive <- read_csv(file.path(config$paths$derived_data, "pod_positive_identifiers.csv"))
demographic_features <- read_csv(file.path(config$paths$derived_data, "demographics_cleaned.csv"))
clinical_features <- read_csv(file.path(config$paths$derived_data, "clinical_features_cleaned.csv"))
pod_medications <- read_csv(file.path(config$paths$derived_data, "pod_medications_cleaned.csv"))

# Combine all features into a single data frame and add a column for binary 
# POD medication status ("Yes" if patient took POD medications; "No" otherwise)
all_features_cleaned <- demographic_features %>% left_join(clinical_features, by = c("subject_id", "hadm_id", "stay_id")) %>%
  mutate(pod_medication_use = case_when(
    subject_id %in% pod_medications$subject_id ~ "Yes",
    TRUE ~ "No"
  )) %>%
  left_join(pod_positive %>% select(-icu_careunit), by = c("subject_id", "hadm_id", "stay_id"))

# Save all_features_cleaned as a csv file
write_csv(all_features_cleaned, "data/derived/all_features_cleaned.csv")

message("All features cleaned features successfully combined and saved.")



