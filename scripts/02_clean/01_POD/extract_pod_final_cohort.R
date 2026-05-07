# Script to filter all_features_cleaned to obtain the final study cohort and save as a csv file

# Load libraries
library(readr)
library(tidyverse)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Load helper functions
source("scripts/00_utils/00_utils.R")

log_message("Applying exclusion criteria to extract the final cohort.")

# Load files
all_features_cleaned <- read_csv(file.path(config$paths$derived_data, "all_features_cleaned.csv"))

# Exclude patients with ICU length of stay greater than 60 days
log_message("Removing patients with LOS > 60 days.")
final_cohort_features <- all_features_cleaned %>% filter(!(los_days > 60))
log_message(sprintf("Total patients remaining: %s", nrow(final_cohort_features)))

# Exclude patients with POD resolution time less than 2 hours
log_message("Removing patients with POD resolution time < 2 hours.")
final_cohort_features <- final_cohort_features %>% filter(!(pod_resolution_days < 2/24))

# Exclude patients missing BMI values (Note: May impute BMI later to increase sample size)
log_message("Removing patients missing BMI values.")
final_cohort_features <- final_cohort_features %>% filter(!(is.na(BMI) == TRUE))

# Save final_cohort as a csv file
write_csv(final_cohort_features, "data/cohorts/final_cohort_features.csv")

log_message(sprintf("Total patients in the final cohort: %s", nrow(final_cohort_features)))
log_message("Final cohort filtered and saved.")

