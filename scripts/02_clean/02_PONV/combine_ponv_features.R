# Script to combine demographic, clinical, and (binary) antiemetic medication use features
# into a single csv file 

# Import libraries
library(tidyverse)
library(readr)

# Load files
ponv_study_cohort_identifiers <- read_csv("data/cohorts/02_PONV/ponv_study_cohort_identifiers.csv")
ponv_demographics_cleaned <- read_csv("data/derived/02_PONV/ponv_demographics_cleaned.csv")
ponv_clinical_features_cleaned <- read_csv("data/derived/02_PONV/ponv_clinical_features_cleaned.csv")
ponv_medications_cleaned <- read_csv("data/derived/02_PONV/ponv_medications_cleaned.csv")

# Add a column to ponv_medications_cleaned to indicate the administration timing relative to the surgery date
ponv_medications_cleaned <- ponv_medications_cleaned %>% mutate(
  admin_interval = case_when(
    difftime(admin_time, procedure_date, units="days") >= -1 & difftime(admin_time, procedure_date, units="days") <= 0 ~ "-1 to 0 days",
    difftime(admin_time, procedure_date, units="days") > 0 & difftime(admin_time, procedure_date, units="days") <= 1 ~ "0 to 1 days",
    difftime(admin_time, procedure_date, units="days") > 1 & difftime(admin_time, procedure_date, units="days") <= 2 ~ "1 to 2 days",
    difftime(admin_time, procedure_date, units="days") > 2 & difftime(admin_time, procedure_date, units="days") <= 4 ~ "2 to 4 days",
  )
)

admin_interval_indicator <- function(df, interval){
  interval_filtered_df <- df %>% filter(admin_interval == interval) %>% select(subject_id, admin_interval) %>% distinct()
  return(interval_filtered_df)
}

antiemetics_day_before_surgery <- admin_interval_indicator(ponv_medications_cleaned, "-1 to 0 days")
antiemetics_day_0_to_1 <- admin_interval_indicator(ponv_medications_cleaned, "0 to 1 days") 
antiemetics_day_1_to_2 <- admin_interval_indicator(ponv_medications_cleaned, "1 to 2 days")
antiemetics_day_2_to_4 <- admin_interval_indicator(ponv_medications_cleaned, "2 to 4 days")

# Combine all features into a single data frame and add a column for binary 
# antiemetic medication use ("Yes" if the patient received perioperative antiemetic medications; "No" otherwise)
# Also create binary features for antiemetic medication administration intervals relative to the surgery date
ponv_study_cohort_features <- ponv_demographics_cleaned %>% left_join(ponv_clinical_features_cleaned, by = c("subject_id", "hadm_id")) %>%
  mutate(antiemetics_use = case_when(
    subject_id %in% ponv_medications_cleaned$subject_id ~ "Yes",
    TRUE ~ "No"
  ),
  antiemetics_day_before_surgery = case_when(
    subject_id %in% antiemetics_day_before_surgery$subject_id ~ "Yes",
    TRUE ~ "No"
  ),
  antiemetics_day_0_to_1 = case_when(
    subject_id %in% antiemetics_day_0_to_1$subject_id ~ "Yes",
    TRUE ~ "No"
  ),
  antiemetics_day_1_to_2 = case_when(
    subject_id %in% antiemetics_day_1_to_2$subject_id ~ "Yes",
    TRUE ~ "No"
  ),
  antiemetics_day_2_to_4 = case_when(
    subject_id %in% antiemetics_day_2_to_4$subject_id ~ "Yes",
    TRUE ~ "No"
  )) %>%
  left_join(ponv_study_cohort_identifiers, by = c("subject_id", "hadm_id"))

ponv_study_cohort_features <- as.data.frame(ponv_study_cohort_features)


# Save all_features_cleaned as a csv file
write_csv(ponv_study_cohort_features, "data/cohorts/02_PONV/ponv_study_cohort_features.csv")

message("All features for the PONV study cohort successfully combined and saved.")