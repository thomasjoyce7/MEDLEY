# Script to create conventional medication representations for the POD application

# Load libraries
library(readr)
library(tidyverse)
library(yaml)

# Load config
config <- yaml::yaml.load_file("config/paths.yml")
pod_derived_dir <- file.path(config$paths$derived_data, "01_POD")

# Import POD cohort
pod_cohort_features <- read_csv(file.path(config$paths$cohorts, "final_cohort_features.csv"))

# Import cleaned POD medication data
pod_medications_cleaned <- read_csv(file.path(config$paths$derived_data, "pod_medications_cleaned.csv"))

# Filter pod_medications_cleaned to subjects in the final cohort
pod_medications_filtered <- pod_medications_cleaned %>% filter(subject_id %in% pod_cohort_features$subject_id)

# Rename Rivastigmine Tartrate as Rivastigmine
pod_medications_filtered <- pod_medications_filtered %>% 
  mutate(medication = case_when(medication == "Rivastigmine Tartrate" ~ "Rivastigmine",
                                TRUE ~ medication))


# Representation 1: Binary exposure indicators for each medication -----------------------------------------------------------

binary_representation <- pod_medications_filtered %>%
  distinct(subject_id, hadm_id, stay_id, medication) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = medication,
    values_from = value,
    values_fill = 0
  )

binary_representation <- pod_cohort_features %>%
  select(subject_id, hadm_id, stay_id) %>%
  left_join(binary_representation,
            by = c("subject_id", "hadm_id", "stay_id")) %>%
  mutate(across(-c(subject_id, hadm_id, stay_id),
                ~replace_na(.x, 0)))

# Representation 2: Medication counts --------------------------------------------------------------------

count_representation <- pod_medications_filtered %>%
  count(subject_id, hadm_id, stay_id, medication, name = "value") %>%
  pivot_wider(
    names_from = medication,
    values_from = value,
    values_fill = 0
  )

count_representation <- pod_cohort_features %>%
  select(subject_id, hadm_id, stay_id) %>%
  left_join(
    count_representation,
    by = c("subject_id", "hadm_id", "stay_id")
  ) %>%
  mutate(across(
    -c(subject_id, hadm_id, stay_id),
    ~replace_na(.x, 0)
  ))

# Representation 3: Cumulative dose exposure for each medication --------------------------------------------------------------------

cumulative_dose_representation <- pod_medications_filtered %>%
  
  # Exclude Rivastigmine patches
  filter(units != "PTCH") %>%
  
  # Convert all doses to mg
  mutate(
    amount_mg = case_when(
      units == "mg"  ~ amount,
      units == "mcg" ~ amount / 1000,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Sum cumulative dose within patient and medication
  group_by(subject_id, hadm_id, stay_id, medication) %>%
  summarize(
    cumulative_mg = sum(amount_mg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Convert to wide format
  pivot_wider(
    names_from = medication,
    values_from = cumulative_mg,
    values_fill = 0
  )

# Include patients with no POD medications
cumulative_dose_representation <- pod_cohort_features %>%
  select(subject_id, hadm_id, stay_id) %>%
  left_join(
    cumulative_dose_representation,
    by = c("subject_id", "hadm_id", "stay_id")
  ) %>%
  mutate(
    across(
      -c(subject_id, hadm_id, stay_id),
      ~ replace_na(.x, 0)
    )
  )

# Save conventional medication representations as csv files ---------------------------------------

dir.create(pod_derived_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(binary_representation, file.path(pod_derived_dir, "pod_med_binary_rep.csv"))
write_csv(count_representation, file.path(pod_derived_dir, "pod_med_count_rep.csv"))
write_csv(cumulative_dose_representation, file.path(pod_derived_dir, "pod_med_cumulative_dose_rep.csv"))





