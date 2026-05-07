# Medication and demographics summary for the HDBSCAN CL List PCA 2d noise observations (n=130)
# Save a latex table as the final output

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)

# Load final cohort cluster assignments 
final_cohort_features <- read_csv("data/results/clustering/final_cohort_pca2d_center_hdbscan_cl_list_clusters.csv")

# Filter to noise observations
noise_observations <- final_cohort_features %>% filter(cluster == 0)

# POD medications 
pod_medications_cleaned <- read_csv("data/derived/pod_medications_cleaned.csv")

# Filter POD medications to noise observations
noise_med <- pod_medications_cleaned %>% filter(subject_id %in% noise_observations$subject_id)

# Demographics table for noise observations -------------------------------------------------------

# Total N
n_total <- nrow(noise_observations)

#--------------------------------------------------
# Table names for binary medical history variables
#--------------------------------------------------

pretty_names <- c(
  diabetes             = "Diabetes",
  stroke               = "Stroke",
  alcohol_use_disorder = "Alcohol use disorder",
  hypertension         = "Hypertension",
  smoking              = "Smoking",
  COPD                 = "COPD",
  liver_disease        = "Liver disease",
  CKD                  = "Chronic kidney disease",
  dementia             = "Dementia",
  depression           = "Depression"
)

binary_vars <- names(pretty_names)

#--------------------------------------------------
# Continuous variables: mean ± SD
#--------------------------------------------------

cont_summary <- function(df, var, label) {
  mean_val <- mean(df[[var]], na.rm = TRUE)
  sd_val   <- sd(df[[var]], na.rm = TRUE)
  
  tibble(
    Characteristics = label,
    `POD Positive` = sprintf("%.1f ± %.1f", mean_val, sd_val)
  )
}

age_row <- cont_summary(noise_observations, "age", "Age (years)")
bmi_row <- cont_summary(noise_observations, "BMI", "BMI")
pod_duration_row <- cont_summary(noise_observations, "pod_duration_days", "POD duration (days)")
icu_los_row <- cont_summary(noise_observations, "los_days", "Length of ICU stay (days)")

#--------------------------------------------------
# Gender, POD medication use, and ventilator use
#--------------------------------------------------

gender_count <- sum(noise_observations$gender == "Male", na.rm = TRUE)
gender_row <- tibble(
  Characteristics = "Male sex (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", gender_count, 100 * gender_count / n_total)
)

pod_med_count <- sum(noise_observations$pod_medication_use == "Yes", na.rm = TRUE)
pod_medication_use_row <- tibble(
  Characteristics = "POD medication use (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", pod_med_count, 100 * pod_med_count / n_total)
)

ventilator_count <- sum(noise_observations$ventilator_use == "Yes", na.rm = TRUE)
ventilator_use_row <- tibble(
  Characteristics = "Ventilator use (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", ventilator_count, 100 * ventilator_count / n_total)
)

#--------------------------------------------------
# Race (with percentages)
#--------------------------------------------------

race_rows <- noise_observations %>%
  count(race) %>%
  mutate(
    Characteristics = paste0(race, " (n (%))"),
    `POD Positive` = sprintf("%d (%.1f)", n, 100 * n_total^{-1} * n)
  ) %>%
  select(Characteristics, `POD Positive`)

# Race header row
race_header <- tibble(
  Characteristics = "Race (n (%))",
  `POD Positive` = ""
)

#--------------------------------------------------
# First surgery type (with percentages)
#--------------------------------------------------

procedure_rows <- noise_observations %>%
  count(first_procedure_type) %>%
  mutate(
    Characteristics = paste0(first_procedure_type, " (n (%))"),
    `POD Positive` = sprintf("%d (%.1f)", n, 100 * n_total^{-1} * n)
  ) %>%
  select(Characteristics, `POD Positive`)

procedure_header <- tibble(
  Characteristics = "First surgery type (n (%))",
  `POD Positive` = ""
)

#--------------------------------------------------
# Medical history (binary comorbidities)
#--------------------------------------------------

summarize_binary <- function(var) {
  yes_count <- sum(noise_observations[[var]] == "Yes", na.rm = TRUE)
  
  tibble(
    Characteristics = paste0(pretty_names[[var]], " (n (%))"),
    `POD Positive` = sprintf("%d (%.1f)", yes_count, 100 * yes_count / n_total)
  )
}

binary_rows <- map_dfr(binary_vars, summarize_binary)

medical_history_header <- tibble(
  Characteristics = "Medical history (n (%))",
  `POD Positive` = ""
)

#--------------------------------------------------
# Combine all sections into one final table
#--------------------------------------------------

baseline_characteristics_data <- bind_rows(
  age_row,
  gender_row,
  bmi_row,
  
  pod_duration_row,
  icu_los_row,
  pod_medication_use_row,
  ventilator_use_row,
  
  race_header,
  race_rows,
  
  procedure_header,
  procedure_rows,
  
  medical_history_header,
  binary_rows
)

#--------------------------------------------------
# Optional: Bold headers in the table
#--------------------------------------------------

header_labels <- c(
  "Race (n (%))",
  "First surgery type (n (%))",
  "Medical history (n (%))"
)

baseline_characteristics_data <- baseline_characteristics_data %>%
  mutate(
    Characteristics = ifelse(
      Characteristics %in% header_labels,
      Characteristics,
      Characteristics
    )
  )
colnames(baseline_characteristics_data) <- c("Characteristics", "Noise observations (n=130)")

#--------------------------------------------------
# Display the final table
#--------------------------------------------------

demo_table <- kable(
  baseline_characteristics_data,
  caption = "HDBSCAN noise observations characteristics summary",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(demo_table, "tables/01_POD/pod_hdbscan_noise_obs_characteristics.tex")

# Medications table for noise observations --------------------------------------------------------

med_counts <- noise_med %>% count(medication) %>% arrange(desc(n))

med_counts <- med_counts %>% mutate(summary = sprintf("%d (%.1f)", n, 100*(n/nrow(noise_med)))) %>% select(-n)

colnames(med_counts) <- c("Medication", "Number of administrations (%)")

med_table <- kable(
  med_counts,
  caption = "HDBSCAN noise observations medication summary",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(med_table, "tables/01_POD/pod_hdbscan_noise_obs_medications.tex")





