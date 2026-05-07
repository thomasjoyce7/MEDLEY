# Demographics summary for POD positive cohort (n=5821)
# Save a latex table as the final output

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(survival)

# Load data
final_cohort_features <- read_csv("data/cohorts/final_cohort_features.csv")

# Total N
n_total <- nrow(final_cohort_features)

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

age_row <- cont_summary(final_cohort_features, "age", "Age (years)")
bmi_row <- cont_summary(final_cohort_features, "BMI", "BMI")
icu_los_row <- cont_summary(final_cohort_features, "los_days", "Length of ICU stay (days)")

#--------------------------------------------------
# KM-estimated median POD resolution time
#--------------------------------------------------

surv_data <- final_cohort_features %>%
  mutate(
    time  = pod_resolution_days,
    event = as.integer(!censored)   # 1 = POD resolved, 0 = censored
  )

km_fit <- survfit(Surv(time, event) ~ 1, data = surv_data)

km_table <- summary(km_fit)$table

pod_resolution_row <- tibble(
  Characteristics = "POD resolution days (median, 95% CI)",
  `POD Positive` = paste0(
    sprintf("%.1f", km_table["median"]),
    " (",
    sprintf("%.1f, %.1f", km_table["0.95LCL"], km_table["0.95UCL"]),
    ")"
  )
)

#--------------------------------------------------
# Censored for POD resolution
# --------------------------------------------------

censored_count <- sum(final_cohort_features$censored == TRUE)
censored_row <- tibble(
  Characteristics = "Censored for POD resolution (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", censored_count, 100 * censored_count / n_total)
)

#--------------------------------------------------
# Gender, POD medication use, and ventilator use
#--------------------------------------------------

gender_count <- sum(final_cohort_features$gender == "Male", na.rm = TRUE)
gender_row <- tibble(
  Characteristics = "Male sex (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", gender_count, 100 * gender_count / n_total)
)

pod_med_count <- sum(final_cohort_features$pod_medication_use == "Yes", na.rm = TRUE)
pod_medication_use_row <- tibble(
  Characteristics = "POD medication use (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", pod_med_count, 100 * pod_med_count / n_total)
)

ventilator_count <- sum(final_cohort_features$ventilator_use == "Yes", na.rm = TRUE)
ventilator_use_row <- tibble(
  Characteristics = "Ventilator use (n (%))",
  `POD Positive` = sprintf("%d (%.1f)", ventilator_count, 100 * ventilator_count / n_total)
)

#--------------------------------------------------
# Race (with percentages)
#--------------------------------------------------

race_rows <- final_cohort_features %>%
  count(race) %>%
  mutate(
    Characteristics = paste0(race),
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

procedure_rows <- final_cohort_features %>%
  count(first_procedure_type) %>%
  mutate(
    Characteristics = paste0(first_procedure_type),
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
  yes_count <- sum(final_cohort_features[[var]] == "Yes", na.rm = TRUE)
  
  tibble(
    Characteristics = paste0(pretty_names[[var]]),
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
  
  pod_resolution_row,
  censored_row, 
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
colnames(baseline_characteristics_data) <- c("Characteristics", "POD Positive (n=5821)")

#--------------------------------------------------
# Display the final table
#--------------------------------------------------

final_table <- kable(
  baseline_characteristics_data,
  caption = "Baseline characteristics of the POD cohort",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(final_table, "tables/01_POD/pod_baseline_characteristics.tex")


                             
