# Cluster characteristics for conventional medication representation methods 
# Save latex tables as the final output

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(survival)
library(yaml)

# Load config
config <- yaml::yaml.load_file("config/paths.yml")
pod_results_benchmarking_dir <- file.path("data", "results", "clustering", "01_POD", "benchmarking")
pod_tables_benchmarking_dir <- file.path(config$paths$tables, "01_POD", "benchmarking")

# Import POD cohort features and medication data 
pod_study_cohort_features <- read_csv(file.path(config$paths$cohorts, "final_cohort_features.csv"))
pod_medications <- read_csv(file.path(config$paths$derived_data, "pod_medications_cleaned.csv"))

# Filter pod_medications to subjects in pod_study_cohort_features
pod_medications_filtered <- pod_medications %>% filter(subject_id %in% pod_study_cohort_features$subject_id)

# Add column for total POD medications administered to pod_study_cohort_features 
pod_med_admin <- pod_medications_filtered %>%
  count(subject_id, name = "total_pod_med_admin")

pod_study_cohort_features <- pod_study_cohort_features %>%
  left_join(pod_med_admin, by = "subject_id") %>%
  mutate(
    total_pod_med_admin = coalesce(total_pod_med_admin, 0L))

# Import cluster assignments
binary_med_pca_cluster_assignments <- read_csv(file.path(pod_results_benchmarking_dir, "binary_med_pca2d_hdbscan_clusters_100minPts.csv"))

count_med_pca_cluster_assignments <- read_csv(file.path(pod_results_benchmarking_dir, "count_med_pca2d_hdbscan_clusters_100minPts.csv"))

cumulative_dose_med_pca_cluster_assignments <- read_csv(file.path(pod_results_benchmarking_dir, "cumulative_dose_med_pca2d_hdbscan_clusters_100minPts.csv"))

dir.create(pod_tables_benchmarking_dir, recursive = TRUE, showWarnings = FALSE)

# Create dataframes that combine cohort features with cluster assignments for each representation approach
binary_med_cluster_features <- left_join(pod_study_cohort_features, binary_med_pca_cluster_assignments %>% select(subject_id, cluster), by = "subject_id")

count_med_cluster_features <- left_join(pod_study_cohort_features, count_med_pca_cluster_assignments %>% select(subject_id, cluster), by = "subject_id")

cumulative_dose_med_cluster_features <- left_join(pod_study_cohort_features, cumulative_dose_med_pca_cluster_assignments %>% select(subject_id, cluster), by = "subject_id")


# Function to summarize demographic characteristics for each cluster 
cluster_summary <- function(df,cluster_number){
  
  cluster_features <- df %>% filter(cluster==cluster_number)
  
  # Total N
  n_total <- nrow(cluster_features)
  
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
  
  # Total count row
  count_row <- tibble(
    Characteristics = "Count",
    `POD Positive` = sprintf("%d", nrow(cluster_features))
  )
  
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
  
  age_row <- cont_summary(cluster_features, "age", "Age (years)")
  bmi_row <- cont_summary(cluster_features, "BMI", "BMI")
  icu_los_row <- cont_summary(cluster_features, "los_days", "Length of ICU stay (days)")
  
  #--------------------------------------------------
  # KM-estimated median POD resolution time
  #--------------------------------------------------
  surv_data <- df %>%
    mutate(
      cluster = as.character(cluster),
      time  = pod_resolution_days,
      event = as.integer(!censored)
    )
  
  km_fit <- survfit(Surv(time, event) ~ cluster, data = surv_data)
  
  km_median <- summary(km_fit)$table %>%
    as.data.frame() %>%
    tibble::rownames_to_column("cluster") %>%
    mutate(
      cluster = sub("^cluster=", "", cluster),
      km_median_pod_resolution = paste0(
        sprintf("%.1f", median),
        " (",
        sprintf("%.1f, %.1f", `0.95LCL`, `0.95UCL`),
        ")"
      )
    ) %>%
    select(cluster, km_median_pod_resolution)
  
  pod_resolution_row <- tibble(
    Characteristics = "POD resolution days (median, 95% CI)",
    `POD Positive` = km_median %>%
      filter(cluster == as.character(cluster_number)) %>%
      pull(km_median_pod_resolution)
  )
  
  #--------------------------------------------------
  # Censored for POD resolution
  # --------------------------------------------------
  
  censored_count <- sum(cluster_features$censored == TRUE)
  censored_row <- tibble(
    Characteristics = "Censored for POD resolution (n (%))",
    `POD Positive` = sprintf("%d (%.1f)", censored_count, 100 * censored_count / n_total)
  )
  
  #--------------------------------------------------
  # Gender, POD medication use, POD med admin, and ventilator use
  #--------------------------------------------------
  
  gender_count <- sum(cluster_features$gender == "Male", na.rm = TRUE)
  gender_row <- tibble(
    Characteristics = "Male sex (n (%))",
    `POD Positive` = sprintf("%d (%.1f)", gender_count, 100 * gender_count / n_total)
  )
  
  pod_med_count <- sum(cluster_features$pod_medication_use == "Yes", na.rm = TRUE)
  pod_medication_use_row <- tibble(
    Characteristics = "POD medication use (n (%))",
    `POD Positive` = sprintf("%d (%.1f)", pod_med_count, 100 * pod_med_count / n_total)
  )
  
  pod_med_admin_row <- tibble(
    Characteristics = "POD med admin (median, (min, max))",
    `POD Positive` = paste0(
      sprintf("%.0f", median(cluster_features$total_pod_med_admin)),
      " (",
      sprintf("%.0f, %.0f", min(cluster_features$total_pod_med_admin), max(cluster_features$total_pod_med_admin)),
      ")"
    )
  )
  
  ventilator_count <- sum(cluster_features$ventilator_use == "Yes", na.rm = TRUE)
  ventilator_use_row <- tibble(
    Characteristics = "Ventilator use (n (%))",
    `POD Positive` = sprintf("%d (%.1f)", ventilator_count, 100 * ventilator_count / n_total)
  )
  
  #--------------------------------------------------
  # Race (with percentages)
  #--------------------------------------------------
  
  race_rows <- cluster_features %>%
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
  
  procedure_rows <- cluster_features %>%
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
    yes_count <- sum(cluster_features[[var]] == "Yes", na.rm = TRUE)
    
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
    count_row,
    age_row,
    gender_row,
    bmi_row,
    
    pod_resolution_row,
    censored_row, 
    icu_los_row,
    pod_medication_use_row,
    pod_med_admin_row, 
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
  colnames(baseline_characteristics_data) <- c("Characteristics", paste("Cluster",cluster_number))
  
  return(baseline_characteristics_data)
  
}

#---------------------------------------------------------
# Create cluster summary tables for each medication representation approach 
# --------------------------------------------------------

# Binary indicators representation ------------------------------------------------

cluster_1_summary <- cluster_summary(binary_med_cluster_features, 1)
cluster_2_summary <- cluster_summary(binary_med_cluster_features, 2)

cluster_characteristics_data <- cbind(cluster_1_summary, cluster_2_summary %>% select(`Cluster 2`))

#--------------------------------------------------
# Display the final table
#--------------------------------------------------

final_table <- kable(
  cluster_characteristics_data,
  caption = "Cluster characteristics for the binary indicators representation.",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(final_table, file.path(pod_tables_benchmarking_dir, "pod_binary_med_pca_cluster_characteristics.tex"))


# Medication counts representation ------------------------------------------------

cluster_1_summary <- cluster_summary(count_med_cluster_features, 1)
cluster_2_summary <- cluster_summary(count_med_cluster_features, 2)
cluster_3_summary <- cluster_summary(count_med_cluster_features, 3)

# Add row for Asian race to cluster_1_summary since it is missing
new_row <- data.frame(col1 = "Asian", col2 = "0 (0.0)")
colnames(new_row) <- c("Characteristics", "Cluster 1")

cluster_1_summary <- rbind(
  cluster_1_summary[1:11, ],
  new_row,
  cluster_1_summary[12:nrow(cluster_1_summary), ]
)

cluster_characteristics_data <- cbind(cluster_1_summary, cluster_2_summary %>% select(`Cluster 2`), cluster_3_summary %>% select(`Cluster 3`))

#--------------------------------------------------
# Save the final table
#--------------------------------------------------

final_table <- kable(
  cluster_characteristics_data,
  caption = "Cluster characteristics for the medication counts representation.",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(final_table, file.path(pod_tables_benchmarking_dir, "pod_count_med_pca_cluster_characteristics.tex"))


# Cumulative dose representation ------------------------------------------------

cluster_1_summary <- cluster_summary(cumulative_dose_med_cluster_features, 1)
cluster_2_summary <- cluster_summary(cumulative_dose_med_cluster_features, 2)

cluster_characteristics_data <- cbind(cluster_1_summary, cluster_2_summary %>% select(`Cluster 2`))

#--------------------------------------------------
# Save the final table
#--------------------------------------------------

final_table <- kable(
  cluster_characteristics_data,
  caption = "Cluster characteristics for the cumulative dose representation.",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(final_table, file.path(pod_tables_benchmarking_dir, "pod_cumulative_dose_med_pca_cluster_characteristics.tex"))
