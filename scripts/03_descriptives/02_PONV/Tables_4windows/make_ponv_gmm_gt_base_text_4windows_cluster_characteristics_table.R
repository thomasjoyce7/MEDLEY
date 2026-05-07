# Demographics summary for the PONV GMM GT-base text template clusters (4 windows)
# Save a latex table as the final output

# Import libraries
library(readr)
library(tidyverse)
library(knitr)
library(kableExtra)

# Load data
final_cohort_features <- read_csv("data/cohorts/02_PONV/ponv_study_cohort_features.csv")

cluster_assignments <- read_csv("data/cohorts/02_PONV/GMM_clusters_4windows/ponv_gmm_4windows_gt_base_text_clusters_1rep_5pcs_centered_unscaled_8clusters.csv")

# Add columns for admission duration (days) and postoperative opioids (i.e., opioid use 1-4 days after surgery date)
final_cohort_features <- final_cohort_features %>% mutate(admission_duration = as.numeric(difftime(hosp_dischtime, hosp_admittime, units = "days")))

final_cohort_features <- final_cohort_features %>% 
  mutate(postoperative_opioids = case_when((opioids_day_1_to_2 == "Yes" | opioids_day_2_to_4 == "Yes") ~ "Yes",
                                           TRUE ~ "No"))

# Also create a column for Nonsmokers (i.e., never smoked)
final_cohort_features <- final_cohort_features %>% 
  mutate(never_smoked = case_when(curr_or_prev_smoker == "No" ~ "Yes",
                                  TRUE ~ "No"))

# Merge final_cohort_features and cluster_assignment
final_cohort_features <- left_join(final_cohort_features, cluster_assignments, by = "subject_id")

# Remove noise observations and rename columns 
final_cohort_features <- final_cohort_features %>% filter(cluster != 0)


# Function to summarize demographic characteristics for each cluster 
cluster_summary <- function(df,cluster_number){
  
  cluster_features <- df %>% filter(cluster==cluster_number)
  
  # Total N
  n_total <- nrow(cluster_features)
  
  # Total count row
  count_row <- tibble(
    Characteristics = "Count",
    Value = sprintf("%d", nrow(cluster_features))
  )
  
  #--------------------------------------------------
  # Continuous variables: mean ± SD
  #--------------------------------------------------
  
  cont_summary <- function(df, var, label) {
    mean_val <- mean(df[[var]], na.rm = TRUE)
    sd_val   <- sd(df[[var]], na.rm = TRUE)
    
    tibble(
      Characteristics = label,
      Value = sprintf("%.1f ± %.1f", mean_val, sd_val)
    )}
  
  cont_summary_with_missing <- function(df, var, label) {
    
    x <- df[[var]]
    
    n_total   <- length(x)
    n_missing <- sum(is.na(x))
    pct_missing <- 100 * n_missing / n_total
    
    mean_val <- mean(x, na.rm = TRUE)
    sd_val   <- sd(x, na.rm = TRUE)
    
    list(
      row = tibble(
        Characteristics = label,
        Value = sprintf("%.1f ± %.1f", mean_val, sd_val)
      ),
      missing_note = sprintf(
        "%s missing for %d/%d patients (%.1f%%).",
        label, n_missing, n_total, pct_missing
      )
    )
  }
  
  age_row <- cont_summary(cluster_features, "age", "Age (years)")
  
  admission_duration_row <- cont_summary(cluster_features, "admission_duration", "Hospital admission duration (days)")
  
  bmi_summary <- cont_summary_with_missing(
    cluster_features,
    "BMI",
    "BMI (kg/m^2)"
  )
  
  bmi_row <- bmi_summary$row
  bmi_footnote <- bmi_summary$missing_note
  
  #--------------------------------------------------
  # Summarize binary features 
  #--------------------------------------------------
  
  gender_count <- sum(cluster_features$gender == "Female", na.rm = TRUE)
  gender_row <- tibble(
    Characteristics = "Female gender (n (%))",
    Value = sprintf("%d (%.1f)", gender_count, 100 * gender_count / n_total)
  )
  
  nonsmoker_count <- sum(cluster_features$never_smoked == "Yes", na.rm = TRUE)
  nonsmoker_row <- tibble(
    Characteristics = "Never smoked (n (%))",
    Value = sprintf("%d (%.1f)", nonsmoker_count, 100 * nonsmoker_count / n_total)
  )
  
  postoperative_opioids_count <- sum(cluster_features$postoperative_opioids == "Yes", na.rm = TRUE)
  postoperative_opioids_row <- tibble(
    Characteristics = "Postoperative opioids (n (%))",
    Value = sprintf("%d (%.1f)", postoperative_opioids_count, 100 * postoperative_opioids_count / n_total)
  )
  
  # Antiemetics header row
  antiemetics_header <- tibble(
    Characteristics = "Antiemetics use (n (%))",
    Value = ""
  )
  
  antiemetics_use_count <- sum(cluster_features$antiemetics_use == "Yes", na.rm = TRUE)
  antiemetics_use_row <- tibble(
    Characteristics = "Perioperative (-1 to +4 days)",
    Value = sprintf("%d (%.1f)", antiemetics_use_count, 100 * antiemetics_use_count / n_total)
  )
  
  antiemetics_day_before_surgery_count <- sum(cluster_features$antiemetics_day_before_surgery == "Yes", na.rm = TRUE)
  antiemetics_day_before_surgery_row <- tibble(
    Characteristics = "Day before surgery",
    Value = sprintf("%d (%.1f)", antiemetics_day_before_surgery_count, 100 * antiemetics_day_before_surgery_count / n_total)
  )
  
  antiemetics_day_0_to_1_count <- sum(cluster_features$antiemetics_day_0_to_1 == "Yes", na.rm = TRUE)
  antiemetics_day_0_to_1_row <- tibble(
    Characteristics = "Day of surgery",
    Value = sprintf("%d (%.1f)", antiemetics_day_0_to_1_count, 100 * antiemetics_day_0_to_1_count / n_total)
  )
  
  antiemetics_day_1_to_2_count <- sum(cluster_features$antiemetics_day_1_to_2 == "Yes", na.rm = TRUE)
  antiemetics_day_1_to_2_row <- tibble(
    Characteristics = "1 to 2 days after surgery",
    Value = sprintf("%d (%.1f)", antiemetics_day_1_to_2_count, 100 * antiemetics_day_1_to_2_count / n_total)
  )
  
  antiemetics_day_2_to_4_count <- sum(cluster_features$antiemetics_day_2_to_4 == "Yes", na.rm = TRUE)
  antiemetics_day_2_to_4_row <- tibble(
    Characteristics = "2 to 4 days after surgery",
    Value = sprintf("%d (%.1f)", antiemetics_day_2_to_4_count, 100 * antiemetics_day_2_to_4_count / n_total)
  )
  
  #--------------------------------------------------
  # Race (with percentages)
  #--------------------------------------------------
  
  race_rows <- cluster_features %>%
    count(race) %>%
    mutate(
      Characteristics = paste0(race),
      Value = sprintf("%d (%.1f)", n, 100 * n_total^{-1} * n)
    ) %>%
    select(Characteristics, Value)
  
  # Race header row
  race_header <- tibble(
    Characteristics = "Race (n (%))",
    Value = ""
  )
  
  #--------------------------------------------------
  # First surgery type (with percentages)
  #--------------------------------------------------
  
  surgery_rows <- cluster_features %>%
    count(surgery_type) %>%
    mutate(
      Characteristics = paste0(surgery_type),
      Value = sprintf("%d (%.1f)", n, 100 * n_total^{-1} * n)
    ) %>%
    select(Characteristics, Value)
  
  surgery_header <- tibble(
    Characteristics = "Surgery Type (n (%))",
    Value = ""
  )
  
  #--------------------------------------------------
  # Combine all sections into one final table
  #--------------------------------------------------
  
  baseline_characteristics_data <- bind_rows(
    count_row, 
    age_row,
    gender_row,
    bmi_row,
    admission_duration_row, 
    
    nonsmoker_row,
    postoperative_opioids_row, 
    
    antiemetics_header,
    antiemetics_use_row, 
    antiemetics_day_before_surgery_row,
    antiemetics_day_0_to_1_row,
    antiemetics_day_1_to_2_row,
    antiemetics_day_2_to_4_row, 
    
    race_header,
    race_rows,
    
    surgery_header,
    surgery_rows,
  )
  
  #--------------------------------------------------
  # Optional: Bold headers in the table
  #--------------------------------------------------
  
  header_labels <- c(
    "Race (n (%))",
    "Surgery Type (n (%))"
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
# Generate summary statistics for all clusters and combine
# --------------------------------------------------------

cluster_1_summary <- cluster_summary(final_cohort_features, 1)
cluster_2_summary <- cluster_summary(final_cohort_features, 2)
cluster_3_summary <- cluster_summary(final_cohort_features, 3)
cluster_4_summary <- cluster_summary(final_cohort_features, 4)
cluster_5_summary <- cluster_summary(final_cohort_features, 5)
cluster_6_summary <- cluster_summary(final_cohort_features, 6)
cluster_7_summary <- cluster_summary(final_cohort_features, 7)
cluster_8_summary <- cluster_summary(final_cohort_features, 8)

cluster_characteristics_data <- cbind(cluster_1_summary, cluster_2_summary %>% select(`Cluster 2`), 
                                      cluster_3_summary %>% select(`Cluster 3`),
                                      cluster_4_summary %>% select(`Cluster 4`), 
                                      cluster_5_summary %>% select(`Cluster 5`), 
                                      cluster_6_summary %>% select(`Cluster 6`),
                                      cluster_7_summary %>% select(`Cluster 7`),
                                      cluster_8_summary %>% select(`Cluster 8`))

#--------------------------------------------------
# Display the final table
#--------------------------------------------------

final_table <- kable(
  cluster_characteristics_data,
  caption = "GMM cluster characteristics for the PONV cohort",
  booktabs = TRUE,
  escape = FALSE,
  format = "latex"
)

save_kable(final_table, "tables/02_PONV/ponv_gmm_gt_base_text_4windows_cluster_characteristics.tex")
