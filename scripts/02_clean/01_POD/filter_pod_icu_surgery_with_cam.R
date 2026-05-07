# Script to filter ICU surgery patients to those with at least one positive or
# negative postoperative CAM-ICU test result

# Load libraries
library(DBI)
library(tidyverse)
library(readr)
library(scales)
library(knitr)
library(readxl)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Load helper functions
source("scripts/00_utils/00_utils.R")

# Load files
icu_surgery <- read_csv(file.path(config$paths$derived_data, "icu_surgery_patients.csv"))
icu_d_items <- read_csv(file.path(config$paths$raw_data, "icu_d_items.csv"))
icu_chartevents <- read_csv(file.path(config$paths$raw_data, "icu_chartevents_cam_filtered.csv"))
hosp_patients <- read_csv(file.path(config$paths$raw_data, "hosp_patients.csv"))


# Filter ICU surgery patients to those with CAM-ICU results ---------------------------------------------------

# Search for itemids related to CAM-ICU in icu_d_items
pod_itemid <- icu_d_items %>%
  filter(
    grepl("delirium", label, ignore.case = TRUE) |
      grepl("cam-icu", label, ignore.case = TRUE)
  ) %>%
  select(itemid, label, linksto, category)

# Add corresponding CAM-ICU labels to icu_chartevents and drop the itemid column
icu_chartevents <- icu_chartevents %>% mutate(label = case_when(itemid %in% c(228302, 228334) ~ "Alerted LOC",
                                                                itemid %in% c(228300, 228337, 229326) ~ "MS Change",
                                                                itemid %in% c(228301, 228336, 229325) ~ "Inattention",
                                                                itemid %in% c(228303, 228335, 229324) ~ "Disorganized thinking",
                                                                itemid == 228332 ~ "Delirium assessment")) %>% select(-itemid)

# By the CAM-ICU method, the patient is diagnosed with delirium if he or she has
# Feature 1 (Alerted LOC) and Feature 2 (MS Change), and Feature 3 (Inattention) or 
# Feature 4 (Disorganized thinking).

# Rows of icu_chartevents with a label of "Delirium assessment" have CAM-ICU results. 
# The value column tells us if they results are Positive, Negative, or UTA.

log_message("Filtering ICU surgery patients to those with postoperative CAM-ICU results.")

# Filter icu_surgery to patients with at least one positive or negative CAM-ICU result
icu_chartevents_results <- icu_chartevents %>% filter(value == "Positive" | value == "Negative")

icu_surgery_with_cam <- icu_surgery %>% filter(stay_id %in% icu_chartevents_results$stay_id)

# Merge icu_surgery_with_cam and icu_chartevents
icu_surgery_with_cam <- merge(icu_surgery_with_cam, icu_chartevents, by = c("subject_id", "hadm_id", "stay_id"))

# Filter icu_surgery_with_cam to postoperative CAM-ICU test results during the ICU stay
# If the patient had multiple surgical procedures, use the first procedure as the anchor
icu_surgery_with_cam <- icu_surgery_with_cam %>% group_by(subject_id, hadm_id, stay_id) %>%
  filter(charttime >= min(procedure_date) & charttime >= icu_intime & charttime <= icu_outtime) %>% ungroup()

# Rename columns
icu_surgery_with_cam <- icu_surgery_with_cam %>% rename(cam_charttime = charttime, cam_value = value, cam_label = label)

log_message(sprintf("Total ICU surgery patients with postoperative CAM-ICU results: %s",
        icu_surgery_with_cam %>% distinct(subject_id) %>% count()))

# Count total patients and positive cases
n_total <- icu_surgery_with_cam %>% distinct(subject_id) %>% nrow()
n_positive <- icu_surgery_with_cam %>% filter(cam_value == "Positive") %>% distinct(subject_id) %>% nrow()

# Compute percent
percent_positive <- round(100 * n_positive / n_total, 2)

# Print log_message
log_message(sprintf("POD positive: %d (%.2f%%)", n_positive, percent_positive))

# Create a separate dataframe only for POD positive patients
positive_subjects <- icu_surgery_with_cam %>% filter(cam_value == "Positive") %>% distinct(subject_id)

icu_surgery_with_cam_pos <- icu_surgery_with_cam %>% filter(subject_id %in% positive_subjects$subject_id)

# Create a separate dataframe only including POD positive patient identifiers
# Add columns for first surgery date, first positive CAM-ICU result time, anchor year, and anchor year group
first_surgery_date <- icu_surgery_with_cam_pos %>% group_by(subject_id, hadm_id, stay_id) %>% filter(procedure_date == min(procedure_date)) %>% ungroup() %>%
  select(subject_id, hadm_id, stay_id, procedure_date) %>% distinct() %>% rename(first_surgery_date = procedure_date)

first_cam_positive_time <- icu_surgery_with_cam_pos %>% filter(cam_value == "Positive") %>% group_by(subject_id, hadm_id, stay_id) %>%
  filter(cam_charttime == min(cam_charttime)) %>% select(subject_id, hadm_id, stay_id, cam_charttime) %>% distinct() %>% rename(first_cam_positive_time = cam_charttime) 

anchors <- hosp_patients %>% filter(subject_id %in% icu_surgery_with_cam_pos$subject_id) %>% select(subject_id, anchor_year, anchor_year_group)

pod_positive_identifiers <- icu_surgery_with_cam_pos %>%
  distinct(subject_id, hadm_id, stay_id, icu_careunit, icu_intime, icu_outtime, los_days, hosp_admittime, hosp_dischtime, deathtime)

pod_positive_identifiers <- left_join(pod_positive_identifiers, first_surgery_date, by = c("subject_id", "hadm_id", "stay_id"))

pod_positive_identifiers <- left_join(pod_positive_identifiers, first_cam_positive_time, by = c("subject_id", "hadm_id", "stay_id"))

pod_positive_identifiers <- left_join(pod_positive_identifiers, anchors, by = c("subject_id"))


# Add columns for POD duration, number of POD episodes, percentage of ICU time in POD positive, recurrent vs. non-recurrent POD, and censoring status to pod_positive_identifiers -----------------------------------------------

# Step 1: Filter and arrange
pod_episodes <- icu_surgery_with_cam_pos %>% 
  select(subject_id, hadm_id, stay_id, icu_intime, icu_outtime, cam_charttime, cam_value, cam_label) %>% distinct() %>%
  filter(cam_value %in% c("Positive", "Negative")) %>%
  arrange(subject_id, hadm_id, stay_id, cam_charttime) %>%
  group_by(subject_id, hadm_id, stay_id) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

# Step 2: Extract POD episodes (including censored)
episode_list <- list()

for (id in unique(pod_episodes$subject_id)) {
  patient_data <- pod_episodes %>% filter(subject_id == id)
  
  i <- 1
  while (i <= nrow(patient_data)) {
    if (patient_data$cam_value[i] == "Positive") {
      start_time <- patient_data$cam_charttime[i]
      icu_out <- patient_data$icu_outtime[i]
      subj_id <- patient_data$subject_id[i]
      hadm_id <- patient_data$hadm_id[i]
      stay_id <- patient_data$stay_id[i]
      censored <- TRUE
      
      j <- i + 1
      while (j <= nrow(patient_data) && patient_data$cam_value[j] != "Negative") {
        j <- j + 1
      }
      
      if (j <= nrow(patient_data)) {
        end_time <- patient_data$cam_charttime[j]
        censored <- FALSE
        i <- j + 1
      } else {
        end_time <- icu_out
        i <- nrow(patient_data) + 1
      }
      
      if (!is.na(end_time)) {
        episode_list[[length(episode_list) + 1]] <- tibble(
          subject_id = subj_id,
          hadm_id = hadm_id,
          stay_id = stay_id,
          start_time = start_time,
          end_time = end_time,
          duration_days = as.numeric(difftime(end_time, start_time, units = "days")),
          censored = censored
        )
      }
    } else {
      i <- i + 1
    }
  }
}

# Step 3: Summarize POD duration per patient
pod_duration <- bind_rows(episode_list) %>%
  group_by(subject_id, hadm_id, stay_id) %>%
  summarise(
    pod_duration_days = sum(duration_days),
    num_pod_episodes = n(),
    censored = any(censored),
    .groups = "drop"
  ) %>% mutate(recurrent = case_when(num_pod_episodes > 1 ~ "Yes", TRUE ~ "No"))

# Add columns to pod_positive_identifiers for POD duration, number of POD episodes, percentage of ICU time in POD positive, recurrent vs. non-recurrent POD, and censoring status
pod_positive_identifiers <- merge(pod_positive_identifiers, pod_duration, by = c("subject_id", "hadm_id", "stay_id"))
pod_positive_identifiers$pct_pod_positive <- 100*(pod_positive_identifiers$pod_duration_days/pod_positive_identifiers$los_days)
pod_positive_identifiers <- pod_positive_identifiers %>% relocate(pct_pod_positive, .before = censored) %>% relocate(recurrent, .before = censored)


# Add column for final POD resolution time --------------------------------------------------------------------- 

# Define POD resolution time as the days from a patient's first CAM-ICU positive result 
# to the first negative result without any subsequent positives (i.e., the negative result terminating the final POD episode).

# For patients lacking negative CAM-ICU results after their last positive, 
# POD resolution time is censored at the time of ICU discharge. 

last_positive_times <- pod_episodes %>% group_by(subject_id) %>% filter(cam_value == "Positive") %>% 
  filter(cam_charttime == max(cam_charttime)) %>% ungroup() %>% select(subject_id, hadm_id, stay_id, cam_charttime) %>%
  rename(last_positive_time = cam_charttime)

pod_episodes <- pod_episodes %>% left_join(last_positive_times, by = c("subject_id", "hadm_id", "stay_id"))

uncensored <- pod_positive_identifiers %>% filter(censored == FALSE) %>% select(subject_id)

uncensored_pod_end_times <- pod_episodes %>% filter(subject_id %in% uncensored$subject_id) %>% group_by(subject_id) %>%
  filter(cam_value == "Negative") %>% filter(cam_charttime > last_positive_time) %>% filter(cam_charttime == min(cam_charttime)) %>% 
  ungroup() %>% rename(end_time = cam_charttime) %>% select(subject_id, hadm_id, stay_id, end_time)
  
censored_pod_end_times <- pod_positive_identifiers %>% filter(censored == TRUE) %>% rename(end_time = icu_outtime) %>%
  select(subject_id, hadm_id, stay_id, end_time)

pod_end_times <- rbind(uncensored_pod_end_times, censored_pod_end_times)

# Add column for POD resolution time (in days) to pod_positive_identifiers
pod_positive_identifiers <- pod_positive_identifiers %>% left_join(pod_end_times, by = c("subject_id", "hadm_id", "stay_id")) %>%
  mutate(pod_resolution_days = as.numeric(difftime(end_time, first_cam_positive_time, units = "days"))) %>%
  select(-end_time) %>% relocate(censored, .after = pod_resolution_days)


# Save output files ----------------------------------------------------------------------------

write_csv(icu_surgery_with_cam, "data/derived/icu_surgery_with_cam.csv")

log_message("ICU surgery patients with postoperative CAM-ICU results filtered and saved.")

write_csv(icu_surgery_with_cam_pos, "data/derived/icu_surgery_with_cam_pos.csv")

write_csv(pod_positive_identifiers, "data/derived/pod_positive_identifiers.csv")

log_message("ICU surgery patients with positive postoperative CAM-ICU results filtered and saved.")




