# Script to extract study cohort for the PONV application

# Load libraries
library(tidyverse)
library(readr)

# Load MIMIC-IV surgical patients
all_surgical_patients <- read_csv("data/derived/all_surgical_patients.csv")

sprintf("There are %s total surgical patients in MIMIC-IV.", all_surgical_patients %>% distinct(subject_id) %>% count())

# Load hosp_patients and add columns for anchor_year and anchor_year_group
hosp_patients <- read_csv("data/raw/hosp_patients.csv")

all_surgical_patients <- left_join(all_surgical_patients, hosp_patients %>% 
                                     select(subject_id, anchor_year, anchor_year_group), by = "subject_id") %>%
                                     mutate(procedure_date = as.POSIXct(procedure_date))

# Step 1: Identify all eligible procedures ------------------------------------------

# For each subject, filter to procedure dates occurring at least 4 days before or after any other procedures (this ensures we can adequately assess antiemetic medication administrations for a single surgical event)
all_surgery_dates <- all_surgical_patients %>% group_by(subject_id) %>% distinct(procedure_date) %>% select(subject_id, procedure_date) %>% mutate(comparison_date = procedure_date) %>% select(subject_id, comparison_date) %>% ungroup()

qualifying_surgery_dates <- left_join(all_surgical_patients, all_surgery_dates, by = "subject_id", relationship = "many-to-many") %>% filter(difftime(procedure_date, comparison_date, units = "days") != 0) %>% filter((abs(difftime(procedure_date, comparison_date, units = "days")) >= 4)) %>% select(-comparison_date) %>% distinct()

# For each subject, filter to eligible procedure dates occurring at admission time or later and at least 48 hours before hospital discharge time (this ensures we can capture medication administrations in the perioperative window)
qualifying_surgery_dates_filtered <- qualifying_surgery_dates %>%
  filter(difftime(procedure_date, hosp_admittime, units = "hours") >=0 & difftime(hosp_dischtime, procedure_date, units = "hours") >= 48)

# Step 2: Filter to the earliest eligible procedure date in the patient's most recent hospital admission -----------------------
surgical_patients_filtered <- qualifying_surgery_dates_filtered %>% group_by(subject_id) %>% filter(hosp_admittime == max(hosp_admittime)) %>% ungroup()

surgical_patients_filtered <- surgical_patients_filtered %>% group_by(hadm_id) %>% filter(procedure_date == min(procedure_date)) %>% ungroup()

sprintf("%s surgical patients have procedure dates at least 4 days before or after any other procedures, at admission time or later, and at least 48 hours before discharge time.", surgical_patients_filtered %>% distinct(subject_id) %>% count())

# Step 3: Exclude patients with an earliest possible procedure date before 2016 (since EMAR data was not fully implemented in all hospital units until 2016)----------------------------------
ponv_study_cohort_procedures <- surgical_patients_filtered %>%
  mutate(
    # Extract earliest year from anchor_year_group (e.g., "2014-2016" -> 2014)
    anchor_year_lb = as.integer(str_sub(anchor_year_group, 1, 4))
  )

ponv_study_cohort_procedures <- ponv_study_cohort_procedures %>%
  mutate(
    procedure_year = year(as.Date(procedure_date)),
    
    earliest_possible_procedure_year =
      anchor_year_lb + (procedure_year - anchor_year)
  )

ponv_study_cohort_procedures <- ponv_study_cohort_procedures %>%
  filter(earliest_possible_procedure_year >= 2016)

ponv_study_cohort_identifiers <- ponv_study_cohort_procedures %>% select(subject_id, hadm_id, hosp_admittime, hosp_dischtime, procedure_date, anchor_year, anchor_year_group) %>% distinct()

sprintf("There are %s surgical patients in the final PONV study cohort, after filtering to patients with earliest possible procedure dates in 2016 or later.", nrow(ponv_study_cohort_identifiers))

# Save ponv_study_cohort_procedures and ponv_study_cohort_identifiers as csv files ---------------------------------------------------------------------

write_csv(ponv_study_cohort_procedures, "data/cohorts/02_PONV/ponv_study_cohort_procedures.csv")

write_csv(ponv_study_cohort_identifiers, "data/cohorts/02_PONV/ponv_study_cohort_identifiers.csv")








