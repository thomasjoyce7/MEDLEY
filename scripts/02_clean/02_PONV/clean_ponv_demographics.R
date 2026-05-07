# Script to obtain and clean demographic features for the PONV study cohort 

# Import libraries
library(tidyverse)
library(readr)
library(naniar)

# Load files
ponv_study_cohort_identifiers <- read_csv("data/cohorts/02_PONV/ponv_study_cohort_identifiers.csv")

hosp_patients <- read_csv("data/raw/hosp_patients.csv")
hosp_admissions <- read_csv("data/raw/hosp_admissions.csv")


# Age and Gender --------------------------------------------------------------------------

age <- hosp_patients %>% filter(subject_id %in% ponv_study_cohort_identifiers$subject_id) %>% select(subject_id, anchor_age) %>% rename(age = anchor_age)

gender <- hosp_patients %>% filter(subject_id %in% ponv_study_cohort_identifiers$subject_id) %>% select(subject_id, gender) %>%
  mutate(gender = case_when(gender == "F" ~ "Female",
                            gender == "M" ~ "Male"))

# Race ---------------------------------------------------------------------------

race <- hosp_admissions %>% filter(hadm_id %in% ponv_study_cohort_identifiers$hadm_id) %>% select(subject_id, hadm_id, race)

# Group race into common categories. If race is missing, make the category "Unknown"
race <- race %>%
  mutate(race = case_when(
    str_detect(race, "ASIAN") ~ "Asian",
    str_detect(race, "BLACK|AFRICAN") ~ "Black/African American",
    str_detect(race, "HISPANIC|LATINO") ~ "Hispanic or Latino",
    race %in% c("OTHER", "MULTIPLE RACE/ETHNICITY", "PORTUGUESE", 
                "SOUTH AMERICAN", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER") ~ "Other",
    race %in% c("PATIENT DECLINED TO ANSWER","UNABLE TO OBTAIN", "UNKNOWN") ~ "Unknown",
    str_detect(race, "WHITE") ~ "White",
    TRUE ~ "Other"
  ))

pct_unknown_race <- round(100*((race %>% filter(race == "Unknown") %>% count())/nrow(ponv_study_cohort_identifiers)),2)
message(sprintf("%s%% of patients in the PONV study cohort have unknown race.", pct_unknown_race))


# Insurance -----------------------------------------------------------------------------

insurance <- hosp_admissions %>% filter(hadm_id %in% ponv_study_cohort_identifiers$hadm_id) %>% select(subject_id, hadm_id, insurance)

# If insurance is missing, make the category "Unknown"
insurance <- insurance %>% mutate(insurance = case_when(is.na(insurance) == TRUE ~ "Unknown",
                                                        TRUE ~ insurance))

pct_unknown_insurance <- round(100*((insurance %>% filter(insurance == "Unknown") %>% count())/nrow(ponv_study_cohort_identifiers)),2)
message(sprintf("%s%% of patients in the PONV study cohort have unknown insurance.", pct_unknown_insurance))


# Marital status -------------------------------------------------------------------------

marital_status <- hosp_admissions %>% filter(hadm_id %in% ponv_study_cohort_identifiers$hadm_id) %>% select(subject_id, hadm_id, marital_status)

# Clean marital status names. If marital status is missing, make the category "Unknown"
marital_status <- marital_status %>% mutate(marital_status = case_when(marital_status == "SINGLE" ~ "Single",
                                                                       marital_status == "MARRIED" ~ "Married",
                                                                       marital_status == "WIDOWED" ~ "Widowed",
                                                                       marital_status == "DIVORCED" ~ "Divorced",
                                                                       is.na(marital_status) == TRUE ~ "Unknown"))

pct_unknown_marriage <- round(100*((marital_status %>% filter(marital_status == "Unknown") %>% count())/nrow(ponv_study_cohort_identifiers)),2)
message(sprintf("%s%% of patients in the PONV study cohort have unknown marital status.", pct_unknown_marriage))

# Discharge location -------------------------------------------------------------------------

discharge_location <- hosp_admissions %>% filter(hadm_id %in% ponv_study_cohort_identifiers$hadm_id) %>% select(subject_id, hadm_id, discharge_location)

# If discharge location is missing, make the category "Unknown"
discharge_location <- discharge_location %>% mutate(discharge_location = case_when(is.na(discharge_location) == TRUE ~ "Unknown",
                                                                                   TRUE ~ discharge_location))

# Merge demographic features and save output file ------------------------------------------------------------------------------------

demographics <- age %>%
  left_join(gender, by = "subject_id") %>%
  left_join(race %>% select(-hadm_id), by = "subject_id") %>%
  left_join(insurance %>% select(-hadm_id), by = "subject_id") %>% 
  left_join(marital_status %>% select(-hadm_id), by = "subject_id") %>%
  left_join(discharge_location %>% select(-hadm_id), by = "subject_id") %>% 
  left_join(ponv_study_cohort_identifiers %>% select(subject_id, hadm_id), by = "subject_id") %>%
  select(subject_id, hadm_id, age, gender, race, insurance, marital_status, discharge_location)

write_csv(demographics, "data/derived/02_PONV/ponv_demographics_cleaned.csv")

message("Demographic features successfully cleaned and saved.")