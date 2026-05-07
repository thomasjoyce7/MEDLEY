# Script to obtain and clean demographic features for ICU surgery patients
# with at least one positive postoperative CAM-ICU result

# Import libraries
library(tidyverse)
library(readr)
library(naniar)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Load files
pod_positive <- read_csv(file.path(config$paths$derived_data, "pod_positive_identifiers.csv"))

hosp_patients <- read_csv(file.path(config$paths$raw_data, "hosp_patients.csv"))
hosp_admissions <- read_csv(file.path(config$paths$raw_data, "hosp_admissions.csv"))


# Age and Gender --------------------------------------------------------------------------

age <- hosp_patients %>% filter(subject_id %in% pod_positive$subject_id) %>% select(subject_id, anchor_age) %>% rename(age = anchor_age)

gender <- hosp_patients %>% filter(subject_id %in% pod_positive$subject_id) %>% select(subject_id, gender) %>%
  mutate(gender = case_when(gender == "F" ~ "Female",
                            gender == "M" ~ "Male"))

message("All POD positive patients have records for anchor age and gender.")


# Race ---------------------------------------------------------------------------

race <- hosp_admissions %>% filter(hadm_id %in% pod_positive$hadm_id) %>% select(subject_id, hadm_id, race)

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

pct_unknown_race <- round(100*((race %>% filter(race == "Unknown") %>% count())/nrow(pod_positive)),2)
message(sprintf("%s%% of POD positive patients have unknown race.", pct_unknown_race))


# Insurance -----------------------------------------------------------------------------

insurance <- hosp_admissions %>% filter(hadm_id %in% pod_positive$hadm_id) %>% select(subject_id, hadm_id, insurance)

# If insurance is missing, make the category "Unknown"
insurance <- insurance %>% mutate(insurance = case_when(is.na(insurance) == TRUE ~ "Unknown",
                                                        TRUE ~ insurance))

pct_unknown_insurance <- round(100*((insurance %>% filter(insurance == "Unknown") %>% count())/nrow(pod_positive)),2)
message(sprintf("%s%% of POD positive patients have unknown insurance.", pct_unknown_insurance))


# Marital status -------------------------------------------------------------------------

marital_status <- hosp_admissions %>% filter(hadm_id %in% pod_positive$hadm_id) %>% select(subject_id, hadm_id, marital_status)

# Clean marital status names. If marital status is missing, make the category "Unknown"
marital_status <- marital_status %>% mutate(marital_status = case_when(marital_status == "SINGLE" ~ "Single",
                                                                       marital_status == "MARRIED" ~ "Married",
                                                                       marital_status == "WIDOWED" ~ "Widowed",
                                                                       marital_status == "DIVORCED" ~ "Divorced",
                                                                       is.na(marital_status) == TRUE ~ "Unknown"))

pct_unknown_marriage <- round(100*((marital_status %>% filter(marital_status == "Unknown") %>% count())/nrow(pod_positive)),2)
message(sprintf("%s%% of POD positive patients have unknown marital status.", pct_unknown_marriage))

# Discharge location -------------------------------------------------------------------------

discharge_location <- hosp_admissions %>% filter(hadm_id %in% pod_positive$hadm_id) %>% select(subject_id, hadm_id, discharge_location)

message("All POD positive patients have records for discharge location.")

# Merge demographic features and save output file ------------------------------------------------------------------------------------

demographics <- age %>%
  left_join(gender, by = "subject_id") %>%
  left_join(race %>% select(-hadm_id), by = "subject_id") %>%
  left_join(insurance %>% select(-hadm_id), by = "subject_id") %>% 
  left_join(marital_status %>% select(-hadm_id), by = "subject_id") %>%
  left_join(discharge_location %>% select(-hadm_id), by = "subject_id") %>% 
  left_join(pod_positive %>% select(subject_id, hadm_id, stay_id), by = "subject_id") %>%
  select(subject_id, hadm_id, stay_id, age, gender, race, insurance, marital_status, discharge_location)

write_csv(demographics, "data/derived/demographics_cleaned.csv")

message("Demographic features successfully cleaned and saved.")




