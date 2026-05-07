# Script to obtain and clean perioperative PONV medications for the PONV study cohort
# Perioperative window: 1 day before the qualifying surgery date to 4 days after the qualifying surgery date

# Import libraries
library(tidyverse)
library(scales)
library(knitr)
library(readr)
library(lubridate)
library(stringr)
library(hms)
library(glue)
library(naniar)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Load PONV study cohort identifiers 
ponv_study_cohort_identifiers <- read_csv("data/cohorts/02_PONV/ponv_study_cohort_identifiers.csv")

# Load relevant antiemetic medication data 
hosp_emar_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_emar_ponv_filtered.csv"))
hosp_emar_detail_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_emar_detail_ponv_filtered.csv"))
hosp_pharmacy_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_pharmacy_ponv_filtered.csv"))
hosp_prescriptions_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_prescriptions_ponv_filtered.csv"))

icu_inputevents_filtered <- read_csv(file.path(config$paths$raw_data, "icu_inputevents_ponv_filtered.csv"))
icu_d_items <- read_csv(file.path(config$paths$raw_data, "icu_d_items.csv"))


# hosp/emar ------------------------------------------------------------------

message("Extracting and cleaning antiemetic medications from hosp/emar.")

# Merge hosp_emar_filtered with ponv_study_cohort_identifiers and filter to administered medications with a charttime occurring -1 to +4 days around surgery 
emar_meds <- left_join(hosp_emar_filtered, ponv_study_cohort_identifiers %>% select(subject_id, hadm_id, procedure_date), by = c("subject_id", "hadm_id")) %>% 
  filter(difftime(charttime, procedure_date, units = "days") >= -1 & difftime(charttime, procedure_date, units = "days") <= 4) %>% 
  filter(event_txt %in% c("Administered", "Administered in Other Location", "Applied", "Applied in Other Location","Confirmed"))

# Extract hosp_emar_detail_filtered rows for corresponding entries in emar_meds (link tables using emar_id)
hosp_ponv_emar_detail <- hosp_emar_detail_filtered %>% filter(emar_id %in% emar_meds$emar_id)

# Extract hosp_prescriptions_filtered rows for corresponding entries in emar_meds (link tables using pharmacy_id)
hosp_ponv_prescriptions <- hosp_prescriptions_filtered %>% filter(pharmacy_id %in% emar_meds$pharmacy_id)

# Extract hosp_pharmacy_filtered rows for corresponding entries in emar_meds (link tables using pharmacy_id)
hosp_ponv_pharmacy <- hosp_pharmacy_filtered %>% filter(pharmacy_id %in% emar_meds$pharmacy_id)

# Obtain given dose amounts and units from hosp_pod_emar_detail (Gold standard for administrations)
# Also take the sum of formulary doses to obtain the complete dose and restrict to positive dose amounts 
hosp_ponv_emar_dosage_data <- hosp_ponv_emar_detail %>%
  filter(!is.na(parent_field_ordinal)) %>%
  mutate(dose_given = as.numeric(dose_given)) %>% 
  group_by(emar_id, dose_given_unit, product_description) %>%
  summarise(true_dose_amount = sum(dose_given, na.rm = TRUE), .groups = "drop") %>%
  filter(true_dose_amount > 0)

# Merge emar_meds with hosp_ponv_emar_dosage_data
emar_meds_with_true_dose_amounts <- merge(emar_meds, hosp_ponv_emar_dosage_data, by = "emar_id")

# Add route information for subjects who have a pharmacy_id (otherwise, we will have to infer the route based on the product description and units)
emar_meds_with_true_dose_amounts_and_route <- left_join(emar_meds_with_true_dose_amounts, hosp_ponv_pharmacy %>% select(pharmacy_id, route), by = "pharmacy_id")

# For emar_ids with NA route, check administration parameters to determine the route
na_route_counts <- emar_meds_with_true_dose_amounts_and_route %>% filter(is.na(route)) %>% count(medication, dose_given_unit, product_description) %>% arrange(desc(n))

# Impute missing route names in emar_meds_with_true_dose_amounts_and_route as appropriate based on the corresponding non-missing route names for each drug and product description 
# If the route cannot easily be determined for a specific medication, label the route as "Unknown"
emar_meds_with_true_dose_amounts_and_route <- emar_meds_with_true_dose_amounts_and_route %>% 
  mutate(updated_route = case_when(
                           medication == "Ondansetron" & is.na(route) ~ "IV",
                           medication == "Ondansetron ODT" & is.na(route) ~ "PO/NG",
                           medication == "Metoclopramide" & (product_description == "Metoclopramide" | is.na(product_description)) & is.na(route) ~ "IV",
                           medication == "Metoclopramide" & product_description == "Metoclopramide 10mg Tablet" & is.na(route) ~ "PO/NG",
                           medication == "Scopolamine Patch" & is.na(route) ~ "TD",
                           medication == "Promethazine" & product_description == "Promethazine 25 mg Tab" & is.na(route) ~ "PO/NG",
                           medication == "Promethazine" & product_description == "Promethazine 25mg/mL Amp" & is.na(route) ~ "IV",
                           medication == "Promethazine" & is.na(product_description) & is.na(route) ~ "Unknown",
                           medication == "Dexamethasone" & is.na(product_description) & is.na(route) ~ "IV",
                           medication == "Dexamethasone" & product_description == "Dexamethasone" & is.na(route) ~ "PO/NG",
                           medication == "Dexamethasone" & product_description == "Dexamethasone Sod Phosphate" & is.na(route) ~ "IV",
                           medication == "Prochlorperazine" & product_description == "Prochlorperazine" & is.na(route) ~ "IV",
                           medication == "Prochlorperazine" & product_description == "Prochlorperazine Maleate 5 mg Tab" & is.na(route) ~ "PO",
                           medication == "Prochlorperazine" & is.na(product_description) & is.na(route) ~ "IV",
                           medication == "Prochlorperazine" & product_description == "Prochlorperazine (Rectal)" & is.na(route) ~ "PR",
                           medication == "Prochlorperazine" & product_description == "Prochlorperazine Maleate" & is.na(route) ~ "PO",
                           medication == "DiphenhydrAMINE" & product_description == "DiphenhydrAMINE 25mg Cap" & is.na(route) ~ "PO/NG",
                           medication == "DiphenhydrAMINE" & product_description == "DiphenhydrAMINE 25mg/10mL Cup" & is.na(route) ~ "PO/NG",
                           medication == "DiphenhydrAMINE" & product_description == "DiphenhydrAMINE 50mg/mL Vial" & is.na(route) ~ "IV",
                           medication == "DiphenhydrAMINE" & is.na(product_description) & is.na(route) ~ "Unknown",
                           medication == "Aprepitant" & product_description == "Aprepitant 80mg Capsule" & is.na(route) ~ "PO/NG",
                           TRUE ~ route))

# Check for emar_ids with missing dose_given_units
missing_units <- emar_meds_with_true_dose_amounts_and_route %>% filter(is.na(dose_given_unit))
                                                                                                      
# All subjects with missing units had "Scopolamine Patch" so input "PTCH" for dose_given_unit
emar_meds_with_true_dose_amounts_and_route <- emar_meds_with_true_dose_amounts_and_route %>% 
  mutate(dose_given_unit = case_when(dose_given_unit = is.na(dose_given_unit) ~ "PTCH",
                                     TRUE ~ dose_given_unit))

# Filter to relevant columns to be combined with medication administrations from the icu/inputevents table 
hosp_emar_ponv_filtered <- emar_meds_with_true_dose_amounts_and_route %>% mutate(table = "emar") %>%
  select(subject_id, hadm_id, medication, charttime, true_dose_amount, dose_given_unit, updated_route, table)

colnames(hosp_emar_ponv_filtered) <- c("subject_id", "hadm_id", "medication", "admin_time", "amount", "units", "route", "table")


# icu/inputevents -----------------------------------------------------------------------------

message("Extracting and cleaning antiemetic medications from icu/inputevents.")

# Join icu_inputevents_filtered with icu_d_items to obtain antiemetic medication names and drop the itemid column
inputevents_meds <- left_join(icu_inputevents_filtered, icu_d_items %>% select(itemid, label), by = "itemid") %>% select(-itemid)

# Merge inputevents_meds with ponv_study_cohort_identifiers and filter to inputevents with a starttime occurring -1 to +4 days around surgery 
icu_inputevents_ponv_filtered <- merge(inputevents_meds, ponv_study_cohort_identifiers %>% select(subject_id, hadm_id, procedure_date), by = c("subject_id", "hadm_id")) %>% 
  filter(difftime(starttime, procedure_date, units = "days") >= -1 & difftime(starttime, procedure_date, units = "days") <= 4) 

# Check medication and route types
icu_inputevents_ponv_filtered %>% count(label, ordercategorydescription)

# Summarize administration duration
icu_inputevents_ponv_filtered %>% summarize(min(difftime(endtime, starttime, units = "mins")), median(difftime(endtime, starttime, units = "mins")), max(difftime(endtime, starttime, units = "mins")))
# Note: All administrations are Drug Push, which are set to 1 minute by default in the MIMIC-IV database 
# For simplicity, we will just keep the starttime of each drug administration.

# Restrict administrations to positive dose amounts 
icu_inputevents_ponv_filtered <- icu_inputevents_ponv_filtered %>% filter(amount > 0)

# Filter to relevant columns to be combined with medication administrations from the hosp/emar table                              
icu_inputevents_ponv_filtered <- icu_inputevents_ponv_filtered %>% mutate(table = "inputevents") %>%
  select(subject_id, hadm_id, label, starttime, amount, amountuom, ordercategorydescription, table)           
                  
colnames(icu_inputevents_ponv_filtered) <- c("subject_id", "hadm_id", "medication", "admin_time", "amount", "units", "route", "table")


# Combine antiemetic medications from hosp_emar_ponv_filtered and icu_inputevents_ponv_filtered -----------------------------------------------------

ponv_medications <- rbind(hosp_emar_ponv_filtered, icu_inputevents_ponv_filtered)


# Additional data cleaning steps ---------------------------------------------------------------------------

# Check all medication, units, and route names 
med_and_route_names <- ponv_medications %>% count(medication, units, route) %>% arrange(desc(n))

# Remove medication administrations that are clearly NOT being used for PONV prevention or treatment 
# Remove "Maalox/Diphenhydramine/Lidocaine"; this is not an antiemetic and it is used for mouthwash. 
# Remove "Tobramycin-Dexamethasone Ophth Susp"; this is an eye medication, not an antiemetic. 
# Remove "Tobramycin-Dexamethasone Ophth Oint"; this is an eye medication, not an antiemetic. 
# Remove "Dexamethasone Ophthalmic Susp 0.1%", this is used for ear drops.
# Remove "raloxifene"; this is not an antiemetic
# Remove "dexamethasone" medication via "INHALATION" route; this is not a typical administration route for antiemetic purposes. 

ponv_medications <- ponv_medications %>% filter(!(medication %in% 
                                                    c("Maalox/Diphenhydramine/Lidocaine",
                                                      "Tobramycin-Dexamethasone Ophth Susp",
                                                      "Tobramycin-Dexamethasone Ophth Oint",
                                                      "Dexamethasone Ophthalmic Susp 0.1%",
                                                      "raloxifene"))) %>%
  filter(!(medication == "dexamethasone" & route == "INHALATION"))


# Rename medications in final_cohort_meds to match all brand names with generic names and assign drug class
ponv_medications <- ponv_medications %>% mutate(medication = case_when(
  medication %in% c("Ondansetron", "Ondansetron (Zofran)", "Ondansetron ODT") ~ "Ondansetron",
  medication == "Metoclopramide" ~ "Metoclopramide",
  medication %in% c("Dexamethasone", "Dexamethasone Oral Soln (0.1mg/1mL)") ~ "Dexamethasone",
  medication == "DiphenhydrAMINE" ~ "Diphenhydramine",
  medication == "Prochlorperazine" ~ "Prochlorperazine",
  medication == "Scopolamine Patch" ~ "Scopolamine",
  medication == "Promethazine" ~ "Promethazine",
  medication == "Haloperidol (Haldol)" ~ "Haloperidol",
  medication == "ChlorproMAZINE" ~ "Chlorpromazine",
  medication == "Fosaprepitant" ~ "Fosaprepitant",
  medication == "Aprepitant" ~ "Aprepitant",
  medication == "Palonosetron" ~ "Palonosetron"
), drug_class = case_when(
  medication %in% c("Ondansetron", "Granisetron", "Ramosetron", "Palonosetron") ~ "5-HT3 receptor antagonists",
  medication %in% c("Chlorpromazine", "Prochlorperazine") ~ "Phenothiazines",
  medication %in% c("Droperidol", "Haloperidol") ~ "Butyrophenones",
  medication %in% c("Diphenhydramine", "Promethazine") ~ "Antihistamines",
  medication %in% c("Scopolamine") ~ "Anticholinergics",
  medication %in% c("Metoclopramide") ~ "Benzamides",
  medication %in% c("Aprepitant", "Fosaprepitant") ~ "Neurokinin-1 antagonists",
  medication %in% c("Dexamethasone") ~ "Corticosteroids"
))

# Rename routes for consistency
ponv_medications <- ponv_medications %>%
  mutate(route = case_when(
    route %in% c("PO", "ORAL") ~ "oral",
    route == "PO/NG" ~ "enteral",
    route == "IM" ~ "intramuscular",
    route == "TD" ~ "transdermal",
    route == "PR" ~ "per rectum",
    route == "SC" ~ "subcutaneous",
    route == "SL" ~ "sublingual",
    route == "Drug Push" ~ "IV",
    route == "Unknown" ~ "unknown",
    TRUE ~ route  
  ))

# Rename PTCH units as "patch"
ponv_medications <- ponv_medications %>% mutate(units = case_when(units == "PTCH" ~ "patch",
                                                                  TRUE ~ units))

# Round all amounts to 2 decimal places
ponv_medications <- ponv_medications %>% mutate(amount = round(amount, 2))

# Filter to distinct rows to remove duplicate drug administrations 
ponv_medications_cleaned <- ponv_medications %>% group_by(subject_id, hadm_id) %>% distinct(medication, admin_time, amount, units, route, drug_class) %>% ungroup()

# Add a column for procedure_date (this will be useful later for determining the medication timing relative to surgery)
ponv_medications_cleaned <- left_join(ponv_medications_cleaned, ponv_study_cohort_identifiers %>% select(subject_id, hadm_id, procedure_date), by = c("subject_id", "hadm_id"))


# Save output files -------------------------------------------------------------

write_csv(ponv_medications_cleaned, "data/derived/02_PONV/ponv_medications_cleaned.csv")

message("PONV medications successfully cleaned and saved.")