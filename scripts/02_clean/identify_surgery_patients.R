# Script to identify all surgery patients and ICU surgery patients

# Load libraries
library(DBI)
library(tidyverse)
library(readr)
library(scales)
library(knitr)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Clear cohort building log 
writeLines("", file.path(config$paths$logs, "cohort_building.log"))

# Load helper functions
source("scripts/00_utils/00_utils.R")

# Load files
procedures_icd <- read_csv(file.path(config$paths$raw_data, "hosp_procedures_icd.csv"))
admissions <- read_csv(file.path(config$paths$raw_data, "hosp_admissions.csv"))
icustays <- read_csv(file.path(config$paths$raw_data, "icu_icustays.csv"))

icd9_pcs <- read_csv(file.path(config$paths$external_data, "hcup_procedure_classes_icd9.csv"), skip = 2)
icd10_pcs <- read_csv(file.path(config$paths$external_data, "hcup_procedure_classes_icd10.csv"), skip = 1)
icd9_cds <- read_csv(file.path(config$paths$external_data, "hcup_clinical_domains_icd9.csv"))
icd10_cds <- read_csv(file.path(config$paths$external_data, "hcup_clinical_domains_icd10.csv"))


# 1) Extract surgery patients by the HCUP procedure class identification method ----------------------------------------------------

# 4 procedure classes:
# Minor diagnostic, minor therapeutic, major diagnostic, major therapeutic
# Surgical patients fall under major diagnostic and major therapeutic

# Links to HCUP procedure classes: 
# ICD-9-CM: https://hcup-us.ahrq.gov/toolssoftware/procedure/procedure.jsp
# ICD-10-PCS: https://hcup-us.ahrq.gov/toolssoftware/procedureicd10/procedure_icd10.jsp

# Links to HCUP clinical classifications software (for clinical domains):
# ICD-9-CM:https://hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp#download
# ICD-10-PCS: https://hcup-us.ahrq.gov/toolssoftware/ccsr/prccsr.jsp

# ICD-10 HCUP files are Procedure Classes Refined v2025.1 and PRCCSR/CCSR v2025.1.

# Rename columns of icd9_pcs
icd9_pcs <- icd9_pcs %>% rename(icd_code=`'ICD-9-CM CODE'`, procedure_description=`'ICD-9-CM CODE DESCRIPTION'`,
                                Category=`'CATEGORY ASSIGNMENT'`)

# Add a column for procedure the class name based on the category number
icd9_pcs <- icd9_pcs %>% mutate(procedure_class = case_when(
  Category == 1 ~ "Minor Diagnostic",
  Category == 2 ~ "Minor Therapeutic",
  Category == 3 ~ "Major Diagnostic",
  Category == 4 ~ "Major Therapeutic"))

# Add a column for icd_version
icd9_pcs$icd_version <- 9

# Remove the quotation marks and trailing white space around the ICD-9 Codes
icd9_pcs$icd_code <- gsub("\\s+$", "", gsub("'", "", icd9_pcs$icd_code))

# Rename columns of icd10_pcs
names(icd10_pcs) <- gsub("^'|'$", "", names(icd10_pcs))
icd10_pcs <- icd10_pcs %>% rename(icd_code = `ICD-10-PCS CODE`, procedure_description = `ICD-10-PCS CODE DESCRIPTION`,
                                  Category = `PROCEDURE CLASS`, procedure_class = `PROCEDURE CLASS NAME`)

# Remove quotes and trailing white space around the ICD-10 codes
icd10_pcs$icd_code <- gsub("\\s+$", "", gsub("'", "", icd10_pcs$icd_code))

# Add a column for icd_version
icd10_pcs$icd_version <- 10

# Combine icd9_pcs and icd10_pcs into a single dataframe
icd_pcs <- rbind(icd9_pcs, icd10_pcs)

# Add a column to procedures_icd for procedure class
procedures_icd <- left_join(procedures_icd, icd_pcs, by = c("icd_code","icd_version"))

# Filter procedures_icd to only include major diagnostic or major therapeutic (i.e., surgery) procedures
surgery_procedures <- procedures_icd %>% filter(procedure_class %in% c("Major Diagnostic","Major Therapeutic"))

log_message(sprintf("Total surgery patients: %s", surgery_procedures %>% distinct(subject_id) %>% count))

# Add a column to surgery_procedures for clinical domains (this will make it easier to group procedures later)
icd9_cds <- icd9_cds %>% select(`'ICD-9-CM CODE'`, `'CCS LVL 1 LABEL'`) %>% rename(icd_code = `'ICD-9-CM CODE'`, clinical_domain = `'CCS LVL 1 LABEL'`) %>%
  mutate(icd_version = 9)

icd9_cds$icd_code <- gsub("'", "", icd9_cds$icd_code)

icd10_cds <- icd10_cds %>% select(`'ICD-10-PCS'`, `'CLINICAL DOMAIN'`) %>% rename(icd_code = `'ICD-10-PCS'`, clinical_domain = `'CLINICAL DOMAIN'`) %>% 
  mutate(icd_version = 10)

icd10_cds$icd_code <- gsub("'", "", icd10_cds$icd_code)

icd_cds <- rbind(icd9_cds, icd10_cds)

icd_cds$icd_code <- trimws(icd_cds$icd_code, which = "right")

surgery_procedures <- left_join(surgery_procedures, icd_cds, by = c("icd_code", "icd_version"))

all_surgical_patients <- left_join(surgery_procedures, admissions, by = c("subject_id", "hadm_id")) %>%
  select(subject_id, hadm_id, admittime, dischtime, chartdate, procedure_description, procedure_class, clinical_domain)
  
colnames(all_surgical_patients) <- c("subject_id", "hadm_id", "hosp_admittime", "hosp_dischtime", "procedure_date", "procedure_description", "procedure_class", "clinical_domain")

# Save all_surgical_patients as a csv file
write_csv(all_surgical_patients, "data/derived/all_surgical_patients.csv")
log_message("All surgical patients have been identified and saved.")


# 2) Filter to ICU patients with qualifying surgeries based on the inclusion criteria ----------------

log_message("Filtering to ICU surgery patients based on inclusion criteria.")

# 1. Filter surgery_procedures to only include hadm_id's that also appear in the icustays table
icu_surgery_procedures <- surgery_procedures %>% filter(hadm_id %in% icustays$hadm_id)

# 2. Merge icustays, admissions, and icu_surgery_procedures
icu_surgery <- merge(merge(icustays %>% select(-last_careunit), admissions %>% select("subject_id","hadm_id","admittime","dischtime","deathtime"), by = c("subject_id","hadm_id")),
                     icu_surgery_procedures %>% select(subject_id, hadm_id, chartdate, procedure_description, procedure_class, clinical_domain), 
                     by = c("subject_id", "hadm_id")) %>% distinct()

# 3. Filter to ICU stay length at least 24 hours
icu_surgery <- icu_surgery %>% filter(los >= 1)

# 4. Filter to surgery before or during ICU stay
icu_surgery <- icu_surgery %>% filter(chartdate <= outtime)

# 5. Keep most recent hospital admission
icu_surgery <- icu_surgery %>%
  group_by(subject_id) %>% 
  filter(admittime == max(admittime)) %>% 
  ungroup()

# 6. Keep first ICU stay within that admission
icu_surgery <- icu_surgery %>%
  group_by(subject_id, hadm_id) %>% 
  filter(intime == min(intime)) %>% 
  ungroup()


# 3) Save output file ----------------------------------------------------------------

# Rename columns for clarity
icu_surgery <- icu_surgery %>% rename(icu_careunit = first_careunit, icu_intime = intime, icu_outtime = outtime, los_days = los,
                                      hosp_admittime = admittime, hosp_dischtime = dischtime, procedure_date = chartdate)

log_message(sprintf("Total ICU surgery patients after filtering: %s", icu_surgery %>% distinct(subject_id) %>% count()))

# Save output file
write_csv(icu_surgery, "data/derived/icu_surgery_patients.csv")
log_message("ICU surgery patients have been identified and saved.")















