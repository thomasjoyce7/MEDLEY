# Script to obtain POD medications for ICU surgery patients with at least 
# one positive postoperative CAM-ICU result and clean up the medication data

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

# Load files
pod_positive <- read_csv(file.path(config$paths$derived_data, "pod_positive_identifiers.csv"))

hosp_emar_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_emar_filtered.csv"))
hosp_emar_detail_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_emar_detail_filtered.csv"))
hosp_pharmacy_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_pharmacy_filtered.csv"))
hosp_prescriptions_filtered <- read_csv(file.path(config$paths$raw_data, "hosp_prescriptions_filtered.csv"))

icu_inputevents_filtered <- read_csv(file.path(config$paths$raw_data, "icu_inputevents_filtered.csv"))
icu_d_items <- read_csv(file.path(config$paths$raw_data, "icu_d_items.csv"))


# hosp/emar ------------------------------------------------------------------

message("Extracting and cleaning POD medications from hosp/emar.")

# Merge hosp_emar_filtered with pod_positive and filter to medications with a charttime 
# occurring during the ICU stay
hosp_pod_emar <- merge(hosp_emar_filtered, pod_positive %>% select(subject_id, hadm_id, stay_id, icu_intime, icu_outtime), by = c("subject_id", "hadm_id")) %>%
  filter(charttime >= icu_intime & charttime <= icu_outtime)

# Filter hosp_pod_emar to administered medications. Assume that an event_txt of 
# "Administered", "Confirmed", "Administered in Other Location", or "Applied" indicates the medication was administered
hosp_pod_emar <- hosp_pod_emar %>% filter(event_txt %in% c("Administered", "Administered in Other Location", "Applied", "Confirmed"))

# Extract hosp_emar_detail_filtered rows for corresponding entries in hosp_pod_emar
hosp_pod_emar_detail <- hosp_emar_detail_filtered %>% filter(emar_id %in% hosp_pod_emar$emar_id)

# Extract hosp_prescriptions_filtered rows for corresponding entries in hosp_pod_emar
hosp_pod_prescriptions <- hosp_prescriptions_filtered %>% filter(pharmacy_id %in% hosp_pod_emar$pharmacy_id)

# Obtain given dose amounts and units from hosp_pod_emar_detail (Gold standard for administrations)
hosp_pod_emar_dosage_data <- hosp_pod_emar_detail %>%
  filter(!is.na(parent_field_ordinal)) %>%
  mutate(dose_given = as.numeric(dose_given)) %>% 
  group_by(emar_id, dose_given_unit) %>%
  summarise(true_amount = sum(dose_given, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(dose_given_unit) & true_amount > 0)

# Merge hosp_pod_emar with hosp_pod_prescriptions and add a column for drug duration
hosp_pod_emar <- merge(hosp_pod_emar %>% select(-emar_seq), hosp_pod_prescriptions, 
                       by = c("subject_id", "hadm_id", "pharmacy_id")) %>% select(-medication, -form_unit_disp) %>%
  mutate(duration = stoptime - starttime) %>%
  mutate(endtime = charttime + duration) %>% select(-starttime, -stoptime, -duration) %>%
  rename(medication = drug)

# Replace the amounts and unit values in hosp_pod_emar with the corresponding entries in 
# hosp_pod_emar_dosage_data for emar_id's in hosp_pod_emar_dosage_data
hosp_pod_emar <- hosp_pod_emar %>%
  left_join(hosp_pod_emar_dosage_data, by = "emar_id")

hosp_pod_emar <- hosp_pod_emar %>% filter(!str_detect(dose_val_rx, "-")) %>%
  mutate(
    amount = case_when(
      !is.na(dose_given_unit) & !is.na(true_amount) ~ true_amount,
      TRUE ~ as.numeric(dose_val_rx)
    ),
    units = case_when(
      !is.na(dose_given_unit) & !is.na(true_amount) ~ dose_given_unit,
      TRUE ~ as.character(dose_unit_rx)
    )
  ) %>% select(-dose_val_rx, -dose_unit_rx, -true_amount, -dose_given_unit)

# Check the administration routes in hosp_pod_emar 
routes <- hosp_pod_emar %>% count(route)

# For medications that are not continuously administered 
# (i.e., PO/NG, PO, IM, NG, ORAL, BU, SL) set starttime and stoptime equal to charttime 
# in the emar table. Otherwise, set starttime equal to charttime and stoptime equal to endtime
hosp_pod_emar <- hosp_pod_emar %>% mutate(starttime = charttime, 
                                          stoptime = case_when(route %in% c("PO/NG", "PO", "IM", "NG", "ORAL", "BU", "SL") ~ charttime,
                                                    TRUE ~ endtime))

# Rearrange the columns of hosp_pod_emar 
hosp_pod_emar <- hosp_pod_emar %>% select(subject_id, hadm_id, stay_id, medication, starttime, stoptime, amount, units, route)


# icu/inputevents -----------------------------------------------------------------------------

message("Extracting and cleaning POD medications from icu/inputevents.")

# Join icu_inputevents_filtered with icu_d_items to obtain POD medication names and drop the itemid column
icu_pod_inputevents <- left_join(icu_inputevents_filtered, icu_d_items %>% select(itemid, label), by = "itemid") %>% select(-itemid)

# Merge icu_pod_inputevents with pod_positive and filter to medications administered during the ICU stay
icu_pod_inputevents <- merge(icu_pod_inputevents, pod_positive %>% select(subject_id, hadm_id, stay_id, icu_intime, icu_outtime), 
                             by = c("subject_id", "hadm_id", "stay_id")) %>% filter(starttime >= icu_intime & starttime <= icu_outtime) %>% select(-icu_intime, -icu_outtime)

# Rearrange the columns of icu_pod_inputevents and rename 
icu_pod_inputevents <- icu_pod_inputevents %>% select(subject_id, hadm_id, stay_id, label, starttime, endtime, amount, amountuom, ordercategorydescription) %>%
  rename(medication = label, stoptime = endtime, units = amountuom, route = ordercategorydescription)


# Combine POD medications from hosp_pod_emar and icu_pod_inputevents -----------------------------------------------------

pod_medications <- rbind(hosp_pod_emar, icu_pod_inputevents)

# Note: There is likely some overlap between the tables so filter to distinct rows
pod_medications <- pod_medications %>% distinct()

# Count all medication and route combinations in pod_medications
medication_routes <- pod_medications %>% count(medication, route)

# There is likely some overlap between Dexmedetomidine (IV DRIP) from hosp/emar and Dexmedetomidine (Continuous Med) from icu/inputevents.
# There is likely some overlap between Haloperidol (IV) from hosp/emar and Haloperidol (Drug Push) from icu/inputevents.
# Since these are IV medications, we should only keep rows from icu/inputevents since they are more accurate.

# For subjects with duplicate rows for Dexmedetomidine, remove the corresponding entries derived from hosp/emar. 
dexmedetomidine_duplicates <- pod_medications %>%
  filter((medication == "Dexmedetomidine" & route == "IV DRIP") |
           (medication == "Dexmedetomidine (Precedex)" & route == "Continuous Med")) %>%
  group_by(subject_id, hadm_id, stay_id) %>%
  filter(any(route == "IV DRIP") & any(route == "Continuous Med")) %>%
  ungroup()

pod_medications <- pod_medications %>%
  anti_join(
    dexmedetomidine_duplicates %>% filter(route == "IV DRIP"),
    by = c("subject_id", "hadm_id", "stay_id", "medication", "route")
  )

# For subjects with duplicate rows for Haloperidol, remove the corresponding entries derived from hosp/emar. 
haloperidol_duplicates <- pod_medications %>%
  filter((medication == "Haloperidol" & route == "IV") |
           (medication == "Haloperidol (Haldol)" & route == "Drug Push")) %>%
  group_by(subject_id, hadm_id, stay_id) %>%
  filter(any(route == "IV") & any(route == "Drug Push")) %>%
  ungroup()

pod_medications <- pod_medications %>%
  anti_join(
    haloperidol_duplicates %>% filter(route == "IV"),
    by = c("subject_id", "hadm_id", "stay_id", "medication", "route")
  )


# Additional data cleaning steps ---------------------------------------------------------------------------

# Remove medications with amount == 0
pod_medications <- pod_medications %>% filter(!(amount == 0))

# Remove medications with starttime > stoptime
pod_medications <- pod_medications %>% filter(!(starttime > stoptime))

# Rename medications by normalizing trade names and salt forms for consistency
pod_medications <- pod_medications %>%
  mutate(medication = case_when(
    medication == "ChlorproMAZINE" ~ "Chlorpromazine",
    medication == "Dexmedetomidine (Precedex)" ~ "Dexmedetomidine",
    medication == "Haloperidol (Haldol)" ~ "Haloperidol",
    medication == "Haloperidol Decanoate (long acting)" ~ "Haloperidol",
    medication == "INV-Dexmedetomidine" ~ "Dexmedetomidine",
    medication == "OLANZapine" ~ "Olanzapine",
    medication == "OLANZapine (Disintegrating Tablet)" ~ "Olanzapine",
    medication == "QUEtiapine Fumarate" ~ "Quetiapine", # Salt form
    medication == "QUEtiapine extended-release" ~ "Quetiapine",
    medication == "RISperidone" ~ "Risperidone",
    medication == "RisperiDONE" ~ "Risperidone",
    medication == "RISperidone (Disintegrating Tablet)" ~ "Risperidone",
    medication == "RisperiDONE (Disintegrating Tablet)" ~ "Risperidone",
    medication == "RISperidone Oral Solution" ~ "Risperidone",
    medication == "RisperiDONE Oral Solution" ~ "Risperidone",
    medication == "rivastigmine" ~ "Rivastigmine",
    medication == "rivastigmine tartrate" ~ "Rivastigmine Tartrate", # Salt form
    TRUE ~ medication
  ))

# Rename routes for consistency
pod_medications <- pod_medications %>%
  mutate(route = case_when(
    route == "PO/NG" ~ "enteral",
    route == "IV DRIP" ~ "IV continuous",
    route == "PO" ~ "oral",
    route == "ORAL" ~ "oral",
    route == "IM" ~ "intramuscular",
    route == "IV" ~ "IV continuous",
    route == "TRANSDERMAL" ~ "transdermal",
    route == "NG" ~ "nasogastric",
    route == "BU" ~ "buccal",
    route == "SL" ~ "sublingual",
    route == "Drug Push" ~ "IV push",
    route == "Continuous Med" ~ "IV continuous",
    route == "PB" ~ "IV piggyback",
    TRUE ~ route  
  ))

# Round amount to 2 decimal places when amount > 0.01;
# Otherwise, round amount to 5 decimal places
pod_medications <- pod_medications %>% mutate(amount = case_when(
  amount > 0.01 ~ round(amount, 2),
  TRUE ~ round(amount, 5)
))

# Summarize missing variables
miss_var_summary(pod_medications)


# Save output files -------------------------------------------------------------

write_csv(pod_medications, "data/derived/pod_medications_cleaned.csv")

message("POD medications successfully cleaned and saved.")

