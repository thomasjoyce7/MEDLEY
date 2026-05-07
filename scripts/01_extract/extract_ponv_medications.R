# Script to extract raw data for PONV medications of interest
# from the hosp/prescriptions, hosp/emar, and icu/inputevents tables

# Load libraries
library(DBI)
library(tidyverse)
library(readr)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Connect to database
con <- dbConnect(RSQLite::SQLite(), Sys.getenv("MIMICIV_DB_PATH", unset = config$paths$db))

# Create raw output folder
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# List of antiemetic drugs (Includes generic names and brand names)
## denotes the drug class
# denotes the generic name 
drug_names <- tolower(c(
  
  ## 1) Serotonin (5-HT3 receptor) antagonists
  
    # Ondansetron 
    "ondansetron", "zofran", "zofran odt", "zuplenz",
    
    # Granisetron 
    "granisetron", "kytril", "sancuso", "sustol",
    
    # Ramosetron
    "ramosetron", "nasea", "iribo",
    
    # Palonosetron
    "palonosetron", "aloxi", "posfrea", "akynzeo",
    
  ## 2) Phenothiazines
  
    # Chlorpromazine (consier removing chlorpromazine since its primarily an antipsychotic medication)
    "chlorpromazine", "thorazine", "largactil", "sonazine",
  
    # Prochlorperazine
    "prochlorperazine", "compazine", "compro", "stemetil", "buccastem",
  
  ## 3) Butyrophenones
  
    # Droperidol
    "droperidol", "inapsine", "droleptan",
  
    # Haloperidol
    "haldol", "haldol decanoate", "serenace",
  
  ## 4) Antihistamines
  
    # Diphenhydramine
    "diphenhydramine", "benadryl",
  
    # Promethazine
    "promethazine", "phenergan", "phenadoz", "promethegan",
  
  ## 5) Anticholinergics
  
    # Scopolamine
    "scopolamine", "transderm scop", "scopace",
  
  ## 6) Benzamides
  
    # Metoclopramide
    "metoclopramide", "reglan", "gimoti", "metozolv",
  
  ## 7) Neurokinin-1 antagonists
  
    # Aprepitant
    "aprepitant", "emend", "cinvanti", "aponvie",
    
    # Fosaprepitant
    "fosaprepitant", "focinvez",
  
  # 8) Corticosteroids
  
    # Dexamethasone
    "dexamethasone", "decadron",
  
  # 9) Others
  
    # Trimethobenzamide
    "trimethobenzamide", "tigan"
    
))


# hosp/prescriptions ----------------------------------------------------------------------------------------------

where_clause <- paste0("LOWER(drug) LIKE '%", drug_names, "%'", collapse = " OR ")

prescriptions_query <- paste0("
  SELECT subject_id, hadm_id, pharmacy_id, starttime, stoptime, drug, dose_val_rx, dose_unit_rx, form_unit_disp, route
  FROM 'hosp/prescriptions'
  WHERE ", where_clause
)

message("Extracting PONV medications from hosp/prescriptions.")
prescriptions <- dbGetQuery(con, prescriptions_query)


# hosp/emar ------------------------------------------------------------------

where_clause <- paste0("LOWER(medication) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT subject_id, hadm_id, emar_id, emar_seq, pharmacy_id, charttime, medication, event_txt
  FROM 'hosp/emar'
  WHERE ", where_clause
)

message("Extracting PONV medications from hosp/emar.")
emar <- dbGetQuery(con, query)

# Extract emar_detail rows for subjects in emar
subject_ids <- paste0("(", paste(unique(emar$subject_id), collapse = ","), ")")

emar_detail_query <- paste0("
  SELECT subject_id, emar_id, emar_seq, parent_field_ordinal, administration_type, product_description, dose_given, dose_given_unit, will_remainder_of_dose_be_given
  FROM 'hosp/emar_detail'
  WHERE subject_id IN ", subject_ids)

message("Extracting PONV medications from hosp/emar_detail.")
emar_detail <- dbGetQuery(con, emar_detail_query)

# Extract pharmacy table rows for subjects in emar
pharmacy_query <- paste0("
  SELECT subject_id, hadm_id, pharmacy_id, starttime, stoptime, medication, doses_per_24_hrs, route, frequency 
  FROM 'hosp/pharmacy'
  WHERE subject_id IN ", subject_ids
)

message("Extracting PONV medications from hosp/pharmacy.")
pharmacy <- dbGetQuery(con, pharmacy_query)


# icu/inputevents -----------------------------------------------------------------------------

where_clause <- paste0("LOWER(label) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT itemid, label
  FROM 'icu/d_items'
  WHERE ", where_clause
)

inputevent_itemids <- dbGetQuery(con, query)

# Extract inputevent records for the corresponding itemids 
itemid_list <- paste0("(", paste(inputevent_itemids$itemid, collapse = ","), ")")

inputevents_query <- paste0("
  SELECT subject_id, hadm_id, stay_id, starttime, endtime, itemid, amount, amountuom, ordercategorydescription
  FROM 'icu/inputevents'
  WHERE itemid IN ", itemid_list
)

message("Extracting PONV medications from icu/inputevents.")
inputevents <- dbGetQuery(con, inputevents_query)


# Save output files -------------------------------------------------------------

write_csv(prescriptions, "data/raw/hosp_prescriptions_ponv_filtered.csv")
write_csv(emar, "data/raw/hosp_emar_ponv_filtered.csv")
write_csv(emar_detail, "data/raw/hosp_emar_detail_ponv_filtered.csv")
write_csv(pharmacy, "data/raw/hosp_pharmacy_ponv_filtered.csv")
write_csv(inputevents, "data/raw/icu_inputevents_ponv_filtered.csv")

message("PONV medications raw tables successfully extracted and saved.")

# Disconnect
dbDisconnect(con)
