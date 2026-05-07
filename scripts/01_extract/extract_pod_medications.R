# Script to extract raw data for POD treatment medications of interest
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

# List of drug names for POD treatment (Includes generic names and trade names)
drug_names <- tolower(c(
  # Haloperidol
  "haloperidol", "haldol", "haldol decanoate", "serenace",
  # Risperidone
  "risperidone", "risperdal consta", "perseris", "risvan", "rykindo", "uzedy",
  # Quetiapine
  "quetiapine", "seroquel", "seroquel XR", "atrolak", "biquelle", "sondate", "zaluron",
  # Olanzapine
  "olanzapine", "zyprexa", "zyprexa zydis", "zxprexa relprevv", "zyprexa intra-muscular", "lybalvi", "olazax", "symbyax", "zalasta", "zypadhera",
  # Dexmedetomidine
  "dexmedetomidine", "precedex", "dexdor", "igalmi",
  # Melatonin
  "melatonin", "circadin", "slenyto", "adaflex", "ceyesto", "syncrodin",
  # Rivastigmine
  "rivastigmine", "exelon", "nimvastid", "prometax",
  # Chlorpromazine
  "chlorpromazine", "thorazine", "largactil", "sonazine"
))


# hosp/prescriptions ----------------------------------------------------------------------------------------------

where_clause <- paste0("LOWER(drug) LIKE '%", drug_names, "%'", collapse = " OR ")

prescriptions_query <- paste0("
  SELECT subject_id, hadm_id, pharmacy_id, starttime, stoptime, drug, dose_val_rx, dose_unit_rx, form_unit_disp, route
  FROM 'hosp/prescriptions'
  WHERE ", where_clause
)

message("Extracting POD medications from hosp/prescriptions.")
prescriptions <- dbGetQuery(con, prescriptions_query)


# hosp/emar ------------------------------------------------------------------

where_clause <- paste0("LOWER(medication) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT subject_id, hadm_id, emar_id, emar_seq, pharmacy_id, charttime, medication, event_txt
  FROM 'hosp/emar'
  WHERE ", where_clause
)

message("Extracting POD medications from hosp/emar.")
emar <- dbGetQuery(con, query)

# Extract emar_detail rows for subjects in emar
subject_ids <- paste0("(", paste(unique(emar$subject_id), collapse = ","), ")")

emar_detail_query <- paste0("
  SELECT subject_id, emar_id, emar_seq, parent_field_ordinal, product_description, dose_given, dose_given_unit, will_remainder_of_dose_be_given
  FROM 'hosp/emar_detail'
  WHERE subject_id IN ", subject_ids)

message("Extracting POD medications from hosp/emar_detail.")
emar_detail <- dbGetQuery(con, emar_detail_query)

# Extract pharmacy table rows for subjects in emar
pharmacy_query <- paste0("
  SELECT subject_id, hadm_id, pharmacy_id, starttime, stoptime, medication, doses_per_24_hrs, route
  FROM 'hosp/pharmacy'
  WHERE subject_id IN ", subject_ids
)

message("Extracting POD medications from hosp/pharmacy.")
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

message("Extracting POD medications from icu/inputevents.")
inputevents <- dbGetQuery(con, inputevents_query)


# Save output files -------------------------------------------------------------

write_csv(prescriptions, "data/raw/hosp_prescriptions_filtered.csv")
write_csv(emar, "data/raw/hosp_emar_filtered.csv")
write_csv(emar_detail, "data/raw/hosp_emar_detail_filtered.csv")
write_csv(pharmacy, "data/raw/hosp_pharmacy_filtered.csv")
write_csv(inputevents, "data/raw/icu_inputevents_filtered.csv")

message("POD medications raw tables successfully extracted and saved.")

# Disconnect
dbDisconnect(con)


